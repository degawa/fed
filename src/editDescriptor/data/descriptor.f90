module fed_editDescriptor_data
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor
    implicit none
    private
    public :: dat

    character(*), public, parameter :: general_data_edit_descriptor_symbol = "G"
        !! G形データ編集記述子（一般形編集）に用いられる英字定数

    !>データ編集記述子を取り扱う派生型．
    !>この型は，具体的な編集
    !>（数値編集，論理編集，文字型編集）
    !>を定義するための祖先型として定義されている．
    type, public, extends(edit_descriptor_type) :: data_edit_descriptor_type
    contains
        procedure, public, pass(this) :: get_repeated_descriptor
        !* 記述子反復数と結合されたデータ編集記述子を返す.
        procedure, public, pass(this) :: r_get_repeated_descriptor
        !* 記述子反復数と結合されたデータ編集記述子を返す.
        generic :: operator(*) => get_repeated_descriptor, r_get_repeated_descriptor
    end type data_edit_descriptor_type

    !>ユーザ定義コンストラクタをビルトインコンストラクタと同じ名前
    !>data_edit_descriptor_typeで呼べるようにするための総称名．
    interface data_edit_descriptor_type
        procedure :: construct_data_edit_descriptor
    end interface

    !>ユーザ定義コンストラクタをビルトインコンストラクタと同じ名前
    !>datで呼べるようにするための総称名．
    !>[[character_string_edit_descriptor_type]]がコンストラクタ
    !>strを持つことに合わせるために定義されている．
    interface dat
        procedure :: construct_data_edit_descriptor
    end interface

contains
    !>文字列として渡された編集記述子を用いて，
    !>data_edit_descriptor_typeインスタンスを生成して返す．
    pure function construct_data_edit_descriptor(descriptor) result(new_edit_descriptor)
        implicit none
        character(*), intent(in) :: descriptor
            !! 編集記述子
        type(data_edit_descriptor_type) :: new_edit_descriptor
            !! 生成されるdata_edit_descriptor_typeインスタンス

        call new_edit_descriptor%set(descriptor)
    end function construct_data_edit_descriptor

    !>記述子反復数と結合されたデータ編集記述子を返す.
    !>この手続は，右側に記述子反復数がおかれる場合の乗算演算子`*`を
    !>オーバーロードする．
    pure function get_repeated_descriptor(this, repeat_count) result(repeated_edit_desc)
        use :: stdlib_strings
        implicit none
        class(data_edit_descriptor_type), intent(in) :: this
            !! 当該実体仮引数
        integer(int32), intent(in) :: repeat_count
            !! 記述子反復数
        type(data_edit_descriptor_type) :: repeated_edit_desc
            !! 繰り返し回数と結合されたデータ編集記述子

        if (repeat_count > 0) then
            call repeated_edit_desc%set(to_string(repeat_count)//this%get())
        else
            call repeated_edit_desc%set(this%get())
        end if
    end function get_repeated_descriptor

    !>記述子反復数と結合されたデータ編集記述子を返す.
    !>この手続は，左側に記述子反復数がおかれる場合の乗算演算子`*`を
    !>オーバーロードする．
    pure function r_get_repeated_descriptor(repeat_count, this) result(repeated_edit_desc)
        implicit none
        integer(int32), intent(in) :: repeat_count
            !! 記述子反復数
        class(data_edit_descriptor_type), intent(in) :: this
            !! 当該実体仮引数
        type(data_edit_descriptor_type) :: repeated_edit_desc
            !! 繰り返し回数と結合されたデータ編集記述子

        repeated_edit_desc = this%get_repeated_descriptor(repeat_count)
    end function r_get_repeated_descriptor
end module fed_editDescriptor_data
