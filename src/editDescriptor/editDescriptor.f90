module fed_editDescriptor
    implicit none
    private

    !>編集記述子を取り扱う派生型．
    !>この型は，具体的な編集記述子
    !>（データ編集記述子，制御編集記述子，文字列編集記述子）を取り扱う型
    !>を定義するための祖先型として定義されている．
    !>原則としてこの型のインスタンスは生成されないが，
    !>書式反復数が付いた書式項目並びは，この型が格納する．
    type, public :: edit_descriptor_type
        character(:), private, allocatable :: desc
            !! 編集記述子
    contains
        procedure, public, pass :: set
        !* 編集記述子を設定
        procedure, public, pass :: get
        !* 編集記述子を文字列で返却
        procedure, public, pass :: destruct
        !* 編集記述子を破棄
        final :: finalize
        !* 後始末
    end type edit_descriptor_type

    !>ユーザ定義コンストラクタをビルトインコンストラクタと同じ名前
    !>edit_descriptor_typeで呼べるようにするための総称名．
    !>edit_descriptor_type自体は型の定義においてpublic指定されているため
    !>この総称名の公開指定は不要．
    interface edit_descriptor_type
        procedure :: construct_edit_descriptor
    end interface
contains
    !>文字列として渡された編集記述子を用いて，
    !>edit_descriptor_typeインスタンスを生成して返す．
    pure function construct_edit_descriptor(descriptor) result(new_edit_descriptor)
        implicit none
        character(*), intent(in) :: descriptor
            !! 編集記述子
        type(edit_descriptor_type) :: new_edit_descriptor
            !! 生成されるedit_descriptor_typeインスタンス

        call new_edit_descriptor%set(descriptor)
    end function construct_edit_descriptor

    !>文字列として渡された編集記述子を設定する．
    pure subroutine set(this, edit_descriptor)
        implicit none
        class(edit_descriptor_type), intent(inout) :: this
            !! 当該実体仮引数
        character(*), intent(in) :: edit_descriptor
            !! 編集記述子

        this%desc = edit_descriptor
    end subroutine set

    !>編集記述子を文字列で返す．
    !>編集記述子が設定されていない場合，長さ0の文字列`""`を返す．
    pure function get(this) result(edit_descriptor)
        implicit none
        class(edit_descriptor_type), intent(in) :: this
            !! 当該実体仮引数
        character(:), allocatable :: edit_descriptor
            !! 編集記述子

        if (allocated(this%desc)) then
            edit_descriptor = this%desc
        else
            edit_descriptor = ""
        end if
    end function get

    !>編集記述子が割り付けられている場合，破棄する．
    pure subroutine destruct(this)
        implicit none
        class(edit_descriptor_type), intent(inout) :: this
            !! 当該実体仮引数

        if (allocated(this%desc)) &
            deallocate (this%desc)
    end subroutine destruct

    !>インスタンスを後始末する．
    pure subroutine finalize(this)
        implicit none
        type(edit_descriptor_type), intent(inout) :: this
            !! 当該実体仮引数

        call this%destruct()
    end subroutine finalize
end module fed_editDescriptor
