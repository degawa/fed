module fed_editDescriptor_data_complex
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor_data
    implicit none
    private
    public :: complex_spec

    character(*), public, parameter :: complex_edit_descriptor_bracket_open = '"(",'
        !! 複素数の実部と虚部の組をくくるための括弧の開き部分
    character(*), public, parameter :: complex_edit_descriptor_bracket_close = ',")"'
        !! 複素数の実部と虚部の組をくくるための括弧の閉じ部分
    character(*), public, parameter :: complex_edit_descriptor_separator = ',",",'
        !! 複素数の実部と虚部の区切り文字

    !>複素数型編集記述子を取り扱う派生型．
    !>この型は，標準形式，指数形式などの各記述子を定義するための
    !>祖先型として定義されている．
    !>
    !>@note
    !>Fortran規格では，単一の編集記述子で実部と虚部を同時に表示できるような
    !>複素数形編集記述子は定義されていない．fedでは複数の編集記述子や
    !>文字列編集記述子を組み合わせた，"(",G0,",",G0,")"のような形式の
    !>文字列を複素数形編集記述子と称している．
    !>実体は書式項目並びであるが，便宜上編集記述子として取り扱う．
    !>
    !>それに伴って，記述子反復数を設定する乗算演算を変更する必要があるため，
    !>反復数をもつ書式項目を生成する手続によってオーバーロードされる．
    !@endnote
    !>
    type, public, extends(data_edit_descriptor_type) :: complex_edit_descriptor_type
    contains
        procedure, public, pass(this) :: get_repeated_descriptor
        !* 記述子反復数が設定された複素数型編集記述子を返す.
        procedure, public, pass(this) :: r_get_repeated_descriptor
        !* 記述子反復数が設定された複素数型編集記述子を返す.
    end type complex_edit_descriptor_type

    interface complex_spec
        procedure :: construct_complex_spec
    end interface

contains
    !>記述子反復数と結合されたデータ編集記述子を返す.
    !>この手続は，右側に記述子反復数がおかれる場合の乗算演算子`*`を
    !>オーバーロードする．
    pure function get_repeated_descriptor(this, repeat_count) result(repeated_complex_desc)
        use :: stdlib_strings
        implicit none
        class(complex_edit_descriptor_type), intent(in) :: this
            !! 当該実体仮引数
        integer(int32), intent(in) :: repeat_count
            !! 記述子反復数
        type(data_edit_descriptor_type) :: repeated_complex_desc
            !! 繰り返し回数と結合された複素数形編集記述子

        if (repeat_count > 0) then
            call repeated_complex_desc%set(to_string(repeat_count) &
                                           //"("//this%get()//")")
        else
            call repeated_complex_desc%set(this%get())
        end if
    end function get_repeated_descriptor

    !>記述子反復数と結合されたデータ編集記述子を返す.
    !>この手続は，左側に記述子反復数がおかれる場合の乗算演算子`*`を
    !>オーバーロードする．
    pure function r_get_repeated_descriptor(repeat_count, this) result(repeated_complex_desc)
        implicit none
        integer(int32), intent(in) :: repeat_count
            !! 記述子反復数
        class(complex_edit_descriptor_type), intent(in) :: this
            !! 当該実体仮引数
        type(data_edit_descriptor_type) :: repeated_complex_desc
            !! 繰り返し回数と結合された複素数形編集記述子

        repeated_complex_desc = this%get_repeated_descriptor(repeat_count)
    end function r_get_repeated_descriptor

    !>複素数の数値入力欄を作成して文字列として返す．
    !>実数型編集記述子と，括弧および区切り文字を結合する．
    pure function construct_complex_spec(real_desc) result(complex_edit_descriptor)
        use :: fed_editDescriptor_data_real
        implicit none
        class(real_edit_descriptor_type), intent(in) :: real_desc
            !! 実数型編集記述子
        character(:), allocatable :: complex_edit_descriptor
            !! 生成される数値入力欄

        complex_edit_descriptor = &
            complex_edit_descriptor_bracket_open & ! (
            //real_desc%get() & ! 実部
            //complex_edit_descriptor_separator & ! ,
            //real_desc%get() & ! 虚部
            //complex_edit_descriptor_bracket_close ! )
    end function construct_complex_spec
end module fed_editDescriptor_data_complex
