module fed_editDescriptor_data_complex
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
    type, public, extends(data_edit_descriptor_type) :: complex_edit_descriptor_type
    end type complex_edit_descriptor_type

    interface complex_spec
        procedure :: construct_complex_spec
    end interface

contains
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
