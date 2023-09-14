module fed_editDescriptor_data_real
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor_data
    implicit none
    private
    public :: general_data_edit_descriptor_symbol

    integer(int32), public, parameter :: default_digits_of_decimal_place = 7
        !! 実数型および複素数形編集における，小数点以下の桁数の標準値

    !>実数型編集記述子を取り扱う派生型．
    !>この型は，標準形式，指数形式などの各記述子を定義するための
    !>祖先型として定義されている．
    type, public, extends(data_edit_descriptor_type) :: real_edit_descriptor_type
    end type real_edit_descriptor_type
end module fed_editDescriptor_data_real
