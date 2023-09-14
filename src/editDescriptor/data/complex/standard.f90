module fed_editDescriptor_data_complex_standard
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor_data_complex
    use :: fed_editDescriptor_data_real_standard
    implicit none
    private
    public :: complex

    !>標準形式の複素数の編集記述子を取り扱う派生型．
    type, public, extends(complex_edit_descriptor_type) :: complex_standard_edit_descriptor_type
    end type complex_standard_edit_descriptor_type

    interface complex
        procedure :: construct_standard_complex_descriptor_w_width_digit
        procedure :: construct_standard_complex_descriptor
    end interface

contains
    !>complex_standard_edit_descriptor_typeインスタンスを生成して返す．
    !>欄幅には`width`，小数部の桁数には`decimal_place_digits`が
    !>用いられる．
    pure function construct_standard_complex_descriptor_w_width_digit(width, decimal_place_digits) result(new_complex_desc)
        use :: stdlib_strings
        implicit none
        integer(int32), intent(in) :: width
            !! 欄幅
        integer(int32), intent(in) :: decimal_place_digits
            !! 小数部の桁数
        type(complex_standard_edit_descriptor_type) :: new_complex_desc
            !!　生成されるインスタンス

        ! 実部と虚部の編集を行う実数型編集記述子
        type(real_standard_edit_descriptor_type) :: real_desc
        real_desc = real(width, decimal_place_digits)

        call new_complex_desc%set(complex_spec(real_desc))
    end function construct_standard_complex_descriptor_w_width_digit

    !>complex_standard_edit_descriptor_typeインスタンスを生成して返す．
    pure function construct_standard_complex_descriptor() result(new_complex_desc)
        implicit none
        type(complex_standard_edit_descriptor_type) :: new_complex_desc
            !!　生成されるインスタンス

        ! 実部と虚部の編集を行う実数型編集記述子
        type(real_standard_edit_descriptor_type) :: real_desc
        real_desc = real()

        call new_complex_desc%set(complex_spec(real_desc))
    end function construct_standard_complex_descriptor
end module fed_editDescriptor_data_complex_standard
