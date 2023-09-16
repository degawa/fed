module fed_editDescriptor_data_real_exponential
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor_data_real
    implicit none
    private
    public :: real_exp

    character(*), private, parameter :: real_exponential_edit_descriptor_symbol = "E"
        !! E形（指数形式）編集記述子に用いられる英字定数

    !>指数形式の実数の編集記述子を取り扱う派生型．
    type, public, extends(real_edit_descriptor_type) :: real_exponential_edit_descriptor_type
    end type real_exponential_edit_descriptor_type

    interface real_exp
        procedure :: construct_exponential_real_spec_w_width_decimal_exp
        procedure :: construct_exponential_real_spec
    end interface

contains
    !>real_exponential_edit_descriptor_typeインスタンスを生成して返す．
    !>欄幅には`width`，小数部の桁数には`decimal_place_digits`，
    !>指数部の桁数には`exponent_digits`が用いられる．
    pure function construct_exponential_real_spec_w_width_decimal_exp &
        (width, decimal_place_digits, exponent_digits) result(new_exp_spec)
        implicit none
        integer(int32), intent(in) :: width
            !! 欄幅
        integer(int32), intent(in) :: decimal_place_digits
            !! 小数部の桁数
        integer(int32), intent(in) :: exponent_digits
            !! 指数部の桁数
        type(real_exponential_edit_descriptor_type) :: new_exp_spec
            !!　生成されるインスタンス

        call new_exp_spec%set(real_exponential_edit_descriptor_symbol// &
                              real_spec(width, decimal_place_digits, exponent_digits, "-.E+"))
    end function construct_exponential_real_spec_w_width_decimal_exp

    !>real_exponential_edit_descriptor_typeインスタンスを生成して返す．
    pure function construct_exponential_real_spec() result(new_exp_spec)
        implicit none
        type(real_exponential_edit_descriptor_type) :: new_exp_spec
            !!　生成されるインスタンス

        new_exp_spec = construct_exponential_real_spec_w_width_decimal_exp(0, 0, 0)
    end function construct_exponential_real_spec
end module fed_editDescriptor_data_real_exponential
