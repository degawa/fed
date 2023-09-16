module fed_editDescriptor_data_real_engineering
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor_data_real
    implicit none
    private
    public :: real_eng

    character(*), private, parameter :: real_engineering_edit_descriptor_symbol = "EN"
       !! EN形（工学形式）編集記述子に用いられる英字定数

    !>工学形式の実数の編集記述子を取り扱う派生型．
    type, public, extends(real_edit_descriptor_type) :: real_engineering_edit_descriptor_type
    end type real_engineering_edit_descriptor_type

    interface real_eng
        procedure :: construct_engineering_real_spec_w_width_decimal_exp
        procedure :: construct_engineering_real_spec
    end interface

contains
    !>real_engineering_edit_descriptor_typeインスタンスを生成して返す．
    !>欄幅には`width`，小数部の桁数には`decimal_place_digits`，
    !>指数部の桁数には`exponent_digits`が用いられる．
    pure function construct_engineering_real_spec_w_width_decimal_exp &
        (width, decimal_place_digits, exponent_digits) result(new_eng_spec)
        implicit none
        integer(int32), intent(in) :: width
            !! 欄幅
        integer(int32), intent(in) :: decimal_place_digits
            !! 小数部の桁数
        integer(int32), intent(in) :: exponent_digits
            !! 指数部の桁数
        type(real_engineering_edit_descriptor_type) :: new_eng_spec
            !!　生成されるインスタンス

        call new_eng_spec%set(real_engineering_edit_descriptor_symbol// &
                              real_spec(width, decimal_place_digits, exponent_digits, "-000.E+"))
    end function construct_engineering_real_spec_w_width_decimal_exp

    !>real_engineering_edit_descriptor_typeインスタンスを生成して返す．
    pure function construct_engineering_real_spec() result(new_eng_spec)
        implicit none
        type(real_engineering_edit_descriptor_type) :: new_eng_spec
            !!　生成されるインスタンス

        ! 標準値となるように，不正な値を与える
        new_eng_spec = construct_engineering_real_spec_w_width_decimal_exp(0, -1, -1)
    end function construct_engineering_real_spec
end module fed_editDescriptor_data_real_engineering
