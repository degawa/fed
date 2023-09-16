module fed_editDescriptor_data_complex_engineering
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor_data_real_engineering
    use :: fed_editDescriptor_data_complex
    implicit none
    private
    public :: complex_eng

    !>指数形式の複素数の編集記述子を取り扱う派生型．
    type, public, extends(complex_edit_descriptor_type) :: complex_engineering_edit_descriptor_type
    end type complex_engineering_edit_descriptor_type

    interface complex_eng
        procedure :: construct_engineering_complex_spec_w_width_decimal_exp
        procedure :: construct_engineering_complex_spec
    end interface

contains
    !>complex_engineering_edit_descriptor_typeインスタンスを生成して返す．
    !>欄幅には`width`，小数部の桁数には`decimal_place_digits`，
    !>指数部の桁数には`exponent_digits`が用いられる．
    pure function construct_engineering_complex_spec_w_width_decimal_exp &
        (width, decimal_place_digits, exponent_digits) result(new_eng_desc)
        implicit none
        integer(int32), intent(in) :: width
            !! 欄幅
        integer(int32), intent(in) :: decimal_place_digits
            !! 小数部の桁数
        integer(int32), intent(in) :: exponent_digits
            !! 指数部の桁数
        type(complex_engineering_edit_descriptor_type) :: new_eng_desc
            !!　生成されるインスタンス

        ! 実部と虚部の編集を行う実数型編集記述子
        type(real_engineering_edit_descriptor_type) :: real_desc
        real_desc = real_eng(width, decimal_place_digits, exponent_digits)

        call new_eng_desc%set(complex_spec(real_desc))
    end function construct_engineering_complex_spec_w_width_decimal_exp

    !>complex_engineering_edit_descriptor_typeインスタンスを生成して返す．
    pure function construct_engineering_complex_spec() result(new_eng_desc)
        implicit none
        type(complex_engineering_edit_descriptor_type) :: new_eng_desc
            !!　生成されるインスタンス

        ! 実部と虚部の編集を行う実数型編集記述子
        type(real_engineering_edit_descriptor_type) :: real_desc
        real_desc = real_eng()

        call new_eng_desc%set(complex_spec(real_desc))
    end function construct_engineering_complex_spec
end module fed_editDescriptor_data_complex_engineering
