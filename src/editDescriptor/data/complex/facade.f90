module fed_editDescriptor_data_complex_facade
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    use :: fed_editDescriptor_data_real_facade
    use :: fed_editDescriptor_data_complex
    use :: fed_editDescriptor_data_complex_exponential
    use :: fed_editDescriptor_data_complex_engineering
    use :: fed_editDescriptor_data_complex_scientific
    use :: fed_editDescriptor_data_complex_standard
    use :: enumul
    implicit none
    private
    public :: complex

    interface complex
        procedure :: complex_form_w_width_decimal_exp
        procedure :: complex_form
    end interface

contains
    !>指定の形式の複素数型編集記述子のインスタンスを生成して返す．
    !>欄幅には`width`，小数部の桁数には`decimal_place_digits`が
    !>用いられる．
    !>指定された形式が指数形式，科学形式，工学形式の
    !>いずれでもない場合は標準形式の複素数型編集記述子のインスタンスが生成される．
    function complex_form_w_width_decimal_exp(form, width, decimal_place_digits, exponent_digits) result(new_complex_spec)
        implicit none
        type(real_form_type), intent(in) :: form
            !! 表示形式
        integer(int32), intent(in) :: width
            !! 欄幅
        integer(int32), intent(in) :: decimal_place_digits
            !! 小数部の桁数
        integer(int32), intent(in) :: exponent_digits
            !! 指数部の桁数
        class(complex_edit_descriptor_type), allocatable :: new_complex_spec
            !! 生成されるインスタンス

        select case (form%enum)
        case (exp_form%enum)
            allocate (new_complex_spec, source=complex_exp(width, decimal_place_digits, exponent_digits))

        case (sci_form%enum)
            allocate (new_complex_spec, source=complex_sci(width, decimal_place_digits, exponent_digits))

        case (eng_form%enum)
            allocate (new_complex_spec, source=complex_eng(width, decimal_place_digits, exponent_digits))

        case default
            allocate (new_complex_spec, source=complex())

        end select
    end function complex_form_w_width_decimal_exp

    !>指定の形式の複素数型編集記述子のインスタンスを生成して返す．
    !>指定された形式が指数形式，科学形式，工学形式の
    !>いずれでもない場合は標準形式の複素数型編集記述子のインスタンスが生成される．
    function complex_form(form) result(new_complex_spec)
        implicit none
        type(real_form_type), intent(in) :: form
            !! 表示形式
        class(complex_edit_descriptor_type), allocatable :: new_complex_spec
            !! 生成されるインスタンス

        select case (form%enum)
        case (exp_form%enum)
            allocate (new_complex_spec, source=complex_exp())

        case (sci_form%enum)
            allocate (new_complex_spec, source=complex_sci())

        case (eng_form%enum)
            allocate (new_complex_spec, source=complex_eng())

        case default
            allocate (new_complex_spec, source=complex())

        end select
    end function complex_form
end module fed_editDescriptor_data_complex_facade
