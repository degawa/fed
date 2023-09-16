module fed_editDescriptor_data_real_facade
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    use :: fed_editDescriptor_data_real
    use :: fed_editDescriptor_data_real_exponential
    use :: fed_editDescriptor_data_real_engineering
    use :: fed_editDescriptor_data_real_scientific
    use :: fed_editDescriptor_data_real_hexadecimalSignificand
    use :: fed_editDescriptor_data_real_standard
    use :: enumul
    implicit none
    private
    public :: real

    !>実数の表示形式を表す列挙型
    type, private, extends(enum_atype) :: real_form_type
    end type real_form_type

    enum, bind(c)
        enumerator :: Form_Exponential
        enumerator :: Form_Scientific
        enumerator :: Form_Engineering
        enumerator :: Form_Hexadecimal_Significand
    end enum

    type(real_form_type), public, parameter :: exp_form = real_form_type(Form_Exponential)
        !! 指数形式の実数を表す列挙子
    type(real_form_type), public, parameter :: sci_form = real_form_type(Form_Scientific)
        !! 科学形式の実数を表す列挙子
    type(real_form_type), public, parameter :: eng_form = real_form_type(Form_Engineering)
        !! 工学形式の実数を表す列挙子

    interface real
        procedure :: real_form_w_width_decimal_exp
        procedure :: real_form
    end interface

contains
    !>指定の形式の実数型編集記述子のインスタンスを生成して返す．
    !>欄幅には`width`，小数部の桁数には`decimal_place_digits`が
    !>用いられる．
    !>指定された形式が指数形式，科学形式，工学形式，16進数形式の
    !>いずれでもない場合は標準形式の実数型編集記述子のインスタンスが生成される．
    function real_form_w_width_decimal_exp(form, width, decimal_place_digits, exponent_digits) result(new_real_spec)
        implicit none
        type(real_form_type), intent(in) :: form
            !! 表示形式
        integer(int32), intent(in) :: width
            !! 欄幅
        integer(int32), intent(in) :: decimal_place_digits
            !! 小数部の桁数
        integer(int32), intent(in) :: exponent_digits
            !! 指数部の桁数
        class(real_edit_descriptor_type), allocatable :: new_real_spec
            !! 生成されるインスタンス

        select case (form%enum)
        case (exp_form%enum)
            allocate (new_real_spec, source=real_exp(width, decimal_place_digits, exponent_digits))

        case (sci_form%enum)
            allocate (new_real_spec, source=real_sci(width, decimal_place_digits, exponent_digits))

        case (eng_form%enum)
            allocate (new_real_spec, source=real_eng(width, decimal_place_digits, exponent_digits))

        case default
            allocate (new_real_spec, source=real())

        end select
    end function real_form_w_width_decimal_exp

    !>指定の形式の実数型編集記述子のインスタンスを生成して返す．
    !>指定された形式が指数形式，科学形式，工学形式，16進数形式の
    !>いずれでもない場合は標準形式の実数型編集記述子のインスタンスが生成される．
    function real_form(form) result(new_real_spec)
        implicit none
        type(real_form_type), intent(in) :: form
            !! 表示形式
        class(real_edit_descriptor_type), allocatable :: new_real_spec
            !! 生成されるインスタンス

        select case (form%enum)
        case (exp_form%enum)
            allocate (new_real_spec, source=real_exp())

        case (sci_form%enum)
            allocate (new_real_spec, source=real_sci())

        case (eng_form%enum)
            allocate (new_real_spec, source=real_eng())

        case default
            allocate (new_real_spec, source=real())

        end select
    end function real_form
end module fed_editDescriptor_data_real_facade
