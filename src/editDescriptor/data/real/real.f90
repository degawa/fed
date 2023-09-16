module fed_editDescriptor_data_real
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor_data
    implicit none
    private
    public :: general_data_edit_descriptor_symbol
    public :: real_spec

    integer(int32), public, parameter :: default_digits_of_decimal_place = 7
        !! 実数型および複素数形編集における，小数点以下の桁数の標準値
    integer(int32), public, parameter :: default_digits_of_exponent = 1
        !! 実数型および複素数形編集における，指数の桁数の標準値
    character(*), public, parameter :: real_edit_descriptor_exponent_symbol = "E"
        !! 指数の桁数指定に用いられる英字定数

    !>実数型編集記述子を取り扱う派生型．
    !>この型は，標準形式，指数形式などの各記述子を定義するための
    !>祖先型として定義されている．
    type, public, extends(data_edit_descriptor_type) :: real_edit_descriptor_type
    end type real_edit_descriptor_type

    interface real_spec
        procedure :: construct_real_exponential_form_spce
    end interface

contains
    !>E形（指数形式）の編集記述子の数値入力欄を作成して文字列として返す．
    !>
    !>`width`が0以下の場合，小数部の桁数+指数部の桁数+その他文字が表示できる
    !>最小の整数に置き換えられる．
    !>`decimal_place_digits`が0未満の場合，小数部の桁数の標準値（=7）
    !>に置き換えられる．
    !>指数部の桁数が0の場合，指数部の桁数指定が省略された数値入力欄が
    !>生成される．
    pure function construct_real_exponential_form_spce &
        (width, decimal_place_digits, exponent_digits, additional_chars) result(real_exp_spec)
        use :: stdlib_strings
        implicit none
        integer(int32), intent(in) :: width
            !! 外部欄
        integer(int32), intent(in) :: decimal_place_digits
            !! 小数部の桁数
        integer(int32), intent(in) :: exponent_digits
            !! 指数部の桁数
        character(*), intent(in) :: additional_chars
            !! 数値内に現れる数字以外の文字
            !! e.g. 小数点，符号，指数記号等
        character(:), allocatable :: real_exp_spec
            !! 生成される数値入力欄

        integer(int32) :: w, d, e, minimum_width
        w = width
        d = decimal_place_digits
        e = exponent_digits

        if (w < 0) w = 0
        if (d < 0) d = default_digits_of_decimal_place
        if (e < 0) e = default_digits_of_exponent

        minimum_width = d + e + len(additional_chars)
        if (w < minimum_width) then
            w = minimum_width
        end if

        if (e == 0) then
            real_exp_spec = to_string(w)//"."//to_string(d)
        else
            real_exp_spec = to_string(w)//"."//to_string(d)// &
                real_edit_descriptor_exponent_symbol//to_string(e)
        end if
    end function construct_real_exponential_form_spce
end module fed_editDescriptor_data_real
