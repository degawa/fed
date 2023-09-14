module fed_editDescriptor_data_real_standard
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor_data_real
    implicit none
    private
    public :: real

    character(*), private, parameter :: real_starndard_edit_descriptor_symbol = "F"
        !! F形（標準形式）編集記述子に用いられる英字定数

    !>標準形式の実数の編集記述子を取り扱う派生型．
    type, public, extends(real_edit_descriptor_type) :: real_standard_edit_descriptor_type
    end type real_standard_edit_descriptor_type

    interface real
        procedure :: construct_standard_real_descriptor_w_width_digit
        procedure :: construct_standard_real_descriptor
    end interface

contains
    !>real_standard_edit_descriptor_typeインスタンスを生成して返す．
    !>欄幅には`width`，小数部の桁数には`decimal_place_digits`が
    !>用いられる．
    pure function construct_standard_real_descriptor_w_width_digit(width, decimal_place_digits) result(new_real_desc)
        use :: stdlib_strings
        implicit none
        integer(int32), intent(in) :: width
            !! 欄幅
        integer(int32), intent(in) :: decimal_place_digits
            !! 小数部の桁数
        type(real_standard_edit_descriptor_type) :: new_real_desc
            !!　生成されるインスタンス

        integer(int32) :: w, d, minimum_width
        w = width
        d = decimal_place_digits

        ! wが0以下の場合はG0を返す
        if (w <= 0) then
            new_real_desc = real()
            return
        end if

        ! dが0未満の場合は，標準の桁数を用いる
        if (d < 0) d = default_digits_of_decimal_place

        ! wが最小幅（d+符号+小数点）未満の場合は最小幅に置き換える
        minimum_width = d + len("-.") !spaces for the sign and the decimal point
        if (w < minimum_width) w = minimum_width

        call new_real_desc%set(real_starndard_edit_descriptor_symbol &
                               //to_string(w)//"."//to_string(d))
    end function construct_standard_real_descriptor_w_width_digit

    !>real_standard_edit_descriptor_typeインスタンスを生成して返す．
    pure function construct_standard_real_descriptor() result(new_real_desc)
        implicit none
        type(real_standard_edit_descriptor_type) :: new_real_desc
            !!　生成されるインスタンス

        call new_real_desc%set(general_data_edit_descriptor_symbol//"0")
    end function construct_standard_real_descriptor
end module fed_editDescriptor_data_real_standard
