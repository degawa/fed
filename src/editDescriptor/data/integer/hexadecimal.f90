module fed_editDescriptor_data_integer_hexadecimal
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor_data_integer
    implicit none
    private
    public :: int_hex

    character(*), private, parameter :: hexadecimal_int_edit_descriptor_symbol = "Z"
        !! Z形編集記述子に用いられる英字定数

    !>16進の整数型編集記述子を取り扱う派生型．
    type, public, extends(integer_edit_descriptor_type) :: hexadecimal_integer_edit_descriptor_type
    end type hexadecimal_integer_edit_descriptor_type

    interface int_hex
        procedure :: construct_hexadecimal_int_descriptor_w_width_pad
        procedure :: construct_hexadecimal_int_descriptor
        procedure :: construct_hexadecimal_int_descriptor_w_width
    end interface

contains
    !>hexadecimal_integer_edit_descriptor_typeインスタンスを生成して返す．
    !>欄幅には`width`，先行ゼロを付ける桁数には`zero_padding_digit`
    !>が用いられる．
    pure function construct_hexadecimal_int_descriptor_w_width_pad(width, zero_padding_digit) result(new_hex_desc)
        implicit none
        integer(int32), intent(in) :: width
            !! 欄幅(>=0)
        integer(int32), intent(in) :: zero_padding_digit
            !! 先行ゼロを付ける表示桁数(>=0, <=width)
        type(hexadecimal_integer_edit_descriptor_type) :: new_hex_desc
            !! 生成されるインスタンス

        call new_hex_desc%set(hexadecimal_int_edit_descriptor_symbol &
                              //int_spec(width, zero_padding_digit))
    end function construct_hexadecimal_int_descriptor_w_width_pad

    !>hexadecimal_integer_edit_descriptor_typeインスタンスを生成して返す．
    !>欄幅には`width`が用いられる．
    !>欄幅に相当する桁数に先行ゼロが付けられる．
    pure function construct_hexadecimal_int_descriptor_w_width(width) result(new_hex_desc)
        implicit none
        integer(int32), intent(in) :: width
            !! 欄幅(>=0)
        type(hexadecimal_integer_edit_descriptor_type) :: new_hex_desc
            !! 生成されるインスタンス

        new_hex_desc = construct_hexadecimal_int_descriptor_w_width_pad(width, width)
    end function construct_hexadecimal_int_descriptor_w_width

    !>hexadecimal_integer_edit_descriptor_typeインスタンスを生成して返す．
    !>欄幅を指定せず，表示される幅を処理系に委ねる．
    pure function construct_hexadecimal_int_descriptor() result(new_hex_desc)
        implicit none
        type(hexadecimal_integer_edit_descriptor_type) :: new_hex_desc
            !! 生成されるインスタンス

        new_hex_desc = construct_hexadecimal_int_descriptor_w_width_pad(0, 0)
    end function construct_hexadecimal_int_descriptor
end module fed_editDescriptor_data_integer_hexadecimal
