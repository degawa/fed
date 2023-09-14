module fed_editDescriptor_data_integer_decimal
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor_data_integer
    implicit none
    private
    public :: int

    character(*), private, parameter :: decimal_int_edit_descriptor_symbol = "I"
        !! I形編集記述子に用いられる英字定数

    !>10進の整数型編集記述子を取り扱う派生型．
    type, public, extends(integer_edit_descriptor_type) :: decimal_integer_edit_descriptor_type
    end type decimal_integer_edit_descriptor_type

    interface int
        procedure :: construct_decimal_int_descriptor_w_digit_pad
        procedure :: construct_decimal_int_descriptor
        procedure :: construct_decimal_int_descriptor_w_digit
    end interface
contains
    !>decimal_integer_edit_descriptor_typeインスタンスを生成して返す．
    !>欄幅には`width`，先行ゼロを付ける桁数には`zero_padding_digit`
    !>が用いられる．
    pure function construct_decimal_int_descriptor_w_digit_pad(width, zero_padding_digit) result(new_int_desc)
        implicit none
        integer(int32), intent(in) :: width
            !! 欄幅(>=0)
        integer(int32), intent(in) :: zero_padding_digit
            !! 先行ゼロを付ける表示桁数(>=0, <=width)
        type(decimal_integer_edit_descriptor_type) :: new_int_desc
            !! 生成されるインスタンス

        call new_int_desc%set(decimal_int_edit_descriptor_symbol &
                              //int_spec(width, zero_padding_digit))
    end function construct_decimal_int_descriptor_w_digit_pad

    !>decimal_integer_edit_descriptor_typeインスタンスを生成して返す．
    !>欄幅には`width`が用いられる．
    pure function construct_decimal_int_descriptor_w_digit(width) result(new_int_desc)
        implicit none
        integer(int32), intent(in) :: width
            !! 欄幅(>=0)
        type(decimal_integer_edit_descriptor_type) :: new_int_desc
            !! 生成されるインスタンス

        ! 先行ゼロを付ける桁数は0とする
        new_int_desc = construct_decimal_int_descriptor_w_digit_pad(width, 0)
    end function construct_decimal_int_descriptor_w_digit

    !>decimal_integer_edit_descriptor_typeインスタンスを生成して返す．
    !>欄幅を指定せず，表示される幅を処理系に委ねる．
    pure function construct_decimal_int_descriptor() result(new_int_desc)
        implicit none
        type(decimal_integer_edit_descriptor_type) :: new_int_desc
            !! 生成されるインスタンス

        ! 欄幅に0を指定することで，"I0"を作成する
        new_int_desc = construct_decimal_int_descriptor_w_digit_pad(0, 0)
    end function construct_decimal_int_descriptor
end module fed_editDescriptor_data_integer_decimal
