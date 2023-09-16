module fed_editDescriptor_data_integer_octal
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor_data_integer
    implicit none
    private
    public :: int_oct

    character(*), private, parameter :: octal_int_edit_descriptor_symbol = "O"
        !! O形編集記述子に用いられる英字定数

    !>8進の整数型編集記述子を取り扱う派生型．
    type, public, extends(integer_edit_descriptor_type) :: octal_integer_edit_descriptor_type
    end type octal_integer_edit_descriptor_type

    interface int_oct
        procedure :: construct_octal_int_descriptor_w_width_pad
        procedure :: construct_octal_int_descriptor
        procedure :: construct_octal_int_descriptor_w_width
    end interface

contains
    !>octal_integer_edit_descriptor_typeインスタンスを生成して返す．
    !>欄幅には`width`，先行ゼロを付ける桁数には`zero_padding_digit`
    !>が用いられる．
    pure function construct_octal_int_descriptor_w_width_pad(width, zero_padding_digit) result(new_oct_desc)
        implicit none
        integer(int32), intent(in) :: width
            !! 欄幅(>=0)
        integer(int32), intent(in) :: zero_padding_digit
            !! 先行ゼロを付ける表示桁数(>=0, <=width)
        type(octal_integer_edit_descriptor_type) :: new_oct_desc
            !! 生成されるインスタンス

        call new_oct_desc%set(octal_int_edit_descriptor_symbol &
                              //int_spec(width, zero_padding_digit))
    end function construct_octal_int_descriptor_w_width_pad

    !>octal_integer_edit_descriptor_typeインスタンスを生成して返す．
    !>欄幅には`width`が用いられる．
    !>欄幅に相当する桁数に先行ゼロが付けられる．
    pure function construct_octal_int_descriptor_w_width(width) result(new_oct_desc)
        implicit none
        integer(int32), intent(in) :: width
            !! 欄幅(>=0)
        type(octal_integer_edit_descriptor_type) :: new_oct_desc
            !! 生成されるインスタンス

        new_oct_desc = construct_octal_int_descriptor_w_width_pad(width, width)
    end function construct_octal_int_descriptor_w_width

    !>octal_integer_edit_descriptor_typeインスタンスを生成して返す．
    !>欄幅を指定せず，表示される幅を処理系に委ねる．
    pure function construct_octal_int_descriptor() result(new_oct_desc)
        implicit none
        type(octal_integer_edit_descriptor_type) :: new_oct_desc
            !! 生成されるインスタンス

        new_oct_desc = construct_octal_int_descriptor_w_width_pad(0, 0)
    end function construct_octal_int_descriptor
end module fed_editDescriptor_data_integer_octal
