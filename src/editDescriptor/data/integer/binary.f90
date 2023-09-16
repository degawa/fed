module fed_editDescriptor_data_integer_binary
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor_data_integer
    implicit none
    private
    public :: int_bin

    character(*), private, parameter :: binary_int_edit_descriptor_symbol = "B"
        !! B形編集記述子に用いられる英字定数

    !>2進の整数型編集記述子を取り扱う派生型．
    type, public, extends(integer_edit_descriptor_type) :: binary_integer_edit_descriptor_type
    end type binary_integer_edit_descriptor_type

    interface int_bin
        procedure :: construct_binary_int_descriptor_w_width_pad
        procedure :: construct_binary_int_descriptor
        procedure :: construct_binary_int_descriptor_w_width
    end interface

contains
    !>binary_integer_edit_descriptor_typeインスタンスを生成して返す．
    !>欄幅には`width`，先行ゼロを付ける桁数には`zero_padding_digit`
    !>が用いられる．
    pure function construct_binary_int_descriptor_w_width_pad(width, zero_padding_digit) result(new_bin_desc)
        implicit none
        integer(int32), intent(in) :: width
            !! 欄幅(>=0)
        integer(int32), intent(in) :: zero_padding_digit
            !! 先行ゼロを付ける表示桁数(>=0, <=width)
        type(binary_integer_edit_descriptor_type) :: new_bin_desc
            !! 生成されるインスタンス

        call new_bin_desc%set(binary_int_edit_descriptor_symbol &
                              //int_spec(width, zero_padding_digit))
    end function construct_binary_int_descriptor_w_width_pad

    !>binary_integer_edit_descriptor_typeインスタンスを生成して返す．
    !>欄幅には`width`が用いられる．
    !>欄幅に相当する桁数に先行ゼロが付けられる．
    pure function construct_binary_int_descriptor_w_width(width) result(new_bin_desc)
        implicit none
        integer(int32), intent(in) :: width
            !! 欄幅(>=0)
        type(binary_integer_edit_descriptor_type) :: new_bin_desc
            !! 生成されるインスタンス

        new_bin_desc = construct_binary_int_descriptor_w_width_pad(width, width)
    end function construct_binary_int_descriptor_w_width

    !>binary_integer_edit_descriptor_typeインスタンスを生成して返す．
    !>欄幅を指定せず，表示される幅を処理系に委ねる．
    pure function construct_binary_int_descriptor() result(new_bin_desc)
        implicit none
        type(binary_integer_edit_descriptor_type) :: new_bin_desc

        new_bin_desc = construct_binary_int_descriptor_w_width_pad(0, 0)
    end function construct_binary_int_descriptor
end module fed_editDescriptor_data_integer_binary
