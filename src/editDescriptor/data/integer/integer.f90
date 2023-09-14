module fed_editDescriptor_data_integer
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor_data
    implicit none
    private
    public :: int_spec

    !>整数型編集記述子を取り扱う派生型．
    !>この型は，2, 8, 10, 16進数の各編集を
    !>定義するための祖先型として定義されている．
    type, public, extends(data_edit_descriptor_type) :: integer_edit_descriptor_type
    end type integer_edit_descriptor_type

    interface int_spec
        procedure :: construct_integer_spec
    end interface
contains
    !>整数型編集記述子の欄幅と先行ゼロを付ける桁数を文字列として返す．
    !>欄幅が0未満の場合は欄幅を0とし，先行ゼロを付ける桁数は考慮されない．
    pure function construct_integer_spec(width, zero_padding_digits) result(integer_spec)
        use :: stdlib_strings
        implicit none
        integer(int32), intent(in) :: width
            !! 欄幅(>=0)
        integer(int32), intent(in) :: zero_padding_digits
            !! 先行ゼロを付ける表示桁数(>=0)
        character(:), allocatable :: integer_spec
            !! 整数型編集記述子の表示指定

        integer(int32) :: w, m

        w = width
        m = zero_padding_digits

        if (w < 0) w = 0 ! invalid w
        if (m < 0) m = 0 ! invalid m
        if (m > w) m = w ! suppress m to w

        if (m /= 0) then
            integer_spec = to_string(w)//"."//to_string(m)
        else
            integer_spec = to_string(w)
        end if
    end function construct_integer_spec
end module fed_editDescriptor_data_integer
