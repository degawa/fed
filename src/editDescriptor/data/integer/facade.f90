module fed_editDescriptor_data_integer_facade
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    use :: fed_editDescriptor_data_integer
    use :: fed_editDescriptor_data_integer_binary
    use :: fed_editDescriptor_data_integer_octal
    use :: fed_editDescriptor_data_integer_hexadecimal
    use :: fed_editDescriptor_data_integer_decimal
    use :: enumul
    implicit none
    private
    public :: int

    !>整数の基数を表す列挙型
    type, private, extends(enum_atype) :: int_form_type
    end type int_form_type

    enum, bind(c)
        enumerator :: Form_Binary
        enumerator :: Form_Octal
        enumerator :: Form_Hexadecimal
    end enum

    type(int_form_type), public, parameter :: bin_digits = int_form_type(Form_Binary)
        !! 2進整数形式を表す列挙子
    type(int_form_type), public, parameter :: oct_digits = int_form_type(Form_Octal)
        !! 8進整数形式を表す列挙子
    type(int_form_type), public, parameter :: hex_digits = int_form_type(Form_Hexadecimal)
        !! 16進整数形式を表す列挙子

    interface int
        procedure :: int_form_w_width_pad
        procedure :: int_form_w_width
        procedure :: int_form
    end interface

contains
    !>指定の基数の整数型編集記述子のインスタンスを生成して返す．
    !>欄幅には`width`，先行ゼロを付ける桁数には`zero_padding_digit`
    !>が用いられる．
    !>指定された基数が2進数，8進数，16進数のいずれでもない場合は
    !>10進数の整数型編集記述子のインスタンスが生成される．
    function int_form_w_width_pad(form, width, zero_padding_digits) result(new_int_spec)
        implicit none
        type(int_form_type), intent(in) :: form
            !! 基数の形式
        integer(int32), intent(in) :: width
            !! 欄幅(>=0)
        integer(int32), intent(in) :: zero_padding_digits
            !! 先行ゼロを付ける表示桁数(>=0, <=width)
        class(integer_edit_descriptor_type), allocatable :: new_int_spec
            !! 生成されるインスタンス

        select case (form%enum)
        case (bin_digits%enum)
            allocate (new_int_spec, source=int_bin(width, zero_padding_digits))

        case (oct_digits%enum)
            allocate (new_int_spec, source=int_oct(width, zero_padding_digits))

        case (hex_digits%enum)
            allocate (new_int_spec, source=int_hex(width, zero_padding_digits))

        case default
            allocate (new_int_spec, source=int())

        end select
    end function int_form_w_width_pad

    !>指定の基数の整数型編集記述子のインスタンスを生成して返す．
    !>欄幅には`width`が用いられる．
    !>指定された基数が2進数，8進数，16進数のいずれでもない場合は
    !>10進数の整数型編集記述子のインスタンスが生成される．
    function int_form_w_width(form, width) result(new_int_spec)
        implicit none
        type(int_form_type), intent(in) :: form
            !! 基数の形式
        integer(int32), intent(in) :: width
            !! 欄幅(>=0)
        class(integer_edit_descriptor_type), allocatable :: new_int_spec
            !! 生成されるインスタンス

        select case (form%enum)
        case (bin_digits%enum)
            allocate (new_int_spec, source=int_bin(width))

        case (oct_digits%enum)
            allocate (new_int_spec, source=int_oct(width))

        case (hex_digits%enum)
            allocate (new_int_spec, source=int_hex(width))

        case default
            allocate (new_int_spec, source=int())

        end select
    end function int_form_w_width

    !>指定の基数の整数型編集記述子のインスタンスを生成して返す．
    !>欄幅を指定せず，表示される幅を処理系に委ねる．
    !>指定された基数が2進数，8進数，16進数のいずれでもない場合は
    !>10進数の整数型編集記述子のインスタンスが生成される．
    function int_form(form) result(new_int_spec)
        implicit none
        type(int_form_type), intent(in) :: form
            !! 基数の形式
        class(integer_edit_descriptor_type), allocatable :: new_int_spec
            !! 生成されるインスタンス

        select case (form%enum)
        case (bin_digits%enum)
            allocate (new_int_spec, source=int_bin())

        case (oct_digits%enum)
            allocate (new_int_spec, source=int_oct())

        case (hex_digits%enum)
            allocate (new_int_spec, source=int_hex())

        case default
            allocate (new_int_spec, source=int())

        end select
    end function int_form
end module fed_editDescriptor_data_integer_facade
