program test_integer_data_descriptor
    use :: fed_editDescriptor_data_integer
    use :: fed_editDescriptor_data_integer_decimal
    use :: fed_editDescriptor_data_integer_binary
    use :: fed_editDescriptor_data_integer_octal
    use :: fed_editDescriptor_data_integer_hexadecimal
    use :: fassert
    implicit none

    print '(A)', "# Testing: integer edit descriptor"
    call int_spec_returns_wm_when_passed_valid_width_pad()
    call int_spec_suppress_m_to_w_when_m_is_gt_w()
    call int_spec_returns_0_when_passed_w_0_and_m()
    call int_spec_returns_w_when_passed_m_0()
    call int_spec_returns_0_when_passed_invalid_w()
    call int_spec_returns_w_when_passed_valid_w_invalid_m()
    call int_spec_returns_0_when_passed_invalid_w_m()

    call dec_int_constructor_returns_decimal_integer_descriptor_instance()
    call int_returns_Iwm_when_passed_w_m()
    call int_returns_I0_when_no_argument_passed()
    call int_returns_Iw_when_passed_w()

    call bin_int_constructor_returns_binary_integer_descriptor_instance()
    call int_bin_returns_Bwm_when_passed_w_m()
    call int_bin_returns_B0_when_no_argument_passed()
    call int_bin_returns_Bww_when_passed_w()

contains
    subroutine int_spec_returns_wm_when_passed_valid_width_pad()
        implicit none

        call assert_equal(int_spec(2, 1), "2.1", &
                          "int_spec(2, 1) should return '2.1'")

        call assert_equal(int_spec(4, 2), "4.2", &
                          "int_spec(4, 2) should return '4.2'")

        call assert_equal(int_spec(14, 14), "14.14", &
                          "int_spec(14, 14) should return '14.14'")
    end subroutine int_spec_returns_wm_when_passed_valid_width_pad

    subroutine int_spec_suppress_m_to_w_when_m_is_gt_w()
        implicit none

        call assert_equal(int_spec(2, 3), "2.2", &
                          "int_spec(2, 3) should return '2.2'")

        call assert_equal(int_spec(6, 8), "6.6", &
                          "int_spec(6, 8) should return '6.6'")
    end subroutine int_spec_suppress_m_to_w_when_m_is_gt_w

    subroutine int_spec_returns_0_when_passed_w_0_and_m()
        implicit none

        call assert_equal(int_spec(0, 3), "0", &
                          "int_spec(0, 3) should return '0'")
    end subroutine int_spec_returns_0_when_passed_w_0_and_m

    subroutine int_spec_returns_w_when_passed_m_0()
        implicit none

        call assert_equal(int_spec(5, 0), "5", &
                          "int_spec(5, 0) should return '5'")
    end subroutine int_spec_returns_w_when_passed_m_0

    subroutine int_spec_returns_0_when_passed_invalid_w()
        implicit none

        call assert_equal(int_spec(-1, 3), "0", &
                          "int_spec(-1, 3) should return '0'")
    end subroutine int_spec_returns_0_when_passed_invalid_w

    subroutine int_spec_returns_w_when_passed_valid_w_invalid_m()
        implicit none

        call assert_equal(int_spec(8, -1), "8", &
                          "int_spec(8, -1) should return '8'")
    end subroutine int_spec_returns_w_when_passed_valid_w_invalid_m

    subroutine int_spec_returns_0_when_passed_invalid_w_m()
        implicit none

        call assert_equal(int_spec(-2, -1), "0", &
                          "int_spec(-2, -1) should return '0'")

        call assert_equal(int_spec(-5, -10), "0", &
                          "int_spec(-5, -10) should return '0'")
    end subroutine int_spec_returns_0_when_passed_invalid_w_m

    subroutine dec_int_constructor_returns_decimal_integer_descriptor_instance()
        use :: fed_editDescriptor
        implicit none
        class(edit_descriptor_type), allocatable :: desc
        type(decimal_integer_edit_descriptor_type) :: type_mold

        ! test
        allocate (desc, source=int())
        call assert_true(same_type_as(desc, type_mold), &
                         "int() should return `decimal_integer_edit_descriptor_type` instance")

        ! teardown
        deallocate (desc)

        ! test
        allocate (desc, source=int(width=4))
        call assert_true(same_type_as(desc, type_mold), &
                         "int(width) should return `decimal_integer_edit_descriptor_type` instance")

        ! teardown
        deallocate (desc)

        ! test
        allocate (desc, source=int(width=7, zero_padding_digit=6))
        call assert_true(same_type_as(desc, type_mold), &
                         "int(width, zero_padding_digit) should return `decimal_integer_edit_descriptor_type` instance")

        ! teardown
        deallocate (desc)
    end subroutine dec_int_constructor_returns_decimal_integer_descriptor_instance

    subroutine int_returns_Iwm_when_passed_w_m()
        implicit none
        type(decimal_integer_edit_descriptor_type) :: desc

        desc = int(2, 1)
        call assert_equal(desc%get(), "I2.1", &
                          "int(2, 1) should return 'I2.1'")
        desc = int(4, 2)
        call assert_equal(desc%get(), "I4.2", &
                          "int(4, 2) should return 'I4.2'")
        desc = int(14, 14)
        call assert_equal(desc%get(), "I14.14", &
                          "int(14, 14) should return 'I14.14'")

        desc = int(2, 3)
        call assert_equal(desc%get(), "I2.2", &
                          "int(2, 3) should return 'I2.2'")
        desc = int(6, 8)
        call assert_equal(desc%get(), "I6.6", &
                          "int(6, 8) should return 'I6.6'")

        desc = int(0, 3)
        call assert_equal(desc%get(), "I0", &
                          "int(0, 3) should return 'I0'")

        desc = int(5, 0)
        call assert_equal(desc%get(), "I5", &
                          "int(5, 0) should return 'I5'")

        desc = int(-1, 3)
        call assert_equal(desc%get(), "I0", &
                          "int(-1, 3) should return 'I0'")

        desc = int(8, -1)
        call assert_equal(desc%get(), "I8", &
                          "int(8, -1) should return 'I8'")

        desc = int(-2, -1)
        call assert_equal(desc%get(), "I0", &
                          "int(-2, -1) should return 'I0'")
        desc = int(-5, -10)
        call assert_equal(desc%get(), "I0", &
                          "int(-5, -10) should return 'I0'")

        ! teardown
        call desc%destruct()
    end subroutine int_returns_Iwm_when_passed_w_m

    subroutine int_returns_I0_when_no_argument_passed()
        implicit none
        type(decimal_integer_edit_descriptor_type) :: desc

        desc = int()
        call assert_equal(desc%get(), "I0", &
                          "int() should return 'I0'")

        ! teardown
        call desc%destruct()
    end subroutine int_returns_I0_when_no_argument_passed

    subroutine int_returns_Iw_when_passed_w()
        implicit none
        type(decimal_integer_edit_descriptor_type) :: desc

        desc = int(2)
        call assert_equal(desc%get(), "I2", &
                          "int(2) should return 'I2'")

        desc = int(0)
        call assert_equal(desc%get(), "I0", &
                          "int(0) should return 'I0'")

        desc = int(-1)
        call assert_equal(desc%get(), "I0", &
                          "int(-1) should return 'I0'")

        ! teardown
        call desc%destruct()
    end subroutine int_returns_Iw_when_passed_w

    subroutine bin_int_constructor_returns_binary_integer_descriptor_instance()
        use :: fed_editDescriptor
        implicit none
        class(edit_descriptor_type), allocatable :: desc
        type(binary_integer_edit_descriptor_type) :: type_mold

        ! test
        allocate (desc, source=int_bin())
        call assert_true(same_type_as(desc, type_mold), &
                         "int_bin() should return `binary_integer_edit_descriptor_type` instance")
        ! teardown
        deallocate (desc)

        ! test
        allocate (desc, source=int_bin(width=8))
        call assert_true(same_type_as(desc, type_mold), &
                         "int_bin(width) should return `binary_integer_edit_descriptor_type` instance")
        ! teardown
        deallocate (desc)

        ! test
        allocate (desc, source=int_bin(width=7, zero_padding_digit=6))
        call assert_true(same_type_as(desc, type_mold), &
                         "int_bin(width, zero_padding_digit) should return `binary_integer_edit_descriptor_type` instance")
        ! teardown
        deallocate (desc)
    end subroutine bin_int_constructor_returns_binary_integer_descriptor_instance

    subroutine int_bin_returns_Bwm_when_passed_w_m()
        implicit none
        type(binary_integer_edit_descriptor_type) :: desc

        desc = int_bin(2, 1)
        call assert_equal(desc%get(), "B2.1", &
                          "int_bin(2, 1) should return 'B2.1'")
        desc = int_bin(4, 2)
        call assert_equal(desc%get(), "B4.2", &
                          "int_bin(4, 2) should return 'B4.2'")
        desc = int_bin(14, 14)
        call assert_equal(desc%get(), "B14.14", &
                          "int_bin(14, 14) should return 'B14.14'")

        desc = int_bin(2, 3)
        call assert_equal(desc%get(), "B2.2", &
                          "int_bin(2, 3) should return 'B2.2'")
        desc = int_bin(6, 8)
        call assert_equal(desc%get(), "B6.6", &
                          "int_bin(6, 8) should return 'B6.6'")

        desc = int_bin(0, 3)
        call assert_equal(desc%get(), "B0", &
                          "int_bin(0, 3) should return 'B0'")

        desc = int_bin(5, 0)
        call assert_equal(desc%get(), "B5", &
                          "int_bin(5, 0) should return 'B5'")

        desc = int_bin(-1, 3)
        call assert_equal(desc%get(), "B0", &
                          "int_bin(-1, 3) should return 'B0'")

        desc = int_bin(8, -1)
        call assert_equal(desc%get(), "B8", &
                          "int_bin(8, -1) should return 'B8'")

        desc = int_bin(-2, -1)
        call assert_equal(desc%get(), "B0", &
                          "int_bin(-2, -1) should return 'B0'")
        desc = int_bin(-5, -10)
        call assert_equal(desc%get(), "B0", &
                          "int_bin(-5, -10) should return 'B0'")

        ! teardown
        call desc%destruct()
    end subroutine int_bin_returns_Bwm_when_passed_w_m

    subroutine int_bin_returns_B0_when_no_argument_passed()
        implicit none
        type(binary_integer_edit_descriptor_type) :: desc

        desc = int_bin()
        call assert_equal(desc%get(), "B0", &
                          "int_bin() should return 'B0'")

        ! teardown
        call desc%destruct()
    end subroutine int_bin_returns_B0_when_no_argument_passed

    subroutine int_bin_returns_Bww_when_passed_w()
        implicit none
        type(binary_integer_edit_descriptor_type) :: desc

        desc = int_bin(2)
        call assert_equal(desc%get(), "B2.2", &
                          "int_bin(2) should return 'B2.2'")

        desc = int_bin(0)
        call assert_equal(desc%get(), "B0", &
                          "int_bin(0) should return 'B0'")

        desc = int_bin(-1)
        call assert_equal(desc%get(), "B0", &
                          "int_bin(-1) should return 'B0'")

        ! teardown
        call desc%destruct()
    end subroutine int_bin_returns_Bww_when_passed_w

    subroutine oct_int_constructor_returns_octal_integer_descriptor_instance()
        use :: fed_editDescriptor
        implicit none
        class(edit_descriptor_type), allocatable :: desc
        type(octal_integer_edit_descriptor_type) :: type_mold

        ! test
        allocate (desc, source=int_oct())
        call assert_true(same_type_as(desc, type_mold), &
                         "int_oct() should return `octal_integer_edit_descriptor_type` instance")
        ! teardown
        deallocate (desc)

        ! test
        allocate (desc, source=int_oct(width=8))
        call assert_true(same_type_as(desc, type_mold), &
                         "int_oct(width) should return `octal_integer_edit_descriptor_type` instance")
        ! teardown
        deallocate (desc)

        ! test
        allocate (desc, source=int_oct(width=7, zero_padding_digit=6))
        call assert_true(same_type_as(desc, type_mold), &
                         "int_oct(width, zero_padding_digit) should return `octal_integer_edit_descriptor_type` instance")
        ! teardown
        deallocate (desc)
    end subroutine oct_int_constructor_returns_octal_integer_descriptor_instance

    subroutine int_oct_returns_Owm_when_passed_w_m()
        implicit none
        type(octal_integer_edit_descriptor_type) :: desc

        desc = int_oct(2, 1)
        call assert_equal(desc%get(), "O2.1", &
                          "int_oct(2, 1) should return 'O2.1'")
        desc = int_oct(4, 2)
        call assert_equal(desc%get(), "O4.2", &
                          "int_oct(4, 2) should return 'O4.2'")
        desc = int_oct(14, 14)
        call assert_equal(desc%get(), "O14.14", &
                          "int_oct(14, 14) should return 'O14.14'")

        desc = int_oct(2, 3)
        call assert_equal(desc%get(), "O2.2", &
                          "int_oct(2, 3) should return 'O2.2'")
        desc = int_oct(6, 8)
        call assert_equal(desc%get(), "O6.6", &
                          "int_oct(6, 8) should return 'O6.6'")

        desc = int_oct(0, 3)
        call assert_equal(desc%get(), "O0", &
                          "int_oct(0, 3) should return 'O0'")

        desc = int_oct(5, 0)
        call assert_equal(desc%get(), "O5", &
                          "int_oct(5, 0) should return 'O5'")

        desc = int_oct(-1, 3)
        call assert_equal(desc%get(), "O0", &
                          "int_oct(-1, 3) should return 'O0'")

        desc = int_oct(8, -1)
        call assert_equal(desc%get(), "O8", &
                          "int_oct(8, -1) should return 'O8'")

        desc = int_oct(-2, -1)
        call assert_equal(desc%get(), "O0", &
                          "int_oct(-2, -1) should return 'O0'")
        desc = int_oct(-5, -10)
        call assert_equal(desc%get(), "O0", &
                          "int_oct(-5, -10) should return 'O0'")

        ! teardown
        call desc%destruct()
    end subroutine int_oct_returns_Owm_when_passed_w_m

    subroutine int_oct_returns_O0_when_no_argument_passed()
        implicit none
        type(octal_integer_edit_descriptor_type) :: desc

        desc = int_oct()
        call assert_equal(desc%get(), "O0", &
                          "int_oct() should return 'O0'")

        ! teardown
        call desc%destruct()
    end subroutine int_oct_returns_O0_when_no_argument_passed

    subroutine int_oct_returns_Oww_when_passed_w()
        implicit none
        type(octal_integer_edit_descriptor_type) :: desc

        desc = int_oct(2)
        call assert_equal(desc%get(), "O2.2", &
                          "int_oct(2) should return 'O2.2'")

        desc = int_oct(0)
        call assert_equal(desc%get(), "O0", &
                          "int_oct(0) should return 'O0'")

        desc = int_oct(-1)
        call assert_equal(desc%get(), "O0", &
                          "int_oct(-1) should return 'O0'")

        ! teardown
        call desc%destruct()
    end subroutine int_oct_returns_Oww_when_passed_w

    subroutine hex_int_constructor_returns_hex_integer_descriptor_instance()
        use :: fed_editDescriptor
        implicit none
        class(edit_descriptor_type), allocatable :: desc
        type(hexadecimal_integer_edit_descriptor_type) :: type_mold

        ! test
        allocate (desc, source=int_hex())
        call assert_true(same_type_as(desc, type_mold), &
                         "int_hex() should return `hexadecimal_integer_edit_descriptor_type` instance")
        ! teardown
        deallocate (desc)

        ! test
        allocate (desc, source=int_hex(width=8))
        call assert_true(same_type_as(desc, type_mold), &
                         "int_hex(width) should return `hexadecimal_integer_edit_descriptor_type` instance")
        ! teardown
        deallocate (desc)

        ! test
        allocate (desc, source=int_hex(width=7, zero_padding_digit=6))
        call assert_true(same_type_as(desc, type_mold), &
                         "int_hex(width, zero_padding_digit) should return `hexadecimal_integer_edit_descriptor_type` instance")
        ! teardown
        deallocate (desc)
    end subroutine hex_int_constructor_returns_hex_integer_descriptor_instance

    subroutine int_hex_returns_Zwm_when_passed_w_m()
        implicit none
        type(hexadecimal_integer_edit_descriptor_type) :: desc

        desc = int_hex(2, 1)
        call assert_equal(desc%get(), "Z2.1", &
                          "int_hex(2, 1) should return 'Z2.1'")
        desc = int_hex(4, 2)
        call assert_equal(desc%get(), "Z4.2", &
                          "int_hex(4, 2) should return 'Z4.2'")
        desc = int_hex(14, 14)
        call assert_equal(desc%get(), "Z14.14", &
                          "int_hex(14, 14) should return 'Z14.14'")

        desc = int_hex(2, 3)
        call assert_equal(desc%get(), "Z2.2", &
                          "int_hex(2, 3) should return 'Z2.2'")
        desc = int_hex(6, 8)
        call assert_equal(desc%get(), "Z6.6", &
                          "int_hex(6, 8) should return 'Z6.6'")

        desc = int_hex(0, 3)
        call assert_equal(desc%get(), "Z0", &
                          "int_hex(0, 3) should return 'Z0'")

        desc = int_hex(5, 0)
        call assert_equal(desc%get(), "Z5", &
                          "int_hex(5, 0) should return 'Z5'")

        desc = int_hex(-1, 3)
        call assert_equal(desc%get(), "Z0", &
                          "int_hex(-1, 3) should return 'Z0'")

        desc = int_hex(8, -1)
        call assert_equal(desc%get(), "Z8", &
                          "int_hex(8, -1) should return 'Z8'")

        desc = int_hex(-2, -1)
        call assert_equal(desc%get(), "Z0", &
                          "int_hex(-2, -1) should return 'Z0'")
        desc = int_hex(-5, -10)
        call assert_equal(desc%get(), "Z0", &
                          "int_hex(-5, -10) should return 'Z0'")

        ! teardown
        call desc%destruct()
    end subroutine int_hex_returns_Zwm_when_passed_w_m

    subroutine int_hex_returns_Z0_when_no_argument_passed()
        implicit none
        type(hexadecimal_integer_edit_descriptor_type) :: desc

        desc = int_hex()
        call assert_equal(desc%get(), "Z0", &
                          "int_hex() should return 'Z0'")

        ! teardown
        call desc%destruct()
    end subroutine int_hex_returns_Z0_when_no_argument_passed

    subroutine int_hex_returns_Zww_when_passed_w()
        implicit none
        type(hexadecimal_integer_edit_descriptor_type) :: desc

        desc = int_hex(2)
        call assert_equal(desc%get(), "Z2.2", &
                          "int_hex(2) should return 'Z2.2'")

        desc = int_hex(0)
        call assert_equal(desc%get(), "Z0", &
                          "int_hex(0) should return 'Z0'")

        desc = int_hex(-1)
        call assert_equal(desc%get(), "Z0", &
                          "int_hex(-1) should return 'Z0'")

        ! teardown
        call desc%destruct()
    end subroutine int_hex_returns_Zww_when_passed_w
end program test_integer_data_descriptor
