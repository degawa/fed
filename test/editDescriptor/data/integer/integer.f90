program test_integer_data_descriptor
    use :: fed_editDescriptor_data_integer
    use :: fed_editDescriptor_data_integer_decimal
    use :: fassert
    implicit none

    print '(A)', "# Testing: integer_edit_descriptor"
    call int_spec_returns_wm_when_passed_valid_width_pad()
    call int_spec_suppress_m_to_w_when_m_is_gt_w()
    call int_spec_returns_0_when_passed_w_0_and_m()
    call int_spec_returns_w_when_passed_m_0()
    call int_spec_returns_0_when_passed_invalid_w()
    call int_spec_returns_w_when_passed_valid_w_invalid_m()
    call int_spec_returns_0_when_passed_invalid_w_m()

    call int_returns_Iwm_when_passed_w_m()
    call int_returns_I0_when_no_argument_passed()
    call int_returns_Iw_when_passed_w()

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
end program test_integer_data_descriptor
