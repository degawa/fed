program test_real_data_descriptor
    use :: fed_editDescriptor_data_real_standard
    use :: fassert
    implicit none

    print '(A)', "# Testing: real edit descriptor"
    call real_returns_G0_when_no_arugment_passed()
    call real_returns_Fwm_when_passed_w_m()
    call real_fixes_w_when_w_is_lt_minimum()
    call real_fixes_m_when_m_is_invalid()
    call real_returns_G0_when_w_is_le_0()

contains
    subroutine real_returns_G0_when_no_arugment_passed()
        implicit none
        type(real_standard_edit_descriptor_type) :: desc

        desc = real()
        call assert_equal(desc%get(), "G0", &
                          "real() should return 'G0'")

        ! teardown
        call desc%destruct()
    end subroutine real_returns_G0_when_no_arugment_passed

    subroutine real_returns_Fwm_when_passed_w_m()
        implicit none
        type(real_standard_edit_descriptor_type) :: desc

        desc = real(10, 7)
        call assert_equal(desc%get(), "F10.7", &
                          "real(10, 7) should return 'F10.7'")

        desc = real(40, 0)
        call assert_equal(desc%get(), "F40.0", &
                          "real(40, 0) should return 'F40.0'")

        ! teardown
        call desc%destruct()
    end subroutine real_returns_Fwm_when_passed_w_m

    subroutine real_fixes_w_when_w_is_lt_minimum()
        implicit none
        type(real_standard_edit_descriptor_type) :: desc

        desc = real(6, 5)
        call assert_equal(desc%get(), "F7.5", &
                          "real(6, 5) should return 'F7.5'")

        desc = real(10, 10)
        call assert_equal(desc%get(), "F12.10", &
                          "real(10, 10) should return 'F12.10'")

        ! teardown
        call desc%destruct()
    end subroutine real_fixes_w_when_w_is_lt_minimum

    subroutine real_fixes_m_when_m_is_invalid()
        implicit none
        type(real_standard_edit_descriptor_type) :: desc

        desc = real(10, -1)
        call assert_equal(desc%get(), "F10.7", &
                          "real(10, -1) should return 'F10.7'")

        desc = real(6, -2)
        call assert_equal(desc%get(), "F9.7", &
                          "real(6, -2) should return 'F9.7'")

        ! teardown
        call desc%destruct()
    end subroutine real_fixes_m_when_m_is_invalid

    subroutine real_returns_G0_when_w_is_le_0()
        implicit none
        type(real_standard_edit_descriptor_type) :: desc

        desc = real(0, 4)
        call assert_equal(desc%get(), "G0", &
                          "real(0, 4) should return 'G0'")

        desc = real(-5, 4)
        call assert_equal(desc%get(), "G0", &
                          "real(-5, 4) should return 'G0'")

        desc = real(-7, 0)
        call assert_equal(desc%get(), "G0", &
                          "real(-7, 0) should return 'G0'")

        desc = real(-3, -5)
        call assert_equal(desc%get(), "G0", &
                          "real(-3, -5) should return 'G0'")

        ! teardown
        call desc%destruct()
    end subroutine real_returns_G0_when_w_is_le_0
end program test_real_data_descriptor
