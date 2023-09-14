program test_real_data_descriptor
    use :: fed_editDescriptor_data_complex
    use :: fed_editDescriptor_data_complex_standard
    use :: fed_editDescriptor_data_real_standard
    use :: fassert
    implicit none

    print '(A)', "# Testing: complex edit descriptor"
    call complex_spec_returns_bracket_separator_desc()
    call cmplx_std_constructor_returns_cmplx_std_descriptor_instance()
    call complex_returns_G0_when_passed_no_argument()
    call complex_returns_cmplx_std_edit_descriptor()

contains
    subroutine complex_spec_returns_bracket_separator_desc()
        implicit none

        call assert_equal(complex_spec(real()), '"(",G0,",",G0,")"', &
                          "complex_spec(real()) should return a string " &
                          //"that is a concatenation of '()', ',', " &
                          //"and 'G0'")

        call assert_equal(complex_spec(real(8, 6)), '"(",F8.6,",",F8.6,")"', &
                          "complex_spec(real(8, 6)) should return a string " &
                          //"that is a concatenation of '()', ',', " &
                          //"and 'F8.6'")

        call assert_equal(complex_spec(real(10, 10)), '"(",F12.10,",",F12.10,")"', &
                          "complex_spec(real(10, 10)) should return a string " &
                          //"that is a concatenation of '()', ',', " &
                          //"and 'F12.10'")

        call assert_equal(complex_spec(real(6, -2)), '"(",F9.7,",",F9.7,")"', &
                          "complex_spec(real(6, -2)) should return a string " &
                          //"that is a concatenation of '()', ',', " &
                          //"and 'F9.7'")

        call assert_equal(complex_spec(real(-3, -5)), '"(",G0,",",G0,")"', &
                          "complex_spec(real(-3, -5)) should return a string " &
                          //"that is a concatenation of '()', ',', " &
                          //"and 'G0'")
    end subroutine complex_spec_returns_bracket_separator_desc

    subroutine cmplx_std_constructor_returns_cmplx_std_descriptor_instance()
        use :: fed_editDescriptor
        implicit none
        class(edit_descriptor_type), allocatable :: desc
        type(complex_standard_edit_descriptor_type) :: type_mold

        ! test
        allocate (desc, source=complex())
        call assert_true(same_type_as(desc, type_mold), &
                         "complex() should return `complex_standard_edit_descriptor_type` instance")

        ! teardown
        deallocate (desc)

        ! test
        allocate (desc, source=complex(width=7, decimal_place_digits=4))
        call assert_true(same_type_as(desc, type_mold), &
                         "complex(width, decimal_place_digits) should return `complex_standard_edit_descriptor_type` instance")

        ! teardown
        deallocate (desc)
    end subroutine cmplx_std_constructor_returns_cmplx_std_descriptor_instance

    subroutine complex_returns_G0_when_passed_no_argument()
        implicit none
        type(complex_standard_edit_descriptor_type) :: desc

        desc = complex()
        call assert_equal(desc%get(), '"(",G0,",",G0,")"', &
                          'complex() should return "(",G0,",",G0,")"')
        call desc%destruct()
    end subroutine complex_returns_G0_when_passed_no_argument

    subroutine complex_returns_cmplx_std_edit_descriptor()
        implicit none
        type(complex_standard_edit_descriptor_type) :: desc

        desc = complex(8, 6)
        call assert_equal(desc%get(), '"(",F8.6,",",F8.6,")"', &
                          'complex(8, 6) should return "(",F8.6,",",F8.6,")"')
        call desc%destruct()

        desc = complex(10, 10)
        call assert_equal(desc%get(), '"(",F12.10,",",F12.10,")"', &
                          'complex(10, 10) should return "(",F12.10,",",F12.10,")"')
        call desc%destruct()

        desc = complex(6, -2)
        call assert_equal(desc%get(), '"(",F9.7,",",F9.7,")"', &
                          'complex(6, -2) should return "(",F9.7,",",F9.7,")"')
        call desc%destruct()

        desc = complex(-3, -5)
        call assert_equal(desc%get(), '"(",G0,",",G0,")"', &
                          'complex(-3, -5) should return "(",G0,",",G0,")"')
        call desc%destruct()
    end subroutine complex_returns_cmplx_std_edit_descriptor
end program test_real_data_descriptor
