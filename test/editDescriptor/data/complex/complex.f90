program test_complex_data_descriptor
    use :: fed_editDescriptor_data_complex
    use :: fed_editDescriptor_data_complex_standard
    use :: fed_editDescriptor_data_complex_exponential
    use :: fed_editDescriptor_data_complex_scientific
    use :: fed_editDescriptor_data_complex_engineering
    use :: fed_editDescriptor_data_complex_facade
    use :: fed_editDescriptor_data_real_standard
    use :: fed_editDescriptor_data_real_exponential
    use :: fed_editDescriptor_data_real_scientific
    use :: fed_editDescriptor_data_real_engineering
    use :: fed_editDescriptor_data_real_facade, only:exp_form, sci_form, eng_form
    use :: fassert
    implicit none

    print '(A)', "# Testing: complex edit descriptor"
    call complex_spec_returns_bracket_separator_desc()
    call cmplx_std_constructor_returns_cmplx_std_descriptor_instance()
    call complex_returns_G0_when_passed_no_argument()
    call complex_returns_cmplx_std_edit_descriptor()
    call complex_exp_returns_cmplx_exp_edit_descriptor()
    call complex_sci_returns_cmplx_sci_edit_descriptor()
    call complex_eng_returns_cmplx_eng_edit_descriptor()
    call complex_exp_form_returns_cmplx_exp_edit_descriptor()
    call complex_sci_form_returns_cmplx_sci_edit_descriptor()
    call complex_eng_form_returns_cmplx_eng_edit_descriptor()
    call multiply_op_returns_data_desc_catenated_w_repeat_count()

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

        call assert_equal(complex_spec(real_exp(12, 6, 1)), '"(",E12.6E1,",",E12.6E1,")"', &
                          "complex_spec(real_exp(12, 6, 1)) should return a string " &
                          //"that is a concatenation of '()', ',', " &
                          //"and 'E12.6E1'")

        call assert_equal(complex_spec(real_exp(6, -1, -1)), '"(",E12.7E1,",",E12.7E1,")"', &
                          "complex_spec(real_exp(6, -1, -1)) should return a string " &
                          //"that is a concatenation of '()', ',', " &
                          //"and 'E12.7E1'")

        call assert_equal(complex_spec(real_exp(12, 6, 0)), '"(",E12.6,",",E12.6,")"', &
                          "complex_spec(real_exp(12, 6, 0)) should return a string " &
                          //"that is a concatenation of '()', ',', " &
                          //"and 'E12.6'")

        call assert_equal(complex_spec(real_exp(10, -1, 0)), '"(",E11.7,",",E11.7,")"', &
                          "complex_spec(real_exp(10, -1, 0)) should return a string " &
                          //"that is a concatenation of '()', ',', " &
                          //"and 'E11.7'")

        call assert_equal(complex_spec(real_exp()), '"(",E12.7E1,",",E12.7E1,")"', &
                          "complex_spec(real_exp()) should return a string " &
                          //"that is a concatenation of '()', ',', " &
                          //"and 'E12.7E1'")

        call assert_equal(complex_spec(real_sci(12, 6, 1)), '"(",ES12.6E1,",",ES12.6E1,")"', &
                          "complex_spec(real_sci(12, 6, 1)) should return a string " &
                          //"that is a concatenation of '()', ',', " &
                          //"and 'ES12.6E1'")

        call assert_equal(complex_spec(real_sci(6, -1, -1)), '"(",ES13.7E1,",",ES13.7E1,")"', &
                          "complex_spec(real_sci(6, -1, -1)) should return a string " &
                          //"that is a concatenation of '()', ',', " &
                          //"and 'ES13.7E1'")

        call assert_equal(complex_spec(real_sci(12, 6, 0)), '"(",ES12.6,",",ES12.6,")"', &
                          "complex_spec(real_sci(12, 6, 0)) should return a string " &
                          //"that is a concatenation of '()', ',', " &
                          //"and 'ES12.6'")

        call assert_equal(complex_spec(real_sci(10, -1, 0)), '"(",ES12.7,",",ES12.7,")"', &
                          "complex_spec(real_sci(10, -1, 0)) should return a string " &
                          //"that is a concatenation of '()', ',', " &
                          //"and 'ES12.7'")

        call assert_equal(complex_spec(real_sci()), '"(",ES13.7E1,",",ES13.7E1,")"', &
                          "complex_spec(real_sci()) should return a string " &
                          //"that is a concatenation of '()', ',', " &
                          //"and 'ES13.7E1'")

        call assert_equal(complex_spec(real_eng(14, 6, 1)), '"(",EN14.6E1,",",EN14.6E1,")"', &
                          "complex_spec(real_eng()) should return a string " &
                          //"that is a concatenation of '()', ',', " &
                          //"and 'EN14.6E1'")

        call assert_equal(complex_spec(real_eng(13, 6, 0)), '"(",EN13.6,",",EN13.6,")"', &
                          "complex_spec(real_eng(13, 6, 0)) should return a string " &
                          //"that is a concatenation of '()', ',', " &
                          //"and 'EN13.6'")

        call assert_equal(complex_spec(real_eng(6, -1, -1)), '"(",EN15.7E1,",",EN15.7E1,")"', &
                          "complex_spec(real_eng(6, -1, -1)) should return a string " &
                          //"that is a concatenation of '()', ',', " &
                          //"and 'EN15.7E1'")

        call assert_equal(complex_spec(real_eng(10, -1, 0)), '"(",EN14.7,",",EN14.7,")"', &
                          "complex_spec(real_eng(10, -1, 0)) should return a string " &
                          //"that is a concatenation of '()', ',', " &
                          //"and 'EN14.7'")

        call assert_equal(complex_spec(real_eng()), '"(",EN15.7E1,",",EN15.7E1,")"', &
                          "complex_spec(real_eng()) should return a string " &
                          //"that is a concatenation of '()', ',', " &
                          //"and 'EN15.7E1'")
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

    subroutine cmplx_exp_constructor_returns_cmplx_exp_descriptor_instance()
        use :: fed_editDescriptor
        implicit none
        class(edit_descriptor_type), allocatable :: desc
        type(complex_exponential_edit_descriptor_type) :: type_mold

        ! test
        allocate (desc, source=complex_exp())
        call assert_true(same_type_as(desc, type_mold), &
                         "complex() should return `complex_exponential_edit_descriptor_type` instance")

        ! teardown
        deallocate (desc)

        ! test
        allocate (desc, source=complex_exp(10, 4, 1))
        call assert_true(same_type_as(desc, type_mold), &
                         "complex(width, decimal_place_digits, exponent_digits) should return " &
                         //"`complex_exponential_edit_descriptor_type` instance")

        ! teardown
        deallocate (desc)
    end subroutine cmplx_exp_constructor_returns_cmplx_exp_descriptor_instance

    subroutine complex_exp_returns_cmplx_exp_edit_descriptor()
        implicit none
        type(complex_exponential_edit_descriptor_type) :: desc

        desc = complex_exp(12, 6, 1)
        call assert_equal(desc%get(), '"(",E12.6E1,",",E12.6E1,")"', &
                          'complex_exp(12, 6, 1) should return "(",E12.6E1,",",E12.6E1,")"')
        call desc%destruct()

        desc = complex_exp(6, -1, -1)
        call assert_equal(desc%get(), '"(",E12.7E1,",",E12.7E1,")"', &
                          'complex_exp(6, -1, -1) should return "(",E12.7E1,",",E12.7E1,")"')
        call desc%destruct()

        desc = complex_exp(12, 6, 0)
        call assert_equal(desc%get(), '"(",E12.6,",",E12.6,")"', &
                          'complex_exp(12, 6, 0) should return "(",E12.6,",",E12.6,")"')
        call desc%destruct()

        desc = complex_exp(10, -1, 0)
        call assert_equal(desc%get(), '"(",E11.7,",",E11.7,")"', &
                          'complex_exp(10, -1, 0) should return "(",E11.7,",",E11.7,")"')
        call desc%destruct()

        desc = complex_exp()
        call assert_equal(desc%get(), '"(",E12.7E1,",",E12.7E1,")"', &
                          'complex_exp() should return "(",E12.7E1,",",E12.7E1,")"')
        call desc%destruct()
    end subroutine complex_exp_returns_cmplx_exp_edit_descriptor

    subroutine complex_sci_returns_cmplx_sci_edit_descriptor()
        implicit none
        type(complex_scientific_edit_descriptor_type) :: desc

        desc = complex_sci(12, 6, 1)
        call assert_equal(desc%get(), '"(",ES12.6E1,",",ES12.6E1,")"', &
                          'complex_sci(12, 6, 1) should return "(",ES12.6E1,",",ES12.6E1,")"')
        call desc%destruct()

        desc = complex_sci(6, -1, -1)
        call assert_equal(desc%get(), '"(",ES13.7E1,",",ES13.7E1,")"', &
                          'complex_sci(6, -1, -1) should return "(",ES13.7E1,",",ES13.7E1,")"')
        call desc%destruct()

        desc = complex_sci(12, 6, 0)
        call assert_equal(desc%get(), '"(",ES12.6,",",ES12.6,")"', &
                          'complex_sci(12, 6, 0) should return "(",ES12.6,",",ES12.6,")"')
        call desc%destruct()

        desc = complex_sci(10, -1, 0)
        call assert_equal(desc%get(), '"(",ES12.7,",",ES12.7,")"', &
                          'complex_sci(10, -1, 0) should return "(",ES12.7,",",ES12.7,")"')
        call desc%destruct()

        desc = complex_sci()
        call assert_equal(desc%get(), '"(",ES13.7E1,",",ES13.7E1,")"', &
                          'complex_sci() should return "(",ES13.7E1,",",ES13.7E1,")"')
        call desc%destruct()
    end subroutine complex_sci_returns_cmplx_sci_edit_descriptor

    subroutine complex_eng_returns_cmplx_eng_edit_descriptor()
        implicit none
        type(complex_engineering_edit_descriptor_type) :: desc

        desc = complex_eng(14, 6, 1)
        call assert_equal(desc%get(), '"(",EN14.6E1,",",EN14.6E1,")"', &
                          'complex_eng(14, 6, 1) should return "(",EN14.6E1,",",EN14.6E1,")"')
        call desc%destruct()

        desc = complex_eng(13, 6, 0)
        call assert_equal(desc%get(), '"(",EN13.6,",",EN13.6,")"', &
                          'complex_eng(13, 6, 0) should return "(",EN13.6,",",EN13.6,")"')
        call desc%destruct()

        desc = complex_eng(6, -1, -1)
        call assert_equal(desc%get(), '"(",EN15.7E1,",",EN15.7E1,")"', &
                          'complex_eng(6, -1, -1) should return "(",EN15.7E1,",",EN15.7E1,")"')
        call desc%destruct()

        desc = complex_eng(10, -1, 0)
        call assert_equal(desc%get(), '"(",EN14.7,",",EN14.7,")"', &
                          'complex_eng(10, -1, 0) should return "(",EN14.7,",",EN14.7,")"')
        call desc%destruct()

        desc = complex_eng()
        call assert_equal(desc%get(), '"(",EN15.7E1,",",EN15.7E1,")"', &
                          'complex_eng() should return "(",EN15.7E1,",",EN15.7E1,")"')
        call desc%destruct()
    end subroutine complex_eng_returns_cmplx_eng_edit_descriptor

    subroutine complex_exp_form_returns_cmplx_exp_edit_descriptor()
        implicit none
        class(complex_edit_descriptor_type), allocatable :: desc

        desc = complex(exp_form, 12, 6, 1)
        call assert_equal(desc%get(), '"(",E12.6E1,",",E12.6E1,")"', &
                          'complex(exp_form,12, 6, 1) should return "(",E12.6E1,",",E12.6E1,")"')
        call desc%destruct()

        desc = complex(exp_form, 6, -1, -1)
        call assert_equal(desc%get(), '"(",E12.7E1,",",E12.7E1,")"', &
                          'complex(exp_form,6, -1, -1) should return "(",E12.7E1,",",E12.7E1,")"')
        call desc%destruct()

        desc = complex(exp_form, 12, 6, 0)
        call assert_equal(desc%get(), '"(",E12.6,",",E12.6,")"', &
                          'complex(exp_form,12, 6, 0) should return "(",E12.6,",",E12.6,")"')
        call desc%destruct()

        desc = complex(exp_form, 10, -1, 0)
        call assert_equal(desc%get(), '"(",E11.7,",",E11.7,")"', &
                          'complex(exp_form,10, -1, 0) should return "(",E11.7,",",E11.7,")"')
        call desc%destruct()

        desc = complex(exp_form)
        call assert_equal(desc%get(), '"(",E12.7E1,",",E12.7E1,")"', &
                          'complex(exp_form) should return "(",E12.7E1,",",E12.7E1,")"')
        call desc%destruct()
    end subroutine complex_exp_form_returns_cmplx_exp_edit_descriptor

    subroutine complex_sci_form_returns_cmplx_sci_edit_descriptor()
        implicit none
        class(complex_edit_descriptor_type), allocatable :: desc

        desc = complex(sci_form, 12, 6, 1)
        call assert_equal(desc%get(), '"(",ES12.6E1,",",ES12.6E1,")"', &
                          'complex(sci_form, 12, 6, 1) should return "(",ES12.6E1,",",ES12.6E1,")"')
        call desc%destruct()

        desc = complex(sci_form, 6, -1, -1)
        call assert_equal(desc%get(), '"(",ES13.7E1,",",ES13.7E1,")"', &
                          'complex(sci_form, 6, -1, -1) should return "(",ES13.7E1,",",ES13.7E1,")"')
        call desc%destruct()

        desc = complex(sci_form, 12, 6, 0)
        call assert_equal(desc%get(), '"(",ES12.6,",",ES12.6,")"', &
                          'complex(sci_form, 12, 6, 0) should return "(",ES12.6,",",ES12.6,")"')
        call desc%destruct()

        desc = complex(sci_form, 10, -1, 0)
        call assert_equal(desc%get(), '"(",ES12.7,",",ES12.7,")"', &
                          'complex(sci_form, 10, -1, 0) should return "(",ES12.7,",",ES12.7,")"')
        call desc%destruct()

        desc = complex(sci_form)
        call assert_equal(desc%get(), '"(",ES13.7E1,",",ES13.7E1,")"', &
                          'complex(sci_form) should return "(",ES13.7E1,",",ES13.7E1,")"')
        call desc%destruct()
    end subroutine complex_sci_form_returns_cmplx_sci_edit_descriptor

    subroutine complex_eng_form_returns_cmplx_eng_edit_descriptor()
        implicit none
        class(complex_edit_descriptor_type), allocatable :: desc

        desc = complex(eng_form, 14, 6, 1)
        call assert_equal(desc%get(), '"(",EN14.6E1,",",EN14.6E1,")"', &
                          'complex(eng_form, 14, 6, 1) should return "(",EN14.6E1,",",EN14.6E1,")"')
        call desc%destruct()

        desc = complex(eng_form, 13, 6, 0)
        call assert_equal(desc%get(), '"(",EN13.6,",",EN13.6,")"', &
                          'complex(eng_form, 13, 6, 0) should return "(",EN13.6,",",EN13.6,")"')
        call desc%destruct()

        desc = complex(eng_form, 6, -1, -1)
        call assert_equal(desc%get(), '"(",EN15.7E1,",",EN15.7E1,")"', &
                          'complex(eng_form, 6, -1, -1) should return "(",EN15.7E1,",",EN15.7E1,")"')
        call desc%destruct()

        desc = complex(eng_form, 10, -1, 0)
        call assert_equal(desc%get(), '"(",EN14.7,",",EN14.7,")"', &
                          'complex(eng_form, 10, -1, 0) should return "(",EN14.7,",",EN14.7,")"')
        call desc%destruct()

        desc = complex(eng_form)
        call assert_equal(desc%get(), '"(",EN15.7E1,",",EN15.7E1,")"', &
                          'complex(eng_form) should return "(",EN15.7E1,",",EN15.7E1,")"')
        call desc%destruct()
    end subroutine complex_eng_form_returns_cmplx_eng_edit_descriptor

    subroutine multiply_op_returns_data_desc_catenated_w_repeat_count()
        use :: fed_editDescriptor_data, only:data_edit_descriptor_type
        implicit none
        class(complex_edit_descriptor_type), allocatable :: desc
        type(data_edit_descriptor_type) :: cnt_desc

        ! setup
        desc = complex(eng_form, 14, 6, 1)

        ! test
        cnt_desc = 2*desc
        call assert_equal(cnt_desc%get(), '2("(",EN14.6E1,",",EN14.6E1,")")', &
                          '2*''"(",EN14.6E1,",",EN14.6E1,")"'' returns ''2("(",EN14.6E1,",",EN14.6E1,")")''')

        cnt_desc = 0*desc
        call assert_equal(cnt_desc%get(), '"(",EN14.6E1,",",EN14.6E1,")"', &
                          '0*''"(",EN14.6E1,",",EN14.6E1,")"'' returns ''"(",EN14.6E1,",",EN14.6E1,")"''')

        cnt_desc = (-1)*desc
        call assert_equal(cnt_desc%get(), '"(",EN14.6E1,",",EN14.6E1,")"', &
                          '-1*''"(",EN14.6E1,",",EN14.6E1,")"'' returns ''"(",EN14.6E1,",",EN14.6E1,")"''')

        cnt_desc = desc*5
        call assert_equal(cnt_desc%get(), '5("(",EN14.6E1,",",EN14.6E1,")")', &
                          '''"(",EN14.6E1,",",EN14.6E1,")"''*5 returns ''5("(",EN14.6E1,",",EN14.6E1,")")''')

        cnt_desc = desc*0
        call assert_equal(cnt_desc%get(), '"(",EN14.6E1,",",EN14.6E1,")"', &
                          '''"(",EN14.6E1,",",EN14.6E1,")"''*0 returns ''"(",EN14.6E1,",",EN14.6E1,")"''')

        cnt_desc = desc*(-1)
        call assert_equal(cnt_desc%get(), '"(",EN14.6E1,",",EN14.6E1,")"', &
                          '''"(",EN14.6E1,",",EN14.6E1,")"''*-1 returns ''("(",EN14.6E1,",",EN14.6E1,")")''')

    end subroutine multiply_op_returns_data_desc_catenated_w_repeat_count
end program test_complex_data_descriptor
