program test_real_data_descriptor
    use :: fed_editDescriptor_data_real
    use :: fed_editDescriptor_data_real_standard
    use :: fed_editDescriptor_data_real_exponential
    use :: fed_editDescriptor_data_real_scientific
    use :: fed_editDescriptor_data_real_engineering
    use :: fed_editDescriptor_data_real_facade
    use :: fassert
    implicit none

    print '(A)', "# Testing: real edit descriptor"
    call real_std_constructor_returns_real_std_descriptor_instance()
    call real_returns_G0_when_no_arugment_passed()
    call real_returns_Fwm_when_passed_w_m()
    call real_fixes_w_when_w_is_lt_minimum()
    call real_fixes_m_when_m_is_invalid()
    call real_returns_G0_when_w_is_le_0()

    call real_spec_returns_wmEe_when_passed_valid_params()
    call real_spec_replace_w_to_melenstr_when_w_is_lt_minimum_len()
    call real_spec_replace_m_to_deafult_value_when_m_is_lt_0()
    call real_spec_replace_e_to_deafult_value_when_e_is_lt_0()
    call real_spec_returns_wm_when_e_is_0()

    call real_exp_returns_EwmEe_when_passed_w_m_e()
    call real_exp_returns_Ewm_when_passed_w_m_0()
    call real_exp_returns_default_spec_when_passed_no_argument()

    call real_sci_returns_ESwmEe_when_passed_w_m_e()
    call real_sci_returns_ESwm_when_passed_w_m_0()
    call real_sci_returns_default_spec_when_passed_no_argument()

    call real_eng_returns_ENwmEe_when_passed_w_m_e()
    call real_eng_returns_ENwm_when_passed_w_m_0()
    call real_eng_returns_default_spec_when_passed_no_argument()

    call comparison_of_enums_returns_true_when_enum_are_same()
    call comparison_of_enums_returns_false_when_enum_are_different()
    call real_exp_form_returns_EwmEe_when_passed_w_m_e()
    call real_exp_form_returns_Ewm_when_passed_w_m_0()
    call real_exp_form_returns_default_spec_when_passed_no_argument()
    call real_sci_form_returns_ESwmEe_when_passed_w_m_e()
    call real_sci_form_returns_ESwm_when_passed_w_m_0()
    call real_sci_form_returns_default_spec_when_passed_no_argument()
    call real_eng_form_returns_ENwmEe_when_passed_w_m_e()
    call real_eng_form_returns_ENwm_when_passed_w_m_0()
    call real_eng_form_returns_default_spec_when_passed_no_argument()
contains
    subroutine real_std_constructor_returns_real_std_descriptor_instance()
        use :: fed_editDescriptor
        implicit none
        class(edit_descriptor_type), allocatable :: desc
        type(real_standard_edit_descriptor_type) :: type_mold

        ! test
        allocate (desc, source=real())
        call assert_true(same_type_as(desc, type_mold), &
                         "real() should return `real_standard_edit_descriptor_type` instance")

        ! teardown
        deallocate (desc)

        ! test
        allocate (desc, source=real(width=7, decimal_place_digits=4))
        call assert_true(same_type_as(desc, type_mold), &
                         "real(width, decimal_place_digits) should return `real_standard_edit_descriptor_type` instance")

        ! teardown
        deallocate (desc)
    end subroutine real_std_constructor_returns_real_std_descriptor_instance

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

    subroutine real_spec_returns_wmEe_when_passed_valid_params()
        implicit none

        call assert_equal(real_spec(10, 3, 1, ""), "10.3E1", &
                          "real_spec(10, 3, 1, '') should return '10.3E1'")

        call assert_equal(real_spec(41, 30, 3, "-.E"), "41.30E3", &
                          "real_spec(41, 30, 3, '-.E') should return '41.30E3'")

        call assert_equal(real_spec(14, 7, 1, "-000.E"), "14.7E1", &
                          "real_spec(14, 7, 1, '-000.E') should return '14.7E1'")

        call assert_equal(real_spec(7, 0, 1, "-.E"), "7.0E1", &
                          "real_spec(7, 0, 1, '-.E') should return '7.0E1'")
    end subroutine real_spec_returns_wmEe_when_passed_valid_params

    subroutine real_spec_replace_w_to_melenstr_when_w_is_lt_minimum_len()
        implicit none

        call assert_equal(real_spec(4, 3, 1, "E"), "5.3E1", &
                          "real_spec(4, 3, 1, 'E') should return '5.3E1'")

        call assert_equal(real_spec(34, 30, 3, "-.E"), "36.30E3", &
                          "real_spec(34, 30, 3, '-.E') should return '36.30E3'")

        call assert_equal(real_spec(12, 7, 1, "-000.E"), "14.7E1", &
                          "real_spec(12, 7, 1, '-000.E') should return '14.7E1'")

        call assert_equal(real_spec(3, 0, 1, "-.E"), "4.0E1", &
                          "real_spec(3, 0, 1, '-.E') should return '4.0E1'")
    end subroutine real_spec_replace_w_to_melenstr_when_w_is_lt_minimum_len

    subroutine real_spec_replace_m_to_deafult_value_when_m_is_lt_0()
        implicit none
        call assert_equal(real_spec(10, -1, 1, "E"), "10.7E1", &
                          "real_spec(10, -1, 1, 'E') should return '10.7E1'")

        call assert_equal(real_spec(10, -2, 3, "-.E"), "13.7E3", &
                          "real_spec(10, -2, 3, '-.E') should return '13.7E3'")
    end subroutine real_spec_replace_m_to_deafult_value_when_m_is_lt_0

    subroutine real_spec_replace_e_to_deafult_value_when_e_is_lt_0()
        implicit none
        call assert_equal(real_spec(10, 9, -3, "E"), "11.9E1", &
                          "real_spec(10, 9 -3, 'E') should return '11.9E1'")

        call assert_equal(real_spec(10, -2, -3, "-.E"), "11.7E1", &
                          "real_spec(10, -2, -3, '-.E') should return '11.7E1'")
    end subroutine real_spec_replace_e_to_deafult_value_when_e_is_lt_0

    subroutine real_spec_returns_wm_when_e_is_0()
        implicit none
        call assert_equal(real_spec(10, 6, 0, "-."), "10.6", &
                          "real_spec(10, 6, 0, '-.') should return '10.6'")

        call assert_equal(real_spec(10, 0, 0, "-."), "10.0", &
                          "real_spec(10, 0, 0, '-.') should return '10.0'")

        call assert_equal(real_spec(9, -1, 0, "-."), "9.7", &
                          "real_spec(9, -1, 0, '-.') should return '9.7'")

        call assert_equal(real_spec(3, -1, 0, "-000."), "12.7", &
                          "real_spec(3, -1, 0, '-000.') should return '12.7'")
    end subroutine real_spec_returns_wm_when_e_is_0

    subroutine real_exp_returns_EwmEe_when_passed_w_m_e()
        implicit none
        type(real_exponential_edit_descriptor_type) :: desc

        desc = real_exp(12, 6, 1)
        call assert_equal(desc%get(), "E12.6E1", &
                          "real_exp(12, 6, 1) should return 'E12.6E1'")

        desc = real_exp(40, 0, 3)
        call assert_equal(desc%get(), "E40.0E3", &
                          "real_exp(40, 0, 3) should return 'E40.0E3'")

        desc = real_exp(10, -1, 1) ! 7+1+len("-.E+")
        call assert_equal(desc%get(), "E12.7E1", &
                          "real_exp(10, -1, 1) should return 'E12.7E1'")

        desc = real_exp(10, 3, -1) ! 3+1+len("-.E+")
        call assert_equal(desc%get(), "E10.3E1", &
                          "real_exp(10, 3, -1) should return 'E10.3E1'")

        desc = real_exp(6, -1, -1) ! 7+1+len("-.E+")
        call assert_equal(desc%get(), "E12.7E1", &
                          "real_exp(6, -1, -1) should return 'E12.7E1'")

        ! teardown
        call desc%destruct()
    end subroutine real_exp_returns_EwmEe_when_passed_w_m_e

    subroutine real_exp_returns_Ewm_when_passed_w_m_0()
        implicit none
        type(real_exponential_edit_descriptor_type) :: desc

        desc = real_exp(12, 6, 0)
        call assert_equal(desc%get(), "E12.6", &
                          "real_exp(12, 6, 0) should return 'E12.6'")

        desc = real_exp(40, 0, 0)
        call assert_equal(desc%get(), "E40.0", &
                          "real_exp(40, 0, 0) should return 'E40.0'")

        desc = real_exp(10, -1, 0) ! 7+0+len("-.E+")
        call assert_equal(desc%get(), "E11.7", &
                          "real_exp(10, -1, 0) should return 'E11.7'")

        ! teardown
        call desc%destruct()
    end subroutine real_exp_returns_Ewm_when_passed_w_m_0

    subroutine real_exp_returns_default_spec_when_passed_no_argument()
        implicit none
        type(real_exponential_edit_descriptor_type) :: desc

        desc = real_exp() ! 7+1+len("-.E+")
        call assert_equal(desc%get(), "E12.7E1", &
                          "real_exp() should return 'E12.7E1'")

        ! teardown
        call desc%destruct()
    end subroutine real_exp_returns_default_spec_when_passed_no_argument

    subroutine real_sci_returns_ESwmEe_when_passed_w_m_e()
        implicit none
        type(real_scientific_edit_descriptor_type) :: desc

        desc = real_sci(12, 6, 1)
        call assert_equal(desc%get(), "ES12.6E1", &
                          "real_sci(12, 6, 1) should return 'ES12.6E1'")

        desc = real_sci(40, 0, 3)
        call assert_equal(desc%get(), "ES40.0E3", &
                          "real_sci(40, 0, 3) should return 'ES40.0E3'")

        desc = real_sci(10, -1, 1) ! 7+1+len("-0.E+")
        call assert_equal(desc%get(), "ES13.7E1", &
                          "real_sci(10, -1, 1) should return 'ES13.7E1'")

        desc = real_sci(10, 3, -1) ! 3+1+len("-0.E+")
        call assert_equal(desc%get(), "ES10.3E1", &
                          "real_sci(10, 3, -1) should return 'ES10.3E1'")

        desc = real_sci(6, -1, -1) ! 7+1+len("-0.E+")
        call assert_equal(desc%get(), "ES13.7E1", &
                          "real_sci(6, -1, -1) should return 'ES13.7E1'")

        ! teardown
        call desc%destruct()
    end subroutine real_sci_returns_ESwmEe_when_passed_w_m_e

    subroutine real_sci_returns_ESwm_when_passed_w_m_0()
        implicit none
        type(real_scientific_edit_descriptor_type) :: desc

        desc = real_sci(12, 6, 0)
        call assert_equal(desc%get(), "ES12.6", &
                          "real_sci(12, 6, 0) should return 'ES12.6'")

        desc = real_sci(40, 0, 0)
        call assert_equal(desc%get(), "ES40.0", &
                          "real_sci(40, 0, 0) should return 'ES40.0'")

        desc = real_sci(10, -1, 0) ! 7+0+len("-0.E+")
        call assert_equal(desc%get(), "ES12.7", &
                          "real_sci(10, -1, 0) should return 'ES12.7'")

        ! teardown
        call desc%destruct()
    end subroutine real_sci_returns_ESwm_when_passed_w_m_0

    subroutine real_sci_returns_default_spec_when_passed_no_argument()
        implicit none
        type(real_scientific_edit_descriptor_type) :: desc

        desc = real_sci() ! 7+1+len("-0.E+")
        call assert_equal(desc%get(), "ES13.7E1", &
                          "real_sci() should return 'ES13.7E1'")

        ! teardown
        call desc%destruct()
    end subroutine real_sci_returns_default_spec_when_passed_no_argument

    subroutine real_eng_returns_ENwmEe_when_passed_w_m_e()
        implicit none
        type(real_engineering_edit_descriptor_type) :: desc

        desc = real_eng(14, 6, 1)
        call assert_equal(desc%get(), "EN14.6E1", &
                          "real_eng(14, 6, 1) should return 'EN14.6E1'")

        desc = real_eng(40, 0, 3)
        call assert_equal(desc%get(), "EN40.0E3", &
                          "real_eng(40, 0, 3) should return 'EN40.0E3'")

        desc = real_eng(10, -1, 1) ! 7+1+len("-000.E+")
        call assert_equal(desc%get(), "EN15.7E1", &
                          "real_eng(10, -1, 1) should return 'EN15.7E1'")

        desc = real_eng(12, 3, -1) ! 3+1+len("-000.E+")
        call assert_equal(desc%get(), "EN12.3E1", &
                          "real_eng(12, 3, -1) should return 'EN12.3E1'")

        desc = real_eng(6, -1, -1) ! 7+1+len("-000.E+")
        call assert_equal(desc%get(), "EN15.7E1", &
                          "real_eng(6, -1, -1) should return 'EN15.7E1'")

        ! teardown
        call desc%destruct()
    end subroutine real_eng_returns_ENwmEe_when_passed_w_m_e

    subroutine real_eng_returns_ENwm_when_passed_w_m_0()
        implicit none
        type(real_engineering_edit_descriptor_type) :: desc

        desc = real_eng(14, 6, 0)
        call assert_equal(desc%get(), "EN14.6", &
                          "real_eng(14, 6, 0) should return 'EN14.6'")

        desc = real_eng(40, 0, 0)
        call assert_equal(desc%get(), "EN40.0", &
                          "real_eng(40, 0, 0) should return 'EN40.0'")

        desc = real_eng(10, -1, 0) ! 7+0+len("-000.E+")
        call assert_equal(desc%get(), "EN14.7", &
                          "real_eng(10, -1, 0) should return 'EN14.7'")

        ! teardown
        call desc%destruct()
    end subroutine real_eng_returns_ENwm_when_passed_w_m_0

    subroutine real_eng_returns_default_spec_when_passed_no_argument()
        implicit none
        type(real_engineering_edit_descriptor_type) :: desc

        desc = real_eng() ! 7+1+len("-000.E+")
        call assert_equal(desc%get(), "EN15.7E1", &
                          "real_eng() should return 'EN15.7E1'")

        ! teardown
        call desc%destruct()
    end subroutine real_eng_returns_default_spec_when_passed_no_argument

    subroutine comparison_of_enums_returns_true_when_enum_are_same()
        implicit none
        call assert_true(exp_form == exp_form, &
                         "comparison of exp_form returns true when enumerator on both sides are the same")

        call assert_true(sci_form == sci_form, &
                         "comparison of sci_form returns true when enumerator on both sides are the same")

        call assert_true(eng_form == eng_form, &
                         "comparison of eng_form returns true when enumerator on both sides are the same")
    end subroutine comparison_of_enums_returns_true_when_enum_are_same

    subroutine comparison_of_enums_returns_false_when_enum_are_different()
        implicit none
        call assert_false(exp_form == sci_form, &
                          "comparison of exp_form and sci_form returns false")

        call assert_false(exp_form == eng_form, &
                          "comparison of exp_form and eng_form returns false")

        call assert_false(sci_form == exp_form, &
                          "comparison of sci_form and exp_form returns false")

        call assert_false(sci_form == eng_form, &
                          "comparison of sci_form and eng_form returns false")

        call assert_false(eng_form == exp_form, &
                          "comparison of eng_form and exp_form returns false")

        call assert_false(eng_form == sci_form, &
                          "comparison of eng_form and sci_form returns false")
    end subroutine comparison_of_enums_returns_false_when_enum_are_different

    subroutine real_exp_form_returns_EwmEe_when_passed_w_m_e()
        implicit none
        class(real_edit_descriptor_type), allocatable :: desc

        desc = real(exp_form, 12, 6, 1)
        call assert_equal(desc%get(), "E12.6E1", &
                          "real(exp_form, 12, 6, 1) should return 'E12.6E1'")

        desc = real(exp_form, 40, 0, 3)
        call assert_equal(desc%get(), "E40.0E3", &
                          "real(exp_form, 40, 0, 3) should return 'E40.0E3'")

        desc = real(exp_form, 10, -1, 1) ! 7+1+len("-.E+")
        call assert_equal(desc%get(), "E12.7E1", &
                          "real(exp_form, 10, -1, 1) should return 'E12.7E1'")

        desc = real(exp_form, 10, 3, -1) ! 3+1+len("-.E+")
        call assert_equal(desc%get(), "E10.3E1", &
                          "real(exp_form, 10, 3, -1) should return 'E10.3E1'")

        desc = real(exp_form, 6, -1, -1) ! 7+1+len("-.E+")
        call assert_equal(desc%get(), "E12.7E1", &
                          "real(exp_form, 6, -1, -1) should return 'E12.7E1'")

        ! teardown
        call desc%destruct()
    end subroutine real_exp_form_returns_EwmEe_when_passed_w_m_e

    subroutine real_exp_form_returns_Ewm_when_passed_w_m_0()
        implicit none
        class(real_edit_descriptor_type), allocatable :: desc

        desc = real(exp_form, 12, 6, 0)
        call assert_equal(desc%get(), "E12.6", &
                          "real(exp_form, 12, 6, 0) should return 'E12.6'")

        desc = real(exp_form, 40, 0, 0)
        call assert_equal(desc%get(), "E40.0", &
                          "real(exp_form, 40, 0, 0) should return 'E40.0'")

        desc = real(exp_form, 10, -1, 0) ! 7+0+len("-.E+")
        call assert_equal(desc%get(), "E11.7", &
                          "real(exp_form, 10, -1, 0) should return 'E11.7'")

        ! teardown
        call desc%destruct()
    end subroutine real_exp_form_returns_Ewm_when_passed_w_m_0

    subroutine real_exp_form_returns_default_spec_when_passed_no_argument()
        implicit none
        class(real_edit_descriptor_type), allocatable :: desc

        desc = real(exp_form) ! 7+1+len("-.E+")
        call assert_equal(desc%get(), "E12.7E1", &
                          "real(exp_form) should return 'E12.7E1'")

        ! teardown
        call desc%destruct()
    end subroutine real_exp_form_returns_default_spec_when_passed_no_argument

    subroutine real_sci_form_returns_ESwmEe_when_passed_w_m_e()
        implicit none
        class(real_edit_descriptor_type), allocatable :: desc

        desc = real(sci_form, 12, 6, 1)
        call assert_equal(desc%get(), "ES12.6E1", &
                          "real(sci_form, 12, 6, 1) should return 'ES12.6E1'")

        desc = real(sci_form, 40, 0, 3)
        call assert_equal(desc%get(), "ES40.0E3", &
                          "real(sci_form, 40, 0, 3) should return 'ES40.0E3'")

        desc = real(sci_form, 10, -1, 1) ! 7+1+len("-0.E+")
        call assert_equal(desc%get(), "ES13.7E1", &
                          "real(sci_form, 10, -1, 1) should return 'ES13.7E1'")

        desc = real(sci_form, 10, 3, -1) ! 3+1+len("-0.E+")
        call assert_equal(desc%get(), "ES10.3E1", &
                          "real(sci_form, 10, 3, -1) should return 'ES10.3E1'")

        desc = real(sci_form, 6, -1, -1) ! 7+1+len("-0.E+")
        call assert_equal(desc%get(), "ES13.7E1", &
                          "real(sci_form, 6, -1, -1) should return 'ES13.7E1'")

        ! teardown
        call desc%destruct()
    end subroutine real_sci_form_returns_ESwmEe_when_passed_w_m_e

    subroutine real_sci_form_returns_ESwm_when_passed_w_m_0()
        implicit none
        class(real_edit_descriptor_type), allocatable :: desc

        desc = real(sci_form, 12, 6, 0)
        call assert_equal(desc%get(), "ES12.6", &
                          "real(sci_form, 12, 6, 0) should return 'ES12.6'")

        desc = real(sci_form, 40, 0, 0)
        call assert_equal(desc%get(), "ES40.0", &
                          "real(sci_form, 40, 0, 0) should return 'ES40.0'")

        desc = real(sci_form, 10, -1, 0) ! 7+0+len("-0.E+")
        call assert_equal(desc%get(), "ES12.7", &
                          "real(sci_form, 10, -1, 0) should return 'ES12.7'")

        ! teardown
        call desc%destruct()
    end subroutine real_sci_form_returns_ESwm_when_passed_w_m_0

    subroutine real_sci_form_returns_default_spec_when_passed_no_argument()
        implicit none
        class(real_edit_descriptor_type), allocatable :: desc

        desc = real(sci_form) ! 7+1+len("-0.E+")
        call assert_equal(desc%get(), "ES13.7E1", &
                          "real(sci_form) should return 'ES13.7E1'")

        ! teardown
        call desc%destruct()
    end subroutine real_sci_form_returns_default_spec_when_passed_no_argument

    subroutine real_eng_form_returns_ENwmEe_when_passed_w_m_e()
        implicit none
        class(real_edit_descriptor_type), allocatable :: desc

        desc = real(eng_form, 14, 6, 1)
        call assert_equal(desc%get(), "EN14.6E1", &
                          "real(eng_form, 14, 6, 1) should return 'EN14.6E1'")

        desc = real(eng_form, 40, 0, 3)
        call assert_equal(desc%get(), "EN40.0E3", &
                          "real(eng_form, 40, 0, 3) should return 'EN40.0E3'")

        desc = real(eng_form, 10, -1, 1) ! 7+1+len("-000.E+")
        call assert_equal(desc%get(), "EN15.7E1", &
                          "real(eng_form, 10, -1, 1) should return 'EN15.7E1'")

        desc = real(eng_form, 12, 3, -1) ! 3+1+len("-000.E+")
        call assert_equal(desc%get(), "EN12.3E1", &
                          "real(eng_form, 12, 3, -1) should return 'EN12.3E1'")

        desc = real(eng_form, 6, -1, -1) ! 7+1+len("-000.E+")
        call assert_equal(desc%get(), "EN15.7E1", &
                          "real(eng_form, 6, -1, -1) should return 'EN15.7E1'")

        ! teardown
        call desc%destruct()
    end subroutine real_eng_form_returns_ENwmEe_when_passed_w_m_e

    subroutine real_eng_form_returns_ENwm_when_passed_w_m_0()
        implicit none
        class(real_edit_descriptor_type), allocatable :: desc

        desc = real(eng_form, 14, 6, 0)
        call assert_equal(desc%get(), "EN14.6", &
                          "real(eng_form, 14, 6, 0) should return 'EN14.6'")

        desc = real(eng_form, 40, 0, 0)
        call assert_equal(desc%get(), "EN40.0", &
                          "real(eng_form, 40, 0, 0) should return 'EN40.0'")

        desc = real(eng_form, 10, -1, 0) ! 7+0+len("-000.E+")
        call assert_equal(desc%get(), "EN14.7", &
                          "real(eng_form, 10, -1, 0) should return 'EN14.7'")

        ! teardown
        call desc%destruct()
    end subroutine real_eng_form_returns_ENwm_when_passed_w_m_0

    subroutine real_eng_form_returns_default_spec_when_passed_no_argument()
        implicit none
        class(real_edit_descriptor_type), allocatable :: desc

        desc = real(eng_form) ! 7+1+len("-000.E+")
        call assert_equal(desc%get(), "EN15.7E1", &
                          "real(eng_form) should return 'EN15.7E1'")

        ! teardown
        call desc%destruct()
    end subroutine real_eng_form_returns_default_spec_when_passed_no_argument
end program test_real_data_descriptor
