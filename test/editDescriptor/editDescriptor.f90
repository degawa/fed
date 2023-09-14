program test_editDescriptor
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor
    use :: fassert
    implicit none

    print '(A)', "# Testing: edit_descritpor_type"
    call get_returns_zero_length_string_when_no_desc_is_set()
    call get_returns_desc_when_desc_is_set()
    call destruct_deallocates_component_desc()
    call constructor_returns_edit_descriptor_type_instance()

contains
    subroutine get_returns_zero_length_string_when_no_desc_is_set()
        implicit none
        type(edit_descriptor_type) :: desc

        ! test
        call assert_equal(desc%get(), "", &
                          "get() should return zero-length string when no descriptor is set")
    end subroutine get_returns_zero_length_string_when_no_desc_is_set

    subroutine get_returns_desc_when_desc_is_set()
        implicit none
        type(edit_descriptor_type) :: desc

        ! setup
        call desc%set("descriptor")

        ! test
        call assert_equal(desc%get(), "descriptor", &
                          "get() should return descriptor as string when descriptor is set")
    end subroutine get_returns_desc_when_desc_is_set

    subroutine destruct_deallocates_component_desc()
        implicit none
        type(edit_descriptor_type) :: desc

        ! setup
        call desc%set("test_for_destruct")
        call assert_equal(desc%get(), "test_for_destruct", &
                          "destract() should deallocate the component `desc`"// &
                          " [pre-test to confirm if the descriptor is set]")

        !test
        call desc%destruct()

        call assert_equal(desc%get(), "", &
                          "destract() should deallocate the component `desc`")
    end subroutine destruct_deallocates_component_desc

    subroutine constructor_returns_edit_descriptor_type_instance()
        implicit none
        class(edit_descriptor_type), allocatable :: desc
        type(edit_descriptor_type) :: type_mold

        ! test
        allocate (desc, source=edit_descriptor_type("test_for_constructor"))
        call assert_true(same_type_as(desc, type_mold), &
                         "constructor should return `edit_descriptor_type` instance"// &
                         " [type check]")

        call assert_equal(desc%get(), "test_for_constructor", &
                          "constructor should return `edit_descriptor_type` instance"// &
                          " [component `desc` check]")

        ! teardown
        deallocate (desc)
    end subroutine constructor_returns_edit_descriptor_type_instance
end program test_editDescriptor
