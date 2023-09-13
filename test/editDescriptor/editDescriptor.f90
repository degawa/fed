program test_editDescriptor
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor
    use :: fassert
    implicit none

    call get_returns_zero_length_string_when_no_desc_is_set()
    call get_returns_desc_when_desc_is_set()
    call destruct_deallocates_component_desc()
    call constructor_returns_edit_descriptor_type_instance()

contains
    subroutine get_returns_zero_length_string_when_no_desc_is_set()
        implicit none
        type(edit_descriptor_type) :: desc
        character(:), allocatable :: desc_str

        ! test
        desc_str = desc%get()
        call assert_equal(desc_str, "", &
                          "get() should return zero-length string when no descriptor is set")

        ! teardown
        deallocate (desc_str)
    end subroutine get_returns_zero_length_string_when_no_desc_is_set

    subroutine get_returns_desc_when_desc_is_set()
        implicit none
        type(edit_descriptor_type) :: desc
        character(:), allocatable :: desc_str

        ! setup
        call desc%set("descriptor")

        ! test
        desc_str = desc%get()
        call assert_equal(desc_str, "descriptor", &
                          "get() should return descriptor as string when descriptor is set")

        ! teardown
        deallocate (desc_str)
    end subroutine get_returns_desc_when_desc_is_set

    subroutine destruct_deallocates_component_desc()
        implicit none
        type(edit_descriptor_type) :: desc
        character(:), allocatable :: desc_str

        ! setup
        call desc%set("test_for_destruct")
        desc_str = desc%get()
        call assert_equal(desc_str, "test_for_destruct", &
                          "destract() should deallocate the component `desc`"// &
                          " [pre-test to confirm if the descriptor is set]")

        !test
        call desc%destruct()

        desc_str = desc%get()
        call assert_equal(desc_str, "", &
                          "destract() should deallocate the component `desc`")

        ! teardown
        deallocate (desc_str)
    end subroutine destruct_deallocates_component_desc

    subroutine constructor_returns_edit_descriptor_type_instance()
        implicit none
        class(edit_descriptor_type), allocatable :: desc
        type(edit_descriptor_type) :: type_mold
        character(:), allocatable :: desc_str

        ! test
        allocate (desc, source=edit_descriptor_type("test_for_constructor"))
        call assert_true(same_type_as(desc, type_mold), &
                         "constructor should return `edit_descriptor_type` instance"// &
                         " [type check]")

        desc_str = desc%get()
        call assert_equal(desc_str, "test_for_constructor", &
                          "constructor should return `edit_descriptor_type` instance"// &
                          " [component `desc` check]")

        ! teardown
        deallocate (desc)
        deallocate (desc_str)
    end subroutine constructor_returns_edit_descriptor_type_instance
end program test_editDescriptor
