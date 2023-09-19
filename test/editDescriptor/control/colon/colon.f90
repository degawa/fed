program test_colon_edit_descriptor
    use :: fed_editDescriptor_control_colon
    use :: fed_editDescriptor
    use :: fassert
    implicit none

    print '(A)', "# Testing: colon edit descriptor"
    call terminate_returns_colon_edit_desc_instance()
    call terminate_returns_colon()

contains
    subroutine terminate_returns_colon_edit_desc_instance()
        implicit none
        class(edit_descriptor_type), allocatable :: desc
        type(colon_edit_descriptor_type) :: type_mold

        ! test
        allocate (desc, source=terminate())
        call assert_true(same_type_as(desc, type_mold), &
                         'terminate() should return `colon_edit_descriptor_type` instance')

        ! teardown
        deallocate (desc)
    end subroutine terminate_returns_colon_edit_desc_instance

    subroutine terminate_returns_colon()
        implicit none
        type(colon_edit_descriptor_type) :: desc

        ! test
        desc = terminate()
        call assert_equal(desc%get(), ':', &
                          'terminate() should return ":"')

        ! teardown
        call desc%destruct()
    end subroutine terminate_returns_colon
end program test_colon_edit_descriptor
