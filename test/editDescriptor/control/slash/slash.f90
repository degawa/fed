program test_slash_edit_descriptor
    use :: fed_editDescriptor_control_slash
    use :: fed_editDescriptor
    use :: fassert
    implicit none

    print '(A)', "# Testing: slash edit descriptor"
    call end_line_returns_slash_edit_desc_instance()
    call end_line_returns_slash()

contains
    subroutine end_line_returns_slash_edit_desc_instance()
        implicit none
        class(edit_descriptor_type), allocatable :: desc
        type(slash_edit_descriptor_type) :: type_mold

        ! test
        allocate (desc, source=end_line())
        call assert_true(same_type_as(desc, type_mold), &
                         'end_line() should return `slash_edit_descriptor_type` instance')

        ! teardown
        deallocate (desc)
    end subroutine end_line_returns_slash_edit_desc_instance

    subroutine end_line_returns_slash()
        implicit none
        type(slash_edit_descriptor_type) :: desc

        ! test
        desc = end_line()
        call assert_equal(desc%get(), '/', &
                          'end_line() should return "/"')

        ! teardown
        call desc%destruct()
    end subroutine end_line_returns_slash
end program test_slash_edit_descriptor
