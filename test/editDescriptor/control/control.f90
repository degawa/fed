program test_control_descriptor
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor_control
    use :: fassert
    implicit none

    print '(A)', "# Testing: control edit descriptor"
    call constructor_returns_control_edit_descriptor_instance()

contains
    subroutine constructor_returns_control_edit_descriptor_instance()
        use :: fed_editDescriptor
        implicit none
        class(edit_descriptor_type), allocatable :: desc
        type(control_edit_descriptor_type) :: type_mold

        ! test
        allocate (desc, source=control_edit_descriptor_type(""))
        call assert_true(same_type_as(desc, type_mold), &
                         'control_edit_descriptor_type("") should return `control_edit_descriptor_type` instance')

        ! teardown
        deallocate (desc)

        ! test
        allocate (desc, source=ctrl(""))
        call assert_true(same_type_as(desc, type_mold), &
                         'ctrl("") should return `control_edit_descriptor_type` instance')

        ! teardown
        deallocate (desc)
    end subroutine constructor_returns_control_edit_descriptor_instance
end program test_control_descriptor
