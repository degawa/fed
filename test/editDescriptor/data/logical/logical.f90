program test_logical_data_descriptor
    use :: fed_editDescriptor_data_logical
    use :: fed_editDescriptor
    use :: fassert
    implicit none

    print '(A)', "# Testing: logical_edit_descriptor_type"
    call constructor_returns_logical_edit_desc_instance()
    call logical_returns_logical_edit_desc_wo_width()
    call logical_width_returns_logical_edit_desc_w_width()

contains
    subroutine constructor_returns_logical_edit_desc_instance()
        implicit none
        class(edit_descriptor_type), allocatable :: desc
        type(logical_edit_descriptor_type) :: type_mold

        ! test
        allocate (desc, source=logical())
        call assert_true(same_type_as(desc, type_mold), &
                         "logical() should return `logical_edit_descriptor_type` instance")

        ! teardown
        deallocate (desc)

        ! test
        allocate (desc, source=logical(width=4))
        call assert_true(same_type_as(desc, type_mold), &
                         "logical(width) should return `logical_edit_descriptor_type` instance")

        ! teardown
        deallocate (desc)
    end subroutine constructor_returns_logical_edit_desc_instance

    subroutine logical_returns_logical_edit_desc_wo_width()
        implicit none
        type(logical_edit_descriptor_type) :: desc

        ! test
        desc = logical()
        call assert_equal(desc%get(), "L1", &
                          "logical() should return 'L1'" &
                          //" (logical edit descriptor wthout width)")
    end subroutine logical_returns_logical_edit_desc_wo_width

    subroutine logical_width_returns_logical_edit_desc_w_width()
        implicit none
        type(logical_edit_descriptor_type) :: desc

        ! test
        desc = logical(8)
        call assert_equal(desc%get(), "L8", &
                          "logical(width) should return 'L'//'<width>'" &
                          //" (logical edit descriptor with width)")
    end subroutine logical_width_returns_logical_edit_desc_w_width
end program test_logical_data_descriptor
