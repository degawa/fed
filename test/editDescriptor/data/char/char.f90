program test_char_data_descriptor
    use :: fed_editDescriptor_data_char
    use :: fed_editDescriptor
    use :: fassert
    implicit none

    print '(A)', "# Testing: char_edit_descriptor_type"
    call constructor_returns_char_edit_desc_instance()
    call char_returns_char_edit_desc_wo_width()
    call char_width_returns_char_edit_desc_w_width()

contains
    subroutine constructor_returns_char_edit_desc_instance()
        implicit none
        class(edit_descriptor_type), allocatable :: desc
        type(char_edit_descriptor_type) :: type_mold

        ! test
        allocate (desc, source=char())
        call assert_true(same_type_as(desc, type_mold), &
                         "char() should return `char_edit_descriptor_type` instance")

        ! teardown
        deallocate (desc)

        ! test
        allocate (desc, source=char(width=4))
        call assert_true(same_type_as(desc, type_mold), &
                         "char(width) should return `char_edit_descriptor_type` instance")

        ! teardown
        deallocate (desc)
    end subroutine constructor_returns_char_edit_desc_instance

    subroutine char_returns_char_edit_desc_wo_width()
        implicit none
        type(char_edit_descriptor_type) :: desc

        ! test
        desc = char()
        call assert_equal(desc%get(), "A", &
                          "char() should return 'A'" &
                          //" (char edit descriptor wthout width)")
    end subroutine char_returns_char_edit_desc_wo_width

    subroutine char_width_returns_char_edit_desc_w_width()
        implicit none
        type(char_edit_descriptor_type) :: desc

        ! test
        desc = char(27)
        call assert_equal(desc%get(), "A27", &
                          "char(width) should return 'A'//'<width>'" &
                          //" (char edit descriptor with width)")
    end subroutine char_width_returns_char_edit_desc_w_width
end program test_char_data_descriptor
