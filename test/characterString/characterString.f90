program test_characterString
    use :: fed_editDescriptor_characterString
    use :: fed_editDescriptor
    use :: fassert
    implicit none

    print '(A)', "# Testing: character edit descriptor"
    call constructor_returns_char_str_edit_desc_instance()
    call str_returns_string_enclosed_by_double_quate()

contains
    subroutine constructor_returns_char_str_edit_desc_instance()
        implicit none
        class(edit_descriptor_type), allocatable :: desc
        type(character_string_edit_descriptor_type) :: type_mold

        ! test
        allocate (desc, source=str("string"))
        call assert_true(same_type_as(desc, type_mold), &
                         'str("string") should return `real_standard_edit_descriptor_type` instance')

        ! teardown
        deallocate (desc)
    end subroutine constructor_returns_char_str_edit_desc_instance

    subroutine str_returns_string_enclosed_by_double_quate()
        implicit none
        type(character_string_edit_descriptor_type) :: desc

        ! test
        desc = str("character string")
        call assert_equal(desc%get(), '"character string"', &
                          "str('character string') should return "//'"character string"')

        ! teardown
        call desc%destruct()
    end subroutine str_returns_string_enclosed_by_double_quate
end program test_characterString
