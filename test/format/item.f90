program test_format_item
    use :: fed_editDescriptor
    use :: fed_format_item
    use :: fassert
    implicit none

    print '(A)', "# Testing: format_item_type"
    call get_returns_zero_length_string_when_no_desc_is_set()
    call get_returns_desc_when_desc_is_set()
    call has_edit_desc_returns_false_when_no_edit_desc_is_set()
    call has_edit_desc_returns_true_when_edit_desc_is_set()
    call destruct_unset_edit_descr()
    call construct_returns_format_item_type_instance()

contains
    subroutine get_returns_zero_length_string_when_no_desc_is_set()
        implicit none
        type(format_item_type) :: itm

        ! test
        call assert_equal(itm%get_edit_descriptor(), "", &
                          "get_edit_descriptor() should return zero-length string when no descriptor is set")
    end subroutine get_returns_zero_length_string_when_no_desc_is_set

    subroutine get_returns_desc_when_desc_is_set()
        implicit none
        type(format_item_type) :: itm

        ! setup
        call itm%set(edit_descriptor_type("format-item"))

        ! test
        call assert_equal(itm%get_edit_descriptor(), "format-item", &
                          "get_edit_descriptor() should return descriptor as string when descriptor is set")
    end subroutine get_returns_desc_when_desc_is_set

    subroutine has_edit_desc_returns_false_when_no_edit_desc_is_set()
        implicit none
        type(format_item_type) :: itm

        ! test
        call assert_false(itm%has_edit_descriptor(), &
                          "has_edit_descriptor() should return `.false.` when component `edit_descriptor` is not allocated")
    end subroutine has_edit_desc_returns_false_when_no_edit_desc_is_set

    subroutine has_edit_desc_returns_true_when_edit_desc_is_set()
        implicit none
        type(format_item_type) :: itm

        ! setup
        call itm%set(edit_descriptor_type("has_edit_desc"))

        ! test
        call assert_true(itm%has_edit_descriptor(), &
                         "has_edit_descriptor() should return `.true.` when component `edit_descriptor` is allocated")
    end subroutine has_edit_desc_returns_true_when_edit_desc_is_set

    subroutine destruct_unset_edit_descr()
        implicit none
        type(format_item_type) :: itm

        ! setup
        call assert_false(itm%has_edit_descriptor(), &
                          "destruct() should unset the component `edit_descriptor`" &
                          //" [pre-test to confirm if the edit_descriptor is not set]")
        call itm%set(edit_descriptor_type("format-item-destruct"))
        call assert_true(itm%has_edit_descriptor(), &
                         "destruct() should unset the component `edit_descriptor`" &
                         //" [pre-test to confirm if the edit_descriptor is set]")

        ! test
        call itm%destruct()
        call assert_false(itm%has_edit_descriptor(), &
                          "destruct() should unset the component `edit_descriptor`")
    end subroutine destruct_unset_edit_descr

    subroutine construct_returns_format_item_type_instance()
        implicit none
        type(format_item_type) :: itm, type_mold

        ! test
        call assert_true(same_type_as(item(edit_descriptor_type("construct")), type_mold), &
                         "item(edit_descriptor) should return a format_item_type instance")

        itm = item(edit_descriptor_type("construct"))
        call assert_equal(itm%get_edit_descriptor(), "construct", &
                          "item(edit_descriptor) should return a format_item_type instance" &
                          //"containing the edit descriptor specified by the argument")
    end subroutine construct_returns_format_item_type_instance
end program test_format_item
