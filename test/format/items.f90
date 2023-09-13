program test_format_items
    use :: fed_format_items
    use :: fed_format_item
    use :: fed_editDescriptor
    use :: fassert
    implicit none

    print '(A)', "# Testing: format_items_type"
    call construct_returns_format_items_type_instance()
    call get_num_items_returns_number_of_contained_items()
    call destruct_unset_format_items()
    call get_edit_desc_returns_edit_desc_for_item_w_specified_num()
    call cat_op_returns_items_containing_lhs_rhs()

contains
    subroutine construct_returns_format_items_type_instance()
        implicit none
        type(format_items_type) :: type_mold

        ! test
        call assert_true(same_type_as(items(edit_descriptor_type("constructor")), type_mold), &
                         "items(edit_descriptor) should return format_items_type instance")

        call assert_true(same_type_as(items(item(edit_descriptor_type("constructor"))), type_mold), &
                         "items(format_item) should return format_items_type instance")
    end subroutine construct_returns_format_items_type_instance

    subroutine get_num_items_returns_number_of_contained_items()
        implicit none
        type(format_items_type) :: itms

        ! setup
        itms = items(edit_descriptor_type(""))

        ! test
        call assert_equal(itms%get_number_of_items(), 1, &
                          "get_number_of_items(num) should return the number of items contained in a format_items_type")
    end subroutine get_num_items_returns_number_of_contained_items

    subroutine destruct_unset_format_items()
        implicit none
        type(format_items_type) :: itms

        ! setup
        itms = items(edit_descriptor_type(""))

        ! test
        call itms%destruct()
        call assert_equal(itms%get_number_of_items(), 0, &
                          "destruct() should unset the component `item`")
    end subroutine destruct_unset_format_items

    subroutine get_edit_desc_returns_edit_desc_for_item_w_specified_num()
        implicit none
        type(format_items_type) :: itms

        ! setup
        itms = items(edit_descriptor_type("No.1"))

        ! test
        call assert_equal(itms%get_edit_descriptor_at(1), "No.1", &
                          "get_edit_descriptor_at(num) should return edit descriptor for item with specified number")
        call assert_equal(itms%get_edit_descriptor_at(1), "No.1", &
                          "get_edit_descriptor_at(num) should return edit descriptor for item with specified number")

        call assert_equal(itms%get_edit_descriptor_at(0), "", &
                          "get_edit_descriptor_at(num) should return zero-length string when num is below the lower limit")
        call assert_equal(itms%get_edit_descriptor_at(2), "", &
                          "get_edit_descriptor_at(num) should return zero-length string when num is above the upper limit")
    end subroutine get_edit_desc_returns_edit_desc_for_item_w_specified_num

    subroutine cat_op_returns_items_containing_lhs_rhs()
        implicit none
        type(format_items_type) :: itms
        type(edit_descriptor_type) :: desc_lhs, desc_rhs
        type(format_items_type) :: itms_lhs, itms_rhs

        ! setup
        desc_lhs = edit_descriptor_type("descriptor at lhs")
        desc_rhs = edit_descriptor_type("descriptor at rhs")

        ! test
        itms = desc_lhs//desc_rhs
        call assert_equal(itms%get_number_of_items(), 2, &
                          "num of items in the result of desc//desc op is the sum of the num of items " &
                          //"on the left and right side of //")
        call assert_equal(itms%get_edit_descriptor_at(1), "descriptor at lhs", &
                          "edit descriptor for item no.1 in the result of desc//desc op is the same as that of the lhs")
        call assert_equal(itms%get_edit_descriptor_at(2), "descriptor at rhs", &
                          "edit descriptor for item no.2 in the result of desc//desc op is the same as that of the rhs")

        ! teardown
        call itms%destruct()
        call desc_lhs%destruct()
        call desc_rhs%destruct()

        ! setup
        itms_lhs = items(edit_descriptor_type("format item at lhs"))
        itms_rhs = items(edit_descriptor_type("format item at rhs"))

        ! test
        itms = itms_lhs//itms_rhs
        call assert_equal(itms%get_number_of_items(), 2, &
                          "num of items in the result of items//items op is the sum of the num of items " &
                          //"on the left and right side of //")
        call assert_equal(itms%get_edit_descriptor_at(1), "format item at lhs", &
                          "edit descriptor for item no.1 in the result of items//items op is the same as that of the lhs")
        call assert_equal(itms%get_edit_descriptor_at(2), "format item at rhs", &
                          "edit descriptor for item no.2 in the result of items//items op is the same as that of the rhs")

        ! teardown
        call itms%destruct()
        call itms_lhs%destruct()
        call itms_rhs%destruct()

        ! setup
        desc_lhs = edit_descriptor_type("descriptor at lhs")
        itms_rhs = items(edit_descriptor_type("format item at rhs"))

        ! test
        itms = desc_lhs//itms_rhs
        call assert_equal(itms%get_number_of_items(), 2, &
                          "num of items in the result of desc//items op is the sum of the num of items " &
                          //"on the left and right side of //")
        call assert_equal(itms%get_edit_descriptor_at(1), "descriptor at lhs", &
                          "edit descriptor for item no.1 in the result of desc//items op is the same as that of the lhs")
        call assert_equal(itms%get_edit_descriptor_at(2), "format item at rhs", &
                          "edit descriptor for item no.2 in the result of desc//items op is the same as that of the rhs")

        ! teardown
        call itms%destruct()
        call desc_lhs%destruct()
        call itms_rhs%destruct()

        ! setup
        itms_lhs = items(edit_descriptor_type("format item at lhs"))
        desc_rhs = edit_descriptor_type("descriptor at rhs")

        ! test
        itms = itms_lhs//desc_rhs
        call assert_equal(itms%get_number_of_items(), 2, &
                          "num of items in the result of items//desc op is the sum of the num of items " &
                          //"on the left and right side of //")
        call assert_equal(itms%get_edit_descriptor_at(1), "format item at lhs", &
                          "edit descriptor for item no.1 in the result of items//desc op is the same as that of the lhs")
        call assert_equal(itms%get_edit_descriptor_at(2), "descriptor at rhs", &
                          "edit descriptor for item no.2 in the result of items//desc op is the same as that of the rhs")

        ! teardown
        call itms%destruct()
        call itms_lhs%destruct()
        call desc_rhs%destruct()

        ! setup
        desc_lhs = edit_descriptor_type("descriptor1")
        desc_rhs = edit_descriptor_type("descriptor2")
        itms_lhs = items(edit_descriptor_type("format item1"))//desc_lhs
        itms_rhs = desc_rhs//items(edit_descriptor_type("format item2"))

        ! test
        itms = itms_lhs//itms_rhs
        call assert_equal(itms%get_number_of_items(), 4, &
                          "num of items in the result of (itms//desc)//(itms//desc) op is " &
                          //"the sum of the num of items on the left and right side of //")
        call assert_equal(itms%get_edit_descriptor_at(1), "format item1", &
                          "edit descriptor for item no.1 should be 'format item1'")
        call assert_equal(itms%get_edit_descriptor_at(2), "descriptor1", &
                          "edit descriptor for item no.2 should be 'descriptor1'")
        call assert_equal(itms%get_edit_descriptor_at(3), "descriptor2", &
                          "edit descriptor for item no.3 should be 'descriptor2'")
        call assert_equal(itms%get_edit_descriptor_at(4), "format item2", &
                          "edit descriptor for item no.4 should be 'format item2'")

        ! teardown
        call itms%destruct()
        call itms_lhs%destruct()
        call itms_rhs%destruct()
    end subroutine cat_op_returns_items_containing_lhs_rhs
end program test_format_items
