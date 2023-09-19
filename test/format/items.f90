program test_format_items
    use :: fed_format_items
    use :: fed_format_item
    use :: fed_editDescriptor
    use :: fed_editDescriptor_characterString
    use :: fassert
    implicit none

    print '(A)', "# Testing: format_items_type"
    call construct_returns_format_items_type_instance()
    call get_num_items_returns_number_of_contained_items()
    call destruct_unset_format_items()
    call get_edit_desc_returns_edit_desc_for_item_w_specified_num()
    call cat_op_returns_items_containing_lhs_rhs()
    call is_data_edit_desc_returns_true_when_item_contains_data()
    call is_data_edit_desc_returns_false_when_desc_item_not_contain_data()
    call is_ctrl_edit_desc_returns_true_when_item_contains_ctrl()
    call is_ctrl_edit_desc_returns_false_when_desc_item_not_contain_ctrl()
    call is_str_edit_desc_returns_true_when_item_contains_str()
    call is_str_edit_desc_returns_false_when_desc_item_not_contain_str()
    call has_data_desc_returns_true_when_itmes_contain_data_desc()
    call has_data_desc_returns_false_when_itmes_not_contain_data_desc()

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
        type(format_item_type) :: itm_lhs, itm_rhs
        type(format_items_type) :: itms_lhs, itms_rhs

        !--- desc//desc
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

        !--- desc//item
        ! setup
        desc_lhs = edit_descriptor_type("descriptor at lhs")
        itm_rhs = item(edit_descriptor_type("format item at rhs"))

        ! test
        itms = desc_lhs//itm_rhs
        call assert_equal(itms%get_number_of_items(), 2, &
                          "num of items in the result of desc//item op is the sum of the num of items " &
                          //"on the left and right side of //")
        call assert_equal(itms%get_edit_descriptor_at(1), "descriptor at lhs", &
                          "edit descriptor for item no.1 in the result of desc//item op is the same as that of the lhs")
        call assert_equal(itms%get_edit_descriptor_at(2), "format item at rhs", &
                          "edit descriptor for item no.2 in the result of desc//item op is the same as that of the rhs")

        ! teardown
        call itms%destruct()
        call desc_lhs%destruct()
        call itm_rhs%destruct()

        !--- desc//items
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

        !--- desc//char
        ! setup
        desc_lhs = edit_descriptor_type("descriptor at lhs")

        ! test
        itms = desc_lhs//"string at rhs"
        call assert_equal(itms%get_number_of_items(), 2, &
                          "num of items in the result of desc//char op is the sum of the num of items " &
                          //"on the left and right side of //")
        call assert_equal(itms%get_edit_descriptor_at(1), "descriptor at lhs", &
                          "edit descriptor for item no.1 in the result of desc//char op is the same as that of the lhs")
        call assert_equal(itms%get_edit_descriptor_at(2), '"string at rhs"', &
                          "edit descriptor for item no.2 in the result of desc//char op is the same as that of the rhs")

        ! teardown
        call itms%destruct()
        call desc_lhs%destruct()

        !--- items//desc
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

        !--- items//item
        ! setup
        itms_lhs = items(edit_descriptor_type("format item at lhs"))
        itm_rhs = item(edit_descriptor_type("format item at rhs"))

        ! test
        itms = itms_lhs//itm_rhs
        call assert_equal(itms%get_number_of_items(), 2, &
                          "num of items in the result of items//item op is the sum of the num of items " &
                          //"on the left and right side of //")
        call assert_equal(itms%get_edit_descriptor_at(1), "format item at lhs", &
                          "edit descriptor for item no.1 in the result of items//item op is the same as that of the lhs")
        call assert_equal(itms%get_edit_descriptor_at(2), "format item at rhs", &
                          "edit descriptor for item no.2 in the result of items//item op is the same as that of the rhs")

        ! teardown
        call itms%destruct()
        call itms_lhs%destruct()
        call itm_rhs%destruct()

        !--- items//items
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

        !--- items//char
        ! setup
        itms_lhs = items(edit_descriptor_type("format item at lhs"))

        ! test
        itms = itms_lhs//"string at rhs"
        call assert_equal(itms%get_number_of_items(), 2, &
                          "num of items in the result of items//char op is the sum of the num of items " &
                          //"on the left and right side of //")
        call assert_equal(itms%get_edit_descriptor_at(1), "format item at lhs", &
                          "edit descriptor for item no.1 in the result of items//char op is the same as that of the lhs")
        call assert_equal(itms%get_edit_descriptor_at(2), '"string at rhs"', &
                          "edit descriptor for item no.2 in the result of items//char op is the same as that of the rhs")

        ! teardown
        call itms%destruct()
        call itms_lhs%destruct()
        call itms_rhs%destruct()

        !--- item//desc
        ! setup
        itm_lhs = item(edit_descriptor_type("format item at lhs"))
        desc_rhs = edit_descriptor_type("descriptor at rhs")

        ! test
        itms = itm_lhs//desc_rhs
        call assert_equal(itms%get_number_of_items(), 2, &
                          "num of items in the result of item//desc op is the sum of the num of items " &
                          //"on the left and right side of //")
        call assert_equal(itms%get_edit_descriptor_at(1), "format item at lhs", &
                          "edit descriptor for item no.1 in the result of item//desc op is the same as that of the lhs")
        call assert_equal(itms%get_edit_descriptor_at(2), "descriptor at rhs", &
                          "edit descriptor for item no.2 in the result of item//desc op is the same as that of the rhs")

        ! teardown
        call itms%destruct()
        call itm_lhs%destruct()
        call desc_rhs%destruct()

        !--- item//item
        ! setup
        itm_lhs = item(edit_descriptor_type("format item at lhs"))
        itm_rhs = item(edit_descriptor_type("format item at rhs"))

        ! test
        itms = itm_lhs//itm_rhs
        call assert_equal(itms%get_number_of_items(), 2, &
                          "num of items in the result of item//item op is the sum of the num of items " &
                          //"on the left and right side of //")
        call assert_equal(itms%get_edit_descriptor_at(1), "format item at lhs", &
                          "edit descriptor for item no.1 in the result of item//item op is the same as that of the lhs")
        call assert_equal(itms%get_edit_descriptor_at(2), "format item at rhs", &
                          "edit descriptor for item no.2 in the result of item//item op is the same as that of the rhs")

        ! teardown
        call itms%destruct()
        call itm_lhs%destruct()
        call itm_rhs%destruct()

        !--- item//items
        ! setup
        itm_lhs = item(edit_descriptor_type("format item at lhs"))
        itms_rhs = items(edit_descriptor_type("format item at rhs"))

        ! test
        itms = itm_lhs//itms_rhs
        call assert_equal(itms%get_number_of_items(), 2, &
                          "num of items in the result of item//items op is the sum of the num of items " &
                          //"on the left and right side of //")
        call assert_equal(itms%get_edit_descriptor_at(1), "format item at lhs", &
                          "edit descriptor for item no.1 in the result of item//items op is the same as that of the lhs")
        call assert_equal(itms%get_edit_descriptor_at(2), "format item at rhs", &
                          "edit descriptor for item no.2 in the result of item//items op is the same as that of the rhs")

        ! teardown
        call itms%destruct()
        call itm_lhs%destruct()
        call itms_rhs%destruct()

        !--- item//char
        ! setup
        itm_lhs = item(edit_descriptor_type("format item at lhs"))

        ! test
        itms = itm_lhs//"string at rhs"
        call assert_equal(itms%get_number_of_items(), 2, &
                          "num of items in the result of item//char op is the sum of the num of items " &
                          //"on the left and right side of //")
        call assert_equal(itms%get_edit_descriptor_at(1), "format item at lhs", &
                          "edit descriptor for item no.1 in the result of item//char op is the same as that of the lhs")
        call assert_equal(itms%get_edit_descriptor_at(2), '"string at rhs"', &
                          "edit descriptor for item no.2 in the result of item//char op is the same as that of the rhs")

        ! teardown
        call itms%destruct()
        call itm_lhs%destruct()

        !--- char//desc
        ! setup
        desc_rhs = edit_descriptor_type("descriptor at rhs")

        ! test
        itms = "string at lhs"//desc_rhs
        call assert_equal(itms%get_number_of_items(), 2, &
                          "num of items in the result of char//desc op is the sum of the num of items " &
                          //"on the left and right side of //")
        call assert_equal(itms%get_edit_descriptor_at(1), '"string at lhs"', &
                          "edit descriptor for item no.1 in the result of char//desc op is the same as that of the lhs")
        call assert_equal(itms%get_edit_descriptor_at(2), "descriptor at rhs", &
                          "edit descriptor for item no.2 in the result of char//desc op is the same as that of the rhs")

        ! teardown
        call itms%destruct()
        call desc_rhs%destruct()

        !--- char//item
        ! setup
        itm_rhs = item(edit_descriptor_type("format item at lhs"))

        ! test
        itms = "string at lhs"//itm_rhs
        call assert_equal(itms%get_number_of_items(), 2, &
                          "num of items in the result of char//item op is the sum of the num of items " &
                          //"on the left and right side of //")
        call assert_equal(itms%get_edit_descriptor_at(1), '"string at lhs"', &
                          "edit descriptor for item no.1 in the result of char//item op is the same as that of the lhs")
        call assert_equal(itms%get_edit_descriptor_at(2), "format item at lhs", &
                          "edit descriptor for item no.2 in the result of char//item op is the same as that of the rhs")

        ! teardown
        call itms%destruct()
        call itm_rhs%destruct()

        !--- char//items
        ! setup
        itms_rhs = items(edit_descriptor_type("format item at lhs"))

        ! test
        itms = "string at lhs"//itms_rhs
        call assert_equal(itms%get_number_of_items(), 2, &
                          "num of items in the result of char//items op is the sum of the num of items " &
                          //"on the left and right side of //")
        call assert_equal(itms%get_edit_descriptor_at(1), '"string at lhs"', &
                          "edit descriptor for item no.1 in the result of char//items op is the same as that of the lhs")
        call assert_equal(itms%get_edit_descriptor_at(2), "format item at lhs", &
                          "edit descriptor for item no.2 in the result of char//items op is the same as that of the rhs")

        ! teardown
        call itms%destruct()
        call itms_rhs%destruct()

        !--- items//items
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

    subroutine is_data_edit_desc_returns_true_when_item_contains_data()
        use :: fed_editDescriptor_data
        use :: fed_editDescriptor_characterString
        use :: fed_editDescriptor_control
        implicit none
        type(format_items_type) :: itms

        ! setup
        itms = item(dat(""))//item(str(""))//item(ctrl(""))//item(str(""))//item(ctrl(""))//item(dat(""))

        ! test
        call assert_true(itms%is_data_edit_descriptor(1), &
                         "is_data_edit_descriptor(i) should return `.true.` "// &
                         "when component `item(i)` contains `data_edit_descriptor_type`")

        call assert_true(itms%is_data_edit_descriptor(6), &
                         "is_data_edit_descriptor(i) should return `.true.` "// &
                         "when component `item(i)` contains `data_edit_descriptor_type`")

    end subroutine is_data_edit_desc_returns_true_when_item_contains_data

    subroutine is_data_edit_desc_returns_false_when_desc_item_not_contain_data()
        use :: fed_editDescriptor_data
        use :: fed_editDescriptor_characterString
        use :: fed_editDescriptor_control
        implicit none
        type(format_items_type) :: itms

        ! setup
        itms = item(dat(""))//item(str(""))//item(ctrl(""))//item(str(""))//item(ctrl(""))//item(dat(""))

        ! test
        call assert_false(itms%is_data_edit_descriptor(2), &
                          "is_data_edit_descriptor(i) should return `.false.` "// &
                          "when component `item(i)` does not contain `data_edit_descriptor_type`")

        call assert_false(itms%is_data_edit_descriptor(3), &
                          "is_data_edit_descriptor(i) should return `.false.` "// &
                          "when component `item(i)` does not contain `data_edit_descriptor_type`")

        call assert_false(itms%is_data_edit_descriptor(4), &
                          "is_data_edit_descriptor(i) should return `.false.` "// &
                          "when component `item(i)` does not contain `data_edit_descriptor_type`")

        call assert_false(itms%is_data_edit_descriptor(5), &
                          "is_data_edit_descriptor(i) should return `.false.` "// &
                          "when component `item(i)` does not contain `data_edit_descriptor_type`")
    end subroutine is_data_edit_desc_returns_false_when_desc_item_not_contain_data

    subroutine is_ctrl_edit_desc_returns_true_when_item_contains_ctrl()
        use :: fed_editDescriptor_data
        use :: fed_editDescriptor_characterString
        use :: fed_editDescriptor_control
        implicit none
        type(format_items_type) :: itms

        ! setup
        itms = item(dat(""))//item(str(""))//item(ctrl(""))//item(str(""))//item(ctrl(""))//item(dat(""))

        ! test
        call assert_true(itms%is_control_edit_descriptor(3), &
                         "is_control_edit_descriptor(i) should return `.true.` "// &
                         "when component `item(i)` contains `control_edit_descriptor_type`")

        call assert_true(itms%is_control_edit_descriptor(5), &
                         "is_control_edit_descriptor(i) should return `.true.` "// &
                         "when component `item(i)` contains `control_edit_descriptor_type`")

    end subroutine is_ctrl_edit_desc_returns_true_when_item_contains_ctrl

    subroutine is_ctrl_edit_desc_returns_false_when_desc_item_not_contain_ctrl()
        use :: fed_editDescriptor_data
        use :: fed_editDescriptor_characterString
        use :: fed_editDescriptor_control
        implicit none
        type(format_items_type) :: itms

        ! setup
        itms = item(dat(""))//item(str(""))//item(ctrl(""))//item(str(""))//item(ctrl(""))//item(dat(""))

        ! test
        call assert_false(itms%is_control_edit_descriptor(1), &
                          "is_control_edit_descriptor(i) should return `.false.` "// &
                          "when component `item(i)` does not contain `control_edit_descriptor_type`")

        call assert_false(itms%is_control_edit_descriptor(2), &
                          "is_control_edit_descriptor(i) should return `.false.` "// &
                          "when component `item(i)` does not contain `control_edit_descriptor_type`")

        call assert_false(itms%is_control_edit_descriptor(4), &
                          "is_control_edit_descriptor(i) should return `.false.` "// &
                          "when component `item(i)` does not contain `control_edit_descriptor_type`")

        call assert_false(itms%is_control_edit_descriptor(6), &
                          "is_control_edit_descriptor(i) should return `.false.` "// &
                          "when component `item(i)` does not contain `control_edit_descriptor_type`")
    end subroutine is_ctrl_edit_desc_returns_false_when_desc_item_not_contain_ctrl

    subroutine is_str_edit_desc_returns_true_when_item_contains_str()
        use :: fed_editDescriptor_data
        use :: fed_editDescriptor_characterString
        use :: fed_editDescriptor_control
        implicit none
        type(format_items_type) :: itms

        ! setup
        itms = item(dat(""))//item(str(""))//item(ctrl(""))//item(str(""))//item(ctrl(""))//item(dat(""))

        ! test
        call assert_true(itms%is_character_string_edit_descriptor(2), &
                         "is_character_string_edit_descriptor(i) should return `.true.` "// &
                         "when component `item(i)` contains `character_string_edit_descriptor_type`")

        call assert_true(itms%is_character_string_edit_descriptor(4), &
                         "is_character_string_edit_descriptor(i) should return `.true.` "// &
                         "when component `item(i)` contains `character_string_edit_descriptor_type`")

    end subroutine is_str_edit_desc_returns_true_when_item_contains_str

    subroutine is_str_edit_desc_returns_false_when_desc_item_not_contain_str()
        use :: fed_editDescriptor_data
        use :: fed_editDescriptor_characterString
        use :: fed_editDescriptor_control
        implicit none
        type(format_items_type) :: itms

        ! setup
        itms = item(dat(""))//item(str(""))//item(ctrl(""))//item(str(""))//item(ctrl(""))//item(dat(""))

        ! test
        call assert_false(itms%is_character_string_edit_descriptor(1), &
                          "is_character_string_edit_descriptor(i) should return `.false.` "// &
                          "when component `item(i)` does not contain `character_string_edit_descriptor_type`")

        call assert_false(itms%is_character_string_edit_descriptor(3), &
                          "is_character_string_edit_descriptor(i) should return `.false.` "// &
                          "when component `item(i)` does not contain `character_string_edit_descriptor_type`")

        call assert_false(itms%is_character_string_edit_descriptor(5), &
                          "is_character_string_edit_descriptor(i) should return `.false.` "// &
                          "when component `item(i)` does not contain `character_string_edit_descriptor_type`")

        call assert_false(itms%is_character_string_edit_descriptor(6), &
                          "is_character_string_edit_descriptor(i) should return `.false.` "// &
                          "when component `item(i)` does not contain `character_string_edit_descriptor_type`")
    end subroutine is_str_edit_desc_returns_false_when_desc_item_not_contain_str

    subroutine has_data_desc_returns_true_when_itmes_contain_data_desc()
        use :: fed_editDescriptor_data
        use :: fed_editDescriptor_control
        use :: fed_editDescriptor_characterString
        implicit none
        type(format_items_type) :: itms

        ! setup
        itms = items(dat("desc"))
        ! test
        call assert_true(itms%has_data_edit_descriptor(), &
                         '`has_data_edit_desciptor` returns `.true.` when items are dat')
        ! teardown
        call itms%destruct()

        ! setup
        itms = dat("desc")//dat("desc")
        ! test
        call assert_true(itms%has_data_edit_descriptor(), &
                         '`has_data_edit_desciptor` returns `.true.` when items are dat//dat')
        ! teardown
        call itms%destruct()

        ! setup
        itms = dat("desc")//str("desc")
        ! test
        call assert_true(itms%has_data_edit_descriptor(), &
                         '`has_data_edit_desciptor` returns `.true.` when items are dat//str')
        ! teardown
        call itms%destruct()

        ! setup
        itms = dat("desc")//ctrl("desc")
        ! test
        call assert_true(itms%has_data_edit_descriptor(), &
                         '`has_data_edit_desciptor` returns `.true.` when items are dat//ctrl')
        ! teardown
        call itms%destruct()

        ! setup
        itms = str("desc")//dat("desc")
        ! test
        call assert_true(itms%has_data_edit_descriptor(), &
                         '`has_data_edit_desciptor` returns `.true.` when items are str//dat')
        ! teardown
        call itms%destruct()

        ! setup
        itms = ctrl("desc")//dat("desc")
        ! test
        call assert_true(itms%has_data_edit_descriptor(), &
                         '`has_data_edit_desciptor` returns `.true.` when items are ctrl//dat')
        ! teardown
        call itms%destruct()

        ! setup
        itms = dat("desc")//dat("desc")//dat("desc")
        ! test
        call assert_true(itms%has_data_edit_descriptor(), &
                         '`has_data_edit_desciptor` returns `.true.` when items are dat//dat//dat')
        ! teardown
        call itms%destruct()

        ! setup
        itms = dat("desc")//dat("desc")//str("desc")
        ! test
        call assert_true(itms%has_data_edit_descriptor(), &
                         '`has_data_edit_desciptor` returns `.true.` when items are dat//dat//str')
        ! teardown
        call itms%destruct()

        ! setup
        itms = dat("desc")//dat("desc")//ctrl("desc")
        ! test
        call assert_true(itms%has_data_edit_descriptor(), &
                         '`has_data_edit_desciptor` returns `.true.` when items are dat//dat//ctrl')
        ! teardown
        call itms%destruct()

        ! setup
        itms = dat("desc")//str("desc")//dat("desc")
        ! test
        call assert_true(itms%has_data_edit_descriptor(), &
                         '`has_data_edit_desciptor` returns `.true.` when items are dat//str//dat')
        ! teardown
        call itms%destruct()

        ! setup
        itms = dat("desc")//str("desc")//str("desc")
        ! test
        call assert_true(itms%has_data_edit_descriptor(), &
                         '`has_data_edit_desciptor` returns `.true.` when items are dat//str//str')
        ! teardown
        call itms%destruct()

        ! setup
        itms = dat("desc")//ctrl("desc")//dat("desc")
        ! test
        call assert_true(itms%has_data_edit_descriptor(), &
                         '`has_data_edit_desciptor` returns `.true.` when items are dat//ctrl//dat')
        ! teardown
        call itms%destruct()

        ! setup
        itms = dat("desc")//ctrl("desc")//str("desc")
        ! test
        call assert_true(itms%has_data_edit_descriptor(), &
                         '`has_data_edit_desciptor` returns `.true.` when items are dat//ctrl//str')
        ! teardown
        call itms%destruct()

        ! setup
        itms = dat("desc")//ctrl("desc")//ctrl("desc")
        ! test
        call assert_true(itms%has_data_edit_descriptor(), &
                         '`has_data_edit_desciptor` returns `.true.` when items are dat//ctrl//ctrl')
        ! teardown
        call itms%destruct()

        ! setup
        itms = str("desc")//dat("desc")//dat("desc")
        ! test
        call assert_true(itms%has_data_edit_descriptor(), &
                         '`has_data_edit_desciptor` returns `.true.` when items are str//dat//dat')
        ! teardown
        call itms%destruct()

        ! setup
        itms = str("desc")//dat("desc")//str("desc")
        ! test
        call assert_true(itms%has_data_edit_descriptor(), &
                         '`has_data_edit_desciptor` returns `.true.` when items are str//dat//str')
        ! teardown
        call itms%destruct()

        ! setup
        itms = str("desc")//dat("desc")//ctrl("desc")
        ! test
        call assert_true(itms%has_data_edit_descriptor(), &
                         '`has_data_edit_desciptor` returns `.true.` when items are str//dat//ctrl')
        ! teardown
        call itms%destruct()

        ! setup
        itms = str("desc")//str("desc")//dat("desc")
        ! test
        call assert_true(itms%has_data_edit_descriptor(), &
                         '`has_data_edit_desciptor` returns `.true.` when items are str//str//dat')
        ! teardown
        call itms%destruct()

        ! setup
        itms = str("desc")//ctrl("desc")//dat("desc")
        ! test
        call assert_true(itms%has_data_edit_descriptor(), &
                         '`has_data_edit_desciptor` returns `.true.` when items are str//ctrl//dat')
        ! teardown
        call itms%destruct()

        ! setup
        itms = ctrl("desc")//dat("desc")//dat("desc")
        ! test
        call assert_true(itms%has_data_edit_descriptor(), &
                         '`has_data_edit_desciptor` returns `.true.` when items are ctrl//dat//dat')
        ! teardown
        call itms%destruct()

        ! setup
        itms = ctrl("desc")//dat("desc")//str("desc")
        ! test
        call assert_true(itms%has_data_edit_descriptor(), &
                         '`has_data_edit_desciptor` returns `.true.` when items are ctrl//dat//str')
        ! teardown
        call itms%destruct()

        ! setup
        itms = ctrl("desc")//dat("desc")//ctrl("desc")
        ! test
        call assert_true(itms%has_data_edit_descriptor(), &
                         '`has_data_edit_desciptor` returns `.true.` when items are ctrl//dat//ctrl')
        ! teardown
        call itms%destruct()

        ! setup
        itms = ctrl("desc")//str("desc")//dat("desc")
        ! test
        call assert_true(itms%has_data_edit_descriptor(), &
                         '`has_data_edit_desciptor` returns `.true.` when items are ctrl//str//dat')
        ! teardown
        call itms%destruct()

        ! setup
        itms = ctrl("desc")//ctrl("desc")//dat("desc")
        ! test
        call assert_true(itms%has_data_edit_descriptor(), &
                         '`has_data_edit_desciptor` returns `.true.` when items are ctrl//ctrl//dat')
        ! teardown
        call itms%destruct()
    end subroutine has_data_desc_returns_true_when_itmes_contain_data_desc

    subroutine has_data_desc_returns_false_when_itmes_not_contain_data_desc()
        use :: fed_editDescriptor_data
        use :: fed_editDescriptor_control
        use :: fed_editDescriptor_characterString
        implicit none
        type(format_items_type) :: itms

        ! setup
        itms = items(str("desc"))
        ! test
        call assert_false(itms%has_data_edit_descriptor(), &
                          '`has_data_edit_desciptor` returns `.false.` when items are str')
        ! teardown
        call itms%destruct()

        ! setup
        itms = items(ctrl("desc"))
        ! test
        call assert_false(itms%has_data_edit_descriptor(), &
                          '`has_data_edit_desciptor` returns `.false.` when items are ctrl')
        ! teardown
        call itms%destruct()

        ! setup
        itms = str("desc")//str("desc")
        ! test
        call assert_false(itms%has_data_edit_descriptor(), &
                          '`has_data_edit_desciptor` returns `.false.` when items are str//str')
        ! teardown
        call itms%destruct()

        ! setup
        itms = str("desc")//ctrl("desc")
        ! test
        call assert_false(itms%has_data_edit_descriptor(), &
                          '`has_data_edit_desciptor` returns `.false.` when items are str//ctrl')
        ! teardown
        call itms%destruct()

        ! setup
        itms = ctrl("desc")//str("desc")
        ! test
        call assert_false(itms%has_data_edit_descriptor(), &
                          '`has_data_edit_desciptor` returns `.false.` when items are ctrl//str')
        ! teardown
        call itms%destruct()

        ! setup
        itms = ctrl("desc")//ctrl("desc")
        ! test
        call assert_false(itms%has_data_edit_descriptor(), &
                          '`has_data_edit_desciptor` returns `.false.` when items are ctrl//ctrl')
        ! teardown
        call itms%destruct()

        ! setup
        itms = str("desc")//str("desc")//str("desc")
        ! test
        call assert_false(itms%has_data_edit_descriptor(), &
                          '`has_data_edit_desciptor` returns `.false.` when items are str//str//str')
        ! teardown
        call itms%destruct()

        ! setup
        itms = str("desc")//str("desc")//ctrl("desc")
        ! test
        call assert_false(itms%has_data_edit_descriptor(), &
                          '`has_data_edit_desciptor` returns `.false.` when items are str//str//ctrl')
        ! teardown
        call itms%destruct()

        ! setup
        itms = str("desc")//ctrl("desc")//str("desc")
        ! test
        call assert_false(itms%has_data_edit_descriptor(), &
                          '`has_data_edit_desciptor` returns `.false.` when items are str//ctrl//str')
        ! teardown
        call itms%destruct()

        ! setup
        itms = str("desc")//ctrl("desc")//ctrl("desc")
        ! test
        call assert_false(itms%has_data_edit_descriptor(), &
                          '`has_data_edit_desciptor` returns `.false.` when items are str//ctrl//ctrl')
        ! teardown
        call itms%destruct()

        ! setup
        itms = ctrl("desc")//str("desc")//str("desc")
        ! test
        call assert_false(itms%has_data_edit_descriptor(), &
                          '`has_data_edit_desciptor` returns `.false.` when items are ctrl//str//str')
        ! teardown
        call itms%destruct()

        ! setup
        itms = ctrl("desc")//str("desc")//ctrl("desc")
        ! test
        call assert_false(itms%has_data_edit_descriptor(), &
                          '`has_data_edit_desciptor` returns `.false.` when items are ctrl//str//ctrl')
        ! teardown
        call itms%destruct()

        ! setup
        itms = ctrl("desc")//ctrl("desc")//str("desc")
        ! test
        call assert_false(itms%has_data_edit_descriptor(), &
                          '`has_data_edit_desciptor` returns `.false.` when items are ctrl//ctrl//str')
        ! teardown
        call itms%destruct()

        ! setup
        itms = ctrl("desc")//ctrl("desc")//ctrl("desc")
        ! test
        call assert_false(itms%has_data_edit_descriptor(), &
                          '`has_data_edit_desciptor` returns `.false.` when items are ctrl//ctrl//ctrl')
        ! teardown
        call itms%destruct()
    end subroutine has_data_desc_returns_false_when_itmes_not_contain_data_desc
end program test_format_items
