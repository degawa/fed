program test_repeat
    use :: fed_repeat
    use :: fed_format_items
    use :: fed_format_item
    use :: fed_editDescriptor
    use :: fassert
    implicit none

    print '(A)', "# Testing: repeat procedures"
    call construct_rep_fmt_itms_returns_repeated_fmt_item()
    call construct_rep_fmt_itms_w_sep_returns_repeated_fmt_item()
    call construct_rep_fmt_itms_by_desc_returns_repeated_fmt_item()
    call construct_rep_fmt_itms_w_sep_by_desc_returns_repeated_fmt_item()

contains
    subroutine construct_rep_fmt_itms_returns_repeated_fmt_item()
        implicit none
        type(format_items_type) :: itms
        type(format_item_type) :: rep_item

        ! setup
        itms = items(edit_descriptor_type("desc1"))
        ! test
        rep_item = repeat(itms, 3)
        call assert_equal(rep_item%get_edit_descriptor(), '3(desc1)', &
                          "repeat(items(desc('desc1')), 3) should return '3(desc1)'")
        ! teardown
        call itms%destruct()
        call rep_item%destruct()

        ! setup
        itms = edit_descriptor_type("desc1")//edit_descriptor_type("desc2")
        ! test
        rep_item = repeat(itms, 2)
        call assert_equal(rep_item%get_edit_descriptor(), '2(desc1,desc2)', &
                          "repeat(desc('desc1')//desc('desc2'), 3) should return '2(desc1,desc2)'")
        ! teardown
        call itms%destruct()
        call rep_item%destruct()

        ! setup
        itms = edit_descriptor_type("desc1")//edit_descriptor_type("desc2")//edit_descriptor_type("desc3")
        ! test
        rep_item = repeat(itms)
        call assert_equal(rep_item%get_edit_descriptor(), '*(desc1,desc2,desc3)', &
                          "repeat(desc('desc1')//desc('desc2')//desc('desc3')) should return '*(desc1,desc2,desc3)'")
        ! teardown
        call itms%destruct()
        call rep_item%destruct()
    end subroutine construct_rep_fmt_itms_returns_repeated_fmt_item

    subroutine construct_rep_fmt_itms_w_sep_returns_repeated_fmt_item()
        implicit none
        type(format_items_type) :: itms
        type(format_item_type) :: rep_item

        ! setup
        itms = edit_descriptor_type("desc1")//edit_descriptor_type("desc2")
        ! test
        rep_item = repeat(itms, ",", 2)
        call assert_equal(rep_item%get_edit_descriptor(), '2(desc1,",",desc2,",")', &
                          "repeat(desc('desc1')//desc('desc2'), ',', 3) should return "//'2(desc1,",",desc2,",")')
        ! teardown
        call itms%destruct()
        call rep_item%destruct()

        ! setup
        itms = edit_descriptor_type("desc1")//edit_descriptor_type("desc2")//edit_descriptor_type("desc3")
        ! test
        rep_item = repeat(itms, ",")
        call assert_equal(rep_item%get_edit_descriptor(), '*(desc1,",",desc2,",",desc3,",")', &
                          "repeat(desc('desc1')//desc('desc2')//desc('desc3')) should return "// &
                          '*(desc1,",",desc2,",",desc3,",")')
        ! teardown
        call itms%destruct()
        call rep_item%destruct()
    end subroutine construct_rep_fmt_itms_w_sep_returns_repeated_fmt_item

    subroutine construct_rep_fmt_itms_by_desc_returns_repeated_fmt_item()
        implicit none
        type(format_item_type) :: rep_item

        ! test
        rep_item = repeat(edit_descriptor_type("desc1"), 5)
        call assert_equal(rep_item%get_edit_descriptor(), '5(desc1)', &
                          "repeat(desc('desc1'), 5) should return '5(desc1)'")
        ! teardown
        call rep_item%destruct()
    end subroutine construct_rep_fmt_itms_by_desc_returns_repeated_fmt_item

    subroutine construct_rep_fmt_itms_w_sep_by_desc_returns_repeated_fmt_item()
        implicit none
        type(format_item_type) :: rep_item

        ! test
        rep_item = repeat(edit_descriptor_type("desc1"), ",", 4)
        call assert_equal(rep_item%get_edit_descriptor(), '4(desc1,",")', &
                          "repeat(desc('desc1'), ',', 4) should return "//'4(desc1,",")')
        ! teardown
        call rep_item%destruct()
    end subroutine construct_rep_fmt_itms_w_sep_by_desc_returns_repeated_fmt_item
end program test_repeat