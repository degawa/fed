program test_format
    use :: fed_format
    use :: fed_format_items
    use :: fed_format_item
    use :: fed_editDescriptor
    use :: fed_editDescriptor_data
    use :: fed_editDescriptor_control
    use :: fed_editDescriptor_characterString
    use :: fassert
    implicit none

    print '(A)', "# Testing: format procedures"
    call construct_format_spec_returns_format_spec()
    call construct_format_spec_w_sep_returns_format_spec()
    call construct_format_spec_by_desc_returns_format_spec()
    call construct_format_spec_by_format_item_returns_format_spec()

contains
    subroutine construct_format_spec_returns_format_spec()
        implicit none
        type(format_items_type) :: itms

        ! setup
        itms = edit_descriptor_type("desc1") &
               //edit_descriptor_type("desc2") &
               //edit_descriptor_type("desc3")

        ! test
        call assert_equal(format(itms), '(desc1,desc2,desc3)', &
                          "format(items) should return format specification")

        ! teardown
        call itms%destruct()
    end subroutine construct_format_spec_returns_format_spec

    subroutine construct_format_spec_w_sep_returns_format_spec()
        implicit none
        type(format_items_type) :: itms

        ! setup
        itms = dat("desc3")//dat("desc4")
        ! test
        call assert_equal(format(itms, ","), '(desc3,",",desc4)', &
                          'format(dat("desc3")//dat("desc4"),",") should return ''(desc3,",",desc4)''')
        ! teardown
        call itms%destruct()

        ! setup
        itms = dat("desc3")//str("desc4")
        ! test
        call assert_equal(format(itms, ","), '(desc3,",","desc4")', &
                          'format(dat("desc3")//str("desc4"),",") should return ''(desc3,",","desc4")''')
        ! teardown
        call itms%destruct()

        ! setup
        itms = dat("desc3")//ctrl("desc4")
        ! test
        call assert_equal(format(itms, ","), '(desc3,desc4)', &
                          'format(dat("desc3")//ctrl("desc4"),",") should return ''(desc4,desc5)''')
        ! teardown
        call itms%destruct()

        ! setup
        itms = str("desc3")//dat("desc4")
        ! test
        call assert_equal(format(itms, ","), '("desc3",",",desc4)', &
                          'format(str("desc3")//dat("desc4"),",") should return ''("desc3",",",desc4)''')
        ! teardown
        call itms%destruct()

        ! setup
        itms = str("desc3")//str("desc4")
        ! test
        call assert_equal(format(itms, ","), '("desc3",",","desc4")', &
                          'format(str("desc3")//str("desc4"),",") should return ''("desc3",",","desc4")''')
        ! teardown
        call itms%destruct()

        ! setup
        itms = str("desc3")//ctrl("desc4")
        ! test
        call assert_equal(format(itms, ","), '("desc3",desc4)', &
                          'format(str("desc3")//ctrl("desc4"),",") should return ''("desc4",desc5)''')
        ! teardown
        call itms%destruct()

        ! setup
        itms = ctrl("desc3")//dat("desc4")
        ! test
        call assert_equal(format(itms, ","), '(desc3,",",desc4)', &
                          'format(ctrl("desc3")//dat("desc4"),",") should return ''(desc3,",",desc4)''')
        ! teardown
        call itms%destruct()

        ! setup
        itms = ctrl("desc3")//str("desc4")
        ! test
        call assert_equal(format(itms, ","), '(desc3,",","desc4")', &
                          'format(ctrl("desc3")//str("desc4"),",") should return ''(desc3,",","desc4")''')
        ! teardown
        call itms%destruct()

        ! setup
        itms = ctrl("desc3")//ctrl("desc4")
        ! test
        call assert_equal(format(itms, ","), '(desc3,desc4)', &
                          'format(ctrl("desc3")//ctrl("desc4"),",") should return ''(desc4,desc5)''')
        ! teardown
        call itms%destruct()
    end subroutine construct_format_spec_w_sep_returns_format_spec

    subroutine construct_format_spec_by_desc_returns_format_spec()
        implicit none

        ! test
        call assert_equal(format(edit_descriptor_type("desc")), '(desc)', &
                          "format(desc) should return format specification")
    end subroutine construct_format_spec_by_desc_returns_format_spec

    subroutine construct_format_spec_by_format_item_returns_format_spec()
        implicit none

        ! test
        call assert_equal(format(item(edit_descriptor_type("item"))), '(item)', &
                          "format(item) should return format specification")
    end subroutine construct_format_spec_by_format_item_returns_format_spec
end program test_format
