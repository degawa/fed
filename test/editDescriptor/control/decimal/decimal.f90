program test_decimal_edit_descriptor
    use :: fed_editDescriptor_control_decimal
    use :: fed_editDescriptor_data
    use :: fed_format_item
    use :: fed_format_items
    use :: fed_format
    use :: fassert
    implicit none

    print '(A)', "# Testing: decimal edit descriptor"
    call decimal_mode_comma_returns_DC_items_default()
    call decimal_mode_point_returns_DP_items_default()

contains
    subroutine decimal_mode_comma_returns_DC_items_default()
        implicit none
        character(:), allocatable :: fmt

        ! test
        fmt = format(decimal_mode%comma(dat("desc")))
        call assert_equal(fmt, '(DC,desc,'//get_decimal_default()//')', &
                          'decimal_mode%comma(dat("desc")) should return ' &
                          //'"DC,desc,'//get_decimal_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(decimal_mode%comma(item(dat("desc1"))))
        call assert_equal(fmt, '(DC,desc1,'//get_decimal_default()//')', &
                          'decimal_mode%comma(dat("desc1")) should return ' &
                          //'"DC,desc1,'//get_decimal_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(decimal_mode%comma(dat("desc1")//dat("desc2")))
        call assert_equal(fmt, '(DC,desc1,desc2,'//get_decimal_default()//')', &
                          'decimal_mode%comma(dat("desc1")//dat("desc2")) should return ' &
                          //'"DC,desc1,desc2,'//get_decimal_default()//'"')
        ! teardown
        deallocate (fmt)
    end subroutine decimal_mode_comma_returns_DC_items_default

    subroutine decimal_mode_point_returns_DP_items_default()
        implicit none
        character(:), allocatable :: fmt

        ! test
        fmt = format(decimal_mode%point(dat("desc")))
        call assert_equal(fmt, '(DP,desc,'//get_decimal_default()//')', &
                          'decimal_mode%point(dat("desc")) should return ' &
                          //'"DP,desc,'//get_decimal_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(decimal_mode%point(item(dat("desc1"))))
        call assert_equal(fmt, '(DP,desc1,'//get_decimal_default()//')', &
                          'decimal_mode%point(dat("desc1")) should return ' &
                          //'"DP,desc1,'//get_decimal_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(decimal_mode%point(dat("desc1")//dat("desc2")))
        call assert_equal(fmt, '(DP,desc1,desc2,'//get_decimal_default()//')', &
                          'decimal_mode%point(dat("desc1")//dat("desc2")) should return ' &
                          //'"DP,desc1,desc2,'//get_decimal_default()//'"')
        ! teardown
        deallocate (fmt)
    end subroutine decimal_mode_point_returns_DP_items_default
end program test_decimal_edit_descriptor
