program test_blank_edit_descriptor
    use :: fed_editDescriptor_control_blank
    use :: fed_editDescriptor_data
    use :: fed_format_item
    use :: fed_format_items
    use :: fed_format
    use :: fassert
    implicit none

    print '(A)', "# Testing: blank edit descriptor"
    call blank_mode_null_returns_BN_items_default()
    call blank_mode_zero_returns_BZ_items_default()

contains
    subroutine blank_mode_null_returns_BN_items_default()
        implicit none
        character(:), allocatable :: fmt

        ! test
        fmt = format(blank_mode%null(dat("desc")))
        call assert_equal(fmt, '(BN,desc,'//get_blank_default()//')', &
                          'blank_mode%null(dat("desc")) should return ' &
                          //'"BN,desc,'//get_blank_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(blank_mode%null(item(dat("desc1"))))
        call assert_equal(fmt, '(BN,desc1,'//get_blank_default()//')', &
                          'blank_mode%null(dat("desc1")) should return ' &
                          //'"BN,desc1,'//get_blank_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(blank_mode%null(dat("desc1")//dat("desc2")))
        call assert_equal(fmt, '(BN,desc1,desc2,'//get_blank_default()//')', &
                          'blank_mode%null(dat("desc1")//dat("desc2")) should return ' &
                          //'"BN,desc1,desc2,'//get_blank_default()//'"')
        ! teardown
        deallocate (fmt)
    end subroutine blank_mode_null_returns_BN_items_default

    subroutine blank_mode_zero_returns_BZ_items_default()
        implicit none
        character(:), allocatable :: fmt

        ! test
        fmt = format(blank_mode%zero(dat("desc")))
        call assert_equal(fmt, '(BZ,desc,'//get_blank_default()//')', &
                          'blank_mode%zero(dat("desc")) should return ' &
                          //'"BZ,desc,'//get_blank_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(blank_mode%zero(item(dat("desc1"))))
        call assert_equal(fmt, '(BZ,desc1,'//get_blank_default()//')', &
                          'blank_mode%zero(dat("desc1")) should return ' &
                          //'"BZ,desc1,'//get_blank_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(blank_mode%zero(dat("desc1")//dat("desc2")))
        call assert_equal(fmt, '(BZ,desc1,desc2,'//get_blank_default()//')', &
                          'blank_mode%zero(dat("desc1")//dat("desc2")) should return ' &
                          //'"BZ,desc1,desc2,'//get_blank_default()//'"')
        ! teardown
        deallocate (fmt)
    end subroutine blank_mode_zero_returns_BZ_items_default
end program test_blank_edit_descriptor
