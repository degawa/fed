program test_sign_edit_descriptor
    use :: fed_editDescriptor_control_sign
    use :: fed_editDescriptor_data
    use :: fed_format_item
    use :: fed_format_items
    use :: fed_format
    use :: fassert
    implicit none

    print '(A)', "# Testing: sign edit descriptor"
    call sign_mode_suppress_returns_SS_items_default()
    call sign_mode_plus_returns_SP_items_default()
    call sign_mode_processor_defined_returns_S_items_default()

contains
    subroutine sign_mode_suppress_returns_SS_items_default()
        implicit none
        character(:), allocatable :: fmt

        ! test
        fmt = format(sign_mode%suppress(dat("desc")))
        call assert_equal(fmt, '(SS,desc,'//get_sign_default()//')', &
                          'sign_mode%suppress(dat("desc")) should return ' &
                          //'"SS,desc,'//get_sign_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(sign_mode%suppress(item(dat("desc1"))))
        call assert_equal(fmt, '(SS,desc1,'//get_sign_default()//')', &
                          'sign_mode%suppress(dat("desc1")) should return ' &
                          //'"SS,desc1,'//get_sign_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(sign_mode%suppress(dat("desc1")//dat("desc2")))
        call assert_equal(fmt, '(SS,desc1,desc2,'//get_sign_default()//')', &
                          'sign_mode%suppress(dat("desc1")//dat("desc2")) should return ' &
                          //'"SS,desc1,desc2,'//get_sign_default()//'"')
        ! teardown
        deallocate (fmt)
    end subroutine sign_mode_suppress_returns_SS_items_default

    subroutine sign_mode_plus_returns_SP_items_default()
        implicit none
        character(:), allocatable :: fmt

        ! test
        fmt = format(sign_mode%plus(dat("desc")))
        call assert_equal(fmt, '(SP,desc,'//get_sign_default()//')', &
                          'sign_mode%plus(dat("desc")) should return ' &
                          //'"SP,desc,'//get_sign_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(sign_mode%plus(item(dat("desc1"))))
        call assert_equal(fmt, '(SP,desc1,'//get_sign_default()//')', &
                          'sign_mode%plus(dat("desc1")) should return ' &
                          //'"SP,desc1,'//get_sign_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(sign_mode%plus(dat("desc1")//dat("desc2")))
        call assert_equal(fmt, '(SP,desc1,desc2,'//get_sign_default()//')', &
                          'sign_mode%plus(dat("desc1")//dat("desc2")) should return ' &
                          //'"SP,desc1,desc2,'//get_sign_default()//'"')
        ! teardown
        deallocate (fmt)
    end subroutine sign_mode_plus_returns_SP_items_default

    subroutine sign_mode_processor_defined_returns_S_items_default()
        implicit none
        character(:), allocatable :: fmt

        ! test
        fmt = format(sign_mode%processor_defined(dat("desc")))
        call assert_equal(fmt, '(S,desc,'//get_sign_default()//')', &
                          'sign_mode%processor_defined(dat("desc")) should return ' &
                          //'"S,desc,'//get_sign_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(sign_mode%processor_defined(item(dat("desc1"))))
        call assert_equal(fmt, '(S,desc1,'//get_sign_default()//')', &
                          'sign_mode%processor_defined(dat("desc1")) should return ' &
                          //'"S,desc1,'//get_sign_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(sign_mode%processor_defined(dat("desc1")//dat("desc2")))
        call assert_equal(fmt, '(S,desc1,desc2,'//get_sign_default()//')', &
                          'sign_mode%processor_defined(dat("desc1")//dat("desc2")) should return ' &
                          //'"S,desc1,desc2,'//get_sign_default()//'"')
        ! teardown
        deallocate (fmt)
    end subroutine sign_mode_processor_defined_returns_S_items_default
end program test_sign_edit_descriptor
