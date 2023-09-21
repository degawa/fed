program test_round_edit_descriptor
    use :: fed_editDescriptor_control_round
    use :: fed_editDescriptor_data
    use :: fed_format_item
    use :: fed_format_items
    use :: fed_format
    use :: fassert
    implicit none

    print '(A)', "# Testing: round edit descriptor"
    call rounding_mode_up_returns_RU_items_default()
    call rounding_mode_down_returns_RD_items_default()
    call rounding_mode_zero_returns_RZ_items_default()
    call rounding_mode_nearest_returns_RN_items_default()
    call rounding_mode_compatible_returns_RC_items_default()
    call rounding_mode_processor_defined_returns_RP_items_default()

contains
    subroutine rounding_mode_up_returns_RU_items_default()
        implicit none
        character(:), allocatable :: fmt

        ! test
        fmt = format(rounding_mode%up(dat("desc")))
        call assert_equal(fmt, '(RU,desc,'//get_round_default()//')', &
                          'rounding_mode%up(dat("desc")) should return ' &
                          //'"RU,desc,'//get_round_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(rounding_mode%up(item(dat("desc1"))))
        call assert_equal(fmt, '(RU,desc1,'//get_round_default()//')', &
                          'rounding_mode%up(dat("desc1")) should return ' &
                          //'"RU,desc1,'//get_round_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(rounding_mode%up(dat("desc1")//dat("desc2")))
        call assert_equal(fmt, '(RU,desc1,desc2,'//get_round_default()//')', &
                          'rounding_mode%up(dat("desc1")//dat("desc2")) should return ' &
                          //'"RU,desc1,desc2,'//get_round_default()//'"')
        ! teardown
        deallocate (fmt)
    end subroutine rounding_mode_up_returns_RU_items_default

    subroutine rounding_mode_down_returns_RD_items_default()
        implicit none
        character(:), allocatable :: fmt

        ! test
        fmt = format(rounding_mode%down(dat("desc")))
        call assert_equal(fmt, '(RD,desc,'//get_round_default()//')', &
                          'rounding_mode%down(dat("desc")) should return ' &
                          //'"RD,desc,'//get_round_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(rounding_mode%down(item(dat("desc1"))))
        call assert_equal(fmt, '(RD,desc1,'//get_round_default()//')', &
                          'rounding_mode%down(dat("desc1")) should return ' &
                          //'"RD,desc1,'//get_round_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(rounding_mode%down(dat("desc1")//dat("desc2")))
        call assert_equal(fmt, '(RD,desc1,desc2,'//get_round_default()//')', &
                          'rounding_mode%down(dat("desc1")//dat("desc2")) should return ' &
                          //'"RD,desc1,desc2,'//get_round_default()//'"')
        ! teardown
        deallocate (fmt)
    end subroutine rounding_mode_down_returns_RD_items_default

    subroutine rounding_mode_zero_returns_RZ_items_default()
        implicit none
        character(:), allocatable :: fmt

        ! test
        fmt = format(rounding_mode%zero(dat("desc")))
        call assert_equal(fmt, '(RZ,desc,'//get_round_default()//')', &
                          'rounding_mode%zero(dat("desc")) should return ' &
                          //'"RZ,desc,'//get_round_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(rounding_mode%zero(item(dat("desc1"))))
        call assert_equal(fmt, '(RZ,desc1,'//get_round_default()//')', &
                          'rounding_mode%zero(dat("desc1")) should return ' &
                          //'"RZ,desc1,'//get_round_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(rounding_mode%zero(dat("desc1")//dat("desc2")))
        call assert_equal(fmt, '(RZ,desc1,desc2,'//get_round_default()//')', &
                          'rounding_mode%zero(dat("desc1")//dat("desc2")) should return ' &
                          //'"RZ,desc1,desc2,'//get_round_default()//'"')
        ! teardown
        deallocate (fmt)
    end subroutine rounding_mode_zero_returns_RZ_items_default

    subroutine rounding_mode_nearest_returns_RN_items_default()
        implicit none
        character(:), allocatable :: fmt

        ! test
        fmt = format(rounding_mode%nearest(dat("desc")))
        call assert_equal(fmt, '(RN,desc,'//get_round_default()//')', &
                          'rounding_mode%nearest(dat("desc")) should return ' &
                          //'"RN,desc,'//get_round_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(rounding_mode%nearest(item(dat("desc1"))))
        call assert_equal(fmt, '(RN,desc1,'//get_round_default()//')', &
                          'rounding_mode%nearest(dat("desc1")) should return ' &
                          //'"RN,desc1,'//get_round_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(rounding_mode%nearest(dat("desc1")//dat("desc2")))
        call assert_equal(fmt, '(RN,desc1,desc2,'//get_round_default()//')', &
                          'rounding_mode%nearest(dat("desc1")//dat("desc2")) should return ' &
                          //'"RN,desc1,desc2,'//get_round_default()//'"')
        ! teardown
        deallocate (fmt)
    end subroutine rounding_mode_nearest_returns_RN_items_default

    subroutine rounding_mode_compatible_returns_RC_items_default()
        implicit none
        character(:), allocatable :: fmt

        ! test
        fmt = format(rounding_mode%compatible(dat("desc")))
        call assert_equal(fmt, '(RC,desc,'//get_round_default()//')', &
                          'rounding_mode%compatible(dat("desc")) should return ' &
                          //'"RC,desc,'//get_round_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(rounding_mode%compatible(item(dat("desc1"))))
        call assert_equal(fmt, '(RC,desc1,'//get_round_default()//')', &
                          'rounding_mode%compatible(dat("desc1")) should return ' &
                          //'"RC,desc1,'//get_round_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(rounding_mode%compatible(dat("desc1")//dat("desc2")))
        call assert_equal(fmt, '(RC,desc1,desc2,'//get_round_default()//')', &
                          'rounding_mode%compatible(dat("desc1")//dat("desc2")) should return ' &
                          //'"RC,desc1,desc2,'//get_round_default()//'"')
        ! teardown
        deallocate (fmt)
    end subroutine rounding_mode_compatible_returns_RC_items_default

    subroutine rounding_mode_processor_defined_returns_RP_items_default()
        implicit none
        character(:), allocatable :: fmt

        ! test
        fmt = format(rounding_mode%processor_defined(dat("desc")))
        call assert_equal(fmt, '(RP,desc,'//get_round_default()//')', &
                          'rounding_mode%processor_defined(dat("desc")) should return ' &
                          //'"RP,desc,'//get_round_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(rounding_mode%processor_defined(item(dat("desc1"))))
        call assert_equal(fmt, '(RP,desc1,'//get_round_default()//')', &
                          'rounding_mode%processor_defined(dat("desc1")) should return ' &
                          //'"RP,desc1,'//get_round_default()//'"')
        ! teardown
        deallocate (fmt)

        ! test
        fmt = format(rounding_mode%processor_defined(dat("desc1")//dat("desc2")))
        call assert_equal(fmt, '(RP,desc1,desc2,'//get_round_default()//')', &
                          'rounding_mode%processor_defined(dat("desc1")//dat("desc2")) should return ' &
                          //'"RP,desc1,desc2,'//get_round_default()//'"')
        ! teardown
        deallocate (fmt)
    end subroutine rounding_mode_processor_defined_returns_RP_items_default
end program test_round_edit_descriptor
