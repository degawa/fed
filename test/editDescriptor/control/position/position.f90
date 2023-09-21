program test_position_edit_descriptor
    use :: fed_editDescriptor_control_position
    use :: fed_editDescriptor
    use :: fassert
    implicit none

    print '(A)', "# Testing: position edit descriptor"
    call move_returns_position_edit_desc_instance()
    call move_to_returns_position_edit_desc_instance()
    call move_returns_TRn_when_passed_n_ge_0()
    call move_returns_TLn_when_passed_n_lt_0()
    call move_to_returns_T1_when_passed_n_le_0()

contains
    subroutine move_returns_position_edit_desc_instance()
        implicit none
        class(edit_descriptor_type), allocatable :: desc
        type(position_edit_descriptor_type) :: type_mold

        ! test
        allocate (desc, source=move(0))
        call assert_true(same_type_as(desc, type_mold), &
                         'move(int) should return `position_edit_descriptor_type` instance')

        ! teardown
        deallocate (desc)
    end subroutine move_returns_position_edit_desc_instance

    subroutine move_to_returns_position_edit_desc_instance()
        implicit none
        class(edit_descriptor_type), allocatable :: desc
        type(position_edit_descriptor_type) :: type_mold

        ! test
        allocate (desc, source=move_to(0))
        call assert_true(same_type_as(desc, type_mold), &
                         'move_to(int) should return `position_edit_descriptor_type` instance')

        ! teardown
        deallocate (desc)
    end subroutine move_to_returns_position_edit_desc_instance

    subroutine move_returns_TRn_when_passed_n_ge_0()
        implicit none
        type(position_edit_descriptor_type) :: desc

        ! test
        desc = move(0)
        call assert_equal(desc%get(), "TR0", &
                          'move(0) should return "TR0"')

        ! teardown
        call desc%destruct()

        ! test
        desc = move(1)
        call assert_equal(desc%get(), "TR1", &
                          'move(1) should return "TR1"')

        ! teardown
        call desc%destruct()

        ! test
        desc = move(9)
        call assert_equal(desc%get(), "TR9", &
                          'move(9) should return "TR9"')

        ! teardown
        call desc%destruct()
    end subroutine move_returns_TRn_when_passed_n_ge_0

    subroutine move_returns_TLn_when_passed_n_lt_0()
        implicit none
        type(position_edit_descriptor_type) :: desc

        ! test
        desc = move(-1)
        call assert_equal(desc%get(), "TL1", &
                          'move(-1) should return "TL1"')

        ! teardown
        call desc%destruct()

        ! test
        desc = move(-8)
        call assert_equal(desc%get(), "TL8", &
                          'move(-8) should return "TL8"')

        ! teardown
        call desc%destruct()
    end subroutine move_returns_TLn_when_passed_n_lt_0

    subroutine move_to_returns_Tn_when_passed_n_ge_1()
        implicit none
        type(position_edit_descriptor_type) :: desc

        ! test
        desc = move_to(1)
        call assert_equal(desc%get(), "T1", &
                          'move_to(1) should return "T1"')

        ! teardown
        call desc%destruct()

        ! test
        desc = move(3)
        call assert_equal(desc%get(), "T3", &
                          'move_to(3) should return "T3"')

        ! teardown
        call desc%destruct()
    end subroutine move_to_returns_Tn_when_passed_n_ge_1

    subroutine move_to_returns_T1_when_passed_n_le_0()
        implicit none
        type(position_edit_descriptor_type) :: desc

        ! test
        desc = move_to(0)
        call assert_equal(desc%get(), "T1", &
                          'move_to(0) should return "T1"')

        ! teardown
        call desc%destruct()

        ! test
        desc = move_to(-2)
        call assert_equal(desc%get(), "T1", &
                          'move_to(-2) should return "T1"')

        ! teardown
        call desc%destruct()
    end subroutine move_to_returns_T1_when_passed_n_le_0
end program test_position_edit_descriptor
