program test_data_descriptor
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor_data
    use :: fassert
    implicit none

    print '(A)', "# Testing: data_edit_descriptor_type"
    call general_data_edit_descriptor_symbol_is_G()
    call multiply_op_returns_data_desc_catenated_w_repeat_count()

contains
    subroutine general_data_edit_descriptor_symbol_is_G()
        implicit none

        ! test
        call assert_equal(general_data_edit_descriptor_symbol, "G", &
                          "general data edit descriptor symbol should be 'G'")
    end subroutine general_data_edit_descriptor_symbol_is_G

    subroutine multiply_op_returns_data_desc_catenated_w_repeat_count()
        implicit none
        type(data_edit_descriptor_type) :: desc, cnt_desc

        ! setup
        call desc%set("I4")

        ! test
        cnt_desc = 2*desc
        call assert_equal(cnt_desc%get(), "2I4", &
                          "2*'I4' returns '2I4'")

        cnt_desc = 0*desc
        call assert_equal(cnt_desc%get(), "I4", &
                          "0*'I4' returns 'I4'")

        cnt_desc = (-1)*desc
        call assert_equal(cnt_desc%get(), "I4", &
                          "-1*'I4' returns 'I4'")

        cnt_desc = desc*5
        call assert_equal(cnt_desc%get(), "5I4", &
                          "'I4'*5 returns '5I4'")

        cnt_desc = desc*0
        call assert_equal(cnt_desc%get(), "I4", &
                          "'I4'*0 returns 'I4'")

        cnt_desc = desc*(-1)
        call assert_equal(cnt_desc%get(), "I4", &
                          "'I4'*-1 returns 'I4'")
    end subroutine multiply_op_returns_data_desc_catenated_w_repeat_count
end program test_data_descriptor
