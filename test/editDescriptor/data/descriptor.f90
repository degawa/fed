program test_data_descriptor
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor_data
    use :: fassert
    implicit none

    print '(A)', "# Testing: data_edit_descriptor_type"
    call general_data_edit_descriptor_symbol_is_G()

contains
    subroutine general_data_edit_descriptor_symbol_is_G()
        implicit none

        ! test
        call assert_equal(general_data_edit_descriptor_symbol, "G", &
                          "general data edit descriptor symbol should be 'G'")
    end subroutine general_data_edit_descriptor_symbol_is_G
end program test_data_descriptor
