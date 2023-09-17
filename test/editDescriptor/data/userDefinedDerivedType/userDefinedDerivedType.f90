program test_userDefinedDerivedType_edit_descriptor
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor
    use :: fed_editDescriptor_data_userDefinedDerivedType
    use :: fassert
    implicit none

    call udt_constructor_returns_udt_edit_desc_instance()
    call udt_returns_DT_when_passed_no_argument()
    call udt_returns_DTiotype_when_passed_iotype()
    call udt_returns_DTiotype_when_passed_vtype()
    call udt_returns_DTiotypevlist_when_passed_iotype_vlist()

contains
    subroutine udt_constructor_returns_udt_edit_desc_instance()
        implicit none
        class(edit_descriptor_type), allocatable :: desc
        type(udt_edit_descriptor_type) :: type_mold

        ! test
        allocate (desc, source=udt("udt"))
        call assert_true(same_type_as(desc, type_mold), &
                         "udt(iotype) should return `udt_edit_descriptor_type` instance")

        ! teardown
        deallocate (desc)

        ! test
        allocate (desc, source=udt("udt", [1, 2, 3]))
        call assert_true(same_type_as(desc, type_mold), &
                         "udt(iotype, v_list) should return `udt_edit_descriptor_type` instance")

        ! teardown
        deallocate (desc)

        ! test
        allocate (desc, source=udt("udt", [integer ::]))
        call assert_true(same_type_as(desc, type_mold), &
                         "udt(iotype, v_list=[]) should return `udt_edit_descriptor_type` instance")

        ! teardown
        deallocate (desc)
    end subroutine udt_constructor_returns_udt_edit_desc_instance

    subroutine udt_returns_DT_when_passed_no_argument()
        implicit none
        type(udt_edit_descriptor_type) :: desc

        desc = udt()
        call assert_equal(desc%get(), 'DT', &
                          'udt() should return ''DT''')
    end subroutine udt_returns_DT_when_passed_no_argument

    subroutine udt_returns_DTiotype_when_passed_iotype()
        implicit none
        type(udt_edit_descriptor_type) :: desc

        desc = udt("derived_type")
        call assert_equal(desc%get(), 'DT"derived_type"', &
                          'udt("derived_type") should return ''DT"derived_type"''')
    end subroutine udt_returns_DTiotype_when_passed_iotype

    subroutine udt_returns_DTiotype_when_passed_vtype()
        implicit none
        type(udt_edit_descriptor_type) :: desc

        desc = udt([1, 2, 3])
        call assert_equal(desc%get(), 'DT(1,2,3)', &
                          'udt([1, 2, 3]) should return ''DT(1,2,3)''')

        desc = udt([integer ::])
        call assert_equal(desc%get(), 'DT', &
                          'udt([integer ::]) should return ''DT''')
    end subroutine udt_returns_DTiotype_when_passed_vtype

    subroutine udt_returns_DTiotypevlist_when_passed_iotype_vlist()
        implicit none
        type(udt_edit_descriptor_type) :: desc

        desc = udt("derived_type", [3, 4, 5])
        call assert_equal(desc%get(), 'DT"derived_type"(3,4,5)', &
                          'udt("derived_type", [3, 4, 5]) should return ''DT"derived_type"(3,4,5)''')

        desc = udt("derived_type", [integer ::])
        call assert_equal(desc%get(), 'DT"derived_type"', &
                          'udt("derived_type", [integer ::]) should return ''DT"derived_type"''')
    end subroutine udt_returns_DTiotypevlist_when_passed_iotype_vlist
end program test_userDefinedDerivedType_edit_descriptor
