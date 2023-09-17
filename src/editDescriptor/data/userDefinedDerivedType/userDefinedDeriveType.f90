module fed_editDescriptor_data_userDefinedDerivedType
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor_data
    implicit none
    private
    public :: udt

    character(*), private, parameter :: udt_edit_descriptor_symbol = "DT"
        !! ユーザ定義派生型編集記述子に用いられる英字定数

    !>ユーザ定義派生型編集記述子を取り扱う派生型．
    type, public, extends(data_edit_descriptor_type) :: udt_edit_descriptor_type
    end type udt_edit_descriptor_type

    interface udt
        procedure :: construct_user_defined_derived_type_descriptor
        procedure :: construct_user_defined_derived_type_descriptor_w_vlist
        procedure :: construct_user_defined_derived_type_descriptor_w_iotype
        procedure :: construct_user_defined_derived_type_descriptor_w_iotype_vlist
    end interface

contains
    !>udt_edit_descriptor_typeインスタンスを生成して返す．
    pure function construct_user_defined_derived_type_descriptor() result(new_dt_desc)
        implicit none
        type(udt_edit_descriptor_type) :: new_dt_desc
            !! 生成されるインスタンス

        ! 'DT'を作成する
        call new_dt_desc%set(udt_edit_descriptor_symbol)
    end function construct_user_defined_derived_type_descriptor

    !>udt_edit_descriptor_typeインスタンスを生成して返す．
    pure function construct_user_defined_derived_type_descriptor_w_iotype(iotype) result(new_dt_desc)
        use :: strings_enclose
        implicit none
        character(*), intent(in) :: iotype
            !! 書式指定文字列
        type(udt_edit_descriptor_type) :: new_dt_desc
            !! 生成されるインスタンス

        ! 'DT(1,2,3,...)'を作成する
        call new_dt_desc%set(udt_edit_descriptor_symbol &
                             //enclose(iotype, '"'))
    end function construct_user_defined_derived_type_descriptor_w_iotype

    !>udt_edit_descriptor_typeインスタンスを生成して返す．
    pure function construct_user_defined_derived_type_descriptor_w_vlist(v_list) result(new_dt_desc)
        implicit none
        integer(int32), intent(in) :: v_list(:)
            !! 編集記述子の値指定並び
        type(udt_edit_descriptor_type) :: new_dt_desc
            !! 生成されるインスタンス

        if (size(v_list) == 0) then
            new_dt_desc = udt()
            return
        end if

        ! 'DT"iotype"'を作成する
        call new_dt_desc%set(udt_edit_descriptor_symbol &
                             //v_list_to_string(v_list))
    end function construct_user_defined_derived_type_descriptor_w_vlist

    !>udt_edit_descriptor_typeインスタンスを生成して返す．
    pure function construct_user_defined_derived_type_descriptor_w_iotype_vlist(iotype, v_list) result(new_dt_desc)
        use :: strings_enclose
        implicit none
        character(*), intent(in) :: iotype
            !! 書式指定文字列
        integer(int32), intent(in) :: v_list(:)
            !! 編集記述子の値指定並び
        type(udt_edit_descriptor_type) :: new_dt_desc
            !! 生成されるインスタンス

        if (size(v_list) == 0) then
            new_dt_desc = udt(iotype)
            return
        end if

        ! 'DT"iotype"(1,2,3,...)'を作成する
        call new_dt_desc%set(udt_edit_descriptor_symbol &
                             //enclose(iotype, '"') &
                             //v_list_to_string(v_list))
    end function construct_user_defined_derived_type_descriptor_w_iotype_vlist

    !>`v_list`を文字列`"(v_list(1), v_list(2), v_list(3), ...)"`に変換して返す．
    pure function v_list_to_string(v_list) result(v_list_str)
        use :: stdlib_strings
        use :: strings_enclose
        implicit none
        integer(int32), intent(in) :: v_list(:)
        ! 編集記述子の値指定並び
        character(:), allocatable :: v_list_str
        ! 文字列に変換され，括弧で囲まれた編集記述子の値指定並び

        integer(int32) :: i, list_size

        list_size = size(v_list)

        v_list_str = to_string(v_list(1))
        do i = 2, list_size
            v_list_str = v_list_str//","
            v_list_str = v_list_str//to_string(v_list(i))
        end do

        v_list_str = enclose(v_list_str, "(")
    end function v_list_to_string
end module fed_editDescriptor_data_userDefinedDerivedType
