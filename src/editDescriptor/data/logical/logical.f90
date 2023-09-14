module fed_editDescriptor_data_logical
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor_data
    implicit none
    private
    public :: logical

    character(*), private, parameter :: logical_edit_descriptor_symbol = "L"
        !! L形編集記述子（論理編集）に用いられる英字定数
    integer(int32), private, parameter :: default_width = 1
        !! L形編集記述子における欄の標準桁数

    !>論理型編集記述子を取り扱う派生型．
    type, public, extends(data_edit_descriptor_type) :: logical_edit_descriptor_type
    end type logical_edit_descriptor_type

    interface logical
        procedure :: construct_logical_edit_descriptor_w_width
        procedure :: construct_logical_edit_descriptor
    end interface

contains
    !>logical_edit_descriptor_typeインスタンスを生成して返す．
    !>欄幅には`width`が用いられる．
    !>`width`が0以下の場合は，標準桁数が用いられる．
    pure function construct_logical_edit_descriptor_w_width(width) result(new_logical_desc)
        use :: stdlib_strings
        implicit none
        integer(int32), intent(in) :: width
            !! 欄幅(>0)
        type(logical_edit_descriptor_type) :: new_logical_desc
            !! 生成されるlogical_edit_descriptor_typeインスタンス

        integer(int32) :: w
        w = width
        if (w <= 0) w = default_width

        call new_logical_desc%set(logical_edit_descriptor_symbol//to_string(w))
    end function construct_logical_edit_descriptor_w_width

    !>logical_edit_descriptor_typeインスタンスを生成して返す．
    !>欄幅には標準桁数が用いられる．
    pure function construct_logical_edit_descriptor() result(new_logical_desc)
        implicit none
        type(logical_edit_descriptor_type) :: new_logical_desc
            !! 生成されるlogical_edit_descriptor_typeインスタンス

        new_logical_desc = construct_logical_edit_descriptor_w_width(width=default_width)
    end function construct_logical_edit_descriptor
end module fed_editDescriptor_data_logical
