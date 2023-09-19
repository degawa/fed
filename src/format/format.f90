module fed_format
    use, intrinsic :: iso_fortran_env
    use :: fed_format_items
    implicit none
    private
    public :: format

    !>書式仕様のコンストラクタ
    interface format
        procedure :: construct_format_specification
        procedure :: construct_format_specification_w_sep
        procedure :: construct_format_specification_by_descriptor
        procedure :: construct_format_specification_by_format_item
    end interface

contains
    !>書式項目並びから書式仕様を文字列で生成して返す．
    pure function construct_format_specification(format_items) result(format_spec)
        use :: strings_enclose
        implicit none
        type(format_items_type), intent(in) :: format_items
            !! 書式項目並び
        character(:), allocatable :: format_spec
            !! 書式仕様

        integer(int32) :: i, num_items
        num_items = format_items%get_number_of_items()

        format_spec = ""
        do i = 1, num_items - 1
            format_spec = format_spec//format_items%get_edit_descriptor_at(i)//','
        end do
        format_spec = format_spec//format_items%get_edit_descriptor_at(num_items)

        format_spec = enclose(format_spec, '(')
    end function construct_format_specification

    !>書式項目並びから書式仕様を文字列で生成して返す．
    !>`"`以外の区切り文字`separator`は，書式項目番号が2番目以降の
    !>データ編集記述子あるいは文字列編集記述子の前に置かれる．
    pure function construct_format_specification_w_sep(format_items, separator) result(format_spec)
        use :: strings_enclose
        implicit none
        type(format_items_type), intent(in) :: format_items
            !! 書式項目並び
        character(*), intent(in) :: separator
            !! 区切り文字（`"`以外）
        character(:), allocatable :: format_spec
            !! 書式仕様

        integer(int32) :: i, num_items
        num_items = format_items%get_number_of_items()

        format_spec = ""
        do i = 1, num_items - 1
            format_spec = format_spec//format_items%get_edit_descriptor_at(i)//','

            if (.not. format_items%is_control_edit_descriptor(i + 1)) &
                format_spec = format_spec//enclose(separator, '"')//','
        end do
        format_spec = format_spec//format_items%get_edit_descriptor_at(num_items)

        format_spec = enclose(format_spec, '(')
    end function construct_format_specification_w_sep

    !>編集記述子から書式仕様を文字列で生成して返す．
    function construct_format_specification_by_descriptor(edit_descriptor) result(format_spec)
        use :: fed_editDescriptor
        implicit none
        class(edit_descriptor_type), intent(in) :: edit_descriptor
            !! 編集記述子
        character(:), allocatable :: format_spec
            !! 書式仕様

        format_spec = format(items(edit_descriptor))
    end function construct_format_specification_by_descriptor

    !>書式項目から書式仕様を文字列で生成して返す．
    function construct_format_specification_by_format_item(format_item) result(format_spec)
        use :: fed_format_item
        implicit none
        class(format_item_type), intent(in) :: format_item
            !! 書式項目
        character(:), allocatable :: format_spec
            !! 書式仕様

        format_spec = format(items(format_item))
    end function construct_format_specification_by_format_item
end module fed_format
