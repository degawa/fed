module fed_format
    use, intrinsic :: iso_fortran_env
    use :: fed_format_items
    implicit none
    private
    public :: format

    !>書式仕様のコンストラクタ
    interface format
        procedure :: construct_format_specification_w_sep
        procedure :: construct_format_specification_by_descriptor
        procedure :: construct_format_specification_by_format_item
    end interface

contains
    !>書式項目並びから書式仕様を文字列で生成して返す．
    !>`"`以外の区切り文字`separator`は，書式項目番号が2番目以降の
    !>データ編集記述子あるいは文字列編集記述子の前に置かれる．
    pure function construct_format_specification_w_sep(format_items, separator) result(format_spec)
        use :: strings_enclose
        use :: fed_format_items, only:format_item_separator
        implicit none
        type(format_items_type), intent(in) :: format_items
            !! 書式項目並び
        character(*), intent(in), optional :: separator
            !! 区切り文字（`"`以外）
        character(:), allocatable :: format_spec
            !! 書式仕様

        integer(int32) :: i, num_items
        character(:), allocatable :: desc_i, enclosed_separator_w_item_sep

        ! 区切り文字が渡されていれば'",",'を作り，
        ! 渡されていなければ空白''とする．
        if (present(separator)) then
            enclosed_separator_w_item_sep = enclose(separator, '"')//format_item_separator
        else
            enclosed_separator_w_item_sep = ""
        end if

        num_items = format_items%get_number_of_items()

        format_spec = ""
        do i = 1, num_items - 1
            desc_i = format_items%get_edit_descriptor_at(i)
            if (desc_i == "") cycle

            format_spec = format_spec//desc_i//format_item_separator

            if (.not. format_items%is_control_edit_descriptor(i + 1)) &
                format_spec = format_spec//enclosed_separator_w_item_sep
        end do
        desc_i = format_items%get_edit_descriptor_at(num_items)
        if (desc_i /= "") format_spec = format_spec//desc_i
        ! 最後の要素が空の場合，(A,)のように最後にカンマだけが残るが
        ! これは問題にならないので，特別な対処は行わない

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
