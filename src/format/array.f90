module fed_array
    use, intrinsic :: iso_fortran_env
    use :: fed_format_item
    use :: fed_repeat
    use :: fed_editDescriptor_data
    use :: stdlib_optval
    implicit none
    private
    public :: array

    interface array
        procedure :: construct_array_format_items_w_desc_size_sep_bracket
        procedure :: construct_array_format_items_w_desc_shape_sep_bracket
    end interface

contains
    !>引数で渡された型の配列を表示する書式項目を生成して返す．
    !>`"`以外の区切り文字`separator`が渡されていれば，
    !>各要素が`separator`で区切られる．
    !>`bracket_open`が渡されていれば，`bracket_open`と対応する閉じ括弧
    !>で配列全体が囲まれる．
    function construct_array_format_items_w_desc_size_sep_bracket( &
        data_edit_descriptor, array_size, separator, bracket_open) result(new_format_item)
        implicit none
        class(data_edit_descriptor_type), intent(in) :: data_edit_descriptor
            !! データ編集記述子
        integer(int32), intent(in) :: array_size
            !! 配列要素数．<br>
            !! `size(array)`として渡される事を想定．
        character(*), intent(in), optional :: separator
            !! 区切り文字
        character(*), intent(in), optional :: bracket_open
            !! 配列を囲む括弧の開き括弧
        type(format_item_type) :: new_format_item
            !! 生成される編集項目

        type(format_item_type) :: repeated_item
        character(:), allocatable :: desc, separator_, bracket_open_

        separator_ = optval(separator, "")
        bracket_open_ = optval(bracket_open, "")

        ! 編集記述子の書式反復数を設定
        repeated_item = repeat(data_edit_descriptor, separator_, array_size)
        ! 括弧が渡されていれば，括弧を結合
        desc = catenate_brackets(repeated_item%get_edit_descriptor(), bracket_open_, separator_)
        ! 書式項目を生成
        new_format_item = item(dat(desc))
    end function construct_array_format_items_w_desc_size_sep_bracket

    !>引数で渡された型の配列を表示する書式項目を生成して返す．
    !>`"`以外の区切り文字`separator`が渡されていれば，
    !>各要素が`separator`で区切られる．
    !>`bracket_open`が渡されていれば，`bracket_open`と対応する閉じ括弧
    !>で配列の各次元が囲まれる．
    function construct_array_format_items_w_desc_shape_sep_bracket( &
        data_edit_descriptor, array_shape, separator, bracket_open) result(new_format_item)
        implicit none
        class(data_edit_descriptor_type), intent(in) :: data_edit_descriptor
            !! データ編集記述子
        integer(int32), intent(in) :: array_shape(1:)
            !! 配列形状．<br>
            !! `shape(array)`として渡される事を想定．
        character(*), intent(in), optional :: separator
            !! 区切り文字
        character(*), intent(in), optional :: bracket_open
            !! 配列を囲む括弧の開き括弧
        type(format_item_type) :: new_format_item
            !! 生成される編集項目

        type(format_item_type) :: repeated_item
        character(:), allocatable :: desc, separator_, bracket_open_
        integer(int32) :: dim

        separator_ = optval(separator, "")
        bracket_open_ = optval(bracket_open, "")

        repeated_item = item(data_edit_descriptor)

        do dim = 1, size(array_shape)
            ! 編集記述子の書式反復数を設定
            repeated_item = repeat(repeated_item, separator_, array_shape(dim))
            ! 括弧が渡されていれば，括弧を結合
            desc = catenate_brackets(repeated_item%get_edit_descriptor(), bracket_open_, separator_)
            ! 書式項目を生成
            repeated_item = item(dat(desc))
        end do

        new_format_item = repeated_item
    end function construct_array_format_items_w_desc_shape_sep_bracket

    !>編集記述子に括弧を連結して返す．
    pure function catenate_brackets(desc, bracket_open, separator) result(enclosed_desc)
        use :: strings_enclose
        use :: fed_editDescriptor_control_position
        use :: fed_format_items, only:format_item_separator
        implicit none
        character(*), intent(in) :: desc
            !! 編集記述子
        character(*), intent(in) :: bracket_open
            !! 開き括弧
        character(*), intent(in) :: separator
            !! 区切り文字
        character(:), allocatable :: enclosed_desc
            !! 括弧が連結された編集記述子

        character(:), allocatable :: bracket_close
        integer(int32) :: len_separator
        type(position_edit_descriptor_type) :: move_to_separator
        type(position_edit_descriptor_type) :: move_after_closing_bracket
        character(:), allocatable :: overwriting_spaces

        ! 区切り文字と括弧がない場合は
        ! 作業する必要が無いので入力をそのまま返す
        if (bracket_open == "" .and. separator == "") then
            enclosed_desc = desc
            return
        end if

        len_separator = len(separator)

        ! 括弧がない場合は末尾の区切り文字を空白で上書きする
        ! 編集項目並びを返す
        if (bracket_open == "" .and. len_separator >= 1) then
            move_to_separator = move(-len_separator)
            overwriting_spaces = repeat(" ", len_separator)
            enclosed_desc = desc &
                            //format_item_separator//move_to_separator%get() &
                            //format_item_separator//enclose(overwriting_spaces, '"') &
                            //format_item_separator//move_to_separator%get()
            return
        end if

        bracket_close = get_closing_brackets(bracket_open)

        select case (len_separator)
        case (0)
            enclosed_desc = enclose(bracket_open, '"') &
                            //format_item_separator//desc &
                            //format_item_separator//enclose(bracket_close, '"')

        case (1)
            ! 要素末尾に区切り文字が置かれる問題を回避するため，
            ! 現在位置を移動して出力された区切り文字を閉じ括弧で上書きする
            move_to_separator = move(-len_separator)

            enclosed_desc = enclose(bracket_open, '"') &
                            //format_item_separator//desc &
                            //format_item_separator//move_to_separator%get() &
                            //format_item_separator//enclose(bracket_close, '"')

        case default
            ! 要素末尾に区切り文字が置かれる問題を回避するため，
            ! 現在位置を移動して出力された区切り文字を閉じ括弧と空白で
            ! 上書きし，閉じ括弧の直後に移動する
            move_to_separator = move(-len_separator)
            move_after_closing_bracket = move(-(len_separator - 1))
            overwriting_spaces = repeat(" ", len_separator - 1)

            enclosed_desc = enclose(bracket_open, '"') &
                            //format_item_separator//desc &
                            //format_item_separator//move_to_separator%get() &
                            //format_item_separator//enclose(bracket_close//overwriting_spaces, '"') &
                            //format_item_separator//move_after_closing_bracket%get()
        end select
    end function catenate_brackets
end module fed_array
