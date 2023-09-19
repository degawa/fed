module fed_repeat
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor
    use :: fed_format_item
    use :: fed_format_items
    implicit none
    private
    public :: repeat

    interface repeat
        procedure :: construct_repeated_format_items
        procedure :: construct_repeated_format_items_w_sep
        procedure :: construct_repeated_format_items_by_descriptor
        procedure :: construct_repeated_format_items_w_sep_by_descriptor
    end interface

contains
    !>書式項目並びから反復数をもつ書式項目を生成して返す．
    !>
    !>`"`以外の区切り文字`separator`は，書式項目番号が2番目以降の
    !>データ編集記述子の前に置かれる．
    !>
    !>@note 書式反復数がなければ無制限繰り返し，0以下であれば書式反復数を1とする．
    !>@warning
    !>無制限繰り返しを行う場合，書式項目並びには
    !>少なくとも一つのデータ編集記述子が必要．
    !>@endwarning
    function construct_repeated_format_items_w_sep(format_items, separator, repeat_count) result(repeated_item)
        use :: strings_enclose
        use :: stdlib_optval
        use :: stdlib_strings
        implicit none
        type(format_items_type), intent(in) :: format_items
            !! 反復数が設定される書式項目並び
        character(*), intent(in) :: separator
            !! 区切り文字
        integer(int32), intent(in), optional :: repeat_count
            !! 書式反復数(>0)
        type(format_item_type) :: repeated_item
            !! 反復数をもつ書式項目

        integer(int32) :: i, num_items, cnt
        character(:), allocatable :: desc, count_str

        ! 書式項目並びの編集記述子を結合する
        num_items = format_items%get_number_of_items()

        desc = ""
        do i = 1, num_items - 1
            desc = desc//format_items%get_edit_descriptor_at(i)//','

            if (format_items%is_data_edit_descriptor(i + 1)) &
                desc = desc//enclose(separator, '"')//','
        end do
        desc = desc//format_items%get_edit_descriptor_at(num_items)//','
        desc = desc//enclose(separator, '"')
        ! 3(I0,:,",")を実現できるように，書式項目末尾にも必ず区切り文字を付ける

        ! 書式反復数を文字列に変換
        if (present(repeat_count)) then
            cnt = repeat_count
            if (cnt <= 0) cnt = 1
            count_str = to_string(cnt)
        else
            count_str = "*"
        end if

        call repeated_item%set(edit_descriptor_type(count_str//enclose(desc, '(')))
    end function construct_repeated_format_items_w_sep

    !>書式項目並びから反復数をもつ書式項目を生成して返す．
    !>
    !>@note 書式反復数がなければ無制限繰り返し，0以下であれば書式反復数を1とする．
    !>@warning
    !>無制限繰り返しを行う場合，書式項目並びには
    !>少なくとも一つのデータ編集記述子が必要．
    !>@endwarning
    function construct_repeated_format_items(format_items, repeat_count) result(repeated_item)
        use :: strings_enclose
        use :: stdlib_optval
        use :: stdlib_strings
        implicit none
        type(format_items_type), intent(in) :: format_items
            !! 反復数が設定される書式項目並び
        integer(int32), intent(in), optional :: repeat_count
            !! 書式反復数(>0)
        type(format_item_type) :: repeated_item
            !! 反復数をもつ書式項目

        integer(int32) :: i, num_items, cnt
        character(:), allocatable :: desc, count_str

        ! 書式項目並びの編集記述子を結合する
        num_items = format_items%get_number_of_items()

        desc = ""
        do i = 1, num_items - 1
            desc = desc//format_items%get_edit_descriptor_at(i)//','
        end do
        desc = desc//format_items%get_edit_descriptor_at(num_items)

        ! 書式反復数を文字列に変換
        if (present(repeat_count)) then
            cnt = repeat_count
            if (cnt <= 0) cnt = 1
            count_str = to_string(cnt)
        else
            count_str = "*"
        end if
        call repeated_item%set(edit_descriptor_type(count_str//enclose(desc, '(')))
    end function construct_repeated_format_items

    !>編集記述子から反復数をもつ書式項目を生成して返す．
    !>
    !>@note 書式反復数がなければ無制限繰り返し，0以下であれば書式反復数を1とする．
    !>@warning
    !>無制限繰り返しを行う場合，編集記述子はデータ編集記述子でなければならない．
    !>@endwarning
    function construct_repeated_format_items_by_descriptor(edit_descriptor, repeat_count) result(repeated_item)
        implicit none
        class(edit_descriptor_type), intent(in) :: edit_descriptor
            !! 書式反復数が設定される編集記述子
        integer(int32), intent(in), optional :: repeat_count
            !! 書式反復数(>0)
        type(format_item_type) :: repeated_item
            !! 反復数をもつ書式項目

        repeated_item = repeat(items(edit_descriptor), repeat_count)
    end function construct_repeated_format_items_by_descriptor

    !>編集記述子から反復数をもつ書式項目を生成して返す．
    !>
    !>@note 書式反復数がなければ無制限繰り返し，0以下であれば書式反復数を1とする．
    !>@warning
    !>無制限繰り返しを行う場合，編集記述子はデータ編集記述子でなければならない．
    !>@endwarning
    function construct_repeated_format_items_w_sep_by_descriptor(edit_descriptor, separator, repeat_count) result(repeated_item)
        implicit none
        class(edit_descriptor_type), intent(in) :: edit_descriptor
            !! 書式反復数が設定される編集記述子
        character(*), intent(in) :: separator
            !! 区切り文字
        integer(int32), intent(in), optional :: repeat_count
            !! 書式反復数(>0)
        type(format_item_type) :: repeated_item
            !! 反復数をもつ書式項目

        repeated_item = repeat(items(edit_descriptor), separator, repeat_count)
    end function construct_repeated_format_items_w_sep_by_descriptor
end module fed_repeat
