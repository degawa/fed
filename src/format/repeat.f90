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
    !>書式反復数がなければ無制限繰り返し，0以下であれば書式反復数を1とする．
    !>無制限繰り返しを行う場合，書式項目並びにデータ編集記述子がなければ，
    !>書式項目並びが書式項目として返される．
    function construct_repeated_format_items_w_sep(format_items, separator, repeat_count) result(repeated_item)
        use :: strings_enclose
        implicit none
        type(format_items_type), intent(in) :: format_items
            !! 反復数が設定される書式項目並び
        character(*), intent(in) :: separator
            !! 区切り文字
        integer(int32), intent(in), optional :: repeat_count
            !! 書式反復数(>0)
        type(format_item_type) :: repeated_item
            !! 反復数をもつ書式項目

        character(:), allocatable :: desc, count_str

        ! 書式項目並びの編集記述子を結合する
        desc = catenate_format_items(format_items, separator)

        ! 書式項目が無効だった場合（repeat(move(0), 3, separator=",")等）は，
        ! 空の編集記述子を持つ書式項目を返す
        if (desc == "") then
            call repeated_item%set(edit_descriptor_type(""))
            return
        end if

        ! 3(I0,:,",")を実現できるように，書式項目末尾にも必ず区切り文字を付ける
        desc = desc//','//enclose(separator, '"')

        ! 書式反復数を文字列に変換
        count_str = get_repeat_count_string(repeat_count)

        if (.not. format_items%has_data_edit_descriptor() .and. count_str == "*") then
            call repeated_item%set(to_edit_descriptor(desc, type_mold=format_items%get_item_at(1)))
            return
        end if

        call repeated_item%set(to_edit_descriptor(count_str//enclose(desc, '('), format_items%get_item_at(1)))
    end function construct_repeated_format_items_w_sep

    !>書式項目並びから反復数をもつ書式項目を生成して返す．
    !>
    !>書式反復数がなければ無制限繰り返し，0以下であれば書式反復数を1とする．
    !>無制限繰り返しを行う場合，書式項目並びにデータ編集記述子がなければ，
    !>書式項目並びが書式項目として返される．
    function construct_repeated_format_items(format_items, repeat_count) result(repeated_item)
        use :: strings_enclose
        implicit none
        type(format_items_type), intent(in) :: format_items
            !! 反復数が設定される書式項目並び
        integer(int32), intent(in), optional :: repeat_count
            !! 書式反復数(>0)
        type(format_item_type) :: repeated_item
            !! 反復数をもつ書式項目

        character(:), allocatable :: desc, count_str

        ! 書式項目並びの編集記述子を結合する
        desc = catenate_format_items(format_items)

        ! 書式項目が無効だった場合（repeat(move(0), 3)等）は，
        ! 空の編集記述子を持つ書式項目を返す
        if (desc == "") then
            call repeated_item%set(edit_descriptor_type(""))
            return
        end if

        ! 書式反復数を文字列に変換
        count_str = get_repeat_count_string(repeat_count)

        if (.not. format_items%has_data_edit_descriptor() .and. count_str == "*") then
            call repeated_item%set(to_edit_descriptor(desc, type_mold=format_items%get_item_at(1)))
            return
        end if

        call repeated_item%set(to_edit_descriptor(count_str//enclose(desc, '('), format_items%get_item_at(1)))
    end function construct_repeated_format_items

    !>書式項目並びの編集記述子を結合して返す．
    pure function catenate_format_items(format_items, separator) result(desc)
        use :: strings_enclose
        implicit none
        type(format_items_type), intent(in) :: format_items
            !! 反復数が設定される書式項目並び
        character(*), intent(in), optional :: separator
            !! 区切り文字
        character(:), allocatable :: desc

        integer(int32) :: num_items, i
        character(:), allocatable :: desc_i, enclosed_separator_w_item_sep

        ! 区切り文字が渡されていれば'",",'を作り，
        ! 渡されていなければ空白''とする．
        if (present(separator)) then
            enclosed_separator_w_item_sep = enclose(separator, '"')//','
        else
            enclosed_separator_w_item_sep = ""
        end if

        ! 書式項目並びの編集記述子を結合する
        num_items = format_items%get_number_of_items()

        desc = ""
        do i = 1, num_items - 1
            desc_i = format_items%get_edit_descriptor_at(i)
            if (desc_i == "") cycle

            desc = desc//desc_i//','

            if (format_items%is_data_edit_descriptor(i + 1)) &
                desc = desc//enclosed_separator_w_item_sep
        end do
        desc_i = format_items%get_edit_descriptor_at(i)
        if (desc_i /= "") desc = desc//desc_i
    end function catenate_format_items

    !>書式反復数を文字列に変換して返す．
    !>書式反復数がなければ無制限繰り返し，0以下であれば書式反復数を1とする．
    pure function get_repeat_count_string(repeat_count) result(count_str)
        use :: stdlib_strings
        implicit none
        integer(int32), intent(in), optional :: repeat_count
            !! 書式反復数(>0)
        character(:), allocatable :: count_str
            !! 文字列に変換された書式反復数

        integer(int32) :: cnt

        if (present(repeat_count)) then
            cnt = repeat_count
            if (cnt <= 0) cnt = 1
            count_str = to_string(cnt)
        else
            count_str = "*"
        end if
    end function get_repeat_count_string

    !>`type_mold`と同じ種類の書式記述子を返す．
    function to_edit_descriptor(desc, type_mold) result(new_desc)
        use :: fed_editDescriptor_data
        use :: fed_editDescriptor_control
        use :: fed_editDescriptor_characterString
        implicit none
        character(*), intent(in) :: desc
            !! 書式記述子
        type(format_item_type), intent(in) :: type_mold
            !! 型見本となる書式項目
        class(edit_descriptor_type), allocatable :: new_desc
            !! 書式項目が持つ書式記述子と同じ型の書式記述子

        if (type_mold%is_data_edit_descriptor()) then
            allocate (new_desc, source=dat(desc))
            return
        end if

        if (type_mold%is_character_string_edit_descriptor()) then
            ! source=str(desc)にすると，descが" "で囲まれてしまうため，
            ! moldで型を確定した後に，setで編集記述子を直接設定する．
            allocate (new_desc, mold=str(""))
            call new_desc%set(desc)
            return
        end if

        if (type_mold%is_control_edit_descriptor()) then
            allocate (new_desc, source=ctrl(desc))
            return
        end if
    end function to_edit_descriptor

    !>書式項目並びから反復数をもつ書式項目を生成して返す．
    !>
    !>書式反復数がなければ無制限繰り返し，0以下であれば書式反復数を1とする．
    !>無制限繰り返しを行う場合，書式項目並びにデータ編集記述子がなければ，
    !>書式項目並びが書式項目として返される．
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

    !>書式項目並びから反復数をもつ書式項目を生成して返す．
    !>
    !>`"`以外の区切り文字`separator`は，書式項目番号が2番目以降の
    !>データ編集記述子の前に置かれる．
    !>
    !>書式反復数がなければ無制限繰り返し，0以下であれば書式反復数を1とする．
    !>無制限繰り返しを行う場合，書式項目並びにデータ編集記述子がなければ，
    !>書式項目並びが書式項目として返される．
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
