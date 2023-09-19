module fed_format_items
    use, intrinsic :: iso_fortran_env
    use :: fed_format_item
    use :: fed_editDescriptor
    use :: fed_editDescriptor_characterString
    implicit none
    private
    public :: items
    public :: operator(//)

    !>書式項目並びを取り扱う派生型．
    !>書式項目並びは，複数の書式項目から構成されるので，
    !>それらを書式項目の配列を用いて表現する．
    type, public :: format_items_type
        type(format_item_type), private, allocatable :: item(:)
            !! 書式項目並び
    contains
        procedure, private, pass :: set_at => set_edit_descriptor_at
        !* 指定した番号の書式項目の編集記述子を設定
        procedure, public, pass :: get_edit_descriptor_at
        !* 指定した番号の書式項目の編集記述子を文字列で返却
        procedure, public, pass :: get_number_of_items
        !* 書式項目の個数を返却
        procedure, public, pass :: is_data_edit_descriptor
        !* 指定した番号の書式項目がデータ編集記述子かを検査
        procedure, public, pass :: is_control_edit_descriptor
        !* 指定した番号の書式項目が制御編集記述子かを検査
        procedure, public, pass :: is_character_string_edit_descriptor
        !* 編集記述子が文字列編集記述子かを検査
        procedure, public, pass :: has_data_edit_descriptor
        !* 書式項目並びの中にデータ編集記述子があるかを返却
        procedure, public, pass :: destruct
        !* 書式項目並びを破棄
        final :: finalize
        !* 後始末
    end type format_items_type

    !>書式項目並びを生成するコンストラクタ
    interface items
        procedure :: construct_format_items_w_descriptor
        procedure :: construct_format_items_w_item
    end interface

    !>書式項目並びを生成する演算子
    interface operator(//)
        procedure :: catenate_desc_desc
        procedure :: catenate_desc_item
        procedure :: catenate_desc_items
        procedure :: catenate_desc_char

        procedure :: catenate_item_desc
        procedure :: catenate_item_item
        procedure :: catenate_item_items
        procedure :: catenate_item_char

        procedure :: catenate_items_desc
        procedure :: catenate_items_item
        procedure :: catenate_items_items
        procedure :: catenate_items_char

        procedure :: catenate_char_desc
        procedure :: catenate_char_item
        procedure :: catenate_char_items
    end interface

contains
    !>編集記述子から書式項目並びformat_item_typeのインスタンスを生成して返す．
    function construct_format_items_w_descriptor(edit_descriptor) result(new_format_items)
        implicit none
        class(edit_descriptor_type), intent(in) :: edit_descriptor
            !! 編集記述子
        type(format_items_type) :: new_format_items
            !! 生成されるformat_items_typeインスタンス

        allocate (new_format_items%item(1))
        call new_format_items%set_at(1, edit_descriptor)
    end function construct_format_items_w_descriptor

    !>書式項目から書式項目並びformat_item_typeのインスタンスを生成して返す．
    function construct_format_items_w_item(format_item) result(new_format_items)
        implicit none
        type(format_item_type), intent(in) :: format_item
            !! 書式項目
        type(format_items_type) :: new_format_items
            !! 生成されるformat_items_typeインスタンス

        allocate (new_format_items%item(1), source=format_item)
    end function construct_format_items_w_item

    !>指定した番号の書式項目の編集記述子を設定する．
    !>指定した番号が範囲外の場合，編集記述子は設定されない．
    subroutine set_edit_descriptor_at(this, item_number, edit_descriptor)
        implicit none
        class(format_items_type), intent(inout) :: this
            !! 当該実体仮引数
        integer(int32), intent(in) :: item_number
            !! 書式項目の番号
        class(edit_descriptor_type), intent(in) :: edit_descriptor
            !! 設定される編集記述子

        if (item_number < 1 .or. this%get_number_of_items() < item_number) then
            return
        end if

        call this%item(item_number)%set(edit_descriptor)
    end subroutine set_edit_descriptor_at

    !>指定した番号の書式項目の編集記述子を文字列で返す．
    !>指定した番号が範囲外の場合，長さ0の文字列`""`を返す．
    pure function get_edit_descriptor_at(this, item_number) result(edit_descriptor)
        implicit none
        class(format_items_type), intent(in) :: this
            !! 当該実体仮引数
        integer(int32), intent(in) :: item_number
            !! 書式項目の番号
        character(:), allocatable :: edit_descriptor
            !! 編集記述子

        if (item_number < 1 .or. this%get_number_of_items() < item_number) then
            edit_descriptor = ""
            return
        end if

        edit_descriptor = this%item(item_number)%get_edit_descriptor()
    end function get_edit_descriptor_at

    !>書式項目の個数を返す．
    pure function get_number_of_items(this) result(number_of_items)
        implicit none
        class(format_items_type), intent(in) :: this
            !! 当該実体仮引数
        integer(int32) :: number_of_items
            !! 書式項目の個数

        if (allocated(this%item)) then
            number_of_items = size(this%item)
        else
            number_of_items = 0
        end if
    end function get_number_of_items

    !>指定した番号の書式項目がデータ編集記述子であれば`.true.`，
    !>そうでなければ`.false.`を返す．
    !>`i`が範囲外の場合，あるいは書式項目がない場合も`.false.`を返す．
    pure logical function is_data_edit_descriptor(this, i)
        implicit none
        class(format_items_type), intent(in) :: this
            !! 当該実体仮引数
        integer(int32), intent(in) :: i
            !! 種別を判定する項目番号

        if (is_item_number_out_of_range(i, this%get_number_of_items())) then
            is_data_edit_descriptor = .false.
            return
        end if

        is_data_edit_descriptor = this%item(i)%is_data_edit_descriptor()
    end function is_data_edit_descriptor

    !>指定した番号の書式項目が制御編集記述子であれば`.true.`，
    !>そうでなければ`.false.`を返す．
    !>`i`が範囲外の場合，あるいは書式項目がない場合も`.false.`を返す．
    pure logical function is_control_edit_descriptor(this, i)
        implicit none
        class(format_items_type), intent(in) :: this
            !! 当該実体仮引数
        integer(int32), intent(in) :: i
            !! 種別を判定する項目番号

        if (is_item_number_out_of_range(i, this%get_number_of_items())) then
            is_control_edit_descriptor = .false.
            return
        end if

        is_control_edit_descriptor = this%item(i)%is_control_edit_descriptor()
    end function is_control_edit_descriptor

    !>指定した番号の書式項目が文字列編集記述子であれば`.true.`，
    !>そうでなければ`.false.`を返す．
    !>`i`が有効範囲外の場合，あるいは書式項目がない場合も`.false.`を返す．
    pure logical function is_character_string_edit_descriptor(this, i)
        implicit none
        class(format_items_type), intent(in) :: this
            !! 当該実体仮引数
        integer(int32), intent(in) :: i
            !! 種別を判定する項目番号

        if (is_item_number_out_of_range(i, this%get_number_of_items())) then
            is_character_string_edit_descriptor = .false.
            return
        end if

        is_character_string_edit_descriptor = this%item(i)%is_character_string_edit_descriptor()
    end function is_character_string_edit_descriptor

    !>書式項目番号が有効範囲外にある場合に`.true.`，
    !>範囲内にある場合に`.false.`を返す．
    pure logical function is_item_number_out_of_range(item_number, number_of_items)
        implicit none
        integer(int32), intent(in) :: item_number
            !! 書式項目番号
        integer(int32), intent(in) :: number_of_items
            !! 書式項目数

        if (number_of_items < 1 .or. (item_number < 1 .or. number_of_items < item_number)) then
            is_item_number_out_of_range = .true.
        else
            is_item_number_out_of_range = .false.
        end if
    end function is_item_number_out_of_range

    !>書式項目並びの中にデータ編集記述子があれば`.true.`，
    !>なければ`.false.`を返す．
    pure logical function has_data_edit_descriptor(this)
        implicit none
        class(format_items_type), intent(in) :: this
        !! 当該実体仮引数

        integer(int32) :: num_items, i

        has_data_edit_descriptor = .false.
        num_items = this%get_number_of_items()

        if (num_items < 1) return

        do i = 1, num_items
            has_data_edit_descriptor = has_data_edit_descriptor .or. &
                                       this%item(i)%is_data_edit_descriptor()
        end do
    end function has_data_edit_descriptor

    !>書式項目並びを破棄する．
    subroutine destruct(this)
        implicit none
        class(format_items_type), intent(inout) :: this
            !! 当該実体仮引数

        if (allocated(this%item)) &
            deallocate (this%item)
    end subroutine destruct

    !>インスタンスを後始末する．
    subroutine finalize(this)
        implicit none
        type(format_items_type), intent(inout) :: this
            !! 当該実体仮引数

        call this%destruct()
    end subroutine finalize

    !>二つの編集記述子から書式項目並びを生成して返す．
    !>結合演算子`//`をオーバーロードする．
    function catenate_desc_desc(lhs, rhs) result(new_format_items)
        implicit none
        class(edit_descriptor_type), intent(in) :: lhs
            !! 結合演算子左辺の編集記述子
        class(edit_descriptor_type), intent(in) :: rhs
            !! 結合演算子右辺の編集記述子
        type(format_items_type) :: new_format_items
            !! 生成された書式項目並び

        new_format_items%item = [item(lhs), item(rhs)]
    end function catenate_desc_desc

    !>編集記述子と書式項目から書式項目並びを生成して返す．
    !>結合演算子`//`をオーバーロードする．
    function catenate_desc_item(lhs, rhs) result(new_format_items)
        implicit none
        class(edit_descriptor_type), intent(in) :: lhs
            !! 結合演算子左辺の編集記述子
        type(format_item_type), intent(in) :: rhs
            !! 結合演算子右辺の書式項目
        type(format_items_type) :: new_format_items
            !! 生成された書式項目並び

        new_format_items%item = [item(lhs), rhs]
    end function catenate_desc_item

    !>編集記述子と書式項目並びから書式項目並びを生成して返す．
    !>結合演算子`//`をオーバーロードする．
    function catenate_desc_items(lhs, rhs) result(new_format_items)
        implicit none
        class(edit_descriptor_type), intent(in) :: lhs
            !! 結合演算子左辺の編集記述子
        type(format_items_type), intent(in) :: rhs
            !! 結合演算子右辺の書式項目並び
        type(format_items_type) :: new_format_items
            !! 生成された書式項目並び

        new_format_items%item = [item(lhs), rhs%item]
    end function catenate_desc_items

    !>編集記述子と文字列（文字列編集記述子）から書式項目並びを生成して返す．
    !>結合演算子`//`をオーバーロードする．
    function catenate_desc_char(lhs, rhs) result(new_format_items)
        implicit none
        class(edit_descriptor_type), intent(in) :: lhs
            !! 結合演算子左辺の編集記述子
        character(*), intent(in) :: rhs
            !! 文字列
        type(format_items_type) :: new_format_items
            !! 生成された書式項目並び

        new_format_items%item = [item(lhs), item(str(rhs))]
    end function catenate_desc_char

    !>書式項目並びと編集記述子から書式項目並びを生成して返す．
    !>結合演算子`//`をオーバーロードする．
    function catenate_items_desc(lhs, rhs) result(new_format_items)
        implicit none
        type(format_items_type), intent(in) :: lhs
            !! 結合演算子左辺の書式項目並び
        class(edit_descriptor_type), intent(in) :: rhs
            !! 結合演算子右辺の編集記述子
        type(format_items_type) :: new_format_items
            !! 生成された書式項目並び

        new_format_items%item = [lhs%item, item(rhs)]
    end function catenate_items_desc

    !>書式項目並びと書式項目から書式項目並びを生成して返す．
    !>結合演算子`//`をオーバーロードする．
    function catenate_items_item(lhs, rhs) result(format_items)
        implicit none
        type(format_items_type), intent(in) :: lhs
            !! 結合演算子左辺の書式項目並び
        type(format_item_type), intent(in) :: rhs
            !! 結合演算子右辺の書式項目
        type(format_items_type) :: format_items
            !! 生成された書式項目並び

        format_items%item = [lhs%item, rhs]
    end function catenate_items_item

    !>書式項目並びと書式項目並びから書式項目並びを生成して返す．
    !>結合演算子`//`をオーバーロードする．
    function catenate_items_items(lhs, rhs) result(format_items)
        implicit none
        type(format_items_type), intent(in) :: lhs
            !! 結合演算子左辺の書式項目並び
        type(format_items_type), intent(in) :: rhs
            !! 結合演算子右辺の書式項目並び
        type(format_items_type) :: format_items
            !! 生成された書式項目並び

        format_items%item = [lhs%item, rhs%item]
    end function catenate_items_items

    !>書式項目並びと文字列（文字列編集記述子）から書式項目並びを生成して返す．
    !>結合演算子`//`をオーバーロードする．
    function catenate_items_char(lhs, rhs) result(new_format_items)
        implicit none
        type(format_items_type), intent(in) :: lhs
            !! 結合演算子左辺の書式項目並び
        character(*), intent(in) :: rhs
            !! 文字列
        type(format_items_type) :: new_format_items
            !! 生成された書式項目並び

        new_format_items%item = [lhs%item, item(str(rhs))]
    end function catenate_items_char

    !>書式項目と編集記述子から書式項目並びを生成して返す．
    !>結合演算子`//`をオーバーロードする．
    function catenate_item_desc(lhs, rhs) result(new_format_items)
        implicit none
        type(format_item_type), intent(in) :: lhs
            !! 結合演算子左辺の書式項目
        class(edit_descriptor_type), intent(in) :: rhs
            !! 結合演算子右辺の編集記述子
        type(format_items_type) :: new_format_items
            !! 生成された書式項目並び

        new_format_items%item = [lhs, item(rhs)]
    end function catenate_item_desc

    !>書式項目と書式項目から書式項目並びを生成して返す．
    !>結合演算子`//`をオーバーロードする．
    function catenate_item_item(lhs, rhs) result(format_items)
        implicit none
        type(format_item_type), intent(in) :: lhs
            !! 結合演算子左辺の書式項目
        type(format_item_type), intent(in) :: rhs
            !! 結合演算子右辺の書式項目
        type(format_items_type) :: format_items
            !! 生成された書式項目並び

        format_items%item = [lhs, rhs]
    end function catenate_item_item

    !>書式項目と書式項目並びから書式項目並びを生成して返す．
    !>結合演算子`//`をオーバーロードする．
    function catenate_item_items(lhs, rhs) result(format_items)
        implicit none
        type(format_item_type), intent(in) :: lhs
            !! 結合演算子左辺の書式項目
        type(format_items_type), intent(in) :: rhs
            !! 結合演算子右辺の書式項目並び
        type(format_items_type) :: format_items
            !! 生成された書式項目並び

        format_items%item = [lhs, rhs%item]
    end function catenate_item_items

    !>書式項目と文字列（文字列編集記述子）から書式項目並びを生成して返す．
    !>結合演算子`//`をオーバーロードする．
    function catenate_item_char(lhs, rhs) result(new_format_items)
        implicit none
        type(format_item_type), intent(in) :: lhs
            !! 結合演算子左辺の書式項目
        character(*), intent(in) :: rhs
            !! 文字列
        type(format_items_type) :: new_format_items
            !! 生成された書式項目並び

        new_format_items%item = [lhs, item(str(rhs))]
    end function catenate_item_char

    !>文字列（文字列編集記述子）と編集記述子から書式項目並びを生成して返す．
    !>結合演算子`//`をオーバーロードする．
    function catenate_char_desc(lhs, rhs) result(new_format_items)
        implicit none
        character(*), intent(in) :: lhs
            !! 文字列
        class(edit_descriptor_type), intent(in) :: rhs
            !! 結合演算子右辺の編集記述子
        type(format_items_type) :: new_format_items
            !! 生成された書式項目並び

        new_format_items%item = [item(str(lhs)), item(rhs)]
    end function catenate_char_desc

    !>文字列（文字列編集記述子）と書式項目から書式項目並びを生成して返す．
    !>結合演算子`//`をオーバーロードする．
    function catenate_char_item(lhs, rhs) result(new_format_items)
        implicit none
        character(*), intent(in) :: lhs
            !! 文字列
        type(format_item_type), intent(in) :: rhs
            !! 結合演算子右辺の書式項目並び
        type(format_items_type) :: new_format_items
            !! 生成された書式項目並び

        new_format_items%item = [item(str(lhs)), rhs]
    end function catenate_char_item

    !>文字列（文字列編集記述子）と書式項目並びから書式項目並びを生成して返す．
    !>結合演算子`//`をオーバーロードする．
    function catenate_char_items(lhs, rhs) result(new_format_items)
        implicit none
        character(*), intent(in) :: lhs
            !! 文字列
        type(format_items_type), intent(in) :: rhs
            !! 結合演算子右辺の書式項目並び
        type(format_items_type) :: new_format_items
            !! 生成された書式項目並び

        new_format_items%item = [item(str(lhs)), rhs%item]
    end function catenate_char_items
end module fed_format_items
