module fed_format_items
    use, intrinsic :: iso_fortran_env
    use :: fed_format_item
    use :: fed_editDescriptor
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
        procedure :: catenate_desc_items
        procedure :: catenate_items_desc
        procedure :: catenate_items_items
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

    !>編集記述子と書式項目並びから書式項目並びを生成して返す．
    !>結合演算子`//`をオーバーロードする．
    function catenate_desc_items(lhs, rhs) result(new_format_items)
        implicit none
        class(edit_descriptor_type), intent(in) :: lhs
            !! 結合演算子左辺の編集記述子
        type(format_items_type), intent(in) :: rhs
            !! 結合演算子右辺の書式項目並びを
        type(format_items_type) :: new_format_items
            !! 生成された書式項目並び

        new_format_items%item = [item(lhs), rhs%item]
    end function catenate_desc_items

    !>書式項目並びと編集記述子から書式項目並びを生成して返す．
    !>結合演算子`//`をオーバーロードする．
    function catenate_items_desc(lhs, rhs) result(new_format_items)
        use, intrinsic :: iso_fortran_env
        implicit none
        type(format_items_type), intent(in) :: lhs
            !! 結合演算子左辺の書式項目並びを
        class(edit_descriptor_type), intent(in) :: rhs
            !! 結合演算子右辺の編集記述子
        type(format_items_type) :: new_format_items
            !! 生成された書式項目並び

        new_format_items%item = [lhs%item, item(rhs)]
    end function catenate_items_desc

    !>書式項目並びと編集記述子から書式項目並びを生成して返す．
    !>結合演算子`//`をオーバーロードする．
    function catenate_items_items(lhs, rhs) result(format_items)
        use, intrinsic :: iso_fortran_env
        implicit none
        type(format_items_type), intent(in) :: lhs
        type(format_items_type), intent(in) :: rhs

        type(format_items_type) :: format_items

        format_items%item = [lhs%item, rhs%item]
    end function catenate_items_items
end module fed_format_items
