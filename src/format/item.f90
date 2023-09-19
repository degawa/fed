module fed_format_item
    use :: fed_editDescriptor
    implicit none
    private
    public :: item

    !>書式項目を取り扱う派生型．
    !>
    !>@note
    !>書式項目は，データ編集記述子，制御編集記述子，文字列編集記述子
    !>からならるため，この型を拡張してそれらの型の祖先型とするべきである．
    !>しかし，Fortranでは異なる型からなる配列は宣言できないため
    !>それらの型の祖先型`edit_descriptor_type`の配列を成分として持たせる
    !>@endnote
    type, public :: format_item_type
        class(edit_descriptor_type), private, allocatable :: edit_descriptor
    contains
        procedure, public, pass :: set => set_edit_descriptor
        !* 編集記述子を設定
        procedure, public, pass :: get_edit_descriptor
        !* 編集記述子を文字列で返却
        procedure, public, pass :: has_edit_descriptor
        !* 編集記述子が設定されているか否かを検査
        procedure, public, pass :: is_data_edit_descriptor
        !* 編集記述子がデータ編集記述子かを検査
        procedure, public, pass :: destruct
        !* 編集記述子を破棄
        final :: finalize
        !* 後始末
    end type format_item_type

    interface item
        procedure :: construct_format_item
    end interface

contains
    !>編集記述子からformat_item_typeインスタンスを生成して返却する．
    function construct_format_item(edit_descriptor) result(new_format_item)
        implicit none
        class(edit_descriptor_type), intent(in) :: edit_descriptor
            !! 編集記述子
        type(format_item_type) :: new_format_item
            !! 生成されるformat_item_typeインスタンス

        call new_format_item%set(edit_descriptor)
    end function construct_format_item

    !>文字列として渡された編集記述子から，編集記述子を設定する．
    subroutine set_edit_descriptor(this, edit_descriptor)
        implicit none
        class(format_item_type), intent(inout) :: this
            !! 当該実体仮引数
        class(edit_descriptor_type), intent(in) :: edit_descriptor
            !! 編集記述子

        if (this%has_edit_descriptor()) &
            call this%destruct()
        allocate (this%edit_descriptor, source=edit_descriptor)
    end subroutine set_edit_descriptor

    !>編集記述子が設定されていれば`.true.`，そうでなければ`.false.`を返す．
    pure logical function has_edit_descriptor(this)
        implicit none
        class(format_item_type), intent(in) :: this
            !! 当該実体仮引数

        has_edit_descriptor = allocated(this%edit_descriptor)
    end function has_edit_descriptor

    !>編集記述子がデータ編集記述子であれば`.true.`，そうでなければ`.false.`を返す．
    pure logical function is_data_edit_descriptor(this)
        use :: fed_editDescriptor_data
        implicit none
        class(format_item_type), intent(in) :: this
            !! 当該実体仮引数

        type(data_edit_descriptor_type) :: type_mold

        is_data_edit_descriptor = this%has_edit_descriptor() .and. &
                                  extends_type_of(this%edit_descriptor, type_mold)
    end function is_data_edit_descriptor

    !>編集記述子を文字列で返す．
    !>編集記述子が設定されていない場合，長さ0の文字列`""`を返す．
    pure function get_edit_descriptor(this) result(edit_descriptor)
        implicit none
        class(format_item_type), intent(in) :: this
            !! 当該実体仮引数
        character(:), allocatable :: edit_descriptor
            !! 編集記述子

        if (this%has_edit_descriptor()) then
            edit_descriptor = this%edit_descriptor%get()
        else
            edit_descriptor = ""
        end if
    end function get_edit_descriptor

    !>編集記述子を破棄する．
    subroutine destruct(this)
        implicit none
        class(format_item_type), intent(inout) :: this
            !! 当該実体仮引数

        if (this%has_edit_descriptor()) &
            deallocate (this%edit_descriptor)
    end subroutine destruct

    !>インスタンスを後始末する．
    subroutine finalize(this)
        implicit none
        type(format_item_type), intent(inout) :: this
            !! 当該実体仮引数

        call this%destruct()
    end subroutine finalize
end module fed_format_item
