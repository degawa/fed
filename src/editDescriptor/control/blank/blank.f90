module fed_editDescriptor_control_blank
    use, intrinsic :: iso_c_binding
    use :: fed_editDescriptor
    use :: fed_editDescriptor_control
    use :: fed_format_item
    use :: fed_format_items
    implicit none
    private
    public :: blank_mode_null
    public :: blank_mode_zero
    public :: get_blank_default ! open to public only for unit tests

    character(*), private, parameter :: blank_interpretation_null_edit_descriptor_symbol = "BN"
        !! BN形編集に用いられる英字定数
    character(*), private, parameter :: blank_interpretation_zero_edit_descriptor_symbol = "BZ"
        !! BZ形編集に用いられる英字定数

    !>空白解釈モードを変更する手続を取り扱う派生型．
    !>
    !>@note 一つのparameterを宣言してそれを利用する事を想定している．
    type, public :: blank_mode_command_type
    contains
        procedure, private, nopass :: change_blank_interpretation_mode_to_null_desc
        procedure, private, nopass :: change_blank_interpretation_mode_to_null_item
        procedure, private, nopass :: change_blank_interpretation_mode_to_null_items
        generic, public :: null => &
            change_blank_interpretation_mode_to_null_desc, &
            change_blank_interpretation_mode_to_null_item, &
            change_blank_interpretation_mode_to_null_items
        !* 空白解釈モードをNULLに変更した編集項目並びを返却
        procedure, private, nopass :: change_blank_interpretation_mode_to_zero_desc
        procedure, private, nopass :: change_blank_interpretation_mode_to_zero_item
        procedure, private, nopass :: change_blank_interpretation_mode_to_zero_items
        generic, public :: zero => &
            change_blank_interpretation_mode_to_zero_desc, &
            change_blank_interpretation_mode_to_zero_item, &
            change_blank_interpretation_mode_to_zero_items
        !* 空白解釈モードをZEROに変更した編集項目並びを返却
    end type blank_mode_command_type

    type(blank_mode_command_type), public, parameter :: blank_mode = blank_mode_command_type()
        !! 空白解釈モードを変更する手続

    !>空白の解釈モードをNULLに変更した編集項目並びを返す
    interface blank_mode_null
        procedure :: change_blank_interpretation_mode_to_null_desc
        procedure :: change_blank_interpretation_mode_to_null_item
        procedure :: change_blank_interpretation_mode_to_null_items
    end interface

    !>空白の解釈モードをZEROに変更した編集項目並びを返す
    interface blank_mode_zero
        procedure :: change_blank_interpretation_mode_to_zero_desc
        procedure :: change_blank_interpretation_mode_to_zero_item
        procedure :: change_blank_interpretation_mode_to_zero_items
    end interface

contains
    !>引数として渡された編集記述子に，空白解釈モードをNULLに
    !>変更するように`BN`形編集記述子を追加した編集項目並びを返す．
    function change_blank_interpretation_mode_to_null_desc(edit_descriptor) result(new_format_items)
        implicit none
        class(edit_descriptor_type), intent(in) :: edit_descriptor
            !! 空白解釈モードがNULLに変更される編集記述子
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(blank_interpretation_null_edit_descriptor_symbol) &
                           //edit_descriptor &
                           //ctrl(get_blank_default())
    end function change_blank_interpretation_mode_to_null_desc

    !>引数として渡された編集項目に，空白解釈モードをNULLに
    !>変更するように`BN`形編集記述子を追加した編集項目並びを返す．
    function change_blank_interpretation_mode_to_null_item(format_item) result(new_format_items)
        implicit none
        type(format_item_type), intent(in) :: format_item
            !! 空白解釈モードがNULLに変更される編集項目
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(blank_interpretation_null_edit_descriptor_symbol) &
                           //format_item &
                           //ctrl(get_blank_default())
    end function change_blank_interpretation_mode_to_null_item

    !>引数として渡された編集項目並びに，空白解釈モードをNULLに
    !>変更するように`BN`形編集記述子を追加した編集項目並びを返す．
    function change_blank_interpretation_mode_to_null_items(format_items) result(new_format_items)
        implicit none
        type(format_items_type), intent(in) :: format_items
            !! 空白解釈モードがNULLに変更される編集項目並び
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(blank_interpretation_null_edit_descriptor_symbol) &
                           //format_items &
                           //ctrl(get_blank_default())
    end function change_blank_interpretation_mode_to_null_items

    !>引数として渡された編集記述子に，空白解釈モードをZEROに
    !>変更するように`BZ`形編集記述子を追加した編集項目並びを返す．
    function change_blank_interpretation_mode_to_zero_desc(edit_descriptor) result(new_format_items)
        implicit none
        class(edit_descriptor_type), intent(in) :: edit_descriptor
            !! 空白解釈モードがZEROに変更される編集記述子
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(blank_interpretation_zero_edit_descriptor_symbol) &
                           //edit_descriptor &
                           //ctrl(get_blank_default())
    end function change_blank_interpretation_mode_to_zero_desc

    !>引数として渡された編集項目に，空白解釈モードをZEROに
    !>変更するように`BZ`形編集記述子を追加した編集項目並びを返す．
    function change_blank_interpretation_mode_to_zero_item(format_item) result(new_format_items)
        implicit none
        type(format_item_type), intent(in) :: format_item
            !! 空白解釈モードがZEROに変更される編集項目
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(blank_interpretation_zero_edit_descriptor_symbol) &
                           //format_item &
                           //ctrl(get_blank_default())
    end function change_blank_interpretation_mode_to_zero_item

    !>引数として渡された編集項目並びに，空白解釈モードをZEROに
    !>変更するように`BZ`形編集記述子を追加した編集項目並びを返す．
    function change_blank_interpretation_mode_to_zero_items(format_items) result(new_format_items)
        implicit none
        type(format_items_type), intent(in) :: format_items
            !! 空白解釈モードがZEROに変更される編集項目並び
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(blank_interpretation_zero_edit_descriptor_symbol) &
                           //format_items &
                           //ctrl(get_blank_default())
    end function change_blank_interpretation_mode_to_zero_items

    !>標準の空白解釈モードを表す編集記述子の英字定数を返す．
    function get_blank_default() result(default)
        implicit none
        character(2) :: default
            !! 標準の空白解釈モードを表す編集記述子の英字定数

        default = blank_interpretation_zero_edit_descriptor_symbol
    end function get_blank_default
end module fed_editDescriptor_control_blank
