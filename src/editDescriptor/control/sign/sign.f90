module fed_editDescriptor_control_sign
    use, intrinsic :: iso_c_binding
    use :: fed_editDescriptor
    use :: fed_editDescriptor_control
    use :: fed_format_item
    use :: fed_format_items
    use :: enumul
    implicit none
    private
    public :: sign_mode_suppress
    public :: sign_mode_plus
    public :: sign_mode_processor_defined
    public :: get_sign_default ! open to public only for unit tests

    character(*), private, parameter :: sign_suppress_edit_descriptor_symbol = "SS"
        !! SS形編集に用いられる英字定数
    character(*), private, parameter :: sign_plus_edit_descriptor_symbol = "SP"
        !! SP形編集に用いられる英字定数
    character(*), private, parameter :: sign_processor_defined_edit_descriptor_symbol = "S"
        !! S形編集に用いられる英字定数

    !>符号モードを変更する手続を取り扱う派生型．
    !>
    !>@note 一つのparameterを宣言してそれを利用する事を想定している．
    type, public :: sign_mode_command_type
    contains
        procedure, private, nopass :: change_sign_mode_to_suppress_desc
        procedure, private, nopass :: change_sign_mode_to_suppress_item
        procedure, private, nopass :: change_sign_mode_to_suppress_items
        generic, public :: suppress => &
            change_sign_mode_to_suppress_desc, &
            change_sign_mode_to_suppress_item, &
            change_sign_mode_to_suppress_items
        !* 符号モードをSUPPRESSに変更した編集項目並びを返却
        procedure, private, nopass :: change_sign_mode_to_plus_desc
        procedure, private, nopass :: change_sign_mode_to_plus_item
        procedure, private, nopass :: change_sign_mode_to_plus_items
        generic, public :: plus => &
            change_sign_mode_to_plus_desc, &
            change_sign_mode_to_plus_item, &
            change_sign_mode_to_plus_items
        !* 符号モードをPLUSに変更した編集項目並びを返却
        procedure, private, nopass :: change_sign_mode_to_processor_defined_desc
        procedure, private, nopass :: change_sign_mode_to_processor_defined_item
        procedure, private, nopass :: change_sign_mode_to_processor_defined_items
        generic, public :: processor_defined => &
            change_sign_mode_to_processor_defined_desc, &
            change_sign_mode_to_processor_defined_item, &
            change_sign_mode_to_processor_defined_items
        !* 符号モードをPROCESSOR_DEFINEDに変更した編集項目並びを返却
    end type sign_mode_command_type

    type(sign_mode_command_type), public, parameter :: sign_mode = sign_mode_command_type()
        !! 符号モードを変更する手続

    !>符号モードをSUPPRESSに変更した編集項目並びを返す
    interface sign_mode_suppress
        procedure :: change_sign_mode_to_suppress_desc
        procedure :: change_sign_mode_to_suppress_item
        procedure :: change_sign_mode_to_suppress_items
    end interface

    !>符号モードをPLUSに変更した編集項目並びを返す
    interface sign_mode_plus
        procedure :: change_sign_mode_to_plus_desc
        procedure :: change_sign_mode_to_plus_item
        procedure :: change_sign_mode_to_plus_items
    end interface

    !>符号モードをPROCESSOR_DEFINEDに変更した編集項目並びを返す
    interface sign_mode_processor_defined
        procedure :: change_sign_mode_to_processor_defined_desc
        procedure :: change_sign_mode_to_processor_defined_item
        procedure :: change_sign_mode_to_processor_defined_items
    end interface

contains
    !>引数として渡された編集記述子に，符号モードをSUPPRESSに
    !>変更するように`SS`形編集記述子を追加した編集項目並びを返す．
    function change_sign_mode_to_suppress_desc(edit_descriptor) result(new_format_items)
        implicit none
        class(edit_descriptor_type), intent(in) :: edit_descriptor
            !! 符号モードがSUPPRESSに変更される編集記述子
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(sign_suppress_edit_descriptor_symbol) &
                           //edit_descriptor &
                           //ctrl(get_sign_default())
    end function change_sign_mode_to_suppress_desc

    !>引数として渡された編集項目に，符号モードをSUPPRESSに
    !>変更するように`SS`形編集記述子を追加した編集項目並びを返す．
    function change_sign_mode_to_suppress_item(format_item) result(new_format_items)
        implicit none
        type(format_item_type), intent(in) :: format_item
            !! 符号モードがSUPPRESSに変更される編集項目
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(sign_suppress_edit_descriptor_symbol) &
                           //format_item &
                           //ctrl(get_sign_default())
    end function change_sign_mode_to_suppress_item

    !>引数として渡された編集項目並びに，符号モードをSUPPRESSに
    !>変更するように`SS`形編集記述子を追加した編集項目並びを返す．
    function change_sign_mode_to_suppress_items(format_items) result(new_format_items)
        implicit none
        type(format_items_type), intent(in) :: format_items
            !! 符号モードがSUPPRESSに変更される編集項目並び
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(sign_suppress_edit_descriptor_symbol) &
                           //format_items &
                           //ctrl(get_sign_default())
    end function change_sign_mode_to_suppress_items

    !>引数として渡された編集記述子に，符号モードをPLUSに
    !>変更するように`SP`形編集記述子を追加した編集項目並びを返す．
    function change_sign_mode_to_plus_desc(edit_descriptor) result(new_format_items)
        implicit none
        class(edit_descriptor_type), intent(in) :: edit_descriptor
            !! 符号モードがPLUSに変更される編集記述子
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(sign_plus_edit_descriptor_symbol) &
                           //edit_descriptor &
                           //ctrl(get_sign_default())
    end function change_sign_mode_to_plus_desc

    !>引数として渡された編集項目に，符号モードをPLUSに
    !>変更するように`SP`形編集記述子を追加した編集項目並びを返す．
    function change_sign_mode_to_plus_item(format_item) result(new_format_items)
        implicit none
        type(format_item_type), intent(in) :: format_item
            !! 符号モードがPLUSに変更される編集項目
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(sign_plus_edit_descriptor_symbol) &
                           //format_item &
                           //ctrl(get_sign_default())
    end function change_sign_mode_to_plus_item

    !>引数として渡された編集項目並びに，符号モードをPLUSに
    !>変更するように`SP`形編集記述子を追加した編集項目並びを返す．
    function change_sign_mode_to_plus_items(format_items) result(new_format_items)
        implicit none
        type(format_items_type), intent(in) :: format_items
            !! 符号モードがPLUSに変更される編集項目並び
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(sign_plus_edit_descriptor_symbol) &
                           //format_items &
                           //ctrl(get_sign_default())
    end function change_sign_mode_to_plus_items

    !>引数として渡された編集記述子に，符号モードをPROCESSOR_DEFINEDに
    !>変更するように`S`形編集記述子を追加した編集項目並びを返す．
    function change_sign_mode_to_processor_defined_desc(edit_descriptor) result(new_format_items)
        implicit none
        class(edit_descriptor_type), intent(in) :: edit_descriptor
            !! 符号モードがPROCESSOR_DEFINEDに変更される編集記述子
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(sign_processor_defined_edit_descriptor_symbol) &
                           //edit_descriptor &
                           //ctrl(get_sign_default())
    end function change_sign_mode_to_processor_defined_desc

    !>引数として渡された編集項目に，符号モードをPROCESSOR_DEFINEDに
    !>変更するように`S`形編集記述子を追加した編集項目並びを返す．
    function change_sign_mode_to_processor_defined_item(format_item) result(new_format_items)
        implicit none
        type(format_item_type), intent(in) :: format_item
            !! 符号モードがPROCESSOR_DEFINEDに変更される編集項目
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(sign_processor_defined_edit_descriptor_symbol) &
                           //format_item &
                           //ctrl(get_sign_default())
    end function change_sign_mode_to_processor_defined_item

    !>引数として渡された編集項目並びに，符号モードをPROCESSOR_DEFINEDに
    !>変更するように`S`形編集記述子を追加した編集項目並びを返す．
    function change_sign_mode_to_processor_defined_items(format_items) result(new_format_items)
        implicit none
        type(format_items_type), intent(in) :: format_items
            !! 符号モードがPROCESSOR_DEFINEDに変更される編集項目並び
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(sign_processor_defined_edit_descriptor_symbol) &
                           //format_items &
                           //ctrl(get_sign_default())
    end function change_sign_mode_to_processor_defined_items

    !>標準の符号モードを表す編集記述子の英字定数を返す．
    function get_sign_default() result(default)
        implicit none
        character(1) :: default
            !! 標準の符号モードを表す編集記述子の英字定数

        default = sign_processor_defined_edit_descriptor_symbol
    end function get_sign_default
end module fed_editDescriptor_control_sign
