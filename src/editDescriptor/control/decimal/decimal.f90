module fed_editDescriptor_control_decimal
    use, intrinsic :: iso_c_binding
    use :: fed_editDescriptor
    use :: fed_editDescriptor_control
    use :: fed_format_item
    use :: fed_format_items
    implicit none
    private
    public :: decimal_mode_comma
    public :: decimal_mode_point
    public :: get_decimal_default ! open to public only for unit tests

    character(*), private, parameter :: decimal_point_edit_descriptor_symbol = "DP"
        !! DP形編集に用いられる英字定数
    character(*), private, parameter :: decimal_comma_edit_descriptor_symbol = "DC"
        !! DC形編集に用いられる英字定数

    !>小数点編集記述子を変更する手続を取り扱う派生型．
    !>
    !>@note 一つのparameterを宣言してそれを利用する事を想定している．
    type, public :: decimal_mode_command_type
    contains
        procedure, private, nopass :: change_decimal_edit_mode_to_comma_desc
        procedure, private, nopass :: change_decimal_edit_mode_to_comma_item
        procedure, private, nopass :: change_decimal_edit_mode_to_comma_items
        generic, public :: comma => &
            change_decimal_edit_mode_to_comma_desc, &
            change_decimal_edit_mode_to_comma_item, &
            change_decimal_edit_mode_to_comma_items
        !* 小数点をカンマに変更した編集項目並びを返却
        procedure, private, nopass :: change_decimal_edit_mode_to_point_desc
        procedure, private, nopass :: change_decimal_edit_mode_to_point_item
        procedure, private, nopass :: change_decimal_edit_mode_to_point_items
        generic, public :: point => &
            change_decimal_edit_mode_to_point_desc, &
            change_decimal_edit_mode_to_point_item, &
            change_decimal_edit_mode_to_point_items
        !* 小数点をピリオドに変更した編集項目並びを返却
    end type decimal_mode_command_type

    type(decimal_mode_command_type), public, parameter :: decimal_mode = decimal_mode_command_type()
        !! 小数点編集記述子を変更する手続

    !>小数点をカンマに変更した編集項目並びを返す
    interface decimal_mode_comma
        procedure :: change_decimal_edit_mode_to_comma_desc
        procedure :: change_decimal_edit_mode_to_comma_item
        procedure :: change_decimal_edit_mode_to_comma_items
    end interface

    !>小数点をピリオドに変更した編集項目並びを返す
    interface decimal_mode_point
        procedure :: change_decimal_edit_mode_to_point_desc
        procedure :: change_decimal_edit_mode_to_point_item
        procedure :: change_decimal_edit_mode_to_point_items
    end interface

contains
    !>引数として渡された編集記述子の小数点をカンマに変更するように
    !>小数点編集記述子を追加した編集項目並びを返す．
    function change_decimal_edit_mode_to_comma_desc(edit_descriptor) result(new_format_items)
        implicit none
        class(edit_descriptor_type), intent(in) :: edit_descriptor
            !! 小数点を変更する編集記述子
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(decimal_comma_edit_descriptor_symbol) &
                           //edit_descriptor &
                           //ctrl(get_decimal_default())
    end function change_decimal_edit_mode_to_comma_desc

    !>引数として渡された編集項目の小数点をカンマに変更するように
    !>小数点編集記述子を追加した編集項目並びを返す．
    function change_decimal_edit_mode_to_comma_item(format_item) result(new_format_items)
        implicit none
        type(format_item_type), intent(in) :: format_item
            !! 小数点を変更する編集項目
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(decimal_comma_edit_descriptor_symbol) &
                           //format_item &
                           //ctrl(get_decimal_default())
    end function change_decimal_edit_mode_to_comma_item

    !>引数として渡された編集項目並びの小数点をカンマに変更するように
    !>小数点編集記述子を追加した編集項目並びを返す．
    function change_decimal_edit_mode_to_comma_items(format_items) result(new_format_items)
        implicit none
        type(format_items_type), intent(in) :: format_items
            !! 小数点を変更する編集項目並び
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(decimal_comma_edit_descriptor_symbol) &
                           //format_items &
                           //ctrl(get_decimal_default())
    end function change_decimal_edit_mode_to_comma_items

    !>引数として渡された編集記述子の小数点をカンマに変更するように
    !>小数点編集記述子を追加した編集項目並びを返す．
    function change_decimal_edit_mode_to_point_desc(edit_descriptor) result(new_format_items)
        implicit none
        class(edit_descriptor_type), intent(in) :: edit_descriptor
            !! 小数点を変更する編集記述子
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(decimal_point_edit_descriptor_symbol) &
                           //edit_descriptor &
                           //ctrl(get_decimal_default())
    end function change_decimal_edit_mode_to_point_desc

    !>引数として渡された編集項目の小数点をカンマに変更するように
    !>小数点編集記述子を追加した編集項目並びを返す．
    function change_decimal_edit_mode_to_point_item(format_item) result(new_format_items)
        implicit none
        type(format_item_type), intent(in) :: format_item
            !! 小数点を変更する編集項目
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(decimal_point_edit_descriptor_symbol) &
                           //format_item &
                           //ctrl(get_decimal_default())
    end function change_decimal_edit_mode_to_point_item

    !>引数として渡された編集項目並びの小数点をカンマに変更するように
    !>小数点編集記述子を追加した編集項目並びを返す．
    function change_decimal_edit_mode_to_point_items(format_items) result(new_format_items)
        implicit none
        type(format_items_type), intent(in) :: format_items
            !! 小数点を変更する編集項目並び
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(decimal_point_edit_descriptor_symbol) &
                           //format_items &
                           //ctrl(get_decimal_default())
    end function change_decimal_edit_mode_to_point_items

    !>標準の小数点モードを表す小数点編集記述子の英字定数を返す．
    function get_decimal_default() result(default)
        use, intrinsic :: iso_fortran_env
        use :: enumul_open_status
        use :: enumul_open_decimal
        implicit none
        character(2) :: default
            !! 標準の小数点モードを表す小数点編集記述子の英字定数

        character(15) :: decimal_expr
        integer(int32) :: unit

        ! 装置をdecimal指定なしで開き，
        ! inquireでdecimalの値を取得することにより
        ! 標準値を確認する．
        open (newunit=unit, status=open_status%scratch%expr)
        inquire (unit, decimal=decimal_expr)
        close (unit)

        select case (decimal_expr)
        case (open_decimal%comma%expr)
            default = decimal_comma_edit_descriptor_symbol

        case (open_decimal%point%expr)
            default = decimal_point_edit_descriptor_symbol

        case default
            default = decimal_point_edit_descriptor_symbol
        end select
    end function get_decimal_default
end module fed_editDescriptor_control_decimal
