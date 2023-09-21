module fed_editDescriptor_control_round
    use, intrinsic :: iso_c_binding
    use :: fed_editDescriptor
    use :: fed_editDescriptor_control
    use :: fed_format_item
    use :: fed_format_items
    use :: enumul
    implicit none
    private
    public :: rounding_mode_up
    public :: rounding_mode_down
    public :: rounding_mode_zero
    public :: rounding_mode_nearest
    public :: rounding_mode_compatible
    public :: rounding_mode_processor_defined
    public :: get_round_default ! open to public only for unit tests

    character(*), private, parameter :: round_up_edit_descriptor_symbol = "RU"
        !! RU形編集に用いられる英字定数
    character(*), private, parameter :: round_down_edit_descriptor_symbol = "RD"
        !! RD形編集に用いられる英字定数
    character(*), private, parameter :: round_zero_edit_descriptor_symbol = "RZ"
        !! RZ形編集に用いられる英字定数
    character(*), private, parameter :: round_nearest_edit_descriptor_symbol = "RN"
        !! RN形編集に用いられる英字定数
    character(*), private, parameter :: round_compatible_edit_descriptor_symbol = "RC"
        !! RC形編集に用いられる英字定数
    character(*), private, parameter :: round_processor_defined_edit_descriptor_symbol = "RP"
        !! RP形編集に用いられる英字定数

    !>入出力丸めモードを変更する手続を取り扱う派生型．
    !>
    !>@note 一つのparameterを宣言してそれを利用する事を想定している．
    type, public :: rounding_mode_command_type
    contains
        procedure, private, nopass :: change_rounding_mode_to_up_desc
        procedure, private, nopass :: change_rounding_mode_to_up_item
        procedure, private, nopass :: change_rounding_mode_to_up_items
        generic, public :: up => &
            change_rounding_mode_to_up_desc, &
            change_rounding_mode_to_up_item, &
            change_rounding_mode_to_up_items
        !* 入出力丸めモードをUPに変更した編集項目並びを返却
        procedure, private, nopass :: change_rounding_mode_to_down_desc
        procedure, private, nopass :: change_rounding_mode_to_down_item
        procedure, private, nopass :: change_rounding_mode_to_down_items
        generic, public :: down => &
            change_rounding_mode_to_down_desc, &
            change_rounding_mode_to_down_item, &
            change_rounding_mode_to_down_items
        !* 入出力丸めモードをDOWNに変更した編集項目並びを返却
        procedure, private, nopass :: change_rounding_mode_to_zero_desc
        procedure, private, nopass :: change_rounding_mode_to_zero_item
        procedure, private, nopass :: change_rounding_mode_to_zero_items
        generic, public :: zero => &
            change_rounding_mode_to_zero_desc, &
            change_rounding_mode_to_zero_item, &
            change_rounding_mode_to_zero_items
        !* 入出力丸めモードをZEROに変更した編集項目並びを返却
        procedure, private, nopass :: change_rounding_mode_to_nearest_desc
        procedure, private, nopass :: change_rounding_mode_to_nearest_item
        procedure, private, nopass :: change_rounding_mode_to_nearest_items
        generic, public :: nearest => &
            change_rounding_mode_to_nearest_desc, &
            change_rounding_mode_to_nearest_item, &
            change_rounding_mode_to_nearest_items
        !* 入出力丸めモードをNEARESTに変更した編集項目並びを返却
        procedure, private, nopass :: change_rounding_mode_to_compatible_desc
        procedure, private, nopass :: change_rounding_mode_to_compatible_item
        procedure, private, nopass :: change_rounding_mode_to_compatible_items
        generic, public :: compatible => &
            change_rounding_mode_to_compatible_desc, &
            change_rounding_mode_to_compatible_item, &
            change_rounding_mode_to_compatible_items
        !* 入出力丸めモードをCOMPATIBLEに変更した編集項目並びを返却
        procedure, private, nopass :: change_rounding_mode_to_processor_defined_desc
        procedure, private, nopass :: change_rounding_mode_to_processor_defined_item
        procedure, private, nopass :: change_rounding_mode_to_processor_defined_items
        generic, public :: processor_defined => &
            change_rounding_mode_to_processor_defined_desc, &
            change_rounding_mode_to_processor_defined_item, &
            change_rounding_mode_to_processor_defined_items
        !* 入出力丸めモードをPROCESSOR_DEFINEDに変更した編集項目並びを返却
    end type rounding_mode_command_type

    type(rounding_mode_command_type), public, parameter :: rounding_mode = rounding_mode_command_type()
        !! 入出力丸めモードを変更する手続

    !>入出力丸めモードをUPに変更した編集項目並びを返す
    interface rounding_mode_up
        procedure :: change_rounding_mode_to_up_desc
        procedure :: change_rounding_mode_to_up_item
        procedure :: change_rounding_mode_to_up_items
    end interface

    !>入出力丸めモードをDOWNに変更した編集項目並びを返す
    interface rounding_mode_down
        procedure :: change_rounding_mode_to_down_desc
        procedure :: change_rounding_mode_to_down_item
        procedure :: change_rounding_mode_to_down_items
    end interface

    !>入出力丸めモードをZEROに変更した編集項目並びを返す
    interface rounding_mode_zero
        procedure :: change_rounding_mode_to_zero_desc
        procedure :: change_rounding_mode_to_zero_item
        procedure :: change_rounding_mode_to_zero_items
    end interface

    !>入出力丸めモードをNEARESTに変更した編集項目並びを返す
    interface rounding_mode_nearest
        procedure :: change_rounding_mode_to_nearest_desc
        procedure :: change_rounding_mode_to_nearest_item
        procedure :: change_rounding_mode_to_nearest_items
    end interface

    !>入出力丸めモードをCOMAPTIBLEに変更した編集項目並びを返す
    interface rounding_mode_compatible
        procedure :: change_rounding_mode_to_compatible_desc
        procedure :: change_rounding_mode_to_compatible_item
        procedure :: change_rounding_mode_to_compatible_items
    end interface

    !>入出力丸めモードをPROCESSOR_DEFINEDに変更した編集項目並びを返す
    interface rounding_mode_processor_defined
        procedure :: change_rounding_mode_to_processor_defined_desc
        procedure :: change_rounding_mode_to_processor_defined_item
        procedure :: change_rounding_mode_to_processor_defined_items
    end interface

contains
    !>引数として渡された編集記述子に，入出力丸めモードをUPに
    !>変更するように`RU`形編集記述子を追加した編集項目並びを返す．
    function change_rounding_mode_to_up_desc(edit_descriptor) result(new_format_items)
        implicit none
        class(edit_descriptor_type), intent(in) :: edit_descriptor
            !! 入出力丸めモードがUPに変更される編集記述子
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(round_up_edit_descriptor_symbol) &
                           //edit_descriptor &
                           //ctrl(get_round_default())
    end function change_rounding_mode_to_up_desc

    !>引数として渡された編集項目に，入出力丸めモードをUPに
    !>変更するように`RU`形編集記述子を追加した編集項目並びを返す．
    function change_rounding_mode_to_up_item(format_item) result(new_format_items)
        implicit none
        type(format_item_type), intent(in) :: format_item
            !! 入出力丸めモードがUPに変更される編集項目
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(round_up_edit_descriptor_symbol) &
                           //format_item &
                           //ctrl(get_round_default())
    end function change_rounding_mode_to_up_item

    !>引数として渡された編集項目並びに，入出力丸めモードをUPに
    !>変更するように`RU`形編集記述子を追加した編集項目並びを返す．
    function change_rounding_mode_to_up_items(format_items) result(new_format_items)
        implicit none
        type(format_items_type), intent(in) :: format_items
            !! 入出力丸めモードがUPに変更される編集項目並び
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(round_up_edit_descriptor_symbol) &
                           //format_items &
                           //ctrl(get_round_default())
    end function change_rounding_mode_to_up_items

    !>引数として渡された編集記述子に，入出力丸めモードをDOWNに
    !>変更するように`RD`形編集記述子を追加した編集項目並びを返す．
    function change_rounding_mode_to_down_desc(edit_descriptor) result(new_format_items)
        implicit none
        class(edit_descriptor_type), intent(in) :: edit_descriptor
            !! 入出力丸めモードがDOWNに変更される編集記述子
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(round_down_edit_descriptor_symbol) &
                           //edit_descriptor &
                           //ctrl(get_round_default())
    end function change_rounding_mode_to_down_desc

    !>引数として渡された編集項目に，入出力丸めモードをDOWNに
    !>変更するように`RD`形編集記述子を追加した編集項目並びを返す．
    function change_rounding_mode_to_down_item(format_item) result(new_format_items)
        implicit none
        type(format_item_type), intent(in) :: format_item
            !! 入出力丸めモードがDOWNに変更される編集項目
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(round_down_edit_descriptor_symbol) &
                           //format_item &
                           //ctrl(get_round_default())
    end function change_rounding_mode_to_down_item

    !>引数として渡された編集項目並びに，入出力丸めモードをDOWNに
    !>変更するように`RD`形編集記述子を追加した編集項目並びを返す．
    function change_rounding_mode_to_down_items(format_items) result(new_format_items)
        implicit none
        type(format_items_type), intent(in) :: format_items
            !! 入出力丸めモードがDOWNに変更される編集項目並び
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(round_down_edit_descriptor_symbol) &
                           //format_items &
                           //ctrl(get_round_default())
    end function change_rounding_mode_to_down_items

    !>引数として渡された編集記述子に，入出力丸めモードをZEROに
    !>変更するように`RZ`形編集記述子を追加した編集項目並びを返す．
    function change_rounding_mode_to_zero_desc(edit_descriptor) result(new_format_items)
        implicit none
        class(edit_descriptor_type), intent(in) :: edit_descriptor
            !! 入出力丸めモードがZEROに変更される編集記述子
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(round_zero_edit_descriptor_symbol) &
                           //edit_descriptor &
                           //ctrl(get_round_default())
    end function change_rounding_mode_to_zero_desc

    !>引数として渡された編集項目に，入出力丸めモードをZEROに
    !>変更するように`RZ`形編集記述子を追加した編集項目並びを返す．
    function change_rounding_mode_to_zero_item(format_item) result(new_format_items)
        implicit none
        type(format_item_type), intent(in) :: format_item
            !! 入出力丸めモードがZEROに変更される編集項目
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(round_zero_edit_descriptor_symbol) &
                           //format_item &
                           //ctrl(get_round_default())
    end function change_rounding_mode_to_zero_item

    !>引数として渡された編集項目並びに，入出力丸めモードをZEROに
    !>変更するように`RZ`形編集記述子を追加した編集項目並びを返す．
    function change_rounding_mode_to_zero_items(format_items) result(new_format_items)
        implicit none
        type(format_items_type), intent(in) :: format_items
            !! 入出力丸めモードがZEROに変更される編集項目並び
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(round_zero_edit_descriptor_symbol) &
                           //format_items &
                           //ctrl(get_round_default())
    end function change_rounding_mode_to_zero_items

    !>引数として渡された編集記述子に，入出力丸めモードをNEARESTに
    !>変更するように`RN`形編集記述子を追加した編集項目並びを返す．
    function change_rounding_mode_to_nearest_desc(edit_descriptor) result(new_format_items)
        implicit none
        class(edit_descriptor_type), intent(in) :: edit_descriptor
            !! 入出力丸めモードがNEARESTに変更される編集記述子
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(round_nearest_edit_descriptor_symbol) &
                           //edit_descriptor &
                           //ctrl(get_round_default())
    end function change_rounding_mode_to_nearest_desc

    !>引数として渡された編集項目に，入出力丸めモードをNEARESTに
    !>変更するように`RN`形編集記述子を追加した編集項目並びを返す．
    function change_rounding_mode_to_nearest_item(format_item) result(new_format_items)
        implicit none
        type(format_item_type), intent(in) :: format_item
            !! 入出力丸めモードがNEARESTに変更される編集項目
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(round_nearest_edit_descriptor_symbol) &
                           //format_item &
                           //ctrl(get_round_default())
    end function change_rounding_mode_to_nearest_item

    !>引数として渡された編集項目並びに，入出力丸めモードをNEARESTに
    !>変更するように`RN`形編集記述子を追加した編集項目並びを返す．
    function change_rounding_mode_to_nearest_items(format_items) result(new_format_items)
        implicit none
        type(format_items_type), intent(in) :: format_items
            !! 入出力丸めモードがNEARESTに変更される編集項目並び
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(round_nearest_edit_descriptor_symbol) &
                           //format_items &
                           //ctrl(get_round_default())
    end function change_rounding_mode_to_nearest_items

    !>引数として渡された編集記述子に，入出力丸めモードをCOMPATIBLEに
    !>変更するように`RC`形編集記述子を追加した編集項目並びを返す．
    function change_rounding_mode_to_compatible_desc(edit_descriptor) result(new_format_items)
        implicit none
        class(edit_descriptor_type), intent(in) :: edit_descriptor
            !! 入出力丸めモードがCOMPATIBLEに変更される編集記述子
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(round_compatible_edit_descriptor_symbol) &
                           //edit_descriptor &
                           //ctrl(get_round_default())
    end function change_rounding_mode_to_compatible_desc

    !>引数として渡された編集項目に，入出力丸めモードをCOMPATIBLEに
    !>変更するように`RC`形編集記述子を追加した編集項目並びを返す．
    function change_rounding_mode_to_compatible_item(format_item) result(new_format_items)
        implicit none
        type(format_item_type), intent(in) :: format_item
            !! 入出力丸めモードがCOMPATIBLEに変更される編集項目
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(round_compatible_edit_descriptor_symbol) &
                           //format_item &
                           //ctrl(get_round_default())
    end function change_rounding_mode_to_compatible_item

    !>引数として渡された編集項目並びに，入出力丸めモードをCOMPATIBLEに
    !>変更するように`RC`形編集記述子を追加した編集項目並びを返す．
    function change_rounding_mode_to_compatible_items(format_items) result(new_format_items)
        implicit none
        type(format_items_type), intent(in) :: format_items
            !! 入出力丸めモードがCOMPATIBLEに変更される編集項目並び
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(round_compatible_edit_descriptor_symbol) &
                           //format_items &
                           //ctrl(get_round_default())
    end function change_rounding_mode_to_compatible_items

    !>引数として渡された編集記述子に，入出力丸めモードをPROCESSOR_DEFINEDに
    !>変更するように`RP`形編集記述子を追加した編集項目並びを返す．
    function change_rounding_mode_to_processor_defined_desc(edit_descriptor) result(new_format_items)
        implicit none
        class(edit_descriptor_type), intent(in) :: edit_descriptor
            !! 入出力丸めモードがPROCESSOR_DEFINEDに変更される編集記述子
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(round_processor_defined_edit_descriptor_symbol) &
                           //edit_descriptor &
                           //ctrl(get_round_default())
    end function change_rounding_mode_to_processor_defined_desc

    !>引数として渡された編集項目に，入出力丸めモードをPROCESSOR_DEFINEDに
    !>変更するように`RP`形編集記述子を追加した編集項目並びを返す．
    function change_rounding_mode_to_processor_defined_item(format_item) result(new_format_items)
        implicit none
        type(format_item_type), intent(in) :: format_item
            !! 入出力丸めモードがPROCESSOR_DEFINEDに変更される編集項目
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(round_processor_defined_edit_descriptor_symbol) &
                           //format_item &
                           //ctrl(get_round_default())
    end function change_rounding_mode_to_processor_defined_item

    !>引数として渡された編集項目並びに，入出力丸めモードをPROCESSOR_DEFINEDに
    !>変更するように`RP`形編集記述子を追加した編集項目並びを返す．
    function change_rounding_mode_to_processor_defined_items(format_items) result(new_format_items)
        implicit none
        type(format_items_type), intent(in) :: format_items
            !! 入出力丸めモードがPROCESSOR_DEFINEDに変更される編集項目並び
        type(format_items_type) :: new_format_items
            !! 生成される編集項目並び

        new_format_items = ctrl(round_processor_defined_edit_descriptor_symbol) &
                           //format_items &
                           //ctrl(get_round_default())
    end function change_rounding_mode_to_processor_defined_items

    !>標準の入出力丸めモードを表す丸め編集記述子の英字定数を返す．
    function get_round_default() result(default)
        use, intrinsic :: iso_fortran_env
        use :: enumul_open_status
        use :: enumul_open_round
        implicit none
        character(2) :: default
            !! 標準の入出力丸めモードを表す丸め編集記述子の英字定数

        character(32) :: round_expr
        integer(int32) :: unit

        ! 装置をround指定なしで開き，
        ! inquireでroundの値を取得することにより
        ! 標準値を確認する．
        open (newunit=unit, status=open_status%scratch%expr)
        inquire (unit, round=round_expr)
        close (unit)

        select case (round_expr)
        case (open_round%up%expr)
            default = round_up_edit_descriptor_symbol

        case (open_round%down%expr)
            default = round_down_edit_descriptor_symbol

        case (open_round%zero%expr)
            default = round_zero_edit_descriptor_symbol

        case (open_round%nearest%expr)
            default = round_nearest_edit_descriptor_symbol

        case (open_round%compatible%expr)
            default = round_compatible_edit_descriptor_symbol

        case (open_round%processor_defined%expr)
            default = round_processor_defined_edit_descriptor_symbol

        case default
            default = round_processor_defined_edit_descriptor_symbol

        end select
    end function get_round_default
end module fed_editDescriptor_control_round
