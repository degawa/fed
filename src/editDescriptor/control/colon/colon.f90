module fed_editDescriptor_control_colon
    use :: fed_editDescriptor_control
    implicit none
    private
    public :: terminate

    character(*), private, parameter :: colon_edit_descriptor_symbol = ":"
        !! コロン編集に用いられる英字定数

    !>コロン編集記述子を取り扱う派生型．
    !>コロン編集は，入出力並びに有効項目が残っていない場合に
    !>書式制御を終了させる．
    type, public, extends(control_edit_descriptor_type) :: colon_edit_descriptor_type
    end type colon_edit_descriptor_type

    !>入出力並びに有効項目が残っていない場合に書式制御を終了させる．
    interface terminate
        procedure :: construct_colon_edit_descriptor
    end interface

contains
    !>colon_edit_descriptor_typeインスタンスを生成して返す．
    !>colon_edit_descriptor_typeが取り扱うコロン編集は，
    !>入出力並びに有効項目が残っていない場合に書式制御を終了させる．
    pure function construct_colon_edit_descriptor() result(new_colon_desc)
        implicit none
        type(colon_edit_descriptor_type) :: new_colon_desc
            !! 生成されるインスタンス

        call new_colon_desc%set(colon_edit_descriptor_symbol)
    end function construct_colon_edit_descriptor
end module fed_editDescriptor_control_colon
