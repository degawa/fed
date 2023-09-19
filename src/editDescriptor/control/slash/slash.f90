module fed_editDescriptor_control_slash
    use :: fed_editDescriptor_control
    implicit none
    private
    public :: end_line

    character(*), private, parameter :: slash_edit_descriptor_symbol = "/"
        !! 斜線編集に用いられる英字定数

    !>斜線編集記述子を取り扱う派生型．
    !>斜線編集は，現在記録への，または現在記録からのデータ転送の
    !>終了を指示する．
    type, public, extends(control_edit_descriptor_type) :: slash_edit_descriptor_type
    end type slash_edit_descriptor_type

    !>現在記録への，または現在記録からのデータ転送の終了を指示する．
    interface end_line
        procedure :: construct_slash_edit_descriptor
    end interface

contains
    !>slash_edit_descriptor_typeインスタンスを生成して返す．
    !>slash_edit_descriptor_typeが取り扱う斜線編集は，
    !>現在記録への，または現在記録からのデータ転送の終了を指示する．
    pure function construct_slash_edit_descriptor() result(new_slash_desc)
        implicit none
        type(slash_edit_descriptor_type) :: new_slash_desc
            !! 生成されるインスタンス

        call new_slash_desc%set(slash_edit_descriptor_symbol)
    end function construct_slash_edit_descriptor
end module fed_editDescriptor_control_slash
