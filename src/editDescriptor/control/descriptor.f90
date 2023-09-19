module fed_editDescriptor_control
    use :: fed_editDescriptor
    implicit none
    private
    public :: ctrl

    !>制御編集記述子を取り扱う派生型．
    type, public, extends(edit_descriptor_type) :: control_edit_descriptor_type
    end type control_edit_descriptor_type

    !>ユーザ定義コンストラクタをビルトインコンストラクタと同じ名前
    !>control_edit_descriptor_typeで呼べるようにするための総称名．
    interface control_edit_descriptor_type
        procedure :: construct_control_edit_descriptor
    end interface

    !>ユーザ定義コンストラクタをビルトインコンストラクタと同じ名前
    !>ctrlで呼べるようにするための総称名．
    !>[[character_string_edit_descriptor_type]]がコンストラクタ
    !>strを持つことに合わせるために定義されている．
    interface ctrl
        procedure :: construct_control_edit_descriptor
    end interface

contains
    !>文字列として渡された編集記述子を用いて，
    !>control_edit_descriptor_typeインスタンスを生成して返す．
    pure function construct_control_edit_descriptor(descriptor) result(new_edit_descriptor)
        implicit none
        character(*), intent(in) :: descriptor
            !! 編集記述子
        type(control_edit_descriptor_type) :: new_edit_descriptor
            !! 生成されるcontrol_edit_descriptor_typeインスタンス

        call new_edit_descriptor%set(descriptor)
    end function construct_control_edit_descriptor
end module fed_editDescriptor_control
