module fed_editDescriptor_data
    use :: fed_editDescriptor
    implicit none
    private

    character(*), public, parameter :: general_data_edit_descriptor_symbol = "G"
        !! G形データ編集記述子（一般形編集）に用いられる英字定数

    !>データ編集記述子を取り扱う派生型．
    !>この型は，具体的な編集
    !>（数値編集，論理編集，文字型編集）
    !>を定義するための祖先型として定義されている．
    type, public, extends(edit_descriptor_type) :: data_edit_descriptor_type
    end type data_edit_descriptor_type
end module fed_editDescriptor_data
