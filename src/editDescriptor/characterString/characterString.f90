module fed_editDescriptor_characterString
    use :: fed_editDescriptor
    use :: strings_enclose
    implicit none
    private
    public :: str

    !>文字列編集記述子を取り扱う派生型．
    type, public, extends(edit_descriptor_type) :: character_string_edit_descriptor_type
    end type character_string_edit_descriptor_type

    interface str
        procedure :: construct_char_str_edit_descriptor
    end interface

contains
    !>character_string_edit_descriptor_typeインスタンスを生成して返す．
    pure function construct_char_str_edit_descriptor(character_string) result(new_char_str_desc)
        implicit none
        character(*), intent(in) :: character_string
            !! 文字列
        type(character_string_edit_descriptor_type) :: new_char_str_desc
            !! 生成されるcharacter_string_edit_descriptor_typeインスタンス

        call new_char_str_desc%set(enclose(character_string, '"'))
    end function construct_char_str_edit_descriptor
end module fed_editDescriptor_characterString
