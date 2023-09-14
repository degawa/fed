module fed_editDescriptor_data_char
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor_data
    implicit none
    private
    public :: char

    character(*), private, parameter :: char_edit_descriptor_symbol = "A"
        !! A形編集記述子に用いられる英字定数

    !>文字型編集記述子を取り扱う派生型．
    type, public, extends(data_edit_descriptor_type) :: char_edit_descriptor_type
    end type char_edit_descriptor_type

    interface char
        procedure :: construct_char_edit_descriptor
        procedure :: construct_char_edit_descriptor_w_width
    end interface

contains
    !>char_edit_descriptor_typeインスタンスを生成して返す．
    pure function construct_char_edit_descriptor() result(new_char_desc)
        implicit none
        type(char_edit_descriptor_type) :: new_char_desc
            !! 生成されるchar_edit_descriptor_typeインスタンス

        call new_char_desc%set(char_edit_descriptor_symbol)
    end function construct_char_edit_descriptor

    !>char_edit_descriptor_typeインスタンスを生成して返す．
    !>欄幅には`width`が用いられる．
    pure function construct_char_edit_descriptor_w_width(width) result(new_char_desc)
        use :: stdlib_strings
        implicit none
        integer(int32), intent(in) :: width
            !! 欄幅(>0)
        type(char_edit_descriptor_type) :: new_char_desc
            !! 生成されるchar_edit_descriptor_typeインスタンス

        integer(int32) :: w
        w = width

        ! 欄幅が0以下の場合は欄幅のないA形編集記述子を返す
        if (w <= 0) then
            new_char_desc = char()
            return
        end if

        call new_char_desc%set(char_edit_descriptor_symbol//to_string(w))
    end function construct_char_edit_descriptor_w_width
end module fed_editDescriptor_data_char
