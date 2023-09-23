module fed_editDescriptor_control_position
    use, intrinsic :: iso_fortran_env
    use :: fed_editDescriptor_control
    use :: stdlib_strings
    implicit none
    private
    public :: move
    public :: move_to

    character(*), private, parameter :: absolute_position_edit_descriptor_symbol = "T"
        !! T形編集に用いられる英字定数
    character(*), private, parameter :: backward_tab_edit_descriptor_symbol = "TL"
        !! TL形編集に用いられる英字定数
    character(*), private, parameter :: forward_tab_edit_descriptor_symbol = "TR"
        !! TR形編集に用いられる英字定数

    !>位置付け編集記述子を取り扱う派生型．
    !>位置付け編集記述子は，記録へ，又は記録から転送される
    !>次の文字の位置を指定する．
    type, public, extends(control_edit_descriptor_type) :: position_edit_descriptor_type
    end type position_edit_descriptor_type

    !>現在の文字位置から移動する．
    interface move
        procedure :: construct_position_edit_descriptor_rel
    end interface

    !>位置付け左限界を1としてcolumn番目に移動する．
    interface move_to
        procedure :: construct_position_edit_descriptor_abs
    end interface

contains
    !>position_edit_descriptor_typeインスタンスを生成して返す．
    !>現在位置から`character`文字移動する．
    !>`character`が1以上の場合，`TRn`形編集記述子が設定され，nにはcharacterが用いられる．
    !>-1以下の場合は`TLn`形編集記述子が設定され，nには|character|が設定される．
    !>nが0の場合は，空の制御編集記述子が設定される．
    pure function construct_position_edit_descriptor_rel(characters) result(new_pos_desc)
        implicit none
        integer(int32), intent(in) :: characters
            !! 現在の文字位置からの相対距離
        type(position_edit_descriptor_type) :: new_pos_desc
            !! 生成されるインスタンス

        if (characters == 0) then
            call new_pos_desc%set("")
            return
        end if

        if (characters >= 1) then
            call new_pos_desc%set(forward_tab_edit_descriptor_symbol &
                                  //to_string(characters))
            return
        end if

        if (characters <= -1) then
            call new_pos_desc%set(backward_tab_edit_descriptor_symbol &
                                  //to_string(abs(characters)))
            return
        end if
    end function construct_position_edit_descriptor_rel

    !>position_edit_descriptor_typeインスタンスを生成して返す．
    !>位置付け左限界を1としてcolumn番目に移動する．
    !>`column`が0以下の場合，1が用いられる．
    pure function construct_position_edit_descriptor_abs(column) result(new_pos_desc)
        implicit none
        integer(int32), intent(in) :: column
            !! 位置付け左限界からの位置
        type(position_edit_descriptor_type) :: new_pos_desc
            !! 生成されるインスタンス

        integer(int32) :: n
        n = column
        if (n < 1) n = 1

        call new_pos_desc%set(absolute_position_edit_descriptor_symbol &
                              //to_string(abs(n)))
    end function construct_position_edit_descriptor_abs
end module fed_editDescriptor_control_position
