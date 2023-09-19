program ex_control
    use :: fed
    implicit none

    logical :: l(2, 3)

    print format(repeat(logical(), separator=",")), l
    !F,F,T,T,F,F,
    print format(repeat(logical()//terminate(), separator=",")), l
    !F,F,T,T,F,F
    print format("["//end_line()//repeat(logical(2), size(l))//end_line()//"]"), l
    ![
    ! F F T T F F
    !]

    print format(str("123456789012345"))
    print format("|"//move_to(8)//"^")
    !12345678901234567890
    !|      ^
    print format(str("123456789012345"))
    print format(move_to(8)//move(-3)//"*"//move(5)//"$")
    !12345678901234567890
    !    *     $
end program ex_control
