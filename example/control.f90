program ex_control
    use :: fed
    implicit none

    block
        logical :: l(2, 3)
        l = .false.

        print format(repeat(logical(), separator=",")), l
        !F,F,F,F,F,F,
        print format(repeat(logical()//terminate(), separator=",")), l
        !F,F,F,F,F,F
        print format("["//end_line()//repeat(logical(2), size(l))//end_line()//"]"), l
        ![
        ! F F F F F F
        !]
    end block

    print format(str("123456789012345"))
    print format("|"//move_to(8)//"^")
    !12345678901234567890
    !|      ^
    print format(str("123456789012345"))
    print format(move_to(8)//move(-3)//"*"//move(5)//"$")
    !12345678901234567890
    !    *     $
    print format(str("123456789012345"))
    print format(move_to(8)//"*"//move(0)//"^")
    !12345678901234567890
    !       *^

    print format(real()//decimal_mode%comma(real())//real(), separator=" "), 0.123, 0.123, -1.23
    !0.123000003 0,123000003 -1.23000002
    print format(complex() &
                 //decimal_mode%comma(complex()) &
                 //complex(), separator=" "), &
        cmplx(0.123, -1.23), cmplx(0.123, -1.23), cmplx(0.123, -1.23)
    !(0.123000003,-1.23000002) (0,123000003,-1,23000002) (0.123000003,-1.23000002)

    print format(rounding_mode%down(real(2, 0)) &
                 //rounding_mode%up(real(2, 0)) &
                 //rounding_mode%zero(real(2, 0)) &
                 //rounding_mode%nearest(real(2, 0)), separator=" "), 1.5, 1.5, 1.5, 1.5
    ! 1. 2. 1. 2.
    print format(rounding_mode%down(real(2, 0)) &
                 //rounding_mode%up(real(2, 0)) &
                 //rounding_mode%zero(real(2, 0)) &
                 //rounding_mode%nearest(real(2, 0)), separator=" "), 0.5, 0.5, 0.5, 0.5
    ! 0. 1. 0. 0.
    print format(rounding_mode%down(real(3, 0)) &
                 //rounding_mode%up(real(3, 0)) &
                 //rounding_mode%zero(real(3, 0)) &
                 //rounding_mode%nearest(real(3, 0)), separator=" "), -0.5, -0.5, -0.5, -0.5
    ! -1. -0. -0. -0.
    print format(rounding_mode%down(real(3, 0)) &
                 //rounding_mode%up(real(3, 0)) &
                 //rounding_mode%zero(real(3, 0)) &
                 //rounding_mode%nearest(real(3, 0)), separator=" "), -1.5, -1.5, -1.5, -1.5
    ! -2. -1. -1. -2.

    print format(sign_mode%plus(int())//int(), separator=" "), 10, 10
    ! +10 10

    block
        integer :: i
        character(:), allocatable :: input

        input = ' 1 23 '
        read (input, format(blank_mode%zero(int(6)))) i
        print format(int()), i
        !10230

        read (input, format(blank_mode%null(int(6)))) i
        print format(int()), i
        !123
    end block
end program ex_control
