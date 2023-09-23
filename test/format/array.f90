program test_array
    use :: fed_array
    use :: fed_format_item
    use :: fed_editDescriptor_data
    use :: fed_editDescriptor_data_char
    use :: fed_editDescriptor_data_integer_decimal
    use :: fed_editDescriptor_data_logical
    use :: fed_editDescriptor_data_real_standard
    use :: fed_editDescriptor_data_complex_standard
    use :: fassert
    implicit none

    call construct_array_fmt_w_size_returns_fmt_item()
    call construct_array_fmt_w_shape_returns_fmt_item()

contains
    subroutine construct_array_fmt_w_size_returns_fmt_item()
        implicit none
        type(format_item_type) :: itm

        ! test
        itm = array(int(), 2)
        call assert_equal(itm%get_edit_descriptor(), '2(I0,"")', &
                          "array(int(), 2) should return "//'2(I0,"")')
        ! teardown
        call itm%destruct()

        ! test
        itm = array(char(), 3)
        call assert_equal(itm%get_edit_descriptor(), '3(A,"")', &
                          "array(char(), 3) should return "//'3(A,"")')
        ! teardown
        call itm%destruct()

        ! test
        itm = array(logical(), 4)
        call assert_equal(itm%get_edit_descriptor(), '4(L1,"")', &
                          "array(logical(), 4) should return "//'4(L1,"")')
        ! teardown
        call itm%destruct()

        ! test
        itm = array(real(), 5)
        call assert_equal(itm%get_edit_descriptor(), '5(G0,"")', &
                          "array(real(), 5) should return "//'5(G0,"")')
        ! teardown
        call itm%destruct()

        ! test
        itm = array(complex(), 6)
        call assert_equal(itm%get_edit_descriptor(), '6("(",G0,",",G0,")","")', &
                          "array(real(), 6) should return "//'6("(",G0,",",G0,")","")')
        ! teardown
        call itm%destruct()

        ! test
        itm = array(int(), 7, ",")
        call assert_equal(itm%get_edit_descriptor(), '7(I0,","),TL1," ",TL1', &
                          'array(int(), 7, ",") should return '//'7(I0,","),TL1," ",TL1')
        ! teardown
        call itm%destruct()

        ! test
        itm = array(char(), 8, "---")
        call assert_equal(itm%get_edit_descriptor(), '8(A,"---"),TL3,"   ",TL3', &
                          'array(char(), 8, "---") should return '//'8(A,"---"),TL3,"   ",TL3')
        ! teardown
        call itm%destruct()

        ! test
        itm = array(real(), 9, bracket_open="[")
        call assert_equal(itm%get_edit_descriptor(), '"[",9(G0,""),"]"', &
                          'array(real(), 9, bracket_open="[") should return '//'"[",9(G0,""),"]"')
        ! teardown
        call itm%destruct()

        ! test
        itm = array(logical(), 10, bracket_open="(/")
        call assert_equal(itm%get_edit_descriptor(), '"(/",10(L1,""),"/)"', &
                          'array(logical(), 10, bracket_open="[") should return '//'"(/",10(L1,""),"/)')
        ! teardown
        call itm%destruct()

        ! test
        itm = array(int(), 11, ",", "[")
        call assert_equal(itm%get_edit_descriptor(), '"[",11(I0,","),TL1,"]"', &
                          'array(int(), 11, ",", "[") should return '//'"[",11(I0,,","),TL1,"]"')
        ! teardown
        call itm%destruct()

        ! test
        itm = array(complex(), 12, "...", "(/{")
        call assert_equal(itm%get_edit_descriptor(), '"(/{",12("(",G0,",",G0,")","..."),TL3,"}/)  ",TL2', &
                          'array(complex(), 12, "...", "(/{") should return '//'"(/{",12("(",G0,",",G0,")","..."),TL3,"}/)  ",TL2')
        ! teardown
        call itm%destruct()
    end subroutine construct_array_fmt_w_size_returns_fmt_item

    subroutine construct_array_fmt_w_shape_returns_fmt_item()
        implicit none
        type(format_item_type) :: itm

        ! test
        itm = array(int(), [2, 3])
        call assert_equal(itm%get_edit_descriptor(), '3(2(I0,""),"")', &
                          "array(int(), [2, 3]) should return "//'3(2(I0,""),"")')
        ! teardown
        call itm%destruct()

        ! test
        itm = array(char(), [2, 3, 4])
        call assert_equal(itm%get_edit_descriptor(), '4(3(2(A,""),""),"")', &
                          "array(char(), [2, 3, 4]) should return "//'4(3(2(A,""),""),"")')
        ! teardown
        call itm%destruct()

        ! test
        itm = array(logical(), [2, 3, 4, 5])
        call assert_equal(itm%get_edit_descriptor(), '5(4(3(2(L1,""),""),""),"")', &
                          "array(logical(), [2, 3, 4, 5]) should return "//'5(4(3(2(L1,""),""),""),"")')
        ! teardown
        call itm%destruct()

        ! test
        itm = array(real(), [2, 3, 4, 5, 6])
        call assert_equal(itm%get_edit_descriptor(), '6(5(4(3(2(G0,""),""),""),""),"")', &
                          "array(real(), [2, 3, 4, 5, 6]) should return "//'6(5(4(3(2(G0,""),""),""),""),"")')
        ! teardown
        call itm%destruct()

        ! test
        itm = array(complex(), [2, 3, 4, 5, 6, 7])
        call assert_equal(itm%get_edit_descriptor(), '7(6(5(4(3(2("(",G0,",",G0,")",""),""),""),""),""),"")', &
                          "array(complex(), [2, 3, 4, 5, 6, 7]) should return " &
                          //'7(6(5(4(3(2("(",G0,",",G0,")",""),""),""),""),""),"")')
        ! teardown
        call itm%destruct()

        ! test
        itm = array(int(), [2, 3, 4, 5, 6, 7, 8])
        call assert_equal(itm%get_edit_descriptor(), '8(7(6(5(4(3(2(I0,""),""),""),""),""),""),"")', &
                          "array(int(), [2, 3, 4, 5, 6, 7, 8]) should return " &
                          //'8(7(6(5(4(3(2(G0,""),""),""),""),""),""),"")')
        ! teardown
        call itm%destruct()

        ! test
        itm = array(char(), [2, 3, 4, 5, 6, 7, 8, 9])
        call assert_equal(itm%get_edit_descriptor(), '9(8(7(6(5(4(3(2(A,""),""),""),""),""),""),""),"")', &
                          "array(char(), [2, 3, 4, 5, 6, 7, 8, 9]) should return " &
                          //'9(8(7(6(5(4(3(2(A,""),""),""),""),""),""),""),"")')
        ! teardown
        call itm%destruct()

        ! test
        itm = array(logical(), [2, 3, 4, 5, 6, 7, 8, 9, 10])
        call assert_equal(itm%get_edit_descriptor(), '10(9(8(7(6(5(4(3(2(L1,""),""),""),""),""),""),""),""),"")', &
                          "array(logical(), [2, 3, 4, 5, 6, 7, 8, 9, 10]) should return " &
                          //'10(9(8(7(6(5(4(3(2(L1,""),""),""),""),""),""),""),""),"")')
        ! teardown
        call itm%destruct()

        ! test
        itm = array(real(), [2, 3, 4, 5, 6, 7, 8, 9, 10, 11])
        call assert_equal(itm%get_edit_descriptor(), '11(10(9(8(7(6(5(4(3(2(G0,""),""),""),""),""),""),""),""),""),"")', &
                          "array(real(), [2, 3, 4, 5, 6, 7, 8, 9, 10, 11]) should return " &
                          //'11(10(9(8(7(6(5(4(3(2(G0,""),""),""),""),""),""),""),""),""),"")')
        ! teardown
        call itm%destruct()

        ! test
        itm = array(int(), [2, 3], ",")
        call assert_equal(itm%get_edit_descriptor(), '3(2(I0,","),TL1," ",TL1,","),TL1," ",TL1', &
                          'array(int(), [2, 3], ",") should return ' &
                          //'3(2(I0,","),TL1," ",TL1,","),TL1," ",TL1')
        ! teardown
        call itm%destruct()

        ! test
        itm = array(int(), [2, 3, 4], ",")
        call assert_equal(itm%get_edit_descriptor(), '4(3(2(I0,","),TL1," ",TL1,","),TL1," ",TL1,","),TL1," ",TL1', &
                          'array(int(), [2, 3, 4], ",") should return ' &
                          //'4(3(2(I0,","),TL1," ",TL1,","),TL1," ",TL1,","),TL1," ",TL1')
        ! teardown
        call itm%destruct()

        ! test
        itm = array(int(), [2, 3], ",", "{")
        call assert_equal(itm%get_edit_descriptor(), '"{",3("{",2(I0,","),TL1,"}",","),TL1,"}"', &
                          'array(int(), [2, 3], ",", "{") should return ' &
                          //'"{",3("{",2(I0,","),TL1,"}",","),TL1,"}"')
        ! teardown
        call itm%destruct()

        ! test
        itm = array(int(), [2, 3, 4], "~~", "(/")
        call assert_equal(itm%get_edit_descriptor(), &
                          '"(/",4("(/",3("(/",2(I0,"~~"),TL2,"/) ",TL1,"~~"),TL2,"/) ",TL1,"~~"),TL2,"/) ",TL1', &
                          'array(int(), [2, 3, 4], "~~", "(/") should return ' &
                          //'"(/",4("(/",3("(/",2(I0,"~~"),TL2,"/) ",TL1,"~~"),TL2,"/) ",TL1,"~~"),TL2,"/) ",TL1')
        ! teardown
        call itm%destruct()
    end subroutine construct_array_fmt_w_shape_returns_fmt_item
end program test_array
