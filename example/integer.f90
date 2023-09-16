program ex_integer
    use :: fed, only:format, int, operator(//), bin_digits, oct_digits, hex_digits
    implicit none

    print format(int()), 123456
    !123456

    print format(int(10)), 123456
    !    123456

    print format(int(10, 10)), 123456
    !0000123456

    print format(int(2)), 123456
    !**

    print format(int()//int()), 123456, huge(0)
    !1234562147483647

    print format(int()//int(), separator=","), 123456, huge(0)
    !123456,2147483647

    print format(int(bin_digits)), 42
    !101010
    print format(int(bin_digits, 8)), 42
    !00101010
    print format(int(bin_digits, 8, 7)), 42
    ! 0101010

    print format(int(oct_digits)), 342391
    !1234567
    print format(int(oct_digits, 11)), 342391
    !    1234567
    print format(int(oct_digits, 11, 8)), 342391
    !   01234567

    print format(int(hex_digits)), 267242409
    !FEDCBA9
    print format(int(hex_digits, 9)), 267242409
    !  FEDCBA9
    print format(int(hex_digits, 9, 9)), 267242409
    !00FEDCBA9

    block
        use :: fed, bin => int_bin, oct => int_oct, hex => int_hex
        print format(bin()), 42
        !101010
        print format(bin(8)), 42
        !00101010
        print format(bin(8, 7)), 42
        ! 0101010

        print format(oct()), 342391
        !1234567
        print format(oct(11)), 342391
        !    1234567
        print format(oct(11, 8)), 342391
        !   01234567

        print format(hex()), 267242409
        !FEDCBA9
        print format(hex(9)), 267242409
        !  FEDCBA9
        print format(hex(9, 9)), 267242409
        !00FEDCBA9

        print format(int()//bin()//oct()//hex(), separator=","), huge(0), huge(0), huge(0), huge(0)
        !!2147483647,1111111111111111111111111111111,17777777777,7FFFFFFF
    end block
end program ex_integer
