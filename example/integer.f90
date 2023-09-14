program ex_integer
    use :: fed, only:format, int, operator(//)
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

end program ex_integer
