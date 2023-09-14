program ex_real
    use :: fed, only:format, real
    implicit none

    print format(real()), -0.1234567890
    !-0.123456791

    print format(real()), huge(0.0)
    !0.340282347E+39

    print format(real(6, 4)), -0.1234567890
    !-.1235

    print format(real(40, 0)), huge(0.0)
    !340282346638528859811704183484516925440.

    print format(real(7, 4)), -10.0
    !*******
end program ex_real
