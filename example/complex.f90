program ex_complex
    use :: fed
    implicit none

    print format(complex()), cmplx(-0.1234567890, 1.234567890)
    !(-0.123456791,1.23456788)

    print format(complex()), cmplx(huge(0.0), -huge(0.0))
    !(0.340282347E+39,-0.340282347E+39)

    print format(complex(6, 4)), cmplx(-0.1234567890, 1.234567890)
    !(-.1235,1.2346)

    print format(complex(41, 0)), cmplx(huge(0.0), -huge(0.0))
    !( 340282346638528859811704183484516925440.,-340282346638528859811704183484516925440.)

    print format(complex(7, 4)), cmplx(-10.0, 10.0)
    !(*******,10.0000)
end program ex_complex
