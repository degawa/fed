program ex_complex
    use :: fed, only:format, complex, exp_form, sci_form, eng_form
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

    print format(complex(exp_form)), cmplx(6.421, -0.5)
    !(0.6421000E+1,-.5000000E+0)
    print format(complex(exp_form)), cmplx(0.00217, 4721.3)
    !(0.2170000E-2,0.4721300E+4)
    print format(complex(exp_form, 12, 3, 0)), cmplx(6.421, -0.5)
    !(   0.642E+01,  -0.500E+00)
    print format(complex(exp_form, 12, 3, 0)), cmplx(0.00217, 4721.3)
    !(   0.217E-02,   0.472E+04)

    print format(complex(sci_form)), cmplx(6.421, -0.5)
    !( 6.4210000E+0,-5.0000000E-1)
    print format(complex(sci_form)), cmplx(0.00217, 4721.3)
    !( 2.1700000E-3, 4.7212998E+3)
    print format(complex(sci_form, 12, 3, 0)), 6.421, -0.5
    !(   6.421E+00,  -5.000E-01)
    print format(complex(sci_form, 12, 3, 0)), 0.00217, 4721.3
    !(   2.170E-03,   4.721E+03)

    print format(complex(eng_form)), 6.421, -0.5
    !(   6.4210000E+0,-500.0000000E-3)
    print format(complex(eng_form)), 0.00217, 4721.3
    !(   2.1700000E-3,   4.7212998E+3)
    print format(complex(eng_form, 12, 3, 0)), 6.421, -0.5
    !(   6.421E+00,-500.000E-03)
    print format(complex(eng_form, 12, 3, 0)), 0.00217, 4721.3
    !(   2.170E-03,   4.721E+03)

    block
        use :: fed, exp => complex_exp, sci => complex_sci, eng => complex_eng
        print format(exp()), cmplx(6.421, -0.5)
        !(0.6421000E+1,-.5000000E+0)
        print format(exp()), cmplx(0.00217, 4721.3)
        !(0.2170000E-2,0.4721300E+4)
        print format(exp(12, 3, 0)), cmplx(6.421, -0.5)
        !(   0.642E+01,  -0.500E+00)
        print format(exp(12, 3, 0)), cmplx(0.00217, 4721.3)
        !(   0.217E-02,   0.472E+04)

        print format(sci()), cmplx(6.421, -0.5)
        !( 6.4210000E+0,-5.0000000E-1)
        print format(sci()), cmplx(0.00217, 4721.3)
        !( 2.1700000E-3, 4.7212998E+3)
        print format(sci(12, 3, 0)), 6.421, -0.5
        !(   6.421E+00,  -5.000E-01)
        print format(sci(12, 3, 0)), 0.00217, 4721.3
        !(   2.170E-03,   4.721E+03)

        print format(eng()), 6.421, -0.5
        !(   6.4210000E+0,-500.0000000E-3)
        print format(eng()), 0.00217, 4721.3
        !(   2.1700000E-3,   4.7212998E+3)
        print format(eng(12, 3, 0)), 6.421, -0.5
        !(   6.421E+00,-500.000E-03)
        print format(eng(12, 3, 0)), 0.00217, 4721.3
        !(   2.170E-03,   4.721E+03)
    end block
end program ex_complex
