program ex_real
    use :: fed, only:format, real, exp_form, sci_form, eng_form
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

    print format(real(exp_form)), 6.421
    !0.6421000E+1
    print format(real(exp_form)), -0.5
    !-.5000000E+0
    print format(real(exp_form)), 0.00217
    !0.2170000E-2
    print format(real(exp_form)), 4721.3
    !0.4721300E+4
    print format(real(exp_form, 12, 3, 0)), 6.421
    !   0.642E+01
    print format(real(exp_form, 12, 3, 0)), -0.5
    !  -0.500E+00
    print format(real(exp_form, 12, 3, 0)), 0.00217
    !   0.217E-02
    print format(real(exp_form, 12, 3, 0)), 4721.3
    !   0.472E+04

    print format(real(sci_form)), 6.421
    ! 6.4210000E+0
    print format(real(sci_form)), -0.5
    !-5.0000000E-1
    print format(real(sci_form)), 0.00217
    ! 2.1700000E-3
    print format(real(sci_form)), 4721.3
    ! 4.7212998E+3
    print format(real(sci_form, 12, 3, 0)), 6.421
    !   6.421E+00
    print format(real(sci_form, 12, 3, 0)), -0.5
    !  -5.000E-01
    print format(real(sci_form, 12, 3, 0)), 0.00217
    !   2.170E-03
    print format(real(sci_form, 12, 3, 0)), 4721.3
    !   4.721E+03

    print format(real(eng_form)), 6.421
    !   6.4210000E+0
    print format(real(eng_form)), -0.5
    !-500.0000000E-3
    print format(real(eng_form)), 0.00217
    !   2.1700000E-3
    print format(real(eng_form)), 4721.3
    !   4.7212998E+3
    print format(real(eng_form, 12, 3, 0)), 6.421
    !   6.421E+00
    print format(real(eng_form, 12, 3, 0)), -0.5
    !-500.000E-03
    print format(real(eng_form, 12, 3, 0)), 0.00217
    !   2.170E-03
    print format(real(eng_form, 12, 3, 0)), 4721.3
    !   4.721E+03

    block
        use :: fed, exp => real_exp, sci => real_sci, eng => real_eng
        print format(exp()), 6.421
        !0.6421000E+1
        print format(exp()), -0.5
        !-.5000000E+0
        print format(exp()), 0.00217
        !0.2170000E-2
        print format(exp()), 4721.3
        !0.4721300E+4
        print format(exp(12, 3, 0)), 6.421
        !   0.642E+01
        print format(exp(12, 3, 0)), -0.5
        !  -0.500E+00
        print format(exp(12, 3, 0)), 0.00217
        !   0.217E-02
        print format(exp(12, 3, 0)), 4721.3
        !   0.472E+04

        print format(sci()), 6.421
        ! 6.4210000E+0
        print format(sci()), -0.5
        !-5.0000000E-1
        print format(sci()), 0.00217
        ! 2.1700000E-3
        print format(sci()), 4721.3
        ! 4.7212998E+3
        print format(sci(12, 3, 0)), 6.421
        !   6.421E+00
        print format(sci(12, 3, 0)), -0.5
        !  -5.000E-01
        print format(sci(12, 3, 0)), 0.00217
        !   2.170E-03
        print format(sci(12, 3, 0)), 4721.3
        !   4.721E+03

        print format(eng()), 6.421
        !   6.4210000E+0
        print format(eng()), -0.5
        !-500.0000000E-3
        print format(eng()), 0.00217
        !   2.1700000E-3
        print format(eng()), 4721.3
        !   4.7212998E+3
        print format(eng(12, 3, 0)), 6.421
        !   6.421E+00
        print format(eng(12, 3, 0)), -0.5
        !-500.000E-03
        print format(eng(12, 3, 0)), 0.00217
        !   2.170E-03
        print format(eng(12, 3, 0)), 4721.3
        !   4.721E+03
    end block
end program ex_real
