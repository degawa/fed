program ex_char
    use :: fed, only:format, char, operator(//)
    implicit none

    print format(char()), "qwerty"
    !qwerty

    print format(char(3)), "qwerty"
    !qwe

    print format(char()), "qwerty", "asdfgh"
    !qwerty
    !asdfgh

    print format(char(3)), "qwerty", "asdfgh"
    !qwe
    !asd

    print format(char()//char()//char()), "qwert", "asdf", "zx"
    !qwertasdfzx

    print format(char()//char()//char(), separator="//"), "qwert", "asdf", "zx"
    !qwert//asdf//zx

    print *, format(char())
    !(A)

    print *, format(char(3))
    !(A3)

    print *, format(char()//char()//char())
    !(A,A,A)

    print *, format(char()//char()//char(), separator="//")
    !(A,"//",A,"//",A)
end program ex_char
