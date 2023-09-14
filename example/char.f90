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
end program ex_char
