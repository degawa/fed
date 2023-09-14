program ex_logical
    use :: fed, only:format, logical, operator(//)
    implicit none

    print format(logical()), .false.
    !F
    print format(logical(3)), .true.
    !  T

    print format(logical()//logical()), .false., .true.
    !FT

    print format(logical()//logical()//logical(), separator=","), .false., .true., .false.
    !F,T,F

    print *, format(logical()//logical())
    !(L1,L1)

    print *, format(logical()//logical()//logical(), separator=",")
    !(L1,",",L1,",",L1)
end program ex_logical
