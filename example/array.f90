program ex_array
    use :: fed
    implicit none

    logical :: l(4), l3d(2, 3)
    l = .false.
    l3d = .false.

    print format(array(logical(), size(l))), l
    !FFFF

    print format(array(logical(), size(l), ",")), l
    !F,F,F,F

    print format(array(logical(), shape(l3d), separator=",")), l3d
    !F,F,F,F,F,F

    print format(array(logical(), shape(l3d), separator=",", bracket_open="[")), l3d
    ![[F,F],[F,F],[F,F]]
end program ex_array
