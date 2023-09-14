program demo
    use, intrinsic :: iso_fortran_env
    use :: fed
    implicit none

    character(:), allocatable :: fmt_bnd
    integer(int32), allocatable :: i(:, :), j(:, :)

    ! combine edit descriptors for different types
    print format(int()//char()//logical()//real()//"256"//complex(), separator=","), &
        256, "'256'", .false., 256.0, cmplx(256.0, 512.0)
    !256,'256',F,256.000000,256,(256.000000,512.000000)

    allocate (i(0:10, -2:3))
    allocate (j(-6:0, -20:-10))

    print format("allocataion status: "//logical()), allocated(i)
    print format("memory size: "//int()//" bits"), storage_size(i)*size(i)
    !allocataion status: T
    !storage size: 2112 bits

    ! a format-specifier can be easily reuse by storing in a string
    fmt_bnd = format("array bounds = ["//repeat(int(4), 2)//"] x ["//repeat(int(4), 2)//"]")
    print fmt_bnd, lbound(i), ubound(i)
    !array bounds = [   0  -2] x [  10   3]

    print format("allocataion status: "//logical()), allocated(j)
    print format("memory size: "//int()//" bits"), storage_size(j)*size(j)
    !allocataion status: T
    !storage size: 2464 bits

    print fmt_bnd, lbound(j), ubound(j)
    !array bounds = [  -5 -20] x [   0 -10]

    deallocate (i)
    deallocate (j)
end program demo
