program demo
    use, intrinsic :: iso_fortran_env
    use :: fed
    implicit none

    character(:), allocatable :: fmt_bnd, stat, memsize
    integer(int32), allocatable :: i(:, :), j(:, :)

    ! combine edit descriptors for different types
    print format(int()//char()//logical()//real()//"256"//complex(), separator=","), &
        256, "'256'", .false., 256.0, cmplx(256.0, 512.0)
    !256,'256',F,256.000000,256,(256.000000,512.000000)

    allocate (i(0:10, -2:3))
    allocate (j(-6:0, -20:-10))

    ! a format-specifier can be easily reuse by storing in a string
    stat = format("allocataion status: "//logical())
    memsize = format("memory size: "//int()//" bits")
    fmt_bnd = format("array bounds = ["//2*int(4)//"] x ["//2*int(4)//"]")

    print stat, allocated(i) !allocataion status: T
    print memsize, storage_size(i)*size(i) !storage size: 2112 bits
    print fmt_bnd, lbound(i), ubound(i) !array bounds = [   0  -2] x [  10   3]

    print stat, allocated(j) !allocataion status: T
    print memsize, storage_size(j)*size(j) !storage size: 2464 bits
    print fmt_bnd, lbound(j), ubound(j) !array bounds = [  -5 -20] x [   0 -10]

    deallocate (i)
    deallocate (j)
end program demo
