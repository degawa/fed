module type_vector_2d
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: vector_2d
    public :: write (formatted)

    type, public :: vector_2d_type
        real(real32), private :: x, y
    end type vector_2d_type

    interface write (formatted)
    procedure formatted_write
    end interface

    contains
    !>vetor_2d_typeインスタンスを返す．
    function vector_2d(x, y) result(new_vector_2d)
        implicit none
        real(real32), intent(in) :: x, y
        type(vector_2d_type) :: new_vector_2d

        new_vector_2d%x = x
        new_vector_2d%y = y
    end function vector_2d

    !>vector_2d_typeの成分を出力する．
    !>ユーザ定義派生型出力write(formatted)として用いられる．
    subroutine formatted_write(vec, unit, iotype, v_list, io_status, io_message)
        use :: fed, sci => real_sci
        implicit none
        class(vector_2d_type), intent(in) :: vec
        integer(int32), intent(in) :: unit
        character(*), intent(in) :: iotype
        integer(int32), intent(in) :: v_list(:)
        integer(int32), intent(out) :: io_status
        character(*), intent(inout) :: io_message

        character(:), allocatable :: fmt

        if ((iotype == "LISTDIRECTED" .or. len(iotype) == len("DT")) &
            .or. size(v_list) < 2) then
            write (unit, *, iostat=io_status, iomsg=io_message) vec%x, vec%y
            io_status = 0
            io_message = ""
            return
        end if

        if (iotype(3:) /= "vector_2d") then
            io_status = 1
            io_message = "type mismatch"
            return
        end if

        if (size(v_list) == 2) fmt = format("["//2*real(v_list(1), v_list(2))//"]")
        if (size(v_list) >= 3) fmt = format("["//2*sci(v_list(1), v_list(2), v_list(3))//"]")

        write (unit, fmt, &
               iostat=io_status, iomsg=io_message) vec%x, vec%y
        io_status = 0
        io_message = ""
    end subroutine formatted_write
end module type_vector_2d

program test_udt
    use :: fed
    use :: type_vector_2d
    implicit none

    type(vector_2d_type) :: vec
    vec = vector_2d(0.00217, 4721.3)

    print *, vec
    !    2.16999999E-03   4721.29980
    print '(DT"vector_2d"(12, 3))', vec
    ![       0.002    4721.300]

    print format(udt("vector_2d", [12, 3])), vec
    ![       0.002    4721.300]
    print format(udt("vector_2d", [12, 3, 1])), vec
    ![    2.170E-3    4.721E+3]
end program test_udt
