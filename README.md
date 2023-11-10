# fed
functions to create Fortran edit descriptors.

## Motivations
I am fed up with Fortran's edit descriptors.
- Unmemorable number of edit descriptors and their features
- Switching between apostrophes `'` and double quotation marks `"` to describe the format specification
- A mismatch between the number of variables and the number of edit descriptors, especially when writing complex type variables
- Repetition of commas (`,`) and quotation marks (`'` or `"`) like `'("(",G0,",",G0,")")'`
- etc.

This library provides procedures and operators for creating format specifications in a more human-understandable manner.

## Supported Features
### format item
- [x] repeated/unlimited format item

### data edit descriptors
- [x] `A` (character)
- [x] `L` (logical)
- [x] integer
    - [x] `I` (decimal)
    - [x] `B` (binary)
    - [x] `O` (octal)
    - [x] `Z` (hexadecimal)
- [x] real
    - [x] `G` (general)
    - [x] `F` (standard form)
    - [x] `E` (exponential form)
    - [x] `EN` (engineering form)
    - [x] `ES` (scientific form)
    - [ ] ~~`EX` (hexadecimal-significand form)~~
- [x] complex
    - [x] `G` (general)
    - [x] `F` (standard form)
    - [x] `E` (exponential form)
    - [x] `EN` (engineering form)
    - [x] `ES` (scientific form)
    - [ ] ~~`EX` (hexadecimal-significand form)~~
- [x] user-defined type
- [x] arrays
    - [x] character
    - [x] logical
    - [x] integer
    - [x] real
    - [x] complex

### control edit descriptors
- [x] position
    - [x] `T`
    - [x] `TL`, `TR`
- [x] slash
- [x] colon
- [x] sign
    - [x] `SS`
    - [x] `SP`
    - [x] `S`
- [x] blank
    - [x] `BN`
    - [x] `BZ`
- [x] round
    - [x] `RU`
    - [x] `RD`
    - [x] `RZ`
    - [x] `RN`
    - [x] `RC`
    - [x] `RP`
- [x] decimal
    - [x] `DC`
    - [x] `DP`

### character string edit descriptor
- [x] character string

## Overview
The current version of fed provides the following functions and operators:

### Procedures
- `char([width])`: creates a edit descriptor for character.
    - `A`
    - `A<width>`
- `logical([width])`: creates a edit descriptor for logiacl.
    - `L<width>`
- `int([form={bin_digits/oct_digits/hex_digits}][, width[, zero_padding_digit]])`: creates a edit descriptor for integer.
    - `I0`
    - `I<width>`
    - `I<width>.<zero_padding_digit>`
    - `B0`
    - `B<width>.<width>`
    - `B<width>.<zero_padding_digit>`
    - `O0`
    - `O<width>.<width>`
    - `O<width>.<zero_padding_digit>`
    - `Z0`
    - `Z<width>.<width>`
    - `Z<width>.<zero_padding_digit>`
- `real([form={exp_form/sci_form/eng_form}][, width[, decimal_place_digits[, exponent_digits]]])`: creates a edit descriptor for real.
    - `G0`, `F<width>.<decimal_place_digits>`
    - `E<width>.<decimal_place_digits>`
    - `E<width>.<decimal_place_digits>E<exponent_digits>`
    - `ES<width>.<decimal_place_digits>`
    - `ES<width>.<decimal_place_digits>E<exponent_digits>`
    - `EN<width>.<decimal_place_digits>`
    - `EN<width>.<decimal_place_digits>E<exponent_digits>`
- `complex([form={exp_form/sci_form/eng_form}][, width[, decimal_place_digits[, exponent_digits]]])`: creates a sequene of edit descriptors for writing complex.
    - `"(",G0,",",G0,")"`
    - `"(",F<width>.<decimal_place_digits>,",",F<width>.<decimal_place_digits>,")"`
    - `"(",E<width>.<decimal_place_digits>,",",E<width>.<decimal_place_digits>,")"`
    - `"(",E<width>.<decimal_place_digits>E<exponent_digits>,",",E<width>.<decimal_place_digits>E<exponent_digits>,")"`
    - `"(",ES<width>.<decimal_place_digits>,",",ES<width>.<decimal_place_digits>,")"`
    - `"(",ES<width>.<decimal_place_digits>E<exponent_digits>,",",ES<width>.<decimal_place_digits>E<exponent_digits>,")"`
    - `"(",EN<width>.<decimal_place_digits>,",",EN<width>.<decimal_place_digits>,")"`
    - `"(",EN<width>.<decimal_place_digits>E<exponent_digits>,",",EN<width>.<decimal_place_digits>E<exponent_digits>,")"`
- `str(character_string)`: creates a character string edit descriptor.
- `array(data_edit_descriptor, {array_size/array_shape}[, separator][, bracket_open])`: creates a sequence of edit descriptors for writing an array.
- `udt([iotype][, v_list(:)])`: creates a edit desciptor for a user-defined derived type.
    - `DT"iotype"(v_list(1), v_list(2), ...)`
- `terminate()`: creates the colon edit descriptor that terminates format control if there are no more effective items in the input/output list.
- `end_line()`: creates the slash edit descriptor that ends data transfer to or from the current record.
- `move(characters)`: creates the `TL abs(<characters>)` or `TR<characters>` edit descriptor that moves `<characters>` characters from the current position.
    - `characters` must not be zero.
- `move_to(column)`: creates the `T<column>` edit descriptor that moves to `<column>`th column from the left tab limit, with the left tab limit as the 1st column.
- `blank_mode%{null/zero}(format_items)`: creates format items with locally changed blank interpretation mode by placing `BN` or `BZ` edit descriptor before and after the `format_items`.
- `decimal_mode%{comma/point}(format_items)`: creates format items with locally changed decimal edit mode by placing `DC` or `DP` edit descriptor before and after the `format_items`.
- `rounding_mode%{up/down/zero/nearest/compatible/processor_defined}(format_items)`: creates format items with locally changed rounding mode by placing `RU`, `RD`, `RZ`, `RN`, `RC` or `RP` edit descriptor before and after the `format_items`.
- `sign_mode%{suppress/plus/processor_defined}(format_items)`: creates format items with locally changed sign mode by placing `SS`, `SP`, or `S` edit descriptor before and after the `format_items`.
- `format(format_items[, separator])`: creates a format specification as characters.
    - The separator is placed before **data and character string edit descriptors** when `separator` is passed.
    - `separator` must not be `"`
- `repeat(format_items[, separator][, repeat_count])`: creates a repeated/unlimited format item.
    - The separator is placed before **data edit descriptors** when `separator` is passed.
    - `separator` must not be `"`


### Operators
- `//`: catenates edit descriptors.
- `*`: sets a repeat count of a data edit descriptor.

### Examples
```Fortran
    use :: fed
    implicit none

    print format(char()), "qwerty"  !qwerty
    print format(char(3)), "qwerty" !qwe

    print format(logical()), .false.  !F
    print format(logical(3)), .true.  !  T

    print format(int()), 123456       !123456
    print format(int(10)), 123456     !    123456
    print format(int(10, 10)), 123456 !0000123456
    print format(int(2)), 123456      !**

    print format(real()), -0.1234567890     !-0.123456791
    print format(real(6, 4)), -0.1234567890 !-.1235
    print format(real(7, 4)), -10.0         !*******

    print format(complex()), cmplx(-0.1234567890, 1.234567890)     !(-0.123456791,1.23456788)
    print format(complex(6, 4)), cmplx(-0.1234567890, 1.234567890) !(-.1235,1.2346)
    print format(complex(7, 4)), cmplx(-10.0, 10.0)                !(*******,10.0000)
```

```Fortran
    use :: fed
    implicit none

    print format(int()//char()//logical()//real()//complex(), separator=","), &
                 256,  "'256'", .false.,   256.0, cmplx(256.0, 512.0)
    !256,'256',F,256.000000,(256.000000,512.000000)
```

```Fortran
    use, intrinsic :: iso_fortran_env
    use :: fed
    implicit none

    character(:), allocatable :: fmt_bnd, stat, memsize
    integer(int32), allocatable :: i(:, :), j(:, :)

    allocate (i(0:10, -2:3))
    allocate (j(-6:0, -20:-10))

    stat = format("allocataion status: "//logical())
    memsize = format("memory size: "//int()//" bits")
    fmt_bnd = format("array bounds = ["//2*int(4)//"] x ["//2*int(4)//"]")

    print stat, allocated(i)               !allocataion status: T
    print memsize, storage_size(i)*size(i) !storage size: 2112 bits
    print fmt_bnd, lbound(i), ubound(i)    !array bounds = [   0  -2] x [  10   3]

    print stat, allocated(j)               !allocataion status: T
    print memsize, storage_size(j)*size(j) !storage size: 2464 bits
    print fmt_bnd, lbound(j), ubound(j)    !array bounds = [  -6 -20] x [   0 -10]

    deallocate (i)
    deallocate (j)
```

```Fortran
    use :: fed
    implicit none

    logical :: l(2, 3)

    print format(repeat(logical(), separator=",")), l
    !F,F,T,T,F,F, ! output values may vary
    print format(repeat(logical()//terminate(), separator=",")), l
    !F,F,T,T,F,F
    print format("["//end_line()//repeat(logical(2), size(l))//end_line()//"]"), l
    ![
    ! F F T T F F
    !]

    print format(str("123456789012345"))
    print format("|"//move_to(8)//"^")
    !12345678901234567890
    !|      ^
    print format(str("123456789012345"))
    print format(move_to(8)//move(-3)//"*"//move(5)//"$")
    !12345678901234567890
    !    *     $

    print format(real()//decimal_mode%comma(real())//real(), separator=" "), 0.123, 0.123, -1.23
    !0.123000003 0,123000003 -1.23000002

    print format(rounding_mode%down(real(3, 0)) &
                 //rounding_mode%up(real(3, 0)) &
                 //rounding_mode%zero(real(3, 0)) &
                 //rounding_mode%nearest(real(3, 0)), separator=" "), -1.5, -1.5, -1.5, -1.5
    ! -2. -1. -1. -2.

    print format(sign_mode%plus(int())//int(), separator=" "), 10, 10
    ! +10 10
```

```Fortran
    use :: fed
    implicit none

    integer :: i
    character(:), allocatable :: input

    input = ' 1 23 '
    read (input, format(blank_mode%zero(int(6)))) i
    print format(int()), i
    !10230

    read (input, format(blank_mode%null(int(6)))) i
    print format(int()), i
    !123
```

```Fortran
    type(vector_2d_type) :: vec
    vec = vector_2d(0.00217, 4721.3)

    print *, vec !    2.16999999E-03   4721.29980
    print format(udt("vector_2d", [12, 3])), vec ![       0.002    4721.300]
    print format(udt("vector_2d", [12, 3, 1])), vec  ![    2.170E-3    4.721E+3]

!---- definition of vector_2d_type and formatted write procedure -----
    type, public :: vector_2d_type
        real(real32), private :: x, y
    end type vector_2d_type

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
```

```Fortran
    use :: fed
    implicit none

    logical :: l(4), l3d(2, 3)
    l = .false.
    l3d = .false.

    print format(array(logical(), size(l), ",", "[")), l
    ![F,F,F,F]

    print format(array(logical(), shape(l3d), separator=",", bracket_open="[")), l3d
    ![[F,F],[F,F],[F,F]]
```

### Types for internal representation
fed defines three categories of user-defined types for constructing Fortran *format specification*.
- type for *format items*
- type for *format item*
- types for *edit descriptor*

According to the Fortran standard, the *format specification* consists of *format items*. The *format items* is an aggregation of multiple *format item*.  The *edit descriptor* is subdivided into three categories: *data edit descriptor*, *control edit descriptor*, and *character string edit descriptor*. A *format item* is one of *data edit descriptor*, *control edit descriptor*, *character string edit descriptor*, or repeated *format items*.

However, users can use fed without paying attention to the differences between those types.

## Getting started
### Requirements
fed has been tested only on Windows 10 but may also work on Linux/Mac OS.
The compilers and versions listed below have been used to develop fed.

- A Fortran 2008 compiler
    - gfortran 11.2 bundled with [quickstart Fortran on Windows](https://github.com/LKedward/quickstart-fortran)
    - Intel Fortran Classic 2021.5.0 Build 20211109_000000
    - NAG Fortran 7.1 Build 7117
- [Fortran Package Manager](https://github.com/fortran-lang/fpm) (fpm) 0.9.0 alpha

### Get the code
To get the code, execute the following commnad:

```console
git clone https://github.com/degawa/fed.git
cd fed
```

### Build with fpm
To build the library using fpm, execute the following command:

```console
fpm build
```

Then, install the library using:

```console
fpm install --prefix path/to/your/libdir
```

### Running the tests
To run the tests for the  using fpm, execute the following command:

```console
fpm test
```

### Reference from your project
Add the following `use` statement to modules or procedures calling fed.

```Fortran
use :: fed
```

### Reference as a fpm project's dependency
To use fed in your fpm project, add the following to the fpm.toml.

```TOML
[dependencies]
fed = {git = "https://github.com/degawa/fed.git"}
```