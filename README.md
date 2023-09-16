# fed
feeds a string with Fortran edit descriptors.

## Motivations
I am fed up with Fortran's edit descriptors.
- Unmemorable number of edit descriptors and their features
- Switching between apostrophes `'` and double quotation marks `"` to describe the format specification
- A mismatch between the number of variables and the number of edit descriptors, especially when writing complex type variables
- Repetition of commas (`,`) and quotation marks (`'` or `"`) like `("(",G0,",",G0,")")'`
- etc.

This library provides procedures and an operator for generating format specifications in a more human-understandable manner.

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
- [ ] user-defined type
- [ ] arrays
    - [ ] character
    - [ ] logical
    - [ ] integer
    - [ ] real
    - [ ] complex

### control edit descriptors
- [ ] position
- [ ] slash
- [ ] colon
- [ ] sign
- [ ] blank
- [ ] round
- [ ] decimal

### character string edit descriptor
- [x] character string

## Overview
The current version of fed provides the following functions and an operator:

### Procedures
|                       procedure                        |                                                                            functionality                                                                             |
| :----------------------------------------------------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `char([width])`                                        |generates a edit descriptor for character. <br>`A<width>` or `A`|
| `logical([width])`                                     |generates a edit descriptor for logiacl. <br>`L<width>`|
| `int([form={bin_digits/oct_digits/hex_digits},][width,[zero_padding_digit]])`|generates a edit descriptor for integer. <br>`I0`<br>`I<width>`<br>`I<width>.<zero_padding_digit>`<br>`B0`<br>`B<width>.<width>`<br>`B<width>.<zero_padding_digit>`<br>`O0`<br>`O<width>.<width>`<br>`O<width>.<zero_padding_digit>`<br>`Z0`<br>`Z<width>.<width>`<br>`Z<width>.<zero_padding_digit>`|
| `real([form={exp_form/sci_form/eng_form},][width,[decimal_place_digits,[exponent_digits]]])`|generates a edit descriptor for real. <br>`G0`<br>`F<width>.<decimal_place_digits>`<br>`E<width>.<decimal_place_digits>`<br>`E<width>.<decimal_place_digits>E<exponent_digits>`<br>`ES<width>.<decimal_place_digits>`<br>`ES<width>.<decimal_place_digits>E<exponent_digits>`<br>`EN<width>.<decimal_place_digits>`<br>`EN<width>.<decimal_place_digits>E<exponent_digits>`|
| `complex([form={exp_form/sci_form/eng_form},][width,[decimal_place_digits,[exponent_digits]]])`|generates a sequene of edit descriptors for writing complex. <br>`"(",G0,",",G0,")"`<br>`"(",F<width>.<decimal_place_digits>,",",F<width>.<decimal_place_digits>,")"`<br>`"(",E<width>.<decimal_place_digits>,",",E<width>.<decimal_place_digits>,")"`<br>`"(",E<width>.<decimal_place_digits>E<exponent_digits>,",",E<width>.<decimal_place_digits>E<exponent_digits>,")"`<br>`"(",ES<width>.<decimal_place_digits>,",",ES<width>.<decimal_place_digits>,")"`<br>`"(",ES<width>.<decimal_place_digits>E<exponent_digits>,",",ES<width>.<decimal_place_digits>E<exponent_digits>,")"`<br>`"(",EN<width>.<decimal_place_digits>,",",EN<width>.<decimal_place_digits>,")"`<br>`"(",EN<width>.<decimal_place_digits>E<exponent_digits>,",",EN<width>.<decimal_place_digits>E<exponent_digits>,")"`|
| `str(character_string)`                                |generates a character string edit descriptor. |
| `format(format_items[, separator])`                    |generates a format specification as character.|
| `repeat(format_items[, separator][, repeat_count])`    |generates a repeated/unlimited format item.<br>`repeat(repeat(...))` is not supported yet.|

### An operator
|operator|functionality|
|:--|:--|
|`//`|catenates edit descriptors.|

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
    fmt_bnd = format("array bounds = ["//repeat(int(4), rank(i))//"] x ["//repeat(int(4), rank(i))//"]")

    print stat, allocated(i)               !allocataion status: T
    print memsize, storage_size(i)*size(i) !storage size: 2112 bits
    print fmt_bnd, lbound(i), ubound(i)    !array bounds = [   0  -2] x [  10   3]

    print stat, allocated(j)               !allocataion status: T
    print memsize, storage_size(j)*size(j) !storage size: 2464 bits
    print fmt_bnd, lbound(j), ubound(j)    !array bounds = [  -5 -20] x [   0 -10]

    deallocate (i)
    deallocate (j)
```
### Types for internal representation
fed defines three categories of user-defined types for constructing Fortran *format specification*.
- type for *format items*
- type for *format item*
- types for *edit descriptor*

According to the Fortran standard, the *format specification* consists of *format items*. The *format items* is an aggregation of multiple *format item*.  The *edit descriptor* is subdivided into three categories: *data edit descriptor*, *control edit descriptor*, and *character string edit descriptor*. A *format item* is one of *data edit descriptor*, *control edit descriptor*, *character string edit descritpro*, or repeated *format items*.

However, users can use fed without being aware of the differences.

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