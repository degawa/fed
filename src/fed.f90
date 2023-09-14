module fed
    use :: fed_editDescriptor_data_char, only:char
    use :: fed_editDescriptor_data_logical, only:logical
    use :: fed_editDescriptor_data_integer_decimal, only:int
    use :: fed_editDescriptor_data_real_standard, only:real
    use :: fed_editDescriptor_data_complex_standard, only:complex
    use :: fed_format, only:format
    use :: fed_format_items, only:operator(//)
    implicit none
    private
    ! procedures
    public :: int
    public :: real
    public :: complex
    public :: char
    public :: logical
    public :: format

    ! operators
    public :: operator(//)
end module fed
