module fed
    use :: fed_editDescriptor_data_char, only:char
    use :: fed_editDescriptor_data_logical, only:logical
    use :: fed_editDescriptor_data_integer_decimal, only:int
    use :: fed_editDescriptor_data_integer_binary, only:int_bin
    use :: fed_editDescriptor_data_integer_octal, only:int_oct
    use :: fed_editDescriptor_data_integer_hexadecimal, only:int_hex
    use :: fed_editDescriptor_data_integer_facade, only:int, bin_digits, oct_digits, hex_digits
    use :: fed_editDescriptor_data_real_standard, only:real
    use :: fed_editDescriptor_data_real_exponential, only:real_exp
    use :: fed_editDescriptor_data_real_scientific, only:real_sci
    use :: fed_editDescriptor_data_real_engineering, only:real_eng
    use :: fed_editDescriptor_data_real_facade, only:real, exp_form, sci_form, eng_form
    use :: fed_editDescriptor_data_complex_standard, only:complex
    use :: fed_editDescriptor_data_complex_exponential, only:complex_exp
    use :: fed_editDescriptor_data_complex_scientific, only:complex_sci
    use :: fed_editDescriptor_data_complex_engineering, only:complex_eng
    use :: fed_editDescriptor_data_complex_facade, only:complex
    use :: fed_editDescriptor_data_userDefinedDerivedType, only:udt
    use :: fed_editDescriptor_characterString, only:str
    use :: fed_editDescriptor_control_colon, only:terminate
    use :: fed_editDescriptor_control_slash, only:end_line
    use :: fed_editDescriptor_control_position, only:move, move_to
    use :: fed_editDescriptor_control_blank, only:blank_mode, blank_mode_null, blank_mode_zero
    use :: fed_editDescriptor_control_decimal, only:decimal_mode, decimal_mode_comma, decimal_mode_point
    use :: fed_editDescriptor_control_round, only:rounding_mode, rounding_mode_up, rounding_mode_down, rounding_mode_zero, &
        rounding_mode_nearest, rounding_mode_compatible, rounding_mode_processor_defined
    use :: fed_editDescriptor_control_sign, only:sign_mode, sign_mode_suppress, sign_mode_plus, sign_mode_processor_defined
    use :: fed_format, only:format
    use :: fed_format_items, only:operator(//)
    use :: fed_repeat, only:repeat
    use :: fed_array, only:array
    implicit none
    private
    ! procedures
    public :: int
    public :: real
    public :: complex
    public :: char
    public :: logical
    public :: udt
    public :: str
    public :: terminate
    public :: end_line
    public :: move, move_to
    public :: blank_mode
    public :: decimal_mode
    public :: rounding_mode
    public :: sign_mode
    public :: format
    public :: repeat
    public :: array

    ! enumerators
    public :: bin_digits, &
              oct_digits, &
              hex_digits
    public :: exp_form, &
              sci_form, &
              eng_form

    ! operators
    public :: operator(//)

    ! procedures opened to public for aliasing
    public :: int_bin, &
              int_oct, &
              int_hex
    public :: real_exp, &
              real_sci, &
              real_eng
    public :: complex_exp, &
              complex_sci, &
              complex_eng
    public :: blank_mode_null, &
              blank_mode_zero
    public :: decimal_mode_comma, &
              decimal_mode_point
    public :: rounding_mode_up, &
              rounding_mode_down, &
              rounding_mode_zero, &
              rounding_mode_nearest, &
              rounding_mode_compatible, &
              rounding_mode_processor_defined
    public :: sign_mode_suppress, &
              sign_mode_plus, &
              sign_mode_processor_defined
end module fed
