!***********************************************************************
! Module: color_utils_mod
!
! Description:
!   The color_utils_mod module provides utilities for handling ANSI 
!   escape codes for text formatting in terminal outputs. It includes 
!   parameters for various text and background colors, text styles, and 
!   functions to initialize and print colored text.
!
!   This module is useful for creating visually appealing terminal 
!   outputs by allowing customization of text color and style. It 
!   includes support for enabling and disabling ANSI escape codes, 
!   making it adaptable to environments that do not support ANSI codes.
!
! Parameters:
!   RESET              - ANSI escape code to reset all attributes
!   RED                - ANSI escape code for red text
!   GREEN              - ANSI escape code for green text
!   YELLOW             - ANSI escape code for yellow text
!   BLUE               - ANSI escape code for blue text
!   MAGENTA            - ANSI escape code for magenta text
!   CYAN               - ANSI escape code for cyan text
!   WHITE              - ANSI escape code for white text
!   BLACK              - ANSI escape code for black text
!   BRIGHT_RED         - ANSI escape code for bright red text
!   BRIGHT_GREEN       - ANSI escape code for bright green text
!   BRIGHT_YELLOW      - ANSI escape code for bright yellow text
!   BRIGHT_BLUE        - ANSI escape code for bright blue text
!   BRIGHT_MAGENTA     - ANSI escape code for bright magenta text
!   BRIGHT_CYAN        - ANSI escape code for bright cyan text
!   BRIGHT_WHITE       - ANSI escape code for bright white text
!   BG_BLACK           - ANSI escape code for black background
!   BG_RED             - ANSI escape code for red background
!   BG_GREEN           - ANSI escape code for green background
!   BG_YELLOW          - ANSI escape code for yellow background
!   BG_BLUE            - ANSI escape code for blue background
!   BG_MAGENTA         - ANSI escape code for magenta background
!   BG_CYAN            - ANSI escape code for cyan background
!   BG_WHITE           - ANSI escape code for white background
!   BG_BRIGHT_BLACK    - ANSI escape code for bright black background
!   BG_BRIGHT_RED      - ANSI escape code for bright red background
!   BG_BRIGHT_GREEN    - ANSI escape code for bright green background
!   BG_BRIGHT_YELLOW   - ANSI escape code for bright yellow background
!   BG_BRIGHT_BLUE     - ANSI escape code for bright blue background
!   BG_BRIGHT_MAGENTA  - ANSI escape code for bright magenta background
!   BG_BRIGHT_CYAN     - ANSI escape code for bright cyan background
!   BG_BRIGHT_WHITE    - ANSI escape code for bright white background
!   BOLD               - ANSI escape code for bold text
!   DIM                - ANSI escape code for dim text
!   ITALIC             - ANSI escape code for italic text
!   UNDERLINE          - ANSI escape code for underlined text
!   BLINK              - ANSI escape code for blinking text
!   INVERSE            - ANSI escape code for inverse text
!   HIDDEN             - ANSI escape code for hidden text
!   STRIKETHROUGH      - ANSI escape code for strikethrough text
!
! Variables:
!   ansi_support       - Logical flag to check if ANSI support is enabled
!
! Functions/Subroutines:
!   initialize_colors  - Initializes ANSI support based on optional input
!   print_colored      - Prints text in specified color and column position
!
! Usage:
!   1. Initialize colors (optional):
!      call initialize_colors(.true.)
!
!   2. Print colored text:
!      call print_colored('Hello, World!', RED)
!      call print_colored('Warning!', YELLOW, 10)
!
! Author:
!   This module was written by Benjamin Ford.
!**********************************************************************
module color_utils_mod
    implicit none
    ! ANSI escape codes for text colors
    character(len=*), parameter :: RESET = char(27) // '[0m'
    character(len=*), parameter :: RED = char(27) // '[31m'
    character(len=*), parameter :: GREEN = char(27) // '[32m'
    character(len=*), parameter :: YELLOW = char(27) // '[33m'
    character(len=*), parameter :: BLUE = char(27) // '[34m'
    character(len=*), parameter :: MAGENTA = char(27) // '[35m'
    character(len=*), parameter :: CYAN = char(27) // '[36m'
    character(len=*), parameter :: WHITE = char(27) // '[37m'
    character(len=*), parameter :: BLACK = char(27) // '[30m'
    character(len=*), parameter :: BRIGHT_RED = char(27) // '[91m'
    character(len=*), parameter :: BRIGHT_GREEN = char(27) // '[92m'
    character(len=*), parameter :: BRIGHT_YELLOW = char(27) // '[93m'
    character(len=*), parameter :: BRIGHT_BLUE = char(27) // '[94m'
    character(len=*), parameter :: BRIGHT_MAGENTA = char(27) // '[95m'
    character(len=*), parameter :: BRIGHT_CYAN = char(27) // '[96m'
    character(len=*), parameter :: BRIGHT_WHITE = char(27) // '[97m'

    ! ANSI escape codes for background colors
    character(len=*), parameter :: BG_BLACK = char(27) // '[40m'
    character(len=*), parameter :: BG_RED = char(27) // '[41m'
    character(len=*), parameter :: BG_GREEN = char(27) // '[42m'
    character(len=*), parameter :: BG_YELLOW = char(27) // '[43m'
    character(len=*), parameter :: BG_BLUE = char(27) // '[44m'
    character(len=*), parameter :: BG_MAGENTA = char(27) // '[45m'
    character(len=*), parameter :: BG_CYAN = char(27) // '[46m'
    character(len=*), parameter :: BG_WHITE = char(27) // '[47m'
    character(len=*), parameter :: BG_BRIGHT_BLACK = char(27) // '[100m'
    character(len=*), parameter :: BG_BRIGHT_RED = char(27) // '[101m'
    character(len=*), parameter :: BG_BRIGHT_GREEN = char(27) // '[102m'
    character(len=*), parameter :: BG_BRIGHT_YELLOW = char(27) // '[103m'
    character(len=*), parameter :: BG_BRIGHT_BLUE = char(27) // '[104m'
    character(len=*), parameter :: BG_BRIGHT_MAGENTA = char(27) // '[105m'
    character(len=*), parameter :: BG_BRIGHT_CYAN = char(27) // '[106m'
    character(len=*), parameter :: BG_BRIGHT_WHITE = char(27) // '[107m'

    ! ANSI escape codes for text styles
    character(len=*), parameter :: BOLD = char(27) // '[1m'
    character(len=*), parameter :: DIM = char(27) // '[2m'
    character(len=*), parameter :: ITALIC = char(27) // '[3m'
    character(len=*), parameter :: UNDERLINE = char(27) // '[4m'
    character(len=*), parameter :: BLINK = char(27) // '[5m'
    character(len=*), parameter :: INVERSE = char(27) // '[7m'
    character(len=*), parameter :: HIDDEN = char(27) // '[8m'
    character(len=*), parameter :: STRIKETHROUGH = char(27) // '[9m'

    ! Flag to check if ANSI support is enabled
    logical :: ansi_support = .true.

contains

    subroutine initialize_colors(enable_colors)
        implicit none
        logical, intent(in), optional :: enable_colors

        ! Set ANSI support based on the optional input
        if (present(enable_colors)) then
            ansi_support = enable_colors
        end if
    end subroutine initialize_colors

    subroutine print_colored(text, color, column)
        implicit none
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: color
        integer, intent(in), optional :: column
    
        character(len=:), allocatable :: formatted_text
        character(len=*), parameter :: RESET = char(27) // '[0m'  ! ANSI reset code
        integer :: col_pos
    
        ! Default column position is 1 if not provided
        col_pos = 1
        if (present(column)) then
            col_pos = column
        end if
    
        ! Format the text with the specified column position
        if (col_pos > 1) then ! Column position specified
            allocate(character(len=col_pos + len(trim(text))) :: formatted_text)
            formatted_text = repeat(' ', col_pos - 1) // trim(text)
        else ! No column position specified
            formatted_text = trim(text)
        end if
    
        ! Print the text with the specified color
        if (ansi_support) then ! ANSI support enabled, print text with color
            write (*, '(A)') trim(color) // formatted_text // RESET
        else ! ANSI support disabled, print text without color
            write (*, '(A)') formatted_text
        end if
    end subroutine print_colored

end module color_utils_mod
