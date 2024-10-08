module fortran_terminal_enhancer_mod
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

    ! Define a derived type to store text style options
    type :: TextStyle
        character(len=15) :: color = "WHITE"
        character(len=20) :: bg_color = ""
        logical :: bold = .false.
        logical :: italic = .false.
        logical :: underline = .false.
        integer :: indent = 0
        character(len=10) :: alignment = "left"
    end type TextStyle

    ! Enhanced preset styles with distinct styling for code
    type(TextStyle), parameter :: TITLE_STYLE = TextStyle(color="BRIGHT_BLUE", &
        bg_color="", bold=.true., underline=.false., alignment="center")

    type(TextStyle), parameter :: SUBTITLE_STYLE = TextStyle(color="WHITE", &
        bg_color="", bold=.true., alignment="center")

    type(TextStyle), parameter :: HEADING_STYLE = TextStyle(color="BLUE", &
        bg_color="", bold=.true., alignment="left")

    type(TextStyle), parameter :: BODY_STYLE_CONST = TextStyle(color="WHITE", &
        bg_color="", alignment="left")
    type(TextStyle), parameter :: BODY_BOLD = TextStyle(color="WHITE", &
        bg_color="", bold=.true., alignment="left")
    type(TextStyle), parameter :: BODY_STYLE_INDENT = TextStyle(color="WHITE", &
        bg_color="", alignment="left", indent=4)
        type(TextStyle), parameter :: BULLET_STYLE = TextStyle(color="CYAN", &
        bg_color="", alignment="left", indent=4)

    ! Block color styles with background colors
    type(TextStyle), parameter :: ERROR_BLOCK_STYLE = TextStyle(color="", &
        bg_color="BG_BRIGHT_RED", bold=.false., italic=.false., underline=.false., alignment="left")

    type(TextStyle), parameter :: SUCCESS_BLOCK_STYLE = TextStyle(color="", &
        bg_color="BG_GREEN", bold=.false., italic=.false., underline=.false., alignment="left")

    type(TextStyle), parameter :: HEADING_BLOCK_STYLE = TextStyle(color="", &
        bg_color="BG_BLUE", bold=.false., italic=.false., underline=.false., alignment="left")

    type(TextStyle), parameter :: WARNING_BLOCK_STYLE = TextStyle(color="", &
        bg_color="BG_YELLOW", bold=.false., italic=.false., underline=.false., alignment="left")

    type(TextStyle), parameter :: INFO_BLOCK_STYLE = TextStyle(color="", &
        bg_color="BG_CYAN", bold=.false., italic=.false., underline=.false., alignment="left")
    
    type(TextStyle), parameter :: PURPLE_BLOCK_STYLE = TextStyle(color="", &
        bg_color="BG_BRIGHT_MAGENTA", bold=.false., italic=.false., underline=.false., alignment="left")
    
    type(TextStyle), parameter :: WHITE_BLOCK_STYLE = TextStyle(color="", &
        bg_color="BG_WHITE", bold=.false., italic=.false., underline=.false., alignment="left")
    
    ! Additional styles
    type(TextStyle), parameter :: ERROR_STYLE = TextStyle(color="BRIGHT_RED", &
        bg_color="", bold=.true., alignment="left")

    type(TextStyle), parameter :: WARNING_STYLE = TextStyle(color="YELLOW", &
        bg_color="", bold=.true., alignment="left")

    type(TextStyle), parameter :: SUCCESS_STYLE = TextStyle(color="GREEN", &
        bg_color="", bold=.true., alignment="left")
    type(TextStyle), parameter :: QUOTE_STYLE = TextStyle(color="MAGENTA", &
        bg_color="", italic=.true., alignment="left", indent=2)

    ! Enhanced CODE_STYLE with background color and dim text
    type(TextStyle), parameter :: CODE_STYLE = TextStyle(color="WHITE", &
        bg_color="BG_BRIGHT_BLACK", bold=.false., italic=.false., alignment="left", indent=4)

    type(TextStyle), parameter :: FOOTER_STYLE = TextStyle(color="BRIGHT_CYAN", &
        bg_color="", bold=.false., alignment="center")

    ! Info style for general informational messages
    type(TextStyle), parameter :: INFO_STYLE = TextStyle(color="CYAN", &
        bg_color="", bold=.false., alignment="left")

    ! Debug style for debug messages
    type(TextStyle), parameter :: DEBUG_STYLE = TextStyle(color="BRIGHT_WHITE", &
        bg_color="", italic=.true., alignment="left")

    ! Input prompt style to prompt user input
    type(TextStyle), parameter :: INPUT_PROMPT_STYLE = TextStyle(color="BRIGHT_YELLOW", &
        bg_color="", bold=.true., alignment="left")

    ! Highlight style for emphasizing important notes or key points
    type(TextStyle), parameter :: HIGHLIGHT_STYLE = TextStyle(color="BRIGHT_WHITE", &
        bg_color="BG_RED", bold=.true., alignment="center")

    ! Link style for displaying clickable or hyperlinked text
    type(TextStyle), parameter :: LINK_STYLE = TextStyle(color="BLUE", &
        bg_color="", underline=.true., alignment="left")

    ! Title highlight style with a background color
    type(TextStyle), parameter :: TITLE_HIGHLIGHT_STYLE = TextStyle(color="WHITE", &
        bg_color="BG_BLUE", bold=.true., alignment="center")

    ! Disabled text style for options that are inactive
    type(TextStyle), parameter :: DISABLED_TEXT_STYLE = TextStyle(color="DIM", &
        bg_color="", italic=.true., alignment="left")

    ! Subheading style for secondary headings or subtopics
    type(TextStyle), parameter :: SUBHEADING_STYLE = TextStyle(color="BRIGHT_CYAN", &
        bg_color="", bold=.true., underline=.false., alignment="left")

    ! Shadowed text style for less emphasized text
    type(TextStyle), parameter :: SHADOWED_TEXT_STYLE = TextStyle(color="BLACK", &
        bg_color="", italic=.false., bold=.false., alignment="left")
    type(TextStyle), parameter :: DIMMED_TEXT_STYLE = TextStyle(color="DIM", &
        bg_color="", italic=.false., bold=.false., alignment="left")
        ! Style used for plotting
    type(TextStyle), parameter :: PLOT_STYLE = TextStyle(color="BRIGHT_WHITE", &
        bg_color="", bold=.false., alignment="left")

    type :: DataSeries
        real, allocatable :: x(:)
        real, allocatable :: y(:)
        character(len=1) :: symbol = '*'
        type(TextStyle) :: style = PLOT_STYLE
    end type DataSeries
 
    ! Generic interface for print_matrix
    interface print_matrix
        module procedure print_matrix_single, print_matrix_double, &
            print_matrix_quadruple
    end interface print_matrix


    contains

    subroutine initialize_colors(enable_colors)
        implicit none
        logical, intent(in), optional :: enable_colors

        ! Set ANSI support based on the optional input
        if (present(enable_colors)) then
            ansi_support = enable_colors
        end if
    end subroutine initialize_colors

    subroutine print_styled(text, style)
        implicit none
        character(len=*), intent(in) :: text
        type(TextStyle), intent(in) :: style
        character(len=:), allocatable :: formatted_text

        ! Generate ANSI codes based on style
        if (ansi_support) then
            formatted_text = create_ansi_code(style) // trim(text) // RESET
        else
            formatted_text = trim(text)
        end if

        ! Apply alignment and indentation
        call print_aligned(formatted_text, style%alignment, style%indent)
    end subroutine print_styled

    function create_ansi_code(style) result(ansi_code)
        implicit none
        type(TextStyle), intent(in) :: style
        character(len=:), allocatable :: ansi_code
    
        ansi_code = ""
    
        ! Append color codes
        select case (trim(adjustl(style%color)))
            case ("RED")
                ansi_code = ansi_code // RED
            case ("GREEN")
                ansi_code = ansi_code // GREEN
            case ("YELLOW")
                ansi_code = ansi_code // YELLOW
            case ("BLUE")
                ansi_code = ansi_code // BLUE
            case ("MAGENTA")
                ansi_code = ansi_code // MAGENTA
            case ("CYAN")
                ansi_code = ansi_code // CYAN
            case ("WHITE")
                ansi_code = ansi_code // WHITE
            case ("BLACK")
                ansi_code = ansi_code // BLACK
            case ("BRIGHT_RED")
                ansi_code = ansi_code // BRIGHT_RED
            case ("BRIGHT_YELLOW")
                ansi_code = ansi_code // BRIGHT_YELLOW
            case ("BRIGHT_BLUE")
                ansi_code = ansi_code // BRIGHT_BLUE
            case ("BRIGHT_MAGENTA")
                ansi_code = ansi_code // BRIGHT_MAGENTA
            case ("BRIGHT_CYAN")
                ansi_code = ansi_code // BRIGHT_CYAN
            case ("BRIGHT_WHITE")
                ansi_code = ansi_code // BRIGHT_WHITE
            case default
                ansi_code = ansi_code // ""
        end select
    
        ! Append background color codes only if specified
        if (trim(style%bg_color) /= "") then
            select case (trim(adjustl(style%bg_color)))
                case ("BG_BLACK")
                    ansi_code = ansi_code // BG_BLACK
                case ("BG_RED")
                    ansi_code = ansi_code // BG_RED
                case ("BG_GREEN")
                    ansi_code = ansi_code // BG_GREEN
                case ("BG_YELLOW")
                    ansi_code = ansi_code // BG_YELLOW
                case ("BG_BLUE")
                    ansi_code = ansi_code // BG_BLUE
                case ("BG_MAGENTA")
                    ansi_code = ansi_code // BG_MAGENTA
                case ("BG_CYAN")
                    ansi_code = ansi_code // BG_CYAN
                case ("BG_WHITE")
                    ansi_code = ansi_code // BG_WHITE
                case ("BG_BRIGHT_BLACK")
                    ansi_code = ansi_code // BG_BRIGHT_BLACK
                case ("BG_BRIGHT_RED")
                    ansi_code = ansi_code // BG_BRIGHT_RED
                case ("BG_BRIGHT_GREEN")
                    ansi_code = ansi_code // BG_BRIGHT_GREEN
                case ("BG_BRIGHT_YELLOW")
                    ansi_code = ansi_code // BG_BRIGHT_YELLOW
                case ("BG_BRIGHT_BLUE")
                    ansi_code = ansi_code // BG_BRIGHT_BLUE
                case ("BG_BRIGHT_MAGENTA")
                    ansi_code = ansi_code // BG_BRIGHT_MAGENTA
                case ("BG_BRIGHT_CYAN")
                    ansi_code = ansi_code // BG_BRIGHT_CYAN
                case ("BG_BRIGHT_WHITE")
                    ansi_code = ansi_code // BG_BRIGHT_WHITE
                case default
                    ansi_code = ansi_code // ""
            end select
        end if
    
        ! Append style codes
        if (style%bold) ansi_code = ansi_code // BOLD
        if (style%italic) ansi_code = ansi_code // ITALIC
        if (style%underline) ansi_code = ansi_code // UNDERLINE
    end function create_ansi_code

    subroutine print_aligned(text, alignment, indent)
        implicit none
        character(len=*), intent(in), optional :: text
        character(len=*), intent(in), optional :: alignment
        integer, intent(in) :: indent
        integer :: text_len, padding

        text_len = len_trim(text)
        padding = max(0, indent)

        select case (alignment)
            case ("left")
                write(*, '(A)') repeat(' ', padding) // trim(text)
            case ("center")
                padding = (80 - text_len) / 2 + indent
                write(*, '(A)') repeat(' ', padding) // trim(text)
            case ("right")
                padding = 80 - text_len + indent
                write(*, '(A)') repeat(' ', padding) // trim(text)
        end select
    end subroutine print_aligned

    subroutine print_bullet(text, bullet_char, indent)
        implicit none
        character(len=*), intent(in) :: text
        character(len=1), intent(in), optional :: bullet_char
        integer, intent(in), optional :: indent
        character(len=1) :: bullet
        integer :: ind

        bullet = "*"
        if (present(bullet_char)) bullet = bullet_char
        ind = 4
        if (present(indent)) ind = indent

        write(*, '(A)') repeat(' ', ind) // bullet // " " // trim(text)
    end subroutine print_bullet

    subroutine print_progress_bar(current, total)
        implicit none
        integer, intent(in) :: current, total
        integer :: progress
        character(len=50) :: bar

        progress = int((real(current) / real(total)) * 50)
        bar = repeat('=', progress) // repeat(' ', 50 - progress)
        write(*, '(A)', advance="no") char(13)
        write(*, '(A, I3, A, A, A)', advance="no") "[", progress * 2, "%] [", trim(bar), "]"
        if (current < total) call flush(6)  ! Flush output to update the progress bar
    end subroutine print_progress_bar

    ! Print matrix with single precision (real(kind=4))
    subroutine print_matrix_single(matrix, rows, cols, style, number_style, precision, use_scientific)
        implicit none
        real(kind=4), dimension(:, :), intent(in) :: matrix
        integer, intent(in) :: rows, cols
        type(TextStyle), intent(in) :: style, number_style
        integer, intent(in) :: precision
        logical, intent(in) :: use_scientific
        integer :: i, j
        character(len=30) :: element_str
        character(len=500) :: row_str
        integer :: total_width
        character(len=30) :: format_string
        character(len=3) :: precision_str
        integer, dimension(:), allocatable :: max_width_per_column
        integer :: column_width
    
        ! Detect if matrix is empty
        if (rows == 0 .or. cols == 0) then
            call print_styled("Empty matrix", style)
            return
        end if
    
        ! Convert precision to a character string for format creation
        write(precision_str, '(I0)') precision
    
        ! Determine the format string based on user specifications
        if (use_scientific) then
            column_width = precision + 8
            write(format_string, '(A, I2.2, A, I0, A)') '(E', column_width, '.', precision, ')'
        else
            column_width = precision + 5
            write(format_string, '(A, I2.2, A, I0, A)') '(F', column_width, '.', precision, ')'
        end if
    
        allocate(max_width_per_column(cols))
        max_width_per_column = 0
    
        ! Find the maximum width of any formatted matrix element for each column
        do j = 1, cols
            do i = 1, rows
                if (matrix(i, j) /= matrix(i, j)) then
                    element_str = 'NaN'
                else if (matrix(i, j) > 1.0E38_4) then
                    element_str = 'Inf'
                else if (matrix(i, j) < -1.0E38_4) then
                    element_str = '-Inf'
                else
                    write(element_str, format_string) matrix(i, j)
                end if
                max_width_per_column(j) = max(max_width_per_column(j), len_trim(adjustl(element_str)))
            end do
        end do
    
        total_width = sum(max_width_per_column) + (cols - 1) + 4
    
        call print_styled("+" // repeat("-", total_width - 2) // "+", style)
    
        do i = 1, rows
            row_str = "|"
            do j = 1, cols
                if (matrix(i, j) /= matrix(i, j)) then
                    element_str = 'NaN'
                else if (matrix(i, j) > 1.0E16_4) then
                    element_str = 'Inf'
                else if (matrix(i, j) < -1.0E16_4) then
                    element_str = '-Inf'
                else
                    write(element_str, format_string) matrix(i, j)
                end if
    
                row_str = trim(row_str) // " " // repeat(' ', max_width_per_column(j) &
                - len_trim(adjustl(element_str))) // trim(adjustl(element_str))
            end do
            row_str = trim(row_str) // " |"
            call print_styled(trim(row_str), number_style)
        end do
    
        call print_styled("+" // repeat("-", total_width - 2) // "+", style)
        call print_styled("", style)
    
        deallocate(max_width_per_column)
    end subroutine print_matrix_single

    ! Print matrix with double precision (real(kind=8))
    subroutine print_matrix_double(matrix, rows, cols, style, number_style, precision, use_scientific)
        implicit none
        real(kind=8), dimension(:, :), intent(in) :: matrix
        integer, intent(in) :: rows, cols
        type(TextStyle), intent(in) :: style, number_style
        integer, intent(in) :: precision
        logical, intent(in) :: use_scientific
        integer :: i, j
        character(len=30) :: element_str
        character(len=500) :: row_str
        integer :: total_width
        character(len=30) :: format_string
        character(len=3) :: precision_str
        integer, dimension(:), allocatable :: max_width_per_column
        integer :: column_width
    
        ! Detect if matrix is empty
        if (rows == 0 .or. cols == 0) then
            call print_styled("Empty matrix", style)
            return
        end if
    
        ! Convert precision to a character string for format creation
        write(precision_str, '(I0)') precision
    
        ! Determine the format string based on user specifications
        if (use_scientific) then
            column_width = precision + 8
            write(format_string, '(A, I2.2, A, I0, A)') '(E', column_width, '.', precision, ')'
        else
            column_width = precision + 5
            write(format_string, '(A, I2.2, A, I0, A)') '(F', column_width, '.', precision, ')'
        end if
    
        allocate(max_width_per_column(cols))
        max_width_per_column = 0
    
        ! Find the maximum width of any formatted matrix element for each column
        do j = 1, cols
            do i = 1, rows
                if (matrix(i, j) /= matrix(i, j)) then
                    element_str = 'NaN'
                else if (matrix(i, j) > 1.0E308_8) then
                    element_str = 'Inf'
                else if (matrix(i, j) < -1.0E308_8) then
                    element_str = '-Inf'
                else
                    write(element_str, format_string) matrix(i, j)
                end if
                max_width_per_column(j) = max(max_width_per_column(j), len_trim(adjustl(element_str)))
            end do
        end do
    
        total_width = sum(max_width_per_column) + (cols - 1) + 4
    
        call print_styled("+" // repeat("-", total_width - 2) // "+", style)
    
        do i = 1, rows
            row_str = "|"
            do j = 1, cols
                if (matrix(i, j) /= matrix(i, j)) then
                    element_str = 'NaN'
                else if (matrix(i, j) > 1.0E16_8) then
                    element_str = 'Inf'
                else if (matrix(i, j) < -1.0E16_8) then
                    element_str = '-Inf'
                else
                    write(element_str, format_string) matrix(i, j)
                end if
    
                row_str = trim(row_str) // " " // repeat(' ', max_width_per_column(j) &
                - len_trim(adjustl(element_str))) // trim(adjustl(element_str))
            end do
            row_str = trim(row_str) // " |"
            call print_styled(trim(row_str), number_style)
        end do
    
        call print_styled("+" // repeat("-", total_width - 2) // "+", style)
        call print_styled("", style)
    
        deallocate(max_width_per_column)
    end subroutine print_matrix_double

    ! Print matrix with uadruple precision (real(kind=16))
    subroutine print_matrix_quadruple(matrix, rows, cols, style, number_style, precision, use_scientific)
        implicit none
        real(kind=16), dimension(:, :), intent(in) :: matrix
        integer, intent(in) :: rows, cols
        type(TextStyle), intent(in) :: style, number_style
        integer, intent(in) :: precision
        logical, intent(in) :: use_scientific
        integer :: i, j
        character(len=30) :: element_str
        character(len=500) :: row_str
        integer :: total_width
        character(len=30) :: format_string
        character(len=3) :: precision_str
        integer, dimension(:), allocatable :: max_width_per_column
        integer :: column_width

        ! Detect if matrix is empty
        if (rows == 0 .or. cols == 0) then
            call print_styled("Empty matrix", style)
            return
        end if

        ! Convert precision to a character string for format creation
        write(precision_str, '(I0)') precision

        ! Determine the format string based on user specifications
        if (use_scientific) then
            column_width = precision + 8
            write(format_string, '(A, I2.2, A, I0, A)') '(E', column_width, '.', precision, ')'
        else
            column_width = precision + 5
            write(format_string, '(A, I2.2, A, I0, A)') '(F', column_width, '.', precision, ')'
        end if

        allocate(max_width_per_column(cols))
        max_width_per_column = 0

        ! Find the maximum width of any formatted matrix element for each column
        do j = 1, cols
            do i = 1, rows
                if (matrix(i, j) /= matrix(i, j)) then
                    element_str = 'NaN'
                else if (matrix(i, j) > 1.0E308_16) then
                    element_str = 'Inf'
                else if (matrix(i, j) < -1.0E308_16) then
                    element_str = '-Inf'
                else
                    write(element_str, format_string) matrix(i, j)
                end if
                max_width_per_column(j) = max(max_width_per_column(j), len_trim(adjustl(element_str)))
            end do
        end do

        total_width = sum(max_width_per_column) + (cols - 1) + 4

        call print_styled("+" // repeat("-", total_width - 2) // "+", style)

        do i = 1, rows
            row_str = "|"
            do j = 1, cols
                if (matrix(i, j) /= matrix(i, j)) then
                    element_str = 'NaN'
                else if (matrix(i, j) > 1.0E16_16) then
                    element_str = 'Inf'
                else if (matrix(i, j) < -1.0E16_16) then
                    element_str = '-Inf'
                else
                    write(element_str, format_string) matrix(i, j)
                end if

                row_str = trim(row_str) // " " // repeat(' ', max_width_per_column(j) &
                - len_trim(adjustl(element_str))) // trim(adjustl(element_str))
            end do
            row_str = trim(row_str) // " |"
            call print_styled(trim(row_str), number_style)
        end do

        call print_styled("+" // repeat("-", total_width - 2) // "+", style)
        call print_styled("", style)

        deallocate(max_width_per_column)
    end subroutine print_matrix_quadruple

    ! Print a horizontal divider with a specified style
    subroutine print_divider(style, divider_char, length)
        implicit none
        type(TextStyle), intent(in) :: style
        character(len=*), intent(in), optional :: divider_char
        integer, intent(in), optional :: length
        character(len=:), allocatable :: divider
        integer :: divider_length, char_length, repeat_count
        character(len=:), allocatable :: full_divider
    
        ! Set default values if optional arguments are not provided
        if (present(divider_char)) then
            divider = divider_char
        else
            divider = "="
        end if
    
        if (present(length)) then
            divider_length = length
        else
            divider_length = 61
        end if
    
        ! Calculate the length of the divider character and the repeat count
        char_length = len_trim(divider)
        repeat_count = ceiling(real(divider_length) / char_length)
    
        ! Create the full divider string
        full_divider = repeat(divider, repeat_count)
    
        ! Truncate to the desired length
        divider = full_divider(1:divider_length)
    
        ! Print the styled divider
        call print_styled(divider, style)

        ! Deallocate the full_divider string
        deallocate(full_divider)
    end subroutine print_divider

    subroutine create_colored_line(style, width)
        implicit none
        type(TextStyle), intent(in) :: style
        integer, intent(in), optional :: width
        integer :: w
        character(len=:), allocatable :: ansi_code
    
        ! Set default width if not provided
        if (present(width)) then
            w = width
        else
            w = 80
        end if
    
        ! Generate ANSI codes based on style and create the colored line
        if (ansi_support) then
            ansi_code = create_ansi_code(style) // repeat(' ', w) // RESET
        else
            ansi_code = repeat(' ', w)
        end if
    
        ! Print the colored line
        write(*, '(A)') ansi_code
    end subroutine create_colored_line

    subroutine create_color_block(style, height, width)
        implicit none
        type(TextStyle), intent(in) :: style
        integer, intent(in), optional :: height, width
        integer :: h_max, w_max
        integer :: h
    
        ! Set default height if not provided
        if (present(height)) then
            h_max = height
        else
            h_max = 1
        end if
    
        ! Set default width if not provided
        if (present(width)) then
            w_max = width
        else
            w_max = 80
        end if
    
        ! Loop to print the colored lines
        do h = 1, h_max
            call create_colored_line(style, w_max)
        end do
    end subroutine create_color_block
    
end module fortran_terminal_enhancer_mod

program color_demo
    use fortran_terminal_enhancer_mod
    implicit none

    ! Create a single bright red block line of default width (80)
    call create_colored_line(ERROR_BLOCK_STYLE)

    ! Create a block of green lines, 5 lines high, 60 columns wide
    call create_color_block(SUCCESS_BLOCK_STYLE, 5, 60)

    ! Create a block of blue lines, 3 lines high, default width (80)
    call create_color_block(HEADING_BLOCK_STYLE, 3)

    ! Create a single yellow block line, 100 columns wide
    call create_colored_line(WARNING_BLOCK_STYLE, 100)

    ! Create a block of cyan lines, 2 lines high, 40 columns wide
    call create_color_block(INFO_BLOCK_STYLE, 2, 40)

    ! Create a block of bright magenta lines, 4 lines high, 70 columns wide
    call create_color_block(PURPLE_BLOCK_STYLE, 4, 70)

    ! Create a white block
    call create_color_block(WHITE_BLOCK_STYLE, 100)
end program color_demo

