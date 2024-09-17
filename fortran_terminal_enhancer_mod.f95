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
        bg_color="", bold=.true., underline=.true., alignment="center")

    type(TextStyle), parameter :: SUBTITLE_STYLE = TextStyle(color="BRIGHT_MAGENTA", &
        bg_color="", bold=.true., alignment="center")

    type(TextStyle), parameter :: HEADING_STYLE = TextStyle(color="BRIGHT_GREEN", &
        bg_color="", bold=.true., alignment="left")

    type(TextStyle), parameter :: BODY_STYLE_CONST = TextStyle(color="WHITE", &
        bg_color="", alignment="left")

    type(TextStyle), parameter :: BULLET_STYLE = TextStyle(color="CYAN", &
        bg_color="", alignment="left", indent=4)

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
    ! Style used for plotting
    type(TextStyle), parameter :: PLOT_STYLE = TextStyle(color="BRIGHT_WHITE", &
        bg_color="", bold=.false., alignment="left")
 
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

    ! Plot Terminal
    subroutine plot_terminal(x, y, style, x_label, y_label)
        implicit none
        real, intent(in) :: x(:), y(:)
        type(TextStyle), intent(in), optional :: style
        character(len=*), intent(in), optional :: x_label, y_label
    
        ! Local variables
        integer :: n_points, i, j
        real :: x_min, x_max, y_min, y_max, y_value, x_value
        integer, parameter :: plot_width = 70
        integer, parameter :: plot_height = 20
        integer, parameter :: label_width = 10
        character(len=plot_width) :: grid(plot_height)
        character(len=plot_width + label_width) :: grid_line
        character(len=label_width - 2) :: y_label_string
        character(len=plot_width + label_width) :: x_axis_line
        integer :: grid_x, grid_y
        type(TextStyle) :: plot_style
        integer :: x_label_pos, num_x_ticks
        character(len=:), allocatable :: x_label_padded, y_label_padded
        real, allocatable :: x_ticks(:)
        character(len=10) :: x_tick_label
        integer :: tick_positions(plot_width)
        integer :: tick_length
    
        ! Initialize plot_style
        if (present(style)) then
            plot_style = style
        else
            plot_style = PLOT_STYLE
        end if
    
        ! Check that x and y arrays have the same size
        n_points = size(x)
        if (size(y) /= n_points) then
            call print_styled("Error: x and y arrays must have the same size.", ERROR_STYLE)
            return
        end if
    
        ! Determine x_min, x_max, y_min, y_max
        x_min = minval(x)
        x_max = maxval(x)
        y_min = minval(y)
        y_max = maxval(y)
    
        ! Handle the case where x_max == x_min or y_max == y_min
        if (x_max == x_min) then
            x_min = x_min - 1.0
            x_max = x_max + 1.0
        end if
        if (y_max == y_min) then
            y_min = y_min - 1.0
            y_max = y_max + 1.0
        end if
    
        ! Initialize grid
        grid = repeat(' ', plot_width)
    
        ! Plot data points onto the grid
        do i = 1, n_points
            grid_x = int( ( (x(i) - x_min) / (x_max - x_min) ) * (plot_width - 1) ) + 1
            grid_y = int( ( (y(i) - y_min) / (y_max - y_min) ) * (plot_height - 1) ) + 1
            grid_y = plot_height - grid_y + 1  ! Invert y-axis for terminal output
    
            ! Ensure grid_x and grid_y are within bounds
            grid_x = max(1, min(grid_x, plot_width))
            grid_y = max(1, min(grid_y, plot_height))
    
            ! Set the grid point to '*'
            grid(grid_y)(grid_x:grid_x) = '*'
        end do
    
        ! Print y-axis label if provided
        if (present(y_label)) then
            ! Since vertical text is impractical in the terminal, print the y-label above the plot
            y_label_padded = adjustl(y_label)
            call print_styled(' ' // trim(y_label_padded), plot_style)
        end if
    
        ! Output the grid with y-axis labels
        do grid_y = 1, plot_height
            ! Calculate y_value
            y_value = y_max - ( (grid_y - 1) / real(plot_height - 1) ) * (y_max - y_min)
            write(y_label_string, '(F8.2)') y_value
    
            ! Construct the grid line with the y-axis label
            grid_line = ''
            grid_line(1:label_width - 2) = adjustl(y_label_string)
            grid_line(label_width - 1:label_width) = ' |'
            grid_line((label_width + 1):(label_width + plot_width)) = grid(grid_y)
            call print_styled(trim(grid_line), plot_style)
        end do
    
        ! Print x-axis line
        x_axis_line = repeat(' ', label_width - 2) // ' +-' // repeat('-', plot_width)
        call print_styled(x_axis_line, plot_style)
    
        ! Determine the number of x-axis ticks (avoiding overlap)
        num_x_ticks = int(plot_width / 15)
        num_x_ticks = max(3, num_x_ticks)  ! Ensure at least 3 ticks (start, middle, end)
    
        allocate(x_ticks(num_x_ticks))
        do i = 1, num_x_ticks
            x_ticks(i) = x_min + (i - 1) * (x_max - x_min) / (num_x_ticks - 1)
        end do
    
        ! Initialize tick positions
        tick_positions = 0
    
        ! Prepare x-axis labels
        x_axis_line = repeat(' ', label_width - 2) // '  ' // repeat(' ', plot_width)
        do i = 1, num_x_ticks
            grid_x = int( ( (x_ticks(i) - x_min) / (x_max - x_min) ) * (plot_width - 1) ) + 1
            grid_x = max(1, min(grid_x, plot_width))
            tick_positions(grid_x) = 1
            write(x_tick_label, '(F7.2)') x_ticks(i)
            tick_length = len_trim(adjustl(x_tick_label))
            ! Ensure the label fits within the plot width
            if (label_width + grid_x + tick_length - 1 <= label_width + plot_width) then
                x_axis_line(label_width + grid_x : label_width + grid_x + tick_length - 1) = adjustl(x_tick_label)
            end if
        end do
    
        ! Print x-axis labels
        call print_styled(x_axis_line, plot_style)
    
        ! Print x-axis label if provided
        if (present(x_label)) then
            ! Center the x_label under the plot
            x_label_padded = adjustl(x_label)
            x_label_pos = label_width + (plot_width - len_trim(x_label_padded)) / 2
            grid_line = repeat(' ', x_label_pos) // trim(x_label_padded)
            call print_styled(grid_line, plot_style)
        end if
    
        deallocate(x_ticks)
    
    end subroutine plot_terminal
      
    
end module fortran_terminal_enhancer_mod


program test_plot
    use fortran_terminal_enhancer_mod
    implicit none

    real, allocatable :: x(:), y(:)
    integer :: n, i

    ! Initialize your data
    n = 100
    allocate(x(n), y(n))
    x = [(i, i = 1, n)] / 10.0
    y = sin(x)

    ! Plot the data with labels
    call plot_terminal(x, y, x_label="X-axis (Time)", y_label="Y-axis (Amplitude)")
end program test_plot

