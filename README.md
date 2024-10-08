# Fortran Terminal Enhancer

`Fortran Terminal Enhancer` is a powerful Fortran module designed to improve terminal output by adding colors, styles, and progress bars using ANSI escape codes. This module provides utilities to format text with various colors, styles, alignments, and visual effects, making command-line outputs more visually appealing and easier to interpret. It also includes features for displaying progress bars and formatted matrices, supporting both single and double precision.

## Table of Contents

- [Features](#features)
- [Installation](#installation)
- [Getting Started](#getting-started)
- [Usage](#usage)
  - [Initialize Colors](#initialize-colors)
  - [Print Styled Text](#print-styled-text)
  - [Print Bullet Points](#print-bullet-points)
  - [Display Progress Bar](#display-progress-bar)
  - [Print Matrices](#print-matrices)
  - [Create Custom Styles](#create-custom-styles)
- [Example Programs](#example-programs)
  - [Simple Example](#simple-example)
  - [Advanced Example](#advanced-example)
  - [Matrix Example](#matrix-example)
- [Compatibility and Limitations](#compatibility-and-limitations)
- [License](#license)
- [Author](#author)
- [Contributing](#contributing)

## Features

- **Text Styling with ANSI Escape Codes**: Print text in various colors and styles (bold, italic, underline) using ANSI escape codes.
- **Background Colors**: Set background colors for text to enhance readability and emphasize important output.
- **Derived Type for Text Styling**: Use the `TextStyle` derived type to simplify styling and maintain consistency.
- **Preset Styles**: Several predefined styles like `TITLE_STYLE`, `ERROR_STYLE`, `CODE_STYLE`, etc., for common output needs.
- **Text Alignment and Indentation**: Supports left, center, and right alignment with customizable indentation.
- **Bullet Point Printing**: Print bullet points with customizable characters and indentation.
- **Progress Bar**: A simple, text-based progress bar to show task progress in the terminal.
- **Matrix Printing**: Display matrices in a formatted way, with support for different numerical notations (scientific or fixed) and precision.
- **ANSI Support Check**: Option to check if ANSI codes are supported by the terminal and disable them if necessary.
- **Enhanced Output**: Additional styles for informational, debugging, input prompts, highlighting, links, and more.

## Installation

To use the `Terminal Enhancer for Fortran` module, include the source file (`fortran_terminal_enhancer_mod.f90`) in your Fortran project. You can compile your program along with the module using any Fortran compiler.

### Example Compilation

```bash
gfortran -o my_program fortran_terminal_enhancer_mod.f90 my_program.f90
```

Replace `my_program.f90` with the name of your Fortran source file.

## Getting Started

To start using `Terminal Enhancer for Fortran`, follow these steps:

1. **Include the Module in Your Program**: Use the `use fortran_terminal_enhancer_mod` statement to include the module.
2. **Initialize ANSI Color Support**: Call `initialize_colors()` to enable or disable ANSI color support based on terminal compatibility.
3. **Use Styling and Utility Subroutines**: Utilize the various subroutines provided to print styled text, bullet points, matrices, and progress bars.

## Usage

### Initialize Colors

Initialize ANSI color support to ensure your terminal supports it. This step is optional but recommended for ensuring compatibility.

```fortran
call initialize_colors(.true.)  ! Enable ANSI color support
```

### Print Styled Text

Use the `print_styled` subroutine to print text with a specific style. You can use predefined styles or define your own.

```fortran
call print_styled("Error: Unable to locate file!", ERROR_STYLE)
call print_styled("Welcome to the Program", TITLE_STYLE)
```

### Print Bullet Points

Print bullet points with customizable indentation and bullet characters using `print_bullet`.

```fortran
call print_bullet("First item")
call print_bullet("Second item", bullet_char="-", indent=2)
```

### Display Progress Bar

Show a progress bar to indicate task progress using `print_progress_bar`.

```fortran
do i = 1, 100
    call print_progress_bar(i, 100)
    call sleep(1)  ! Simulate work
end do
write(*,*)  ! Move to a new line after the progress bar
```

### Print Matrices

Display matrices in a formatted manner, with support for scientific and normal notation, and control over precision. The module supports both single and double precision matrices.

```fortran
real, dimension(3, 3) :: matrix = reshape([1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9], [3, 3])
call print_matrix(matrix, 3, 3, BODY_STYLE_CONST, SUCCESS_STYLE, 2, .false.)
```

### Create Custom Styles

You can create your own text styles by defining a new `TextStyle` type. This allows you to customize text color, background color, text attributes (bold, italic, underline), alignment, and indentation. 

Here’s how to create and use a custom style:

```fortran
type(TextStyle) :: custom_style

! Define a custom style
custom_style%color = "BRIGHT_GREEN"
custom_style%bg_color = "BG_BLACK"
custom_style%bold = .true.
custom_style%italic = .false.
custom_style%underline = .true.
custom_style%alignment = "center"
custom_style%indent = 2

! Use the custom style
call print_styled("This is a custom styled text!", custom_style)
```

**Explanation:**
- You can modify any attribute of the `TextStyle` derived type to fit your needs.
- The `color` and `bg_color` fields accept the predefined color constants like `"BRIGHT_GREEN"`, `"BG_BLACK"`, etc.
- The `bold`, `italic`, and `underline` fields are logical values that enable or disable the corresponding text styles.
- The `alignment` field can be `"left"`, `"center"`, or `"right"` to align the text accordingly.
- The `indent` field specifies the number of spaces to indent the text.

## Example Programs

### Simple Example

Here's a simple example to get started with `Terminal Enhancer for Fortran`. This example covers basic usage of styled text and progress bars.

```fortran
program simple_example
    use fortran_terminal_enhancer_mod, only: initialize_colors, print_styled, print_progress_bar, SUCCESS_STYLE, ERROR_STYLE, INFO_STYLE
    implicit none

    integer :: i

    ! Initialize ANSI color support
    call initialize_colors(.true.)

    ! Print some styled text
    call print_styled("Starting simple example...", INFO_STYLE)
    call print_styled("Everything is working fine.", SUCCESS_STYLE)
    call print_styled("An error occurred!", ERROR_STYLE)

    ! Display a progress bar
    do i = 1, 100
        call print_progress_bar(i, 100)
        call sleep(1)  ! Simulate work
    end do
    write(*,*)  ! Move to a new line after the progress bar

    ! End of simple example
    call print_styled("Simple example completed.", SUCCESS_STYLE)
end program simple_example
```

### Advanced Example

After familiarizing yourself with the basics, here is a more advanced example demonstrating additional features, such as matrix printing, bullet points, and handling different text styles.

```fortran
program test_colors
    use fortran_terminal_enhancer_mod, only: TextStyle, initialize_colors, print_matrix, print_styled, print_bullet, print_progress_bar, &
                               BODY_STYLE_CONST, SUCCESS_STYLE, ERROR_STYLE, HEADING_STYLE, TITLE_STYLE, &
                               FOOTER_STYLE, CODE_STYLE, WARNING_STYLE, INFO_STYLE, QUOTE_STYLE, DEBUG_STYLE, &
                               INPUT_PROMPT_STYLE, HIGHLIGHT_STYLE, LINK_STYLE, TITLE_HIGHLIGHT_STYLE, &
                               DISABLED_TEXT_STYLE, SUBHEADING_STYLE, SUBTITLE_STYLE
    implicit none

    type(TextStyle) :: border_style, number_style
    real, dimension(3, 3) :: matrix
    integer :: i, precision
    logical :: use_scientific

    ! Initialize colors
    call initialize_colors(.true.)

    ! Initialize matrix
    matrix = reshape([1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9], [3, 3])

    ! Print Title
    call print_styled("=== Welcome to the Terminal Enhancer Utility ===", TITLE_STYLE)
    call print_styled("Version 1.0.0", SUBTITLE_STYLE)
    call print_styled("Initializing components...", BODY_STYLE_CONST)

    ! Simulate loading configurations
    call print_styled("Loading configuration file: config.cfg", INFO_STYLE)
    call print_styled("Configuration loaded successfully.", SUCCESS_STYLE)

    ! Simulate checking system status
    call print_styled("Checking system status...", INFO_STYLE)
    call print_styled("Warning: Low disk space detected on drive C:.", WARNING_STYLE)
    call print_styled("Error: Unable to locate the backup drive.", ERROR_STYLE)

    ! Display a motivational quote for the user
    call print_styled("Tip of the Day: The only way to do great work is to love what you do. - Steve Jobs", QUOTE_STYLE)

    ! Simulate debugging information
    call print_styled("Debug: Entering the main processing loop...", DEBUG_STYLE)

    ! Simulate code output section
    call print_styled("Generating report...", BODY_STYLE_CONST)
    call print_styled("def generate_report():", CODE_STYLE)
    call print_styled("    print('Generating data...')", CODE_STYLE)
    call print_styled("    # Placeholder for actual report generation logic", CODE_STYLE)

    ! Simulate user input prompt
    call print_styled("Please enter the output directory: ", INPUT_PROMPT_STYLE)

    ! Print Matrix
    precision = 2
    use_scientific = .false.
    call print_styled("=== Matrix Example ===", HEADING_STYLE)
    call print_matrix(matrix, 3, 3, BODY_STYLE_CONST, SUCCESS_STYLE, precision, use_scientific)

    ! Displaying report headings and details
    call print_styled("=== Report Summary ===", HEADING_STYLE)
    call print_styled("Task Name: Data Analysis", BODY_STYLE_CONST)
    call print_styled("Status: Completed", SUCCESS_STYLE)
    call print_styled("Details:", BODY_STYLE_CONST)
    call print_bullet("Processed 1000 records", "*", 4)
    call print_bullet("Found 3 anomalies", "*", 4)

    ! Displaying additional notes or links
    call print_styled("For more information, visit the documentation at: ", BODY_STYLE_CONST)
    call print_styled("https://example.com/docs", LINK_STYLE)

    ! Highlight important information
    call print_styled("Important: Ensure all dependencies are installed before running.", HIGHLIGHT_STYLE)

    ! Simulate progress of a long-running operation
    call print_styled("Starting data cleanup operation...", BODY_STYLE_CONST)
    do i = 1, 100
        call print_progress_bar(i, 100)
        call sleep(1)  ! Simulate some work with a delay
    end do
    write(*,*)  ! Move to a new line after the progress bar
    call print_styled("Data cleanup operation completed successfully.", SUCCESS_STYLE)

    ! Display a title with background highlight
    call print_styled("=== Summary of Operations ===", TITLE_HIGHLIGHT_STYLE)

    ! Simulate displaying a disabled option
    call print_styled("Feature X: Disabled due to compatibility issues.", DISABLED_TEXT_STYLE)

    ! Simulate a subheading for additional section
    call print_styled("=== Next Steps ===", SUBHEADING_STYLE)
    call print_styled("1. Review the generated report for accuracy.", BODY_STYLE_CONST)
    call print_styled("2. Address any anomalies found during analysis.", BODY_STYLE_CONST)
    call print_styled("3. Backup the system and prepare for the next operation.", BODY_STYLE_CONST)

    ! Simulate footer or ending message
    call print_styled("Thank you for using Terminal Enhancer Utility!", FOOTER_STYLE)
    call print_styled("=== End of Program ===", TITLE_STYLE)
end program test_colors
```

### Matrix Example

This example demonstrates the matrix printing capabilities of this module in more detail, specifically showcasing the different data types supported and the ability to accept formatting.

```fortran
program matrix_printing_demo
    use fortran_terminal_enhancer_mod
    implicit none

    ! Declare matrices
    real(kind=4), dimension(2, 2) :: matrix_single
    real(kind=8), dimension(2, 2) :: matrix_double
    real(kind=16), dimension(2, 2) :: matrix_quadruple
    integer :: precision
    logical :: use_scientific

    ! Initialize ANSI color support
    call initialize_colors(.true.)

    ! Initialize matrices with sample values
    matrix_single = reshape([1.23_4, 4.56_4, 7.89_4, 0.12_4], [2, 2])
    matrix_double = reshape([1.234567_8, 8.9101112_8, 3.1415926_8, 2.7182818_8], [2, 2])  
    matrix_quadruple = reshape([1.234567890123456_16, 8.910111213141516_16, &
        3.141592653589793_16, 2.718281828459045_16], [2, 2])

    ! Print title
    call print_styled("=== Matrix Printing Demo ===", TITLE_STYLE)

    ! Example 1: Single precision matrix in fixed-point notation with BODY_STYLE_CONST
    precision = 4
    use_scientific = .false.
    call print_styled("Single Precision Matrix (Fixed-Point, Precision: 2) with BODY_STYLE_CONST:", CODE_STYLE)
    call print_matrix(matrix_single, 2, 2, BODY_STYLE_CONST, BODY_STYLE_CONST, precision, use_scientific)

    ! Example 2: Double precision matrix in fixed-point notation with SUCCESS_STYLE
    precision = 4
    use_scientific = .false.
    call print_styled("Double Precision Matrix (Fixed-Point, Precision: 4) with SUCCESS_STYLE:", CODE_STYLE)
    call print_matrix(matrix_double, 2, 2, SUCCESS_STYLE, SUCCESS_STYLE, precision, use_scientific)

    ! Example 3: Single precision matrix in scientific notation with WARNING_STYLE
    precision = 2
    use_scientific = .true.
    call print_styled("Single Precision Matrix (Scientific Notation, Precision: 2) with WARNING_STYLE:", CODE_STYLE)
    call print_matrix(matrix_single, 2, 2, WARNING_STYLE, WARNING_STYLE, precision, use_scientific)

    ! Example 4: Double precision matrix in scientific notation with ERROR_STYLE
    precision = 6
    use_scientific = .true.
    call print_styled("Double Precision Matrix (Scientific Notation, Precision: 6) with ERROR_STYLE:", CODE_STYLE)
    call print_matrix(matrix_double, 2, 2, ERROR_STYLE, ERROR_STYLE, precision, use_scientific)

    ! Example 5: Quadruple precision matrix in scientific notation with INFO_STYLE
    precision = 10
    use_scientific = .true.
    call print_styled("Quadruple Precision Matrix (Scientific Notation, Precision: 10) with INFO_STYLE:", CODE_STYLE)
    call print_matrix(matrix_quadruple, 2, 2, INFO_STYLE, INFO_STYLE, precision, use_scientific)

end program matrix_printing_demo
```

## Compatibility and Limitations

- **ANSI Escape Codes**: This module relies on ANSI escape codes for text styling, which may not be supported by all terminals. The module includes a check (`ansi_support` flag) to enable or disable ANSI colors based on terminal compatibility.
- **Progress Bar**: The progress bar uses carriage return (`\r`) to update the output in place. This may not work as expected in some terminal environments or text editors.
- **Terminal Width**: The `print_aligned` subroutine assumes a terminal width of 80 characters for centering and right-aligning text. Adjust the code if a different terminal width is required.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Author

The `Terminal Enhancer for Fortran` module is written by Benjamin Ford, with enhancements for additional styling options and progress bar functionality.

## Contributing

Contributions are welcome! Feel free to submit issues or pull requests to improve the module.