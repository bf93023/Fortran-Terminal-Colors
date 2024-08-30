## Fortran Terminal Colors - Enhanced with Progress Bar

This enhanced `fort_colors_mod` module provides utilities for colorful and stylish terminal output in Fortran programs, along with a new feature for displaying progress bars.

### New Features

* **Derived Type `TextStyle`:**
    * Simplifies text styling by encapsulating color, background color, bold, italic, underline, indent, and alignment options in a single structure.
    * Provides better organization and ease of use compared to passing individual parameters.

* **Enhanced Preset Styles:**
    * `TITLE_STYLE`, `SUBTITLE_STYLE`, `HEADING_STYLE`, `BODY_STYLE_CONST`, `BULLET_STYLE`, `ERROR_STYLE`, `WARNING_STYLE`, `SUCCESS_STYLE`, `QUOTE_STYLE`, `CODE_STYLE`, and `FOOTER_STYLE`.
    * Offer distinct styling for common text elements like titles, subtitles, headings, body text, code blocks, and more.
    * Enable visually appealing and informative terminal output with minimal effort.

* **Enhanced `CODE_STYLE`:**
    * Utilizes a background color and dims text for better differentiation of code blocks from other text elements.
    * Improves code readability and visual organization.

* **Alignment and Indentation:**
    * Allows left, center, and right alignment of text.
    * Supports indentation for flexible formatting.

* **Bullet Point Printing:**
    * A dedicated subroutine for printing bullet points with customizable bullet characters and indentation.

* **ANSI Support Check:**
    * A flag to check if ANSI color codes are supported by the terminal.
    * Ensures graceful degradation on terminals without ANSI support.

* **Clearer Code Structure:**
    * Functions and subroutines are better organized, promoting maintainability and readability.

* **Progress Bar:**
    * A new subroutine `print_progress_bar` to display a simple text-based progress bar.
    * Takes the current progress value and the total value as input.
    * Updates the progress bar in place on the same line, providing a visual indication of the task's completion status.

### How to Use

1. **Include the Module:**

    ```fortran
    program my_program
        use fort_colors_mod
        implicit none
        ... 
    end program my_program
    ```

2. **Initialize Colors (Optional):**

    ```fortran
    call initialize_colors(.true.)  ! Enable ANSI color support
    ```

3. **Print Styled Text:**

    ```fortran
    call print_styled("This is an error message", ERROR_STYLE)
    call print_styled("This is a warning message", WARNING_STYLE)
    call print_styled("Code block:", HEADING_STYLE)
    call print_styled("  print('Hello, world!')", CODE_STYLE)
    ```

4. **Print Bullet Points:**

    ```fortran
    call print_bullet("First item")
    call print_bullet("Second item", bullet_char="-", indent=2)
    ```

5. **Display Progress Bar:**

    ```fortran
    do i = 1, 100
        call print_progress_bar(i, 100)
        call sleep(1)  ! Simulate some work with a delay
    end do
    write(*,*)  ! Move to a new line after the progress bar
    ```

### Example

```fortran
program test_colors
    use fort_colors_mod
    implicit none

    type(TextStyle) :: body_style
    integer :: i

    ! Initialize colors
    call initialize_colors(.true.)

    ! ... (rest of the example code is the same as before)

    ! Test the progress bar
    do i = 1, 100
        call print_progress_bar(i, 100)
        call sleep(1)  ! Simulate some work with a delay
    end do
    write(*,*)  ! Move to a new line after the progress bar

    ! ... (rest of the example code is the same as before)
end program test_colors
```

### License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

### Author

This module was originally written by Benjamin Ford and has been enhanced with additional features and improvements, including the progress bar functionality.

Please note that ANSI escape codes and the progress bar feature may not be supported by all terminals. The module includes a check for ANSI support and uses carriage return (`\r`) to update the progress bar, which might behave differently on some terminals.

Feel free to contribute to this project or report any issues you encounter.

**Enjoy adding colors, styles, and progress bars to your Fortran terminal output!** 
