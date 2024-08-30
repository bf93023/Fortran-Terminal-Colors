## Fortran Terminal Colors - Enhanced

The `fort_colors_mod` module is an enhanced version of the original, offering more robust functionalities for colorful and stylish terminal output in Fortran programs.

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

### Example

```fortran
program test_colors
    use fort_colors_mod
    implicit none

    type(TextStyle) :: body_style

    ! Initialize colors
    call initialize_colors(.true.)

    ! Test new preset styles
    call print_styled("Error: File not found!", ERROR_STYLE)
    call print_styled("Warning: Low disk space.", WARNING_STYLE)
    call print_styled("Success: Operation completed successfully.", SUCCESS_STYLE)
    call print_styled("The only way to do great work is to love what you do. - Steve Jobs", QUOTE_STYLE)
    call print_styled("def example_function():", CODE_STYLE)
    call print_styled("    print('Hello, World!')", CODE_STYLE)
    call print_styled("Footer: Thank you for using our software!", FOOTER_STYLE)

    ! Create a modifiable copy of BODY_STYLE for alignment changes
    body_style = BODY_STYLE_CONST

    ! Test alignment with body style
    call print_styled("Left aligned text", body_style)

    body_style%alignment = "center"
    call print_styled("Center aligned text", body_style)

    body_style%alignment = "right"
    call print_styled("Right aligned text", body_style)
end program test_colors
```

### License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

### Author

This module was originally written by Benjamin Ford and has been enhanced with additional features and improvements.

Please note that ANSI escape codes may not be supported by all terminals. The module includes a check for ANSI support to ensure graceful degradation on incompatible terminals.

Feel free to contribute to this project or report any issues you encounter.

**Enjoy adding colors and styles to your Fortran terminal output!** 
