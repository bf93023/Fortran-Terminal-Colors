# Fortran Terminal Colors

The `fort_colors_mod` module provides utilities for handling ANSI escape codes for text formatting in terminal outputs. It includes parameters for various text and background colors, text styles, and functions to initialize and print colored text.

## Features

- Text colors (e.g., RED, GREEN, YELLOW, etc.)
- Background colors
- Text styles (e.g., BOLD, ITALIC, UNDERLINE, etc.)
- Initialize ANSI support
- Print colored text with optional column positioning

## Usage

1. Initialize colors (optional):
   ```fortran
   call initialize_colors(.true.)
   ```

2. Print colored text:
   ```fortran
   call print_colored('Hello, World!', RED)
   call print_colored('Warning!', YELLOW, 10)
   ```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Author

This module was written by Benjamin Ford.