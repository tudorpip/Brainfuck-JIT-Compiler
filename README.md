# Brainfuck JIT Compiler

This project is an assembly-based Just-In-Time (JIT) compiler for the Brainfuck programming language. It is designed to optimize execution by transforming Brainfuck commands into bytecode before execution. The compiler employs pattern recognition and pre-calculations to enhance performance, aiming to outperform most existing benchmarks for Brainfuck execution.

## Features

- **JIT Compilation**: Converts Brainfuck commands into optimized bytecode for faster execution.
- **Pattern Recognition**: Identifies common patterns in Brainfuck code to apply optimizations.
- **Pre-Calculations**: Performs calculations ahead of time to reduce runtime overhead.
- **High Performance**: Designed with optimization techniques to beat existing benchmarks.

## Getting Started

To use this JIT compiler, follow these steps:

1. **Clone the Repository**: Copy the entire directory to your local machine.
2. **Compilation**: Navigate to the root directory of the project and run the `make` command to compile the source files. This step requires a working `make` installation.
3. **Execution**: To execute a Brainfuck program, use the following command format from the root directory:
   ./brainfuck <path_to_brainfuck_file>

For example, to run a file named `long.b`, you would use:
./brainfuck benchmarks/long.b

## Project Structure

- `main.s`: Entry point of the application. It reads a file containing Brainfuck code and passes it to the JIT compiler.
- `read_file.s`: Contains a subroutine for reading the contents of a file. Used by `main.s` to read Brainfuck code.
- `brainfuck.s`: The core of the JIT compiler. Implements the transformation of Brainfuck code into bytecode and its execution.
- `Makefile`: Contains compilation instructions for the project. Use `make` command to compile the project.

## Requirements

- GNU Make
- An x86-64 assembler (e.g., NASM, GAS)
- A Unix-like environment (Linux, macOS)

## Contributing

Contributions to improve the JIT compiler are welcome. Please feel free to fork the repository, make your changes, and submit a pull request.

## License

This project is open-source and available under the MIT License. See the LICENSE file for more details.
