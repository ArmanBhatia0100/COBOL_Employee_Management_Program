# Employee Management Program

## Overview
This COBOL program, `EMPLOYEE-MANAGEMENT-PROGRAM`, allows users to input employee data, store it in an external file, and then display a formatted table of employees with 10.5 or more years of service. It’s a simple demonstration of file I/O and data processing in COBOL.

## Features
- Collects employee information interactively from the user:
  - Employee ID (6 digits)
  - Department Code (3 digits)
  - First Name (up to 20 characters)
  - Last Name (up to 20 characters)
  - Years of Service (e.g., 20.5)
- Writes the data to `EMPLOYEE-FILE.txt`.
- Reads the file and displays a formatted table of employees with 10.5+ years of service.
- Includes a header and separator line for the output table.

## Prerequisites
- A COBOL compiler (e.g., GnuCOBOL or IBM Enterprise COBOL).
- A compatible environment to run COBOL programs (e.g., Linux, Windows with GnuCOBOL, or a mainframe).
- Write permissions in the parent directory for creating `EMPLOYEE-FILE.txt`.

## File Structure
### Output/Input File
- **`EMPLOYEE-FILE.txt`**  
  - Format: Line-sequential  
  - Record Structure:
    - Employee ID (6 digits)
    - Department Code (3 digits)
    - First Name (20 chars, alphabetic)
    - Last Name (20 chars, alphabetic)
    - Years of Service (3 digits, 2 before decimal, 1 after, e.g., 20.5)  
  - Example:
    ```
    123456123John                 Smith                 015
    789123456Jane                 Doe                   012
    ```

## Installation
1. Clone the repository:
   ```bash
   git clone https://github.com/ArmanBhatia0100/employee-management-program.git
   ```
2. Ensure the program has access to the parent directory (or adjust the file path in the `FILE-CONTROL` section).
3. Compile the COBOL program:
   ```bash
   cobc -x EMPLOYEE-MANAGEMENT-PROGRAM.cbl
   ```

## Usage
1. Run the compiled program:
   ```bash
   ./EMPLOYEE-MANAGEMENT-PROGRAM
   ```
2. Follow the prompts to enter employee data:
   - Input each field as requested (e.g., Employee ID, Department Code, etc.).
   - Choose "Y" to continue adding employees or "N" to finish.
3. After input is complete, the program will display a table of employees with 10.5+ years of service.

### Sample Interaction
```
WHAT IS THE EMPLOYEE-ID? ex(123456)
123456
WHAT IS DEP-CODE? ex(123)
123
WHAT IS THE FIRST NAME?
John
WHAT IS THE LAST NAME?
Smith
WHAT IS THE YEAR OF SERVICE? (ex: 20.5)
15.5
DO YOU WANT TO CONTINUE? Y/N
N
```
**Output:**
```
EMP-ID DEPT FIRST-NAME           LAST-NAME            SERVICE-YEAR
--------------------------------------------------------------------------------
123456 123  JohnSmith15.5
```

## Program Structure
- **IDENTIFICATION DIVISION**: Defines the program name and metadata (author: Arman Bhatia, date: Feb 10, 2025).
- **ENVIRONMENT DIVISION**: Configures the employee file.
- **DATA DIVISION**: Defines file structure and working storage variables.
- **PROCEDURE DIVISION**: Main logic:
  - `101-INITIALIZE-OUTPUT-FILE`: Opens the file for writing.
  - `102-GET-INPUT-FROM-USER`: Collects and writes employee data.
  - `103-CLOSE-OUTPUT-FILE`: Closes the output file.
  - `201-INITIALIZE-INPUT-FILE`: Opens the file for reading.
  - `210-PRINT-HEADER`: Displays the table header.
  - `202-GET-RECORD-FROM-FILE`: Reads and displays qualifying records.
  - `203-CLOSE-INPUT-FILE`: Closes the input file.

## Limitations
- Displays only employees with 10.5+ years of service.
- No input validation (e.g., accepts invalid formats).
- Output formatting is basic and lacks consistent spacing.
- Overwrites `EMPLOYEE-FILE.txt` each run (no append mode).

## Contributing
Feel free to fork this repository and submit pull requests for enhancements, such as:
- Adding input validation for numeric and alphabetic fields.
- Improving table formatting with better spacing.
- Supporting file append instead of overwrite.
- Including all employees in the output with a filter option.

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contact
For questions or suggestions, open an issue or contact [arman.bhatia.1407@gmail.com].
