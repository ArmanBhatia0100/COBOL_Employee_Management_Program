      ******************************************************************
      * Author: Arman Bhatia
      * Date: Feb 10, 2025
      * Purpose: This program takes employees' input from the user and -
      * Adds it to an external file, then displays the data in a formatted table.
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-MANAGEMENT-PROGRAM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEES-FILE ASSIGN TO "../EMPLOYEE-FILE.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEES-FILE.
       01 EMPLOYEE-INFO.
           05 EMP-ID           PIC 9(6).
           05 EMP-DEP-CODE     PIC 9(3).
           05 FIRST-NAME       PIC A(20).
           05 LAST-NAME        PIC A(20).
           05 YEAR-OF-SERVICE  PIC 99.9.


       WORKING-STORAGE SECTION.
       01 MY-FIELDS.
           05 EOF-FLAG         PIC X(1) VALUE "N".
           05 I-PROMPT         PIC A(1).
       01 I-EMP-ID             PIC 9(6).
       01 I-EMP-DEP-CODE       PIC 9(3).
       01 I-FIRST-NAME         PIC A(20).
       01 I-LAST-NAME          PIC A(20).
       01 I-YEAR-OF-SERVICE    PIC 99.9.
       01 HEADER-LINE          PIC X(80).
       01 SEPARATOR-LINE       PIC X(80) VALUE ALL "-".

       PROCEDURE DIVISION.
       100-PRODUCE-EMPLOYEE-RECORDS.
           PERFORM 101-INITIALIZE-OUTPUT-FILE.
           PERFORM 102-GET-INPUT-FROM-USER UNTIL EOF-FLAG = "Y".
           PERFORM 103-CLOSE-OUTPUT-FILE.

           PERFORM 201-INITIALIZE-INPUT-FILE.
           PERFORM 210-PRINT-HEADER.
           PERFORM 202-GET-RECORD-FROM-FILE UNTIL EOF-FLAG = "Y".
           PERFORM 203-CLOSE-INPUT-FILE.
           STOP RUN.


      *This will INITIALISE output file for send the output to it.
       101-INITIALIZE-OUTPUT-FILE.
           OPEN OUTPUT EMPLOYEES-FILE.
           MOVE "N" TO EOF-FLAG.

      * Getting INFO from the user.
       102-GET-INPUT-FROM-USER.
           DISPLAY "WHAT IS THE EMPLOYEE-ID? ex(123456)".
           ACCEPT I-EMP-ID.
           MOVE I-EMP-ID TO EMP-ID.

           DISPLAY "WHAT IS DEP-CODE? ex(123)".
           ACCEPT I-EMP-DEP-CODE.
           MOVE I-EMP-DEP-CODE TO EMP-DEP-CODE.

           DISPLAY "WHAT IS THE FIRST NAME?".
           ACCEPT I-FIRST-NAME.
           MOVE I-FIRST-NAME TO FIRST-NAME.

           DISPLAY "WHAT IS THE LAST NAME?".
           ACCEPT I-LAST-NAME.
           MOVE I-LAST-NAME TO LAST-NAME.

           DISPLAY "WHAT IS THE YEAR OF SERVICE? (ex: 20.5)".
           ACCEPT I-YEAR-OF-SERVICE.
           MOVE I-YEAR-OF-SERVICE TO YEAR-OF-SERVICE.

           DISPLAY "DO YOU WANT TO CONTINUE? Y/N".
           ACCEPT I-PROMPT.
           WRITE EMPLOYEE-INFO.

           IF I-PROMPT = "N" MOVE "Y" TO EOF-FLAG.


      * Closing outfile after sending output to the file.
       103-CLOSE-OUTPUT-FILE.
           CLOSE EMPLOYEES-FILE.
           MOVE "N" TO EOF-FLAG.

      * INITIALISING Employee-File file for getting the input.
       201-INITIALIZE-INPUT-FILE.
           OPEN INPUT EMPLOYEES-FILE.
           MOVE "N" TO EOF-FLAG.

      *This will read record from the file and output it to the screen.
       202-GET-RECORD-FROM-FILE.
           READ EMPLOYEES-FILE AT END MOVE "Y" TO EOF-FLAG.
           IF EOF-FLAG NOT = "Y" THEN
               IF YEAR-OF-SERVICE >= 10.5
                   DISPLAY EMP-ID " "EMP-DEP-CODE "  "
                   FIRST-NAME LAST-NAME
                   YEAR-OF-SERVICE
           END-IF.
      * Closing input file after processing.
       203-CLOSE-INPUT-FILE.
           CLOSE EMPLOYEES-FILE.

      * This function will format the out.
       210-PRINT-HEADER.
           MOVE FUNCTION CONCATENATE("EMP-ID DEPT FIRST-NAME","       ",
           "   LAST-NAME           ","SERVICE-YEAR") TO HEADER-LINE

           DISPLAY HEADER-LINE
           DISPLAY SEPARATOR-LINE.

       END PROGRAM EMPLOYEE-MANAGEMENT-PROGRAM.
