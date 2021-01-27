       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRICE-GEN.
       AUTHOR. CARSON GERBITZ.
       DATE-WRITTEN. 3/5/2019.
      ******************************************************************
      * This program asks the user for the current date and then it 
      * reads records from a given inventory data file. Then it 
      * calculates prices for each item based on a set profit margin.
      * Then it saves the calculated prices to a new file. Once it
      * gets through every record it waits for the user to end it by 
      * pressing enter.
      *
      * Input file: inv.dat        This file contains invantory records.
      * Output file: price.dat     This file will contain price records.
      *
      * inv.dat:
      *A B                    C   D   
      * A=item number, B=item description, C=unused, D=item cost
      *
      * price.dat:
      *A B                    C   D     E    F
      * A=item number, B=item description, C=unused, D=item price,
      * E=unused, F=current date
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INV ASSIGN TO 'inv.dat'
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRICE ASSIGN TO 'price.dat'
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INV.
       01 INV-FILE.
           05 INV-ITEM-NUM                     PIC 9(2).
           05 INV-ITEM-DESC                    PIC X(20).
           05 UNUSED                           PIC X(4).
           05 INV-ITEM-COST                    PIC 9(4).
       FD PRICE.
       01 PRICE-FILE.
           05 PRICE-ITEM-NUM                   PIC 9(2).
           05 PRICE-ITEM-DESC                  PIC X(20).
           05 UNUSED                           PIC X(3).
           05 PRICE-ITEM-COST                  PIC 9(5).
           05 UNUSED                           PIC X(5).
           05 PRICE-CURRENT-DATE               PIC 9(8).
       WORKING-STORAGE SECTION.
       01 WS-INV.
           05 WS-INV-ITEM-NUM                  PIC 9(2).
           05 WS-INV-ITEM-DESC                 PIC X(20).
           05 WS-UNUSED                        PIC X(4).
           05 WS-INV-ITEM-COST                 PIC 9(4).
       01 WS-EOF                               PIC A(1).
       01 WS-PROFIT-MARGIN                     PIC 9(2) VALUE 27.

       PROCEDURE DIVISION.
      ******************************************************************
      * This section opens the two input and output files for use.
           OPEN INPUT INV.
           OPEN EXTEND PRICE.
      ******************************************************************
      ******************************************************************
      * This section gets the current date from the user and displays
      * a simple header.
      ******************************************************************
           DISPLAY "Please enter today's date:             MMDDYYYY"
           ACCEPT PRICE-CURRENT-DATE
               LINE 1 COL 28.
           DISPLAY "##Description            Cost      Today"
               LINE 2 COL 1.
           DISPLAY "-------------------------------------------"
               LINE 3 COL 1.
           DISPLAY " ".
      ******************************************************************
      * This section reads in records from the inv.dat file, moves the 
      * item number and description over to the price variables, 
      * calculates the new prices, and saves the price records to file.
      ******************************************************************
           PERFORM UNTIL WS-EOF='T'
               READ INV INTO WS-INV
               AT END MOVE 'T' TO WS-EOF
               NOT AT END MOVE WS-INV-ITEM-NUM TO PRICE-ITEM-NUM
                   MOVE WS-INV-ITEM-DESC TO PRICE-ITEM-DESC
                   COMPUTE PRICE-ITEM-COST = (100 / (100 -
                           WS-PROFIT-MARGIN)) * WS-INV-ITEM-COST
                   DISPLAY PRICE-FILE
                   WRITE PRICE-FILE
                   END-WRITE
               END-READ
           END-PERFORM
      ******************************************************************
      * This section closes the input and output files.
      ******************************************************************
           CLOSE INV.
           CLOSE PRICE.
      ******************************************************************
      * This section announces the programs completion and waits for the
      * user to terminate it.
      ******************************************************************
           DISPLAY "Prices updates! Press enter to exit program."
           ACCEPT WS-EOF.
       STOP RUN.