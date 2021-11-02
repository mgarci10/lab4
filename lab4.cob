        IDENTIFICATION DIVISION.
        PROGRAM-ID. LAB4.

        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
                SELECT COURSE-FILE ASSIGN TO 'DA-S-COURSE' ORGANIZATION
                        IS LINE SEQUENTIAL.
           SELECT PRINT-FILE ASSIGN TO 'UR-S-PRINT' ORGANIZATION IS LINE
                   SEQUENTIAL.

        DATA DIVISION.
        FILE SECTION.
        FD COURSE-FILE.
        01 COURSE-DATA.
        88 END-OF-DATA VALUE HIGH-VALUES.
        02 C-COURSE.
        03 C-ABB PIC XXX.
        03 C-NUMB PIC XXXX.
        03 C-SEC PIC XXX.
        03 C-TITLE PIC X(20).
        03 C-SEATS-REMAINING PIC S999.
        03 C-CLASSLIMIT PIC 999.
        03 FILLER PIC XXX.
        02 C-STARTING-TIME.
        03 C-STARTING-HOUR PIC 99.
        03 C-STARTING-MIN PIC 99.
        03 FILLER PIC XX.
        02 C-DAYS.
        03 C-MON PIC X.
        03 C-TUE PIC X.
        03 C-WED PIC X.
        03 C-THU PIC X.
        03 C-FRI PIC X.
        03 FILLER PIC X.
        02 C-LOCATION.
        03 C-BUILDING PIC XX.
        03 C-ROOM PIC XXX.
        03 FILLER PIC X(24).
      *RECORDING MODE IS F
      *LABEL RECORDS ARE STANDARD.

      *01 EMP-REC PIC X(80).

       FD PRINT-FILE.
      *RECORDING MODE IS F
      *LABEL RECORDS ARE STANDARD.
        01 PRINT-REC PIC X(180).

      * WORKING STORAGE
        WORKING-STORAGE SECTION.
        01 MISC.
        03 EOF PIC X VALUE 'N'.
      *88 END-OF-DATA VALUE 'Y'.
      *          88 END-OF-DATA VALUE HIGH-VALUES.
        03 LINE-CT PIC 99 VALUE 0.
        03 CTAKEN PIC 999.
        03 CTOTAL-LIMIT PIC 999 VALUE 0.
        03 CTOTAL-REMAINING PIC 999 VALUE 0.
        03 CTOTAL-TAKEN PIC 999 VALUE 9.
      ******************************************************************
      * DESCRIPTION OF INPUT DATA LAYOUT ***
      ******************************************************************

      ******************************************************************
      *** DESCRIPTION OF HEADING PRINT LINES *** ***
      ******************************************************************
        01 HEADING1.
        03 FILLER PIC X(10) VALUE SPACES.
        03 FILLER PIC X(5) VALUE 'CLASS'.
        03 FILLER PIC X(11) VALUE SPACES.
        03 FILLER PIC X(8) VALUE 'LOCATION'.
        03 FILLER PIC X(8) VALUE SPACES.
        03 FILLER PIC X(4) VALUE 'DAYS'.
        03 FILLER PIC X(11) VALUE SPACES.
        03 FILLER PIC X(4) VALUE 'TIME'.
        03 FILLER PIC X(10) VALUE SPACES.
        03 FILLER PIC X(5) VALUE 'CLASS'.
        03 FILLER PIC X(7) VALUE SPACES.
        03 FILLER PIC XXXX VALUE 'OPEN'.
        01 HEADING2.
        03 FILLER PIC X(71) VALUE SPACES.
        03 FILLER PIC X(5) VALUE 'LIMIT'.
        03 FILLER PIC X(7) VALUE SPACES.
        03 FILLER PIC X(5) VALUE 'SEATS'.
        03 FILLER PIC X(7) VALUE SPACES.
        03 FILLER PIC X(5) VALUE 'TAKEN'.
      ******************************************************************
      * DESCRIPTION OF PRINT DATA LAYOUT ***
      ******************************************************************
        01 PRINT-DATA.
        03 FILLER PIC X(10) VALUE SPACES.
        03 PABB PIC XXX.
        03 FILLER PIC X VALUE SPACES.
        03 PNUMB PIC XXXX.
        03 FILLER PIC X VALUE SPACES.
        03 PSEC PIC XXX.
        03 FILLER PIC X(5) VALUE SPACES.
        03 PBUILDING PIC XX.
        03 FILLER PIC X VALUE SPACES.
        03 PROOM PIC z99.
        03 FILLER PIC X(9) VALUE SPACES.
      *  03 PDAYS.
        03 PMON PIC X.
        03 PTUE PIC X.
        03 PWED PIC X.
        03 PTHU PIC X.
        03 PFRI PIC X.
        03 FILLER PIC X VALUE SPACES.

        03 FILLER PIC X(10) VALUE SPACES.
        03 PSTARTING-HOUR PIC Z9.
        03 FILLER PIC X VALUE ':'.
        03 PSTARTING-MIN PIC 99.
        03 FILLER PIC X(9) VALUE SPACES.
        03 PCLASSLIMIT PIC ZZ9.
        03 FILLER PIC X(8).
        03 PSEATS-REMAINING PIC ZZ9-.
        03 FILLER PIC X(8).
        03 PTAKEN PIC ZZ9.

       01 PRINT-GRD-TOTAL.
        03 FILLER PIC X(10) VALUE SPACES.
        03 FILLER PIC X(11) VALUE 'GRAND TOTAL'.
        03 FILLER PIC X(51) VALUE SPACES.
        03 PTOTAL-LIMIT PIC ZZ9.
        03 FILLER PIC X(8) VALUE SPACES.
        03 PTOTAL-REMAINING PIC ZZ9.
        03 FILLER PIC X(9) VALUE SPACES.
        03 PTOTAL-TAKEN PIC ZZ9.



        PROCEDURE DIVISION.
        000-MAINLINE.
        OPEN INPUT COURSE-FILE
        OUTPUT PRINT-FILE.
           READ COURSE-FILE INTO COURSE-DATA
                   AT END SET END-OF-DATA TO TRUE
           END-READ
      *  PERFORM 800-READ-COURSE-FILE.
        PERFORM 225-COURSE-HEADINGS.
        PERFORM UNTIL END-OF-DATA
              PERFORM 100-PROCESS-LOOP
      *        PERFORM 800-READ-COURSE-FILE
           READ COURSE-FILE INTO COURSE-DATA
                   AT END SET END-OF-DATA TO TRUE
           END-READ
        END-PERFORM
        PERFORM 2021-PRNT-TOTALS.
        CLOSE COURSE-FILE, PRINT-FILE.
        STOP RUN.
      ******************************************************************
      * PRINT EACH CLASS ***
      ******************************************************************
        100-PROCESS-LOOP.
        IF LINE-CT > 45
        THEN

                PERFORM 225-COURSE-HEADINGS
        END-IF.
        MOVE C-ABB TO PABB.
        MOVE C-NUMB TO PNUMB.
        MOVE C-SEC TO PSEC.
        MOVE C-BUILDING TO PBUILDING.
        MOVE C-ROOM TO PROOM.
      *MOVE C-DAYS TO PDAYS.
        IF NOT (C-MON = 'M' ) THEN
                MOVE '-' TO C-MON
        END-IF.
        MOVE C-MON TO PMON.

        IF NOT ( C-TUE = 'T' ) THEN
                MOVE '-' TO C-TUE
        END-IF.
        MOVE C-TUE TO PTUE.

        IF NOT ( C-WED = 'W' ) THEN
                MOVE '-' TO C-WED
        END-IF.
        MOVE C-WED TO PWED.

        IF NOT ( C-THU = 'H' ) THEN
                MOVE '-' TO C-THU
        END-IF.
        MOVE C-THU TO PTHU.

        IF NOT( C-FRI = 'F' ) THEN
                MOVE '-' TO C-FRI
        END-IF.
        MOVE C-FRI TO PFRI.

        MOVE C-STARTING-HOUR TO PSTARTING-HOUR.
        MOVE C-STARTING-MIN TO PSTARTING-MIN.
        MOVE C-SEATS-REMAINING TO PSEATS-REMAINING.
        MOVE C-CLASSLIMIT TO PCLASSLIMIT.
           COMPUTE CTAKEN = C-CLASSLIMIT - C-SEATS-REMAINING.
        MOVE CTAKEN TO PTAKEN.
        COMPUTE CTOTAL-LIMIT = CTOTAL-LIMIT + C-CLASSLIMIT.
        COMPUTE CTOTAL-REMAINING = CTOTAL-REMAINING + C-SEATS-REMAINING.
        COMPUTE CTOTAL-TAKEN = CTOTAL-TAKEN + CTAKEN.
        WRITE PRINT-REC FROM PRINT-DATA AFTER ADVANCING 1 LINE.
        ADD 1 TO LINE-CT.
      *PERFORM 800-READ-COURSE-FILE.
      ******************************************************************
      * PRINTS HEADING LINE ***
      ******************************************************************
       225-COURSE-HEADINGS.
        WRITE PRINT-REC FROM HEADING1 AFTER ADVANCING PAGE.
        WRITE PRINT-REC FROM HEADING2 AFTER ADVANCING 1.
        MOVE SPACES TO PRINT-REC.
        WRITE PRINT-REC AFTER ADVANCING 1.
        MOVE 0 TO LINE-CT.
      ******************************************************************
      * READS THE DATA FILE ***
      ******************************************************************
       800-READ-COURSE-FILE.
           READ COURSE-FILE INTO COURSE-DATA
                   AT END SET END-OF-DATA TO TRUE
           END-READ.

       2021-PRNT-TOTALS.
        MOVE CTOTAL-LIMIT TO PTOTAL-LIMIT.
        MOVE CTOTAL-REMAINING TO PTOTAL-REMAINING.
        MOVE CTOTAL-TAKEN TO PTOTAL-TAKEN.
        WRITE PRINT-REC FROM PRINT-GRD-TOTAL AFTER ADVANCING 2 LINES.
