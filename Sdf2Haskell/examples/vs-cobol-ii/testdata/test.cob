       IDENTIFICATION DIVISION.
        PROGRAM-ID. LITTLE-Y2K-TEST.

       DATA DIVISION.
        WORKING-STORAGE SECTION.

         01  SEEK-NAME       PIC 99.
         01  OTHER-NAME-1    PIC 99.
         01  OTHER-NAME-2    PIC 99999.
         01  OTHER-NAME-3    PIC 99.

       PROCEDURE DIVISION.

        IF A
         IF B
          CONTINUE
         ELSE
          CONTINUE
        END-IF.


