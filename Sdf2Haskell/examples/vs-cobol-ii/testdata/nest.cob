       IDENTIFICATION DIVISION.
        PROGRAM-ID. A-LITTLE-NEST.

       DATA DIVISION.
        WORKING-STORAGE SECTION.
         01 OP-CODE PIC 99.

       PROCEDURE DIVISION.

           PERFORM MY-MAIN.
           STOP RUN.

        MY-MAIN.
	   PERFORM A THRU C.
	   PERFORM C.
           PERFORM D.
        A.
           MOVE 1 TO OP-CODE.
           PERFORM MY-HANDLER.
        B.
           MOVE 2 TO OP-CODE.
           PERFORM MY-HANDLER.
        C.
           PERFORM D.
	D.
	   PERFORM E.
        E.
           MOVE 3 TO OP-CODE.
           PERFORM MY-HANDLER.

        COPY "GLORY-REST.COB".


