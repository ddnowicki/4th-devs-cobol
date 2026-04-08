      *> ============================================================
      *> JSONREAD-PROC.cpy - Read JSON File into WS-JBUF
      *> COPY in PROCEDURE DIVISION
      *> ============================================================
       READ-JSON-FILE.
           MOVE SPACES TO WS-JBUF
           MOVE 0 TO WS-JLEN
           MOVE "N" TO WS-EOF

           OPEN INPUT WORK-FILE
           IF WS-FS NOT = "00"
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL WS-EOF = "Y"
               READ WORK-FILE INTO WS-LINE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       MOVE LENGTH(
                           TRIM(WS-LINE
                           TRAILING))
                           TO WS-K
                       IF WS-K > 0
                           IF WS-JLEN > 0
                               ADD 1
                                 TO WS-JLEN
                               MOVE " "
                                 TO WS-JBUF(
                                 WS-JLEN:1)
                           END-IF
                           IF WS-K > 16000
                               MOVE 16000
                                   TO WS-K
                           END-IF
                           IF WS-JLEN + WS-K
                               <= 32000
                               MOVE WS-LINE(
                                   1:WS-K)
                                   TO WS-JBUF(
                                   WS-JLEN
                                   + 1:WS-K)
                               ADD WS-K
                                   TO WS-JLEN
                           END-IF
                       END-IF
               END-READ
           END-PERFORM

           CLOSE WORK-FILE
           MOVE "N" TO WS-EOF
           .
