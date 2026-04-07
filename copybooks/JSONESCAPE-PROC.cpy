      *> ============================================================
      *> JSONESCAPE-PROC.cpy - JSON String Escaping
      *> COPY in PROCEDURE DIVISION
      *> ============================================================
       JSON-ESCAPE-STR.
           MOVE SPACES TO WS-ESC-OUT
           MOVE 0 TO WS-ESC-OLEN
           MOVE LENGTH(TRIM(WS-ESC-IN))
               TO WS-ESC-ILEN

           IF WS-ESC-ILEN = 0
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-ESC-I
               FROM 1 BY 1
               UNTIL WS-ESC-I
               > WS-ESC-ILEN
               EVALUATE TRUE
               WHEN WS-ESC-IN(
                   WS-ESC-I:1) = WS-QT
                   ADD 1 TO WS-ESC-OLEN
                   MOVE X"5C"
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
                   ADD 1 TO WS-ESC-OLEN
                   MOVE WS-QT
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
               WHEN WS-ESC-IN(
                   WS-ESC-I:1) = X"5C"
                   ADD 1 TO WS-ESC-OLEN
                   MOVE X"5C"
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
                   ADD 1 TO WS-ESC-OLEN
                   MOVE X"5C"
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
               WHEN WS-ESC-IN(
                   WS-ESC-I:1) = X"0A"
                   ADD 1 TO WS-ESC-OLEN
                   MOVE X"5C"
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
                   ADD 1 TO WS-ESC-OLEN
                   MOVE "n"
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
               WHEN WS-ESC-IN(
                   WS-ESC-I:1) = X"0D"
                   ADD 1 TO WS-ESC-OLEN
                   MOVE X"5C"
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
                   ADD 1 TO WS-ESC-OLEN
                   MOVE "r"
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
               WHEN WS-ESC-IN(
                   WS-ESC-I:1) = X"09"
                   ADD 1 TO WS-ESC-OLEN
                   MOVE X"5C"
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
                   ADD 1 TO WS-ESC-OLEN
                   MOVE "t"
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
               WHEN OTHER
                   ADD 1 TO WS-ESC-OLEN
                   MOVE WS-ESC-IN(
                       WS-ESC-I:1)
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
               END-EVALUATE
           END-PERFORM
           .
