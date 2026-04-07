      *> ============================================================
      *> JSONPARSE-PROC.cpy - Find JSON Value by Key
      *> COPY in PROCEDURE DIVISION
      *> ============================================================
       FIND-JSON-VAL.
           MOVE SPACES TO WS-JVAL
           MOVE SPACES TO WS-TMP2
           STRING WS-QT
               TRIM(WS-KEY-SEARCH)
               WS-QT
               DELIMITED SIZE
               INTO WS-TMP2
           END-STRING

           MOVE 0 TO WS-KEY-POS
           PERFORM VARYING WS-FJV-POS
               FROM WS-JPOS BY 1
               UNTIL WS-FJV-POS
                   > WS-JLEN
               OR WS-KEY-POS > 0
               IF WS-FJV-POS
                   + LENGTH(
                   TRIM(WS-TMP2))
                   - 1 <= WS-JLEN
               AND WS-JBUF(
                   WS-FJV-POS:
                   LENGTH(
                   TRIM(WS-TMP2)))
                   = TRIM(WS-TMP2)
                   MOVE WS-FJV-POS
                       TO WS-KEY-POS
               END-IF
           END-PERFORM

           IF WS-KEY-POS = 0
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-FJV-POS =
               WS-KEY-POS
               + LENGTH(
               TRIM(WS-TMP2))
           PERFORM UNTIL
               WS-FJV-POS > WS-JLEN
               OR WS-JBUF(
               WS-FJV-POS:1) = ":"
               ADD 1 TO WS-FJV-POS
           END-PERFORM
           ADD 1 TO WS-FJV-POS

           PERFORM UNTIL
               WS-FJV-POS > WS-JLEN
               OR WS-JBUF(
               WS-FJV-POS:1)
               NOT = " "
               ADD 1 TO WS-FJV-POS
           END-PERFORM

           IF WS-JBUF(
               WS-FJV-POS:1) = WS-QT
               ADD 1 TO WS-FJV-POS
               MOVE WS-FJV-POS
                   TO WS-VAL-START
               PERFORM UNTIL
                   WS-FJV-POS
                   > WS-JLEN
                   IF WS-JBUF(
                       WS-FJV-POS:1)
                       = X"5C"
                   AND WS-FJV-POS
                       < WS-JLEN
                       ADD 2
                       TO WS-FJV-POS
                   ELSE
                       IF WS-JBUF(
                           WS-FJV-POS:1)
                           = WS-QT
                           EXIT PERFORM
                       END-IF
                       ADD 1
                       TO WS-FJV-POS
                   END-IF
               END-PERFORM
               COMPUTE WS-VAL-END =
                   WS-FJV-POS - 1
               IF WS-VAL-END
                   >= WS-VAL-START
               AND WS-VAL-END
                   - WS-VAL-START
                   + 1 <= 4000
                   MOVE WS-JBUF(
                       WS-VAL-START:
                       WS-VAL-END
                       - WS-VAL-START
                       + 1) TO WS-JVAL
               END-IF
               ADD 1 TO WS-FJV-POS
           ELSE
               MOVE WS-FJV-POS
                   TO WS-VAL-START
               PERFORM UNTIL
                   WS-FJV-POS
                   > WS-JLEN
                   OR WS-JBUF(
                   WS-FJV-POS:1)
                   = ","
                   OR WS-JBUF(
                   WS-FJV-POS:1)
                   = "}"
                   OR WS-JBUF(
                   WS-FJV-POS:1)
                   = "]"
                   OR WS-JBUF(
                   WS-FJV-POS:1)
                   = " "
                   ADD 1
                   TO WS-FJV-POS
               END-PERFORM
               COMPUTE WS-VAL-END =
                   WS-FJV-POS - 1
               IF WS-VAL-END
                   >= WS-VAL-START
                   MOVE WS-JBUF(
                       WS-VAL-START:
                       WS-VAL-END
                       - WS-VAL-START
                       + 1) TO WS-JVAL
               END-IF
           END-IF
           MOVE WS-FJV-POS
               TO WS-JPOS
           .
