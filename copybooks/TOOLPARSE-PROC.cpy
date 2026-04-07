      *> ============================================================
      *> TOOLPARSE-PROC.cpy - Parse Tool Call from LLM Response
      *> COPY in PROCEDURE DIVISION
      *> ============================================================
       PARSE-TOOL-CALL.
           MOVE SPACES TO WS-TOOL-NAME
           MOVE SPACES TO WS-TOOL-CALL-ID
           MOVE SPACES TO WS-TOOL-ARGS

           MOVE SPACES TO WS-TMP2
           STRING WS-QT "tool_calls" WS-QT
               DELIMITED SIZE INTO WS-TMP2
           END-STRING

           MOVE 0 TO WS-KEY-POS
           PERFORM VARYING WS-FJV-POS
               FROM 1 BY 1
               UNTIL WS-FJV-POS > WS-JLEN
               OR WS-KEY-POS > 0
               IF WS-JBUF(WS-FJV-POS:
                   LENGTH(TRIM(WS-TMP2)))
                   = TRIM(WS-TMP2)
                   MOVE WS-FJV-POS
                       TO WS-KEY-POS
               END-IF
           END-PERFORM

           IF WS-KEY-POS = 0
               EXIT PARAGRAPH
           END-IF

      *>   Save JBUF state, parse within
           MOVE WS-JBUF(1:WS-JLEN)
               TO WS-JBUF-SAVE
           MOVE WS-JLEN TO WS-JLEN-SAVE

      *>   Extract id, name, arguments
           MOVE "id" TO WS-KEY-SEARCH
           MOVE WS-KEY-POS TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL)
               TO WS-TOOL-CALL-ID

           MOVE "name" TO WS-KEY-SEARCH
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL)
               TO WS-TOOL-NAME

           MOVE "arguments"
               TO WS-KEY-SEARCH
           PERFORM FIND-JSON-VAL
      *>   Unescape arguments JSON
           MOVE WS-JVAL TO WS-ESC-IN
           PERFORM JSON-UNESCAPE-STR
           MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
               TO WS-TOOL-ARGS

      *>   Restore JBUF
           MOVE WS-JBUF-SAVE(1:WS-JLEN-SAVE)
               TO WS-JBUF
           MOVE WS-JLEN-SAVE TO WS-JLEN

           DISPLAY "  Tool ID: "
               TRIM(WS-TOOL-CALL-ID)
           DISPLAY "  Args: "
               TRIM(WS-TOOL-ARGS)(1:300)
           .
