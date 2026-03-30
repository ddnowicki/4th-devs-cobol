       IDENTIFICATION DIVISION.
       PROGRAM-ID. S04E01-OKOEDITOR.
      *> ============================================================
      *> S04E01 - OKO Editor (Pure COBOL)
      *> 1. Call help API to get docs
      *> 2. Deterministic updates (proven content)
      *> 3. LLM recovery with function calling if needed
      *> Tools: update_incident, update_task, finalize
      *> ============================================================

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WORK-FILE ASSIGN TO WS-WORK-PATH
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  WORK-FILE.
       01  WORK-REC                PIC X(64000).

       WORKING-STORAGE SECTION.
      *> -- Config --
       01  WS-HUB-KEY              PIC X(50).
       01  WS-OPENAI-KEY           PIC X(200).
       01  WS-QT                   PIC X(1) VALUE '"'.
       01  WS-FS                   PIC XX.
       01  WS-WORK-PATH            PIC X(100)
                                   VALUE "work.tmp".

      *> -- URLs --
       01  WS-HUB-URL              PIC X(100).
       01  WS-OPENAI-URL           PIC X(200).
       01  WS-VERIFY-URL           PIC X(200).

      *> -- JSON newline: backslash + n --
       01  WS-NL                   PIC X(2).

      *> -- STRING pointer --
       01  WS-PTR                  PIC 9(5).

      *> -- System command --
       01  WS-CMD                  PIC X(4000).

      *> -- Request JSON buffer --
       01  WS-REQ-JSON             PIC X(64000).

      *> -- Hub API request body --
       01  WS-HUB-BODY             PIC X(4000).

      *> -- JSON buffer for parsing --
       01  WS-JBUF                 PIC X(16000).
       01  WS-JLEN                 PIC 9(5).
       01  WS-JPOS                 PIC 9(5).
       01  WS-JVAL                 PIC X(4000).

      *> -- JSON parsing temps --
       01  WS-KEY-SEARCH           PIC X(50).
       01  WS-KEY-POS              PIC 9(5).
       01  WS-VAL-START            PIC 9(5).
       01  WS-VAL-END              PIC 9(5).
       01  WS-FJV-POS              PIC 9(5).
       01  WS-TMP                  PIC X(4000).
       01  WS-TMP2                 PIC X(500).

      *> -- Task constants --
       01  WS-SKOLWIN-ID           PIC X(32)
           VALUE "380792b2c86d9c5be670b3bde48e187b"
           .
       01  WS-KOMAROWO-ID          PIC X(32)
           VALUE "351c0d9c90d66b4c040fff1259dd191d"
           .
       01  WS-TASK-NAME            PIC X(20)
                                   VALUE "okoeditor".

      *> -- Help docs (escaped for JSON) --
       01  WS-HELP-DOCS            PIC X(8000).
       01  WS-HELP-LEN             PIC 9(5).

      *> -- JSON escape I/O --
       01  WS-ESC-IN               PIC X(8000).
       01  WS-ESC-OUT              PIC X(16000).
       01  WS-ESC-ILEN             PIC 9(5).
       01  WS-ESC-OLEN             PIC 9(5).
       01  WS-ESC-I                PIC 9(5).

      *> -- Agent conversation buffer --
       01  WS-CONV-BUF             PIC X(64000).
       01  WS-CONV-PTR             PIC 9(5).

      *> -- Agent loop --
       01  WS-AG-STEP              PIC 9(2) VALUE 0.
       01  WS-AG-DONE              PIC X VALUE "N".

      *> -- Tool call parsing --
       01  WS-TOOL-NAME            PIC X(50).
       01  WS-TOOL-CALL-ID         PIC X(100).
       01  WS-TOOL-ARGS            PIC X(2000).
       01  WS-TOOL-RESULT          PIC X(8000).
       01  WS-TOOL-RESULT-LEN      PIC 9(5).

      *> -- Tool arg values --
       01  WS-TA-ID                PIC X(50).
       01  WS-TA-TITLE             PIC X(500).
       01  WS-TA-CONTENT           PIC X(2000).
       01  WS-TA-DONE              PIC X(3).
       01  WS-TA-PAGE              PIC X(20).

      *> -- Loop/misc --
       01  WS-I                    PIC 9(5).
       01  WS-K                    PIC 9(5).
       01  WS-EOF                  PIC X VALUE "N".
       01  WS-LINE                 PIC X(4000).
       01  WS-TALLY-CNT            PIC 9(4).

      *> -- Success/flag tracking --
       01  WS-FLAG-FOUND           PIC X VALUE "N".
       01  WS-DET-ERROR             PIC X(2000).

      *> -- Nudge counter --
       01  WS-NUDGE-CT             PIC 9(1) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S04E01 OKOEDITOR - COBOL ==="

           ACCEPT WS-HUB-KEY
               FROM ENVIRONMENT "HUB_API_KEY"
           ACCEPT WS-OPENAI-KEY
               FROM ENVIRONMENT "OPENAI_API_KEY"
           ACCEPT WS-HUB-URL
               FROM ENVIRONMENT "HUB_API_URL"
           ACCEPT WS-OPENAI-URL
               FROM ENVIRONMENT "OPENAI_API_URL"

           IF WS-HUB-KEY = SPACES
               DISPLAY "ERR: HUB_API_KEY!"
               STOP RUN
           END-IF
           IF WS-OPENAI-KEY = SPACES
               DISPLAY "ERR: OPENAI_API_KEY!"
               STOP RUN
           END-IF
           IF WS-HUB-URL = SPACES
               DISPLAY "ERR: HUB_API_URL!"
               STOP RUN
           END-IF
           IF WS-OPENAI-URL = SPACES
               DISPLAY "ERR: OPENAI_API_URL!"
               STOP RUN
           END-IF

           MOVE SPACES TO WS-VERIFY-URL
           STRING TRIM(WS-HUB-URL)
               "/verify"
               DELIMITED SIZE
               INTO WS-VERIFY-URL
           END-STRING

      *>   Init JSON newline
           MOVE X"5C" TO WS-NL(1:1)
           MOVE "n"    TO WS-NL(2:1)

      *>   Stage 1: Fetch help docs
           DISPLAY " "
           DISPLAY "[STAGE 1] Fetching help docs..."
           PERFORM CALL-HELP-API

      *>   Stage 2: Deterministic updates
           DISPLAY " "
           DISPLAY "[STAGE 2] Deterministic updates..."
           PERFORM RUN-DETERMINISTIC

           IF WS-FLAG-FOUND = "Y"
               DISPLAY " "
               DISPLAY "=== ZAKONCZONO ==="
               STOP RUN
           END-IF

      *>   Stage 3: LLM recovery
           DISPLAY " "
           DISPLAY "[STAGE 3] LLM recovery..."
           PERFORM RUN-LLM-RECOVERY

           DISPLAY " "
           IF WS-FLAG-FOUND = "Y"
               DISPLAY "  >>> SUKCES!"
           ELSE
               DISPLAY "  NIEPOWODZENIE."
           END-IF
           DISPLAY "=== ZAKONCZONO ==="
           STOP RUN.

      *> ============================================================
      *> CALL-HELP-API: Get API docs via help action
      *> ============================================================
       CALL-HELP-API.
           MOVE SPACES TO WS-HUB-BODY
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME) WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT "help" WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
           END-STRING

           MOVE "hub_req.tmp" TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           WRITE WORK-REC FROM WS-HUB-BODY
           CLOSE WORK-FILE

           INITIALIZE WS-CMD
           STRING
               "curl -s "
               "-o help_resp.json"
               " -X POST "
               TRIM(WS-VERIFY-URL)
               " -H " WS-QT
               "Content-Type: "
               "application/json"
               WS-QT
               " -d @hub_req.tmp"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           MOVE "help_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           DISPLAY "  Help response: "
               WS-JBUF(1:500)

      *>   Store help docs escaped for LLM
           MOVE WS-JBUF(1:WS-JLEN) TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR
           MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
               TO WS-HELP-DOCS
           MOVE WS-ESC-OLEN TO WS-HELP-LEN
           .

      *> ============================================================
      *> RUN-DETERMINISTIC: Known-good Polish content
      *> ============================================================
       RUN-DETERMINISTIC.
      *>   1. Skolwin incident -> animals (MOVE04)
           DISPLAY " "
           DISPLAY "  [1/4] Skolwin incident..."
           PERFORM DET-SKOLWIN-INCIDENT
           CALL "C$SLEEP" USING 1

      *>   2. Skolwin task -> done
           DISPLAY "  [2/4] Skolwin task..."
           PERFORM DET-SKOLWIN-TASK
           CALL "C$SLEEP" USING 1

      *>   3. Komarowo incident -> humans (MOVE01)
           DISPLAY "  [3/4] Komarowo incident..."
           PERFORM DET-KOMAROWO-INCIDENT
           CALL "C$SLEEP" USING 1

      *>   4. Done action
           DISPLAY "  [4/4] Calling done..."
           PERFORM CALL-DONE-ACTION

      *>   Check for flag
           MOVE 0 TO WS-TALLY-CNT
           IF WS-JLEN > 0
               INSPECT WS-JBUF(1:WS-JLEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL "FLG"
           END-IF
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-FLAG-FOUND
               DISPLAY "  >>> FLAG FOUND <<<"
           ELSE
               MOVE WS-JBUF(1:WS-JLEN)
                   TO WS-DET-ERROR
               DISPLAY "  No flag from done."
           END-IF
           .

      *> ============================================================
      *> DET-SKOLWIN-INCIDENT
      *> ============================================================
       DET-SKOLWIN-INCIDENT.
           MOVE SPACES TO WS-HUB-BODY
           MOVE 1 TO WS-PTR
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME)
               WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT "update" WS-QT ","
               WS-QT "page" WS-QT ":"
               WS-QT "incydenty" WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "id" WS-QT ":"
               WS-QT TRIM(WS-SKOLWIN-ID)
               WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "title" WS-QT ":"
               WS-QT
               "MOVE04 Trudne do klasyf"
               "ikacji ruchy nieopodal"
               " miasta Skolwin"
               WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "content" WS-QT ":"
               WS-QT
               "W okolicach miasta Sk"
               "olwin wykryto ruch zw"
               "ierzat. Analiza danyc"
               "h wskazuje na obecnos"
               "c dzikiej fauny, praw"
               "dopodobnie bobrow lub"
               " innych zwierzat wodn"
               "ych poruszajacych sie"
               " w poblizu rzeki."
               WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           PERFORM SEND-HUB-REQUEST
           .

      *> ============================================================
      *> DET-SKOLWIN-TASK
      *> ============================================================
       DET-SKOLWIN-TASK.
           MOVE SPACES TO WS-HUB-BODY
           MOVE 1 TO WS-PTR
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME)
               WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT "update" WS-QT ","
               WS-QT "page" WS-QT ":"
               WS-QT "zadania" WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "id" WS-QT ":"
               WS-QT TRIM(WS-SKOLWIN-ID)
               WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "content" WS-QT ":"
               WS-QT
               "Zadanie zakonczone. Z"
               "aobserwowano ruch zwi"
               "erzat (bobry) w okoli"
               "cach Skolwina. Reklas"
               "yfikacja incydentu z "
               "MOVE03 na MOVE04."
               WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "done" WS-QT ":"
               WS-QT "YES" WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           PERFORM SEND-HUB-REQUEST
           .

      *> ============================================================
      *> DET-KOMAROWO-INCIDENT
      *> ============================================================
       DET-KOMAROWO-INCIDENT.
           MOVE SPACES TO WS-HUB-BODY
           MOVE 1 TO WS-PTR
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME)
               WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT "update" WS-QT ","
               WS-QT "page" WS-QT ":"
               WS-QT "incydenty" WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "id" WS-QT ":"
               WS-QT TRIM(WS-KOMAROWO-ID)
               WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "title" WS-QT ":"
               WS-QT
               "MOVE01 Wykrycie ruchu "
               "ludzi w okolicach mia"
               "sta Komarowo"
               WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "content" WS-QT ":"
               WS-QT
               "W okolicach niezamies"
               "zkalego miasta Komaro"
               "wo wykryto ruch ludzi"
               ". Czujniki zarejestro"
               "waly obecnosc osob pr"
               "zemieszczajacych sie "
               "w poblizu opuszczonyc"
               "h budynkow."
               WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           PERFORM SEND-HUB-REQUEST
           .

      *> ============================================================
      *> CALL-DONE-ACTION
      *> ============================================================
       CALL-DONE-ACTION.
           MOVE SPACES TO WS-HUB-BODY
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME) WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT "done" WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
           END-STRING

           PERFORM SEND-HUB-REQUEST
           .

      *> ============================================================
      *> SEND-HUB-REQUEST: Write body + curl POST
      *> ============================================================
       SEND-HUB-REQUEST.
           MOVE "hub_req.tmp" TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           WRITE WORK-REC FROM WS-HUB-BODY
           CLOSE WORK-FILE

           INITIALIZE WS-CMD
           STRING
               "curl -s "
               "-o hub_resp.json"
               " -X POST "
               TRIM(WS-VERIFY-URL)
               " -H " WS-QT
               "Content-Type: "
               "application/json"
               WS-QT
               " -d @hub_req.tmp"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           MOVE "hub_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           DISPLAY "    Hub: "
               WS-JBUF(1:500)
           .

      *> ============================================================
      *> RUN-LLM-RECOVERY: Function calling loop
      *> ============================================================
       RUN-LLM-RECOVERY.
           DISPLAY " "
           DISPLAY "--- LLM Agent starting ---"

           MOVE SPACES TO WS-CONV-BUF
           MOVE 1 TO WS-CONV-PTR

      *>   System message
           STRING
               "[{" WS-QT "role" WS-QT ":"
               WS-QT "system" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "Jestes edytorem systemu"
               " monitoringu incydentow"
               " i zadan. WSZYSTKIE "
               "tresci (title, content)"
               " MUSISZ pisac WYLACZNIE"
               " PO POLSKU."
               WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "## Kody klasyfikacji"
               WS-NL
               "- MOVE01 = ruch ludzi"
               WS-NL
               "- MOVE02 = ruch pojazdu"
               WS-NL
               "- MOVE03 = ruch pojazdu"
               " i czlowieka"
               WS-NL
               "- MOVE04 = ruch zwierzat"
               WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "## Wymagane zmiany:"
               WS-NL
               "1. INCYDENT Skolwin (id:"
               " "
               TRIM(WS-SKOLWIN-ID)
               "): tytul MOVE04, tresc "
               "o ruchu zwierzat"
               WS-NL
               "2. ZADANIE Skolwin (id: "
               TRIM(WS-SKOLWIN-ID)
               "): done=YES, tresc o "
               "bobrach"
               WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "3. INCYDENT Komarowo "
               "(id: "
               TRIM(WS-KOMAROWO-ID)
               "): tytul MOVE01, tresc"
               " o ruchu ludzi"
               WS-NL
               "4. Wywolaj finalize."
               WS-NL WS-NL
               "Wywolaj narzedzia teraz."
               WS-QT "},"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

      *>   User message with help docs + error
           STRING
               "{" WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               "Dokumentacja API:" WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

      *>   Include help docs
           IF WS-HELP-LEN > 0
               STRING
                   WS-HELP-DOCS(
                   1:WS-HELP-LEN)
                   DELIMITED SIZE
                   INTO WS-CONV-BUF
                   WITH POINTER WS-CONV-PTR
               END-STRING
           END-IF

      *>   Include error context
           MOVE TRIM(WS-DET-ERROR) TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR
           IF WS-ESC-OLEN > 0
               STRING
                   WS-NL WS-NL
                   "Blad deterministic: "
                   WS-ESC-OUT(1:WS-ESC-OLEN)
                   DELIMITED SIZE
                   INTO WS-CONV-BUF
                   WITH POINTER WS-CONV-PTR
               END-STRING
           END-IF

           STRING
               WS-NL WS-NL
               "Wykonaj wszystkie 4 "
               "wywolania narzedzi "
               "teraz."
               WS-QT "}]"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

      *>   Agent loop
           MOVE "N" TO WS-AG-DONE
           MOVE 0 TO WS-AG-STEP
           MOVE 0 TO WS-NUDGE-CT

           PERFORM UNTIL WS-AG-DONE = "Y"
               OR WS-AG-STEP >= 15

               ADD 1 TO WS-AG-STEP
               DISPLAY " "
               DISPLAY "  --- Step "
                   WS-AG-STEP " ---"

               PERFORM SEND-AGENT-REQUEST

               MOVE "agent_resp.json"
                   TO WS-WORK-PATH
               PERFORM READ-JSON-FILE
               MOVE "work.tmp"
                   TO WS-WORK-PATH

               IF WS-JLEN = 0
                   DISPLAY "  Empty response!"
                   MOVE "Y" TO WS-AG-DONE
                   EXIT PERFORM
               END-IF

      *>       Check API error
               MOVE 0 TO WS-TALLY-CNT
               INSPECT WS-JBUF(1:WS-JLEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL '"error"'
               IF WS-TALLY-CNT > 0
                   DISPLAY "  API ERROR: "
                       WS-JBUF(1:500)
                   MOVE "Y" TO WS-AG-DONE
                   EXIT PERFORM
               END-IF

      *>       Check for tool_calls
               MOVE 0 TO WS-TALLY-CNT
               INSPECT WS-JBUF(1:WS-JLEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL '"tool_calls"'

               IF WS-TALLY-CNT > 0
                   PERFORM PARSE-TOOL-CALL
                   MOVE 0 TO WS-NUDGE-CT

                   DISPLAY "  Tool: "
                       TRIM(WS-TOOL-NAME)

                   PERFORM DISPATCH-TOOL
                   PERFORM APPEND-TOOL-EXCHANGE

      *>           If finalize, check flag
                   IF TRIM(WS-TOOL-NAME)
                       = "finalize"
                       MOVE 0 TO WS-TALLY-CNT
                       IF WS-TOOL-RESULT-LEN
                           > 0
                           INSPECT
                             WS-TOOL-RESULT(
                             1:WS-TOOL-RESULT-LEN
                             )
                             TALLYING
                             WS-TALLY-CNT
                             FOR ALL "FLG"
                       END-IF
                       IF WS-TALLY-CNT > 0
                           MOVE "Y"
                               TO WS-FLAG-FOUND
                           DISPLAY
                             "  >>> FLAG <<<"
                           MOVE "Y"
                               TO WS-AG-DONE
                       ELSE
      *>                   Feed error back
                           SUBTRACT 1
                             FROM WS-CONV-PTR
                           STRING ","
                             "{"
                             WS-QT "role" WS-QT
                             ":"
                             WS-QT "user" WS-QT
                             ","
                             WS-QT "content"
                             WS-QT ":"
                             WS-QT
                             "Akcja done zwrocila"
                             " blad. Popraw i "
                             "wywolaj finalize "
                             "ponownie."
                             WS-QT "}]"
                             DELIMITED SIZE
                             INTO WS-CONV-BUF
                             WITH POINTER
                             WS-CONV-PTR
                           END-STRING
                       END-IF
                   END-IF
               ELSE
      *>           Text response - nudge
                   MOVE "content"
                       TO WS-KEY-SEARCH
                   MOVE 1 TO WS-JPOS
                   PERFORM FIND-JSON-VAL
                   DISPLAY "  Agent: "
                       WS-JVAL(1:200)

      *>           Check for flag in text
                   MOVE 0 TO WS-TALLY-CNT
                   INSPECT TRIM(WS-JVAL)
                       TALLYING WS-TALLY-CNT
                       FOR ALL "FLG"
                   IF WS-TALLY-CNT > 0
                       MOVE "Y"
                           TO WS-FLAG-FOUND
                       MOVE "Y"
                           TO WS-AG-DONE
                   ELSE
                       ADD 1 TO WS-NUDGE-CT
                       IF WS-NUDGE-CT > 3
                           DISPLAY
                               "  Agent stuck"
                           MOVE "Y"
                               TO WS-AG-DONE
                       ELSE
                           PERFORM
                             APPEND-TEXT-NUDGE
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> SEND-AGENT-REQUEST: Build + send to OpenAI
      *> ============================================================
       SEND-AGENT-REQUEST.
           MOVE SPACES TO WS-REQ-JSON
           MOVE 1 TO WS-PTR

           STRING
               "{"
               WS-QT "model" WS-QT ":"
               WS-QT "gpt-4.1-mini" WS-QT ","
               WS-QT "temperature" WS-QT
               ":0,"
               WS-QT "messages" WS-QT ":"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Copy conversation
           COMPUTE WS-K = WS-CONV-PTR - 1
           MOVE WS-CONV-BUF(1:WS-K)
               TO WS-REQ-JSON(WS-PTR:WS-K)
           ADD WS-K TO WS-PTR

      *>   Tool definitions
           STRING ","
               WS-QT "tools" WS-QT ":["
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Tool 1: update_incident
           STRING
               "{" WS-QT "type" WS-QT ":"
               WS-QT "function" WS-QT ","
               WS-QT "function" WS-QT ":{"
               WS-QT "name" WS-QT ":"
               WS-QT "update_incident"
               WS-QT ","
               WS-QT "description" WS-QT ":"
               WS-QT "Aktualizuje wpis "
               "incydentu" WS-QT ","
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "parameters" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "object" WS-QT ","
               WS-QT "properties" WS-QT ":{"
               WS-QT "id" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "string" WS-QT "},"
               WS-QT "title" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "string" WS-QT "},"
               WS-QT "content" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "string" WS-QT "}},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "required" WS-QT ":["
               WS-QT "id" WS-QT ","
               WS-QT "title" WS-QT ","
               WS-QT "content" WS-QT
               "]}}},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Tool 2: update_task
           STRING
               "{" WS-QT "type" WS-QT ":"
               WS-QT "function" WS-QT ","
               WS-QT "function" WS-QT ":{"
               WS-QT "name" WS-QT ":"
               WS-QT "update_task" WS-QT ","
               WS-QT "description" WS-QT ":"
               WS-QT "Aktualizuje wpis "
               "zadania" WS-QT ","
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "parameters" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "object" WS-QT ","
               WS-QT "properties" WS-QT ":{"
               WS-QT "id" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "string" WS-QT "},"
               WS-QT "content" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "string" WS-QT "},"
               WS-QT "done" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "string" WS-QT ","
               WS-QT "enum" WS-QT ":["
               WS-QT "YES" WS-QT ","
               WS-QT "NO" WS-QT
               "]}},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "required" WS-QT ":["
               WS-QT "id" WS-QT ","
               WS-QT "content" WS-QT ","
               WS-QT "done" WS-QT
               "]}}},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Tool 3: finalize
           STRING
               "{" WS-QT "type" WS-QT ":"
               WS-QT "function" WS-QT ","
               WS-QT "function" WS-QT ":{"
               WS-QT "name" WS-QT ":"
               WS-QT "finalize" WS-QT ","
               WS-QT "description" WS-QT ":"
               WS-QT "Wysyla done aby "
               "zwalidowac zmiany"
               WS-QT ","
               WS-QT "parameters" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "object" WS-QT ","
               WS-QT "properties" WS-QT ":{},"
               WS-QT "required" WS-QT ":[]"
               "}}}],"
               WS-QT "tool_choice" WS-QT ":"
               WS-QT "auto" WS-QT "}"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Write and send
           MOVE "agent_req.json"
               TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           WRITE WORK-REC FROM WS-REQ-JSON
           CLOSE WORK-FILE

           INITIALIZE WS-CMD
           STRING
               "curl -s "
               "-o agent_resp.json"
               " -X POST "
               TRIM(WS-OPENAI-URL)
               " -H " WS-QT
               "Content-Type: "
               "application/json"
               WS-QT
               " -H " WS-QT
               "Authorization: "
               "Bearer "
               TRIM(WS-OPENAI-KEY)
               WS-QT
               " -d @agent_req.json"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD
           .

      *> ============================================================
      *> DISPATCH-TOOL: Execute the called tool
      *> ============================================================
       DISPATCH-TOOL.
           MOVE SPACES TO WS-TOOL-RESULT
           MOVE 0 TO WS-TOOL-RESULT-LEN

           EVALUATE TRIM(WS-TOOL-NAME)
           WHEN "update_incident"
               PERFORM TOOL-UPDATE-INCIDENT
           WHEN "update_task"
               PERFORM TOOL-UPDATE-TASK
           WHEN "finalize"
               PERFORM TOOL-FINALIZE
           WHEN OTHER
               MOVE '{"error":"Unknown tool"}'
                   TO WS-TOOL-RESULT
               MOVE 23 TO WS-TOOL-RESULT-LEN
           END-EVALUATE
           .

      *> ============================================================
      *> TOOL-UPDATE-INCIDENT
      *> ============================================================
       TOOL-UPDATE-INCIDENT.
      *>   Parse args: id, title, content
           MOVE WS-TOOL-ARGS TO WS-JBUF
           MOVE LENGTH(TRIM(WS-TOOL-ARGS))
               TO WS-JLEN

           MOVE "id" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL) TO WS-TA-ID

           MOVE "title" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE WS-JVAL TO WS-TA-TITLE

           MOVE "content" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE WS-JVAL TO WS-TA-CONTENT

      *>   Build hub request
           MOVE SPACES TO WS-HUB-BODY
           MOVE 1 TO WS-PTR

           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME)
               WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT "update" WS-QT ","
               WS-QT "page" WS-QT ":"
               WS-QT "incydenty" WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "id" WS-QT ":"
               WS-QT TRIM(WS-TA-ID)
               WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

      *>   Escape title for JSON
           MOVE TRIM(WS-TA-TITLE)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

           STRING
               WS-QT "title" WS-QT ":"
               WS-QT
               WS-ESC-OUT(1:WS-ESC-OLEN)
               WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

      *>   Escape content for JSON
           MOVE TRIM(WS-TA-CONTENT)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

           STRING
               WS-QT "content" WS-QT ":"
               WS-QT
               WS-ESC-OUT(1:WS-ESC-OLEN)
               WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           CALL "C$SLEEP" USING 1
           PERFORM SEND-HUB-REQUEST

      *>   Store result
           COMPUTE WS-TOOL-RESULT-LEN =
               LENGTH(TRIM(WS-JBUF))
           IF WS-TOOL-RESULT-LEN > 8000
               MOVE 8000
                   TO WS-TOOL-RESULT-LEN
           END-IF
           MOVE WS-JBUF(
               1:WS-TOOL-RESULT-LEN)
               TO WS-TOOL-RESULT
           .

      *> ============================================================
      *> TOOL-UPDATE-TASK
      *> ============================================================
       TOOL-UPDATE-TASK.
      *>   Parse args: id, content, done
           MOVE WS-TOOL-ARGS TO WS-JBUF
           MOVE LENGTH(TRIM(WS-TOOL-ARGS))
               TO WS-JLEN

           MOVE "id" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL) TO WS-TA-ID

           MOVE "content" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE WS-JVAL TO WS-TA-CONTENT

           MOVE "done" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL) TO WS-TA-DONE

      *>   Build hub request
           MOVE SPACES TO WS-HUB-BODY
           MOVE 1 TO WS-PTR

           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME)
               WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT "update" WS-QT ","
               WS-QT "page" WS-QT ":"
               WS-QT "zadania" WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "id" WS-QT ":"
               WS-QT TRIM(WS-TA-ID)
               WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

      *>   Escape content for JSON
           MOVE TRIM(WS-TA-CONTENT)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

           STRING
               WS-QT "content" WS-QT ":"
               WS-QT
               WS-ESC-OUT(1:WS-ESC-OLEN)
               WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "done" WS-QT ":"
               WS-QT TRIM(WS-TA-DONE)
               WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           CALL "C$SLEEP" USING 1
           PERFORM SEND-HUB-REQUEST

      *>   Store result
           COMPUTE WS-TOOL-RESULT-LEN =
               LENGTH(TRIM(WS-JBUF))
           IF WS-TOOL-RESULT-LEN > 8000
               MOVE 8000
                   TO WS-TOOL-RESULT-LEN
           END-IF
           MOVE WS-JBUF(
               1:WS-TOOL-RESULT-LEN)
               TO WS-TOOL-RESULT
           .

      *> ============================================================
      *> TOOL-FINALIZE: Call done action
      *> ============================================================
       TOOL-FINALIZE.
           CALL "C$SLEEP" USING 1
           PERFORM CALL-DONE-ACTION

      *>   Store result
           COMPUTE WS-TOOL-RESULT-LEN =
               LENGTH(TRIM(WS-JBUF))
           IF WS-TOOL-RESULT-LEN > 8000
               MOVE 8000
                   TO WS-TOOL-RESULT-LEN
           END-IF
           MOVE WS-JBUF(
               1:WS-TOOL-RESULT-LEN)
               TO WS-TOOL-RESULT

           DISPLAY "  Finalize result: "
               WS-TOOL-RESULT(1:500)
           .

      *> ============================================================
      *> APPEND-TOOL-EXCHANGE
      *> ============================================================
       APPEND-TOOL-EXCHANGE.
           SUBTRACT 1 FROM WS-CONV-PTR

      *>   Escape tool args
           MOVE TRIM(WS-TOOL-ARGS)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

           STRING ","
               "{" WS-QT "role" WS-QT ":"
               WS-QT "assistant" WS-QT ","
               WS-QT "content" WS-QT
               ":null,"
               WS-QT "tool_calls" WS-QT
               ":["
               "{" WS-QT "id" WS-QT ":"
               WS-QT TRIM(WS-TOOL-CALL-ID)
               WS-QT ","
               WS-QT "type" WS-QT ":"
               WS-QT "function" WS-QT ","
               WS-QT "function" WS-QT ":{"
               WS-QT "name" WS-QT ":"
               WS-QT TRIM(WS-TOOL-NAME)
               WS-QT ","
               WS-QT "arguments" WS-QT ":"
               WS-QT
               WS-ESC-OUT(1:WS-ESC-OLEN)
               WS-QT
               "}}]},"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

      *>   Escape tool result
           IF WS-TOOL-RESULT-LEN > 0
               MOVE WS-TOOL-RESULT(
                   1:WS-TOOL-RESULT-LEN)
                   TO WS-ESC-IN
           ELSE
               MOVE "{}" TO WS-ESC-IN
           END-IF
           PERFORM JSON-ESCAPE-STR

           STRING
               "{" WS-QT "role" WS-QT ":"
               WS-QT "tool" WS-QT ","
               WS-QT "tool_call_id" WS-QT
               ":"
               WS-QT TRIM(WS-TOOL-CALL-ID)
               WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               WS-ESC-OUT(1:WS-ESC-OLEN)
               WS-QT "}]"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING
           .

      *> ============================================================
      *> APPEND-TEXT-NUDGE
      *> ============================================================
       APPEND-TEXT-NUDGE.
           SUBTRACT 1 FROM WS-CONV-PTR

           STRING ","
               "{" WS-QT "role" WS-QT ":"
               WS-QT "assistant" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT "OK" WS-QT "},"
               "{" WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT "Musisz uzyc narzedzi."
               " Wywolaj narzedzia "
               "teraz." WS-QT "}]"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING
           .

      *> ============================================================
      *> PARSE-TOOL-CALL
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

           MOVE "arguments" TO WS-KEY-SEARCH
           PERFORM FIND-JSON-VAL
      *>   Unescape arguments JSON
           MOVE WS-JVAL TO WS-ESC-IN
           PERFORM JSON-UNESCAPE-STR
           MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
               TO WS-TOOL-ARGS

           DISPLAY "  Tool ID: "
               TRIM(WS-TOOL-CALL-ID)
           DISPLAY "  Args: "
               TRIM(WS-TOOL-ARGS)(1:200)
           .

      *> ============================================================
      *> JSON-ESCAPE-STR
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

      *> ============================================================
      *> JSON-UNESCAPE-STR
      *> ============================================================
       JSON-UNESCAPE-STR.
           MOVE SPACES TO WS-ESC-OUT
           MOVE 0 TO WS-ESC-OLEN
           MOVE LENGTH(TRIM(WS-ESC-IN))
               TO WS-ESC-ILEN

           IF WS-ESC-ILEN = 0
               EXIT PARAGRAPH
           END-IF

           MOVE 1 TO WS-ESC-I
           PERFORM UNTIL
               WS-ESC-I > WS-ESC-ILEN
               IF WS-ESC-IN(
                   WS-ESC-I:1) = X"5C"
               AND WS-ESC-I
                   < WS-ESC-ILEN
                   ADD 1 TO WS-ESC-I
                   ADD 1 TO WS-ESC-OLEN
                   MOVE WS-ESC-IN(
                       WS-ESC-I:1)
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
               ELSE
                   ADD 1 TO WS-ESC-OLEN
                   MOVE WS-ESC-IN(
                       WS-ESC-I:1)
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
               END-IF
               ADD 1 TO WS-ESC-I
           END-PERFORM
           .

      *> ============================================================
      *> READ-JSON-FILE
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
               READ WORK-FILE
                   INTO WS-LINE
                   AT END
                       MOVE "Y"
                           TO WS-EOF
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
                           MOVE WS-LINE(
                               1:WS-K)
                               TO WS-JBUF(
                               WS-JLEN
                               + 1:WS-K)
                           ADD WS-K
                               TO WS-JLEN
                       END-IF
               END-READ
           END-PERFORM

           CLOSE WORK-FILE
           MOVE "N" TO WS-EOF
           .

      *> ============================================================
      *> FIND-JSON-VAL
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
