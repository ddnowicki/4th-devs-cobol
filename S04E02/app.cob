       IDENTIFICATION DIVISION.
       PROGRAM-ID. S04E02-WINDPOWER.
      *> ============================================================
      *> S04E02 - Windpower: Configure wind turbine schedule
      *> 1. Phase 1 (no time limit): fetch help+docs, LLM extracts
      *>    turbine rules via function calling
      *> 2. Phase 2 (40s): start -> queue weather+turbinecheck ->
      *>    event-driven poll -> classify -> queue unlock codes ->
      *>    poll codes -> batch config -> done
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
       01  WS-HUB-KEY              PIC X(100).
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
       01  WS-HUB-BODY             PIC X(8000).

      *> -- JSON buffer for parsing --
       01  WS-JBUF                 PIC X(32000).
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
       01  WS-TASK-NAME            PIC X(20)
                                   VALUE "windpower".

      *> -- JSON escape I/O --
       01  WS-ESC-IN               PIC X(16000).
       01  WS-ESC-OUT              PIC X(32000).
       01  WS-ESC-ILEN             PIC 9(5).
       01  WS-ESC-OLEN             PIC 9(5).
       01  WS-ESC-I                PIC 9(5).

      *> -- Loop/misc --
       01  WS-I                    PIC 9(5).
       01  WS-K                    PIC 9(5).
       01  WS-EOF                  PIC X VALUE "N".
       01  WS-LINE                 PIC X(16000).
       01  WS-TALLY-CNT            PIC 9(4).

      *> -- Flag tracking --
       01  WS-FLAG-FOUND           PIC X VALUE "N".

      *> -- Help + docs (escaped for LLM) --
       01  WS-HELP-ESC             PIC X(8000).
       01  WS-HELP-ESC-LEN         PIC 9(5).
       01  WS-DOC-ESC              PIC X(8000).
       01  WS-DOC-ESC-LEN          PIC 9(5).

      *> -- Tool call parsing --
       01  WS-TOOL-NAME            PIC X(50).
       01  WS-TOOL-CALL-ID         PIC X(100).
       01  WS-TOOL-ARGS            PIC X(2000).

      *> -- Turbine rules (from LLM) --
       01  WS-STORM-THRESH         PIC 9(3)V9
                                   VALUE 14.0.
       01  WS-MIN-PROD-WIND        PIC 9(3)V9
                                   VALUE 4.0.
       01  WS-STORM-PITCH          PIC 9(3) VALUE 90.
       01  WS-PROD-PITCH           PIC 9(3) VALUE 0.
       01  WS-STORM-MODE           PIC X(20)
                                   VALUE "idle".
       01  WS-PROD-MODE            PIC X(20)
                                   VALUE "production".

      *> -- Forecast data (up to 100 entries) --
       01  WS-FC-COUNT             PIC 9(3) VALUE 0.
       01  WS-FORECASTS.
           05 WS-FC OCCURS 100 TIMES.
              10 WS-FC-TS          PIC X(20).
              10 WS-FC-WIND        PIC 9(3)V9.
              10 WS-FC-CLASS       PIC X(12).

      *> -- Config slots to send --
       01  WS-CFG-COUNT            PIC 9(3) VALUE 0.
       01  WS-CONFIGS.
           05 WS-CFG OCCURS 100 TIMES.
              10 WS-CFG-DATE       PIC X(11).
              10 WS-CFG-HOUR       PIC X(9).
              10 WS-CFG-PITCH      PIC 9(3).
              10 WS-CFG-MODE       PIC X(20).
              10 WS-CFG-CODE       PIC X(64).

      *> -- Unlock codes map --
       01  WS-UNL-COUNT            PIC 9(3) VALUE 0.
       01  WS-UNLOCKS.
           05 WS-UNL OCCURS 100 TIMES.
              10 WS-UNL-KEY        PIC X(20).
              10 WS-UNL-CODE       PIC X(64).

      *> -- Polling state --
       01  WS-GOT-WEATHER          PIC X VALUE "N".
       01  WS-GOT-TURBINE          PIC X VALUE "N".
       01  WS-CODES-QUEUED         PIC X VALUE "N".
       01  WS-EXPECTED-CODES       PIC 9(3) VALUE 0.
       01  WS-API-CODE             PIC S9(5).

      *> -- Temp for numeric parsing --
       01  WS-NUM-STR              PIC X(20).
       01  WS-NUM-VAL              PIC 9(5)V9.
       01  WS-NUM-INT              PIC 9(5).

      *> -- Forecast parsing helpers --
       01  WS-FP-POS               PIC 9(5).
       01  WS-FP-START             PIC 9(5).
       01  WS-FP-END               PIC 9(5).
       01  WS-FP-DEPTH             PIC 9(3).
       01  WS-FP-TS                PIC X(20).
       01  WS-FP-WIND              PIC X(20).
       01  WS-BEST-PROD-IDX        PIC 9(3).
       01  WS-BEST-PROD-WIND       PIC 9(3)V9.

      *> -- Config batch JSON buffer --
       01  WS-CFG-JSON             PIC X(8000).
       01  WS-CFG-PTR              PIC 9(5).

      *> -- Conversation buffer for LLM --
       01  WS-CONV-BUF             PIC X(32000).
       01  WS-CONV-PTR             PIC 9(5).

      *> -- Retry counter --
       01  WS-RETRY-CT             PIC 9(2) VALUE 0.

      *> -- JBUF save/restore --
       01  WS-JBUF-SAVE            PIC X(32000).
       01  WS-JLEN-SAVE            PIC 9(5).

      *> -- Parsed numeric from tool args --
       01  WS-PA-NUM-STR           PIC X(20).

      *> -- Source function check --
       01  WS-SRC-FUNC             PIC X(50).

      *> -- Wind decimal format --
       01  WS-WIND-FMT             PIC Z(4)9.9.

      *> -- Integer without leading zeros --
       01  WS-INT-FMT              PIC Z(4)9.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S04E02 WINDPOWER - COBOL ==="

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

      *>   Phase 1: Analyze documentation
           DISPLAY " "
           DISPLAY "[PHASE 1] Analyze documentation"
           PERFORM PHASE1-ANALYZE

      *>   Phase 2: Timed execution (retry up to 3x)
           MOVE 0 TO WS-RETRY-CT
           PERFORM UNTIL WS-FLAG-FOUND = "Y"
               OR WS-RETRY-CT >= 3
               ADD 1 TO WS-RETRY-CT
               DISPLAY " "
               DISPLAY "[PHASE 2] Attempt "
                   WS-RETRY-CT
               CALL "C$SLEEP" USING 2
               PERFORM PHASE2-EXECUTE
           END-PERFORM

           DISPLAY " "
           IF WS-FLAG-FOUND = "Y"
               DISPLAY ">>> FLAG FOUND <<<"
           ELSE
               DISPLAY "No flag found."
           END-IF
           DISPLAY "=== DONE ==="
           STOP RUN.

      *> ============================================================
      *> PHASE1-ANALYZE: Fetch help+docs, LLM extracts rules
      *> ============================================================
       PHASE1-ANALYZE.
      *>   1. Fetch help
           DISPLAY " "
           DISPLAY "  [1.1] Fetching help..."
           PERFORM API-CALL-HELP

      *>   Store help (escaped)
           IF WS-JLEN > 8000
               MOVE 8000 TO WS-JLEN
           END-IF
           MOVE WS-JBUF(1:WS-JLEN) TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR
           IF WS-ESC-OLEN > 8000
               MOVE 8000 TO WS-ESC-OLEN
           END-IF
           MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
               TO WS-HELP-ESC
           MOVE WS-ESC-OLEN TO WS-HELP-ESC-LEN

      *>   2. Start temp session + fetch docs
           DISPLAY "  [1.2] Fetching docs..."
           PERFORM API-CALL-START
           PERFORM API-CALL-GET-DOC

      *>   Store docs (escaped)
           IF WS-JLEN > 8000
               MOVE 8000 TO WS-JLEN
           END-IF
           MOVE WS-JBUF(1:WS-JLEN) TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR
           IF WS-ESC-OLEN > 8000
               MOVE 8000 TO WS-ESC-OLEN
           END-IF
           MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
               TO WS-DOC-ESC
           MOVE WS-ESC-OLEN TO WS-DOC-ESC-LEN

      *>   3. LLM extraction
           DISPLAY "  [1.3] LLM extracting rules..."
           PERFORM LLM-EXTRACT-RULES

           DISPLAY "  Rules extracted:"
           DISPLAY "    storm_thresh="
               WS-STORM-THRESH
           DISPLAY "    min_prod_wind="
               WS-MIN-PROD-WIND
           DISPLAY "    storm_pitch="
               WS-STORM-PITCH
           DISPLAY "    prod_pitch="
               WS-PROD-PITCH
           DISPLAY "    storm_mode="
               TRIM(WS-STORM-MODE)
           DISPLAY "    prod_mode="
               TRIM(WS-PROD-MODE)
           .

      *> ============================================================
      *> PHASE2-EXECUTE: Single timed session
      *> ============================================================
       PHASE2-EXECUTE.
      *>   Reset state
           MOVE 0 TO WS-FC-COUNT
           MOVE 0 TO WS-CFG-COUNT
           MOVE 0 TO WS-UNL-COUNT
           MOVE "N" TO WS-GOT-WEATHER
           MOVE "N" TO WS-GOT-TURBINE
           MOVE "N" TO WS-CODES-QUEUED
           MOVE 0 TO WS-EXPECTED-CODES

      *>   Step 1: Start fresh session
           DISPLAY " "
           DISPLAY "  [2.1] Starting session..."
           PERFORM API-CALL-START
           DISPLAY "  Start resp: "
               WS-JBUF(1:300)

      *>   Step 2: Queue weather + turbinecheck
           DISPLAY "  [2.2] Queuing weather+check..."
           PERFORM API-CALL-GET-WEATHER
           PERFORM API-CALL-GET-TURBINE

      *>   Step 3: Event-driven polling
           DISPLAY "  [2.3] Polling..."
           MOVE "N" TO WS-GOT-WEATHER
           MOVE "N" TO WS-GOT-TURBINE
           MOVE "N" TO WS-CODES-QUEUED
           MOVE 0 TO WS-UNL-COUNT
           MOVE 0 TO WS-RETRY-CT

           PERFORM UNTIL WS-RETRY-CT > 60
               ADD 1 TO WS-RETRY-CT

               PERFORM API-CALL-GETRESULT

      *>       Parse code field
               MOVE "code" TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               MOVE 0 TO WS-API-CODE
               IF TRIM(WS-JVAL) NOT = SPACES
                   COMPUTE WS-API-CODE =
                       NUMVAL(TRIM(WS-JVAL))
               END-IF

      *>       Code 11 = not ready
               IF WS-API-CODE = 11
                   CALL "C$SLEEP" USING 1
                   EXIT PERFORM CYCLE
               END-IF

      *>       Session timeout = fatal
               IF WS-API-CODE = -805
                   DISPLAY "  SESSION TIMEOUT!"
                   EXIT PERFORM
               END-IF

               DISPLAY "  Result (code="
                   WS-API-CODE "): "
                   WS-JBUF(1:300)

      *>       Check sourceFunction
               MOVE SPACES TO WS-SRC-FUNC
               MOVE "sourceFunction"
                   TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               IF TRIM(WS-JVAL)
                   NOT = SPACES
                   MOVE TRIM(WS-JVAL)
                       TO WS-SRC-FUNC
               END-IF

      *>       Check for forecast (weather)
               IF WS-GOT-WEATHER = "N"
                   MOVE 0 TO WS-TALLY-CNT
                   INSPECT WS-JBUF(1:WS-JLEN)
                       TALLYING WS-TALLY-CNT
                       FOR ALL '"forecast"'
                   IF WS-TALLY-CNT > 0
                       DISPLAY "  -> WEATHER!"
                       PERFORM PARSE-FORECAST
                       MOVE "Y"
                           TO WS-GOT-WEATHER

      *>               Classify + queue codes
                       DISPLAY "  [2.4] Classify"
                       PERFORM CLASSIFY-WEATHER
                       DISPLAY "  [2.5] Queue "
                           WS-CFG-COUNT " codes"
                       PERFORM QUEUE-UNLOCK-CODES
                       MOVE "Y"
                           TO WS-CODES-QUEUED
                       MOVE WS-CFG-COUNT
                           TO WS-EXPECTED-CODES
                       EXIT PERFORM CYCLE
                   END-IF
               END-IF

      *>       Check for unlockCode
               IF TRIM(WS-SRC-FUNC)
                   = "unlockCodeGenerator"
                   IF WS-CODES-QUEUED = "Y"
                       PERFORM
                           STORE-UNLOCK-CODE
                       DISPLAY "  -> UNLOCK #"
                           WS-UNL-COUNT
      *>               All codes collected?
                       IF WS-UNL-COUNT
                           >= WS-EXPECTED-CODES
                           DISPLAY
                             "  All codes!"
                           EXIT PERFORM
                       END-IF
                   ELSE
                       DISPLAY
                           "  Skip stale unlock"
                   END-IF
                   EXIT PERFORM CYCLE
               END-IF

      *>       Check for turbinecheck
               IF TRIM(WS-SRC-FUNC)
                   = "turbinecheck"
                   MOVE "Y"
                       TO WS-GOT-TURBINE
                   DISPLAY "  -> TURBINECHECK"
               ELSE
      *>           Check for unlockCode w/o src
                   MOVE 0 TO WS-TALLY-CNT
                   INSPECT WS-JBUF(1:WS-JLEN)
                       TALLYING WS-TALLY-CNT
                       FOR ALL '"unlockCode"'
                   IF WS-TALLY-CNT > 0
                   AND WS-CODES-QUEUED = "Y"
                       PERFORM
                           STORE-UNLOCK-CODE
                       DISPLAY "  -> UNLOCK #"
                           WS-UNL-COUNT
                       IF WS-UNL-COUNT
                           >= WS-EXPECTED-CODES
                           EXIT PERFORM
                       END-IF
                   ELSE
                       DISPLAY "  -> OTHER: "
                           TRIM(WS-SRC-FUNC)
                   END-IF
               END-IF

      *>       If all codes done, exit
               IF WS-CODES-QUEUED = "Y"
               AND WS-UNL-COUNT
                   >= WS-EXPECTED-CODES
                   EXIT PERFORM
               END-IF
           END-PERFORM

           DISPLAY "  Weather="
               WS-GOT-WEATHER
               " codes="
               WS-UNL-COUNT "/" WS-EXPECTED-CODES

           IF WS-GOT-WEATHER = "N"
           OR WS-CFG-COUNT = 0
               DISPLAY "  ERROR: no weather!"
               EXIT PARAGRAPH
           END-IF

      *>   Step 4: Match unlock codes to config
           PERFORM MATCH-CODES-TO-CONFIG

      *>   Step 5: Send batch config
           DISPLAY "  [2.6] Sending config..."
           PERFORM SEND-BATCH-CONFIG

      *>   Step 6: Done
           DISPLAY "  [2.7] Calling done..."
           PERFORM API-CALL-DONE

      *>   Check for flag
           MOVE 0 TO WS-TALLY-CNT
           IF WS-JLEN > 0
               INSPECT WS-JBUF(1:WS-JLEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL "FLG"
           END-IF
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-FLAG-FOUND
               DISPLAY "  >>> FLAG: "
                   WS-JBUF(1:500)
           ELSE
               DISPLAY "  No flag: "
                   WS-JBUF(1:500)
           END-IF
           .

      *> ============================================================
      *> API-CALL-HELP: action=help
      *> ============================================================
       API-CALL-HELP.
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
           PERFORM SEND-HUB-REQUEST
           DISPLAY "  Help: "
               WS-JBUF(1:400)
           .

      *> ============================================================
      *> API-CALL-START: action=start
      *> ============================================================
       API-CALL-START.
           MOVE SPACES TO WS-HUB-BODY
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME) WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT "start" WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
           END-STRING
           PERFORM SEND-HUB-REQUEST
           .

      *> ============================================================
      *> API-CALL-GET-DOC: action=get, param=documentation
      *> ============================================================
       API-CALL-GET-DOC.
           MOVE SPACES TO WS-HUB-BODY
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME) WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT "get" WS-QT ","
               WS-QT "param" WS-QT ":"
               WS-QT "documentation" WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
           END-STRING
           PERFORM SEND-HUB-REQUEST
           DISPLAY "  Doc: "
               WS-JBUF(1:400)
           .

      *> ============================================================
      *> API-CALL-GET-WEATHER: action=get, param=weather
      *> ============================================================
       API-CALL-GET-WEATHER.
           MOVE SPACES TO WS-HUB-BODY
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME) WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT "get" WS-QT ","
               WS-QT "param" WS-QT ":"
               WS-QT "weather" WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
           END-STRING
           PERFORM SEND-HUB-REQUEST
           .

      *> ============================================================
      *> API-CALL-GET-TURBINE: action=get, param=turbinecheck
      *> ============================================================
       API-CALL-GET-TURBINE.
           MOVE SPACES TO WS-HUB-BODY
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME) WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT "get" WS-QT ","
               WS-QT "param" WS-QT ":"
               WS-QT "turbinecheck" WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
           END-STRING
           PERFORM SEND-HUB-REQUEST
           .

      *> ============================================================
      *> API-CALL-GETRESULT: action=getResult
      *> ============================================================
       API-CALL-GETRESULT.
           MOVE SPACES TO WS-HUB-BODY
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME) WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT "getResult" WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
           END-STRING
           PERFORM SEND-HUB-REQUEST
           .

      *> ============================================================
      *> API-CALL-UNLOCK: queue unlockCodeGenerator
      *> Uses WS-CFG-DATE, WS-CFG-HOUR, WS-FC-WIND,
      *>      WS-CFG-PITCH from current config slot
      *> ============================================================
       API-CALL-UNLOCK.
           MOVE SPACES TO WS-HUB-BODY
           MOVE 1 TO WS-PTR
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME) WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT "unlockCodeGenerator"
               WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "startDate" WS-QT ":"
               WS-QT
               TRIM(WS-CFG-DATE(WS-I))
               WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "startHour" WS-QT ":"
               WS-QT
               TRIM(WS-CFG-HOUR(WS-I))
               WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "windMs" WS-QT ":"
               TRIM(WS-NUM-STR)
               ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "pitchAngle" WS-QT ":"
               TRIM(WS-PA-NUM-STR)
               "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           PERFORM SEND-HUB-REQUEST
           .

      *> ============================================================
      *> API-CALL-DONE: action=done
      *> ============================================================
       API-CALL-DONE.
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
      *> LLM-EXTRACT-RULES: Use function calling to get rules
      *> ============================================================
       LLM-EXTRACT-RULES.
           MOVE SPACES TO WS-REQ-JSON
           MOVE 1 TO WS-PTR

      *>   Build request with tool
           STRING
               "{"
               WS-QT "model" WS-QT ":"
               WS-QT "gpt-4.1-mini" WS-QT ","
               WS-QT "temperature" WS-QT
               ":0,"
               WS-QT "messages" WS-QT ":["
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   System message
           STRING
               "{" WS-QT "role" WS-QT ":"
               WS-QT "system" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               "You analyze wind turbine"
               " API docs. Extract exact"
               " config rules: wind speed"
               " thresholds, pitch angles"
               ", turbine modes. Call the"
               " report_turbine_rules "
               "function."
               WS-QT "},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   User message with help + docs
           STRING
               "{" WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT "API Help:" WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           IF WS-HELP-ESC-LEN > 0
               STRING
                   WS-HELP-ESC(
                   1:WS-HELP-ESC-LEN)
                   DELIMITED SIZE
                   INTO WS-REQ-JSON
                   WITH POINTER WS-PTR
               END-STRING
           END-IF

           STRING
               WS-NL WS-NL
               "Documentation:" WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           IF WS-DOC-ESC-LEN > 0
               STRING
                   WS-DOC-ESC(
                   1:WS-DOC-ESC-LEN)
                   DELIMITED SIZE
                   INTO WS-REQ-JSON
                   WITH POINTER WS-PTR
               END-STRING
           END-IF

           STRING
               WS-QT "}],"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Tool definition
           STRING
               WS-QT "tools" WS-QT ":["
               "{" WS-QT "type" WS-QT ":"
               WS-QT "function" WS-QT ","
               WS-QT "function" WS-QT ":{"
               WS-QT "name" WS-QT ":"
               WS-QT "report_turbine_rules"
               WS-QT ","
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "description" WS-QT ":"
               WS-QT "Report extracted wind"
               " turbine config rules"
               WS-QT ","
               WS-QT "parameters" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "object" WS-QT ","
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "properties" WS-QT ":{"
               WS-QT "storm_threshold_ms"
               WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "number" WS-QT "},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "min_production_wind"
               "_ms" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "number" WS-QT "},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "storm_pitch_angle"
               WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "integer" WS-QT "},"
               WS-QT "production_pitch"
               "_angle" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "integer" WS-QT "},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "storm_mode" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "string" WS-QT "},"
               WS-QT "production_mode"
               WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "string" WS-QT "}},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "required" WS-QT ":["
               WS-QT "storm_threshold_ms"
               WS-QT ","
               WS-QT "min_production_wind"
               "_ms" WS-QT ","
               WS-QT "storm_pitch_angle"
               WS-QT ","
               WS-QT "production_pitch"
               "_angle" WS-QT ","
               WS-QT "storm_mode" WS-QT ","
               WS-QT "production_mode"
               WS-QT "]}}}],"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "tool_choice" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "function" WS-QT ","
               WS-QT "function" WS-QT ":{"
               WS-QT "name" WS-QT ":"
               WS-QT "report_turbine_rules"
               WS-QT "}}}"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Write and send
           MOVE "llm_req.json"
               TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           WRITE WORK-REC FROM WS-REQ-JSON
           CLOSE WORK-FILE

           INITIALIZE WS-CMD
           STRING
               "curl -s "
               "-o llm_resp.json"
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
               " -d @llm_req.json"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           MOVE "llm_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           DISPLAY "  LLM resp: "
               WS-JBUF(1:500)

      *>   Parse tool_calls -> arguments
           PERFORM PARSE-TOOL-CALL

      *>   Now parse rule values from args
           MOVE WS-TOOL-ARGS TO WS-JBUF-SAVE
           MOVE LENGTH(TRIM(WS-TOOL-ARGS))
               TO WS-JLEN-SAVE

      *>   Temporarily use JBUF for arg parsing
           MOVE WS-TOOL-ARGS TO WS-JBUF
           MOVE WS-JLEN-SAVE TO WS-JLEN

      *>   storm_threshold_ms
           MOVE "storm_threshold_ms"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF TRIM(WS-JVAL) NOT = SPACES
               COMPUTE WS-STORM-THRESH =
                   NUMVAL(TRIM(WS-JVAL))
           END-IF

      *>   min_production_wind_ms
           MOVE "min_production_wind_ms"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF TRIM(WS-JVAL) NOT = SPACES
               COMPUTE WS-MIN-PROD-WIND =
                   NUMVAL(TRIM(WS-JVAL))
           END-IF

      *>   storm_pitch_angle
           MOVE "storm_pitch_angle"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF TRIM(WS-JVAL) NOT = SPACES
               COMPUTE WS-STORM-PITCH =
                   NUMVAL(TRIM(WS-JVAL))
           END-IF

      *>   production_pitch_angle
           MOVE "production_pitch_angle"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF TRIM(WS-JVAL) NOT = SPACES
               COMPUTE WS-PROD-PITCH =
                   NUMVAL(TRIM(WS-JVAL))
           END-IF

      *>   storm_mode
           MOVE "storm_mode"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF TRIM(WS-JVAL) NOT = SPACES
               MOVE TRIM(WS-JVAL)
                   TO WS-STORM-MODE
           END-IF

      *>   production_mode
           MOVE "production_mode"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF TRIM(WS-JVAL) NOT = SPACES
               MOVE TRIM(WS-JVAL)
                   TO WS-PROD-MODE
           END-IF

      *>   Validate modes
           IF TRIM(WS-STORM-MODE) NOT = "idle"
           AND TRIM(WS-STORM-MODE)
               NOT = "production"
               MOVE "idle" TO WS-STORM-MODE
           END-IF
           IF TRIM(WS-PROD-MODE) NOT = "idle"
           AND TRIM(WS-PROD-MODE)
               NOT = "production"
               MOVE "production" TO WS-PROD-MODE
           END-IF
           .

      *> ============================================================
      *> PARSE-FORECAST: Extract forecast array from JBUF
      *> Format: "forecast":[{"timestamp":"...","windMs":N},...]
      *> ============================================================
       PARSE-FORECAST.
           MOVE 0 TO WS-FC-COUNT

      *>   Find "forecast"
           MOVE 0 TO WS-FP-POS
           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I > WS-JLEN - 10
               OR WS-FP-POS > 0
               IF WS-JBUF(WS-I:10)
                   = '"forecast"'
                   MOVE WS-I TO WS-FP-POS
               END-IF
           END-PERFORM

           IF WS-FP-POS = 0
      *>       Try inside message field
               MOVE "message" TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               IF TRIM(WS-JVAL) NOT = SPACES
      *>           Parse message as JSON
                   MOVE WS-JVAL TO WS-ESC-IN
                   PERFORM JSON-UNESCAPE-STR
                   MOVE WS-ESC-OUT(
                       1:WS-ESC-OLEN)
                       TO WS-JBUF
                   MOVE WS-ESC-OLEN
                       TO WS-JLEN
      *>           Try again
                   MOVE 0 TO WS-FP-POS
                   PERFORM VARYING WS-I
                       FROM 1 BY 1
                       UNTIL WS-I
                           > WS-JLEN - 10
                       OR WS-FP-POS > 0
                       IF WS-JBUF(WS-I:10)
                           = '"forecast"'
                           MOVE WS-I
                               TO WS-FP-POS
                       END-IF
                   END-PERFORM
               END-IF
           END-IF

           IF WS-FP-POS = 0
               DISPLAY "  No forecast found!"
               EXIT PARAGRAPH
           END-IF

      *>   Find [ after "forecast":
           COMPUTE WS-FP-POS =
               WS-FP-POS + 10
           PERFORM UNTIL WS-FP-POS > WS-JLEN
               OR WS-JBUF(WS-FP-POS:1) = "["
               ADD 1 TO WS-FP-POS
           END-PERFORM
           ADD 1 TO WS-FP-POS

      *>   Parse each {timestamp, windMs} object
           PERFORM UNTIL WS-FP-POS > WS-JLEN
               OR WS-FC-COUNT >= 100

      *>       Skip whitespace/commas
               PERFORM UNTIL WS-FP-POS
                   > WS-JLEN
                   OR (WS-JBUF(WS-FP-POS:1)
                       NOT = " "
                   AND WS-JBUF(WS-FP-POS:1)
                       NOT = ","
                   AND WS-JBUF(WS-FP-POS:1)
                       NOT = X"0A"
                   AND WS-JBUF(WS-FP-POS:1)
                       NOT = X"0D")
                   ADD 1 TO WS-FP-POS
               END-PERFORM

               IF WS-FP-POS > WS-JLEN
                   EXIT PERFORM
               END-IF
               IF WS-JBUF(WS-FP-POS:1) = "]"
                   EXIT PERFORM
               END-IF
               IF WS-JBUF(WS-FP-POS:1)
                   NOT = "{"
                   ADD 1 TO WS-FP-POS
                   EXIT PERFORM CYCLE
               END-IF

      *>       Find end of this object
               MOVE 1 TO WS-FP-DEPTH
               MOVE WS-FP-POS TO WS-FP-START
               ADD 1 TO WS-FP-POS
               PERFORM UNTIL WS-FP-POS
                   > WS-JLEN
                   OR WS-FP-DEPTH = 0
                   IF WS-JBUF(WS-FP-POS:1)
                       = "{"
                       ADD 1 TO WS-FP-DEPTH
                   END-IF
                   IF WS-JBUF(WS-FP-POS:1)
                       = "}"
                       SUBTRACT 1
                           FROM WS-FP-DEPTH
                   END-IF
                   ADD 1 TO WS-FP-POS
               END-PERFORM
               MOVE WS-FP-POS TO WS-FP-END

      *>       Parse timestamp + windMs within obj
               MOVE SPACES TO WS-FP-TS
               MOVE SPACES TO WS-FP-WIND

      *>       Use FIND-JSON-VAL with range
               MOVE WS-JBUF(WS-FP-START:
                   WS-FP-END - WS-FP-START)
                   TO WS-TMP
               MOVE WS-JBUF TO WS-JBUF-SAVE
               MOVE WS-JLEN TO WS-JLEN-SAVE
               MOVE WS-TMP TO WS-JBUF
               COMPUTE WS-JLEN =
                   WS-FP-END - WS-FP-START

               MOVE "timestamp"
                   TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               MOVE TRIM(WS-JVAL)
                   TO WS-FP-TS

               MOVE "windMs"
                   TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               MOVE TRIM(WS-JVAL)
                   TO WS-FP-WIND

      *>       Restore JBUF
               MOVE WS-JBUF-SAVE TO WS-JBUF
               MOVE WS-JLEN-SAVE TO WS-JLEN

      *>       Store entry
               IF TRIM(WS-FP-TS)
                   NOT = SPACES
               AND TRIM(WS-FP-WIND)
                   NOT = SPACES
                   ADD 1 TO WS-FC-COUNT
                   MOVE TRIM(WS-FP-TS)
                       TO WS-FC-TS(
                       WS-FC-COUNT)
                   COMPUTE WS-FC-WIND(
                       WS-FC-COUNT) =
                       NUMVAL(
                       TRIM(WS-FP-WIND))
                   MOVE SPACES
                       TO WS-FC-CLASS(
                       WS-FC-COUNT)
                   DISPLAY "    FC#"
                       WS-FC-COUNT ": "
                       TRIM(WS-FC-TS(
                           WS-FC-COUNT))
                       " wind="
                       WS-FC-WIND(
                           WS-FC-COUNT)
               END-IF
           END-PERFORM

           DISPLAY "  Parsed " WS-FC-COUNT
               " forecast entries"
           .

      *> ============================================================
      *> CLASSIFY-WEATHER: Deterministic classification
      *> ============================================================
       CLASSIFY-WEATHER.
           MOVE 0 TO WS-CFG-COUNT
           MOVE 0 TO WS-BEST-PROD-IDX
           MOVE 0 TO WS-BEST-PROD-WIND

           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I > WS-FC-COUNT

      *>       Storm?
               IF WS-FC-WIND(WS-I)
                   > WS-STORM-THRESH
                   MOVE "storm"
                       TO WS-FC-CLASS(WS-I)
                   ADD 1 TO WS-CFG-COUNT
                   PERFORM PARSE-TIMESTAMP
                   MOVE WS-CFG-DATE(
                       WS-CFG-COUNT)
                       TO WS-CFG-DATE(
                       WS-CFG-COUNT)
                   MOVE WS-STORM-PITCH
                       TO WS-CFG-PITCH(
                       WS-CFG-COUNT)
                   MOVE WS-STORM-MODE
                       TO WS-CFG-MODE(
                       WS-CFG-COUNT)
                   MOVE SPACES
                       TO WS-CFG-CODE(
                       WS-CFG-COUNT)
                   DISPLAY "    STORM: "
                       TRIM(WS-FC-TS(WS-I))
                       " " WS-FC-WIND(WS-I)
                       " m/s"
               ELSE
      *>           Productive?
                   IF WS-FC-WIND(WS-I)
                       >= WS-MIN-PROD-WIND
                       MOVE "productive"
                           TO WS-FC-CLASS(
                           WS-I)
      *>               Track best
                       IF WS-FC-WIND(WS-I)
                           > WS-BEST-PROD-WIND
                           MOVE WS-FC-WIND(
                               WS-I)
                               TO
                               WS-BEST-PROD-WIND
                           MOVE WS-I
                               TO
                               WS-BEST-PROD-IDX
                       END-IF
                   ELSE
                       MOVE "idle"
                           TO WS-FC-CLASS(
                           WS-I)
                   END-IF
               END-IF
           END-PERFORM

      *>   Add best production slot
           IF WS-BEST-PROD-IDX > 0
               ADD 1 TO WS-CFG-COUNT
               MOVE WS-BEST-PROD-IDX TO WS-I
               PERFORM PARSE-TIMESTAMP
               MOVE WS-PROD-PITCH
                   TO WS-CFG-PITCH(
                   WS-CFG-COUNT)
               MOVE WS-PROD-MODE
                   TO WS-CFG-MODE(
                   WS-CFG-COUNT)
               MOVE SPACES
                   TO WS-CFG-CODE(
                   WS-CFG-COUNT)
               DISPLAY "    BEST PROD: "
                   TRIM(WS-FC-TS(
                       WS-BEST-PROD-IDX))
                   " " WS-BEST-PROD-WIND
                   " m/s"
           END-IF

           DISPLAY "  Total config slots: "
               WS-CFG-COUNT
           .

      *> ============================================================
      *> PARSE-TIMESTAMP: Split WS-FC-TS(WS-I) into
      *> WS-CFG-DATE and WS-CFG-HOUR for WS-CFG-COUNT
      *> ============================================================
       PARSE-TIMESTAMP.
           MOVE SPACES TO WS-CFG-DATE(
               WS-CFG-COUNT)
           MOVE SPACES TO WS-CFG-HOUR(
               WS-CFG-COUNT)

      *>   Find space separator in timestamp
           MOVE 0 TO WS-K
           PERFORM VARYING WS-FP-POS
               FROM 1 BY 1
               UNTIL WS-FP-POS > 20
               OR WS-K > 0
               IF WS-FC-TS(WS-I)(
                   WS-FP-POS:1) = " "
                   MOVE WS-FP-POS TO WS-K
               END-IF
           END-PERFORM

           IF WS-K > 1
      *>       Date = before space
               MOVE WS-FC-TS(WS-I)(1:
                   WS-K - 1)
                   TO WS-CFG-DATE(
                   WS-CFG-COUNT)
      *>       Hour = after space
               COMPUTE WS-FP-POS = WS-K + 1
               MOVE WS-FC-TS(WS-I)(
                   WS-FP-POS:8)
                   TO WS-CFG-HOUR(
                   WS-CFG-COUNT)
           ELSE
      *>       Try T separator
               MOVE 0 TO WS-K
               PERFORM VARYING WS-FP-POS
                   FROM 1 BY 1
                   UNTIL WS-FP-POS > 20
                   OR WS-K > 0
                   IF WS-FC-TS(WS-I)(
                       WS-FP-POS:1) = "T"
                       MOVE WS-FP-POS
                           TO WS-K
                   END-IF
               END-PERFORM
               IF WS-K > 1
                   MOVE WS-FC-TS(WS-I)(1:
                       WS-K - 1)
                       TO WS-CFG-DATE(
                       WS-CFG-COUNT)
                   COMPUTE WS-FP-POS =
                       WS-K + 1
                   MOVE WS-FC-TS(WS-I)(
                       WS-FP-POS:8)
                       TO WS-CFG-HOUR(
                       WS-CFG-COUNT)
               ELSE
                   MOVE TRIM(
                       WS-FC-TS(WS-I))
                       TO WS-CFG-DATE(
                       WS-CFG-COUNT)
                   MOVE "00:00:00"
                       TO WS-CFG-HOUR(
                       WS-CFG-COUNT)
               END-IF
           END-IF

      *>   Ensure hour has seconds
           IF WS-CFG-HOUR(WS-CFG-COUNT)
               (6:1) = SPACES
           OR WS-CFG-HOUR(WS-CFG-COUNT)
               (6:1) = LOW-VALUES
               STRING TRIM(
                   WS-CFG-HOUR(WS-CFG-COUNT))
                   ":00"
                   DELIMITED SIZE
                   INTO WS-CFG-HOUR(
                   WS-CFG-COUNT)
               END-STRING
           END-IF
           .

      *> ============================================================
      *> QUEUE-UNLOCK-CODES: Queue unlockCodeGenerator
      *>                     for each config slot
      *> ============================================================
       QUEUE-UNLOCK-CODES.
           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I > WS-CFG-COUNT

      *>       Build windMs as number string
               MOVE SPACES TO WS-NUM-STR
               MOVE WS-FC-WIND(
                   WS-BEST-PROD-IDX)
                   TO WS-NUM-VAL

      *>       Find the right forecast entry
               PERFORM FIND-WIND-FOR-SLOT

               MOVE SPACES TO WS-PA-NUM-STR
               MOVE WS-CFG-PITCH(WS-I)
                   TO WS-INT-FMT
               STRING TRIM(WS-INT-FMT)
                   DELIMITED SIZE
                   INTO WS-PA-NUM-STR
               END-STRING

               DISPLAY "    Queue unlock #"
                   WS-I ": "
                   TRIM(WS-CFG-DATE(WS-I))
                   " " TRIM(WS-CFG-HOUR(
                   WS-I))
                   " wind=" TRIM(WS-NUM-STR)
                   " pitch="
                   TRIM(WS-PA-NUM-STR)
               PERFORM API-CALL-UNLOCK
           END-PERFORM
           .

      *> ============================================================
      *> FIND-WIND-FOR-SLOT: Find wind value matching
      *> current config slot WS-I, put into WS-NUM-STR
      *> ============================================================
       FIND-WIND-FOR-SLOT.
           MOVE SPACES TO WS-NUM-STR
           MOVE 0 TO WS-NUM-VAL

      *>   Match by date+hour to forecast entry
           PERFORM VARYING WS-K
               FROM 1 BY 1
               UNTIL WS-K > WS-FC-COUNT
               MOVE SPACES TO WS-TMP
               STRING
                   TRIM(WS-CFG-DATE(WS-I))
                   " "
                   TRIM(WS-CFG-HOUR(WS-I))
                   DELIMITED SIZE
                   INTO WS-TMP
               END-STRING
               IF TRIM(WS-FC-TS(WS-K))
                   = TRIM(WS-TMP)
                   MOVE WS-FC-WIND(WS-K)
                       TO WS-NUM-VAL
                   EXIT PERFORM
               END-IF
           END-PERFORM

      *>   Format wind as decimal for API
           MOVE WS-NUM-VAL TO WS-WIND-FMT
           MOVE SPACES TO WS-NUM-STR
           STRING TRIM(WS-WIND-FMT)
               DELIMITED SIZE
               INTO WS-NUM-STR
           END-STRING
           .

      *> ============================================================
      *> STORE-UNLOCK-CODE: Parse unlockCode from JBUF
      *> ============================================================
       STORE-UNLOCK-CODE.
      *>   Extract unlockCode
           MOVE "unlockCode" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF TRIM(WS-JVAL) = SPACES
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO WS-UNL-COUNT

           MOVE TRIM(WS-JVAL)
               TO WS-UNL-CODE(WS-UNL-COUNT)

      *>   Extract key from signedParams
           MOVE SPACES TO WS-TMP
           MOVE "startDate" TO WS-KEY-SEARCH
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL) TO WS-TMP

           MOVE SPACES TO WS-TMP2
           MOVE "startHour" TO WS-KEY-SEARCH
           PERFORM FIND-JSON-VAL

      *>   Build key: "date hour"
           MOVE SPACES TO
               WS-UNL-KEY(WS-UNL-COUNT)
           STRING TRIM(WS-TMP)
               " "
               TRIM(WS-JVAL)
               DELIMITED SIZE
               INTO WS-UNL-KEY(
               WS-UNL-COUNT)
           END-STRING

           DISPLAY "    Unlock "
               TRIM(WS-UNL-KEY(
                   WS-UNL-COUNT))
               ": "
               WS-UNL-CODE(
                   WS-UNL-COUNT)(1:32)
               "..."
           .

      *> ============================================================
      *> MATCH-CODES-TO-CONFIG: Match unlock codes to slots
      *> ============================================================
       MATCH-CODES-TO-CONFIG.
           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I > WS-CFG-COUNT

      *>       Build key for this config
               MOVE SPACES TO WS-TMP
               STRING TRIM(WS-CFG-DATE(WS-I))
                   " "
                   TRIM(WS-CFG-HOUR(WS-I))
                   DELIMITED SIZE
                   INTO WS-TMP
               END-STRING

      *>       Find matching unlock code
               PERFORM VARYING WS-K
                   FROM 1 BY 1
                   UNTIL WS-K > WS-UNL-COUNT
                   IF TRIM(WS-UNL-KEY(WS-K))
                       = TRIM(WS-TMP)
                       MOVE WS-UNL-CODE(
                           WS-K)
                           TO WS-CFG-CODE(
                           WS-I)
                       EXIT PERFORM
                   END-IF
               END-PERFORM

               IF WS-CFG-CODE(WS-I) = SPACES
                   DISPLAY "  WARNING: no code"
                       " for " TRIM(WS-TMP)
               ELSE
                   DISPLAY "  Matched: "
                       TRIM(WS-TMP)
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> SEND-BATCH-CONFIG: Send all configs at once
      *> ============================================================
       SEND-BATCH-CONFIG.
           MOVE SPACES TO WS-HUB-BODY
           MOVE 1 TO WS-PTR

           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME) WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT "config" WS-QT ","
               WS-QT "configs" WS-QT ":{"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I > WS-CFG-COUNT

               IF WS-I > 1
                   STRING ","
                       DELIMITED SIZE
                       INTO WS-HUB-BODY
                       WITH POINTER WS-PTR
                   END-STRING
               END-IF

      *>       Key: "date hour"
               STRING
                   WS-QT
                   TRIM(WS-CFG-DATE(WS-I))
                   " "
                   TRIM(WS-CFG-HOUR(WS-I))
                   WS-QT ":{"
                   DELIMITED SIZE
                   INTO WS-HUB-BODY
                   WITH POINTER WS-PTR
               END-STRING

      *>       pitchAngle (numeric, no zeros)
               MOVE WS-CFG-PITCH(WS-I)
                   TO WS-INT-FMT
               STRING
                   WS-QT "pitchAngle"
                   WS-QT ":"
                   TRIM(WS-INT-FMT)
                   ","
                   DELIMITED SIZE
                   INTO WS-HUB-BODY
                   WITH POINTER WS-PTR
               END-STRING

      *>       turbineMode
               STRING
                   WS-QT "turbineMode"
                   WS-QT ":"
                   WS-QT
                   TRIM(WS-CFG-MODE(WS-I))
                   WS-QT ","
                   DELIMITED SIZE
                   INTO WS-HUB-BODY
                   WITH POINTER WS-PTR
               END-STRING

      *>       unlockCode
               STRING
                   WS-QT "unlockCode"
                   WS-QT ":"
                   WS-QT
                   TRIM(WS-CFG-CODE(WS-I))
                   WS-QT "}"
                   DELIMITED SIZE
                   INTO WS-HUB-BODY
                   WITH POINTER WS-PTR
               END-STRING
           END-PERFORM

           STRING "}}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           DISPLAY "  Config body: "
               WS-HUB-BODY(1:500)

           PERFORM SEND-HUB-REQUEST

           DISPLAY "  Config resp: "
               WS-JBUF(1:500)

      *>   If batch failed, try individual
           MOVE "code" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF TRIM(WS-JVAL) NOT = SPACES
               COMPUTE WS-API-CODE =
                   NUMVAL(TRIM(WS-JVAL))
               IF WS-API-CODE < 0
                   DISPLAY
                       "  Batch failed, "
                       "trying individual..."
                   PERFORM SEND-INDIVIDUAL-CFGS
               END-IF
           END-IF
           .

      *> ============================================================
      *> SEND-INDIVIDUAL-CFGS: Fallback individual config sends
      *> ============================================================
       SEND-INDIVIDUAL-CFGS.
           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I > WS-CFG-COUNT

               MOVE SPACES TO WS-HUB-BODY
               MOVE 1 TO WS-PTR

               STRING
                   "{" WS-QT "apikey" WS-QT
                   ":" WS-QT
                   TRIM(WS-HUB-KEY) WS-QT ","
                   WS-QT "task" WS-QT ":"
                   WS-QT TRIM(WS-TASK-NAME)
                   WS-QT ","
                   WS-QT "answer" WS-QT ":{"
                   WS-QT "action" WS-QT ":"
                   WS-QT "config" WS-QT ","
                   DELIMITED SIZE
                   INTO WS-HUB-BODY
                   WITH POINTER WS-PTR
               END-STRING

               STRING
                   WS-QT "startDate" WS-QT
                   ":" WS-QT
                   TRIM(WS-CFG-DATE(WS-I))
                   WS-QT ","
                   WS-QT "startHour" WS-QT
                   ":" WS-QT
                   TRIM(WS-CFG-HOUR(WS-I))
                   WS-QT ","
                   DELIMITED SIZE
                   INTO WS-HUB-BODY
                   WITH POINTER WS-PTR
               END-STRING

               MOVE WS-CFG-PITCH(WS-I)
                   TO WS-INT-FMT
               STRING
                   WS-QT "pitchAngle" WS-QT
                   ":" TRIM(WS-INT-FMT) ","
                   WS-QT "turbineMode" WS-QT
                   ":" WS-QT
                   TRIM(WS-CFG-MODE(WS-I))
                   WS-QT ","
                   DELIMITED SIZE
                   INTO WS-HUB-BODY
                   WITH POINTER WS-PTR
               END-STRING

               STRING
                   WS-QT "unlockCode" WS-QT
                   ":" WS-QT
                   TRIM(WS-CFG-CODE(WS-I))
                   WS-QT "}}"
                   DELIMITED SIZE
                   INTO WS-HUB-BODY
                   WITH POINTER WS-PTR
               END-STRING

               PERFORM SEND-HUB-REQUEST
               DISPLAY "  Cfg #" WS-I ": "
                   WS-JBUF(1:200)
           END-PERFORM
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
           .

      *> ============================================================
      *> PARSE-TOOL-CALL: Extract tool name + args from LLM
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
               IF WS-FJV-POS
                   + LENGTH(TRIM(WS-TMP2))
                   - 1 <= WS-JLEN
               AND WS-JBUF(WS-FJV-POS:
                   LENGTH(TRIM(WS-TMP2)))
                   = TRIM(WS-TMP2)
                   MOVE WS-FJV-POS
                       TO WS-KEY-POS
               END-IF
           END-PERFORM

           IF WS-KEY-POS = 0
               DISPLAY "  No tool_calls!"
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

           DISPLAY "  Tool: "
               TRIM(WS-TOOL-NAME)
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
                           IF WS-JLEN + WS-K
                               > 32000
                               COMPUTE WS-K =
                                   32000
                                   - WS-JLEN
                           END-IF
                           IF WS-K > 0
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
