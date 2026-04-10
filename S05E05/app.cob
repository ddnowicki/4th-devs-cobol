       IDENTIFICATION DIVISION.
       PROGRAM-ID. S05E05-TIMETRAVEL.
      *> ============================================================
      *> S05E05 - Time Travel: CHRONOS-P1 Configuration
      *> 1. Fetch timetravel.md docs
      *> 2. API help + reset
      *> 3. Execute 3 time jumps:
      *>    Jump 1: 2238-11-05 (future, PTB)
      *>    Jump 2: today (past from 2238, PTA)
      *>    Jump 3: 2024-11-12 (tunnel, PTA+PTB)
      *> 4. Each jump: configure date, syncRatio,
      *>    AI-interpret stabilization, set PWR/PT,
      *>    poll internalMode+fluxDensity, activate
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
      *> === Environment (via copybook) ===
       COPY ENVLOAD-WS.

      *> === File I/O ===
       01  WS-FS                   PIC XX.
       01  WS-WORK-PATH            PIC X(100)
                                   VALUE "work.tmp".

      *> === HTTP ===
       01  WS-CMD                  PIC X(4000).
       01  WS-BACKEND-URL          PIC X(200).

      *> === Shared copybooks (WS) ===
       COPY JSONPARSE-WS.
       COPY JSONREAD-WS.
       COPY JSONESCAPE-WS.
       COPY HUBSUBMIT-WS.

      *> === Task Configuration ===
       01  WS-TASK-NAME            PIC X(20)
                                   VALUE "timetravel".

      *> === Current date ===
       01  WS-CURR-DATE            PIC X(21).
       01  WS-CUR-YEAR             PIC 9(4).
       01  WS-CUR-MONTH            PIC 9(2).
       01  WS-CUR-DAY              PIC 9(2).

      *> === Jump table (3 jumps) ===
       01  WS-NUM-JUMPS            PIC 9(1) VALUE 3.
       01  WS-JUMP-IDX             PIC 9(1).

       01  WS-JUMP-TABLE.
         05  WS-JT OCCURS 3 TIMES.
           10  WS-JT-YEAR          PIC 9(4).
           10  WS-JT-MONTH         PIC 9(2).
           10  WS-JT-DAY           PIC 9(2).
           10  WS-JT-FROM-YEAR     PIC 9(4).
           10  WS-JT-TYPE          PIC X(6).
      *>       "jump" or "tunnel"
           10  WS-JT-PWR           PIC 9(3).

      *> === Current jump vars ===
       01  WS-YEAR                 PIC 9(4).
       01  WS-MONTH                PIC 9(2).
       01  WS-DAY                  PIC 9(2).
       01  WS-FROM-YEAR            PIC 9(4).
       01  WS-JUMP-TYPE            PIC X(6).
       01  WS-PWR                  PIC 9(3).

      *> === syncRatio computation ===
       01  WS-SYNC-RAW             PIC 9(10).
       01  WS-SYNC-MOD             PIC 9(3).
       01  WS-SYNC-STR             PIC X(10).
       01  WS-SYNC-INT             PIC 9(3).
       01  WS-SYNC-DEC             PIC 9(2).

      *> === PT switches ===
       01  WS-PTA                  PIC X(5).
       01  WS-PTB                  PIC X(5).

      *> === internalMode target ===
       01  WS-TARGET-MODE          PIC 9(1).
       01  WS-CUR-MODE             PIC X(10).

      *> === fluxDensity ===
       01  WS-FLUX-VAL             PIC X(10).

      *> === AI response / stabilization ===
       01  WS-STAB-VALUE           PIC X(200).
       01  WS-REQ-JSON             PIC X(32000).
       01  WS-AI-PROMPT            PIC X(16000).
       01  WS-AI-PROMPT-ESC        PIC X(16000).
       01  WS-AI-PROMPT-LEN        PIC 9(5).
       01  WS-CONFIG-RESP          PIC X(8000).
       01  WS-CONFIG-RESP-ESC      PIC X(16000).
       01  WS-CONFIG-LEN           PIC 9(5).
       01  WS-CONFIG-ESC-LEN       PIC 9(5).

      *> === Verify request body ===
       01  WS-VERIFY-BODY          PIC X(2000).

      *> === Backend request body ===
       01  WS-BE-BODY              PIC X(2000).

      *> === Control Flow ===
       01  WS-PTR                  PIC 9(5).
       01  WS-I                    PIC 9(5).
       01  WS-TALLY-CNT            PIC 9(4).
       01  WS-FLAG-FOUND           PIC X VALUE "N".
       01  WS-POLL-CT              PIC 9(2).
       01  WS-POLL-MAX             PIC 9(2) VALUE 30.
       01  WS-SLEEP-SEC            PIC 9(1) VALUE 1.
       01  WS-NUM-STR              PIC X(20).
       01  WS-YEAR-STR             PIC X(4).
       01  WS-MONTH-STR            PIC X(2).
       01  WS-DAY-STR              PIC X(2).
       01  WS-PWR-STR              PIC X(3).
       01  WS-MODE-MATCHED         PIC X VALUE "N".
       01  WS-FLUX-MATCHED         PIC X VALUE "N".

      *> === Sanitize stabilization ===
       01  WS-SAN-LEN              PIC 9(5).
       01  WS-SAN-I                PIC 9(5).
       01  WS-SAN-START            PIC 9(5).
       01  WS-SAN-END              PIC 9(5).
       01  WS-SAN-FOUND            PIC X VALUE "N".

      *> === Stabilization retry ===
       01  WS-STAB-RETRY           PIC 9(1).
       01  WS-STAB-OK              PIC X VALUE "N".
       01  WS-PREV-STAB            PIC X(200).
       01  WS-NEED-CFG             PIC X(2000).
       01  WS-NEED-CFG-ESC         PIC X(4000).
       01  WS-NEED-CFG-LEN         PIC 9(5).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S05E05 TIMETRAVEL ==="

           PERFORM LOAD-ENV-VARS

      *>   Build backend URL
           MOVE SPACES TO WS-BACKEND-URL
           STRING TRIM(WS-HUB-URL)
               "/timetravel_backend"
               DELIMITED SIZE
               INTO WS-BACKEND-URL
           END-STRING

      *>   Get current date
           MOVE CURRENT-DATE TO WS-CURR-DATE
           MOVE WS-CURR-DATE(1:4)
               TO WS-YEAR-STR
           MOVE WS-CURR-DATE(5:2)
               TO WS-MONTH-STR
           MOVE WS-CURR-DATE(7:2)
               TO WS-DAY-STR
           MOVE NUMVAL(WS-YEAR-STR)
               TO WS-CUR-YEAR
           MOVE NUMVAL(WS-MONTH-STR)
               TO WS-CUR-MONTH
           MOVE NUMVAL(WS-DAY-STR)
               TO WS-CUR-DAY
           DISPLAY "  Today: "
               WS-CUR-YEAR "-"
               WS-CUR-MONTH "-"
               WS-CUR-DAY

      *>   Initialize jump table
           PERFORM INIT-JUMP-TABLE

      *>   Step 0: Fetch docs
           DISPLAY " "
           DISPLAY "[INIT] Fetch timetravel.md"
           PERFORM FETCH-DOCS

      *>   Step 1: API help
           DISPLAY " "
           DISPLAY "[INIT] API help"
           MOVE "help" TO WS-NUM-STR
           PERFORM SEND-VERIFY-ACTION

      *>   Step 2: API reset
           DISPLAY " "
           DISPLAY "[INIT] API reset"
           MOVE "reset" TO WS-NUM-STR
           PERFORM SEND-VERIFY-ACTION

      *>   Step 3: Get initial config
           DISPLAY " "
           DISPLAY "[INIT] getConfig"
           MOVE "getConfig" TO WS-NUM-STR
           PERFORM SEND-VERIFY-ACTION

      *>   Step 4: Get initial backend state
           DISPLAY " "
           DISPLAY "[INIT] Backend poll"
           PERFORM BACKEND-GET

      *>   Execute jumps
           PERFORM VARYING WS-JUMP-IDX
               FROM 1 BY 1
               UNTIL WS-JUMP-IDX > WS-NUM-JUMPS
               OR WS-FLAG-FOUND = "Y"

               DISPLAY " "
               DISPLAY "===================="
                   "===================="
               DISPLAY "JUMP " WS-JUMP-IDX

      *>       Load current jump data
               MOVE WS-JT-YEAR(WS-JUMP-IDX)
                   TO WS-YEAR
               MOVE WS-JT-MONTH(WS-JUMP-IDX)
                   TO WS-MONTH
               MOVE WS-JT-DAY(WS-JUMP-IDX)
                   TO WS-DAY
               MOVE WS-JT-FROM-YEAR(
                   WS-JUMP-IDX)
                   TO WS-FROM-YEAR
               MOVE WS-JT-TYPE(WS-JUMP-IDX)
                   TO WS-JUMP-TYPE
               MOVE WS-JT-PWR(WS-JUMP-IDX)
                   TO WS-PWR

               DISPLAY "  Target: "
                   WS-YEAR "-" WS-MONTH
                   "-" WS-DAY
                   " type=" TRIM(WS-JUMP-TYPE)
                   " PWR=" WS-PWR

               PERFORM EXECUTE-JUMP
           END-PERFORM

      *>   Final check
           IF WS-FLAG-FOUND NOT = "Y"
               DISPLAY " "
               DISPLAY "[FINAL] getConfig"
               MOVE "getConfig" TO WS-NUM-STR
               PERFORM SEND-VERIFY-ACTION
               PERFORM BACKEND-GET
           END-IF

           DISPLAY " "
           IF WS-FLAG-FOUND = "Y"
               DISPLAY ">>> FLAG FOUND <<<"
           ELSE
               DISPLAY "No flag found."
           END-IF
           DISPLAY "=== DONE ==="
           STOP RUN.

      *> ============================================================
      *> INIT-JUMP-TABLE: Set up the 3 jumps
      *> ============================================================
       INIT-JUMP-TABLE.
      *>   Jump 1: 2238-11-05 (future from today)
           MOVE 2238 TO WS-JT-YEAR(1)
           MOVE 11   TO WS-JT-MONTH(1)
           MOVE 05   TO WS-JT-DAY(1)
           MOVE WS-CUR-YEAR
               TO WS-JT-FROM-YEAR(1)
           MOVE "jump"   TO WS-JT-TYPE(1)
           MOVE 91  TO WS-JT-PWR(1)

      *>   Jump 2: today (past from 2238)
           MOVE WS-CUR-YEAR
               TO WS-JT-YEAR(2)
           MOVE WS-CUR-MONTH
               TO WS-JT-MONTH(2)
           MOVE WS-CUR-DAY
               TO WS-JT-DAY(2)
           MOVE 2238
               TO WS-JT-FROM-YEAR(2)
           MOVE "jump"   TO WS-JT-TYPE(2)
           MOVE 28  TO WS-JT-PWR(2)

      *>   Jump 3: 2024-11-12 (tunnel)
           MOVE 2024 TO WS-JT-YEAR(3)
           MOVE 11   TO WS-JT-MONTH(3)
           MOVE 12   TO WS-JT-DAY(3)
           MOVE WS-CUR-YEAR
               TO WS-JT-FROM-YEAR(3)
           MOVE "tunnel" TO WS-JT-TYPE(3)
           MOVE 19  TO WS-JT-PWR(3)
           .

      *> ============================================================
      *> FETCH-DOCS: Download timetravel.md
      *> ============================================================
       FETCH-DOCS.
           INITIALIZE WS-CMD
           STRING
               "curl -s -o "
               "timetravel_doc.md "
               TRIM(WS-HUB-URL)
               "/dane/timetravel.md"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD
           DISPLAY "  Docs fetched."
           .

      *> ============================================================
      *> EXECUTE-JUMP: Run a single time jump
      *> ============================================================
       EXECUTE-JUMP.
      *>   Step 1: Set standby via backend
           DISPLAY " "
           DISPLAY "  [1] Standby mode"
           PERFORM BACKEND-SET-STANDBY
           CALL "C$SLEEP" USING 1

      *>   Step 2: Configure year
           DISPLAY " "
           DISPLAY "  [2] Configure date"
           MOVE WS-YEAR TO WS-NUM-STR
           PERFORM SEND-VERIFY-CONFIGURE-YEAR

      *>   Step 3: Configure month
           MOVE WS-MONTH TO WS-NUM-STR
           PERFORM STRIP-LEADING-ZEROS
           PERFORM SEND-VERIFY-CONFIGURE-MONTH

      *>   Step 4: Configure day
           MOVE WS-DAY TO WS-NUM-STR
           PERFORM STRIP-LEADING-ZEROS
           PERFORM SEND-VERIFY-CONFIGURE-DAY

      *>   Step 5: Compute + set syncRatio
           DISPLAY " "
           DISPLAY "  [3] syncRatio"
           PERFORM COMPUTE-SYNC-RATIO
           PERFORM SEND-VERIFY-CONFIGURE-SYNC

      *>   Step 6: getConfig for stabilization
           DISPLAY " "
           DISPLAY "  [4] getConfig for hints"
           MOVE "getConfig" TO WS-NUM-STR
           PERFORM SEND-VERIFY-ACTION
      *>   Save response for AI
           MOVE WS-JBUF(1:WS-JLEN)
               TO WS-CONFIG-RESP
           MOVE WS-JLEN TO WS-CONFIG-LEN

      *>   Steps 5-10: Stab + config + poll
      *>   Retry up to 5 times if unstable
           MOVE "N" TO WS-STAB-OK
           MOVE SPACES TO WS-PREV-STAB
           MOVE 0 TO WS-NEED-CFG-LEN
           PERFORM VARYING WS-STAB-RETRY
               FROM 1 BY 1
               UNTIL WS-STAB-RETRY > 5
               OR WS-STAB-OK = "Y"

               IF WS-STAB-RETRY > 1
                   DISPLAY " "
                   DISPLAY "  *** RETRY "
                       WS-STAB-RETRY
                       " ***"
      *>           Save prev value
                   MOVE WS-STAB-VALUE
                       TO WS-PREV-STAB
      *>           Re-fetch config
                   MOVE "getConfig"
                       TO WS-NUM-STR
                   PERFORM
                       SEND-VERIFY-ACTION
                   MOVE WS-JBUF(1:WS-JLEN)
                       TO WS-CONFIG-RESP
                   MOVE WS-JLEN
                       TO WS-CONFIG-LEN
      *>           Extract needConfig
                   MOVE "needConfig"
                       TO WS-KEY-SEARCH
                   PERFORM
                       FIND-JSON-VAL
                   MOVE SPACES
                       TO WS-NEED-CFG
                   IF TRIM(WS-JVAL)
                       NOT = SPACES
                       MOVE
                         TRIM(WS-JVAL)
                         TO WS-NEED-CFG
      *>               Escape for JSON
                       MOVE WS-NEED-CFG
                         TO WS-ESC-IN
                       PERFORM
                         JSON-ESCAPE-STR
                       MOVE
                         WS-ESC-OUT(
                         1:WS-ESC-OLEN)
                         TO
                         WS-NEED-CFG-ESC
                       MOVE
                         WS-ESC-OLEN
                         TO
                         WS-NEED-CFG-LEN
                   END-IF
               END-IF

      *>       AI interpret stabilization
               DISPLAY " "
               DISPLAY "  [5] AI stab"
               PERFORM AI-GET-STABILIZATION

      *>       Set stabilization
               DISPLAY " "
               DISPLAY "  [6] Set stab"
               PERFORM
                   SEND-VERIFY-CONFIGURE-STAB

      *>       Set PWR via backend
               DISPLAY " "
               DISPLAY "  [7] Set PWR"
               PERFORM BACKEND-SET-PWR

      *>       Set PTA/PTB
               DISPLAY " "
               DISPLAY "  [8] Set PTA/PTB"
               PERFORM COMPUTE-PT-SWITCHES
               PERFORM BACKEND-SET-PT

      *>       Poll internalMode
               DISPLAY " "
               DISPLAY "  [9] Poll mode"
               PERFORM COMPUTE-TARGET-MODE
               PERFORM POLL-INTERNAL-MODE

      *>       Poll fluxDensity
               DISPLAY " "
               DISPLAY "  [10] Poll flux"
               PERFORM POLL-FLUX-DENSITY

               IF WS-FLUX-MATCHED = "Y"
                   MOVE "Y"
                       TO WS-STAB-OK
               ELSE
                   DISPLAY "  Unstable!"
                   DISPLAY "  Will retry"
               END-IF
           END-PERFORM

      *>   Step 13: Activate
           DISPLAY " "
           DISPLAY "  [11] Active mode"
           PERFORM BACKEND-SET-ACTIVE
           CALL "C$SLEEP" USING 2

      *>   Step 14: Time travel
           DISPLAY " "
           DISPLAY "  [12] timeTravel"
           MOVE "timeTravel" TO WS-NUM-STR
           PERFORM SEND-VERIFY-ACTION
           PERFORM CHECK-FLAG-IN-JBUF

           IF WS-FLAG-FOUND = "Y"
               EXIT PARAGRAPH
           END-IF

           CALL "C$SLEEP" USING 3

      *>   Step 15: Verify jump
           DISPLAY " "
           DISPLAY "  [13] Verify jump"
           MOVE "getConfig" TO WS-NUM-STR
           PERFORM SEND-VERIFY-ACTION
           PERFORM CHECK-FLAG-IN-JBUF

           PERFORM BACKEND-GET
           PERFORM CHECK-FLAG-IN-JBUF
           .

      *> ============================================================
      *> SEND-VERIFY-ACTION: POST action-only to /verify
      *> Input: WS-NUM-STR = action name
      *> ============================================================
       SEND-VERIFY-ACTION.
           MOVE SPACES TO WS-HUB-BODY
           STRING
               "{" WS-QT "task" WS-QT ":"
               WS-QT "timetravel" WS-QT ","
               WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT
               "," WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT TRIM(WS-NUM-STR)
               WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
           END-STRING
           DISPLAY "  -> action="
               TRIM(WS-NUM-STR)
           PERFORM SEND-HUB-REQUEST
           DISPLAY "  <- "
               WS-JBUF(1:500)
           PERFORM CHECK-FLAG-IN-JBUF
           .

      *> ============================================================
      *> SEND-VERIFY-CONFIGURE-YEAR
      *> ============================================================
       SEND-VERIFY-CONFIGURE-YEAR.
           MOVE SPACES TO WS-HUB-BODY
           STRING
               "{" WS-QT "task" WS-QT ":"
               WS-QT "timetravel" WS-QT ","
               WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT
               "," WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT "configure" WS-QT ","
               WS-QT "param" WS-QT ":"
               WS-QT "year" WS-QT ","
               WS-QT "value" WS-QT ":"
               TRIM(WS-NUM-STR)
               "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
           END-STRING
           DISPLAY "  -> configure year="
               TRIM(WS-NUM-STR)
           PERFORM SEND-HUB-REQUEST
           DISPLAY "  <- "
               WS-JBUF(1:500)
           PERFORM CHECK-FLAG-IN-JBUF
           .

      *> ============================================================
      *> SEND-VERIFY-CONFIGURE-MONTH
      *> ============================================================
       SEND-VERIFY-CONFIGURE-MONTH.
           MOVE SPACES TO WS-HUB-BODY
           STRING
               "{" WS-QT "task" WS-QT ":"
               WS-QT "timetravel" WS-QT ","
               WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT
               "," WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT "configure" WS-QT ","
               WS-QT "param" WS-QT ":"
               WS-QT "month" WS-QT ","
               WS-QT "value" WS-QT ":"
               TRIM(WS-NUM-STR)
               "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
           END-STRING
           DISPLAY "  -> configure month="
               TRIM(WS-NUM-STR)
           PERFORM SEND-HUB-REQUEST
           DISPLAY "  <- "
               WS-JBUF(1:500)
           PERFORM CHECK-FLAG-IN-JBUF
           .

      *> ============================================================
      *> SEND-VERIFY-CONFIGURE-DAY
      *> ============================================================
       SEND-VERIFY-CONFIGURE-DAY.
           MOVE SPACES TO WS-HUB-BODY
           STRING
               "{" WS-QT "task" WS-QT ":"
               WS-QT "timetravel" WS-QT ","
               WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT
               "," WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT "configure" WS-QT ","
               WS-QT "param" WS-QT ":"
               WS-QT "day" WS-QT ","
               WS-QT "value" WS-QT ":"
               TRIM(WS-NUM-STR)
               "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
           END-STRING
           DISPLAY "  -> configure day="
               TRIM(WS-NUM-STR)
           PERFORM SEND-HUB-REQUEST
           DISPLAY "  <- "
               WS-JBUF(1:500)
           PERFORM CHECK-FLAG-IN-JBUF
           .

      *> ============================================================
      *> COMPUTE-SYNC-RATIO: (day*8+month*12+year*7) MOD 101
      *> Result in WS-SYNC-STR as "0.XX"
      *> ============================================================
       COMPUTE-SYNC-RATIO.
           COMPUTE WS-SYNC-RAW =
               WS-DAY * 8
               + WS-MONTH * 12
               + WS-YEAR * 7
           COMPUTE WS-SYNC-MOD =
               FUNCTION MOD(WS-SYNC-RAW, 101)
           DISPLAY "  syncRatio raw="
               WS-SYNC-RAW
               " mod101=" WS-SYNC-MOD

      *>   Format as 0.XX (no leading zeros)
           MOVE SPACES TO WS-SYNC-STR
           IF WS-SYNC-MOD = 0
               MOVE "0.0" TO WS-SYNC-STR
           ELSE IF WS-SYNC-MOD = 100
               MOVE "1.0" TO WS-SYNC-STR
           ELSE
               MOVE WS-SYNC-MOD
                   TO WS-NUM-STR
               PERFORM STRIP-LEADING-ZEROS
      *>       Pad single digit: 5 -> 0.05
               IF WS-SYNC-MOD < 10
                   STRING "0.0"
                       TRIM(WS-NUM-STR)
                       DELIMITED SIZE
                       INTO WS-SYNC-STR
                   END-STRING
               ELSE
                   STRING "0."
                       TRIM(WS-NUM-STR)
                       DELIMITED SIZE
                       INTO WS-SYNC-STR
                   END-STRING
               END-IF
           END-IF
           END-IF
           DISPLAY "  syncRatio="
               TRIM(WS-SYNC-STR)
           .

      *> ============================================================
      *> SEND-VERIFY-CONFIGURE-SYNC
      *> ============================================================
       SEND-VERIFY-CONFIGURE-SYNC.
           MOVE SPACES TO WS-HUB-BODY
           STRING
               "{" WS-QT "task" WS-QT ":"
               WS-QT "timetravel" WS-QT ","
               WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT
               "," WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT "configure" WS-QT ","
               WS-QT "param" WS-QT ":"
               WS-QT "syncRatio" WS-QT ","
               WS-QT "value" WS-QT ":"
               TRIM(WS-SYNC-STR)
               "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
           END-STRING
           DISPLAY "  -> configure syncRatio="
               TRIM(WS-SYNC-STR)
           PERFORM SEND-HUB-REQUEST
           DISPLAY "  <- "
               WS-JBUF(1:500)
           PERFORM CHECK-FLAG-IN-JBUF
           .

      *> ============================================================
      *> AI-GET-STABILIZATION: Call GPT-4.1-mini
      *> ============================================================
       AI-GET-STABILIZATION.
      *>   Escape the config response for JSON
           MOVE WS-CONFIG-RESP(
               1:WS-CONFIG-LEN)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR
           MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
               TO WS-CONFIG-RESP-ESC
           MOVE WS-ESC-OLEN
               TO WS-CONFIG-ESC-LEN

      *>   Build AI request JSON
           MOVE SPACES TO WS-REQ-JSON
           MOVE 1 TO WS-PTR

           STRING
               "{"
               WS-QT "model" WS-QT ":"
               WS-QT "gpt-4.1-mini"
               WS-QT ","
               WS-QT "temperature"
               WS-QT ":0,"
               WS-QT "max_tokens"
               WS-QT ":300,"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   response_format for JSON output
           STRING
               WS-QT "response_format"
               WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "json_object"
               WS-QT "},"
               WS-QT "messages" WS-QT
               ":["
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   System message
           STRING
               "{"
               WS-QT "role" WS-QT ":"
               WS-QT "system" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               "You extract the "
               "stabilization value "
               "from time machine "
               "config. The config "
               "has a hint (often "
               "in Polish) about "
               "what stabilization "
               "value to set."
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               " The hint may say "
               "to add or subtract "
               "numbers. COMPUTE "
               "the final result "
               "yourself."
               WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Unicode mapping for Polish chars
           STRING
               "IMPORTANT: The input"
               " text contains "
               X"5C" X"5C"
               "uXXXX JSON "
               "unicode escapes for"
               " Polish characters."
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "You MUST decode "
               "them before "
               "interpreting. "
               "Key mappings:"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               X"5C" X"5C"
               "u0142=" X"C5" X"82"
               " "
               X"5C" X"5C"
               "u0119=" X"C4" X"99"
               " "
               X"5C" X"5C"
               "u0107=" X"C4" X"87"
               " "
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               X"5C" X"5C"
               "u015b=" X"C5" X"9B"
               " "
               X"5C" X"5C"
               "u017c=" X"C5" X"BC"
               " "
               X"5C" X"5C"
               "u0105=" X"C4" X"85"
               " "
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               X"5C" X"5C"
               "u0144=" X"C5" X"84"
               " "
               X"5C" X"5C"
               "u00f3=" X"C3" X"B3"
               " "
               X"5C" X"5C"
               "u017a=" X"C5" X"BA"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               X"5C" X"5C"
               "u0104=" X"C4" X"84"
               " "
               X"5C" X"5C"
               "u0106=" X"C4" X"86"
               " "
               X"5C" X"5C"
               "u0118=" X"C4" X"98"
               " "
               X"5C" X"5C"
               "u0141=" X"C5" X"81"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "Example: dziewi"
               X"5C" X"5C"
               "u0119" X"5C" X"5C"
               "u0107set = "
               "dziewi"
               X"C4" X"99" X"C4"
               X"87" "set = 900."
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "siedemset jedena"
               X"5C" X"5C"
               "u015bcie = "
               "siedemset "
               "jedena"
               X"C5" X"9B"
               "cie = 711."
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "Carefully decode "
               "ALL Polish words "
               "before computing "
               "the math."
               WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Polish compound number rules
           STRING
               "CRITICAL POLISH "
               "NUMBER RULES: "
               "Polish numbers are "
               "COMPOUND."
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "When a hundreds-"
               "word is followed "
               "by tens/units, "
               "they form ONE "
               "number by ADDITION:"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "Hundreds: sto=100 "
               "dwie" X"5C"
               "u015bcie=200 "
               "trzysta=300 "
               "czterysta=400"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "pi" X"5C"
               "u0119" X"5C"
               "u0107set=500 "
               "sze" X"5C"
               "u015b" X"5C"
               "u0107set=600 "
               "siedemset=700"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "osiemset=800 "
               "dziewi" X"5C"
               "u0119" X"5C"
               "u0107set=900"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "Teens: "
               "jedena" X"5C"
               "u015bcie=11 "
               "dwana" X"5C"
               "u015bcie=12 "
               "trzyna" X"5C"
               "u015bcie=13"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "czterna" X"5C"
               "u015bcie=14 "
               "pi" X"5C"
               "u0119tna" X"5C"
               "u015bcie=15 "
               "szesna" X"5C"
               "u015bcie=16"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "siedemna" X"5C"
               "u015bcie=17 "
               "osiemna" X"5C"
               "u015bcie=18 "
               "dziewi" X"5C"
               "u0119tna" X"5C"
               "u015bcie=19"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "Tens: "
               "dwadzie" X"5C"
               "u015bcia=20 "
               "trzydzie" X"5C"
               "u015bci=30 "
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "czterdzies"
               X"5C" "u0107i=40 "
               "pi" X"5C"
               "u0119" X"5C"
               "u0107dziesi"
               X"5C" "u0105t=50"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "sze" X"5C"
               "u015b" X"5C"
               "u0107dziesi"
               X"5C" "u0105t=60 "
               "siedemdziesi"
               X"5C" "u0105t=70"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "osiemdziesi"
               X"5C" "u0105t=80 "
               "dziewi" X"5C"
               "u0119" X"5C"
               "u0107dziesi"
               X"5C" "u0105t=90"
               WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "EXAMPLES:" WS-NL
               "siedemset "
               "jedena" X"5C"
               "u015bcie "
               "= 700+11 = 711"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "dziewi" X"5C"
               "u0119" X"5C"
               "u0107set = 900"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "dziewi" X"5C"
               "u0119" X"5C"
               "u0107set minus "
               "siedemset "
               "jedena" X"5C"
               "u015bcie "
               "= 900-711 = 189"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "NEVER treat "
               "siedemset and "
               "jedena" X"5C"
               "u015bcie as "
               "separate ops"
               WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "RULES:" WS-NL
               "- Return ONLY the "
               "final computed "
               "integer" WS-NL
               "- Do ALL math "
               "yourself" WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "- NO math "
               "expressions" WS-NL
               "- NO explanation"
               WS-NL
               "- NO equals sign"
               WS-NL
               "- JUST the number"
               WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "Examples:" WS-NL
               "Hint: base 700 "
               "minus 112 -> {"
               X"5C" WS-QT
               "stabilization"
               X"5C" WS-QT ":"
               X"5C" WS-QT "588"
               X"5C" WS-QT "}"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "Hint: set to 42"
               " -> {"
               X"5C" WS-QT
               "stabilization"
               X"5C" WS-QT ":"
               X"5C" WS-QT "42"
               X"5C" WS-QT "}"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "Hint: dodaj 8 do "
               "42 -> {"
               X"5C" WS-QT
               "stabilization"
               X"5C" WS-QT ":"
               X"5C" WS-QT "50"
               X"5C" WS-QT "}"
               WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "Return JSON: {"
               X"5C" WS-QT
               "stabilization"
               X"5C" WS-QT ":"
               X"5C" WS-QT
               "VALUE"
               X"5C" WS-QT "}"
               WS-QT "},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   User message with config response
           STRING
               "{"
               WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               "Target: "
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           MOVE WS-YEAR TO WS-YEAR-STR
           MOVE WS-MONTH TO WS-MONTH-STR
           MOVE WS-DAY TO WS-DAY-STR
           STRING
               TRIM(WS-YEAR-STR) "-"
               TRIM(WS-MONTH-STR) "-"
               TRIM(WS-DAY-STR)
               WS-NL WS-NL
               "API config response:"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Append escaped config response
           IF WS-CONFIG-ESC-LEN > 0
               STRING
                   WS-CONFIG-RESP-ESC(
                   1:WS-CONFIG-ESC-LEN)
                   DELIMITED SIZE
                   INTO WS-REQ-JSON
                   WITH POINTER WS-PTR
               END-STRING
           END-IF

      *>   Add retry context if retrying
           IF WS-PREV-STAB NOT = SPACES
               STRING
                   WS-NL WS-NL
                   "WARNING: Previous "
                   "value "
                   TRIM(WS-PREV-STAB)
                   " was INCORRECT - "
                   "condition is still"
                   " unstable. "
                   DELIMITED SIZE
                   INTO WS-REQ-JSON
                   WITH POINTER WS-PTR
               END-STRING
      *>       Include needConfig hint
               IF WS-NEED-CFG-LEN > 0
                   STRING
                       WS-NL
                       "The hint says: "
                       WS-NEED-CFG-ESC(
                       1:WS-NEED-CFG-LEN
                       )
                       WS-NL
                       DELIMITED SIZE
                       INTO WS-REQ-JSON
                       WITH POINTER
                           WS-PTR
                   END-STRING
               END-IF
               STRING
                   "Recalculate more "
                   "carefully."
                   DELIMITED SIZE
                   INTO WS-REQ-JSON
                   WITH POINTER WS-PTR
               END-STRING
           END-IF

           STRING
               WS-NL WS-NL
               "What exact value for "
               "stabilization param?"
               WS-QT
               "}]}"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Write request + call API
           MOVE "ai_req.json"
               TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "ERR: OPEN "
                   TRIM(WS-WORK-PATH)
                   " FS=" WS-FS
               STOP RUN
           END-IF
           WRITE WORK-REC FROM WS-REQ-JSON
           CLOSE WORK-FILE

           INITIALIZE WS-CMD
           STRING
               "curl -s "
               "-o ai_resp.json"
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
               " -d @ai_req.json"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Parse AI response
           MOVE "ai_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp"
               TO WS-WORK-PATH

           IF WS-JLEN = 0
               DISPLAY "  ERR: Empty AI resp"
               MOVE "1" TO WS-STAB-VALUE
               EXIT PARAGRAPH
           END-IF

           DISPLAY "  AI resp: "
               WS-JBUF(1:500)

      *>   Extract content from AI response
           MOVE "content" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL

           DISPLAY "  AI content: "
               TRIM(WS-JVAL)(1:300)

      *>   Try to extract stabilization value
      *>   from JSON response {"stabilization":"X"}
           MOVE WS-JVAL TO WS-ESC-IN
           PERFORM JSON-UNESCAPE-STR
           MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
               TO WS-JVAL

      *>   Parse the stabilization value
      *>   from the AI content
           MOVE WS-JVAL(1:WS-ESC-OLEN)
               TO WS-JBUF
           MOVE WS-ESC-OLEN TO WS-JLEN
           MOVE "stabilization"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL

           IF TRIM(WS-JVAL) NOT = SPACES
               MOVE TRIM(WS-JVAL)
                   TO WS-STAB-VALUE
               DISPLAY "  Raw stab="
                   TRIM(WS-STAB-VALUE)
           ELSE
      *>       Fallback: use entire content
               DISPLAY "  WARN: No stab key"
               MOVE TRIM(WS-JVAL)
                   TO WS-STAB-VALUE
           END-IF

      *>   Sanitize: extract digits only
           PERFORM SANITIZE-STAB-VALUE
           DISPLAY "  Stabilization="
               TRIM(WS-STAB-VALUE)
           .

      *> ============================================================
      *> SEND-VERIFY-CONFIGURE-STAB
      *> ============================================================
       SEND-VERIFY-CONFIGURE-STAB.
      *>   Try sending as number first, if purely
      *>   numeric; otherwise send as string
           MOVE SPACES TO WS-HUB-BODY
           MOVE 0 TO WS-TALLY-CNT
           INSPECT TRIM(WS-STAB-VALUE)
               TALLYING WS-TALLY-CNT
               FOR ALL "."
           INSPECT TRIM(WS-STAB-VALUE)
               TALLYING WS-TALLY-CNT
               FOR ALL "0"
           INSPECT TRIM(WS-STAB-VALUE)
               TALLYING WS-TALLY-CNT
               FOR ALL "1"
           INSPECT TRIM(WS-STAB-VALUE)
               TALLYING WS-TALLY-CNT
               FOR ALL "2"
           INSPECT TRIM(WS-STAB-VALUE)
               TALLYING WS-TALLY-CNT
               FOR ALL "3"
           INSPECT TRIM(WS-STAB-VALUE)
               TALLYING WS-TALLY-CNT
               FOR ALL "4"
           INSPECT TRIM(WS-STAB-VALUE)
               TALLYING WS-TALLY-CNT
               FOR ALL "5"
           INSPECT TRIM(WS-STAB-VALUE)
               TALLYING WS-TALLY-CNT
               FOR ALL "6"
           INSPECT TRIM(WS-STAB-VALUE)
               TALLYING WS-TALLY-CNT
               FOR ALL "7"
           INSPECT TRIM(WS-STAB-VALUE)
               TALLYING WS-TALLY-CNT
               FOR ALL "8"
           INSPECT TRIM(WS-STAB-VALUE)
               TALLYING WS-TALLY-CNT
               FOR ALL "9"

           IF WS-TALLY-CNT =
               LENGTH(TRIM(WS-STAB-VALUE))
           AND WS-TALLY-CNT > 0
      *>       Numeric - send without quotes
               STRING
                   "{" WS-QT "task" WS-QT ":"
                   WS-QT "timetravel" WS-QT
                   "," WS-QT "apikey" WS-QT ":"
                   WS-QT TRIM(WS-HUB-KEY)
                   WS-QT ","
                   WS-QT "answer" WS-QT ":{"
                   WS-QT "action" WS-QT ":"
                   WS-QT "configure" WS-QT ","
                   WS-QT "param" WS-QT ":"
                   WS-QT "stabilization"
                   WS-QT ","
                   WS-QT "value" WS-QT ":"
                   TRIM(WS-STAB-VALUE)
                   "}}"
                   DELIMITED SIZE
                   INTO WS-HUB-BODY
               END-STRING
           ELSE
      *>       String - send with quotes
               STRING
                   "{" WS-QT "task" WS-QT ":"
                   WS-QT "timetravel" WS-QT
                   "," WS-QT "apikey" WS-QT ":"
                   WS-QT TRIM(WS-HUB-KEY)
                   WS-QT ","
                   WS-QT "answer" WS-QT ":{"
                   WS-QT "action" WS-QT ":"
                   WS-QT "configure" WS-QT ","
                   WS-QT "param" WS-QT ":"
                   WS-QT "stabilization"
                   WS-QT ","
                   WS-QT "value" WS-QT ":"
                   WS-QT
                   TRIM(WS-STAB-VALUE)
                   WS-QT
                   "}}"
                   DELIMITED SIZE
                   INTO WS-HUB-BODY
               END-STRING
           END-IF

           DISPLAY "  -> configure stab="
               TRIM(WS-STAB-VALUE)
           PERFORM SEND-HUB-REQUEST
           DISPLAY "  <- "
               WS-JBUF(1:500)
           PERFORM CHECK-FLAG-IN-JBUF
           .

      *> ============================================================
      *> COMPUTE-PT-SWITCHES: Determine PTA/PTB
      *> ============================================================
       COMPUTE-PT-SWITCHES.
           IF TRIM(WS-JUMP-TYPE) = "tunnel"
               MOVE "true"  TO WS-PTA
               MOVE "true"  TO WS-PTB
           ELSE IF WS-YEAR < WS-FROM-YEAR
               MOVE "true"  TO WS-PTA
               MOVE "false" TO WS-PTB
           ELSE
               MOVE "false" TO WS-PTA
               MOVE "true"  TO WS-PTB
           END-IF
           END-IF
           DISPLAY "  PTA=" TRIM(WS-PTA)
               " PTB=" TRIM(WS-PTB)
           .

      *> ============================================================
      *> COMPUTE-TARGET-MODE: year range -> mode
      *> ============================================================
       COMPUTE-TARGET-MODE.
           EVALUATE TRUE
           WHEN WS-YEAR < 2000
               MOVE 1 TO WS-TARGET-MODE
           WHEN WS-YEAR <= 2150
               MOVE 2 TO WS-TARGET-MODE
           WHEN WS-YEAR <= 2300
               MOVE 3 TO WS-TARGET-MODE
           WHEN OTHER
               MOVE 4 TO WS-TARGET-MODE
           END-EVALUATE
           DISPLAY "  Target mode="
               WS-TARGET-MODE
               " for year " WS-YEAR
           .

      *> ============================================================
      *> BACKEND-GET: GET /timetravel_backend
      *> ============================================================
       BACKEND-GET.
           INITIALIZE WS-CMD
           STRING
               "curl -s "
               "-o be_resp.json "
               TRIM(WS-BACKEND-URL)
               "?apikey="
               TRIM(WS-HUB-KEY)
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           MOVE "be_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH
           DISPLAY "  BE GET: "
               WS-JBUF(1:500)
           .

      *> ============================================================
      *> BACKEND-POST: POST to /timetravel_backend
      *> Input: WS-BE-BODY has JSON body
      *> ============================================================
       BACKEND-POST.
           MOVE "be_req.tmp" TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "ERR: OPEN "
                   TRIM(WS-WORK-PATH)
                   " FS=" WS-FS
               STOP RUN
           END-IF
           WRITE WORK-REC FROM WS-BE-BODY
           CLOSE WORK-FILE

           INITIALIZE WS-CMD
           STRING
               "curl -s "
               "-o be_resp.json"
               " -X POST "
               TRIM(WS-BACKEND-URL)
               " -H " WS-QT
               "Content-Type: "
               "application/json"
               WS-QT
               " -d @be_req.tmp"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           MOVE "be_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH
           DISPLAY "  BE POST: "
               WS-JBUF(1:500)
           PERFORM CHECK-FLAG-IN-JBUF
           .

      *> ============================================================
      *> BACKEND-SET-STANDBY
      *> ============================================================
       BACKEND-SET-STANDBY.
           MOVE SPACES TO WS-BE-BODY
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT
               "," WS-QT "mode" WS-QT ":"
               WS-QT "standby" WS-QT "}"
               DELIMITED SIZE
               INTO WS-BE-BODY
           END-STRING
           PERFORM BACKEND-POST
           .

      *> ============================================================
      *> BACKEND-SET-ACTIVE
      *> ============================================================
       BACKEND-SET-ACTIVE.
           MOVE SPACES TO WS-BE-BODY
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT
               "," WS-QT "mode" WS-QT ":"
               WS-QT "active" WS-QT "}"
               DELIMITED SIZE
               INTO WS-BE-BODY
           END-STRING
           PERFORM BACKEND-POST
           .

      *> ============================================================
      *> BACKEND-SET-PWR
      *> ============================================================
       BACKEND-SET-PWR.
           MOVE WS-PWR TO WS-NUM-STR
           PERFORM STRIP-LEADING-ZEROS
           MOVE TRIM(WS-NUM-STR)
               TO WS-PWR-STR
           MOVE SPACES TO WS-BE-BODY
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT
               "," WS-QT "PWR" WS-QT ":"
               TRIM(WS-PWR-STR)
               "}"
               DELIMITED SIZE
               INTO WS-BE-BODY
           END-STRING
           DISPLAY "  -> PWR="
               TRIM(WS-PWR-STR)
           PERFORM BACKEND-POST
           .

      *> ============================================================
      *> BACKEND-SET-PT: Set PTA + PTB switches
      *> ============================================================
       BACKEND-SET-PT.
           MOVE SPACES TO WS-BE-BODY
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT
               "," WS-QT "PTA" WS-QT ":"
               TRIM(WS-PTA)
               "," WS-QT "PTB" WS-QT ":"
               TRIM(WS-PTB) "}"
               DELIMITED SIZE
               INTO WS-BE-BODY
           END-STRING
           DISPLAY "  -> PTA="
               TRIM(WS-PTA)
               " PTB=" TRIM(WS-PTB)
           PERFORM BACKEND-POST
           .

      *> ============================================================
      *> POLL-INTERNAL-MODE: Poll getConfig
      *> ============================================================
       POLL-INTERNAL-MODE.
           MOVE "N" TO WS-MODE-MATCHED
           PERFORM VARYING WS-POLL-CT
               FROM 1 BY 1
               UNTIL WS-POLL-CT > WS-POLL-MAX
               OR WS-MODE-MATCHED = "Y"

      *>       Call getConfig
               MOVE "getConfig" TO WS-NUM-STR
               PERFORM SEND-VERIFY-ACTION

      *>       Extract internalMode
               MOVE "internalMode"
                   TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               MOVE TRIM(WS-JVAL)
                   TO WS-CUR-MODE

               DISPLAY "    Poll "
                   WS-POLL-CT
                   ": mode="
                   TRIM(WS-CUR-MODE)
                   " need=" WS-TARGET-MODE

               IF TRIM(WS-CUR-MODE)
                   NOT = SPACES
                   MOVE WS-TARGET-MODE
                       TO WS-NUM-STR
                   IF TRIM(WS-CUR-MODE) =
                       TRIM(WS-NUM-STR)
                       DISPLAY "  Mode matched!"
                       MOVE "Y"
                           TO WS-MODE-MATCHED
                   END-IF
               END-IF

               IF WS-MODE-MATCHED = "N"
                   CALL "C$SLEEP" USING 1
               END-IF
           END-PERFORM

           IF WS-MODE-MATCHED = "N"
               DISPLAY "  WARN: mode timeout"
           END-IF
           .

      *> ============================================================
      *> POLL-FLUX-DENSITY: Poll getConfig
      *> ============================================================
       POLL-FLUX-DENSITY.
           MOVE "N" TO WS-FLUX-MATCHED
           PERFORM VARYING WS-POLL-CT
               FROM 1 BY 1
               UNTIL WS-POLL-CT > WS-POLL-MAX
               OR WS-FLUX-MATCHED = "Y"

      *>       getConfig
               MOVE "getConfig" TO WS-NUM-STR
               PERFORM SEND-VERIFY-ACTION

      *>       Look for fluxDensity
               MOVE "fluxDensity"
                   TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               MOVE TRIM(WS-JVAL)
                   TO WS-FLUX-VAL

               IF TRIM(WS-FLUX-VAL) = SPACES
      *>           Try flux_density
                   MOVE "flux_density"
                       TO WS-KEY-SEARCH
                   MOVE 1 TO WS-JPOS
                   PERFORM FIND-JSON-VAL
                   MOVE TRIM(WS-JVAL)
                       TO WS-FLUX-VAL
               END-IF

               IF TRIM(WS-FLUX-VAL) = SPACES
      *>           Try backend GET
                   PERFORM BACKEND-GET
                   MOVE "fluxDensity"
                       TO WS-KEY-SEARCH
                   MOVE 1 TO WS-JPOS
                   PERFORM FIND-JSON-VAL
                   MOVE TRIM(WS-JVAL)
                       TO WS-FLUX-VAL
               END-IF

               DISPLAY "    Poll "
                   WS-POLL-CT
                   ": flux="
                   TRIM(WS-FLUX-VAL)

               IF TRIM(WS-FLUX-VAL)
                   NOT = SPACES
                   IF TRIM(WS-FLUX-VAL)
                       = "100"
                       DISPLAY "  Flux=100!"
                       MOVE "Y"
                           TO WS-FLUX-MATCHED
                   END-IF
               END-IF

               IF WS-FLUX-MATCHED = "N"
                   CALL "C$SLEEP" USING 1
               END-IF
           END-PERFORM

           IF WS-FLUX-MATCHED = "N"
               DISPLAY "  WARN: flux timeout"
               DISPLAY "  Proceed anyway"
           END-IF
           .

      *> ============================================================
      *> CHECK-FLAG-IN-JBUF: Scan JBUF for FLG:
      *> ============================================================
       CHECK-FLAG-IN-JBUF.
           IF WS-JLEN > 0
               MOVE 0 TO WS-TALLY-CNT
               INSPECT WS-JBUF(1:WS-JLEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL "FLG"
               IF WS-TALLY-CNT > 0
                   DISPLAY " "
                   DISPLAY ">>> FLAG FOUND! <<<"
                   DISPLAY WS-JBUF(1:WS-JLEN)
                   MOVE "Y"
                       TO WS-FLAG-FOUND
               END-IF
           END-IF
           .

      *> ============================================================
      *> SANITIZE-STAB-VALUE: Extract last digit sequence
      *> "42+8=50" -> "50", "the value is 7" -> "7"
      *> Input/Output: WS-STAB-VALUE (in-place)
      *> ============================================================
       SANITIZE-STAB-VALUE.
           MOVE LENGTH(TRIM(WS-STAB-VALUE))
               TO WS-SAN-LEN
           IF WS-SAN-LEN = 0
               MOVE "0" TO WS-STAB-VALUE
               EXIT PARAGRAPH
           END-IF

      *>   Scan backwards for last digit seq
           MOVE "N" TO WS-SAN-FOUND
           MOVE 0 TO WS-SAN-END
           MOVE 0 TO WS-SAN-START

      *>   Find last digit
           PERFORM VARYING WS-SAN-I
               FROM WS-SAN-LEN BY -1
               UNTIL WS-SAN-I < 1
               OR WS-SAN-FOUND = "Y"
               IF WS-STAB-VALUE(
                   WS-SAN-I:1) >= "0"
               AND WS-STAB-VALUE(
                   WS-SAN-I:1) <= "9"
                   MOVE WS-SAN-I
                       TO WS-SAN-END
                   MOVE "Y"
                       TO WS-SAN-FOUND
               END-IF
           END-PERFORM

           IF WS-SAN-FOUND = "N"
               EXIT PARAGRAPH
           END-IF

      *>   Scan back to find start of seq
           MOVE WS-SAN-END
               TO WS-SAN-START
           PERFORM VARYING WS-SAN-I
               FROM WS-SAN-END BY -1
               UNTIL WS-SAN-I < 1
               IF WS-STAB-VALUE(
                   WS-SAN-I:1) >= "0"
               AND WS-STAB-VALUE(
                   WS-SAN-I:1) <= "9"
                   MOVE WS-SAN-I
                       TO WS-SAN-START
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM

      *>   Extract the digit sequence
           MOVE SPACES TO WS-NUM-STR
           MOVE WS-STAB-VALUE(
               WS-SAN-START:
               WS-SAN-END
               - WS-SAN-START + 1)
               TO WS-NUM-STR
           PERFORM STRIP-LEADING-ZEROS
           MOVE SPACES TO WS-STAB-VALUE
           MOVE TRIM(WS-NUM-STR)
               TO WS-STAB-VALUE
           .

      *> ============================================================
      *> STRIP-LEADING-ZEROS: "007" -> "7", "0" -> "0"
      *> Input/Output: WS-NUM-STR (in-place)
      *> ============================================================
       STRIP-LEADING-ZEROS.
           MOVE TRIM(WS-NUM-STR)
               TO WS-NUM-STR
           IF TRIM(WS-NUM-STR) = SPACES
               MOVE "0" TO WS-NUM-STR
               EXIT PARAGRAPH
           END-IF
           MOVE 0 TO WS-I
           INSPECT TRIM(WS-NUM-STR)
               TALLYING WS-I
               FOR LEADING "0"
           IF WS-I >=
               LENGTH(TRIM(WS-NUM-STR))
               MOVE "0" TO WS-NUM-STR
           ELSE IF WS-I > 0
               MOVE TRIM(WS-NUM-STR)
                   (WS-I + 1:)
                   TO WS-NUM-STR
           END-IF
           END-IF
           .

      *> ============================================================
      *> Copybook procedures
      *> ============================================================
       COPY ENVLOAD-PROC.
       COPY HUBSUBMIT-PROC.
       COPY JSONREAD-PROC.
       COPY JSONPARSE-PROC.
       COPY JSONESCAPE-PROC.
       COPY JSONUNESCAPE-PROC.
