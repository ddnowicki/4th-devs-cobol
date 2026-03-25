       IDENTIFICATION DIVISION.
       PROGRAM-ID. S03E03-REACTOR.
      *> ============================================================
      *> S03E03 - Reactor: Navigate robot across 7x5 grid
      *> 1. Send "start" to get initial board state
      *> 2. ONE GPT-4.1-mini call for strategy insight
      *> 3. Deterministic navigation: RIGHT > WAIT > LEFT
      *> 4. Block simulation with bounce at boundaries
      *> 5. Flag detection via INSPECT TALLYING
      *> ============================================================

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WORK-FILE
               ASSIGN TO WS-WORK-PATH
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  WORK-FILE.
       01  WORK-REC              PIC X(16000).

       WORKING-STORAGE SECTION.
      *> -- Config --
       01  WS-HUB-KEY            PIC X(100).
       01  WS-OPENAI-KEY         PIC X(200).
       01  WS-QT                 PIC X(1) VALUE '"'.
       01  WS-FS                 PIC XX.
       01  WS-WORK-PATH          PIC X(200)
                                 VALUE "work.tmp".

      *> -- URLs --
       01  WS-HUB-URL            PIC X(100).
       01  WS-OPENAI-URL         PIC X(200).
       01  WS-VERIFY-URL         PIC X(200).

      *> -- JSON helpers --
       01  WS-NL                 PIC X(2).
       01  WS-PTR                PIC 9(5).

      *> -- System command --
       01  WS-CMD                PIC X(4000).

      *> -- Request / payload --
       01  WS-REQ-JSON           PIC X(16000).
       01  WS-PAYLOAD            PIC X(4000).

      *> -- JSON parse buffer --
       01  WS-JBUF               PIC X(16000).
       01  WS-JLEN               PIC 9(5).
       01  WS-JPOS               PIC 9(5).
       01  WS-JVAL               PIC X(8000).

      *> -- JSON parsing temps --
       01  WS-KEY-SEARCH         PIC X(50).
       01  WS-KEY-POS            PIC 9(5).
       01  WS-VAL-START          PIC 9(5).
       01  WS-VAL-END            PIC 9(5).
       01  WS-FJV-POS            PIC 9(5).
       01  WS-TMP                PIC X(500).

      *> -- File I/O --
       01  WS-EOF                PIC X VALUE "N".
       01  WS-LINE               PIC X(16000).
       01  WS-K                  PIC 9(5).
       01  WS-I                  PIC 9(5).
       01  WS-TALLY-CNT          PIC 9(5).

      *> -- Game constants --
       01  WS-GRID-COLS          PIC 9(1)
                                 VALUE 7.
       01  WS-GRID-ROWS          PIC 9(1)
                                 VALUE 5.
       01  WS-PLAYER-ROW         PIC 9(1)
                                 VALUE 5.
       01  WS-MAX-STEPS          PIC 9(2)
                                 VALUE 50.

      *> -- Game state --
       01  WS-PLAYER-COL         PIC 9(1).
       01  WS-STEP               PIC 9(2).
       01  WS-MOVE-CMD           PIC X(10).
       01  WS-REACHED            PIC X VALUE "N".
       01  WS-SUCCESS            PIC X VALUE "N".

      *> -- Blocks table (current) --
       01  WS-BLOCK-COUNT        PIC 9(2)
                                 VALUE 0.
       01  WS-BLOCKS.
           05  WS-BLK OCCURS 10 TIMES.
               10  WS-BLK-COL   PIC 9(1).
               10  WS-BLK-TOP   PIC 9(1).
               10  WS-BLK-BOT   PIC 9(1).
               10  WS-BLK-DIR   PIC X(4).

      *> -- Simulated future blocks --
       01  WS-FUT-COUNT          PIC 9(2).
       01  WS-FUT-BLOCKS.
           05  WS-FBK OCCURS 10 TIMES.
               10  WS-FBK-COL   PIC 9(1).
               10  WS-FBK-TOP   PIC 9(1).
               10  WS-FBK-BOT   PIC 9(1).
               10  WS-FBK-DIR   PIC X(4).

      *> -- Column safety check --
       01  WS-CHECK-COL          PIC 9(1).
       01  WS-COL-SAFE           PIC X VALUE "Y".

      *> -- Retry --
       01  WS-RETRY              PIC 9(1).

      *> -- Block parsing temps --
       01  WS-ARR-POS            PIC 9(5).
       01  WS-ARR-DEPTH          PIC 9(2).
       01  WS-OBJ-START          PIC 9(5).
       01  WS-OBJ-END            PIC 9(5).
       01  WS-BPARSE-I           PIC 9(2).
       01  WS-BVAL               PIC X(20).
       01  WS-BPOS               PIC 9(5).
       01  WS-CH                 PIC X(1).

      *> -- AI board text --
       01  WS-AI-BUF             PIC X(4000).
       01  WS-AI-LEN             PIC 9(5).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S03E03 REACTOR ==="

           PERFORM INIT-ENV

      *>   Send START
           MOVE "start" TO WS-MOVE-CMD
           PERFORM SEND-COMMAND

      *>   Check immediate flag
           PERFORM CHECK-FLAG
           IF WS-SUCCESS = "Y"
               DISPLAY "=== SUCCESS ==="
               STOP RUN
           END-IF

      *>   Parse initial state
           PERFORM PARSE-PLAYER
           PERFORM PARSE-BLOCKS
           DISPLAY "  Player col: "
               WS-PLAYER-COL
           DISPLAY "  Blocks: "
               WS-BLOCK-COUNT

      *>   ONE AI call for strategy
           PERFORM AI-ANALYZE

      *>   Game loop
           PERFORM VARYING WS-STEP
               FROM 1 BY 1
               UNTIL WS-STEP
               > WS-MAX-STEPS
               OR WS-SUCCESS = "Y"
               OR WS-REACHED = "Y"

               PERFORM DECIDE-MOVE
               DISPLAY "  Step "
                   WS-STEP ": "
                   TRIM(WS-MOVE-CMD)
                   " (col "
                   WS-PLAYER-COL ")"

               PERFORM SEND-COMMAND

      *>       Check flag
               PERFORM CHECK-FLAG
               IF WS-SUCCESS = "Y"
                   EXIT PERFORM
               END-IF

      *>       Check reached_goal
               PERFORM CHECK-REACHED
               IF WS-REACHED = "Y"
                   DISPLAY "  GOAL!"
                   PERFORM CHECK-FLAG
                   EXIT PERFORM
               END-IF

      *>       Update state
               PERFORM UPDATE-STATE
           END-PERFORM

           IF WS-SUCCESS = "Y"
               DISPLAY " "
               DISPLAY "=== FLAG FOUND ==="
           ELSE
               DISPLAY " "
               DISPLAY "=== NO FLAG ==="
           END-IF
           STOP RUN.

      *> ============================================================
      *> INIT-ENV
      *> ============================================================
       INIT-ENV.
           ACCEPT WS-HUB-KEY
               FROM ENVIRONMENT
               "HUB_API_KEY"
           ACCEPT WS-OPENAI-KEY
               FROM ENVIRONMENT
               "OPENAI_API_KEY"
           ACCEPT WS-HUB-URL
               FROM ENVIRONMENT
               "HUB_API_URL"
           ACCEPT WS-OPENAI-URL
               FROM ENVIRONMENT
               "OPENAI_API_URL"

           IF WS-HUB-KEY = SPACES
               DISPLAY "ERR: HUB_API_KEY!"
               STOP RUN
           END-IF
           IF WS-OPENAI-KEY = SPACES
               DISPLAY
                   "ERR: OPENAI_API_KEY!"
               STOP RUN
           END-IF
           IF WS-HUB-URL = SPACES
               DISPLAY "ERR: HUB_API_URL!"
               STOP RUN
           END-IF
           IF WS-OPENAI-URL = SPACES
               DISPLAY
                   "ERR: OPENAI_API_URL!"
               STOP RUN
           END-IF

           MOVE SPACES TO WS-VERIFY-URL
           STRING TRIM(WS-HUB-URL)
               "/verify"
               DELIMITED SIZE
               INTO WS-VERIFY-URL
           END-STRING

           MOVE X"5C" TO WS-NL(1:1)
           MOVE "n"    TO WS-NL(2:1)

           MOVE 1 TO WS-PLAYER-COL
           DISPLAY "  Verify: "
               TRIM(WS-VERIFY-URL)
           .

      *> ============================================================
      *> SEND-COMMAND: POST command to /verify
      *> ============================================================
       SEND-COMMAND.
           PERFORM VARYING WS-RETRY
               FROM 1 BY 1
               UNTIL WS-RETRY > 5

      *>       Build JSON payload
               MOVE SPACES TO WS-PAYLOAD
               STRING
                   "{"
                   WS-QT "apikey" WS-QT
                   ":" WS-QT
                   TRIM(WS-HUB-KEY)
                   WS-QT ","
                   WS-QT "task" WS-QT
                   ":" WS-QT "reactor"
                   WS-QT ","
                   WS-QT "answer" WS-QT
                   ":{"
                   WS-QT "command" WS-QT
                   ":" WS-QT
                   TRIM(WS-MOVE-CMD)
                   WS-QT "}}"
                   DELIMITED SIZE
                   INTO WS-PAYLOAD
               END-STRING

      *>       Write to file
               OPEN OUTPUT WORK-FILE
               WRITE WORK-REC
                   FROM WS-PAYLOAD
               CLOSE WORK-FILE

      *>       curl POST
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
                   " -d @work.tmp"
                   DELIMITED SIZE
                   INTO WS-CMD
               END-STRING
               CALL "SYSTEM"
                   USING WS-CMD

      *>       Read response
               MOVE "hub_resp.json"
                   TO WS-WORK-PATH
               PERFORM READ-JSON-FILE
               MOVE "work.tmp"
                   TO WS-WORK-PATH

               IF WS-JLEN = 0
                   DISPLAY "  Empty resp!"
                   CALL "C$SLEEP" USING 3
               ELSE
      *>           Check rate limit
                   MOVE 0 TO WS-TALLY-CNT
                   INSPECT
                       WS-JBUF(1:WS-JLEN)
                       TALLYING
                       WS-TALLY-CNT
                       FOR ALL "RLIMIT"
                   IF WS-TALLY-CNT > 0
                       DISPLAY
                           "  [RATE] wait"
                       CALL "C$SLEEP"
                           USING 5
                   ELSE
                       EXIT PERFORM
                   END-IF
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> PARSE-PLAYER: Extract player.col
      *> ============================================================
       PARSE-PLAYER.
           IF WS-JLEN = 0
               EXIT PARAGRAPH
           END-IF

      *>   Find "player" then "col" inside
           MOVE "player" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL

      *>   WS-JPOS now past player value
      *>   Re-search for "col" from player
           MOVE "col" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS

      *>   Find "player" pos first
           MOVE SPACES TO WS-TMP
           STRING WS-QT "player" WS-QT
               DELIMITED SIZE
               INTO WS-TMP
           END-STRING
           MOVE 0 TO WS-KEY-POS
           PERFORM VARYING WS-FJV-POS
               FROM 1 BY 1
               UNTIL WS-FJV-POS
               > WS-JLEN
               OR WS-KEY-POS > 0
               IF WS-FJV-POS + 7
                   <= WS-JLEN
               AND WS-JBUF(
                   WS-FJV-POS:8)
                   = TRIM(WS-TMP)
                   MOVE WS-FJV-POS
                       TO WS-KEY-POS
               END-IF
           END-PERFORM

           IF WS-KEY-POS = 0
               EXIT PARAGRAPH
           END-IF

      *>   Now find "col" after player
           MOVE "col" TO WS-KEY-SEARCH
           MOVE WS-KEY-POS TO WS-JPOS
           PERFORM FIND-JSON-VAL

           IF TRIM(WS-JVAL)
               NOT = SPACES
               MOVE WS-JVAL(1:1)
                   TO WS-PLAYER-COL
           END-IF
           .

      *> ============================================================
      *> PARSE-BLOCKS: Extract blocks array
      *> ============================================================
       PARSE-BLOCKS.
           MOVE 0 TO WS-BLOCK-COUNT
           IF WS-JLEN = 0
               EXIT PARAGRAPH
           END-IF

      *>   Find "blocks" key
           MOVE SPACES TO WS-TMP
           STRING WS-QT "blocks" WS-QT
               DELIMITED SIZE
               INTO WS-TMP
           END-STRING

           MOVE 0 TO WS-ARR-POS
           PERFORM VARYING WS-FJV-POS
               FROM 1 BY 1
               UNTIL WS-FJV-POS
               > WS-JLEN
               OR WS-ARR-POS > 0
               IF WS-FJV-POS + 7
                   <= WS-JLEN
               AND WS-JBUF(
                   WS-FJV-POS:8)
                   = TRIM(WS-TMP)
                   MOVE WS-FJV-POS
                       TO WS-ARR-POS
               END-IF
           END-PERFORM

           IF WS-ARR-POS = 0
               EXIT PARAGRAPH
           END-IF

      *>   Skip to [ after "blocks":
           ADD 8 TO WS-ARR-POS
           PERFORM UNTIL
               WS-ARR-POS > WS-JLEN
               OR WS-JBUF(
               WS-ARR-POS:1) = "["
               ADD 1 TO WS-ARR-POS
           END-PERFORM
           ADD 1 TO WS-ARR-POS

      *>   Parse each block object
           MOVE 0 TO WS-BPARSE-I
           PERFORM UNTIL
               WS-ARR-POS > WS-JLEN
               OR WS-JBUF(
               WS-ARR-POS:1) = "]"
               OR WS-BPARSE-I >= 10

      *>       Skip whitespace/commas
               PERFORM UNTIL
                   WS-ARR-POS > WS-JLEN
                   OR WS-JBUF(
                   WS-ARR-POS:1) = "{"
                   OR WS-JBUF(
                   WS-ARR-POS:1) = "]"
                   ADD 1 TO WS-ARR-POS
               END-PERFORM

               IF WS-ARR-POS > WS-JLEN
               OR WS-JBUF(
                   WS-ARR-POS:1) = "]"
                   EXIT PERFORM
               END-IF

      *>       Found {, parse block
               ADD 1 TO WS-BPARSE-I
               ADD 1 TO WS-ARR-POS

      *>       Extract col
               MOVE "col"
                   TO WS-KEY-SEARCH
               MOVE WS-ARR-POS
                   TO WS-JPOS
               PERFORM FIND-JSON-VAL
               IF TRIM(WS-JVAL)
                   NOT = SPACES
                   MOVE WS-JVAL(1:1)
                       TO WS-BLK-COL(
                       WS-BPARSE-I)
               END-IF

      *>       Extract top_row
               MOVE "top_row"
                   TO WS-KEY-SEARCH
               MOVE WS-ARR-POS
                   TO WS-JPOS
               PERFORM FIND-JSON-VAL
               IF TRIM(WS-JVAL)
                   NOT = SPACES
                   MOVE WS-JVAL(1:1)
                       TO WS-BLK-TOP(
                       WS-BPARSE-I)
               END-IF

      *>       Extract bottom_row
               MOVE "bottom_row"
                   TO WS-KEY-SEARCH
               MOVE WS-ARR-POS
                   TO WS-JPOS
               PERFORM FIND-JSON-VAL
               IF TRIM(WS-JVAL)
                   NOT = SPACES
                   MOVE WS-JVAL(1:1)
                       TO WS-BLK-BOT(
                       WS-BPARSE-I)
               END-IF

      *>       Extract direction
               MOVE "direction"
                   TO WS-KEY-SEARCH
               MOVE WS-ARR-POS
                   TO WS-JPOS
               PERFORM FIND-JSON-VAL
               IF TRIM(WS-JVAL)
                   NOT = SPACES
                   MOVE TRIM(WS-JVAL)
                       TO WS-BLK-DIR(
                       WS-BPARSE-I)
               END-IF

      *>       Skip to end of object }
               PERFORM UNTIL
                   WS-ARR-POS
                   > WS-JLEN
                   OR WS-JBUF(
                   WS-ARR-POS:1)
                   = "}"
                   ADD 1
                       TO WS-ARR-POS
               END-PERFORM
               ADD 1 TO WS-ARR-POS
           END-PERFORM

           MOVE WS-BPARSE-I
               TO WS-BLOCK-COUNT
           .

      *> ============================================================
      *> SIMULATE-BLOCKS: Predict next positions
      *> ============================================================
       SIMULATE-BLOCKS.
           MOVE WS-BLOCK-COUNT
               TO WS-FUT-COUNT

           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I
               > WS-BLOCK-COUNT

               MOVE WS-BLK-COL(WS-I)
                   TO WS-FBK-COL(WS-I)
               MOVE WS-BLK-TOP(WS-I)
                   TO WS-FBK-TOP(WS-I)
               MOVE WS-BLK-BOT(WS-I)
                   TO WS-FBK-BOT(WS-I)
               MOVE WS-BLK-DIR(WS-I)
                   TO WS-FBK-DIR(WS-I)

      *>       Move block one step
               IF WS-FBK-DIR(WS-I)
                   = "up"
                   IF WS-FBK-TOP(WS-I)
                       > 1
                       SUBTRACT 1 FROM
                           WS-FBK-TOP(
                           WS-I)
                       SUBTRACT 1 FROM
                           WS-FBK-BOT(
                           WS-I)
                   ELSE
      *>               Bounce down
                       ADD 1 TO
                           WS-FBK-TOP(
                           WS-I)
                       ADD 1 TO
                           WS-FBK-BOT(
                           WS-I)
                       MOVE "down"
                           TO WS-FBK-DIR(
                           WS-I)
                   END-IF
               ELSE
      *>           Direction is down
                   IF WS-FBK-BOT(WS-I)
                       < WS-GRID-ROWS
                       ADD 1 TO
                           WS-FBK-TOP(
                           WS-I)
                       ADD 1 TO
                           WS-FBK-BOT(
                           WS-I)
                   ELSE
      *>               Bounce up
                       SUBTRACT 1 FROM
                           WS-FBK-TOP(
                           WS-I)
                       SUBTRACT 1 FROM
                           WS-FBK-BOT(
                           WS-I)
                       MOVE "up"
                           TO WS-FBK-DIR(
                           WS-I)
                   END-IF
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> DECIDE-MOVE: RIGHT > WAIT > LEFT
      *> ============================================================
       DECIDE-MOVE.
           PERFORM SIMULATE-BLOCKS

      *>   Try RIGHT
           IF WS-PLAYER-COL
               < WS-GRID-COLS
               COMPUTE WS-CHECK-COL =
                   WS-PLAYER-COL + 1
               PERFORM CHECK-COL-SAFE
               IF WS-COL-SAFE = "Y"
                   MOVE "right"
                       TO WS-MOVE-CMD
                   EXIT PARAGRAPH
               END-IF
           END-IF

      *>   Try WAIT
           MOVE WS-PLAYER-COL
               TO WS-CHECK-COL
           PERFORM CHECK-COL-SAFE
           IF WS-COL-SAFE = "Y"
               MOVE "wait"
                   TO WS-MOVE-CMD
               EXIT PARAGRAPH
           END-IF

      *>   Try LEFT
           IF WS-PLAYER-COL > 1
               COMPUTE WS-CHECK-COL =
                   WS-PLAYER-COL - 1
               PERFORM CHECK-COL-SAFE
               IF WS-COL-SAFE = "Y"
                   MOVE "left"
                       TO WS-MOVE-CMD
                   EXIT PARAGRAPH
               END-IF
           END-IF

      *>   All unsafe, wait
           MOVE "wait" TO WS-MOVE-CMD
           .

      *> ============================================================
      *> CHECK-COL-SAFE: Is column safe?
      *> ============================================================
       CHECK-COL-SAFE.
           MOVE "Y" TO WS-COL-SAFE

           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I
               > WS-FUT-COUNT
               IF WS-FBK-COL(WS-I)
                   = WS-CHECK-COL
               AND WS-FBK-TOP(WS-I)
                   <= WS-PLAYER-ROW
               AND WS-FBK-BOT(WS-I)
                   >= WS-PLAYER-ROW
                   MOVE "N"
                       TO WS-COL-SAFE
                   EXIT PERFORM
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> CHECK-REACHED: Check reached_goal
      *> ============================================================
       CHECK-REACHED.
           MOVE "N" TO WS-REACHED
           IF WS-JLEN = 0
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-JBUF(1:WS-JLEN)
               TALLYING WS-TALLY-CNT
               FOR ALL "reached_goal"
           IF WS-TALLY-CNT = 0
               EXIT PARAGRAPH
           END-IF

      *>   Find reached_goal value
           MOVE "reached_goal"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF TRIM(WS-JVAL) = "true"
               MOVE "Y" TO WS-REACHED
           END-IF
           .

      *> ============================================================
      *> UPDATE-STATE: Parse new state
      *> ============================================================
       UPDATE-STATE.
      *>   Try parse from response
           PERFORM PARSE-PLAYER
           PERFORM PARSE-BLOCKS

      *>   If parse failed, infer
           IF WS-BLOCK-COUNT = 0
               IF TRIM(WS-MOVE-CMD)
                   = "right"
               AND WS-PLAYER-COL < 7
                   ADD 1
                       TO WS-PLAYER-COL
               END-IF
               IF TRIM(WS-MOVE-CMD)
                   = "left"
               AND WS-PLAYER-COL > 1
                   SUBTRACT 1
                       FROM WS-PLAYER-COL
               END-IF
           END-IF
           .

      *> ============================================================
      *> AI-ANALYZE: ONE gpt-4.1-mini call
      *> ============================================================
       AI-ANALYZE.
           DISPLAY " "
           DISPLAY "--- AI ANALYSIS ---"

           MOVE SPACES TO WS-REQ-JSON
           MOVE 1 TO WS-PTR

      *>   Build JSON request
           STRING
               "{"
               WS-QT "model" WS-QT ":"
               WS-QT "gpt-4.1-mini"
               WS-QT ","
               WS-QT "messages"
               WS-QT ":["
               "{"
               WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT
               ":" WS-QT
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Prompt with game state
           STRING
               "Reactor grid 7x5. "
               "Robot at col "
               WS-PLAYER-COL
               " row 5, goal col 7"
               " row 5."
               WS-NL "Blocks: "
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Add block info
           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I
               > WS-BLOCK-COUNT
               STRING
                   "Col"
                   WS-BLK-COL(WS-I)
                   " rows"
                   WS-BLK-TOP(WS-I)
                   "-"
                   WS-BLK-BOT(WS-I)
                   " "
                   TRIM(WS-BLK-DIR(
                   WS-I))
                   "; "
                   DELIMITED SIZE
                   INTO WS-REQ-JSON
                   WITH POINTER WS-PTR
               END-STRING
           END-PERFORM

           STRING
               WS-NL
               "Blocks move 1 step"
               " per command, bounce"
               " at rows 1-5. "
               "Which columns are "
               "most dangerous? "
               "Brief strategy (3 "
               "sentences)."
               WS-QT "}],"
               WS-QT "temperature"
               WS-QT ":0,"
               WS-QT "max_tokens"
               WS-QT ":200}"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Write + call
           OPEN OUTPUT WORK-FILE
           WRITE WORK-REC
               FROM WS-REQ-JSON
           CLOSE WORK-FILE

           PERFORM CALL-OPENAI

      *>   Read AI response
           MOVE "llm_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp"
               TO WS-WORK-PATH

           IF WS-JLEN > 0
               MOVE "content"
                   TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               IF TRIM(WS-JVAL)
                   NOT = SPACES
                   DISPLAY "  AI: "
                       WS-JVAL(1:200)
               END-IF
           END-IF
           .

      *> ============================================================
      *> CALL-OPENAI: POST work.tmp
      *> ============================================================
       CALL-OPENAI.
           DISPLAY "  Calling OpenAI..."
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
               " -d @work.tmp"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD
           .

      *> ============================================================
      *> CHECK-FLAG
      *> ============================================================
       CHECK-FLAG.
           IF WS-JLEN = 0
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-JBUF(1:WS-JLEN)
               TALLYING WS-TALLY-CNT
               FOR ALL "FLG"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-SUCCESS
               DISPLAY " "
               DISPLAY "  >>> FLAG: "
                   TRIM(WS-JBUF)(1:500)
           END-IF
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
           MOVE SPACES TO WS-TMP
           STRING WS-QT
               TRIM(WS-KEY-SEARCH)
               WS-QT
               DELIMITED SIZE
               INTO WS-TMP
           END-STRING

           MOVE 0 TO WS-KEY-POS
           PERFORM VARYING WS-FJV-POS
               FROM WS-JPOS BY 1
               UNTIL WS-FJV-POS
                   > WS-JLEN
               OR WS-KEY-POS > 0
               IF WS-FJV-POS
                   + LENGTH(
                   TRIM(WS-TMP))
                   - 1 <= WS-JLEN
               AND WS-JBUF(
                   WS-FJV-POS:
                   LENGTH(
                   TRIM(WS-TMP)))
                   = TRIM(WS-TMP)
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
               TRIM(WS-TMP))
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
                       = WS-QT
                       IF WS-FJV-POS
                           > 1
                       AND WS-JBUF(
                           WS-FJV-POS
                           - 1:1)
                           = X"5C"
                           ADD 1
                           TO WS-FJV-POS
                       ELSE
                           EXIT PERFORM
                       END-IF
                   ELSE
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
                   + 1 <= 8000
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
