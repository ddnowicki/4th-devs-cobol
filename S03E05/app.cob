       IDENTIFICATION DIVISION.
       PROGRAM-ID. S03E05-SAVETHEM.
      *> ============================================================
      *> S03E05 - SaveThem: Optimal route to Skolwin.
      *> 1. Discover map tool via toolsearch (dynamic)
      *> 2. Build static tools JSON (query_map + submit_route)
      *> 3. LLM agent calls query_map("Skolwin")
      *> 4. COBOL fetches map JSON, parses 10x10 grid
      *> 5. BFS finds shortest path avoiding R/T tiles
      *> 6. Computes rocket+dismount+walk recommendation
      *> 7. LLM reads recommendation, calls submit_route
      *> 8. COBOL builds answer array, POSTs to /verify
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
       01  WORK-REC               PIC X(32000).

       WORKING-STORAGE SECTION.
      *> -- Config --
       01  WS-HUB-KEY             PIC X(100).
       01  WS-OPENAI-KEY          PIC X(200).
       01  WS-QT                  PIC X(1) VALUE '"'.
       01  WS-FS                  PIC XX.
       01  WS-WORK-PATH           PIC X(100)
                                  VALUE "work.tmp".

      *> -- URLs (all from env vars) --
       01  WS-HUB-URL             PIC X(100).
       01  WS-OPENAI-URL          PIC X(200).
       01  WS-VERIFY-URL          PIC X(200).
       01  WS-TSEARCH-URL         PIC X(200).
       01  WS-MAP-URL             PIC X(200).

      *> -- JSON newline: backslash + n --
       01  WS-NL                  PIC X(2).

      *> -- STRING pointer --
       01  WS-PTR                 PIC 9(5).

      *> -- Large JSON buffers --
       01  WS-JBUF                PIC X(32000).
       01  WS-JLEN                PIC 9(5).
       01  WS-JPOS                PIC 9(5).
       01  WS-JVAL                PIC X(8000).
       01  WS-JBUF-SAVE           PIC X(32000).
       01  WS-JLEN-SAVE           PIC 9(5).

      *> -- JSON parsing temps --
       01  WS-KEY-SEARCH          PIC X(50).
       01  WS-KEY-POS             PIC 9(5).
       01  WS-VAL-START           PIC 9(5).
       01  WS-VAL-END             PIC 9(5).
       01  WS-FJV-POS             PIC 9(5).
       01  WS-TMP                 PIC X(500).

      *> -- JSON escape in/out --
       01  WS-ESC-IN              PIC X(16000).
       01  WS-ESC-OUT             PIC X(32000).
       01  WS-ESC-ILEN            PIC 9(5).
       01  WS-ESC-OLEN            PIC 9(5).
       01  WS-ESC-I               PIC 9(5).

      *> -- Loop/misc --
       01  WS-K                   PIC 9(5).
       01  WS-I                   PIC 9(5).
       01  WS-J                   PIC 9(5).
       01  WS-EOF                 PIC X VALUE "N".
       01  WS-LINE                PIC X(16000).
       01  WS-TALLY-CNT           PIC 9(4).

      *> -- System command --
       01  WS-CMD                 PIC X(4000).

      *> -- Conversation buffer --
       01  WS-CONV-BUF            PIC X(32000).
       01  WS-CONV-PTR            PIC 9(5).

      *> -- Agent loop --
       01  WS-AG-STEP             PIC 9(2) VALUE 0.
       01  WS-AG-DONE             PIC X VALUE "N".
       01  WS-AG-MAX-STEPS        PIC 9(2) VALUE 20.
       01  WS-FLAG-FOUND          PIC X VALUE "N".
       01  WS-NUDGE-CT            PIC 9(1) VALUE 0.
       01  WS-AG-CONTENT          PIC X(4000).

      *> -- Tool call parsing --
       01  WS-TOOL-NAME           PIC X(50).
       01  WS-TOOL-CALL-ID        PIC X(100).
       01  WS-TOOL-ARGS           PIC X(4000).
       01  WS-TOOL-RESULT         PIC X(16000).
       01  WS-TOOL-RESULT-LEN     PIC 9(5).
       01  WS-TC-COUNT            PIC 9(2).
       01  WS-TC-IDX              PIC 9(2).
       01  WS-TC-IDS.
           05 WS-TC-ID OCCURS 10 TIMES
                                  PIC X(100).
       01  WS-TC-NAMES.
           05 WS-TC-NM OCCURS 10 TIMES
                                  PIC X(50).
       01  WS-TC-ARGSS.
           05 WS-TC-AR OCCURS 10 TIMES
                                  PIC X(4000).
       01  WS-TC-RESULTS.
           05 WS-TC-RS OCCURS 10 TIMES
                                  PIC X(8000).
       01  WS-TC-RS-LENS.
           05 WS-TC-RL OCCURS 10 TIMES
                                  PIC 9(5).
       01  WS-TC-SECTION          PIC X(8000).
       01  WS-TC-SECTION-LEN      PIC 9(5).
       01  WS-BRACE-DEPTH         PIC 9(3).
       01  WS-BRACE-START         PIC 9(5).
       01  WS-SEARCH-POS          PIC 9(5).
       01  WS-SEARCH-I            PIC 9(5).

      *> -- LLM request / tools JSON --
       01  WS-REQ-JSON            PIC X(32000).
       01  WS-TOOLS-JSON          PIC X(4000).
       01  WS-TOOLS-PTR           PIC 9(4).

      *> -- Retry --
       01  WS-RETRY-CT            PIC 9(2) VALUE 0.
       01  WS-SLEEP-SECS          PIC 9(3).

      *> -- Response buffer --
       01  WS-RESP-BUF            PIC X(16000).
       01  WS-RESP-LEN            PIC 9(5).

      *> -- Grid (flat 100 cells, row-major 0..9) --
       01  WS-GRID                PIC X(100).
       01  WS-CELL-CNT            PIC 9(3).
       01  WS-MAP-DEPTH           PIC 9(2).
       01  WS-CELL                PIC X(1).
       01  WS-START-R             PIC 9.
       01  WS-START-C             PIC 9.
       01  WS-GOAL-R              PIC 9.
       01  WS-GOAL-C              PIC 9.

      *> -- BFS visited / from tables (100 cells) --
       01  WS-BV-TABLE.
           05 WS-BV OCCURS 100 TIMES
                                  PIC X.
       01  WS-BFR-TABLE.
           05 WS-BFR OCCURS 100 TIMES
                                  PIC 9.
       01  WS-BFC-TABLE.
           05 WS-BFC OCCURS 100 TIMES
                                  PIC 9.
       01  WS-BFD-TABLE.
           05 WS-BFD OCCURS 100 TIMES
                                  PIC X(6).

      *> -- BFS queue (up to 200 entries) --
       01  WS-BQ-R-TABLE.
           05 WS-BQ-R OCCURS 200 TIMES
                                  PIC 9.
       01  WS-BQ-C-TABLE.
           05 WS-BQ-C OCCURS 200 TIMES
                                  PIC 9.
       01  WS-BQ-H               PIC 9(3).
       01  WS-BQ-T               PIC 9(3).

      *> -- BFS helpers --
       01  WS-BFS-FOUND           PIC X VALUE "N".
       01  WS-CUR-R               PIC 9.
       01  WS-CUR-C               PIC 9.
       01  WS-IDX                 PIC 9(3).
       01  WS-NR                  PIC S9(2).
       01  WS-NC                  PIC S9(2).
       01  WS-NI                  PIC 9(3).
       01  WS-DI                  PIC 9.

      *> -- BFS direction table (up/down/left/right) --
       01  WS-DIR-DR.
           05 WS-DDR OCCURS 4 TIMES
                                  PIC S9(2).
       01  WS-DIR-DC.
           05 WS-DDC OCCURS 4 TIMES
                                  PIC S9(2).
       01  WS-DIR-NM.
           05 WS-DDN OCCURS 4 TIMES
                                  PIC X(6).

      *> -- Path (raw=reverse from goal, fwd=correct) --
       01  WS-PATH-LEN            PIC 9(2).
       01  WS-PATH-D.
           05 WS-PD OCCURS 30 TIMES
                                  PIC X(6).
       01  WS-PATH-F.
           05 WS-PF OCCURS 30 TIMES
                                  PIC X(6).

      *> -- Water crossing detection --
       01  WS-WATER-IDX           PIC 9(2) VALUE 99.
       01  WS-TR                  PIC 9.
       01  WS-TC-VAR              PIC 9.

      *> -- submit_route args --
       01  WS-SUB-VEHICLE         PIC X(15).
       01  WS-SUB-MV-CNT          PIC 9(2).
       01  WS-SUB-MOVES.
           05 WS-SM OCCURS 35 TIMES
                                  PIC X(10).

      *> -- Misc parse helpers --
       01  WS-MP-POS              PIC 9(5).
       01  WS-CITY-NAME           PIC X(50).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S03E05 SAVETHEM ==="

           ACCEPT WS-HUB-KEY
               FROM ENVIRONMENT "HUB_API_KEY"
           ACCEPT WS-OPENAI-KEY
               FROM ENVIRONMENT "OPENAI_API_KEY"
           ACCEPT WS-HUB-URL
               FROM ENVIRONMENT "HUB_API_URL"
           ACCEPT WS-OPENAI-URL
               FROM ENVIRONMENT "OPENAI_API_URL"

           IF WS-HUB-KEY = SPACES
               DISPLAY "ERROR: HUB_API_KEY missing"
               STOP RUN
           END-IF
           IF WS-OPENAI-KEY = SPACES
               DISPLAY "ERROR: OPENAI_API_KEY missing"
               STOP RUN
           END-IF
           IF WS-HUB-URL = SPACES
               DISPLAY "ERROR: HUB_API_URL missing"
               STOP RUN
           END-IF
           IF WS-OPENAI-URL = SPACES
               DISPLAY "ERROR: OPENAI_API_URL missing"
               STOP RUN
           END-IF

      *>   Build derived URLs
           INITIALIZE WS-VERIFY-URL
           STRING TRIM(WS-HUB-URL) "/verify"
               DELIMITED SIZE INTO WS-VERIFY-URL
           END-STRING

           INITIALIZE WS-TSEARCH-URL
           STRING TRIM(WS-HUB-URL)
               "/api/toolsearch"
               DELIMITED SIZE INTO WS-TSEARCH-URL
           END-STRING

      *>   Init JSON newline (backslash-n)
           MOVE X"5C" TO WS-NL(1:1)
           MOVE "n"   TO WS-NL(2:1)

      *>   Init BFS direction table
           MOVE -1 TO WS-DDR(1)
           MOVE  0 TO WS-DDC(1)
           MOVE "up" TO WS-DDN(1)
           MOVE  1 TO WS-DDR(2)
           MOVE  0 TO WS-DDC(2)
           MOVE "down" TO WS-DDN(2)
           MOVE  0 TO WS-DDR(3)
           MOVE -1 TO WS-DDC(3)
           MOVE "left" TO WS-DDN(3)
           MOVE  0 TO WS-DDR(4)
           MOVE  1 TO WS-DDC(4)
           MOVE "right" TO WS-DDN(4)

      *>   Phase 1: Discover map tool
           DISPLAY " "
           DISPLAY "[PHASE 1] Discovering tools..."
           PERFORM DISCOVER-MAP-TOOL

           IF WS-MAP-URL = SPACES
               DISPLAY "ERROR: map tool not found!"
               STOP RUN
           END-IF
           DISPLAY "  Map URL: " TRIM(WS-MAP-URL)

      *>   Phase 2: Build static tools JSON
           PERFORM BUILD-TOOLS-JSON

      *>   Phase 3: Run agent
           DISPLAY " "
           DISPLAY "[PHASE 2] Running agent..."
           PERFORM RUN-AGENT

           DISPLAY " "
           DISPLAY "=== DONE ==="
           STOP RUN.

      *> ============================================================
      *> DISCOVER-MAP-TOOL: toolsearch for map tool URL
      *> ============================================================
       DISCOVER-MAP-TOOL.
           DISPLAY "  Querying toolsearch..."
           INITIALIZE WS-RESP-BUF
           MOVE 1 TO WS-PTR
           STRING
               '{"apikey":"'
               TRIM(WS-HUB-KEY)
               '","query":"map terrain'
               ' grid 10x10 cells"}'
               DELIMITED SIZE
               INTO WS-RESP-BUF
               WITH POINTER WS-PTR
           END-STRING

           MOVE "ts_req.json" TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           WRITE WORK-REC FROM WS-RESP-BUF
           CLOSE WORK-FILE

           INITIALIZE WS-CMD
           STRING
               "curl -s -o ts_resp.json"
               " -X POST "
               TRIM(WS-TSEARCH-URL)
               " -H " WS-QT
               "Content-Type: "
               "application/json" WS-QT
               " -d @ts_req.json"
               DELIMITED SIZE INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           MOVE "ts_resp.json" TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           IF WS-JLEN = 0
               DISPLAY "  Toolsearch empty!"
               EXIT PARAGRAPH
           END-IF

           DISPLAY "  Toolsearch: "
               WS-JBUF(1:200)

      *>   Extract first "url" value
           MOVE 0 TO WS-MP-POS
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-JLEN - 4
               OR WS-MP-POS > 0
               IF WS-JBUF(WS-I:5) = '"url"'
                   MOVE WS-I TO WS-MP-POS
               END-IF
           END-PERFORM

           IF WS-MP-POS = 0
               DISPLAY "  No url in toolsearch!"
               EXIT PARAGRAPH
           END-IF

           MOVE WS-MP-POS TO WS-JPOS
           MOVE "url" TO WS-KEY-SEARCH
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL) TO WS-MAP-URL

      *>   If relative URL, prepend hub base URL
           IF WS-MAP-URL(1:1) = "/"
               INITIALIZE WS-TMP
               STRING TRIM(WS-HUB-URL)
                   TRIM(WS-MAP-URL)
                   DELIMITED SIZE INTO WS-TMP
               END-STRING
               MOVE WS-TMP TO WS-MAP-URL
           END-IF
           .

      *> ============================================================
      *> BUILD-TOOLS-JSON: static tool defs for agent
      *> ============================================================
       BUILD-TOOLS-JSON.
           INITIALIZE WS-TOOLS-JSON
           MOVE 1 TO WS-TOOLS-PTR

      *>   Tool 1: query_map
           STRING
               '{"type":"function",'
               '"function":{'
               '"name":"query_map",'
               '"description":"Query'
               ' terrain map. Pass city'
               ' name as query.'
               ' Returns 10x10 grid AND'
               ' RECOMMENDED ROUTE.'
               ' Call with Skolwin.",'
               '"parameters":{'
               '"type":"object",'
               '"properties":{'
               '"query":{'
               '"type":"string"}},'
               '"required":["query"]}}}'
               DELIMITED SIZE
               INTO WS-TOOLS-JSON
               WITH POINTER WS-TOOLS-PTR
           END-STRING

      *>   Tool 2: submit_route
           STRING
               ',{"type":"function",'
               '"function":{'
               '"name":"submit_route",'
               '"description":"Submit'
               ' route. vehicle=start'
               ' vehicle name.'
               ' moves=array of'
               ' directions and'
               ' optional dismount.'
               ' Call ONCE with the'
               ' RECOMMENDED ROUTE.",'
               '"parameters":{'
               '"type":"object",'
               '"properties":{'
               '"vehicle":{'
               '"type":"string"},'
               '"moves":{'
               '"type":"array",'
               '"items":{'
               '"type":"string"}}},'
               '"required":['
               '"vehicle","moves"]}}}'
               DELIMITED SIZE
               INTO WS-TOOLS-JSON
               WITH POINTER WS-TOOLS-PTR
           END-STRING

           DISPLAY "  Tools JSON built ("
               WS-TOOLS-PTR " chars)"
           .

      *> ============================================================
      *> RUN-AGENT: Main agent loop
      *> ============================================================
       RUN-AGENT.
           MOVE SPACES TO WS-CONV-BUF
           MOVE 1 TO WS-CONV-PTR

      *>   System prompt - start
           STRING
               '[{"role":"system",'
               '"content":"You are a'
               ' route planning agent.'
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               ' Guide messenger S to G'
               ' on 10x10 grid.'
               WS-NL WS-NL
               'BUDGET: 10 fuel,'
               ' 10 food total.'
               WS-NL WS-NL
               'VEHICLES per move:'
               WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               'rocket fuel=1.0'
               ' food=0.1 NO water.'
               WS-NL
               'car fuel=0.7'
               ' food=1.0 NO water.'
               WS-NL
               'horse fuel=0.0'
               ' food=1.6 CAN water.'
               WS-NL
               'walk fuel=0.0'
               ' food=2.5 CAN water.'
               WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               'DISMOUNT: insert'
               ' dismount in moves to'
               ' switch to walk mode.'
               WS-NL WS-NL
               'STEPS:'
               WS-NL
               '1. Call query_map'
               ' query=Skolwin'
               WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               '2. Read RECOMMENDED'
               ' ROUTE in the response'
               WS-NL
               '3. Call submit_route'
               ' with EXACTLY that'
               ' vehicle and moves."}'
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

      *>   User message
           STRING
               ',{"role":"user",'
               '"content":"Plan and'
               ' submit the optimal'
               ' route to Skolwin."}]'
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           MOVE "N" TO WS-AG-DONE
           MOVE "N" TO WS-FLAG-FOUND
           MOVE 0 TO WS-AG-STEP
           MOVE 0 TO WS-NUDGE-CT

           PERFORM UNTIL WS-AG-DONE = "Y"
               OR WS-AG-STEP >= WS-AG-MAX-STEPS

               ADD 1 TO WS-AG-STEP
               DISPLAY " "
               DISPLAY "--- Agent step "
                   WS-AG-STEP " ---"

               PERFORM SEND-AGENT-REQUEST

               MOVE "agent_resp.json"
                   TO WS-WORK-PATH
               PERFORM READ-JSON-FILE
               MOVE "work.tmp"
                   TO WS-WORK-PATH

               IF WS-JLEN = 0
                   DISPLAY "  ERROR: empty!"
                   MOVE "Y" TO WS-AG-DONE
                   EXIT PERFORM
               END-IF

      *>         Check for API error
               MOVE 0 TO WS-TALLY-CNT
               INSPECT WS-JBUF(1:WS-JLEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL '"choices"'
               IF WS-TALLY-CNT = 0
                   DISPLAY "  API ERROR: "
                       WS-JBUF(1:300)
                   MOVE "Y" TO WS-AG-DONE
                   EXIT PERFORM
               END-IF

      *>         Check for tool_calls
               MOVE 0 TO WS-TALLY-CNT
               INSPECT WS-JBUF(1:WS-JLEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL '"tool_calls"'

               IF WS-TALLY-CNT > 0
                   PERFORM PARSE-ALL-TOOL-CALLS
                   IF WS-TC-COUNT > 0
                       MOVE 0 TO WS-NUDGE-CT
                       PERFORM
                           EXECUTE-ALL-TOOL-CALLS
                   ELSE
                       PERFORM HANDLE-TEXT-RESP
                   END-IF
               ELSE
                   PERFORM HANDLE-TEXT-RESP
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> SEND-AGENT-REQUEST
      *> ============================================================
       SEND-AGENT-REQUEST.
           MOVE SPACES TO WS-REQ-JSON
           MOVE 1 TO WS-PTR

           STRING
               '{"model":"gpt-4.1-mini"'
               ',"messages":'
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Copy conversation
           COMPUTE WS-K = WS-CONV-PTR - 1
           IF WS-K > 0
               MOVE WS-CONV-BUF(1:WS-K)
                   TO WS-REQ-JSON(WS-PTR:WS-K)
               ADD WS-K TO WS-PTR
           END-IF

      *>   Add tools
           STRING
               ',"tools":['
               TRIM(WS-TOOLS-JSON)
               '],"tool_choice":"auto"'
               ',"temperature":0}'
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           MOVE "agent_req.json"
               TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           WRITE WORK-REC FROM WS-REQ-JSON
           CLOSE WORK-FILE

      *>   Call OpenAI with retry
           MOVE 0 TO WS-RETRY-CT
           PERFORM UNTIL WS-RETRY-CT >= 5
               DISPLAY "  Calling LLM..."
               INITIALIZE WS-CMD
               STRING
                   "curl -s"
                   " -o agent_resp.json"
                   " -X POST "
                   TRIM(WS-OPENAI-URL)
                   " -H " WS-QT
                   "Content-Type: "
                   "application/json"
                   WS-QT
                   " -H " WS-QT
                   "Authorization: Bearer "
                   TRIM(WS-OPENAI-KEY)
                   WS-QT
                   " -d @agent_req.json"
                   DELIMITED SIZE
                   INTO WS-CMD
               END-STRING
               CALL "SYSTEM" USING WS-CMD

               MOVE "agent_resp.json"
                   TO WS-WORK-PATH
               PERFORM READ-JSON-FILE
               MOVE "work.tmp"
                   TO WS-WORK-PATH

               IF WS-JLEN > 10
                   MOVE 0 TO WS-TALLY-CNT
                   INSPECT WS-JBUF(1:WS-JLEN)
                       TALLYING WS-TALLY-CNT
                       FOR ALL '"choices"'
                   IF WS-TALLY-CNT > 0
                       EXIT PERFORM
                   END-IF
               END-IF

               ADD 1 TO WS-RETRY-CT
               IF WS-RETRY-CT < 5
                   DISPLAY "  LLM retry "
                       WS-RETRY-CT
                   MOVE 3 TO WS-SLEEP-SECS
                   CALL "C$SLEEP"
                       USING WS-SLEEP-SECS
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> PARSE-ALL-TOOL-CALLS: Extract tool call array
      *> ============================================================
       PARSE-ALL-TOOL-CALLS.
           MOVE 0 TO WS-TC-COUNT

      *>   Find "tool_calls"
           MOVE 0 TO WS-SEARCH-POS
           PERFORM VARYING WS-SEARCH-I
               FROM 1 BY 1
               UNTIL WS-SEARCH-I
                   > WS-JLEN - 11
               OR WS-SEARCH-POS > 0
               IF WS-JBUF(WS-SEARCH-I:12)
                   = '"tool_calls"'
                   MOVE WS-SEARCH-I
                       TO WS-SEARCH-POS
               END-IF
           END-PERFORM

           IF WS-SEARCH-POS = 0
               EXIT PARAGRAPH
           END-IF

      *>   Find opening [
           MOVE WS-SEARCH-POS
               TO WS-SEARCH-I
           PERFORM UNTIL
               WS-SEARCH-I > WS-JLEN
               OR WS-JBUF(WS-SEARCH-I:1)
                   = "["
               ADD 1 TO WS-SEARCH-I
           END-PERFORM

      *>   Extract entire [...] array
           MOVE WS-SEARCH-I
               TO WS-BRACE-START
           MOVE 1 TO WS-BRACE-DEPTH
           ADD 1 TO WS-SEARCH-I
           PERFORM UNTIL
               WS-SEARCH-I > WS-JLEN
               OR WS-BRACE-DEPTH = 0
               IF WS-JBUF(WS-SEARCH-I:1)
                   = "["
                   ADD 1 TO WS-BRACE-DEPTH
               END-IF
               IF WS-JBUF(WS-SEARCH-I:1)
                   = "]"
                   SUBTRACT 1
                       FROM WS-BRACE-DEPTH
               END-IF
               ADD 1 TO WS-SEARCH-I
           END-PERFORM

           COMPUTE WS-TC-SECTION-LEN =
               WS-SEARCH-I - WS-BRACE-START
           IF WS-TC-SECTION-LEN > 8000
               MOVE 8000
                   TO WS-TC-SECTION-LEN
           END-IF
           MOVE WS-JBUF(WS-BRACE-START:
               WS-TC-SECTION-LEN)
               TO WS-TC-SECTION

      *>   Iterate: find "id" for each call
           MOVE 1 TO WS-SEARCH-I
           PERFORM UNTIL WS-SEARCH-I
               >= WS-TC-SECTION-LEN
               OR WS-TC-COUNT >= 10

               MOVE 0 TO WS-SEARCH-POS
               PERFORM UNTIL WS-SEARCH-I
                   >= WS-TC-SECTION-LEN - 3
                   OR WS-SEARCH-POS > 0
                   IF WS-TC-SECTION(
                       WS-SEARCH-I:4)
                       = '"id"'
                       MOVE WS-SEARCH-I
                           TO WS-SEARCH-POS
                   END-IF
                   ADD 1 TO WS-SEARCH-I
               END-PERFORM

               IF WS-SEARCH-POS = 0
                   EXIT PERFORM
               END-IF

               ADD 1 TO WS-TC-COUNT

      *>         Parse via JBUF swap
               MOVE WS-JBUF TO WS-JBUF-SAVE
               MOVE WS-JLEN TO WS-JLEN-SAVE
               MOVE WS-TC-SECTION TO WS-JBUF
               MOVE WS-TC-SECTION-LEN
                   TO WS-JLEN

               MOVE "id" TO WS-KEY-SEARCH
               MOVE WS-SEARCH-POS TO WS-JPOS
               PERFORM FIND-JSON-VAL
               MOVE TRIM(WS-JVAL)
                   TO WS-TC-ID(WS-TC-COUNT)

               MOVE "name"
                   TO WS-KEY-SEARCH
               PERFORM FIND-JSON-VAL
               MOVE TRIM(WS-JVAL)
                   TO WS-TC-NM(WS-TC-COUNT)

               MOVE "arguments"
                   TO WS-KEY-SEARCH
               PERFORM FIND-JSON-VAL

      *>         Unescape arguments
               MOVE WS-JVAL TO WS-ESC-IN
               PERFORM JSON-UNESCAPE-STR
               MOVE TRIM(WS-ESC-OUT)
                   TO WS-TC-AR(WS-TC-COUNT)

               DISPLAY "  TC"
                   WS-TC-COUNT ": "
                   TRIM(WS-TC-NM(
                       WS-TC-COUNT))
                   "("
                   TRIM(WS-TC-AR(
                       WS-TC-COUNT))
                       (1:200)
                   ")"

               MOVE WS-JBUF-SAVE TO WS-JBUF
               MOVE WS-JLEN-SAVE TO WS-JLEN
           END-PERFORM

           DISPLAY "  Tool calls: "
               WS-TC-COUNT
           .

      *> ============================================================
      *> EXECUTE-ALL-TOOL-CALLS
      *> ============================================================
       EXECUTE-ALL-TOOL-CALLS.
           PERFORM VARYING WS-TC-IDX
               FROM 1 BY 1
               UNTIL WS-TC-IDX > WS-TC-COUNT
               OR WS-FLAG-FOUND = "Y"

               MOVE WS-TC-NM(WS-TC-IDX)
                   TO WS-TOOL-NAME
               MOVE WS-TC-AR(WS-TC-IDX)
                   TO WS-TOOL-ARGS
               MOVE WS-TC-ID(WS-TC-IDX)
                   TO WS-TOOL-CALL-ID

               PERFORM DISPATCH-TOOL

               MOVE WS-TOOL-RESULT-LEN
                   TO WS-TC-RL(WS-TC-IDX)
               IF WS-TOOL-RESULT-LEN > 8000
                   MOVE 8000
                       TO WS-TC-RL(WS-TC-IDX)
               END-IF
               MOVE WS-TOOL-RESULT(1:
                   WS-TC-RL(WS-TC-IDX))
                   TO WS-TC-RS(WS-TC-IDX)
           END-PERFORM

           IF WS-FLAG-FOUND = "Y"
               EXIT PARAGRAPH
           END-IF

           PERFORM APPEND-MULTI-TOOL-EXCHANGE
           .

      *> ============================================================
      *> DISPATCH-TOOL: Route to handler
      *> ============================================================
       DISPATCH-TOOL.
           MOVE SPACES TO WS-TOOL-RESULT
           MOVE 0 TO WS-TOOL-RESULT-LEN

           IF TRIM(WS-TOOL-NAME) =
               "query_map"
               PERFORM TOOL-QUERY-MAP
               EXIT PARAGRAPH
           END-IF

           IF TRIM(WS-TOOL-NAME) =
               "submit_route"
               PERFORM TOOL-SUBMIT-ROUTE
               EXIT PARAGRAPH
           END-IF

           MOVE '{"error":"Unknown tool"}'
               TO WS-TOOL-RESULT
           MOVE 24 TO WS-TOOL-RESULT-LEN
           .

      *> ============================================================
      *> TOOL-QUERY-MAP: Fetch map, BFS, return recommendation
      *> ============================================================
       TOOL-QUERY-MAP.
      *>   Parse query arg
           MOVE WS-JBUF TO WS-JBUF-SAVE
           MOVE WS-JLEN TO WS-JLEN-SAVE

           MOVE WS-TOOL-ARGS TO WS-JBUF
           MOVE LENGTH(TRIM(WS-TOOL-ARGS))
               TO WS-JLEN
           MOVE "query" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL) TO WS-CITY-NAME

           MOVE WS-JBUF-SAVE TO WS-JBUF
           MOVE WS-JLEN-SAVE TO WS-JLEN

           DISPLAY "  [query_map] city="
               TRIM(WS-CITY-NAME)

      *>   Escape city name for JSON
           MOVE TRIM(WS-CITY-NAME)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

      *>   Build map request
           INITIALIZE WS-RESP-BUF
           MOVE 1 TO WS-PTR
           STRING
               '{"apikey":"'
               TRIM(WS-HUB-KEY)
               '","query":"'
               WS-ESC-OUT(1:WS-ESC-OLEN)
               '"}'
               DELIMITED SIZE
               INTO WS-RESP-BUF
               WITH POINTER WS-PTR
           END-STRING

           MOVE "map_req.json"
               TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           WRITE WORK-REC FROM WS-RESP-BUF
           CLOSE WORK-FILE

      *>   Fetch map via curl
           INITIALIZE WS-CMD
           STRING
               "curl -s -o map_resp.json"
               " -X POST "
               TRIM(WS-MAP-URL)
               " -H " WS-QT
               "Content-Type: "
               "application/json" WS-QT
               " -d @map_req.json"
               DELIMITED SIZE INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Read map response
           MOVE "map_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           IF WS-JLEN = 0
               MOVE '{"error":"empty map"}'
                   TO WS-TOOL-RESULT
               MOVE 21
                   TO WS-TOOL-RESULT-LEN
               EXIT PARAGRAPH
           END-IF

           DISPLAY "  Map resp: "
               WS-JBUF(1:200)

      *>   Parse grid from JSON
           PERFORM PARSE-GRID-JSON

           IF WS-CELL-CNT < 100
               DISPLAY "  Grid parse fail: "
                   WS-CELL-CNT " cells"
               MOVE
                   '{"error":"grid parse"}'
                   TO WS-TOOL-RESULT
               MOVE 23
                   TO WS-TOOL-RESULT-LEN
               EXIT PARAGRAPH
           END-IF

           DISPLAY "  Grid: S=("
               WS-START-R ","
               WS-START-C ") G=("
               WS-GOAL-R ","
               WS-GOAL-C ")"

      *>   Run BFS
           PERFORM RUN-BFS-PATH

           IF WS-BFS-FOUND = "N"
               MOVE
                   '{"error":"no path"}'
                   TO WS-TOOL-RESULT
               MOVE 19
                   TO WS-TOOL-RESULT-LEN
               EXIT PARAGRAPH
           END-IF

      *>   Find first water step
           PERFORM FIND-WATER-STEP

      *>   Build formatted response
           PERFORM BUILD-MAP-RESPONSE
           .

      *> ============================================================
      *> PARSE-GRID-JSON: Extract 10x10 grid from map JSON
      *> ============================================================
       PARSE-GRID-JSON.
           MOVE 0 TO WS-CELL-CNT
           MOVE SPACES TO WS-GRID
           MOVE 0 TO WS-START-R
           MOVE 0 TO WS-START-C
           MOVE 9 TO WS-GOAL-R
           MOVE 9 TO WS-GOAL-C

      *>   Find "map" key
           MOVE 0 TO WS-MP-POS
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-JLEN - 4
               OR WS-MP-POS > 0
               IF WS-JBUF(WS-I:5) = '"map"'
                   MOVE WS-I TO WS-MP-POS
               END-IF
           END-PERFORM

           IF WS-MP-POS = 0
               EXIT PARAGRAPH
           END-IF

      *>   Find first [ (outer map array)
           MOVE WS-MP-POS TO WS-I
           PERFORM UNTIL WS-I > WS-JLEN
               OR WS-JBUF(WS-I:1) = "["
               ADD 1 TO WS-I
           END-PERFORM
           ADD 1 TO WS-I

      *>   Scan with depth tracking
           MOVE 1 TO WS-MAP-DEPTH

           PERFORM UNTIL WS-I > WS-JLEN
               OR WS-MAP-DEPTH = 0

               EVALUATE WS-JBUF(WS-I:1)

               WHEN "["
                   ADD 1 TO WS-MAP-DEPTH
                   ADD 1 TO WS-I

               WHEN "]"
                   SUBTRACT 1
                       FROM WS-MAP-DEPTH
                   ADD 1 TO WS-I

               WHEN WS-QT
      *>             Quoted single-char cell
                   ADD 1 TO WS-I
                   MOVE WS-JBUF(WS-I:1)
                       TO WS-CELL
                   ADD 1 TO WS-I
      *>             Skip closing quote
                   ADD 1 TO WS-I

                   ADD 1 TO WS-CELL-CNT
                   IF WS-CELL-CNT <= 100
                       MOVE WS-CELL TO
                           WS-GRID(
                           WS-CELL-CNT:1)
      *>                 Calc row/col (0-based)
                       COMPUTE WS-J =
                           (WS-CELL-CNT - 1)
                       COMPUTE WS-K =
                           WS-J / 10
                       COMPUTE WS-J =
                           FUNCTION MOD(
                           WS-J, 10)
      *>                 k=row, j=col
                       IF WS-CELL = "S"
                           MOVE WS-K
                               TO WS-START-R
                           MOVE WS-J
                               TO WS-START-C
                       END-IF
                       IF WS-CELL = "G"
                           MOVE WS-K
                               TO WS-GOAL-R
                           MOVE WS-J
                               TO WS-GOAL-C
                       END-IF
                   END-IF

               WHEN OTHER
                   ADD 1 TO WS-I

               END-EVALUATE
           END-PERFORM
           .

      *> ============================================================
      *> RUN-BFS-PATH: BFS from S to G, avoiding R/T
      *> ============================================================
       RUN-BFS-PATH.
           MOVE "N" TO WS-BFS-FOUND
           MOVE 0 TO WS-PATH-LEN

      *>   Initialize visited / from tables
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 100
               MOVE "N" TO WS-BV(WS-I)
               MOVE 0 TO WS-BFR(WS-I)
               MOVE 0 TO WS-BFC(WS-I)
               MOVE SPACES TO WS-BFD(WS-I)
           END-PERFORM

      *>   Enqueue start
           MOVE 1 TO WS-BQ-H
           MOVE 1 TO WS-BQ-T
           COMPUTE WS-IDX =
               WS-START-R * 10
               + WS-START-C + 1
           MOVE "Y" TO WS-BV(WS-IDX)
           MOVE WS-START-R
               TO WS-BQ-R(WS-BQ-T)
           MOVE WS-START-C
               TO WS-BQ-C(WS-BQ-T)
           ADD 1 TO WS-BQ-T

      *>   BFS loop
           PERFORM UNTIL WS-BQ-H >= WS-BQ-T
               MOVE WS-BQ-R(WS-BQ-H)
                   TO WS-CUR-R
               MOVE WS-BQ-C(WS-BQ-H)
                   TO WS-CUR-C
               ADD 1 TO WS-BQ-H

      *>         Check goal
               IF WS-CUR-R = WS-GOAL-R
               AND WS-CUR-C = WS-GOAL-C
                   MOVE "Y" TO WS-BFS-FOUND
                   EXIT PERFORM
               END-IF

      *>         Try 4 directions
               PERFORM VARYING WS-DI
                   FROM 1 BY 1
                   UNTIL WS-DI > 4

                   COMPUTE WS-NR =
                       WS-CUR-R
                       + WS-DDR(WS-DI)
                   COMPUTE WS-NC =
                       WS-CUR-C
                       + WS-DDC(WS-DI)

      *>             Bounds check
                   IF WS-NR >= 0
                   AND WS-NR <= 9
                   AND WS-NC >= 0
                   AND WS-NC <= 9
                       COMPUTE WS-NI =
                           WS-NR * 10
                           + WS-NC + 1
                       MOVE WS-GRID(
                           WS-NI:1)
                           TO WS-CELL
      *>                 Passable and unvisited?
                       IF WS-CELL NOT = "R"
                       AND WS-CELL NOT = "T"
                       AND WS-BV(WS-NI) = "N"
                           MOVE "Y"
                               TO WS-BV(
                               WS-NI)
                           MOVE WS-CUR-R
                               TO WS-BFR(
                               WS-NI)
                           MOVE WS-CUR-C
                               TO WS-BFC(
                               WS-NI)
                           MOVE WS-DDN(
                               WS-DI)
                               TO WS-BFD(
                               WS-NI)
                           MOVE WS-NR
                               TO WS-BQ-R(
                               WS-BQ-T)
                           MOVE WS-NC
                               TO WS-BQ-C(
                               WS-BQ-T)
                           ADD 1 TO WS-BQ-T
                           IF WS-BQ-T > 200
                               MOVE 1
                                 TO WS-BQ-T
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM

           IF WS-BFS-FOUND = "N"
               EXIT PARAGRAPH
           END-IF

      *>   Reconstruct path (trace back from goal)
           MOVE WS-GOAL-R TO WS-TR
           MOVE WS-GOAL-C TO WS-TC-VAR
           MOVE 0 TO WS-PATH-LEN

           PERFORM UNTIL WS-TR = WS-START-R
               AND WS-TC-VAR = WS-START-C

               COMPUTE WS-IDX =
                   WS-TR * 10
                   + WS-TC-VAR + 1

               ADD 1 TO WS-PATH-LEN
               MOVE WS-BFD(WS-IDX)
                   TO WS-PD(WS-PATH-LEN)

               MOVE WS-BFR(WS-IDX) TO WS-I
               MOVE WS-BFC(WS-IDX) TO WS-J
               MOVE WS-I TO WS-TR
               MOVE WS-J TO WS-TC-VAR
           END-PERFORM

      *>   Reverse into WS-PATH-F
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-PATH-LEN
               COMPUTE WS-J =
                   WS-PATH-LEN - WS-I + 1
               MOVE WS-PD(WS-J)
                   TO WS-PF(WS-I)
           END-PERFORM

           DISPLAY "  BFS path len="
               WS-PATH-LEN
           .

      *> ============================================================
      *> FIND-WATER-STEP: first 1-based step landing on W
      *> ============================================================
       FIND-WATER-STEP.
           MOVE 99 TO WS-WATER-IDX
           MOVE WS-START-R TO WS-TR
           MOVE WS-START-C TO WS-TC-VAR

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-PATH-LEN

               EVALUATE TRIM(WS-PF(WS-I))
               WHEN "up"
                   SUBTRACT 1 FROM WS-TR
               WHEN "down"
                   ADD 1 TO WS-TR
               WHEN "left"
                   SUBTRACT 1
                       FROM WS-TC-VAR
               WHEN "right"
                   ADD 1 TO WS-TC-VAR
               WHEN OTHER
                   CONTINUE
               END-EVALUATE

               COMPUTE WS-IDX =
                   WS-TR * 10
                   + WS-TC-VAR + 1
               IF WS-GRID(WS-IDX:1) = "W"
               AND WS-WATER-IDX = 99
                   MOVE WS-I
                       TO WS-WATER-IDX
               END-IF
           END-PERFORM

           IF WS-WATER-IDX < 99
               DISPLAY "  Water at step "
                   WS-WATER-IDX
           ELSE
               DISPLAY "  No water on path"
           END-IF
           .

      *> ============================================================
      *> BUILD-MAP-RESPONSE: Build text for LLM tool result
      *> ============================================================
       BUILD-MAP-RESPONSE.
           MOVE SPACES TO WS-TOOL-RESULT
           MOVE 1 TO WS-PTR

      *>   Grid header
           STRING
               "Map " TRIM(WS-CITY-NAME)
               " 10x10. "
               "S=(" WS-START-R ","
               WS-START-C ") "
               "G=(" WS-GOAL-R ","
               WS-GOAL-C "). "
               DELIMITED SIZE
               INTO WS-TOOL-RESULT
               WITH POINTER WS-PTR
           END-STRING

      *>   Legend
           STRING
               "Legend: .=passable "
               "W=water R=rock T=tree."
               " W can be crossed only"
               " by walk/horse."
               " rocket/car blocked by"
               " W. "
               DELIMITED SIZE
               INTO WS-TOOL-RESULT
               WITH POINTER WS-PTR
           END-STRING

      *>   Path length
           STRING
               "OPTIMAL PATH "
               WS-PATH-LEN " steps. "
               "Moves: "
               DELIMITED SIZE
               INTO WS-TOOL-RESULT
               WITH POINTER WS-PTR
           END-STRING

      *>   List all moves
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-PATH-LEN
               STRING
                   TRIM(WS-PF(WS-I)) " "
                   DELIMITED SIZE
                   INTO WS-TOOL-RESULT
                   WITH POINTER WS-PTR
               END-STRING
           END-PERFORM

      *>   Water info
           IF WS-WATER-IDX < 99
               STRING
                   "Water at step "
                   WS-WATER-IDX ". "
                   DELIMITED SIZE
                   INTO WS-TOOL-RESULT
                   WITH POINTER WS-PTR
               END-STRING
           ELSE
               STRING
                   "No water tiles. "
                   DELIMITED SIZE
                   INTO WS-TOOL-RESULT
                   WITH POINTER WS-PTR
               END-STRING
           END-IF

      *>   Build RECOMMENDED ROUTE
           STRING
               "RECOMMENDED ROUTE: "
               DELIMITED SIZE
               INTO WS-TOOL-RESULT
               WITH POINTER WS-PTR
           END-STRING

           IF WS-WATER-IDX < 99
      *>       rocket up to water, dismount, walk rest
               STRING
                   "vehicle=rocket "
                   "moves="
                   DELIMITED SIZE
                   INTO WS-TOOL-RESULT
                   WITH POINTER WS-PTR
               END-STRING

      *>         Pre-water moves (rocket)
               PERFORM VARYING WS-I
                   FROM 1 BY 1
                   UNTIL WS-I >= WS-WATER-IDX
                   STRING
                       TRIM(WS-PF(WS-I))
                       " "
                       DELIMITED SIZE
                       INTO WS-TOOL-RESULT
                       WITH POINTER WS-PTR
                   END-STRING
               END-PERFORM

      *>         Insert dismount
               STRING
                   "dismount "
                   DELIMITED SIZE
                   INTO WS-TOOL-RESULT
                   WITH POINTER WS-PTR
               END-STRING

      *>         Post-water moves (walk)
               PERFORM VARYING WS-I
                   FROM WS-WATER-IDX BY 1
                   UNTIL WS-I > WS-PATH-LEN
                   STRING
                       TRIM(WS-PF(WS-I))
                       " "
                       DELIMITED SIZE
                       INTO WS-TOOL-RESULT
                       WITH POINTER WS-PTR
                   END-STRING
               END-PERFORM

           ELSE
      *>       No water - just use rocket
               STRING
                   "vehicle=rocket "
                   "moves="
                   DELIMITED SIZE
                   INTO WS-TOOL-RESULT
                   WITH POINTER WS-PTR
               END-STRING
               PERFORM VARYING WS-I
                   FROM 1 BY 1
                   UNTIL WS-I > WS-PATH-LEN
                   STRING
                       TRIM(WS-PF(WS-I))
                       " "
                       DELIMITED SIZE
                       INTO WS-TOOL-RESULT
                       WITH POINTER WS-PTR
                   END-STRING
               END-PERFORM
           END-IF

      *>   Compute and display budget
           IF WS-WATER-IDX < 99
               COMPUTE WS-K =
                   WS-WATER-IDX - 1
               STRING
                   "Budget: fuel="
                   WS-K
                   ".0/10 food=OK. "
                   "Call submit_route"
                   " NOW with this"
                   " vehicle and moves."
                   DELIMITED SIZE
                   INTO WS-TOOL-RESULT
                   WITH POINTER WS-PTR
               END-STRING
           ELSE
               STRING
                   "Budget: fuel="
                   WS-PATH-LEN
                   ".0/10 food=OK. "
                   "Call submit_route"
                   " NOW with this"
                   " vehicle and moves."
                   DELIMITED SIZE
                   INTO WS-TOOL-RESULT
                   WITH POINTER WS-PTR
               END-STRING
           END-IF

           COMPUTE WS-TOOL-RESULT-LEN =
               WS-PTR - 1

           DISPLAY "  Map response ("
               WS-TOOL-RESULT-LEN
               " chars): "
               WS-TOOL-RESULT(
               1:FUNCTION MIN(
               WS-TOOL-RESULT-LEN, 400))
           .

      *> ============================================================
      *> TOOL-SUBMIT-ROUTE: Parse args, verify with Hub
      *> ============================================================
       TOOL-SUBMIT-ROUTE.
      *>   Parse vehicle and moves from args
           MOVE WS-JBUF TO WS-JBUF-SAVE
           MOVE WS-JLEN TO WS-JLEN-SAVE

           MOVE WS-TOOL-ARGS TO WS-JBUF
           MOVE LENGTH(TRIM(WS-TOOL-ARGS))
               TO WS-JLEN

           MOVE "vehicle" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL)
               TO WS-SUB-VEHICLE

           MOVE WS-JBUF-SAVE TO WS-JBUF
           MOVE WS-JLEN-SAVE TO WS-JLEN

      *>   Parse moves array from raw args
           PERFORM PARSE-MOVES-ARRAY

           DISPLAY "  submit_route:"
               " vehicle="
               TRIM(WS-SUB-VEHICLE)
               " moves=" WS-SUB-MV-CNT

      *>   Build answer JSON array
           INITIALIZE WS-RESP-BUF
           MOVE 1 TO WS-PTR

           STRING
               '{"apikey":"'
               TRIM(WS-HUB-KEY)
               '","task":"savethem"'
               ',"answer":["'
               TRIM(WS-SUB-VEHICLE)
               '"'
               DELIMITED SIZE
               INTO WS-RESP-BUF
               WITH POINTER WS-PTR
           END-STRING

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-SUB-MV-CNT
               STRING
                   ',"'
                   TRIM(WS-SM(WS-I))
                   '"'
                   DELIMITED SIZE
                   INTO WS-RESP-BUF
                   WITH POINTER WS-PTR
               END-STRING
           END-PERFORM

           STRING ']}'
               DELIMITED SIZE
               INTO WS-RESP-BUF
               WITH POINTER WS-PTR
           END-STRING

           COMPUTE WS-RESP-LEN = WS-PTR - 1

           DISPLAY "  Verify payload: "
               WS-RESP-BUF(1:WS-RESP-LEN)

           MOVE "verify_req.json"
               TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           WRITE WORK-REC FROM WS-RESP-BUF
           CLOSE WORK-FILE

      *>   POST to /verify
           INITIALIZE WS-CMD
           STRING
               "curl -s"
               " -o verify_resp.json"
               " -X POST "
               TRIM(WS-VERIFY-URL)
               " -H " WS-QT
               "Content-Type: "
               "application/json" WS-QT
               " -d @verify_req.json"
               DELIMITED SIZE INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           MOVE "verify_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-RESP-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           IF WS-TOOL-RESULT-LEN = 0
               MOVE '{"error":"no resp"}'
                   TO WS-TOOL-RESULT
               MOVE 19
                   TO WS-TOOL-RESULT-LEN
           END-IF

           DISPLAY "  Verify resp: "
               WS-TOOL-RESULT(
               1:FUNCTION MIN(
               WS-TOOL-RESULT-LEN,500))

      *>   Check for flag
           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-TOOL-RESULT(
               1:WS-TOOL-RESULT-LEN)
               TALLYING WS-TALLY-CNT
               FOR ALL "FLG"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-FLAG-FOUND
               MOVE "Y" TO WS-AG-DONE
               DISPLAY "*** FLAG FOUND! ***"
           END-IF
           .

      *> ============================================================
      *> PARSE-MOVES-ARRAY: Extract moves from args JSON
      *> ============================================================
       PARSE-MOVES-ARRAY.
           MOVE 0 TO WS-SUB-MV-CNT
           INITIALIZE WS-SUB-MOVES

      *>   Find "moves" key in raw args
           MOVE 0 TO WS-MP-POS
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 3990
               OR WS-MP-POS > 0
               IF WS-TOOL-ARGS(WS-I:8)
                   = '"moves"'
                   OR WS-TOOL-ARGS(WS-I:7)
                   = '"moves"'
                   MOVE WS-I TO WS-MP-POS
               END-IF
           END-PERFORM

           IF WS-MP-POS = 0
               EXIT PARAGRAPH
           END-IF

      *>   Find opening [
           MOVE WS-MP-POS TO WS-I
           PERFORM UNTIL WS-I > 3990
               OR WS-TOOL-ARGS(WS-I:1)
                   = "["
               ADD 1 TO WS-I
           END-PERFORM
           ADD 1 TO WS-I

      *>   Scan for quoted values
           PERFORM UNTIL WS-I > 3990
               OR WS-TOOL-ARGS(WS-I:1)
                   = "]"
               OR WS-SUB-MV-CNT >= 35

               IF WS-TOOL-ARGS(WS-I:1)
                   = WS-QT
      *>             Found start of a value
                   ADD 1 TO WS-I
                   MOVE WS-I TO WS-K
      *>             Read until closing "
                   PERFORM UNTIL WS-I > 3990
                       OR WS-TOOL-ARGS(
                           WS-I:1) = WS-QT
                       ADD 1 TO WS-I
                   END-PERFORM
      *>             Extract value
                   IF WS-I > WS-K
                       ADD 1 TO WS-SUB-MV-CNT
                       COMPUTE WS-J =
                           WS-I - WS-K
                       IF WS-J > 10
                           MOVE 10 TO WS-J
                       END-IF
                       MOVE WS-TOOL-ARGS(
                           WS-K:WS-J)
                           TO WS-SM(
                           WS-SUB-MV-CNT)
                   END-IF
                   ADD 1 TO WS-I
               ELSE
                   ADD 1 TO WS-I
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> APPEND-MULTI-TOOL-EXCHANGE
      *> ============================================================
       APPEND-MULTI-TOOL-EXCHANGE.
           IF WS-CONV-PTR > 28000
               DISPLAY "  Buffer trim..."
               PERFORM TRIM-CONV-BUFFER
           END-IF

      *>   Remove trailing ]
           SUBTRACT 1 FROM WS-CONV-PTR

      *>   Append assistant message
           STRING ","
               '{"role":"assistant"'
               ',"content":null'
               ',"tool_calls":'
               WS-TC-SECTION(
               1:WS-TC-SECTION-LEN)
               '}'
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

      *>   Append each tool result
           PERFORM VARYING WS-TC-IDX
               FROM 1 BY 1
               UNTIL WS-TC-IDX > WS-TC-COUNT

               IF WS-TC-RL(WS-TC-IDX) > 6000
                   MOVE 6000
                       TO WS-TC-RL(WS-TC-IDX)
               END-IF

               MOVE WS-TC-RS(WS-TC-IDX)(
                   1:WS-TC-RL(WS-TC-IDX))
                   TO WS-ESC-IN
               PERFORM JSON-ESCAPE-STR

               STRING ","
                   '{"role":"tool"'
                   ',"tool_call_id":"'
                   TRIM(WS-TC-ID(WS-TC-IDX))
                   '","content":"'
                   WS-ESC-OUT(1:WS-ESC-OLEN)
                   '"}'
                   DELIMITED SIZE
                   INTO WS-CONV-BUF
                   WITH POINTER WS-CONV-PTR
               END-STRING
           END-PERFORM

      *>   Close array
           STRING "]"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING
           .

      *> ============================================================
      *> HANDLE-TEXT-RESP
      *> ============================================================
       HANDLE-TEXT-RESP.
           MOVE "content" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE WS-JVAL TO WS-AG-CONTENT
           DISPLAY "  Agent text: "
               TRIM(WS-AG-CONTENT)(1:500)

           MOVE 0 TO WS-TALLY-CNT
           IF LENGTH(TRIM(WS-AG-CONTENT)) > 0
               INSPECT WS-AG-CONTENT
                   TALLYING WS-TALLY-CNT
                   FOR ALL "FLG"
           END-IF
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-FLAG-FOUND
               MOVE "Y" TO WS-AG-DONE
               DISPLAY "*** FLAG FOUND ***"
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO WS-NUDGE-CT
           IF WS-NUDGE-CT > 2
               DISPLAY "  Agent stuck."
               MOVE "Y" TO WS-AG-DONE
           ELSE
               PERFORM APPEND-TEXT-AND-NUDGE
           END-IF
           .

      *> ============================================================
      *> APPEND-TEXT-AND-NUDGE
      *> ============================================================
       APPEND-TEXT-AND-NUDGE.
           SUBTRACT 1 FROM WS-CONV-PTR
           STRING ","
               '{"role":"assistant"'
               ',"content":"OK"},'
               '{"role":"user"'
               ',"content":"Call'
               ' query_map now with'
               ' Skolwin."}]'
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING
           .

      *> ============================================================
      *> TRIM-CONV-BUFFER
      *> ============================================================
       TRIM-CONV-BUFFER.
      *>   Find end of system message
           MOVE 0 TO WS-SEARCH-POS
           PERFORM VARYING WS-SEARCH-I
               FROM 1 BY 1
               UNTIL WS-SEARCH-I
                   > WS-CONV-PTR - 10
               OR WS-SEARCH-POS > 0
               IF WS-CONV-BUF(
                   WS-SEARCH-I:7)
                   = '"user",'
               OR WS-CONV-BUF(
                   WS-SEARCH-I:6)
                   = '"user"'
                   MOVE WS-SEARCH-I
                       TO WS-SEARCH-POS
               END-IF
           END-PERFORM

           IF WS-SEARCH-POS = 0
               EXIT PARAGRAPH
           END-IF

      *>   Find start of user message {
           MOVE WS-SEARCH-POS TO WS-SEARCH-I
           PERFORM UNTIL WS-SEARCH-I = 1
               OR WS-CONV-BUF(
                   WS-SEARCH-I:1) = "{"
               SUBTRACT 1
                   FROM WS-SEARCH-I
           END-PERFORM

           IF WS-SEARCH-I <= 1
               EXIT PARAGRAPH
           END-IF

      *>   Keep: system message + last 8000
           COMPUTE WS-K =
               WS-CONV-PTR - WS-SEARCH-I
           IF WS-K > 8000
               MOVE 8000 TO WS-K
           END-IF

           MOVE WS-CONV-BUF(1:WS-SEARCH-I)
               TO WS-TMP(1:WS-SEARCH-I)

           MOVE WS-CONV-BUF(
               WS-CONV-PTR - WS-K:WS-K)
               TO WS-RESP-BUF(1:WS-K)

           MOVE SPACES TO WS-CONV-BUF
           MOVE WS-CONV-BUF(1:
               WS-SEARCH-I)
               TO WS-CONV-BUF
           MOVE WS-TMP(1:WS-SEARCH-I)
               TO WS-CONV-BUF

           COMPUTE WS-CONV-PTR =
               WS-SEARCH-I + 1
           MOVE WS-RESP-BUF(1:WS-K)
               TO WS-CONV-BUF(
               WS-CONV-PTR:WS-K)
           ADD WS-K TO WS-CONV-PTR
           .

      *> ============================================================
      *> READ-JSON-FILE: Read into WS-JBUF/WS-JLEN
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

      *> ============================================================
      *> READ-RESP-FILE: Read into WS-TOOL-RESULT
      *> ============================================================
       READ-RESP-FILE.
           MOVE SPACES TO WS-TOOL-RESULT
           MOVE 0 TO WS-TOOL-RESULT-LEN
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
                           IF
                             WS-TOOL-RESULT-LEN
                             > 0
                               ADD 1 TO
                                 WS-TOOL-RESULT-LEN
                               MOVE " " TO
                                 WS-TOOL-RESULT(
                                 WS-TOOL-RESULT-LEN
                                 :1)
                           END-IF
                           IF WS-K > 16000
                               MOVE 16000
                                   TO WS-K
                           END-IF
                           IF
                             WS-TOOL-RESULT-LEN
                             + WS-K <= 16000
                               MOVE WS-LINE(
                                   1:WS-K) TO
                                 WS-TOOL-RESULT(
                                 WS-TOOL-RESULT-LEN
                                 + 1:WS-K)
                               ADD WS-K TO
                                 WS-TOOL-RESULT-LEN
                           END-IF
                       END-IF
               END-READ
           END-PERFORM

           CLOSE WORK-FILE
           MOVE "N" TO WS-EOF
           .

      *> ============================================================
      *> FIND-JSON-VAL: Find value for WS-KEY-SEARCH in JBUF
      *> ============================================================
       FIND-JSON-VAL.
           MOVE SPACES TO WS-JVAL
           MOVE SPACES TO WS-TMP
           STRING WS-QT
               TRIM(WS-KEY-SEARCH) WS-QT
               DELIMITED SIZE INTO WS-TMP
           END-STRING

           MOVE 0 TO WS-KEY-POS
           PERFORM VARYING WS-FJV-POS
               FROM WS-JPOS BY 1
               UNTIL WS-FJV-POS > WS-JLEN
               OR WS-KEY-POS > 0
               IF WS-FJV-POS
                   + LENGTH(
                   TRIM(WS-TMP))
                   - 1 <= WS-JLEN
               AND WS-JBUF(WS-FJV-POS:
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
               + LENGTH(TRIM(WS-TMP))
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
                   WS-FJV-POS:1) NOT = " "
               ADD 1 TO WS-FJV-POS
           END-PERFORM

           IF WS-JBUF(WS-FJV-POS:1)
               = WS-QT
               ADD 1 TO WS-FJV-POS
               MOVE WS-FJV-POS
                   TO WS-VAL-START
               PERFORM UNTIL
                   WS-FJV-POS > WS-JLEN
                   IF WS-JBUF(
                       WS-FJV-POS:1)
                       = X"5C"
                   AND WS-FJV-POS
                       < WS-JLEN
                       ADD 2 TO WS-FJV-POS
                   ELSE
                       IF WS-JBUF(
                           WS-FJV-POS:1)
                           = WS-QT
                           EXIT PERFORM
                       END-IF
                       ADD 1 TO WS-FJV-POS
                   END-IF
               END-PERFORM
               COMPUTE WS-VAL-END =
                   WS-FJV-POS - 1
               IF WS-VAL-END >=
                   WS-VAL-START
               AND WS-VAL-END
                   - WS-VAL-START
                   + 1 <= 8000
                   MOVE WS-JBUF(
                       WS-VAL-START:
                       WS-VAL-END
                       - WS-VAL-START + 1)
                       TO WS-JVAL
               END-IF
               ADD 1 TO WS-FJV-POS
           ELSE
               MOVE WS-FJV-POS
                   TO WS-VAL-START
               PERFORM UNTIL
                   WS-FJV-POS > WS-JLEN
                   OR WS-JBUF(
                       WS-FJV-POS:1) = ","
                   OR WS-JBUF(
                       WS-FJV-POS:1) = "}"
                   OR WS-JBUF(
                       WS-FJV-POS:1) = "]"
                   OR WS-JBUF(
                       WS-FJV-POS:1) = " "
                   ADD 1 TO WS-FJV-POS
               END-PERFORM
               COMPUTE WS-VAL-END =
                   WS-FJV-POS - 1
               IF WS-VAL-END >=
                   WS-VAL-START
                   MOVE WS-JBUF(
                       WS-VAL-START:
                       WS-VAL-END
                       - WS-VAL-START + 1)
                       TO WS-JVAL
               END-IF
           END-IF
           MOVE WS-FJV-POS TO WS-JPOS
           .

      *> ============================================================
      *> JSON-ESCAPE-STR: Escape WS-ESC-IN -> WS-ESC-OUT
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
               UNTIL WS-ESC-I > WS-ESC-ILEN
               OR WS-ESC-OLEN > 31000
               EVALUATE TRUE
               WHEN WS-ESC-IN(WS-ESC-I:1)
                   = WS-QT
                   ADD 1 TO WS-ESC-OLEN
                   MOVE X"5C" TO
                     WS-ESC-OUT(
                     WS-ESC-OLEN:1)
                   ADD 1 TO WS-ESC-OLEN
                   MOVE WS-QT TO
                     WS-ESC-OUT(
                     WS-ESC-OLEN:1)
               WHEN WS-ESC-IN(WS-ESC-I:1)
                   = X"5C"
                   ADD 1 TO WS-ESC-OLEN
                   MOVE X"5C" TO
                     WS-ESC-OUT(
                     WS-ESC-OLEN:1)
                   ADD 1 TO WS-ESC-OLEN
                   MOVE X"5C" TO
                     WS-ESC-OUT(
                     WS-ESC-OLEN:1)
               WHEN WS-ESC-IN(WS-ESC-I:1)
                   = X"0A"
                   ADD 1 TO WS-ESC-OLEN
                   MOVE X"5C" TO
                     WS-ESC-OUT(
                     WS-ESC-OLEN:1)
                   ADD 1 TO WS-ESC-OLEN
                   MOVE "n" TO
                     WS-ESC-OUT(
                     WS-ESC-OLEN:1)
               WHEN WS-ESC-IN(WS-ESC-I:1)
                   = X"0D"
                   CONTINUE
               WHEN WS-ESC-IN(WS-ESC-I:1)
                   = X"09"
                   ADD 1 TO WS-ESC-OLEN
                   MOVE " " TO
                     WS-ESC-OUT(
                     WS-ESC-OLEN:1)
               WHEN OTHER
                   ADD 1 TO WS-ESC-OLEN
                   MOVE WS-ESC-IN(
                     WS-ESC-I:1) TO
                     WS-ESC-OUT(
                     WS-ESC-OLEN:1)
               END-EVALUATE
           END-PERFORM
           .

      *> ============================================================
      *> JSON-UNESCAPE-STR: Unescape WS-ESC-IN -> WS-ESC-OUT
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
               IF WS-ESC-IN(WS-ESC-I:1)
                   = X"5C"
               AND WS-ESC-I < WS-ESC-ILEN
                   ADD 1 TO WS-ESC-I
                   EVALUATE TRUE
                   WHEN WS-ESC-IN(
                       WS-ESC-I:1) = "n"
                       ADD 1 TO WS-ESC-OLEN
                       MOVE X"0A" TO
                         WS-ESC-OUT(
                         WS-ESC-OLEN:1)
                   WHEN WS-ESC-IN(
                       WS-ESC-I:1) = "r"
                       CONTINUE
                   WHEN WS-ESC-IN(
                       WS-ESC-I:1) = "t"
                       ADD 1 TO WS-ESC-OLEN
                       MOVE X"09" TO
                         WS-ESC-OUT(
                         WS-ESC-OLEN:1)
                   WHEN OTHER
                       ADD 1 TO WS-ESC-OLEN
                       MOVE WS-ESC-IN(
                           WS-ESC-I:1) TO
                         WS-ESC-OUT(
                         WS-ESC-OLEN:1)
                   END-EVALUATE
               ELSE
                   ADD 1 TO WS-ESC-OLEN
                   MOVE WS-ESC-IN(
                       WS-ESC-I:1) TO
                     WS-ESC-OUT(
                     WS-ESC-OLEN:1)
               END-IF
               ADD 1 TO WS-ESC-I
           END-PERFORM
           .
