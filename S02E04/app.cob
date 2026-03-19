       IDENTIFICATION DIVISION.
       PROGRAM-ID. S02E04-MAILBOX.
      *> ============================================================
      *> S02E04 - Mailbox Agent: search emails for date, password,
      *>          confirmation_code via function calling agent loop.
      *> Dynamic per-action tools generated from help endpoint.
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
       01  WS-ZMAIL-URL            PIC X(200)
           VALUE
           "REDACTED_HUB_URL/api/zmail".

      *> -- JSON newline: backslash + n --
       01  WS-NL                   PIC X(2).

      *> -- STRING pointer --
       01  WS-PTR                  PIC 9(5).

      *> -- Large JSON buffers --
       01  WS-JBUF                 PIC X(32000).
       01  WS-JLEN                 PIC 9(5).
       01  WS-JPOS                 PIC 9(5).
       01  WS-JVAL                 PIC X(8000).

      *> -- JSON parsing temps --
       01  WS-KEY-SEARCH           PIC X(50).
       01  WS-KEY-POS              PIC 9(5).
       01  WS-VAL-START            PIC 9(5).
       01  WS-VAL-END              PIC 9(5).
       01  WS-FJV-POS              PIC 9(5).
       01  WS-TMP                  PIC X(500).

      *> -- JSON escape input/output --
       01  WS-ESC-IN               PIC X(16000).
       01  WS-ESC-OUT              PIC X(32000).
       01  WS-ESC-ILEN             PIC 9(5).
       01  WS-ESC-OLEN             PIC 9(5).
       01  WS-ESC-I                PIC 9(5).

      *> -- Loop/misc --
       01  WS-K                    PIC 9(5).
       01  WS-EOF                  PIC X VALUE "N".
       01  WS-LINE                 PIC X(16000).
       01  WS-TALLY-CNT            PIC 9(4).

      *> -- System command buffer --
       01  WS-CMD                  PIC X(4000).

      *> -- Agent conversation buffer (very large) --
       01  WS-CONV-BUF             PIC X(64000).
       01  WS-CONV-PTR             PIC 9(5).

      *> -- Agent loop --
       01  WS-AG-STEP              PIC 9(2) VALUE 0.
       01  WS-AG-DONE              PIC X VALUE "N".
       01  WS-AG-MAX-STEPS         PIC 9(2) VALUE 25.

      *> -- Tool call parsing --
       01  WS-TOOL-NAME            PIC X(50).
       01  WS-TOOL-CALL-ID         PIC X(100).
       01  WS-TOOL-ARGS            PIC X(2000).
       01  WS-TOOL-RESULT          PIC X(16000).
       01  WS-TOOL-RESULT-LEN      PIC 9(5).

      *> -- Multiple tool calls --
       01  WS-TC-COUNT             PIC 9(2).
       01  WS-TC-IDX               PIC 9(2).
       01  WS-TC-IDS.
           05 WS-TC-ID OCCURS 10 TIMES
                                   PIC X(100).
       01  WS-TC-NAMES.
           05 WS-TC-NM OCCURS 10 TIMES
                                   PIC X(50).
       01  WS-TC-ARGSS.
           05 WS-TC-AR OCCURS 10 TIMES
                                   PIC X(2000).
       01  WS-TC-RESULTS.
           05 WS-TC-RS OCCURS 10 TIMES
                                   PIC X(8000).
       01  WS-TC-RS-LENS.
           05 WS-TC-RL OCCURS 10 TIMES
                                   PIC 9(5).

      *> -- Parsed tool arguments --
       01  WS-TA-PAYLOAD           PIC X(2000).
       01  WS-TA-DATE              PIC X(20).
       01  WS-TA-PASSWORD          PIC X(200).
       01  WS-TA-CONFCODE          PIC X(100).

      *> -- LLM request file --
       01  WS-REQ-JSON             PIC X(64000).

      *> -- Response buffer for API calls --
       01  WS-RESP-BUF             PIC X(16000).
       01  WS-RESP-LEN             PIC 9(5).

      *> -- Flag --
       01  WS-FLAG-FOUND           PIC X VALUE "N".

      *> -- JBUF save/restore --
       01  WS-JBUF-SAVE            PIC X(32000).
       01  WS-JLEN-SAVE            PIC 9(5).

      *> -- Tool call section raw --
       01  WS-TC-SECTION           PIC X(8000).
       01  WS-TC-SECTION-LEN       PIC 9(5).

      *> -- Brace tracking --
       01  WS-BRACE-DEPTH          PIC 9(3).
       01  WS-BRACE-START          PIC 9(5).

      *> -- Search helpers --
       01  WS-SEARCH-POS           PIC 9(5).
       01  WS-SEARCH-I             PIC 9(5).

      *> -- Retry --
       01  WS-RETRY-CT             PIC 9(2) VALUE 0.
       01  WS-SLEEP-SECS           PIC 9(3).

      *> -- Nudge counter --
       01  WS-NUDGE-CT             PIC 9(1) VALUE 0.

      *> -- Content extract --
       01  WS-AG-CONTENT           PIC X(4000).

      *> -- Help response (escaped for prompt) --
       01  WS-HELP-ESC             PIC X(4000).
       01  WS-HELP-ESC-LEN         PIC 9(5).

      *> -- Dynamic tool definitions (built from help) --
       01  WS-TOOLS-JSON           PIC X(4000).

      *> -- Dynamic action table --
       01  WS-ACT-COUNT            PIC 9(2) VALUE 0.
       01  WS-ACT-NAMES.
           05 WS-ACT-NM OCCURS 10 TIMES
                                   PIC X(30).

      *> -- Temp for building zmail JSON body --
       01  WS-ZMAIL-BODY           PIC X(2000).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S02E04 MAILBOX - Agent ==="

           ACCEPT WS-HUB-KEY
               FROM ENVIRONMENT "HUB_API_KEY"
           ACCEPT WS-OPENAI-KEY
               FROM ENVIRONMENT "OPENAI_API_KEY"
           ACCEPT WS-HUB-URL
               FROM ENVIRONMENT "HUB_API_URL"
           ACCEPT WS-OPENAI-URL
               FROM ENVIRONMENT "OPENAI_API_URL"

           IF WS-HUB-KEY = SPACES
               DISPLAY "ERROR: HUB_API_KEY!"
               STOP RUN
           END-IF
           IF WS-OPENAI-KEY = SPACES
               DISPLAY "ERROR: OPENAI_API_KEY!"
               STOP RUN
           END-IF
           IF WS-HUB-URL = SPACES
               DISPLAY "ERROR: HUB_API_URL!"
               STOP RUN
           END-IF
           IF WS-OPENAI-URL = SPACES
               DISPLAY "ERROR: OPENAI_API_URL!"
               STOP RUN
           END-IF

           INITIALIZE WS-VERIFY-URL
           STRING TRIM(WS-HUB-URL) "/verify"
               DELIMITED SIZE INTO WS-VERIFY-URL
           END-STRING

      *>   Init JSON newline (backslash + n)
           MOVE X"5C" TO WS-NL(1:1)
           MOVE "n"    TO WS-NL(2:1)

      *>   Step 1: Call help to discover actions
           PERFORM FETCH-HELP

      *>   Build tool definitions from help
           PERFORM BUILD-DYNAMIC-TOOLS

      *>   Run the agent
           PERFORM RUN-AGENT

           DISPLAY " "
           DISPLAY "=== DONE ==="
           STOP RUN.

      *> ============================================================
      *> FETCH-HELP: Call zmail help endpoint
      *> ============================================================
       FETCH-HELP.
           DISPLAY "Fetching mailbox API help..."

      *>   Build help request
           INITIALIZE WS-RESP-BUF
           MOVE 1 TO WS-PTR
           STRING
               '{"apikey":"'
               TRIM(WS-HUB-KEY)
               '","action":"help"}'
               DELIMITED SIZE
               INTO WS-RESP-BUF
               WITH POINTER WS-PTR
           END-STRING

           MOVE "help_req.json" TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           WRITE WORK-REC FROM WS-RESP-BUF
           CLOSE WORK-FILE

           INITIALIZE WS-CMD
           STRING
               "curl -s -o help_resp.json"
               " -X POST "
               TRIM(WS-ZMAIL-URL)
               " -H " WS-QT
               "Content-Type: "
               "application/json" WS-QT
               " -d @help_req.json"
               DELIMITED SIZE INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Read help response
           MOVE "help_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-RESP-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           DISPLAY "Help response: "
               TRIM(WS-TOOL-RESULT)(1:500)

      *>   Escape it for embedding in prompt
           IF WS-TOOL-RESULT-LEN > 4000
               MOVE 4000 TO WS-TOOL-RESULT-LEN
           END-IF
           MOVE WS-TOOL-RESULT(
               1:WS-TOOL-RESULT-LEN)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR
           MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
               TO WS-HELP-ESC
           MOVE WS-ESC-OLEN TO WS-HELP-ESC-LEN
           .

      *> ============================================================
      *> BUILD-DYNAMIC-TOOLS: Generate per-action tools
      *> from help response + static submit_answer.
      *> Actions: getInbox, getThread, getMessages,
      *>          search (skip help, reset)
      *> ============================================================
       BUILD-DYNAMIC-TOOLS.
           INITIALIZE WS-TOOLS-JSON
           MOVE 1 TO WS-PTR
           MOVE 0 TO WS-ACT-COUNT

      *> -- Tool: getInbox(page, perPage) --
           ADD 1 TO WS-ACT-COUNT
           MOVE "getInbox"
               TO WS-ACT-NM(WS-ACT-COUNT)

           STRING
               '{"type":"function","function":{'
               '"name":"getInbox",'
               '"description":"Return list of '
               'threads in the mailbox.",'
               '"parameters":{"type":"object",'
               '"properties":{'
               '"page":{"type":"integer",'
               '"description":"Page number"},'
               '"perPage":{"type":"integer",'
               '"description":"Results per page'
               ' 5-20"}},'
               '"required":["page","perPage"]'
               '}}},'
               DELIMITED SIZE
               INTO WS-TOOLS-JSON
               WITH POINTER WS-PTR
           END-STRING

      *> -- Tool: getThread(threadID) --
           ADD 1 TO WS-ACT-COUNT
           MOVE "getThread"
               TO WS-ACT-NM(WS-ACT-COUNT)

           STRING
               '{"type":"function","function":{'
               '"name":"getThread",'
               '"description":"Return rowID and'
               ' messageID list for a thread.",'
               '"parameters":{"type":"object",'
               '"properties":{'
               '"threadID":{"type":"integer",'
               '"description":"Thread ID"}},'
               '"required":["threadID"]'
               '}}},'
               DELIMITED SIZE
               INTO WS-TOOLS-JSON
               WITH POINTER WS-PTR
           END-STRING

      *> -- Tool: getMessages(ids) --
           ADD 1 TO WS-ACT-COUNT
           MOVE "getMessages"
               TO WS-ACT-NM(WS-ACT-COUNT)

           STRING
               '{"type":"function","function":{'
               '"name":"getMessages",'
               '"description":"Return full '
               'message bodies by rowID. '
               'Pass ALL IDs at once in a '
               'single call to be efficient.",'
               '"parameters":{"type":"object",'
               '"properties":{'
               '"ids":{"type":"array",'
               '"items":{"type":"string"},'
               '"description":"Array of rowID'
               ' strings e.g. [92,93,5]"}},'
               '"required":["ids"]'
               '}}},'
               DELIMITED SIZE
               INTO WS-TOOLS-JSON
               WITH POINTER WS-PTR
           END-STRING

      *> -- Tool: search(query, page, perPage) --
           ADD 1 TO WS-ACT-COUNT
           MOVE "search"
               TO WS-ACT-NM(WS-ACT-COUNT)

           STRING
               '{"type":"function","function":{'
               '"name":"search",'
               '"description":"Search messages '
               'with full-text query and '
               'Gmail-like operators: from: '
               'to: subject: OR AND -exclude",'
               '"parameters":{"type":"object",'
               '"properties":{'
               '"query":{"type":"string",'
               '"description":"Search query"},'
               '"page":{"type":"integer",'
               '"description":"Page number"},'
               '"perPage":{"type":"integer",'
               '"description":"Results per page'
               ' 5-20"}},'
               '"required":["query","page",'
               '"perPage"]'
               '}}},'
               DELIMITED SIZE
               INTO WS-TOOLS-JSON
               WITH POINTER WS-PTR
           END-STRING

      *> -- Tool: submit_answer (static) --
           STRING
               '{"type":"function","function":{'
               '"name":"submit_answer",'
               '"description":"Submit all 3 '
               'found values to the hub.",'
               '"parameters":{"type":"object",'
               '"properties":{'
               '"date":{"type":"string",'
               '"description":"YYYY-MM-DD"},'
               '"password":{"type":"string",'
               '"description":"Password"},'
               '"confirmation_code":'
               '{"type":"string",'
               '"description":"SEC- + 32 hex"}'
               '},"required":["date",'
               '"password",'
               '"confirmation_code"]'
               '}}}'
               DELIMITED SIZE
               INTO WS-TOOLS-JSON
               WITH POINTER WS-PTR
           END-STRING

           DISPLAY "Tools: " WS-ACT-COUNT
               " mailbox + submit_answer"
           DISPLAY "Actions: "
               TRIM(WS-ACT-NM(1)) " "
               TRIM(WS-ACT-NM(2)) " "
               TRIM(WS-ACT-NM(3)) " "
               TRIM(WS-ACT-NM(4))
           .

      *> ============================================================
      *> RUN-AGENT: Main agent loop
      *> ============================================================
       RUN-AGENT.
           DISPLAY " "
           DISPLAY "--- Agent starting ---"

           MOVE SPACES TO WS-CONV-BUF
           MOVE 1 TO WS-CONV-PTR

      *>   System prompt with help info
           STRING
               '[{"role":"system","content":"'
               'Email agent. Find 3 values: '
               'date (YYYY-MM-DD of attack), '
               'password, confirmation_code '
               '(SEC-+32hex).'
               WS-NL WS-NL
               'STEP 1: Call these 3 searches '
               'IN PARALLEL right now:'
               WS-NL
               '- search from:proton.me '
               'perPage=20'
               WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               '- search SEC- perPage=20'
               WS-NL
               '- search has'
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

      *>   Polish ł = UTF-8 C5 82
           MOVE X"C582" TO WS-TMP(1:2)
           STRING
               WS-TMP(1:2)
               'o perPage=20'
               WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               'STEP 2: For each search result '
               'with threads, call getThread. '
               'Do all getThread calls in '
               'parallel.'
               WS-NL WS-NL
               'STEP 3: For each thread, call '
               'getMessages with ALL rowIDs '
               'as an array. Read ALL messages '
               '- confirmation codes may be '
               'corrected in later messages.'
               WS-NL WS-NL
               'STEP 4: submit_answer.'
               WS-NL WS-NL
               'Rules: perPage=20 always. '
               'Batch getMessages - pass all '
               'IDs in one array. '
               'Use parallel tool calls.'
               '"},'
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

      *>   User message
           STRING
               '{"role":"user","content":"'
               'Find date, password, and '
               'confirmation_code by searching '
               'the mailbox. Start now."}]'
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
               DISPLAY "=========================="
                   "=========================="
               DISPLAY "Step " WS-AG-STEP
                   " / " WS-AG-MAX-STEPS
                   " (buf=" WS-CONV-PTR ")"

      *>       Build and send LLM request
               PERFORM SEND-AGENT-REQUEST

      *>       Read response
               MOVE "agent_resp.json"
                   TO WS-WORK-PATH
               PERFORM READ-JSON-FILE
               MOVE "work.tmp" TO WS-WORK-PATH

               IF WS-JLEN = 0
                   DISPLAY "  ERROR: Empty!"
                   MOVE "Y" TO WS-AG-DONE
                   EXIT PERFORM
               END-IF

      *>       Check API error (no choices)
               MOVE 0 TO WS-TALLY-CNT
               INSPECT WS-JBUF(1:WS-JLEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL '"choices"'
               IF WS-TALLY-CNT = 0
                   DISPLAY "  API ERROR: "
                       WS-JBUF(1:500)
                   MOVE "Y" TO WS-AG-DONE
                   EXIT PERFORM
               END-IF

      *>       Check for finish_reason tool_calls
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
      *> HANDLE-TEXT-RESP
      *> ============================================================
       HANDLE-TEXT-RESP.
           MOVE "content" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE WS-JVAL TO WS-AG-CONTENT
           DISPLAY "  Agent: "
               TRIM(WS-AG-CONTENT)(1:1000)

      *>   Check for flag
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
      *> SEND-AGENT-REQUEST
      *> ============================================================
       SEND-AGENT-REQUEST.
           MOVE SPACES TO WS-REQ-JSON
           MOVE 1 TO WS-PTR

           STRING
               '{"model":"gpt-4.1-mini",'
               '"messages":'
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
               '],"tool_choice":"auto",'
               '"temperature":0}'
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Write request
           MOVE "agent_req.json"
               TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           WRITE WORK-REC FROM WS-REQ-JSON
           CLOSE WORK-FILE

      *>   Call OpenAI with retry
           MOVE 0 TO WS-RETRY-CT
           PERFORM UNTIL WS-RETRY-CT >= 5
               DISPLAY "  Calling OpenAI..."
               INITIALIZE WS-CMD
               STRING
                   "curl -s -o agent_resp.json"
                   " -X POST "
                   TRIM(WS-OPENAI-URL)
                   " -H " WS-QT
                   "Content-Type: "
                   "application/json" WS-QT
                   " -H " WS-QT
                   "Authorization: Bearer "
                   TRIM(WS-OPENAI-KEY) WS-QT
                   " -d @agent_req.json"
                   DELIMITED SIZE
                   INTO WS-CMD
               END-STRING
               CALL "SYSTEM" USING WS-CMD

               MOVE "agent_resp.json"
                   TO WS-WORK-PATH
               PERFORM READ-JSON-FILE
               MOVE "work.tmp" TO WS-WORK-PATH

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
                       WS-RETRY-CT "..."
                   MOVE 3 TO WS-SLEEP-SECS
                   CALL "C$SLEEP"
                       USING WS-SLEEP-SECS
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> PARSE-ALL-TOOL-CALLS
      *> ============================================================
       PARSE-ALL-TOOL-CALLS.
           MOVE 0 TO WS-TC-COUNT

      *>   Find "tool_calls" position
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

      *>   Find the [ that starts the array
           MOVE WS-SEARCH-POS TO WS-SEARCH-I
           PERFORM UNTIL WS-SEARCH-I > WS-JLEN
               OR WS-JBUF(WS-SEARCH-I:1) = "["
               ADD 1 TO WS-SEARCH-I
           END-PERFORM

      *>   Extract entire tool_calls [...]
           MOVE WS-SEARCH-I TO WS-BRACE-START
           MOVE 1 TO WS-BRACE-DEPTH
           ADD 1 TO WS-SEARCH-I
           PERFORM UNTIL WS-SEARCH-I > WS-JLEN
               OR WS-BRACE-DEPTH = 0
               IF WS-JBUF(WS-SEARCH-I:1) = "["
                   ADD 1 TO WS-BRACE-DEPTH
               END-IF
               IF WS-JBUF(WS-SEARCH-I:1) = "]"
                   SUBTRACT 1
                       FROM WS-BRACE-DEPTH
               END-IF
               ADD 1 TO WS-SEARCH-I
           END-PERFORM

           COMPUTE WS-TC-SECTION-LEN =
               WS-SEARCH-I - WS-BRACE-START
           IF WS-TC-SECTION-LEN > 8000
               MOVE 8000 TO WS-TC-SECTION-LEN
           END-IF
           MOVE WS-JBUF(WS-BRACE-START:
               WS-TC-SECTION-LEN)
               TO WS-TC-SECTION

      *>   Iterate tool call objects
           MOVE 1 TO WS-SEARCH-I
           PERFORM UNTIL WS-SEARCH-I
               >= WS-TC-SECTION-LEN
               OR WS-TC-COUNT >= 10

      *>       Find next "id" in section
               MOVE 0 TO WS-SEARCH-POS
               PERFORM UNTIL WS-SEARCH-I
                   >= WS-TC-SECTION-LEN - 3
                   OR WS-SEARCH-POS > 0
                   IF WS-TC-SECTION(
                       WS-SEARCH-I:4) = '"id"'
                       MOVE WS-SEARCH-I
                           TO WS-SEARCH-POS
                   END-IF
                   ADD 1 TO WS-SEARCH-I
               END-PERFORM

               IF WS-SEARCH-POS = 0
                   EXIT PERFORM
               END-IF

               ADD 1 TO WS-TC-COUNT

      *>       Parse using JBUF
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

               MOVE "name" TO WS-KEY-SEARCH
               PERFORM FIND-JSON-VAL
               MOVE TRIM(WS-JVAL)
                   TO WS-TC-NM(WS-TC-COUNT)

               MOVE "arguments"
                   TO WS-KEY-SEARCH
               PERFORM FIND-JSON-VAL

      *>       Unescape arguments
               MOVE WS-JVAL TO WS-ESC-IN
               PERFORM JSON-UNESCAPE-STR
               MOVE TRIM(WS-ESC-OUT)
                   TO WS-TC-AR(WS-TC-COUNT)

               DISPLAY "  TC" WS-TC-COUNT ": "
                   TRIM(WS-TC-NM(WS-TC-COUNT))
                   "("
                   TRIM(WS-TC-AR(
                       WS-TC-COUNT))(1:200)
                   ")"

               MOVE WS-JBUF-SAVE TO WS-JBUF
               MOVE WS-JLEN-SAVE TO WS-JLEN
           END-PERFORM

           DISPLAY "  Tool calls: " WS-TC-COUNT
           .

      *> ============================================================
      *> EXECUTE-ALL-TOOL-CALLS
      *> ============================================================
       EXECUTE-ALL-TOOL-CALLS.
           PERFORM VARYING WS-TC-IDX FROM 1 BY 1
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
               MOVE WS-TOOL-RESULT(
                   1:WS-TC-RL(WS-TC-IDX))
                   TO WS-TC-RS(WS-TC-IDX)
           END-PERFORM

           IF WS-FLAG-FOUND = "Y"
               EXIT PARAGRAPH
           END-IF

           PERFORM APPEND-MULTI-TOOL-EXCHANGE
           .

      *> ============================================================
      *> DISPATCH-TOOL: Match tool name to mailbox
      *> action or submit_answer
      *> ============================================================
       DISPATCH-TOOL.
           MOVE SPACES TO WS-TOOL-RESULT
           MOVE 0 TO WS-TOOL-RESULT-LEN

           EVALUATE TRIM(WS-TOOL-NAME)
           WHEN "getInbox"
               PERFORM TOOL-MAILBOX-ACTION
           WHEN "getThread"
               PERFORM TOOL-MAILBOX-ACTION
           WHEN "getMessages"
               PERFORM TOOL-MAILBOX-ACTION
           WHEN "search"
               PERFORM TOOL-MAILBOX-ACTION
           WHEN "submit_answer"
               PERFORM TOOL-SUBMIT-ANSWER
           WHEN OTHER
               MOVE '{"error":"Unknown tool"}'
                   TO WS-TOOL-RESULT
               MOVE 23 TO WS-TOOL-RESULT-LEN
           END-EVALUATE
           .

      *> ============================================================
      *> TOOL-MAILBOX-ACTION: Build zmail payload from
      *> tool_name (=action) + args JSON, add apikey.
      *> Args is like {"query":"x","page":1,"perPage":20}
      *> We build: {"apikey":"K","action":"N",...args}
      *> ============================================================
       TOOL-MAILBOX-ACTION.
           INITIALIZE WS-ZMAIL-BODY
           MOVE 1 TO WS-PTR

      *>   Start with apikey and action
           STRING
               '{"apikey":"'
               TRIM(WS-HUB-KEY)
               '","action":"'
               TRIM(WS-TOOL-NAME)
               '"'
               DELIMITED SIZE
               INTO WS-ZMAIL-BODY
               WITH POINTER WS-PTR
           END-STRING

      *>   Merge remaining args from tool args JSON
      *>   The args look like {"key":"val",...}
      *>   We need to extract content after first {
           MOVE LENGTH(TRIM(WS-TOOL-ARGS))
               TO WS-K
           IF WS-K > 2
      *>       Find first { and strip it
               IF WS-TOOL-ARGS(1:1) = "{"
      *>           Get content between { and }
      *>           i.e. skip first { and last }
                   COMPUTE WS-RESP-LEN =
                       WS-K - 2
                   IF WS-RESP-LEN > 0
                       STRING
                           ","
                           WS-TOOL-ARGS(
                               2:WS-RESP-LEN)
                           DELIMITED SIZE
                           INTO WS-ZMAIL-BODY
                           WITH POINTER WS-PTR
                       END-STRING
                   END-IF
               END-IF
           END-IF

      *>   Close the JSON object
           STRING
               "}"
               DELIMITED SIZE
               INTO WS-ZMAIL-BODY
               WITH POINTER WS-PTR
           END-STRING

           COMPUTE WS-RESP-LEN = WS-PTR - 1

           IF WS-RESP-LEN > 500
               DISPLAY "  -> POST zmail: "
                   WS-ZMAIL-BODY(1:500)
           ELSE
               DISPLAY "  -> POST zmail: "
                   WS-ZMAIL-BODY(
                       1:WS-RESP-LEN)
           END-IF

      *>   Write and POST
           MOVE WS-ZMAIL-BODY(1:WS-RESP-LEN)
               TO WS-RESP-BUF
           MOVE "zmail_req.json" TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           WRITE WORK-REC FROM WS-RESP-BUF
           CLOSE WORK-FILE

           MOVE 0 TO WS-RETRY-CT
           PERFORM UNTIL WS-RETRY-CT >= 10
               INITIALIZE WS-CMD
               STRING
                   "curl -s -o zmail_resp.json"
                   " -X POST "
                   TRIM(WS-ZMAIL-URL)
                   " -H " WS-QT
                   "Content-Type: "
                   "application/json" WS-QT
                   " -d @zmail_req.json"
                   DELIMITED SIZE
                   INTO WS-CMD
               END-STRING
               CALL "SYSTEM" USING WS-CMD

               MOVE "zmail_resp.json"
                   TO WS-WORK-PATH
               PERFORM READ-RESP-FILE
               MOVE "work.tmp" TO WS-WORK-PATH

               IF WS-TOOL-RESULT-LEN > 0
      *>           Check for rate limit
                   MOVE 0 TO WS-TALLY-CNT
                   INSPECT WS-TOOL-RESULT(
                       1:WS-TOOL-RESULT-LEN)
                       TALLYING WS-TALLY-CNT
                       FOR ALL "-9999"
                   IF WS-TALLY-CNT > 0
                       ADD 1 TO WS-RETRY-CT
                       DISPLAY "    Rate limit,"
                           " wait 5s ("
                           WS-RETRY-CT ")..."
                       MOVE 5 TO WS-SLEEP-SECS
                       CALL "C$SLEEP"
                           USING WS-SLEEP-SECS
                   ELSE
                       EXIT PERFORM
                   END-IF
               ELSE
                   ADD 1 TO WS-RETRY-CT
                   DISPLAY "    No resp, retry "
                       WS-RETRY-CT "..."
                   MOVE 3 TO WS-SLEEP-SECS
                   CALL "C$SLEEP"
                       USING WS-SLEEP-SECS
               END-IF
           END-PERFORM

           IF WS-TOOL-RESULT-LEN = 0
               MOVE '{"error":"no response"}'
                   TO WS-TOOL-RESULT
               MOVE 22 TO WS-TOOL-RESULT-LEN
           END-IF

           DISPLAY "  <- "
               TRIM(WS-TOOL-RESULT)(1:500)

      *>   Check for flag
           MOVE 0 TO WS-TALLY-CNT
           IF WS-TOOL-RESULT-LEN > 0
               INSPECT WS-TOOL-RESULT(
                   1:WS-TOOL-RESULT-LEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL "FLG"
           END-IF
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-FLAG-FOUND
               MOVE "Y" TO WS-AG-DONE
               DISPLAY "*** FLAG FOUND! ***"
           END-IF
           .

      *> ============================================================
      *> TOOL-SUBMIT-ANSWER
      *> ============================================================
       TOOL-SUBMIT-ANSWER.
           MOVE SPACES TO WS-TA-DATE
           MOVE SPACES TO WS-TA-PASSWORD
           MOVE SPACES TO WS-TA-CONFCODE

           MOVE WS-JBUF TO WS-JBUF-SAVE
           MOVE WS-JLEN TO WS-JLEN-SAVE

           MOVE WS-TOOL-ARGS TO WS-JBUF
           MOVE LENGTH(TRIM(WS-TOOL-ARGS))
               TO WS-JLEN

           MOVE "date" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL) TO WS-TA-DATE

           MOVE "password" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL)
               TO WS-TA-PASSWORD

           MOVE "confirmation_code"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL) TO WS-TA-CONFCODE

           MOVE WS-JBUF-SAVE TO WS-JBUF
           MOVE WS-JLEN-SAVE TO WS-JLEN

           DISPLAY "  Submit: date="
               TRIM(WS-TA-DATE)
               " pass="
               TRIM(WS-TA-PASSWORD)
               " code="
               TRIM(WS-TA-CONFCODE)

      *>   Escape password for JSON
           MOVE TRIM(WS-TA-PASSWORD)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

           MOVE SPACES TO WS-RESP-BUF
           MOVE 1 TO WS-PTR
           STRING
               '{"apikey":"'
               TRIM(WS-HUB-KEY)
               '","task":"mailbox",'
               '"answer":{'
               '"password":"'
               WS-ESC-OUT(1:WS-ESC-OLEN)
               '","date":"'
               TRIM(WS-TA-DATE)
               '","confirmation_code":"'
               TRIM(WS-TA-CONFCODE)
               '"}}'
               DELIMITED SIZE
               INTO WS-RESP-BUF
               WITH POINTER WS-PTR
           END-STRING

           COMPUTE WS-RESP-LEN = WS-PTR - 1

           DISPLAY "  Verify: "
               WS-RESP-BUF(1:WS-RESP-LEN)

           MOVE "verify_req.json"
               TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           WRITE WORK-REC FROM WS-RESP-BUF
           CLOSE WORK-FILE

           INITIALIZE WS-CMD
           STRING
               "curl -s -o verify_resp.json"
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
               MOVE '{"error":"no verify"}'
                   TO WS-TOOL-RESULT
               MOVE 21 TO WS-TOOL-RESULT-LEN
           END-IF

           DISPLAY "  Result: "
               TRIM(WS-TOOL-RESULT)(1:500)

           MOVE 0 TO WS-TALLY-CNT
           IF WS-TOOL-RESULT-LEN > 0
               INSPECT WS-TOOL-RESULT(
                   1:WS-TOOL-RESULT-LEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL "FLG"
           END-IF
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-FLAG-FOUND
               MOVE "Y" TO WS-AG-DONE
               DISPLAY "*** FLAG FOUND! ***"
           END-IF
           .

      *> ============================================================
      *> APPEND-MULTI-TOOL-EXCHANGE
      *> ============================================================
       APPEND-MULTI-TOOL-EXCHANGE.
      *>   Check buffer space - if low, truncate
      *>   old messages keeping system + last few
           IF WS-CONV-PTR > 50000
               DISPLAY "  Buffer trimming..."
               PERFORM TRIM-CONV-BUFFER
           END-IF

      *>   Remove trailing ]
           SUBTRACT 1 FROM WS-CONV-PTR

      *>   Append assistant message
           STRING ","
               '{"role":"assistant",'
               '"content":null,'
               '"tool_calls":'
               WS-TC-SECTION(
                   1:WS-TC-SECTION-LEN)
               '}'
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

      *>   Append each tool result
           PERFORM VARYING WS-TC-IDX FROM 1 BY 1
               UNTIL WS-TC-IDX > WS-TC-COUNT

      *>       Truncate very large results
               IF WS-TC-RL(WS-TC-IDX) > 6000
                   MOVE 6000
                       TO WS-TC-RL(WS-TC-IDX)
               END-IF

      *>       Escape tool result
               MOVE WS-TC-RS(WS-TC-IDX)(
                   1:WS-TC-RL(WS-TC-IDX))
                   TO WS-ESC-IN
               PERFORM JSON-ESCAPE-STR

               STRING ","
                   '{"role":"tool",'
                   '"tool_call_id":"'
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
      *> TRIM-CONV-BUFFER: Keep system msg + last
      *> portion of conversation, discard middle
      *> ============================================================
       TRIM-CONV-BUFFER.
      *>   Find end of system message (first },)
      *>   We look for },"role":"user" pattern
      *>   to find where system msg ends
           MOVE 0 TO WS-SEARCH-POS
           PERFORM VARYING WS-SEARCH-I
               FROM 1 BY 1
               UNTIL WS-SEARCH-I
                   > WS-CONV-PTR - 10
               OR WS-SEARCH-POS > 0
               IF WS-CONV-BUF(WS-SEARCH-I:7)
                   = '"user",'
               OR WS-CONV-BUF(WS-SEARCH-I:6)
                   = '"user"'
                   MOVE WS-SEARCH-I
                       TO WS-SEARCH-POS
               END-IF
           END-PERFORM

      *>   If we found user, back up to start of
      *>   that message object
           IF WS-SEARCH-POS > 10
      *>       Find the { before "role":"user"
               MOVE WS-SEARCH-POS TO WS-SEARCH-I
               PERFORM UNTIL WS-SEARCH-I < 2
                   OR WS-CONV-BUF(
                       WS-SEARCH-I:1) = "{"
                   SUBTRACT 1
                       FROM WS-SEARCH-I
               END-PERFORM
      *>       Back up one more for the comma
               IF WS-SEARCH-I > 1
               AND WS-CONV-BUF(
                   WS-SEARCH-I - 1:1) = ","
                   SUBTRACT 1
                       FROM WS-SEARCH-I
               END-IF
      *>       WS-SEARCH-I = start of user msg
      *>       Keep system + user msg, then jump
      *>       to last 20000 chars
               COMPUTE WS-K =
                   WS-CONV-PTR - 1
               IF WS-K > 20000
      *>           Keep first SEARCH-I chars
      *>           + last 20000 chars
                   MOVE SPACES TO WS-JBUF-SAVE
                   MOVE 1 TO WS-PTR
      *>           Copy system prompt part
                   STRING
                       WS-CONV-BUF(
                           1:WS-SEARCH-I)
                       DELIMITED SIZE
                       INTO WS-JBUF-SAVE
                       WITH POINTER WS-PTR
                   END-STRING
      *>           Copy last 20000 chars
                   COMPUTE WS-SEARCH-POS =
                       WS-K - 20000 + 1
      *>           Find a good break point
      *>           (look for ,{"role")
                   PERFORM UNTIL
                       WS-SEARCH-POS >= WS-K
                       OR WS-CONV-BUF(
                           WS-SEARCH-POS:8)
                           = ',{"role"'
                       ADD 1
                           TO WS-SEARCH-POS
                   END-PERFORM
                   IF WS-SEARCH-POS < WS-K
                       COMPUTE WS-RESP-LEN =
                           WS-K
                           - WS-SEARCH-POS + 1
                       STRING
                           WS-CONV-BUF(
                               WS-SEARCH-POS:
                               WS-RESP-LEN)
                           DELIMITED SIZE
                           INTO WS-JBUF-SAVE
                           WITH POINTER WS-PTR
                       END-STRING
                   END-IF
      *>           Copy back
                   MOVE SPACES TO WS-CONV-BUF
                   COMPUTE WS-CONV-PTR =
                       WS-PTR
                   MOVE WS-JBUF-SAVE(
                       1:WS-CONV-PTR - 1)
                       TO WS-CONV-BUF(
                           1:WS-CONV-PTR - 1)
                   DISPLAY "    Trimmed to "
                       WS-CONV-PTR
               END-IF
           END-IF
           .

      *> ============================================================
      *> APPEND-TEXT-AND-NUDGE
      *> ============================================================
       APPEND-TEXT-AND-NUDGE.
           SUBTRACT 1 FROM WS-CONV-PTR

           STRING ","
               '{"role":"assistant",'
               '"content":"OK"},'
               '{"role":"user",'
               '"content":"Continue. '
               'Call a tool now."}]'
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING
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
               READ WORK-FILE INTO WS-LINE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       MOVE LENGTH(
                           TRIM(WS-LINE
                           TRAILING)) TO WS-K
                       IF WS-K > 0
                           IF WS-JLEN > 0
                               ADD 1 TO WS-JLEN
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
                           TRAILING)) TO WS-K
                       IF WS-K > 0
                           IF WS-TOOL-RESULT-LEN
                               > 0
                               ADD 1
                                 TO
                                 WS-TOOL-RESULT-LEN
                               MOVE " "
                                 TO
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
                                   1:WS-K)
                                 TO
                                 WS-TOOL-RESULT(
                                 WS-TOOL-RESULT-LEN
                                 + 1:WS-K)
                               ADD WS-K
                                 TO
                                 WS-TOOL-RESULT-LEN
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
           MOVE SPACES TO WS-TMP
           STRING WS-QT TRIM(WS-KEY-SEARCH) WS-QT
               DELIMITED SIZE INTO WS-TMP
           END-STRING

           MOVE 0 TO WS-KEY-POS
           PERFORM VARYING WS-FJV-POS
               FROM WS-JPOS BY 1
               UNTIL WS-FJV-POS > WS-JLEN
               OR WS-KEY-POS > 0
               IF WS-FJV-POS
                   + LENGTH(TRIM(WS-TMP))
                   - 1 <= WS-JLEN
               AND WS-JBUF(WS-FJV-POS:
                   LENGTH(TRIM(WS-TMP)))
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
           PERFORM UNTIL WS-FJV-POS > WS-JLEN
               OR WS-JBUF(WS-FJV-POS:1) = ":"
               ADD 1 TO WS-FJV-POS
           END-PERFORM
           ADD 1 TO WS-FJV-POS

           PERFORM UNTIL WS-FJV-POS > WS-JLEN
               OR WS-JBUF(WS-FJV-POS:1)
                   NOT = " "
               ADD 1 TO WS-FJV-POS
           END-PERFORM

           IF WS-JBUF(WS-FJV-POS:1) = WS-QT
               ADD 1 TO WS-FJV-POS
               MOVE WS-FJV-POS TO WS-VAL-START
               PERFORM UNTIL
                   WS-FJV-POS > WS-JLEN
                   IF WS-JBUF(WS-FJV-POS:1)
                       = X"5C"
                   AND WS-FJV-POS < WS-JLEN
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
               IF WS-VAL-END >= WS-VAL-START
               AND WS-VAL-END - WS-VAL-START
                   + 1 <= 8000
                   MOVE WS-JBUF(WS-VAL-START:
                       WS-VAL-END
                       - WS-VAL-START + 1)
                       TO WS-JVAL
               END-IF
               ADD 1 TO WS-FJV-POS
           ELSE
               MOVE WS-FJV-POS TO WS-VAL-START
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
               IF WS-VAL-END >= WS-VAL-START
                   MOVE WS-JBUF(WS-VAL-START:
                       WS-VAL-END
                       - WS-VAL-START + 1)
                       TO WS-JVAL
               END-IF
           END-IF
           MOVE WS-FJV-POS TO WS-JPOS
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

           PERFORM VARYING WS-ESC-I FROM 1 BY 1
               UNTIL WS-ESC-I > WS-ESC-ILEN
               OR WS-ESC-OLEN > 31000
               EVALUATE TRUE
               WHEN WS-ESC-IN(WS-ESC-I:1)
                   = WS-QT
                   ADD 1 TO WS-ESC-OLEN
                   MOVE X"5C"
                     TO WS-ESC-OUT(
                       WS-ESC-OLEN:1)
                   ADD 1 TO WS-ESC-OLEN
                   MOVE WS-QT
                     TO WS-ESC-OUT(
                       WS-ESC-OLEN:1)
               WHEN WS-ESC-IN(WS-ESC-I:1)
                   = X"5C"
                   ADD 1 TO WS-ESC-OLEN
                   MOVE X"5C"
                     TO WS-ESC-OUT(
                       WS-ESC-OLEN:1)
                   ADD 1 TO WS-ESC-OLEN
                   MOVE X"5C"
                     TO WS-ESC-OUT(
                       WS-ESC-OLEN:1)
               WHEN WS-ESC-IN(WS-ESC-I:1)
                   = X"0A"
                   ADD 1 TO WS-ESC-OLEN
                   MOVE X"5C"
                     TO WS-ESC-OUT(
                       WS-ESC-OLEN:1)
                   ADD 1 TO WS-ESC-OLEN
                   MOVE "n"
                     TO WS-ESC-OUT(
                       WS-ESC-OLEN:1)
               WHEN WS-ESC-IN(WS-ESC-I:1)
                   = X"0D"
                   CONTINUE
               WHEN WS-ESC-IN(WS-ESC-I:1)
                   = X"09"
                   ADD 1 TO WS-ESC-OLEN
                   MOVE " "
                     TO WS-ESC-OUT(
                       WS-ESC-OLEN:1)
               WHEN OTHER
                   ADD 1 TO WS-ESC-OLEN
                   MOVE WS-ESC-IN(WS-ESC-I:1)
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
           PERFORM UNTIL WS-ESC-I > WS-ESC-ILEN
               IF WS-ESC-IN(WS-ESC-I:1) = X"5C"
               AND WS-ESC-I < WS-ESC-ILEN
                   ADD 1 TO WS-ESC-I
                   EVALUATE TRUE
                   WHEN WS-ESC-IN(WS-ESC-I:1)
                       = "n"
                       ADD 1 TO WS-ESC-OLEN
                       MOVE X"0A"
                         TO WS-ESC-OUT(
                           WS-ESC-OLEN:1)
                   WHEN WS-ESC-IN(WS-ESC-I:1)
                       = "r"
                       CONTINUE
                   WHEN WS-ESC-IN(WS-ESC-I:1)
                       = "t"
                       ADD 1 TO WS-ESC-OLEN
                       MOVE X"09"
                         TO WS-ESC-OUT(
                           WS-ESC-OLEN:1)
                   WHEN OTHER
                       ADD 1 TO WS-ESC-OLEN
                       MOVE WS-ESC-IN(
                           WS-ESC-I:1)
                         TO WS-ESC-OUT(
                           WS-ESC-OLEN:1)
                   END-EVALUATE
               ELSE
                   ADD 1 TO WS-ESC-OLEN
                   MOVE WS-ESC-IN(WS-ESC-I:1)
                     TO WS-ESC-OUT(
                       WS-ESC-OLEN:1)
               END-IF
               ADD 1 TO WS-ESC-I
           END-PERFORM
           .
