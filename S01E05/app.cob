       IDENTIFICATION DIVISION.
       PROGRAM-ID. S01E05-RAILWAY.
      *> ============================================================
      *> S01E05 - Railway Route Activation (Function Calling Agent)
      *> Uses OpenAI function calling in an agent loop:
      *> 1. Send system prompt + user message + tool definition
      *> 2. Parse response for tool_calls
      *> 3. Execute tool call via hub API, append result
      *> 4. Loop until no more tool_calls or flag found
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
       01  WORK-REC                PIC X(32000).

       WORKING-STORAGE SECTION.
      *> -- Config --
       01  WS-HUB-KEY              PIC X(50).
       01  WS-OPENAI-KEY           PIC X(200).
       01  WS-QT                   PIC X(1) VALUE '"'.
       01  WS-FS                   PIC XX.
       01  WS-WORK-PATH            PIC X(100) VALUE "work.tmp".

      *> -- URLs --
       01  WS-HUB-URL              PIC X(100).
       01  WS-VERIFY-URL           PIC X(200).
       01  WS-OPENAI-URL           PIC X(200).
       01  WS-TASK-NAME            PIC X(20) VALUE "railway".

      *> -- System command --
       01  WS-CMD                  PIC X(4000).

      *> -- Request/Response --
       01  WS-PAYLOAD              PIC X(4000).
       01  WS-RESP-BUF             PIC X(8000).
       01  WS-RESP-LEN             PIC 9(5) VALUE 0.
       01  WS-STATUS-CODE          PIC X(3).

      *> -- Retry --
       01  WS-MAX-RETRIES          PIC 9(2) VALUE 50.
       01  WS-ATTEMPT              PIC 9(2).
       01  WS-DONE                 PIC X VALUE "N".

      *> -- Retry-After parsing --
       01  WS-RETRY-AFTER          PIC 9(3) VALUE 0.
       01  WS-RETRY-STR            PIC X(10).
       01  WS-SLEEP-SECS           PIC 9(3).

      *> -- File reading --
       01  WS-EOF                  PIC X VALUE "N".
       01  WS-LINE                 PIC X(16000).
       01  WS-K                    PIC 9(5).

      *> -- Flag --
       01  WS-FLAG-FOUND           PIC X VALUE "N".
       01  WS-TALLY-CNT            PIC 9(4).

      *> -- Agent loop --
       01  WS-STEP                 PIC 9(2) VALUE 0.
       01  WS-MAX-STEPS            PIC 9(2) VALUE 20.
       01  WS-HAS-TOOL-CALLS      PIC X VALUE "N".

      *> -- Messages buffer (accumulated JSON) --
      *> We store the full messages array as a string that grows
       01  WS-MSGS                 PIC X(64000).
       01  WS-MSGS-LEN            PIC 9(5) VALUE 0.

      *> -- LLM request builder --
       01  WS-REQ-JSON             PIC X(64000).
       01  WS-PTR                  PIC 9(5).

      *> -- JSON parsing --
       01  WS-JBUF                 PIC X(32000).
       01  WS-JLEN                 PIC 9(5).
       01  WS-JPOS                 PIC 9(5).
       01  WS-JVAL                 PIC X(4000).
       01  WS-KEY-SEARCH           PIC X(50).
       01  WS-KEY-POS              PIC 9(5).
       01  WS-VAL-START            PIC 9(5).
       01  WS-VAL-END              PIC 9(5).
       01  WS-FJV-POS             PIC 9(5).
       01  WS-TMP                  PIC X(500).

      *> -- Tool call parsing --
       01  WS-TOOL-CALL-ID         PIC X(100).
       01  WS-FUNC-NAME            PIC X(100).
       01  WS-FUNC-ARGS            PIC X(2000).
       01  WS-TC-ACTION            PIC X(50).
       01  WS-TC-ROUTE             PIC X(50).
       01  WS-TC-VALUE             PIC X(200).

      *> -- Tool result --
       01  WS-TOOL-RESULT          PIC X(4000).
       01  WS-TOOL-RESULT-LEN      PIC 9(5).

      *> -- Assistant message raw (for appending tool_calls) --
       01  WS-ASST-MSG-RAW         PIC X(4000).
       01  WS-ASST-MSG-LEN         PIC 9(5).

      *> -- Content from final response --
       01  WS-CONTENT              PIC X(4000).

      *> -- JSON escaping --
       01  WS-ESC-BUF              PIC X(8000).
       01  WS-ESC-LEN              PIC 9(5).
       01  WS-ESC-I                PIC 9(5).
       01  WS-ESC-SRC              PIC X(8000).
       01  WS-ESC-SRC-LEN          PIC 9(5).

      *> -- Action JSON for hub --
       01  WS-ACTION-JSON          PIC X(500).
       01  WS-STEP-NAME            PIC X(30).

      *> -- Temp for string building --
       01  WS-TMPBUF               PIC X(8000).
       01  WS-TMPLEN               PIC 9(5).

      *> -- Tool definition (constant) --
       01  WS-TOOL-DEF             PIC X(1000).

      *> -- Search helpers --
       01  WS-SEARCH-STR           PIC X(100).
       01  WS-SEARCH-POS           PIC 9(5).
       01  WS-SEARCH-I             PIC 9(5).
       01  WS-SEARCH-SLEN          PIC 9(5).

      *> -- Multiple tool calls --
       01  WS-TC-SEARCH-START      PIC 9(5).

      *> -- NL for JSON --
       01  WS-NL                   PIC X(2).

      *> -- Extract between brackets --
       01  WS-BRACE-DEPTH          PIC 9(3).
       01  WS-BRACE-START          PIC 9(5).
       01  WS-BRACE-POS            PIC 9(5).

      *> -- Tool calls section raw --
       01  WS-TC-SECTION           PIC X(4000).
       01  WS-TC-SECTION-LEN       PIC 9(5).

      *> -- Msg append temp --
       01  WS-APPEND-BUF           PIC X(8000).
       01  WS-APPEND-LEN           PIC 9(5).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S01E05 RAILWAY - Agent Loop ==="

           ACCEPT WS-HUB-KEY FROM ENVIRONMENT "HUB_API_KEY"
           ACCEPT WS-OPENAI-KEY
               FROM ENVIRONMENT "OPENAI_API_KEY"
           ACCEPT WS-HUB-URL FROM ENVIRONMENT "HUB_API_URL"
           ACCEPT WS-OPENAI-URL
               FROM ENVIRONMENT "OPENAI_API_URL"

           IF WS-HUB-KEY = SPACES
               DISPLAY "ERROR: Set HUB_API_KEY!"
               STOP RUN
           END-IF
           IF WS-OPENAI-KEY = SPACES
               DISPLAY "ERROR: Set OPENAI_API_KEY!"
               STOP RUN
           END-IF
           IF WS-HUB-URL = SPACES
               DISPLAY "ERROR: Set HUB_API_URL!"
               STOP RUN
           END-IF
           IF WS-OPENAI-URL = SPACES
               DISPLAY "ERROR: Set OPENAI_API_URL!"
               STOP RUN
           END-IF

           INITIALIZE WS-VERIFY-URL
           STRING
               TRIM(WS-HUB-URL) "/verify"
               DELIMITED SIZE INTO WS-VERIFY-URL
           END-STRING

      *>   Build tool definition JSON (constant)
           INITIALIZE WS-TOOL-DEF
           STRING
               '{"type":"function","function":{'
               '"name":"send_railway_action",'
               '"description":"Send an action '
               'to the railway API. Use this to '
               'interact with the railway system.'
               ' Always start with action help '
               'to discover available actions.",'
               '"parameters":{"type":"object",'
               '"properties":{'
               '"action":{"type":"string",'
               '"description":"The API action '
               'to execute (e.g. help, '
               'reconfigure, getstatus, '
               'setstatus, save)"},'
               '"route":{"type":"string",'
               '"description":"Route identifier'
               ', e.g. X-01. Required for most '
               'actions except help."},'
               '"value":{"type":"string",'
               '"description":"Value parameter, '
               'used with actions like '
               'setstatus."}},'
               '"required":["action"]}}}'
               DELIMITED SIZE INTO WS-TOOL-DEF
           END-STRING

      *>   Init newline escape sequence
           MOVE X"5C" TO WS-NL(1:1)
           MOVE "n"    TO WS-NL(2:1)

           MOVE "N" TO WS-FLAG-FOUND

      *>   Initialize messages array with system + user
           INITIALIZE WS-MSGS
           MOVE 1 TO WS-PTR
           STRING
               '{"role":"system","content":"'
               'You are an AI agent that '
               'activates railway routes via '
               'an API.'
               WS-NL WS-NL
               'Your goal: activate route X-01.'
               WS-NL WS-NL
               'Rules:'
               WS-NL
               '1. Start by calling '
               'send_railway_action with action '
               WS-NL
               '   help to discover the API '
               'documentation.'
               WS-NL
               '2. Read the API response '
               'carefully and follow the '
               'documented steps exactly.'
               WS-NL
               '3. Call one action at a time. '
               'After each response, decide the '
               'next action.'
               WS-NL
               '4. If an API response contains '
               'an error, analyze it and adjust '
               'your approach.'
               WS-NL
               '5. Continue until you see a flag'
               ' in format {FLG:...} in any '
               'response.'
               WS-NL
               '6. When you find the flag, '
               'respond with DONE and the flag.'
               WS-NL WS-NL
               'Do NOT guess actions or '
               'parameters - rely only on what '
               'the API tells you.'
               '"},'
               '{"role":"user","content":"'
               'Activate railway route X-01. '
               'Start by calling help."}'
               DELIMITED SIZE
               INTO WS-MSGS
               WITH POINTER WS-PTR
           END-STRING
           COMPUTE WS-MSGS-LEN = WS-PTR - 1

      *>   === AGENT LOOP ===
           PERFORM VARYING WS-STEP FROM 1 BY 1
               UNTIL WS-STEP > WS-MAX-STEPS
               OR WS-FLAG-FOUND = "Y"

               DISPLAY " "
               DISPLAY "============================="
                   "========================="
               DISPLAY "Step " WS-STEP
                   ": calling LLM..."

      *>       Build full OpenAI request
               PERFORM BUILD-OPENAI-REQUEST

      *>       Write request to file
               MOVE "llm_req.json" TO WS-WORK-PATH
               OPEN OUTPUT WORK-FILE
               WRITE WORK-REC FROM WS-REQ-JSON
               CLOSE WORK-FILE

      *>       Call OpenAI
               PERFORM CALL-OPENAI-API

      *>       Read response
               MOVE "llm_resp.json" TO WS-WORK-PATH
               PERFORM READ-JSON-FILE
               MOVE "work.tmp" TO WS-WORK-PATH

      *>       Check for tool_calls in response
               PERFORM CHECK-FOR-TOOL-CALLS

               IF WS-HAS-TOOL-CALLS = "Y"
      *>           Extract and execute tool call
                   PERFORM PROCESS-TOOL-CALLS
               ELSE
      *>           No tool calls - extract content
                   PERFORM EXTRACT-FINAL-CONTENT
                   DISPLAY " "
                   DISPLAY "Agent: "
                       TRIM(WS-CONTENT)(1:1000)

      *>           Check for flag in content
                   MOVE 0 TO WS-TALLY-CNT
                   IF LENGTH(TRIM(WS-CONTENT)) > 0
                       INSPECT WS-CONTENT
                           TALLYING WS-TALLY-CNT
                           FOR ALL "FLG"
                   END-IF
                   IF WS-TALLY-CNT > 0
                       MOVE "Y" TO WS-FLAG-FOUND
                       DISPLAY " "
                       DISPLAY "*** FLAG FOUND IN "
                           "CONTENT ***"
                   END-IF

      *>           Done - no more tool calls
                   EXIT PERFORM
               END-IF
           END-PERFORM

           DISPLAY " "
           DISPLAY "Agent finished."
           DISPLAY "=== DONE ==="
           STOP RUN.

      *> ============================================================
      *> BUILD-OPENAI-REQUEST: Build the full API request JSON
      *> with messages array and tools
      *> ============================================================
       BUILD-OPENAI-REQUEST.
           INITIALIZE WS-REQ-JSON
           MOVE 1 TO WS-PTR

           STRING
               '{"model":"gpt-4.1-mini",'
               '"messages":['
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Append messages
           IF WS-MSGS-LEN > 0
               STRING
                   WS-MSGS(1:WS-MSGS-LEN)
                   DELIMITED SIZE
                   INTO WS-REQ-JSON
                   WITH POINTER WS-PTR
               END-STRING
           END-IF

      *>   Close messages, add tools
           STRING
               '],"tools":['
               TRIM(WS-TOOL-DEF)
               '],"tool_choice":"auto",'
               '"temperature":0}'
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           .

      *> ============================================================
      *> CALL-OPENAI-API: POST llm_req.json to OpenAI
      *> ============================================================
       CALL-OPENAI-API.
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
               " -d @llm_req.json"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD
           .

      *> ============================================================
      *> CHECK-FOR-TOOL-CALLS: Check if response has tool_calls
      *> ============================================================
       CHECK-FOR-TOOL-CALLS.
           MOVE "N" TO WS-HAS-TOOL-CALLS

      *>   Search for "tool_calls" in the response
           MOVE "tool_calls" TO WS-SEARCH-STR
           MOVE 10 TO WS-SEARCH-SLEN
           MOVE 0 TO WS-SEARCH-POS

           IF WS-JLEN < 12
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-SEARCH-I
               FROM 1 BY 1
               UNTIL WS-SEARCH-I > WS-JLEN - 11
               OR WS-SEARCH-POS > 0
               IF WS-JBUF(WS-SEARCH-I:10)
                   = "tool_calls"
      *>           Make sure it is not "tool_calls":null
                   MOVE WS-SEARCH-I TO WS-SEARCH-POS
               END-IF
           END-PERFORM

           IF WS-SEARCH-POS = 0
               EXIT PARAGRAPH
           END-IF

      *>   Check it's not null - find : after tool_calls
           COMPUTE WS-SEARCH-I =
               WS-SEARCH-POS + 10
           PERFORM UNTIL WS-SEARCH-I > WS-JLEN
               OR WS-JBUF(WS-SEARCH-I:1) = ":"
               ADD 1 TO WS-SEARCH-I
           END-PERFORM
           ADD 1 TO WS-SEARCH-I
      *>   Skip spaces
           PERFORM UNTIL WS-SEARCH-I > WS-JLEN
               OR WS-JBUF(WS-SEARCH-I:1)
               NOT = " "
               ADD 1 TO WS-SEARCH-I
           END-PERFORM

      *>   If it starts with [ it has tool calls
           IF WS-SEARCH-I <= WS-JLEN
           AND WS-JBUF(WS-SEARCH-I:1) = "["
               MOVE "Y" TO WS-HAS-TOOL-CALLS
           END-IF

      *>   Also check not "null"
           IF WS-SEARCH-I + 3 <= WS-JLEN
           AND WS-JBUF(WS-SEARCH-I:4) = "null"
               MOVE "N" TO WS-HAS-TOOL-CALLS
           END-IF
           .

      *> ============================================================
      *> PROCESS-TOOL-CALLS: Extract tool call info, execute,
      *> and append messages
      *> ============================================================
       PROCESS-TOOL-CALLS.
      *>   First, extract the assistant message to append
      *>   We need: role=assistant, tool_calls=[...]
      *>   Find the "choices" array, then the message object

      *>   Extract tool_call_id
           MOVE "id" TO WS-KEY-SEARCH

      *>   We need to find "id" inside tool_calls section
      *>   First find "tool_calls" position again
           MOVE "tool_calls" TO WS-SEARCH-STR
           MOVE 0 TO WS-TC-SEARCH-START
           PERFORM VARYING WS-SEARCH-I
               FROM 1 BY 1
               UNTIL WS-SEARCH-I > WS-JLEN - 11
               OR WS-TC-SEARCH-START > 0
               IF WS-JBUF(WS-SEARCH-I:10)
                   = "tool_calls"
                   MOVE WS-SEARCH-I
                       TO WS-TC-SEARCH-START
               END-IF
           END-PERFORM

      *>   Find the [ that starts the tool_calls array
           MOVE WS-TC-SEARCH-START TO WS-SEARCH-I
           PERFORM UNTIL WS-SEARCH-I > WS-JLEN
               OR WS-JBUF(WS-SEARCH-I:1) = "["
               ADD 1 TO WS-SEARCH-I
           END-PERFORM

      *>   Extract the entire tool_calls array [...] content
           MOVE WS-SEARCH-I TO WS-BRACE-START
           MOVE 1 TO WS-BRACE-DEPTH
           ADD 1 TO WS-SEARCH-I
           PERFORM UNTIL WS-SEARCH-I > WS-JLEN
               OR WS-BRACE-DEPTH = 0
               IF WS-JBUF(WS-SEARCH-I:1) = "["
                   ADD 1 TO WS-BRACE-DEPTH
               END-IF
               IF WS-JBUF(WS-SEARCH-I:1) = "]"
                   SUBTRACT 1 FROM WS-BRACE-DEPTH
               END-IF
               ADD 1 TO WS-SEARCH-I
           END-PERFORM
      *>   WS-SEARCH-I is now past the closing ]
           COMPUTE WS-TC-SECTION-LEN =
               WS-SEARCH-I - WS-BRACE-START
           IF WS-TC-SECTION-LEN > 4000
               MOVE 4000 TO WS-TC-SECTION-LEN
           END-IF
           MOVE WS-JBUF(WS-BRACE-START:
               WS-TC-SECTION-LEN) TO WS-TC-SECTION

      *>   Now extract tool_call id from within tool_calls
      *>   Find "id" after tool_calls position
           MOVE WS-TC-SEARCH-START TO WS-JPOS
           MOVE "id" TO WS-KEY-SEARCH
           PERFORM FIND-JSON-VAL
           MOVE WS-JVAL TO WS-TOOL-CALL-ID
           DISPLAY "  Tool call ID: "
               TRIM(WS-TOOL-CALL-ID)(1:60)

      *>   Extract function name
      *>   Find "name" after tool_calls (inside function)
      *>   We search for "name" after "function" keyword
           MOVE "name" TO WS-KEY-SEARCH
           PERFORM FIND-JSON-VAL
           MOVE WS-JVAL TO WS-FUNC-NAME
           DISPLAY "  Function: " TRIM(WS-FUNC-NAME)

      *>   Extract arguments - it's a JSON string
           MOVE "arguments" TO WS-KEY-SEARCH
           PERFORM FIND-JSON-VAL
           MOVE WS-JVAL TO WS-FUNC-ARGS
           DISPLAY "  Arguments: "
               TRIM(WS-FUNC-ARGS)(1:200)

      *>   Parse the arguments JSON to get action/route/value
           PERFORM PARSE-TOOL-ARGS

      *>   Build the action JSON for the hub API
           PERFORM BUILD-ACTION-JSON

           DISPLAY " "
           DISPLAY ">> AI calls: "
               TRIM(WS-ACTION-JSON)

      *>   Execute the action (POST to hub)
           MOVE "tool_exec" TO WS-STEP-NAME
           PERFORM SEND-ACTION-WITH-RETRY

           IF WS-FLAG-FOUND = "Y"
               EXIT PARAGRAPH
           END-IF

      *>   Now append assistant message + tool result to messages
      *>   Assistant message: {"role":"assistant","content":null,
      *>     "tool_calls":[...]}
      *>   Tool result: {"role":"tool","tool_call_id":"...","content":"..."}

           PERFORM APPEND-ASST-AND-TOOL-MSGS
           .

      *> ============================================================
      *> PARSE-TOOL-ARGS: Parse action/route/value from arguments
      *> The arguments field is a JSON string that was inside quotes
      *> so backslash-quotes are present. We need to unescape first.
      *> ============================================================
       PARSE-TOOL-ARGS.
           MOVE SPACES TO WS-TC-ACTION
           MOVE SPACES TO WS-TC-ROUTE
           MOVE SPACES TO WS-TC-VALUE

      *>   The FUNC-ARGS might have escaped quotes \" that we
      *>   need to unescape, OR it might already be clean JSON
      *>   from FIND-JSON-VAL. Let's handle both.

      *>   Unescape \" to " in WS-FUNC-ARGS
           MOVE SPACES TO WS-TMPBUF
           MOVE 0 TO WS-TMPLEN
           MOVE LENGTH(TRIM(WS-FUNC-ARGS)) TO WS-K
           MOVE 1 TO WS-ESC-I

           PERFORM UNTIL WS-ESC-I > WS-K
               IF WS-ESC-I < WS-K
               AND WS-FUNC-ARGS(WS-ESC-I:1) = X"5C"
               AND WS-FUNC-ARGS(WS-ESC-I + 1:1)
                   = WS-QT
                   ADD 1 TO WS-TMPLEN
                   MOVE WS-QT
                       TO WS-TMPBUF(WS-TMPLEN:1)
                   ADD 2 TO WS-ESC-I
               ELSE
               IF WS-ESC-I < WS-K
               AND WS-FUNC-ARGS(WS-ESC-I:1) = X"5C"
               AND WS-FUNC-ARGS(WS-ESC-I + 1:1)
                   = X"5C"
                   ADD 1 TO WS-TMPLEN
                   MOVE X"5C"
                       TO WS-TMPBUF(WS-TMPLEN:1)
                   ADD 2 TO WS-ESC-I
               ELSE
                   ADD 1 TO WS-TMPLEN
                   MOVE WS-FUNC-ARGS(WS-ESC-I:1)
                       TO WS-TMPBUF(WS-TMPLEN:1)
                   ADD 1 TO WS-ESC-I
               END-IF
               END-IF
           END-PERFORM

      *>   Now parse from TMPBUF as JSON
      *>   Copy to JBUF for reuse of FIND-JSON-VAL
           MOVE WS-TMPBUF TO WS-JBUF
           MOVE WS-TMPLEN TO WS-JLEN

      *>   Extract "action"
           MOVE "action" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL) TO WS-TC-ACTION

      *>   Extract "route" (optional)
           MOVE "route" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL) TO WS-TC-ROUTE

      *>   Extract "value" (optional)
           MOVE "value" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL) TO WS-TC-VALUE

           DISPLAY "  Parsed - action: "
               TRIM(WS-TC-ACTION)
               " route: " TRIM(WS-TC-ROUTE)
               " value: " TRIM(WS-TC-VALUE)

      *>   Restore JBUF from the LLM response file
           MOVE "llm_resp.json" TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH
           .

      *> ============================================================
      *> BUILD-ACTION-JSON: Build action JSON from parsed args
      *> ============================================================
       BUILD-ACTION-JSON.
           INITIALIZE WS-ACTION-JSON
           MOVE 1 TO WS-PTR

           STRING
               '{"action":"'
               TRIM(WS-TC-ACTION)
               '"'
               DELIMITED SIZE
               INTO WS-ACTION-JSON
               WITH POINTER WS-PTR
           END-STRING

           IF TRIM(WS-TC-ROUTE) NOT = SPACES
               STRING
                   ',"route":"'
                   TRIM(WS-TC-ROUTE)
                   '"'
                   DELIMITED SIZE
                   INTO WS-ACTION-JSON
                   WITH POINTER WS-PTR
               END-STRING
           END-IF

           IF TRIM(WS-TC-VALUE) NOT = SPACES
               STRING
                   ',"value":"'
                   TRIM(WS-TC-VALUE)
                   '"'
                   DELIMITED SIZE
                   INTO WS-ACTION-JSON
                   WITH POINTER WS-PTR
               END-STRING
           END-IF

           STRING
               "}"
               DELIMITED SIZE
               INTO WS-ACTION-JSON
               WITH POINTER WS-PTR
           END-STRING
           .

      *> ============================================================
      *> APPEND-ASST-AND-TOOL-MSGS: Append assistant + tool
      *> messages to WS-MSGS
      *> ============================================================
       APPEND-ASST-AND-TOOL-MSGS.
      *>   We need to append:
      *>   ,{"role":"assistant","content":null,
      *>     "tool_calls":[<raw tool_calls>]}
      *>   ,{"role":"tool","tool_call_id":"<id>",
      *>     "content":"<escaped result>"}

      *>   First escape the tool result for JSON embedding
           MOVE WS-RESP-BUF TO WS-ESC-SRC
           MOVE WS-RESP-LEN TO WS-ESC-SRC-LEN
           PERFORM ESCAPE-GENERIC

      *>   Append to messages
           MOVE 1 TO WS-PTR
           INITIALIZE WS-APPEND-BUF
           STRING
               ',{"role":"assistant",'
               '"content":null,'
               '"tool_calls":'
               WS-TC-SECTION(1:WS-TC-SECTION-LEN)
               '},'
               '{"role":"tool",'
               '"tool_call_id":"'
               TRIM(WS-TOOL-CALL-ID)
               '","content":"'
               DELIMITED SIZE
               INTO WS-APPEND-BUF
               WITH POINTER WS-PTR
           END-STRING

      *>   Append escaped result
           IF WS-ESC-LEN > 0
               STRING
                   WS-ESC-BUF(1:WS-ESC-LEN)
                   DELIMITED SIZE
                   INTO WS-APPEND-BUF
                   WITH POINTER WS-PTR
               END-STRING
           END-IF

           STRING
               '"}'
               DELIMITED SIZE
               INTO WS-APPEND-BUF
               WITH POINTER WS-PTR
           END-STRING

           COMPUTE WS-APPEND-LEN = WS-PTR - 1

      *>   Now append to WS-MSGS
           IF WS-MSGS-LEN + WS-APPEND-LEN < 64000
               MOVE WS-APPEND-BUF(1:WS-APPEND-LEN)
                   TO WS-MSGS(WS-MSGS-LEN + 1:
                   WS-APPEND-LEN)
               ADD WS-APPEND-LEN TO WS-MSGS-LEN
           ELSE
               DISPLAY "  WARNING: Messages buffer "
                   "overflow, truncating"
           END-IF
           .

      *> ============================================================
      *> EXTRACT-FINAL-CONTENT: Get content from non-tool response
      *> ============================================================
       EXTRACT-FINAL-CONTENT.
           MOVE SPACES TO WS-CONTENT
           MOVE "content" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE WS-JVAL TO WS-CONTENT
           .

      *> ============================================================
      *> ESCAPE-GENERIC: JSON-escape WS-ESC-SRC into WS-ESC-BUF
      *> ============================================================
       ESCAPE-GENERIC.
           MOVE SPACES TO WS-ESC-BUF
           MOVE 0 TO WS-ESC-LEN

           IF WS-ESC-SRC-LEN = 0
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-ESC-I
               FROM 1 BY 1
               UNTIL WS-ESC-I > WS-ESC-SRC-LEN
               OR WS-ESC-LEN > 7900
               EVALUATE TRUE
               WHEN WS-ESC-SRC(WS-ESC-I:1)
                   = WS-QT
                   ADD 1 TO WS-ESC-LEN
                   MOVE X"5C"
                       TO WS-ESC-BUF(WS-ESC-LEN:1)
                   ADD 1 TO WS-ESC-LEN
                   MOVE WS-QT
                       TO WS-ESC-BUF(WS-ESC-LEN:1)
               WHEN WS-ESC-SRC(WS-ESC-I:1)
                   = X"5C"
                   ADD 1 TO WS-ESC-LEN
                   MOVE X"5C"
                       TO WS-ESC-BUF(WS-ESC-LEN:1)
                   ADD 1 TO WS-ESC-LEN
                   MOVE X"5C"
                       TO WS-ESC-BUF(WS-ESC-LEN:1)
               WHEN WS-ESC-SRC(WS-ESC-I:1)
                   = X"0A"
                   ADD 1 TO WS-ESC-LEN
                   MOVE X"5C"
                       TO WS-ESC-BUF(WS-ESC-LEN:1)
                   ADD 1 TO WS-ESC-LEN
                   MOVE "n"
                       TO WS-ESC-BUF(WS-ESC-LEN:1)
               WHEN WS-ESC-SRC(WS-ESC-I:1)
                   = X"0D"
                   CONTINUE
               WHEN WS-ESC-SRC(WS-ESC-I:1)
                   = X"09"
                   ADD 1 TO WS-ESC-LEN
                   MOVE " "
                       TO WS-ESC-BUF(WS-ESC-LEN:1)
               WHEN OTHER
                   ADD 1 TO WS-ESC-LEN
                   MOVE WS-ESC-SRC(WS-ESC-I:1)
                       TO WS-ESC-BUF(WS-ESC-LEN:1)
               END-EVALUATE
           END-PERFORM
           .

      *> ============================================================
      *> SEND-ACTION-WITH-RETRY: POST action, retry on 503/429
      *> ============================================================
       SEND-ACTION-WITH-RETRY.
      *>   Build full payload
           INITIALIZE WS-PAYLOAD
           STRING
               '{"apikey":"'
               TRIM(WS-HUB-KEY)
               '","task":"'
               TRIM(WS-TASK-NAME)
               '","answer":'
               TRIM(WS-ACTION-JSON)
               '}'
               DELIMITED SIZE INTO WS-PAYLOAD
           END-STRING

      *>   Write payload to temp file
           MOVE "work.tmp" TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           WRITE WORK-REC FROM WS-PAYLOAD
           CLOSE WORK-FILE

      *>   Retry loop
           MOVE 1 TO WS-ATTEMPT
           MOVE "N" TO WS-DONE

           PERFORM UNTIL WS-ATTEMPT > WS-MAX-RETRIES
               OR WS-DONE = "Y"
               PERFORM SEND-SINGLE-ATTEMPT
               ADD 1 TO WS-ATTEMPT
           END-PERFORM

           IF WS-DONE NOT = "Y"
           AND WS-FLAG-FOUND NOT = "Y"
               DISPLAY "  Max retries exceeded!"
           END-IF
           .

      *> ============================================================
      *> SEND-SINGLE-ATTEMPT: One curl POST + check result
      *> ============================================================
       SEND-SINGLE-ATTEMPT.
      *>   POST via curl, dump headers to headers.tmp
           INITIALIZE WS-CMD
           STRING
               "curl -s -o resp.json"
               " -D headers.tmp"
               " -X POST "
               TRIM(WS-VERIFY-URL)
               " -H " WS-QT
               "Content-Type: application/json"
               WS-QT
               " -d @work.tmp"
               DELIMITED SIZE INTO WS-CMD
           END-STRING

           CALL "SYSTEM" USING WS-CMD

      *>   Read HTTP status + Retry-After from headers
           MOVE "headers.tmp" TO WS-WORK-PATH
           PERFORM READ-HTTP-HEADERS
           MOVE "work.tmp" TO WS-WORK-PATH

      *>   Handle 503
           IF WS-STATUS-CODE = "503"
               DISPLAY "  [" WS-ATTEMPT "] 503, wait 1s"
               MOVE 1 TO WS-SLEEP-SECS
               CALL "C$SLEEP" USING WS-SLEEP-SECS
               EXIT PARAGRAPH
           END-IF

      *>   Handle 429
           IF WS-STATUS-CODE = "429"
               IF WS-RETRY-AFTER < 1
                   MOVE 30 TO WS-RETRY-AFTER
               END-IF
               DISPLAY "  [" WS-ATTEMPT "] 429, wait "
                   WS-RETRY-AFTER "s"
               MOVE WS-RETRY-AFTER TO WS-SLEEP-SECS
               CALL "C$SLEEP" USING WS-SLEEP-SECS
               EXIT PARAGRAPH
           END-IF

      *>   Read response body
           MOVE "resp.json" TO WS-WORK-PATH
           PERFORM READ-RESPONSE-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           DISPLAY "  => "
               TRIM(WS-RESP-BUF)(1:500)

      *>   Check for flag {FLG:...}
           MOVE 0 TO WS-TALLY-CNT
           IF WS-RESP-LEN > 0
               INSPECT WS-RESP-BUF(1:WS-RESP-LEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL "FLG"
           END-IF
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-FLAG-FOUND
               MOVE "Y" TO WS-DONE
               DISPLAY " "
               DISPLAY "*** FLAG FOUND! ***"
               EXIT PARAGRAPH
           END-IF

      *>   Step done
           MOVE "Y" TO WS-DONE
           .

      *> ============================================================
      *> READ-HTTP-HEADERS: Parse status code + Retry-After
      *> ============================================================
       READ-HTTP-HEADERS.
           MOVE "000" TO WS-STATUS-CODE
           MOVE 0 TO WS-RETRY-AFTER
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
                       IF WS-LINE(1:5) = "HTTP/"
                           PERFORM EXTRACT-STATUS-CODE
                       END-IF
                       IF WS-LINE(1:13) =
                           "Retry-After: "
                           PERFORM EXTRACT-RETRY-AFTER
                       END-IF
               END-READ
           END-PERFORM

           CLOSE WORK-FILE
           MOVE "N" TO WS-EOF
           .

      *> ============================================================
      *> EXTRACT-STATUS-CODE: Get NNN from "HTTP/x.x NNN ..."
      *> ============================================================
       EXTRACT-STATUS-CODE.
           MOVE 6 TO WS-K
           PERFORM UNTIL WS-K > 15
               OR WS-LINE(WS-K:1) = " "
               ADD 1 TO WS-K
           END-PERFORM
           ADD 1 TO WS-K
           IF WS-K <= 12
               MOVE WS-LINE(WS-K:3)
                   TO WS-STATUS-CODE
           END-IF
           .

      *> ============================================================
      *> EXTRACT-RETRY-AFTER: Get seconds from "Retry-After: NN"
      *> ============================================================
       EXTRACT-RETRY-AFTER.
           MOVE SPACES TO WS-RETRY-STR
           MOVE TRIM(WS-LINE(14:)) TO WS-RETRY-STR
           INSPECT WS-RETRY-STR REPLACING ALL X"0D"
               BY SPACE
           INSPECT WS-RETRY-STR REPLACING ALL X"0A"
               BY SPACE
           MOVE TRIM(WS-RETRY-STR) TO WS-RETRY-STR
           IF WS-RETRY-STR IS NUMERIC
               MOVE WS-RETRY-STR TO WS-RETRY-AFTER
           ELSE
               MOVE 30 TO WS-RETRY-AFTER
           END-IF
           .

      *> ============================================================
      *> READ-RESPONSE-FILE: Read response body into WS-RESP-BUF
      *> ============================================================
       READ-RESPONSE-FILE.
           MOVE SPACES TO WS-RESP-BUF
           MOVE 0 TO WS-RESP-LEN
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
                       MOVE LENGTH(TRIM(WS-LINE
                           TRAILING)) TO WS-K
                       IF WS-K > 0
                           IF WS-RESP-LEN > 0
                               ADD 1 TO WS-RESP-LEN
                               MOVE " "
                                   TO WS-RESP-BUF(
                                   WS-RESP-LEN:1)
                           END-IF
                           IF WS-K > 8000
                               MOVE 8000 TO WS-K
                           END-IF
                           IF WS-RESP-LEN + WS-K
                               <= 8000
                               MOVE WS-LINE(1:WS-K)
                                   TO WS-RESP-BUF(
                                   WS-RESP-LEN
                                   + 1:WS-K)
                               ADD WS-K
                                   TO WS-RESP-LEN
                           END-IF
                       END-IF
               END-READ
           END-PERFORM

           CLOSE WORK-FILE
           MOVE "N" TO WS-EOF
           .

      *> ============================================================
      *> READ-JSON-FILE: Read file into WS-JBUF
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
                               MOVE 16000 TO WS-K
                           END-IF
                           IF WS-JLEN + WS-K
                               <= 32000
                               MOVE WS-LINE(1:WS-K)
                                   TO WS-JBUF(
                                   WS-JLEN
                                   + 1:WS-K)
                               ADD WS-K TO WS-JLEN
                           END-IF
                       END-IF
               END-READ
           END-PERFORM

           CLOSE WORK-FILE
           MOVE "N" TO WS-EOF
           .

      *> ============================================================
      *> FIND-JSON-VAL: Find value for key in WS-JBUF
      *> starting from WS-JPOS. Updates WS-JPOS to after value.
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
               UNTIL WS-FJV-POS > WS-JLEN
               OR WS-KEY-POS > 0
               IF WS-FJV-POS
                   + LENGTH(TRIM(WS-TMP))
                   - 1 <= WS-JLEN
               AND WS-JBUF(
                   WS-FJV-POS:
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

           IF WS-JBUF(WS-FJV-POS:1) = WS-QT
               ADD 1 TO WS-FJV-POS
               MOVE WS-FJV-POS
                   TO WS-VAL-START
               PERFORM UNTIL
                   WS-FJV-POS > WS-JLEN
                   IF WS-JBUF(
                       WS-FJV-POS:1)
                       = WS-QT
                       IF WS-FJV-POS > 1
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
               IF WS-VAL-END >= WS-VAL-START
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
                   MOVE WS-JBUF(
                       WS-VAL-START:
                       WS-VAL-END
                       - WS-VAL-START
                       + 1) TO WS-JVAL
               END-IF
           END-IF
           MOVE WS-FJV-POS TO WS-JPOS
           .
