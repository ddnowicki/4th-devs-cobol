       IDENTIFICATION DIVISION.
       PROGRAM-ID. S04E05-FOODWAREHOUSE.
      *> ============================================================
      *> S04E05 - Food Warehouse
      *> Agent loop with LLM function calling:
      *> 1. Build system prompt with all tool definitions
      *> 2. LLM decides which tools to call
      *> 3. COBOL executes tool calls via Hub API
      *> 4. Results fed back to LLM
      *> 5. Loop until done tool returns flag
      *> Tools: reset, help, database, signatureGenerator,
      *>        orders, done
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
       01  WS-REQ-JSON             PIC X(64000).
       01  WS-HUB-BODY             PIC X(8000).
       01  WS-DATA-URL             PIC X(200).

      *> === Shared copybooks (WS) ===
       COPY JSONPARSE-WS.
       COPY JSONREAD-WS.
       COPY JSONESCAPE-WS.
       COPY TOOLPARSE-WS.

      *> === Task Configuration ===
       01  WS-TASK-NAME            PIC X(20)
                                   VALUE
                                   "foodwarehouse".

      *> === Task Data ===
      *> -- Agent conversation --
       01  WS-CONV-BUF             PIC X(64000).
       01  WS-CONV-PTR             PIC 9(5).
       01  WS-AG-STEP              PIC 9(2)
                                   VALUE 0.
       01  WS-AG-DONE              PIC X
                                   VALUE "N".
       01  WS-NUDGE-CT             PIC 9(1)
                                   VALUE 0.

      *> -- Tool call result --
       01  WS-TOOL-RESULT          PIC X(8000).
       01  WS-TOOL-RESULT-LEN      PIC 9(5).

      *> -- Tool arg values --
       01  WS-TA-QUERY             PIC X(2000).
       01  WS-TA-ACTION            PIC X(50).
       01  WS-TA-LOGIN             PIC X(200).
       01  WS-TA-BIRTHDAY          PIC X(50).
       01  WS-TA-DEST              PIC X(50).
       01  WS-TA-TITLE             PIC X(500).
       01  WS-TA-CREATOR           PIC X(50).
       01  WS-TA-SIGNATURE         PIC X(100).
       01  WS-TA-ID                PIC X(50).
       01  WS-TA-NAME              PIC X(200).
       01  WS-TA-ITEMS             PIC X(2000).

      *> -- Food data --
       01  WS-FOOD-DATA            PIC X(8000).
       01  WS-FOOD-LEN             PIC 9(5).
       01  WS-FOOD-ESC             PIC X(16000).
       01  WS-FOOD-ESC-LEN         PIC 9(5).

      *> === Control Flow ===
       01  WS-PTR                  PIC 9(5).
       01  WS-I                    PIC 9(5).
       01  WS-TALLY-CNT            PIC 9(4).
       01  WS-FLAG-FOUND           PIC X VALUE "N".
       01  WS-RETRY-CT             PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S04E05 FOODWAREHOUSE ==="

           PERFORM LOAD-ENV-VARS

           MOVE SPACES TO WS-DATA-URL
           STRING TRIM(WS-HUB-URL)
               "/dane/food4cities.json"
               DELIMITED SIZE
               INTO WS-DATA-URL
           END-STRING

      *>   Step 1: Fetch food data
           DISPLAY " "
           DISPLAY "[1] Fetch food data"
           PERFORM FETCH-FOOD-DATA

      *>   Step 2: Run agent loop
           DISPLAY " "
           DISPLAY "[2] Agent loop"
           PERFORM RUN-AGENT-LOOP

           DISPLAY " "
           IF WS-FLAG-FOUND = "Y"
               DISPLAY ">>> FLAG FOUND <<<"
           ELSE
               DISPLAY "No flag found."
           END-IF
           DISPLAY "=== DONE ==="
           STOP RUN.

      *> ============================================================
      *> FETCH-FOOD-DATA: Download food4cities.json
      *> ============================================================
       FETCH-FOOD-DATA.
           INITIALIZE WS-CMD
           STRING
               "curl -s "
               "-o food_data.json"
               " " TRIM(WS-DATA-URL)
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           MOVE "food_data.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           IF WS-JLEN > 0
               MOVE WS-JBUF(1:WS-JLEN)
                   TO WS-FOOD-DATA
               MOVE WS-JLEN TO WS-FOOD-LEN
               DISPLAY "  Food data: "
                   WS-JBUF(1:500)
           ELSE
               DISPLAY "  ERR: No food data!"
           END-IF

      *>   Escape food data for LLM prompt
           MOVE WS-FOOD-DATA(1:WS-FOOD-LEN)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR
           MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
               TO WS-FOOD-ESC
           MOVE WS-ESC-OLEN
               TO WS-FOOD-ESC-LEN
           .

      *> ============================================================
      *> RUN-AGENT-LOOP: LLM function calling loop
      *> ============================================================
       RUN-AGENT-LOOP.
           DISPLAY " "
           DISPLAY "--- Agent starting ---"

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
               "You are a food warehouse "
               "manager. Execute the "
               "foodwarehouse task step "
               "by step."
               WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "## Pipeline" WS-NL
               "1. Call reset to init "
               "state" WS-NL
               "2. Call help to get API "
               "docs" WS-NL
               "3. Query database: first "
               "'show tables', then "
               "'SELECT * FROM tablename "
               "LIMIT 30' for each table. "
               "If a table has 30 rows, "
               "paginate with OFFSET. "
               "Check ALL columns."
               WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "4. CRITICAL creator "
               "selection: Query users "
               "with role=2 AND non-"
               "empty login AND non-"
               "empty birthday. Check "
               "for is_active column - "
               "only use active users."
               WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "   Pick the FIRST valid "
               "user (lowest id) and "
               "reuse that SAME user as "
               "creatorID for ALL orders."
               " Never vary creators."
               WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "5. From destinations "
               "table, map each city "
               "in food data to its "
               "destination code."
               WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "6. For each city: "
               "generate signature "
               "(signatureGenerator with "
               "action=generate, login, "
               "birthday, destination), "
               "then create order "
               "(orders action=create "
               "with title, creatorID, "
               "destination, signature),"
               " then append items "
               "(orders action=append "
               "with id and items as "
               "object {name:qty,...})."
               WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "7. When all orders done,"
               " call done tool."
               WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "## Rules" WS-NL
               "- Database returns max "
               "30 rows. Use OFFSET "
               "for pagination."
               WS-NL
               "- Signature must be 40 "
               "hex chars. Verify before"
               " using." WS-NL
               "- Order ID is 32 hex "
               "chars returned in "
               "reply or order.id."
               WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "- Use batch append: "
               "items as object "
               "{name1:qty1,name2:qty2}"
               WS-NL
               "- Call tools one at a "
               "time. After each result,"
               " decide what next."
               WS-NL
               "- destination field in "
               "orders create and "
               "signatureGenerator is "
               "an integer."
               WS-NL
               "- If create order fails "
               "with error about creator"
               " (e.g. -652), try a "
               "different role=2 user. "
               "Do NOT reset. Just "
               "delete the bad order "
               "and recreate with a "
               "different creatorID."
               WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

      *>   Include food data in system prompt
           STRING
               "## Food requirements "
               "(food4cities.json):"
               WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           IF WS-FOOD-ESC-LEN > 0
               STRING
                   WS-FOOD-ESC(
                   1:WS-FOOD-ESC-LEN)
                   DELIMITED SIZE
                   INTO WS-CONV-BUF
                   WITH POINTER WS-CONV-PTR
               END-STRING
           END-IF

           STRING
               WS-NL WS-NL
               "Start now. Call reset "
               "first."
               WS-QT "},"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

      *>   User message
           STRING
               "{" WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               "Begin the foodwarehouse "
               "task. Call reset tool "
               "to start."
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
                   DISPLAY "  Empty resp!"
                   MOVE "Y" TO WS-AG-DONE
                   EXIT PERFORM
               END-IF

      *>       Check API error
               MOVE 0 TO WS-TALLY-CNT
               INSPECT WS-JBUF(1:WS-JLEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL '"error"'
               IF WS-TALLY-CNT > 0
                   DISPLAY "  API ERR: "
                       WS-JBUF(1:500)
                   EXIT PERFORM CYCLE
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
                   PERFORM
                       APPEND-TOOL-EXCHANGE

      *>           Check flag in result
                   IF TRIM(WS-TOOL-NAME)
                       = "call_done"
                       MOVE 0
                           TO WS-TALLY-CNT
                       IF WS-TOOL-RESULT-LEN
                           > 0
                           INSPECT
                             WS-TOOL-RESULT(
                             1:
                             WS-TOOL-RESULT-LEN
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
      *>                   No flag - nudge
                           SUBTRACT 1
                             FROM WS-CONV-PTR
                           STRING ","
                             "{"
                             WS-QT "role"
                             WS-QT ":"
                             WS-QT "user"
                             WS-QT ","
                             WS-QT "content"
                             WS-QT ":"
                             WS-QT
                             "Done returned no"
                             " flag. Read the "
                             "error. If creator"
                             " error (-652), "
                             "reset and redo "
                             "with a different "
                             "role=2 user. "
                             "Check orders "
                             "with get first."
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

      *>           Check flag in text
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
                       PERFORM
                         APPEND-TEXT-NUDGE
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

      *>   Tool 1: call_reset
           STRING
               "{" WS-QT "type" WS-QT ":"
               WS-QT "function" WS-QT ","
               WS-QT "function" WS-QT ":{"
               WS-QT "name" WS-QT ":"
               WS-QT "call_reset" WS-QT ","
               WS-QT "description" WS-QT ":"
               WS-QT "Reset warehouse "
               "state" WS-QT ","
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "parameters" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "object" WS-QT ","
               WS-QT "properties" WS-QT
               ":{},"
               WS-QT "required" WS-QT
               ":[]}}},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Tool 2: call_help
           STRING
               "{" WS-QT "type" WS-QT ":"
               WS-QT "function" WS-QT ","
               WS-QT "function" WS-QT ":{"
               WS-QT "name" WS-QT ":"
               WS-QT "call_help" WS-QT ","
               WS-QT "description" WS-QT ":"
               WS-QT "Get API docs" WS-QT ","
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "parameters" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "object" WS-QT ","
               WS-QT "properties" WS-QT
               ":{},"
               WS-QT "required" WS-QT
               ":[]}}},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Tool 3: call_database
           STRING
               "{" WS-QT "type" WS-QT ":"
               WS-QT "function" WS-QT ","
               WS-QT "function" WS-QT ":{"
               WS-QT "name" WS-QT ":"
               WS-QT "call_database" WS-QT ","
               WS-QT "description" WS-QT ":"
               WS-QT "Query SQLite DB. "
               "Max 30 rows returned. "
               "Use OFFSET to paginate."
               WS-QT ","
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "parameters" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "object" WS-QT ","
               WS-QT "properties" WS-QT ":{"
               WS-QT "query" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "string" WS-QT ","
               WS-QT "description" WS-QT ":"
               WS-QT "SQL query" WS-QT
               "}},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "required" WS-QT ":["
               WS-QT "query" WS-QT
               "]}}},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Tool 4: call_signature
           STRING
               "{" WS-QT "type" WS-QT ":"
               WS-QT "function" WS-QT ","
               WS-QT "function" WS-QT ":{"
               WS-QT "name" WS-QT ":"
               WS-QT "call_signature"
               WS-QT ","
               WS-QT "description" WS-QT ":"
               WS-QT "Generate SHA1 "
               "signature. Returns "
               "40-char hex."
               WS-QT ","
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "parameters" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "object" WS-QT ","
               WS-QT "properties" WS-QT ":{"
               WS-QT "login" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "string" WS-QT "},"
               WS-QT "birthday" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "string" WS-QT "},"
               WS-QT "destination" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "integer" WS-QT
               "}},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "required" WS-QT ":["
               WS-QT "login" WS-QT ","
               WS-QT "birthday" WS-QT ","
               WS-QT "destination" WS-QT
               "]}}},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Tool 5: call_orders
           STRING
               "{" WS-QT "type" WS-QT ":"
               WS-QT "function" WS-QT ","
               WS-QT "function" WS-QT ":{"
               WS-QT "name" WS-QT ":"
               WS-QT "call_orders" WS-QT ","
               WS-QT "description" WS-QT ":"
               WS-QT "Manage orders. "
               "Actions: get, create, "
               "append, delete."
               WS-QT ","
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "parameters" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "object" WS-QT ","
               WS-QT "properties" WS-QT ":{"
               WS-QT "action" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "string" WS-QT ","
               WS-QT "enum" WS-QT ":["
               WS-QT "get" WS-QT ","
               WS-QT "create" WS-QT ","
               WS-QT "append" WS-QT ","
               WS-QT "delete" WS-QT "]},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "title" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "string" WS-QT "},"
               WS-QT "creatorID" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "integer" WS-QT "},"
               WS-QT "destination" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "integer" WS-QT "},"
               WS-QT "signature" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "string" WS-QT "},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "id" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "string" WS-QT "},"
               WS-QT "name" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "string" WS-QT "},"
               WS-QT "items" WS-QT ":{"
               WS-QT "description" WS-QT ":"
               WS-QT "Item count (int) "
               "or object {name:qty}"
               WS-QT "}},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "required" WS-QT ":["
               WS-QT "action" WS-QT
               "]}}},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Tool 6: call_done
           STRING
               "{" WS-QT "type" WS-QT ":"
               WS-QT "function" WS-QT ","
               WS-QT "function" WS-QT ":{"
               WS-QT "name" WS-QT ":"
               WS-QT "call_done" WS-QT ","
               WS-QT "description" WS-QT ":"
               WS-QT "Final verification."
               " Call when all orders "
               "are ready."
               WS-QT ","
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "parameters" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "object" WS-QT ","
               WS-QT "properties" WS-QT
               ":{},"
               WS-QT "required" WS-QT
               ":[]}}}],"
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
           WHEN "call_reset"
               PERFORM TOOL-RESET
           WHEN "call_help"
               PERFORM TOOL-HELP
           WHEN "call_database"
               PERFORM TOOL-DATABASE
           WHEN "call_signature"
               PERFORM TOOL-SIGNATURE
           WHEN "call_orders"
               PERFORM TOOL-ORDERS
           WHEN "call_done"
               PERFORM TOOL-DONE
           WHEN OTHER
               MOVE
                   '{"error":"Unknown tool"}'
                   TO WS-TOOL-RESULT
               MOVE 24
                   TO WS-TOOL-RESULT-LEN
           END-EVALUATE
           .

      *> ============================================================
      *> TOOL-RESET
      *> ============================================================
       TOOL-RESET.
           MOVE SPACES TO WS-HUB-BODY
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY)
               WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME)
               WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "tool" WS-QT ":"
               WS-QT "reset" WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
           END-STRING

           PERFORM SEND-HUB-REQUEST
           PERFORM STORE-TOOL-RESULT
           .

      *> ============================================================
      *> TOOL-HELP
      *> ============================================================
       TOOL-HELP.
           MOVE SPACES TO WS-HUB-BODY
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY)
               WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME)
               WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "tool" WS-QT ":"
               WS-QT "help" WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
           END-STRING

           PERFORM SEND-HUB-REQUEST
           PERFORM STORE-TOOL-RESULT
           .

      *> ============================================================
      *> TOOL-DATABASE
      *> ============================================================
       TOOL-DATABASE.
      *>   Parse query from args
           MOVE WS-TOOL-ARGS TO WS-JBUF
           MOVE LENGTH(TRIM(WS-TOOL-ARGS))
               TO WS-JLEN

           MOVE "query" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL)
               TO WS-TA-QUERY

      *>   Escape query for JSON
           MOVE TRIM(WS-TA-QUERY)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

      *>   Build hub request
           MOVE SPACES TO WS-HUB-BODY
           MOVE 1 TO WS-PTR
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY)
               WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME)
               WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "tool" WS-QT ":"
               WS-QT "database" WS-QT ","
               WS-QT "query" WS-QT ":"
               WS-QT
               WS-ESC-OUT(1:WS-ESC-OLEN)
               WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           PERFORM SEND-HUB-REQUEST
           PERFORM STORE-TOOL-RESULT
           .

      *> ============================================================
      *> TOOL-SIGNATURE
      *> ============================================================
       TOOL-SIGNATURE.
      *>   Parse args
           MOVE WS-TOOL-ARGS TO WS-JBUF
           MOVE LENGTH(TRIM(WS-TOOL-ARGS))
               TO WS-JLEN

           MOVE "login" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL)
               TO WS-TA-LOGIN

           MOVE "birthday" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL)
               TO WS-TA-BIRTHDAY

           MOVE "destination"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL)
               TO WS-TA-DEST

      *>   Build hub request
           MOVE SPACES TO WS-HUB-BODY
           MOVE 1 TO WS-PTR
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY)
               WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME)
               WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "tool" WS-QT ":"
               WS-QT "signatureGenerator"
               WS-QT ","
               WS-QT "action" WS-QT ":"
               WS-QT "generate" WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

      *>   Escape login
           MOVE TRIM(WS-TA-LOGIN)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

           STRING
               WS-QT "login" WS-QT ":"
               WS-QT
               WS-ESC-OUT(1:WS-ESC-OLEN)
               WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

      *>   Escape birthday
           MOVE TRIM(WS-TA-BIRTHDAY)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

           STRING
               WS-QT "birthday" WS-QT ":"
               WS-QT
               WS-ESC-OUT(1:WS-ESC-OLEN)
               WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

      *>   Destination as number
           STRING
               WS-QT "destination" WS-QT ":"
               TRIM(WS-TA-DEST)
               "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           PERFORM SEND-HUB-REQUEST
           PERFORM STORE-TOOL-RESULT
           .

      *> ============================================================
      *> TOOL-ORDERS
      *> ============================================================
       TOOL-ORDERS.
      *>   Parse action from args
           MOVE WS-TOOL-ARGS TO WS-JBUF
           MOVE LENGTH(TRIM(WS-TOOL-ARGS))
               TO WS-JLEN

           MOVE "action" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL)
               TO WS-TA-ACTION

           DISPLAY "    Orders action: "
               TRIM(WS-TA-ACTION)

           EVALUATE TRIM(WS-TA-ACTION)
           WHEN "get"
               PERFORM TOOL-ORDERS-GET
           WHEN "create"
               PERFORM TOOL-ORDERS-CREATE
           WHEN "append"
               PERFORM TOOL-ORDERS-APPEND
           WHEN "delete"
               PERFORM TOOL-ORDERS-DELETE
           WHEN OTHER
               MOVE '{"error":"Bad action"}'
                   TO WS-TOOL-RESULT
               MOVE 22
                   TO WS-TOOL-RESULT-LEN
           END-EVALUATE
           .

      *> ============================================================
      *> TOOL-ORDERS-GET
      *> ============================================================
       TOOL-ORDERS-GET.
           MOVE SPACES TO WS-HUB-BODY
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY)
               WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME)
               WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "tool" WS-QT ":"
               WS-QT "orders" WS-QT ","
               WS-QT "action" WS-QT ":"
               WS-QT "get" WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
           END-STRING

           PERFORM SEND-HUB-REQUEST
           PERFORM STORE-TOOL-RESULT
           .

      *> ============================================================
      *> TOOL-ORDERS-CREATE
      *> ============================================================
       TOOL-ORDERS-CREATE.
      *>   Parse remaining fields
           MOVE "title" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE WS-JVAL TO WS-TA-TITLE

           MOVE "creatorID"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL)
               TO WS-TA-CREATOR

           MOVE "destination"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL)
               TO WS-TA-DEST

           MOVE "signature"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL)
               TO WS-TA-SIGNATURE

      *>   Build hub request
           MOVE SPACES TO WS-HUB-BODY
           MOVE 1 TO WS-PTR
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY)
               WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME)
               WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "tool" WS-QT ":"
               WS-QT "orders" WS-QT ","
               WS-QT "action" WS-QT ":"
               WS-QT "create" WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

      *>   Escape title
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

      *>   creatorID as number
           STRING
               WS-QT "creatorID" WS-QT ":"
               TRIM(WS-TA-CREATOR) ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

      *>   destination as number
           STRING
               WS-QT "destination" WS-QT ":"
               TRIM(WS-TA-DEST) ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

      *>   signature as string
           STRING
               WS-QT "signature" WS-QT ":"
               WS-QT TRIM(WS-TA-SIGNATURE)
               WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           PERFORM SEND-HUB-REQUEST
           PERFORM STORE-TOOL-RESULT
           .

      *> ============================================================
      *> TOOL-ORDERS-APPEND
      *> ============================================================
       TOOL-ORDERS-APPEND.
      *>   Parse id and items from args
           MOVE "id" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL)
               TO WS-TA-ID

      *>   For append, we pass the raw args
      *>   through as the answer JSON, but we
      *>   need to build the hub body properly.
      *>   Extract items sub-object from args.
           PERFORM EXTRACT-ITEMS-JSON

      *>   Build hub request
           MOVE SPACES TO WS-HUB-BODY
           MOVE 1 TO WS-PTR
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY)
               WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME)
               WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "tool" WS-QT ":"
               WS-QT "orders" WS-QT ","
               WS-QT "action" WS-QT ":"
               WS-QT "append" WS-QT ","
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

      *>   Append items JSON
           STRING
               WS-QT "items" WS-QT ":"
               TRIM(WS-TA-ITEMS)
               "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           PERFORM SEND-HUB-REQUEST
           PERFORM STORE-TOOL-RESULT
           .

      *> ============================================================
      *> TOOL-ORDERS-DELETE
      *> ============================================================
       TOOL-ORDERS-DELETE.
      *>   Parse id
           MOVE "id" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL)
               TO WS-TA-ID

           MOVE SPACES TO WS-HUB-BODY
           MOVE 1 TO WS-PTR
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY)
               WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME)
               WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "tool" WS-QT ":"
               WS-QT "orders" WS-QT ","
               WS-QT "action" WS-QT ":"
               WS-QT "delete" WS-QT ","
               WS-QT "id" WS-QT ":"
               WS-QT TRIM(WS-TA-ID)
               WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           PERFORM SEND-HUB-REQUEST
           PERFORM STORE-TOOL-RESULT
           .

      *> ============================================================
      *> TOOL-DONE
      *> ============================================================
       TOOL-DONE.
           MOVE SPACES TO WS-HUB-BODY
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY)
               WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME)
               WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "tool" WS-QT ":"
               WS-QT "done" WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
           END-STRING

           PERFORM SEND-HUB-REQUEST
           PERFORM STORE-TOOL-RESULT

           DISPLAY "  Done result: "
               WS-TOOL-RESULT(1:500)
           .

      *> ============================================================
      *> EXTRACT-ITEMS-JSON: Get items from args
      *> Handles both object and integer items
      *> ============================================================
       EXTRACT-ITEMS-JSON.
           MOVE SPACES TO WS-TA-ITEMS

      *>   Find "items" key in WS-TOOL-ARGS
           MOVE SPACES TO WS-TMP2
           STRING WS-QT "items" WS-QT
               DELIMITED SIZE INTO WS-TMP2
           END-STRING

           MOVE 0 TO WS-KEY-POS
           MOVE LENGTH(TRIM(WS-TMP2))
               TO WS-K
           PERFORM VARYING WS-FJV-POS
               FROM 1 BY 1
               UNTIL WS-FJV-POS >
                   LENGTH(
                   TRIM(WS-TOOL-ARGS))
               OR WS-KEY-POS > 0
               IF WS-TOOL-ARGS(
                   WS-FJV-POS:WS-K)
                   = TRIM(WS-TMP2)
                   MOVE WS-FJV-POS
                       TO WS-KEY-POS
               END-IF
           END-PERFORM

           IF WS-KEY-POS = 0
               MOVE "{}" TO WS-TA-ITEMS
               EXIT PARAGRAPH
           END-IF

      *>   Skip to colon then value
           COMPUTE WS-FJV-POS =
               WS-KEY-POS + WS-K
           PERFORM UNTIL
               WS-FJV-POS >
               LENGTH(TRIM(WS-TOOL-ARGS))
               OR WS-TOOL-ARGS(
               WS-FJV-POS:1) = ":"
               ADD 1 TO WS-FJV-POS
           END-PERFORM
           ADD 1 TO WS-FJV-POS

      *>   Skip whitespace
           PERFORM UNTIL
               WS-FJV-POS >
               LENGTH(TRIM(WS-TOOL-ARGS))
               OR WS-TOOL-ARGS(
               WS-FJV-POS:1) NOT = " "
               ADD 1 TO WS-FJV-POS
           END-PERFORM

      *>   Check if object or number
           IF WS-TOOL-ARGS(
               WS-FJV-POS:1) = "{"
      *>       Object - find matching }
               MOVE WS-FJV-POS
                   TO WS-VAL-START
               MOVE 1 TO WS-I
               ADD 1 TO WS-FJV-POS
               PERFORM UNTIL
                   WS-FJV-POS >
                   LENGTH(
                   TRIM(WS-TOOL-ARGS))
                   OR WS-I = 0
                   IF WS-TOOL-ARGS(
                       WS-FJV-POS:1) = "{"
                       ADD 1 TO WS-I
                   END-IF
                   IF WS-TOOL-ARGS(
                       WS-FJV-POS:1) = "}"
                       SUBTRACT 1 FROM WS-I
                   END-IF
                   IF WS-I > 0
                       ADD 1 TO WS-FJV-POS
                   END-IF
               END-PERFORM
               COMPUTE WS-VAL-END =
                   WS-FJV-POS
               COMPUTE WS-K =
                   WS-VAL-END -
                   WS-VAL-START + 1
               IF WS-K > 0 AND WS-K < 2000
                   MOVE WS-TOOL-ARGS(
                       WS-VAL-START:WS-K)
                       TO WS-TA-ITEMS
               END-IF
           ELSE
      *>       Single integer - extract it
               MOVE WS-FJV-POS
                   TO WS-VAL-START
               PERFORM UNTIL
                   WS-FJV-POS >
                   LENGTH(
                   TRIM(WS-TOOL-ARGS))
                   OR WS-TOOL-ARGS(
                   WS-FJV-POS:1) = ","
                   OR WS-TOOL-ARGS(
                   WS-FJV-POS:1) = "}"
                   OR WS-TOOL-ARGS(
                   WS-FJV-POS:1) = " "
                   ADD 1 TO WS-FJV-POS
               END-PERFORM
               COMPUTE WS-VAL-END =
                   WS-FJV-POS - 1
               IF WS-VAL-END
                   >= WS-VAL-START
                   COMPUTE WS-K =
                       WS-VAL-END -
                       WS-VAL-START + 1
                   MOVE WS-TOOL-ARGS(
                       WS-VAL-START:WS-K)
                       TO WS-TA-ITEMS
               END-IF
           END-IF

           DISPLAY "    Items: "
               TRIM(WS-TA-ITEMS)(1:200)
           .

      *> ============================================================
      *> SEND-HUB-REQUEST: Write body + curl POST
      *> 503 retry
      *> ============================================================
       SEND-HUB-REQUEST.
           MOVE 0 TO WS-RETRY-CT

           PERFORM UNTIL WS-RETRY-CT > 7

               MOVE "hub_req.tmp"
                   TO WS-WORK-PATH
               OPEN OUTPUT WORK-FILE
               IF WS-FS NOT = "00"
                   DISPLAY "ERR: OPEN "
                       TRIM(WS-WORK-PATH)
                       " FS=" WS-FS
                   STOP RUN
               END-IF
               WRITE WORK-REC
                   FROM WS-HUB-BODY
               CLOSE WORK-FILE

               INITIALIZE WS-CMD
               STRING
                   "curl -s "
                   "-o hub_resp.json"
                   " -w " WS-QT
                   "%{http_code}"
                   WS-QT
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
               MOVE "work.tmp"
                   TO WS-WORK-PATH

      *>       Check for 503
               MOVE 0 TO WS-TALLY-CNT
               IF WS-JLEN > 0
                   INSPECT
                       WS-JBUF(1:WS-JLEN)
                       TALLYING
                       WS-TALLY-CNT
                       FOR ALL "503"
               END-IF
               IF WS-TALLY-CNT > 0
                   ADD 1 TO WS-RETRY-CT
                   DISPLAY "    503, retry "
                       WS-RETRY-CT
                   EXIT PERFORM CYCLE
               END-IF

      *>       Success - exit retry loop
               EXIT PERFORM
           END-PERFORM

           DISPLAY "    Hub: "
               WS-JBUF(1:500)
           .

      *> ============================================================
      *> STORE-TOOL-RESULT: Copy JBUF to result
      *> ============================================================
       STORE-TOOL-RESULT.
           COMPUTE WS-TOOL-RESULT-LEN =
               LENGTH(TRIM(WS-JBUF))
           IF WS-TOOL-RESULT-LEN > 8000
               MOVE 8000
                   TO WS-TOOL-RESULT-LEN
           END-IF
           IF WS-TOOL-RESULT-LEN > 0
               MOVE WS-JBUF(
                   1:WS-TOOL-RESULT-LEN)
                   TO WS-TOOL-RESULT
           END-IF
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
               WS-QT "Use the tools. Call"
               " the next tool now."
               WS-QT "}]"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING
           .

       COPY TOOLPARSE-PROC.
       COPY JSONESCAPE-PROC.
       COPY JSONUNESCAPE-PROC.
       COPY JSONREAD-PROC.
       COPY JSONPARSE-PROC.

       COPY ENVLOAD-PROC.
