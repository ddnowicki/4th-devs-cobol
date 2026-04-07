       IDENTIFICATION DIVISION.
       PROGRAM-ID. S01E04-SENDIT.
      *> ============================================================
      *> S01E04 - SPK Transport Declaration (Pure COBOL)
      *> 1. Fetch SPK docs via curl
      *> 2. Read & JSON-escape docs in COBOL
      *> 3. Build LLM request JSON in COBOL
      *> 4. Call OpenAI API via curl
      *> 5. Submit to Hub /verify with retry
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
       01  WORK-REC                PIC X(16000).

       WORKING-STORAGE SECTION.
      *> === Environment (via copybook) ===
       COPY ENVLOAD-WS.

      *> === Constants ===
       01  WS-MAX-RETRIES          PIC 9(1) VALUE 5.

      *> === Shared WS (via copybooks) ===
       COPY JSONPARSE-WS.
       COPY JSONREAD-WS.

      *> === File I/O ===
       01  WS-FS                   PIC XX.
       01  WS-WORK-PATH            PIC X(100)
                                   VALUE "work.tmp".

      *> === HTTP ===
       01  WS-CMD                  PIC X(4000).

      *> === Task Configuration ===
       01  WS-CURR-DATE            PIC X(21).
       01  WS-DATE-STR             PIC X(10).

      *> === Task Data ===
       01  WS-REQ-JSON             PIC X(16000).
       01  WS-DECL                 PIC X(2000).
       01  WS-PAYLOAD              PIC X(4000).
       01  WS-DOC-BUF              PIC X(4000).
       01  WS-DOC-LEN              PIC 9(5).
       01  WS-VIS-BUF              PIC X(2000).
       01  WS-VIS-LEN              PIC 9(5).

      *> === Control Flow ===
       01  WS-PTR                  PIC 9(5).
       01  WS-I                    PIC 9(5).
       01  WS-ATTEMPT              PIC 9(1).
       01  WS-SUCCESS              PIC X VALUE "N".
       01  WS-ERROR-MSG            PIC X(500).
       01  WS-TALLY-CNT            PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S01E04 SENDIT - COBOL ==="

           PERFORM LOAD-ENV-VARS

           PERFORM GET-CURRENT-DATE
           PERFORM FETCH-DOCS
           PERFORM CALL-VISION-API

           MOVE SPACES TO WS-ERROR-MSG
           MOVE "N" TO WS-SUCCESS

           PERFORM VARYING WS-ATTEMPT
               FROM 1 BY 1
               UNTIL WS-ATTEMPT > WS-MAX-RETRIES
               OR WS-SUCCESS = "Y"
               DISPLAY " "
               DISPLAY "--- Proba "
                   WS-ATTEMPT "/5 ---"
               PERFORM BUILD-LLM-REQUEST
               PERFORM CALL-OPENAI
               PERFORM EXTRACT-DECLARATION
               IF TRIM(WS-DECL) = SPACES
                   DISPLAY "  Brak dekl!"
                   EXIT PERFORM
               END-IF
               PERFORM SUBMIT-DECLARATION
               PERFORM CHECK-RESPONSE
           END-PERFORM

           IF WS-SUCCESS NOT = "Y"
               DISPLAY " "
               DISPLAY "  NIEPOWODZENIE."
           END-IF
           DISPLAY " "
           DISPLAY "=== ZAKONCZONO ==="
           STOP RUN.

      *> ============================================================
       GET-CURRENT-DATE.
           MOVE CURRENT-DATE
               TO WS-CURR-DATE
           INITIALIZE WS-DATE-STR
           STRING WS-CURR-DATE(1:4)
               "-"
               WS-CURR-DATE(5:2)
               "-"
               WS-CURR-DATE(7:2)
               DELIMITED SIZE
               INTO WS-DATE-STR
           END-STRING
           DISPLAY "  Data: "
               TRIM(WS-DATE-STR)
           .

      *> ============================================================
      *> FETCH-DOCS: Download doc files via curl
      *> ============================================================
       FETCH-DOCS.
           DISPLAY "  Fetch docs..."

           INITIALIZE WS-CMD
           STRING
               "curl -s -o "
               "doc_wagons.tmp "
               TRIM(WS-HUB-URL)
               "/dane/doc/"
               "dodatkowe-wagony.md"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           INITIALIZE WS-CMD
           STRING
               "curl -s -o "
               "doc_template.tmp "
               TRIM(WS-HUB-URL)
               "/dane/doc/"
               "zalacznik-E.md"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           INITIALIZE WS-CMD
           STRING
               "curl -s -o "
               "doc_glossary.tmp "
               TRIM(WS-HUB-URL)
               "/dane/doc/"
               "zalacznik-G.md"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           INITIALIZE WS-CMD
           STRING
               "curl -s -o "
               "doc_network.tmp "
               TRIM(WS-HUB-URL)
               "/dane/doc/"
               "zalacznik-F.md"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           DISPLAY "  Docs fetched."
           .

      *> ============================================================
      *> CALL-VISION-API: Extract routes from image
      *> ============================================================
       CALL-VISION-API.
           DISPLAY "  Vision API..."
           MOVE SPACES TO WS-REQ-JSON
           MOVE 1 TO WS-PTR

           STRING
               "{"
               WS-QT "model" WS-QT ":"
               WS-QT "gpt-4.1-mini"
               WS-QT ","
               WS-QT "messages" WS-QT
               ":["
               "{"
               WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT
               ":["
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "{"
               WS-QT "type" WS-QT ":"
               WS-QT "text" WS-QT ","
               WS-QT "text" WS-QT ":"
               WS-QT
               "Extract the COMPLETE"
               " table. Return ONLY"
               " plain text, one "
               "route per line: "
               "Kod trasy | Przebieg"
               " | Powod | Data | "
               "Prognoza"
               WS-QT "},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "{"
               WS-QT "type" WS-QT ":"
               WS-QT "image_url"
               WS-QT ","
               WS-QT "image_url"
               WS-QT ":{"
               WS-QT "url" WS-QT ":"
               WS-QT
               TRIM(WS-HUB-URL)
               "/dane/doc/"
               "trasy-wylaczone.png"
               WS-QT "}}]}],"
               WS-QT "temperature"
               WS-QT ":0}"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Write + curl
           MOVE "vision_req.json"
               TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "ERR: OPEN "
                   TRIM(WS-WORK-PATH)
                   " FS=" WS-FS
               STOP RUN
           END-IF
           WRITE WORK-REC
               FROM WS-REQ-JSON
           CLOSE WORK-FILE

           INITIALIZE WS-CMD
           STRING
               "curl -s "
               "-o vision_resp.json"
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
               " -d @vision_req.json"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Parse vision response
           MOVE "vision_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE

           MOVE "content"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL

      *>   Store (already JSON-escaped)
           MOVE WS-JVAL TO WS-VIS-BUF
           MOVE LENGTH(TRIM(WS-JVAL))
               TO WS-VIS-LEN
           DISPLAY "  Routes: "
               WS-VIS-LEN " chars"

           MOVE "work.tmp"
               TO WS-WORK-PATH
           .

      *> ============================================================
      *> READ-AND-ESCAPE: Read file, escape for JSON
      *> Input:  WS-WORK-PATH = file path
      *> Output: WS-DOC-BUF, WS-DOC-LEN
      *> ============================================================
       READ-AND-ESCAPE.
           MOVE SPACES TO WS-DOC-BUF
           MOVE 0 TO WS-DOC-LEN
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
      *>               Add \n between lines
                       IF WS-DOC-LEN > 0
                           ADD 1
                             TO WS-DOC-LEN
                           MOVE X"5C"
                             TO WS-DOC-BUF(
                             WS-DOC-LEN:1)
                           ADD 1
                             TO WS-DOC-LEN
                           MOVE "n"
                             TO WS-DOC-BUF(
                             WS-DOC-LEN:1)
                       END-IF
      *>               Escape each char
                       PERFORM VARYING
                           WS-I FROM 1
                           BY 1
                           UNTIL WS-I
                           > WS-K
                           EVALUATE TRUE
                           WHEN
                           WS-LINE(
                           WS-I:1)
                           = WS-QT
                             ADD 1
                             TO WS-DOC-LEN
                             MOVE X"5C"
                             TO WS-DOC-BUF(
                             WS-DOC-LEN:1)
                             ADD 1
                             TO WS-DOC-LEN
                             MOVE WS-QT
                             TO WS-DOC-BUF(
                             WS-DOC-LEN:1)
                           WHEN
                           WS-LINE(
                           WS-I:1)
                           = X"5C"
                             ADD 1
                             TO WS-DOC-LEN
                             MOVE X"5C"
                             TO WS-DOC-BUF(
                             WS-DOC-LEN:1)
                             ADD 1
                             TO WS-DOC-LEN
                             MOVE X"5C"
                             TO WS-DOC-BUF(
                             WS-DOC-LEN:1)
                           WHEN OTHER
                             ADD 1
                             TO WS-DOC-LEN
                             MOVE WS-LINE(
                             WS-I:1)
                             TO WS-DOC-BUF(
                             WS-DOC-LEN:1)
                           END-EVALUATE
                       END-PERFORM
               END-READ
           END-PERFORM

           CLOSE WORK-FILE
           MOVE "N" TO WS-EOF
           .

      *> ============================================================
      *> BUILD-LLM-REQUEST: Assemble JSON from docs
      *> ============================================================
       BUILD-LLM-REQUEST.
           DISPLAY "  Build request..."
           MOVE SPACES TO WS-REQ-JSON
           MOVE 1 TO WS-PTR

      *>   JSON opening + system prompt
           STRING
               "{"
               WS-QT "model" WS-QT ":"
               WS-QT "gpt-4.1-mini"
               WS-QT ","
               WS-QT "messages" WS-QT
               ":["
               "{"
               WS-QT "role" WS-QT ":"
               WS-QT "system" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   System prompt (no special chars)
           STRING
               "You fill SPK transport"
               " declarations. Output"
               " ONLY the filled "
               "declaration text. "
               "No explanations, no "
               "markdown, no extra "
               "text. Match template "
               "EXACTLY."
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               " Rules: Cat A "
               "(Strategic) includes"
               " reactor fuel. "
               "Cat A and B: exempt "
               "from ALL fees "
               "(KWOTA = 0 PP). "
               "Cat A and B CAN use "
               "blocked routes (X-xx"
               " codes). "
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               "UWAGI SPECJALNE must"
               " be: brak. "
               "Separators: exactly "
               "54 dashes or 54 "
               "equals signs. "
               "Use provided docs to"
               " determine route, "
               "WDP, and all values."
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Close system, open user
           STRING
               WS-QT "},{"
               WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   === TEMPLATE ===
           STRING
               "=== DECLARATION "
               "TEMPLATE ===" WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           MOVE "doc_template.tmp"
               TO WS-WORK-PATH
           PERFORM READ-AND-ESCAPE
           STRING
               WS-DOC-BUF(
               1:WS-DOC-LEN)
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   === WAGONS ===
           STRING
               WS-NL WS-NL
               "=== ADDITIONAL "
               "WAGONS INFO ===" WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           MOVE "doc_wagons.tmp"
               TO WS-WORK-PATH
           PERFORM READ-AND-ESCAPE
           STRING
               WS-DOC-BUF(
               1:WS-DOC-LEN)
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   === GLOSSARY ===
           STRING
               WS-NL WS-NL
               "=== GLOSSARY ===" WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           MOVE "doc_glossary.tmp"
               TO WS-WORK-PATH
           PERFORM READ-AND-ESCAPE
           STRING
               WS-DOC-BUF(
               1:WS-DOC-LEN)
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   === NETWORK MAP ===
           STRING
               WS-NL WS-NL
               "=== NETWORK MAP ==="
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           MOVE "doc_network.tmp"
               TO WS-WORK-PATH
           PERFORM READ-AND-ESCAPE
           STRING
               WS-DOC-BUF(
               1:WS-DOC-LEN)
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   === BLOCKED ROUTES ===
           STRING
               WS-NL WS-NL
               "=== BLOCKED ROUTES"
               " (from image) ===" WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           IF WS-VIS-LEN > 0
               STRING
                   WS-VIS-BUF(
                   1:WS-VIS-LEN)
                   DELIMITED SIZE
                   INTO WS-REQ-JSON
                   WITH POINTER WS-PTR
               END-STRING
           END-IF

      *>   === TASK DATA ===
           STRING
               WS-NL WS-NL
               "=== SHIPMENT DATA ==="
               WS-NL
               "DATA: "
               TRIM(WS-DATE-STR)
               WS-NL
               "NADAWCA: 450202122"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               "PUNKT NADAWCZY: "
               "Gdańsk"
               WS-NL
               "PUNKT DOCELOWY: "
               "Żarnowiec"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               "Zawartość: kasety z "
               "paliwem do reaktora"
               WS-NL
               "Masa: 2800 kg"
               WS-NL
               "Budżet: 0 PP"
               WS-NL
               "Uwagi specjalne: brak"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Error feedback if retry
           IF TRIM(WS-ERROR-MSG)
               NOT = SPACES
               STRING
                   WS-NL WS-NL
                   "PREVIOUS ERROR: "
                   TRIM(WS-ERROR-MSG)
                   WS-NL
                   "Fix the declaration."
                   DELIMITED SIZE
                   INTO WS-REQ-JSON
                   WITH POINTER WS-PTR
               END-STRING
           END-IF

      *>   Close JSON
           STRING
               WS-QT "}],"
               WS-QT "temperature"
               WS-QT ":0}"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Write request to file
           MOVE "work.tmp"
               TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "ERR: OPEN "
                   TRIM(WS-WORK-PATH)
                   " FS=" WS-FS
               STOP RUN
           END-IF
           WRITE WORK-REC
               FROM WS-REQ-JSON
           CLOSE WORK-FILE
           .

      *> ============================================================
      *> CALL-OPENAI: POST work.tmp to OpenAI
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
      *> EXTRACT-DECLARATION
      *> ============================================================
       EXTRACT-DECLARATION.
           MOVE SPACES TO WS-DECL

           MOVE "llm_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp"
               TO WS-WORK-PATH

           IF WS-JLEN = 0
               DISPLAY "  Empty response!"
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-JBUF(1:WS-JLEN)
               TALLYING WS-TALLY-CNT
               FOR ALL '"error"'
           IF WS-TALLY-CNT > 0
               DISPLAY "  API ERR: "
                   TRIM(WS-JBUF)(1:300)
               EXIT PARAGRAPH
           END-IF

           MOVE "content"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL

           IF TRIM(WS-JVAL) = SPACES
           OR TRIM(WS-JVAL) = "null"
               DISPLAY "  No content!"
               EXIT PARAGRAPH
           END-IF

           MOVE WS-JVAL TO WS-DECL
           DISPLAY "  Decl ("
               LENGTH(TRIM(WS-DECL))
               " chars)"
           DISPLAY "  Preview: "
               WS-DECL(1:200)
           .

      *> ============================================================
      *> SUBMIT-DECLARATION
      *> ============================================================
       SUBMIT-DECLARATION.
           DISPLAY "  Submit to Hub..."

           MOVE SPACES TO WS-PAYLOAD
           STRING
               "{" WS-QT "apikey"
               WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY)
               WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT "sendit" WS-QT ","
               WS-QT "answer" WS-QT
               ":{"
               WS-QT "declaration"
               WS-QT ":"
               WS-QT TRIM(WS-DECL)
               WS-QT "}}"
               DELIMITED SIZE
               INTO WS-PAYLOAD
           END-STRING

           OPEN OUTPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "ERR: OPEN "
                   TRIM(WS-WORK-PATH)
                   " FS=" WS-FS
               STOP RUN
           END-IF
           WRITE WORK-REC
               FROM WS-PAYLOAD
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
               " -d @work.tmp"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD
           .

      *> ============================================================
      *> CHECK-RESPONSE
      *> ============================================================
       CHECK-RESPONSE.
           MOVE "hub_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp"
               TO WS-WORK-PATH

           DISPLAY "  Hub: "
               TRIM(WS-JBUF)(1:500)

           MOVE 0 TO WS-TALLY-CNT
           IF WS-JLEN > 0
               INSPECT
                   WS-JBUF(1:WS-JLEN)
                   TALLYING
                   WS-TALLY-CNT
                   FOR ALL "FLG"
           END-IF
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-SUCCESS
               DISPLAY " "
               DISPLAY "  >>> SUKCES!"
               EXIT PARAGRAPH
           END-IF

           MOVE "code"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF TRIM(WS-JVAL) = "0"
               MOVE "Y" TO WS-SUCCESS
               DISPLAY "  >>> SUKCES!"
               EXIT PARAGRAPH
           END-IF

           MOVE "message"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL)
               TO WS-ERROR-MSG
           DISPLAY "  Blad - retry..."
           .

       COPY ENVLOAD-PROC.
       COPY JSONPARSE-PROC.
       COPY JSONREAD-PROC.
