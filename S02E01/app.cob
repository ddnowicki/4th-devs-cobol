       IDENTIFICATION DIVISION.
       PROGRAM-ID. S02E01-CATEGORIZE.
      *> ============================================================
      *> S02E01 - Item Categorization (Pure COBOL)
      *> 1. Fetch CSV (code,description) from Hub
      *> 2. Reset budget via POST to /verify
      *> 3. For each item, send classification prompt to /verify
      *> 4. Check for flag in response
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
       01  WORK-REC                PIC X(8000).

       WORKING-STORAGE SECTION.
      *> -- Config --
       01  WS-HUB-KEY              PIC X(50).
       01  WS-QT                   PIC X(1) VALUE '"'.
       01  WS-FS                   PIC XX.
       01  WS-WORK-PATH            PIC X(100) VALUE "work.tmp".

      *> -- URLs --
       01  WS-HUB-URL              PIC X(100).
       01  WS-VERIFY-URL           PIC X(200).
       01  WS-DATA-URL             PIC X(200).
       01  WS-TASK-NAME            PIC X(20) VALUE "categorize".

      *> -- System command --
       01  WS-CMD                  PIC X(4000).

      *> -- Prompt template --
       01  WS-PROMPT-TPL           PIC X(200) VALUE
           "Reply only DNG or NEU."
           & "DNG=weapon,explosive,drug,bomb,firearm,"
           & "handgun,rifle,ammunition."
           & "Reactor,fuel cassette=NEU."
           & "Else NEU.".

      *> -- CSV parsing --
       01  WS-CSV-PATH             PIC X(100) VALUE
           "categorize.csv".
       01  WS-CSV-LINE             PIC X(2000).
       01  WS-CSV-HEADER           PIC X(1) VALUE "Y".
       01  WS-FIELD-PTR            PIC 9(4).

      *> -- CSV fields --
       01  WS-FLD-CODE             PIC X(20).
       01  WS-FLD-DESC             PIC X(500).

      *> -- Items table (max 50) --
       01  WS-MAX-ITEMS            PIC 9(3) VALUE 50.
       01  WS-ITEM-COUNT           PIC 9(3) VALUE 0.
       01  WS-ITEMS-TABLE.
           05  WS-ITEM OCCURS 50 TIMES.
               10  WS-I-CODE       PIC X(20).
               10  WS-I-DESC       PIC X(500).

      *> -- Request/Response --
       01  WS-PAYLOAD              PIC X(4000).
       01  WS-RESP-BUF             PIC X(8000).
       01  WS-RESP-LEN             PIC 9(5) VALUE 0.

      *> -- Counters --
       01  WS-IDX                  PIC 9(3).
       01  WS-TOTAL-CSV            PIC 9(5) VALUE 0.
       01  WS-TALLY-CNT            PIC 9(4) VALUE 0.

      *> -- File reading --
       01  WS-EOF                  PIC X VALUE "N".
       01  WS-LINE                 PIC X(4000).
       01  WS-K                    PIC 9(5).

      *> -- Flag --
       01  WS-FLAG-FOUND           PIC X VALUE "N".

      *> -- Prompt building --
       01  WS-PROMPT               PIC X(1000).

      *> -- Retry --
       01  WS-MAX-RETRIES          PIC 9(2) VALUE 5.
       01  WS-ATTEMPT              PIC 9(2).
       01  WS-RETRY-DONE           PIC X VALUE "N".
       01  WS-SLEEP-SECS           PIC 9(3).
       01  WS-STATUS-CODE          PIC X(3).

      *> -- JSON parsing (for output/code fields) --
       01  WS-KEY-SEARCH           PIC X(50).
       01  WS-KEY-POS              PIC 9(5).
       01  WS-VAL-START            PIC 9(5).
       01  WS-VAL-END              PIC 9(5).
       01  WS-FJV-POS              PIC 9(5).
       01  WS-JVAL                 PIC X(500).
       01  WS-TMP                  PIC X(500).
       01  WS-JPOS                 PIC 9(5).

      *> -- Code value --
       01  WS-CODE-VAL             PIC X(20).
       01  WS-CODE-NUM             PIC S9(4).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S02E01 CATEGORIZE - COBOL ==="

           ACCEPT WS-HUB-KEY FROM ENVIRONMENT "HUB_API_KEY"
           ACCEPT WS-HUB-URL FROM ENVIRONMENT "HUB_API_URL"

           IF WS-HUB-KEY = SPACES
               DISPLAY "BLAD: Ustaw HUB_API_KEY!"
               STOP RUN
           END-IF
           IF WS-HUB-URL = SPACES
               DISPLAY "BLAD: Ustaw HUB_API_URL!"
               STOP RUN
           END-IF

           INITIALIZE WS-VERIFY-URL
           STRING
               TRIM(WS-HUB-URL) "/verify"
               DELIMITED SIZE INTO WS-VERIFY-URL
           END-STRING

           MOVE "N" TO WS-FLAG-FOUND

           PERFORM STEP-1-FETCH-CSV
           PERFORM STEP-2-PARSE-CSV
           PERFORM STEP-3-RESET-BUDGET
           PERFORM STEP-4-CLASSIFY-ITEMS

           IF WS-FLAG-FOUND NOT = "Y"
               DISPLAY " "
               DISPLAY "Gotowe (bez flagi)."
           END-IF

           DISPLAY "=== ZAKONCZONO ==="
           STOP RUN.

      *> ============================================================
      *> STEP 1: Download CSV via curl GET
      *> ============================================================
       STEP-1-FETCH-CSV.
           DISPLAY " "
           DISPLAY "--- Krok 1: Pobieranie CSV ---"

           INITIALIZE WS-DATA-URL
           STRING
               TRIM(WS-HUB-URL) "/data/"
               TRIM(WS-HUB-KEY)
               "/categorize.csv"
               DELIMITED SIZE INTO WS-DATA-URL
           END-STRING

           DISPLAY "  URL: " TRIM(WS-DATA-URL)

           INITIALIZE WS-CMD
           STRING
               "curl -s -o " TRIM(WS-CSV-PATH)
               " " WS-QT
               TRIM(WS-DATA-URL)
               WS-QT
               DELIMITED SIZE INTO WS-CMD
           END-STRING

           CALL "SYSTEM" USING WS-CMD

           DISPLAY "  CSV pobrany do: " TRIM(WS-CSV-PATH)
           .

      *> ============================================================
      *> STEP 2: Parse CSV into items table
      *> ============================================================
       STEP-2-PARSE-CSV.
           DISPLAY " "
           DISPLAY "--- Krok 2: Parsowanie CSV ---"

           MOVE "Y" TO WS-CSV-HEADER
           MOVE "N" TO WS-EOF
           MOVE 0 TO WS-ITEM-COUNT
           MOVE 0 TO WS-TOTAL-CSV

           MOVE WS-CSV-PATH TO WS-WORK-PATH
           OPEN INPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "  BLAD: Nie mozna otworzyc CSV! "
                   WS-FS
               STOP RUN
           END-IF

           PERFORM UNTIL WS-EOF = "Y"
               READ WORK-FILE INTO WS-CSV-LINE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       IF WS-CSV-HEADER = "Y"
                           MOVE "N" TO WS-CSV-HEADER
                       ELSE
                           ADD 1 TO WS-TOTAL-CSV
                           PERFORM PARSE-CSV-LINE
                       END-IF
               END-READ
           END-PERFORM

           CLOSE WORK-FILE
           MOVE "N" TO WS-EOF
           MOVE "work.tmp" TO WS-WORK-PATH

           DISPLAY "  Wczytano: " WS-ITEM-COUNT " pozycji"
           .

      *> -- Parse one CSV line: code,description --
       PARSE-CSV-LINE.
           MOVE SPACES TO WS-FLD-CODE
           MOVE SPACES TO WS-FLD-DESC
           MOVE 1 TO WS-FIELD-PTR

           UNSTRING WS-CSV-LINE DELIMITED BY ","
               INTO WS-FLD-CODE
               WITH POINTER WS-FIELD-PTR
           END-UNSTRING

      *>   Rest of line is description
           IF WS-FIELD-PTR > 0 AND
              WS-FIELD-PTR < LENGTH(TRIM(WS-CSV-LINE))
               MOVE WS-CSV-LINE(WS-FIELD-PTR:)
                   TO WS-FLD-DESC
           END-IF

      *>   Strip surrounding quotes from description
           IF TRIM(WS-FLD-DESC)(1:1) = WS-QT
               MOVE TRIM(WS-FLD-DESC)(2:) TO WS-FLD-DESC
               MOVE LENGTH(TRIM(WS-FLD-DESC)) TO WS-K
               IF WS-K > 0 AND
                  WS-FLD-DESC(WS-K:1) = WS-QT
                   MOVE SPACE TO WS-FLD-DESC(WS-K:1)
               END-IF
           END-IF

           IF WS-ITEM-COUNT < WS-MAX-ITEMS
               ADD 1 TO WS-ITEM-COUNT
               MOVE TRIM(WS-FLD-CODE)
                   TO WS-I-CODE(WS-ITEM-COUNT)
               MOVE TRIM(WS-FLD-DESC)
                   TO WS-I-DESC(WS-ITEM-COUNT)
           END-IF
           .

      *> ============================================================
      *> STEP 3: Reset budget via POST
      *> ============================================================
       STEP-3-RESET-BUDGET.
           DISPLAY " "
           DISPLAY "--- Krok 3: Reset budzetu ---"

           INITIALIZE WS-PAYLOAD
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME) WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "prompt" WS-QT ":"
               WS-QT "reset" WS-QT
               "}}"
               DELIMITED SIZE INTO WS-PAYLOAD
           END-STRING

      *>   Write payload to temp file
           OPEN OUTPUT WORK-FILE
           WRITE WORK-REC FROM WS-PAYLOAD
           CLOSE WORK-FILE

      *>   POST via curl
           INITIALIZE WS-CMD
           STRING
               "curl -s -o resp.json"
               " -X POST "
               TRIM(WS-VERIFY-URL)
               " -H " WS-QT
               "Content-Type: application/json"
               WS-QT
               " -d @work.tmp"
               DELIMITED SIZE INTO WS-CMD
           END-STRING

           CALL "SYSTEM" USING WS-CMD

      *>   Read and display response
           MOVE "resp.json" TO WS-WORK-PATH
           PERFORM READ-RESPONSE-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           DISPLAY "  Reset: " TRIM(WS-RESP-BUF)(1:200)
           .

      *> ============================================================
      *> STEP 4: Classify each item sequentially
      *> ============================================================
       STEP-4-CLASSIFY-ITEMS.
           DISPLAY " "
           DISPLAY "--- Krok 4: Klasyfikacja pozycji ---"

           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-ITEM-COUNT
               OR WS-FLAG-FOUND = "Y"
               PERFORM CLASSIFY-SINGLE-ITEM
           END-PERFORM
           .

      *> -- Classify one item with retry on 502/503 --
       CLASSIFY-SINGLE-ITEM.
      *>   Build prompt: template + code: description
           INITIALIZE WS-PROMPT
           STRING
               TRIM(WS-PROMPT-TPL)
               TRIM(WS-I-CODE(WS-IDX))
               ": "
               TRIM(WS-I-DESC(WS-IDX))
               DELIMITED SIZE INTO WS-PROMPT
           END-STRING

      *>   Build JSON payload
           INITIALIZE WS-PAYLOAD
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME) WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "prompt" WS-QT ":"
               WS-QT TRIM(WS-PROMPT) WS-QT
               "}}"
               DELIMITED SIZE INTO WS-PAYLOAD
           END-STRING

      *>   Write payload to temp file
           OPEN OUTPUT WORK-FILE
           WRITE WORK-REC FROM WS-PAYLOAD
           CLOSE WORK-FILE

      *>   Retry loop
           MOVE 1 TO WS-ATTEMPT
           MOVE "N" TO WS-RETRY-DONE

           PERFORM UNTIL WS-ATTEMPT > WS-MAX-RETRIES
               OR WS-RETRY-DONE = "Y"
               OR WS-FLAG-FOUND = "Y"

      *>       POST via curl with headers dump
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

      *>       Read HTTP status from headers
               MOVE "headers.tmp" TO WS-WORK-PATH
               PERFORM READ-HTTP-STATUS
               MOVE "work.tmp" TO WS-WORK-PATH

      *>       Retry on 502/503
               IF WS-STATUS-CODE = "502"
               OR WS-STATUS-CODE = "503"
                   DISPLAY "  [" WS-IDX "] "
                       TRIM(WS-I-CODE(WS-IDX))
                       " " WS-STATUS-CODE
                       " retry " WS-ATTEMPT
                   MOVE 2 TO WS-SLEEP-SECS
                   CALL "C$SLEEP" USING WS-SLEEP-SECS
                   ADD 1 TO WS-ATTEMPT
               ELSE
                   MOVE "Y" TO WS-RETRY-DONE
               END-IF
           END-PERFORM

      *>   Read response body
           MOVE "resp.json" TO WS-WORK-PATH
           PERFORM READ-RESPONSE-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

      *>   Extract "output" from debug
           MOVE "output" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL

           DISPLAY "  [" WS-IDX "] "
               TRIM(WS-I-CODE(WS-IDX))
               " -> " TRIM(WS-JVAL)

      *>   Check for flag {FLG:...}
           MOVE 0 TO WS-TALLY-CNT
           IF WS-RESP-LEN > 0
               INSPECT WS-RESP-BUF(1:WS-RESP-LEN)
                   TALLYING WS-TALLY-CNT FOR ALL "FLG"
           END-IF
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-FLAG-FOUND
               DISPLAY " "
               DISPLAY "*** FLAGA ZNALEZIONA! ***"
               DISPLAY TRIM(WS-RESP-BUF)(1:500)
               EXIT PARAGRAPH
           END-IF

      *>   Check for error (code < 0)
           MOVE "code" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE SPACES TO WS-CODE-VAL
           MOVE TRIM(WS-JVAL) TO WS-CODE-VAL

           IF WS-CODE-VAL(1:1) = "-"
               DISPLAY "  BLAD: "
                   TRIM(WS-RESP-BUF)(1:300)
               MOVE "Y" TO WS-FLAG-FOUND
           END-IF
           .

      *> ============================================================
      *> READ-HTTP-STATUS: Get status code from headers.tmp
      *> ============================================================
       READ-HTTP-STATUS.
           MOVE "000" TO WS-STATUS-CODE
           MOVE "N" TO WS-EOF

           OPEN INPUT WORK-FILE
           IF WS-FS NOT = "00"
               EXIT PARAGRAPH
           END-IF

      *>   Read first line: "HTTP/x.x NNN ..."
           READ WORK-FILE INTO WS-LINE
               AT END
                   MOVE "Y" TO WS-EOF
               NOT AT END
                   IF WS-LINE(1:5) = "HTTP/"
      *>               Find space after version
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
                   END-IF
           END-READ

           CLOSE WORK-FILE
           MOVE "N" TO WS-EOF
           .

      *> ============================================================
      *> READ-RESPONSE-FILE: Read file into WS-RESP-BUF
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
                           MOVE WS-LINE(1:WS-K)
                               TO WS-RESP-BUF(
                               WS-RESP-LEN + 1:WS-K)
                           ADD WS-K TO WS-RESP-LEN
                       END-IF
               END-READ
           END-PERFORM

           CLOSE WORK-FILE
           MOVE "N" TO WS-EOF
           .

      *> ============================================================
      *> FIND-JSON-VAL: Find "key":"value" in WS-RESP-BUF
      *> Input: WS-KEY-SEARCH, WS-JPOS (start)
      *> Output: WS-JVAL, WS-JPOS (updated)
      *> ============================================================
       FIND-JSON-VAL.
           MOVE SPACES TO WS-JVAL
           MOVE SPACES TO WS-TMP
           STRING WS-QT TRIM(WS-KEY-SEARCH) WS-QT
               DELIMITED SIZE INTO WS-TMP
           END-STRING

      *>   Find the key
           MOVE 0 TO WS-KEY-POS
           PERFORM VARYING WS-FJV-POS FROM WS-JPOS BY 1
               UNTIL WS-FJV-POS > WS-RESP-LEN
               OR WS-KEY-POS > 0
               IF WS-FJV-POS + LENGTH(TRIM(WS-TMP))
                   - 1 <= WS-RESP-LEN
               AND WS-RESP-BUF(WS-FJV-POS:
                   LENGTH(TRIM(WS-TMP)))
                   = TRIM(WS-TMP)
                   MOVE WS-FJV-POS TO WS-KEY-POS
               END-IF
           END-PERFORM

           IF WS-KEY-POS = 0
               EXIT PARAGRAPH
           END-IF

      *>   Skip past key and colon
           COMPUTE WS-FJV-POS =
               WS-KEY-POS + LENGTH(TRIM(WS-TMP))
           PERFORM UNTIL WS-FJV-POS > WS-RESP-LEN
               OR WS-RESP-BUF(WS-FJV-POS:1) = ":"
               ADD 1 TO WS-FJV-POS
           END-PERFORM
           ADD 1 TO WS-FJV-POS

      *>   Skip whitespace
           PERFORM UNTIL WS-FJV-POS > WS-RESP-LEN
               OR WS-RESP-BUF(WS-FJV-POS:1) NOT = " "
               ADD 1 TO WS-FJV-POS
           END-PERFORM

      *>   Check if string value (starts with quote)
           IF WS-RESP-BUF(WS-FJV-POS:1) = WS-QT
               ADD 1 TO WS-FJV-POS
               MOVE WS-FJV-POS TO WS-VAL-START
      *>       Find closing quote (skip escaped \")
               PERFORM UNTIL WS-FJV-POS > WS-RESP-LEN
                   IF WS-RESP-BUF(WS-FJV-POS:1) = WS-QT
                       IF WS-FJV-POS > 1
                       AND WS-RESP-BUF(
                           WS-FJV-POS - 1:1)
                           = X"5C"
                           ADD 1 TO WS-FJV-POS
                       ELSE
                           EXIT PERFORM
                       END-IF
                   ELSE
                       ADD 1 TO WS-FJV-POS
                   END-IF
               END-PERFORM
               COMPUTE WS-VAL-END =
                   WS-FJV-POS - 1
               IF WS-VAL-END >= WS-VAL-START
               AND WS-VAL-END - WS-VAL-START
                   + 1 <= 500
                   MOVE WS-RESP-BUF(WS-VAL-START:
                       WS-VAL-END - WS-VAL-START
                       + 1) TO WS-JVAL
               END-IF
               ADD 1 TO WS-FJV-POS
           ELSE
      *>       Numeric: read until delimiter
               MOVE WS-FJV-POS TO WS-VAL-START
               PERFORM UNTIL WS-FJV-POS > WS-RESP-LEN
                   OR WS-RESP-BUF(WS-FJV-POS:1) = ","
                   OR WS-RESP-BUF(WS-FJV-POS:1) = "}"
                   OR WS-RESP-BUF(WS-FJV-POS:1) = "]"
                   OR WS-RESP-BUF(WS-FJV-POS:1) = " "
                   ADD 1 TO WS-FJV-POS
               END-PERFORM
               COMPUTE WS-VAL-END =
                   WS-FJV-POS - 1
               IF WS-VAL-END >= WS-VAL-START
                   MOVE WS-RESP-BUF(WS-VAL-START:
                       WS-VAL-END - WS-VAL-START
                       + 1) TO WS-JVAL
               END-IF
           END-IF
           MOVE WS-FJV-POS TO WS-JPOS
           .
