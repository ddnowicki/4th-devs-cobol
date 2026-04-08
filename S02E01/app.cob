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
      *> === Environment ===
       01  WS-HUB-KEY              PIC X(50).
       01  WS-HUB-URL              PIC X(100).
       01  WS-QT                   PIC X(1) VALUE '"'.

      *> === File I/O ===
       01  WS-FS                   PIC XX.
       01  WS-WORK-PATH            PIC X(100) VALUE "work.tmp".

      *> === HTTP ===
       01  WS-VERIFY-URL           PIC X(200).
       01  WS-DATA-URL             PIC X(200).
       01  WS-CMD                  PIC X(4000).
       01  WS-PAYLOAD              PIC X(4000).
       01  WS-STATUS-CODE          PIC X(3).

      *> === Task Configuration ===
       01  WS-TASK-NAME            PIC X(20) VALUE "categorize".
       01  WS-PROMPT-TPL           PIC X(300) VALUE
           "Reply DNG or NEU only." & X"0A"
           & "DNG if weapon,spear,bow,arrow,sword,"
           & "explosive,mine,bomb,firearm,gun,"
           & "pistol,rifle,ammunition,drug." & X"0A"
           & "Reactor,fuel cassette=NEU." & X"0A"
           & "Tools,parts,cables=NEU." & X"0A"
           & "Else NEU." & X"0A".

      *> === CSV Parsing ===
       01  WS-CSV-PATH             PIC X(100) VALUE
           "categorize.csv".
       01  WS-CSV-LINE             PIC X(2000).
       01  WS-CSV-HEADER           PIC X(1) VALUE "Y".
       01  WS-FIELD-PTR            PIC 9(4).
       01  WS-FLD-CODE             PIC X(20).
       01  WS-FLD-DESC             PIC X(500).

      *> === Task Data ===
       01  WS-MAX-ITEMS            PIC 9(3) VALUE 50.
       01  WS-ITEM-COUNT           PIC 9(3) VALUE 0.
       01  WS-ITEMS-TABLE.
           05  WS-ITEM OCCURS 50 TIMES.
               10  WS-I-CODE       PIC X(20).
               10  WS-I-DESC       PIC X(500).

      *> === JSON Parsing (copybooks) ===
       COPY JSONPARSE-WS.
       COPY JSONREAD-WS.
       COPY JSONESCAPE-WS.

      *> === Task-specific ===
       01  WS-PROMPT               PIC X(1000).

      *> === Constants ===
       01  WS-MAX-RETRIES          PIC 9(1) VALUE 5.
       01  WS-RETRY-DELAY          PIC 9(2) VALUE 2.

      *> === Control Flow ===
       01  WS-IDX                  PIC 9(3).
       01  WS-TOTAL-CSV            PIC 9(5) VALUE 0.
       01  WS-TALLY-CNT            PIC 9(4) VALUE 0.
       01  WS-FLAG-FOUND           PIC X VALUE "N".
       01  WS-ATTEMPT              PIC 9(2).
       01  WS-RETRY-DONE           PIC X VALUE "N".
       01  WS-SLEEP-SECS           PIC 9(3).
       01  WS-CODE-VAL             PIC X(20).
       01  WS-CODE-NUM             PIC S9(4).
       01  WS-MAIN-ATTEMPT         PIC 9(2) VALUE 0.
       01  WS-MAX-MAIN-RETRIES     PIC 9(2) VALUE 3.
       01  WS-CLASS-ERROR          PIC X VALUE "N".

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
           MOVE 0 TO WS-MAIN-ATTEMPT

           PERFORM FETCH-CSV-DATA
           PERFORM PARSE-CSV-ITEMS

           PERFORM UNTIL WS-MAIN-ATTEMPT
               >= WS-MAX-MAIN-RETRIES
               OR WS-FLAG-FOUND = "Y"
               ADD 1 TO WS-MAIN-ATTEMPT
               MOVE "N" TO WS-CLASS-ERROR
               DISPLAY " "
               DISPLAY "=== Proba "
                   WS-MAIN-ATTEMPT " ==="
               PERFORM RESET-BUDGET
               PERFORM CLASSIFY-ALL-ITEMS
               IF WS-CLASS-ERROR = "Y"
                   DISPLAY "  Blad - ponowna proba"
                   MOVE 3 TO WS-SLEEP-SECS
                   CALL "C$SLEEP"
                       USING WS-SLEEP-SECS
               END-IF
           END-PERFORM

           IF WS-FLAG-FOUND NOT = "Y"
               DISPLAY " "
               DISPLAY "Gotowe (bez flagi)."
           END-IF

           DISPLAY "=== ZAKONCZONO ==="
           STOP RUN.

      *> ============================================================
      *> FETCH-CSV-DATA: Download CSV via curl GET
      *> ============================================================
       FETCH-CSV-DATA.
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
      *> PARSE-CSV-ITEMS: Parse CSV into items table
      *> ============================================================
       PARSE-CSV-ITEMS.
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
      *> RESET-BUDGET: Reset budget via POST
      *> ============================================================
       RESET-BUDGET.
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
           IF WS-FS NOT = "00"
               DISPLAY "ERR: OPEN "
                   TRIM(WS-WORK-PATH)
                   " FS=" WS-FS
               STOP RUN
           END-IF
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
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           DISPLAY "  Reset: " TRIM(WS-JBUF)(1:200)
           .

      *> ============================================================
      *> CLASSIFY-ALL-ITEMS: Classify each item sequentially
      *> ============================================================
       CLASSIFY-ALL-ITEMS.
           DISPLAY " "
           DISPLAY "--- Krok 4: Klasyfikacja pozycji ---"

           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-ITEM-COUNT
               OR WS-FLAG-FOUND = "Y"
               OR WS-CLASS-ERROR = "Y"
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

      *>   Escape prompt for safe JSON embedding
           MOVE WS-PROMPT TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

      *>   Build JSON payload
           INITIALIZE WS-PAYLOAD
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME) WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "prompt" WS-QT ":"
               WS-QT WS-ESC-OUT(1:WS-ESC-OLEN)
               WS-QT
               "}}"
               DELIMITED SIZE INTO WS-PAYLOAD
           END-STRING

      *>   Write payload to temp file
           OPEN OUTPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "ERR: OPEN "
                   TRIM(WS-WORK-PATH)
                   " FS=" WS-FS
               STOP RUN
           END-IF
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
                   MOVE WS-RETRY-DELAY
                       TO WS-SLEEP-SECS
                   CALL "C$SLEEP" USING WS-SLEEP-SECS
                   ADD 1 TO WS-ATTEMPT
               ELSE
                   MOVE "Y" TO WS-RETRY-DONE
               END-IF
           END-PERFORM

      *>   Read response body
           MOVE "resp.json" TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
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
           IF WS-JLEN > 0
               INSPECT WS-JBUF(1:WS-JLEN)
                   TALLYING WS-TALLY-CNT FOR ALL "FLG"
           END-IF
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-FLAG-FOUND
               DISPLAY " "
               DISPLAY "*** FLAGA ZNALEZIONA! ***"
               DISPLAY TRIM(WS-JBUF)(1:500)
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
                   TRIM(WS-JBUF)(1:300)
               MOVE "Y" TO WS-CLASS-ERROR
           END-IF
           .

      *> (ESCAPE-STRING-FOR-JSON removed - use JSON-ESCAPE-STR)

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

      *> (READ-RESPONSE-FILE removed - use READ-JSON-FILE)

      *> === Shared paragraphs (copybooks) ===
       COPY JSONPARSE-PROC.
       COPY JSONREAD-PROC.
       COPY JSONESCAPE-PROC.
