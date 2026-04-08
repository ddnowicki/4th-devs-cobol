       IDENTIFICATION DIVISION.
       PROGRAM-ID. S01E01-PEOPLE.
       AUTHOR. CLAUDE-COBOL-PORT.
      *> ============================================================
      *> S01E01 to GnuCOBOL
      *> 1. Fetch CSV via CALL "SYSTEM" curl
      *> 2. Parse and filter (men, Grudziadz, age 20-40 in 2026)
      *> 3. Tag jobs via OpenAI API (CALL "SYSTEM" curl)
      *> 4. Filter by "transport" tag
      *> 5. Submit answer via CALL "SYSTEM" curl
      *> ============================================================

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CSV-FILE ASSIGN TO WS-CSV-PATH
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
           SELECT TAG-RESP-FILE ASSIGN TO WS-TAG-RESP-PATH
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
           SELECT SUBMIT-RESP-FILE ASSIGN TO WS-SUBMIT-RESP-PATH
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
           SELECT REQ-BODY-FILE ASSIGN TO WS-REQ-BODY-PATH
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CSV-FILE.
       01  CSV-RECORD              PIC X(2000).

       FD  TAG-RESP-FILE.
       01  TAG-RESP-RECORD         PIC X(4000).

       FD  SUBMIT-RESP-FILE.
       01  SUBMIT-RESP-RECORD      PIC X(4000).

       FD  REQ-BODY-FILE.
       01  REQ-BODY-RECORD         PIC X(8000).

       WORKING-STORAGE SECTION.
      *> === Environment (via copybook) ===
       COPY ENVLOAD-WS.
       01  WS-MODEL                PIC X(20) VALUE "gpt-4.1-mini".

      *> === Constants ===
       01  WS-MAX-RETRIES          PIC 9(1) VALUE 5.

      *> === File I/O ===
       01  WS-FILE-STATUS          PIC XX.
       01  WS-CSV-PATH             PIC X(100) VALUE
           "people.csv".
       01  WS-TAG-RESP-PATH        PIC X(100) VALUE
           "tag_response.json".
       01  WS-SUBMIT-RESP-PATH     PIC X(100) VALUE
           "submit_response.json".
       01  WS-REQ-BODY-PATH        PIC X(100) VALUE
           "request_body.tmp".
       01  WS-EOF                  PIC X(1) VALUE "N".

      *> === HTTP ===
       01  WS-CMD                  PIC X(4000).
       01  WS-DATA-URL             PIC X(200).

      *> === CSV Parsing ===
       01  WS-CSV-LINE             PIC X(2000).
       01  WS-CSV-HEADER           PIC X(1) VALUE "Y".
       01  WS-FIELD-PTR            PIC 9(4).
       01  WS-FLD-NAME             PIC X(50).
       01  WS-FLD-SURNAME          PIC X(50).
       01  WS-FLD-GENDER           PIC X(10).
       01  WS-FLD-BIRTHDATE        PIC X(20).
       01  WS-FLD-BIRTHPLACE       PIC X(100).
       01  WS-FLD-BIRTHCOUNTRY     PIC X(50).
       01  WS-FLD-JOB              PIC X(500).
       01  WS-QUOTE-POS            PIC 9(4).
       01  WS-REST-LINE            PIC X(2000).

      *> === Date Parsing ===
       01  WS-BORN-YEAR            PIC 9(4).
       01  WS-AGE-2026             PIC 9(3).

      *> === JSON Building ===
       01  WS-JSON-BUF             PIC X(8000).
       01  WS-JSON-ENTRY           PIC X(500).
       01  WS-ANSWER-JSON          PIC X(8000).
       01  WS-TAG-JSON             PIC X(4000).

      *> === JSON Escape (via copybook) ===
       COPY JSONESCAPE-WS.

      *> === Task Data ===
       01  WS-MAX-PEOPLE           PIC 9(3) VALUE 500.
       01  WS-PEOPLE-COUNT         PIC 9(3) VALUE 0.
       01  WS-PEOPLE-TABLE.
           05  WS-PERSON OCCURS 500 TIMES.
               10  WS-P-NAME      PIC X(50).
               10  WS-P-SURNAME   PIC X(50).
               10  WS-P-GENDER    PIC X(10).
               10  WS-P-BIRTHDATE PIC X(20).
               10  WS-P-BIRTHPLACE PIC X(100).
               10  WS-P-JOB       PIC X(500).
               10  WS-P-BORN-YEAR PIC 9(4).
               10  WS-P-TAGS      PIC X(500).
               10  WS-P-HAS-TRANSPORT PIC X(1).
       01  WS-TRANSPORT-COUNT      PIC 9(3) VALUE 0.
       01  WS-TRANSPORT-IDX        OCCURS 100 TIMES PIC 9(3).
       01  WS-SYS-PROMPT           PIC X(500) VALUE
           "Przypisz tagi do opisu stanowiska pracy. Dostepne tagi:"
           & " IT, transport, edukacja, medycyna, praca z ludzmi,"
           & " praca z pojazdami, praca fizyczna."
           & " Zwroc TYLKO pasujace tagi. Osoba moze miec wiele tagow."
           & " Odpowiedz TYLKO jako JSON: {tags:[tag1,tag2]}".

      *> === Control Flow ===
       01  WS-IDX                  PIC 9(3).
       01  WS-TOTAL-CSV            PIC 9(5) VALUE 0.
       01  WS-TALLY-CNT            PIC 9(4) VALUE 0.
       01  WS-PI                   PIC 9(4).
       01  WS-TAG-LINE             PIC X(4000).
       01  WS-TRIMMED              PIC X(500).
       01  WS-RESP-LINE            PIC X(4000).
       01  WS-TAG-SRC              PIC X(500).
       01  WS-TAG-PART             PIC X(100).
       01  WS-TAG-PTR              PIC 9(4).
       01  WS-TAG-FIRST            PIC X(1).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "=== S01E01 PEOPLE - COBOL ==="

           PERFORM LOAD-ENV-VARS

           PERFORM FETCH-CSV
           PERFORM PARSE-AND-FILTER
           PERFORM TAG-ALL-JOBS
           PERFORM FILTER-TRANSPORT
           PERFORM SUBMIT-ANSWER

           DISPLAY " "
           DISPLAY "=== PROGRAM ZAKONCZONY ==="
           STOP RUN.

      *> === Fetch CSV ===
       FETCH-CSV.
           DISPLAY " "
           DISPLAY "--- Krok 1: Pobieranie CSV ---"

           INITIALIZE WS-DATA-URL
           STRING TRIM(WS-HUB-URL) "/data/"
                  TRIM(WS-HUB-KEY)
                  "/people.csv"
                  DELIMITED SIZE
                  INTO WS-DATA-URL
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

      *> === Parse CSV and Filter ===
       PARSE-AND-FILTER.
           DISPLAY " "
           DISPLAY "--- Krok 2: Parsowanie i filtracja CSV ---"
           DISPLAY "  Filtr: mezczyzni, Grudziadz, wiek 20-40 (2026)"

           MOVE "Y" TO WS-CSV-HEADER
           MOVE "N" TO WS-EOF
           MOVE 0 TO WS-PEOPLE-COUNT
           MOVE 0 TO WS-TOTAL-CSV

           OPEN INPUT CSV-FILE
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "ERR: OPEN "
                   TRIM(WS-CSV-PATH)
                   " FS=" WS-FILE-STATUS
               STOP RUN
           END-IF

           PERFORM UNTIL WS-EOF = "Y"
               READ CSV-FILE INTO WS-CSV-LINE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       IF WS-CSV-HEADER = "Y"
                           MOVE "N" TO WS-CSV-HEADER
                       ELSE
                           ADD 1 TO WS-TOTAL-CSV
                           PERFORM PARSE-CSV-LINE
                           PERFORM CHECK-FILTER
                       END-IF
               END-READ
           END-PERFORM

           CLOSE CSV-FILE

           DISPLAY "  Wczytano rekordow: " WS-TOTAL-CSV
           DISPLAY "  Po filtracji: " WS-PEOPLE-COUNT " osob"
           .

      *> -- Parse one CSV line into fields --
       PARSE-CSV-LINE.
           MOVE SPACES TO WS-FLD-NAME
                          WS-FLD-SURNAME
                          WS-FLD-GENDER
                          WS-FLD-BIRTHDATE
                          WS-FLD-BIRTHPLACE
                          WS-FLD-BIRTHCOUNTRY
                          WS-FLD-JOB
           MOVE 1 TO WS-FIELD-PTR

           UNSTRING WS-CSV-LINE DELIMITED BY ","
               INTO WS-FLD-NAME
                    WS-FLD-SURNAME
                    WS-FLD-GENDER
                    WS-FLD-BIRTHDATE
                    WS-FLD-BIRTHPLACE
                    WS-FLD-BIRTHCOUNTRY
               WITH POINTER WS-FIELD-PTR
           END-UNSTRING

           IF WS-FIELD-PTR > 0 AND
              WS-FIELD-PTR < LENGTH(TRIM(WS-CSV-LINE))
               MOVE WS-CSV-LINE(WS-FIELD-PTR:) TO WS-REST-LINE
               IF WS-REST-LINE(1:1) = WS-QT
                   MOVE WS-REST-LINE(2:) TO WS-FLD-JOB
                   INSPECT WS-FLD-JOB REPLACING TRAILING
                       SPACES BY SPACES
                   MOVE 0 TO WS-QUOTE-POS
                   INSPECT FUNCTION REVERSE(TRIM(WS-FLD-JOB))
                       TALLYING WS-QUOTE-POS FOR LEADING SPACES
                   MOVE LENGTH(TRIM(WS-FLD-JOB)) TO WS-QUOTE-POS
                   IF WS-QUOTE-POS > 0 AND
                      WS-FLD-JOB(WS-QUOTE-POS:1) = WS-QT
                       MOVE SPACE TO WS-FLD-JOB(WS-QUOTE-POS:1)
                   END-IF
               ELSE
                   MOVE WS-REST-LINE TO WS-FLD-JOB
               END-IF
           END-IF
           .

      *> -- Check if person matches filter --
       CHECK-FILTER.
           IF TRIM(WS-FLD-GENDER) NOT = "M"
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-FLD-BIRTHPLACE
               TALLYING WS-TALLY-CNT FOR ALL "rudzi"
           IF WS-TALLY-CNT = 0
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-TRIMMED
           MOVE TRIM(WS-FLD-BIRTHDATE) TO WS-TRIMMED
           IF WS-TRIMMED = SPACES
               EXIT PARAGRAPH
           END-IF

           MOVE WS-TRIMMED(1:4) TO WS-BORN-YEAR
           IF WS-BORN-YEAR NOT NUMERIC
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-AGE-2026 = 2026 - WS-BORN-YEAR
           IF WS-AGE-2026 < 20 OR WS-AGE-2026 > 40
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO WS-PEOPLE-COUNT
           IF WS-PEOPLE-COUNT > WS-MAX-PEOPLE
               DISPLAY "  UWAGA: Przekroczono limit osob!"
               SUBTRACT 1 FROM WS-PEOPLE-COUNT
               EXIT PARAGRAPH
           END-IF

           MOVE TRIM(WS-FLD-NAME) TO
               WS-P-NAME(WS-PEOPLE-COUNT)
           MOVE TRIM(WS-FLD-SURNAME) TO
               WS-P-SURNAME(WS-PEOPLE-COUNT)
           MOVE TRIM(WS-FLD-GENDER) TO
               WS-P-GENDER(WS-PEOPLE-COUNT)
           MOVE TRIM(WS-FLD-BIRTHDATE) TO
               WS-P-BIRTHDATE(WS-PEOPLE-COUNT)
           MOVE TRIM(WS-FLD-BIRTHPLACE) TO
               WS-P-BIRTHPLACE(WS-PEOPLE-COUNT)
           MOVE TRIM(WS-FLD-JOB) TO
               WS-P-JOB(WS-PEOPLE-COUNT)
           MOVE WS-BORN-YEAR TO
               WS-P-BORN-YEAR(WS-PEOPLE-COUNT)
           MOVE "N" TO WS-P-HAS-TRANSPORT(WS-PEOPLE-COUNT)
           .

      *> === Tag All Jobs via OpenAI API ===
       TAG-ALL-JOBS.
           DISPLAY " "
           DISPLAY "--- Krok 3: Tagowanie zawodow przez LLM ---"
           DISPLAY "  Model: " TRIM(WS-MODEL)
           DISPLAY "  Osob do otagowania: " WS-PEOPLE-COUNT

           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-PEOPLE-COUNT
               PERFORM TAG-SINGLE-JOB
           END-PERFORM
           .

      *> -- Tag one person's job via OpenAI API --
       TAG-SINGLE-JOB.
           DISPLAY "  [" WS-IDX "/" WS-PEOPLE-COUNT "] "
               TRIM(WS-P-NAME(WS-IDX)) " "
               TRIM(WS-P-SURNAME(WS-IDX))
               " - job: " TRIM(WS-P-JOB(WS-IDX))

           MOVE WS-P-JOB(WS-IDX) TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

           INITIALIZE WS-TAG-JSON
           STRING
               "{"
               WS-QT "model" WS-QT ":" WS-QT
               TRIM(WS-MODEL) WS-QT ","
               WS-QT "messages" WS-QT ":["
               "{"
               WS-QT "role" WS-QT ":" WS-QT "system" WS-QT ","
               WS-QT "content" WS-QT ":" WS-QT
               TRIM(WS-SYS-PROMPT) WS-QT
               "},"
               "{"
               WS-QT "role" WS-QT ":" WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT ":" WS-QT
               WS-ESC-OUT(1:WS-ESC-OLEN) WS-QT
               "}"
               "],"
               WS-QT "temperature" WS-QT ":0.2"
               "}"
               DELIMITED SIZE
               INTO WS-TAG-JSON
           END-STRING

           OPEN OUTPUT REQ-BODY-FILE
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "ERR: OPEN "
                   TRIM(WS-REQ-BODY-PATH)
                   " FS=" WS-FILE-STATUS
               STOP RUN
           END-IF
           WRITE REQ-BODY-RECORD FROM WS-TAG-JSON
           CLOSE REQ-BODY-FILE

           INITIALIZE WS-CMD
           STRING
               "curl -s -o " TRIM(WS-TAG-RESP-PATH)
               " -X POST "
               TRIM(WS-OPENAI-URL)
               " -H " WS-QT
               "Content-Type: application/json"
               WS-QT
               " -H " WS-QT
               "Authorization: Bearer "
               TRIM(WS-OPENAI-KEY) WS-QT
               " -d @" TRIM(WS-REQ-BODY-PATH)
               DELIMITED SIZE INTO WS-CMD
           END-STRING

           CALL "SYSTEM" USING WS-CMD

           PERFORM READ-TAG-RESPONSE
           DISPLAY "    -> tagi: " TRIM(WS-P-TAGS(WS-IDX))
           .

      *> -- Read tag response and extract tags --
       READ-TAG-RESPONSE.
           MOVE SPACES TO WS-P-TAGS(WS-IDX)
           MOVE "N" TO WS-EOF
           MOVE SPACES TO WS-TAG-LINE

           OPEN INPUT TAG-RESP-FILE
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "    BLAD otwarcia odpowiedzi: " WS-FILE-STATUS
               MOVE "brak" TO WS-P-TAGS(WS-IDX)
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-TAG-LINE
           PERFORM UNTIL WS-EOF = "Y"
               MOVE SPACES TO WS-RESP-LINE
               READ TAG-RESP-FILE INTO WS-RESP-LINE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       STRING TRIM(WS-TAG-LINE)
                           " "
                           TRIM(WS-RESP-LINE)
                           DELIMITED SIZE
                           INTO WS-TAG-LINE
                       END-STRING
               END-READ
           END-PERFORM
           CLOSE TAG-RESP-FILE
           MOVE "N" TO WS-EOF

           PERFORM EXTRACT-TAGS-FROM-RESPONSE
           .

      *> -- Extract known tags from response text --
       EXTRACT-TAGS-FROM-RESPONSE.
           MOVE SPACES TO WS-P-TAGS(WS-IDX)
           MOVE "N" TO WS-P-HAS-TRANSPORT(WS-IDX)

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-TAG-LINE
               TALLYING WS-TALLY-CNT FOR ALL "transport"
           IF WS-TALLY-CNT > 0
               PERFORM APPEND-TAG-SEPARATOR
               STRING TRIM(WS-P-TAGS(WS-IDX))
                   "transport" DELIMITED SIZE
                   INTO WS-P-TAGS(WS-IDX)
               END-STRING
               MOVE "Y" TO WS-P-HAS-TRANSPORT(WS-IDX)
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-TAG-LINE
               TALLYING WS-TALLY-CNT FOR ALL ":IT"
           IF WS-TALLY-CNT = 0
               INSPECT WS-TAG-LINE
                   TALLYING WS-TALLY-CNT FOR ALL " IT"
           END-IF
           IF WS-TALLY-CNT > 0
               PERFORM APPEND-TAG-SEPARATOR
               STRING TRIM(WS-P-TAGS(WS-IDX))
                   "IT" DELIMITED SIZE
                   INTO WS-P-TAGS(WS-IDX)
               END-STRING
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-TAG-LINE
               TALLYING WS-TALLY-CNT FOR ALL "edukacja"
           IF WS-TALLY-CNT > 0
               PERFORM APPEND-TAG-SEPARATOR
               STRING TRIM(WS-P-TAGS(WS-IDX))
                   "edukacja" DELIMITED SIZE
                   INTO WS-P-TAGS(WS-IDX)
               END-STRING
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-TAG-LINE
               TALLYING WS-TALLY-CNT FOR ALL "medycyna"
           IF WS-TALLY-CNT > 0
               PERFORM APPEND-TAG-SEPARATOR
               STRING TRIM(WS-P-TAGS(WS-IDX))
                   "medycyna" DELIMITED SIZE
                   INTO WS-P-TAGS(WS-IDX)
               END-STRING
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-TAG-LINE
               TALLYING WS-TALLY-CNT FOR ALL "praca z ludzmi"
           IF WS-TALLY-CNT > 0
               PERFORM APPEND-TAG-SEPARATOR
               STRING TRIM(WS-P-TAGS(WS-IDX))
                   "praca z ludzmi" DELIMITED SIZE
                   INTO WS-P-TAGS(WS-IDX)
               END-STRING
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-TAG-LINE
               TALLYING WS-TALLY-CNT FOR ALL "praca z pojazdami"
           IF WS-TALLY-CNT > 0
               PERFORM APPEND-TAG-SEPARATOR
               STRING TRIM(WS-P-TAGS(WS-IDX))
                   "praca z pojazdami" DELIMITED SIZE
                   INTO WS-P-TAGS(WS-IDX)
               END-STRING
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-TAG-LINE
               TALLYING WS-TALLY-CNT FOR ALL "praca fizyczna"
           IF WS-TALLY-CNT > 0
               PERFORM APPEND-TAG-SEPARATOR
               STRING TRIM(WS-P-TAGS(WS-IDX))
                   "praca fizyczna" DELIMITED SIZE
                   INTO WS-P-TAGS(WS-IDX)
               END-STRING
           END-IF

           IF WS-P-TAGS(WS-IDX) = SPACES
               MOVE "brak" TO WS-P-TAGS(WS-IDX)
           END-IF
           .

      *> -- Add comma separator between tags --
       APPEND-TAG-SEPARATOR.
           IF TRIM(WS-P-TAGS(WS-IDX)) NOT = SPACES
               STRING TRIM(WS-P-TAGS(WS-IDX))
                   ", " DELIMITED SIZE
                   INTO WS-P-TAGS(WS-IDX)
               END-STRING
           END-IF
           .

      *> === Filter Transport People ===
       FILTER-TRANSPORT.
           DISPLAY " "
           DISPLAY "--- Krok 4: Filtrowanie - tag transport ---"

           MOVE 0 TO WS-TRANSPORT-COUNT

           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-PEOPLE-COUNT
               IF WS-P-HAS-TRANSPORT(WS-IDX) = "Y"
                   ADD 1 TO WS-TRANSPORT-COUNT
                   MOVE WS-IDX TO
                       WS-TRANSPORT-IDX(WS-TRANSPORT-COUNT)
                   DISPLAY "  -> " TRIM(WS-P-NAME(WS-IDX)) " "
                       TRIM(WS-P-SURNAME(WS-IDX))
               END-IF
           END-PERFORM

           DISPLAY "  Osoby z tagiem transport: "
               WS-TRANSPORT-COUNT
           .

      *> === Build and Submit Answer ===
       SUBMIT-ANSWER.
           DISPLAY " "
           DISPLAY "--- Krok 5: Wysylanie odpowiedzi ---"

           IF WS-TRANSPORT-COUNT = 0
               DISPLAY "  Brak osob do wyslania!"
               EXIT PARAGRAPH
           END-IF

           PERFORM BUILD-ANSWER-JSON
           PERFORM SEND-ANSWER
           .

      *> -- Build answer JSON --
       BUILD-ANSWER-JSON.
           INITIALIZE WS-ANSWER-JSON
           MOVE "[" TO WS-ANSWER-JSON

           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-TRANSPORT-COUNT

               MOVE WS-TRANSPORT-IDX(WS-IDX) TO WS-PI
               INITIALIZE WS-JSON-ENTRY

               INITIALIZE WS-TAG-JSON
               PERFORM BUILD-TAGS-ARRAY

               MOVE WS-P-NAME(WS-PI)
                   TO WS-ESC-IN
               PERFORM JSON-ESCAPE-STR
               STRING
                   "{"
                   WS-QT "name" WS-QT ":"
                   WS-QT
                   WS-ESC-OUT(
                   1:WS-ESC-OLEN)
                   WS-QT ","
                   DELIMITED SIZE
                   INTO WS-JSON-ENTRY
               END-STRING

               MOVE WS-P-SURNAME(WS-PI)
                   TO WS-ESC-IN
               PERFORM JSON-ESCAPE-STR
               STRING
                   TRIM(WS-JSON-ENTRY)
                   WS-QT "surname" WS-QT ":"
                   WS-QT
                   WS-ESC-OUT(
                   1:WS-ESC-OLEN)
                   WS-QT ","
                   DELIMITED SIZE
                   INTO WS-JSON-ENTRY
               END-STRING

               STRING
                   TRIM(WS-JSON-ENTRY)
                   WS-QT "gender" WS-QT ":"
                   WS-QT TRIM(WS-P-GENDER(WS-PI))
                   WS-QT ","
                   WS-QT "born" WS-QT ":"
                   WS-P-BORN-YEAR(WS-PI) ","
                   DELIMITED SIZE
                   INTO WS-JSON-ENTRY
               END-STRING

               MOVE WS-P-BIRTHPLACE(WS-PI)
                   TO WS-ESC-IN
               PERFORM JSON-ESCAPE-STR
               STRING
                   TRIM(WS-JSON-ENTRY)
                   WS-QT "city" WS-QT ":"
                   WS-QT
                   WS-ESC-OUT(
                   1:WS-ESC-OLEN)
                   WS-QT ","
                   WS-QT "tags" WS-QT ":"
                   TRIM(WS-TAG-JSON)
                   "}"
                   DELIMITED SIZE
                   INTO WS-JSON-ENTRY
               END-STRING

               IF WS-IDX > 1
                   STRING TRIM(WS-ANSWER-JSON)
                       "," TRIM(WS-JSON-ENTRY)
                       DELIMITED SIZE
                       INTO WS-ANSWER-JSON
                   END-STRING
               ELSE
                   STRING TRIM(WS-ANSWER-JSON)
                       TRIM(WS-JSON-ENTRY)
                       DELIMITED SIZE
                       INTO WS-ANSWER-JSON
                   END-STRING
               END-IF
           END-PERFORM

           STRING TRIM(WS-ANSWER-JSON) "]"
               DELIMITED SIZE INTO WS-ANSWER-JSON
           END-STRING
           .

      *> -- Build JSON array from comma-separated tags --
       BUILD-TAGS-ARRAY.
           MOVE TRIM(WS-P-TAGS(WS-PI)) TO WS-TAG-SRC
           MOVE "[" TO WS-TAG-JSON
           MOVE "Y" TO WS-TAG-FIRST
           MOVE 1 TO WS-TAG-PTR

           PERFORM UNTIL WS-TAG-PTR >
               LENGTH(TRIM(WS-TAG-SRC))
               MOVE SPACES TO WS-TAG-PART
               UNSTRING WS-TAG-SRC DELIMITED BY ","
                   INTO WS-TAG-PART
                   WITH POINTER WS-TAG-PTR
               END-UNSTRING
               IF TRIM(WS-TAG-PART) NOT = SPACES
                   IF WS-TAG-FIRST = "Y"
                       MOVE "N" TO WS-TAG-FIRST
                       STRING TRIM(WS-TAG-JSON)
                           WS-QT TRIM(WS-TAG-PART) WS-QT
                           DELIMITED SIZE
                           INTO WS-TAG-JSON
                       END-STRING
                   ELSE
                       STRING TRIM(WS-TAG-JSON)
                           "," WS-QT TRIM(WS-TAG-PART) WS-QT
                           DELIMITED SIZE
                           INTO WS-TAG-JSON
                       END-STRING
                   END-IF
               END-IF
           END-PERFORM

           STRING TRIM(WS-TAG-JSON) "]"
               DELIMITED SIZE INTO WS-TAG-JSON
           END-STRING
           .

      *> -- Send answer to verify endpoint --
       SEND-ANSWER.
           INITIALIZE WS-JSON-BUF
           STRING
               "{"
               WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT "people" WS-QT ","
               WS-QT "answer" WS-QT ":"
               TRIM(WS-ANSWER-JSON)
               "}"
               DELIMITED SIZE
               INTO WS-JSON-BUF
           END-STRING

           DISPLAY "  Payload: " TRIM(WS-JSON-BUF)

           OPEN OUTPUT REQ-BODY-FILE
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "ERR: OPEN "
                   TRIM(WS-REQ-BODY-PATH)
                   " FS=" WS-FILE-STATUS
               STOP RUN
           END-IF
           WRITE REQ-BODY-RECORD FROM WS-JSON-BUF
           CLOSE REQ-BODY-FILE

           INITIALIZE WS-CMD
           STRING
               "curl -s -o " TRIM(WS-SUBMIT-RESP-PATH)
               " -X POST "
               TRIM(WS-VERIFY-URL)
               " -H " WS-QT
               "Content-Type: application/json"
               WS-QT
               " -d @" TRIM(WS-REQ-BODY-PATH)
               DELIMITED SIZE INTO WS-CMD
           END-STRING

           CALL "SYSTEM" USING WS-CMD

           DISPLAY "  Wysylam do: " TRIM(WS-VERIFY-URL)

           MOVE SPACES TO WS-RESP-LINE
           MOVE "N" TO WS-EOF
           OPEN INPUT SUBMIT-RESP-FILE
           IF WS-FILE-STATUS = "00"
               PERFORM UNTIL WS-EOF = "Y"
                   MOVE SPACES TO WS-TAG-LINE
                   READ SUBMIT-RESP-FILE INTO WS-TAG-LINE
                       AT END
                           MOVE "Y" TO WS-EOF
                       NOT AT END
                           STRING TRIM(WS-RESP-LINE) " "
                               TRIM(WS-TAG-LINE)
                               DELIMITED SIZE
                               INTO WS-RESP-LINE
                           END-STRING
                   END-READ
               END-PERFORM
               CLOSE SUBMIT-RESP-FILE
               MOVE "N" TO WS-EOF
               DISPLAY "  Odpowiedz: " TRIM(WS-RESP-LINE)
           ELSE
               DISPLAY "  BLAD: Nie mozna odczytac odpowiedzi!"
           END-IF
           .

       COPY ENVLOAD-PROC.
       COPY JSONESCAPE-PROC.
