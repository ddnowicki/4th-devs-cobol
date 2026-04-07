       IDENTIFICATION DIVISION.
       PROGRAM-ID. S03E04-NEGOTIATIONS.
      *> ============================================================
      *> S03E04 - Negotiations: Tool API for item city lookup
      *> 1. Fetch 3 CSVs (cities, items, connections) via curl
      *> 2. TCP server on configurable port
      *> 3. POST /api/search: keyword scoring + LLM match
      *> 4. Return comma-separated city names (<500 bytes)
      *> 5. Submit tool registration + poll result
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

      *> === File I/O ===
       01  WS-FS                   PIC XX.
       01  WS-WORK-PATH            PIC X(200)
                                   VALUE "work.tmp".

      *> === HTTP ===
       01  WS-CMD                  PIC X(4000).
       01  WS-REQ-JSON             PIC X(16000).
       01  WS-CSV-URL              PIC X(300).
       01  WS-PAYLOAD              PIC X(4000).

      *> === JSON Parsing (copybook) ===
       COPY JSONPARSE-WS.
       COPY JSONREAD-WS.

      *> === TCP Server ===
       01  WS-PORT                 PIC 9(5).
       01  WS-PORT-STR             PIC X(10).
       01  WS-SERVER-FD            PIC S9(9) COMP-5.
       01  WS-CLIENT-FD            PIC S9(9) COMP-5.
       01  WS-RESULT               PIC S9(9) COMP-5.
       01  WS-ONE                  PIC S9(9) COMP-5
                                   VALUE 1.
       01  WS-BACKLOG              PIC S9(9) COMP-5
                                   VALUE 10.
       01  WS-SOCKADDR.
           05  WS-SIN-FAMILY       PIC 9(4) COMP-5.
           05  WS-SIN-PORT         PIC 9(4) COMP-5.
           05  WS-SIN-ADDR         PIC 9(9) COMP-5.
           05  WS-SIN-ZERO         PIC X(8).
       01  WS-READ-BUF             PIC X(8000).
       01  WS-READ-LEN             PIC S9(9) COMP-5.
       01  WS-WRITE-BUF            PIC X(16000).
       01  WS-WRITE-LEN            PIC S9(9) COMP-5.
       01  WS-HTTP-METHOD          PIC X(10).
       01  WS-HTTP-PATH            PIC X(200).
       01  WS-BODY                 PIC X(4000).
       01  WS-BODY-START           PIC 9(5).
       01  WS-RESPONSE-MSG         PIC X(4000).
       01  WS-PORT-HI              PIC 9(3).
       01  WS-PORT-LO              PIC 9(3).
       78  AF-INET                 VALUE 2.
       78  SOCK-STREAM             VALUE 1.
       78  SOL-SOCKET              VALUE 1.
       78  SO-REUSEADDR            VALUE 2.
       78  INADDR-ANY              VALUE 0.

      *> === Task Data ===
       01  WS-CITY-COUNT           PIC 9(4) VALUE 0.
       01  WS-CITIES.
           05  WS-CITY OCCURS 100 TIMES.
               10  WS-CITY-CODE    PIC X(10).
               10  WS-CITY-NAME    PIC X(80).

      *>
       01  WS-ITEM-COUNT           PIC 9(5) VALUE 0.
       01  WS-ITEMS.
           05  WS-ITEM OCCURS 2500 TIMES.
               10  WS-ITEM-CODE    PIC X(10).
               10  WS-ITEM-NAME    PIC X(200).
               10  WS-ITEM-LOWER   PIC X(200).

      *>
       01  WS-CONN-COUNT           PIC 9(5) VALUE 0.
       01  WS-CONNS.
           05  WS-CONN OCCURS 6000 TIMES.
               10  WS-CONN-ITEM    PIC X(10).
               10  WS-CONN-CITY    PIC X(10).

      *> === Search Logic ===
       01  WS-QUERY                PIC X(500).
       01  WS-QUERY-LOWER          PIC X(500).
       01  WS-QUERY-LEN            PIC 9(5).

      *>
       01  WS-TOKEN-COUNT          PIC 9(3) VALUE 0.
       01  WS-TOKENS.
           05  WS-TOKEN OCCURS 50 TIMES.
               10  WS-TKN-TEXT     PIC X(50).
               10  WS-TKN-LEN     PIC 9(3).

      *>
       01  WS-SCORE                PIC 9(5)V99.
       01  WS-BEST-SCORE           PIC 9(5)V99.
       01  WS-MATCH-SCORE          PIC 9(5)V99.

      *>
       01  WS-CAND-COUNT           PIC 9(3) VALUE 0.
       01  WS-CANDIDATES.
           05  WS-CAND OCCURS 30 TIMES.
               10  WS-CAND-IDX    PIC 9(5).
               10  WS-CAND-SCORE  PIC 9(5)V99.

      *>
       01  WS-ITEM-TOKEN-CNT       PIC 9(3).
       01  WS-ITEM-TOKENS.
           05  WS-ITOK OCCURS 50 TIMES.
               10  WS-ITOK-TEXT    PIC X(50).
               10  WS-ITOK-LEN    PIC 9(3).
       01  WS-OVERLAP              PIC 9(3).
       01  WS-SUBSTR-SCORE         PIC 9(5)V99.

      *>
       01  WS-MATCHED-CODE         PIC X(10).
       01  WS-MATCHED-NAME         PIC X(200).

      *>
       01  WS-RESULT-CITIES        PIC X(500).
       01  WS-RESULT-LEN           PIC 9(5).
       01  WS-CITY-FOUND           PIC X VALUE "N".

      *>
       01  WS-RES-CITY-COUNT       PIC 9(3) VALUE 0.
       01  WS-RES-CITIES.
           05  WS-RES-CITY OCCURS 100 TIMES.
               10  WS-RES-CITY-NM PIC X(80).
       01  WS-SORT-TEMP            PIC X(80).

      *>
       01  WS-CAND-LIST            PIC X(8000).
       01  WS-CAND-LIST-LEN        PIC 9(5).

      *>
       01  WS-CSV-FIELD1           PIC X(200).
       01  WS-CSV-FIELD2           PIC X(200).
       01  WS-CSV-PTR              PIC 9(5).
       01  WS-CSV-HEADER           PIC X VALUE "Y".

      *>
       01  WS-CH                   PIC X(1).
       01  WS-IN-TOKEN             PIC X VALUE "N".
       01  WS-CUR-TOKEN            PIC X(50).
       01  WS-CUR-TKN-LEN         PIC 9(3).

      *> === Submission ===
       01  WS-TUNNEL-URL           PIC X(200).
       01  WS-SUBMIT-DONE          PIC X VALUE "N".
       01  WS-CHECK-ATTEMPT        PIC 9(2).

      *> === JSON Escape ===
       01  WS-ESC-BUF              PIC X(8000).
       01  WS-ESC-LEN              PIC 9(5).

      *> === Control Flow ===
       01  WS-PTR                  PIC 9(5).
       01  WS-IDX                  PIC 9(5).
       01  WS-IDX2                 PIC 9(5).
       01  WS-TALLY-CNT            PIC 9(5).
       01  WS-I                    PIC 9(5).
       01  WS-J                    PIC 9(5).
       01  WS-TEMP                 PIC X(4000).
       01  WS-TEMP2                PIC X(4000).
       01  WS-RUNNING              PIC X VALUE "Y".

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S03E04 NEGOTIATIONS ==="

           PERFORM INIT-ENV
           PERFORM FETCH-CSVS
           PERFORM PARSE-CITIES-CSV
           PERFORM PARSE-ITEMS-CSV
           PERFORM PARSE-CONNS-CSV

           DISPLAY "  Data loaded: "
               TRIM(WS-CITY-COUNT)
               " cities, "
               TRIM(WS-ITEM-COUNT)
               " items, "
               TRIM(WS-CONN-COUNT)
               " connections"

      *>   Bind port first, then submit, then serve
           PERFORM START-SERVER

           STOP RUN.

      *> ============================================================
      *> INIT-ENV: Load env vars and build URLs
      *> ============================================================
       INIT-ENV.
           PERFORM LOAD-ENV-VARS

           ACCEPT WS-PORT-STR
               FROM ENVIRONMENT "PORT"
           IF WS-PORT-STR = SPACES
               MOVE "5000"
                   TO WS-PORT-STR
           END-IF
           MOVE NUMVAL(TRIM(WS-PORT-STR))
               TO WS-PORT

      *>   Get server URL from env
           ACCEPT WS-TUNNEL-URL
               FROM ENVIRONMENT
               "SERVER_URL"
           IF WS-TUNNEL-URL = SPACES
               DISPLAY
                   "ERR: SERVER_URL!"
               STOP RUN
           END-IF

           DISPLAY "  Port: " WS-PORT
           DISPLAY "  URL: "
               TRIM(WS-TUNNEL-URL)
           .

      *> ============================================================
      *> FETCH-CSVS: Download CSV files via curl
      *> ============================================================
       FETCH-CSVS.
           DISPLAY "  Fetching CSVs..."

           INITIALIZE WS-CMD
           STRING
               "curl -s -o "
               "cities.csv "
               TRIM(WS-HUB-URL)
               "/dane/s03e04_csv/"
               "cities.csv"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           INITIALIZE WS-CMD
           STRING
               "curl -s -o "
               "items.csv "
               TRIM(WS-HUB-URL)
               "/dane/s03e04_csv/"
               "items.csv"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           INITIALIZE WS-CMD
           STRING
               "curl -s -o "
               "connections.csv "
               TRIM(WS-HUB-URL)
               "/dane/s03e04_csv/"
               "connections.csv"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           DISPLAY "  CSVs fetched."
           .

      *> ============================================================
      *> PARSE-CITIES-CSV: Parse cities.csv
      *> ============================================================
       PARSE-CITIES-CSV.
           MOVE 0 TO WS-CITY-COUNT
           MOVE "Y" TO WS-CSV-HEADER
           MOVE "N" TO WS-EOF
           MOVE "cities.csv"
               TO WS-WORK-PATH
           OPEN INPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "ERR: cities.csv!"
               STOP RUN
           END-IF

           PERFORM UNTIL WS-EOF = "Y"
               MOVE SPACES TO WS-LINE
               READ WORK-FILE
                   INTO WS-LINE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       IF WS-CSV-HEADER = "Y"
                           MOVE "N"
                           TO WS-CSV-HEADER
                       ELSE
                           PERFORM
                           PARSE-CITY-LINE
                       END-IF
               END-READ
           END-PERFORM

           CLOSE WORK-FILE
           MOVE "N" TO WS-EOF
           MOVE "work.tmp"
               TO WS-WORK-PATH
           .

      *> ============================================================
       PARSE-CITY-LINE.
           MOVE SPACES TO WS-CSV-FIELD1
           MOVE SPACES TO WS-CSV-FIELD2
           MOVE 1 TO WS-CSV-PTR

           UNSTRING WS-LINE
               DELIMITED BY ","
               INTO WS-CSV-FIELD1
                    WS-CSV-FIELD2
               WITH POINTER WS-CSV-PTR
           END-UNSTRING

           IF TRIM(WS-CSV-FIELD1)
               NOT = SPACES
               ADD 1 TO WS-CITY-COUNT
               IF WS-CITY-COUNT <= 100
                   MOVE TRIM(WS-CSV-FIELD2)
                       TO WS-CITY-CODE(
                       WS-CITY-COUNT)
                   MOVE TRIM(WS-CSV-FIELD1)
                       TO WS-CITY-NAME(
                       WS-CITY-COUNT)
               END-IF
           END-IF
           .

      *> ============================================================
      *> PARSE-ITEMS-CSV: Parse items.csv
      *> ============================================================
       PARSE-ITEMS-CSV.
           MOVE 0 TO WS-ITEM-COUNT
           MOVE "Y" TO WS-CSV-HEADER
           MOVE "N" TO WS-EOF
           MOVE "items.csv"
               TO WS-WORK-PATH
           OPEN INPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "ERR: items.csv!"
               STOP RUN
           END-IF

           PERFORM UNTIL WS-EOF = "Y"
               MOVE SPACES TO WS-LINE
               READ WORK-FILE
                   INTO WS-LINE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       IF WS-CSV-HEADER = "Y"
                           MOVE "N"
                           TO WS-CSV-HEADER
                       ELSE
                           PERFORM
                           PARSE-ITEM-LINE
                       END-IF
               END-READ
           END-PERFORM

           CLOSE WORK-FILE
           MOVE "N" TO WS-EOF
           MOVE "work.tmp"
               TO WS-WORK-PATH
           .

      *> ============================================================
       PARSE-ITEM-LINE.
           MOVE SPACES TO WS-CSV-FIELD1
           MOVE SPACES TO WS-CSV-FIELD2
           MOVE 1 TO WS-CSV-PTR

           UNSTRING WS-LINE
               DELIMITED BY ","
               INTO WS-CSV-FIELD1
                    WS-CSV-FIELD2
               WITH POINTER WS-CSV-PTR
           END-UNSTRING

           IF TRIM(WS-CSV-FIELD1)
               NOT = SPACES
               ADD 1 TO WS-ITEM-COUNT
               IF WS-ITEM-COUNT <= 2500
                   MOVE TRIM(WS-CSV-FIELD2)
                       TO WS-ITEM-CODE(
                       WS-ITEM-COUNT)
                   MOVE TRIM(WS-CSV-FIELD1)
                       TO WS-ITEM-NAME(
                       WS-ITEM-COUNT)
                   MOVE LOWER-CASE(
                       TRIM(WS-CSV-FIELD1))
                       TO WS-ITEM-LOWER(
                       WS-ITEM-COUNT)
               END-IF
           END-IF
           .

      *> ============================================================
      *> PARSE-CONNS-CSV: Parse connections.csv
      *> ============================================================
       PARSE-CONNS-CSV.
           MOVE 0 TO WS-CONN-COUNT
           MOVE "Y" TO WS-CSV-HEADER
           MOVE "N" TO WS-EOF
           MOVE "connections.csv"
               TO WS-WORK-PATH
           OPEN INPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "ERR: connections.csv!"
               STOP RUN
           END-IF

           PERFORM UNTIL WS-EOF = "Y"
               MOVE SPACES TO WS-LINE
               READ WORK-FILE
                   INTO WS-LINE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       IF WS-CSV-HEADER = "Y"
                           MOVE "N"
                           TO WS-CSV-HEADER
                       ELSE
                           PERFORM
                           PARSE-CONN-LINE
                       END-IF
               END-READ
           END-PERFORM

           CLOSE WORK-FILE
           MOVE "N" TO WS-EOF
           MOVE "work.tmp"
               TO WS-WORK-PATH
           .

      *> ============================================================
       PARSE-CONN-LINE.
           MOVE SPACES TO WS-CSV-FIELD1
           MOVE SPACES TO WS-CSV-FIELD2
           MOVE 1 TO WS-CSV-PTR

           UNSTRING WS-LINE
               DELIMITED BY ","
               INTO WS-CSV-FIELD1
                    WS-CSV-FIELD2
               WITH POINTER WS-CSV-PTR
           END-UNSTRING

           IF TRIM(WS-CSV-FIELD1)
               NOT = SPACES
               ADD 1 TO WS-CONN-COUNT
               IF WS-CONN-COUNT <= 6000
                   MOVE TRIM(WS-CSV-FIELD1)
                       TO WS-CONN-ITEM(
                       WS-CONN-COUNT)
                   MOVE TRIM(WS-CSV-FIELD2)
                       TO WS-CONN-CITY(
                       WS-CONN-COUNT)
               END-IF
           END-IF
           .

      *> ============================================================
      *> SUBMIT-TOOL: Register tool with hub
      *> ============================================================
       SUBMIT-TOOL.
           DISPLAY "  Submitting tool..."

           MOVE SPACES TO WS-PAYLOAD
           MOVE 1 TO WS-PTR
           STRING
               "{"
               WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY)
               WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT "negotiations"
               WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "tools" WS-QT ":["
               "{"
               WS-QT "URL" WS-QT ":"
               WS-QT
               TRIM(WS-TUNNEL-URL)
               "/api/search"
               WS-QT ","
               DELIMITED SIZE
               INTO WS-PAYLOAD
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "description"
               WS-QT ":"
               WS-QT
               "Search for an item "
               "by name or "
               "description (in "
               "Polish). Send POST "
               "with params "
               "containing query. "
               "Returns cities "
               "where item is "
               "available."
               WS-QT "}]}}"
               DELIMITED SIZE
               INTO WS-PAYLOAD
               WITH POINTER WS-PTR
           END-STRING

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

      *>   Read and display response
           MOVE "hub_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp"
               TO WS-WORK-PATH
           DISPLAY "  Submit: "
               TRIM(WS-JBUF)(1:300)
           .

      *> ============================================================
      *> START-SERVER: TCP server with POSIX sockets
      *> ============================================================
       START-SERVER.
      *>   Create socket
           CALL "socket" USING
               BY VALUE AF-INET
               BY VALUE SOCK-STREAM
               BY VALUE 0
               RETURNING WS-SERVER-FD
           END-CALL

           IF WS-SERVER-FD < 0
               DISPLAY "ERR: socket()!"
               STOP RUN
           END-IF

      *>   Set SO_REUSEADDR
           CALL "setsockopt" USING
               BY VALUE WS-SERVER-FD
               BY VALUE SOL-SOCKET
               BY VALUE SO-REUSEADDR
               BY REFERENCE WS-ONE
               BY VALUE 4
               RETURNING WS-RESULT
           END-CALL

      *>   Prepare sockaddr_in
           INITIALIZE WS-SOCKADDR
           MOVE AF-INET
               TO WS-SIN-FAMILY

      *>   htons(port)
           DIVIDE WS-PORT BY 256
               GIVING WS-PORT-HI
               REMAINDER WS-PORT-LO
           END-DIVIDE
           COMPUTE WS-SIN-PORT =
               WS-PORT-LO * 256
               + WS-PORT-HI
           END-COMPUTE

           MOVE INADDR-ANY
               TO WS-SIN-ADDR
           MOVE LOW-VALUES
               TO WS-SIN-ZERO

      *>   Bind
           CALL "bind" USING
               BY VALUE WS-SERVER-FD
               BY REFERENCE WS-SOCKADDR
               BY VALUE 16
               RETURNING WS-RESULT
           END-CALL

           IF WS-RESULT < 0
               DISPLAY "ERR: bind()!"
               STOP RUN
           END-IF

      *>   Listen
           CALL "listen" USING
               BY VALUE WS-SERVER-FD
               BY VALUE WS-BACKLOG
               RETURNING WS-RESULT
           END-CALL

           IF WS-RESULT < 0
               DISPLAY "ERR: listen()!"
               STOP RUN
           END-IF

           DISPLAY "  Listening on port "
               WS-PORT
           DISPLAY "  URL: "
               TRIM(WS-TUNNEL-URL)

      *>   Accept loop (submit done by CI step)
           PERFORM UNTIL
               WS-RUNNING = "N"
               PERFORM ACCEPT-CONN
           END-PERFORM
           .

      *> ============================================================
      *> ACCEPT-CONN: Accept and handle one HTTP request
      *> ============================================================
       ACCEPT-CONN.
           CALL "accept" USING
               BY VALUE WS-SERVER-FD
               BY VALUE 0
               BY VALUE 0
               RETURNING WS-CLIENT-FD
           END-CALL

           IF WS-CLIENT-FD < 0
               EXIT PARAGRAPH
           END-IF

      *>   Read request
           INITIALIZE WS-READ-BUF
           CALL "read" USING
               BY VALUE WS-CLIENT-FD
               BY REFERENCE WS-READ-BUF
               BY VALUE 7999
               RETURNING WS-READ-LEN
           END-CALL

           IF WS-READ-LEN > 0
               PERFORM PARSE-HTTP-REQ
               PERFORM HANDLE-REQUEST
               PERFORM SEND-HTTP-RESP
           END-IF

      *>   Close client
           CALL "close" USING
               BY VALUE WS-CLIENT-FD
           END-CALL
           .

      *> ============================================================
      *> PARSE-HTTP-REQ: Extract method, path, body
      *> ============================================================
       PARSE-HTTP-REQ.
           INITIALIZE WS-HTTP-METHOD
           INITIALIZE WS-HTTP-PATH
           INITIALIZE WS-BODY

      *>   Extract method and path
           MOVE 1 TO WS-IDX
           UNSTRING WS-READ-BUF
               DELIMITED BY " "
               INTO WS-HTTP-METHOD
               WITH POINTER WS-IDX
           END-UNSTRING
           UNSTRING WS-READ-BUF
               DELIMITED BY " "
               INTO WS-HTTP-PATH
               WITH POINTER WS-IDX
           END-UNSTRING

           DISPLAY "  "
               TRIM(WS-HTTP-METHOD) " "
               TRIM(WS-HTTP-PATH)

      *>   Find body after \r\n\r\n
           MOVE 0 TO WS-BODY-START
           PERFORM VARYING WS-IDX
               FROM 1 BY 1
               UNTIL WS-IDX
               > WS-READ-LEN - 3
               OR WS-BODY-START > 0
               IF WS-READ-BUF(
                   WS-IDX:4)
                   = X"0D0A0D0A"
                   COMPUTE
                       WS-BODY-START =
                       WS-IDX + 4
               END-IF
               IF WS-BODY-START = 0
               AND WS-READ-BUF(
                   WS-IDX:2)
                   = X"0A0A"
                   COMPUTE
                       WS-BODY-START =
                       WS-IDX + 2
               END-IF
           END-PERFORM

           IF WS-BODY-START > 0
           AND WS-BODY-START
               <= WS-READ-LEN
               COMPUTE WS-IDX =
                   WS-READ-LEN
                   - WS-BODY-START + 1
               IF WS-IDX > 4000
                   MOVE 4000 TO WS-IDX
               END-IF
               MOVE WS-READ-BUF(
                   WS-BODY-START:WS-IDX)
                   TO WS-BODY
           END-IF
           .

      *> ============================================================
      *> HANDLE-REQUEST: Route by method/path
      *> ============================================================
       HANDLE-REQUEST.
           INITIALIZE WS-RESPONSE-MSG

           IF TRIM(WS-HTTP-METHOD) = "GET"
               STRING
                   "{" WS-QT "status"
                   WS-QT ":"
                   WS-QT "ok" WS-QT ","
                   WS-QT "items" WS-QT
                   ":" WS-ITEM-COUNT ","
                   WS-QT "cities" WS-QT
                   ":" WS-CITY-COUNT "}"
                   DELIMITED SIZE
                   INTO WS-RESPONSE-MSG
               END-STRING
               EXIT PARAGRAPH
           END-IF

           IF TRIM(WS-HTTP-METHOD)
               NOT = "POST"
               STRING
                   "{" WS-QT "output"
                   WS-QT ":"
                   WS-QT "method error"
                   WS-QT "}"
                   DELIMITED SIZE
                   INTO WS-RESPONSE-MSG
               END-STRING
               EXIT PARAGRAPH
           END-IF

      *>   Check /api/report
           MOVE 0 TO WS-TALLY-CNT
           INSPECT TRIM(WS-HTTP-PATH)
               TALLYING WS-TALLY-CNT
               FOR ALL "/api/report"
           IF WS-TALLY-CNT > 0
               DISPLAY "  POST /api/report"
      *>       Extract params
               MOVE WS-BODY TO WS-JBUF
               MOVE LENGTH(TRIM(WS-BODY))
                   TO WS-JLEN
               MOVE "params"
                   TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               DISPLAY "  Report: "
                   TRIM(WS-JVAL)(1:200)
               MOVE SPACES
                   TO WS-RESPONSE-MSG
               STRING
                   "{" WS-QT "output"
                   WS-QT ":"
                   WS-QT "Confirmed: "
                   TRIM(WS-JVAL)(1:200)
                   WS-QT "}"
                   DELIMITED SIZE
                   INTO WS-RESPONSE-MSG
               END-STRING
               EXIT PARAGRAPH
           END-IF

      *>   Check path contains /api/search
           MOVE 0 TO WS-TALLY-CNT
           INSPECT TRIM(WS-HTTP-PATH)
               TALLYING WS-TALLY-CNT
               FOR ALL "/api/search"
           IF WS-TALLY-CNT = 0
      *>       Also handle check result
               MOVE 0 TO WS-TALLY-CNT
               INSPECT TRIM(WS-HTTP-PATH)
                   TALLYING WS-TALLY-CNT
                   FOR ALL "/api/check"
               IF WS-TALLY-CNT > 0
                   PERFORM CHECK-RESULT
                   EXIT PARAGRAPH
               END-IF
               STRING
                   "{" WS-QT "output"
                   WS-QT ":"
                   WS-QT "not found"
                   WS-QT "}"
                   DELIMITED SIZE
                   INTO WS-RESPONSE-MSG
               END-STRING
               EXIT PARAGRAPH
           END-IF

      *>   Handle search
           PERFORM HANDLE-SEARCH
           .

      *> ============================================================
      *> HANDLE-SEARCH: Main search logic
      *> ============================================================
       HANDLE-SEARCH.
      *>   Extract params from body JSON
           MOVE WS-BODY TO WS-JBUF
           MOVE LENGTH(TRIM(WS-BODY))
               TO WS-JLEN
           MOVE "params" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL) TO WS-QUERY

           IF TRIM(WS-QUERY) = SPACES
               STRING
                   "{" WS-QT "output"
                   WS-QT ":"
                   WS-QT "error: empty"
                   WS-QT "}"
                   DELIMITED SIZE
                   INTO WS-RESPONSE-MSG
               END-STRING
               EXIT PARAGRAPH
           END-IF

           DISPLAY "  Query: "
               TRIM(WS-QUERY)(1:200)

      *>   Lowercase the query
           MOVE LOWER-CASE(
               TRIM(WS-QUERY))
               TO WS-QUERY-LOWER
           MOVE LENGTH(
               TRIM(WS-QUERY-LOWER))
               TO WS-QUERY-LEN

      *>   Step 1: Tokenize query
           PERFORM TOKENIZE-QUERY

      *>   Step 2: Score all items
           PERFORM FIND-CANDIDATES

           IF WS-CAND-COUNT = 0
               DISPLAY "  No candidates"
               STRING
                   "{" WS-QT "output"
                   WS-QT ":"
                   WS-QT
                   "No matching items"
                   WS-QT "}"
                   DELIMITED SIZE
                   INTO WS-RESPONSE-MSG
               END-STRING
               EXIT PARAGRAPH
           END-IF

      *>   Step 3: LLM picks best match
           PERFORM LLM-MATCH-ITEM

           IF TRIM(WS-MATCHED-CODE)
               = SPACES
               DISPLAY "  LLM: no match"
               STRING
                   "{" WS-QT "output"
                   WS-QT ":"
                   WS-QT
                   "No matching items"
                   WS-QT "}"
                   DELIMITED SIZE
                   INTO WS-RESPONSE-MSG
               END-STRING
               EXIT PARAGRAPH
           END-IF

      *>   Step 4: Lookup cities
           PERFORM LOOKUP-CITIES

           IF WS-RES-CITY-COUNT = 0
               DISPLAY "  No cities"
               STRING
                   "{" WS-QT "output"
                   WS-QT ":"
                   WS-QT
                   "no cities found"
                   WS-QT "}"
                   DELIMITED SIZE
                   INTO WS-RESPONSE-MSG
               END-STRING
               EXIT PARAGRAPH
           END-IF

      *>   Step 5: Sort cities
           PERFORM SORT-RESULT-CITIES

      *>   Step 6: Build response
           PERFORM BUILD-CITY-RESPONSE
           .

      *> ============================================================
      *> TOKENIZE-QUERY: Split query into tokens
      *> ============================================================
       TOKENIZE-QUERY.
           MOVE 0 TO WS-TOKEN-COUNT
           MOVE "N" TO WS-IN-TOKEN
           MOVE SPACES TO WS-CUR-TOKEN
           MOVE 0 TO WS-CUR-TKN-LEN

           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I > WS-QUERY-LEN
               MOVE WS-QUERY-LOWER(
                   WS-I:1)
                   TO WS-CH

      *>       Check if alphanumeric
               IF (WS-CH >= "a"
               AND WS-CH <= "z")
               OR (WS-CH >= "0"
               AND WS-CH <= "9")
               OR WS-CH = "."
               OR WS-CH = ","
      *>           Part of token
                   IF WS-IN-TOKEN = "N"
                       MOVE "Y"
                           TO WS-IN-TOKEN
                       MOVE SPACES
                           TO WS-CUR-TOKEN
                       MOVE 0
                           TO WS-CUR-TKN-LEN
                   END-IF
                   ADD 1
                       TO WS-CUR-TKN-LEN
                   IF WS-CUR-TKN-LEN <= 50
                       MOVE WS-CH
                           TO WS-CUR-TOKEN(
                           WS-CUR-TKN-LEN
                           :1)
                   END-IF
               ELSE
      *>           End of token
                   IF WS-IN-TOKEN = "Y"
                       IF WS-CUR-TKN-LEN
                           >= 2
                       AND WS-TOKEN-COUNT
                           < 50
                           ADD 1
                           TO WS-TOKEN-COUNT
                           MOVE WS-CUR-TOKEN
                           TO WS-TKN-TEXT(
                           WS-TOKEN-COUNT)
                           MOVE
                           WS-CUR-TKN-LEN
                           TO WS-TKN-LEN(
                           WS-TOKEN-COUNT)
                       END-IF
                       MOVE "N"
                           TO WS-IN-TOKEN
                   END-IF
               END-IF
           END-PERFORM

      *>   Flush last token
           IF WS-IN-TOKEN = "Y"
           AND WS-CUR-TKN-LEN >= 2
           AND WS-TOKEN-COUNT < 50
               ADD 1 TO WS-TOKEN-COUNT
               MOVE WS-CUR-TOKEN
                   TO WS-TKN-TEXT(
                   WS-TOKEN-COUNT)
               MOVE WS-CUR-TKN-LEN
                   TO WS-TKN-LEN(
                   WS-TOKEN-COUNT)
           END-IF

           DISPLAY "  Tokens: "
               WS-TOKEN-COUNT
           .

      *> ============================================================
      *> TOKENIZE-ITEM: Tokenize item name into WS-ITEM-TOKENS
      *> Input: WS-TEMP2 (lowercased item)
      *> ============================================================
       TOKENIZE-ITEM.
           MOVE 0 TO WS-ITEM-TOKEN-CNT
           MOVE "N" TO WS-IN-TOKEN
           MOVE SPACES TO WS-CUR-TOKEN
           MOVE 0 TO WS-CUR-TKN-LEN
           MOVE LENGTH(TRIM(WS-TEMP2))
               TO WS-K

           PERFORM VARYING WS-J
               FROM 1 BY 1
               UNTIL WS-J > WS-K
               MOVE WS-TEMP2(WS-J:1)
                   TO WS-CH

               IF (WS-CH >= "a"
               AND WS-CH <= "z")
               OR (WS-CH >= "0"
               AND WS-CH <= "9")
               OR WS-CH = "."
               OR WS-CH = ","
                   IF WS-IN-TOKEN = "N"
                       MOVE "Y"
                           TO WS-IN-TOKEN
                       MOVE SPACES
                           TO WS-CUR-TOKEN
                       MOVE 0
                           TO WS-CUR-TKN-LEN
                   END-IF
                   ADD 1
                       TO WS-CUR-TKN-LEN
                   IF WS-CUR-TKN-LEN <= 50
                       MOVE WS-CH
                       TO WS-CUR-TOKEN(
                       WS-CUR-TKN-LEN:1)
                   END-IF
               ELSE
                   IF WS-IN-TOKEN = "Y"
                       IF WS-CUR-TKN-LEN
                           >= 2
                       AND WS-ITEM-TOKEN-CNT
                           < 50
                           ADD 1
                           TO
                           WS-ITEM-TOKEN-CNT
                           MOVE WS-CUR-TOKEN
                           TO WS-ITOK-TEXT(
                           WS-ITEM-TOKEN-CNT
                           )
                           MOVE
                           WS-CUR-TKN-LEN
                           TO WS-ITOK-LEN(
                           WS-ITEM-TOKEN-CNT
                           )
                       END-IF
                       MOVE "N"
                           TO WS-IN-TOKEN
                   END-IF
               END-IF
           END-PERFORM

      *>   Flush last token
           IF WS-IN-TOKEN = "Y"
           AND WS-CUR-TKN-LEN >= 2
           AND WS-ITEM-TOKEN-CNT < 50
               ADD 1
                   TO WS-ITEM-TOKEN-CNT
               MOVE WS-CUR-TOKEN
                   TO WS-ITOK-TEXT(
                   WS-ITEM-TOKEN-CNT)
               MOVE WS-CUR-TKN-LEN
                   TO WS-ITOK-LEN(
                   WS-ITEM-TOKEN-CNT)
           END-IF
           .

      *> ============================================================
      *> FIND-CANDIDATES: Score items, pick top 30
      *> ============================================================
       FIND-CANDIDATES.
           MOVE 0 TO WS-CAND-COUNT

           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I > WS-ITEM-COUNT

      *>       Tokenize this item
               MOVE WS-ITEM-LOWER(WS-I)
                   TO WS-TEMP2
               PERFORM TOKENIZE-ITEM

      *>       Score: token overlap
               MOVE 0 TO WS-OVERLAP
               PERFORM VARYING WS-IDX
                   FROM 1 BY 1
                   UNTIL WS-IDX
                   > WS-TOKEN-COUNT
                   PERFORM VARYING WS-IDX2
                       FROM 1 BY 1
                       UNTIL WS-IDX2
                       > WS-ITEM-TOKEN-CNT
                       IF TRIM(WS-TKN-TEXT(
                           WS-IDX))
                           = TRIM(
                           WS-ITOK-TEXT(
                           WS-IDX2))
                           ADD 1
                           TO WS-OVERLAP
                           EXIT PERFORM
                       END-IF
                   END-PERFORM
               END-PERFORM

               IF WS-OVERLAP > 0
      *>           Base score
                   COMPUTE WS-SCORE =
                       WS-OVERLAP * 100
                       / WS-TOKEN-COUNT

      *>           Bonus: full query
      *>           substring match
                   MOVE 0 TO WS-TALLY-CNT
                   INSPECT
                       TRIM(WS-ITEM-LOWER(
                       WS-I))
                       TALLYING
                       WS-TALLY-CNT
                       FOR ALL
                       TRIM(
                       WS-QUERY-LOWER)
                   IF WS-TALLY-CNT > 0
                       ADD 200 TO WS-SCORE
                   END-IF

                   PERFORM
                       INSERT-CANDIDATE
               ELSE
      *>           Try substring matching
                   MOVE 0 TO WS-SUBSTR-SCORE
                   PERFORM VARYING WS-IDX
                       FROM 1 BY 1
                       UNTIL WS-IDX
                       > WS-TOKEN-COUNT
                       IF WS-TKN-LEN(
                           WS-IDX) >= 3
                           MOVE 0
                           TO WS-TALLY-CNT
                           INSPECT
                           TRIM(
                           WS-ITEM-LOWER(
                           WS-I))
                           TALLYING
                           WS-TALLY-CNT
                           FOR ALL
                           TRIM(
                           WS-TKN-TEXT(
                           WS-IDX))
                           IF WS-TALLY-CNT
                               > 0
                               ADD 10
                               TO
                               WS-SUBSTR-SCORE
                           END-IF
                       END-IF
                   END-PERFORM
      *>           Also check item tokens
      *>           as substrings of query
                   PERFORM VARYING WS-IDX2
                       FROM 1 BY 1
                       UNTIL WS-IDX2
                       > WS-ITEM-TOKEN-CNT
                       IF WS-ITOK-LEN(
                           WS-IDX2) >= 3
                           MOVE 0
                           TO WS-TALLY-CNT
                           INSPECT
                           TRIM(
                           WS-QUERY-LOWER)
                           TALLYING
                           WS-TALLY-CNT
                           FOR ALL
                           TRIM(
                           WS-ITOK-TEXT(
                           WS-IDX2))
                           IF WS-TALLY-CNT
                               > 0
                               ADD 10
                               TO
                               WS-SUBSTR-SCORE
                           END-IF
                       END-IF
                   END-PERFORM

                   IF WS-SUBSTR-SCORE > 0
                       MOVE WS-SUBSTR-SCORE
                           TO WS-SCORE
                       PERFORM
                           INSERT-CANDIDATE
                   END-IF
               END-IF
           END-PERFORM

           DISPLAY "  Candidates: "
               WS-CAND-COUNT
           .

      *> ============================================================
      *> INSERT-CANDIDATE: Insert into top-30 sorted
      *> ============================================================
       INSERT-CANDIDATE.
           IF WS-CAND-COUNT < 30
               ADD 1 TO WS-CAND-COUNT
               MOVE WS-I
                   TO WS-CAND-IDX(
                   WS-CAND-COUNT)
               MOVE WS-SCORE
                   TO WS-CAND-SCORE(
                   WS-CAND-COUNT)
      *>       Bubble sort position
               MOVE WS-CAND-COUNT
                   TO WS-IDX
               PERFORM UNTIL
                   WS-IDX <= 1
                   COMPUTE WS-IDX2 =
                       WS-IDX - 1
                   IF WS-CAND-SCORE(
                       WS-IDX)
                       > WS-CAND-SCORE(
                       WS-IDX2)
      *>               Swap
                       MOVE WS-CAND-IDX(
                           WS-IDX)
                           TO WS-K
                       MOVE WS-CAND-SCORE(
                           WS-IDX)
                           TO WS-MATCH-SCORE
                       MOVE WS-CAND-IDX(
                           WS-IDX2)
                           TO WS-CAND-IDX(
                           WS-IDX)
                       MOVE WS-CAND-SCORE(
                           WS-IDX2)
                           TO WS-CAND-SCORE(
                           WS-IDX)
                       MOVE WS-K
                           TO WS-CAND-IDX(
                           WS-IDX2)
                       MOVE WS-MATCH-SCORE
                           TO WS-CAND-SCORE(
                           WS-IDX2)
                       SUBTRACT 1
                           FROM WS-IDX
                   ELSE
                       MOVE 1 TO WS-IDX
                   END-IF
               END-PERFORM
           ELSE
      *>       Check if better than worst
               IF WS-SCORE
                   > WS-CAND-SCORE(30)
                   MOVE WS-I
                       TO WS-CAND-IDX(30)
                   MOVE WS-SCORE
                       TO WS-CAND-SCORE(30)
      *>           Bubble up
                   MOVE 30 TO WS-IDX
                   PERFORM UNTIL
                       WS-IDX <= 1
                       COMPUTE WS-IDX2 =
                           WS-IDX - 1
                       IF WS-CAND-SCORE(
                           WS-IDX)
                           > WS-CAND-SCORE(
                           WS-IDX2)
                           MOVE
                           WS-CAND-IDX(
                           WS-IDX)
                           TO WS-K
                           MOVE
                           WS-CAND-SCORE(
                           WS-IDX)
                           TO
                           WS-MATCH-SCORE
                           MOVE
                           WS-CAND-IDX(
                           WS-IDX2)
                           TO WS-CAND-IDX(
                           WS-IDX)
                           MOVE
                           WS-CAND-SCORE(
                           WS-IDX2)
                           TO
                           WS-CAND-SCORE(
                           WS-IDX)
                           MOVE WS-K
                           TO WS-CAND-IDX(
                           WS-IDX2)
                           MOVE
                           WS-MATCH-SCORE
                           TO
                           WS-CAND-SCORE(
                           WS-IDX2)
                           SUBTRACT 1
                           FROM WS-IDX
                       ELSE
                           MOVE 1
                               TO WS-IDX
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
           .

      *> ============================================================
      *> LLM-MATCH-ITEM: Ask LLM to pick best match
      *> ============================================================
       LLM-MATCH-ITEM.
           MOVE SPACES TO WS-MATCHED-CODE
           MOVE SPACES TO WS-MATCHED-NAME

      *>   Build candidate list string
           MOVE SPACES TO WS-CAND-LIST
           MOVE 1 TO WS-CAND-LIST-LEN

           PERFORM VARYING WS-IDX
               FROM 1 BY 1
               UNTIL WS-IDX
               > WS-CAND-COUNT

               MOVE WS-CAND-IDX(WS-IDX)
                   TO WS-IDX2

      *>       Escape item name for JSON
               MOVE SPACES TO WS-ESC-BUF
               MOVE 0 TO WS-ESC-LEN
               MOVE TRIM(WS-ITEM-NAME(
                   WS-IDX2))
                   TO WS-TEMP
               MOVE LENGTH(TRIM(WS-TEMP))
                   TO WS-K
               PERFORM VARYING WS-J
                   FROM 1 BY 1
                   UNTIL WS-J > WS-K
                   IF WS-TEMP(WS-J:1)
                       = WS-QT
                       ADD 1 TO WS-ESC-LEN
                       MOVE X"5C"
                       TO WS-ESC-BUF(
                       WS-ESC-LEN:1)
                       ADD 1 TO WS-ESC-LEN
                       MOVE WS-QT
                       TO WS-ESC-BUF(
                       WS-ESC-LEN:1)
                   ELSE
                   IF WS-TEMP(WS-J:1)
                       = X"5C"
                       ADD 1 TO WS-ESC-LEN
                       MOVE X"5C"
                       TO WS-ESC-BUF(
                       WS-ESC-LEN:1)
                       ADD 1 TO WS-ESC-LEN
                       MOVE X"5C"
                       TO WS-ESC-BUF(
                       WS-ESC-LEN:1)
                   ELSE
                       ADD 1 TO WS-ESC-LEN
                       MOVE WS-TEMP(WS-J:1)
                       TO WS-ESC-BUF(
                       WS-ESC-LEN:1)
                   END-IF
                   END-IF
               END-PERFORM

      *>       Add "\n- name" to list
               IF WS-CAND-LIST-LEN > 1
                   STRING
                       WS-NL "- "
                       WS-ESC-BUF(
                       1:WS-ESC-LEN)
                       DELIMITED SIZE
                       INTO WS-CAND-LIST
                       WITH POINTER
                       WS-CAND-LIST-LEN
                   END-STRING
               ELSE
                   STRING
                       "- "
                       WS-ESC-BUF(
                       1:WS-ESC-LEN)
                       DELIMITED SIZE
                       INTO WS-CAND-LIST
                       WITH POINTER
                       WS-CAND-LIST-LEN
                   END-STRING
               END-IF
           END-PERFORM
           SUBTRACT 1
               FROM WS-CAND-LIST-LEN

      *>   Build LLM request
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
               WS-QT "system" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               "You are a product "
               "matching assistant."
               " Given a query and "
               "a list of product "
               "names, pick the ONE"
               " product that best "
               "matches the query. "
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "Return ONLY the "
               "exact product name "
               "from the list, "
               "nothing else. "
               "If none match, "
               "return: NONE"
               WS-QT "},{"
               WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT "Query: "
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Escape query for JSON
           MOVE SPACES TO WS-ESC-BUF
           MOVE 0 TO WS-ESC-LEN
           MOVE TRIM(WS-QUERY)
               TO WS-TEMP
           MOVE LENGTH(TRIM(WS-TEMP))
               TO WS-K
           PERFORM VARYING WS-J
               FROM 1 BY 1
               UNTIL WS-J > WS-K
               IF WS-TEMP(WS-J:1)
                   = WS-QT
                   ADD 1 TO WS-ESC-LEN
                   MOVE X"5C"
                   TO WS-ESC-BUF(
                   WS-ESC-LEN:1)
                   ADD 1 TO WS-ESC-LEN
                   MOVE WS-QT
                   TO WS-ESC-BUF(
                   WS-ESC-LEN:1)
               ELSE
               IF WS-TEMP(WS-J:1)
                   = X"5C"
                   ADD 1 TO WS-ESC-LEN
                   MOVE X"5C"
                   TO WS-ESC-BUF(
                   WS-ESC-LEN:1)
                   ADD 1 TO WS-ESC-LEN
                   MOVE X"5C"
                   TO WS-ESC-BUF(
                   WS-ESC-LEN:1)
               ELSE
                   ADD 1 TO WS-ESC-LEN
                   MOVE WS-TEMP(WS-J:1)
                   TO WS-ESC-BUF(
                   WS-ESC-LEN:1)
               END-IF
               END-IF
           END-PERFORM

           STRING
               WS-ESC-BUF(
               1:WS-ESC-LEN)
               WS-NL WS-NL
               "Products:" WS-NL
               WS-CAND-LIST(
               1:WS-CAND-LIST-LEN)
               WS-QT "}],"
               WS-QT "temperature"
               WS-QT ":0,"
               WS-QT "max_tokens"
               WS-QT ":150}"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Write request and call OpenAI
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

           DISPLAY "  Calling LLM..."
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

      *>   Parse LLM response
           MOVE "llm_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp"
               TO WS-WORK-PATH

           IF WS-JLEN = 0
               DISPLAY "  LLM: empty resp!"
               EXIT PARAGRAPH
           END-IF

      *>   Check for error
           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-JBUF(1:WS-JLEN)
               TALLYING WS-TALLY-CNT
               FOR ALL '"error"'
           IF WS-TALLY-CNT > 0
               DISPLAY "  LLM ERR: "
                   TRIM(WS-JBUF)(1:300)
               EXIT PARAGRAPH
           END-IF

      *>   Extract content
           MOVE "content"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL

           IF TRIM(WS-JVAL) = SPACES
           OR TRIM(WS-JVAL) = "null"
               DISPLAY "  LLM: no content!"
               EXIT PARAGRAPH
           END-IF

           MOVE TRIM(WS-JVAL)
               TO WS-TEMP
           DISPLAY "  LLM match: "
               TRIM(WS-TEMP)(1:200)

           IF TRIM(WS-TEMP) = "NONE"
               EXIT PARAGRAPH
           END-IF

      *>   Find matched item in our list
           MOVE LOWER-CASE(
               TRIM(WS-TEMP))
               TO WS-TEMP2

           PERFORM VARYING WS-IDX
               FROM 1 BY 1
               UNTIL WS-IDX
               > WS-ITEM-COUNT
               IF WS-ITEM-LOWER(WS-IDX)
                   = TRIM(WS-TEMP2)
                   MOVE WS-ITEM-CODE(
                       WS-IDX)
                       TO WS-MATCHED-CODE
                   MOVE WS-ITEM-NAME(
                       WS-IDX)
                       TO WS-MATCHED-NAME
                   DISPLAY "  Matched: "
                       TRIM(
                       WS-MATCHED-NAME)
                       (1:100)
                   EXIT PERFORM
               END-IF
           END-PERFORM

      *>   If exact match failed, try
      *>   partial (substring) match
           IF TRIM(WS-MATCHED-CODE)
               = SPACES
      *>       Check candidates
               PERFORM VARYING WS-IDX
                   FROM 1 BY 1
                   UNTIL WS-IDX
                   > WS-CAND-COUNT
                   MOVE WS-CAND-IDX(
                       WS-IDX)
                       TO WS-IDX2
                   MOVE LOWER-CASE(
                       TRIM(WS-ITEM-NAME(
                       WS-IDX2)))
                       TO WS-TEMP
                   MOVE 0
                       TO WS-TALLY-CNT
                   INSPECT TRIM(WS-TEMP)
                       TALLYING
                       WS-TALLY-CNT
                       FOR ALL
                       TRIM(WS-TEMP2)
                   IF WS-TALLY-CNT > 0
                       MOVE
                       WS-ITEM-CODE(
                       WS-IDX2)
                       TO WS-MATCHED-CODE
                       MOVE
                       WS-ITEM-NAME(
                       WS-IDX2)
                       TO WS-MATCHED-NAME
                       DISPLAY
                       "  Partial: "
                       TRIM(
                       WS-MATCHED-NAME)
                       (1:100)
                       EXIT PERFORM
                   END-IF
      *>           Also check reverse
                   MOVE 0
                       TO WS-TALLY-CNT
                   INSPECT
                       TRIM(WS-TEMP2)
                       TALLYING
                       WS-TALLY-CNT
                       FOR ALL
                       TRIM(WS-TEMP)
                   IF WS-TALLY-CNT > 0
                       MOVE
                       WS-ITEM-CODE(
                       WS-IDX2)
                       TO WS-MATCHED-CODE
                       MOVE
                       WS-ITEM-NAME(
                       WS-IDX2)
                       TO WS-MATCHED-NAME
                       DISPLAY
                       "  PartialR: "
                       TRIM(
                       WS-MATCHED-NAME)
                       (1:100)
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           END-IF
           .

      *> ============================================================
      *> LOOKUP-CITIES: Find cities for matched item
      *> ============================================================
       LOOKUP-CITIES.
           MOVE 0 TO WS-RES-CITY-COUNT

           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I > WS-CONN-COUNT
               IF TRIM(WS-CONN-ITEM(WS-I))
                   = TRIM(WS-MATCHED-CODE)
      *>           Find city name
                   PERFORM VARYING WS-J
                       FROM 1 BY 1
                       UNTIL WS-J
                       > WS-CITY-COUNT
                       IF TRIM(
                           WS-CITY-CODE(
                           WS-J))
                           = TRIM(
                           WS-CONN-CITY(
                           WS-I))
                           ADD 1
                           TO
                           WS-RES-CITY-COUNT
                           IF
                           WS-RES-CITY-COUNT
                           <= 100
                           MOVE TRIM(
                           WS-CITY-NAME(
                           WS-J))
                           TO
                           WS-RES-CITY-NM(
                           WS-RES-CITY-COUNT
                           )
                           END-IF
                           EXIT PERFORM
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> SORT-RESULT-CITIES: Simple bubble sort
      *> ============================================================
       SORT-RESULT-CITIES.
           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I
               >= WS-RES-CITY-COUNT
               PERFORM VARYING WS-J
                   FROM 1 BY 1
                   UNTIL WS-J
                   >= WS-RES-CITY-COUNT
                   COMPUTE WS-IDX =
                       WS-J + 1
                   IF WS-RES-CITY-NM(WS-J)
                       > WS-RES-CITY-NM(
                       WS-IDX)
                       MOVE
                       WS-RES-CITY-NM(
                       WS-J)
                       TO WS-SORT-TEMP
                       MOVE
                       WS-RES-CITY-NM(
                       WS-IDX)
                       TO
                       WS-RES-CITY-NM(
                       WS-J)
                       MOVE WS-SORT-TEMP
                       TO
                       WS-RES-CITY-NM(
                       WS-IDX)
                   END-IF
               END-PERFORM
           END-PERFORM
           .

      *> ============================================================
      *> BUILD-CITY-RESPONSE: CSV city list
      *> ============================================================
       BUILD-CITY-RESPONSE.
           MOVE SPACES TO WS-RESULT-CITIES
           MOVE 1 TO WS-PTR

           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I
               > WS-RES-CITY-COUNT
               IF WS-I > 1
                   STRING ","
                       DELIMITED SIZE
                       INTO WS-RESULT-CITIES
                       WITH POINTER WS-PTR
                   END-STRING
               END-IF
               STRING
                   TRIM(WS-RES-CITY-NM(
                   WS-I))
                   DELIMITED SIZE
                   INTO WS-RESULT-CITIES
                   WITH POINTER WS-PTR
               END-STRING

      *>       Check 500 byte limit
               IF WS-PTR > 495
                   EXIT PERFORM
               END-IF
           END-PERFORM

           COMPUTE WS-RESULT-LEN =
               WS-PTR - 1

           DISPLAY "  Response ("
               WS-RESULT-LEN
               " bytes): "
               WS-RESULT-CITIES(
               1:WS-RESULT-LEN)

      *>   Escape for JSON and build
      *>   response
           MOVE SPACES TO WS-ESC-BUF
           MOVE 0 TO WS-ESC-LEN
           PERFORM VARYING WS-J
               FROM 1 BY 1
               UNTIL WS-J > WS-RESULT-LEN
               IF WS-RESULT-CITIES(
                   WS-J:1) = WS-QT
                   ADD 1 TO WS-ESC-LEN
                   MOVE X"5C"
                   TO WS-ESC-BUF(
                   WS-ESC-LEN:1)
                   ADD 1 TO WS-ESC-LEN
                   MOVE WS-QT
                   TO WS-ESC-BUF(
                   WS-ESC-LEN:1)
               ELSE
                   ADD 1 TO WS-ESC-LEN
                   MOVE
                   WS-RESULT-CITIES(
                   WS-J:1)
                   TO WS-ESC-BUF(
                   WS-ESC-LEN:1)
               END-IF
           END-PERFORM

           MOVE SPACES TO WS-RESPONSE-MSG
           STRING
               "{" WS-QT "output"
               WS-QT ":"
               WS-QT
               WS-ESC-BUF(1:WS-ESC-LEN)
               WS-QT "}"
               DELIMITED SIZE
               INTO WS-RESPONSE-MSG
           END-STRING
           .

      *> ============================================================
      *> CHECK-RESULT: Poll for verification result
      *> ============================================================
       CHECK-RESULT.
           MOVE SPACES TO WS-PAYLOAD
           STRING
               "{"
               WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY)
               WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT "negotiations"
               WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT "check" WS-QT
               "}}"
               DELIMITED SIZE
               INTO WS-PAYLOAD
           END-STRING

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
               FROM WS-PAYLOAD
           CLOSE WORK-FILE

           INITIALIZE WS-CMD
           STRING
               "curl -s "
               "-o check_resp.json"
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

           MOVE "check_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp"
               TO WS-WORK-PATH

           DISPLAY "  Check: "
               TRIM(WS-JBUF)(1:500)

           MOVE SPACES TO WS-RESPONSE-MSG
           MOVE TRIM(WS-JBUF)
               TO WS-RESPONSE-MSG
           .

      *> ============================================================
      *> SEND-HTTP-RESP: Send HTTP response to client
      *> ============================================================
       SEND-HTTP-RESP.
           INITIALIZE WS-WRITE-BUF
           STRING
               "HTTP/1.1 200 OK"
               X"0D0A"
               "Content-Type: "
               "application/json"
               X"0D0A"
               "Access-Control-"
               "Allow-Origin: *"
               X"0D0A"
               "Connection: close"
               X"0D0A"
               X"0D0A"
               TRIM(WS-RESPONSE-MSG)
               DELIMITED SIZE
               INTO WS-WRITE-BUF
           END-STRING

           MOVE LENGTH(
               TRIM(WS-WRITE-BUF))
               TO WS-WRITE-LEN

           CALL "write" USING
               BY VALUE WS-CLIENT-FD
               BY REFERENCE WS-WRITE-BUF
               BY VALUE WS-WRITE-LEN
               RETURNING WS-RESULT
           END-CALL

           DISPLAY "  Sent "
               WS-RESULT " bytes"
           .

       COPY JSONREAD-PROC.

       COPY JSONPARSE-PROC.

       COPY ENVLOAD-PROC.
