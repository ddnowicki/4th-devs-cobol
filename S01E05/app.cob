       IDENTIFICATION DIVISION.
       PROGRAM-ID. S01E05-RAILWAY.
      *> ============================================================
      *> S01E05 - Railway Route Activation (Pure COBOL)
      *> Steps: reconfigure -> getstatus -> setstatus RTOPEN -> save
      *> Handles 503/429 retries with Retry-After header parsing
      *> Rate limit: 1 req per 30s window, penalty for early retry
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
       01  WS-ROUTE                PIC X(10) VALUE "X-01".
       01  WS-TASK-NAME            PIC X(20) VALUE "railway".

      *> -- System command --
       01  WS-CMD                  PIC X(4000).

      *> -- Request/Response --
       01  WS-PAYLOAD              PIC X(4000).
       01  WS-RESP-BUF             PIC X(8000).
       01  WS-RESP-LEN             PIC 9(5) VALUE 0.
       01  WS-STATUS-CODE          PIC X(3).

      *> -- Retry --
       01  WS-MAX-RETRIES          PIC 9(2) VALUE 20.
       01  WS-ATTEMPT              PIC 9(2).
       01  WS-DONE                 PIC X VALUE "N".

      *> -- Retry-After parsing --
       01  WS-RETRY-AFTER          PIC 9(3) VALUE 0.
       01  WS-RETRY-STR            PIC X(10).
       01  WS-SLEEP-SECS           PIC 9(3).

      *> -- File reading --
       01  WS-EOF                  PIC X VALUE "N".
       01  WS-LINE                 PIC X(4000).
       01  WS-K                    PIC 9(5).

      *> -- Flag --
       01  WS-FLAG-FOUND           PIC X VALUE "N".
       01  WS-TALLY-CNT            PIC 9(4).

      *> -- Steps --
       01  WS-STEP-NAME            PIC X(30).
       01  WS-ACTION-JSON          PIC X(500).

      *> -- Sleep (cross-platform via GnuCOBOL C$SLEEP) --

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S01E05 RAILWAY - COBOL ==="

           ACCEPT WS-HUB-KEY FROM ENVIRONMENT "HUB_API_KEY"

           IF WS-HUB-KEY = SPACES
               DISPLAY "BLAD: Ustaw HUB_API_KEY!"
               STOP RUN
           END-IF

           ACCEPT WS-HUB-URL FROM ENVIRONMENT "HUB_API_URL"

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

      *>   Step 1: reconfigure
           MOVE "reconfigure" TO WS-STEP-NAME
           INITIALIZE WS-ACTION-JSON
           STRING
               "{" WS-QT "action" WS-QT ":"
               WS-QT "reconfigure" WS-QT ","
               WS-QT "route" WS-QT ":"
               WS-QT TRIM(WS-ROUTE) WS-QT "}"
               DELIMITED SIZE INTO WS-ACTION-JSON
           END-STRING
           PERFORM SEND-ACTION-WITH-RETRY
           IF WS-FLAG-FOUND = "Y"
               STOP RUN
           END-IF

      *>   Step 2: getstatus
           MOVE "getstatus" TO WS-STEP-NAME
           INITIALIZE WS-ACTION-JSON
           STRING
               "{" WS-QT "action" WS-QT ":"
               WS-QT "getstatus" WS-QT ","
               WS-QT "route" WS-QT ":"
               WS-QT TRIM(WS-ROUTE) WS-QT "}"
               DELIMITED SIZE INTO WS-ACTION-JSON
           END-STRING
           PERFORM SEND-ACTION-WITH-RETRY
           IF WS-FLAG-FOUND = "Y"
               STOP RUN
           END-IF

      *>   Step 3: setstatus RTOPEN
           MOVE "setstatus RTOPEN" TO WS-STEP-NAME
           INITIALIZE WS-ACTION-JSON
           STRING
               "{" WS-QT "action" WS-QT ":"
               WS-QT "setstatus" WS-QT ","
               WS-QT "route" WS-QT ":"
               WS-QT TRIM(WS-ROUTE) WS-QT ","
               WS-QT "value" WS-QT ":"
               WS-QT "RTOPEN" WS-QT "}"
               DELIMITED SIZE INTO WS-ACTION-JSON
           END-STRING
           PERFORM SEND-ACTION-WITH-RETRY
           IF WS-FLAG-FOUND = "Y"
               STOP RUN
           END-IF

      *>   Step 4: save
           MOVE "save" TO WS-STEP-NAME
           INITIALIZE WS-ACTION-JSON
           STRING
               "{" WS-QT "action" WS-QT ":"
               WS-QT "save" WS-QT ","
               WS-QT "route" WS-QT ":"
               WS-QT TRIM(WS-ROUTE) WS-QT "}"
               DELIMITED SIZE INTO WS-ACTION-JSON
           END-STRING
           PERFORM SEND-ACTION-WITH-RETRY
           IF WS-FLAG-FOUND = "Y"
               STOP RUN
           END-IF

           DISPLAY " "
           DISPLAY "Wszystkie kroki wykonane."
           DISPLAY "=== ZAKONCZONO ==="
           STOP RUN.

      *> ============================================================
      *> SEND-ACTION-WITH-RETRY: POST action, retry on 503/429
      *> ============================================================
       SEND-ACTION-WITH-RETRY.
           DISPLAY " "
           DISPLAY ">> " TRIM(WS-STEP-NAME) ": "
               TRIM(WS-ACTION-JSON)

      *>   Build full payload
           INITIALIZE WS-PAYLOAD
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME) WS-QT ","
               WS-QT "answer" WS-QT ":"
               TRIM(WS-ACTION-JSON)
               "}"
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
               DISPLAY "  Przekroczono limit prob!"
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

      *>   Handle 503 - server overloaded (no penalty, quick retry)
           IF WS-STATUS-CODE = "503"
               DISPLAY "  [" WS-ATTEMPT "] 503, czekam 1s"
               MOVE 1 TO WS-SLEEP-SECS
               CALL "C$SLEEP" USING WS-SLEEP-SECS
               EXIT PARAGRAPH
           END-IF

      *>   Handle 429 - rate limit (respect Retry-After!)
           IF WS-STATUS-CODE = "429"
               IF WS-RETRY-AFTER < 1
                   MOVE 30 TO WS-RETRY-AFTER
               END-IF
               DISPLAY "  [" WS-ATTEMPT "] 429, czekam "
                   WS-RETRY-AFTER "s (Retry-After)"
               MOVE WS-RETRY-AFTER TO WS-SLEEP-SECS
               CALL "C$SLEEP" USING WS-SLEEP-SECS
               EXIT PARAGRAPH
           END-IF

      *>   Read response body
           MOVE "resp.json" TO WS-WORK-PATH
           PERFORM READ-RESPONSE-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           DISPLAY "  => " TRIM(WS-RESP-BUF)(1:500)

      *>   Check for flag {FLG:...}
           MOVE 0 TO WS-TALLY-CNT
           IF WS-RESP-LEN > 0
               INSPECT WS-RESP-BUF(1:WS-RESP-LEN)
                   TALLYING WS-TALLY-CNT FOR ALL "FLG"
           END-IF
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-FLAG-FOUND
               MOVE "Y" TO WS-DONE
               DISPLAY " "
               DISPLAY "*** FLAGA ZNALEZIONA! ***"
               EXIT PARAGRAPH
           END-IF

      *>   Step done (got a non-retry response)
           MOVE "Y" TO WS-DONE
           .

      *> ============================================================
      *> READ-HTTP-HEADERS: Parse status code + Retry-After
      *> from headers.tmp
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
      *>               Parse HTTP status line
                       IF WS-LINE(1:5) = "HTTP/"
                           PERFORM EXTRACT-STATUS-CODE
                       END-IF
      *>               Parse Retry-After header
                       IF WS-LINE(1:13) = "Retry-After: "
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
      *>   Find space after protocol version
           MOVE 6 TO WS-K
           PERFORM UNTIL WS-K > 15
               OR WS-LINE(WS-K:1) = " "
               ADD 1 TO WS-K
           END-PERFORM
      *>   Skip the space
           ADD 1 TO WS-K
      *>   Next 3 chars are the status code
           IF WS-K <= 12
               MOVE WS-LINE(WS-K:3) TO WS-STATUS-CODE
           END-IF
           .

      *> ============================================================
      *> EXTRACT-RETRY-AFTER: Get seconds from "Retry-After: NN"
      *> ============================================================
       EXTRACT-RETRY-AFTER.
           MOVE SPACES TO WS-RETRY-STR
           MOVE TRIM(WS-LINE(14:)) TO WS-RETRY-STR
      *>   Remove trailing CR/LF
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
