       IDENTIFICATION DIVISION.
       PROGRAM-ID. S02E05-DRONE.
      *> ============================================================
      *> S02E05 - Drone Mission
      *> 1. Fetch drone API docs (HTML -> strip tags)
      *> 2. GPT-4o vision: find dam on map grid
      *> 3. Build deterministic drone instructions
      *> 4. Submit + neighbor fallback for coord errors
      *> 5. GPT-4.1-mini agent for non-coord errors
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
       01  WS-DOCS-URL             PIC X(200).
       01  WS-MAP-URL              PIC X(300).

      *> === JSON Parsing (copybooks) ===
       COPY JSONPARSE-WS.
       COPY JSONREAD-WS.
       COPY JSONESCAPE-WS.

      *> === Control Flow ===
       01  WS-PTR                  PIC 9(5).
       01  WS-TALLY-CNT            PIC 9(4).

       01  WS-CMD                  PIC X(4000).

      *> === Task Data ===
       01  WS-DOCS-BUF             PIC X(8000).
       01  WS-DOCS-LEN             PIC 9(5).

      *> --- HTML strip state ---
       01  WS-HTML-RAW             PIC X(16000).
       01  WS-HTML-LEN             PIC 9(5).
       01  WS-HTML-I               PIC 9(5).
       01  WS-IN-TAG               PIC X VALUE "N".
       01  WS-LAST-SPACE           PIC X VALUE "N".
       01  WS-IN-STYLE             PIC X VALUE "N".
       01  WS-TAG-BUF              PIC X(20).
       01  WS-TAG-LEN              PIC 9(2).

      *> === Vision & Grid ===
       01  WS-TARGET-COL           PIC 9(2).
       01  WS-TARGET-ROW           PIC 9(2).
       01  WS-GRID-COLS            PIC 9(2) VALUE 4.
       01  WS-GRID-ROWS            PIC 9(2) VALUE 4.

      *> --- Instructions ---
       01  WS-INSTR-BUF            PIC X(4000).
       01  WS-INSTR-LEN            PIC 9(5).
       01  WS-INSTR-COL            PIC 9(2).
       01  WS-INSTR-ROW            PIC 9(2).

      *> --- Candidate coordinates ---
       01  WS-CAND-TABLE.
           05 WS-CAND OCCURS 20 TIMES.
              10 WS-CAND-C          PIC 9(2).
              10 WS-CAND-R          PIC 9(2).
       01  WS-CAND-COUNT           PIC 9(2).
       01  WS-CAND-IDX             PIC 9(2).

      *> --- Neighbor offsets (8 directions) ---
       01  WS-NEIGH-DC.
           05 FILLER PIC S9(2) VALUE +1.
           05 FILLER PIC S9(2) VALUE -1.
           05 FILLER PIC S9(2) VALUE +0.
           05 FILLER PIC S9(2) VALUE +0.
           05 FILLER PIC S9(2) VALUE +1.
           05 FILLER PIC S9(2) VALUE +1.
           05 FILLER PIC S9(2) VALUE -1.
           05 FILLER PIC S9(2) VALUE -1.
       01  WS-NDC REDEFINES WS-NEIGH-DC.
           05 WS-NDC-V OCCURS 8 TIMES
                                   PIC S9(2).
       01  WS-NEIGH-DR.
           05 FILLER PIC S9(2) VALUE +0.
           05 FILLER PIC S9(2) VALUE +0.
           05 FILLER PIC S9(2) VALUE +1.
           05 FILLER PIC S9(2) VALUE -1.
           05 FILLER PIC S9(2) VALUE +1.
           05 FILLER PIC S9(2) VALUE -1.
           05 FILLER PIC S9(2) VALUE +1.
           05 FILLER PIC S9(2) VALUE -1.
       01  WS-NDR REDEFINES WS-NEIGH-DR.
           05 WS-NDR-V OCCURS 8 TIMES
                                   PIC S9(2).
       01  WS-NC                   PIC S9(3).
       01  WS-NR                   PIC S9(3).
       01  WS-NI                   PIC 9(2).

      *> === API Response ===
       01  WS-RESP-BUF             PIC X(16000).
       01  WS-RESP-LEN             PIC 9(5).

       01  WS-COORD-ERR            PIC X VALUE "N".
       01  WS-FLAG-FOUND           PIC X VALUE "N".
       01  WS-REQ-JSON             PIC X(32000).

      *> === Agent Loop ===
       01  WS-DOCS-ESC             PIC X(16000).
       01  WS-DOCS-ESC-LEN        PIC 9(5).

      *> --- Agent loop ---
       01  WS-AG-STEP              PIC 9(2) VALUE 0.
       01  WS-AG-MAX               PIC 9(2) VALUE 10.

      *> --- Last error msg ---
       01  WS-LAST-ERR             PIC X(4000).
       01  WS-LAST-ERR-LEN        PIC 9(5).

      *> --- Last instructions for agent ---
       01  WS-LAST-INSTR           PIC X(4000).

      *> --- Agent response content ---
       01  WS-AG-CONTENT           PIC X(8000).

      *> --- Retry ---
       01  WS-MAX-RETRIES          PIC 9(2) VALUE 5.
       01  WS-RETRY-DELAY          PIC 9(2) VALUE 3.
       01  WS-RETRY-CT             PIC 9(2) VALUE 0.
       01  WS-SLEEP-SECS           PIC 9(3).

      *> --- Temp col/row as text ---
       01  WS-COL-TXT              PIC 9(2).
       01  WS-ROW-TXT              PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S02E05 DRONE MISSION ==="

           PERFORM LOAD-ENV-VARS

      *>   Build URLs
           INITIALIZE WS-DOCS-URL
           STRING TRIM(WS-HUB-URL)
               "/dane/drone.html"
               DELIMITED SIZE INTO WS-DOCS-URL
           END-STRING

           INITIALIZE WS-MAP-URL
           STRING TRIM(WS-HUB-URL)
               "/data/"
               TRIM(WS-HUB-KEY)
               "/drone.png"
               DELIMITED SIZE INTO WS-MAP-URL
           END-STRING

      *>   Stage 1: Fetch drone docs
           PERFORM FETCH-DRONE-DOCS

      *>   Stage 2: Vision analysis
           PERFORM ANALYZE-MAP

      *>   Stage 3+4: Submit + neighbors
           PERFORM BUILD-CANDIDATES
           PERFORM TRY-CANDIDATES

      *>   Stage 5: Agent if needed
           IF WS-FLAG-FOUND = "N"
               PERFORM RUN-AGENT
           END-IF

           DISPLAY " "
           DISPLAY "=== DONE ==="
           STOP RUN.

      *> ============================================================
      *> FETCH-DRONE-DOCS
      *> ============================================================
       FETCH-DRONE-DOCS.
           DISPLAY "[STAGE 1] Fetching drone docs..."

           INITIALIZE WS-CMD
           STRING
               "curl -s -o drone_docs.html "
               TRIM(WS-DOCS-URL)
               DELIMITED SIZE INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Read HTML file
           MOVE "drone_docs.html"
               TO WS-WORK-PATH
           MOVE SPACES TO WS-HTML-RAW
           MOVE 0 TO WS-HTML-LEN
           MOVE "N" TO WS-EOF

           OPEN INPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "  Cannot read docs!"
               MOVE "work.tmp" TO WS-WORK-PATH
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL WS-EOF = "Y"
               READ WORK-FILE INTO WS-LINE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       MOVE LENGTH(
                           TRIM(WS-LINE TRAILING))
                           TO WS-K
                       IF WS-K > 0
                       AND WS-HTML-LEN + WS-K
                           + 1 <= 16000
                           IF WS-HTML-LEN > 0
                               ADD 1
                                   TO WS-HTML-LEN
                               MOVE " "
                                   TO WS-HTML-RAW(
                                   WS-HTML-LEN:1)
                           END-IF
                           MOVE WS-LINE(1:WS-K)
                               TO WS-HTML-RAW(
                               WS-HTML-LEN + 1:
                               WS-K)
                           ADD WS-K
                               TO WS-HTML-LEN
                       END-IF
               END-READ
           END-PERFORM
           CLOSE WORK-FILE
           MOVE "N" TO WS-EOF
           MOVE "work.tmp" TO WS-WORK-PATH

           DISPLAY "  HTML: " WS-HTML-LEN " chars"

      *>   Strip HTML tags
           PERFORM STRIP-HTML-TAGS

           DISPLAY "  Docs: " WS-DOCS-LEN " chars"
           IF WS-DOCS-LEN > 500
               DISPLAY "  " WS-DOCS-BUF(1:500)
           ELSE
               DISPLAY "  "
                   WS-DOCS-BUF(1:WS-DOCS-LEN)
           END-IF
           .

      *> ============================================================
      *> STRIP-HTML-TAGS: Remove <...> tags, collapse
      *> whitespace. Input: WS-HTML-RAW/LEN
      *> Output: WS-DOCS-BUF/LEN
      *> ============================================================
       STRIP-HTML-TAGS.
           MOVE SPACES TO WS-DOCS-BUF
           MOVE 0 TO WS-DOCS-LEN
           MOVE "N" TO WS-IN-TAG
           MOVE "N" TO WS-LAST-SPACE
           MOVE "N" TO WS-IN-STYLE
           MOVE SPACES TO WS-TAG-BUF
           MOVE 0 TO WS-TAG-LEN

           PERFORM VARYING WS-HTML-I FROM 1 BY 1
               UNTIL WS-HTML-I > WS-HTML-LEN
               OR WS-DOCS-LEN >= 7900

               IF WS-HTML-RAW(WS-HTML-I:1) = "<"
                   MOVE "Y" TO WS-IN-TAG
                   MOVE SPACES TO WS-TAG-BUF
                   MOVE 0 TO WS-TAG-LEN
               ELSE IF WS-HTML-RAW(WS-HTML-I:1)
                   = ">"
                   MOVE "N" TO WS-IN-TAG
      *>           Check tag name for style
                   IF WS-TAG-LEN >= 5
                   AND WS-TAG-BUF(1:5)
                       = "style"
                       MOVE "Y"
                           TO WS-IN-STYLE
                   END-IF
                   IF WS-TAG-LEN >= 6
                   AND WS-TAG-BUF(1:6)
                       = "/style"
                       MOVE "N"
                           TO WS-IN-STYLE
                   END-IF
                   IF WS-IN-STYLE = "N"
                   AND WS-LAST-SPACE = "N"
                   AND WS-DOCS-LEN > 0
                       ADD 1 TO WS-DOCS-LEN
                       MOVE " "
                           TO WS-DOCS-BUF(
                           WS-DOCS-LEN:1)
                       MOVE "Y"
                           TO WS-LAST-SPACE
                   END-IF
               ELSE IF WS-IN-TAG = "Y"
      *>           Collect tag name chars
                   IF WS-TAG-LEN < 20
                       ADD 1 TO WS-TAG-LEN
                       MOVE WS-HTML-RAW(
                           WS-HTML-I:1)
                           TO WS-TAG-BUF(
                           WS-TAG-LEN:1)
                   END-IF
               ELSE IF WS-IN-STYLE = "N"
                   IF WS-HTML-RAW(WS-HTML-I:1)
                       = " "
                   OR WS-HTML-RAW(WS-HTML-I:1)
                       = X"0A"
                   OR WS-HTML-RAW(WS-HTML-I:1)
                       = X"0D"
                   OR WS-HTML-RAW(WS-HTML-I:1)
                       = X"09"
                       IF WS-LAST-SPACE = "N"
                       AND WS-DOCS-LEN > 0
                           ADD 1 TO WS-DOCS-LEN
                           MOVE " "
                               TO WS-DOCS-BUF(
                               WS-DOCS-LEN:1)
                           MOVE "Y"
                               TO WS-LAST-SPACE
                       END-IF
                   ELSE
                       ADD 1 TO WS-DOCS-LEN
                       MOVE WS-HTML-RAW(
                           WS-HTML-I:1)
                           TO WS-DOCS-BUF(
                           WS-DOCS-LEN:1)
                       MOVE "N"
                           TO WS-LAST-SPACE
                   END-IF
               END-IF
               END-IF
               END-IF
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> ANALYZE-MAP: GPT-4o vision to find dam
      *> ============================================================
       ANALYZE-MAP.
           DISPLAY " "
           DISPLAY "[STAGE 2] Analyzing map with "
               "GPT-4o vision..."
           DISPLAY "  Map URL: "
               TRIM(WS-MAP-URL)(1:80)

      *>   Build vision API request
      *>   GPT-4o can access URLs directly
           MOVE SPACES TO WS-REQ-JSON
           MOVE 1 TO WS-PTR

           STRING
               '{"model":"gpt-4o",'
               '"messages":[{"role":"user",'
               '"content":['
               '{"type":"text","text":"'
               'This aerial image shows a map '
               'divided by red grid lines into '
               'sectors.'
               DELIMITED SIZE INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-NL WS-NL
               'IMPORTANT: The grid has EXACTLY '
               '4 rows and 4 columns.'
               WS-NL
               'Do NOT try to count the grid '
               'lines yourself.'
               WS-NL WS-NL
               DELIMITED SIZE INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               'Your job: find which sector '
               'contains the DAM.'
               WS-NL
               'The dam has ARTIFICIALLY '
               'INTENSIFIED teal/cyan water '
               'color - noticeably brighter '
               'and more saturated than any '
               'other water.'
               DELIMITED SIZE INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-NL
               'Do NOT confuse it with normal '
               'water bodies or the power '
               'plant.'
               WS-NL
               'Look for unnaturally vivid '
               'teal/cyan water.'
               WS-NL WS-NL
               DELIMITED SIZE INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               'Report the sector as '
               'target_column and target_row '
               'where column 1 is leftmost '
               'and row 1 is topmost.'
               WS-NL WS-NL
               'Reply in JSON format ONLY:'
               WS-NL
               DELIMITED SIZE INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               '{' X"5C" WS-QT
               'target_column' X"5C" WS-QT
               ': <number>, '
               X"5C" WS-QT
               'target_row' X"5C" WS-QT
               ': <number>}'
               '"},'
               DELIMITED SIZE INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Image URL part
           STRING
               '{"type":"image_url",'
               '"image_url":{"url":"'
               TRIM(WS-MAP-URL)
               '","detail":"high"}}'
               ']}],'
               '"max_tokens":200,'
               '"temperature":0}'
               DELIMITED SIZE INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           COMPUTE WS-K = WS-PTR - 1

      *>   Write request
           MOVE "vision_req.json"
               TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "OPEN vision_req.json "
                   "failed: " WS-FS
               STOP RUN
           END-IF
           WRITE WORK-REC FROM WS-REQ-JSON
           CLOSE WORK-FILE

      *>   Send with retry
           MOVE 0 TO WS-RETRY-CT
           PERFORM UNTIL WS-RETRY-CT >=
                   WS-MAX-RETRIES
               DISPLAY "  Calling GPT-4o..."
               INITIALIZE WS-CMD
               STRING
                   "curl -s -o vision_resp.json"
                   " -X POST "
                   TRIM(WS-OPENAI-URL)
                   " -H " WS-QT
                   "Content-Type: "
                   "application/json" WS-QT
                   " -H " WS-QT
                   "Authorization: Bearer "
                   TRIM(WS-OPENAI-KEY) WS-QT
                   " -d @vision_req.json"
                   DELIMITED SIZE INTO WS-CMD
               END-STRING
               CALL "SYSTEM" USING WS-CMD

               MOVE "vision_resp.json"
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
               IF WS-RETRY-CT <
                       WS-MAX-RETRIES
                   DISPLAY "  Retry " WS-RETRY-CT
                   MOVE WS-RETRY-DELAY
                       TO WS-SLEEP-SECS
                   CALL "C$SLEEP"
                       USING WS-SLEEP-SECS
               END-IF
           END-PERFORM

           IF WS-JLEN < 10
               DISPLAY "  Vision failed!"
               MOVE 2 TO WS-TARGET-COL
               MOVE 4 TO WS-TARGET-ROW
               EXIT PARAGRAPH
           END-IF

           DISPLAY "  Vision resp: "
               WS-JBUF(1:MIN(500, WS-JLEN))

      *>   Parse target_column
           PERFORM PARSE-VISION-RESPONSE

           DISPLAY "  Target: col="
               WS-TARGET-COL " row="
               WS-TARGET-ROW
           .

      *> ============================================================
      *> PARSE-VISION-RESPONSE
      *> ============================================================
       PARSE-VISION-RESPONSE.
      *>   First extract the content string
           MOVE "content" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE WS-JVAL TO WS-AG-CONTENT

      *>   Now parse target_column from content
      *>   Content contains JSON like
      *>   {"target_column": 2, "target_row": 3}
      *>   But it may be escaped or have markdown
      *>   Look for target_column followed by number
           MOVE 2 TO WS-TARGET-COL
           MOVE 4 TO WS-TARGET-ROW

      *>   Search for target_column in content
           MOVE 0 TO WS-KEY-POS
           PERFORM VARYING WS-K FROM 1 BY 1
               UNTIL WS-K >
                   LENGTH(TRIM(WS-AG-CONTENT))
                   - 13
               OR WS-KEY-POS > 0
               IF WS-AG-CONTENT(WS-K:13)
                   = "target_column"
                   MOVE WS-K TO WS-KEY-POS
               END-IF
           END-PERFORM

           IF WS-KEY-POS > 0
      *>       Find the number after it
               COMPUTE WS-K = WS-KEY-POS + 13
               PERFORM UNTIL WS-K >
                   LENGTH(TRIM(WS-AG-CONTENT))
                   OR (WS-AG-CONTENT(WS-K:1)
                       >= "1"
                   AND WS-AG-CONTENT(WS-K:1)
                       <= "9")
                   ADD 1 TO WS-K
               END-PERFORM
               IF WS-K <=
                   LENGTH(TRIM(WS-AG-CONTENT))
                   MOVE WS-AG-CONTENT(WS-K:1)
                       TO WS-TARGET-COL
               END-IF
           END-IF

      *>   Search for target_row
           MOVE 0 TO WS-KEY-POS
           PERFORM VARYING WS-K FROM 1 BY 1
               UNTIL WS-K >
                   LENGTH(TRIM(WS-AG-CONTENT))
                   - 10
               OR WS-KEY-POS > 0
               IF WS-AG-CONTENT(WS-K:10)
                   = "target_row"
                   MOVE WS-K TO WS-KEY-POS
               END-IF
           END-PERFORM

           IF WS-KEY-POS > 0
               COMPUTE WS-K = WS-KEY-POS + 10
               PERFORM UNTIL WS-K >
                   LENGTH(TRIM(WS-AG-CONTENT))
                   OR (WS-AG-CONTENT(WS-K:1)
                       >= "1"
                   AND WS-AG-CONTENT(WS-K:1)
                       <= "9")
                   ADD 1 TO WS-K
               END-PERFORM
               IF WS-K <=
                   LENGTH(TRIM(WS-AG-CONTENT))
                   MOVE WS-AG-CONTENT(WS-K:1)
                       TO WS-TARGET-ROW
               END-IF
           END-IF

      *>   Clamp to valid range
           IF WS-TARGET-COL < 1
               MOVE 1 TO WS-TARGET-COL
           END-IF
           IF WS-TARGET-COL > WS-GRID-COLS
               MOVE WS-GRID-COLS
                   TO WS-TARGET-COL
           END-IF
           IF WS-TARGET-ROW < 1
               MOVE 1 TO WS-TARGET-ROW
           END-IF
           IF WS-TARGET-ROW > WS-GRID-ROWS
               MOVE WS-GRID-ROWS
                   TO WS-TARGET-ROW
           END-IF
           .

      *> ============================================================
      *> BUILD-CANDIDATES: Vision pick + 8 neighbors
      *> ============================================================
       BUILD-CANDIDATES.
           MOVE 0 TO WS-CAND-COUNT

      *>   Primary candidate
           ADD 1 TO WS-CAND-COUNT
           MOVE WS-TARGET-COL
               TO WS-CAND-C(WS-CAND-COUNT)
           MOVE WS-TARGET-ROW
               TO WS-CAND-R(WS-CAND-COUNT)

      *>   8 neighbors
           PERFORM VARYING WS-NI FROM 1 BY 1
               UNTIL WS-NI > 8
               COMPUTE WS-NC =
                   WS-TARGET-COL + WS-NDC-V(WS-NI)
               COMPUTE WS-NR =
                   WS-TARGET-ROW + WS-NDR-V(WS-NI)
               IF WS-NC >= 1
               AND WS-NC <= WS-GRID-COLS
               AND WS-NR >= 1
               AND WS-NR <= WS-GRID-ROWS
                   ADD 1 TO WS-CAND-COUNT
                   MOVE WS-NC
                       TO WS-CAND-C(WS-CAND-COUNT)
                   MOVE WS-NR
                       TO WS-CAND-R(WS-CAND-COUNT)
               END-IF
           END-PERFORM

           DISPLAY " "
           DISPLAY "[STAGE 3+4] Trying "
               WS-CAND-COUNT " candidates"
           .

      *> ============================================================
      *> TRY-CANDIDATES
      *> ============================================================
       TRY-CANDIDATES.
           PERFORM VARYING WS-CAND-IDX
               FROM 1 BY 1
               UNTIL WS-CAND-IDX > WS-CAND-COUNT
               OR WS-FLAG-FOUND = "Y"

               MOVE WS-CAND-C(WS-CAND-IDX)
                   TO WS-INSTR-COL
               MOVE WS-CAND-R(WS-CAND-IDX)
                   TO WS-INSTR-ROW

               IF WS-CAND-IDX = 1
                   DISPLAY "--- Vision pick: ("
                       WS-INSTR-COL ","
                       WS-INSTR-ROW ") ---"
               ELSE
                   DISPLAY "--- Neighbor "
                       WS-CAND-IDX ": ("
                       WS-INSTR-COL ","
                       WS-INSTR-ROW ") ---"
               END-IF

      *>       Build instructions
               PERFORM BUILD-INSTRUCTIONS
      *>       Submit
               PERFORM SUBMIT-INSTRUCTIONS

      *>       Check for flag
               MOVE 0 TO WS-TALLY-CNT
               IF WS-RESP-LEN > 0
                   INSPECT WS-RESP-BUF(
                       1:WS-RESP-LEN)
                       TALLYING WS-TALLY-CNT
                       FOR ALL "FLG"
               END-IF
               IF WS-TALLY-CNT > 0
                   DISPLAY "*** FLAG FOUND ***"
                   MOVE "Y" TO WS-FLAG-FOUND
                   EXIT PERFORM
               END-IF

      *>       Check coord error keywords
               PERFORM CHECK-COORD-ERROR
               IF WS-COORD-ERR = "Y"
                   DISPLAY "  Coord error ->"
                       " trying next..."
               ELSE
                   DISPLAY "  Non-coord error"
                       " -> agent mode"
      *>           Save last error and instructions
                   PERFORM SAVE-LAST-ERROR
                   EXIT PERFORM
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> BUILD-INSTRUCTIONS
      *> ============================================================
       BUILD-INSTRUCTIONS.
           MOVE SPACES TO WS-INSTR-BUF
           MOVE 1 TO WS-PTR
           MOVE WS-INSTR-COL TO WS-COL-TXT
           MOVE WS-INSTR-ROW TO WS-ROW-TXT

           STRING
               '"hardReset",'
               '"setDestinationObject('
               'PWR6132PL)",'
               '"set('
               TRIM(WS-COL-TXT) ","
               TRIM(WS-ROW-TXT) ')",'
               '"set(engineON)",'
               '"set(100%)",'
               '"set(10m)",'
               '"set(destroy)",'
               '"set(return)",'
               '"flyToLocation"'
               DELIMITED SIZE INTO WS-INSTR-BUF
               WITH POINTER WS-PTR
           END-STRING

           COMPUTE WS-INSTR-LEN = WS-PTR - 1
           .

      *> ============================================================
      *> SUBMIT-INSTRUCTIONS
      *> ============================================================
       SUBMIT-INSTRUCTIONS.
           MOVE SPACES TO WS-RESP-BUF
           MOVE 0 TO WS-RESP-LEN

           MOVE SPACES TO WS-REQ-JSON
           MOVE 1 TO WS-PTR
           STRING
               '{"apikey":"'
               TRIM(WS-HUB-KEY)
               '","task":"drone",'
               '"answer":{"instructions":['
               WS-INSTR-BUF(1:WS-INSTR-LEN)
               ']}}'
               DELIMITED SIZE INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           COMPUTE WS-K = WS-PTR - 1
           DISPLAY "  Submit: "
               WS-REQ-JSON(1:MIN(500, WS-K))

           MOVE "submit_req.json"
               TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "OPEN submit_req.json "
                   "failed: " WS-FS
               STOP RUN
           END-IF
           WRITE WORK-REC FROM WS-REQ-JSON
           CLOSE WORK-FILE

           INITIALIZE WS-CMD
           STRING
               "curl -s -o submit_resp.json"
               " -X POST "
               TRIM(WS-VERIFY-URL)
               " -H " WS-QT
               "Content-Type: "
               "application/json" WS-QT
               " -d @submit_req.json"
               DELIMITED SIZE INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Read response
           MOVE "submit_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-RESP-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           IF WS-RESP-LEN > 500
               DISPLAY "  Resp: "
                   WS-RESP-BUF(1:500)
           ELSE IF WS-RESP-LEN > 0
               DISPLAY "  Resp: "
                   WS-RESP-BUF(1:WS-RESP-LEN)
           ELSE
               DISPLAY "  Resp: (empty)"
           END-IF
           END-IF
           .

      *> ============================================================
      *> CHECK-COORD-ERROR
      *> ============================================================
       CHECK-COORD-ERROR.
           MOVE "N" TO WS-COORD-ERR
           IF WS-RESP-LEN = 0
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-RESP-BUF(1:WS-RESP-LEN)
               TALLYING WS-TALLY-CNT
               FOR ALL "nearby"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-COORD-ERR
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-RESP-BUF(1:WS-RESP-LEN)
               TALLYING WS-TALLY-CNT
               FOR ALL "hit the dam"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-COORD-ERR
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-RESP-BUF(1:WS-RESP-LEN)
               TALLYING WS-TALLY-CNT
               FOR ALL "pretending"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-COORD-ERR
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-RESP-BUF(1:WS-RESP-LEN)
               TALLYING WS-TALLY-CNT
               FOR ALL "power plant"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-COORD-ERR
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-RESP-BUF(1:WS-RESP-LEN)
               TALLYING WS-TALLY-CNT
               FOR ALL "wrong"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-COORD-ERR
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-RESP-BUF(1:WS-RESP-LEN)
               TALLYING WS-TALLY-CNT
               FOR ALL "incorrect"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-COORD-ERR
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-RESP-BUF(1:WS-RESP-LEN)
               TALLYING WS-TALLY-CNT
               FOR ALL "not found"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-COORD-ERR
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-RESP-BUF(1:WS-RESP-LEN)
               TALLYING WS-TALLY-CNT
               FOR ALL "empty"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-COORD-ERR
           END-IF
           .

      *> ============================================================
      *> SAVE-LAST-ERROR
      *> ============================================================
       SAVE-LAST-ERROR.
           MOVE SPACES TO WS-LAST-ERR
           IF WS-RESP-LEN > 4000
               MOVE WS-RESP-BUF(1:4000)
                   TO WS-LAST-ERR
               MOVE 4000 TO WS-LAST-ERR-LEN
           ELSE
               MOVE WS-RESP-BUF(1:WS-RESP-LEN)
                   TO WS-LAST-ERR
               MOVE WS-RESP-LEN
                   TO WS-LAST-ERR-LEN
           END-IF
           MOVE WS-INSTR-BUF(1:WS-INSTR-LEN)
               TO WS-LAST-INSTR
           .

      *> ============================================================
      *> RUN-AGENT: GPT-4.1-mini error recovery
      *> ============================================================
       RUN-AGENT.
           DISPLAY " "
           DISPLAY "[STAGE 5] Agent error recovery"

      *>   Escape docs for prompt
           MOVE WS-DOCS-BUF(1:WS-DOCS-LEN)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR
           MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
               TO WS-DOCS-ESC
           MOVE WS-ESC-OLEN TO WS-DOCS-ESC-LEN

           MOVE 0 TO WS-AG-STEP
           PERFORM UNTIL WS-AG-STEP >= WS-AG-MAX
               OR WS-FLAG-FOUND = "Y"

               ADD 1 TO WS-AG-STEP
               DISPLAY " "
               DISPLAY "--- Agent step "
                   WS-AG-STEP " / "
                   WS-AG-MAX " ---"

               PERFORM CALL-AGENT
               IF WS-FLAG-FOUND = "Y"
                   EXIT PERFORM
               END-IF

      *>       Parse corrected instructions
               PERFORM EXTRACT-AGENT-INSTR

      *>       Submit corrected instructions
               PERFORM SUBMIT-INSTRUCTIONS

      *>       Check for flag
               MOVE 0 TO WS-TALLY-CNT
               IF WS-RESP-LEN > 0
                   INSPECT WS-RESP-BUF(
                       1:WS-RESP-LEN)
                       TALLYING WS-TALLY-CNT
                       FOR ALL "FLG"
               END-IF
               IF WS-TALLY-CNT > 0
                   DISPLAY "*** FLAG FOUND ***"
                   MOVE "Y" TO WS-FLAG-FOUND
                   EXIT PERFORM
               END-IF

      *>       Update error for next iteration
               PERFORM SAVE-LAST-ERROR
           END-PERFORM
           .

      *> ============================================================
      *> CALL-AGENT: Send to GPT-4.1-mini
      *> ============================================================
       CALL-AGENT.
      *>   Escape last error for JSON
           MOVE WS-LAST-ERR(1:WS-LAST-ERR-LEN)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR
      *>   Save escaped error
           MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
               TO WS-LAST-ERR
           MOVE WS-ESC-OLEN TO WS-LAST-ERR-LEN

      *>   Escape last instructions
           MOVE TRIM(WS-LAST-INSTR)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

           MOVE SPACES TO WS-REQ-JSON
           MOVE 1 TO WS-PTR

           MOVE WS-INSTR-COL TO WS-COL-TXT
           MOVE WS-INSTR-ROW TO WS-ROW-TXT

           STRING
               '{"model":"gpt-4.1-mini",'
               '"messages":['
               '{"role":"system","content":"'
               'You are a drone mission '
               'programmer. Fix the instruction '
               'sequence based on API error '
               'feedback.'
               WS-NL WS-NL
               'Drone API Documentation:'
               WS-NL
               DELIMITED SIZE INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Embed escaped docs (up to 4000 chars)
           IF WS-DOCS-ESC-LEN > 4000
               MOVE 4000 TO WS-DOCS-ESC-LEN
           END-IF
           MOVE WS-DOCS-ESC(
               1:WS-DOCS-ESC-LEN)
               TO WS-REQ-JSON(
               WS-PTR:WS-DOCS-ESC-LEN)
           ADD WS-DOCS-ESC-LEN TO WS-PTR

           STRING
               WS-NL WS-NL
               'CRITICAL RULES:'
               WS-NL
               '- Each instruction is a separate '
               'string in a JSON array'
               WS-NL
               '- flyToLocation MUST be LAST'
               WS-NL
               '- Start with hardReset'
               WS-NL
               '- Target: PWR6132PL at ('
               TRIM(WS-COL-TXT) ","
               TRIM(WS-ROW-TXT) ")"
               WS-NL
               '- Objective: destroy'
               WS-NL WS-NL
               'Output ONLY a JSON array of '
               'strings. No explanation."},'
               DELIMITED SIZE INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   User message with error
           STRING
               '{"role":"user","content":"'
               'Latest attempt: ['
               DELIMITED SIZE INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Embed escaped instructions
           MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
               TO WS-REQ-JSON(
               WS-PTR:WS-ESC-OLEN)
           ADD WS-ESC-OLEN TO WS-PTR

           STRING
               ']'
               WS-NL
               'Error: '
               DELIMITED SIZE INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Embed error
           MOVE WS-LAST-ERR(
               1:WS-LAST-ERR-LEN)
               TO WS-REQ-JSON(
               WS-PTR:WS-LAST-ERR-LEN)
           ADD WS-LAST-ERR-LEN TO WS-PTR

           STRING
               WS-NL
               'Fix it. Output ONLY a JSON '
               'array."}],'
               '"max_tokens":500,'
               '"temperature":0.2}'
               DELIMITED SIZE INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           COMPUTE WS-K = WS-PTR - 1

           MOVE "agent_req.json"
               TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "OPEN agent_req.json "
                   "failed: " WS-FS
               STOP RUN
           END-IF
           WRITE WORK-REC FROM WS-REQ-JSON
           CLOSE WORK-FILE

      *>   Call OpenAI with retry
           MOVE 0 TO WS-RETRY-CT
           PERFORM UNTIL WS-RETRY-CT >=
                   WS-MAX-RETRIES
               DISPLAY "  Calling gpt-4.1-mini..."
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
                   DELIMITED SIZE INTO WS-CMD
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
               IF WS-RETRY-CT <
                       WS-MAX-RETRIES
                   DISPLAY "  LLM retry "
                       WS-RETRY-CT "..."
                   MOVE WS-RETRY-DELAY
                       TO WS-SLEEP-SECS
                   CALL "C$SLEEP"
                       USING WS-SLEEP-SECS
               END-IF
           END-PERFORM

      *>   Parse content
           MOVE "content" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE WS-JVAL TO WS-AG-CONTENT

           DISPLAY "  Agent: "
               TRIM(WS-AG-CONTENT)(1:500)

      *>   Check for flag in agent response
           MOVE 0 TO WS-TALLY-CNT
           IF LENGTH(TRIM(WS-AG-CONTENT)) > 0
               INSPECT WS-AG-CONTENT
                   TALLYING WS-TALLY-CNT
                   FOR ALL "FLG"
           END-IF
           IF WS-TALLY-CNT > 0
               DISPLAY "*** FLAG IN AGENT ***"
               MOVE "Y" TO WS-FLAG-FOUND
           END-IF
           .

      *> ============================================================
      *> EXTRACT-AGENT-INSTR: Parse JSON array from
      *> WS-AG-CONTENT into WS-INSTR-BUF
      *> ============================================================
       EXTRACT-AGENT-INSTR.
      *>   Find [ in content (start of array)
           MOVE 0 TO WS-KEY-POS
           PERFORM VARYING WS-K FROM 1 BY 1
               UNTIL WS-K >
                   LENGTH(TRIM(WS-AG-CONTENT))
               OR WS-KEY-POS > 0
               IF WS-AG-CONTENT(WS-K:1) = "["
                   MOVE WS-K TO WS-KEY-POS
               END-IF
           END-PERFORM

           IF WS-KEY-POS = 0
               DISPLAY "  No array in agent resp"
               EXIT PARAGRAPH
           END-IF

      *>   Find matching ]
           MOVE 0 TO WS-VAL-END
           PERFORM VARYING WS-K
               FROM WS-KEY-POS BY 1
               UNTIL WS-K >
                   LENGTH(TRIM(WS-AG-CONTENT))
               OR WS-VAL-END > 0
               IF WS-AG-CONTENT(WS-K:1) = "]"
                   MOVE WS-K TO WS-VAL-END
               END-IF
           END-PERFORM

           IF WS-VAL-END = 0
               DISPLAY "  No ] in agent resp"
               EXIT PARAGRAPH
           END-IF

      *>   Extract content between [ and ] exclusive
           COMPUTE WS-K = WS-VAL-END
               - WS-KEY-POS - 1
           IF WS-K > 0 AND WS-K <= 4000
               MOVE WS-AG-CONTENT(
                   WS-KEY-POS + 1:WS-K)
                   TO WS-INSTR-BUF
               MOVE WS-K TO WS-INSTR-LEN
           END-IF

      *>   Unescape \" -> " in the instructions
      *>   (LLM content is JSON-escaped)
           MOVE WS-INSTR-BUF(1:WS-INSTR-LEN)
               TO WS-ESC-IN
           PERFORM JSON-UNESCAPE-STR
           MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
               TO WS-INSTR-BUF
           MOVE WS-ESC-OLEN TO WS-INSTR-LEN

           DISPLAY "  Parsed instructions: "
               WS-INSTR-BUF(1:
               MIN(500, WS-INSTR-LEN))
           .

      *> (READ-JSON-FILE removed - from copybook)

      *> ============================================================
      *> READ-RESP-FILE: Read into WS-RESP-BUF/LEN
      *> ============================================================
       READ-RESP-FILE.
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
                       MOVE LENGTH(
                           TRIM(WS-LINE
                           TRAILING)) TO WS-K
                       IF WS-K > 0
                           IF WS-RESP-LEN > 0
                               ADD 1
                                   TO WS-RESP-LEN
                               MOVE " "
                                   TO WS-RESP-BUF(
                                   WS-RESP-LEN:1)
                           END-IF
                           IF WS-K > 16000
                               MOVE 16000
                                   TO WS-K
                           END-IF
                           IF WS-RESP-LEN
                               + WS-K <= 16000
                               MOVE WS-LINE(
                                   1:WS-K)
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

      *> === Shared paragraphs (copybooks) ===
       COPY JSONPARSE-PROC.
       COPY JSONREAD-PROC.
       COPY JSONESCAPE-PROC.
       COPY JSONUNESCAPE-PROC.

       COPY ENVLOAD-PROC.
