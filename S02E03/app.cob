       IDENTIFICATION DIVISION.
       PROGRAM-ID. S02E03-FAILURE.
      *> ============================================================
      *> S02E03 - Failure Log Condensation (Pure COBOL)
      *> 1. Fetch failure.log via curl GET
      *> 2. Parse log lines, filter relevant events
      *> 3. Deduplicate by severity+message
      *> 4. Format compact log text
      *> 5. Condense with LLM if over token limit
      *> 6. Submit to Hub /verify, iterate on feedback
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
           SELECT LOG-FILE ASSIGN TO "failure.log"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  WORK-FILE.
       01  WORK-REC                PIC X(32000).

       FD  LOG-FILE.
       01  LOG-REC                 PIC X(500).

       WORKING-STORAGE SECTION.
      *> === Environment (via copybook) ===
       COPY ENVLOAD-WS.

      *> === File I/O ===
       01  WS-FS                   PIC XX.
       01  WS-WORK-PATH            PIC X(100)
                                   VALUE "work.tmp".

      *> === HTTP ===
       01  WS-LOG-URL              PIC X(200).
       01  WS-CMD                  PIC X(4000).
       01  WS-REQ-JSON             PIC X(32000).

      *> === Log Parsing ===
       01  WS-LOG-LINE             PIC X(500).
       01  WS-LOG-LEN              PIC 9(5).
       01  WS-TOTAL-LINES          PIC 9(5) VALUE 0.
       01  WS-P-DATE               PIC X(10).
       01  WS-P-TIME               PIC X(8).
       01  WS-P-SEV                PIC X(4).
       01  WS-P-MSG                PIC X(300).
       01  WS-MSG-LOWER            PIC X(300).
       01  WS-KW-FOUND             PIC X VALUE "N".
       01  WS-PPOS                 PIC 9(5).
       01  WS-PEND                 PIC 9(5).
       01  WS-CH                   PIC X(1).

      *> === Task Data ===
       01  WS-MAX-ENTRIES          PIC 9(3) VALUE 300.
       01  WS-REL-COUNT            PIC 9(3) VALUE 0.
       01  WS-REL-TABLE.
           05  WS-REL OCCURS 300 TIMES.
               10  WS-R-DATE      PIC X(10).
               10  WS-R-TIME      PIC X(8).
               10  WS-R-SEV       PIC X(4).
               10  WS-R-MSG       PIC X(300).
               10  WS-R-CNT       PIC 9(3)
                                  VALUE 1.
       01  WS-DED-COUNT            PIC 9(3) VALUE 0.
       01  WS-DED-TABLE.
           05  WS-DED OCCURS 200 TIMES.
               10  WS-D-DATE      PIC X(10).
               10  WS-D-TIME      PIC X(8).
               10  WS-D-SEV       PIC X(4).
               10  WS-D-MSG       PIC X(300).
               10  WS-D-CNT       PIC 9(3)
                                  VALUE 1.
       01  WS-COMPACT              PIC X(16000).
       01  WS-COMPACT-LEN          PIC 9(5) VALUE 0.
       01  WS-COMPACT-LINE         PIC X(350).
       01  WS-CONDENSED            PIC X(16000).
       01  WS-COND-LEN             PIC 9(5) VALUE 0.
       01  WS-EST-TOKENS           PIC 9(5).
       01  WS-TOKEN-LIMIT          PIC 9(5) VALUE 6000.

      *> === JSON Parsing (copybooks) ===
       COPY JSONPARSE-WS.
       COPY JSONREAD-WS.
       COPY JSONESCAPE-WS.

      *> === Control Flow ===
       01  WS-PTR                  PIC 9(5).
       01  WS-I                    PIC 9(5).
       01  WS-J                    PIC 9(5).
       01  WS-TALLY-CNT            PIC 9(4).
       01  WS-DUP-FOUND            PIC X VALUE "N".
       01  WS-ATTEMPT              PIC 9(1).
       01  WS-SUCCESS              PIC X VALUE "N".
       01  WS-FEEDBACK             PIC X(2000).
       01  WS-FEEDBACK-LEN         PIC 9(5) VALUE 0.
       01  WS-FEEDBACK-ESC         PIC X(4000).
       01  WS-FEEDBACK-ESC-LEN     PIC 9(5).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S02E03 FAILURE LOG ==="

           PERFORM LOAD-ENV-VARS

           PERFORM FETCH-LOG
           PERFORM PARSE-AND-FILTER
           PERFORM DEDUPLICATE-ENTRIES
           PERFORM FORMAT-COMPACT

      *>   Check estimated tokens
           COMPUTE WS-EST-TOKENS =
               WS-COMPACT-LEN / 4
           DISPLAY "  Est tokens: "
               WS-EST-TOKENS

           IF WS-EST-TOKENS > 1500
               DISPLAY "  Over limit, LLM.."
               MOVE SPACES TO WS-FEEDBACK
               MOVE 0 TO WS-FEEDBACK-LEN
               PERFORM CONDENSE-WITH-LLM
           ELSE
               DISPLAY "  Within limit."
               MOVE WS-COMPACT(
                   1:WS-COMPACT-LEN)
                   TO WS-CONDENSED
               MOVE WS-COMPACT-LEN
                   TO WS-COND-LEN
           END-IF

      *>   Submit with retry loop
           MOVE "N" TO WS-SUCCESS
           MOVE SPACES TO WS-FEEDBACK
           MOVE 0 TO WS-FEEDBACK-LEN

           PERFORM VARYING WS-ATTEMPT
               FROM 1 BY 1
               UNTIL WS-ATTEMPT > 5
               OR WS-SUCCESS = "Y"
               DISPLAY " "
               DISPLAY "--- Attempt "
                   WS-ATTEMPT "/5 ---"
               PERFORM SUBMIT-TO-HUB
               PERFORM CHECK-RESPONSE
               IF WS-SUCCESS NOT = "Y"
               AND WS-FEEDBACK-LEN > 0
               AND WS-ATTEMPT < 5
                   DISPLAY "  Re-condense..."
                   PERFORM CONDENSE-WITH-LLM
               END-IF
           END-PERFORM

           IF WS-SUCCESS NOT = "Y"
               DISPLAY " "
               DISPLAY "  FAILED."
           END-IF
           DISPLAY " "
           DISPLAY "=== DONE ==="
           STOP RUN.

      *> ============================================================
      *> FETCH-LOG: Download failure.log
      *> ============================================================
       FETCH-LOG.
           DISPLAY "  Fetching log..."

      *>   Check if file already exists
           OPEN INPUT LOG-FILE
           IF WS-FS = "00"
               CLOSE LOG-FILE
               DISPLAY "  Using cached file."
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-LOG-URL
           STRING
               TRIM(WS-HUB-URL)
               "/data/"
               TRIM(WS-HUB-KEY)
               "/failure.log"
               DELIMITED SIZE
               INTO WS-LOG-URL
           END-STRING

           INITIALIZE WS-CMD
           STRING
               "curl -s -o "
               "failure.log "
               WS-QT
               TRIM(WS-LOG-URL)
               WS-QT
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           DISPLAY "  Log fetched."
           .

      *> ============================================================
      *> PARSE-AND-FILTER: Read log, parse, keep relevant
      *> Format: [YYYY-MM-DD HH:MM] [SEV] message
      *> ============================================================
       PARSE-AND-FILTER.
           DISPLAY "  Parsing log..."
           MOVE "N" TO WS-EOF
           MOVE 0 TO WS-TOTAL-LINES
           MOVE 0 TO WS-REL-COUNT

           OPEN INPUT LOG-FILE
           IF WS-FS NOT = "00"
               DISPLAY "  ERR open log: "
                   WS-FS
               STOP RUN
           END-IF

           PERFORM UNTIL WS-EOF = "Y"
               READ LOG-FILE
                   INTO WS-LOG-LINE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       ADD 1 TO WS-TOTAL-LINES
                       PERFORM PARSE-ONE-LINE
                       IF WS-P-SEV
                           NOT = SPACES
                           PERFORM
                           FILTER-ONE-ENTRY
                       END-IF
               END-READ
           END-PERFORM

           CLOSE LOG-FILE
           MOVE "N" TO WS-EOF

           DISPLAY "  Total lines: "
               WS-TOTAL-LINES
           DISPLAY "  Relevant: "
               WS-REL-COUNT
           .

      *> ============================================================
      *> PARSE-ONE-LINE: Extract date, time, severity, message
      *> ============================================================
       PARSE-ONE-LINE.
           MOVE SPACES TO WS-P-DATE
           MOVE SPACES TO WS-P-TIME
           MOVE SPACES TO WS-P-SEV
           MOVE SPACES TO WS-P-MSG

           MOVE LENGTH(TRIM(WS-LOG-LINE
               TRAILING))
               TO WS-LOG-LEN
           IF WS-LOG-LEN < 20
               EXIT PARAGRAPH
           END-IF

      *>   Must start with [
           IF WS-LOG-LINE(1:1) NOT = "["
               EXIT PARAGRAPH
           END-IF

      *>   Extract date: chars 2-11
           MOVE WS-LOG-LINE(2:10)
               TO WS-P-DATE

      *>   Find time after space (pos 12+)
           MOVE 13 TO WS-PPOS
      *>   Skip spaces
           PERFORM UNTIL WS-PPOS
               > WS-LOG-LEN
               OR WS-LOG-LINE(
               WS-PPOS:1) NOT = " "
               ADD 1 TO WS-PPOS
           END-PERFORM

      *>   Read time until ]
           MOVE WS-PPOS TO WS-PEND
           PERFORM UNTIL WS-PEND
               > WS-LOG-LEN
               OR WS-LOG-LINE(
               WS-PEND:1) = "]"
               ADD 1 TO WS-PEND
           END-PERFORM
           IF WS-PEND > WS-PPOS
               MOVE WS-LOG-LINE(
                   WS-PPOS:
                   WS-PEND - WS-PPOS)
                   TO WS-P-TIME
           END-IF

      *>   Skip ] and spaces to find [SEV]
           ADD 1 TO WS-PEND
           PERFORM UNTIL WS-PEND
               > WS-LOG-LEN
               OR WS-LOG-LINE(
               WS-PEND:1) = "["
               ADD 1 TO WS-PEND
           END-PERFORM

      *>   Extract severity
           IF WS-PEND > WS-LOG-LEN
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-PEND
           MOVE WS-PEND TO WS-PPOS
           PERFORM UNTIL WS-PEND
               > WS-LOG-LEN
               OR WS-LOG-LINE(
               WS-PEND:1) = "]"
               ADD 1 TO WS-PEND
           END-PERFORM
           IF WS-PEND > WS-PPOS
               MOVE WS-LOG-LINE(
                   WS-PPOS:
                   WS-PEND - WS-PPOS)
                   TO WS-P-SEV
           END-IF

      *>   Skip ] and space to get message
           ADD 1 TO WS-PEND
           PERFORM UNTIL WS-PEND
               > WS-LOG-LEN
               OR WS-LOG-LINE(
               WS-PEND:1) NOT = " "
               ADD 1 TO WS-PEND
           END-PERFORM
           IF WS-PEND <= WS-LOG-LEN
               MOVE WS-LOG-LINE(
                   WS-PEND:
                   WS-LOG-LEN
                   - WS-PEND + 1)
                   TO WS-P-MSG
           END-IF
           .

      *> ============================================================
      *> FILTER-ONE-ENTRY: Keep CRIT, ERRO, relevant WARN
      *> ============================================================
       FILTER-ONE-ENTRY.
           IF TRIM(WS-P-SEV) = "CRIT"
               PERFORM ADD-RELEVANT
               EXIT PARAGRAPH
           END-IF
           IF TRIM(WS-P-SEV) = "ERRO"
               PERFORM ADD-RELEVANT
               EXIT PARAGRAPH
           END-IF
           IF TRIM(WS-P-SEV) = "WARN"
               PERFORM CHECK-KEYWORDS
               IF WS-KW-FOUND = "Y"
                   PERFORM ADD-RELEVANT
               END-IF
           END-IF
           .

      *> ============================================================
      *> CHECK-KEYWORDS: Search for failure keywords
      *> ============================================================
       CHECK-KEYWORDS.
           MOVE "N" TO WS-KW-FOUND
           MOVE LOWER-CASE(
               TRIM(WS-P-MSG))
               TO WS-MSG-LOWER

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-MSG-LOWER
               TALLYING WS-TALLY-CNT
               FOR ALL "trip"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-KW-FOUND
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-MSG-LOWER
               TALLYING WS-TALLY-CNT
               FOR ALL "fault"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-KW-FOUND
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-MSG-LOWER
               TALLYING WS-TALLY-CNT
               FOR ALL "fail"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-KW-FOUND
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-MSG-LOWER
               TALLYING WS-TALLY-CNT
               FOR ALL "critical"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-KW-FOUND
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-MSG-LOWER
               TALLYING WS-TALLY-CNT
               FOR ALL "emergency"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-KW-FOUND
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-MSG-LOWER
               TALLYING WS-TALLY-CNT
               FOR ALL "shutdown"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-KW-FOUND
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-MSG-LOWER
               TALLYING WS-TALLY-CNT
               FOR ALL "exceeded"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-KW-FOUND
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-MSG-LOWER
               TALLYING WS-TALLY-CNT
               FOR ALL "below"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-KW-FOUND
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-MSG-LOWER
               TALLYING WS-TALLY-CNT
               FOR ALL "lost"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-KW-FOUND
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-MSG-LOWER
               TALLYING WS-TALLY-CNT
               FOR ALL "compromised"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-KW-FOUND
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-MSG-LOWER
               TALLYING WS-TALLY-CNT
               FOR ALL "terminated"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-KW-FOUND
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-MSG-LOWER
               TALLYING WS-TALLY-CNT
               FOR ALL "locked"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-KW-FOUND
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-MSG-LOWER
               TALLYING WS-TALLY-CNT
               FOR ALL "saturated"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-KW-FOUND
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-MSG-LOWER
               TALLYING WS-TALLY-CNT
               FOR ALL "cavitation"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-KW-FOUND
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-MSG-LOWER
               TALLYING WS-TALLY-CNT
               FOR ALL "runaway"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-KW-FOUND
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-MSG-LOWER
               TALLYING WS-TALLY-CNT
               FOR ALL "shedding"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-KW-FOUND
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-MSG-LOWER
               TALLYING WS-TALLY-CNT
               FOR ALL "nonrecoverable"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-KW-FOUND
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-MSG-LOWER
               TALLYING WS-TALLY-CNT
               FOR ALL "restricted"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-KW-FOUND
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-MSG-LOWER
               TALLYING WS-TALLY-CNT
               FOR ALL "constrained"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-KW-FOUND
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-MSG-LOWER
               TALLYING WS-TALLY-CNT
               FOR ALL "unstable"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-KW-FOUND
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-MSG-LOWER
               TALLYING WS-TALLY-CNT
               FOR ALL "overflow"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-KW-FOUND
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-MSG-LOWER
               TALLYING WS-TALLY-CNT
               FOR ALL "alarm"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-KW-FOUND
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-MSG-LOWER
               TALLYING WS-TALLY-CNT
               FOR ALL "error"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-KW-FOUND
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-MSG-LOWER
               TALLYING WS-TALLY-CNT
               FOR ALL "missing"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-KW-FOUND
               EXIT PARAGRAPH
           END-IF
           .

      *> ============================================================
      *> ADD-RELEVANT: Add current parsed entry
      *> ============================================================
       ADD-RELEVANT.
           IF WS-REL-COUNT >= WS-MAX-ENTRIES
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-REL-COUNT
           MOVE WS-P-DATE TO
               WS-R-DATE(WS-REL-COUNT)
           MOVE WS-P-TIME TO
               WS-R-TIME(WS-REL-COUNT)
           MOVE WS-P-SEV TO
               WS-R-SEV(WS-REL-COUNT)
           MOVE WS-P-MSG TO
               WS-R-MSG(WS-REL-COUNT)
           MOVE 1 TO
               WS-R-CNT(WS-REL-COUNT)
           .

      *> ============================================================
      *> DEDUPLICATE-ENTRIES: Merge by sev+msg
      *> ============================================================
       DEDUPLICATE-ENTRIES.
           DISPLAY "  Deduplicating..."
           MOVE 0 TO WS-DED-COUNT

           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I > WS-REL-COUNT
               MOVE "N" TO WS-DUP-FOUND
               PERFORM VARYING WS-J
                   FROM 1 BY 1
                   UNTIL WS-J
                   > WS-DED-COUNT
                   OR WS-DUP-FOUND = "Y"
                   IF TRIM(
                       WS-R-SEV(WS-I))
                       = TRIM(
                       WS-D-SEV(WS-J))
                   AND TRIM(
                       WS-R-MSG(WS-I))
                       = TRIM(
                       WS-D-MSG(WS-J))
                       ADD 1 TO
                           WS-D-CNT(WS-J)
                       MOVE "Y"
                           TO WS-DUP-FOUND
                   END-IF
               END-PERFORM
               IF WS-DUP-FOUND = "N"
                   IF WS-DED-COUNT < 200
                       ADD 1
                           TO WS-DED-COUNT
                       MOVE WS-R-DATE(
                           WS-I)
                           TO WS-D-DATE(
                           WS-DED-COUNT)
                       MOVE WS-R-TIME(
                           WS-I)
                           TO WS-D-TIME(
                           WS-DED-COUNT)
                       MOVE WS-R-SEV(
                           WS-I)
                           TO WS-D-SEV(
                           WS-DED-COUNT)
                       MOVE WS-R-MSG(
                           WS-I)
                           TO WS-D-MSG(
                           WS-DED-COUNT)
                       MOVE 1
                           TO WS-D-CNT(
                           WS-DED-COUNT)
                   END-IF
               END-IF
           END-PERFORM

           DISPLAY "  Unique events: "
               WS-DED-COUNT
           .

      *> ============================================================
      *> FORMAT-COMPACT: Build compact log text
      *> ============================================================
       FORMAT-COMPACT.
           DISPLAY "  Formatting..."
           MOVE SPACES TO WS-COMPACT
           MOVE 0 TO WS-COMPACT-LEN

           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I > WS-DED-COUNT
      *>       Build: [date HH:MM] [SEV] msg
               MOVE SPACES
                   TO WS-COMPACT-LINE
               MOVE 1 TO WS-PTR
               STRING
                   "["
                   TRIM(WS-D-DATE(WS-I))
                   " "
                   TRIM(WS-D-TIME(WS-I))
                   "] ["
                   TRIM(WS-D-SEV(WS-I))
                   "] "
                   TRIM(WS-D-MSG(WS-I))
                   DELIMITED SIZE
                   INTO WS-COMPACT-LINE
                   WITH POINTER WS-PTR
               END-STRING
      *>       Add (xN) if count > 1
               IF WS-D-CNT(WS-I) > 1
                   STRING
                       " (x"
                       TRIM(WS-D-CNT(
                           WS-I))
                       ")"
                       DELIMITED SIZE
                       INTO WS-COMPACT-LINE
                       WITH POINTER WS-PTR
                   END-STRING
               END-IF

      *>       Add newline between entries
               IF WS-COMPACT-LEN > 0
                   ADD 1 TO WS-COMPACT-LEN
                   MOVE X"0A"
                       TO WS-COMPACT(
                       WS-COMPACT-LEN:1)
               END-IF
      *>       Copy line to compact buf
               MOVE WS-PTR TO WS-K
               SUBTRACT 1 FROM WS-K
               IF WS-K > 0
                   MOVE WS-COMPACT-LINE(
                       1:WS-K)
                       TO WS-COMPACT(
                       WS-COMPACT-LEN
                       + 1:WS-K)
                   ADD WS-K
                       TO WS-COMPACT-LEN
               END-IF
           END-PERFORM

           DISPLAY "  Compact len: "
               WS-COMPACT-LEN " chars"
           .

      *> ============================================================
      *> CONDENSE-WITH-LLM: Call OpenAI to condense
      *> ============================================================
       CONDENSE-WITH-LLM.
           DISPLAY "  LLM condense..."

      *>   JSON-escape the compact log text
           MOVE WS-COMPACT(
               1:WS-COMPACT-LEN)
               TO WS-ESC-IN
           MOVE WS-COMPACT-LEN
               TO WS-ESC-ILEN
           PERFORM JSON-ESCAPE-STR

      *>   Build LLM request JSON
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
               WS-QT "content" WS-QT ":"
               WS-QT
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Prompt text
           STRING
               "You are a power plant"
               " log analyst. Condense"
               " these filtered failure"
               " logs to fit within"
               " 1500 tokens."
               WS-NL WS-NL
               "Rules:"
               WS-NL
               "- One event per line"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "- Keep format: "
               "[YYYY-MM-DD HH:MM]"
               " [SEVERITY] "
               "COMPONENT_ID desc"
               WS-NL
               "- Preserve: timestamps"
               ", severity levels "
               "(CRIT/ERRO/WARN), "
               "component IDs"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "- Keep all unique event"
               " types - merge "
               "duplicates with (xN)"
               WS-NL
               "- Prioritize CRIT, "
               "then ERRO, then WARN"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "- Preserve failure "
               "cascade: thermal "
               "warnings -> coolant "
               "-> pump failures -> "
               "power instability "
               "-> turbine -> "
               "firmware -> shutdown"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "- Shorten desc but "
               "keep technical meaning"
               WS-NL
               "- Target: <=1500 tokens"
               WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Add feedback if any
           IF WS-FEEDBACK-LEN > 0
               MOVE WS-FEEDBACK(
                   1:WS-FEEDBACK-LEN)
                   TO WS-ESC-IN
               MOVE WS-FEEDBACK-LEN
                   TO WS-ESC-ILEN
               PERFORM JSON-ESCAPE-STR
               STRING
                   "Previous feedback "
                   "from technicians:"
                   WS-NL
                   WS-ESC-OUT(
                   1:WS-ESC-OLEN)
                   WS-NL
                   "Address this feedback."
                   WS-NL WS-NL
                   DELIMITED SIZE
                   INTO WS-REQ-JSON
                   WITH POINTER WS-PTR
               END-STRING
           END-IF

      *>   Add the logs
           MOVE WS-COMPACT(
               1:WS-COMPACT-LEN)
               TO WS-ESC-IN
           MOVE WS-COMPACT-LEN
               TO WS-ESC-ILEN
           PERFORM JSON-ESCAPE-STR

           STRING
               "Logs to condense:"
               WS-NL
               WS-ESC-OUT(
               1:WS-ESC-OLEN)
               WS-NL WS-NL
               "Return ONLY condensed"
               " log lines."
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Close JSON
           STRING
               WS-QT "}],"
               WS-QT "temperature"
               WS-QT ":0.1,"
               WS-QT "max_tokens"
               WS-QT ":2000}"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Write request to file
           MOVE "llm_req.json"
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

      *>   POST to OpenAI
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

      *>   Parse LLM response
           MOVE "llm_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp"
               TO WS-WORK-PATH

           IF WS-JLEN = 0
               DISPLAY "  Empty LLM resp!"
               EXIT PARAGRAPH
           END-IF

      *>   Check for API error
           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-JBUF(1:WS-JLEN)
               TALLYING WS-TALLY-CNT
               FOR ALL '"error"'
           IF WS-TALLY-CNT > 0
               DISPLAY "  API ERR: "
                   TRIM(WS-JBUF)(1:300)
               EXIT PARAGRAPH
           END-IF

      *>   Extract content
           MOVE "content"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL

           IF TRIM(WS-JVAL) = SPACES
               DISPLAY "  No content!"
               EXIT PARAGRAPH
           END-IF

      *>   Unescape the content into condensed
           PERFORM UNESCAPE-JVAL
           DISPLAY "  Condensed: "
               WS-COND-LEN " chars"
           DISPLAY "  Preview: "
               WS-CONDENSED(1:200)
           .

      *> ============================================================
      *> UNESCAPE-JVAL: Convert \n to real newlines
      *> ============================================================
       UNESCAPE-JVAL.
           MOVE SPACES TO WS-CONDENSED
           MOVE 0 TO WS-COND-LEN
           MOVE LENGTH(TRIM(WS-JVAL))
               TO WS-K

           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > WS-K
               IF WS-I < WS-K
               AND WS-JVAL(WS-I:1)
                   = X"5C"
                   EVALUATE TRUE
                   WHEN WS-JVAL(
                       WS-I + 1:1) = "n"
                       ADD 1
                           TO WS-COND-LEN
                       MOVE X"0A"
                           TO WS-CONDENSED(
                           WS-COND-LEN:1)
                       ADD 2 TO WS-I
                   WHEN WS-JVAL(
                       WS-I + 1:1)
                       = WS-QT
                       ADD 1
                           TO WS-COND-LEN
                       MOVE WS-QT
                           TO WS-CONDENSED(
                           WS-COND-LEN:1)
                       ADD 2 TO WS-I
                   WHEN WS-JVAL(
                       WS-I + 1:1)
                       = X"5C"
                       ADD 1
                           TO WS-COND-LEN
                       MOVE X"5C"
                           TO WS-CONDENSED(
                           WS-COND-LEN:1)
                       ADD 2 TO WS-I
                   WHEN OTHER
                       ADD 1
                           TO WS-COND-LEN
                       MOVE WS-JVAL(
                           WS-I:1)
                           TO WS-CONDENSED(
                           WS-COND-LEN:1)
                       ADD 1 TO WS-I
                   END-EVALUATE
               ELSE
                   ADD 1 TO WS-COND-LEN
                   MOVE WS-JVAL(WS-I:1)
                       TO WS-CONDENSED(
                       WS-COND-LEN:1)
                   ADD 1 TO WS-I
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> SUBMIT-TO-HUB: POST condensed logs
      *> ============================================================
       SUBMIT-TO-HUB.
           DISPLAY "  Submitting..."

      *>   JSON-escape the condensed logs
      *>   The "logs" field needs \n for newlines
           MOVE WS-CONDENSED(
               1:WS-COND-LEN)
               TO WS-ESC-IN
           MOVE WS-COND-LEN
               TO WS-ESC-ILEN
           PERFORM JSON-ESCAPE-STR

      *>   Build payload
           MOVE SPACES TO WS-REQ-JSON
           MOVE 1 TO WS-PTR
           STRING
               "{"
               WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY)
               WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT "failure" WS-QT ","
               WS-QT "answer" WS-QT ":"
               "{"
               WS-QT "logs" WS-QT ":"
               WS-QT
               WS-ESC-OUT(
               1:WS-ESC-OLEN)
               WS-QT "}}"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Write and POST
           MOVE "hub_req.json"
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
               "-o hub_resp.json"
               " -X POST "
               TRIM(WS-VERIFY-URL)
               " -H " WS-QT
               "Content-Type: "
               "application/json"
               WS-QT
               " -d @hub_req.json"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD
           .

      *> ============================================================
      *> CHECK-RESPONSE: Check Hub response
      *> ============================================================
       CHECK-RESPONSE.
           MOVE "hub_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp"
               TO WS-WORK-PATH

           IF WS-JLEN > 500
               DISPLAY "  Hub: "
                   WS-JBUF(1:500)
           ELSE IF WS-JLEN > 0
               DISPLAY "  Hub: "
                   TRIM(WS-JBUF)
           ELSE
               DISPLAY "  Hub: empty!"
           END-IF
           END-IF

      *>   Check for flag
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
               DISPLAY "  >>> FLAG FOUND!"
               EXIT PARAGRAPH
           END-IF

      *>   Extract feedback message
           MOVE "message"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL

           IF TRIM(WS-JVAL)
               NOT = SPACES
      *>       Unescape into feedback
               MOVE SPACES
                   TO WS-FEEDBACK
               MOVE 0
                   TO WS-FEEDBACK-LEN
               MOVE LENGTH(
                   TRIM(WS-JVAL))
                   TO WS-K
               MOVE 1 TO WS-I
               PERFORM UNTIL
                   WS-I > WS-K
                   IF WS-I < WS-K
                   AND WS-JVAL(WS-I:1)
                       = X"5C"
                   AND WS-JVAL(
                       WS-I + 1:1)
                       = "n"
                       ADD 1
                       TO WS-FEEDBACK-LEN
                       MOVE " "
                       TO WS-FEEDBACK(
                       WS-FEEDBACK-LEN
                       :1)
                       ADD 2 TO WS-I
                   ELSE
                       ADD 1
                       TO WS-FEEDBACK-LEN
                       MOVE WS-JVAL(
                           WS-I:1)
                       TO WS-FEEDBACK(
                       WS-FEEDBACK-LEN
                       :1)
                       ADD 1 TO WS-I
                   END-IF
               END-PERFORM
               DISPLAY "  Feedback: "
                   WS-FEEDBACK(
                   1:WS-FEEDBACK-LEN)
           ELSE
               MOVE 0
                   TO WS-FEEDBACK-LEN
           END-IF
           .

      *> === Shared paragraphs (copybooks) ===
       COPY JSONESCAPE-PROC.
       COPY JSONREAD-PROC.
       COPY JSONPARSE-PROC.

       COPY ENVLOAD-PROC.
