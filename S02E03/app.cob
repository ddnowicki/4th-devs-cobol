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
      *> -- Config --
       01  WS-HUB-KEY              PIC X(50).
       01  WS-OPENAI-KEY           PIC X(200).
       01  WS-HUB-URL              PIC X(100).
       01  WS-OPENAI-URL           PIC X(200).
       01  WS-QT                   PIC X(1) VALUE '"'.
       01  WS-FS                   PIC XX.
       01  WS-WORK-PATH            PIC X(100)
                                   VALUE "work.tmp".

      *> -- URLs --
       01  WS-VERIFY-URL           PIC X(200).
       01  WS-LOG-URL              PIC X(200).

      *> -- System command --
       01  WS-CMD                  PIC X(4000).

      *> -- JSON newline: backslash + n --
       01  WS-NL                   PIC X(2).

      *> -- STRING pointer --
       01  WS-PTR                  PIC 9(5).

      *> -- Log parsing --
       01  WS-LOG-LINE             PIC X(500).
       01  WS-LOG-LEN              PIC 9(5).
       01  WS-EOF                  PIC X VALUE "N".
       01  WS-TOTAL-LINES          PIC 9(5) VALUE 0.

      *> -- Parsed fields for current line --
       01  WS-P-DATE               PIC X(10).
       01  WS-P-TIME               PIC X(8).
       01  WS-P-SEV                PIC X(4).
       01  WS-P-MSG                PIC X(300).

      *> -- Filtering --
       01  WS-MSG-LOWER            PIC X(300).
       01  WS-TALLY-CNT            PIC 9(4).
       01  WS-KW-FOUND             PIC X VALUE "N".

      *> -- Relevant entries array (max 300) --
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

      *> -- Dedup entries (max 200) --
       01  WS-DED-COUNT            PIC 9(3) VALUE 0.
       01  WS-DED-TABLE.
           05  WS-DED OCCURS 200 TIMES.
               10  WS-D-DATE      PIC X(10).
               10  WS-D-TIME      PIC X(8).
               10  WS-D-SEV       PIC X(4).
               10  WS-D-MSG       PIC X(300).
               10  WS-D-CNT       PIC 9(3)
                                  VALUE 1.

      *> -- Compact log buffer --
       01  WS-COMPACT              PIC X(16000).
       01  WS-COMPACT-LEN          PIC 9(5) VALUE 0.
       01  WS-COMPACT-LINE         PIC X(350).

      *> -- Condensed log (from LLM or compact) --
       01  WS-CONDENSED            PIC X(16000).
       01  WS-COND-LEN             PIC 9(5) VALUE 0.

      *> -- Token estimation --
       01  WS-EST-TOKENS           PIC 9(5).
       01  WS-TOKEN-LIMIT          PIC 9(5) VALUE 6000.

      *> -- Counters --
       01  WS-I                    PIC 9(5).
       01  WS-J                    PIC 9(5).
       01  WS-K                    PIC 9(5).
       01  WS-DUP-FOUND            PIC X VALUE "N".

      *> -- Request JSON buffer --
       01  WS-REQ-JSON             PIC X(32000).

      *> -- JSON buffer for parsing --
       01  WS-JBUF                 PIC X(16000).
       01  WS-JLEN                 PIC 9(5).
       01  WS-JPOS                 PIC 9(5).
       01  WS-JVAL                 PIC X(8000).

      *> -- JSON parsing temps --
       01  WS-KEY-SEARCH           PIC X(50).
       01  WS-KEY-POS              PIC 9(5).
       01  WS-VAL-START            PIC 9(5).
       01  WS-VAL-END              PIC 9(5).
       01  WS-FJV-POS              PIC 9(5).
       01  WS-TMP                  PIC X(500).

      *> -- JSON escaping --
       01  WS-ESC-BUF              PIC X(32000).
       01  WS-ESC-LEN              PIC 9(5).
       01  WS-ESC-I                PIC 9(5).
       01  WS-ESC-SRC              PIC X(16000).
       01  WS-ESC-SRC-LEN          PIC 9(5).

      *> -- Retry / feedback --
       01  WS-ATTEMPT              PIC 9(1).
       01  WS-SUCCESS              PIC X VALUE "N".
       01  WS-FEEDBACK             PIC X(2000).
       01  WS-FEEDBACK-LEN         PIC 9(5) VALUE 0.
       01  WS-FEEDBACK-ESC         PIC X(4000).
       01  WS-FEEDBACK-ESC-LEN     PIC 9(5).

      *> -- Response reading --
       01  WS-LINE                 PIC X(16000).

      *> -- Parse position helpers --
       01  WS-PPOS                 PIC 9(5).
       01  WS-PEND                 PIC 9(5).
       01  WS-CH                   PIC X(1).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S02E03 FAILURE LOG ==="

           ACCEPT WS-HUB-KEY
               FROM ENVIRONMENT "HUB_API_KEY"
           ACCEPT WS-OPENAI-KEY
               FROM ENVIRONMENT "OPENAI_API_KEY"
           ACCEPT WS-HUB-URL
               FROM ENVIRONMENT "HUB_API_URL"
           ACCEPT WS-OPENAI-URL
               FROM ENVIRONMENT "OPENAI_API_URL"

           IF WS-HUB-KEY = SPACES
               DISPLAY "ERR: HUB_API_KEY!"
               STOP RUN
           END-IF
           IF WS-OPENAI-KEY = SPACES
               DISPLAY "ERR: OPENAI_API_KEY!"
               STOP RUN
           END-IF
           IF WS-HUB-URL = SPACES
               DISPLAY "ERR: HUB_API_URL!"
               STOP RUN
           END-IF
           IF WS-OPENAI-URL = SPACES
               DISPLAY "ERR: OPENAI_API_URL!"
               STOP RUN
           END-IF

           MOVE SPACES TO WS-VERIFY-URL
           STRING TRIM(WS-HUB-URL)
               "/verify"
               DELIMITED SIZE
               INTO WS-VERIFY-URL
           END-STRING

      *>   Init JSON newline
           MOVE X"5C" TO WS-NL(1:1)
           MOVE "n"    TO WS-NL(2:1)

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
               TO WS-ESC-SRC
           MOVE WS-COMPACT-LEN
               TO WS-ESC-SRC-LEN
           PERFORM JSON-ESCAPE-BUF

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
                   TO WS-ESC-SRC
               MOVE WS-FEEDBACK-LEN
                   TO WS-ESC-SRC-LEN
               PERFORM JSON-ESCAPE-BUF
               STRING
                   "Previous feedback "
                   "from technicians:"
                   WS-NL
                   WS-ESC-BUF(
                   1:WS-ESC-LEN)
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
               TO WS-ESC-SRC
           MOVE WS-COMPACT-LEN
               TO WS-ESC-SRC-LEN
           PERFORM JSON-ESCAPE-BUF

           STRING
               "Logs to condense:"
               WS-NL
               WS-ESC-BUF(
               1:WS-ESC-LEN)
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
               TO WS-ESC-SRC
           MOVE WS-COND-LEN
               TO WS-ESC-SRC-LEN
           PERFORM JSON-ESCAPE-BUF

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
               WS-ESC-BUF(
               1:WS-ESC-LEN)
               WS-QT "}}"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Write and POST
           MOVE "hub_req.json"
               TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
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

      *> ============================================================
      *> JSON-ESCAPE-BUF: Escape src for JSON
      *> Input:  WS-ESC-SRC, WS-ESC-SRC-LEN
      *> Output: WS-ESC-BUF, WS-ESC-LEN
      *> ============================================================
       JSON-ESCAPE-BUF.
           MOVE SPACES TO WS-ESC-BUF
           MOVE 0 TO WS-ESC-LEN

           PERFORM VARYING WS-ESC-I
               FROM 1 BY 1
               UNTIL WS-ESC-I
               > WS-ESC-SRC-LEN
               EVALUATE TRUE
               WHEN WS-ESC-SRC(
                   WS-ESC-I:1)
                   = WS-QT
                   ADD 1 TO WS-ESC-LEN
                   MOVE X"5C"
                       TO WS-ESC-BUF(
                       WS-ESC-LEN:1)
                   ADD 1 TO WS-ESC-LEN
                   MOVE WS-QT
                       TO WS-ESC-BUF(
                       WS-ESC-LEN:1)
               WHEN WS-ESC-SRC(
                   WS-ESC-I:1)
                   = X"5C"
                   ADD 1 TO WS-ESC-LEN
                   MOVE X"5C"
                       TO WS-ESC-BUF(
                       WS-ESC-LEN:1)
                   ADD 1 TO WS-ESC-LEN
                   MOVE X"5C"
                       TO WS-ESC-BUF(
                       WS-ESC-LEN:1)
               WHEN WS-ESC-SRC(
                   WS-ESC-I:1)
                   = X"0A"
                   ADD 1 TO WS-ESC-LEN
                   MOVE X"5C"
                       TO WS-ESC-BUF(
                       WS-ESC-LEN:1)
                   ADD 1 TO WS-ESC-LEN
                   MOVE "n"
                       TO WS-ESC-BUF(
                       WS-ESC-LEN:1)
               WHEN WS-ESC-SRC(
                   WS-ESC-I:1)
                   = X"0D"
                   CONTINUE
               WHEN WS-ESC-SRC(
                   WS-ESC-I:1)
                   = X"09"
                   ADD 1 TO WS-ESC-LEN
                   MOVE X"5C"
                       TO WS-ESC-BUF(
                       WS-ESC-LEN:1)
                   ADD 1 TO WS-ESC-LEN
                   MOVE "t"
                       TO WS-ESC-BUF(
                       WS-ESC-LEN:1)
               WHEN OTHER
                   ADD 1 TO WS-ESC-LEN
                   MOVE WS-ESC-SRC(
                       WS-ESC-I:1)
                       TO WS-ESC-BUF(
                       WS-ESC-LEN:1)
               END-EVALUATE
           END-PERFORM
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
                       IF WS-K > 0
                           IF WS-JLEN > 0
                               ADD 1
                                 TO WS-JLEN
                               MOVE " "
                                 TO WS-JBUF(
                                 WS-JLEN:1)
                           END-IF
                           IF WS-JLEN
                               + WS-K
                               < 16000
                               MOVE WS-LINE(
                                   1:WS-K)
                                   TO WS-JBUF(
                                   WS-JLEN
                                   + 1:WS-K)
                               ADD WS-K
                                   TO WS-JLEN
                           END-IF
                       END-IF
               END-READ
           END-PERFORM

           CLOSE WORK-FILE
           MOVE "N" TO WS-EOF
           .

      *> ============================================================
      *> FIND-JSON-VAL: Find value for key
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
               UNTIL WS-FJV-POS
                   > WS-JLEN
               OR WS-KEY-POS > 0
               IF WS-FJV-POS
                   + LENGTH(
                   TRIM(WS-TMP))
                   - 1 <= WS-JLEN
               AND WS-JBUF(
                   WS-FJV-POS:
                   LENGTH(
                   TRIM(WS-TMP)))
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
               + LENGTH(
               TRIM(WS-TMP))
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

           IF WS-JBUF(
               WS-FJV-POS:1) = WS-QT
               ADD 1 TO WS-FJV-POS
               MOVE WS-FJV-POS
                   TO WS-VAL-START
               PERFORM UNTIL
                   WS-FJV-POS
                   > WS-JLEN
                   IF WS-JBUF(
                       WS-FJV-POS:1)
                       = WS-QT
                       IF WS-FJV-POS
                           > 1
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
               IF WS-VAL-END
                   >= WS-VAL-START
               AND WS-VAL-END
                   - WS-VAL-START
                   + 1 <= 8000
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
                   WS-FJV-POS
                   > WS-JLEN
                   OR WS-JBUF(
                   WS-FJV-POS:1)
                   = ","
                   OR WS-JBUF(
                   WS-FJV-POS:1)
                   = "}"
                   OR WS-JBUF(
                   WS-FJV-POS:1)
                   = "]"
                   OR WS-JBUF(
                   WS-FJV-POS:1)
                   = " "
                   ADD 1
                   TO WS-FJV-POS
               END-PERFORM
               COMPUTE WS-VAL-END =
                   WS-FJV-POS - 1
               IF WS-VAL-END
                   >= WS-VAL-START
                   MOVE WS-JBUF(
                       WS-VAL-START:
                       WS-VAL-END
                       - WS-VAL-START
                       + 1) TO WS-JVAL
               END-IF
           END-IF
           MOVE WS-FJV-POS
               TO WS-JPOS
           .
