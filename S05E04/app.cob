       IDENTIFICATION DIVISION.
       PROGRAM-ID. S05E04-GOINGTHERE-V2.
      *> ============================================================
      *> S05E04 v2 - Goingthere, parallel implementation.
      *>
      *> Goal: first-try success without leaning on the restart
      *> loop. The restart cap is therefore dropped from 20 to 3.
      *>
      *> Four upgrades vs v1 (see v1 app.cob for v1 baseline):
      *>   1. Rate-limit handler no longer blind-walks; when all
      *>      three refetches throttle we fall back to a freeRows-
      *>      aware base-row-seeking safe move policy.
      *>   2. Scanner 502 / gateway-error fallback skips LLM
      *>      extract + disarm entirely and marks the scanner as
      *>      UNKNOWN so we never submit a hash built from the word
      *>      "error code".
      *>   3. Deterministic hint rewriter runs BEFORE the LLM. It
      *>      tags each directional slot SAFE/DANGER via windowed
      *>      vocabulary scanning and resolves the common cases
      *>      without an LLM roundtrip.
      *>   4. When the rewriter defers, a tightened LLM prompt with
      *>      6 few-shots pulled from the v1 run logs takes over.
      *>      Labels shrunk from 7 (up/down/ahead/mid1/mid2/mid3/
      *>      unknown) to 4 (UP/DOWN/AHEAD/MIDDLE).
      *>
      *> CLAUDE.md: zero hardcoded URLs / keys; everything via env
      *> vars. Pure COBOL + CALL "SYSTEM" for curl + sha1sum +
      *> CALL "C$SLEEP" for pacing.
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
       01  WORK-REC                PIC X(65535).

       WORKING-STORAGE SECTION.
      *> === Environment (copybook) ===
       COPY ENVLOAD-WS.

      *> === File I/O ===
       01  WS-BS                   PIC X(1)
                                   VALUE X"5C".
       01  WS-FS                   PIC XX.
       01  WS-WORK-PATH            PIC X(100)
                                   VALUE "work.tmp".

      *> -- STRING pointer --
       01  WS-PTR                  PIC 9(6).

      *> -- System command --
       01  WS-CMD                  PIC X(4000).

      *> -- Hub request body --
       01  WS-HUB-BODY             PIC X(16000).

      *> === JSONPARSE-WS (inline, enlarged for preview payloads) ==
       01  WS-JBUF                 PIC X(65535).
      *> -- v6: lowercase mirror of WS-JBUF used only by
      *> -- CHECK-SCAN-LOOKS-REAL (pre-LLM token gate). No fuzzy
      *> -- matcher / canon buffer any more.
       01  WS-JBUF-LC              PIC X(65535).
       01  WS-JLEN                 PIC 9(6).
       01  WS-JPOS                 PIC 9(6).
       01  WS-JVAL                 PIC X(8000).
       01  WS-KEY-SEARCH           PIC X(50).
       01  WS-KEY-POS              PIC 9(6).
       01  WS-VAL-START            PIC 9(6).
       01  WS-VAL-END              PIC 9(6).
       01  WS-FJV-POS              PIC 9(6).
       01  WS-TMP                  PIC X(4000).
       01  WS-TMP2                 PIC X(500).

      *> === JSONESCAPE-WS (inline) ===
       01  WS-ESC-IN               PIC X(8000).
       01  WS-ESC-OUT              PIC X(16000).
       01  WS-ESC-ILEN             PIC 9(6).
       01  WS-ESC-OLEN             PIC 9(6).
       01  WS-ESC-I                PIC 9(6).
      *> -- JSON-UNESCAPE-STR \uXXXX decoder scratch fields --
       01  WS-UNESC-CP             PIC 9(5).
       01  WS-UNESC-HX             PIC X.
       01  WS-UNESC-NIB            PIC 9(2).
       01  WS-UNESC-K              PIC 9(1).
       01  WS-UNESC-B              PIC 9(3).
       01  WS-UNESC-Q              PIC 9(5).
       01  WS-UNESC-OK             PIC X.

      *> === JSONREAD-WS (inline, enlarged) ===
       01  WS-EOF                  PIC X VALUE "N".
       01  WS-LINE                 PIC X(65535).
       01  WS-K                    PIC 9(6).

      *> === Game-specific WS ===
       COPY "ROCKET-NAV-WS.cpy".

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S05E04 GOINGTHERE V2 ==="

           PERFORM LOAD-ENV-VARS
           PERFORM BUILD-URLS
           PERFORM RECORD-START-TIME
           PERFORM LOAD-REWRITER-TABLES
           PERFORM LOAD-PROMPT-CONSTANTS

      *>   Clean temp files from any prior run
           INITIALIZE WS-CMD
           STRING
               "rm -f hub_resp.json "
               "hub_req.tmp "
               "scan_resp.txt "
               "hint_req.tmp hint_resp.json "
               "disarm_req.tmp "
               "disarm_resp.json "
               "sha_in.txt sha_out.txt "
               "work.tmp"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           MOVE "N" TO WS-FLAG-FOUND

           PERFORM CHECK-WALL-CLOCK
           IF WS-ELAPSED >= WS-BUDGET-SEC
               DISPLAY "  [main] wall-clock budget "
                   "exhausted - not starting"
           END-IF
           IF WS-ELAPSED < WS-BUDGET-SEC
               PERFORM PLAY-ONE-GAME
           END-IF

           DISPLAY " "
           IF WS-FLAG-FOUND = "Y"
               DISPLAY "=== SUCCESS ==="
               DISPLAY "FLAG: " TRIM(WS-FLAG-TEXT)
               STOP RUN
           ELSE
               DISPLAY "=== FAILURE: single attempt crashed ==="
               MOVE 1 TO RETURN-CODE
               STOP RUN
           END-IF
           .

      *> ============================================================
      *> BUILD-URLS
      *> ============================================================
       BUILD-URLS.
           MOVE SPACES TO WS-SCANNER-URL
           STRING TRIM(WS-HUB-URL)
               "/api/frequencyScanner"
               DELIMITED SIZE
               INTO WS-SCANNER-URL
           END-STRING

           MOVE SPACES TO WS-SCANNER-GET-URL
           STRING TRIM(WS-SCANNER-URL)
               "?key="
               TRIM(WS-HUB-KEY)
               DELIMITED SIZE
               INTO WS-SCANNER-GET-URL
           END-STRING

           MOVE SPACES TO WS-HINT-URL
           STRING TRIM(WS-HUB-URL)
               "/api/getmessage"
               DELIMITED SIZE
               INTO WS-HINT-URL
           END-STRING

           MOVE SPACES TO WS-PREVIEW-URL
           STRING TRIM(WS-HUB-URL)
               "/goingthere_backend"
               DELIMITED SIZE
               INTO WS-PREVIEW-URL
           END-STRING
           .

      *> ============================================================
      *> RECORD-START-TIME / CHECK-WALL-CLOCK
      *> ============================================================
       RECORD-START-TIME.
           MOVE FUNCTION CURRENT-DATE TO WS-CURR-DT
           COMPUTE WS-START-TIME =
               WS-DT-HOUR * 3600
               + WS-DT-MIN * 60
               + WS-DT-SEC
           .

       CHECK-WALL-CLOCK.
           MOVE FUNCTION CURRENT-DATE TO WS-CURR-DT
           COMPUTE WS-NOW-TIME =
               WS-DT-HOUR * 3600
               + WS-DT-MIN * 60
               + WS-DT-SEC
           IF WS-NOW-TIME >= WS-START-TIME
               COMPUTE WS-ELAPSED =
                   WS-NOW-TIME - WS-START-TIME
           ELSE
               COMPUTE WS-ELAPSED =
                   WS-NOW-TIME + 86400 - WS-START-TIME
           END-IF
           .

      *> ============================================================
      *> PLAY-ONE-GAME
      *> Run one attempt. Preview + BFS if available, otherwise
      *> hint mode per column.
      *> ============================================================
       PLAY-ONE-GAME.
           MOVE "N" TO WS-RATE-LIMIT-SEEN
           MOVE "N" TO WS-RATE-LIMIT-FAIL
           MOVE 2 TO WS-STEP-SLEEP

           PERFORM START-GAME
           IF WS-FLAG-FOUND = "Y"
               EXIT PARAGRAPH
           END-IF
           CALL "C$SLEEP" USING 2

           IF WS-PLAYER-ROW = 0 OR WS-PLAYER-COL = 0
               DISPLAY "  [game] start failed to set "
                   "player position - aborting attempt"
               EXIT PARAGRAPH
           END-IF

           MOVE "H" TO WS-MODE
           MOVE 0 TO WS-PLAN-CNT
           PERFORM FETCH-PREVIEW
           IF WS-PREVIEW-OK = "Y"
               PERFORM PLAN-PATH
               IF WS-PLAN-CNT > 0
                   MOVE "P" TO WS-MODE
                   MOVE WS-PREVIEW-PLAYER-ROW TO WS-PLAYER-ROW
                   MOVE WS-PREVIEW-PLAYER-COL TO WS-PLAYER-COL
                   MOVE WS-PREVIEW-BASE-ROW TO WS-BASE-ROW
                   MOVE WS-PREVIEW-BASE-COL TO WS-BASE-COL
                   DISPLAY "  [plan] mode=planned moves="
                       WS-PLAN-CNT
               ELSE
                   DISPLAY "  [plan] BFS failed "
                       "- hint mode fallback"
               END-IF
           ELSE
               DISPLAY "  [plan] mode=hint "
                   "(preview unavailable or fog)"
           END-IF

           MOVE 0 TO WS-STEP
           PERFORM UNTIL WS-STEP >= WS-MAX-STEPS
                      OR WS-FLAG-FOUND = "Y"
                      OR WS-PLAYER-COL >= WS-BASE-COL

               ADD 1 TO WS-STEP
               MOVE WS-STEP TO WS-STEP-DSP
               MOVE WS-PLAYER-ROW TO WS-ROW-DSP
               MOVE WS-PLAYER-COL TO WS-COL-DSP
               DISPLAY " "
               DISPLAY "  [step " WS-STEP-DSP
                   "] at row " WS-ROW-DSP
                   " col " WS-COL-DSP
                   " base_row " WS-BASE-ROW
                   " free "
                   WS-FREE-ROW-1
                   WS-FREE-ROW-2
                   WS-FREE-ROW-3
                   " mode " WS-MODE

               PERFORM CHECK-WALL-CLOCK
               IF WS-ELAPSED >= WS-BUDGET-SEC
                   DISPLAY "  [wall-clock] aborting attempt"
                   EXIT PARAGRAPH
               END-IF

      *>       1. Scanner with v2 502-skip policy
               PERFORM SCAN-FREQUENCY-V2
               IF WS-SCAN-TRAP = "Y" AND WS-SCAN-UNKNOWN = "N"
                   PERFORM DISARM-TRAP
                   IF WS-SCAN-TRAP = "Y"
                       DISPLAY "  [disarm] failed - abort"
                       EXIT PARAGRAPH
                   END-IF
               END-IF

               IF WS-MODE = "P"
                   IF WS-STEP > WS-PLAN-CNT
                       MOVE "go" TO WS-MOVE
                       DISPLAY "  [move] go "
                           "(planned exhausted)"
                   ELSE
                       MOVE WS-PLAN-MOVE(WS-STEP) TO WS-MOVE
                       DISPLAY "  [move] " TRIM(WS-MOVE)
                           " (planned)"
                   END-IF
               ELSE
      *>           Hint mode: fetch hint (v2 long sleeps), run
      *>           rewriter, maybe LLM, pick move.
                   CALL "C$SLEEP" USING 2
                   PERFORM GET-HINT-V2
                   PERFORM CLASSIFY-HINT-V2
                   PERFORM PICK-MOVE-V2
                   DISPLAY "  [move] " TRIM(WS-MOVE)
               END-IF

      *>       V4-3: capture the stone row of the column we are
      *>       about to leave. freeRow=0 marks a stone; if exactly
      *>       one row has freeRow=0 that is the prev stone row.
               MOVE 0 TO WS-PREV-STONE-ROW
               IF WS-FREE-ROW-1 = 0
               AND WS-FREE-ROW-2 = 1
               AND WS-FREE-ROW-3 = 1
                   MOVE 1 TO WS-PREV-STONE-ROW
               END-IF
               IF WS-FREE-ROW-1 = 1
               AND WS-FREE-ROW-2 = 0
               AND WS-FREE-ROW-3 = 1
                   MOVE 2 TO WS-PREV-STONE-ROW
               END-IF
               IF WS-FREE-ROW-1 = 1
               AND WS-FREE-ROW-2 = 1
               AND WS-FREE-ROW-3 = 0
                   MOVE 3 TO WS-PREV-STONE-ROW
               END-IF

               PERFORM SEND-MOVE
               PERFORM CHECK-FLAG-IN-RESP
               IF WS-FLAG-FOUND = "Y"
                   EXIT PARAGRAPH
               END-IF

               PERFORM CHECK-CRASH-IN-RESP
               IF WS-SCAN-TRAP = "C"
                   DISPLAY "  CRASH - aborting (no restart)"
                   EXIT PARAGRAPH
               END-IF

               PERFORM UPDATE-STATE-FROM-RESP

      *>       Inter-step sleep (bumped after rate-limit seen)
               CALL "C$SLEEP" USING WS-STEP-SLEEP
           END-PERFORM
           .

      *> ============================================================
      *> START-GAME
      *> POST {apikey,task,answer:{command:"start"}} to /verify.
      *> ============================================================
       START-GAME.
           MOVE SPACES TO WS-HUB-BODY
           MOVE 1 TO WS-PTR
           STRING
               "{"
               WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME) WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "command" WS-QT ":"
               WS-QT "start" WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           PERFORM POST-HUB-VERIFY

           DISPLAY "  [start] resp(400): "
               WS-JBUF(1:400)

           PERFORM CHECK-FLAG-IN-RESP
           IF WS-FLAG-FOUND = "Y"
               EXIT PARAGRAPH
           END-IF

           MOVE 2 TO WS-PLAYER-ROW
           MOVE 1 TO WS-PLAYER-COL
           MOVE 2 TO WS-BASE-ROW
           MOVE 12 TO WS-BASE-COL

           PERFORM PARSE-PLAYER-POS
           PERFORM PARSE-BASE-POS
           PERFORM PARSE-FREE-ROWS

           MOVE WS-PLAYER-ROW TO WS-ROW-DSP
           MOVE WS-PLAYER-COL TO WS-COL-DSP
           DISPLAY "  [start] player=(" WS-ROW-DSP
               "," WS-COL-DSP ") base_row="
               WS-BASE-ROW " base_col="
               WS-BASE-COL
           .

      *> ============================================================
      *> POST-HUB-VERIFY
      *> ============================================================
       POST-HUB-VERIFY.
           MOVE 0 TO WS-RETRY
           PERFORM UNTIL WS-RETRY >= WS-MAX-RETRY
               ADD 1 TO WS-RETRY

               MOVE "hub_req.tmp" TO WS-WORK-PATH
               OPEN OUTPUT WORK-FILE
               IF WS-FS NOT = "00"
                   DISPLAY "    hub_req open err " WS-FS
                   MOVE "work.tmp" TO WS-WORK-PATH
                   EXIT PARAGRAPH
               END-IF
               WRITE WORK-REC FROM WS-HUB-BODY
               CLOSE WORK-FILE
               MOVE "work.tmp" TO WS-WORK-PATH

               INITIALIZE WS-CMD
               STRING "rm -f hub_resp.json"
                   DELIMITED SIZE INTO WS-CMD
               END-STRING
               CALL "SYSTEM" USING WS-CMD

               INITIALIZE WS-CMD
               STRING
                   "curl -s --max-time 30 "
                   "-o hub_resp.json"
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

               MOVE "hub_resp.json" TO WS-WORK-PATH
               PERFORM READ-JSON-FILE
               MOVE "work.tmp" TO WS-WORK-PATH

               IF WS-JLEN > 0
                   EXIT PERFORM
               END-IF
               DISPLAY "    [retry " WS-RETRY
                   "] empty hub response"
               CALL "C$SLEEP" USING 1
           END-PERFORM
           .

      *> ============================================================
      *> SCAN-FREQUENCY-V2  (Upgrade 2)
      *> GET scanner. On gateway-error bodies, retry up to 4 times,
      *> then MARK the scanner as UNKNOWN and skip LLM extract + 
      *> disarm entirely. Only run LLM extract when the body looks
      *> like real scanner JSON (has freq/weap/track/detect tokens).
      *> ============================================================
       SCAN-FREQUENCY-V2.
           MOVE "N" TO WS-SCAN-TRAP
           MOVE "N" TO WS-SCAN-UNKNOWN
           MOVE 0 TO WS-FREQUENCY
           MOVE SPACES TO WS-DETECT-CODE

           MOVE 0 TO WS-RETRY
           PERFORM UNTIL WS-RETRY >= WS-MAX-RETRY
               ADD 1 TO WS-RETRY
               INITIALIZE WS-CMD
               STRING "rm -f scan_resp.txt"
                   DELIMITED SIZE INTO WS-CMD
               END-STRING
               CALL "SYSTEM" USING WS-CMD

               INITIALIZE WS-CMD
               STRING
                   "curl -s --max-time 30 "
                   "-o scan_resp.txt "
                   WS-QT
                   TRIM(WS-SCANNER-GET-URL)
                   WS-QT
                   DELIMITED SIZE
                   INTO WS-CMD
               END-STRING
               CALL "SYSTEM" USING WS-CMD

               MOVE "scan_resp.txt" TO WS-WORK-PATH
               PERFORM READ-JSON-FILE
               MOVE "work.tmp" TO WS-WORK-PATH

               IF WS-JLEN > 0
                   PERFORM CHECK-SCAN-GATEWAY-ERR
                   IF WS-SCAN-GW-ERR = "N"
                       EXIT PERFORM
                   END-IF
                   DISPLAY "    [scan retry " WS-RETRY
                       "] gateway err body"
                   CALL "C$SLEEP" USING 2
               ELSE
                   DISPLAY "    [scan retry " WS-RETRY "]"
                   CALL "C$SLEEP" USING 1
               END-IF
           END-PERFORM

           IF WS-JLEN = 0
               DISPLAY "  [scan] empty - assume clear"
               EXIT PARAGRAPH
           END-IF

           DISPLAY "  [scan] body(200): "
               WS-JBUF(1:200)

      *>   V2-1: build lowercased copy of the body for case-
      *>   insensitive key lookups. Walks WS-JBUF byte-by-byte,
      *>   lowercasing A-Z only and preserving all other bytes.
           PERFORM BUILD-JBUF-LC

      *>   If after all retries we still have a gateway body,
      *>   skip disarm entirely. Do NOT feed "error code: 502"
      *>   into the LLM extractor.
           PERFORM CHECK-SCAN-GATEWAY-ERR
           IF WS-SCAN-GW-ERR = "Y"
               DISPLAY "  [scan] persistent gateway err - "
                   "skipping disarm, marking UNKNOWN"
               MOVE "N" TO WS-SCAN-TRAP
               MOVE "Y" TO WS-SCAN-UNKNOWN
               EXIT PARAGRAPH
           END-IF

      *>   Real body -> decide trap vs clear with the v1 heuristic.
           PERFORM IS-SCAN-CLEAR
           IF WS-SCAN-TRAP = "N"
               EXIT PARAGRAPH
           END-IF

      *>   Trap-suspected. Only skip disarm if the body has NO
      *>   scanner tokens at all (e.g. "error code: 502"). If
      *>   tokens exist we try COBOL parse first, then LLM extract.
           PERFORM CHECK-SCAN-LOOKS-REAL
           IF WS-SCAN-LOOKS-REAL = "N"
               DISPLAY "  [scan] trap-ish body but no real "
                   "scanner tokens - skip LLM extract, UNKNOWN"
               MOVE "N" TO WS-SCAN-TRAP
               MOVE "Y" TO WS-SCAN-UNKNOWN
               EXIT PARAGRAPH
           END-IF

           PERFORM PARSE-SCANNER-FIELDS

           IF WS-FREQUENCY = 0 OR WS-DETECT-CODE = SPACES
               DISPLAY "  [scan] det parse fail - LLM extract"
               PERFORM LLM-EXTRACT-SCANNER-V2
           END-IF

           IF WS-FREQUENCY = 0 OR WS-DETECT-CODE = SPACES
               DISPLAY "  [scan] extract still empty - "
                   "marking scanner UNKNOWN, skip disarm"
               MOVE "N" TO WS-SCAN-TRAP
               MOVE "Y" TO WS-SCAN-UNKNOWN
           END-IF
           .

      *> ============================================================
      *> BUILD-JBUF-LC  (v6 - simplified)
      *> Copy WS-JBUF(1:WS-JLEN) into WS-JBUF-LC, lowercasing A-Z
      *> only and preserving all other bytes verbatim. This mirror
      *> is only consumed by CHECK-SCAN-LOOKS-REAL. The v3-1 canon
      *> buffer (WS-JBUF-LC-CANON) and its fuzzy-key machinery are
      *> gone in v6; the new scanner path tries a literal parse
      *> first and falls straight through to the LLM.
      *> ============================================================
       BUILD-JBUF-LC.
           MOVE SPACES TO WS-JBUF-LC
           IF WS-JLEN = 0
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION LOWER-CASE(WS-JBUF(1:WS-JLEN))
               TO WS-JBUF-LC(1:WS-JLEN)
           .

      *> ============================================================
      *> CHECK-SCAN-GATEWAY-ERR
      *> ============================================================
       CHECK-SCAN-GATEWAY-ERR.
           MOVE "N" TO WS-SCAN-GW-ERR
           IF WS-JLEN = 0
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-LINE
           MOVE FUNCTION LOWER-CASE(WS-JBUF(1:WS-JLEN))
               TO WS-LINE(1:WS-JLEN)
           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-LINE(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "error code"
           INSPECT WS-LINE(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "502"
           INSPECT WS-LINE(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "503"
           INSPECT WS-LINE(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "504"
           INSPECT WS-LINE(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "gateway"
           INSPECT WS-LINE(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "bad gateway"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-SCAN-GW-ERR
           END-IF
           .

      *> ============================================================
      *> CHECK-SCAN-LOOKS-REAL (v6)
      *> Conservative pre-LLM gate: body is a real scanner payload
      *> only if we see at least ONE of the genuine scanner tokens
      *> in the lowercased mirror. Token list keeps both canonical
      *> spellings (frequency, detect, track, being, data) AND the
      *> observed b/d, q/p substitutions (frepuency, trackeb, bata,
      *> betect) so obviously-corrupted bodies still route to the
      *> LLM rather than being rejected here.
      *> ============================================================
       CHECK-SCAN-LOOKS-REAL.
           MOVE "N" TO WS-SCAN-LOOKS-REAL
           IF WS-JLEN = 0
               EXIT PARAGRAPH
           END-IF
           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-JBUF-LC(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "frequency"
           INSPECT WS-JBUF-LC(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "frepuency"
           INSPECT WS-JBUF-LC(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "freq"
           INSPECT WS-JBUF-LC(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "frep"
           INSPECT WS-JBUF-LC(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "weap"
           INSPECT WS-JBUF-LC(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "weaq"
           INSPECT WS-JBUF-LC(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "track"
           INSPECT WS-JBUF-LC(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "trackeb"
           INSPECT WS-JBUF-LC(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "detect"
           INSPECT WS-JBUF-LC(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "betect"
           INSPECT WS-JBUF-LC(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "missile"
           INSPECT WS-JBUF-LC(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "being"
           INSPECT WS-JBUF-LC(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "bein"
           INSPECT WS-JBUF-LC(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "bata"
           INSPECT WS-JBUF-LC(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "rocket"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-SCAN-LOOKS-REAL
           END-IF
           .

      *> ============================================================
      *> IS-SCAN-CLEAR (v2 - trimmed, gateway handled by caller)
      *> ============================================================
       IS-SCAN-CLEAR.
           MOVE 0 TO WS-TALLY-CNT
           MOVE SPACES TO WS-LINE
           MOVE FUNCTION LOWER-CASE(WS-JBUF(1:WS-JLEN))
               TO WS-LINE(1:WS-JLEN)

           INSPECT WS-LINE(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "weap"
           INSPECT WS-LINE(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "miss"
           INSPECT WS-LINE(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "track"
           INSPECT WS-LINE(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "detect"
           INSPECT WS-LINE(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "betect"
           INSPECT WS-LINE(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "being"
           INSPECT WS-LINE(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "bein"
           INSPECT WS-LINE(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "bata"
           INSPECT WS-LINE(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "freq"
           INSPECT WS-LINE(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "frep"
           INSPECT WS-LINE(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "freb"
           INSPECT WS-LINE(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "air-to"
           INSPECT WS-LINE(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "rocket"
           INSPECT WS-LINE(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "targeted"
           INSPECT WS-LINE(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "radar"

           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-SCAN-TRAP
           ELSE
               MOVE "N" TO WS-SCAN-TRAP
           END-IF
           .

      *> ============================================================
      *> PARSE-SCANNER-FIELDS (v6 Stage 1)
      *> Try to pull frequency and detectionCode out of WS-JBUF as
      *> literal JSON keys via FIND-JSON-VAL. No canon, no lowercase
      *> fallback, no fuzzy spellings. If the server returned an
      *> uncorrupted body this wins immediately; otherwise both
      *> fields come back SPACES / 0 and the caller falls straight
      *> through to LLM-EXTRACT-SCANNER-V2.
      *>
      *> Note: FIND-JSON-VAL searches WS-JBUF case-sensitively, so
      *> only the canonical "frequency" / "detectionCode" spellings
      *> hit here. Anything else -> Stage 2.
      *> ============================================================
       PARSE-SCANNER-FIELDS.
           MOVE 0 TO WS-FREQUENCY
           MOVE SPACES TO WS-DETECT-CODE

      *>   frequency (bare integer in the real body)
           MOVE "frequency" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF WS-JVAL NOT = SPACES
              AND FUNCTION TRIM(WS-JVAL) NOT = "null"
               PERFORM CONVERT-FREQ-VAL
           END-IF

      *>   detectionCode (quoted 6-char string in the real body)
           MOVE "detectionCode" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF WS-JVAL NOT = SPACES
              AND FUNCTION TRIM(WS-JVAL) NOT = "null"
               MOVE WS-JVAL TO WS-DETECT-CODE
               PERFORM STRIP-DETECT-CODE-TAIL
           END-IF

           MOVE WS-FREQUENCY TO WS-FREQUENCY-DSP
           DISPLAY "  [scan] stage1 freq=" WS-FREQUENCY-DSP
               " code="
               TRIM(WS-DETECT-CODE)(1:60)
           .

      *> ============================================================
      *> STRIP-DETECT-CODE-TAIL (unchanged vs v1; apostrophe as
      *> X"27" literal)
      *> ============================================================
       STRIP-DETECT-CODE-TAIL.
           MOVE 0 TO WS-DETECT-CODE-LEN
           PERFORM VARYING WS-K FROM 200 BY -1
               UNTIL WS-K < 1
                  OR WS-DETECT-CODE-LEN > 0
               IF WS-DETECT-CODE(WS-K:1) NOT = " "
                   MOVE WS-K TO WS-DETECT-CODE-LEN
               END-IF
           END-PERFORM

           IF WS-DETECT-CODE-LEN = 0
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL WS-DETECT-CODE-LEN = 0
                     OR (WS-DETECT-CODE(
                         WS-DETECT-CODE-LEN:1) NOT = X"27"
                     AND WS-DETECT-CODE(
                         WS-DETECT-CODE-LEN:1) NOT = X"60"
                     AND WS-DETECT-CODE(
                         WS-DETECT-CODE-LEN:1) NOT = X"22"
                     AND WS-DETECT-CODE(
                         WS-DETECT-CODE-LEN:1) NOT = "}"
                     AND WS-DETECT-CODE(
                         WS-DETECT-CODE-LEN:1) NOT = ")"
                     AND WS-DETECT-CODE(
                         WS-DETECT-CODE-LEN:1) NOT = " ")
               MOVE " " TO WS-DETECT-CODE(
                   WS-DETECT-CODE-LEN:1)
               SUBTRACT 1 FROM WS-DETECT-CODE-LEN
           END-PERFORM
           .

      *> ============================================================
      *> CONVERT-FREQ-VAL (unchanged vs v1)
      *> ============================================================
       CONVERT-FREQ-VAL.
           MOVE 0 TO WS-FREQUENCY
           MOVE 0 TO WS-K
           INSPECT FUNCTION TRIM(WS-JVAL) TALLYING
               WS-K FOR ALL "."
           IF WS-K > 0
               MOVE 0 TO WS-K
           END-IF

           MOVE 1 TO WS-M
           PERFORM VARYING WS-N FROM 1 BY 1
               UNTIL WS-N > 10
                  OR WS-JVAL(WS-N:1) NOT >= "0"
                  OR WS-JVAL(WS-N:1) NOT <= "9"
               COMPUTE WS-FREQUENCY =
                   WS-FREQUENCY * 10
                   + (FUNCTION NUMVAL(WS-JVAL(WS-N:1)))
           END-PERFORM
           .

      *> ============================================================
      *> DISARM-TRAP (unchanged vs v1)
      *> ============================================================
       DISARM-TRAP.
           INITIALIZE WS-CMD
           STRING
               "printf "
               WS-QT "%s" WS-QT
               " "
               WS-QT
               TRIM(WS-DETECT-CODE)
               "disarm"
               WS-QT
               " > sha_in.txt"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           INITIALIZE WS-CMD
           STRING
               "sha1sum sha_in.txt"
               " | cut -c1-40"
               " > sha_out.txt"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           MOVE SPACES TO WS-DISARM-HASH
           MOVE "sha_out.txt" TO WS-WORK-PATH
           OPEN INPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "  [disarm] cannot read sha_out "
                   "(FS=" WS-FS ")"
               MOVE "work.tmp" TO WS-WORK-PATH
               EXIT PARAGRAPH
           END-IF
           READ WORK-FILE INTO WS-LINE
               AT END
                   CONTINUE
               NOT AT END
                   MOVE WS-LINE(1:40) TO WS-DISARM-HASH
           END-READ
           CLOSE WORK-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           DISPLAY "  [disarm] hash=" WS-DISARM-HASH

           MOVE WS-FREQUENCY TO WS-FREQ-EDIT
           MOVE SPACES TO WS-HUB-BODY
           MOVE 1 TO WS-PTR
           STRING
               "{"
               WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "frequency" WS-QT ":"
               FUNCTION TRIM(WS-FREQ-EDIT LEADING)
               ","
               WS-QT "disarmHash" WS-QT ":"
               WS-QT TRIM(WS-DISARM-HASH) WS-QT
               "}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           MOVE "disarm_req.tmp" TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "  [disarm] open err " WS-FS
               MOVE "work.tmp" TO WS-WORK-PATH
               EXIT PARAGRAPH
           END-IF
           WRITE WORK-REC FROM WS-HUB-BODY
           CLOSE WORK-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           INITIALIZE WS-CMD
           STRING "rm -f disarm_resp.json"
               DELIMITED SIZE INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           INITIALIZE WS-CMD
           STRING
               "curl -s --max-time 30 "
               "-o disarm_resp.json"
               " -X POST "
               TRIM(WS-SCANNER-URL)
               " -H " WS-QT
               "Content-Type: "
               "application/json"
               WS-QT
               " -d @disarm_req.tmp"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           MOVE "disarm_resp.json" TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           DISPLAY "  [disarm] resp(200): "
               WS-JBUF(1:200)

           MOVE "N" TO WS-SCAN-TRAP
           IF WS-JLEN = 0
               MOVE "Y" TO WS-SCAN-TRAP
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-JBUF(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "-430"
           IF WS-TALLY-CNT > 0
               DISPLAY "  [disarm] code -430 (no trap)"
               MOVE "N" TO WS-SCAN-TRAP
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-JBUF(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL "error"
           IF WS-TALLY-CNT > 0
               DISPLAY "  [disarm] error in response"
               MOVE "Y" TO WS-SCAN-TRAP
               EXIT PARAGRAPH
           END-IF
           .

      *> ============================================================
      *> GET-HINT-V2
      *> POST {apikey} to /api/getmessage with v2 retry pacing.
      *> Rate-limit backoff is *longer* than v1 and is owned by
      *> CLASSIFY-HINT-V2 via PERFORM GET-HINT-V2 inside its
      *> refetch loop. This paragraph just fetches once per call.
      *> ============================================================
       GET-HINT-V2.
           MOVE SPACES TO WS-HINT-TEXT
           MOVE SPACES TO WS-HINT-LOWER
           MOVE 0 TO WS-HINT-LEN

           MOVE SPACES TO WS-HUB-BODY
           MOVE 1 TO WS-PTR
           STRING
               "{"
               WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT
               "}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           MOVE 0 TO WS-RETRY
           PERFORM UNTIL WS-RETRY >= WS-MAX-RETRY
               ADD 1 TO WS-RETRY

               MOVE "hint_req.tmp" TO WS-WORK-PATH
               OPEN OUTPUT WORK-FILE
               IF WS-FS NOT = "00"
                   DISPLAY "  [hint] open err " WS-FS
                   MOVE "work.tmp" TO WS-WORK-PATH
                   EXIT PARAGRAPH
               END-IF
               WRITE WORK-REC FROM WS-HUB-BODY
               CLOSE WORK-FILE
               MOVE "work.tmp" TO WS-WORK-PATH

               INITIALIZE WS-CMD
               STRING "rm -f hint_resp.json"
                   DELIMITED SIZE INTO WS-CMD
               END-STRING
               CALL "SYSTEM" USING WS-CMD

               INITIALIZE WS-CMD
               STRING
                   "curl -s --max-time 30 "
                   "-o hint_resp.json"
                   " -X POST "
                   TRIM(WS-HINT-URL)
                   " -H " WS-QT
                   "Content-Type: "
                   "application/json"
                   WS-QT
                   " -d @hint_req.tmp"
                   DELIMITED SIZE
                   INTO WS-CMD
               END-STRING
               CALL "SYSTEM" USING WS-CMD

               MOVE "hint_resp.json" TO WS-WORK-PATH
               PERFORM READ-JSON-FILE
               MOVE "work.tmp" TO WS-WORK-PATH

               IF WS-JLEN > 0
                   MOVE "hint" TO WS-KEY-SEARCH
                   MOVE 1 TO WS-JPOS
                   PERFORM FIND-JSON-VAL
                   IF WS-JVAL NOT = SPACES
                       MOVE WS-JVAL TO WS-HINT-TEXT
                       EXIT PERFORM
                   END-IF
                   MOVE "message" TO WS-KEY-SEARCH
                   MOVE 1 TO WS-JPOS
                   PERFORM FIND-JSON-VAL
                   IF WS-JVAL NOT = SPACES
                       MOVE WS-JVAL TO WS-HINT-TEXT
                       EXIT PERFORM
                   END-IF
               END-IF

               DISPLAY "  [hint retry " WS-RETRY "]"
               CALL "C$SLEEP" USING 1
           END-PERFORM

           MOVE LENGTH(FUNCTION TRIM(WS-HINT-TEXT TRAILING))
               TO WS-HINT-LEN
           DISPLAY "  [hint] "
               WS-HINT-TEXT(1:300)

           IF WS-HINT-LEN > 0
               MOVE FUNCTION LOWER-CASE(WS-HINT-TEXT)
                   TO WS-HINT-LOWER
           END-IF
           .

      *> ============================================================
      *> CHECK-HINT-RATE-LIMIT (same as v1)
      *> ============================================================
       CHECK-HINT-RATE-LIMIT.
           MOVE "N" TO WS-HINT-RL
           IF WS-HINT-LEN = 0
               EXIT PARAGRAPH
           END-IF
           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-HINT-LOWER
               TALLYING WS-TALLY-CNT FOR ALL "zwolnij"
           INSPECT WS-HINT-LOWER
               TALLYING WS-TALLY-CNT FOR ALL "too often"
           INSPECT WS-HINT-LOWER
               TALLYING WS-TALLY-CNT FOR ALL "czesto"
           INSPECT WS-HINT-LOWER
               TALLYING WS-TALLY-CNT FOR ALL "za czesto"
           INSPECT WS-HINT-LOWER
               TALLYING WS-TALLY-CNT FOR ALL "slow down"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-HINT-RL
           END-IF
           .

      *> ============================================================
      *> CLASSIFY-HINT-V2 (Upgrade 1 + 3 + 4)
      *>
      *> 1. If the hint body is the Polish rate-limit marker:
      *>    sleep 12s, refetch. Still throttled? sleep 20s. Still?
      *>    sleep 30s. Still? set WS-RATE-LIMIT-FAIL=Y and EXIT;
      *>    PICK-MOVE-V2 will use the freeRows-aware safe policy.
      *>    V7: wider ladder (62s vs v6's 36s) to absorb longer
      *>    server cool-offs. Once RL is seen at all, we also bump
      *>    the inter-step sleep from 2s to 5s (was 3s in v6) to
      *>    reduce downstream pressure for the rest of the attempt.
      *> 2. Otherwise run the deterministic rewriter.
      *> 3. If the rewriter yields a decision, use it directly.
      *>    Else call LLM-CLASSIFY-HINT-V2 with the few-shots.
      *>
      *> V7-NOTE (RC-1): We have observed the server returning a
      *> hint body of literal "It's clear!" while simultaneously
      *> responding to the move with crashReason: trap. This is
      *> unrecoverable from the client - there is no deterministic
      *> signal to reject a syntactically-clean "It's clear" body.
      *> v8: the restart loop has been removed (single-attempt
      *> mode). If this pathological case fires on the one attempt,
      *> the game crashes and the process exits non-zero. Empirical
      *> v7 data (3/3 first-try pass, 0 crashes) shows this is rare
      *> enough that retry scaffolding is dead weight.
      *> ============================================================
       CLASSIFY-HINT-V2.
           MOVE 0.3333 TO WS-P-ROW-1
           MOVE 0.3334 TO WS-P-ROW-2
           MOVE 0.3333 TO WS-P-ROW-3
           MOVE "N" TO WS-RATE-LIMIT-FAIL

           IF WS-HINT-LEN = 0
               DISPLAY "  [classify] empty hint"
               EXIT PARAGRAPH
           END-IF

           PERFORM CHECK-HINT-RATE-LIMIT
           IF WS-HINT-RL = "Y"
               MOVE "Y" TO WS-RATE-LIMIT-SEEN
               MOVE 5 TO WS-STEP-SLEEP
               DISPLAY "  [classify] rate-limited - sleep 12s"
               CALL "C$SLEEP" USING 12
               PERFORM GET-HINT-V2
               PERFORM CHECK-HINT-RATE-LIMIT
               IF WS-HINT-RL = "Y"
                   DISPLAY "  [classify] still RL - sleep 20s"
                   CALL "C$SLEEP" USING 20
                   PERFORM GET-HINT-V2
                   PERFORM CHECK-HINT-RATE-LIMIT
                   IF WS-HINT-RL = "Y"
                       DISPLAY "  [classify] still RL "
                           "- sleep 30s"
                       CALL "C$SLEEP" USING 30
                       PERFORM GET-HINT-V2
                       PERFORM CHECK-HINT-RATE-LIMIT
                       IF WS-HINT-RL = "Y"
                           DISPLAY "  [classify] all 4 "
                               "refetches throttled - "
                               "freeRows-aware fallback"
                           MOVE "Y" TO WS-RATE-LIMIT-FAIL
                           EXIT PARAGRAPH
                       END-IF
                   END-IF
               END-IF
           END-IF

      *>   Deterministic rewriter first
           PERFORM HINT-REWRITER
           DISPLAY "  [rewriter] normalized="
               TRIM(WS-HINT-NORMALIZED)
               " decision=" WS-REW-DECISION
               "/" TRIM(WS-HINT-CLASS)

           IF WS-REW-DECISION = "HIT"
               DISPLAY "  [classify] rewriter="
                   TRIM(WS-HINT-CLASS)
           ELSE
               MOVE "N" TO WS-CLASS-RETRY
               PERFORM LLM-CLASSIFY-HINT-V2
               DISPLAY "  [classify] LLM="
                   TRIM(WS-HINT-CLASS)
               PERFORM VALIDATE-CLASSIFIER-OUTPUT
           END-IF

      *>   V5-2: MIDDLE is frequently a loose idiom for "the rock
      *>   is in the route / player's path". The literal
      *>   "absolute row 2" reading only holds when the player
      *>   actually IS on row 2 - otherwise "middle of the route"
      *>   means the player's current row. Remap MIDDLE -> AHEAD
      *>   when player_row != 2.
           IF WS-HINT-CLASS = "MIDDLE" AND WS-PLAYER-ROW NOT = 2
               DISPLAY "  [middle-remap] class=MIDDLE player_row="
                   WS-PLAYER-ROW
                   " -> remapped to AHEAD"
               MOVE "AHEAD" TO WS-HINT-CLASS
           END-IF

           EVALUATE WS-HINT-CLASS
               WHEN "MIDDLE"
                   MOVE 0.05 TO WS-P-ROW-1
                   MOVE 0.90 TO WS-P-ROW-2
                   MOVE 0.05 TO WS-P-ROW-3
                   DISPLAY "  [dist] class=MIDDLE"
                       " player_row=" WS-PLAYER-ROW
                       " stone_row=2"
                       " p=" WS-P-ROW-1 "/" WS-P-ROW-2
                       "/" WS-P-ROW-3
               WHEN "AHEAD"
                   MOVE WS-PLAYER-ROW TO WS-ROW-TMP
                   PERFORM APPLY-TARGET-DIST
               WHEN "UP"
                   COMPUTE WS-ROW-TMP = WS-PLAYER-ROW - 1
                   IF WS-ROW-TMP < 1
                       MOVE 1 TO WS-ROW-TMP
                   END-IF
                   PERFORM APPLY-TARGET-DIST
               WHEN "DOWN"
                   COMPUTE WS-ROW-TMP = WS-PLAYER-ROW + 1
                   IF WS-ROW-TMP > 3
                       MOVE 3 TO WS-ROW-TMP
                   END-IF
                   PERFORM APPLY-TARGET-DIST
               WHEN OTHER
                   DISPLAY "  [classify] unknown - uniform"
           END-EVALUATE
           .

      *> ============================================================
      *> VALIDATE-CLASSIFIER-OUTPUT (V4-1)
      *>
      *> Some LLM outputs are physically impossible: UP is illegal
      *> when the player sits on row 1, and DOWN is illegal when
      *> the player sits on row 3. We refuse to apply these and
      *> retry the LLM once with an extra system note listing the
      *> legal labels. If the retry still produces an invalid
      *> label we fall back to AHEAD (NOT MIDDLE - MIDDLE assumes
      *> the stone is on absolute row 2, which is a separate
      *> claim we do not have evidence for here).
      *> ============================================================
       VALIDATE-CLASSIFIER-OUTPUT.
           MOVE "N" TO WS-CLASS-INVALID
           MOVE WS-HINT-CLASS TO WS-CLASS-ORIG
           MOVE WS-PLAYER-ROW TO WS-CLASS-PLAYER-ROW-DSP

           IF WS-HINT-CLASS = "UP" AND WS-PLAYER-ROW <= 1
               MOVE "Y" TO WS-CLASS-INVALID
           END-IF
           IF WS-HINT-CLASS = "DOWN" AND WS-PLAYER-ROW >= 3
               MOVE "Y" TO WS-CLASS-INVALID
           END-IF

           IF WS-CLASS-INVALID = "N"
               EXIT PARAGRAPH
           END-IF

      *>   Build legal-labels string for the retry system note.
           MOVE SPACES TO WS-LEGAL-LABELS
           IF WS-PLAYER-ROW <= 1
               MOVE "DOWN, AHEAD, MIDDLE" TO WS-LEGAL-LABELS
           ELSE
               IF WS-PLAYER-ROW >= 3
                   MOVE "UP, AHEAD, MIDDLE" TO WS-LEGAL-LABELS
               ELSE
                   MOVE "UP, DOWN, AHEAD, MIDDLE"
                       TO WS-LEGAL-LABELS
               END-IF
           END-IF

           MOVE "Y" TO WS-CLASS-RETRY
           PERFORM LLM-CLASSIFY-HINT-V2

      *>   Re-validate retry output.
           MOVE "N" TO WS-CLASS-INVALID
           IF WS-HINT-CLASS = "UP" AND WS-PLAYER-ROW <= 1
               MOVE "Y" TO WS-CLASS-INVALID
           END-IF
           IF WS-HINT-CLASS = "DOWN" AND WS-PLAYER-ROW >= 3
               MOVE "Y" TO WS-CLASS-INVALID
           END-IF

           IF WS-CLASS-INVALID = "Y"
               DISPLAY "  [classify-validate] orig_class="
                   TRIM(WS-CLASS-ORIG)
                   " player_row=" WS-CLASS-PLAYER-ROW-DSP
                   " -> INVALID -> retry -> "
                   TRIM(WS-HINT-CLASS)
                   " (still invalid) -> AHEAD"
               MOVE "AHEAD" TO WS-HINT-CLASS
           ELSE
               DISPLAY "  [classify-validate] orig_class="
                   TRIM(WS-CLASS-ORIG)
                   " player_row=" WS-CLASS-PLAYER-ROW-DSP
                   " -> INVALID -> retry -> "
                   TRIM(WS-HINT-CLASS)
           END-IF

           MOVE "N" TO WS-CLASS-RETRY
           .

      *> ============================================================
      *> APPLY-TARGET-DIST (same as v1)
      *> V2-3: debug trace the chosen row distribution so we can
      *> verify the hint label -> probability mapping end-to-end.
      *> ============================================================
       APPLY-TARGET-DIST.
           MOVE 0.05 TO WS-P-ROW-1
           MOVE 0.05 TO WS-P-ROW-2
           MOVE 0.05 TO WS-P-ROW-3
           EVALUATE WS-ROW-TMP
               WHEN 1
                   MOVE 0.90 TO WS-P-ROW-1
               WHEN 2
                   MOVE 0.90 TO WS-P-ROW-2
               WHEN 3
                   MOVE 0.90 TO WS-P-ROW-3
           END-EVALUATE
           DISPLAY "  [dist] class=" TRIM(WS-HINT-CLASS)
               " player_row=" WS-PLAYER-ROW
               " stone_row=" WS-ROW-TMP
               " p=" WS-P-ROW-1 "/" WS-P-ROW-2 "/" WS-P-ROW-3
           .

      *> ============================================================
      *> LOAD-REWRITER-TABLES
      *> Build the three keyword tables in WORKING-STORAGE. Keeping
      *> this in code instead of data literals lets us express the
      *> slot enum cleanly.
      *> ============================================================
       LOAD-REWRITER-TABLES.
      *>   Directional tokens (22 total). Slot encoding:
      *>     PORT  -> UP
      *>     STARB -> DOWN
      *>     BOW   -> AHEAD
      *>     MID   -> MIDDLE (absolute row 2)
           MOVE "port"                 TO WS-DIR-TXT(1)
           MOVE 4                      TO WS-DIR-LEN(1)
           MOVE "PORT"                 TO WS-DIR-SLOT(1)

           MOVE "larboard"             TO WS-DIR-TXT(2)
           MOVE 8                      TO WS-DIR-LEN(2)
           MOVE "PORT"                 TO WS-DIR-SLOT(2)

           MOVE "left"                 TO WS-DIR-TXT(3)
           MOVE 4                      TO WS-DIR-LEN(3)
           MOVE "PORT"                 TO WS-DIR-SLOT(3)

           MOVE "starboard"            TO WS-DIR-TXT(4)
           MOVE 9                      TO WS-DIR-LEN(4)
           MOVE "STARB"                TO WS-DIR-SLOT(4)

           MOVE "right"                TO WS-DIR-TXT(5)
           MOVE 5                      TO WS-DIR-LEN(5)
           MOVE "STARB"                TO WS-DIR-SLOT(5)

           MOVE "bow"                  TO WS-DIR-TXT(6)
           MOVE 3                      TO WS-DIR-LEN(6)
           MOVE "BOW"                  TO WS-DIR-SLOT(6)

           MOVE "forward"              TO WS-DIR-TXT(7)
           MOVE 7                      TO WS-DIR-LEN(7)
           MOVE "BOW"                  TO WS-DIR-SLOT(7)

           MOVE "nose"                 TO WS-DIR-TXT(8)
           MOVE 4                      TO WS-DIR-LEN(8)
           MOVE "BOW"                  TO WS-DIR-SLOT(8)

           MOVE "cockpit"              TO WS-DIR-TXT(9)
           MOVE 7                      TO WS-DIR-LEN(9)
           MOVE "BOW"                  TO WS-DIR-SLOT(9)

           MOVE "straight"             TO WS-DIR-TXT(10)
           MOVE 8                      TO WS-DIR-LEN(10)
           MOVE "BOW"                  TO WS-DIR-SLOT(10)

           MOVE "current trajectory"   TO WS-DIR-TXT(11)
           MOVE 18                     TO WS-DIR-LEN(11)
           MOVE "BOW"                  TO WS-DIR-SLOT(11)

           MOVE "same line"            TO WS-DIR-TXT(12)
           MOVE 9                      TO WS-DIR-LEN(12)
           MOVE "BOW"                  TO WS-DIR-SLOT(12)

           MOVE "forward line"         TO WS-DIR-TXT(13)
           MOVE 12                     TO WS-DIR-LEN(13)
           MOVE "BOW"                  TO WS-DIR-SLOT(13)

           MOVE "forward corridor"     TO WS-DIR-TXT(14)
           MOVE 16                     TO WS-DIR-LEN(14)
           MOVE "BOW"                  TO WS-DIR-SLOT(14)

           MOVE "bow line"             TO WS-DIR-TXT(15)
           MOVE 8                      TO WS-DIR-LEN(15)
           MOVE "BOW"                  TO WS-DIR-SLOT(15)

           MOVE "in front"             TO WS-DIR-TXT(16)
           MOVE 8                      TO WS-DIR-LEN(16)
           MOVE "BOW"                  TO WS-DIR-SLOT(16)

           MOVE "ahead"                TO WS-DIR-TXT(17)
           MOVE 5                      TO WS-DIR-LEN(17)
           MOVE "BOW"                  TO WS-DIR-SLOT(17)

           MOVE "middle"               TO WS-DIR-TXT(18)
           MOVE 6                      TO WS-DIR-LEN(18)
           MOVE "MID"                  TO WS-DIR-SLOT(18)

           MOVE "central path"         TO WS-DIR-TXT(19)
           MOVE 12                     TO WS-DIR-LEN(19)
           MOVE "MID"                  TO WS-DIR-SLOT(19)

           MOVE "center"               TO WS-DIR-TXT(20)
           MOVE 6                      TO WS-DIR-LEN(20)
           MOVE "MID"                  TO WS-DIR-SLOT(20)

           MOVE "centre"               TO WS-DIR-TXT(21)
           MOVE 6                      TO WS-DIR-LEN(21)
           MOVE "MID"                  TO WS-DIR-SLOT(21)

           MOVE "central"              TO WS-DIR-TXT(22)
           MOVE 7                      TO WS-DIR-LEN(22)
           MOVE "MID"                  TO WS-DIR-SLOT(22)

      *>   V2-2: longer BOW phrases harvested from run1_v2 hints.
      *>   Order matters for longest-first match priority but the
      *>   current rewriter scans each entry independently. These
      *>   phrases all ground to the forward slot.
           MOVE "direction the craft is facing" TO WS-DIR-TXT(23)
           MOVE 29                     TO WS-DIR-LEN(23)
           MOVE "BOW"                  TO WS-DIR-SLOT(23)

           MOVE "in front of you"      TO WS-DIR-TXT(24)
           MOVE 15                     TO WS-DIR-LEN(24)
           MOVE "BOW"                  TO WS-DIR-SLOT(24)

           MOVE "the front"            TO WS-DIR-TXT(25)
           MOVE 9                      TO WS-DIR-LEN(25)
           MOVE "BOW"                  TO WS-DIR-SLOT(25)

           MOVE "current path"         TO WS-DIR-TXT(26)
           MOVE 12                     TO WS-DIR-LEN(26)
           MOVE "BOW"                  TO WS-DIR-SLOT(26)

           MOVE "line of travel"       TO WS-DIR-TXT(27)
           MOVE 14                     TO WS-DIR-LEN(27)
           MOVE "BOW"                  TO WS-DIR-SLOT(27)

           MOVE "flight line"          TO WS-DIR-TXT(28)
           MOVE 11                     TO WS-DIR-LEN(28)
           MOVE "BOW"                  TO WS-DIR-SLOT(28)

      *>   V7 BOW expansion (#29..#31)
           MOVE "the route"            TO WS-DIR-TXT(29)
           MOVE 9                      TO WS-DIR-LEN(29)
           MOVE "BOW"                  TO WS-DIR-SLOT(29)

           MOVE "the path"             TO WS-DIR-TXT(30)
           MOVE 8                      TO WS-DIR-LEN(30)
           MOVE "BOW"                  TO WS-DIR-SLOT(30)

           MOVE "forward path"         TO WS-DIR-TXT(31)
           MOVE 12                     TO WS-DIR-LEN(31)
           MOVE "BOW"                  TO WS-DIR-SLOT(31)

      *>   V7 flank expansion (#32..#41): compound PORT+STARB
      *>   entries. Each phrase gets TWO rows, one per slot,
      *>   because the HINT-REWRITER loop tags a single slot
      *>   per entry.
           MOVE "side paths"           TO WS-DIR-TXT(32)
           MOVE 10                     TO WS-DIR-LEN(32)
           MOVE "PORT"                 TO WS-DIR-SLOT(32)

           MOVE "side paths"           TO WS-DIR-TXT(33)
           MOVE 10                     TO WS-DIR-LEN(33)
           MOVE "STARB"                TO WS-DIR-SLOT(33)

           MOVE "both sides"           TO WS-DIR-TXT(34)
           MOVE 10                     TO WS-DIR-LEN(34)
           MOVE "PORT"                 TO WS-DIR-SLOT(34)

           MOVE "both sides"           TO WS-DIR-TXT(35)
           MOVE 10                     TO WS-DIR-LEN(35)
           MOVE "STARB"                TO WS-DIR-SLOT(35)

           MOVE "either side"          TO WS-DIR-TXT(36)
           MOVE 11                     TO WS-DIR-LEN(36)
           MOVE "PORT"                 TO WS-DIR-SLOT(36)

           MOVE "either side"          TO WS-DIR-TXT(37)
           MOVE 11                     TO WS-DIR-LEN(37)
           MOVE "STARB"                TO WS-DIR-SLOT(37)

           MOVE "flanks"               TO WS-DIR-TXT(38)
           MOVE 6                      TO WS-DIR-LEN(38)
           MOVE "PORT"                 TO WS-DIR-SLOT(38)

           MOVE "flanks"               TO WS-DIR-TXT(39)
           MOVE 6                      TO WS-DIR-LEN(39)
           MOVE "STARB"                TO WS-DIR-SLOT(39)

           MOVE "wings"                TO WS-DIR-TXT(40)
           MOVE 5                      TO WS-DIR-LEN(40)
           MOVE "PORT"                 TO WS-DIR-SLOT(40)

           MOVE "wings"                TO WS-DIR-TXT(41)
           MOVE 5                      TO WS-DIR-LEN(41)
           MOVE "STARB"                TO WS-DIR-SLOT(41)

      *>   Safe vocabulary (71 entries).
           MOVE "clear"                TO WS-SAFE-TXT(1)
           MOVE 5                      TO WS-SAFE-LEN(1)
           MOVE "open"                 TO WS-SAFE-TXT(2)
           MOVE 4                      TO WS-SAFE-LEN(2)
           MOVE "harmless"             TO WS-SAFE-TXT(3)
           MOVE 8                      TO WS-SAFE-LEN(3)
           MOVE "clean"                TO WS-SAFE-TXT(4)
           MOVE 5                      TO WS-SAFE-LEN(4)
           MOVE "no warning"           TO WS-SAFE-TXT(5)
           MOVE 10                     TO WS-SAFE-LEN(5)
           MOVE "no issue"             TO WS-SAFE-TXT(6)
           MOVE 8                      TO WS-SAFE-LEN(6)
           MOVE "no threat"            TO WS-SAFE-TXT(7)
           MOVE 9                      TO WS-SAFE-LEN(7)
           MOVE "safe passage"         TO WS-SAFE-TXT(8)
           MOVE 12                     TO WS-SAFE-LEN(8)
           MOVE "breathing room"       TO WS-SAFE-TXT(9)
           MOVE 14                     TO WS-SAFE-LEN(9)
           MOVE "gives room"           TO WS-SAFE-TXT(10)
           MOVE 10                     TO WS-SAFE-LEN(10)
           MOVE "stays friendly"       TO WS-SAFE-TXT(11)
           MOVE 14                     TO WS-SAFE-LEN(11)
           MOVE "no problem"           TO WS-SAFE-TXT(12)
           MOVE 10                     TO WS-SAFE-LEN(12)
           MOVE "looks alone"          TO WS-SAFE-TXT(13)
           MOVE 11                     TO WS-SAFE-LEN(13)
           MOVE "passable"             TO WS-SAFE-TXT(14)
           MOVE 8                      TO WS-SAFE-LEN(14)
           MOVE "still possible"       TO WS-SAFE-TXT(15)
           MOVE 14                     TO WS-SAFE-LEN(15)
           MOVE "empty"                TO WS-SAFE-TXT(16)
           MOVE 5                      TO WS-SAFE-LEN(16)
           MOVE "no warning"           TO WS-SAFE-TXT(17)
           MOVE 10                     TO WS-SAFE-LEN(17)
           MOVE "nothing pressing"     TO WS-SAFE-TXT(18)
           MOVE 16                     TO WS-SAFE-LEN(18)
           MOVE "nothing alarming"     TO WS-SAFE-TXT(19)
           MOVE 16                     TO WS-SAFE-LEN(19)
           MOVE "not blocked"          TO WS-SAFE-TXT(20)
           MOVE 11                     TO WS-SAFE-LEN(20)
           MOVE "not trailing"         TO WS-SAFE-TXT(21)
           MOVE 12                     TO WS-SAFE-LEN(21)
           MOVE "not hugging"          TO WS-SAFE-TXT(22)
           MOVE 11                     TO WS-SAFE-LEN(22)
           MOVE "not beside"           TO WS-SAFE-TXT(23)
           MOVE 10                     TO WS-SAFE-LEN(23)
           MOVE "friendly"             TO WS-SAFE-TXT(24)
           MOVE 8                      TO WS-SAFE-LEN(24)
           MOVE "dodge either"         TO WS-SAFE-TXT(25)
           MOVE 12                     TO WS-SAFE-LEN(25)
           MOVE "keep going"           TO WS-SAFE-TXT(26)
           MOVE 10                     TO WS-SAFE-LEN(26)
           MOVE "keep moving"          TO WS-SAFE-TXT(27)
           MOVE 11                     TO WS-SAFE-LEN(27)
           MOVE "breathing"            TO WS-SAFE-TXT(28)
           MOVE 9                      TO WS-SAFE-LEN(28)
           MOVE "clean lane"           TO WS-SAFE-TXT(29)
           MOVE 10                     TO WS-SAFE-LEN(29)
           MOVE "stays open"           TO WS-SAFE-TXT(30)
           MOVE 10                     TO WS-SAFE-LEN(30)
           MOVE "stays empty"          TO WS-SAFE-TXT(31)
           MOVE 11                     TO WS-SAFE-LEN(31)
           MOVE "stays quiet"          TO WS-SAFE-TXT(32)
           MOVE 11                     TO WS-SAFE-LEN(32)
           MOVE "remains clear"        TO WS-SAFE-TXT(33)
           MOVE 13                     TO WS-SAFE-LEN(33)
           MOVE "remain clear"         TO WS-SAFE-TXT(34)
           MOVE 12                     TO WS-SAFE-LEN(34)
           MOVE "look clear"           TO WS-SAFE-TXT(35)
           MOVE 10                     TO WS-SAFE-LEN(35)
           MOVE "looks clear"          TO WS-SAFE-TXT(36)
           MOVE 11                     TO WS-SAFE-LEN(36)
           MOVE "quiet"                TO WS-SAFE-TXT(37)
           MOVE 5                      TO WS-SAFE-LEN(37)
           MOVE "safe"                 TO WS-SAFE-TXT(38)
           MOVE 4                      TO WS-SAFE-LEN(38)
           MOVE "leaves"               TO WS-SAFE-TXT(39)
           MOVE 6                      TO WS-SAFE-LEN(39)
           MOVE "stays clear"          TO WS-SAFE-TXT(40)
           MOVE 11                     TO WS-SAFE-LEN(40)

      *>   V2-2 expansion (#41..#58): phrases from run1_v2 hints.
           MOVE "leaves alone"         TO WS-SAFE-TXT(41)
           MOVE 12                     TO WS-SAFE-LEN(41)
           MOVE "gives no warning"     TO WS-SAFE-TXT(42)
           MOVE 16                     TO WS-SAFE-LEN(42)
           MOVE "no issue hugs"        TO WS-SAFE-TXT(43)
           MOVE 13                     TO WS-SAFE-LEN(43)
           MOVE "is empty"             TO WS-SAFE-TXT(44)
           MOVE 8                      TO WS-SAFE-LEN(44)
           MOVE "lane is empty"        TO WS-SAFE-TXT(45)
           MOVE 13                     TO WS-SAFE-LEN(45)
           MOVE "leave alone"          TO WS-SAFE-TXT(46)
           MOVE 11                     TO WS-SAFE-LEN(46)
           MOVE "stay friendly"        TO WS-SAFE-TXT(47)
           MOVE 13                     TO WS-SAFE-LEN(47)
           MOVE "look usable"          TO WS-SAFE-TXT(48)
           MOVE 11                     TO WS-SAFE-LEN(48)
           MOVE "looks usable"         TO WS-SAFE-TXT(49)
           MOVE 12                     TO WS-SAFE-LEN(49)
           MOVE "both look usable"     TO WS-SAFE-TXT(50)
           MOVE 17                     TO WS-SAFE-LEN(50)
           MOVE "can keep going"       TO WS-SAFE-TXT(51)
           MOVE 14                     TO WS-SAFE-LEN(51)
           MOVE "can keep moving"      TO WS-SAFE-TXT(52)
           MOVE 15                     TO WS-SAFE-LEN(52)
           MOVE "room on both sides"   TO WS-SAFE-TXT(53)
           MOVE 18                     TO WS-SAFE-LEN(53)
           MOVE "have room"            TO WS-SAFE-TXT(54)
           MOVE 9                      TO WS-SAFE-LEN(54)
           MOVE "flanks are clean"     TO WS-SAFE-TXT(55)
           MOVE 16                     TO WS-SAFE-LEN(55)
           MOVE "flanks are clear"     TO WS-SAFE-TXT(56)
           MOVE 16                     TO WS-SAFE-LEN(56)

      *>   V4-2 expansion (#57..#71): phrases for "clean space",
      *>   "open area", "clear lane", and "centered / slightly
      *>   to" constructions seen in run3_v2 hints.
      *>   V7: renumbered from #59..#73 after "both sides" and
      *>   "either side" migrated out of SAFE into DIR compound.
           MOVE "clean space"          TO WS-SAFE-TXT(57)
           MOVE 11                     TO WS-SAFE-LEN(57)
           MOVE "the clean"            TO WS-SAFE-TXT(58)
           MOVE 9                      TO WS-SAFE-LEN(58)
           MOVE "open area"            TO WS-SAFE-TXT(59)
           MOVE 9                      TO WS-SAFE-LEN(59)
           MOVE "out to"               TO WS-SAFE-TXT(60)
           MOVE 6                      TO WS-SAFE-LEN(60)
           MOVE "clear lane"           TO WS-SAFE-TXT(61)
           MOVE 10                     TO WS-SAFE-LEN(61)
           MOVE "lane is clean"        TO WS-SAFE-TXT(62)
           MOVE 13                     TO WS-SAFE-LEN(62)
           MOVE "lane is clear"        TO WS-SAFE-TXT(63)
           MOVE 13                     TO WS-SAFE-LEN(63)
           MOVE "centered and"         TO WS-SAFE-TXT(64)
           MOVE 12                     TO WS-SAFE-LEN(64)
           MOVE "slightly to"          TO WS-SAFE-TXT(65)
           MOVE 11                     TO WS-SAFE-LEN(65)
           MOVE "slightly toward"      TO WS-SAFE-TXT(66)
           MOVE 15                     TO WS-SAFE-LEN(66)
           MOVE "runs clean"           TO WS-SAFE-TXT(67)
           MOVE 10                     TO WS-SAFE-LEN(67)
           MOVE "looks clean"          TO WS-SAFE-TXT(68)
           MOVE 11                     TO WS-SAFE-LEN(68)
           MOVE "nothing in"           TO WS-SAFE-TXT(69)
           MOVE 10                     TO WS-SAFE-LEN(69)
           MOVE "no rock in"           TO WS-SAFE-TXT(70)
           MOVE 10                     TO WS-SAFE-LEN(70)
           MOVE "space in front"       TO WS-SAFE-TXT(71)
           MOVE 14                     TO WS-SAFE-LEN(71)

      *>   Danger vocabulary (44 entries).
           MOVE "rock"                 TO WS-DANG-TXT(1)
           MOVE 4                      TO WS-DANG-LEN(1)
           MOVE "stone"                TO WS-DANG-TXT(2)
           MOVE 5                      TO WS-DANG-LEN(2)
           MOVE "obstacle"             TO WS-DANG-TXT(3)
           MOVE 8                      TO WS-DANG-LEN(3)
           MOVE "obstruction"          TO WS-DANG-TXT(4)
           MOVE 11                     TO WS-DANG-LEN(4)
           MOVE "blockage"             TO WS-DANG-TXT(5)
           MOVE 8                      TO WS-DANG-LEN(5)
           MOVE "blocked"              TO WS-DANG-TXT(6)
           MOVE 7                      TO WS-DANG-LEN(6)
           MOVE "closed"               TO WS-DANG-TXT(7)
           MOVE 6                      TO WS-DANG-LEN(7)
           MOVE "threat"               TO WS-DANG-TXT(8)
           MOVE 6                      TO WS-DANG-LEN(8)
           MOVE "hazard"               TO WS-DANG-TXT(9)
           MOVE 6                      TO WS-DANG-LEN(9)
           MOVE "trouble"              TO WS-DANG-TXT(10)
           MOVE 7                      TO WS-DANG-LEN(10)
           MOVE "problem"              TO WS-DANG-TXT(11)
           MOVE 7                      TO WS-DANG-LEN(11)
           MOVE "danger"               TO WS-DANG-TXT(12)
           MOVE 6                      TO WS-DANG-LEN(12)
           MOVE "crowd"                TO WS-DANG-TXT(13)
           MOVE 5                      TO WS-DANG-LEN(13)
           MOVE "posted"               TO WS-DANG-TXT(14)
           MOVE 6                      TO WS-DANG-LEN(14)
           MOVE "sitting"              TO WS-DANG-TXT(15)
           MOVE 7                      TO WS-DANG-LEN(15)
           MOVE "waiting"              TO WS-DANG-TXT(16)
           MOVE 7                      TO WS-DANG-LEN(16)
           MOVE "aimed"                TO WS-DANG-TXT(17)
           MOVE 5                      TO WS-DANG-LEN(17)
           MOVE "distrust"             TO WS-DANG-TXT(18)
           MOVE 8                      TO WS-DANG-LEN(18)
           MOVE "hugging"              TO WS-DANG-TXT(19)
           MOVE 7                      TO WS-DANG-LEN(19)
           MOVE "trailing"             TO WS-DANG-TXT(20)
           MOVE 8                      TO WS-DANG-LEN(20)
           MOVE "hit"                  TO WS-DANG-TXT(21)
           MOVE 3                      TO WS-DANG-LEN(21)
           MOVE "shadowing"            TO WS-DANG-TXT(22)
           MOVE 9                      TO WS-DANG-LEN(22)
           MOVE "stationed"            TO WS-DANG-TXT(23)
           MOVE 9                      TO WS-DANG-LEN(23)
           MOVE "risk"                 TO WS-DANG-TXT(24)
           MOVE 4                      TO WS-DANG-LEN(24)
           MOVE "carries"              TO WS-DANG-TXT(25)
           MOVE 7                      TO WS-DANG-LEN(25)
           MOVE "beside"               TO WS-DANG-TXT(26)
           MOVE 6                      TO WS-DANG-LEN(26)
           MOVE "off "                 TO WS-DANG-TXT(27)
           MOVE 4                      TO WS-DANG-LEN(27)
           MOVE "hanging"              TO WS-DANG-TXT(28)
           MOVE 7                      TO WS-DANG-LEN(28)
           MOVE "planted"              TO WS-DANG-TXT(29)
           MOVE 7                      TO WS-DANG-LEN(29)
           MOVE "chunk"                TO WS-DANG-TXT(30)
           MOVE 5                      TO WS-DANG-LEN(30)

      *>   V2-2 expansion (#31..#44): phrases from run1_v2 hints.
           MOVE "aimed right at"       TO WS-DANG-TXT(31)
           MOVE 14                     TO WS-DANG-LEN(31)
           MOVE "planted right"        TO WS-DANG-TXT(32)
           MOVE 13                     TO WS-DANG-LEN(32)
           MOVE "right in front"       TO WS-DANG-TXT(33)
           MOVE 14                     TO WS-DANG-LEN(33)
           MOVE "straight along"       TO WS-DANG-TXT(34)
           MOVE 14                     TO WS-DANG-LEN(34)
           MOVE "impact risk"          TO WS-DANG-TXT(35)
           MOVE 11                     TO WS-DANG-LEN(35)
           MOVE "resting beside"       TO WS-DANG-TXT(36)
           MOVE 14                     TO WS-DANG-LEN(36)
           MOVE "resting on"           TO WS-DANG-TXT(37)
           MOVE 10                     TO WS-DANG-LEN(37)
           MOVE "centered on"          TO WS-DANG-TXT(38)
           MOVE 11                     TO WS-DANG-LEN(38)
           MOVE "sits beside"          TO WS-DANG-TXT(39)
           MOVE 11                     TO WS-DANG-LEN(39)
           MOVE "sits along"           TO WS-DANG-TXT(40)
           MOVE 10                     TO WS-DANG-LEN(40)
           MOVE "hangs"                TO WS-DANG-TXT(41)
           MOVE 5                      TO WS-DANG-LEN(41)
           MOVE "rocket crash"         TO WS-DANG-TXT(42)
           MOVE 12                     TO WS-DANG-LEN(42)
           MOVE "hugs"                 TO WS-DANG-TXT(43)
           MOVE 4                      TO WS-DANG-LEN(43)
           MOVE "big rock"             TO WS-DANG-TXT(44)
           MOVE 8                      TO WS-DANG-LEN(44)

      *>   V2-4 negation markers: when one of these lands in the
      *>   ±30 window around a DIR token, tag the slot DNGR. Used
      *>   to catch phrasings like "but not in the direction the
      *>   craft is facing" where the SAFE token also fires and
      *>   the whole slot becomes AMBG -> deferred to LLM.
           MOVE "not in the direction" TO WS-DANG-TXT(45)
           MOVE 20                     TO WS-DANG-LEN(45)
           MOVE "but not in"           TO WS-DANG-TXT(46)
           MOVE 10                     TO WS-DANG-LEN(46)
           MOVE "not on the"           TO WS-DANG-TXT(47)
           MOVE 10                     TO WS-DANG-LEN(47)
           MOVE "not the side"         TO WS-DANG-TXT(48)
           MOVE 12                     TO WS-DANG-LEN(48)

      *>   V4-2 expansion (#49..#65): "stone/rock sitting/parked",
      *>   "blocked side", and ownership verbs like "holds" and
      *>   "harbors" seen in run3_v2 hints.
           MOVE "is sitting on"        TO WS-DANG-TXT(49)
           MOVE 13                     TO WS-DANG-LEN(49)
           MOVE "sitting on the"       TO WS-DANG-TXT(50)
           MOVE 14                     TO WS-DANG-LEN(50)
           MOVE "is parked on"         TO WS-DANG-TXT(51)
           MOVE 12                     TO WS-DANG-LEN(51)
           MOVE "parked at"            TO WS-DANG-TXT(52)
           MOVE 9                      TO WS-DANG-LEN(52)
           MOVE "the blocked side"     TO WS-DANG-TXT(53)
           MOVE 16                     TO WS-DANG-LEN(53)
           MOVE "blocked side is"      TO WS-DANG-TXT(54)
           MOVE 15                     TO WS-DANG-LEN(54)
           MOVE "the stone is"         TO WS-DANG-TXT(55)
           MOVE 12                     TO WS-DANG-LEN(55)
           MOVE "the rock is"          TO WS-DANG-TXT(56)
           MOVE 11                     TO WS-DANG-LEN(56)
           MOVE "a rock sits"          TO WS-DANG-TXT(57)
           MOVE 11                     TO WS-DANG-LEN(57)
           MOVE "nestled by"           TO WS-DANG-TXT(58)
           MOVE 10                     TO WS-DANG-LEN(58)
           MOVE "nestled on"           TO WS-DANG-TXT(59)
           MOVE 10                     TO WS-DANG-LEN(59)
           MOVE "tucked beside"        TO WS-DANG-TXT(60)
           MOVE 13                     TO WS-DANG-LEN(60)
           MOVE "tucked along"         TO WS-DANG-TXT(61)
           MOVE 12                     TO WS-DANG-LEN(61)
           MOVE "holds the"            TO WS-DANG-TXT(62)
           MOVE 9                      TO WS-DANG-LEN(62)
           MOVE "holds a"              TO WS-DANG-TXT(63)
           MOVE 7                      TO WS-DANG-LEN(63)
           MOVE "harbors"              TO WS-DANG-TXT(64)
           MOVE 7                      TO WS-DANG-LEN(64)
           MOVE "harbors the"          TO WS-DANG-TXT(65)
           MOVE 11                     TO WS-DANG-LEN(65)

      *>   V5-1 inversion markers (8 entries). Presence in the
      *>   WS-INV-RADIUS chars immediately preceding a DIR token
      *>   flips port<->starboard (and forces AMBG for BOW/MID).
           MOVE "opposite"             TO WS-INV-TXT(1)
           MOVE 8                      TO WS-INV-LEN(1)
           MOVE "away from"            TO WS-INV-TXT(2)
           MOVE 9                      TO WS-INV-LEN(2)
           MOVE "other than"           TO WS-INV-TXT(3)
           MOVE 10                     TO WS-INV-LEN(3)
           MOVE "not on the"           TO WS-INV-TXT(4)
           MOVE 10                     TO WS-INV-LEN(4)
           MOVE "not the"              TO WS-INV-TXT(5)
           MOVE 7                      TO WS-INV-LEN(5)
           MOVE "furthest from"        TO WS-INV-TXT(6)
           MOVE 13                     TO WS-INV-LEN(6)
           MOVE "far from"             TO WS-INV-TXT(7)
           MOVE 8                      TO WS-INV-LEN(7)
           MOVE "unlike"               TO WS-INV-TXT(8)
           MOVE 6                      TO WS-INV-LEN(8)
           .

      *> ============================================================
      *> HINT-REWRITER (Upgrade 3)
      *>
      *> Input:  WS-HINT-LOWER (lowercased hint, WS-HINT-LEN chars)
      *> Output: WS-HINT-NORMALIZED, WS-HINT-CLASS, WS-REW-DECISION
      *>
      *> For every directional token in WS-DIR-TOKENS we scan
      *> WS-HINT-LOWER for all occurrences. At each match we open a
      *> +/- WS-REW-WINDOW char window and look for any SAFE or
      *> DANGER vocabulary hit. We OR the outcome into the slot's
      *> tag (PORT/STARB/BOW/MID -> NONE/SAFE/DNGR/AMBG).
      *>
      *> Then REWRITER-DECIDE counts the tags and either sets
      *> WS-HINT-CLASS directly (decision="HIT") or defers to the
      *> LLM (decision="DEFER").
      *> ============================================================
       HINT-REWRITER.
           MOVE "NONE" TO WS-SLOT-PORT-TAG
           MOVE "NONE" TO WS-SLOT-STARB-TAG
           MOVE "NONE" TO WS-SLOT-BOW-TAG
           MOVE "NONE" TO WS-SLOT-MID-TAG
           MOVE "DEFER" TO WS-REW-DECISION
           MOVE "UNKNOWN" TO WS-HINT-CLASS
           MOVE SPACES TO WS-HINT-NORMALIZED
           MOVE 1 TO WS-HINT-NORM-PTR
           DISPLAY "  [rewriter] dir_tokens=" WS-DIR-TOKEN-CNT
               " safe_tokens=" WS-SAFE-TOKEN-CNT
               " dang_tokens=" WS-DANG-TOKEN-CNT

           IF WS-HINT-LEN = 0
               EXIT PARAGRAPH
           END-IF

      *>   Walk every directional token entry; for each, find all
      *>   occurrences in WS-HINT-LOWER and tag.
           PERFORM VARYING WS-REW-DIR-IDX FROM 1 BY 1
               UNTIL WS-REW-DIR-IDX > WS-DIR-TOKEN-CNT
               MOVE WS-DIR-LEN(WS-REW-DIR-IDX)
                   TO WS-REW-MATCH-LEN
               MOVE WS-DIR-SLOT(WS-REW-DIR-IDX)
                   TO WS-REW-SLOT-BUF
               MOVE 1 TO WS-REW-I
               PERFORM UNTIL WS-REW-I > WS-HINT-LEN
                         OR WS-REW-I + WS-REW-MATCH-LEN - 1
                            > WS-HINT-LEN
                   IF WS-HINT-LOWER(
                       WS-REW-I:WS-REW-MATCH-LEN) =
                       WS-DIR-TXT(WS-REW-DIR-IDX)(
                           1:WS-REW-MATCH-LEN)
                       MOVE WS-REW-I TO WS-REW-MATCH-POS
                       MOVE "N" TO WS-REW-SKIP
                       PERFORM REWRITER-IDIOM-GUARD
                       IF WS-REW-SKIP = "N"
                           PERFORM REWRITER-INVERSION-CHECK
                           PERFORM REWRITER-SCAN-WINDOW
                           PERFORM REWRITER-TAG-SLOT
                       END-IF
                       ADD WS-REW-MATCH-LEN TO WS-REW-I
                   ELSE
                       ADD 1 TO WS-REW-I
                   END-IF
               END-PERFORM
           END-PERFORM

           PERFORM REWRITER-BUILD-NORMALIZED
           PERFORM REWRITER-DECIDE
           .

      *> ============================================================
      *> REWRITER-IDIOM-GUARD (V2-2)
      *>
      *> Some short directional tokens ("right", "left", "in front")
      *> collide with BOW idioms like "right in front" or "straight
      *> in front". Before tagging a slot, check whether the current
      *> match position is part of such an idiom. If so, set
      *> WS-REW-SKIP="Y" so the caller leaves the slot untouched.
      *>
      *> Specifically:
      *>  - STARB matches on "right": skip if followed by " in front"
      *>    (so "right in front" only tags BOW via the "in front"
      *>    entry).
      *>  - PORT matches on "left": same logic ("left in front" is
      *>    rare but defensive).
      *>  - BOW match on "in front" while preceded by "not "
      *>    ("not in front" reads "not forward") -> don't tag BOW
      *>    as BOW; let the flanks win.
      *> ============================================================
       REWRITER-IDIOM-GUARD.
           MOVE "N" TO WS-REW-SKIP

      *>   "right" adverbial idiom guards (STARB)
           IF WS-REW-SLOT-BUF = "STARB"
           AND WS-REW-MATCH-LEN = 5
           AND WS-REW-MATCH-POS + 5 <= WS-HINT-LEN
           AND WS-HINT-LOWER(
               WS-REW-MATCH-POS:5) = "right"
               IF WS-REW-MATCH-POS + 13 <= WS-HINT-LEN
               AND WS-HINT-LOWER(
                   WS-REW-MATCH-POS + 5:9) = " in front"
                   MOVE "Y" TO WS-REW-SKIP
                   EXIT PARAGRAPH
               END-IF
               IF WS-REW-MATCH-POS + 8 <= WS-HINT-LEN
               AND WS-HINT-LOWER(
                   WS-REW-MATCH-POS + 5:4) = " at "
                   MOVE "Y" TO WS-REW-SKIP
                   EXIT PARAGRAPH
               END-IF
               IF WS-REW-MATCH-POS + 8 <= WS-HINT-LEN
               AND WS-HINT-LOWER(
                   WS-REW-MATCH-POS + 5:4) = " on "
                   MOVE "Y" TO WS-REW-SKIP
                   EXIT PARAGRAPH
               END-IF
               IF WS-REW-MATCH-POS + 10 <= WS-HINT-LEN
               AND WS-HINT-LOWER(
                   WS-REW-MATCH-POS + 5:6) = " away "
                   MOVE "Y" TO WS-REW-SKIP
                   EXIT PARAGRAPH
               END-IF
           END-IF

      *>   "left" adverbial idiom guards (PORT)
           IF WS-REW-SLOT-BUF = "PORT"
           AND WS-REW-MATCH-LEN = 4
           AND WS-REW-MATCH-POS + 4 <= WS-HINT-LEN
           AND WS-HINT-LOWER(
               WS-REW-MATCH-POS:4) = "left"
               IF WS-REW-MATCH-POS + 12 <= WS-HINT-LEN
               AND WS-HINT-LOWER(
                   WS-REW-MATCH-POS + 4:9) = " in front"
                   MOVE "Y" TO WS-REW-SKIP
                   EXIT PARAGRAPH
               END-IF
               IF WS-REW-MATCH-POS + 10 <= WS-HINT-LEN
               AND WS-HINT-LOWER(
                   WS-REW-MATCH-POS + 4:7) = " alone "
                   MOVE "Y" TO WS-REW-SKIP
                   EXIT PARAGRAPH
               END-IF
           END-IF

      *>   "not in front" / "not in the front" guard - suppress
      *>   BOW tagging from "in front" when negated.
           IF WS-REW-SLOT-BUF = "BOW"
           AND WS-REW-MATCH-LEN = 8
           AND WS-HINT-LOWER(
               WS-REW-MATCH-POS:8) = "in front"
           AND WS-REW-MATCH-POS >= 5
               IF WS-HINT-LOWER(
                   WS-REW-MATCH-POS - 4:4) = "not "
                   MOVE "Y" TO WS-REW-SKIP
                   EXIT PARAGRAPH
               END-IF
           END-IF
           .

      *> ============================================================
      *> REWRITER-INVERSION-CHECK (V5-1)
      *>
      *> Look at up to WS-INV-RADIUS chars immediately BEFORE the
      *> current directional match position. If one of the
      *> inversion markers in WS-INV-TOKENS is present (and no
      *> sentence terminator lies between the marker and the
      *> directional token), flip the slot:
      *>   PORT  <-> STARB
      *>   BOW / MID -> force SKIP=Y (no meaningful opposite on
      *>                               the port<->starboard axis)
      *> The flipped slot is then fed to REWRITER-SCAN-WINDOW
      *> unchanged - the SAFE/DANGER vocabulary that the window
      *> finds now attaches to the CORRECT side.
      *> ============================================================
       REWRITER-INVERSION-CHECK.
           MOVE "N" TO WS-REW-INV-HIT

      *>   Compute the left-context window
      *>   [WS-REW-INV-START .. WS-REW-MATCH-POS-1]
           IF WS-REW-MATCH-POS <= 1
               EXIT PARAGRAPH
           END-IF

           IF WS-REW-MATCH-POS > WS-INV-RADIUS + 1
               COMPUTE WS-REW-INV-START =
                   WS-REW-MATCH-POS - WS-INV-RADIUS
           ELSE
               MOVE 1 TO WS-REW-INV-START
           END-IF

      *>   Pull WS-REW-INV-START forward if a sentence terminator
      *>   lies between it and the match position.
           COMPUTE WS-REW-J = WS-REW-MATCH-POS - 1
           PERFORM UNTIL WS-REW-J < WS-REW-INV-START
               IF WS-HINT-LOWER(WS-REW-J:1) = "."
               OR WS-HINT-LOWER(WS-REW-J:1) = "!"
               OR WS-HINT-LOWER(WS-REW-J:1) = "?"
               OR WS-HINT-LOWER(WS-REW-J:1) = ";"
                   COMPUTE WS-REW-INV-START = WS-REW-J + 1
                   EXIT PERFORM
               END-IF
               IF WS-REW-J = 1
                   EXIT PERFORM
               END-IF
               SUBTRACT 1 FROM WS-REW-J
           END-PERFORM

           COMPUTE WS-REW-INV-LEN =
               WS-REW-MATCH-POS - WS-REW-INV-START
           IF WS-REW-INV-LEN < 1
               EXIT PARAGRAPH
           END-IF

      *>   Walk the inversion table against the left-context.
           PERFORM VARYING WS-REW-INV-IDX FROM 1 BY 1
               UNTIL WS-REW-INV-IDX > WS-INV-TOKEN-CNT
                  OR WS-REW-INV-HIT = "Y"
               MOVE 0 TO WS-TALLY-CNT
               INSPECT WS-HINT-LOWER(
                   WS-REW-INV-START:WS-REW-INV-LEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL WS-INV-TXT(WS-REW-INV-IDX)(
                       1:WS-INV-LEN(WS-REW-INV-IDX))
               IF WS-TALLY-CNT > 0
                   MOVE "Y" TO WS-REW-INV-HIT
               END-IF
           END-PERFORM

           IF WS-REW-INV-HIT = "N"
               EXIT PARAGRAPH
           END-IF

      *>   Inversion hit. Flip the slot.
           EVALUATE WS-REW-SLOT-BUF
               WHEN "PORT"
                   MOVE "STARB" TO WS-REW-SLOT-BUF
                   DISPLAY "  [rewriter-inversion] port at pos "
                       WS-REW-MATCH-POS
                       " inverted to STARB"
               WHEN "STARB"
                   MOVE "PORT" TO WS-REW-SLOT-BUF
                   DISPLAY "  [rewriter-inversion] starb at pos "
                       WS-REW-MATCH-POS
                       " inverted to PORT"
               WHEN "BOW"
                   MOVE "Y" TO WS-REW-SKIP
                   DISPLAY "  [rewriter-inversion] bow at pos "
                       WS-REW-MATCH-POS
                       " no-opposite -> skip"
               WHEN "MID"
                   MOVE "Y" TO WS-REW-SKIP
                   DISPLAY "  [rewriter-inversion] mid at pos "
                       WS-REW-MATCH-POS
                       " no-opposite -> skip"
           END-EVALUATE
           .

      *> ============================================================
      *> REWRITER-SCAN-WINDOW
      *> Given WS-REW-MATCH-POS / WS-REW-MATCH-LEN, compute the
      *> +/- WS-REW-WINDOW char window and set WS-REW-HAS-SAFE /
      *> WS-REW-HAS-DANGER flags by scanning the SAFE and DANGER
      *> tables.
      *> ============================================================
       REWRITER-SCAN-WINDOW.
           MOVE "N" TO WS-REW-HAS-SAFE
           MOVE "N" TO WS-REW-HAS-DANGER

      *>   Window start = max(1, match_pos - WINDOW)
           IF WS-REW-MATCH-POS > WS-REW-WINDOW
               COMPUTE WS-REW-WIN-START =
                   WS-REW-MATCH-POS - WS-REW-WINDOW
           ELSE
               MOVE 1 TO WS-REW-WIN-START
           END-IF

      *>   Window end = min(len, match_pos + match_len - 1 + WIN)
           COMPUTE WS-REW-WIN-END =
               WS-REW-MATCH-POS + WS-REW-MATCH-LEN - 1
               + WS-REW-WINDOW
           IF WS-REW-WIN-END > WS-HINT-LEN
               MOVE WS-HINT-LEN TO WS-REW-WIN-END
           END-IF

      *>   V4-2: sentence-boundary bail. The ±30 window must not
      *>   cross a sentence terminator (. ! ? ;) or a DANGER word
      *>   from the next sentence can leak into the match's
      *>   window. Walk backward from (match_pos - 1) to the
      *>   existing WS-REW-WIN-START looking for a terminator; if
      *>   found, pull WS-REW-WIN-START to (terminator + 1). Walk
      *>   forward from (match_pos + match_len) to WS-REW-WIN-END
      *>   looking for a terminator; if found, pull
      *>   WS-REW-WIN-END back to (terminator - 1).
           IF WS-REW-MATCH-POS > 1
               COMPUTE WS-REW-J = WS-REW-MATCH-POS - 1
               PERFORM UNTIL WS-REW-J < WS-REW-WIN-START
                   IF WS-HINT-LOWER(WS-REW-J:1) = "."
                   OR WS-HINT-LOWER(WS-REW-J:1) = "!"
                   OR WS-HINT-LOWER(WS-REW-J:1) = "?"
                   OR WS-HINT-LOWER(WS-REW-J:1) = ";"
                       COMPUTE WS-REW-WIN-START = WS-REW-J + 1
                       MOVE 0 TO WS-REW-J
                       EXIT PERFORM
                   END-IF
                   IF WS-REW-J = 1
                       MOVE 0 TO WS-REW-J
                       EXIT PERFORM
                   END-IF
                   SUBTRACT 1 FROM WS-REW-J
               END-PERFORM
           END-IF

           COMPUTE WS-REW-J =
               WS-REW-MATCH-POS + WS-REW-MATCH-LEN
           PERFORM UNTIL WS-REW-J > WS-REW-WIN-END
               IF WS-HINT-LOWER(WS-REW-J:1) = "."
               OR WS-HINT-LOWER(WS-REW-J:1) = "!"
               OR WS-HINT-LOWER(WS-REW-J:1) = "?"
               OR WS-HINT-LOWER(WS-REW-J:1) = ";"
                   COMPUTE WS-REW-WIN-END = WS-REW-J - 1
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-REW-J
           END-PERFORM

           COMPUTE WS-REW-WIN-LEN =
               WS-REW-WIN-END - WS-REW-WIN-START + 1
           IF WS-REW-WIN-LEN < 1
               EXIT PARAGRAPH
           END-IF

      *>   Walk the SAFE table and use INSPECT TALLYING for each
      *>   entry against the windowed slice. If any hit, set flag.
           PERFORM VARYING WS-REW-SAFE-IDX FROM 1 BY 1
               UNTIL WS-REW-SAFE-IDX > WS-SAFE-TOKEN-CNT
                  OR WS-REW-HAS-SAFE = "Y"
               MOVE 0 TO WS-TALLY-CNT
               INSPECT WS-HINT-LOWER(
                   WS-REW-WIN-START:WS-REW-WIN-LEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL WS-SAFE-TXT(WS-REW-SAFE-IDX)(
                       1:WS-SAFE-LEN(WS-REW-SAFE-IDX))
               IF WS-TALLY-CNT > 0
                   MOVE "Y" TO WS-REW-HAS-SAFE
               END-IF
           END-PERFORM

           PERFORM VARYING WS-REW-DANG-IDX FROM 1 BY 1
               UNTIL WS-REW-DANG-IDX > WS-DANG-TOKEN-CNT
                  OR WS-REW-HAS-DANGER = "Y"
               MOVE 0 TO WS-TALLY-CNT
               INSPECT WS-HINT-LOWER(
                   WS-REW-WIN-START:WS-REW-WIN-LEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL WS-DANG-TXT(WS-REW-DANG-IDX)(
                       1:WS-DANG-LEN(WS-REW-DANG-IDX))
               IF WS-TALLY-CNT > 0
                   MOVE "Y" TO WS-REW-HAS-DANGER
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> REWRITER-TAG-SLOT
      *> OR the (HAS-SAFE,HAS-DANGER) result into the appropriate
      *> slot tag. Combination rule over multiple matches per slot:
      *>   NONE + SAFE  = SAFE
      *>   NONE + DNGR  = DNGR
      *>   SAFE + SAFE  = SAFE
      *>   DNGR + DNGR  = DNGR
      *>   SAFE + DNGR  = AMBG
      *>   DNGR + SAFE  = AMBG
      *>   * + AMBG     = AMBG
      *>   no markers   = leave tag untouched (still NONE)
      *> ============================================================
       REWRITER-TAG-SLOT.
      *>   Determine the per-match tag first.
           IF WS-REW-HAS-SAFE = "Y" AND WS-REW-HAS-DANGER = "Y"
               MOVE "AMBG" TO WS-REW-TMP-TAG
           ELSE
               IF WS-REW-HAS-DANGER = "Y"
                   MOVE "DNGR" TO WS-REW-TMP-TAG
               ELSE
                   IF WS-REW-HAS-SAFE = "Y"
                       MOVE "SAFE" TO WS-REW-TMP-TAG
                   ELSE
                       EXIT PARAGRAPH
                   END-IF
               END-IF
           END-IF

           EVALUATE WS-REW-SLOT-BUF
               WHEN "PORT"
                   MOVE WS-SLOT-PORT-TAG TO WS-REW-CUR-TAG
                   PERFORM REWRITER-OR-TAG
                   MOVE WS-REW-CUR-TAG TO WS-SLOT-PORT-TAG
               WHEN "STARB"
                   MOVE WS-SLOT-STARB-TAG TO WS-REW-CUR-TAG
                   PERFORM REWRITER-OR-TAG
                   MOVE WS-REW-CUR-TAG TO WS-SLOT-STARB-TAG
               WHEN "BOW"
                   MOVE WS-SLOT-BOW-TAG TO WS-REW-CUR-TAG
                   PERFORM REWRITER-OR-TAG
                   MOVE WS-REW-CUR-TAG TO WS-SLOT-BOW-TAG
               WHEN "MID"
                   MOVE WS-SLOT-MID-TAG TO WS-REW-CUR-TAG
                   PERFORM REWRITER-OR-TAG
                   MOVE WS-REW-CUR-TAG TO WS-SLOT-MID-TAG
           END-EVALUATE
           .

      *> ============================================================
      *> REWRITER-OR-TAG
      *> Combine WS-REW-CUR-TAG with WS-REW-TMP-TAG following the
      *> lattice: NONE -> {SAFE,DNGR} -> AMBG. Result left in
      *> WS-REW-CUR-TAG.
      *> ============================================================
       REWRITER-OR-TAG.
           IF WS-REW-CUR-TAG = "AMBG"
               EXIT PARAGRAPH
           END-IF
           IF WS-REW-TMP-TAG = "AMBG"
               MOVE "AMBG" TO WS-REW-CUR-TAG
               EXIT PARAGRAPH
           END-IF
           IF WS-REW-CUR-TAG = "NONE"
               MOVE WS-REW-TMP-TAG TO WS-REW-CUR-TAG
               EXIT PARAGRAPH
           END-IF
           IF WS-REW-CUR-TAG = WS-REW-TMP-TAG
               EXIT PARAGRAPH
           END-IF
      *>   Conflict: one was SAFE, the other DNGR -> AMBG
           MOVE "AMBG" TO WS-REW-CUR-TAG
           .

      *> ============================================================
      *> REWRITER-BUILD-NORMALIZED
      *> Emit a compact log line like [SAFE:PORT][DNGR:BOW][...].
      *> ============================================================
       REWRITER-BUILD-NORMALIZED.
           MOVE SPACES TO WS-HINT-NORMALIZED
           MOVE 1 TO WS-HINT-NORM-PTR

           IF WS-SLOT-PORT-TAG NOT = "NONE"
               STRING "[" WS-SLOT-PORT-TAG ":PORT]"
                   DELIMITED SIZE
                   INTO WS-HINT-NORMALIZED
                   WITH POINTER WS-HINT-NORM-PTR
               END-STRING
           END-IF
           IF WS-SLOT-STARB-TAG NOT = "NONE"
               STRING "[" WS-SLOT-STARB-TAG ":STARB]"
                   DELIMITED SIZE
                   INTO WS-HINT-NORMALIZED
                   WITH POINTER WS-HINT-NORM-PTR
               END-STRING
           END-IF
           IF WS-SLOT-BOW-TAG NOT = "NONE"
               STRING "[" WS-SLOT-BOW-TAG ":BOW]"
                   DELIMITED SIZE
                   INTO WS-HINT-NORMALIZED
                   WITH POINTER WS-HINT-NORM-PTR
               END-STRING
           END-IF
           IF WS-SLOT-MID-TAG NOT = "NONE"
               STRING "[" WS-SLOT-MID-TAG ":MID]"
                   DELIMITED SIZE
                   INTO WS-HINT-NORMALIZED
                   WITH POINTER WS-HINT-NORM-PTR
               END-STRING
           END-IF
           .

      *> ============================================================
      *> REWRITER-DECIDE
      *> Count SAFE / DANGER / AMBIGUOUS tags and apply the rule.
      *>
      *>   exactly 1 DANGER  (and 0 AMBIG) -> pick that slot
      *>   0 DANGER, 0 AMBIG, >=2 SAFE     -> see fallback below
      *>   anything else                   -> defer to LLM
      *>
      *> Fallback when zero DANGER but multiple SAFE (common
      *> phrasing: "sides are clear" with no explicit rock token):
      *>   - PORT SAFE + STARB SAFE, BOW untouched  -> AHEAD
      *>   - PORT SAFE + BOW SAFE                   -> DOWN  (starb)
      *>   - STARB SAFE + BOW SAFE                  -> UP    (port)
      *>   - otherwise                              -> defer
      *> ============================================================
       REWRITER-DECIDE.
           MOVE 0 TO WS-CNT-DANGER
           MOVE 0 TO WS-CNT-SAFE
           MOVE 0 TO WS-CNT-AMBIG

           IF WS-SLOT-PORT-TAG  = "AMBG"
               ADD 1 TO WS-CNT-AMBIG
           END-IF
           IF WS-SLOT-STARB-TAG = "AMBG"
               ADD 1 TO WS-CNT-AMBIG
           END-IF
           IF WS-SLOT-BOW-TAG   = "AMBG"
               ADD 1 TO WS-CNT-AMBIG
           END-IF
           IF WS-SLOT-MID-TAG   = "AMBG"
               ADD 1 TO WS-CNT-AMBIG
           END-IF

           IF WS-SLOT-PORT-TAG  = "DNGR"
               ADD 1 TO WS-CNT-DANGER
           END-IF
           IF WS-SLOT-STARB-TAG = "DNGR"
               ADD 1 TO WS-CNT-DANGER
           END-IF
           IF WS-SLOT-BOW-TAG   = "DNGR"
               ADD 1 TO WS-CNT-DANGER
           END-IF
           IF WS-SLOT-MID-TAG   = "DNGR"
               ADD 1 TO WS-CNT-DANGER
           END-IF

           IF WS-SLOT-PORT-TAG  = "SAFE"
               ADD 1 TO WS-CNT-SAFE
           END-IF
           IF WS-SLOT-STARB-TAG = "SAFE"
               ADD 1 TO WS-CNT-SAFE
           END-IF
           IF WS-SLOT-BOW-TAG   = "SAFE"
               ADD 1 TO WS-CNT-SAFE
           END-IF
           IF WS-SLOT-MID-TAG   = "SAFE"
               ADD 1 TO WS-CNT-SAFE
           END-IF

      *>   Ambiguous -> defer, EXCEPT when both flanks are cleanly
      *>   SAFE and BOW is AMBG (two-safe-flanks-forces-forward
      *>   heuristic, e.g. "port and starboard stay friendly; bow,
      *>   however, is aimed right at a stone").
           IF WS-CNT-AMBIG > 0
               IF WS-SLOT-PORT-TAG  = "SAFE"
               AND WS-SLOT-STARB-TAG = "SAFE"
               AND WS-SLOT-BOW-TAG   = "AMBG"
                   MOVE "HIT"  TO WS-REW-DECISION
                   MOVE "AHEAD" TO WS-HINT-CLASS
                   EXIT PARAGRAPH
               END-IF
               IF WS-SLOT-PORT-TAG  = "SAFE"
               AND WS-SLOT-BOW-TAG   = "SAFE"
               AND WS-SLOT-STARB-TAG = "AMBG"
                   MOVE "HIT"  TO WS-REW-DECISION
                   MOVE "DOWN" TO WS-HINT-CLASS
                   EXIT PARAGRAPH
               END-IF
               IF WS-SLOT-STARB-TAG = "SAFE"
               AND WS-SLOT-BOW-TAG   = "SAFE"
               AND WS-SLOT-PORT-TAG  = "AMBG"
                   MOVE "HIT" TO WS-REW-DECISION
                   MOVE "UP"  TO WS-HINT-CLASS
                   EXIT PARAGRAPH
               END-IF
               EXIT PARAGRAPH
           END-IF

      *>   Exactly one DANGER -> hit
           IF WS-CNT-DANGER = 1
               MOVE "HIT" TO WS-REW-DECISION
               IF WS-SLOT-PORT-TAG = "DNGR"
                   MOVE "UP" TO WS-HINT-CLASS
                   EXIT PARAGRAPH
               END-IF
               IF WS-SLOT-STARB-TAG = "DNGR"
                   MOVE "DOWN" TO WS-HINT-CLASS
                   EXIT PARAGRAPH
               END-IF
               IF WS-SLOT-BOW-TAG = "DNGR"
                   MOVE "AHEAD" TO WS-HINT-CLASS
                   EXIT PARAGRAPH
               END-IF
               IF WS-SLOT-MID-TAG = "DNGR"
                   MOVE "MIDDLE" TO WS-HINT-CLASS
                   EXIT PARAGRAPH
               END-IF
           END-IF

      *>   Multiple DANGER -> too noisy, defer
           IF WS-CNT-DANGER > 1
               EXIT PARAGRAPH
           END-IF

      *>   Zero DANGER, use SAFE-only pattern fallback
           IF WS-CNT-DANGER = 0 AND WS-CNT-SAFE >= 2
               IF WS-SLOT-PORT-TAG  = "SAFE"
               AND WS-SLOT-STARB-TAG = "SAFE"
               AND WS-SLOT-BOW-TAG  = "NONE"
                   MOVE "HIT"  TO WS-REW-DECISION
                   MOVE "AHEAD" TO WS-HINT-CLASS
                   EXIT PARAGRAPH
               END-IF
               IF WS-SLOT-PORT-TAG  = "SAFE"
               AND WS-SLOT-BOW-TAG   = "SAFE"
               AND WS-SLOT-STARB-TAG = "NONE"
                   MOVE "HIT"  TO WS-REW-DECISION
                   MOVE "DOWN" TO WS-HINT-CLASS
                   EXIT PARAGRAPH
               END-IF
               IF WS-SLOT-STARB-TAG = "SAFE"
               AND WS-SLOT-BOW-TAG   = "SAFE"
               AND WS-SLOT-PORT-TAG  = "NONE"
                   MOVE "HIT" TO WS-REW-DECISION
                   MOVE "UP"  TO WS-HINT-CLASS
                   EXIT PARAGRAPH
               END-IF
           END-IF
      *>   Otherwise: defer
           .

      *> ============================================================
      *> LOAD-PROMPT-CONSTANTS
      *> Populate WS-LLM-SYSTEM-PROMPT and WS-LLM-FEWSHOT-BLOCK once
      *> at startup. These strings are already JSON-escaped fragments
      *> ready to drop between "content":"..." markers, and contain
      *> six few-shot examples harvested from v1 run logs (cases the
      *> v1 LLM got wrong OR that the v2 rewriter will defer on).
      *>
      *> Few-shots (Q -> A):
      *>   1. "The only side you should distrust is port."    -> UP
      *>   2. "Starboard side shows trouble, port is clean."  -> DOWN
      *>   3. "The path directly ahead is not safe."          -> AHEAD
      *>   4. "Port and starboard are open - hold your line." -> AHEAD
      *>   5. "Keep to the middle of the column."             -> MIDDLE
      *>   6. "Port lane clear; obstacle ahead dead in front."-> AHEAD
      *> ============================================================
       LOAD-PROMPT-CONSTANTS.
           MOVE SPACES TO WS-LLM-SYSTEM-PROMPT
           MOVE 1 TO WS-PTR
           STRING
               "You classify nautical navigation hints for a 3-"
               "row by 12-column rocket grid. Row 1 is top, row "
               "3 is bottom. The player sits in some row and "
               "advances one column at a time. Each hint tells "
               "the player where a stone is relative to their "
               "next step. Answer with exactly ONE uppercase "
               "word from this set: UP, DOWN, AHEAD, MIDDLE. "
               "UP means the stone is one row ABOVE the player "
               "(port / left / larboard). DOWN means one row "
               "BELOW the player (starboard / right). AHEAD "
               "means the stone is in the same row as the "
               "player (bow / forward / directly in front / "
               "dead ahead). MIDDLE means the stone sits on the "
               "absolute centre row (row 2) regardless of the "
               "player. IMPORTANT interpretation rule: many "
               "hints describe which side is SAFE or CLEAR; the "
               "stone is on the OTHER side. For example "
               DELIMITED SIZE
               INTO WS-LLM-SYSTEM-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-BS WS-QT "port is clear, starboard shows "
               "trouble" WS-BS WS-QT " means DOWN (stone on "
               "starboard). Another rule: phrases like "
               WS-BS WS-QT "hold your line" WS-BS WS-QT
               ", " WS-BS WS-QT "keep course" WS-BS WS-QT ", "
               WS-BS WS-QT "steady as she goes" WS-BS WS-QT
               " combined with clear sides mean the stone is "
               "AHEAD. Output only the single word, no prose, "
               "no punctuation, no explanation."
               DELIMITED SIZE
               INTO WS-LLM-SYSTEM-PROMPT
               WITH POINTER WS-PTR
           END-STRING

           MOVE SPACES TO WS-LLM-FEWSHOT-BLOCK
           MOVE 1 TO WS-PTR
           STRING
               "EXAMPLES:" WS-BS "n"
               "Hint: The only side you should distrust is "
               "port." WS-BS "n"
               "Answer: UP" WS-BS "n"
               WS-BS "n"
               "Hint: Starboard side shows trouble, port is "
               "clean." WS-BS "n"
               "Answer: DOWN" WS-BS "n"
               WS-BS "n"
               "Hint: The path directly ahead is not safe." WS-BS "n"
               "Answer: AHEAD" WS-BS "n"
               WS-BS "n"
               "Hint: Port and starboard are open - hold your "
               "line." WS-BS "n"
               "Answer: AHEAD" WS-BS "n"
               WS-BS "n"
               "Hint: Keep to the middle of the column." WS-BS "n"
               "Answer: MIDDLE" WS-BS "n"
               WS-BS "n"
               "Hint: Port lane clear; obstacle dead ahead." WS-BS "n"
               "Answer: AHEAD" WS-BS "n"
               WS-BS "n"
               "END EXAMPLES."
               DELIMITED SIZE
               INTO WS-LLM-FEWSHOT-BLOCK
               WITH POINTER WS-PTR
           END-STRING

      *>   v6 scanner-extract system prompt. Built as one big
      *>   pre-JSON-escaped blob so LLM-EXTRACT-SCANNER-V2 can just
      *>   paste it between "content":"..." with no per-call work.
      *>   Every " in the prompt is already \" (WS-BS WS-QT) and
      *>   every newline is \n (WS-BS "n"). No \ appears in the
      *>   prompt text itself so no \\ escaping is needed.
           MOVE SPACES TO WS-LLM-SCAN-SYSTEM-PROMPT
           MOVE 1 TO WS-PTR

      *>   === Role and objective ===
           STRING
               "# Role and Objective" WS-BS "n"
               "You extract two fields from a CORRUPTED "
               "scanner payload produced by a hostile "
               "server. Output STRICT JSON with exactly "
               "these keys: frequency (integer 100-999 or "
               "null) and detectionCode (string of exactly "
               "6 alphanumeric characters or null). "
               "Nothing else." WS-BS "n" WS-BS "n"
               DELIMITED SIZE
               INTO WS-LLM-SCAN-SYSTEM-PROMPT
               WITH POINTER WS-PTR
           END-STRING

      *>   === Critical rules: intro + homoglyph rule ===
           STRING
               "# Critical rules" WS-BS "n"
               "- The payload is JSON-like but the server "
               "visually corrupts KEYS using homoglyph / "
               "leetspeak substitutions (b<->d, q<->p, "
               "o<->0, and possibly others you have never "
               "seen) plus random casing, missing commas, "
               "and mixed quote characters "
               WS-BS WS-QT " " X"27" " " X"60" "." WS-BS "n"
               DELIMITED SIZE
               INTO WS-LLM-SCAN-SYSTEM-PROMPT
               WITH POINTER WS-PTR
           END-STRING

      *>   === Critical rules: byte-for-byte value rule ===
           STRING
               "- You MUST reverse corruption ONLY when "
               "IDENTIFYING which key is which. When "
               "COPYING a value you MUST copy it byte-for-"
               "byte exactly as it appears in the payload. "
               "Do not normalise case, do not un-leet "
               "digits, do not "
               WS-BS WS-QT "fix" WS-BS WS-QT
               " spelling inside a value. The detectionCode"
               " value is hashed downstream; any edit "
               "breaks it." WS-BS "n"
               DELIMITED SIZE
               INTO WS-LLM-SCAN-SYSTEM-PROMPT
               WITH POINTER WS-PTR
           END-STRING

      *>   === Critical rules: type constraints ===
           STRING
               "- frequency is always a bare integer in "
               "the range 100..999. Copy the digits "
               "exactly; do not invent one." WS-BS "n"
               "- detectionCode is always exactly 6 "
               "alphanumeric characters ([A-Za-z0-9]{6}). "
               "Trim ONLY surrounding quote characters ("
               WS-BS WS-QT " " X"27" " " X"60"
               ") and surrounding whitespace. Never trim "
               "letters or digits from inside the value."
               WS-BS "n"
               DELIMITED SIZE
               INTO WS-LLM-SCAN-SYSTEM-PROMPT
               WITH POINTER WS-PTR
           END-STRING

      *>   === Critical rules: null + ignore + output shape ===
           STRING
               "- If a field is genuinely absent or you "
               "cannot identify it with high confidence, "
               "return null for that field. Never guess."
               WS-BS "n"
               "- Ignore every other field (weaponType, "
               "beingTracked, data, etc.)." WS-BS "n"
               "- Output STRICT JSON, one line, no "
               "markdown, no prose, no code fences, no "
               "comments, no trailing text. Exactly:"
               WS-BS "n"
               "  {" WS-BS WS-QT "frequency" WS-BS WS-QT
               ":<int or null>," WS-BS WS-QT "detectionCode"
               WS-BS WS-QT ":" WS-BS WS-QT "<6 chars>"
               WS-BS WS-QT " or null}" WS-BS "n" WS-BS "n"
               DELIMITED SIZE
               INTO WS-LLM-SCAN-SYSTEM-PROMPT
               WITH POINTER WS-PTR
           END-STRING

      *>   === Key matching guidance ===
           STRING
               "# How to match corrupted keys (generalise, "
               "do not memorise)" WS-BS "n"
               "Treat key matching as fuzzy / homoglyph-"
               "tolerant:" WS-BS "n"
               "- Lowercase the key before comparing."
               WS-BS "n"
               "- Treat these pairs as EQUIVALENT for key "
               "matching only: b=d, p=q, 0=o, 1=i=l, 5=s, "
               "3=e, 9=g, 8=b (extend this family to any "
               "visual/leet confusion you recognise)."
               WS-BS "n"
               DELIMITED SIZE
               INTO WS-LLM-SCAN-SYSTEM-PROMPT
               WITH POINTER WS-PTR
           END-STRING

      *>   === Key matching examples ===
           STRING
               "- " WS-BS WS-QT "frequency" WS-BS WS-QT
               " may appear as frepuency, freQuency, "
               "frEPUenCY, etc." WS-BS "n"
               "- " WS-BS WS-QT "detectionCode" WS-BS WS-QT
               " may appear as detectioncobe, "
               "betectioncode, betecti0nc0be, "
               "deteCti0NCODe, etc." WS-BS "n"
               "- The fields may be nested inside a "
               WS-BS WS-QT "data" WS-BS WS-QT
               " object whose own key may itself be "
               "corrupted (bata, daTA, BATa, etc.)."
               WS-BS "n"
               "- If two candidate keys could match, pick "
               "the one whose value fits the type "
               "constraint (int 100-999 / 6-char "
               "alphanumeric)." WS-BS "n" WS-BS "n"
               DELIMITED SIZE
               INTO WS-LLM-SCAN-SYSTEM-PROMPT
               WITH POINTER WS-PTR
           END-STRING

      *>   === Examples header ===
           STRING
               "# Examples" WS-BS "n"
               DELIMITED SIZE
               INTO WS-LLM-SCAN-SYSTEM-PROMPT
               WITH POINTER WS-PTR
           END-STRING

      *>   === Example 1 ===
           STRING
               "INPUT:" WS-BS "n"
               "{" WS-BS WS-QT "beingTracked" WS-BS WS-QT
               ": true, " WS-BS WS-QT "frequency"
               WS-BS WS-QT ": 173, " WS-BS WS-QT "data"
               WS-BS WS-QT ": {" WS-BS WS-QT "weaponType"
               WS-BS WS-QT ": " WS-BS WS-QT
               "air-to-air missile" WS-BS WS-QT ", "
               WS-BS WS-QT "detectionCode" WS-BS WS-QT
               ": " WS-BS WS-QT "W5Y6Ax" WS-BS WS-QT "}}"
               WS-BS "n"
               "OUTPUT:" WS-BS "n"
               "{" WS-BS WS-QT "frequency" WS-BS WS-QT
               ":173," WS-BS WS-QT "detectionCode"
               WS-BS WS-QT ":" WS-BS WS-QT "W5Y6Ax"
               WS-BS WS-QT "}" WS-BS "n" WS-BS "n"
               DELIMITED SIZE
               INTO WS-LLM-SCAN-SYSTEM-PROMPT
               WITH POINTER WS-PTR
           END-STRING

      *>   === Example 2 ===
           STRING
               "INPUT:" WS-BS "n"
               "{" WS-BS WS-QT "BATa" WS-BS WS-QT ": {"
               WS-BS WS-QT "BeTeCTi0NC0BE" WS-BS WS-QT
               ": " WS-BS WS-QT "nnMDni" WS-BS WS-QT ", "
               WS-BS WS-QT "weaP0NtYpe" WS-BS WS-QT
               ": " WS-BS WS-QT "surface-to-air missile"
               WS-BS WS-QT "}, " WS-BS WS-QT "fREPUencY"
               WS-BS WS-QT ": 555, " WS-BS WS-QT
               "BEInGtRAckEb" WS-BS WS-QT ": true}"
               WS-BS "n"
               "OUTPUT:" WS-BS "n"
               "{" WS-BS WS-QT "frequency" WS-BS WS-QT
               ":555," WS-BS WS-QT "detectionCode"
               WS-BS WS-QT ":" WS-BS WS-QT "nnMDni"
               WS-BS WS-QT "}" WS-BS "n" WS-BS "n"
               DELIMITED SIZE
               INTO WS-LLM-SCAN-SYSTEM-PROMPT
               WITH POINTER WS-PTR
           END-STRING

      *>   === Example 3 ===
           STRING
               "INPUT:" WS-BS "n"
               "{" WS-BS WS-QT "bEInGtraCked" WS-BS WS-QT
               ": true, " WS-BS WS-QT "FrEQuEnCY"
               WS-BS WS-QT ": 573, " WS-BS WS-QT "datA"
               WS-BS WS-QT ": {" WS-BS WS-QT
               "detECtIoNcODe" WS-BS WS-QT ": "
               WS-BS WS-QT "8OUQPR" WS-BS WS-QT ", "
               WS-BS WS-QT "WeAPontYpE" WS-BS WS-QT
               ": " WS-BS WS-QT "pursuit missile"
               WS-BS WS-QT "}}" WS-BS "n"
               "OUTPUT:" WS-BS "n"
               "{" WS-BS WS-QT "frequency" WS-BS WS-QT
               ":573," WS-BS WS-QT "detectionCode"
               WS-BS WS-QT ":" WS-BS WS-QT "8OUQPR"
               WS-BS WS-QT "}" WS-BS "n" WS-BS "n"
               DELIMITED SIZE
               INTO WS-LLM-SCAN-SYSTEM-PROMPT
               WITH POINTER WS-PTR
           END-STRING

      *>   === Example 4 ===
           STRING
               "INPUT:" WS-BS "n"
               "{" WS-BS WS-QT "frepuency" WS-BS WS-QT
               ": 357, " WS-BS WS-QT "beingTrackeb"
               WS-BS WS-QT ": true, " WS-BS WS-QT "bata"
               WS-BS WS-QT ": {" WS-BS WS-QT
               "betecti0nC0be" WS-BS WS-QT ": "
               WS-BS WS-QT "5iKfT6" WS-BS WS-QT ", "
               WS-BS WS-QT "weap0nType" WS-BS WS-QT
               ": " WS-BS WS-QT "self-guided missile"
               WS-BS WS-QT "}}" WS-BS "n"
               "OUTPUT:" WS-BS "n"
               "{" WS-BS WS-QT "frequency" WS-BS WS-QT
               ":357," WS-BS WS-QT "detectionCode"
               WS-BS WS-QT ":" WS-BS WS-QT "5iKfT6"
               WS-BS WS-QT "}" WS-BS "n" WS-BS "n"
               DELIMITED SIZE
               INTO WS-LLM-SCAN-SYSTEM-PROMPT
               WITH POINTER WS-PTR
           END-STRING

      *>   === Example 5 ===
           STRING
               "INPUT:" WS-BS "n"
               "{" WS-BS WS-QT "beingTrackeb" WS-BS WS-QT
               ": true, " WS-BS WS-QT "frepuency"
               WS-BS WS-QT ": 849, " WS-BS WS-QT "bata"
               WS-BS WS-QT ": {" WS-BS WS-QT
               "weap0nType" WS-BS WS-QT ": "
               WS-BS WS-QT "surface-to-air missile"
               WS-BS WS-QT ", " WS-BS WS-QT
               "betecti0nC0be" WS-BS WS-QT ": "
               WS-BS WS-QT "oxtVYC" WS-BS WS-QT "}}"
               WS-BS "n"
               "OUTPUT:" WS-BS "n"
               "{" WS-BS WS-QT "frequency" WS-BS WS-QT
               ":849," WS-BS WS-QT "detectionCode"
               WS-BS WS-QT ":" WS-BS WS-QT "oxtVYC"
               WS-BS WS-QT "}" WS-BS "n" WS-BS "n"
               DELIMITED SIZE
               INTO WS-LLM-SCAN-SYSTEM-PROMPT
               WITH POINTER WS-PTR
           END-STRING

      *>   === Example 6 ===
           STRING
               "INPUT:" WS-BS "n"
               "{" WS-BS WS-QT "beingTrackeb" WS-BS WS-QT
               ": true, " WS-BS WS-QT "frepuency"
               WS-BS WS-QT ": 210, " WS-BS WS-QT "bata"
               WS-BS WS-QT ": {" WS-BS WS-QT
               "weap0nType" WS-BS WS-QT ": "
               WS-BS WS-QT "self-guided missile"
               WS-BS WS-QT ", " WS-BS WS-QT
               "betecti0nC0be" WS-BS WS-QT ": "
               WS-BS WS-QT "qvjQPe" WS-BS WS-QT "}}"
               WS-BS "n"
               "OUTPUT:" WS-BS "n"
               "{" WS-BS WS-QT "frequency" WS-BS WS-QT
               ":210," WS-BS WS-QT "detectionCode"
               WS-BS WS-QT ":" WS-BS WS-QT "qvjQPe"
               WS-BS WS-QT "}" WS-BS "n" WS-BS "n"
               DELIMITED SIZE
               INTO WS-LLM-SCAN-SYSTEM-PROMPT
               WITH POINTER WS-PTR
           END-STRING

      *>   === Closing reminder ===
           STRING
               "# Reminder (do not skip)" WS-BS "n"
               "- Copy values byte-for-byte. Do NOT apply "
               "the b/d/p/q/o/0 rules inside values. Those "
               "rules are for MATCHING KEYS ONLY."
               WS-BS "n"
               "- If unsure, return null for that field, "
               "never a guess." WS-BS "n"
               "- Return STRICT one-line JSON. No prose."
               DELIMITED SIZE
               INTO WS-LLM-SCAN-SYSTEM-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           .

      *> ============================================================
      *> LLM-CLASSIFY-HINT-V2
      *> Ask gpt-4.1-mini to classify WS-HINT-TEXT into one of four
      *> labels: UP / DOWN / AHEAD / MIDDLE. Prompt includes the
      *> system prompt + few-shots built in LOAD-PROMPT-CONSTANTS.
      *> Temperature 0, max_tokens 3, strict uppercase word matcher.
      *> ============================================================
       LLM-CLASSIFY-HINT-V2.
           MOVE "UNKNOWN" TO WS-HINT-CLASS
           IF WS-HINT-LEN = 0
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-ESC-IN
           MOVE WS-HINT-TEXT TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

           MOVE SPACES TO WS-REQ-JSON
           MOVE 1 TO WS-PTR
           STRING
               "{"
               WS-QT "model" WS-QT ":"
               WS-QT "gpt-4.1-mini" WS-QT ","
               WS-QT "temperature" WS-QT ":0,"
               WS-QT "max_tokens" WS-QT ":3,"
               WS-QT "messages" WS-QT ":["
               "{" WS-QT "role" WS-QT ":"
               WS-QT "system" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           MOVE LENGTH(FUNCTION TRIM(
               WS-LLM-SYSTEM-PROMPT TRAILING)) TO WS-K
           IF WS-K > 0
               MOVE WS-LLM-SYSTEM-PROMPT(1:WS-K)
                   TO WS-REQ-JSON(WS-PTR:WS-K)
               ADD WS-K TO WS-PTR
           END-IF

      *>   V4-1: on retry, append a constraint note telling the
      *>   model which labels are physically legal given the
      *>   player's current row.
           IF WS-CLASS-RETRY = "Y"
               MOVE WS-PLAYER-ROW TO WS-CLASS-PLAYER-ROW-DSP
               STRING
                   " IMPORTANT: player is on row "
                   WS-CLASS-PLAYER-ROW-DSP
                   ". UP is impossible if player_row<=1. "
                   "DOWN is impossible if player_row>=3. "
                   "Choose only from legal labels: "
                   FUNCTION TRIM(WS-LEGAL-LABELS)
                   "."
                   DELIMITED SIZE
                   INTO WS-REQ-JSON
                   WITH POINTER WS-PTR
               END-STRING
           END-IF

           STRING
               WS-QT "},"
               "{" WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           MOVE LENGTH(FUNCTION TRIM(
               WS-LLM-FEWSHOT-BLOCK TRAILING)) TO WS-K
           IF WS-K > 0
               MOVE WS-LLM-FEWSHOT-BLOCK(1:WS-K)
                   TO WS-REQ-JSON(WS-PTR:WS-K)
               ADD WS-K TO WS-PTR
           END-IF

           STRING
               WS-BS "n" WS-BS "n"
               "Hint: "
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           IF WS-ESC-OLEN > 0
               MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
                   TO WS-REQ-JSON(WS-PTR:WS-ESC-OLEN)
               ADD WS-ESC-OLEN TO WS-PTR
           END-IF

           STRING
               WS-BS "n"
               "Answer:"
               WS-QT "}]}"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           PERFORM WRITE-LLM-REQ
           PERFORM SEND-LLM-REQUEST

           IF WS-LLM-RAW = SPACES
               EXIT PARAGRAPH
           END-IF

      *>   Normalize to uppercase, trim, then string match.
           MOVE FUNCTION UPPER-CASE(WS-LLM-RAW)
               TO WS-TMP

      *>   Scan for the first recognized token anywhere in the
      *>   first 64 chars (some models add whitespace).
           IF WS-TMP(1:6) = "MIDDLE"
               MOVE "MIDDLE" TO WS-HINT-CLASS
               EXIT PARAGRAPH
           END-IF
           IF WS-TMP(1:5) = "AHEAD"
               MOVE "AHEAD" TO WS-HINT-CLASS
               EXIT PARAGRAPH
           END-IF
           IF WS-TMP(1:4) = "DOWN"
               MOVE "DOWN" TO WS-HINT-CLASS
               EXIT PARAGRAPH
           END-IF
           IF WS-TMP(1:2) = "UP"
               MOVE "UP" TO WS-HINT-CLASS
               EXIT PARAGRAPH
           END-IF

      *>   Tolerate leading whitespace/newlines: scan manually.
           PERFORM VARYING WS-N FROM 1 BY 1
               UNTIL WS-N > 60
                  OR WS-HINT-CLASS NOT = "UNKNOWN"
               IF WS-TMP(WS-N:6) = "MIDDLE"
                   MOVE "MIDDLE" TO WS-HINT-CLASS
               END-IF
               IF WS-TMP(WS-N:5) = "AHEAD"
                   MOVE "AHEAD" TO WS-HINT-CLASS
               END-IF
               IF WS-TMP(WS-N:4) = "DOWN"
                   MOVE "DOWN" TO WS-HINT-CLASS
               END-IF
               IF WS-TMP(WS-N:2) = "UP"
                   MOVE "UP" TO WS-HINT-CLASS
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> LLM-EXTRACT-SCANNER-V2 (v6)
      *> Stage 2 of the v6 scanner path. Invoked when
      *> PARSE-SCANNER-FIELDS (Stage 1) failed to pull both fields
      *> as literal JSON keys, i.e. the server corrupted the keys.
      *>
      *> The system prompt is pre-baked into WS-LLM-SCAN-SYSTEM-
      *> PROMPT by LOAD-PROMPT-CONSTANTS and holds: role definition,
      *> homoglyph-tolerant key-matching rules, a byte-for-byte
      *> value-copying rule (the detection code is hashed
      *> downstream), type constraints, six worked examples, and a
      *> hard instruction to return null rather than guess.
      *>
      *> The model (gpt-4.1-mini, temperature 0, max_tokens 120)
      *> is asked to return STRICT one-line JSON with frequency
      *> (int or null) and detectionCode (string or null). If it
      *> returns null for a field FIND-JSON-VAL will copy the bare
      *> token "null" into WS-JVAL; we detect that explicitly and
      *> leave the target SPACES / 0 so SCAN-FREQUENCY-V2's existing
      *> "scan UNKNOWN -> skip disarm" gate catches it.
      *> ============================================================
       LLM-EXTRACT-SCANNER-V2.
           MOVE SPACES TO WS-ESC-IN
           IF WS-JLEN > 4000
               MOVE WS-JBUF(1:4000) TO WS-ESC-IN
           ELSE
               MOVE WS-JBUF(1:WS-JLEN) TO WS-ESC-IN
           END-IF
           PERFORM JSON-ESCAPE-STR

           MOVE SPACES TO WS-REQ-JSON
           MOVE 1 TO WS-PTR

      *>   Request envelope + model params.
      *>   v6-strict: response_format json_schema with strict:true
      *>   forces gpt-4.1-mini to emit JSON matching our schema
      *>   exactly. Nullable fields use the type-union pattern
      *>   ["integer","null"] / ["string","null"] rather than the
      *>   deprecated nullable keyword (which has known bugs in
      *>   openai-node).
           STRING
               "{"
               WS-QT "model" WS-QT ":"
               WS-QT "gpt-4.1-mini" WS-QT ","
               WS-QT "temperature" WS-QT ":0,"
               WS-QT "max_tokens" WS-QT ":120,"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Structured-outputs response_format: outer wrapper +
      *>   schema name + strict flag
           STRING
               WS-QT "response_format" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "json_schema" WS-QT ","
               WS-QT "json_schema" WS-QT ":{"
               WS-QT "name" WS-QT ":"
               WS-QT "scanner_extraction" WS-QT ","
               WS-QT "strict" WS-QT ":true,"
               WS-QT "schema" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "object" WS-QT ","
               WS-QT "additionalProperties" WS-QT ":false,"
               WS-QT "required" WS-QT ":["
               WS-QT "frequency" WS-QT ","
               WS-QT "detectionCode" WS-QT "],"
               WS-QT "properties" WS-QT ":{"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   frequency property: nullable integer via type union.
      *>   No description field - strict mode can reject it in
      *>   some setups; schema enforces structure, prompt enforces
      *>   semantics.
           STRING
               WS-QT "frequency" WS-QT ":{"
               WS-QT "type" WS-QT ":["
               WS-QT "integer" WS-QT ","
               WS-QT "null" WS-QT "]},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   detectionCode property: nullable string, minimal
           STRING
               WS-QT "detectionCode" WS-QT ":{"
               WS-QT "type" WS-QT ":["
               WS-QT "string" WS-QT ","
               WS-QT "null" WS-QT "]}"
               "}}}},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Resume regular request body: messages array + system
           STRING
               WS-QT "messages" WS-QT ":["
               "{" WS-QT "role" WS-QT ":"
               WS-QT "system" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Paste the pre-escaped system prompt blob
           MOVE LENGTH(FUNCTION TRIM(
               WS-LLM-SCAN-SYSTEM-PROMPT TRAILING)) TO WS-K
           IF WS-K > 0
               MOVE WS-LLM-SCAN-SYSTEM-PROMPT(1:WS-K)
                   TO WS-REQ-JSON(WS-PTR:WS-K)
               ADD WS-K TO WS-PTR
           END-IF

      *>   Close system content, open user content + user template
           STRING
               WS-QT "},"
               "{" WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               "PAYLOAD:" WS-BS "n"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Paste the JSON-escaped corrupted body
           IF WS-ESC-OLEN > 0
               MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
                   TO WS-REQ-JSON(
                   WS-PTR:WS-ESC-OLEN)
               ADD WS-ESC-OLEN TO WS-PTR
           END-IF

      *>   Tail instruction + close user content + close request
           STRING
               WS-BS "n" WS-BS "n"
               "Extract frequency and detectionCode as "
               "specified. Output the JSON object on a "
               "single line, nothing else. Remember: copy "
               "values byte-for-byte, and use null if you "
               "cannot identify a field."
               WS-QT "}]}"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           PERFORM WRITE-LLM-REQ
           PERFORM SEND-LLM-REQUEST

           IF WS-LLM-RAW = SPACES
               EXIT PARAGRAPH
           END-IF

      *>   Re-use WS-JBUF as a scratch parse buffer. We don't need
      *>   the original scanner body again past this point.
           MOVE SPACES TO WS-JBUF
           MOVE 0 TO WS-JLEN
           COMPUTE WS-JLEN = FUNCTION LENGTH(
               FUNCTION TRIM(WS-LLM-RAW TRAILING))
           IF WS-JLEN > 0
               MOVE WS-LLM-RAW(1:WS-JLEN)
                   TO WS-JBUF(1:WS-JLEN)
           END-IF

      *>   frequency: honour model refusal (null) by skipping
           MOVE "frequency" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF WS-JVAL NOT = SPACES
              AND FUNCTION TRIM(WS-JVAL) NOT = "null"
               PERFORM CONVERT-FREQ-VAL
           END-IF

      *>   detectionCode: ditto. Strip any trailing quote / brace
      *>   junk that FIND-JSON-VAL left behind on the bare path.
           MOVE "detectionCode" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF WS-JVAL NOT = SPACES
              AND FUNCTION TRIM(WS-JVAL) NOT = "null"
               MOVE WS-JVAL TO WS-DETECT-CODE
               PERFORM STRIP-DETECT-CODE-TAIL
           END-IF

           MOVE WS-FREQUENCY TO WS-FREQUENCY-DSP
           DISPLAY "  [scan-LLM-v2] freq=" WS-FREQUENCY-DSP
               " code="
               TRIM(WS-DETECT-CODE)(1:60)
           .

      *> ============================================================
      *> PICK-MOVE-V2 (Upgrade 1)
      *>
      *> Normal path: lexicographic scoring
      *>   (p_crash, |landing_row - base_row|, 0 if go else 1)
      *> over three candidates: left diagonal, go, right diagonal.
      *> Diagonal candidates must be legal per WS-FREE-ROW-* (the
      *> current column freeRows) unless nothing else is legal.
      *>
      *> Fallback path (WS-RATE-LIMIT-FAIL = "Y"): we have NO hint
      *> information. Seek the base row while respecting freeRows.
      *>   - if already on base row -> go
      *>   - else prefer the diagonal that takes us toward base
      *>   - if that diagonal is not in freeRows, try go
      *>   - if go's row is not in freeRows either, try the other
      *>     diagonal (still toward base, if possible) and finally
      *>     relax freeRows entirely and pick the base-direction
      *>     diagonal.
      *> ============================================================
       PICK-MOVE-V2.
           MOVE SPACES TO WS-MOVE

           IF WS-RATE-LIMIT-FAIL = "Y"
               PERFORM PICK-MOVE-RL-FALLBACK
               EXIT PARAGRAPH
           END-IF

           MOVE "N" TO WS-CAND-LEGAL-1
           MOVE "N" TO WS-CAND-LEGAL-2
           MOVE "N" TO WS-CAND-LEGAL-3

      *>   Candidate 1: LEFT -> player_row - 1
           IF WS-PLAYER-ROW > 1
               PERFORM CHECK-DIAG-LEGAL-LEFT
           END-IF

      *>   Candidate 2: GO -> player_row (always in-bounds)
           MOVE "Y" TO WS-CAND-LEGAL-2

      *>   Candidate 3: RIGHT -> player_row + 1
           IF WS-PLAYER-ROW < 3
               PERFORM CHECK-DIAG-LEGAL-RIGHT
           END-IF

      *>   If no legal diagonal AND both candidates are blocked,
      *>   relax freeRows so we at least have SOMETHING to play.
           IF WS-CAND-LEGAL-1 = "N"
           AND WS-CAND-LEGAL-3 = "N"
               IF WS-PLAYER-ROW > 1
                   MOVE "Y" TO WS-CAND-LEGAL-1
               END-IF
               IF WS-PLAYER-ROW < 3
                   MOVE "Y" TO WS-CAND-LEGAL-3
               END-IF
           END-IF

           PERFORM SCORE-CANDIDATE-1
           PERFORM SCORE-CANDIDATE-2
           PERFORM SCORE-CANDIDATE-3

           MOVE "go" TO WS-MOVE
           IF WS-CAND-LEGAL-2 = "Y"
               MOVE "go" TO WS-MOVE
           END-IF

           IF WS-CAND-LEGAL-1 = "Y"
               IF WS-MOVE = SPACES OR WS-MOVE = "go"
                   IF WS-CAND-LEGAL-2 = "N"
                       MOVE "left" TO WS-MOVE
                   ELSE
                       IF WS-CAND-SCORE-1 < WS-CAND-SCORE-2
                           MOVE "left" TO WS-MOVE
                       END-IF
                   END-IF
               END-IF
           END-IF

           IF WS-CAND-LEGAL-3 = "Y"
               EVALUATE TRUE
                   WHEN WS-MOVE = "go"
                       IF WS-CAND-SCORE-3 < WS-CAND-SCORE-2
                           MOVE "right" TO WS-MOVE
                       END-IF
                   WHEN WS-MOVE = "left"
                       IF WS-CAND-SCORE-3 < WS-CAND-SCORE-1
                           MOVE "right" TO WS-MOVE
                       END-IF
                   WHEN OTHER
                       MOVE "right" TO WS-MOVE
               END-EVALUATE
           END-IF

           IF WS-MOVE = SPACES
               MOVE "go" TO WS-MOVE
           END-IF
           .

      *> ============================================================
      *> PICK-MOVE-RL-FALLBACK
      *> Invoked when the hint API gave up throttling us. No stone
      *> distribution info, so just walk toward the base row while
      *> honoring freeRows. Mirrors Python's safe_move_no_hint.
      *>
      *> V4-3: anti-correlation bias. The stone at the column we
      *> just exited is stored in WS-PREV-STONE-ROW; adjacent
      *> columns rarely share a stone row (~75%). When multiple
      *> candidates are legal, prefer the one whose landing row is
      *> NOT equal to WS-PREV-STONE-ROW.
      *> ============================================================
       PICK-MOVE-RL-FALLBACK.
           MOVE SPACES TO WS-MOVE

      *>   Already on base row -> go (if go's row is free).
           IF WS-PLAYER-ROW = WS-BASE-ROW
               PERFORM RL-TRY-GO
               IF WS-MOVE NOT = SPACES
                   PERFORM RL-ANTI-CORRELATE
                   EXIT PARAGRAPH
               END-IF
           END-IF

      *>   Choose preferred diagonal: toward base row.
           IF WS-PLAYER-ROW > WS-BASE-ROW
      *>       Base is above -> try LEFT first, then GO, then RIGHT.
               PERFORM RL-TRY-LEFT
               IF WS-MOVE NOT = SPACES
                   PERFORM RL-ANTI-CORRELATE
                   EXIT PARAGRAPH
               END-IF
               PERFORM RL-TRY-GO
               IF WS-MOVE NOT = SPACES
                   PERFORM RL-ANTI-CORRELATE
                   EXIT PARAGRAPH
               END-IF
               PERFORM RL-TRY-RIGHT
               IF WS-MOVE NOT = SPACES
                   PERFORM RL-ANTI-CORRELATE
                   EXIT PARAGRAPH
               END-IF
           END-IF

           IF WS-PLAYER-ROW < WS-BASE-ROW
      *>       Base is below -> try RIGHT first, then GO, then LEFT.
               PERFORM RL-TRY-RIGHT
               IF WS-MOVE NOT = SPACES
                   PERFORM RL-ANTI-CORRELATE
                   EXIT PARAGRAPH
               END-IF
               PERFORM RL-TRY-GO
               IF WS-MOVE NOT = SPACES
                   PERFORM RL-ANTI-CORRELATE
                   EXIT PARAGRAPH
               END-IF
               PERFORM RL-TRY-LEFT
               IF WS-MOVE NOT = SPACES
                   PERFORM RL-ANTI-CORRELATE
                   EXIT PARAGRAPH
               END-IF
           END-IF

      *>   On base row but go wasn't free -> try any diagonal.
           PERFORM RL-TRY-LEFT
           IF WS-MOVE NOT = SPACES
               PERFORM RL-ANTI-CORRELATE
               EXIT PARAGRAPH
           END-IF
           PERFORM RL-TRY-RIGHT
           IF WS-MOVE NOT = SPACES
               PERFORM RL-ANTI-CORRELATE
               EXIT PARAGRAPH
           END-IF

      *>   Nothing legal. Relax freeRows and bias toward base row.
           IF WS-PLAYER-ROW > WS-BASE-ROW AND WS-PLAYER-ROW > 1
               MOVE "left" TO WS-MOVE
               EXIT PARAGRAPH
           END-IF
           IF WS-PLAYER-ROW < WS-BASE-ROW AND WS-PLAYER-ROW < 3
               MOVE "right" TO WS-MOVE
               EXIT PARAGRAPH
           END-IF
           MOVE "go" TO WS-MOVE
           .

      *> ============================================================
      *> RL-ANTI-CORRELATE (V4-3)
      *>
      *> Given the primary candidate in WS-MOVE, check whether its
      *> landing row equals WS-PREV-STONE-ROW. If so, and at least
      *> one alternative candidate is legal AND lands on a
      *> different row, swap to the alternative. Emits a
      *> [fallback] debug line recording the candidates and the
      *> chosen reason.
      *> ============================================================
       RL-ANTI-CORRELATE.
           IF WS-PREV-STONE-ROW = 0
               DISPLAY "  [fallback] primary=" TRIM(WS-MOVE)
                   " prev_stone_row=0 "
                   "reason=no_prev_stone_info"
               EXIT PARAGRAPH
           END-IF

      *>   Compute landing row of the primary candidate.
           MOVE 0 TO WS-ROW-TMP
           IF WS-MOVE = "go"
               MOVE WS-PLAYER-ROW TO WS-ROW-TMP
           END-IF
           IF WS-MOVE = "left"
               IF WS-PLAYER-ROW > 1
                   COMPUTE WS-ROW-TMP = WS-PLAYER-ROW - 1
               END-IF
           END-IF
           IF WS-MOVE = "right"
               IF WS-PLAYER-ROW < 3
                   COMPUTE WS-ROW-TMP = WS-PLAYER-ROW + 1
               END-IF
           END-IF

           IF WS-ROW-TMP NOT = WS-PREV-STONE-ROW
               DISPLAY "  [fallback] primary=" TRIM(WS-MOVE)
                   " landing_row=" WS-ROW-TMP
                   " prev_stone_row=" WS-PREV-STONE-ROW
                   " reason=preserve_freerow_safety"
               EXIT PARAGRAPH
           END-IF

      *>   Primary lands on the previous column's stone row. Look
      *>   for a legal alternative whose landing row differs.
           MOVE SPACES TO WS-TMP2
           MOVE WS-MOVE TO WS-TMP2

      *>   Try "go" first as the alternative.
           IF WS-TMP2 NOT = "go"
           AND WS-PLAYER-ROW NOT = WS-PREV-STONE-ROW
               EVALUATE WS-PLAYER-ROW
                   WHEN 1
                       IF WS-FREE-ROW-1 = 1
                           MOVE "go" TO WS-MOVE
                       END-IF
                   WHEN 2
                       IF WS-FREE-ROW-2 = 1
                           MOVE "go" TO WS-MOVE
                       END-IF
                   WHEN 3
                       IF WS-FREE-ROW-3 = 1
                           MOVE "go" TO WS-MOVE
                       END-IF
               END-EVALUATE
               IF WS-MOVE = "go"
                   DISPLAY "  [fallback] primary="
                       TRIM(WS-TMP2)
                       " -> alt=go landing_row="
                       WS-PLAYER-ROW
                       " prev_stone_row="
                       WS-PREV-STONE-ROW
                       " reason=anti_correlate"
                   EXIT PARAGRAPH
               END-IF
           END-IF

      *>   Try "left" alternative.
           IF WS-TMP2 NOT = "left"
           AND WS-PLAYER-ROW > 1
               COMPUTE WS-ROW-TMP = WS-PLAYER-ROW - 1
               IF WS-ROW-TMP NOT = WS-PREV-STONE-ROW
                   EVALUATE WS-ROW-TMP
                       WHEN 1
                           IF WS-FREE-ROW-1 = 1
                               MOVE "left" TO WS-MOVE
                           END-IF
                       WHEN 2
                           IF WS-FREE-ROW-2 = 1
                               MOVE "left" TO WS-MOVE
                           END-IF
                   END-EVALUATE
                   IF WS-MOVE = "left"
                       DISPLAY "  [fallback] primary="
                           TRIM(WS-TMP2)
                           " -> alt=left landing_row="
                           WS-ROW-TMP
                           " prev_stone_row="
                           WS-PREV-STONE-ROW
                           " reason=anti_correlate"
                       EXIT PARAGRAPH
                   END-IF
               END-IF
           END-IF

      *>   Try "right" alternative.
           IF WS-TMP2 NOT = "right"
           AND WS-PLAYER-ROW < 3
               COMPUTE WS-ROW-TMP = WS-PLAYER-ROW + 1
               IF WS-ROW-TMP NOT = WS-PREV-STONE-ROW
                   EVALUATE WS-ROW-TMP
                       WHEN 2
                           IF WS-FREE-ROW-2 = 1
                               MOVE "right" TO WS-MOVE
                           END-IF
                       WHEN 3
                           IF WS-FREE-ROW-3 = 1
                               MOVE "right" TO WS-MOVE
                           END-IF
                   END-EVALUATE
                   IF WS-MOVE = "right"
                       DISPLAY "  [fallback] primary="
                           TRIM(WS-TMP2)
                           " -> alt=right landing_row="
                           WS-ROW-TMP
                           " prev_stone_row="
                           WS-PREV-STONE-ROW
                           " reason=anti_correlate"
                       EXIT PARAGRAPH
                   END-IF
               END-IF
           END-IF

      *>   No legal anti-correlated alternative. Keep primary.
           MOVE WS-TMP2 TO WS-MOVE
           DISPLAY "  [fallback] primary=" TRIM(WS-MOVE)
               " prev_stone_row=" WS-PREV-STONE-ROW
               " reason=no_legal_alternative"
           .

       RL-TRY-GO.
      *>   Landing row = player_row. Check freeRows.
           EVALUATE WS-PLAYER-ROW
               WHEN 1
                   IF WS-FREE-ROW-1 = 1
                       MOVE "go" TO WS-MOVE
                   END-IF
               WHEN 2
                   IF WS-FREE-ROW-2 = 1
                       MOVE "go" TO WS-MOVE
                   END-IF
               WHEN 3
                   IF WS-FREE-ROW-3 = 1
                       MOVE "go" TO WS-MOVE
                   END-IF
           END-EVALUATE
           .

       RL-TRY-LEFT.
           IF WS-PLAYER-ROW <= 1
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-ROW-TMP = WS-PLAYER-ROW - 1
           EVALUATE WS-ROW-TMP
               WHEN 1
                   IF WS-FREE-ROW-1 = 1
                       MOVE "left" TO WS-MOVE
                   END-IF
               WHEN 2
                   IF WS-FREE-ROW-2 = 1
                       MOVE "left" TO WS-MOVE
                   END-IF
               WHEN 3
                   IF WS-FREE-ROW-3 = 1
                       MOVE "left" TO WS-MOVE
                   END-IF
           END-EVALUATE
           .

       RL-TRY-RIGHT.
           IF WS-PLAYER-ROW >= 3
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-ROW-TMP = WS-PLAYER-ROW + 1
           EVALUATE WS-ROW-TMP
               WHEN 1
                   IF WS-FREE-ROW-1 = 1
                       MOVE "right" TO WS-MOVE
                   END-IF
               WHEN 2
                   IF WS-FREE-ROW-2 = 1
                       MOVE "right" TO WS-MOVE
                   END-IF
               WHEN 3
                   IF WS-FREE-ROW-3 = 1
                       MOVE "right" TO WS-MOVE
                   END-IF
           END-EVALUATE
           .


      *> ============================================================
      *> CHECK-DIAG-LEGAL-LEFT / CHECK-DIAG-LEGAL-RIGHT
      *> Diagonal moves pass through (cur_col, dest_row) so dest_row
      *> must be in WS-FREE-ROW-*. Sets WS-CAND-LEGAL-1 / -3 = "Y".
      *> ============================================================
       CHECK-DIAG-LEGAL-LEFT.
           COMPUTE WS-ROW-TMP = WS-PLAYER-ROW - 1
           EVALUATE WS-ROW-TMP
               WHEN 1
                   IF WS-FREE-ROW-1 = 1
                       MOVE "Y" TO WS-CAND-LEGAL-1
                   END-IF
               WHEN 2
                   IF WS-FREE-ROW-2 = 1
                       MOVE "Y" TO WS-CAND-LEGAL-1
                   END-IF
               WHEN 3
                   IF WS-FREE-ROW-3 = 1
                       MOVE "Y" TO WS-CAND-LEGAL-1
                   END-IF
           END-EVALUATE
           .

       CHECK-DIAG-LEGAL-RIGHT.
           COMPUTE WS-ROW-TMP = WS-PLAYER-ROW + 1
           EVALUATE WS-ROW-TMP
               WHEN 1
                   IF WS-FREE-ROW-1 = 1
                       MOVE "Y" TO WS-CAND-LEGAL-3
                   END-IF
               WHEN 2
                   IF WS-FREE-ROW-2 = 1
                       MOVE "Y" TO WS-CAND-LEGAL-3
                   END-IF
               WHEN 3
                   IF WS-FREE-ROW-3 = 1
                       MOVE "Y" TO WS-CAND-LEGAL-3
                   END-IF
           END-EVALUATE
           .

      *> ============================================================
      *> SCORE-CANDIDATE-{1,2,3}
      *> Lexicographic score packed into PIC 9(3)V9(6):
      *>   p_crash (0..1)            : dominant
      *>   + 0.00010 * |row - base|  : distance-to-base term
      *>   + 0.00001                 : non-go penalty (cand 1,3)
      *>   + 0.000005 if landing_row : V7 anti-correlation
      *>     = WS-PREV-STONE-ROW       micro-penalty (breaks ties
      *>                                toward rows that did not
      *>                                hold a stone in the prior
      *>                                col)
      *> ============================================================
       SCORE-CANDIDATE-1.
           MOVE 9.000000 TO WS-CAND-SCORE-1
           IF WS-PLAYER-ROW > 1
               COMPUTE WS-ROW-TMP = WS-PLAYER-ROW - 1
               IF WS-ROW-TMP = 1
                   MOVE WS-P-ROW-1 TO WS-CAND-SCORE-1
               END-IF
               IF WS-ROW-TMP = 2
                   MOVE WS-P-ROW-2 TO WS-CAND-SCORE-1
               END-IF
               IF WS-ROW-TMP = 3
                   MOVE WS-P-ROW-3 TO WS-CAND-SCORE-1
               END-IF
               COMPUTE WS-CAND-SCORE-1 =
                   WS-CAND-SCORE-1
                 + 0.00010 * FUNCTION ABS(
                       WS-ROW-TMP - WS-BASE-ROW)
                 + 0.00001
               IF WS-PREV-STONE-ROW > 0
                  AND WS-ROW-TMP = WS-PREV-STONE-ROW
                   COMPUTE WS-CAND-SCORE-1 =
                       WS-CAND-SCORE-1 + 0.000005
                   DISPLAY "  [score-v7] cand=1 landing_row="
                       WS-ROW-TMP
                       " prev_stone_row=" WS-PREV-STONE-ROW
                       " penalty=0.000005"
               END-IF
           END-IF
           .

       SCORE-CANDIDATE-2.
           MOVE WS-PLAYER-ROW TO WS-ROW-TMP
           IF WS-ROW-TMP = 1
               MOVE WS-P-ROW-1 TO WS-CAND-SCORE-2
           END-IF
           IF WS-ROW-TMP = 2
               MOVE WS-P-ROW-2 TO WS-CAND-SCORE-2
           END-IF
           IF WS-ROW-TMP = 3
               MOVE WS-P-ROW-3 TO WS-CAND-SCORE-2
           END-IF
           COMPUTE WS-CAND-SCORE-2 =
               WS-CAND-SCORE-2
             + 0.00010 * FUNCTION ABS(
                   WS-ROW-TMP - WS-BASE-ROW)
           IF WS-PREV-STONE-ROW > 0
              AND WS-ROW-TMP = WS-PREV-STONE-ROW
               COMPUTE WS-CAND-SCORE-2 =
                   WS-CAND-SCORE-2 + 0.000005
               DISPLAY "  [score-v7] cand=2 landing_row="
                   WS-ROW-TMP
                   " prev_stone_row=" WS-PREV-STONE-ROW
                   " penalty=0.000005"
           END-IF
           .

       SCORE-CANDIDATE-3.
           MOVE 9.000000 TO WS-CAND-SCORE-3
           IF WS-PLAYER-ROW < 3
               COMPUTE WS-ROW-TMP = WS-PLAYER-ROW + 1
               IF WS-ROW-TMP = 1
                   MOVE WS-P-ROW-1 TO WS-CAND-SCORE-3
               END-IF
               IF WS-ROW-TMP = 2
                   MOVE WS-P-ROW-2 TO WS-CAND-SCORE-3
               END-IF
               IF WS-ROW-TMP = 3
                   MOVE WS-P-ROW-3 TO WS-CAND-SCORE-3
               END-IF
               COMPUTE WS-CAND-SCORE-3 =
                   WS-CAND-SCORE-3
                 + 0.00010 * FUNCTION ABS(
                       WS-ROW-TMP - WS-BASE-ROW)
                 + 0.00001
               IF WS-PREV-STONE-ROW > 0
                  AND WS-ROW-TMP = WS-PREV-STONE-ROW
                   COMPUTE WS-CAND-SCORE-3 =
                       WS-CAND-SCORE-3 + 0.000005
                   DISPLAY "  [score-v7] cand=3 landing_row="
                       WS-ROW-TMP
                       " prev_stone_row=" WS-PREV-STONE-ROW
                       " penalty=0.000005"
               END-IF
           END-IF
           .

      *> ============================================================
      *> SEND-MOVE
      *> POST {apikey,task,answer:{command:<WS-MOVE>}} to /verify.
      *> Response lands in WS-JBUF.
      *> ============================================================
       SEND-MOVE.
           MOVE SPACES TO WS-HUB-BODY
           MOVE 1 TO WS-PTR
           STRING
               "{"
               WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME) WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "command" WS-QT ":"
               WS-QT TRIM(WS-MOVE) WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           PERFORM POST-HUB-VERIFY
           DISPLAY "  [sent] resp(300): "
               WS-JBUF(1:300)
           .

      *> ============================================================
      *> CHECK-FLAG-IN-RESP
      *> ============================================================
       CHECK-FLAG-IN-RESP.
           MOVE 0 TO WS-TALLY-CNT
           IF WS-JLEN > 0
               INSPECT WS-JBUF(1:WS-JLEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL "{FLG:"
           END-IF
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-FLAG-FOUND
               PERFORM EXTRACT-FLAG-TOKEN
           END-IF
           .

      *> ============================================================
      *> EXTRACT-FLAG-TOKEN
      *> ============================================================
       EXTRACT-FLAG-TOKEN.
           MOVE SPACES TO WS-FLAG-TEXT
           MOVE 0 TO WS-KEY-POS
           PERFORM VARYING WS-N FROM 1 BY 1
               UNTIL WS-N > WS-JLEN - 4
                  OR WS-KEY-POS > 0
               IF WS-JBUF(WS-N:5) = "{FLG:"
                   MOVE WS-N TO WS-KEY-POS
               END-IF
           END-PERFORM

           IF WS-KEY-POS = 0
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-M
           PERFORM VARYING WS-N FROM WS-KEY-POS BY 1
               UNTIL WS-N > WS-JLEN
                  OR WS-M > 0
               IF WS-JBUF(WS-N:1) = "}"
                   MOVE WS-N TO WS-M
               END-IF
           END-PERFORM

           IF WS-M = 0
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-K = WS-M - WS-KEY-POS + 1
           IF WS-K > 200
               MOVE 200 TO WS-K
           END-IF
           MOVE WS-JBUF(WS-KEY-POS:WS-K)
               TO WS-FLAG-TEXT(1:WS-K)
           .

      *> ============================================================
      *> CHECK-CRASH-IN-RESP
      *> Sets WS-SCAN-TRAP to "C" sentinel on crash.
      *> ============================================================
       CHECK-CRASH-IN-RESP.
           MOVE "N" TO WS-SCAN-TRAP
           MOVE 0 TO WS-TALLY-CNT
           IF WS-JLEN > 0
               INSPECT WS-JBUF(1:WS-JLEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL "-950"
           END-IF
           IF WS-TALLY-CNT > 0
               MOVE "C" TO WS-SCAN-TRAP
               EXIT PARAGRAPH
           END-IF
           MOVE 0 TO WS-TALLY-CNT
           IF WS-JLEN > 0
               INSPECT WS-JBUF(1:WS-JLEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL "crashed"
           END-IF
           IF WS-TALLY-CNT > 0
               MOVE 0 TO WS-TALLY-CNT
               INSPECT WS-JBUF(1:WS-JLEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL "crashed" & X"22" & ":true"
               IF WS-TALLY-CNT > 0
                   MOVE "C" TO WS-SCAN-TRAP
               END-IF
           END-IF
           .


      *> ============================================================
      *> UPDATE-STATE-FROM-RESP
      *> ============================================================
       UPDATE-STATE-FROM-RESP.
           PERFORM PARSE-PLAYER-POS
           PERFORM PARSE-FREE-ROWS
           .

      *> ============================================================
      *> PARSE-PLAYER-POS
      *> Look for "player":{"row":R,"col":C} in WS-JBUF.
      *> ============================================================
       PARSE-PLAYER-POS.
           MOVE "player" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-PLAYER-START
           IF WS-JPOS = 0
               EXIT PARAGRAPH
           END-IF

           MOVE "row" TO WS-KEY-SEARCH
           PERFORM FIND-JSON-VAL
           IF WS-JVAL NOT = SPACES
               COMPUTE WS-PLAYER-ROW =
                   FUNCTION NUMVAL(WS-JVAL)
           END-IF

           MOVE "col" TO WS-KEY-SEARCH
           PERFORM FIND-JSON-VAL
           IF WS-JVAL NOT = SPACES
               COMPUTE WS-PLAYER-COL =
                   FUNCTION NUMVAL(WS-JVAL)
           END-IF
           .

      *> ============================================================
      *> FIND-PLAYER-START
      *> Scan WS-JBUF for "player" key and set WS-JPOS to just past
      *> the opening '{' that follows it.
      *> ============================================================
       FIND-PLAYER-START.
           MOVE 0 TO WS-KEY-POS
           PERFORM VARYING WS-N FROM 1 BY 1
               UNTIL WS-N + 8 > WS-JLEN
                  OR WS-KEY-POS > 0
               IF WS-JBUF(WS-N:8) = X"22" & "player" & X"22"
                   MOVE WS-N TO WS-KEY-POS
               END-IF
           END-PERFORM
           IF WS-KEY-POS = 0
               MOVE 0 TO WS-JPOS
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-N = WS-KEY-POS + 8
           PERFORM UNTIL WS-N > WS-JLEN
                     OR WS-JBUF(WS-N:1) = "{"
               ADD 1 TO WS-N
           END-PERFORM
           IF WS-N > WS-JLEN
               MOVE 0 TO WS-JPOS
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-N
           MOVE WS-N TO WS-JPOS
           .

      *> ============================================================
      *> PARSE-BASE-POS
      *> ============================================================
       PARSE-BASE-POS.
           MOVE 0 TO WS-KEY-POS
           PERFORM VARYING WS-N FROM 1 BY 1
               UNTIL WS-N + 6 > WS-JLEN
                  OR WS-KEY-POS > 0
               IF WS-JBUF(WS-N:6) = X"22" & "base" & X"22"
                   MOVE WS-N TO WS-KEY-POS
               END-IF
           END-PERFORM
           IF WS-KEY-POS = 0
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-N = WS-KEY-POS + 6
           PERFORM UNTIL WS-N > WS-JLEN
                     OR WS-JBUF(WS-N:1) = "{"
               ADD 1 TO WS-N
           END-PERFORM
           IF WS-N > WS-JLEN
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-N
           MOVE WS-N TO WS-JPOS

           MOVE "row" TO WS-KEY-SEARCH
           PERFORM FIND-JSON-VAL
           IF WS-JVAL NOT = SPACES
               COMPUTE WS-BASE-ROW =
                   FUNCTION NUMVAL(WS-JVAL)
           END-IF

           MOVE "col" TO WS-KEY-SEARCH
           PERFORM FIND-JSON-VAL
           IF WS-JVAL NOT = SPACES
               COMPUTE WS-BASE-COL =
                   FUNCTION NUMVAL(WS-JVAL)
           END-IF
           .

      *> ============================================================
      *> PARSE-FREE-ROWS
      *> Look for "freeRows":[ ... ] and set WS-FREE-ROW-1..3.
      *> ============================================================
       PARSE-FREE-ROWS.
           MOVE 0 TO WS-FREE-ROW-1
           MOVE 0 TO WS-FREE-ROW-2
           MOVE 0 TO WS-FREE-ROW-3

           MOVE 0 TO WS-KEY-POS
           PERFORM VARYING WS-N FROM 1 BY 1
               UNTIL WS-N + 10 > WS-JLEN
                  OR WS-KEY-POS > 0
               IF WS-JBUF(WS-N:10) =
                   X"22" & "freeRows" & X"22"
                   MOVE WS-N TO WS-KEY-POS
               END-IF
           END-PERFORM

           IF WS-KEY-POS = 0
               MOVE 1 TO WS-FREE-ROW-1
               MOVE 1 TO WS-FREE-ROW-2
               MOVE 1 TO WS-FREE-ROW-3
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-N = WS-KEY-POS + 10
           PERFORM UNTIL WS-N > WS-JLEN
                     OR WS-JBUF(WS-N:1) = "["
               ADD 1 TO WS-N
           END-PERFORM
           IF WS-N > WS-JLEN
               MOVE 1 TO WS-FREE-ROW-1
               MOVE 1 TO WS-FREE-ROW-2
               MOVE 1 TO WS-FREE-ROW-3
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-N

           PERFORM UNTIL WS-N > WS-JLEN
                     OR WS-JBUF(WS-N:1) = "]"
               EVALUATE WS-JBUF(WS-N:1)
                   WHEN "1"
                       MOVE 1 TO WS-FREE-ROW-1
                   WHEN "2"
                       MOVE 1 TO WS-FREE-ROW-2
                   WHEN "3"
                       MOVE 1 TO WS-FREE-ROW-3
               END-EVALUATE
               ADD 1 TO WS-N
           END-PERFORM

           IF WS-FREE-ROW-1 = 0
           AND WS-FREE-ROW-2 = 0
           AND WS-FREE-ROW-3 = 0
               MOVE 1 TO WS-FREE-ROW-1
               MOVE 1 TO WS-FREE-ROW-2
               MOVE 1 TO WS-FREE-ROW-3
           END-IF
           .


      *> ============================================================
      *> FETCH-PREVIEW
      *> POST key + after_event_id=0 as form-urlencoded to the
      *> /goingthere_backend preview endpoint. Populates WS-MAP and
      *> WS-PREVIEW-* on success. Sets WS-PREVIEW-OK = "Y".
      *> ============================================================
       FETCH-PREVIEW.
           MOVE "N" TO WS-PREVIEW-OK

           MOVE SPACES TO WS-HUB-BODY
           MOVE 1 TO WS-PTR
           STRING
               "key="
               TRIM(WS-HUB-KEY)
               "&after_event_id=0"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           MOVE "hub_req.tmp" TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "  [preview] hub_req open err " WS-FS
               MOVE "work.tmp" TO WS-WORK-PATH
               EXIT PARAGRAPH
           END-IF
           WRITE WORK-REC FROM WS-HUB-BODY
           CLOSE WORK-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           INITIALIZE WS-CMD
           STRING "rm -f hub_resp.json"
               DELIMITED SIZE INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           INITIALIZE WS-CMD
           STRING
               "curl -s --max-time 30 "
               "-o hub_resp.json"
               " -X POST "
               TRIM(WS-PREVIEW-URL)
               " -H " WS-QT
               "Content-Type: "
               "application/x-www-form-urlencoded"
               WS-QT
               " -d @hub_req.tmp"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           MOVE "hub_resp.json" TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           IF WS-JLEN = 0
               DISPLAY "  [preview] empty response"
               EXIT PARAGRAPH
           END-IF

           DISPLAY "  [preview] resp(200): "
               WS-JBUF(1:200)

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-JBUF(1:WS-JLEN)
               TALLYING WS-TALLY-CNT
               FOR ALL X"22" & "active" & X"22" & ":true"
           IF WS-TALLY-CNT = 0
               DISPLAY "  [preview] not active"
               EXIT PARAGRAPH
           END-IF

           PERFORM FIND-PLAYER-START
           IF WS-JPOS = 0
               DISPLAY "  [preview] no player block"
               EXIT PARAGRAPH
           END-IF

           MOVE "row" TO WS-KEY-SEARCH
           PERFORM FIND-JSON-VAL
           IF WS-JVAL = SPACES
               DISPLAY "  [preview] no player.row"
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-PREVIEW-PLAYER-ROW =
               FUNCTION NUMVAL(WS-JVAL) + 1

           MOVE "col" TO WS-KEY-SEARCH
           PERFORM FIND-JSON-VAL
           IF WS-JVAL = SPACES
               DISPLAY "  [preview] no player.col"
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-PREVIEW-PLAYER-COL =
               FUNCTION NUMVAL(WS-JVAL) + 1

           MOVE "baseRow" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF WS-JVAL = SPACES
               DISPLAY "  [preview] no baseRow"
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-PREVIEW-BASE-ROW =
               FUNCTION NUMVAL(WS-JVAL) + 1
           MOVE 12 TO WS-PREVIEW-BASE-COL

           PERFORM VARYING WS-PARSE-ROW FROM 1 BY 1
               UNTIL WS-PARSE-ROW > 3
               PERFORM VARYING WS-PARSE-COL FROM 1 BY 1
                   UNTIL WS-PARSE-COL > 12
                   MOVE "?" TO WS-MAP-CELL(
                       WS-PARSE-ROW, WS-PARSE-COL)
               END-PERFORM
           END-PERFORM

           PERFORM PARSE-PREVIEW-MAP
           IF WS-PREVIEW-OK NOT = "Y"
               DISPLAY "  [preview] map parse fail"
               EXIT PARAGRAPH
           END-IF

           MOVE "Y" TO WS-PREVIEW-OK
           PERFORM VARYING WS-PARSE-COL FROM
               WS-PREVIEW-PLAYER-COL BY 1
               UNTIL WS-PARSE-COL > 12
                  OR WS-PREVIEW-OK = "N"
               IF WS-MAP-CELL(1, WS-PARSE-COL) = "?"
               AND WS-MAP-CELL(2, WS-PARSE-COL) = "?"
               AND WS-MAP-CELL(3, WS-PARSE-COL) = "?"
                   DISPLAY "  [preview] fog at col "
                       WS-PARSE-COL " - abandon planned"
                   MOVE "N" TO WS-PREVIEW-OK
               END-IF
           END-PERFORM

           IF WS-PREVIEW-OK = "Y"
               DISPLAY "  [preview] full map OK player=("
                   WS-PREVIEW-PLAYER-ROW ","
                   WS-PREVIEW-PLAYER-COL ") base=("
                   WS-PREVIEW-BASE-ROW ","
                   WS-PREVIEW-BASE-COL ")"
           END-IF
           .

      *> ============================================================
      *> PARSE-PREVIEW-MAP
      *> ============================================================
       PARSE-PREVIEW-MAP.
           MOVE "N" TO WS-PREVIEW-OK

           MOVE 0 TO WS-KEY-POS
           PERFORM VARYING WS-N FROM 1 BY 1
               UNTIL WS-N + 6 > WS-JLEN
                  OR WS-KEY-POS > 0
               IF WS-JBUF(WS-N:6) =
                   X"22" & "map" & X"22" & ":"
                   MOVE WS-N TO WS-KEY-POS
               END-IF
           END-PERFORM
           IF WS-KEY-POS = 0
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-PARSE-POS = WS-KEY-POS + 6
           PERFORM UNTIL WS-PARSE-POS > WS-JLEN
                     OR WS-JBUF(WS-PARSE-POS:1) = "["
               ADD 1 TO WS-PARSE-POS
           END-PERFORM
           IF WS-PARSE-POS > WS-JLEN
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-PARSE-POS

           MOVE 1 TO WS-PARSE-ROW
           PERFORM UNTIL WS-PARSE-ROW > 3
               PERFORM UNTIL WS-PARSE-POS > WS-JLEN
                         OR WS-JBUF(WS-PARSE-POS:1) = "["
                   ADD 1 TO WS-PARSE-POS
               END-PERFORM
               IF WS-PARSE-POS > WS-JLEN
                   EXIT PARAGRAPH
               END-IF
               ADD 1 TO WS-PARSE-POS

               MOVE 1 TO WS-PARSE-COL
               PERFORM UNTIL WS-PARSE-POS > WS-JLEN
                         OR WS-JBUF(WS-PARSE-POS:1) = "]"
                         OR WS-PARSE-COL > 12
                   IF WS-JBUF(WS-PARSE-POS:1) = " "
                   OR WS-JBUF(WS-PARSE-POS:1) = ","
                   OR WS-JBUF(WS-PARSE-POS:1) = X"09"
                   OR WS-JBUF(WS-PARSE-POS:1) = X"0A"
                   OR WS-JBUF(WS-PARSE-POS:1) = X"0D"
                       ADD 1 TO WS-PARSE-POS
                   ELSE
                       MOVE SPACES TO WS-PARSE-VAL
                       MOVE 0 TO WS-PARSE-VAL-LEN
                       PERFORM UNTIL WS-PARSE-POS > WS-JLEN
                             OR WS-JBUF(WS-PARSE-POS:1) = ","
                             OR WS-JBUF(WS-PARSE-POS:1) = "]"
                             OR WS-JBUF(WS-PARSE-POS:1) = " "
                           IF WS-PARSE-VAL-LEN < 10
                               ADD 1 TO WS-PARSE-VAL-LEN
                               MOVE WS-JBUF(WS-PARSE-POS:1)
                                 TO WS-PARSE-VAL(
                                   WS-PARSE-VAL-LEN:1)
                           END-IF
                           ADD 1 TO WS-PARSE-POS
                       END-PERFORM

                       IF WS-PARSE-VAL-LEN > 0
                       AND WS-PARSE-COL <= 12
                           IF WS-PARSE-VAL(1:1) = "1"
                               MOVE "S" TO WS-MAP-CELL(
                                   WS-PARSE-ROW,
                                   WS-PARSE-COL)
                           ELSE
                               IF WS-PARSE-VAL(1:4) = "null"
                                   MOVE "?" TO WS-MAP-CELL(
                                       WS-PARSE-ROW,
                                       WS-PARSE-COL)
                               ELSE
                                   MOVE "F" TO WS-MAP-CELL(
                                       WS-PARSE-ROW,
                                       WS-PARSE-COL)
                               END-IF
                           END-IF
                           ADD 1 TO WS-PARSE-COL
                       END-IF
                   END-IF
               END-PERFORM

               IF WS-PARSE-POS <= WS-JLEN
                  AND WS-JBUF(WS-PARSE-POS:1) = "]"
                   ADD 1 TO WS-PARSE-POS
               END-IF
               ADD 1 TO WS-PARSE-ROW
           END-PERFORM

           MOVE "Y" TO WS-PREVIEW-OK
           .

      *> ============================================================
      *> PLAN-PATH
      *> Plain BFS from (player_row, player_col) to any cell in
      *> column base_col (12). Cells "S" or "?" are blocked.
      *> ============================================================
       PLAN-PATH.
           MOVE 0 TO WS-PLAN-CNT
           MOVE "N" TO WS-BEST-FOUND
           MOVE 0 TO WS-BEST-LEN
           MOVE 99 TO WS-BEST-DIAG

           PERFORM VARYING WS-BFS-R FROM 1 BY 1
               UNTIL WS-BFS-R > 3
               PERFORM VARYING WS-BFS-C FROM 1 BY 1
                   UNTIL WS-BFS-C > 12
                   MOVE "N" TO WS-VISITED-CELL(
                       WS-BFS-R, WS-BFS-C)
               END-PERFORM
           END-PERFORM

           MOVE 1 TO WS-BFS-CNT
           MOVE 1 TO WS-BFS-HEAD
           MOVE WS-PREVIEW-PLAYER-ROW TO WS-BFS-ROW(1)
           MOVE WS-PREVIEW-PLAYER-COL TO WS-BFS-COL(1)
           MOVE 0 TO WS-BFS-PATH-LEN(1)
           MOVE "Y" TO WS-VISITED-CELL(
               WS-PREVIEW-PLAYER-ROW, WS-PREVIEW-PLAYER-COL)

           PERFORM UNTIL WS-BFS-HEAD > WS-BFS-CNT
                     OR WS-BFS-HEAD > 39

               MOVE WS-BFS-ROW(WS-BFS-HEAD) TO WS-BFS-R
               MOVE WS-BFS-COL(WS-BFS-HEAD) TO WS-BFS-C
               MOVE WS-BFS-PATH-LEN(WS-BFS-HEAD) TO WS-BFS-L

               IF WS-BFS-C >= WS-PREVIEW-BASE-COL
                   MOVE 0 TO WS-M
                   PERFORM VARYING WS-N FROM 1 BY 1
                       UNTIL WS-N > WS-BFS-L
                       IF WS-BFS-PATH(WS-BFS-HEAD, WS-N)
                           NOT = "go"
                           ADD 1 TO WS-M
                       END-IF
                   END-PERFORM

                   COMPUTE WS-BFS-DR =
                       WS-BFS-R - WS-PREVIEW-BASE-ROW
                   IF WS-BFS-DR < 0
                       COMPUTE WS-BFS-DR = 0 - WS-BFS-DR
                   END-IF
                   MOVE WS-BFS-DR TO WS-BFS-DR-UNS

                   IF WS-BEST-FOUND = "N"
                   OR WS-BFS-DR-UNS < WS-BEST-DIAG
                   OR (WS-BFS-DR-UNS = WS-BEST-DIAG
                       AND WS-M < WS-BEST-LEN)
                       MOVE "Y" TO WS-BEST-FOUND
                       MOVE WS-BFS-R TO WS-BEST-ROW
                       MOVE WS-BFS-DR-UNS TO WS-BEST-DIAG
                       MOVE WS-M TO WS-BEST-LEN
                       MOVE WS-BFS-L TO WS-PLAN-CNT
                       PERFORM VARYING WS-N FROM 1 BY 1
                           UNTIL WS-N > WS-BFS-L
                           MOVE WS-BFS-PATH(
                               WS-BFS-HEAD, WS-N)
                               TO WS-PLAN-MOVE(WS-N)
                       END-PERFORM
                   END-IF
               ELSE
                   PERFORM BFS-EXPAND-UP
                   PERFORM BFS-EXPAND-GO
                   PERFORM BFS-EXPAND-DOWN
               END-IF

               ADD 1 TO WS-BFS-HEAD
           END-PERFORM

           IF WS-BEST-FOUND = "N"
               DISPLAY "  [plan] BFS no path"
               MOVE 0 TO WS-PLAN-CNT
           ELSE
               DISPLAY "  [plan] picked end_row="
                   WS-BEST-ROW " len=" WS-PLAN-CNT
                   " diag=" WS-BEST-LEN
           END-IF
           .

      *> ============================================================
      *> BFS-EXPAND-UP (dr=-1, "left")
      *> ============================================================
       BFS-EXPAND-UP.
           IF WS-BFS-R <= 1
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-BFS-NR = WS-BFS-R - 1
           COMPUTE WS-BFS-NC = WS-BFS-C + 1
           IF WS-BFS-NC > WS-PREVIEW-BASE-COL
               EXIT PARAGRAPH
           END-IF
           IF WS-VISITED-CELL(WS-BFS-NR, WS-BFS-NC) = "Y"
               EXIT PARAGRAPH
           END-IF
           IF WS-MAP-CELL(WS-BFS-NR, WS-BFS-NC) = "S"
           OR WS-MAP-CELL(WS-BFS-NR, WS-BFS-NC) = "?"
               EXIT PARAGRAPH
           END-IF
           IF WS-BFS-CNT >= 40
               EXIT PARAGRAPH
           END-IF
           IF WS-BFS-L >= 15
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO WS-BFS-CNT
           MOVE WS-BFS-NR TO WS-BFS-ROW(WS-BFS-CNT)
           MOVE WS-BFS-NC TO WS-BFS-COL(WS-BFS-CNT)
           COMPUTE WS-BFS-PATH-LEN(WS-BFS-CNT) =
               WS-BFS-L + 1
           PERFORM VARYING WS-N FROM 1 BY 1
               UNTIL WS-N > WS-BFS-L
               MOVE WS-BFS-PATH(WS-BFS-HEAD, WS-N)
                   TO WS-BFS-PATH(WS-BFS-CNT, WS-N)
           END-PERFORM
           COMPUTE WS-N = WS-BFS-L + 1
           MOVE "left" TO WS-BFS-PATH(WS-BFS-CNT, WS-N)
           MOVE "Y" TO WS-VISITED-CELL(WS-BFS-NR, WS-BFS-NC)
           .

      *> ============================================================
      *> BFS-EXPAND-GO (dr=0, "go")
      *> ============================================================
       BFS-EXPAND-GO.
           MOVE WS-BFS-R TO WS-BFS-NR
           COMPUTE WS-BFS-NC = WS-BFS-C + 1
           IF WS-BFS-NC > WS-PREVIEW-BASE-COL
               EXIT PARAGRAPH
           END-IF
           IF WS-VISITED-CELL(WS-BFS-NR, WS-BFS-NC) = "Y"
               EXIT PARAGRAPH
           END-IF
           IF WS-MAP-CELL(WS-BFS-NR, WS-BFS-NC) = "S"
           OR WS-MAP-CELL(WS-BFS-NR, WS-BFS-NC) = "?"
               EXIT PARAGRAPH
           END-IF
           IF WS-BFS-CNT >= 40
               EXIT PARAGRAPH
           END-IF
           IF WS-BFS-L >= 15
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO WS-BFS-CNT
           MOVE WS-BFS-NR TO WS-BFS-ROW(WS-BFS-CNT)
           MOVE WS-BFS-NC TO WS-BFS-COL(WS-BFS-CNT)
           COMPUTE WS-BFS-PATH-LEN(WS-BFS-CNT) =
               WS-BFS-L + 1
           PERFORM VARYING WS-N FROM 1 BY 1
               UNTIL WS-N > WS-BFS-L
               MOVE WS-BFS-PATH(WS-BFS-HEAD, WS-N)
                   TO WS-BFS-PATH(WS-BFS-CNT, WS-N)
           END-PERFORM
           COMPUTE WS-N = WS-BFS-L + 1
           MOVE "go" TO WS-BFS-PATH(WS-BFS-CNT, WS-N)
           MOVE "Y" TO WS-VISITED-CELL(WS-BFS-NR, WS-BFS-NC)
           .

      *> ============================================================
      *> BFS-EXPAND-DOWN (dr=+1, "right")
      *> ============================================================
       BFS-EXPAND-DOWN.
           IF WS-BFS-R >= 3
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-BFS-NR = WS-BFS-R + 1
           COMPUTE WS-BFS-NC = WS-BFS-C + 1
           IF WS-BFS-NC > WS-PREVIEW-BASE-COL
               EXIT PARAGRAPH
           END-IF
           IF WS-VISITED-CELL(WS-BFS-NR, WS-BFS-NC) = "Y"
               EXIT PARAGRAPH
           END-IF
           IF WS-MAP-CELL(WS-BFS-NR, WS-BFS-NC) = "S"
           OR WS-MAP-CELL(WS-BFS-NR, WS-BFS-NC) = "?"
               EXIT PARAGRAPH
           END-IF
           IF WS-BFS-CNT >= 40
               EXIT PARAGRAPH
           END-IF
           IF WS-BFS-L >= 15
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO WS-BFS-CNT
           MOVE WS-BFS-NR TO WS-BFS-ROW(WS-BFS-CNT)
           MOVE WS-BFS-NC TO WS-BFS-COL(WS-BFS-CNT)
           COMPUTE WS-BFS-PATH-LEN(WS-BFS-CNT) =
               WS-BFS-L + 1
           PERFORM VARYING WS-N FROM 1 BY 1
               UNTIL WS-N > WS-BFS-L
               MOVE WS-BFS-PATH(WS-BFS-HEAD, WS-N)
                   TO WS-BFS-PATH(WS-BFS-CNT, WS-N)
           END-PERFORM
           COMPUTE WS-N = WS-BFS-L + 1
           MOVE "right" TO WS-BFS-PATH(WS-BFS-CNT, WS-N)
           MOVE "Y" TO WS-VISITED-CELL(WS-BFS-NR, WS-BFS-NC)
           .


      *> ============================================================
      *> WRITE-LLM-REQ
      *> Dump WS-REQ-JSON(1:WS-PTR-1) to llm_req.json as one rec.
      *> ============================================================
       WRITE-LLM-REQ.
           MOVE "llm_req.json" TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "    llm_req open err " WS-FS
               MOVE "work.tmp" TO WS-WORK-PATH
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-K = WS-PTR - 1
           IF WS-K < 1
               MOVE 1 TO WS-K
           END-IF
           MOVE SPACES TO WORK-REC
           MOVE WS-REQ-JSON(1:WS-K)
               TO WORK-REC(1:WS-K)
           WRITE WORK-REC
           CLOSE WORK-FILE
           MOVE "work.tmp" TO WS-WORK-PATH
           .

      *> ============================================================
      *> SEND-LLM-REQUEST
      *> curl the request file to OpenAI and parse "content" into
      *> WS-LLM-RAW.
      *> ============================================================
       SEND-LLM-REQUEST.
           MOVE SPACES TO WS-LLM-RAW

           INITIALIZE WS-CMD
           STRING "rm -f llm_resp.json"
               DELIMITED SIZE INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           INITIALIZE WS-CMD
           STRING
               "curl -s --max-time 60 "
               "-o llm_resp.json"
               " -X POST "
               TRIM(WS-OPENAI-URL)
               " -H " WS-QT
               "Content-Type: "
               "application/json"
               WS-QT
               " -H " WS-QT
               "Authorization: Bearer "
               TRIM(WS-OPENAI-KEY)
               WS-QT
               " -d @llm_req.json"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           MOVE "llm_resp.json" TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           IF WS-JLEN = 0
               DISPLAY "    [llm] empty response"
               EXIT PARAGRAPH
           END-IF

           MOVE "message" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL

           MOVE "content" TO WS-KEY-SEARCH
           PERFORM FIND-JSON-VAL

           IF WS-JVAL NOT = SPACES
               MOVE SPACES TO WS-ESC-IN
               MOVE WS-JVAL TO WS-ESC-IN
               PERFORM JSON-UNESCAPE-STR
               IF WS-ESC-OLEN > 0
                   IF WS-ESC-OLEN > 4000
                       MOVE WS-ESC-OUT(1:4000)
                           TO WS-LLM-RAW
                   ELSE
                       MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
                           TO WS-LLM-RAW
                   END-IF
               END-IF
           END-IF
           .

      *> ============================================================
      *> Copybook procedures (env loader, JSON helpers)
      *> ============================================================
       COPY ENVLOAD-PROC.
       COPY JSONPARSE-PROC.
       COPY JSONESCAPE-PROC.
       COPY JSONUNESCAPE-PROC.

      *> ============================================================
      *> READ-JSON-FILE (inline, buffer matches WS-JBUF / WS-LINE)
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
               READ WORK-FILE INTO WS-LINE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       MOVE LENGTH(
                           TRIM(WS-LINE TRAILING))
                           TO WS-K
                       IF WS-K > 0
                           IF WS-JLEN > 0
                               ADD 1 TO WS-JLEN
                               MOVE " "
                                 TO WS-JBUF(
                                 WS-JLEN:1)
                           END-IF
                           IF WS-K > 65535
                               MOVE 65535 TO WS-K
                           END-IF
                           IF WS-JLEN + WS-K
                               <= 65535
                               MOVE WS-LINE(1:WS-K)
                                 TO WS-JBUF(
                                 WS-JLEN + 1:WS-K)
                               ADD WS-K TO WS-JLEN
                           END-IF
                       END-IF
               END-READ
           END-PERFORM

           CLOSE WORK-FILE
           MOVE "N" TO WS-EOF
           .

