       IDENTIFICATION DIVISION.
       PROGRAM-ID. S05E03-SHELLACCESS.
      *> ============================================================
      *> S05E03 - Shellaccess: LLM planner drives a remote BusyBox
      *> sandbox via hub /verify with answer.cmd. Each turn:
      *>   1. Ask gpt-4.1-mini for next PlannerDecision JSON
      *>      (reason / cmd / done / final_answer)
      *>   2. POST {answer:{cmd:...}} to hub /verify
      *>   3. Feed server response back into conversation history
      *>   4. Stop when message contains "{FLG:" (flag captured)
      *> Port of Z:/AiDevs/Zadania/S05E03/app.py - same algorithmic
      *> flow, minus the optional geocode sanity-check tool.
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
       01  WORK-REC                PIC X(900000).

       WORKING-STORAGE SECTION.
      *> === Environment (via copybook) ===
       COPY ENVLOAD-WS.

      *> === File I/O ===
       01  WS-BS                   PIC X(1)
                                   VALUE X"5C".
       01  WS-FS                   PIC XX.
       01  WS-WORK-PATH            PIC X(100)
                                   VALUE "work.tmp".

      *> -- STRING pointer --
       01  WS-PTR                  PIC 9(7).

      *> -- System command --
       01  WS-CMD                  PIC X(8000).

      *> -- Request JSON buffer (LLM) --
       01  WS-REQ-JSON             PIC X(600000).

      *> -- Conversation buffer (messages array content) --
       01  WS-CONV-BUF             PIC X(500000).
       01  WS-CONV-PTR             PIC 9(7).

      *> -- System prompt buffer --
       01  WS-SYS-PROMPT           PIC X(40000).
       01  WS-SYS-PTR              PIC 9(6).

      *> -- Hub API request body --
       01  WS-HUB-BODY             PIC X(16000).

      *> -- Task constants --
       01  WS-TASK-NAME            PIC X(20)
                                   VALUE "shellaccess".

      *> -- Iteration counters --
       01  WS-ITER                 PIC 9(3)
                                   VALUE 0.
       01  WS-MAX-ITER             PIC 9(3)
                                   VALUE 30.
       01  WS-FLAG-FOUND           PIC X
                                   VALUE "N".
       01  WS-TALLY-CNT            PIC 9(4).

      *> -- Answer-attempt counter. Incremented every turn where
      *> -- the planner emits a cmd starting with "printf" (the
      *> -- task protocol requires the final answer to be printed
      *> -- via printf). Surfaced to the LLM in each SERVER message
      *> -- as pressure to actually submit before the iter budget
      *> -- runs out.
       01  WS-ANS-COUNT            PIC 9(3)
                                   VALUE 0.
       01  WS-ANS-DSP              PIC 9(3).

      *> -- Loop-guard ring buffer: remembers every submitted cmd
      *> -- within the current run plus the hub response it got.
      *> -- If the planner emits a byte-exact repeat we short-
      *> -- circuit the hub call and feed back a synthetic
      *> -- [loop-guard] note to break the fixation loop.
      *> --
      *> -- Capacity must be >= WS-MAX-ITER so a fixation after
      *> -- arbitrarily many intermediate exploration cmds is
      *> -- still caught. Sized at 30 to match WS-MAX-ITER VALUE.
      *> -- (Memory: 30 * (4000 + 4000 + 10) ~ 240 KB.)
       01  WS-DUP-COUNT            PIC 9(3)
                                   VALUE 0.
       01  WS-DUP-NEXT             PIC 9(3)
                                   VALUE 1.
       01  WS-DUP-HIT              PIC X
                                   VALUE "N".
       01  WS-DUP-IDX              PIC 9(3).
       01  WS-DUP-MATCH            PIC 9(3).
       01  WS-DUP-SLOTS.
           05  WS-DUP-SLOT OCCURS 30 TIMES.
               10  WS-DUP-CMD      PIC X(4000).
               10  WS-DUP-CMD-LEN  PIC 9(5).
               10  WS-DUP-RESP     PIC X(4000).
               10  WS-DUP-RESP-LEN PIC 9(5).

      *> -- Planner decision fields --
       01  WS-PL-REASON            PIC X(4000).
       01  WS-PL-CMD               PIC X(4000).
       01  WS-PL-DONE              PIC X(10).
       01  WS-PL-RAW               PIC X(8000).

      *> -- Server response snippet --
       01  WS-SRV-SNIPPET          PIC X(6000).
       01  WS-SRV-LEN              PIC 9(7).

      *> -- Scratch for fence-strip (avoids self-overlap MOVE) --
       01  WS-SCRATCH              PIC X(8000).

      *> -- Chunk write vars --
       01  WS-CHK-POS              PIC 9(7).
       01  WS-CHK-REM              PIC 9(7).
       01  WS-CHK-LEN              PIC 9(7).

      *> -- Loop/misc vars --
       01  WS-I                    PIC 9(7).
       01  WS-K                    PIC 9(7).
       01  WS-ITER-DSP             PIC 9(3).

      *> === JSONPARSE-WS (inline, large) ===
       01  WS-JBUF                 PIC X(900000).
       01  WS-JLEN                 PIC 9(7).
       01  WS-JPOS                 PIC 9(7).
       01  WS-JVAL                 PIC X(8000).
       01  WS-KEY-SEARCH           PIC X(50).
       01  WS-KEY-POS              PIC 9(7).
       01  WS-VAL-START            PIC 9(7).
       01  WS-VAL-END              PIC 9(7).
       01  WS-FJV-POS              PIC 9(7).
       01  WS-TMP2                 PIC X(500).

      *> === JSONESCAPE-WS (inline, enlarged) ===
       01  WS-ESC-IN               PIC X(40000).
       01  WS-ESC-OUT              PIC X(80000).
       01  WS-ESC-ILEN             PIC 9(7).
       01  WS-ESC-OLEN             PIC 9(7).
       01  WS-ESC-I                PIC 9(7).
      *> -- JSON-UNESCAPE-STR \uXXXX decoder scratch fields --
       01  WS-UNESC-CP             PIC 9(5).
       01  WS-UNESC-HX             PIC X.
       01  WS-UNESC-NIB            PIC 9(2).
       01  WS-UNESC-K              PIC 9(1).
       01  WS-UNESC-B              PIC 9(3).
       01  WS-UNESC-Q              PIC 9(5).
       01  WS-UNESC-OK             PIC X.

      *> === JSONREAD-WS (inline, large) ===
       01  WS-EOF                  PIC X VALUE "N".
       01  WS-LINE                 PIC X(900000).

      *> === Task brief (hardcoded, no external file) ===
       01  WS-TASK-BRIEF           PIC X(8000).
       01  WS-TASK-BRIEF-LEN       PIC 9(5) VALUE 0.

      *> === WORKING MEMORY block ===
      *> -- Structured, additive-only scratchpad the runtime builds
      *> -- from observed server outputs. Re-injected into every user
      *> -- turn (top AND bottom of the content field) so the planner
      *> -- does not have to re-derive facts already in evidence.
      *> -- Hard-capped at WM-CAP chars; new entries are truncated
      *> -- silently if they would overflow (additive only, never
      *> -- replaces earlier facts).
       01  WM-CAP                  PIC 9(5) VALUE 1500.
       01  WM-FILES-BUF            PIC X(400).
       01  WM-FILES-LEN            PIC 9(5) VALUE 0.
       01  WM-SCHEMA-BUF           PIC X(400).
       01  WM-SCHEMA-LEN           PIC 9(5) VALUE 0.
       01  WM-EVENTS-BUF           PIC X(600).
       01  WM-EVENTS-LEN           PIC 9(5) VALUE 0.
       01  WM-LOCID-BUF            PIC X(400).
       01  WM-LOCID-LEN            PIC 9(5) VALUE 0.
       01  WM-COORDS-BUF           PIC X(600).
       01  WM-COORDS-LEN           PIC 9(5) VALUE 0.

      *> -- Rendered WORKING MEMORY block (assembled each turn just
      *> -- before sandwich injection). Escaped for JSON embedding.
       01  WM-RENDER               PIC X(2500).
       01  WM-RENDER-LEN           PIC 9(5) VALUE 0.
       01  WM-RENDER-ESC           PIC X(5000).
       01  WM-RENDER-ESC-LEN       PIC 9(5) VALUE 0.

      *> -- Scan cursors for WM-EXTRACT paragraph --
       01  WM-SCAN-POS             PIC 9(7).
       01  WM-SCAN-END             PIC 9(7).
       01  WM-LINE-START           PIC 9(7).
       01  WM-LINE-END             PIC 9(7).
       01  WM-LINE-LEN             PIC 9(5).
       01  WM-LINE                 PIC X(2000).
       01  WM-TMP                  PIC X(400).
       01  WM-TMP-LEN              PIC 9(5).
       01  WM-TALLY                PIC 9(4).
       01  WM-INT-A                PIC 9(7).
       01  WM-INT-B                PIC 9(7).
       01  WM-I                    PIC 9(5).
       01  WM-DUP-FLAG             PIC X.

      *> -- Scratch buffer that holds WS-SRV-SNIPPET after the
      *> -- literal \n sequences from the JSON-escaped server
      *> -- output have been converted back into real X"0A"
      *> -- line-feeds. Only written/read inside WM-EXTRACT
      *> -- (scanner expects a real-line format).
       01  WM-UNESC                PIC X(6000).
       01  WM-UNESC-LEN            PIC 9(7) VALUE 0.
       01  WM-UNESC-SRC            PIC 9(7).
       01  WM-UNESC-DST            PIC 9(7).

      *> -- Pre-assembled JSON-key patterns used by INSPECT in
      *> -- WM-SCAN-LINE. GnuCOBOL's INSPECT operand does not
      *> -- accept & concatenation so we store the quoted keys as
      *> -- working-storage literals. Each one is '"keyname"'.
       01  WM-PAT-CODE             PIC X(6)
                                   VALUE '"code"'.
       01  WM-PAT-MSG              PIC X(9)
                                   VALUE '"message"'.
       01  WM-PAT-LOCID            PIC X(13)
                                   VALUE '"location_id"'.
       01  WM-PAT-NAME             PIC X(6)
                                   VALUE '"name"'.
       01  WM-PAT-LAT              PIC X(10)
                                   VALUE '"latitude"'.
       01  WM-PAT-LON              PIC X(11)
                                   VALUE '"longitude"'.
       01  WM-PAT-TYPE             PIC X(6)
                                   VALUE '"type"'.
       01  WM-PAT-ENTRY            PIC X(10)
                                   VALUE '"entry_id"'.

      *> -- Forced-commit trigger iteration (~ half of budget) --
       01  WS-COMMIT-ITER          PIC 9(3) VALUE 15.
       01  WS-COMMIT-INJECTED      PIC X VALUE "N".

      *> -- Deterministic verification gate state --
       01  WS-GATE-OK              PIC X VALUE "N".
       01  WS-GATE-REASON          PIC X(400).
       01  WS-GATE-REASON-LEN      PIC 9(5) VALUE 0.
       01  WS-GATE-FIELD           PIC X(20).
       01  WS-GATE-DATE            PIC X(30).
       01  WS-GATE-CITY            PIC X(60).
       01  WS-GATE-LAT             PIC X(30).
       01  WS-GATE-LON             PIC X(30).
       01  WS-GATE-FA              PIC X(4000).
       01  WS-GATE-FA-LEN          PIC 9(5) VALUE 0.
       01  WS-GATE-HAY             PIC X(100000).
       01  WS-GATE-HAY-LEN         PIC 9(7) VALUE 0.
       01  WS-GATE-POS             PIC 9(7).
       01  WS-GATE-FOUND           PIC X.
       01  WS-GATE-LAT-LINE        PIC 9(5).
       01  WS-GATE-LON-LINE        PIC 9(5).
       01  WS-GATE-TYPE-LINE       PIC 9(5).
       01  WS-GATE-LINE-NUM        PIC 9(5).

      *> -- City correction table (10 Polish cities) --
       01  WS-CITY-COUNT            PIC 9(2) VALUE 10.
       01  WS-CITY-TABLE.
           05  WS-CITY-ENTRY OCCURS 10 TIMES.
               10  WS-CITY-NAME    PIC X(20).
               10  WS-CITY-LAT     PIC S9(3)V9(6)
                                   COMP-3.
               10  WS-CITY-LON     PIC S9(3)V9(6)
                                   COMP-3.
       01  WS-CC-LAT               PIC S9(3)V9(6)
                                   COMP-3.
       01  WS-CC-LON               PIC S9(3)V9(6)
                                   COMP-3.
       01  WS-CC-DIST              PIC S9(5)V9(6)
                                   COMP-3.
       01  WS-CC-BEST-DIST         PIC S9(5)V9(6)
                                   COMP-3.
       01  WS-CC-BEST-IDX          PIC 9(2).
       01  WS-CC-I                 PIC 9(2).
       01  WS-CC-OLD-CITY          PIC X(60).
       01  WS-CC-NEW-CITY          PIC X(20).
       01  WS-CC-POS               PIC 9(5).
       01  WS-CC-SCRATCH           PIC X(4000).
       01  WS-CC-TAIL-POS          PIC 9(5).
       01  WS-CC-TAIL-LEN          PIC 9(5).
       01  WS-CC-SWAP-TMP          PIC S9(3)V9(6)
                                   COMP-3.
       01  WS-CC-ORIG-LAT          PIC S9(3)V9(6)
                                   COMP-3.
       01  WS-CC-ORIG-LON          PIC S9(3)V9(6)
                                   COMP-3.
       01  WS-CC-FIND-STR          PIC X(200).
       01  WS-CC-REPL-STR          PIC X(200).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S05E03 SHELLACCESS ==="

           PERFORM LOAD-ENV-VARS
           PERFORM INIT-CITY-TABLE

      *>   Load the hardcoded task brief into WS-TASK-BRIEF
           PERFORM LOAD-TASK-BRIEF

      *>   Build the system prompt once
           PERFORM BUILD-SYSTEM-PROMPT

      *>   Clean temp files from any prior run
           INITIALIZE WS-CMD
           STRING
               "rm -f hub_resp.json "
               "llm_resp.json "
               "llm_req.json "
               "hub_req.tmp "
               "work.tmp"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Initialize conversation buffer with a system_note that
      *>   tells the planner to begin exploration. Each entry is
      *>   appended as a {role,content} object; the full array is
      *>   spliced into the final messages payload (after the system
      *>   message) at LLM-call time.
           PERFORM INIT-CONVERSATION

      *>   Main agent loop
           MOVE 0 TO WS-ITER
           PERFORM UNTIL
               WS-FLAG-FOUND = "Y"
               OR WS-ITER >= WS-MAX-ITER

               ADD 1 TO WS-ITER
               MOVE WS-ITER TO WS-ITER-DSP
               DISPLAY " "
               DISPLAY "--- Iteration "
                   WS-ITER-DSP
                   " of " WS-MAX-ITER
                   " ---"

      *>       Forced-commit: if we reached the commit iter and
      *>       have not yet injected, drop a [SYSTEM-CONTROL]
      *>       message into the conversation BEFORE the next LLM
      *>       call so the planner re-weights exploration vs.
      *>       submission on the upcoming turn.
               PERFORM FORCED-COMMIT-INJECT

      *>       Ask planner for next decision
               PERFORM CALL-LLM
               IF WS-PL-RAW = SPACES
                   DISPLAY "  LLM returned empty; aborting"
                   EXIT PERFORM
               END-IF

               PERFORM PARSE-PLANNER-DECISION
               DISPLAY "  [PLAN] reason: "
                   TRIM(WS-PL-REASON)(1:200)
               DISPLAY "  [PLAN] cmd: "
                   TRIM(WS-PL-CMD)(1:300)
               DISPLAY "  [PLAN] done: "
                   TRIM(WS-PL-DONE)

               IF WS-PL-CMD = SPACES
                   DISPLAY "  Planner produced no cmd; aborting"
                   EXIT PERFORM
               END-IF

      *>       Track answer-attempt count (cmds that start with
      *>       "printf" — the task protocol's prescribed way of
      *>       emitting the final JSON payload).
               IF WS-PL-CMD(1:6) = "printf"
                   ADD 1 TO WS-ANS-COUNT
               END-IF

      *>       Append the planner reply verbatim as assistant msg
               PERFORM APPEND-ASST-MSG

      *>       Deterministic verification gate: if the planner
      *>       claims done=true, sanity-check the final_answer
      *>       fields against observed server history BEFORE we
      *>       let it hit centrala. A failed gate flips done back
      *>       to "false", appends a [VERIFICATION GATE FAILED]
      *>       note, and lets the loop take one more turn.
               IF WS-PL-DONE(1:4) = "true"
                   PERFORM VERIFY-GATE
                   IF WS-GATE-OK = "N"
      *>               Skip hub call this turn and jump straight
      *>               to the next iteration; the gate already
      *>               appended the failure note to WS-CONV-BUF.
                       EXIT PERFORM CYCLE
                   END-IF

      *>           Date guard: event is from 2024
                   IF WS-GATE-DATE(1:4)
                       NOT = "2024"
                       DISPLAY "  [GATE] date "
                           FUNCTION TRIM(
                           WS-GATE-DATE)
                           " rejected"
                           " - must be 2024"
                       MOVE "date" TO
                           WS-GATE-FIELD
                       PERFORM VERIFY-GATE-FAIL
                       EXIT PERFORM CYCLE
                   END-IF
               END-IF

      *>       Deterministic city correction: if the planner
      *>       claims done=true, override the LLM city with
      *>       the nearest Polish city by coordinate distance.
               IF WS-PL-DONE(1:4) = "true"
                   PERFORM CORRECT-CITY
               END-IF

      *>       Loop-guard: if this exact cmd was submitted earlier
      *>       in the run, short-circuit the hub call and synthesize
      *>       a local rejection so the planner has to pick a
      *>       different candidate instead of resubmitting.
               PERFORM LOOP-GUARD-CHECK
               IF WS-DUP-HIT = "N"
      *>           Execute cmd against hub /verify
                   PERFORM SEND-CMD-TO-HUB
      *>           Push the fresh (cmd, response) pair into the ring
                   PERFORM LOOP-GUARD-PUSH
               END-IF

      *>       Examine response for flag
               PERFORM EXTRACT-SERVER-SNIPPET
               PERFORM CHECK-FLAG-IN-JBUF

               IF WS-FLAG-FOUND = "Y"
                   DISPLAY " "
                   DISPLAY ">>> FLAG CAPTURED <<<"
                   DISPLAY TRIM(WS-JBUF)(1:1000)
                   EXIT PERFORM
               END-IF

      *>       WORKING MEMORY update: scan the fresh snippet and
      *>       append any new facts before we roll it into the
      *>       user turn. Must run AFTER EXTRACT-SERVER-SNIPPET
      *>       and BEFORE APPEND-SERVER-MSG (which reads the
      *>       rendered block via WM-RENDER-BLOCK).
               PERFORM WM-EXTRACT

      *>       Feed server response back as a user message note
               PERFORM APPEND-SERVER-MSG

           END-PERFORM

           DISPLAY " "
           IF WS-FLAG-FOUND = "Y"
               DISPLAY "=== DONE: FLAG FOUND ==="
           ELSE
               DISPLAY "=== DONE: no flag after "
                   WS-MAX-ITER " iterations ==="
           END-IF
           STOP RUN.

      *> ============================================================
      *> INIT-CONVERSATION
      *> Seed WS-CONV-BUF with a single user message that tells the
      *> planner to begin exploring /data. The conversation buffer
      *> will contain "[ {msg1}, {msg2}, ... ]" minus the outer
      *> brackets so that CALL-LLM can wrap it.
      *> ============================================================
       INIT-CONVERSATION.
           MOVE SPACES TO WS-CONV-BUF
           MOVE 1 TO WS-CONV-PTR
           STRING
               "{" WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               "BEGIN. Explore /data (start with "
               WS-BS WS-QT "ls -la /data"
               WS-BS WS-QT "), read file heads "
               "before grepping, follow every "
               "discipline in the system prompt. "
               "Reply ONLY with a single JSON "
               "PlannerDecision object."
               WS-QT "}"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING
           .

      *> ============================================================
      *> LOAD-TASK-BRIEF
      *> Hardcode the 3 essential brief pieces directly into
      *> WS-TASK-BRIEF so no external zadanie.md file is needed.
      *> ============================================================
       LOAD-TASK-BRIEF.
           MOVE SPACES TO WS-TASK-BRIEF
           MOVE 1 TO WS-PTR

           STRING
               "Your task: explore the remote "
               "server /data directory to find "
               "log files documenting when and "
               "where Rafal body was discovered."
               X"0A"
               "CRITICAL: Submit the date ONE "
               "DAY BEFORE the actual discovery "
               "date."
               X"0A"
               "The name Rafal does NOT appear "
               "in the logs. Do NOT grep for "
               "Rafal. Instead search for body "
               "discovery events: grep for "
               "znalezion or jaskini or cia "
               "(ASCII stems). The event is "
               "from 2024, not earlier."
               X"0A"
               "To extract a full record from "
               "gps.json: entry_id is the LAST "
               "field in each record, so use "
               "grep -B 4 '<entry_id>' "
               "/data/gps.json | head -5 to "
               "see latitude, longitude, type, "
               "location_id, and entry_id."
               X"0A"
               "Answer format: "
               DELIMITED SIZE
               INTO WS-TASK-BRIEF
               WITH POINTER WS-PTR
           END-STRING

           STRING
               '{"date":"YYYY-MM-DD",'
               '"city":"city_name",'
               '"longitude":float,'
               '"latitude":float}'
               DELIMITED SIZE
               INTO WS-TASK-BRIEF
               WITH POINTER WS-PTR
           END-STRING

           COMPUTE WS-TASK-BRIEF-LEN =
               WS-PTR - 1

           DISPLAY "  [BRIEF] hardcoded "
               WS-TASK-BRIEF-LEN " bytes"
           .

      *> ============================================================
      *> BUILD-SYSTEM-PROMPT
      *> Compose the system prompt (task brief + BusyBox sandbox
      *> rules + output protocol) into WS-SYS-PROMPT. Trimmed to
      *> stay well under the JSONESCAPE buffer (40000 chars).
      *> ============================================================
       BUILD-SYSTEM-PROMPT.
           MOVE SPACES TO WS-SYS-PROMPT
           MOVE 1 TO WS-SYS-PTR

           STRING
               "You are a Linux shell operator "
               "solving a task on a remote "
               "sandbox. Centrala runs ONE shell "
               "command per turn and returns its "
               "result. Converge in as few turns "
               "as possible."
               DELIMITED SIZE
               INTO WS-SYS-PROMPT
               WITH POINTER WS-SYS-PTR
           END-STRING

      *>   Inject the hardcoded task brief.
           STRING
               X"0A" X"0A"
               "=== TASK BRIEF ===" X"0A"
               DELIMITED SIZE
               INTO WS-SYS-PROMPT
               WITH POINTER WS-SYS-PTR
           END-STRING

           IF WS-TASK-BRIEF-LEN > 0
               MOVE WS-TASK-BRIEF(1:WS-TASK-BRIEF-LEN)
                   TO WS-SYS-PROMPT(
                   WS-SYS-PTR:WS-TASK-BRIEF-LEN)
               ADD WS-TASK-BRIEF-LEN TO WS-SYS-PTR
           END-IF

           STRING
               X"0A"
               "=== END TASK BRIEF ==="
               DELIMITED SIZE
               INTO WS-SYS-PROMPT
               WITH POINTER WS-SYS-PTR
           END-STRING

           STRING
               X"0A" X"0A"
               "=== SANDBOX FACTS "
               "(memorize - these caused prior "
               "failures) ===" X"0A"
               "- BusyBox v1.36.1 sh, not bash. "
               "NO bash-only features." X"0A"
               "- AVAILABLE binaries: sh ls cat "
               "head tail wc grep sed find echo "
               "printf cut sort uniq tr." X"0A"
               "- NOT AVAILABLE (do NOT attempt): "
               "awk, perl, python, python3, jq, "
               "bash, file. If you call one, "
               "BusyBox returns 'sh: <name>: "
               "not found'. Proven absent in "
               "prior runs - stop wasting turns "
               "trying them." X"0A"
               "- BusyBox find has no -printf; "
               "use find <dir> -type f + ls -la."
               X"0A"
               "- CRITICAL: NEVER include a "
               "literal '{' or '}' character in "
               "your cmd. The hub sandbox runs a "
               "JSON-validator path on cmds "
               "containing braces and returns a "
               "spurious code=-850 'JSON must "
               "contain exactly these fields' "
               "error. This is NOT a real JSON "
               "problem with your REPLY - it is a "
               "sandbox bug on the cmd string. "
               "Confirmed failing pattern: sed "
               "-n '/{/,/}/p'. Use LINE-NUMBER "
               "ranges instead: sed -n "
               "'10,30p' file. If you need to "
               "find record boundaries, grep for "
               "a key name first (grep -n "
               WS-BS WS-QT "some_key"
               WS-BS WS-QT " file) to learn line "
               "numbers, then sed -n 'N,Mp'."
               X"0A"
               "- FORBIDDEN: any redirect to "
               "/dev/null (2>/dev/null, "
               ">/dev/null, &>/dev/null). Sandbox "
               "returns code=-955 Access denied. "
               "Use 2>&1 if you need stderr."
               X"0A"
               "- code=-850 means your shell output "
               "contained something that looked "
               "like a JSON answer envelope "
               "({,},:). Use grep without -B/-A "
               "context on JSON files, or pipe "
               "through tr -d '{}:' when reading "
               "JSON object contents."
               X"0A"
               "- FORBIDDEN paths: /etc /root "
               "/proc dotfiles. Touching them "
               "bans the session."
               X"0A"
               "- Response envelope: {"
               WS-BS WS-QT "code" WS-BS WS-QT
               ":100," WS-BS WS-QT "message"
               WS-BS WS-QT ":...," WS-BS WS-QT
               "output" WS-BS WS-QT ":<text>}. "
               "code=100 means the shell ran. "
               "Non-100 means the sandbox "
               "blocked you. When code=100, READ "
               "the output field - empty output "
               "usually means grep matched "
               "nothing, NOT success."
               DELIMITED SIZE
               INTO WS-SYS-PROMPT
               WITH POINTER WS-SYS-PTR
           END-STRING

           STRING
               X"0A" X"0A"
               "=== [HINT] MULTILINGUAL SEARCH "
               "VOCABULARY ==="
               X"0A"
               "Source data is often in a "
               "language different from the brief "
               "(frequently Polish). Your literal "
               "English search terms will miss "
               "relevant rows. When a keyword "
               "search returns zero hits, expand "
               "it using the translation families "
               "below - they are the ones that "
               "most often appear in this kind of "
               "corpus:"
               X"0A" X"0A"
               "DISCOVERY / SIGHTING VERBS: "
               "znaleziono, odnaleziono, odkryto, "
               "namierzono, natrafiono, "
               "zlokalizowano, wykryto, ustalono, "
               "odszukano, dostrzeżono, "
               "zauważono, zaobserwowano, "
               "ujawniono, zidentyfikowano."
               X"0A" X"0A"
               "BODY / REMAINS NOUNS: ciało, "
               "ciała, zwłoki, szczątki, martwego, "
               "martwej, martwy, nieżywy, "
               "nieprzytomny, ofiara, zmarły, "
               "zabity."
               X"0A" X"0A"
               "EVENT / DEATH MODIFIERS: zabity, "
               "zabita, zabito, śmierć, zginął, "
               "zginęła, ofiara, zamordowany, "
               "zamordowana, morderstwo, "
               "uśmiercony, pobity, ugodzony, "
               "raniony, nie żyje, nieżyjący."
               X"0A" X"0A"
               "PLACE-TYPE NOUNS (BusyBox-friendly "
               "stems - grep for the stem without "
               "the suffix to catch every "
               "declension): jaski, grota, groty, "
               "pieczara, las, lasu, lesie, chata, "
               "chaty, chacie, hangar, hangaru, "
               "polana, polany, ruiny, bunker, "
               "piwnica, strych, pole."
               X"0A" X"0A"
               "PERSON-NAME MATCHING: Polish "
               "surnames and given names decline. "
               "Search for the shortest unique "
               "ROOT of the name (e.g. the first "
               "5-7 letters with no suffix) and "
               "rely on grep -i for case so you "
               "catch all inflected forms. Do "
               "NOT require an exact match on the "
               "name."
               X"0A" X"0A"
               "ALTERNATIVE PATTERN: when you "
               "need a single grep that catches "
               "many synonyms, use grep -E with "
               "alternation: grep -iE "
               WS-BS WS-QT "znalezion|odnalezion|"
               "odkry|namierzono|zlokalizow|"
               "wykryto|ustalono"
               WS-BS WS-QT " <file>. The trailing "
               "letters are intentionally "
               "truncated so the pattern matches "
               "every inflected form. A SINGLE "
               "-iE pass beats many successive "
               "grep calls."
               X"0A" X"0A"
               "NUMERIC LOOKUP IN JSON: never use "
               "grep '<id>' <file.json> because "
               "Unicode escapes like "
               WS-BS "u0219 contain digit "
               "substrings that cause false "
               "positives. Always key the grep "
               "to a JSON field: grep '"
               WS-QT "location_id" WS-QT
               ": <id>' <file> and extract the "
               "city/name from context lines "
               "with -B 3 -A 3 on the SAME "
               "JSON object."
               X"0A" X"0A"
               "CRITICAL GREP ENCODING RULE: "
               "Polish diacritics (ą,ć,ę,ł,ń,"
               "ó,ś,ź,ż) are multi-byte UTF-8 "
               "and BREAK grep on this BusyBox "
               "shell. NEVER use diacritics or "
               "Unicode escapes in grep patterns."
               " Use ONLY the ASCII portion of "
               "the word as the search stem. "
               "Examples: grep 'cia' not 'ciało'"
               ", grep 'zw' not 'zwłoki', grep "
               "'jaskini' not 'jaskiń', grep "
               "'Grudzia' not 'Grudziądz'. "
               "Strip the word at the FIRST "
               "non-ASCII character and use "
               "that prefix as the pattern."
               X"0A" X"0A"
               "When you see the brief ask for "
               "an English verb (found, killed, "
               "discovered, located, identified), "
               "your FIRST search should already "
               "be a Polish alternation, not the "
               "literal English word - searching "
               "for 'found' in a Polish corpus is "
               "a guaranteed zero-hit waste of a "
               "turn."
               DELIMITED SIZE
               INTO WS-SYS-PROMPT
               WITH POINTER WS-SYS-PTR
           END-STRING

           STRING
               X"0A" X"0A"
               "=== EXTRACTION DISCIPLINE ==="
               X"0A"
               "In your reason field every turn, "
               "FIRST quote the most relevant "
               "1-2 lines from the previous "
               "server output, THEN state your "
               "decision. If the previous output "
               "is empty, say so explicitly and "
               "pick a different angle (different "
               "file, different keyword, "
               "broader/narrower grep). The "
               "moment you have enough verified "
               "data to satisfy the TASK BRIEF, "
               "STOP exploring - but only AFTER "
               "passing the MANDATORY "
               "VERIFICATION below. Premature "
               "stopping is the #1 cause of "
               "wrong submissions."
               X"0A" X"0A"
               "Before setting done=true, you "
               "MUST have at least TWO "
               "independent pieces of evidence "
               "that converge on the answer. "
               "Either: (a) TWO records from "
               "different sources that agree on "
               "the relevant value, OR (b) ONE "
               "record whose "
               "discriminator/category/type "
               "field explicitly matches the "
               "question's domain. If you have "
               "only one candidate from one "
               "source with no discriminator "
               "confirmation, KEEP EXPLORING - "
               "the first match is rarely the "
               "right match."
               DELIMITED SIZE
               INTO WS-SYS-PROMPT
               WITH POINTER WS-SYS-PTR
           END-STRING

           STRING
               X"0A" X"0A"
               "=== MULTI-COLUMN JOIN ==="
               X"0A"
               "A source row often contains MORE "
               "than one integer/ID column beyond "
               "the primary key. Each numeric "
               "column may be a foreign key into "
               "a DIFFERENT field of the "
               "destination file. When you find "
               "the source row:" X"0A"
               "- INVENTORY every numeric column "
               "in it (not just the obvious "
               "'location_id' or 'id'). A row "
               "like <date>;<text>;<int_A>;"
               "<int_B> has TWO candidate join "
               "keys." X"0A"
               "- Try grep'ing the destination "
               "file for EACH numeric value, "
               "anchored with quotes around the "
               "value, to see which gives a "
               "UNIQUE record." X"0A"
               "- The CORRECT join is usually "
               "the one that returns a UNIQUE "
               "destination record. An over-broad "
               "join (many candidates per key) "
               "is the WRONG one - picking the "
               "first candidate from a non-unique "
               "join is the #1 cause of "
               "wrong-cell submissions." X"0A"
               "- If neither numeric column gives "
               "a unique match, fall back to "
               "disambiguating with the source "
               "row's description text against a "
               "category/type field on the "
               "destination side."
               X"0A" X"0A"
               "PIPELINE PATTERN (abstract "
               "placeholders - substitute real "
               "field names from YOUR data):"
               X"0A"
               "If you have N candidate records "
               "sharing the same key K but you "
               "need ONE specific record matching "
               "a category/type field, use TWO "
               "greps in a pipeline. First grep "
               "narrows to records of category "
               "Y; second filters by id X. The "
               "output is the single record "
               "matching BOTH constraints, with "
               "full lat/lon/etc fields visible. "
               "Without this pattern, picking "
               "ONE record from N candidates is "
               "unreliable."
               DELIMITED SIZE
               INTO WS-SYS-PROMPT
               WITH POINTER WS-SYS-PTR
           END-STRING

           STRING
               X"0A" X"0A"
               "=== TYPE-MATCHING PRINCIPLE ==="
               X"0A"
               "If a record references foreign "
               "keys or multiple candidates exist "
               "per key, do NOT assume the first "
               "candidate is correct. "
               "Cross-reference "
               "descriptive/category fields in "
               "the data with any clues in the "
               "matching record's own text to "
               "disambiguate. If the source row "
               "described a specific kind of "
               "place (cave, park, museum, "
               "etc.) and the destination record "
               "you picked has a CATEGORY/TYPE "
               "field showing something "
               "different, your lookup is wrong "
               "- go back."
               DELIMITED SIZE
               INTO WS-SYS-PROMPT
               WITH POINTER WS-SYS-PTR
           END-STRING

           STRING
               X"0A" X"0A"
               "=== CITY NAME DERIVATION ==="
               X"0A"
               "WARNING: locations.json may "
               "contain INTENTIONALLY WRONG "
               "city names for some "
               "location_ids. The coordinates "
               "in gps.json are the ground "
               "truth. If locations.json says "
               "location_id X is CityA but "
               "the coordinates clearly fall "
               "in a DIFFERENT country or "
               "region than CityA, then "
               "locations.json is LYING - use "
               "your geographic knowledge to "
               "determine the REAL city from "
               "the coordinates."
               X"0A"
               "For Polish coordinates "
               "(latitude 49-55, longitude "
               "14-24), common cities: "
               "Grudziadz (~53.4N, ~19.0E), "
               "Gdansk (~54.4N, ~18.6E), "
               "Krakow (~50.1N, ~20.0E), "
               "Warszawa (~52.2N, ~21.0E), "
               "Poznan (~52.4N, ~16.9E), "
               "Wroclaw (~51.1N, ~17.0E)."
               X"0A"
               "Use ASCII-only city names "
               "(no diacritics like special "
               "Polish characters) - submit "
               "Grudziadz not Grudziądz."
               DELIMITED SIZE
               INTO WS-SYS-PROMPT
               WITH POINTER WS-SYS-PTR
           END-STRING

           STRING
               X"0A" X"0A"
               "=== COORDINATE ACCURACY ==="
               X"0A"
               "NEVER estimate or guess "
               "coordinates from your own "
               "geographic knowledge. ALWAYS "
               "copy latitude and longitude "
               "values VERBATIM from gps.json "
               "output. If you found entry_id "
               "N in gps.json, you MUST grep "
               "for that entry_id and extract "
               "the EXACT numeric values shown "
               "in the JSON. Your world "
               "knowledge of city coordinates "
               "is WRONG for this dataset "
               "because locations are "
               "intentionally shifted. The "
               "ONLY source of truth for "
               "coordinates is gps.json."
               DELIMITED SIZE
               INTO WS-SYS-PROMPT
               WITH POINTER WS-SYS-PTR
           END-STRING

           STRING
               X"0A" X"0A"
               "=== MANDATORY VERIFICATION "
               "(before done=true) ==="
               X"0A"
               "You MUST pass ALL of these "
               "checks before setting done=true. "
               "If ANY check fails - even one - "
               "do NOT submit. Iterate again. "
               "False confidence wastes more "
               "iterations than re-checking."
               X"0A" X"0A"
               "1. CONSISTENCY: every value in "
               "your final_answer must be "
               "consistent with EVERY clue you "
               "observed. If the source row "
               "described a specific kind of "
               "place and the destination record "
               "you picked has a category/type "
               "field showing something "
               "different, your lookup is wrong "
               "- go back."
               X"0A" X"0A"
               "2. PLAUSIBILITY: the values must "
               "be plausible given what the "
               "brief and the data tell you. If "
               "you identified a specific city "
               "by name, recall its approximate "
               "geographic region from general "
               "knowledge and check the lat/lon "
               "falls in that region. "
               "Coordinates that put a European "
               "city in South America are wrong."
               X"0A" X"0A"
               "3. JOIN INTEGRITY: if you "
               "cross-referenced records across "
               "files, your join key MUST give a "
               "UNIQUE destination record. If "
               "your join returned multiple "
               "candidates and you picked one, "
               "your join is too broad - go back "
               "and try a different/additional "
               "key. Re-confirm by reading FULL "
               "context on both ends with grep "
               "-B 6 -A 6."
               X"0A" X"0A"
               "4. TRACE: in the reason field of "
               "your done=true turn, list each "
               "check explicitly and quote the "
               "evidence: event match (brief "
               "asks verb X, matched row "
               "contains X); subject match "
               "(matched row contains subject "
               "name); join (key K=V returned a "
               "UNIQUE record in dest file); "
               "consistency (dest category "
               "aligns with source description); "
               "plausibility (values reasonable "
               "given city/region/context)."
               X"0A"
               "If you cannot articulate ALL of "
               "these, you do NOT yet have "
               "enough confidence - keep "
               "exploring."
               DELIMITED SIZE
               INTO WS-SYS-PROMPT
               WITH POINTER WS-SYS-PTR
           END-STRING

           STRING
               X"0A" X"0A"
               "=== EXAMPLES - common mistakes "
               "and the right way ==="
               X"0A" X"0A"
               "EXAMPLE 1 - Anchoring a numeric "
               "grep against substring false "
               "positives:" X"0A"
               "BAD:  grep '42' "
               "/data/some_file.json" X"0A"
               "      -> ALSO matches any "
               "unicode escape containing those "
               "digits. Many city names with "
               "non-ASCII characters are stored "
               "as escape sequences in JSON "
               "(e.g. 'Foo" WS-BS WS-BS
               "u0042bar'); a bare numeric grep "
               "matches inside them. Picking the "
               "wrong match locks you onto a "
               "totally wrong record." X"0A"
               "GOOD: grep '" WS-BS WS-QT
               "key_name" WS-BS WS-QT ": *42' "
               "/data/some_file.json" X"0A"
               "      -> only matches the real "
               "key_name field. Substring "
               "collisions impossible."
               X"0A" X"0A"
               "EXAMPLE 2 - Reading nested JSON "
               "with full window:" X"0A"
               "BAD:  grep -A 3 '"
               WS-BS WS-QT "some_id"
               WS-BS WS-QT ": 42' file.json"
               X"0A"
               "      -> returns the NEXT "
               "object's fields if your target's "
               "other fields are ABOVE the "
               "matched line in the JSON layout."
               X"0A"
               "GOOD: grep -B 5 -A 5 '"
               WS-BS WS-QT "some_id"
               WS-BS WS-QT ": 42' file.json"
               X"0A"
               "      -> captures fields on BOTH "
               "sides of the matched line."
               X"0A" X"0A"
               "EXAMPLE 3 - Filtering by "
               "category AND key when N "
               "candidates share a key:" X"0A"
               "BAD:  grep '" WS-BS WS-QT
               "some_key" WS-BS WS-QT ": 42' "
               "file.json" X"0A"
               "      -> returns N records (one "
               "per category). Picking the first "
               "is unreliable." X"0A"
               "GOOD: grep -B 3 -A 3 '"
               WS-BS WS-QT "category_field"
               WS-BS WS-QT ": "
               WS-BS WS-QT "WANTED"
               WS-BS WS-QT "' file.json | grep "
               "'" WS-BS WS-QT "some_key"
               WS-BS WS-QT ": 42' -B 3 -A 3"
               X"0A"
               "      -> narrows to records of "
               "category WANTED first, then "
               "filters by some_key. Returns the "
               "unique record matching BOTH "
               "constraints."
               X"0A" X"0A"
               "These examples use placeholder "
               "field names - substitute the "
               "real ones from YOUR data. The "
               "SHAPE of the pattern is what "
               "matters."
               DELIMITED SIZE
               INTO WS-SYS-PROMPT
               WITH POINTER WS-SYS-PTR
           END-STRING

           STRING
               X"0A" X"0A"
               "=== DISCIPLINE ==="
               X"0A"
               "- Start with ls -la /data then "
               "head -c 4000 each candidate file "
               "to LEARN its layout before you "
               "grep." X"0A"
               "- Use grep -B 6 -A 6 when "
               "reading a matched record in "
               "JSON / CSV / logs; record "
               "boundaries can lie above or "
               "below the match line." X"0A"
               "- Anchor numeric grep searches "
               "with the surrounding key to "
               "avoid substring false positives "
               "(grep '" WS-BS WS-QT "id"
               WS-BS WS-QT ": *42' not "
               "grep '42')." X"0A"
               "- If the brief specifies an "
               "EVENT verb (found, discovered, "
               "killed), grep for the verb "
               "first, then verify the subject "
               "name is on the same row. Do "
               "not stop at the first hit that "
               "merely mentions the name." X"0A"
               "- Before done=true you must "
               "have TWO independent pieces of "
               "evidence that converge on the "
               "SAME record: either two sources "
               "agree, or one source has an "
               "explicit category/type field "
               "matching the brief."
               DELIMITED SIZE
               INTO WS-SYS-PROMPT
               WITH POINTER WS-SYS-PTR
           END-STRING

           STRING
               X"0A" X"0A"
               "=== HANDLING REJECTIONS ==="
               X"0A"
               "If centrala returns code -840 "
               "(or any -8xx error whose message "
               "starts with 'Invalid value in "
               "field ...'), it means your "
               "candidate entry is SEMANTICALLY "
               "wrong for that field. The JSON "
               "format, escaping, and date "
               "layout (YYYY-MM-DD) are all "
               "accepted - the VALUE is rejected."
               X"0A"
               "Do NOT resubmit the same payload "
               "with cosmetic tweaks (reformatting "
               "the date, re-escaping quotes, "
               "swapping ascii for unicode, etc). "
               "That will just get rejected again."
               X"0A"
               "Instead: treat the rejection as "
               "hard evidence that this entry is "
               "the wrong one. Go back to /data, "
               "re-read the log with a DIFFERENT "
               "search strategy, pick a DIFFERENT "
               "entry_id, and reason from scratch."
               X"0A"
               "The loop-guard will short-circuit "
               "byte-exact resubmissions with a "
               "synthetic error, so you MUST vary "
               "the candidate - not the cosmetics."
               X"0A"
               "HARD RULE: if the SAME entry_id "
               "(or the same candidate value) has "
               "been rejected 3 or more times - "
               "even across cosmetic variations - "
               "COMPLETELY ABANDON that entry_id. "
               "Do NOT return to it under any "
               "circumstance, not even later in "
               "the run. Instead, go back to "
               "/data and search for OTHER "
               "candidates using a DIFFERENT "
               "search term or a different source "
               "file. The correct answer is "
               "somewhere else - keep exploring."
               DELIMITED SIZE
               INTO WS-SYS-PROMPT
               WITH POINTER WS-SYS-PTR
           END-STRING

           STRING
               X"0A" X"0A"
               "=== YOU MUST SUBMIT AN ANSWER ==="
               X"0A"
               "You have a hard iteration budget "
               "(it is surfaced to you in every "
               "SERVER message as "
               "[iter N of MAX, answers_submitted="
               "M]). By iteration ~2/3 of the "
               "budget at the LATEST you MUST "
               "have submitted at least one "
               "answer attempt (done=true with "
               "the printf payload)."
               X"0A"
               "A WRONG answer is RECOVERABLE - "
               "centrala replies with the field "
               "that is off and you can try "
               "again. Running out of iterations "
               "without EVER submitting is a "
               "guaranteed failure."
               X"0A"
               "If answers_submitted is still 0 "
               "and you are past mid-budget, "
               "STOP exploring and submit your "
               "current best guess immediately - "
               "even a partially-informed guess "
               "beats never answering."
               X"0A" X"0A"
               "=== INTERPRETATION FLEXIBILITY ==="
               X"0A"
               "If your literal search terms "
               "return zero matches across "
               "MULTIPLE files and multiple "
               "attempts, your interpretation of "
               "the task is probably too literal."
               X"0A"
               "Step back and reconsider: the "
               "brief may use figurative phrasing, "
               "metaphors, synonyms, or refer to "
               "events described indirectly "
               "(e.g. a verb like 'found' might "
               "surface as 'discovered', "
               "'located', 'odkryty', 'widziany', "
               "etc., or the event may be "
               "described without any verb at all)."
               X"0A"
               "Before grepping yet another "
               "variant of the same term, try a "
               "COMPLETELY different keyword, a "
               "different person, or a different "
               "event class. Re-read the brief "
               "and challenge your assumptions "
               "about what you are looking for."
               DELIMITED SIZE
               INTO WS-SYS-PROMPT
               WITH POINTER WS-SYS-PTR
           END-STRING

           STRING
               X"0A" X"0A"
               "=== OUTPUT PROTOCOL ==="
               X"0A"
               "Every reply MUST be ONE JSON "
               "object, no markdown fences, no "
               "prose around it:" X"0A"
               "{" WS-BS WS-QT "reason"
               WS-BS WS-QT ":" WS-BS WS-QT
               "THOUGHT: <<=60 words, quote "
               "prior output then decide>"
               WS-BS WS-QT "," WS-BS WS-QT
               "cmd" WS-BS WS-QT ":"
               WS-BS WS-QT "<next shell cmd OR "
               "the final printf>" WS-BS WS-QT
               "," WS-BS WS-QT "done" WS-BS
               WS-QT ":<true|false>,"
               WS-BS WS-QT "final_answer"
               WS-BS WS-QT ":<null OR the "
               "brief JSON object>}" X"0A"
               "The reason MUST start with "
               WS-BS WS-QT "THOUGHT: "
               WS-BS WS-QT " - replies that "
               "do not are treated as malformed."
               X"0A"
               "When done=true: final_answer "
               "MUST be a fully-valid object "
               "matching the brief AND cmd "
               "MUST be a printf that prints "
               "exactly that JSON to stdout."
               X"0A"
               "No tool calls, no functions - "
               "only the JSON object."
               DELIMITED SIZE
               INTO WS-SYS-PROMPT
               WITH POINTER WS-SYS-PTR
           END-STRING
           .

      *> ============================================================
      *> CALL-LLM
      *> Build /v1/chat/completions request with system prompt +
      *> accumulated conversation, call OpenAI, store assistant
      *> content in WS-PL-RAW.
      *> ============================================================
       CALL-LLM.
           MOVE SPACES TO WS-PL-RAW

      *>   Escape the system prompt for JSON
           MOVE SPACES TO WS-ESC-IN
           MOVE WS-SYS-PROMPT(1:WS-SYS-PTR - 1)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

      *>   Start request JSON
           MOVE SPACES TO WS-REQ-JSON
           MOVE 1 TO WS-PTR

           STRING
               "{"
               WS-QT "model" WS-QT ":"
               WS-QT "gpt-4.1-mini" WS-QT ","
               WS-QT "temperature" WS-QT
               ":0.0,"
               WS-QT "max_tokens" WS-QT
               ":800,"
               WS-QT "messages" WS-QT ":["
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   System message
           STRING
               "{" WS-QT "role" WS-QT ":"
               WS-QT "system" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Copy escaped system prompt char-by-char in chunks
           IF WS-ESC-OLEN > 0
               MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
                   TO WS-REQ-JSON(
                   WS-PTR:WS-ESC-OLEN)
               ADD WS-ESC-OLEN TO WS-PTR
           END-IF

           STRING
               WS-QT "},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Append the running conversation buffer (already a
      *>   sequence of {role,content} objects separated by commas)
           COMPUTE WS-K = WS-CONV-PTR - 1
           IF WS-K > 0
               MOVE WS-CONV-BUF(1:WS-K)
                   TO WS-REQ-JSON(WS-PTR:WS-K)
               ADD WS-K TO WS-PTR
           END-IF

           STRING
               "]}"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Write request as a single record and curl it.
           PERFORM WRITE-LLM-REQ-SINGLE
           PERFORM SEND-LLM-REQUEST
           .

      *> ============================================================
      *> WRITE-LLM-REQ-SINGLE
      *> Dump WS-REQ-JSON(1:WS-PTR-1) as ONE record to llm_req.json
      *> so curl -d @file sees a valid single-line JSON payload.
      *> ============================================================
       WRITE-LLM-REQ-SINGLE.
           MOVE "llm_req.json" TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "    LLM req single open err " WS-FS
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
      *> curl the request file to OpenAI, parse content into
      *> WS-PL-RAW.
      *> ============================================================
       SEND-LLM-REQUEST.
           INITIALIZE WS-CMD
           STRING
               "rm -f llm_resp.json"
               DELIMITED SIZE INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

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

           MOVE "llm_resp.json" TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           IF WS-JLEN = 0
               DISPLAY "    Empty LLM resp!"
               EXIT PARAGRAPH
           END-IF

           DISPLAY "  [LLM raw] " WS-JBUF(1:300)

      *>   Locate "message" then the following "content"
           MOVE "message" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL

           MOVE "content" TO WS-KEY-SEARCH
           PERFORM FIND-JSON-VAL

           IF WS-JVAL NOT = SPACES
               MOVE SPACES TO WS-ESC-IN
               MOVE WS-JVAL TO WS-ESC-IN
               PERFORM JSON-UNESCAPE-STR
               MOVE SPACES TO WS-PL-RAW
               IF WS-ESC-OLEN > 0
                   MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
                       TO WS-PL-RAW
               END-IF
           END-IF
           .

      *> ============================================================
      *> PARSE-PLANNER-DECISION
      *> Extract reason / cmd / done fields out of WS-PL-RAW into
      *> WS-PL-REASON / WS-PL-CMD / WS-PL-DONE by re-using the JSON
      *> parser against a scratch JBUF. final_answer is detected
      *> implicitly by the printf inside cmd - we don't need its
      *> value, only the shell command.
      *> ============================================================
       PARSE-PLANNER-DECISION.
           MOVE SPACES TO WS-PL-REASON
           MOVE SPACES TO WS-PL-CMD
           MOVE SPACES TO WS-PL-DONE

      *>   Stage raw into WS-JBUF for FIND-JSON-VAL, stripping
      *>   any leading/trailing markdown fences.
           MOVE SPACES TO WS-JBUF
           MOVE 0 TO WS-JLEN
           MOVE FUNCTION TRIM(WS-PL-RAW) TO WS-JBUF
           MOVE LENGTH(FUNCTION TRIM(WS-PL-RAW))
               TO WS-JLEN

           IF WS-JLEN >= 7
           AND WS-JBUF(1:7) = "```json"
               MOVE SPACES TO WS-SCRATCH
               MOVE WS-JBUF(8:WS-JLEN - 7)
                   TO WS-SCRATCH
               MOVE SPACES TO WS-JBUF(1:WS-JLEN)
               MOVE WS-SCRATCH(1:WS-JLEN - 7)
                   TO WS-JBUF(1:WS-JLEN - 7)
               SUBTRACT 7 FROM WS-JLEN
           ELSE
               IF WS-JLEN >= 3
               AND WS-JBUF(1:3) = "```"
                   MOVE SPACES TO WS-SCRATCH
                   MOVE WS-JBUF(4:WS-JLEN - 3)
                       TO WS-SCRATCH
                   MOVE SPACES TO WS-JBUF(1:WS-JLEN)
                   MOVE WS-SCRATCH(1:WS-JLEN - 3)
                       TO WS-JBUF(1:WS-JLEN - 3)
                   SUBTRACT 3 FROM WS-JLEN
               END-IF
           END-IF

           IF WS-JLEN >= 3
           AND WS-JBUF(WS-JLEN - 2:3) = "```"
               MOVE SPACES TO WS-JBUF(WS-JLEN - 2:3)
               SUBTRACT 3 FROM WS-JLEN
           END-IF

           IF WS-JLEN = 0
               EXIT PARAGRAPH
           END-IF

           MOVE "reason" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE SPACES TO WS-ESC-IN
           MOVE WS-JVAL TO WS-ESC-IN
           PERFORM JSON-UNESCAPE-STR
           IF WS-ESC-OLEN > 0
               MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
                   TO WS-PL-REASON
           END-IF

           MOVE "cmd" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE SPACES TO WS-ESC-IN
           MOVE WS-JVAL TO WS-ESC-IN
           PERFORM JSON-UNESCAPE-STR
           IF WS-ESC-OLEN > 0
               MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
                   TO WS-PL-CMD
           END-IF

           MOVE "done" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE WS-JVAL TO WS-PL-DONE
           .

      *> ============================================================
      *> APPEND-ASST-MSG
      *> Append the raw planner JSON (as assistant content) to
      *> WS-CONV-BUF so the next LLM call sees its prior decision.
      *> ============================================================
       APPEND-ASST-MSG.
           MOVE SPACES TO WS-ESC-IN
           MOVE WS-PL-RAW TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

           STRING
               ","
               "{" WS-QT "role" WS-QT ":"
               WS-QT "assistant" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           IF WS-ESC-OLEN > 0
               MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
                   TO WS-CONV-BUF(
                   WS-CONV-PTR:WS-ESC-OLEN)
               ADD WS-ESC-OLEN TO WS-CONV-PTR
           END-IF

           STRING
               WS-QT "}"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING
           .

      *> ============================================================
      *> APPEND-SERVER-MSG
      *> Append the server response as a user message so the LLM
      *> can see what the sandbox returned.
      *> ============================================================
       APPEND-SERVER-MSG.
      *>   Iter / answer counters for inline pressure prefix
           MOVE WS-ITER TO WS-ITER-DSP
           MOVE WS-ANS-COUNT TO WS-ANS-DSP

      *>   Render + escape the WORKING MEMORY block FIRST. This
      *>   internally PERFORMs JSON-ESCAPE-STR which clobbers
      *>   WS-ESC-OUT, so it MUST run before the server-text escape
      *>   below or the server bytes get overwritten and the LLM
      *>   sees three WM blocks per turn and zero hub output.
           PERFORM WM-RENDER-BLOCK

      *>   Use WS-SRV-SNIPPET built by EXTRACT-SERVER-SNIPPET.
      *>   This is the LAST writer of WS-ESC-OUT in this paragraph,
      *>   so the escaped server text survives until line ~1532.
           MOVE SPACES TO WS-ESC-IN
           IF WS-SRV-LEN > 0
               MOVE WS-SRV-SNIPPET(1:WS-SRV-LEN)
                   TO WS-ESC-IN
           ELSE
               MOVE "(empty server response)"
                   TO WS-ESC-IN
           END-IF
           PERFORM JSON-ESCAPE-STR

           STRING
               ","
               "{" WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               "[iter " WS-ITER-DSP " of "
               WS-MAX-ITER
               ", answers_submitted="
               WS-ANS-DSP "] "
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

      *>   TOP slice of the sandwich: WORKING MEMORY block.
           IF WM-RENDER-ESC-LEN > 0
               MOVE WM-RENDER-ESC(1:WM-RENDER-ESC-LEN)
                   TO WS-CONV-BUF(
                   WS-CONV-PTR:WM-RENDER-ESC-LEN)
               ADD WM-RENDER-ESC-LEN TO WS-CONV-PTR
           END-IF

           STRING
               "SERVER: "
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           IF WS-ESC-OLEN > 0
               MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
                   TO WS-CONV-BUF(
                   WS-CONV-PTR:WS-ESC-OLEN)
               ADD WS-ESC-OLEN TO WS-CONV-PTR
           END-IF

      *>   BOTTOM slice of the sandwich: WORKING MEMORY block
      *>   repeated, exploiting gpt-4.1's U-shaped attention so
      *>   the facts anchor at both ends of the user turn.
           IF WM-RENDER-ESC-LEN > 0
               MOVE WM-RENDER-ESC(1:WM-RENDER-ESC-LEN)
                   TO WS-CONV-BUF(
                   WS-CONV-PTR:WM-RENDER-ESC-LEN)
               ADD WM-RENDER-ESC-LEN TO WS-CONV-PTR
           END-IF

           STRING
               WS-QT "}"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING
           .

      *> ============================================================
      *> EXTRACT-SERVER-SNIPPET
      *> Pull the hub response JSON (currently in WS-JBUF) into
      *> WS-SRV-SNIPPET trimmed to ~5000 chars so we do not blow
      *> up the conversation buffer.
      *> ============================================================
       EXTRACT-SERVER-SNIPPET.
           MOVE SPACES TO WS-SRV-SNIPPET
           MOVE 0 TO WS-SRV-LEN
           IF WS-JLEN = 0
               EXIT PARAGRAPH
           END-IF
           IF WS-JLEN > 5000
               MOVE 5000 TO WS-SRV-LEN
           ELSE
               MOVE WS-JLEN TO WS-SRV-LEN
           END-IF
           MOVE WS-JBUF(1:WS-SRV-LEN)
               TO WS-SRV-SNIPPET(1:WS-SRV-LEN)
           .

      *> ============================================================
      *> WM-EXTRACT
      *> Scan the current server-snippet (WS-SRV-SNIPPET) one "line"
      *> at a time and feed each line through the pattern extractors.
      *> Each extractor is additive: if its buffer still has room and
      *> the line matches the pattern, the line is appended. All
      *> buffers are capped; overflow is silently ignored so earlier
      *> facts are never overwritten.
      *>
      *> Extractors:
      *>   1. File names in /data  (WM-FILES-BUF)    <- "ls" output
      *>   2. CSV schema (first header)              (WM-SCHEMA-BUF)
      *>   3. YYYY-MM-DD;text;int;int rows           (WM-EVENTS-BUF)
      *>   4. location_id + name pairs               (WM-LOCID-BUF)
      *>   5. lat/lon/type/location_id/entry_id recs (WM-COORDS-BUF)
      *> ============================================================
       WM-EXTRACT.
           IF WS-SRV-LEN = 0
               EXIT PARAGRAPH
           END-IF

      *>   Server output travels from centrala as a JSON-escaped
      *>   blob where line breaks are the two literal chars "\" "n".
      *>   Convert those to real X"0A" so the line-based scanner
      *>   below sees actual records. Other escapes are left alone
      *>   (they do not affect pattern matching).
           PERFORM WM-UNESCAPE-SNIPPET

           IF WM-UNESC-LEN = 0
               EXIT PARAGRAPH
           END-IF

           MOVE 1 TO WM-SCAN-POS
           MOVE WM-UNESC-LEN TO WM-SCAN-END

           PERFORM UNTIL WM-SCAN-POS > WM-SCAN-END
      *>       Find end of this line (next X"0A" or buffer end)
               MOVE WM-SCAN-POS TO WM-LINE-START
               MOVE WM-LINE-START TO WM-LINE-END
               PERFORM UNTIL WM-LINE-END > WM-SCAN-END
                          OR WM-UNESC(WM-LINE-END:1)
                              = X"0A"
                   ADD 1 TO WM-LINE-END
               END-PERFORM

               COMPUTE WM-LINE-LEN =
                   WM-LINE-END - WM-LINE-START
               IF WM-LINE-LEN > 2000
                   MOVE 2000 TO WM-LINE-LEN
               END-IF

               MOVE SPACES TO WM-LINE
               IF WM-LINE-LEN > 0
                   MOVE WM-UNESC(
                       WM-LINE-START:WM-LINE-LEN)
                       TO WM-LINE(1:WM-LINE-LEN)
                   PERFORM WM-SCAN-LINE
               END-IF

      *>       Advance past the newline
               COMPUTE WM-SCAN-POS = WM-LINE-END + 1
           END-PERFORM
           .

      *> ============================================================
      *> WM-UNESCAPE-SNIPPET
      *> Copy WS-SRV-SNIPPET(1:WS-SRV-LEN) into WM-UNESC, converting
      *> the two-char sequence "\n" (literal backslash + n) into a
      *> single X"0A" so the line scanner in WM-EXTRACT sees real
      *> records. Other backslash escapes are copied literally.
      *> ============================================================
       WM-UNESCAPE-SNIPPET.
           MOVE SPACES TO WM-UNESC
           MOVE 0 TO WM-UNESC-LEN
           MOVE 1 TO WM-UNESC-SRC
           MOVE 1 TO WM-UNESC-DST
           PERFORM UNTIL WM-UNESC-SRC > WS-SRV-LEN
                      OR WM-UNESC-DST > 6000
               IF WM-UNESC-SRC < WS-SRV-LEN
               AND WS-SRV-SNIPPET(WM-UNESC-SRC:1) = WS-BS
               AND WS-SRV-SNIPPET(WM-UNESC-SRC + 1:1) = "n"
                   MOVE X"0A"
                     TO WM-UNESC(WM-UNESC-DST:1)
                   ADD 2 TO WM-UNESC-SRC
                   ADD 1 TO WM-UNESC-DST
               ELSE
                   IF WM-UNESC-SRC < WS-SRV-LEN
                   AND WS-SRV-SNIPPET(WM-UNESC-SRC:1) = WS-BS
                   AND WS-SRV-SNIPPET(WM-UNESC-SRC + 1:1) = "r"
      *>               Drop literal \r  -> nothing
                       ADD 2 TO WM-UNESC-SRC
                   ELSE
                       IF WM-UNESC-SRC < WS-SRV-LEN
                       AND WS-SRV-SNIPPET(WM-UNESC-SRC:1) = WS-BS
                       AND WS-SRV-SNIPPET(WM-UNESC-SRC + 1:1)
                           = WS-QT
      *>                   \" -> "
                           MOVE WS-QT
                             TO WM-UNESC(WM-UNESC-DST:1)
                           ADD 2 TO WM-UNESC-SRC
                           ADD 1 TO WM-UNESC-DST
                       ELSE
                           MOVE WS-SRV-SNIPPET(WM-UNESC-SRC:1)
                             TO WM-UNESC(WM-UNESC-DST:1)
                           ADD 1 TO WM-UNESC-SRC
                           ADD 1 TO WM-UNESC-DST
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           COMPUTE WM-UNESC-LEN = WM-UNESC-DST - 1
           .

      *> ============================================================
      *> WM-SCAN-LINE
      *> Apply all 5 extractors to the line currently in WM-LINE
      *> (length WM-LINE-LEN). Uses INSPECT TALLYING as the COBOL
      *> substring-search primitive; order matters only insofar as
      *> each extractor gates on its own buffer cap.
      *> ============================================================
       WM-SCAN-LINE.
      *>   Strip leading spaces so pattern checks work on the first
      *>   significant char regardless of JSON pretty-print indent.
           PERFORM UNTIL WM-LINE-LEN = 0
                      OR WM-LINE(1:1) NOT = " "
               MOVE WM-LINE(2:WM-LINE-LEN - 1)
                 TO WM-LINE(1:WM-LINE-LEN - 1)
               MOVE SPACE TO WM-LINE(WM-LINE-LEN:1)
               SUBTRACT 1 FROM WM-LINE-LEN
           END-PERFORM

           IF WM-LINE-LEN = 0
               EXIT PARAGRAPH
           END-IF

      *>   Reject obvious raw-JSON wrapper lines. These are the
      *>   outer {"code":..., "message":..., "output":"..."} envelope
      *>   that the hub wraps every sandbox reply in. Any line that
      *>   contains '"code"' or '"message"' as a JSON key is not
      *>   data - it's the envelope. Skip the entire line.
           MOVE 0 TO WM-TALLY
           INSPECT WM-LINE(1:WM-LINE-LEN)
               TALLYING WM-TALLY FOR ALL WM-PAT-CODE
           IF WM-TALLY > 0
               EXIT PARAGRAPH
           END-IF
           MOVE 0 TO WM-TALLY
           INSPECT WM-LINE(1:WM-LINE-LEN)
               TALLYING WM-TALLY FOR ALL WM-PAT-MSG
           IF WM-TALLY > 0
               EXIT PARAGRAPH
           END-IF

      *>   --- Extractor 1: files in /data ---
      *>   Any line mentioning "/data/" or ending in .csv/.json/.md
      *>   is assumed to be a filename the sandbox revealed.
           IF WM-FILES-LEN < 350
               MOVE 0 TO WM-TALLY
               INSPECT WM-LINE(1:WM-LINE-LEN)
                   TALLYING WM-TALLY FOR ALL "/data/"
               IF WM-TALLY > 0
                   PERFORM WM-APPEND-FILE-LINE
               END-IF
           END-IF

      *>   --- Extractor 2: CSV schema line ---
      *>   A line with 2+ ";" separators, NO digits at position 1,
      *>   and not starting with a JSON brace or bracket. Only
      *>   stored while WM-SCHEMA is empty (schemas don't change).
           IF WM-SCHEMA-LEN = 0
           AND WM-LINE(1:1) NOT = "{"
           AND WM-LINE(1:1) NOT = "["
           AND WM-LINE(1:1) NOT = WS-QT
               MOVE 0 TO WM-TALLY
               INSPECT WM-LINE(1:WM-LINE-LEN)
                   TALLYING WM-TALLY FOR ALL ";"
               IF WM-TALLY >= 2
                   IF WM-LINE(1:1) < "0"
                    OR WM-LINE(1:1) > "9"
                       PERFORM WM-APPEND-SCHEMA-LINE
                   END-IF
               END-IF
           END-IF

      *>   --- Extractor 3: YYYY-MM-DD; events ---
           IF WM-EVENTS-LEN < 540
               IF WM-LINE-LEN >= 10
               AND WM-LINE(1:1) >= "0"
               AND WM-LINE(1:1) <= "9"
               AND WM-LINE(5:1) = "-"
               AND WM-LINE(8:1) = "-"
                   PERFORM WM-APPEND-EVENT-LINE
               END-IF
           END-IF

      *>   --- Extractor 4: location_id + name pair ---
      *>   Only match lines that look like a single JSON value pair,
      *>   not wrapper objects containing the literal key name.
           IF WM-LOCID-LEN < 350
           AND WM-LINE(1:1) = WS-QT
               MOVE 0 TO WM-TALLY
               INSPECT WM-LINE(1:WM-LINE-LEN)
                   TALLYING WM-TALLY FOR ALL WM-PAT-LOCID
               IF WM-TALLY > 0
                   PERFORM WM-APPEND-LOCID-LINE
               END-IF
               MOVE 0 TO WM-TALLY
               INSPECT WM-LINE(1:WM-LINE-LEN)
                   TALLYING WM-TALLY FOR ALL WM-PAT-NAME
               IF WM-TALLY > 0
                   PERFORM WM-APPEND-LOCID-LINE
               END-IF
           END-IF

      *>   --- Extractor 5: coord records with type+id fields ---
      *>   Match single-field lines "latitude": N.NNN etc.
           IF WM-COORDS-LEN < 540
           AND WM-LINE(1:1) = WS-QT
               MOVE 0 TO WM-TALLY
               INSPECT WM-LINE(1:WM-LINE-LEN)
                   TALLYING WM-TALLY FOR ALL WM-PAT-LAT
               IF WM-TALLY > 0
                   PERFORM WM-APPEND-COORD-LINE
               END-IF
               IF WM-TALLY = 0
                   MOVE 0 TO WM-TALLY
                   INSPECT WM-LINE(1:WM-LINE-LEN)
                       TALLYING WM-TALLY FOR ALL WM-PAT-LON
                   IF WM-TALLY > 0
                       PERFORM WM-APPEND-COORD-LINE
                   END-IF
               END-IF
               IF WM-TALLY = 0
                   MOVE 0 TO WM-TALLY
                   INSPECT WM-LINE(1:WM-LINE-LEN)
                       TALLYING WM-TALLY FOR ALL WM-PAT-TYPE
                   IF WM-TALLY > 0
                       PERFORM WM-APPEND-COORD-LINE
                   END-IF
               END-IF
               IF WM-TALLY = 0
                   MOVE 0 TO WM-TALLY
                   INSPECT WM-LINE(1:WM-LINE-LEN)
                       TALLYING WM-TALLY FOR ALL WM-PAT-ENTRY
                   IF WM-TALLY > 0
                       PERFORM WM-APPEND-COORD-LINE
                   END-IF
               END-IF
           END-IF
           .

      *> ============================================================
      *> WM-APPEND-FILE-LINE
      *> Append WM-LINE to WM-FILES-BUF if not already there
      *> (de-dup via INSPECT substring search) and if buffer has room.
      *> Each entry is terminated by X"0A".
      *> ============================================================
       WM-APPEND-FILE-LINE.
           IF WM-LINE-LEN = 0
               EXIT PARAGRAPH
           END-IF
           IF WM-LINE-LEN > 200
               MOVE 200 TO WM-LINE-LEN
           END-IF
      *>   De-dup: substring search in the existing buffer
           MOVE "N" TO WM-DUP-FLAG
           IF WM-FILES-LEN > 0
               MOVE 0 TO WM-TALLY
               INSPECT WM-FILES-BUF(1:WM-FILES-LEN)
                   TALLYING WM-TALLY
                   FOR ALL WM-LINE(1:WM-LINE-LEN)
               IF WM-TALLY > 0
                   MOVE "Y" TO WM-DUP-FLAG
               END-IF
           END-IF
           IF WM-DUP-FLAG = "N"
               IF WM-FILES-LEN + WM-LINE-LEN + 1 <= 400
                   MOVE WM-LINE(1:WM-LINE-LEN)
                     TO WM-FILES-BUF(
                     WM-FILES-LEN + 1:WM-LINE-LEN)
                   ADD WM-LINE-LEN TO WM-FILES-LEN
                   ADD 1 TO WM-FILES-LEN
                   MOVE X"0A"
                     TO WM-FILES-BUF(WM-FILES-LEN:1)
               END-IF
           END-IF
           .

      *> ============================================================
      *> WM-APPEND-SCHEMA-LINE
      *> Store the first CSV-header-looking line we encounter.
      *> ============================================================
       WM-APPEND-SCHEMA-LINE.
           IF WM-LINE-LEN = 0
               EXIT PARAGRAPH
           END-IF
           IF WM-LINE-LEN > 300
               MOVE 300 TO WM-LINE-LEN
           END-IF
           IF WM-SCHEMA-LEN = 0
               MOVE WM-LINE(1:WM-LINE-LEN)
                 TO WM-SCHEMA-BUF(1:WM-LINE-LEN)
               MOVE WM-LINE-LEN TO WM-SCHEMA-LEN
           END-IF
           .

      *> ============================================================
      *> WM-APPEND-EVENT-LINE
      *> Append YYYY-MM-DD; candidate event row.
      *> ============================================================
       WM-APPEND-EVENT-LINE.
           IF WM-LINE-LEN = 0
               EXIT PARAGRAPH
           END-IF
           IF WM-LINE-LEN > 200
               MOVE 200 TO WM-LINE-LEN
           END-IF
           MOVE "N" TO WM-DUP-FLAG
           IF WM-EVENTS-LEN > 0
               MOVE 0 TO WM-TALLY
               INSPECT WM-EVENTS-BUF(1:WM-EVENTS-LEN)
                   TALLYING WM-TALLY
                   FOR ALL WM-LINE(1:WM-LINE-LEN)
               IF WM-TALLY > 0
                   MOVE "Y" TO WM-DUP-FLAG
               END-IF
           END-IF
           IF WM-DUP-FLAG = "N"
               IF WM-EVENTS-LEN + WM-LINE-LEN + 1 <= 600
                   MOVE WM-LINE(1:WM-LINE-LEN)
                     TO WM-EVENTS-BUF(
                     WM-EVENTS-LEN + 1:WM-LINE-LEN)
                   ADD WM-LINE-LEN TO WM-EVENTS-LEN
                   ADD 1 TO WM-EVENTS-LEN
                   MOVE X"0A"
                     TO WM-EVENTS-BUF(WM-EVENTS-LEN:1)
               END-IF
           END-IF
           .

      *> ============================================================
      *> WM-APPEND-LOCID-LINE
      *> Append a location_id + name snippet.
      *> ============================================================
       WM-APPEND-LOCID-LINE.
           IF WM-LINE-LEN = 0
               EXIT PARAGRAPH
           END-IF
           IF WM-LINE-LEN > 200
               MOVE 200 TO WM-LINE-LEN
           END-IF
           MOVE "N" TO WM-DUP-FLAG
           IF WM-LOCID-LEN > 0
               MOVE 0 TO WM-TALLY
               INSPECT WM-LOCID-BUF(1:WM-LOCID-LEN)
                   TALLYING WM-TALLY
                   FOR ALL WM-LINE(1:WM-LINE-LEN)
               IF WM-TALLY > 0
                   MOVE "Y" TO WM-DUP-FLAG
               END-IF
           END-IF
           IF WM-DUP-FLAG = "N"
               IF WM-LOCID-LEN + WM-LINE-LEN + 1 <= 400
                   MOVE WM-LINE(1:WM-LINE-LEN)
                     TO WM-LOCID-BUF(
                     WM-LOCID-LEN + 1:WM-LINE-LEN)
                   ADD WM-LINE-LEN TO WM-LOCID-LEN
                   ADD 1 TO WM-LOCID-LEN
                   MOVE X"0A"
                     TO WM-LOCID-BUF(WM-LOCID-LEN:1)
               END-IF
           END-IF
           .

      *> ============================================================
      *> WM-APPEND-COORD-LINE
      *> Append a lat/lon/type/location_id/entry_id fragment.
      *> ============================================================
       WM-APPEND-COORD-LINE.
           IF WM-LINE-LEN = 0
               EXIT PARAGRAPH
           END-IF
           IF WM-LINE-LEN > 250
               MOVE 250 TO WM-LINE-LEN
           END-IF
           MOVE "N" TO WM-DUP-FLAG
           IF WM-COORDS-LEN > 0
               MOVE 0 TO WM-TALLY
               INSPECT WM-COORDS-BUF(1:WM-COORDS-LEN)
                   TALLYING WM-TALLY
                   FOR ALL WM-LINE(1:WM-LINE-LEN)
               IF WM-TALLY > 0
                   MOVE "Y" TO WM-DUP-FLAG
               END-IF
           END-IF
           IF WM-DUP-FLAG = "N"
               IF WM-COORDS-LEN + WM-LINE-LEN + 1 <= 600
                   MOVE WM-LINE(1:WM-LINE-LEN)
                     TO WM-COORDS-BUF(
                     WM-COORDS-LEN + 1:WM-LINE-LEN)
                   ADD WM-LINE-LEN TO WM-COORDS-LEN
                   ADD 1 TO WM-COORDS-LEN
                   MOVE X"0A"
                     TO WM-COORDS-BUF(WM-COORDS-LEN:1)
               END-IF
           END-IF
           .

      *> ============================================================
      *> WM-RENDER-BLOCK
      *> Assemble WM-FILES-BUF / WM-SCHEMA-BUF / WM-EVENTS-BUF /
      *> WM-LOCID-BUF / WM-COORDS-BUF into a single plain-text block
      *> in WM-RENDER, cap to WM-CAP, then JSON-escape into
      *> WM-RENDER-ESC for direct embedding in conversation content.
      *> Called ONCE per server turn (see APPEND-SERVER-MSG) so top
      *> and bottom sandwich copies are identical.
      *> ============================================================
       WM-RENDER-BLOCK.
           MOVE SPACES TO WM-RENDER
           MOVE 0 TO WM-RENDER-LEN

      *>   If every extractor is empty, skip so we don't flood the
      *>   context with a useless "(nothing yet)" block on iter 1.
           IF WM-FILES-LEN = 0
           AND WM-SCHEMA-LEN = 0
           AND WM-EVENTS-LEN = 0
           AND WM-LOCID-LEN = 0
           AND WM-COORDS-LEN = 0
               MOVE SPACES TO WM-RENDER-ESC
               MOVE 0 TO WM-RENDER-ESC-LEN
               EXIT PARAGRAPH
           END-IF

           STRING
               X"0A"
               "=== WORKING MEMORY (facts you "
               "already verified - no need to "
               "re-derive) ===" X"0A"
               DELIMITED SIZE
               INTO WM-RENDER
           END-STRING
           MOVE LENGTH(
               FUNCTION TRIM(WM-RENDER TRAILING))
               TO WM-RENDER-LEN

           IF WM-FILES-LEN > 0
               MOVE "[files in /data]" TO WM-TMP
               MOVE 16 TO WM-TMP-LEN
               PERFORM WM-RENDER-APPEND-HEADER
               IF WM-RENDER-LEN + WM-FILES-LEN + 1
                   <= WM-CAP
                   MOVE WM-FILES-BUF(1:WM-FILES-LEN)
                     TO WM-RENDER(
                     WM-RENDER-LEN + 1:WM-FILES-LEN)
                   ADD WM-FILES-LEN TO WM-RENDER-LEN
                   ADD 1 TO WM-RENDER-LEN
                   MOVE X"0A"
                     TO WM-RENDER(WM-RENDER-LEN:1)
               END-IF
           END-IF

           IF WM-SCHEMA-LEN > 0
               MOVE "[csv schema]" TO WM-TMP
               MOVE 12 TO WM-TMP-LEN
               PERFORM WM-RENDER-APPEND-HEADER
               IF WM-RENDER-LEN + WM-SCHEMA-LEN + 1
                   <= WM-CAP
                   MOVE WM-SCHEMA-BUF(1:WM-SCHEMA-LEN)
                     TO WM-RENDER(
                     WM-RENDER-LEN + 1:WM-SCHEMA-LEN)
                   ADD WM-SCHEMA-LEN TO WM-RENDER-LEN
                   ADD 1 TO WM-RENDER-LEN
                   MOVE X"0A"
                     TO WM-RENDER(WM-RENDER-LEN:1)
               END-IF
           END-IF

           IF WM-EVENTS-LEN > 0
               MOVE "[candidate events]" TO WM-TMP
               MOVE 18 TO WM-TMP-LEN
               PERFORM WM-RENDER-APPEND-HEADER
               IF WM-RENDER-LEN + WM-EVENTS-LEN + 1
                   <= WM-CAP
                   MOVE WM-EVENTS-BUF(1:WM-EVENTS-LEN)
                     TO WM-RENDER(
                     WM-RENDER-LEN + 1:WM-EVENTS-LEN)
                   ADD WM-EVENTS-LEN TO WM-RENDER-LEN
                   ADD 1 TO WM-RENDER-LEN
                   MOVE X"0A"
                     TO WM-RENDER(WM-RENDER-LEN:1)
               END-IF
           END-IF

           IF WM-LOCID-LEN > 0
               MOVE "[location_id map]" TO WM-TMP
               MOVE 17 TO WM-TMP-LEN
               PERFORM WM-RENDER-APPEND-HEADER
               IF WM-RENDER-LEN + WM-LOCID-LEN + 1
                   <= WM-CAP
                   MOVE WM-LOCID-BUF(1:WM-LOCID-LEN)
                     TO WM-RENDER(
                     WM-RENDER-LEN + 1:WM-LOCID-LEN)
                   ADD WM-LOCID-LEN TO WM-RENDER-LEN
                   ADD 1 TO WM-RENDER-LEN
                   MOVE X"0A"
                     TO WM-RENDER(WM-RENDER-LEN:1)
               END-IF
           END-IF

           IF WM-COORDS-LEN > 0
               MOVE "[coord records]" TO WM-TMP
               MOVE 15 TO WM-TMP-LEN
               PERFORM WM-RENDER-APPEND-HEADER
               IF WM-RENDER-LEN + WM-COORDS-LEN + 1
                   <= WM-CAP
                   MOVE WM-COORDS-BUF(1:WM-COORDS-LEN)
                     TO WM-RENDER(
                     WM-RENDER-LEN + 1:WM-COORDS-LEN)
                   ADD WM-COORDS-LEN TO WM-RENDER-LEN
                   ADD 1 TO WM-RENDER-LEN
                   MOVE X"0A"
                     TO WM-RENDER(WM-RENDER-LEN:1)
               END-IF
           END-IF

           IF WM-RENDER-LEN + 27 <= WM-CAP
               MOVE "=== END WORKING MEMORY ==="
                 TO WM-RENDER(WM-RENDER-LEN + 1:26)
               ADD 26 TO WM-RENDER-LEN
               ADD 1 TO WM-RENDER-LEN
               MOVE X"0A"
                 TO WM-RENDER(WM-RENDER-LEN:1)
           END-IF

      *>   JSON-escape for embedding in content field
           MOVE SPACES TO WS-ESC-IN
           IF WM-RENDER-LEN > 0
               MOVE WM-RENDER(1:WM-RENDER-LEN)
                   TO WS-ESC-IN
           END-IF
           PERFORM JSON-ESCAPE-STR
           MOVE SPACES TO WM-RENDER-ESC
           IF WS-ESC-OLEN > 0
               IF WS-ESC-OLEN > 5000
                   MOVE WS-ESC-OUT(1:5000)
                       TO WM-RENDER-ESC
                   MOVE 5000 TO WM-RENDER-ESC-LEN
               ELSE
                   MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
                       TO WM-RENDER-ESC
                   MOVE WS-ESC-OLEN TO WM-RENDER-ESC-LEN
               END-IF
           ELSE
               MOVE 0 TO WM-RENDER-ESC-LEN
           END-IF
           .

      *> ============================================================
      *> WM-RENDER-APPEND-HEADER
      *> Append the section header currently in WM-TMP (length
      *> WM-TMP-LEN) to WM-RENDER, terminated by X"0A". Caller fills
      *> WM-TMP + WM-TMP-LEN before PERFORMing this paragraph.
      *> ============================================================
       WM-RENDER-APPEND-HEADER.
           IF WM-TMP-LEN = 0
               EXIT PARAGRAPH
           END-IF
           IF WM-RENDER-LEN + WM-TMP-LEN + 1 > WM-CAP
               EXIT PARAGRAPH
           END-IF
           MOVE WM-TMP(1:WM-TMP-LEN)
             TO WM-RENDER(WM-RENDER-LEN + 1:WM-TMP-LEN)
           ADD WM-TMP-LEN TO WM-RENDER-LEN
           ADD 1 TO WM-RENDER-LEN
           MOVE X"0A"
             TO WM-RENDER(WM-RENDER-LEN:1)
           .

      *> ============================================================
      *> FORCED-COMMIT-INJECT
      *> Once per run, at iter = WS-COMMIT-ITER, inject a synthetic
      *> [SYSTEM-CONTROL] user message into WS-CONV-BUF BEFORE the
      *> next CALL-LLM so the planner is forced to re-prioritise
      *> submission over exploration. Content varies depending on
      *> whether WORKING MEMORY already holds enough evidence:
      *>
      *>   (a) Has >=1 candidate event OR coord record:
      *>       hard "STOP exploring, commit to the evidence you
      *>       already have" message.
      *>   (b) Otherwise:
      *>       softer "focus on Polish synonyms, broaden search"
      *>       hint - still commit-biased but admits there is
      *>       nothing to commit to yet.
      *>
      *> WS-COMMIT-INJECTED is a latch so the message is sent once.
      *> ============================================================
       FORCED-COMMIT-INJECT.
           IF WS-COMMIT-INJECTED = "Y"
               EXIT PARAGRAPH
           END-IF
           IF WS-ITER NOT = WS-COMMIT-ITER
               EXIT PARAGRAPH
           END-IF
           MOVE "Y" TO WS-COMMIT-INJECTED

           STRING
               ","
               "{" WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               "[SYSTEM-CONTROL] "
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           IF WM-EVENTS-LEN > 0
           OR WM-COORDS-LEN > 0
               STRING
                   "You are half-way through the "
                   "iter budget and WORKING MEMORY "
                   "already contains at least one "
                   "candidate event and/or coord "
                   "record. STOP exploring. On the "
                   "NEXT turn, pick the strongest "
                   "candidate in WORKING MEMORY, "
                   "run the MANDATORY VERIFICATION "
                   "checks on it, and if they "
                   "pass, submit it via printf "
                   "with done=true. A wrong answer "
                   "is recoverable; never "
                   "submitting is not. "
                   "Before picking coords, "
                   "cross-check the event "
                   "description (jaskini, chata, "
                   "las, hangar, polana, ruiny) "
                   "with the gps.json 'type' "
                   "field - the correct entry_id "
                   "is the one where 'type' "
                   "matches the place-type word "
                   "in the event row."
                   DELIMITED SIZE
                   INTO WS-CONV-BUF
                   WITH POINTER WS-CONV-PTR
               END-STRING
           ELSE
               STRING
                   "You are half-way through the "
                   "iter budget and WORKING MEMORY "
                   "is still thin. Your literal "
                   "search terms are not finding "
                   "evidence. Switch strategy: "
                   "grep the source files with a "
                   "Polish synonym alternation "
                   "from the [HINT] block (grep "
                   "-iE with a truncated-root "
                   "pattern covers all inflected "
                   "forms in one call). Broaden "
                   "the keyword, not the file set."
                   DELIMITED SIZE
                   INTO WS-CONV-BUF
                   WITH POINTER WS-CONV-PTR
               END-STRING
           END-IF

           STRING
               WS-QT "}"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           DISPLAY "  [COMMIT] forced-commit "
               "message injected at iter "
               WS-ITER-DSP
           .

      *> ============================================================
      *> VERIFY-GATE
      *> Deterministic verification gate. Invoked AFTER the planner
      *> sets done=true but BEFORE SEND-CMD-TO-HUB ships the printf.
      *> Instead of trusting the LLM's self-check, the gate scans
      *> the accumulated server-response history (the loop-guard
      *> ring buffer) looking for ALL of:
      *>
      *>   - final_answer.date       appears verbatim in some
      *>                             code=100 server response
      *>   - final_answer.city       appears verbatim in some
      *>                             code=100 server response
      *>   - final_answer.latitude   appears within 5 lines of
      *>                             final_answer.longitude AND
      *>                             at least one of those lines
      *>                             also contains "type" or a
      *>                             body/event noun root
      *>
      *> On PASS: sets WS-GATE-OK="Y". Caller proceeds to send.
      *> On FAIL: sets WS-GATE-OK="N" + appends a [VERIFICATION
      *>          GATE FAILED: evidence for field X not found]
      *>          system note into WS-CONV-BUF, forces WS-PL-DONE
      *>          back to "false" so the loop takes one more turn,
      *>          and does NOT count the turn as a hub call.
      *> ============================================================
       VERIFY-GATE.
           MOVE "N" TO WS-GATE-OK
           MOVE SPACES TO WS-GATE-REASON
           MOVE 0 TO WS-GATE-REASON-LEN
           MOVE SPACES TO WS-GATE-FIELD

      *>   Extract final_answer from the planner's WS-PL-RAW.
           MOVE SPACES TO WS-JBUF
           MOVE 0 TO WS-JLEN
           MOVE FUNCTION TRIM(WS-PL-RAW) TO WS-JBUF
           MOVE LENGTH(FUNCTION TRIM(WS-PL-RAW))
               TO WS-JLEN

           MOVE "final_answer" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE SPACES TO WS-GATE-FA
           MOVE 0 TO WS-GATE-FA-LEN
           IF WS-JVAL NOT = SPACES
               MOVE WS-JVAL TO WS-GATE-FA
               MOVE LENGTH(
                   FUNCTION TRIM(WS-JVAL TRAILING))
                   TO WS-GATE-FA-LEN
           END-IF

      *>   Sub-parse final_answer by staging it in WS-JBUF and
      *>   pulling out date/city/latitude/longitude fields. Any
      *>   field not present is simply skipped.
           MOVE SPACES TO WS-GATE-DATE
           MOVE SPACES TO WS-GATE-CITY
           MOVE SPACES TO WS-GATE-LAT
           MOVE SPACES TO WS-GATE-LON

           IF WS-GATE-FA-LEN > 0
               MOVE SPACES TO WS-JBUF
               MOVE 0 TO WS-JLEN
               MOVE WS-GATE-FA(1:WS-GATE-FA-LEN)
                   TO WS-JBUF
               MOVE WS-GATE-FA-LEN TO WS-JLEN

               MOVE "date" TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               IF WS-JVAL NOT = SPACES
                   MOVE WS-JVAL TO WS-GATE-DATE
               END-IF

               MOVE "city" TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               IF WS-JVAL NOT = SPACES
                   MOVE WS-JVAL TO WS-GATE-CITY
               END-IF

               MOVE "latitude" TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               IF WS-JVAL NOT = SPACES
                   MOVE WS-JVAL TO WS-GATE-LAT
               END-IF

               MOVE "longitude" TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               IF WS-JVAL NOT = SPACES
                   MOVE WS-JVAL TO WS-GATE-LON
               END-IF
           END-IF

      *>   Assemble the server-response history (loop-guard ring)
      *>   into WS-GATE-HAY (concatenated). This is the haystack
      *>   we grep-search for the final_answer fields.
           MOVE SPACES TO WS-GATE-HAY
           MOVE 0 TO WS-GATE-HAY-LEN
           PERFORM VARYING WM-I FROM 1 BY 1
               UNTIL WM-I > WS-DUP-COUNT
               IF WS-DUP-RESP-LEN(WM-I) > 0
                   IF WS-GATE-HAY-LEN
                       + WS-DUP-RESP-LEN(WM-I) + 1
                       <= 100000
                       MOVE WS-DUP-RESP(WM-I)(
                           1:WS-DUP-RESP-LEN(WM-I))
                         TO WS-GATE-HAY(
                         WS-GATE-HAY-LEN + 1:
                         WS-DUP-RESP-LEN(WM-I))
                       ADD WS-DUP-RESP-LEN(WM-I)
                           TO WS-GATE-HAY-LEN
                       ADD 1 TO WS-GATE-HAY-LEN
                       MOVE X"0A"
                         TO WS-GATE-HAY(
                         WS-GATE-HAY-LEN:1)
                   END-IF
               END-IF
           END-PERFORM

           IF WS-GATE-HAY-LEN = 0
               MOVE "no-history" TO WS-GATE-FIELD
               PERFORM VERIFY-GATE-FAIL
               EXIT PARAGRAPH
           END-IF

      *>   Gate check #1 (date) removed: the task requires
      *>   "day-before" date arithmetic, so the answer date is
      *>   intentionally one day off from any date in server
      *>   history and can never appear verbatim in the haystack.
      *>   Keeping this check would reject every valid answer.

      *>   Gate check #2 - city: SKIPPED — CORRECT-CITY
      *>   deterministically fixes the city after the gate.

      *>   Gate check #3 - latitude and longitude both present.
      *>   (The "within 5 lines of each other on a line also
      *>   containing 'type'" refinement is relaxed here to:
      *>   both substrings appear somewhere in the haystack.
      *>   Our ring buffer stores per-response text which is
      *>   already a natural locality window, so substring
      *>   presence is a strong proxy for same-record.)
           IF WS-GATE-LAT NOT = SPACES
               MOVE LENGTH(
                   FUNCTION TRIM(WS-GATE-LAT TRAILING))
                   TO WM-I
               IF WM-I > 0
                   MOVE 0 TO WM-TALLY
                   INSPECT WS-GATE-HAY(1:WS-GATE-HAY-LEN)
                       TALLYING WM-TALLY
                       FOR ALL WS-GATE-LAT(1:WM-I)
                   IF WM-TALLY = 0
                       MOVE "latitude" TO WS-GATE-FIELD
                       PERFORM VERIFY-GATE-FAIL
                       EXIT PARAGRAPH
                   END-IF
               END-IF
           END-IF

           IF WS-GATE-LON NOT = SPACES
               MOVE LENGTH(
                   FUNCTION TRIM(WS-GATE-LON TRAILING))
                   TO WM-I
               IF WM-I > 0
                   MOVE 0 TO WM-TALLY
                   INSPECT WS-GATE-HAY(1:WS-GATE-HAY-LEN)
                       TALLYING WM-TALLY
                       FOR ALL WS-GATE-LON(1:WM-I)
                   IF WM-TALLY = 0
                       MOVE "longitude" TO WS-GATE-FIELD
                       PERFORM VERIFY-GATE-FAIL
                       EXIT PARAGRAPH
                   END-IF
               END-IF
           END-IF

      *>   All checks passed.
           MOVE "Y" TO WS-GATE-OK
           DISPLAY "  [GATE] verification passed - "
               "forwarding to centrala"
           .

      *> ============================================================
      *> VERIFY-GATE-FAIL
      *> Common failure path for VERIFY-GATE. Leaves WS-GATE-OK="N",
      *> forces WS-PL-DONE back to "false" so the main loop takes
      *> one more iteration, and appends a [VERIFICATION GATE
      *> FAILED: evidence for field X not found] user message into
      *> WS-CONV-BUF so the planner sees the specific gap.
      *> ============================================================
       VERIFY-GATE-FAIL.
           MOVE "N" TO WS-GATE-OK
           MOVE "false" TO WS-PL-DONE

           DISPLAY "  [GATE] FAILED - field "
               TRIM(WS-GATE-FIELD)
               " not found in server history"

           STRING
               ","
               "{" WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               "[VERIFICATION GATE FAILED: "
               "evidence for field "
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           MOVE LENGTH(
               FUNCTION TRIM(WS-GATE-FIELD TRAILING))
               TO WM-I
           IF WM-I > 0
               MOVE WS-GATE-FIELD(1:WM-I)
                 TO WS-CONV-BUF(WS-CONV-PTR:WM-I)
               ADD WM-I TO WS-CONV-PTR
           END-IF

           STRING
               " not found verbatim in any prior "
               "code=100 server response. Do NOT "
               "submit this candidate. Go back to "
               "/data, re-verify with a fresh "
               "grep, and only set done=true "
               "after you have observed the "
               "exact value in server output.]"
               WS-QT "}"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING
           .

      *> ============================================================
      *> SEND-CMD-TO-HUB
      *> POST {apikey,task,answer:{cmd:...}} to /verify and read the
      *> response back into WS-JBUF.
      *> ============================================================
       SEND-CMD-TO-HUB.
      *>   Escape cmd for JSON
           MOVE SPACES TO WS-ESC-IN
           MOVE WS-PL-CMD TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

           MOVE SPACES TO WS-HUB-BODY
           MOVE 1 TO WS-PTR
           STRING
               "{"
               WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME) WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "cmd" WS-QT ":"
               WS-QT
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           IF WS-ESC-OLEN > 0
               MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
                   TO WS-HUB-BODY(
                   WS-PTR:WS-ESC-OLEN)
               ADD WS-ESC-OLEN TO WS-PTR
           END-IF

           STRING
               WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

      *>   Write request and send via curl
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
               "curl -s "
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

           DISPLAY "  [HUB] " WS-JBUF(1:400)
           .

      *> ============================================================
      *> LOOP-GUARD-CHECK
      *> Scan ring buffer of the last 30 submitted cmds. If the
      *> current WS-PL-CMD is a byte-exact match for any prior
      *> entry, mark WS-DUP-HIT="Y" and synthesize a local error
      *> into WS-JBUF so the planner sees hard feedback WITHOUT
      *> re-charging centrala. Sets WS-DUP-HIT="N" otherwise.
      *> ============================================================
       LOOP-GUARD-CHECK.
           MOVE "N" TO WS-DUP-HIT
           MOVE 0 TO WS-DUP-MATCH
           IF WS-DUP-COUNT = 0
               EXIT PARAGRAPH
           END-IF

      *>   Compute current cmd length (chars up to last non-space)
           MOVE 0 TO WS-K
           COMPUTE WS-K = FUNCTION LENGTH(
               FUNCTION TRIM(WS-PL-CMD TRAILING))
           IF WS-K = 0
               EXIT PARAGRAPH
           END-IF

      *>   Byte-exact scan of occupied slots
           PERFORM VARYING WS-DUP-IDX FROM 1 BY 1
               UNTIL WS-DUP-IDX > WS-DUP-COUNT
                  OR WS-DUP-MATCH > 0
               IF WS-DUP-CMD-LEN(WS-DUP-IDX) = WS-K
                   IF WS-DUP-CMD(WS-DUP-IDX)(1:WS-K)
                       = WS-PL-CMD(1:WS-K)
                       MOVE WS-DUP-IDX TO WS-DUP-MATCH
                   END-IF
               END-IF
           END-PERFORM

           IF WS-DUP-MATCH = 0
               EXIT PARAGRAPH
           END-IF

      *>   Hit: synthesize a rejection into WS-JBUF so downstream
      *>   EXTRACT-SERVER-SNIPPET / APPEND-SERVER-MSG feed it back
      *>   to the planner as if centrala had answered.
           MOVE "Y" TO WS-DUP-HIT
           MOVE SPACES TO WS-JBUF
           MOVE 1 TO WS-PTR
           STRING
               "{" WS-QT "code" WS-QT ":-999,"
               WS-QT "message" WS-QT ":"
               WS-QT "[loop-guard] duplicate "
               "submission blocked locally. "
               "You already sent this exact "
               "cmd earlier and it failed with: "
               DELIMITED SIZE
               INTO WS-JBUF
               WITH POINTER WS-PTR
           END-STRING

      *>   Splice in the prior response (escaped) if we have one
           IF WS-DUP-RESP-LEN(WS-DUP-MATCH) > 0
               MOVE SPACES TO WS-ESC-IN
               MOVE WS-DUP-RESP(WS-DUP-MATCH)(
                   1:WS-DUP-RESP-LEN(WS-DUP-MATCH))
                   TO WS-ESC-IN
               PERFORM JSON-ESCAPE-STR
               IF WS-ESC-OLEN > 0
                   MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
                       TO WS-JBUF(
                       WS-PTR:WS-ESC-OLEN)
                   ADD WS-ESC-OLEN TO WS-PTR
               END-IF
           END-IF

           STRING
               ". Pick a DIFFERENT entry_id "
               "from /data and reason from "
               "scratch - do NOT reformat or "
               "re-escape the same candidate."
               WS-QT "}"
               DELIMITED SIZE
               INTO WS-JBUF
               WITH POINTER WS-PTR
           END-STRING

           COMPUTE WS-JLEN = WS-PTR - 1
           DISPLAY "  [GUARD] duplicate cmd blocked (slot "
               WS-DUP-MATCH ")"
           .

      *> ============================================================
      *> LOOP-GUARD-PUSH
      *> Push (WS-PL-CMD, WS-JBUF-snippet) into the ring buffer
      *> slot pointed to by WS-DUP-NEXT, then advance the pointer.
      *> Called only after a real hub call (not after a guarded
      *> short-circuit) so we never teach the guard to match its
      *> own synthetic responses.
      *> ============================================================
       LOOP-GUARD-PUSH.
      *>   Current cmd length
           MOVE 0 TO WS-K
           COMPUTE WS-K = FUNCTION LENGTH(
               FUNCTION TRIM(WS-PL-CMD TRAILING))
           IF WS-K = 0
               EXIT PARAGRAPH
           END-IF
           IF WS-K > 4000
               MOVE 4000 TO WS-K
           END-IF

      *>   Store cmd in next slot
           MOVE SPACES TO WS-DUP-CMD(WS-DUP-NEXT)
           MOVE WS-PL-CMD(1:WS-K)
               TO WS-DUP-CMD(WS-DUP-NEXT)(1:WS-K)
           MOVE WS-K TO WS-DUP-CMD-LEN(WS-DUP-NEXT)

      *>   Store truncated response (first 4000 chars of WS-JBUF)
           MOVE SPACES TO WS-DUP-RESP(WS-DUP-NEXT)
           MOVE 0 TO WS-DUP-RESP-LEN(WS-DUP-NEXT)
           IF WS-JLEN > 0
               IF WS-JLEN > 4000
                   MOVE WS-JBUF(1:4000)
                       TO WS-DUP-RESP(WS-DUP-NEXT)(1:4000)
                   MOVE 4000
                       TO WS-DUP-RESP-LEN(WS-DUP-NEXT)
               ELSE
                   MOVE WS-JBUF(1:WS-JLEN)
                       TO WS-DUP-RESP(WS-DUP-NEXT)(
                       1:WS-JLEN)
                   MOVE WS-JLEN
                       TO WS-DUP-RESP-LEN(WS-DUP-NEXT)
               END-IF
           END-IF

      *>   Advance ring pointer (1..30 circular, matches OCCURS)
           IF WS-DUP-COUNT < 30
               ADD 1 TO WS-DUP-COUNT
           END-IF
           IF WS-DUP-NEXT < 30
               ADD 1 TO WS-DUP-NEXT
           ELSE
               MOVE 1 TO WS-DUP-NEXT
           END-IF
           .

      *> ============================================================
      *> CHECK-FLAG-IN-JBUF
      *> INSPECT the hub response for "{FLG:" pattern.
      *> ============================================================
       CHECK-FLAG-IN-JBUF.
           MOVE 0 TO WS-TALLY-CNT
           IF WS-JLEN > 0
               INSPECT WS-JBUF(1:WS-JLEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL "{FLG:"
           END-IF
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-FLAG-FOUND
           END-IF
           .

      *> ============================================================
      *> INIT-CITY-TABLE: Populate the 10-city Polish lookup table
      *> ============================================================
       INIT-CITY-TABLE.
           MOVE "Grudziadz"  TO WS-CITY-NAME(1)
           MOVE 53.486111     TO WS-CITY-LAT(1)
           MOVE 18.753611     TO WS-CITY-LON(1)

           MOVE "Gdansk"     TO WS-CITY-NAME(2)
           MOVE 54.350000     TO WS-CITY-LAT(2)
           MOVE 18.650000     TO WS-CITY-LON(2)

           MOVE "Krakow"     TO WS-CITY-NAME(3)
           MOVE 50.061667     TO WS-CITY-LAT(3)
           MOVE 19.937500     TO WS-CITY-LON(3)

           MOVE "Warszawa"   TO WS-CITY-NAME(4)
           MOVE 52.229722     TO WS-CITY-LAT(4)
           MOVE 21.011944     TO WS-CITY-LON(4)

           MOVE "Poznan"     TO WS-CITY-NAME(5)
           MOVE 52.406667     TO WS-CITY-LAT(5)
           MOVE 16.925000     TO WS-CITY-LON(5)

           MOVE "Wroclaw"    TO WS-CITY-NAME(6)
           MOVE 51.110000     TO WS-CITY-LAT(6)
           MOVE 17.030000     TO WS-CITY-LON(6)

           MOVE "Lodz"       TO WS-CITY-NAME(7)
           MOVE 51.759722     TO WS-CITY-LAT(7)
           MOVE 19.455833     TO WS-CITY-LON(7)

           MOVE "Szczecin"   TO WS-CITY-NAME(8)
           MOVE 53.428611     TO WS-CITY-LAT(8)
           MOVE 14.552778     TO WS-CITY-LON(8)

           MOVE "Bydgoszcz"  TO WS-CITY-NAME(9)
           MOVE 53.123333     TO WS-CITY-LAT(9)
           MOVE 18.008333     TO WS-CITY-LON(9)

           MOVE "Torun"      TO WS-CITY-NAME(10)
           MOVE 53.013889     TO WS-CITY-LAT(10)
           MOVE 18.598333     TO WS-CITY-LON(10)
           .

      *> ============================================================
      *> CORRECT-CITY: Override LLM city with nearest-city match
      *> Uses WS-GATE-LAT, WS-GATE-LON, WS-GATE-CITY (set by gate)
      *> Patches WS-PL-CMD in place if city differs
      *> ============================================================
       CORRECT-CITY.
      *>   Parse lat/lon/city from the printf cmd JSON
      *>   WS-PL-CMD = printf '{"date":...,"city":"X",
      *>                "longitude":N,"latitude":N}'
           MOVE SPACES TO WS-JBUF
           MOVE 0 TO WS-JLEN
      *>   Find the opening { in the printf cmd
           MOVE 0 TO WS-CC-POS
           INSPECT WS-PL-CMD
               TALLYING WS-CC-POS
               FOR CHARACTERS
               BEFORE INITIAL "{"
           IF WS-CC-POS >=
               FUNCTION LENGTH(
               FUNCTION TRIM(WS-PL-CMD TRAILING))
               DISPLAY "  [CITY] no JSON in cmd"
               EXIT PARAGRAPH
           END-IF
           MOVE WS-PL-CMD(WS-CC-POS + 1:
               FUNCTION LENGTH(
               FUNCTION TRIM(
               WS-PL-CMD TRAILING))
               - WS-CC-POS)
               TO WS-JBUF
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-JBUF TRAILING))
               TO WS-JLEN

           MOVE "latitude" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF WS-JVAL = SPACES
               DISPLAY "  [CITY] no lat in cmd"
               EXIT PARAGRAPH
           END-IF
           MOVE WS-JVAL TO WS-GATE-LAT
           COMPUTE WS-CC-LAT =
               FUNCTION NUMVAL(
               FUNCTION TRIM(WS-JVAL))

      *>   Re-stage JBUF (FIND-JSON-VAL may modify)
           MOVE SPACES TO WS-JBUF
           MOVE WS-PL-CMD(WS-CC-POS + 1:
               FUNCTION LENGTH(
               FUNCTION TRIM(
               WS-PL-CMD TRAILING))
               - WS-CC-POS)
               TO WS-JBUF
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-JBUF TRAILING))
               TO WS-JLEN

           MOVE "longitude" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF WS-JVAL = SPACES
               DISPLAY "  [CITY] no lon in cmd"
               EXIT PARAGRAPH
           END-IF
           MOVE WS-JVAL TO WS-GATE-LON
           COMPUTE WS-CC-LON =
               FUNCTION NUMVAL(
               FUNCTION TRIM(WS-JVAL))

      *>   Detect lat/lon swap: if lat is in lon
      *>   range [14,25] AND lon is in lat range
      *>   [49,56], they are swapped.
           MOVE WS-CC-LAT TO WS-CC-ORIG-LAT
           MOVE WS-CC-LON TO WS-CC-ORIG-LON
           IF WS-CC-LAT >= 14 AND WS-CC-LAT <= 25
               AND WS-CC-LON >= 49
               AND WS-CC-LON <= 56
               DISPLAY "  [CITY] lat/lon swap "
                   "detected - correcting"
               MOVE WS-CC-LAT TO WS-CC-SWAP-TMP
               MOVE WS-CC-LON TO WS-CC-LAT
               MOVE WS-CC-SWAP-TMP TO WS-CC-LON
               PERFORM SWAP-CMD-LATLON
           END-IF

      *>   Re-stage JBUF for city extraction
           MOVE SPACES TO WS-JBUF
           MOVE WS-PL-CMD(WS-CC-POS + 1:
               FUNCTION LENGTH(
               FUNCTION TRIM(
               WS-PL-CMD TRAILING))
               - WS-CC-POS)
               TO WS-JBUF
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-JBUF TRAILING))
               TO WS-JLEN

           MOVE "city" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF WS-JVAL NOT = SPACES
               MOVE WS-JVAL TO WS-GATE-CITY
           END-IF

      *>   Only correct for Polish coordinates
      *>   (lat 49-56, lon 14-25)
           IF WS-CC-LAT < 49 OR WS-CC-LAT > 56
               OR WS-CC-LON < 14 OR WS-CC-LON > 25
               DISPLAY "  [CITY] coords outside PL"
                   " - skipping correction"
               EXIT PARAGRAPH
           END-IF

      *>   Find nearest city (Manhattan distance)
           MOVE 999999 TO WS-CC-BEST-DIST
           MOVE 0 TO WS-CC-BEST-IDX
           PERFORM VARYING WS-CC-I
               FROM 1 BY 1
               UNTIL WS-CC-I > WS-CITY-COUNT
               COMPUTE WS-CC-DIST =
                   FUNCTION ABS(
                   WS-CC-LAT - WS-CITY-LAT(WS-CC-I))
                   + FUNCTION ABS(
                   WS-CC-LON - WS-CITY-LON(WS-CC-I))
               IF WS-CC-DIST < WS-CC-BEST-DIST
                   MOVE WS-CC-DIST
                       TO WS-CC-BEST-DIST
                   MOVE WS-CC-I
                       TO WS-CC-BEST-IDX
               END-IF
           END-PERFORM

           IF WS-CC-BEST-IDX = 0
               EXIT PARAGRAPH
           END-IF

           MOVE FUNCTION TRIM(
               WS-CITY-NAME(WS-CC-BEST-IDX))
               TO WS-CC-NEW-CITY

           DISPLAY "  [CITY] coords ("
               FUNCTION TRIM(WS-GATE-LAT)
               ", "
               FUNCTION TRIM(WS-GATE-LON)
               ") -> "
               FUNCTION TRIM(WS-CC-NEW-CITY)
               " (dist="
               WS-CC-BEST-DIST ")"

      *>   If city already matches, nothing to do
           IF FUNCTION TRIM(WS-GATE-CITY)
               = FUNCTION TRIM(WS-CC-NEW-CITY)
               EXIT PARAGRAPH
           END-IF

           DISPLAY "  [CITY] correcting "
               FUNCTION TRIM(WS-GATE-CITY)
               " -> "
               FUNCTION TRIM(WS-CC-NEW-CITY)

      *>   Use generic REPLACE-IN-CMD to patch city
           MOVE FUNCTION TRIM(WS-GATE-CITY)
               TO WS-CC-FIND-STR
           MOVE FUNCTION TRIM(WS-CC-NEW-CITY)
               TO WS-CC-REPL-STR
           PERFORM REPLACE-IN-CMD
           DISPLAY "  [CITY] cmd patched OK"
           .

      *> ============================================================
      *> REPLACE-IN-CMD: generic find/replace in WS-PL-CMD
      *> IN:  WS-CC-FIND-STR  = text to find
      *>      WS-CC-REPL-STR  = replacement text
      *> ============================================================
       REPLACE-IN-CMD.
           INITIALIZE WS-CC-SCRATCH
           MOVE 0 TO WS-CC-POS
           INSPECT WS-PL-CMD
               TALLYING WS-CC-POS
               FOR CHARACTERS
               BEFORE INITIAL
               WS-CC-FIND-STR(1:
               FUNCTION LENGTH(
               FUNCTION TRIM(
               WS-CC-FIND-STR)))

           IF WS-CC-POS = 0
               AND WS-PL-CMD(1:
               FUNCTION LENGTH(
               FUNCTION TRIM(
               WS-CC-FIND-STR)))
               NOT = WS-CC-FIND-STR(1:
               FUNCTION LENGTH(
               FUNCTION TRIM(
               WS-CC-FIND-STR)))
               DISPLAY "  [REPL] '"
                   FUNCTION TRIM(
                   WS-CC-FIND-STR)
                   "' not found in cmd"
               EXIT PARAGRAPH
           END-IF

           MOVE WS-CC-POS TO WS-CC-TAIL-POS
           ADD FUNCTION LENGTH(
               FUNCTION TRIM(
               WS-CC-FIND-STR))
               TO WS-CC-TAIL-POS

           COMPUTE WS-CC-TAIL-LEN =
               FUNCTION LENGTH(
               FUNCTION TRIM(
               WS-PL-CMD TRAILING))
               - WS-CC-TAIL-POS

           INITIALIZE WS-CC-SCRATCH
           IF WS-CC-POS > 0
               STRING
                   WS-PL-CMD(1:WS-CC-POS)
                   FUNCTION TRIM(WS-CC-REPL-STR)
                   WS-PL-CMD(
                   WS-CC-TAIL-POS + 1:
                   WS-CC-TAIL-LEN)
                   DELIMITED SIZE
                   INTO WS-CC-SCRATCH
               END-STRING
           ELSE
               STRING
                   FUNCTION TRIM(WS-CC-REPL-STR)
                   WS-PL-CMD(
                   WS-CC-TAIL-POS + 1:
                   WS-CC-TAIL-LEN)
                   DELIMITED SIZE
                   INTO WS-CC-SCRATCH
               END-STRING
           END-IF

           MOVE SPACES TO WS-PL-CMD
           MOVE WS-CC-SCRATCH TO WS-PL-CMD
           .

      *> ============================================================
      *> SWAP-CMD-LATLON: 3-pass sentinel swap of lat/lon
      *> values in the printf JSON inside WS-PL-CMD.
      *> Uses WS-CC-ORIG-LAT/LON (values before swap).
      *> Pass 1: latitude value  -> __SENTINEL__
      *> Pass 2: longitude value -> old latitude value
      *> Pass 3: __SENTINEL__    -> old longitude value
      *> ============================================================
       SWAP-CMD-LATLON.
      *>   Build string representations of the values
      *>   Pass 1: replace latitude value with sentinel
           MOVE SPACES TO WS-CC-FIND-STR
           MOVE SPACES TO WS-CC-REPL-STR
           STRING
               FUNCTION TRIM(WS-GATE-LAT)
               DELIMITED SIZE
               INTO WS-CC-FIND-STR
           END-STRING
           MOVE "__SENTINEL__" TO WS-CC-REPL-STR
           PERFORM REPLACE-IN-CMD

      *>   Pass 2: replace longitude value with old lat
           MOVE SPACES TO WS-CC-FIND-STR
           MOVE SPACES TO WS-CC-REPL-STR
           STRING
               FUNCTION TRIM(WS-GATE-LON)
               DELIMITED SIZE
               INTO WS-CC-FIND-STR
           END-STRING
           STRING
               FUNCTION TRIM(WS-GATE-LAT)
               DELIMITED SIZE
               INTO WS-CC-REPL-STR
           END-STRING
           PERFORM REPLACE-IN-CMD

      *>   Pass 3: replace sentinel with old lon
           MOVE "__SENTINEL__" TO WS-CC-FIND-STR
           MOVE SPACES TO WS-CC-REPL-STR
           STRING
               FUNCTION TRIM(WS-GATE-LON)
               DELIMITED SIZE
               INTO WS-CC-REPL-STR
           END-STRING
           PERFORM REPLACE-IN-CMD

           DISPLAY "  [CITY] lat/lon keys "
               "swapped in cmd"
           .

      *> ============================================================
      *> Copybook procedures
      *> ============================================================
       COPY ENVLOAD-PROC.
       COPY JSONPARSE-PROC.
       COPY JSONESCAPE-PROC.
       COPY JSONUNESCAPE-PROC.

      *> ============================================================
      *> READ-JSON-FILE (inline, large buffer matching WS-JBUF)
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
                           IF WS-K > 900000
                               MOVE 900000 TO WS-K
                           END-IF
                           IF WS-JLEN + WS-K
                               <= 900000
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
