      *> ============================================================
      *> ROCKET-NAV-WS.cpy - rocket navigation working storage (S05E04)
      *> COPY in WORKING-STORAGE SECTION after ENVLOAD-WS.
      *>
      *> v2 differences vs v1 copybook:
      *>   - WS-MAX-RESTARTS-V2 capped at 3 (v1 used 20)
      *>   - WS-RATE-LIMIT-FAIL flag for freeRows-aware fallback
      *>   - Rewriter working fields (WS-HINT-NORMALIZED, token
      *>     tables, decision enums)
      *>   - 4-label hint class (UP/DOWN/AHEAD/MIDDLE) - mid1/mid2/
      *>     mid3 removed
      *>   - Scanner-UNKNOWN flag to skip disarm on persistent 502
      *>   - WS-STEP-SLEEP adjustable inter-step delay
      *> ============================================================

      *> -- Task constants --
       01  WS-TASK-NAME            PIC X(20)
                                   VALUE "goingthere".

       01  WS-GRID-ROWS            PIC 9(2) VALUE 3.
       01  WS-GRID-COLS            PIC 9(2) VALUE 12.

      *> -- Task endpoint URLs --
       01  WS-SCANNER-URL          PIC X(250).
       01  WS-SCANNER-GET-URL      PIC X(300).
       01  WS-HINT-URL             PIC X(250).

      *> -- Game state --
       01  WS-PLAYER-ROW           PIC 9(2) VALUE 2.
       01  WS-PLAYER-COL           PIC 9(2) VALUE 1.
       01  WS-BASE-ROW             PIC 9(2) VALUE 2.
       01  WS-BASE-COL             PIC 9(2) VALUE 12.

      *> -- freeRows for current column: 3 slots --
       01  WS-FREE-ROWS.
           05  WS-FREE-ROW-1       PIC 9 VALUE 1.
           05  WS-FREE-ROW-2       PIC 9 VALUE 1.
           05  WS-FREE-ROW-3       PIC 9 VALUE 1.

      *> -- Stone row distribution (per-column prediction) --
       01  WS-P-ROW-1              PIC 9V9(4)
                                   VALUE 0.3333.
       01  WS-P-ROW-2              PIC 9V9(4)
                                   VALUE 0.3333.
       01  WS-P-ROW-3              PIC 9V9(4)
                                   VALUE 0.3333.

      *> -- Current scan parse results --
       01  WS-SCAN-TRAP            PIC X VALUE "N".
       01  WS-SCAN-GW-ERR          PIC X VALUE "N".
       01  WS-SCAN-UNKNOWN         PIC X VALUE "N".
       01  WS-SCAN-LOOKS-REAL      PIC X VALUE "N".
       01  WS-FREQUENCY            PIC 9(10).
       01  WS-FREQUENCY-DSP        PIC 9(10).
       01  WS-FREQ-EDIT            PIC Z(9)9.
       01  WS-DETECT-CODE          PIC X(200).
       01  WS-DETECT-CODE-LEN      PIC 9(4).
       01  WS-DISARM-HASH          PIC X(40).

      *> -- Hint text (lowercased for keyword scan) --
       01  WS-HINT-TEXT            PIC X(4000).
       01  WS-HINT-LOWER           PIC X(4000).
       01  WS-HINT-LEN             PIC 9(5).
       01  WS-HINT-RL              PIC X VALUE "N".

      *> -- Rate-limit bookkeeping: once we've seen throttling in
      *> -- this attempt, bump the inter-step sleep to 3s to ease
      *> -- pressure. WS-RATE-LIMIT-FAIL is set if ALL 3 refetches
      *> -- came back throttled, in which case PICK-MOVE-V2 uses
      *> -- the freeRows-aware base-row-seeking policy.
       01  WS-RATE-LIMIT-SEEN      PIC X VALUE "N".
       01  WS-RATE-LIMIT-FAIL      PIC X VALUE "N".
       01  WS-STEP-SLEEP           PIC 9 VALUE 2.

      *> -- Move command selected --
       01  WS-MOVE                 PIC X(10).

      *> -- Flag detection --
       01  WS-FLAG-FOUND           PIC X VALUE "N".
       01  WS-FLAG-TEXT            PIC X(200).
       01  WS-TALLY-CNT            PIC 9(4).

      *> -- Game step counters --
       01  WS-STEP                 PIC 9(3) VALUE 0.
       01  WS-STEP-DSP             PIC 9(3).
       01  WS-MAX-STEPS            PIC 9(3) VALUE 15.

      *> -- Start time for wall-clock budget --
       01  WS-START-TIME           PIC 9(8) VALUE 0.
       01  WS-NOW-TIME             PIC 9(8) VALUE 0.
       01  WS-ELAPSED              PIC 9(8) VALUE 0.
       01  WS-BUDGET-SEC           PIC 9(4) VALUE 300.

      *> -- Current datetime (for seconds-of-day) --
       01  WS-CURR-DT              PIC 9(21).
       01  WS-CURR-DT-PARTS REDEFINES WS-CURR-DT.
           05  WS-DT-YEAR          PIC 9(4).
           05  WS-DT-MONTH         PIC 9(2).
           05  WS-DT-DAY           PIC 9(2).
           05  WS-DT-HOUR          PIC 9(2).
           05  WS-DT-MIN           PIC 9(2).
           05  WS-DT-SEC           PIC 9(2).
           05  WS-DT-REST          PIC X(7).

      *> -- Candidate move scoring --
       01  WS-CAND-LEGAL-1         PIC X VALUE "N".
       01  WS-CAND-LEGAL-2         PIC X VALUE "N".
       01  WS-CAND-LEGAL-3         PIC X VALUE "N".
       01  WS-CAND-SCORE-1         PIC 9(3)V9(6).
       01  WS-CAND-SCORE-2         PIC 9(3)V9(6).
       01  WS-CAND-SCORE-3         PIC 9(3)V9(6).

      *> -- Retry counter for transient HTTP --
       01  WS-RETRY                PIC 9(2).
       01  WS-MAX-RETRY            PIC 9(2) VALUE 4.

      *> -- Last HTTP body raw buffer mirror --
       01  WS-LAST-RESP            PIC X(8000).
       01  WS-LAST-RESP-LEN        PIC 9(5).

      *> -- Scratch numerics --
       01  WS-N                    PIC 9(6).
       01  WS-M                    PIC 9(6).
       01  WS-ROW-TMP              PIC 9(2).
       01  WS-COL-TMP              PIC 9(2).

      *> -- Display helpers --
       01  WS-ROW-DSP              PIC 9.
       01  WS-COL-DSP              PIC 9(2).

      *> -- LLM request / response buffers --
       01  WS-REQ-JSON             PIC X(16000).
       01  WS-LLM-RAW              PIC X(4000).
       01  WS-LLM-ANS              PIC X(200).

      *> -- Classification result from the rewriter or the LLM.
      *> -- Values: "UP", "DOWN", "AHEAD", "MIDDLE", "UNKNOWN".
       01  WS-HINT-CLASS           PIC X(10).

      *> -- V4-1: classifier output validation. If the LLM returns
      *> -- UP while the player is already on row 1, or DOWN while
      *> -- the player is on row 3, the label is physically
      *> -- impossible. WS-CLASS-INVALID flips to "Y" and the
      *> -- caller retries the LLM ONCE with a constraint note.
      *> -- WS-CLASS-RETRY tracks whether this is the retry pass.
      *> -- WS-LEGAL-LABELS holds the comma-separated legal labels
      *> -- to inject into the retry prompt.
       01  WS-CLASS-INVALID        PIC X VALUE "N".
       01  WS-CLASS-RETRY          PIC X VALUE "N".
       01  WS-LEGAL-LABELS         PIC X(40).
       01  WS-CLASS-PLAYER-ROW-DSP PIC 9.
       01  WS-CLASS-ORIG           PIC X(10).

      *> -- V4-3: remember the row where the stone was in the
      *> -- column we just exited. PICK-MOVE-RL-FALLBACK uses this
      *> -- to bias against moving to that row since adjacent
      *> -- columns rarely have the same stone row (~75%).
       01  WS-PREV-STONE-ROW       PIC 9 VALUE 0.

      *> -- Preview backend URL (/goingthere_backend) --
       01  WS-PREVIEW-URL          PIC X(250).

      *> -- Planned mode state --
       01  WS-MODE                 PIC X VALUE "H".

      *> -- Parsed preview map: 3 rows x 12 cols --
       01  WS-MAP.
           05  WS-MAP-ROW OCCURS 3 TIMES.
               10  WS-MAP-CELL     PIC X OCCURS 12 TIMES.

      *> -- Planned move list --
       01  WS-PLAN-CNT             PIC 9(2) VALUE 0.
       01  WS-PLAN-MOVES.
           05  WS-PLAN-MOVE OCCURS 15 TIMES PIC X(5).

      *> -- Preview fetch bookkeeping --
       01  WS-PREVIEW-OK           PIC X VALUE "N".
       01  WS-PREVIEW-PLAYER-ROW   PIC 9(2).
       01  WS-PREVIEW-PLAYER-COL   PIC 9(2).
       01  WS-PREVIEW-BASE-ROW     PIC 9(2).
       01  WS-PREVIEW-BASE-COL     PIC 9(2).

      *> -- BFS queue (max 40 states for 3x12) --
       01  WS-BFS-CNT              PIC 9(3) VALUE 0.
       01  WS-BFS-HEAD             PIC 9(3) VALUE 0.
       01  WS-BFS-ENTRY OCCURS 40 TIMES.
           05  WS-BFS-ROW          PIC 9(2).
           05  WS-BFS-COL          PIC 9(2).
           05  WS-BFS-PATH-LEN     PIC 9(2).
           05  WS-BFS-PATH OCCURS 15 TIMES PIC X(5).
       01  WS-VISITED.
           05  WS-VISITED-ROW OCCURS 3 TIMES.
               10  WS-VISITED-CELL PIC X OCCURS 12 TIMES.

      *> -- BFS scratch --
       01  WS-BFS-R                PIC 9(2).
       01  WS-BFS-C                PIC 9(2).
       01  WS-BFS-NR               PIC 9(2).
       01  WS-BFS-NC               PIC 9(2).
       01  WS-BFS-L                PIC 9(2).
       01  WS-BFS-DR               PIC S9(2).
       01  WS-BFS-DR-UNS           PIC 9(2).

      *> -- Best BFS candidate end-state --
       01  WS-BEST-FOUND           PIC X VALUE "N".
       01  WS-BEST-ROW             PIC 9(2).
       01  WS-BEST-LEN             PIC 9(2).
       01  WS-BEST-DIAG            PIC 9(2).
       01  WS-BEST-MOVES.
           05  WS-BEST-MOVE OCCURS 15 TIMES PIC X(5).

      *> -- Preview JSON scratch --
       01  WS-PARSE-POS            PIC 9(6).
       01  WS-PARSE-DEPTH          PIC 9(2).
       01  WS-PARSE-ROW            PIC 9(2).
       01  WS-PARSE-COL            PIC 9(2).
       01  WS-PARSE-VAL            PIC X(10).
       01  WS-PARSE-VAL-LEN        PIC 9(2).
       01  WS-PARSE-CH             PIC X.

      *> ============================================================
      *> HINT REWRITER tables (Upgrade 3)
      *>
      *> The rewriter is the biggest v2 lever. Each directional
      *> token describes a physical slot on the grid; within +/- 30
      *> characters of that token we look for SAFE vs DANGER
      *> vocabulary to tag the slot. The outcome is a normalized
      *> label like [SAFE:PORT][DANGER:BOW][SAFE:STARBOARD].
      *>
      *> Slots (after mapping aliases):
      *>   PORT    -> UP    (above the player row)
      *>   STARB   -> DOWN  (below the player row)
      *>   BOW     -> AHEAD (same row as the player)
      *>   MIDDLE  -> MIDDLE (absolute row 2)
      *>
      *> Tagging rules:
      *>   - If a directional token has a SAFE marker within the
      *>     +/- 30 char window: tag SAFE.
      *>   - If it has a DANGER marker within the window: tag
      *>     DANGER.
      *>   - If both markers are present (rare): tag AMBIGUOUS and
      *>     the whole rewriter defers to the LLM.
      *>
      *> Decision rule over the collected tags:
      *>   - Exactly one DANGER, no AMBIGUOUS: pick that slot's
      *>     label.
      *>   - Zero DANGER, exactly two SAFE covering the two non-
      *>     forward slots: the third slot (usually BOW) is the
      *>     rock -> decide AHEAD.
      *>   - Else: DEFER-TO-LLM.
      *> ============================================================

      *> -- Rewriter context window radius (chars, each direction)
       01  WS-REW-WINDOW           PIC 9(2) VALUE 30.

      *> -- Directional token table. slot = PORT|STARB|BOW|MID.
       01  WS-DIR-TOKEN-CNT        PIC 9(3) VALUE 41.
       01  WS-DIR-TOKENS.
           05  WS-DIR-ENTRY OCCURS 41 TIMES.
               10  WS-DIR-TXT      PIC X(32).
               10  WS-DIR-LEN      PIC 9(2).
               10  WS-DIR-SLOT     PIC X(5).

      *> -- Safe vocabulary (applied to context window) --
       01  WS-SAFE-TOKEN-CNT       PIC 9(3) VALUE 71.
       01  WS-SAFE-TOKENS.
           05  WS-SAFE-ENTRY OCCURS 71 TIMES.
               10  WS-SAFE-TXT     PIC X(32).
               10  WS-SAFE-LEN     PIC 9(2).

      *> -- Danger vocabulary (applied to context window) --
       01  WS-DANG-TOKEN-CNT       PIC 9(3) VALUE 65.
       01  WS-DANG-TOKENS.
           05  WS-DANG-ENTRY OCCURS 65 TIMES.
               10  WS-DANG-TXT     PIC X(32).
               10  WS-DANG-LEN     PIC 9(2).

      *> -- V5-1 inversion markers. When one of these appears in
      *> -- the ~20 chars immediately BEFORE a directional token
      *> -- (with no sentence boundary in between), the slot is
      *> -- flipped to its opposite (port<->starboard), and
      *> -- bow/ahead/middle are forced to AMBG since they have
      *> -- no meaningful opposite on a 1-D axis.
       01  WS-INV-TOKEN-CNT        PIC 9(3) VALUE 8.
       01  WS-INV-TOKENS.
           05  WS-INV-ENTRY OCCURS 8 TIMES.
               10  WS-INV-TXT      PIC X(32).
               10  WS-INV-LEN      PIC 9(2).
       01  WS-INV-RADIUS           PIC 9(2) VALUE 20.
       01  WS-REW-INV-HIT          PIC X VALUE "N".
       01  WS-REW-INV-START        PIC 9(5).
       01  WS-REW-INV-LEN          PIC 9(5).
       01  WS-REW-INV-IDX          PIC 9(3).

      *> -- Per-hint slot tags (computed by rewriter). Tag values:
      *>   "NONE"  slot not mentioned
      *>   "SAFE"  slot mentioned with only SAFE markers nearby
      *>   "DNGR"  slot mentioned with only DANGER markers nearby
      *>   "AMBG"  slot mentioned with both
       01  WS-SLOT-PORT-TAG        PIC X(4) VALUE "NONE".
       01  WS-SLOT-STARB-TAG       PIC X(4) VALUE "NONE".
       01  WS-SLOT-BOW-TAG         PIC X(4) VALUE "NONE".
       01  WS-SLOT-MID-TAG         PIC X(4) VALUE "NONE".

      *> -- Aggregate counters across the 4 slots --
       01  WS-CNT-DANGER           PIC 9(2) VALUE 0.
       01  WS-CNT-SAFE             PIC 9(2) VALUE 0.
       01  WS-CNT-AMBIG            PIC 9(2) VALUE 0.

      *> -- Rewriter outcome: "HIT" means we decided WS-HINT-CLASS
      *> -- directly; "DEFER" means call the LLM classifier.
       01  WS-REW-DECISION         PIC X(5) VALUE "DEFER".

      *> -- Normalized form display (for log lines) --
       01  WS-HINT-NORMALIZED      PIC X(200).
       01  WS-HINT-NORM-PTR        PIC 9(4).

      *> -- Scratch used by the rewriter passes --
       01  WS-REW-I                PIC 9(5).
       01  WS-REW-J                PIC 9(5).
       01  WS-REW-WIN-START        PIC 9(5).
       01  WS-REW-WIN-END          PIC 9(5).
       01  WS-REW-WIN-LEN          PIC 9(5).
       01  WS-REW-HAS-SAFE         PIC X.
       01  WS-REW-HAS-DANGER       PIC X.
       01  WS-REW-TMP-TAG          PIC X(4).
       01  WS-REW-DIR-IDX          PIC 9(3).
       01  WS-REW-SAFE-IDX         PIC 9(3).
       01  WS-REW-DANG-IDX         PIC 9(3).
       01  WS-REW-MATCH-POS        PIC 9(5).
       01  WS-REW-MATCH-LEN        PIC 9(2).
       01  WS-REW-SLOT-BUF         PIC X(5).
       01  WS-REW-CUR-TAG          PIC X(4).
       01  WS-REW-SKIP             PIC X VALUE "N".

      *> ============================================================
      *> Few-shot prompt scaffold (Upgrade 4)
      *>
      *> The system prompt + the 6 example pairs are stored as
      *> literal-ish constants (built in LOAD-PROMPT-CONSTANTS) to
      *> keep LLM-CLASSIFY-HINT-V2 compact and readable.
      *> ============================================================
       01  WS-LLM-SYSTEM-PROMPT    PIC X(2000).
       01  WS-LLM-FEWSHOT-BLOCK    PIC X(2400).

      *> -- v6: pre-baked scanner-extract system prompt. Built once
      *> -- in LOAD-PROMPT-CONSTANTS and pasted into the request by
      *> -- LLM-EXTRACT-SCANNER-V2. Holds the full role definition,
      *> -- homoglyph rules, and worked examples as a single JSON-
      *> -- escaped blob (so every " is already \" and newlines are
      *> -- already \n).
       01  WS-LLM-SCAN-SYSTEM-PROMPT PIC X(6000).
