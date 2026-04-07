       IDENTIFICATION DIVISION.
       PROGRAM-ID. S04E03-DOMATOWO.
      *> ============================================================
      *> S04E03 - Domatowo Search & Rescue
      *> Dynamic map parsing, target clustering,
      *> scout deployment, LLM log analysis.
      *> No hardcoded positions or URLs.
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
       01  WS-BS                   PIC X(1)
                                   VALUE X"5C".

      *> === File I/O ===
       01  WS-FS                   PIC XX.
       01  WS-WORK-PATH            PIC X(100)
                                   VALUE "work.tmp".

      *> === HTTP ===
       01  WS-CMD                  PIC X(4000).
       01  WS-REQ-JSON             PIC X(64000).

      *> === Shared copybooks (WS) ===
       COPY JSONPARSE-WS.
       COPY JSONREAD-WS.
       COPY JSONESCAPE-WS.
       COPY HUBSUBMIT-WS.

      *> === Task Configuration ===
       01  WS-TASK-NAME            PIC X(20)
                                   VALUE "domatowo".

      *> === Control Flow ===
       01  WS-PTR                  PIC 9(5).
       01  WS-I                    PIC 9(5).
       01  WS-J                    PIC 9(5).
       01  WS-M                    PIC 9(5).
       01  WS-TALLY-CNT            PIC 9(4).
       01  WS-FLAG-FOUND           PIC X VALUE "N".

      *> === Task Data ===
       01  WS-COLS                 PIC X(11)
                                   VALUE
                                   "ABCDEFGHIJK".

      *> -- Map grid (up to 11x11) --
       01  WS-MAP-SIZE             PIC 9(2)
                                   VALUE 11.
       01  WS-GRID.
           05 WS-GROW OCCURS 11.
              10 WS-GCELL OCCURS 11
                                   PIC X(10).

      *> -- Grid parse state --
       01  WS-GP-POS               PIC 9(5).
       01  WS-GP-DEPTH             PIC 9(2).
       01  WS-GP-ROW               PIC 9(2).
       01  WS-GP-COL               PIC 9(2).
       01  WS-GP-STR               PIC X(10).
       01  WS-GP-SLEN              PIC 9(2).
       01  WS-GP-CH                PIC X(1).
       01  WS-GP-DONE              PIC X VALUE "N".

      *> -- Roads array --
       01  WS-ROAD-CT              PIC 9(3)
                                   VALUE 0.
       01  WS-ROADS.
           05 WS-RD OCCURS 121.
              10 WS-RD-COL         PIC 9(2).
              10 WS-RD-ROW         PIC 9(2).

      *> -- Target cells (tallest blocks) --
       01  WS-TGT-CT               PIC 9(2)
                                   VALUE 0.
       01  WS-TARGETS.
           05 WS-TG OCCURS 30.
              10 WS-TG-COL         PIC 9(2).
              10 WS-TG-ROW         PIC 9(2).
              10 WS-TG-CLUST       PIC 9(2).
              10 WS-TG-PAIRED      PIC X.

      *> -- Building height tracking --
       01  WS-MAX-HEIGHT           PIC 9(1)
                                   VALUE 0.
       01  WS-CUR-HEIGHT           PIC 9(1).

      *> -- Clustering --
       01  WS-CLUST-CT             PIC 9(2)
                                   VALUE 0.
       01  WS-CL-CHANGED           PIC X.
       01  WS-CL-ID                PIC 9(2).

      *> -- Current cluster target indices --
       01  WS-CL-TGT-CT            PIC 9(2).
       01  WS-CL-TGTS.
           05 WS-CL-TG OCCURS 30
                                   PIC 9(2).

      *> -- Pairs for current cluster --
       01  WS-PAIR-CT              PIC 9(2).
       01  WS-PAIRS.
           05 WS-PR OCCURS 15.
              10 WS-PR-T1          PIC 9(2).
              10 WS-PR-T2          PIC 9(2).

      *> -- Known unit hashes --
       01  WS-KNOWN-CT             PIC 9(2)
                                   VALUE 0.
       01  WS-KNOWN.
           05 WS-KH OCCURS 20
                                   PIC X(40).

      *> -- Scouts for current cluster --
       01  WS-SC-CT                PIC 9(2)
                                   VALUE 0.
       01  WS-SCOUTS.
           05 WS-SC OCCURS 8.
              10 WS-SC-HASH        PIC X(40).
              10 WS-SC-COL         PIC 9(2).
              10 WS-SC-ROW         PIC 9(2).

      *> -- Inspected cells --
       01  WS-INSP-CT              PIC 9(2)
                                   VALUE 0.
       01  WS-INSPECTED.
           05 WS-INS OCCURS 30
                                   PIC X(4).

      *> -- Transporter hash --
       01  WS-T-HASH               PIC X(40).

      *> -- Drop point --
       01  WS-DROP-COL             PIC 9(2).
       01  WS-DROP-ROW             PIC 9(2).
       01  WS-DROP-FOUND           PIC X.

      *> -- Distance calculations --
       01  WS-BEST-DIST            PIC 9(5).
       01  WS-CUR-DIST             PIC 9(5).
       01  WS-BEST-IDX             PIC 9(2).
       01  WS-SUM-DIST             PIC 9(5).
       01  WS-DIFF                 PIC S9(5).

      *> -- Cell name builder --
       01  WS-CELL-NAME            PIC X(4).
       01  WS-CELL-COL             PIC 9(2).
       01  WS-CELL-ROW             PIC 9(2).
       01  WS-ROW-NUM              PIC Z9.

      *> -- Object parsing --
       01  WS-OBJ-POS              PIC 9(5).
       01  WS-OBJ-HASH             PIC X(40).
       01  WS-OBJ-TYP              PIC X(20).
       01  WS-OBJ-POSN             PIC X(10).
       01  WS-IS-KNOWN             PIC X.
       01  WS-WANT-TYPE            PIC X(20).
       01  WS-NEW-CT               PIC 9(2).

      *> -- Log text for LLM --
       01  WS-LOG-TEXT              PIC X(8000).
       01  WS-LOG-PTR              PIC 9(5).

      *> -- Found cell from LLM --
       01  WS-FOUND-CELL           PIC X(4).

      *> -- Scout count per cluster --
       01  WS-N-SCOUTS             PIC 9(2).

      *> -- JBUF save/restore --
       01  WS-JBUF-SAVE            PIC X(32000).
       01  WS-JLEN-SAVE            PIC 9(5).

      *> -- Mdist point B coords --
       01  WS-CHK-COL              PIC 9(2).
       01  WS-CHK-ROW              PIC 9(2).

      *> -- Hub action name --
       01  WS-ACTION-NAME          PIC X(20).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S04E03 DOMATOWO ==="

           PERFORM LOAD-ENV-VARS

      *>   Step 1: Reset
           DISPLAY " "
           DISPLAY "[1] Reset"
           PERFORM HUB-RESET

      *>   Step 2: Get map and parse
           DISPLAY "[2] Get map"
           PERFORM HUB-GET-MAP
           PERFORM PARSE-GRID
           PERFORM FIND-ROADS-TARGETS
           DISPLAY "  Map: " WS-MAP-SIZE
               "x" WS-MAP-SIZE
           DISPLAY "  Roads: " WS-ROAD-CT
           DISPLAY "  Targets: " WS-TGT-CT

      *>   Display target cells
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-TGT-CT
               MOVE WS-TG-COL(WS-I)
                   TO WS-CELL-COL
               MOVE WS-TG-ROW(WS-I)
                   TO WS-CELL-ROW
               PERFORM CELL-TO-NAME
               DISPLAY "    Target: "
                   TRIM(WS-CELL-NAME)
           END-PERFORM

      *>   Step 3: Cluster targets
           DISPLAY "[3] Cluster"
           PERFORM CLUSTER-TARGETS
           DISPLAY "  Clusters: "
               WS-CLUST-CT

      *>   Step 4: Process each cluster
           DISPLAY "[4] Execute"
           PERFORM PROCESS-CLUSTERS

      *>   Step 5: Get logs and analyze
           DISPLAY "[5] Analyze ("
               WS-INSP-CT " cells)"
           PERFORM HUB-GET-LOGS
           PERFORM BUILD-LOG-TEXT
           PERFORM LLM-FIND-PERSON

      *>   Step 6: Call helicopter
           IF TRIM(WS-FOUND-CELL)
               NOT = SPACES
               DISPLAY "  Helicopter -> "
                   TRIM(WS-FOUND-CELL)
               MOVE TRIM(WS-FOUND-CELL)
                   TO WS-CELL-NAME
               PERFORM HUB-HELICOPTER
               PERFORM CHECK-FLAG
           END-IF

      *>   Safety net: try each inspected cell
           IF WS-FLAG-FOUND = "N"
               DISPLAY "[Safety net]"
               PERFORM VARYING WS-I
                   FROM 1 BY 1
                   UNTIL WS-I > WS-INSP-CT
                   OR WS-FLAG-FOUND = "Y"
                   MOVE WS-INS(WS-I)
                       TO WS-CELL-NAME
                   DISPLAY "  Try "
                       TRIM(WS-CELL-NAME)
                   PERFORM HUB-HELICOPTER
                   PERFORM CHECK-FLAG
               END-PERFORM
           END-IF

           DISPLAY " "
           IF WS-FLAG-FOUND = "Y"
               DISPLAY ">>> FLAG FOUND <<<"
           ELSE
               DISPLAY "No flag found."
           END-IF
           DISPLAY "=== DONE ==="
           STOP RUN.

      *> ============================================================
      *> PARSE-GRID: Parse grid array from getMap JSON
      *> ============================================================
       PARSE-GRID.
      *>   Parse map size first
           MOVE "size" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF TRIM(WS-JVAL) NOT = SPACES
               COMPUTE WS-MAP-SIZE =
                   NUMVAL(TRIM(WS-JVAL))
           END-IF

      *>   Find "grid" key in JBUF
           MOVE SPACES TO WS-TMP2
           STRING WS-QT "grid" WS-QT
               DELIMITED SIZE
               INTO WS-TMP2
           END-STRING

           MOVE 0 TO WS-KEY-POS
           MOVE LENGTH(TRIM(WS-TMP2))
               TO WS-K
           PERFORM VARYING WS-GP-POS
               FROM 1 BY 1
               UNTIL WS-GP-POS > WS-JLEN
               OR WS-KEY-POS > 0
               IF WS-GP-POS + WS-K - 1
                   <= WS-JLEN
                   IF WS-JBUF(
                       WS-GP-POS:WS-K)
                       = TRIM(WS-TMP2)
                       MOVE WS-GP-POS
                           TO WS-KEY-POS
                   END-IF
               END-IF
           END-PERFORM

           IF WS-KEY-POS = 0
               DISPLAY "  ERR: no grid!"
               EXIT PARAGRAPH
           END-IF

      *>   Skip past "grid": to first [
           COMPUTE WS-GP-POS =
               WS-KEY-POS + WS-K
           PERFORM UNTIL
               WS-GP-POS > WS-JLEN
               OR WS-JBUF(WS-GP-POS:1)
               = "["
               ADD 1 TO WS-GP-POS
           END-PERFORM

      *>   Parse nested arrays
           MOVE 0 TO WS-GP-DEPTH
           MOVE 1 TO WS-GP-ROW
           MOVE 1 TO WS-GP-COL
           MOVE "N" TO WS-GP-DONE

           PERFORM UNTIL
               WS-GP-POS > WS-JLEN
               OR WS-GP-DONE = "Y"
               MOVE WS-JBUF(WS-GP-POS:1)
                   TO WS-GP-CH

               EVALUATE TRUE
               WHEN WS-GP-CH = "["
                   ADD 1 TO WS-GP-DEPTH

               WHEN WS-GP-CH = "]"
                   SUBTRACT 1
                       FROM WS-GP-DEPTH
                   IF WS-GP-DEPTH = 1
                       ADD 1 TO WS-GP-ROW
                       MOVE 1 TO WS-GP-COL
                   END-IF
                   IF WS-GP-DEPTH = 0
                       MOVE "Y"
                           TO WS-GP-DONE
                   END-IF

               WHEN WS-GP-CH = WS-QT
                   IF WS-GP-DEPTH = 2
      *>               Read quoted string
                       ADD 1 TO WS-GP-POS
                       MOVE SPACES
                           TO WS-GP-STR
                       MOVE 0
                           TO WS-GP-SLEN
                       PERFORM UNTIL
                           WS-GP-POS
                           > WS-JLEN
                           OR WS-JBUF(
                           WS-GP-POS:1)
                           = WS-QT
                           IF WS-GP-SLEN
                               < 10
                               ADD 1
                                 TO WS-GP-SLEN
                               MOVE WS-JBUF(
                                 WS-GP-POS:1)
                                 TO WS-GP-STR(
                                 WS-GP-SLEN:1)
                           END-IF
                           ADD 1
                               TO WS-GP-POS
                       END-PERFORM
      *>               Store in grid
                       IF WS-GP-ROW <= 11
                       AND WS-GP-COL <= 11
                           MOVE WS-GP-STR
                               TO WS-GCELL(
                               WS-GP-ROW,
                               WS-GP-COL)
                       END-IF
                       ADD 1 TO WS-GP-COL
                   END-IF

               END-EVALUATE

               ADD 1 TO WS-GP-POS
           END-PERFORM
           .

      *> ============================================================
      *> FIND-ROADS-TARGETS: Analyze grid
      *> ============================================================
       FIND-ROADS-TARGETS.
      *>   Pass 1: find roads and max building ht
           MOVE 0 TO WS-MAX-HEIGHT
           MOVE 0 TO WS-ROAD-CT

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-MAP-SIZE
               PERFORM VARYING WS-J
                   FROM 1 BY 1
                   UNTIL WS-J > WS-MAP-SIZE

      *>           Check for road
                   IF TRIM(WS-GCELL(
                       WS-I, WS-J))
                       = "road"
                       ADD 1 TO WS-ROAD-CT
                       MOVE WS-J
                           TO WS-RD-COL(
                           WS-ROAD-CT)
                       MOVE WS-I
                           TO WS-RD-ROW(
                           WS-ROAD-CT)
                   END-IF

      *>           Scan tile name for digit
                   MOVE 0 TO WS-CUR-HEIGHT
                   MOVE WS-GCELL(WS-I, WS-J)
                       TO WS-GP-STR
                   PERFORM VARYING WS-K
                       FROM 1 BY 1
                       UNTIL WS-K > 10
                       OR WS-GP-STR(WS-K:1)
                       = SPACE
                       IF WS-GP-STR(WS-K:1)
                           >= "0"
                       AND WS-GP-STR(WS-K:1)
                           <= "9"
                           COMPUTE
                             WS-CUR-HEIGHT =
                             ORD(WS-GP-STR(
                             WS-K:1))
                             - ORD("0")
                           EXIT PERFORM
                       END-IF
                   END-PERFORM

                   IF WS-CUR-HEIGHT
                       > WS-MAX-HEIGHT
                       MOVE WS-CUR-HEIGHT
                           TO WS-MAX-HEIGHT
                   END-IF
               END-PERFORM
           END-PERFORM

      *>   Pass 2: cells with max height = targets
           MOVE 0 TO WS-TGT-CT
           IF WS-MAX-HEIGHT = 0
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-MAP-SIZE
               PERFORM VARYING WS-J
                   FROM 1 BY 1
                   UNTIL WS-J > WS-MAP-SIZE

                   MOVE 0 TO WS-CUR-HEIGHT
                   MOVE WS-GCELL(WS-I, WS-J)
                       TO WS-GP-STR
                   PERFORM VARYING WS-K
                       FROM 1 BY 1
                       UNTIL WS-K > 10
                       OR WS-GP-STR(WS-K:1)
                       = SPACE
                       IF WS-GP-STR(WS-K:1)
                           >= "0"
                       AND WS-GP-STR(WS-K:1)
                           <= "9"
                           COMPUTE
                             WS-CUR-HEIGHT =
                             ORD(WS-GP-STR(
                             WS-K:1))
                             - ORD("0")
                           EXIT PERFORM
                       END-IF
                   END-PERFORM

                   IF WS-CUR-HEIGHT
                       = WS-MAX-HEIGHT
                       ADD 1 TO WS-TGT-CT
                       MOVE WS-J
                           TO WS-TG-COL(
                           WS-TGT-CT)
                       MOVE WS-I
                           TO WS-TG-ROW(
                           WS-TGT-CT)
                       MOVE 0
                           TO WS-TG-CLUST(
                           WS-TGT-CT)
                       MOVE "N"
                           TO WS-TG-PAIRED(
                           WS-TGT-CT)
                   END-IF
               END-PERFORM
           END-PERFORM
           .

      *> ============================================================
      *> CLUSTER-TARGETS: Group nearby targets
      *> ============================================================
       CLUSTER-TARGETS.
           MOVE 0 TO WS-CLUST-CT

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-TGT-CT
               IF WS-TG-CLUST(WS-I) = 0
                   ADD 1 TO WS-CLUST-CT
                   MOVE WS-CLUST-CT
                       TO WS-TG-CLUST(WS-I)
                   PERFORM EXPAND-CLUSTER
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> EXPAND-CLUSTER: Add nearby unassigned targets
      *> ============================================================
       EXPAND-CLUSTER.
           MOVE "Y" TO WS-CL-CHANGED
           PERFORM UNTIL
               WS-CL-CHANGED = "N"
               MOVE "N" TO WS-CL-CHANGED
               PERFORM VARYING WS-J
                   FROM 1 BY 1
                   UNTIL WS-J > WS-TGT-CT
                   IF WS-TG-CLUST(WS-J) = 0
                       PERFORM
                           CHK-NEAR-CLUSTER
                   END-IF
               END-PERFORM
           END-PERFORM
           .

      *> ============================================================
      *> CHK-NEAR-CLUSTER: Check if target J is near
      *> ============================================================
       CHK-NEAR-CLUSTER.
           PERFORM VARYING WS-K FROM 1 BY 1
               UNTIL WS-K > WS-TGT-CT
               IF WS-TG-CLUST(WS-K)
                   = WS-CLUST-CT
      *>           Manhattan distance
                   MOVE WS-TG-COL(WS-J)
                       TO WS-CELL-COL
                   MOVE WS-TG-ROW(WS-J)
                       TO WS-CELL-ROW
                   MOVE WS-TG-COL(WS-K)
                       TO WS-CHK-COL
                   MOVE WS-TG-ROW(WS-K)
                       TO WS-CHK-ROW
                   PERFORM CALC-MDIST
                   IF WS-CUR-DIST <= 4
                       MOVE WS-CLUST-CT
                           TO WS-TG-CLUST(
                           WS-J)
                       MOVE "Y"
                           TO WS-CL-CHANGED
                       EXIT PERFORM
                   END-IF
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> PROCESS-CLUSTERS
      *> ============================================================
       PROCESS-CLUSTERS.
           PERFORM VARYING WS-CL-ID
               FROM 1 BY 1
               UNTIL WS-CL-ID > WS-CLUST-CT
               PERFORM PROCESS-ONE-CLUSTER
           END-PERFORM
           .

      *> ============================================================
      *> PROCESS-ONE-CLUSTER
      *> ============================================================
       PROCESS-ONE-CLUSTER.
      *>   1. Collect targets for this cluster
           PERFORM COLLECT-CLUSTER-TGTS
           DISPLAY "  [Cluster " WS-CL-ID
               "] " WS-CL-TGT-CT " targets"

      *>   2. Pair targets
           PERFORM PAIR-CL-TARGETS
           MOVE WS-PAIR-CT TO WS-N-SCOUTS
           DISPLAY "    " WS-N-SCOUTS
               " scouts needed"

      *>   3. Find drop point
           PERFORM FIND-DROP-POINT
           IF WS-DROP-FOUND = "N"
               DISPLAY "    WARN: no drop!"
               EXIT PARAGRAPH
           END-IF
           MOVE WS-DROP-COL TO WS-CELL-COL
           MOVE WS-DROP-ROW TO WS-CELL-ROW
           PERFORM CELL-TO-NAME
           DISPLAY "    Drop: "
               TRIM(WS-CELL-NAME)

      *>   4. Create transporter
           PERFORM HUB-CREATE-TRANSPORT
           MOVE "transporter"
               TO WS-WANT-TYPE
           PERFORM GET-NEW-UNITS
           IF WS-NEW-CT = 0
               DISPLAY "    WARN: create fail"
               EXIT PARAGRAPH
           END-IF

      *>   5. Move transporter to drop
           MOVE WS-T-HASH TO WS-OBJ-HASH
           MOVE WS-DROP-COL TO WS-CELL-COL
           MOVE WS-DROP-ROW TO WS-CELL-ROW
           PERFORM CELL-TO-NAME
           PERFORM HUB-MOVE-UNIT

      *>   6. Dismount all scouts
           PERFORM HUB-DISMOUNT
           MOVE "scout" TO WS-WANT-TYPE
           PERFORM GET-NEW-UNITS
           DISPLAY "    Scouts: " WS-SC-CT

      *>   7. Move scouts and inspect
           PERFORM ASSIGN-AND-INSPECT
           .

      *> ============================================================
      *> COLLECT-CLUSTER-TGTS
      *> ============================================================
       COLLECT-CLUSTER-TGTS.
           MOVE 0 TO WS-CL-TGT-CT
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-TGT-CT
               IF WS-TG-CLUST(WS-I)
                   = WS-CL-ID
                   ADD 1 TO WS-CL-TGT-CT
                   MOVE WS-I TO WS-CL-TG(
                       WS-CL-TGT-CT)
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> PAIR-CL-TARGETS: Greedy nearest-neighbor
      *> ============================================================
       PAIR-CL-TARGETS.
           MOVE 0 TO WS-PAIR-CT
      *>   Reset paired flags
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-CL-TGT-CT
               MOVE WS-CL-TG(WS-I) TO WS-M
               MOVE "N"
                   TO WS-TG-PAIRED(WS-M)
           END-PERFORM

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-CL-TGT-CT
               MOVE WS-CL-TG(WS-I) TO WS-J
               IF WS-TG-PAIRED(WS-J) = "N"
                   MOVE "Y"
                       TO WS-TG-PAIRED(WS-J)
                   ADD 1 TO WS-PAIR-CT
                   MOVE WS-J
                       TO WS-PR-T1(
                       WS-PAIR-CT)
                   MOVE 0
                       TO WS-PR-T2(
                       WS-PAIR-CT)

      *>           Find nearest unpaired
                   MOVE 99999
                       TO WS-BEST-DIST
                   MOVE 0 TO WS-BEST-IDX
                   PERFORM VARYING WS-K
                       FROM 1 BY 1
                       UNTIL WS-K
                       > WS-CL-TGT-CT
                       MOVE WS-CL-TG(WS-K)
                           TO WS-M
                       IF WS-TG-PAIRED(
                           WS-M) = "N"
                           MOVE WS-TG-COL(
                               WS-J)
                               TO WS-CELL-COL
                           MOVE WS-TG-ROW(
                               WS-J)
                               TO WS-CELL-ROW
                           MOVE WS-TG-COL(
                               WS-M)
                               TO WS-CHK-COL
                           MOVE WS-TG-ROW(
                               WS-M)
                               TO WS-CHK-ROW
                           PERFORM CALC-MDIST
                           IF WS-CUR-DIST
                               < WS-BEST-DIST
                               MOVE
                                 WS-CUR-DIST
                                 TO
                                 WS-BEST-DIST
                               MOVE WS-M
                                 TO
                                 WS-BEST-IDX
                           END-IF
                       END-IF
                   END-PERFORM

                   IF WS-BEST-IDX > 0
                       MOVE WS-BEST-IDX
                           TO WS-PR-T2(
                           WS-PAIR-CT)
                       MOVE "Y"
                           TO WS-TG-PAIRED(
                           WS-BEST-IDX)
                   END-IF
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> FIND-DROP-POINT: Road minimizing total dist
      *> ============================================================
       FIND-DROP-POINT.
           MOVE "N" TO WS-DROP-FOUND
           MOVE 99999 TO WS-BEST-DIST

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-ROAD-CT
               MOVE 0 TO WS-SUM-DIST
               PERFORM VARYING WS-K
                   FROM 1 BY 1
                   UNTIL WS-K > WS-CL-TGT-CT
                   MOVE WS-CL-TG(WS-K)
                       TO WS-M
                   MOVE WS-RD-COL(WS-I)
                       TO WS-CELL-COL
                   MOVE WS-RD-ROW(WS-I)
                       TO WS-CELL-ROW
                   MOVE WS-TG-COL(WS-M)
                       TO WS-CHK-COL
                   MOVE WS-TG-ROW(WS-M)
                       TO WS-CHK-ROW
                   PERFORM CALC-MDIST
                   ADD WS-CUR-DIST
                       TO WS-SUM-DIST
               END-PERFORM

               IF WS-SUM-DIST
                   < WS-BEST-DIST
                   MOVE WS-SUM-DIST
                       TO WS-BEST-DIST
                   MOVE WS-RD-COL(WS-I)
                       TO WS-DROP-COL
                   MOVE WS-RD-ROW(WS-I)
                       TO WS-DROP-ROW
                   MOVE "Y"
                       TO WS-DROP-FOUND
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> GET-NEW-UNITS: getObjects + find new
      *> ============================================================
       GET-NEW-UNITS.
           PERFORM HUB-GET-OBJECTS
           MOVE 0 TO WS-NEW-CT
           IF TRIM(WS-WANT-TYPE) = "scout"
               MOVE 0 TO WS-SC-CT
           END-IF

      *>   Find "objects" array
           MOVE "objects" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-ARRAY-START
           IF WS-OBJ-POS = 0
               EXIT PARAGRAPH
           END-IF

      *>   Iterate through objects
           PERFORM UNTIL
               WS-OBJ-POS > WS-JLEN
      *>       Find next { or ]
               PERFORM UNTIL
                   WS-OBJ-POS > WS-JLEN
                   OR WS-JBUF(
                   WS-OBJ-POS:1) = "{"
                   OR WS-JBUF(
                   WS-OBJ-POS:1) = "]"
                   ADD 1 TO WS-OBJ-POS
               END-PERFORM

               IF WS-OBJ-POS > WS-JLEN
               OR WS-JBUF(
                   WS-OBJ-POS:1) = "]"
                   EXIT PERFORM
               END-IF

      *>       Extract id
               MOVE "id" TO WS-KEY-SEARCH
               MOVE WS-OBJ-POS
                   TO WS-JPOS
               PERFORM FIND-JSON-VAL
               MOVE TRIM(WS-JVAL)
                   TO WS-OBJ-HASH

      *>       Extract typ
               MOVE "typ" TO WS-KEY-SEARCH
               MOVE WS-OBJ-POS
                   TO WS-JPOS
               PERFORM FIND-JSON-VAL
               MOVE TRIM(WS-JVAL)
                   TO WS-OBJ-TYP

      *>       Extract position
               MOVE "position"
                   TO WS-KEY-SEARCH
               MOVE WS-OBJ-POS
                   TO WS-JPOS
               PERFORM FIND-JSON-VAL
               MOVE TRIM(WS-JVAL)
                   TO WS-OBJ-POSN

      *>       Skip to }
               PERFORM UNTIL
                   WS-OBJ-POS > WS-JLEN
                   OR WS-JBUF(
                   WS-OBJ-POS:1) = "}"
                   ADD 1 TO WS-OBJ-POS
               END-PERFORM
               ADD 1 TO WS-OBJ-POS

      *>       Check if already known
               MOVE "N" TO WS-IS-KNOWN
               PERFORM VARYING WS-K
                   FROM 1 BY 1
                   UNTIL WS-K > WS-KNOWN-CT
                   IF WS-KH(WS-K)
                       = WS-OBJ-HASH
                       MOVE "Y"
                           TO WS-IS-KNOWN
                       EXIT PERFORM
                   END-IF
               END-PERFORM

               IF WS-IS-KNOWN = "N"
               AND TRIM(WS-OBJ-TYP) =
                   TRIM(WS-WANT-TYPE)
      *>           Add to known set
                   ADD 1 TO WS-KNOWN-CT
                   MOVE WS-OBJ-HASH
                       TO WS-KH(
                       WS-KNOWN-CT)
                   ADD 1 TO WS-NEW-CT

                   IF TRIM(WS-WANT-TYPE)
                       = "transporter"
                       MOVE WS-OBJ-HASH
                           TO WS-T-HASH
                   ELSE
      *>               Scout
                       ADD 1 TO WS-SC-CT
                       MOVE WS-OBJ-HASH
                           TO WS-SC-HASH(
                           WS-SC-CT)
                       PERFORM
                           PARSE-POS-TO-CR
                   END-IF
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> PARSE-POS-TO-CR: "A6" -> col,row
      *> ============================================================
       PARSE-POS-TO-CR.
           MOVE 0 TO WS-SC-COL(WS-SC-CT)
           MOVE 0 TO WS-SC-ROW(WS-SC-CT)

      *>   Column letter -> index
           PERFORM VARYING WS-K FROM 1 BY 1
               UNTIL WS-K > 11
               IF WS-COLS(WS-K:1) =
                   WS-OBJ-POSN(1:1)
                   MOVE WS-K
                       TO WS-SC-COL(
                       WS-SC-CT)
                   EXIT PERFORM
               END-IF
           END-PERFORM

      *>   Row number
           MOVE SPACES TO WS-TMP2
           MOVE WS-OBJ-POSN(2:3)
               TO WS-TMP2
           IF TRIM(WS-TMP2) NOT = SPACES
               COMPUTE WS-SC-ROW(WS-SC-CT)
                   = NUMVAL(TRIM(WS-TMP2))
           END-IF
           .

      *> ============================================================
      *> ASSIGN-AND-INSPECT
      *> ============================================================
       ASSIGN-AND-INSPECT.
           MOVE 1 TO WS-J
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-PAIR-CT
               IF WS-J <= WS-SC-CT
      *>           Scout hash for this pair
                   MOVE WS-SC-HASH(WS-J)
                       TO WS-OBJ-HASH

      *>           Target 1
                   MOVE WS-PR-T1(WS-I)
                       TO WS-M
                   MOVE WS-TG-COL(WS-M)
                       TO WS-CELL-COL
                   MOVE WS-TG-ROW(WS-M)
                       TO WS-CELL-ROW
                   PERFORM CELL-TO-NAME
                   PERFORM HUB-MOVE-UNIT
                   PERFORM HUB-INSPECT
                   ADD 1 TO WS-INSP-CT
                   MOVE WS-CELL-NAME
                       TO WS-INS(
                       WS-INSP-CT)
                   DISPLAY "    Inspected "
                       TRIM(WS-CELL-NAME)

      *>           Target 2 (if paired)
                   IF WS-PR-T2(WS-I) > 0
                       MOVE WS-PR-T2(WS-I)
                           TO WS-M
                       MOVE WS-TG-COL(WS-M)
                           TO WS-CELL-COL
                       MOVE WS-TG-ROW(WS-M)
                           TO WS-CELL-ROW
                       PERFORM CELL-TO-NAME
                       PERFORM HUB-MOVE-UNIT
                       PERFORM HUB-INSPECT
                       ADD 1 TO WS-INSP-CT
                       MOVE WS-CELL-NAME
                           TO WS-INS(
                           WS-INSP-CT)
                       DISPLAY
                           "    Inspected "
                           TRIM(WS-CELL-NAME)
                   END-IF

                   ADD 1 TO WS-J
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> BUILD-LOG-TEXT: Parse getLogs -> text
      *> ============================================================
       BUILD-LOG-TEXT.
           MOVE SPACES TO WS-LOG-TEXT
           MOVE 1 TO WS-LOG-PTR

           MOVE "logs" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-ARRAY-START
           IF WS-OBJ-POS = 0
               DISPLAY "  No logs found!"
               EXIT PARAGRAPH
           END-IF

      *>   Iterate log entries
           PERFORM UNTIL
               WS-OBJ-POS > WS-JLEN
      *>       Find next { or ]
               PERFORM UNTIL
                   WS-OBJ-POS > WS-JLEN
                   OR WS-JBUF(
                   WS-OBJ-POS:1) = "{"
                   OR WS-JBUF(
                   WS-OBJ-POS:1) = "]"
                   ADD 1 TO WS-OBJ-POS
               END-PERFORM

               IF WS-OBJ-POS > WS-JLEN
               OR WS-JBUF(
                   WS-OBJ-POS:1) = "]"
                   EXIT PERFORM
               END-IF

      *>       Extract field
               MOVE "field"
                   TO WS-KEY-SEARCH
               MOVE WS-OBJ-POS
                   TO WS-JPOS
               PERFORM FIND-JSON-VAL
               MOVE TRIM(WS-JVAL)
                   TO WS-TMP

      *>       Extract msg
               MOVE "msg"
                   TO WS-KEY-SEARCH
               MOVE WS-OBJ-POS
                   TO WS-JPOS
               PERFORM FIND-JSON-VAL

      *>       Append: "FIELD: MSG\n"
               IF WS-LOG-PTR < 7900
                   STRING
                       TRIM(WS-TMP) ": "
                       TRIM(WS-JVAL)
                       X"0A"
                       DELIMITED SIZE
                       INTO WS-LOG-TEXT
                       WITH POINTER
                       WS-LOG-PTR
                   END-STRING
               END-IF

      *>       Skip to }
               PERFORM UNTIL
                   WS-OBJ-POS > WS-JLEN
                   OR WS-JBUF(
                   WS-OBJ-POS:1) = "}"
                   ADD 1 TO WS-OBJ-POS
               END-PERFORM
               ADD 1 TO WS-OBJ-POS
           END-PERFORM

           DISPLAY "  Log entries built"
           .

      *> ============================================================
      *> LLM-FIND-PERSON: AI analysis of logs
      *> ============================================================
       LLM-FIND-PERSON.
           MOVE SPACES TO WS-FOUND-CELL

      *>   Escape log text for JSON
           MOVE WS-LOG-TEXT TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

      *>   Build LLM request JSON
           MOVE SPACES TO WS-REQ-JSON
           MOVE 1 TO WS-PTR

      *>   Opening + model + messages start
           STRING
               "{" WS-QT "model" WS-QT ":"
               WS-QT "gpt-4.1-mini" WS-QT
               "," WS-QT "messages" WS-QT
               ":[{" WS-QT "role" WS-QT ":"
               WS-QT "system" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   System prompt
           STRING
               "Analyze scout reports."
               " Exactly one mentions"
               " finding a living"
               " person."
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               " Return JSON: {"
               WS-BS WS-QT "cell"
               WS-BS WS-QT ": "
               WS-BS WS-QT "X1"
               WS-BS WS-QT
               "} with that cell."
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Close system, open user
           STRING
               WS-QT "},{" WS-QT
               "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Escaped log text
           IF WS-ESC-OLEN > 0
               STRING
                   WS-ESC-OUT(
                   1:WS-ESC-OLEN)
                   DELIMITED SIZE
                   INTO WS-REQ-JSON
                   WITH POINTER WS-PTR
               END-STRING
           END-IF

      *>   Close user + settings
           STRING
               WS-QT "}],"
               WS-QT "temperature" WS-QT
               ":0,"
               WS-QT "max_tokens" WS-QT
               ":50,"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "response_format"
               WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "json_object"
               WS-QT "}}"
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
           WRITE WORK-REC FROM WS-REQ-JSON
           CLOSE WORK-FILE

      *>   Send via curl
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
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           MOVE LENGTH(TRIM(WS-CMD))
               TO WS-PTR
           ADD 1 TO WS-PTR
           STRING
               " -H " WS-QT
               "Authorization: Bearer "
               TRIM(WS-OPENAI-KEY)
               WS-QT
               " -d @llm_req.json"
               DELIMITED SIZE
               INTO WS-CMD
               WITH POINTER WS-PTR
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Read response
           MOVE "llm_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH
           DISPLAY "  LLM resp: "
               WS-JBUF(1:200)

      *>   Extract content from LLM response
           MOVE "content" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL

      *>   Unescape the content JSON
           MOVE WS-JVAL TO WS-ESC-IN
           PERFORM JSON-UNESCAPE-STR

      *>   Parse cell from unescaped JSON
           IF WS-ESC-OLEN > 0
               MOVE WS-ESC-OUT(
                   1:WS-ESC-OLEN)
                   TO WS-JBUF
               MOVE WS-ESC-OLEN
                   TO WS-JLEN
               MOVE "cell"
                   TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               MOVE TRIM(WS-JVAL)
                   TO WS-FOUND-CELL
           END-IF

           DISPLAY "  Found: "
               TRIM(WS-FOUND-CELL)
           .

      *> ============================================================
      *> CHECK-FLAG
      *> ============================================================
       CHECK-FLAG.
           MOVE 0 TO WS-TALLY-CNT
           IF WS-JLEN > 0
               INSPECT WS-JBUF(1:WS-JLEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL "FLG"
           END-IF
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-FLAG-FOUND
               DISPLAY "  FLAG: "
                   WS-JBUF(1:500)
           END-IF
           .

      *> ============================================================
      *> CELL-TO-NAME: col,row -> "A1".."K11"
      *> ============================================================
       CELL-TO-NAME.
           MOVE SPACES TO WS-CELL-NAME
           MOVE WS-CELL-ROW TO WS-ROW-NUM
           MOVE 1 TO WS-PTR
           STRING
               WS-COLS(WS-CELL-COL:1)
               TRIM(WS-ROW-NUM)
               DELIMITED SIZE
               INTO WS-CELL-NAME
               WITH POINTER WS-PTR
           END-STRING
           .

      *> ============================================================
      *> CALC-MDIST: Manhattan distance
      *> Input: WS-CELL-COL/ROW, WS-CHK-COL/ROW
      *> Output: WS-CUR-DIST
      *> ============================================================
       CALC-MDIST.
           COMPUTE WS-DIFF =
               WS-CELL-COL - WS-CHK-COL
           IF WS-DIFF < 0
               COMPUTE WS-DIFF =
                   0 - WS-DIFF
           END-IF
           MOVE WS-DIFF TO WS-CUR-DIST
           COMPUTE WS-DIFF =
               WS-CELL-ROW - WS-CHK-ROW
           IF WS-DIFF < 0
               COMPUTE WS-DIFF =
                   0 - WS-DIFF
           END-IF
           ADD WS-DIFF TO WS-CUR-DIST
           .

      *> ============================================================
      *> FIND-ARRAY-START: Find "key":[ position
      *> Input: WS-KEY-SEARCH, WS-JPOS
      *> Output: WS-OBJ-POS (past [, 0=not found)
      *> ============================================================
       FIND-ARRAY-START.
           MOVE SPACES TO WS-TMP2
           STRING WS-QT
               TRIM(WS-KEY-SEARCH)
               WS-QT
               DELIMITED SIZE
               INTO WS-TMP2
           END-STRING

           MOVE 0 TO WS-OBJ-POS
           MOVE LENGTH(TRIM(WS-TMP2))
               TO WS-K
           PERFORM VARYING WS-I
               FROM WS-JPOS BY 1
               UNTIL WS-I > WS-JLEN
               OR WS-OBJ-POS > 0
               IF WS-I + WS-K - 1
                   <= WS-JLEN
                   IF WS-JBUF(WS-I:WS-K)
                       = TRIM(WS-TMP2)
                       COMPUTE WS-OBJ-POS
                           = WS-I + WS-K
                   END-IF
               END-IF
           END-PERFORM

           IF WS-OBJ-POS = 0
               EXIT PARAGRAPH
           END-IF

      *>   Skip to [ and past it
           PERFORM UNTIL
               WS-OBJ-POS > WS-JLEN
               OR WS-JBUF(
               WS-OBJ-POS:1) = "["
               ADD 1 TO WS-OBJ-POS
           END-PERFORM
           ADD 1 TO WS-OBJ-POS
           .

      *> ============================================================
      *> BUILD-HUB-PREFIX: Common JSON prefix
      *> ============================================================
       BUILD-HUB-PREFIX.
           MOVE SPACES TO WS-HUB-BODY
           MOVE 1 TO WS-PTR
           STRING
               "{" WS-QT "apikey" WS-QT
               ":" WS-QT
               TRIM(WS-HUB-KEY)
               WS-QT "," WS-QT "task"
               WS-QT ":" WS-QT
               TRIM(WS-TASK-NAME)
               WS-QT "," WS-QT "answer"
               WS-QT ":{"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING
           .

      *> ============================================================
      *> HUB-SIMPLE: Simple action (no params)
      *> ============================================================
       HUB-SIMPLE.
           PERFORM BUILD-HUB-PREFIX
           STRING
               WS-QT "action" WS-QT ":"
               WS-QT
               TRIM(WS-ACTION-NAME)
               WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING
           PERFORM SEND-HUB-REQUEST
           .

       HUB-RESET.
           MOVE "reset" TO WS-ACTION-NAME
           PERFORM HUB-SIMPLE
           .

       HUB-GET-MAP.
           MOVE "getMap" TO WS-ACTION-NAME
           PERFORM HUB-SIMPLE
           .

       HUB-GET-OBJECTS.
           MOVE "getObjects"
               TO WS-ACTION-NAME
           PERFORM HUB-SIMPLE
           .

       HUB-GET-LOGS.
           MOVE "getLogs"
               TO WS-ACTION-NAME
           PERFORM HUB-SIMPLE
           .

      *> ============================================================
      *> HUB-CREATE-TRANSPORT
      *> ============================================================
       HUB-CREATE-TRANSPORT.
           PERFORM BUILD-HUB-PREFIX
           MOVE WS-N-SCOUTS TO WS-ROW-NUM
           STRING
               WS-QT "action" WS-QT ":"
               WS-QT "create" WS-QT ","
               WS-QT "type" WS-QT ":"
               WS-QT "transporter"
               WS-QT "," WS-QT
               "passengers" WS-QT ":"
               TRIM(WS-ROW-NUM)
               "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING
           PERFORM SEND-HUB-REQUEST
           .

      *> ============================================================
      *> HUB-MOVE-UNIT: move object to cell
      *> Uses WS-OBJ-HASH and WS-CELL-NAME
      *> ============================================================
       HUB-MOVE-UNIT.
           PERFORM BUILD-HUB-PREFIX
           STRING
               WS-QT "action" WS-QT ":"
               WS-QT "move" WS-QT ","
               WS-QT "object" WS-QT ":"
               WS-QT TRIM(WS-OBJ-HASH)
               WS-QT "," WS-QT "where"
               WS-QT ":" WS-QT
               TRIM(WS-CELL-NAME)
               WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING
           PERFORM SEND-HUB-REQUEST
           .

      *> ============================================================
      *> HUB-INSPECT: inspect with scout
      *> Uses WS-OBJ-HASH
      *> ============================================================
       HUB-INSPECT.
           PERFORM BUILD-HUB-PREFIX
           STRING
               WS-QT "action" WS-QT ":"
               WS-QT "inspect" WS-QT ","
               WS-QT "object" WS-QT ":"
               WS-QT TRIM(WS-OBJ-HASH)
               WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING
           PERFORM SEND-HUB-REQUEST
           .

      *> ============================================================
      *> HUB-DISMOUNT: dismount scouts
      *> Uses WS-T-HASH and WS-N-SCOUTS
      *> ============================================================
       HUB-DISMOUNT.
           PERFORM BUILD-HUB-PREFIX
           MOVE WS-N-SCOUTS TO WS-ROW-NUM
           STRING
               WS-QT "action" WS-QT ":"
               WS-QT "dismount" WS-QT ","
               WS-QT "object" WS-QT ":"
               WS-QT TRIM(WS-T-HASH)
               WS-QT "," WS-QT
               "passengers" WS-QT ":"
               TRIM(WS-ROW-NUM)
               "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING
           PERFORM SEND-HUB-REQUEST
           .

      *> ============================================================
      *> HUB-HELICOPTER
      *> Uses WS-CELL-NAME
      *> ============================================================
       HUB-HELICOPTER.
           PERFORM BUILD-HUB-PREFIX
           STRING
               WS-QT "action" WS-QT ":"
               WS-QT "callHelicopter"
               WS-QT "," WS-QT
               "destination" WS-QT ":"
               WS-QT
               TRIM(WS-CELL-NAME)
               WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING
           PERFORM SEND-HUB-REQUEST
           .

       COPY HUBSUBMIT-PROC.
       COPY JSONREAD-PROC.
       COPY JSONPARSE-PROC.
       COPY JSONESCAPE-PROC.
       COPY JSONUNESCAPE-PROC.

       COPY ENVLOAD-PROC.
