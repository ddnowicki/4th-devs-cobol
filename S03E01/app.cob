       IDENTIFICATION DIVISION.
       PROGRAM-ID. S03E01-SENSORS.
      *> ============================================================
      *> S03E01 - Sensor Anomaly Detection (Pure COBOL)
      *> 1. Download & extract sensors.zip
      *> 2. Programmatic anomaly detection on ~10k JSON files
      *> 3. LLM classification of operator notes (deduplicated)
      *> 4. Submit combined anomaly list to /verify
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
       01  WS-WORK-PATH            PIC X(200)
                                   VALUE "work.tmp".

      *> === HTTP ===
       01  WS-CMD                  PIC X(4000).
       01  WS-REQ-JSON             PIC X(64000).

      *> === JSON Parsing (copybook) ===
       COPY JSONPARSE-WS.
       COPY JSONREAD-WS.

      *> === JSON Escape (copybook) ===
       COPY JSONESCAPE-WS.

      *> === Task Configuration ===
       01  WS-BATCH-SIZE           PIC 9(3) VALUE 50.
       01  WS-MAX-LLM              PIC 9(3) VALUE 200.
       01  WS-MAX-FLAGS            PIC 9(5)
                                   VALUE 15000.
       01  WS-MAX-VALID            PIC 9(5)
                                   VALUE 15000.
       01  WS-MAX-UNIQ             PIC 9(5)
                                   VALUE 2000.

      *> === Task Data ===
       01  WS-RESP-BUF             PIC X(32000).
       01  WS-RESP-LEN             PIC 9(5).
       01  WS-ZIP-EXISTS           PIC X VALUE "N".
       01  WS-FILE-LINE            PIC X(300).
       01  WS-FILE-ID              PIC X(20).
       01  WS-SENSOR-TYPE          PIC X(200).
       01  WS-SENSOR-LOW           PIC X(200).
       01  WS-NOTES-RAW            PIC X(1000).
       01  WS-VAL-STR              PIC X(30).
       01  WS-TEMP-K               PIC S9(6)V9(4).
       01  WS-PRESS-BAR            PIC S9(6)V9(4).
       01  WS-WATER-M              PIC S9(6)V9(4).
       01  WS-VOLT-V               PIC S9(6)V9(4).
       01  WS-HUMID-PCT            PIC S9(6)V9(4).
       01  WS-ACT-TEMP             PIC X VALUE "N".
       01  WS-ACT-PRESS            PIC X VALUE "N".
       01  WS-ACT-WATER            PIC X VALUE "N".
       01  WS-ACT-VOLT             PIC X VALUE "N".
       01  WS-ACT-HUMID            PIC X VALUE "N".
       01  WS-IS-ANOMALY           PIC X VALUE "N".
       01  WS-TOTAL-FILES          PIC 9(5) VALUE 0.
       01  WS-FLAGGED-DATA         PIC 9(5) VALUE 0.
       01  WS-VALID-DATA           PIC 9(5) VALUE 0.
       01  WS-FLAG-COUNT           PIC 9(5) VALUE 0.
       01  WS-FLAG-IDS.
           05 WS-FLAG-ID OCCURS 15000 TIMES
                                   PIC X(20).
       01  WS-VLD-COUNT            PIC 9(5) VALUE 0.
       01  WS-VLD-ENTRIES.
           05 WS-VLD-ENT OCCURS 15000 TIMES.
              10 WS-VLD-ID         PIC X(20).
              10 WS-VLD-NOTE       PIC X(500).
       01  WS-UNIQ-COUNT           PIC 9(5) VALUE 0.
       01  WS-UNIQ-NOTES.
           05 WS-UNIQ-ENT OCCURS 2000 TIMES.
              10 WS-UNIQ-TEXT      PIC X(500).
              10 WS-UNIQ-CLASS     PIC X(10).
       01  WS-NOTE-FLAG-CT         PIC 9(5) VALUE 0.
       01  WS-NOTE-FLAGS.
           05 WS-NOTE-FLG OCCURS 5000 TIMES
                                   PIC X(20).
       01  WS-COMBINED-CT          PIC 9(5) VALUE 0.
       01  WS-COMBINED-IDS.
           05 WS-COMB-ID OCCURS 15000 TIMES
                                   PIC X(20).
       01  WS-ANS-BUF              PIC X(64000).
       01  WS-ANS-PTR              PIC 9(5).
       01  WS-FIRST-ANS            PIC X VALUE "Y".
       01  WS-STYPE-POS            PIC 9(3).
       01  WS-STYPE-LEN            PIC 9(3).
       01  WS-STYPE-PART           PIC X(30).
       01  WS-STYPE-PARTL          PIC X(30).
       01  WS-FILE-NUM             PIC 9(5).
       01  WS-FILE-NUM-STR         PIC 9(4).
       01  WS-GEN-PATH             PIC X(30).

      *> === Control Flow ===
       01  WS-PTR                  PIC 9(5).
       01  WS-I                    PIC 9(5).
       01  WS-J                    PIC 9(5).
       01  WS-N                    PIC 9(5).
       01  WS-TALLY-CNT            PIC 9(5).
       01  WS-FOUND-FLAG           PIC X VALUE "N".
       01  WS-BATCH-START          PIC 9(5).
       01  WS-BATCH-END            PIC 9(5).
       01  WS-LLM-CALLS            PIC 9(3) VALUE 0.
       01  WS-SORT-TEMP            PIC X(20).
       01  WS-SORT-I               PIC 9(5).
       01  WS-SORT-J               PIC 9(5).
       01  WS-SORT-SWAPPED         PIC X.
       01  WS-RETRY-CT             PIC 9(2) VALUE 0.
       01  WS-SLEEP-SECS           PIC 9(8).
       01  WS-DISP-NUM             PIC Z(4)9.
       01  WS-NUMVAL-ERR           PIC X VALUE "N".
       01  WS-PART-COUNT           PIC 9(2).

      *> -- ZIP extraction via POSIX FFI --
       01  WS-ZIP-FD         PIC S9(9) COMP-5.
       01  WS-OUT-FD         PIC S9(9) COMP-5.
       01  WS-ZIP-PATH       PIC X(30).
       01  WS-OUT-PATH       PIC X(60).
       01  WS-BYTES-READ
                             PIC S9(9) COMP-5.
       01  WS-ZIP-HDR.
           05  WS-ZIP-SIG   PIC X(4).
           05  WS-ZIP-VER
                             PIC 9(4) COMP-5.
           05  WS-ZIP-FLAGS
                             PIC 9(4) COMP-5.
           05  WS-ZIP-METHOD
                             PIC 9(4) COMP-5.
           05  WS-ZIP-MTIME
                             PIC 9(4) COMP-5.
           05  WS-ZIP-MDATE
                             PIC 9(4) COMP-5.
           05  WS-ZIP-CRC
                             PIC 9(9) COMP-5.
           05  WS-ZIP-CSIZE
                             PIC 9(9) COMP-5.
           05  WS-ZIP-USIZE
                             PIC 9(9) COMP-5.
           05  WS-ZIP-NLEN
                             PIC 9(4) COMP-5.
           05  WS-ZIP-XLEN
                             PIC 9(4) COMP-5.
       01  WS-ZIP-FNAME      PIC X(256).
       01  WS-ZIP-BNAME      PIC X(256).
       01  WS-ZIP-EXTRA      PIC X(1024).
       01  WS-COMP-BUF       PIC X(64000).
       01  WS-DECOMP-BUF     PIC X(64000).
       01  WS-ZIP-DONE        PIC X VALUE "N".
       01  WS-SLASH-POS       PIC 9(3).
       01  WS-BNAME-I         PIC 9(3).
       01  WS-MKDIR-RC
                             PIC S9(9) COMP-5.
       01  WS-EXTRACTED-CT    PIC 9(5) VALUE 0.

      *> -- zlib z_stream (64-bit Linux, 112 bytes) --
      *> Must match C struct layout with padding
       01  WS-ZSTREAM.
           05  WS-Z-NEXT-IN
                          USAGE POINTER.
           05  WS-Z-AVAIL-IN
                          PIC 9(9) COMP-5.
           05  WS-Z-PAD1  PIC X(4).
           05  WS-Z-TOTAL-IN
                          PIC 9(18) COMP-5.
           05  WS-Z-NEXT-OUT
                          USAGE POINTER.
           05  WS-Z-AVAIL-OUT
                          PIC 9(9) COMP-5.
           05  WS-Z-PAD2  PIC X(4).
           05  WS-Z-TOTAL-OUT
                          PIC 9(18) COMP-5.
           05  WS-Z-MSG
                          USAGE POINTER.
           05  WS-Z-STATE
                          USAGE POINTER.
           05  WS-Z-ZALLOC
                          USAGE POINTER.
           05  WS-Z-ZFREE
                          USAGE POINTER.
           05  WS-Z-OPAQUE
                          USAGE POINTER.
           05  WS-Z-DATA-TYPE
                          PIC S9(9) COMP-5.
           05  WS-Z-PAD3  PIC X(4).
           05  WS-Z-ADLER
                          PIC 9(18) COMP-5.
           05  WS-Z-RESERVED
                          PIC 9(18) COMP-5.
       01  WS-Z-RET
                          PIC S9(9) COMP-5.
       01  WS-ZLIB-VER    PIC X(10)
                          VALUE Z"1.3.1".
       01  WS-ZVER-PTR    USAGE POINTER.
       01  WS-ZVER-LEN    PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S03E01 SENSORS ==="

           PERFORM LOAD-ENV-VARS

      *>   Phase 1: Download & Extract
           PERFORM PHASE1-DOWNLOAD

      *>   Phase 2: Programmatic checks
           PERFORM PHASE2-CHECK-FILES

      *>   Phase 3: LLM note classification
           PERFORM PHASE3-CLASSIFY-NOTES

      *>   Phase 4: Submit
           PERFORM PHASE4-SUBMIT

           DISPLAY " "
           DISPLAY "=== DONE ==="
           STOP RUN.

      *> ============================================================
      *> PHASE 1: Download & extract sensors.zip
      *> ============================================================
       PHASE1-DOWNLOAD.
           DISPLAY " "
           DISPLAY "[Phase 1] Download & extract"

      *>   Check if data exists already
           MOVE "sensors/0001.json"
               TO WS-WORK-PATH
           OPEN INPUT WORK-FILE
           IF WS-FS = "00"
               CLOSE WORK-FILE
               DISPLAY "  Data exists."
               MOVE "work.tmp"
                   TO WS-WORK-PATH
               EXIT PARAGRAPH
           END-IF

      *>   Download sensors.zip
           DISPLAY "  Downloading..."
           INITIALIZE WS-CMD
           STRING
               "curl -s -o "
               "sensors.zip "
               TRIM(WS-HUB-URL)
               "/dane/sensors.zip"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Create sensors directory
           CALL "CBL_CREATE_DIR"
               USING "sensors"

      *>   Extract ZIP via POSIX+zlib
           DISPLAY "  Extracting..."
           PERFORM EXTRACT-ZIP
           MOVE WS-EXTRACTED-CT
               TO WS-DISP-NUM
           DISPLAY "  Extracted "
               TRIM(WS-DISP-NUM)
               " files."
           MOVE "work.tmp"
               TO WS-WORK-PATH
           .

      *> ============================================================
      *> PHASE 2: Programmatic anomaly detection
      *> ============================================================
       PHASE2-CHECK-FILES.
           DISPLAY " "
           DISPLAY "[Phase 2] Checking files"

           MOVE 0 TO WS-TOTAL-FILES
           MOVE 0 TO WS-FLAGGED-DATA
           MOVE 0 TO WS-VALID-DATA
           MOVE 0 TO WS-FLAG-COUNT
           MOVE 0 TO WS-VLD-COUNT

      *>   Iterate numerically 0001-9999
           PERFORM VARYING WS-FILE-NUM
               FROM 1 BY 1
               UNTIL WS-FILE-NUM > 9999

               MOVE WS-FILE-NUM
                   TO WS-FILE-NUM-STR
               MOVE SPACES
                   TO WS-GEN-PATH
               STRING
                   "sensors/"
                   WS-FILE-NUM-STR
                   ".json"
                   DELIMITED SIZE
                   INTO WS-GEN-PATH
               END-STRING
               MOVE WS-GEN-PATH
                   TO WS-WORK-PATH

               OPEN INPUT WORK-FILE
               IF WS-FS = "00"
                   CLOSE WORK-FILE
                   MOVE TRIM(WS-GEN-PATH)
                       TO WS-FILE-LINE
                   ADD 1
                       TO WS-TOTAL-FILES
                   PERFORM
                       PROCESS-ONE-FILE
               END-IF
           END-PERFORM

           MOVE WS-TOTAL-FILES
               TO WS-DISP-NUM
           DISPLAY "  Total: "
               TRIM(WS-DISP-NUM)
           MOVE WS-FLAGGED-DATA
               TO WS-DISP-NUM
           DISPLAY "  Flagged(data): "
               TRIM(WS-DISP-NUM)
           MOVE WS-VALID-DATA
               TO WS-DISP-NUM
           DISPLAY "  Valid: "
               TRIM(WS-DISP-NUM)

           MOVE "work.tmp"
               TO WS-WORK-PATH
           .

      *> ============================================================
      *> EXTRACT-ZIP: Open ZIP, parse local headers
      *> ============================================================
       EXTRACT-ZIP.
           MOVE 0 TO WS-EXTRACTED-CT
           MOVE "N" TO WS-ZIP-DONE
           MOVE "sensors.zip" & X"00"
               TO WS-ZIP-PATH

      *>   Open ZIP for binary reading
           CALL "open" USING
               WS-ZIP-PATH
               BY VALUE 0
               RETURNING WS-ZIP-FD
           IF WS-ZIP-FD < 0
               DISPLAY "  ERR: open zip"
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL
               WS-ZIP-DONE = "Y"
      *>       Read 30-byte header
               CALL "read" USING
                   BY VALUE WS-ZIP-FD
                   BY REFERENCE
                   WS-ZIP-HDR
                   BY VALUE 30
                   RETURNING
                   WS-BYTES-READ
               IF WS-BYTES-READ < 30
                   MOVE "Y"
                       TO WS-ZIP-DONE
                   EXIT PERFORM
               END-IF

      *>       Check PK\x03\x04 sig
               IF WS-ZIP-SIG NOT =
                   X"504B0304"
                   MOVE "Y"
                       TO WS-ZIP-DONE
                   EXIT PERFORM
               END-IF

      *>       Read filename
               IF WS-ZIP-NLEN > 0
               AND WS-ZIP-NLEN
                   <= 256
                   MOVE SPACES
                       TO WS-ZIP-FNAME
                   CALL "read" USING
                       BY VALUE
                       WS-ZIP-FD
                       BY REFERENCE
                       WS-ZIP-FNAME
                       BY VALUE
                       WS-ZIP-NLEN
                       RETURNING
                       WS-BYTES-READ
               ELSE
                   MOVE "Y"
                       TO WS-ZIP-DONE
                   EXIT PERFORM
               END-IF

      *>       Skip extra field
               IF WS-ZIP-XLEN > 0
                   CALL "read" USING
                       BY VALUE
                       WS-ZIP-FD
                       BY REFERENCE
                       WS-ZIP-EXTRA
                       BY VALUE
                       WS-ZIP-XLEN
                       RETURNING
                       WS-BYTES-READ
               END-IF

      *>       Read compressed data
               IF WS-ZIP-CSIZE > 0
               AND WS-ZIP-CSIZE
                   <= 64000
                   MOVE SPACES
                       TO WS-COMP-BUF
                   CALL "read" USING
                       BY VALUE
                       WS-ZIP-FD
                       BY REFERENCE
                       WS-COMP-BUF
                       BY VALUE
                       WS-ZIP-CSIZE
                       RETURNING
                       WS-BYTES-READ
               ELSE
                   IF WS-ZIP-CSIZE > 0
                       MOVE "Y"
                           TO WS-ZIP-DONE
                       EXIT PERFORM
                   END-IF
               END-IF

      *>       Extract basename
               PERFORM
                   GET-ZIP-BASENAME

      *>       Skip directories
               IF TRIM(WS-ZIP-BNAME)
                   = SPACES
                   EXIT PERFORM
                       CYCLE
               END-IF

      *>       Build output path
               MOVE SPACES
                   TO WS-OUT-PATH
               STRING
                   "sensors/"
                   TRIM(WS-ZIP-BNAME)
                   X"00"
                   DELIMITED SIZE
                   INTO WS-OUT-PATH
               END-STRING

      *>       Decompress or store
               IF WS-ZIP-METHOD = 0
                   PERFORM
                       ZIP-WRITE-STORED
               ELSE
                   IF WS-ZIP-METHOD
                       = 8
                       PERFORM
                          ZIP-INFLATE
                   END-IF
               END-IF
           END-PERFORM

           CALL "close" USING
               BY VALUE WS-ZIP-FD
           .

      *> ============================================================
      *> GET-ZIP-BASENAME: strip path
      *> ============================================================
       GET-ZIP-BASENAME.
           MOVE SPACES
               TO WS-ZIP-BNAME
           MOVE 0 TO WS-SLASH-POS

      *>   Find last /
           PERFORM VARYING WS-BNAME-I
               FROM 1 BY 1
               UNTIL WS-BNAME-I
                   > WS-ZIP-NLEN
               IF WS-ZIP-FNAME(
                   WS-BNAME-I:1) = "/"
               OR WS-ZIP-FNAME(
                   WS-BNAME-I:1) = X"5C"
                   MOVE WS-BNAME-I
                       TO WS-SLASH-POS
               END-IF
           END-PERFORM

           IF WS-SLASH-POS > 0
               ADD 1 TO WS-SLASH-POS
               IF WS-SLASH-POS
                   <= WS-ZIP-NLEN
                   COMPUTE WS-K =
                       WS-ZIP-NLEN
                       - WS-SLASH-POS
                       + 1
                   MOVE WS-ZIP-FNAME(
                       WS-SLASH-POS:
                       WS-K)
                       TO WS-ZIP-BNAME
               END-IF
           ELSE
               MOVE WS-ZIP-FNAME(
                   1:WS-ZIP-NLEN)
                   TO WS-ZIP-BNAME
           END-IF
           .

      *> ============================================================
      *> ZIP-WRITE-STORED: write uncompressed
      *> ============================================================
       ZIP-WRITE-STORED.
           CALL "open" USING
               WS-OUT-PATH
               BY VALUE 577
               BY VALUE 420
               RETURNING WS-OUT-FD
           IF WS-OUT-FD >= 0
               IF WS-ZIP-USIZE > 0
                   CALL "write" USING
                       BY VALUE
                       WS-OUT-FD
                       BY REFERENCE
                       WS-COMP-BUF
                       BY VALUE
                       WS-ZIP-USIZE
               END-IF
               CALL "close" USING
                   BY VALUE WS-OUT-FD
               ADD 1
                   TO WS-EXTRACTED-CT
           END-IF
           .

      *> ============================================================
      *> ZIP-INFLATE: decompress deflate
      *> ============================================================
       ZIP-INFLATE.
           MOVE SPACES
               TO WS-DECOMP-BUF
           INITIALIZE WS-ZSTREAM

      *>   inflateInit2 with raw=-15
           CALL "inflateInit2_" USING
               WS-ZSTREAM
               BY VALUE -15
               BY REFERENCE
               WS-ZLIB-VER
               BY VALUE
               LENGTH(WS-ZSTREAM)
               RETURNING WS-Z-RET
           IF WS-Z-RET NOT = 0
               DISPLAY "  zlib init:"
                   WS-Z-RET
               EXIT PARAGRAPH
           END-IF

      *>   Setup buffers
           SET WS-Z-NEXT-IN TO
               ADDRESS OF
               WS-COMP-BUF
           MOVE WS-ZIP-CSIZE
               TO WS-Z-AVAIL-IN
           SET WS-Z-NEXT-OUT TO
               ADDRESS OF
               WS-DECOMP-BUF
           MOVE 64000
               TO WS-Z-AVAIL-OUT

      *>   Inflate with Z_FINISH=4
           CALL "inflate" USING
               WS-ZSTREAM
               BY VALUE 4
               RETURNING WS-Z-RET

      *>   Cleanup zlib
           CALL "inflateEnd" USING
               WS-ZSTREAM

      *>   Write output file
           CALL "open" USING
               WS-OUT-PATH
               BY VALUE 577
               BY VALUE 420
               RETURNING WS-OUT-FD
           IF WS-OUT-FD >= 0
               IF WS-ZIP-USIZE > 0
                   CALL "write" USING
                       BY VALUE
                       WS-OUT-FD
                       BY REFERENCE
                       WS-DECOMP-BUF
                       BY VALUE
                       WS-ZIP-USIZE
               END-IF
               CALL "close" USING
                   BY VALUE WS-OUT-FD
               ADD 1
                   TO WS-EXTRACTED-CT
           END-IF
           .

      *> ============================================================
      *> PROCESS-ONE-FILE: Read a sensor JSON, check ranges
      *> ============================================================
       PROCESS-ONE-FILE.
      *>   Extract file ID from path
      *>   e.g. sensors/0001.json -> 0001
           PERFORM EXTRACT-FILE-ID

      *>   Read the JSON file content
           MOVE TRIM(WS-FILE-LINE)
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp"
               TO WS-WORK-PATH

           IF WS-JLEN = 0
      *>       Empty/unreadable = anomaly
               ADD 1 TO WS-FLAGGED-DATA
               PERFORM ADD-DATA-FLAG
               EXIT PARAGRAPH
           END-IF

      *>   Parse sensor_type
           MOVE "sensor_type"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE WS-JVAL TO WS-SENSOR-TYPE

      *>   Parse active sensors from type
           PERFORM PARSE-SENSOR-TYPE

      *>   Parse all sensor values
           PERFORM PARSE-SENSOR-VALUES

      *>   Parse operator_notes
           MOVE "operator_notes"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE WS-JVAL TO WS-NOTES-RAW

      *>   Check ranges
           MOVE "N" TO WS-IS-ANOMALY
           PERFORM CHECK-SENSOR-RANGES

           IF WS-IS-ANOMALY = "Y"
               ADD 1 TO WS-FLAGGED-DATA
               PERFORM ADD-DATA-FLAG
           ELSE
               ADD 1 TO WS-VALID-DATA
               PERFORM ADD-VALID-ENTRY
           END-IF
           .

      *> ============================================================
      *> EXTRACT-FILE-ID: Get ID from path
      *> e.g. "sensors/0001.json" -> "0001"
      *> ============================================================
       EXTRACT-FILE-ID.
           MOVE SPACES TO WS-FILE-ID
           MOVE TRIM(WS-FILE-LINE)
               TO WS-TMP

      *>   Find last / position
           MOVE 0 TO WS-I
           MOVE LENGTH(TRIM(WS-TMP))
               TO WS-N
           PERFORM VARYING WS-J
               FROM 1 BY 1
               UNTIL WS-J > WS-N
               IF WS-TMP(WS-J:1) = "/"
               OR WS-TMP(WS-J:1) = X"5C"
                   MOVE WS-J TO WS-I
               END-IF
           END-PERFORM

      *>   Extract filename after last /
           ADD 1 TO WS-I
           MOVE SPACES TO WS-FILE-ID

      *>   Copy up to .json
           MOVE 1 TO WS-K
           PERFORM UNTIL WS-I > WS-N
               IF WS-TMP(WS-I:1) = "."
                   EXIT PERFORM
               END-IF
               MOVE WS-TMP(WS-I:1)
                   TO WS-FILE-ID(WS-K:1)
               ADD 1 TO WS-I
               ADD 1 TO WS-K
           END-PERFORM
           .

      *> ============================================================
      *> PARSE-SENSOR-TYPE: Set active flags
      *> ============================================================
       PARSE-SENSOR-TYPE.
           MOVE "N" TO WS-ACT-TEMP
           MOVE "N" TO WS-ACT-PRESS
           MOVE "N" TO WS-ACT-WATER
           MOVE "N" TO WS-ACT-VOLT
           MOVE "N" TO WS-ACT-HUMID

      *>   Convert to lower for matching
           MOVE LOWER-CASE(
               TRIM(WS-SENSOR-TYPE))
               TO WS-SENSOR-LOW

           MOVE LENGTH(
               TRIM(WS-SENSOR-LOW))
               TO WS-STYPE-LEN

      *>   Split by / and check each part
           MOVE 1 TO WS-STYPE-POS
           PERFORM UNTIL
               WS-STYPE-POS > WS-STYPE-LEN
      *>       Find next /
               MOVE SPACES
                   TO WS-STYPE-PART
               MOVE 1 TO WS-K
               PERFORM UNTIL
                   WS-STYPE-POS
                   > WS-STYPE-LEN
                   IF WS-SENSOR-LOW(
                       WS-STYPE-POS:1)
                       = "/"
                       ADD 1
                           TO WS-STYPE-POS
                       EXIT PERFORM
                   END-IF
                   MOVE WS-SENSOR-LOW(
                       WS-STYPE-POS:1)
                       TO WS-STYPE-PART(
                       WS-K:1)
                   ADD 1
                       TO WS-STYPE-POS
                   ADD 1 TO WS-K
               END-PERFORM

               MOVE TRIM(WS-STYPE-PART)
                   TO WS-STYPE-PARTL

               PERFORM CHECK-SENSOR-PART
           END-PERFORM
           .

      *> ============================================================
      *> CHECK-SENSOR-PART: Match a sensor type keyword
      *> ============================================================
       CHECK-SENSOR-PART.
           IF WS-STYPE-PARTL = SPACES
               EXIT PARAGRAPH
           END-IF

           IF WS-STYPE-PARTL =
               "temperature"
               MOVE "Y" TO WS-ACT-TEMP
           END-IF
           IF WS-STYPE-PARTL =
               "pressure"
               MOVE "Y" TO WS-ACT-PRESS
           END-IF
           IF WS-STYPE-PARTL =
               "water_level"
           OR WS-STYPE-PARTL =
               "water"
               MOVE "Y" TO WS-ACT-WATER
           END-IF
           IF WS-STYPE-PARTL =
               "voltage"
               MOVE "Y" TO WS-ACT-VOLT
           END-IF
           IF WS-STYPE-PARTL =
               "humidity"
               MOVE "Y" TO WS-ACT-HUMID
           END-IF

      *>   Partial matching
           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-STYPE-PARTL
               TALLYING WS-TALLY-CNT
               FOR ALL "temperatur"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-ACT-TEMP
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-STYPE-PARTL
               TALLYING WS-TALLY-CNT
               FOR ALL "pressur"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-ACT-PRESS
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-STYPE-PARTL
               TALLYING WS-TALLY-CNT
               FOR ALL "water"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-ACT-WATER
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-STYPE-PARTL
               TALLYING WS-TALLY-CNT
               FOR ALL "voltag"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-ACT-VOLT
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-STYPE-PARTL
               TALLYING WS-TALLY-CNT
               FOR ALL "humid"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-ACT-HUMID
           END-IF
           .

      *> ============================================================
      *> PARSE-SENSOR-VALUES: Extract numeric values
      *> ============================================================
       PARSE-SENSOR-VALUES.
           MOVE 0 TO WS-TEMP-K
           MOVE 0 TO WS-PRESS-BAR
           MOVE 0 TO WS-WATER-M
           MOVE 0 TO WS-VOLT-V
           MOVE 0 TO WS-HUMID-PCT

      *>   temperature_K
           MOVE "temperature_K"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF TRIM(WS-JVAL)
               NOT = SPACES
               PERFORM SAFE-NUMVAL-TEMP
           END-IF

      *>   pressure_bar
           MOVE "pressure_bar"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF TRIM(WS-JVAL)
               NOT = SPACES
               PERFORM SAFE-NUMVAL-PRESS
           END-IF

      *>   water_level_meters
           MOVE "water_level_meters"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF TRIM(WS-JVAL)
               NOT = SPACES
               PERFORM SAFE-NUMVAL-WATER
           END-IF

      *>   voltage_supply_v
           MOVE "voltage_supply_v"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF TRIM(WS-JVAL)
               NOT = SPACES
               PERFORM SAFE-NUMVAL-VOLT
           END-IF

      *>   humidity_percent
           MOVE "humidity_percent"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF TRIM(WS-JVAL)
               NOT = SPACES
               PERFORM SAFE-NUMVAL-HUMID
           END-IF
           .

      *> -- Safe NUMVAL wrappers --
       SAFE-NUMVAL-TEMP.
           MOVE "N" TO WS-NUMVAL-ERR
           COMPUTE WS-TEMP-K =
               NUMVAL(TRIM(WS-JVAL))
           .

       SAFE-NUMVAL-PRESS.
           MOVE "N" TO WS-NUMVAL-ERR
           COMPUTE WS-PRESS-BAR =
               NUMVAL(TRIM(WS-JVAL))
           .

       SAFE-NUMVAL-WATER.
           MOVE "N" TO WS-NUMVAL-ERR
           COMPUTE WS-WATER-M =
               NUMVAL(TRIM(WS-JVAL))
           .

       SAFE-NUMVAL-VOLT.
           MOVE "N" TO WS-NUMVAL-ERR
           COMPUTE WS-VOLT-V =
               NUMVAL(TRIM(WS-JVAL))
           .

       SAFE-NUMVAL-HUMID.
           MOVE "N" TO WS-NUMVAL-ERR
           COMPUTE WS-HUMID-PCT =
               NUMVAL(TRIM(WS-JVAL))
           .

      *> ============================================================
      *> CHECK-SENSOR-RANGES: Validate ranges
      *> ============================================================
       CHECK-SENSOR-RANGES.
      *>   Active sensors: must be in range
      *>   Inactive sensors: must be 0

      *>   temperature_K
           IF WS-ACT-TEMP = "Y"
               IF WS-TEMP-K < 553
               OR WS-TEMP-K > 873
                   MOVE "Y"
                       TO WS-IS-ANOMALY
               END-IF
           ELSE
               IF WS-TEMP-K NOT = 0
                   MOVE "Y"
                       TO WS-IS-ANOMALY
               END-IF
           END-IF

      *>   pressure_bar
           IF WS-ACT-PRESS = "Y"
               IF WS-PRESS-BAR < 60
               OR WS-PRESS-BAR > 160
                   MOVE "Y"
                       TO WS-IS-ANOMALY
               END-IF
           ELSE
               IF WS-PRESS-BAR NOT = 0
                   MOVE "Y"
                       TO WS-IS-ANOMALY
               END-IF
           END-IF

      *>   water_level_meters
           IF WS-ACT-WATER = "Y"
               IF WS-WATER-M < 5.0
               OR WS-WATER-M > 15.0
                   MOVE "Y"
                       TO WS-IS-ANOMALY
               END-IF
           ELSE
               IF WS-WATER-M NOT = 0
                   MOVE "Y"
                       TO WS-IS-ANOMALY
               END-IF
           END-IF

      *>   voltage_supply_v
           IF WS-ACT-VOLT = "Y"
               IF WS-VOLT-V < 229.0
               OR WS-VOLT-V > 231.0
                   MOVE "Y"
                       TO WS-IS-ANOMALY
               END-IF
           ELSE
               IF WS-VOLT-V NOT = 0
                   MOVE "Y"
                       TO WS-IS-ANOMALY
               END-IF
           END-IF

      *>   humidity_percent
           IF WS-ACT-HUMID = "Y"
               IF WS-HUMID-PCT < 40.0
               OR WS-HUMID-PCT > 80.0
                   MOVE "Y"
                       TO WS-IS-ANOMALY
               END-IF
           ELSE
               IF WS-HUMID-PCT NOT = 0
                   MOVE "Y"
                       TO WS-IS-ANOMALY
               END-IF
           END-IF
           .

      *> ============================================================
      *> ADD-DATA-FLAG: Add file ID to flagged list
      *> ============================================================
       ADD-DATA-FLAG.
           IF WS-FLAG-COUNT
               < WS-MAX-FLAGS
               ADD 1 TO WS-FLAG-COUNT
               MOVE TRIM(WS-FILE-ID)
                   TO WS-FLAG-ID(
                   WS-FLAG-COUNT)
           END-IF
           .

      *> ============================================================
      *> ADD-VALID-ENTRY: Add to valid list
      *> ============================================================
       ADD-VALID-ENTRY.
           IF WS-VLD-COUNT
               < WS-MAX-VALID
               ADD 1 TO WS-VLD-COUNT
               MOVE TRIM(WS-FILE-ID)
                   TO WS-VLD-ID(
                   WS-VLD-COUNT)
               MOVE TRIM(WS-NOTES-RAW)
                   TO WS-VLD-NOTE(
                   WS-VLD-COUNT)
           END-IF
           .

      *> ============================================================
      *> PHASE 3: LLM note classification
      *> ============================================================
       PHASE3-CLASSIFY-NOTES.
           DISPLAY " "
           DISPLAY "[Phase 3] Note classification"
           MOVE WS-VLD-COUNT
               TO WS-DISP-NUM
           DISPLAY "  Valid files: "
               TRIM(WS-DISP-NUM)

      *>   Step 1: Deduplicate notes
           MOVE 0 TO WS-UNIQ-COUNT
           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I > WS-VLD-COUNT
               PERFORM DEDUP-ADD-NOTE
           END-PERFORM

           MOVE WS-UNIQ-COUNT
               TO WS-DISP-NUM
           DISPLAY "  Unique notes: "
               TRIM(WS-DISP-NUM)

      *>   Step 2: Classify in batches
           MOVE 0 TO WS-LLM-CALLS
           MOVE 0 TO WS-NOTE-FLAG-CT
           MOVE 1 TO WS-BATCH-START

           PERFORM UNTIL
               WS-BATCH-START
               > WS-UNIQ-COUNT
               OR WS-LLM-CALLS
               >= WS-MAX-LLM
               COMPUTE WS-BATCH-END =
                   WS-BATCH-START
                   + WS-BATCH-SIZE - 1
               IF WS-BATCH-END
                   > WS-UNIQ-COUNT
                   MOVE WS-UNIQ-COUNT
                       TO WS-BATCH-END
               END-IF

               PERFORM CLASSIFY-BATCH

               ADD 1 TO WS-LLM-CALLS
               COMPUTE WS-BATCH-START =
                   WS-BATCH-END + 1
           END-PERFORM

      *>   Step 3: Map back to file IDs
           PERFORM MAP-NOTES-TO-FILES

           MOVE WS-NOTE-FLAG-CT
               TO WS-DISP-NUM
           DISPLAY "  Flagged(notes): "
               TRIM(WS-DISP-NUM)
           .

      *> ============================================================
      *> DEDUP-ADD-NOTE: Add note to unique if new
      *> ============================================================
       DEDUP-ADD-NOTE.
           MOVE "N" TO WS-FOUND-FLAG

           PERFORM VARYING WS-J
               FROM 1 BY 1
               UNTIL WS-J > WS-UNIQ-COUNT
               OR WS-FOUND-FLAG = "Y"
               IF TRIM(WS-VLD-NOTE(WS-I))
                   = TRIM(
                   WS-UNIQ-TEXT(WS-J))
                   MOVE "Y"
                       TO WS-FOUND-FLAG
               END-IF
           END-PERFORM

           IF WS-FOUND-FLAG = "N"
           AND WS-UNIQ-COUNT
               < WS-MAX-UNIQ
               ADD 1 TO WS-UNIQ-COUNT
               MOVE TRIM(
                   WS-VLD-NOTE(WS-I))
                   TO WS-UNIQ-TEXT(
                   WS-UNIQ-COUNT)
               MOVE "ok"
                   TO WS-UNIQ-CLASS(
                   WS-UNIQ-COUNT)
           END-IF
           .

      *> ============================================================
      *> CLASSIFY-BATCH: Send batch to LLM
      *> ============================================================
       CLASSIFY-BATCH.
           MOVE WS-LLM-CALLS
               TO WS-DISP-NUM
           DISPLAY "  Batch "
               TRIM(WS-DISP-NUM)
               " (items "
               WS-BATCH-START "-"
               WS-BATCH-END ")"

      *>   Build request JSON
           MOVE SPACES TO WS-REQ-JSON
           MOVE 1 TO WS-PTR

      *>   Opening
           STRING
               "{"
               WS-QT "model" WS-QT ":"
               WS-QT "gpt-4.1-mini"
               WS-QT ","
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "messages" WS-QT
               ":["
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   System message
           STRING
               "{"
               WS-QT "role" WS-QT ":"
               WS-QT "system" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "You classify nuclear "
               "power plant operator "
               "notes. For each note,"
               " decide:"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "- ok = operator says "
               "everything is fine, "
               "normal, stable, "
               "within range"
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "- problem = any issue"
               ", error, anomaly, "
               "warning, malfunction,"
               " unusual reading"
               WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "Respond ONLY with a "
               "JSON object mapping "
               "note number to "
               "classification."
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "Example: {"
               X"5C" WS-QT "1"
               X"5C" WS-QT ":"
               X"5C" WS-QT "ok"
               X"5C" WS-QT ","
               X"5C" WS-QT "2"
               X"5C" WS-QT ":"
               X"5C" WS-QT "problem"
               X"5C" WS-QT "}"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Close system, open user
           STRING
               WS-QT "},{"
               WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "Classify these "
               "operator notes:"
               WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Add numbered notes
           PERFORM VARYING WS-I
               FROM WS-BATCH-START BY 1
               UNTIL WS-I > WS-BATCH-END
      *>       Escape note text
               MOVE TRIM(
                   WS-UNIQ-TEXT(WS-I))
                   TO WS-ESC-IN
               MOVE LENGTH(
                   TRIM(WS-ESC-IN))
                   TO WS-ESC-ILEN
               PERFORM JSON-ESCAPE-STR

      *>       Compute note number in batch
               COMPUTE WS-N =
                   WS-I
                   - WS-BATCH-START + 1
               MOVE WS-N TO WS-DISP-NUM

               STRING
                   TRIM(WS-DISP-NUM)
                   ". "
                   WS-ESC-OUT(
                   1:WS-ESC-OLEN)
                   WS-NL
                   DELIMITED SIZE
                   INTO WS-REQ-JSON
                   WITH POINTER WS-PTR
               END-STRING
           END-PERFORM

      *>   Close user + messages + request
           STRING
               WS-QT "}],"
               WS-QT "temperature"
               WS-QT ":0,"
               WS-QT "max_tokens"
               WS-QT ":4000}"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Write to file
           MOVE "req.json"
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

      *>   Call OpenAI
           INITIALIZE WS-CMD
           STRING
               "curl -s "
               "-o resp.tmp"
               " -X POST "
               TRIM(WS-OPENAI-URL)
               " -H " WS-QT
               "Content-Type: "
               "application/json"
               WS-QT
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING

           INITIALIZE WS-TMP
           STRING
               " -H " WS-QT
               "Authorization: "
               "Bearer "
               TRIM(WS-OPENAI-KEY)
               WS-QT
               " -d @req.json"
               DELIMITED SIZE
               INTO WS-TMP
           END-STRING

           STRING TRIM(WS-CMD)
               " "
               TRIM(WS-TMP)
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING

           CALL "SYSTEM" USING WS-CMD

      *>   Parse response
           MOVE "resp.tmp"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp"
               TO WS-WORK-PATH

      *>   Extract content
           MOVE "content"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL

      *>   Parse classifications from content
           PERFORM PARSE-CLASSIFICATIONS
           .

      *> ============================================================
      *> PARSE-CLASSIFICATIONS: Parse LLM JSON
      *> response and update WS-UNIQ-CLASS
      *> ============================================================
       PARSE-CLASSIFICATIONS.
      *>   WS-JVAL has the content with
      *>   escaped JSON like {\"1\":\"ok\"...}
      *>   Unescape: remove \ before "
           MOVE TRIM(WS-JVAL)
               TO WS-ESC-IN
           MOVE LENGTH(
               TRIM(WS-ESC-IN))
               TO WS-ESC-ILEN
           MOVE SPACES TO WS-RESP-BUF
           MOVE 0 TO WS-RESP-LEN
           MOVE 1 TO WS-ESC-I
           PERFORM UNTIL
               WS-ESC-I > WS-ESC-ILEN
               IF WS-ESC-IN(
                   WS-ESC-I:1) = X"5C"
               AND WS-ESC-I
                   < WS-ESC-ILEN
                   ADD 1 TO WS-ESC-I
                   ADD 1 TO WS-RESP-LEN
                   MOVE WS-ESC-IN(
                       WS-ESC-I:1)
                       TO WS-RESP-BUF(
                       WS-RESP-LEN:1)
               ELSE
                   ADD 1 TO WS-RESP-LEN
                   MOVE WS-ESC-IN(
                       WS-ESC-I:1)
                       TO WS-RESP-BUF(
                       WS-RESP-LEN:1)
               END-IF
               ADD 1 TO WS-ESC-I
           END-PERFORM

      *>   For each item in the batch
           PERFORM VARYING WS-I
               FROM WS-BATCH-START BY 1
               UNTIL WS-I > WS-BATCH-END

               COMPUTE WS-N =
                   WS-I
                   - WS-BATCH-START + 1

      *>       Build search key like "1"
               MOVE WS-N TO WS-DISP-NUM
               MOVE SPACES
                   TO WS-KEY-SEARCH
               MOVE TRIM(WS-DISP-NUM)
                   TO WS-KEY-SEARCH

      *>       Search in response buf
      *>       using the JBUF for parsing
               MOVE WS-RESP-BUF
                   TO WS-JBUF
               MOVE WS-RESP-LEN
                   TO WS-JLEN
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL

               IF TRIM(WS-JVAL)
                   NOT = SPACES
                   MOVE LOWER-CASE(
                       TRIM(WS-JVAL))
                       TO WS-UNIQ-CLASS(
                       WS-I)
               END-IF
           END-PERFORM

      *>   Restore JBUF state
           MOVE SPACES TO WS-JBUF
           MOVE 0 TO WS-JLEN
           .

      *> ============================================================
      *> MAP-NOTES-TO-FILES: Flag valid files
      *> with "problem" notes
      *> ============================================================
       MAP-NOTES-TO-FILES.
           MOVE 0 TO WS-NOTE-FLAG-CT

           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I > WS-VLD-COUNT

      *>       Find this note in unique table
               PERFORM VARYING WS-J
                   FROM 1 BY 1
                   UNTIL WS-J
                       > WS-UNIQ-COUNT
                   IF TRIM(
                       WS-VLD-NOTE(WS-I))
                       = TRIM(
                       WS-UNIQ-TEXT(WS-J))
                       IF TRIM(
                           WS-UNIQ-CLASS(
                           WS-J))
                           = "problem"
                           ADD 1
                               TO
                               WS-NOTE-FLAG-CT
                           IF WS-NOTE-FLAG-CT
                               <= 5000
                               MOVE TRIM(
                                   WS-VLD-ID(
                                   WS-I))
                                   TO
                                   WS-NOTE-FLG(
                                   WS-NOTE-FLAG-CT)
                           END-IF
                       END-IF
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           END-PERFORM
           .

      *> ============================================================
      *> PHASE 4: Submit combined results
      *> ============================================================
       PHASE4-SUBMIT.
           DISPLAY " "
           DISPLAY "[Phase 4] Submit"

      *>   Combine Phase 2 + Phase 3 flags
           MOVE 0 TO WS-COMBINED-CT

      *>   Add Phase 2 flags
           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I > WS-FLAG-COUNT
               ADD 1 TO WS-COMBINED-CT
               MOVE TRIM(
                   WS-FLAG-ID(WS-I))
                   TO WS-COMB-ID(
                   WS-COMBINED-CT)
           END-PERFORM

      *>   Add Phase 3 flags (avoid dups)
           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I
                   > WS-NOTE-FLAG-CT
               MOVE "N" TO WS-FOUND-FLAG
               PERFORM VARYING WS-J
                   FROM 1 BY 1
                   UNTIL WS-J
                       > WS-COMBINED-CT
                   OR WS-FOUND-FLAG = "Y"
                   IF TRIM(
                       WS-NOTE-FLG(WS-I))
                       = TRIM(
                       WS-COMB-ID(WS-J))
                       MOVE "Y"
                           TO WS-FOUND-FLAG
                   END-IF
               END-PERFORM
               IF WS-FOUND-FLAG = "N"
                   ADD 1
                       TO WS-COMBINED-CT
                   MOVE TRIM(
                       WS-NOTE-FLG(WS-I))
                       TO WS-COMB-ID(
                       WS-COMBINED-CT)
               END-IF
           END-PERFORM

      *>   Sort combined IDs
           PERFORM SORT-COMBINED-IDS

           MOVE WS-COMBINED-CT
               TO WS-DISP-NUM
           DISPLAY "  Total anomalies: "
               TRIM(WS-DISP-NUM)

      *>   Build answer JSON
           MOVE SPACES TO WS-ANS-BUF
           MOVE 1 TO WS-ANS-PTR

           STRING
               "{"
               WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY)
               WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT "evaluation"
               WS-QT ","
               DELIMITED SIZE
               INTO WS-ANS-BUF
               WITH POINTER WS-ANS-PTR
           END-STRING

           STRING
               WS-QT "answer" WS-QT
               ":{"
               WS-QT "recheck" WS-QT
               ":["
               DELIMITED SIZE
               INTO WS-ANS-BUF
               WITH POINTER WS-ANS-PTR
           END-STRING

      *>   Add sorted IDs
           MOVE "Y" TO WS-FIRST-ANS
           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I
                   > WS-COMBINED-CT
               IF WS-FIRST-ANS = "N"
                   STRING ","
                       DELIMITED SIZE
                       INTO WS-ANS-BUF
                       WITH POINTER
                       WS-ANS-PTR
                   END-STRING
               END-IF
               STRING
                   WS-QT
                   TRIM(WS-COMB-ID(
                   WS-I))
                   WS-QT
                   DELIMITED SIZE
                   INTO WS-ANS-BUF
                   WITH POINTER
                   WS-ANS-PTR
               END-STRING
               MOVE "N"
                   TO WS-FIRST-ANS
           END-PERFORM

           STRING
               "]}}"
               DELIMITED SIZE
               INTO WS-ANS-BUF
               WITH POINTER WS-ANS-PTR
           END-STRING

      *>   Write and POST
           MOVE "answer.json"
               TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "ERR: OPEN "
                   TRIM(WS-WORK-PATH)
                   " FS=" WS-FS
               STOP RUN
           END-IF
           WRITE WORK-REC
               FROM WS-ANS-BUF
           CLOSE WORK-FILE

           INITIALIZE WS-CMD
           STRING
               "curl -s "
               "-o hub_resp.tmp"
               " -X POST "
               TRIM(WS-VERIFY-URL)
               " -H " WS-QT
               "Content-Type: "
               "application/json"
               WS-QT
               " -d @answer.json"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Read and check response
           MOVE "hub_resp.tmp"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp"
               TO WS-WORK-PATH

           DISPLAY "  Response: "
               TRIM(WS-JBUF)(1:500)

           MOVE 0 TO WS-TALLY-CNT
           IF WS-JLEN > 0
               INSPECT
                   WS-JBUF(1:WS-JLEN)
                   TALLYING
                   WS-TALLY-CNT
                   FOR ALL "FLG:"
           END-IF
           IF WS-TALLY-CNT > 0
               DISPLAY " "
               DISPLAY "  >>> FLAG FOUND!"
           ELSE
               DISPLAY " "
               DISPLAY "  No flag found."
           END-IF
           .

      *> ============================================================
      *> SORT-COMBINED-IDS: Bubble sort
      *> ============================================================
       SORT-COMBINED-IDS.
           IF WS-COMBINED-CT <= 1
               EXIT PARAGRAPH
           END-IF

           MOVE "Y" TO WS-SORT-SWAPPED
           PERFORM UNTIL
               WS-SORT-SWAPPED = "N"
               MOVE "N"
                   TO WS-SORT-SWAPPED
               COMPUTE WS-N =
                   WS-COMBINED-CT - 1
               PERFORM VARYING WS-SORT-I
                   FROM 1 BY 1
                   UNTIL WS-SORT-I > WS-N
                   COMPUTE WS-SORT-J =
                       WS-SORT-I + 1
                   IF WS-COMB-ID(
                       WS-SORT-I)
                       > WS-COMB-ID(
                       WS-SORT-J)
                       MOVE WS-COMB-ID(
                           WS-SORT-I)
                           TO WS-SORT-TEMP
                       MOVE WS-COMB-ID(
                           WS-SORT-J)
                           TO WS-COMB-ID(
                           WS-SORT-I)
                       MOVE WS-SORT-TEMP
                           TO WS-COMB-ID(
                           WS-SORT-J)
                       MOVE "Y"
                           TO
                           WS-SORT-SWAPPED
                   END-IF
               END-PERFORM
           END-PERFORM
           .

       COPY JSONESCAPE-PROC.

       COPY JSONREAD-PROC.

       COPY JSONPARSE-PROC.

       COPY ENVLOAD-PROC.
