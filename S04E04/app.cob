       IDENTIFICATION DIVISION.
       PROGRAM-ID. S04E04-FILESYSTEM.
      *> ============================================================
      *> S04E04 - Filesystem: organize Natan's notes
      *> 1. Download & extract ZIP with notes
      *> 2. Read all note files, escape for JSON
      *> 3. Parse transakcje.txt deterministically
      *> 4. LLM call for cities+people extraction
      *> 5. Build filesystem ops JSON, submit batch
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
       01  WORK-REC                PIC X(32000).

       WORKING-STORAGE SECTION.
      *> === Environment (via copybook) ===
       COPY ENVLOAD-WS.

      *> === File I/O ===
       01  WS-FS                   PIC XX.
       01  WS-WORK-PATH            PIC X(200)
                                   VALUE "work.tmp".

      *> === HTTP ===
       01  WS-CMD                  PIC X(4000).
       01  WS-REQ-JSON             PIC X(32000).

      *> === Shared copybooks (WS) ===
       COPY JSONPARSE-WS.
       COPY JSONREAD-WS.
       COPY JSONESCAPE-WS.

      *> === Control Flow ===
       01  WS-PTR                  PIC 9(5).
       01  WS-TALLY-CNT            PIC 9(5).

      *> === Task Data ===
      *> -- Text file buffers (escaped) --
       01  WS-OGL-BUF              PIC X(4000).
       01  WS-OGL-LEN              PIC 9(5).
       01  WS-ROZ-BUF              PIC X(4000).
       01  WS-ROZ-LEN              PIC 9(5).
       01  WS-TRN-BUF              PIC X(4000).
       01  WS-TRN-LEN              PIC 9(5).

      *> -- Raw transaction buffer --
       01  WS-TRN-RAW              PIC X(4000).
       01  WS-TRN-RAWLEN           PIC 9(5).

      *> -- Loop variables --
       01  WS-I                    PIC 9(5).
       01  WS-J                    PIC 9(5).
       01  WS-N                    PIC 9(5).

      *> -- City table --
       01  WS-CITY-COUNT           PIC 9(2) VALUE 0.
       01  WS-CITY-TABLE.
           05 WS-CITY-ENT OCCURS 10 TIMES.
              10 WS-CITY-NAME      PIC X(30).
              10 WS-CITY-DISP      PIC X(30).
              10 WS-CITY-FILE      PIC X(20).
              10 WS-CITY-GCNT      PIC 9(2)
                                   VALUE 0.
              10 WS-CITY-GOODS OCCURS 10 TIMES.
                 15 WS-CG-NAME     PIC X(20).
                 15 WS-CG-QTY      PIC 9(5).

      *> -- Person table --
       01  WS-PERS-COUNT           PIC 9(2) VALUE 0.
       01  WS-PERS-TABLE.
           05 WS-PERS-ENT OCCURS 10 TIMES.
              10 WS-PERS-FIRST     PIC X(20).
              10 WS-PERS-LAST      PIC X(20).
              10 WS-PERS-CITY      PIC X(30).
              10 WS-PERS-FILE      PIC X(20).

      *> -- Goods table (from transactions) --
       01  WS-GOOD-COUNT           PIC 9(2) VALUE 0.
       01  WS-GOOD-TABLE.
           05 WS-GOOD-ENT OCCURS 20 TIMES.
              10 WS-GOOD-NAME      PIC X(20).
              10 WS-GOOD-FILE      PIC X(20).
              10 WS-GOOD-SCNT      PIC 9(2)
                                   VALUE 0.
              10 WS-GOOD-SRC OCCURS 10 TIMES.
                 15 WS-GS-CITY     PIC X(30).

      *> -- Batch JSON buffer --
       01  WS-BATCH-BUF            PIC X(32000).
       01  WS-BATCH-PTR            PIC 9(5).

      *> -- Temp for parsing --
       01  WS-PARSE-LINE           PIC X(200).
       01  WS-PARSE-POS            PIC 9(5).
       01  WS-PARSE-WORD           PIC X(50).
       01  WS-PARSE-LEN            PIC 9(5).

      *> -- Transaction parsing --
       01  WS-TXN-SELLER           PIC X(30).
       01  WS-TXN-GOOD             PIC X(30).
       01  WS-TXN-BUYER            PIC X(30).

      *> -- Polish normalization --
       01  WS-NORM-IN              PIC X(50).
       01  WS-NORM-OUT             PIC X(50).
       01  WS-NORM-LEN             PIC 9(3).
       01  WS-NORM-OPOS            PIC 9(3).
       01  WS-NORM-BYTE1           PIC X(1).
       01  WS-NORM-BYTE2           PIC X(1).

      *> -- Numeric formatting --
       01  WS-NUM-EDITED           PIC Z(4)9.

      *> -- Content buffer for file ops --
       01  WS-CONTENT-BUF          PIC X(2000).
       01  WS-CONTENT-PTR          PIC 9(5).

      *> -- LLM response parsing --
       01  WS-LLM-RESP             PIC X(8000).
       01  WS-LLM-RLEN             PIC 9(5).
       01  WS-RESP-LINE            PIC X(500).
       01  WS-RESP-POS             PIC 9(5).
       01  WS-RESP-TYPE            PIC X(10).
       01  WS-FIELD1               PIC X(50).
       01  WS-FIELD2               PIC X(50).
       01  WS-FIELD3               PIC X(200).

      *> -- Retry --
       01  WS-ATTEMPT              PIC 9(1).
       01  WS-SUCCESS              PIC X VALUE "N".

      *> -- Misc --
       01  WS-FOUND-FLAG           PIC X VALUE "N".
       01  WS-SAN-IN               PIC X(50).
       01  WS-SAN-OUT              PIC X(20).
       01  WS-SAN-POS              PIC 9(3).
       01  WS-SAN-OPOS             PIC 9(3).
       01  WS-SAN-IDX              PIC 9(3).
       01  WS-SAN-CH               PIC X(1).
       01  WS-FIRST-FLAG           PIC X VALUE "Y".

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
       01  WS-EXTRACTED-CT    PIC 9(5) VALUE 0.

      *> -- zlib z_stream (64-bit Linux) --
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

      *> -- Plural-singular table --
       01  WS-PLUR-COUNT           PIC 9(2) VALUE 7.
       01  WS-PLUR-TABLE.
           05 WS-PLUR-ENT OCCURS 7 TIMES.
              10 WS-PLUR-FROM      PIC X(20).
              10 WS-PLUR-TO        PIC X(20).

      *> -- Temp for city lookup --
       01  WS-LOOKUP-CITY          PIC X(30).
       01  WS-LOOKUP-IDX           PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S04E04 FILESYSTEM ==="

           PERFORM LOAD-ENV-VARS

      *>   Init plural table
           PERFORM INIT-PLURAL-TABLE

      *>   Phase 1: Download + extract ZIP
           PERFORM PHASE1-DOWNLOAD

      *>   Phase 2: Read + escape text files
           PERFORM PHASE2-READ-FILES

      *>   Phase 3: Parse transactions
           PERFORM PHASE3-PARSE-TXN

      *>   Phase 4-6: LLM + submit with retry
           MOVE "N" TO WS-SUCCESS
           PERFORM VARYING WS-ATTEMPT
               FROM 1 BY 1
               UNTIL WS-ATTEMPT > 3
               OR WS-SUCCESS = "Y"
               DISPLAY " "
               DISPLAY "--- Attempt "
                   WS-ATTEMPT "/3 ---"
               PERFORM PHASE4-CALL-LLM
               PERFORM PHASE5-BUILD-BATCH
               PERFORM PHASE6-SUBMIT
               IF WS-SUCCESS NOT = "Y"
                   CALL "C$SLEEP" USING 5
               END-IF
           END-PERFORM

           IF WS-SUCCESS NOT = "Y"
               DISPLAY "  NIEPOWODZENIE."
           END-IF
           DISPLAY " "
           DISPLAY "=== ZAKONCZONO ==="
           STOP RUN.

      *> ============================================================
      *> INIT-PLURAL-TABLE
      *> ============================================================
       INIT-PLURAL-TABLE.
           MOVE "ziemniaki"
               TO WS-PLUR-FROM(1)
           MOVE "ziemniak"
               TO WS-PLUR-TO(1)
           MOVE "mlotki"
               TO WS-PLUR-FROM(2)
           MOVE "mlotek"
               TO WS-PLUR-TO(2)
           MOVE "lopaty"
               TO WS-PLUR-FROM(3)
           MOVE "lopata"
               TO WS-PLUR-TO(3)
           MOVE "chleby"
               TO WS-PLUR-FROM(4)
           MOVE "chleb"
               TO WS-PLUR-TO(4)
           MOVE "wiertarki"
               TO WS-PLUR-FROM(5)
           MOVE "wiertarka"
               TO WS-PLUR-TO(5)
           MOVE "kilofy"
               TO WS-PLUR-FROM(6)
           MOVE "kilof"
               TO WS-PLUR-TO(6)
           MOVE "koparki"
               TO WS-PLUR-FROM(7)
           MOVE "koparka"
               TO WS-PLUR-TO(7)
           .

      *> ============================================================
      *> PHASE 1: Download & extract ZIP
      *> ============================================================
       PHASE1-DOWNLOAD.
           DISPLAY " "
           DISPLAY "[Phase 1] Download & extract"

      *>   Check if files exist already
           MOVE "notes/rozmowy.txt"
               TO WS-WORK-PATH
           OPEN INPUT WORK-FILE
           IF WS-FS = "00"
               CLOSE WORK-FILE
               DISPLAY "  Data exists."
               MOVE "work.tmp"
                   TO WS-WORK-PATH
               EXIT PARAGRAPH
           END-IF

      *>   Download ZIP
           DISPLAY "  Downloading..."
           INITIALIZE WS-CMD
           STRING
               "curl -s -o "
               "natan_notes.zip "
               TRIM(WS-HUB-URL)
               "/dane/natan_notes.zip"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Create output dir
           CALL "CBL_CREATE_DIR"
               USING "notes"

      *>   Extract ZIP via system unzip
           DISPLAY "  Extracting..."
           INITIALIZE WS-CMD
           STRING
               "unzip -o -j "
               "natan_notes.zip "
               "-d notes"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD
      *>   Rename Polish-named file
           INITIALIZE WS-CMD
           STRING
               "mv notes/og"
               X"C5" X"82"
               "oszenia.txt "
               "notes/ogloszenia.txt "
               "2>/dev/null; true"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD
           DISPLAY "  Extracted via unzip."
           MOVE "work.tmp"
               TO WS-WORK-PATH
           .

      *> ============================================================
      *> PHASE 2: Read text files
      *> ============================================================
       PHASE2-READ-FILES.
           DISPLAY " "
           DISPLAY "[Phase 2] Reading files"

      *>   Ensure ASCII filename
           INITIALIZE WS-CMD
           STRING
               "copy /y "
               WS-QT "notes\og"
               X"C5" X"82"
               "oszenia.txt" WS-QT
               " "
               WS-QT "notes\o"
               "gloszenia.txt" WS-QT
               " >nul 2>nul"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Read ogloszenia.txt
           MOVE "notes/ogloszenia.txt"
               TO WS-WORK-PATH
           PERFORM READ-AND-ESCAPE
           MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
               TO WS-OGL-BUF
           MOVE WS-ESC-OLEN TO WS-OGL-LEN
           DISPLAY "  ogloszenia: "
               WS-OGL-LEN " chars"

      *>   Read rozmowy.txt
           MOVE "notes/rozmowy.txt"
               TO WS-WORK-PATH
           PERFORM READ-AND-ESCAPE
           MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
               TO WS-ROZ-BUF
           MOVE WS-ESC-OLEN TO WS-ROZ-LEN
           DISPLAY "  rozmowy: "
               WS-ROZ-LEN " chars"

      *>   Read transakcje.txt raw
           MOVE "notes/transakcje.txt"
               TO WS-WORK-PATH
           PERFORM READ-RAW-FILE
           DISPLAY "  transakcje: "
               WS-TRN-RAWLEN " chars"

      *>   Also escape transakcje for LLM
           MOVE WS-TRN-RAW(1:WS-TRN-RAWLEN)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR
           MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
               TO WS-TRN-BUF
           MOVE WS-ESC-OLEN TO WS-TRN-LEN

           MOVE "work.tmp"
               TO WS-WORK-PATH
           .

      *> ============================================================
      *> PHASE 3: Parse transactions deterministically
      *> ============================================================
       PHASE3-PARSE-TXN.
           DISPLAY " "
           DISPLAY "[Phase 3] Parsing transactions"

           MOVE 0 TO WS-GOOD-COUNT
           MOVE 1 TO WS-PARSE-POS

      *>   Scan lines in raw buffer
           PERFORM UNTIL
               WS-PARSE-POS > WS-TRN-RAWLEN

      *>       Extract one line
               MOVE SPACES TO WS-PARSE-LINE
               MOVE 1 TO WS-PARSE-LEN
               PERFORM UNTIL
                   WS-PARSE-POS
                   > WS-TRN-RAWLEN
                   IF WS-TRN-RAW(
                       WS-PARSE-POS:1)
                       = X"0A"
                       ADD 1
                           TO WS-PARSE-POS
                       EXIT PERFORM
                   END-IF
                   MOVE WS-TRN-RAW(
                       WS-PARSE-POS:1)
                       TO WS-PARSE-LINE(
                       WS-PARSE-LEN:1)
                   ADD 1 TO WS-PARSE-LEN
                   ADD 1 TO WS-PARSE-POS
               END-PERFORM
               SUBTRACT 1
                   FROM WS-PARSE-LEN

               IF WS-PARSE-LEN > 3
                   PERFORM PARSE-ONE-TXN
               END-IF
           END-PERFORM

           DISPLAY "  Found "
               WS-GOOD-COUNT " goods"

      *>   Display goods
           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I > WS-GOOD-COUNT
               DISPLAY "    "
                   TRIM(WS-GOOD-NAME(WS-I))
                   " <- "
                   WS-GOOD-SCNT(WS-I)
                   " cities"
           END-PERFORM
           .

      *> ============================================================
      *> PARSE-ONE-TXN: Parse "Seller -> good -> Buyer"
      *> ============================================================
       PARSE-ONE-TXN.
           MOVE SPACES TO WS-TXN-SELLER
           MOVE SPACES TO WS-TXN-GOOD
           MOVE SPACES TO WS-TXN-BUYER

      *>   Find first " -> "
           MOVE 0 TO WS-I
           PERFORM VARYING WS-J
               FROM 1 BY 1
               UNTIL WS-J > WS-PARSE-LEN
               OR WS-I > 0
               IF WS-J + 3 <= WS-PARSE-LEN
               AND WS-PARSE-LINE(WS-J:4)
                   = " -> "
                   MOVE WS-J TO WS-I
               END-IF
           END-PERFORM

           IF WS-I = 0
               EXIT PARAGRAPH
           END-IF

      *>   Seller = chars before first ->
           IF WS-I > 1
               MOVE WS-PARSE-LINE(
                   1:WS-I - 1)
                   TO WS-TXN-SELLER
           END-IF

      *>   Find second " -> "
           COMPUTE WS-N = WS-I + 4
           MOVE 0 TO WS-I
           PERFORM VARYING WS-J
               FROM WS-N BY 1
               UNTIL WS-J > WS-PARSE-LEN
               OR WS-I > 0
               IF WS-J + 3 <= WS-PARSE-LEN
               AND WS-PARSE-LINE(WS-J:4)
                   = " -> "
                   MOVE WS-J TO WS-I
               END-IF
           END-PERFORM

           IF WS-I = 0
               EXIT PARAGRAPH
           END-IF

      *>   Good = between the two arrows
           IF WS-I > WS-N
               COMPUTE WS-K = WS-I - WS-N
               MOVE WS-PARSE-LINE(
                   WS-N:WS-K)
                   TO WS-TXN-GOOD
           END-IF

      *>   Buyer = after second ->
           COMPUTE WS-N = WS-I + 4
           IF WS-N <= WS-PARSE-LEN
               COMPUTE WS-K =
                   WS-PARSE-LEN - WS-N + 1
               MOVE WS-PARSE-LINE(
                   WS-N:WS-K)
                   TO WS-TXN-BUYER
           END-IF

      *>   Normalize seller and good
           MOVE TRIM(WS-TXN-SELLER)
               TO WS-NORM-IN
           PERFORM NORMALIZE-POLISH
           MOVE WS-NORM-OUT
               TO WS-TXN-SELLER

           MOVE TRIM(WS-TXN-GOOD)
               TO WS-NORM-IN
           PERFORM NORMALIZE-POLISH
           MOVE WS-NORM-OUT
               TO WS-TXN-GOOD

      *>   Apply plural -> singular
           PERFORM APPLY-SINGULAR

      *>   Add to goods table
           PERFORM ADD-GOOD-SOURCE
           .

      *> ============================================================
      *> NORMALIZE-POLISH: UTF-8 diacritics -> ASCII
      *> ============================================================
       NORMALIZE-POLISH.
           MOVE SPACES TO WS-NORM-OUT
           MOVE LENGTH(TRIM(WS-NORM-IN))
               TO WS-NORM-LEN
           MOVE 1 TO WS-I
           MOVE 1 TO WS-NORM-OPOS

           PERFORM UNTIL WS-I > WS-NORM-LEN
               MOVE WS-NORM-IN(WS-I:1)
                   TO WS-NORM-BYTE1
               EVALUATE TRUE
      *>           C4 xx sequences
               WHEN WS-NORM-BYTE1 = X"C4"
                   ADD 1 TO WS-I
                   IF WS-I <= WS-NORM-LEN
                       MOVE WS-NORM-IN(
                           WS-I:1)
                           TO WS-NORM-BYTE2
                       EVALUATE TRUE
                       WHEN WS-NORM-BYTE2
                           = X"84"
                           MOVE "A"
                           TO WS-NORM-OUT(
                           WS-NORM-OPOS:1)
                       WHEN WS-NORM-BYTE2
                           = X"85"
                           MOVE "a"
                           TO WS-NORM-OUT(
                           WS-NORM-OPOS:1)
                       WHEN WS-NORM-BYTE2
                           = X"86"
                           MOVE "C"
                           TO WS-NORM-OUT(
                           WS-NORM-OPOS:1)
                       WHEN WS-NORM-BYTE2
                           = X"87"
                           MOVE "c"
                           TO WS-NORM-OUT(
                           WS-NORM-OPOS:1)
                       WHEN WS-NORM-BYTE2
                           = X"98"
                           MOVE "E"
                           TO WS-NORM-OUT(
                           WS-NORM-OPOS:1)
                       WHEN WS-NORM-BYTE2
                           = X"99"
                           MOVE "e"
                           TO WS-NORM-OUT(
                           WS-NORM-OPOS:1)
                       WHEN OTHER
                           MOVE "?"
                           TO WS-NORM-OUT(
                           WS-NORM-OPOS:1)
                       END-EVALUATE
                       ADD 1 TO WS-NORM-OPOS
                   END-IF
      *>           C5 xx sequences
               WHEN WS-NORM-BYTE1 = X"C5"
                   ADD 1 TO WS-I
                   IF WS-I <= WS-NORM-LEN
                       MOVE WS-NORM-IN(
                           WS-I:1)
                           TO WS-NORM-BYTE2
                       EVALUATE TRUE
                       WHEN WS-NORM-BYTE2
                           = X"81"
                           MOVE "L"
                           TO WS-NORM-OUT(
                           WS-NORM-OPOS:1)
                       WHEN WS-NORM-BYTE2
                           = X"82"
                           MOVE "l"
                           TO WS-NORM-OUT(
                           WS-NORM-OPOS:1)
                       WHEN WS-NORM-BYTE2
                           = X"83"
                           MOVE "N"
                           TO WS-NORM-OUT(
                           WS-NORM-OPOS:1)
                       WHEN WS-NORM-BYTE2
                           = X"84"
                           MOVE "n"
                           TO WS-NORM-OUT(
                           WS-NORM-OPOS:1)
                       WHEN WS-NORM-BYTE2
                           = X"9A"
                           MOVE "S"
                           TO WS-NORM-OUT(
                           WS-NORM-OPOS:1)
                       WHEN WS-NORM-BYTE2
                           = X"9B"
                           MOVE "s"
                           TO WS-NORM-OUT(
                           WS-NORM-OPOS:1)
                       WHEN WS-NORM-BYTE2
                           = X"B9"
                           MOVE "Z"
                           TO WS-NORM-OUT(
                           WS-NORM-OPOS:1)
                       WHEN WS-NORM-BYTE2
                           = X"BA"
                           MOVE "z"
                           TO WS-NORM-OUT(
                           WS-NORM-OPOS:1)
                       WHEN WS-NORM-BYTE2
                           = X"BB"
                           MOVE "Z"
                           TO WS-NORM-OUT(
                           WS-NORM-OPOS:1)
                       WHEN WS-NORM-BYTE2
                           = X"BC"
                           MOVE "z"
                           TO WS-NORM-OUT(
                           WS-NORM-OPOS:1)
                       WHEN OTHER
                           MOVE "?"
                           TO WS-NORM-OUT(
                           WS-NORM-OPOS:1)
                       END-EVALUATE
                       ADD 1 TO WS-NORM-OPOS
                   END-IF
      *>           C3 xx sequences (o-acute)
               WHEN WS-NORM-BYTE1 = X"C3"
                   ADD 1 TO WS-I
                   IF WS-I <= WS-NORM-LEN
                       MOVE WS-NORM-IN(
                           WS-I:1)
                           TO WS-NORM-BYTE2
                       EVALUATE TRUE
                       WHEN WS-NORM-BYTE2
                           = X"93"
                           MOVE "O"
                           TO WS-NORM-OUT(
                           WS-NORM-OPOS:1)
                       WHEN WS-NORM-BYTE2
                           = X"B3"
                           MOVE "o"
                           TO WS-NORM-OUT(
                           WS-NORM-OPOS:1)
                       WHEN OTHER
                           MOVE WS-NORM-BYTE2
                           TO WS-NORM-OUT(
                           WS-NORM-OPOS:1)
                       END-EVALUATE
                       ADD 1 TO WS-NORM-OPOS
                   END-IF
               WHEN OTHER
                   MOVE WS-NORM-BYTE1
                       TO WS-NORM-OUT(
                       WS-NORM-OPOS:1)
                   ADD 1 TO WS-NORM-OPOS
               END-EVALUATE
               ADD 1 TO WS-I
           END-PERFORM

      *>   Lowercase the result
           MOVE LOWER-CASE(
               TRIM(WS-NORM-OUT))
               TO WS-NORM-OUT
           .

      *> ============================================================
      *> APPLY-SINGULAR: plural->singular on WS-TXN-GOOD
      *> ============================================================
       APPLY-SINGULAR.
           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I > WS-PLUR-COUNT
               IF TRIM(WS-TXN-GOOD) =
                   TRIM(WS-PLUR-FROM(WS-I))
                   MOVE WS-PLUR-TO(WS-I)
                       TO WS-TXN-GOOD
                   EXIT PERFORM
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> ADD-GOOD-SOURCE: Add seller to goods table
      *> ============================================================
       ADD-GOOD-SOURCE.
      *>   Find or create good entry
           MOVE 0 TO WS-LOOKUP-IDX
           PERFORM VARYING WS-J
               FROM 1 BY 1
               UNTIL WS-J > WS-GOOD-COUNT
               OR WS-LOOKUP-IDX > 0
               IF TRIM(WS-GOOD-NAME(WS-J))
                   = TRIM(WS-TXN-GOOD)
                   MOVE WS-J
                       TO WS-LOOKUP-IDX
               END-IF
           END-PERFORM

           IF WS-LOOKUP-IDX = 0
      *>       Create new good entry
               ADD 1 TO WS-GOOD-COUNT
               MOVE WS-GOOD-COUNT
                   TO WS-LOOKUP-IDX
               MOVE TRIM(WS-TXN-GOOD)
                   TO WS-GOOD-NAME(
                   WS-LOOKUP-IDX)
      *>       Sanitize filename
               MOVE TRIM(WS-TXN-GOOD)
                   TO WS-SAN-IN
               PERFORM SANITIZE-FILENAME
               MOVE WS-SAN-OUT
                   TO WS-GOOD-FILE(
                   WS-LOOKUP-IDX)
               MOVE 0 TO WS-GOOD-SCNT(
                   WS-LOOKUP-IDX)
           END-IF

      *>   Check if seller already listed
           MOVE "N" TO WS-FOUND-FLAG
           PERFORM VARYING WS-J
               FROM 1 BY 1
               UNTIL WS-J > WS-GOOD-SCNT(
                   WS-LOOKUP-IDX)
               OR WS-FOUND-FLAG = "Y"
               IF TRIM(WS-GS-CITY(
                   WS-LOOKUP-IDX, WS-J))
                   = TRIM(WS-TXN-SELLER)
                   MOVE "Y"
                       TO WS-FOUND-FLAG
               END-IF
           END-PERFORM

           IF WS-FOUND-FLAG = "N"
               ADD 1 TO WS-GOOD-SCNT(
                   WS-LOOKUP-IDX)
               MOVE WS-GOOD-SCNT(
                   WS-LOOKUP-IDX)
                   TO WS-J
               MOVE TRIM(WS-TXN-SELLER)
                   TO WS-GS-CITY(
                   WS-LOOKUP-IDX, WS-J)
           END-IF
           .

      *> ============================================================
      *> PHASE 4: Call LLM to extract cities + people
      *> ============================================================
       PHASE4-CALL-LLM.
           DISPLAY " "
           DISPLAY "[Phase 4] Calling LLM"

           MOVE SPACES TO WS-REQ-JSON
           MOVE 1 TO WS-PTR

      *>   Build request JSON
           STRING
               "{"
               WS-QT "model" WS-QT ":"
               WS-QT "gpt-4.1-mini"
               WS-QT ","
               WS-QT "temperature" WS-QT
               ":0,"
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
               "You are a data "
               "extraction assistant."
               " Analyze Polish-language"
               " trade notes." WS-NL
               WS-NL
               "RULES:" WS-NL
               "- All names: lowercase,"
               " no Polish diacritics"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-NL
               "- Goods: lowercase, "
               "nominative singular,"
               " no diacritics" WS-NL
               "- ziemniaki=ziemniak,"
               " mlotki=mlotek,"
               " lopaty=lopata,"
               " chleby=chleb" WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "- butelki wody=woda,"
               " worki ryzu=ryz,"
               " porcje wolowiny"
               "=wolowina" WS-NL
               "- porcje kurczaka"
               "=kurczak,"
               " wiertarki=wiertarka,"
               " kilofy=kilof" WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "- maka stays maka,"
               " marchew stays marchew"
               WS-NL
               "- kapusta stays kapusta"
               ", makaron stays makaron"
               WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "OUTPUT FORMAT:" WS-NL
               "Return pipe-delimited"
               " lines, nothing else."
               WS-NL WS-NL
               "For cities from "
               "ogloszenia.txt:" WS-NL
               "CITY|cityname|"
               "good1:qty1,"
               "good2:qty2,..." WS-NL
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "For people from "
               "rozmowy.txt:" WS-NL
               "PERSON|firstname|"
               "lastname|cityname"
               WS-NL WS-NL
               "There are exactly 8 "
               "cities and 8 people."
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               "DEDUP: Kisiel from"
               " Brudzewo = Rafal"
               " Kisiel. Konkel from"
               " Karlinkowo = Lena"
               " Konkel." WS-NL
               "Natan Rams = Domatowo."
               WS-QT "},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   User message with file contents
           STRING
               "{"
               WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               "=== ogloszenia.txt ==="
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Append ogloszenia content
           IF WS-OGL-LEN > 0
               STRING
                   WS-OGL-BUF(
                   1:WS-OGL-LEN)
                   DELIMITED SIZE
                   INTO WS-REQ-JSON
                   WITH POINTER WS-PTR
               END-STRING
           END-IF

           STRING
               WS-NL WS-NL
               "=== rozmowy.txt ==="
               WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Append rozmowy content
           IF WS-ROZ-LEN > 0
               STRING
                   WS-ROZ-BUF(
                   1:WS-ROZ-LEN)
                   DELIMITED SIZE
                   INTO WS-REQ-JSON
                   WITH POINTER WS-PTR
           END-STRING
           END-IF

           STRING
               WS-QT "}]}"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Write + curl
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

      *>   Parse response
           MOVE "llm_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp"
               TO WS-WORK-PATH

           IF WS-JLEN = 0
               DISPLAY "  Empty LLM response!"
               EXIT PARAGRAPH
           END-IF

      *>   Check for API error
           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-JBUF(1:WS-JLEN)
               TALLYING WS-TALLY-CNT
               FOR ALL '"error"'
           IF WS-TALLY-CNT > 0
               DISPLAY "  API ERR: "
                   WS-JBUF(1:500)
               EXIT PARAGRAPH
           END-IF

      *>   Extract content value
           MOVE "content"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL

           IF TRIM(WS-JVAL) = SPACES
               DISPLAY "  No content!"
               EXIT PARAGRAPH
           END-IF

      *>   Unescape the response (inline: handles \n,\r,\t,\\,\")
      *>   Copybook JSON-UNESCAPE-STR drops the escape char
      *>   (e.g. "\n" -> "n") which breaks pipe-line parsing.
           MOVE WS-JVAL TO WS-ESC-IN
           MOVE SPACES TO WS-ESC-OUT
           MOVE 0 TO WS-ESC-OLEN
           MOVE LENGTH(TRIM(WS-ESC-IN))
               TO WS-ESC-ILEN
           IF WS-ESC-ILEN > 0
               MOVE 1 TO WS-ESC-I
               PERFORM UNTIL
                   WS-ESC-I > WS-ESC-ILEN
                   IF WS-ESC-IN(
                       WS-ESC-I:1) = X"5C"
                   AND WS-ESC-I
                       < WS-ESC-ILEN
                       ADD 1 TO WS-ESC-I
                       EVALUATE TRUE
                       WHEN WS-ESC-IN(
                           WS-ESC-I:1) = "n"
                           ADD 1 TO WS-ESC-OLEN
                           MOVE X"0A"
                             TO WS-ESC-OUT(
                             WS-ESC-OLEN:1)
                       WHEN WS-ESC-IN(
                           WS-ESC-I:1) = "r"
                           ADD 1 TO WS-ESC-OLEN
                           MOVE X"0D"
                             TO WS-ESC-OUT(
                             WS-ESC-OLEN:1)
                       WHEN WS-ESC-IN(
                           WS-ESC-I:1) = "t"
                           ADD 1 TO WS-ESC-OLEN
                           MOVE X"09"
                             TO WS-ESC-OUT(
                             WS-ESC-OLEN:1)
                       WHEN OTHER
                           ADD 1 TO WS-ESC-OLEN
                           MOVE WS-ESC-IN(
                               WS-ESC-I:1)
                             TO WS-ESC-OUT(
                             WS-ESC-OLEN:1)
                       END-EVALUATE
                   ELSE
                       ADD 1 TO WS-ESC-OLEN
                       MOVE WS-ESC-IN(
                           WS-ESC-I:1)
                         TO WS-ESC-OUT(
                         WS-ESC-OLEN:1)
                   END-IF
                   ADD 1 TO WS-ESC-I
               END-PERFORM
           END-IF
           MOVE WS-ESC-OUT TO WS-LLM-RESP
           MOVE WS-ESC-OLEN TO WS-LLM-RLEN

           DISPLAY "  LLM response ("
               WS-LLM-RLEN " chars):"
           DISPLAY WS-LLM-RESP(1:500)

      *>   Parse LLM pipe-delimited lines
           PERFORM PARSE-LLM-RESPONSE
           .

      *> ============================================================
      *> PARSE-LLM-RESPONSE
      *> ============================================================
       PARSE-LLM-RESPONSE.
           MOVE 0 TO WS-CITY-COUNT
           MOVE 0 TO WS-PERS-COUNT
           MOVE 1 TO WS-RESP-POS

           PERFORM UNTIL
               WS-RESP-POS > WS-LLM-RLEN

      *>       Extract one line
               MOVE SPACES TO WS-RESP-LINE
               MOVE 1 TO WS-K
               PERFORM UNTIL
                   WS-RESP-POS
                   > WS-LLM-RLEN
                   IF WS-LLM-RESP(
                       WS-RESP-POS:1)
                       = X"0A"
                       ADD 1
                           TO WS-RESP-POS
                       EXIT PERFORM
                   END-IF
                   IF WS-LLM-RESP(
                       WS-RESP-POS:1)
                       = X"0D"
                       ADD 1
                           TO WS-RESP-POS
                       EXIT PERFORM
                   END-IF
                   MOVE WS-LLM-RESP(
                       WS-RESP-POS:1)
                       TO WS-RESP-LINE(
                       WS-K:1)
                   ADD 1 TO WS-K
                   ADD 1 TO WS-RESP-POS
               END-PERFORM
               SUBTRACT 1 FROM WS-K

      *>       Skip blank lines
               IF WS-K < 3
                   EXIT PERFORM CYCLE
               END-IF

      *>       Get type (before first |)
               MOVE SPACES TO WS-RESP-TYPE
               MOVE 1 TO WS-N
               PERFORM VARYING WS-I
                   FROM 1 BY 1
                   UNTIL WS-I > WS-K
                   IF WS-RESP-LINE(
                       WS-I:1) = "|"
                       MOVE WS-I TO WS-N
                       EXIT PERFORM
                   END-IF
               END-PERFORM

               IF WS-N > 1
                   MOVE WS-RESP-LINE(
                       1:WS-N - 1)
                       TO WS-RESP-TYPE
               END-IF

               IF TRIM(WS-RESP-TYPE)
                   = "CITY"
                   PERFORM PARSE-CITY-LINE
               END-IF
               IF TRIM(WS-RESP-TYPE)
                   = "PERSON"
                   PERFORM PARSE-PERSON-LINE
               END-IF
           END-PERFORM

           DISPLAY "  Cities: "
               WS-CITY-COUNT
           DISPLAY "  People: "
               WS-PERS-COUNT
           .

      *> ============================================================
      *> PARSE-CITY-LINE: CITY|name|good:qty,...
      *> ============================================================
       PARSE-CITY-LINE.
           ADD 1 TO WS-CITY-COUNT
           MOVE WS-CITY-COUNT TO WS-LOOKUP-IDX

      *>   Skip "CITY|"
           MOVE SPACES TO WS-FIELD1
           MOVE SPACES TO WS-FIELD3
           COMPUTE WS-I = WS-N + 1

      *>   Get city name (field2 = to next |)
           MOVE 1 TO WS-J
           PERFORM UNTIL WS-I > WS-K
               IF WS-RESP-LINE(WS-I:1)
                   = "|"
                   ADD 1 TO WS-I
                   EXIT PERFORM
               END-IF
               MOVE WS-RESP-LINE(WS-I:1)
                   TO WS-FIELD1(WS-J:1)
               ADD 1 TO WS-J
               ADD 1 TO WS-I
           END-PERFORM

      *>   Save WS-I (next parse pos)
           MOVE WS-I TO WS-VAL-START

      *>   Normalize city name
           MOVE TRIM(WS-FIELD1) TO WS-NORM-IN
           PERFORM NORMALIZE-POLISH
           MOVE TRIM(WS-NORM-OUT)
               TO WS-CITY-NAME(WS-LOOKUP-IDX)

      *>   Display name = capitalize first
           MOVE TRIM(WS-NORM-OUT)
               TO WS-CITY-DISP(WS-LOOKUP-IDX)
           MOVE UPPER-CASE(
               WS-CITY-DISP(
               WS-LOOKUP-IDX)(1:1))
               TO WS-CITY-DISP(
               WS-LOOKUP-IDX)(1:1)

      *>   Sanitize filename
           MOVE TRIM(WS-NORM-OUT)
               TO WS-SAN-IN
           PERFORM SANITIZE-FILENAME
           MOVE WS-SAN-OUT
               TO WS-CITY-FILE(WS-LOOKUP-IDX)

      *>   Restore WS-I (next parse pos)
           MOVE WS-VAL-START TO WS-I

      *>   Get goods string (rest of line)
           MOVE 1 TO WS-J
           PERFORM UNTIL WS-I > WS-K
               MOVE WS-RESP-LINE(WS-I:1)
                   TO WS-FIELD3(WS-J:1)
               ADD 1 TO WS-J
               ADD 1 TO WS-I
           END-PERFORM

      *>   Parse goods: good1:qty1,good2:qty2
           MOVE 0 TO WS-CITY-GCNT(
               WS-LOOKUP-IDX)
           MOVE 1 TO WS-I
           MOVE LENGTH(TRIM(WS-FIELD3))
               TO WS-J

           PERFORM UNTIL WS-I > WS-J
      *>       Get good name (before :)
               MOVE SPACES TO WS-PARSE-WORD
               MOVE 1 TO WS-PARSE-LEN
               PERFORM UNTIL WS-I > WS-J
                   IF WS-FIELD3(WS-I:1)
                       = ":"
                       ADD 1 TO WS-I
                       EXIT PERFORM
                   END-IF
                   MOVE WS-FIELD3(WS-I:1)
                       TO WS-PARSE-WORD(
                       WS-PARSE-LEN:1)
                   ADD 1 TO WS-PARSE-LEN
                   ADD 1 TO WS-I
               END-PERFORM

      *>       Get quantity (before , or end)
               MOVE SPACES TO WS-FIELD2
               MOVE 1 TO WS-PARSE-LEN
               PERFORM UNTIL WS-I > WS-J
                   IF WS-FIELD3(WS-I:1)
                       = ","
                       ADD 1 TO WS-I
                       EXIT PERFORM
                   END-IF
                   MOVE WS-FIELD3(WS-I:1)
                       TO WS-FIELD2(
                       WS-PARSE-LEN:1)
                   ADD 1 TO WS-PARSE-LEN
                   ADD 1 TO WS-I
               END-PERFORM

               IF TRIM(WS-PARSE-WORD)
                   NOT = SPACES
               AND TRIM(WS-FIELD2)
                   NOT = SPACES
                   ADD 1 TO WS-CITY-GCNT(
                       WS-LOOKUP-IDX)
                   MOVE WS-CITY-GCNT(
                       WS-LOOKUP-IDX)
                       TO WS-N
                   MOVE TRIM(WS-PARSE-WORD)
                       TO WS-CG-NAME(
                       WS-LOOKUP-IDX,
                       WS-N)
                   MOVE NUMVAL(
                       TRIM(WS-FIELD2))
                       TO WS-CG-QTY(
                       WS-LOOKUP-IDX,
                       WS-N)
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> PARSE-PERSON-LINE: PERSON|first|last|city
      *> ============================================================
       PARSE-PERSON-LINE.
           ADD 1 TO WS-PERS-COUNT
           MOVE WS-PERS-COUNT
               TO WS-LOOKUP-IDX

      *>   Skip "PERSON|"
           COMPUTE WS-I = WS-N + 1

      *>   Get first name
           MOVE SPACES TO WS-FIELD1
           MOVE 1 TO WS-J
           PERFORM UNTIL WS-I > WS-K
               IF WS-RESP-LINE(WS-I:1)
                   = "|"
                   ADD 1 TO WS-I
                   EXIT PERFORM
               END-IF
               MOVE WS-RESP-LINE(WS-I:1)
                   TO WS-FIELD1(WS-J:1)
               ADD 1 TO WS-J
               ADD 1 TO WS-I
           END-PERFORM

      *>   Get last name
           MOVE SPACES TO WS-FIELD2
           MOVE 1 TO WS-J
           PERFORM UNTIL WS-I > WS-K
               IF WS-RESP-LINE(WS-I:1)
                   = "|"
                   ADD 1 TO WS-I
                   EXIT PERFORM
               END-IF
               MOVE WS-RESP-LINE(WS-I:1)
                   TO WS-FIELD2(WS-J:1)
               ADD 1 TO WS-J
               ADD 1 TO WS-I
           END-PERFORM

      *>   Get city
           MOVE SPACES TO WS-FIELD3
           MOVE 1 TO WS-J
           PERFORM UNTIL WS-I > WS-K
               IF WS-RESP-LINE(WS-I:1)
                   = "|"
                   ADD 1 TO WS-I
                   EXIT PERFORM
               END-IF
               MOVE WS-RESP-LINE(WS-I:1)
                   TO WS-FIELD3(WS-J:1)
               ADD 1 TO WS-J
               ADD 1 TO WS-I
           END-PERFORM

      *>   Normalize city
           MOVE TRIM(WS-FIELD3)
               TO WS-NORM-IN
           PERFORM NORMALIZE-POLISH
           MOVE TRIM(WS-NORM-OUT)
               TO WS-PERS-CITY(
               WS-LOOKUP-IDX)

      *>   Store names (lowercase)
           MOVE LOWER-CASE(
               TRIM(WS-FIELD1))
               TO WS-PERS-FIRST(
               WS-LOOKUP-IDX)
           MOVE LOWER-CASE(
               TRIM(WS-FIELD2))
               TO WS-PERS-LAST(
               WS-LOOKUP-IDX)

      *>   Person filename: first_last
           MOVE SPACES TO WS-SAN-IN
           STRING
               TRIM(WS-PERS-FIRST(
               WS-LOOKUP-IDX))
               "_"
               TRIM(WS-PERS-LAST(
               WS-LOOKUP-IDX))
               DELIMITED SIZE
               INTO WS-SAN-IN
           END-STRING
           PERFORM SANITIZE-FILENAME
           MOVE WS-SAN-OUT
               TO WS-PERS-FILE(
               WS-LOOKUP-IDX)
           .

      *> ============================================================
      *> PHASE 5: Build batch JSON
      *> ============================================================
       PHASE5-BUILD-BATCH.
           DISPLAY " "
           DISPLAY "[Phase 5] Building batch"

           MOVE SPACES TO WS-BATCH-BUF
           MOVE 1 TO WS-BATCH-PTR

      *>   Opening: apikey, task, answer array
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY)
               WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT "filesystem" WS-QT ","
               WS-QT "answer" WS-QT ":["
               DELIMITED SIZE
               INTO WS-BATCH-BUF
               WITH POINTER WS-BATCH-PTR
           END-STRING

      *>   1. Reset
           STRING
               "{" WS-QT "action" WS-QT ":"
               WS-QT "reset" WS-QT "},"
               DELIMITED SIZE
               INTO WS-BATCH-BUF
               WITH POINTER WS-BATCH-PTR
           END-STRING

      *>   2. Create directories
           STRING
               "{" WS-QT "action" WS-QT ":"
               WS-QT "createDirectory"
               WS-QT ","
               WS-QT "path" WS-QT ":"
               WS-QT "/miasta" WS-QT "},"
               DELIMITED SIZE
               INTO WS-BATCH-BUF
               WITH POINTER WS-BATCH-PTR
           END-STRING

           STRING
               "{" WS-QT "action" WS-QT ":"
               WS-QT "createDirectory"
               WS-QT ","
               WS-QT "path" WS-QT ":"
               WS-QT "/osoby" WS-QT "},"
               DELIMITED SIZE
               INTO WS-BATCH-BUF
               WITH POINTER WS-BATCH-PTR
           END-STRING

           STRING
               "{" WS-QT "action" WS-QT ":"
               WS-QT "createDirectory"
               WS-QT ","
               WS-QT "path" WS-QT ":"
               WS-QT "/towary" WS-QT "},"
               DELIMITED SIZE
               INTO WS-BATCH-BUF
               WITH POINTER WS-BATCH-PTR
           END-STRING

      *>   3. City files
           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I > WS-CITY-COUNT
               PERFORM BUILD-CITY-OP
           END-PERFORM

      *>   4. Person files
           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I > WS-PERS-COUNT
               PERFORM BUILD-PERSON-OP
           END-PERFORM

      *>   5. Good files
           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I > WS-GOOD-COUNT
               PERFORM BUILD-GOOD-OP
           END-PERFORM

      *>   Remove trailing comma, close array
           SUBTRACT 1 FROM WS-BATCH-PTR
           IF WS-BATCH-BUF(
               WS-BATCH-PTR:1) = ","
               MOVE "]" TO WS-BATCH-BUF(
                   WS-BATCH-PTR:1)
               ADD 1 TO WS-BATCH-PTR
           ELSE
               ADD 1 TO WS-BATCH-PTR
               STRING "]"
                   DELIMITED SIZE
                   INTO WS-BATCH-BUF
                   WITH POINTER WS-BATCH-PTR
               END-STRING
           END-IF

      *>   Close main object
           STRING "}"
               DELIMITED SIZE
               INTO WS-BATCH-BUF
               WITH POINTER WS-BATCH-PTR
           END-STRING

           COMPUTE WS-N = WS-BATCH-PTR - 1
           DISPLAY "  Batch JSON: "
               WS-N " chars"
           .

      *> ============================================================
      *> BUILD-CITY-OP: createFile for one city
      *> ============================================================
       BUILD-CITY-OP.
           STRING
               "{" WS-QT "action" WS-QT ":"
               WS-QT "createFile" WS-QT ","
               WS-QT "path" WS-QT ":"
               WS-QT "/miasta/"
               TRIM(WS-CITY-FILE(WS-I))
               WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               DELIMITED SIZE
               INTO WS-BATCH-BUF
               WITH POINTER WS-BATCH-PTR
           END-STRING

      *>   Build JSON content: {"good1": qty1, ...}
      *>   Must be escaped for embedding
           MOVE SPACES TO WS-CONTENT-BUF
           MOVE 1 TO WS-CONTENT-PTR

           STRING "{"
               DELIMITED SIZE
               INTO WS-CONTENT-BUF
               WITH POINTER WS-CONTENT-PTR
           END-STRING

           PERFORM VARYING WS-J
               FROM 1 BY 1
               UNTIL WS-J > WS-CITY-GCNT(
                   WS-I)
               IF WS-J > 1
                   STRING ", "
                       DELIMITED SIZE
                       INTO WS-CONTENT-BUF
                       WITH POINTER
                       WS-CONTENT-PTR
                   END-STRING
               END-IF

      *>       Escape the quote for nested JSON
      *>       We need \" in output
               MOVE WS-CG-NAME(
                   WS-I, WS-J)
                   TO WS-TMP
               STRING
                   X"5C" WS-QT
                   TRIM(WS-TMP)
                   X"5C" WS-QT ": "
                   DELIMITED SIZE
                   INTO WS-CONTENT-BUF
                   WITH POINTER
                   WS-CONTENT-PTR
               END-STRING

               MOVE WS-CG-QTY(WS-I, WS-J)
                   TO WS-NUM-EDITED
               STRING
                   TRIM(WS-NUM-EDITED)
                   DELIMITED SIZE
                   INTO WS-CONTENT-BUF
                   WITH POINTER
                   WS-CONTENT-PTR
               END-STRING
           END-PERFORM

           STRING "}"
               DELIMITED SIZE
               INTO WS-CONTENT-BUF
               WITH POINTER WS-CONTENT-PTR
           END-STRING

      *>   Append content to batch
           COMPUTE WS-N =
               WS-CONTENT-PTR - 1
           STRING
               WS-CONTENT-BUF(1:WS-N)
               WS-QT "},"
               DELIMITED SIZE
               INTO WS-BATCH-BUF
               WITH POINTER WS-BATCH-PTR
           END-STRING
           .

      *> ============================================================
      *> BUILD-PERSON-OP
      *> ============================================================
       BUILD-PERSON-OP.
      *>   Find city display name + filename
           MOVE SPACES TO WS-FIELD1
           MOVE SPACES TO WS-FIELD2

           PERFORM VARYING WS-J
               FROM 1 BY 1
               UNTIL WS-J > WS-CITY-COUNT
               IF TRIM(WS-CITY-NAME(WS-J))
                   = TRIM(WS-PERS-CITY(WS-I))
                   MOVE TRIM(
                       WS-CITY-DISP(WS-J))
                       TO WS-FIELD1
                   MOVE TRIM(
                       WS-CITY-FILE(WS-J))
                       TO WS-FIELD2
                   EXIT PERFORM
               END-IF
           END-PERFORM

      *>   Fallback if city not found
           IF TRIM(WS-FIELD2) = SPACES
               MOVE TRIM(WS-PERS-CITY(
                   WS-I)) TO WS-SAN-IN
               PERFORM SANITIZE-FILENAME
               MOVE WS-SAN-OUT TO WS-FIELD2
               MOVE TRIM(WS-PERS-CITY(
                   WS-I)) TO WS-FIELD1
               MOVE UPPER-CASE(
                   WS-FIELD1(1:1))
                   TO WS-FIELD1(1:1)
           END-IF

      *>   Capitalize person names
           MOVE SPACES TO WS-FIELD3
           MOVE TRIM(WS-PERS-FIRST(WS-I))
               TO WS-FIELD3
           MOVE UPPER-CASE(
               WS-FIELD3(1:1))
               TO WS-FIELD3(1:1)

           MOVE SPACES TO WS-TMP
           MOVE TRIM(WS-PERS-LAST(WS-I))
               TO WS-TMP
           MOVE UPPER-CASE(
               WS-TMP(1:1))
               TO WS-TMP(1:1)

      *>   Content: FirstName LastName
      *>     [CityDisp](/miasta/cityfile)
           STRING
               "{" WS-QT "action" WS-QT ":"
               WS-QT "createFile" WS-QT ","
               WS-QT "path" WS-QT ":"
               WS-QT "/osoby/"
               TRIM(WS-PERS-FILE(WS-I))
               WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               TRIM(WS-FIELD3) " "
               TRIM(WS-TMP) " ["
               TRIM(WS-FIELD1)
               "](/miasta/"
               TRIM(WS-FIELD2) ")"
               WS-QT "},"
               DELIMITED SIZE
               INTO WS-BATCH-BUF
               WITH POINTER WS-BATCH-PTR
           END-STRING
           .

      *> ============================================================
      *> BUILD-GOOD-OP
      *> ============================================================
       BUILD-GOOD-OP.
           STRING
               "{" WS-QT "action" WS-QT ":"
               WS-QT "createFile" WS-QT ","
               WS-QT "path" WS-QT ":"
               WS-QT "/towary/"
               TRIM(WS-GOOD-FILE(WS-I))
               WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               DELIMITED SIZE
               INTO WS-BATCH-BUF
               WITH POINTER WS-BATCH-PTR
           END-STRING

      *>   Build content: [City1](/miasta/f1)\n...
           PERFORM VARYING WS-J
               FROM 1 BY 1
               UNTIL WS-J > WS-GOOD-SCNT(
                   WS-I)

      *>       Newline between entries
               IF WS-J > 1
                   STRING WS-NL
                       DELIMITED SIZE
                       INTO WS-BATCH-BUF
                       WITH POINTER
                       WS-BATCH-PTR
                   END-STRING
               END-IF

      *>       Find city display name + filename
               MOVE WS-GS-CITY(
                   WS-I, WS-J)
                   TO WS-LOOKUP-CITY
               MOVE SPACES TO WS-FIELD1
               MOVE SPACES TO WS-FIELD2

               PERFORM VARYING WS-N
                   FROM 1 BY 1
                   UNTIL WS-N
                       > WS-CITY-COUNT
                   IF TRIM(WS-CITY-NAME(
                       WS-N))
                       = TRIM(WS-LOOKUP-CITY)
                       MOVE TRIM(
                           WS-CITY-DISP(
                           WS-N))
                           TO WS-FIELD1
                       MOVE TRIM(
                           WS-CITY-FILE(
                           WS-N))
                           TO WS-FIELD2
                       EXIT PERFORM
                   END-IF
               END-PERFORM

      *>       Fallback
               IF TRIM(WS-FIELD2) = SPACES
                   MOVE TRIM(
                       WS-LOOKUP-CITY)
                       TO WS-SAN-IN
                   PERFORM SANITIZE-FILENAME
                   MOVE WS-SAN-OUT
                       TO WS-FIELD2
                   MOVE TRIM(
                       WS-LOOKUP-CITY)
                       TO WS-FIELD1
                   MOVE UPPER-CASE(
                       WS-FIELD1(1:1))
                       TO WS-FIELD1(1:1)
               END-IF

               STRING
                   "[" TRIM(WS-FIELD1)
                   "](/miasta/"
                   TRIM(WS-FIELD2) ")"
                   DELIMITED SIZE
                   INTO WS-BATCH-BUF
                   WITH POINTER
                   WS-BATCH-PTR
               END-STRING
           END-PERFORM

           STRING
               WS-QT "},"
               DELIMITED SIZE
               INTO WS-BATCH-BUF
               WITH POINTER WS-BATCH-PTR
           END-STRING
           .

      *> ============================================================
      *> PHASE 6: Submit batch + done
      *> ============================================================
       PHASE6-SUBMIT.
           DISPLAY " "
           DISPLAY "[Phase 6] Submitting"

      *>   Write batch to file
           MOVE "batch_req.json"
               TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "ERR: OPEN "
                   TRIM(WS-WORK-PATH)
                   " FS=" WS-FS
               STOP RUN
           END-IF
           COMPUTE WS-N = WS-BATCH-PTR - 1
           WRITE WORK-REC
               FROM WS-BATCH-BUF
           CLOSE WORK-FILE

      *>   Submit batch
           INITIALIZE WS-CMD
           STRING
               "curl -s "
               "-o batch_resp.json"
               " -X POST "
               TRIM(WS-VERIFY-URL)
               " -H " WS-QT
               "Content-Type: "
               "application/json"
               WS-QT
               " -d @batch_req.json"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Read batch response
           MOVE "batch_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp"
               TO WS-WORK-PATH

           DISPLAY "  Batch resp: "
               WS-JBUF(1:500)

      *>   Check for flag in batch response
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
               DISPLAY "  >>> FLAG IN BATCH"
               EXIT PARAGRAPH
           END-IF

      *>   Submit done action
           DISPLAY "  Submitting done..."

           MOVE SPACES TO WS-BATCH-BUF
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY)
               WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT "filesystem" WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT "done" WS-QT "}}"
               DELIMITED SIZE
               INTO WS-BATCH-BUF
           END-STRING

           MOVE "done_req.json"
               TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "ERR: OPEN "
                   TRIM(WS-WORK-PATH)
                   " FS=" WS-FS
               STOP RUN
           END-IF
           WRITE WORK-REC
               FROM WS-BATCH-BUF
           CLOSE WORK-FILE

           INITIALIZE WS-CMD
           STRING
               "curl -s "
               "-o done_resp.json"
               " -X POST "
               TRIM(WS-VERIFY-URL)
               " -H " WS-QT
               "Content-Type: "
               "application/json"
               WS-QT
               " -d @done_req.json"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Read done response
           MOVE "done_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp"
               TO WS-WORK-PATH

           DISPLAY "  Done resp: "
               WS-JBUF(1:500)

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
               DISPLAY "  >>> FLAG FOUND!"
           ELSE
               DISPLAY "  No flag yet."
           END-IF
           .

      *> ============================================================
      *> SANITIZE-FILENAME: a-z0-9_ only, max 20
      *> ============================================================
       SANITIZE-FILENAME.
           MOVE SPACES TO WS-SAN-OUT
           MOVE LOWER-CASE(TRIM(WS-SAN-IN))
               TO WS-SAN-IN
           MOVE LENGTH(TRIM(WS-SAN-IN))
               TO WS-SAN-POS
           MOVE 1 TO WS-SAN-OPOS

           PERFORM VARYING WS-SAN-IDX
               FROM 1 BY 1
               UNTIL WS-SAN-IDX > WS-SAN-POS
               OR WS-SAN-OPOS > 20
               MOVE WS-SAN-IN(WS-SAN-IDX:1)
                   TO WS-SAN-CH
               EVALUATE TRUE
               WHEN WS-SAN-CH >= "a"
               AND WS-SAN-CH <= "z"
                   MOVE WS-SAN-CH
                       TO WS-SAN-OUT(
                       WS-SAN-OPOS:1)
                   ADD 1 TO WS-SAN-OPOS
               WHEN WS-SAN-CH >= "0"
               AND WS-SAN-CH <= "9"
                   MOVE WS-SAN-CH
                       TO WS-SAN-OUT(
                       WS-SAN-OPOS:1)
                   ADD 1 TO WS-SAN-OPOS
               WHEN WS-SAN-CH = "_"
                   MOVE "_"
                       TO WS-SAN-OUT(
                       WS-SAN-OPOS:1)
                   ADD 1 TO WS-SAN-OPOS
               WHEN OTHER
                   MOVE "_"
                       TO WS-SAN-OUT(
                       WS-SAN-OPOS:1)
                   ADD 1 TO WS-SAN-OPOS
               END-EVALUATE
           END-PERFORM
           .

      *> ============================================================
      *> EXTRACT-ZIP: POSIX FFI + zlib
      *> ============================================================
       EXTRACT-ZIP.
           MOVE 0 TO WS-EXTRACTED-CT
           MOVE "N" TO WS-ZIP-DONE
           MOVE "natan_notes.zip" & X"00"
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
                   "notes/"
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
      *> GET-ZIP-BASENAME
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
      *> ZIP-WRITE-STORED
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
      *> ZIP-INFLATE
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
      *> READ-AND-ESCAPE: Read file, JSON-escape
      *> Output: WS-ESC-OUT, WS-ESC-OLEN
      *> ============================================================
       READ-AND-ESCAPE.
           MOVE SPACES TO WS-ESC-IN
           MOVE 0 TO WS-ESC-ILEN
           MOVE "N" TO WS-EOF

      *>   First read raw into ESC-IN
           OPEN INPUT WORK-FILE
           IF WS-FS NOT = "00"
               MOVE 0 TO WS-ESC-OLEN
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
      *>               Add newline between
                       IF WS-ESC-ILEN > 0
                           ADD 1
                             TO WS-ESC-ILEN
                           MOVE X"0A"
                             TO WS-ESC-IN(
                             WS-ESC-ILEN:1)
                       END-IF
                       IF WS-K > 0
                           MOVE WS-LINE(
                               1:WS-K)
                               TO WS-ESC-IN(
                               WS-ESC-ILEN
                               + 1:WS-K)
                           ADD WS-K
                               TO WS-ESC-ILEN
                       END-IF
               END-READ
           END-PERFORM

           CLOSE WORK-FILE
           MOVE "N" TO WS-EOF

      *>   Now escape it
           PERFORM JSON-ESCAPE-STR
           .

      *> ============================================================
      *> READ-RAW-FILE: Read file into TRN-RAW
      *> ============================================================
       READ-RAW-FILE.
           MOVE SPACES TO WS-TRN-RAW
           MOVE 0 TO WS-TRN-RAWLEN
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
                       IF WS-TRN-RAWLEN > 0
                           ADD 1
                             TO WS-TRN-RAWLEN
                           MOVE X"0A"
                             TO WS-TRN-RAW(
                             WS-TRN-RAWLEN:1)
                       END-IF
                       IF WS-K > 0
                           MOVE WS-LINE(
                               1:WS-K)
                               TO WS-TRN-RAW(
                               WS-TRN-RAWLEN
                               + 1:WS-K)
                           ADD WS-K
                               TO WS-TRN-RAWLEN
                       END-IF
               END-READ
           END-PERFORM

           CLOSE WORK-FILE
           MOVE "N" TO WS-EOF
           .

       COPY JSONESCAPE-PROC.
       COPY JSONUNESCAPE-PROC.
       COPY JSONREAD-PROC.
       COPY JSONPARSE-PROC.

       COPY ENVLOAD-PROC.
