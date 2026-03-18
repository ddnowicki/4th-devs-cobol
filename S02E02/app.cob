       IDENTIFICATION DIVISION.
       PROGRAM-ID. S02E02-V2.
      *> ============================================================
      *> S02E02 - Electricity Puzzle v2 (Pure COBOL Image Processing)
      *> Mirrors app.py step by step:
      *>  1. Download 2 board PNGs
      *>  2. PNG decode (parse chunks, deflate, unfilter)
      *>  3. Grayscale conversion
      *>  4. Grid line detection
      *>  5. Crop 9 cells, resize 200x200, binarize
      *>  6. For each cell: rotate 0/90/180/270, encode PNG, base64
      *>  7. Send 5 images to Gemini per cell
      *>  8. Parse rotation answers, execute via API
      *>
      *> Compile: cobc -x app_v2.cob
      *> Run:     COB_FILE_FORMAT=1 ./app_v2
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
           SELECT JSON-FILE ASSIGN TO "vision_req.json"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-JFS.
           SELECT BIN-FILE ASSIGN TO WS-BIN-PATH
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-BFS.
           SELECT BIN-FILE-OUT ASSIGN TO "debug_cell.png"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-BFS2.

       DATA DIVISION.
       FILE SECTION.
       FD  WORK-FILE.
       01  WORK-REC                PIC X(65000).
       FD  JSON-FILE
           RECORD IS VARYING IN SIZE
           FROM 1 TO 999999
           DEPENDING ON WS-JSON-LEN.
       01  JSON-REC                PIC X(999999).
       FD  BIN-FILE.
       01  BIN-FILE-BYTE           PIC X(1).
       FD  BIN-FILE-OUT.
       01  BIN-OUT-BYTE            PIC X(1).

       WORKING-STORAGE SECTION.
      *> ============================================================
      *> CONFIG
      *> ============================================================
       01  WS-HUB-KEY              PIC X(50).
       01  WS-HUB-URL              PIC X(100).
       01  WS-OR-KEY               PIC X(200).
       01  WS-QT                   PIC X(1) VALUE '"'.
       01  WS-FS                   PIC XX.
       01  WS-JFS                  PIC XX.
       01  WS-BFS                  PIC XX.
       01  WS-BFS2                 PIC XX.
       01  WS-WORK-PATH            PIC X(200) VALUE "work.tmp".
       01  WS-BIN-PATH             PIC X(200).

       01  WS-VERIFY-URL           PIC X(200).
       01  WS-BOARD-URL            PIC X(300).
       01  WS-SOLVED-URL           PIC X(300).
       01  WS-API-URL              PIC X(200) VALUE
           "https://openrouter.ai/api/v1/chat/completions".
       01  WS-TASK-NAME            PIC X(20) VALUE "electricity".

       01  WS-CMD                  PIC X(8000).
       01  WS-JSON-LEN             PIC 9(7).
       01  WS-EOF                  PIC X VALUE "N".
       01  WS-LINE                 PIC X(65000).
       01  WS-BIN-EOF              PIC 9 VALUE 0.

      *> ============================================================
      *> CELLS
      *> ============================================================
       01  WS-CELL-TABLE.
           05  WS-CELL OCCURS 9 TIMES.
               10  WS-CELL-ID      PIC X(3).
               10  WS-CELL-ROW     PIC 9(1).
               10  WS-CELL-COL     PIC 9(1).
       01  WS-PLAN-TABLE.
           05  WS-PLAN-ROT OCCURS 9 TIMES PIC 9(1).
       01  WS-IDX                  PIC 9(2).
       01  WS-ROT-I                PIC 9(1).
       01  WS-TOTAL-ROTS           PIC 9(2) VALUE 0.
       01  WS-FLAG-FOUND           PIC X VALUE "N".
       01  WS-TALLY-CNT            PIC 9(4).

      *> ============================================================
      *> PNG RAW BYTES BUFFER (for reading files)
      *> Max ~270KB per PNG
      *> ============================================================
       01  WS-PNG-BUF              PIC X(300000).
       01  WS-PNG-LEN              PIC 9(7) VALUE 0.

      *> ============================================================
      *> PNG PARSER VARIABLES
      *> ============================================================
       01  WS-PNG-POS              PIC 9(7).
       01  WS-CHUNK-LEN            PIC 9(9) COMP-5.
       01  WS-CHUNK-TYPE           PIC X(4).
       01  WS-IMG-WIDTH            PIC 9(5) COMP-5.
       01  WS-IMG-HEIGHT           PIC 9(5) COMP-5.
       01  WS-IMG-BITDEPTH         PIC 9(3).
       01  WS-IMG-COLORTYPE        PIC 9(3).
       01  WS-BYTE-VAL             PIC 9(3).
       01  WS-BYTE-VAL2            PIC 9(3).
       01  WS-TMP-I                PIC 9(7).
       01  WS-TMP-J                PIC 9(7).
       01  WS-TMP-K                PIC 9(7).
       01  WS-TMP-V                PIC S9(9) COMP-5.
       01  WS-TMP-V2               PIC S9(9) COMP-5.
       01  WS-TMP-V3               PIC S9(9) COMP-5.

      *> IDAT compressed data buffer (collect all IDAT chunks)
       01  WS-ZDATA               PIC X(300000).
       01  WS-ZDATA-LEN           PIC 9(7) VALUE 0.

      *> ============================================================
      *> DEFLATE DECOMPRESSOR
      *> ============================================================
      *> Bit reader state
       01  WS-BIT-POS             PIC 9(9) COMP-5 VALUE 0.
      *> Input starts at byte 3 (skip zlib 2-byte header)
       01  WS-ZDATA-START         PIC 9(7) VALUE 3.

      *> Output buffer (uncompressed = filter+pixels)
      *> Max: (1+W*BPP)*H e.g. (1+500*3)*500 = 750500
       01  WS-OUT-BUF             PIC X(900000).
       01  WS-OUT-LEN             PIC 9(7) VALUE 0.

      *> Deflate working vars
       01  WS-BFINAL              PIC 9(1).
       01  WS-BTYPE               PIC 9(1).
       01  WS-BLOCK-LEN           PIC 9(5) COMP-5.
       01  WS-BLOCK-NLEN          PIC 9(5) COMP-5.
       01  WS-BITS-VAL            PIC 9(9) COMP-5.
       01  WS-NBITS               PIC 9(3).
       01  WS-READ-BITS-RESULT    PIC 9(9) COMP-5.
      *> Dedicated vars for READ-BITS (avoid clobbering caller)
       01  WS-RB-I                PIC 9(7) COMP-5.
       01  WS-RB-BYTEPOS          PIC 9(7) COMP-5.
       01  WS-RB-BITOFF           PIC 9(2) COMP-5.
       01  WS-RB-BYTEVAL          PIC 9(3) COMP-5.
       01  WS-RB-SHIFTED          PIC S9(9) COMP-5.
       01  WS-RB-BIT              PIC 9(1) COMP-5.
       01  WS-RB-POWER            PIC 9(9) COMP-5.
       01  WS-RB-SHCNT            PIC 9(2) COMP-5.

      *> Huffman tables for fixed codes
      *> Lit/len: 0-143=8bit, 144-255=9bit, 256-279=7bit, 280-287=8
      *> We decode bit by bit using tree structure
      *> For fixed Huffman: max code 287 for lit/len, 31 for dist

      *> Dynamic Huffman
       01  WS-HLIT               PIC 9(3) COMP-5.
       01  WS-HDIST              PIC 9(3) COMP-5.
       01  WS-HCLEN              PIC 9(3) COMP-5.

      *> Code length code lengths (19 values, order: 16,17,18,0..15)
       01  WS-CL-ORDER.
           05  FILLER PIC 9(2) COMP-5 VALUE 16.
           05  FILLER PIC 9(2) COMP-5 VALUE 17.
           05  FILLER PIC 9(2) COMP-5 VALUE 18.
           05  FILLER PIC 9(2) COMP-5 VALUE 0.
           05  FILLER PIC 9(2) COMP-5 VALUE 8.
           05  FILLER PIC 9(2) COMP-5 VALUE 7.
           05  FILLER PIC 9(2) COMP-5 VALUE 9.
           05  FILLER PIC 9(2) COMP-5 VALUE 6.
           05  FILLER PIC 9(2) COMP-5 VALUE 10.
           05  FILLER PIC 9(2) COMP-5 VALUE 5.
           05  FILLER PIC 9(2) COMP-5 VALUE 11.
           05  FILLER PIC 9(2) COMP-5 VALUE 4.
           05  FILLER PIC 9(2) COMP-5 VALUE 12.
           05  FILLER PIC 9(2) COMP-5 VALUE 3.
           05  FILLER PIC 9(2) COMP-5 VALUE 13.
           05  FILLER PIC 9(2) COMP-5 VALUE 2.
           05  FILLER PIC 9(2) COMP-5 VALUE 14.
           05  FILLER PIC 9(2) COMP-5 VALUE 1.
           05  FILLER PIC 9(2) COMP-5 VALUE 15.
       01  WS-CL-ORD-TAB REDEFINES WS-CL-ORDER.
           05  WS-CL-ORD OCCURS 19 TIMES PIC 9(2) COMP-5.

      *> Code length code lengths array (indexed 0-18)
       01  WS-CL-LENS.
           05  WS-CL-LEN OCCURS 19 TIMES PIC 9(2) COMP-5.

      *> Combined lit/len + dist code lengths (max 286+32=318)
       01  WS-ALL-LENS.
           05  WS-ALL-LEN OCCURS 320 TIMES PIC 9(2) COMP-5.

      *> Huffman decode trees (flat arrays)
      *> Tree node: left-child, right-child, or value if leaf
      *> We use a simple approach: decode bit by bit
      *> Store as lookup tables: for each code length, the codes
      *>
      *> Actually, let us use a different approach:
      *> For each alphabet (litlen, dist, codelen), store
      *> code lengths, then decode by reading bits and matching.
      *>
      *> Huffman table: sorted by (length, code)
      *> We store: HT-SYM(i), HT-CODELEN(i), HT-CODE(i)
      *> Max entries: 288 for litlen, 32 for dist, 19 for codelen

      *> Litlen Huffman table
       01  WS-LL-COUNT            PIC 9(3) COMP-5.
       01  WS-LL-TABLE.
           05  WS-LL-ENTRY OCCURS 288 TIMES.
               10  WS-LL-SYM     PIC 9(4) COMP-5.
               10  WS-LL-CLEN    PIC 9(2) COMP-5.
               10  WS-LL-CODE    PIC 9(5) COMP-5.

      *> Dist Huffman table
       01  WS-DT-COUNT            PIC 9(3) COMP-5.
       01  WS-DT-TABLE.
           05  WS-DT-ENTRY OCCURS 32 TIMES.
               10  WS-DT-SYM     PIC 9(4) COMP-5.
               10  WS-DT-CLEN    PIC 9(2) COMP-5.
               10  WS-DT-CODE    PIC 9(5) COMP-5.

      *> Codelen Huffman table
       01  WS-CLT-COUNT           PIC 9(3) COMP-5.
       01  WS-CLT-TABLE.
           05  WS-CLT-ENTRY OCCURS 19 TIMES.
               10  WS-CLT-SYM    PIC 9(4) COMP-5.
               10  WS-CLT-CLEN   PIC 9(2) COMP-5.
               10  WS-CLT-CODE   PIC 9(5) COMP-5.

      *> Huffman build working vars
       01  WS-HB-LENS.
           05  WS-HB-LEN OCCURS 320 TIMES PIC 9(2) COMP-5.
       01  WS-HB-COUNT           PIC 9(3) COMP-5.
       01  WS-HB-BL-COUNT.
           05  WS-HB-BLC OCCURS 16 TIMES PIC 9(3) COMP-5.
       01  WS-HB-NEXT-CODE.
           05  WS-HB-NC OCCURS 16 TIMES PIC 9(5) COMP-5.
       01  WS-HB-MAXBITS         PIC 9(2) COMP-5.
       01  WS-HB-I               PIC 9(3) COMP-5.
       01  WS-HB-J               PIC 9(3) COMP-5.
       01  WS-HB-BITS            PIC 9(2) COMP-5.
       01  WS-HB-OCOUNT          PIC 9(3) COMP-5.
      *> Which table to build: "L"=litlen, "D"=dist, "C"=codelen
       01  WS-HB-TARGET          PIC X(1).

      *> Huffman decode working vars
       01  WS-HD-CODE            PIC 9(5) COMP-5.
       01  WS-HD-BITS            PIC 9(2) COMP-5.
       01  WS-HD-BIT             PIC 9(1) COMP-5.
       01  WS-HD-I               PIC 9(3) COMP-5.
       01  WS-HD-RESULT          PIC 9(4) COMP-5.
      *> Which table: "L"=litlen, "D"=dist, "C"=codelen
       01  WS-HD-TABLE           PIC X(1).

      *> Length/distance extra bits tables
      *> Length codes 257-285: base length + extra bits
       01  WS-LEN-BASE-TAB.
           05  FILLER PIC 9(4) COMP-5 VALUE 3.
           05  FILLER PIC 9(4) COMP-5 VALUE 4.
           05  FILLER PIC 9(4) COMP-5 VALUE 5.
           05  FILLER PIC 9(4) COMP-5 VALUE 6.
           05  FILLER PIC 9(4) COMP-5 VALUE 7.
           05  FILLER PIC 9(4) COMP-5 VALUE 8.
           05  FILLER PIC 9(4) COMP-5 VALUE 9.
           05  FILLER PIC 9(4) COMP-5 VALUE 10.
           05  FILLER PIC 9(4) COMP-5 VALUE 11.
           05  FILLER PIC 9(4) COMP-5 VALUE 13.
           05  FILLER PIC 9(4) COMP-5 VALUE 15.
           05  FILLER PIC 9(4) COMP-5 VALUE 17.
           05  FILLER PIC 9(4) COMP-5 VALUE 19.
           05  FILLER PIC 9(4) COMP-5 VALUE 23.
           05  FILLER PIC 9(4) COMP-5 VALUE 27.
           05  FILLER PIC 9(4) COMP-5 VALUE 31.
           05  FILLER PIC 9(4) COMP-5 VALUE 35.
           05  FILLER PIC 9(4) COMP-5 VALUE 43.
           05  FILLER PIC 9(4) COMP-5 VALUE 51.
           05  FILLER PIC 9(4) COMP-5 VALUE 59.
           05  FILLER PIC 9(4) COMP-5 VALUE 67.
           05  FILLER PIC 9(4) COMP-5 VALUE 83.
           05  FILLER PIC 9(4) COMP-5 VALUE 99.
           05  FILLER PIC 9(4) COMP-5 VALUE 115.
           05  FILLER PIC 9(4) COMP-5 VALUE 131.
           05  FILLER PIC 9(4) COMP-5 VALUE 163.
           05  FILLER PIC 9(4) COMP-5 VALUE 195.
           05  FILLER PIC 9(4) COMP-5 VALUE 227.
           05  FILLER PIC 9(4) COMP-5 VALUE 258.
       01  WS-LEN-BASES REDEFINES WS-LEN-BASE-TAB.
           05  WS-LEN-BASE OCCURS 29 TIMES PIC 9(4) COMP-5.

       01  WS-LEN-EXTRA-TAB.
           05  FILLER PIC 9(1) COMP-5 VALUE 0.
           05  FILLER PIC 9(1) COMP-5 VALUE 0.
           05  FILLER PIC 9(1) COMP-5 VALUE 0.
           05  FILLER PIC 9(1) COMP-5 VALUE 0.
           05  FILLER PIC 9(1) COMP-5 VALUE 0.
           05  FILLER PIC 9(1) COMP-5 VALUE 0.
           05  FILLER PIC 9(1) COMP-5 VALUE 0.
           05  FILLER PIC 9(1) COMP-5 VALUE 0.
           05  FILLER PIC 9(1) COMP-5 VALUE 1.
           05  FILLER PIC 9(1) COMP-5 VALUE 1.
           05  FILLER PIC 9(1) COMP-5 VALUE 1.
           05  FILLER PIC 9(1) COMP-5 VALUE 1.
           05  FILLER PIC 9(1) COMP-5 VALUE 2.
           05  FILLER PIC 9(1) COMP-5 VALUE 2.
           05  FILLER PIC 9(1) COMP-5 VALUE 2.
           05  FILLER PIC 9(1) COMP-5 VALUE 2.
           05  FILLER PIC 9(1) COMP-5 VALUE 3.
           05  FILLER PIC 9(1) COMP-5 VALUE 3.
           05  FILLER PIC 9(1) COMP-5 VALUE 3.
           05  FILLER PIC 9(1) COMP-5 VALUE 3.
           05  FILLER PIC 9(1) COMP-5 VALUE 4.
           05  FILLER PIC 9(1) COMP-5 VALUE 4.
           05  FILLER PIC 9(1) COMP-5 VALUE 4.
           05  FILLER PIC 9(1) COMP-5 VALUE 4.
           05  FILLER PIC 9(1) COMP-5 VALUE 5.
           05  FILLER PIC 9(1) COMP-5 VALUE 5.
           05  FILLER PIC 9(1) COMP-5 VALUE 5.
           05  FILLER PIC 9(1) COMP-5 VALUE 5.
           05  FILLER PIC 9(1) COMP-5 VALUE 0.
       01  WS-LEN-EXTRAS REDEFINES WS-LEN-EXTRA-TAB.
           05  WS-LEN-EXTRA OCCURS 29 TIMES PIC 9(1) COMP-5.

       01  WS-DIST-BASE-TAB.
           05  FILLER PIC 9(5) COMP-5 VALUE 1.
           05  FILLER PIC 9(5) COMP-5 VALUE 2.
           05  FILLER PIC 9(5) COMP-5 VALUE 3.
           05  FILLER PIC 9(5) COMP-5 VALUE 4.
           05  FILLER PIC 9(5) COMP-5 VALUE 5.
           05  FILLER PIC 9(5) COMP-5 VALUE 7.
           05  FILLER PIC 9(5) COMP-5 VALUE 9.
           05  FILLER PIC 9(5) COMP-5 VALUE 13.
           05  FILLER PIC 9(5) COMP-5 VALUE 17.
           05  FILLER PIC 9(5) COMP-5 VALUE 25.
           05  FILLER PIC 9(5) COMP-5 VALUE 33.
           05  FILLER PIC 9(5) COMP-5 VALUE 49.
           05  FILLER PIC 9(5) COMP-5 VALUE 65.
           05  FILLER PIC 9(5) COMP-5 VALUE 97.
           05  FILLER PIC 9(5) COMP-5 VALUE 129.
           05  FILLER PIC 9(5) COMP-5 VALUE 193.
           05  FILLER PIC 9(5) COMP-5 VALUE 257.
           05  FILLER PIC 9(5) COMP-5 VALUE 385.
           05  FILLER PIC 9(5) COMP-5 VALUE 513.
           05  FILLER PIC 9(5) COMP-5 VALUE 769.
           05  FILLER PIC 9(5) COMP-5 VALUE 1025.
           05  FILLER PIC 9(5) COMP-5 VALUE 1537.
           05  FILLER PIC 9(5) COMP-5 VALUE 2049.
           05  FILLER PIC 9(5) COMP-5 VALUE 3073.
           05  FILLER PIC 9(5) COMP-5 VALUE 4097.
           05  FILLER PIC 9(5) COMP-5 VALUE 6145.
           05  FILLER PIC 9(5) COMP-5 VALUE 8193.
           05  FILLER PIC 9(5) COMP-5 VALUE 12289.
           05  FILLER PIC 9(5) COMP-5 VALUE 16385.
           05  FILLER PIC 9(5) COMP-5 VALUE 24577.
       01  WS-DIST-BASES REDEFINES WS-DIST-BASE-TAB.
           05  WS-DIST-BASE OCCURS 30 TIMES PIC 9(5) COMP-5.

       01  WS-DIST-EXTRA-TAB.
           05  FILLER PIC 9(2) COMP-5 VALUE 0.
           05  FILLER PIC 9(2) COMP-5 VALUE 0.
           05  FILLER PIC 9(2) COMP-5 VALUE 0.
           05  FILLER PIC 9(2) COMP-5 VALUE 0.
           05  FILLER PIC 9(2) COMP-5 VALUE 1.
           05  FILLER PIC 9(2) COMP-5 VALUE 1.
           05  FILLER PIC 9(2) COMP-5 VALUE 2.
           05  FILLER PIC 9(2) COMP-5 VALUE 2.
           05  FILLER PIC 9(2) COMP-5 VALUE 3.
           05  FILLER PIC 9(2) COMP-5 VALUE 3.
           05  FILLER PIC 9(2) COMP-5 VALUE 4.
           05  FILLER PIC 9(2) COMP-5 VALUE 4.
           05  FILLER PIC 9(2) COMP-5 VALUE 5.
           05  FILLER PIC 9(2) COMP-5 VALUE 5.
           05  FILLER PIC 9(2) COMP-5 VALUE 6.
           05  FILLER PIC 9(2) COMP-5 VALUE 6.
           05  FILLER PIC 9(2) COMP-5 VALUE 7.
           05  FILLER PIC 9(2) COMP-5 VALUE 7.
           05  FILLER PIC 9(2) COMP-5 VALUE 8.
           05  FILLER PIC 9(2) COMP-5 VALUE 8.
           05  FILLER PIC 9(2) COMP-5 VALUE 9.
           05  FILLER PIC 9(2) COMP-5 VALUE 9.
           05  FILLER PIC 9(2) COMP-5 VALUE 10.
           05  FILLER PIC 9(2) COMP-5 VALUE 10.
           05  FILLER PIC 9(2) COMP-5 VALUE 11.
           05  FILLER PIC 9(2) COMP-5 VALUE 11.
           05  FILLER PIC 9(2) COMP-5 VALUE 12.
           05  FILLER PIC 9(2) COMP-5 VALUE 12.
           05  FILLER PIC 9(2) COMP-5 VALUE 13.
           05  FILLER PIC 9(2) COMP-5 VALUE 13.
       01  WS-DIST-EXTRAS REDEFINES WS-DIST-EXTRA-TAB.
           05  WS-DIST-EXTRA OCCURS 30 TIMES PIC 9(2) COMP-5.

      *> Decoded symbol
       01  WS-DEC-SYM            PIC 9(4) COMP-5.
       01  WS-DEC-LEN            PIC 9(5) COMP-5.
       01  WS-DEC-DIST           PIC 9(5) COMP-5.
       01  WS-COPY-SRC           PIC 9(7) COMP-5.
       01  WS-COPY-I             PIC 9(7) COMP-5.

      *> ============================================================
      *> UNFILTERED PIXEL DATA (grayscale)
      *> After PNG decode: RGB or grayscale
      *> We store grayscale as 1 byte per pixel
      *> Max 600*600 = 360000
      *> ============================================================
       01  WS-GRAY-BUF           PIC X(400000).
       01  WS-GRAY-W             PIC 9(5) COMP-5.
       01  WS-GRAY-H             PIC 9(5) COMP-5.

      *> Same for target image
       01  WS-GRAY-TGT           PIC X(400000).
       01  WS-GRAY-TGT-W         PIC 9(5) COMP-5.
       01  WS-GRAY-TGT-H         PIC 9(5) COMP-5.
      *> Saved dimensions for buffer swap
       01  WS-SAVED-W             PIC 9(5) COMP-5.
       01  WS-SAVED-H             PIC 9(5) COMP-5.
      *> Saved cell row/col indices (survives encode calls)
       01  WS-SAVED-ROW           PIC 9(3) COMP-5.
       01  WS-SAVED-COL           PIC 9(3) COMP-5.

      *> Pixel access vars
       01  WS-PX-R               PIC 9(3) COMP-5.
       01  WS-PX-G               PIC 9(3) COMP-5.
       01  WS-PX-B               PIC 9(3) COMP-5.
       01  WS-PX-GRAY            PIC 9(3) COMP-5.
       01  WS-BPP                PIC 9(1) COMP-5.
       01  WS-ROW-BYTES          PIC 9(5) COMP-5.

      *> ============================================================
      *> GRID DETECTION
      *> ============================================================
      *> Row sums and col sums of dark pixels
       01  WS-ROW-SUMS.
           05  WS-ROW-SUM OCCURS 900 TIMES PIC 9(5) COMP-5.
       01  WS-COL-SUMS.
           05  WS-COL-SUM OCCURS 900 TIMES PIC 9(5) COMP-5.

      *> Grid line positions (4 rows, 4 cols)
       01  WS-GRID-ROWS.
           05  WS-GRID-ROW OCCURS 4 TIMES PIC 9(5) COMP-5.
       01  WS-GRID-COLS.
           05  WS-GRID-COL OCCURS 4 TIMES PIC 9(5) COMP-5.

      *> Target grid
       01  WS-TGRID-ROWS.
           05  WS-TGRID-ROW OCCURS 4 TIMES PIC 9(5) COMP-5.
       01  WS-TGRID-COLS.
           05  WS-TGRID-COL OCCURS 4 TIMES PIC 9(5) COMP-5.

      *> Grid detection working vars
       01  WS-GD-MAX             PIC 9(5) COMP-5.
       01  WS-GD-THRESH          PIC 9(5) COMP-5.
       01  WS-GD-GROUPS.
           05  WS-GD-GROUP OCCURS 20 TIMES.
               10  WS-GD-GRP-START PIC 9(5) COMP-5.
               10  WS-GD-GRP-END   PIC 9(5) COMP-5.
       01  WS-GD-NGROUPS         PIC 9(3) COMP-5.
       01  WS-GD-CENTERS.
           05  WS-GD-CENTER OCCURS 20 TIMES PIC 9(5) COMP-5.
       01  WS-GD-NCENTERS        PIC 9(3) COMP-5.

      *> Combo selection (find 4 evenly-spaced)
       01  WS-GD-BEST-SCORE      PIC S9(9) COMP-5.
       01  WS-GD-FOUND           PIC X VALUE "N".
       01  WS-GD-BEST-VAR        PIC 9(5) COMP-5.
       01  WS-GD-BEST.
           05  WS-GD-BEST-V OCCURS 4 TIMES PIC 9(5) COMP-5.
       01  WS-GD-COMBO.
           05  WS-GD-C OCCURS 4 TIMES PIC 9(3) COMP-5.
       01  WS-GD-SPAN            PIC 9(5) COMP-5.
       01  WS-GD-VAR             PIC 9(5) COMP-5.
       01  WS-GD-GAP1            PIC 9(5) COMP-5.
       01  WS-GD-GAP2            PIC 9(5) COMP-5.
       01  WS-GD-GAP3            PIC 9(5) COMP-5.
       01  WS-GD-GAPMAX          PIC 9(5) COMP-5.
       01  WS-GD-GAPMIN          PIC 9(5) COMP-5.
      *> Which set to write: "R"=rows, "C"=cols
       01  WS-GD-TARGET          PIC X(1).
      *> Which image: "C"=current, "T"=target
       01  WS-GD-IMAGE           PIC X(1).

       01  CROP-MARGIN            PIC 9(3) COMP-5 VALUE 8.

      *> ============================================================
      *> CELL IMAGE BUFFERS
      *> Cell: 200x200 grayscale = 40000 bytes
      *> ============================================================
       01  WS-CELL-BUF           PIC X(40000).
       01  WS-CELL-W             PIC 9(5) COMP-5 VALUE 200.
       01  WS-CELL-H             PIC 9(5) COMP-5 VALUE 200.

      *> Rotated cell
       01  WS-ROT-BUF            PIC X(40000).

      *> ============================================================
      *> PNG WRITER
      *> ============================================================
      *> Output PNG buffer
       01  WS-PNGOUT             PIC X(200000).
       01  WS-PNGOUT-LEN         PIC 9(7) VALUE 0.
       01  WS-PNGOUT-POS         PIC 9(7).

      *> CRC32 table (256 entries * 4 bytes each)
       01  WS-CRC32-TABLE.
           05  WS-CRC32-ENTRY OCCURS 256 TIMES
                                  PIC 9(10) COMP-5.
       01  WS-CRC32-INIT         PIC X VALUE "N".
       01  WS-CRC32-VAL          PIC 9(10) COMP-5.
       01  WS-CRC32-C            PIC 9(10) COMP-5.
       01  WS-CRC32-N            PIC 9(3) COMP-5.
       01  WS-CRC32-K            PIC 9(2) COMP-5.
       01  WS-CRC32-IDX          PIC 9(3) COMP-5.

      *> Adler32
       01  WS-ADLER-A            PIC 9(10) COMP-5.
       01  WS-ADLER-B            PIC 9(10) COMP-5.
       01  WS-ADLER-VAL          PIC 9(10) COMP-5.
      *> CRC32 byte-XOR intermediate results (safe from XOR clobbering)
       01  WS-XOR-B0             PIC 9(3) COMP-5.
       01  WS-XOR-B1             PIC 9(3) COMP-5.
       01  WS-XOR-B2             PIC 9(3) COMP-5.

      *> PNG write helpers
       01  WS-PW-CHUNK-DATA      PIC X(125000).
       01  WS-PW-CHUNK-LEN       PIC 9(7) COMP-5.
       01  WS-PW-CHUNK-TYPE      PIC X(4).

      *> IDAT data for writer (uncompressed deflate)
       01  WS-IDAT-BUF           PIC X(125000).
       01  WS-IDAT-LEN           PIC 9(7) COMP-5.

      *> ============================================================
      *> BASE64
      *> ============================================================
       01  WS-B64-TABLE           PIC X(64) VALUE
           "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           & "abcdefghijklmnopqrstuvwxyz"
           & "0123456789+/".
       01  WS-B64-SRC-BUF         PIC X(200000).
       01  WS-B64-SRC-LEN         PIC 9(7).
       01  WS-B64-OUT-BUF         PIC X(270000).
       01  WS-B64-OUT-LEN         PIC 9(7) VALUE 0.
       01  WS-B64-I               PIC 9(7).
       01  WS-B64-PTR             PIC 9(7).
       01  WS-B1                  PIC 9(3).
       01  WS-B2                  PIC 9(3).
       01  WS-B3                  PIC 9(3).
       01  WS-B64-IDX1            PIC 9(3).
       01  WS-B64-IDX2            PIC 9(3).
       01  WS-B64-IDX3            PIC 9(3).
       01  WS-B64-IDX4            PIC 9(3).
       01  WS-B64-REMAIN          PIC 9(7).
       01  WS-B64-BYTE-X          PIC X(1).
       01  WS-B64-ORD             PIC 9(5).

      *> ============================================================
      *> JSON BUILDING (for vision API)
      *> ============================================================
       01  WS-BIG-JSON            PIC X(999999).
       01  WS-JP                  PIC 9(7).

      *> Per-cell base64 images (5 per cell: 0/90/180/270 + target)
       01  WS-CELL-B64.
           05  WS-CELL-B64-DATA OCCURS 5 TIMES
                                  PIC X(170000).
       01  WS-CELL-B64-LENS.
           05  WS-CELL-B64-LEN OCCURS 5 TIMES
                                  PIC 9(7).

      *> Crop coordinates
       01  WS-CROP-X1             PIC 9(5) COMP-5.
       01  WS-CROP-Y1             PIC 9(5) COMP-5.
       01  WS-CROP-X2             PIC 9(5) COMP-5.
       01  WS-CROP-Y2             PIC 9(5) COMP-5.
       01  WS-CROP-W              PIC 9(5) COMP-5.
       01  WS-CROP-H              PIC 9(5) COMP-5.
       01  WS-SRC-X               PIC 9(5) COMP-5.
       01  WS-SRC-Y               PIC 9(5) COMP-5.
       01  WS-SRC-POS             PIC 9(7) COMP-5.
       01  WS-DST-POS             PIC 9(7) COMP-5.

      *> Response parsing
       01  WS-RESP-BUF            PIC X(8000).
       01  WS-RESP-LEN            PIC 9(5) VALUE 0.
       01  WS-KEY-POS             PIC 9(5).
       01  WS-K                   PIC 9(7).
       01  WS-PARSE-CH            PIC X(1).

      *> Retry / HTTP
       01  WS-STATUS-CODE         PIC X(3).
       01  WS-ATTEMPT             PIC 9(2).
       01  WS-MAX-RETRIES         PIC 9(2) VALUE 5.
       01  WS-RETRY-DONE          PIC X VALUE "N".
       01  WS-SLEEP-SECS          PIC 9(3).
       01  WS-RETRY-AFTER         PIC 9(3) VALUE 0.
       01  WS-RETRY-STR           PIC X(10).
       01  WS-PAYLOAD             PIC X(4000).

      *> Current cell being processed
       01  WS-CUR-CELL            PIC 9(2).
       01  WS-CUR-ROT             PIC 9(1).

      *> Rotation answer from API
       01  WS-ROT-ANSWER          PIC 9(1).

      *> Image source flag for crop: "C"=current, "T"=target
       01  WS-IMG-SOURCE          PIC X(1).

      *> Working vars for pixel ops
       01  WS-PX-I                PIC 9(7) COMP-5.
       01  WS-PX-J                PIC 9(5) COMP-5.
       01  WS-PX-POS              PIC 9(7) COMP-5.
       01  WS-PX-VAL              PIC 9(3) COMP-5.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S02E02 ELECTRICITY v2 ==="
           DISPLAY "=== PURE COBOL IMAGE PROCESSING ==="
           PERFORM INIT-CONFIG
           PERFORM INIT-CELL-IDS
           PERFORM INIT-CRC32-TABLE
           MOVE "N" TO WS-FLAG-FOUND

      *>   Step 1: Download both board PNGs
           DISPLAY " "
           DISPLAY "[1/6] Downloading board PNGs..."
           PERFORM STEP-DOWNLOAD-PNGS

      *>   Step 2: Decode PNG -> grayscale for current board
           DISPLAY "[2/6] Decoding PNGs..."
           MOVE "board_cur.png" TO WS-BIN-PATH
           PERFORM READ-PNG-FILE
           PERFORM PNG-PARSE-CHUNKS
           PERFORM DEFLATE-DECOMPRESS
           PERFORM PNG-UNFILTER
           PERFORM CONVERT-TO-GRAY
           MOVE WS-GRAY-W TO WS-IMG-WIDTH
           MOVE WS-GRAY-H TO WS-IMG-HEIGHT
           DISPLAY "  Current: " WS-IMG-WIDTH "x" WS-IMG-HEIGHT

      *>   Save current grayscale to TGT buffer temporarily
           MOVE WS-GRAY-BUF TO WS-GRAY-TGT
           MOVE WS-GRAY-W TO WS-SAVED-W
           MOVE WS-GRAY-H TO WS-SAVED-H

      *>   Detect grid on current (still in WS-GRAY-BUF)
           MOVE "C" TO WS-GD-IMAGE
           PERFORM DETECT-GRID
           DISPLAY "  Cur grid rows: "
               WS-GRID-ROW(1) " "
               WS-GRID-ROW(2) " "
               WS-GRID-ROW(3) " "
               WS-GRID-ROW(4)
           DISPLAY "  Cur grid cols: "
               WS-GRID-COL(1) " "
               WS-GRID-COL(2) " "
               WS-GRID-COL(3) " "
               WS-GRID-COL(4)

      *>   Now decode target
           MOVE "board_tgt.png" TO WS-BIN-PATH
           PERFORM READ-PNG-FILE
           PERFORM PNG-PARSE-CHUNKS
           PERFORM DEFLATE-DECOMPRESS
           PERFORM PNG-UNFILTER
           PERFORM CONVERT-TO-GRAY

      *>   Detect grid on target (now in WS-GRAY-BUF)
           MOVE "T" TO WS-GD-IMAGE
           PERFORM DETECT-GRID
           DISPLAY "  Tgt grid rows: "
               WS-TGRID-ROW(1) " "
               WS-TGRID-ROW(2) " "
               WS-TGRID-ROW(3) " "
               WS-TGRID-ROW(4)
           DISPLAY "  Tgt grid cols: "
               WS-TGRID-COL(1) " "
               WS-TGRID-COL(2) " "
               WS-TGRID-COL(3) " "
               WS-TGRID-COL(4)

      *>   Now: WS-GRAY-BUF = target, WS-GRAY-TGT = current
      *>   Swap so BUF=current, TGT=target
           MOVE WS-GRAY-W TO WS-GRAY-TGT-W
           MOVE WS-GRAY-H TO WS-GRAY-TGT-H
           MOVE WS-GRAY-BUF TO WS-OUT-BUF
           MOVE WS-GRAY-TGT TO WS-GRAY-BUF
           MOVE WS-OUT-BUF TO WS-GRAY-TGT
           MOVE WS-SAVED-W TO WS-GRAY-W
           MOVE WS-SAVED-H TO WS-GRAY-H

           DISPLAY "  Current gray: " WS-GRAY-W "x" WS-GRAY-H
           DISPLAY "  Target gray: "
               WS-GRAY-TGT-W "x" WS-GRAY-TGT-H

      *>   Re-init cell IDs (guard against buffer overwrite)
           PERFORM INIT-CELL-IDS

      *>   Step 3: Process each cell
           DISPLAY " "
           DISPLAY "[3/6] Processing cells..."
           PERFORM VARYING WS-CUR-CELL FROM 1 BY 1
               UNTIL WS-CUR-CELL > 9
               OR WS-FLAG-FOUND = "Y"
               PERFORM PROCESS-ONE-CELL
           END-PERFORM

      *>   Step 4: Show plan and total
           DISPLAY " "
           DISPLAY "[4/6] Rotation plan:"
           MOVE 0 TO WS-TOTAL-ROTS
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > 9
               ADD WS-PLAN-ROT(WS-IDX) TO WS-TOTAL-ROTS
               DISPLAY "  " WS-CELL-ID(WS-IDX) ": "
                   WS-PLAN-ROT(WS-IDX)
           END-PERFORM
           DISPLAY "  Total: " WS-TOTAL-ROTS

      *>   Re-init key and URL (guard against corruption)
           MOVE "REDACTED_HUB_API_KEY"
               TO WS-HUB-KEY
           MOVE "REDACTED_HUB_URL" TO WS-HUB-URL

      *>   Re-build URLs and re-reset board before rotations
           INITIALIZE WS-BOARD-URL
           STRING TRIM(WS-HUB-URL) "/data/"
               TRIM(WS-HUB-KEY) "/electricity.png"
               DELIMITED SIZE INTO WS-BOARD-URL
           END-STRING
           INITIALIZE WS-VERIFY-URL
           STRING TRIM(WS-HUB-URL) "/verify"
               DELIMITED SIZE INTO WS-VERIFY-URL
           END-STRING
           INITIALIZE WS-CMD
           STRING "curl -s -o /dev/null " WS-QT
               TRIM(WS-BOARD-URL) "?reset=1" WS-QT
               DELIMITED SIZE INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Step 5: Execute rotations
           IF WS-TOTAL-ROTS = 0
               DISPLAY "[5/6] Already solved!"
           ELSE
               DISPLAY " "
               DISPLAY "[5/6] Executing rotations..."
               PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > 9 OR WS-FLAG-FOUND = "Y"
                   IF WS-PLAN-ROT(WS-IDX) > 0
                       PERFORM DO-CELL-ROTS
                   END-IF
               END-PERFORM
           END-IF

           DISPLAY " "
           IF WS-FLAG-FOUND = "Y"
               DISPLAY "[6/6] FLAG FOUND!"
           ELSE
               DISPLAY "[6/6] No flag. Done."
           END-IF
           STOP RUN.

      *> ============================================================
      *> INIT-CONFIG
      *> ============================================================
       INIT-CONFIG.
           MOVE "REDACTED_HUB_API_KEY"
               TO WS-HUB-KEY
           MOVE "REDACTED_HUB_URL" TO WS-HUB-URL
           MOVE "REDACTED_OPENROUTER_KEY"
               & "REDACTED_OPENROUTER_KEY_PART2"
               TO WS-OR-KEY

           INITIALIZE WS-VERIFY-URL
           STRING TRIM(WS-HUB-URL) "/verify"
               DELIMITED SIZE INTO WS-VERIFY-URL
           END-STRING
           INITIALIZE WS-BOARD-URL
           STRING TRIM(WS-HUB-URL) "/data/"
               TRIM(WS-HUB-KEY) "/electricity.png"
               DELIMITED SIZE INTO WS-BOARD-URL
           END-STRING
           INITIALIZE WS-SOLVED-URL
           STRING TRIM(WS-HUB-URL)
               "/i/solved_electricity.png"
               DELIMITED SIZE INTO WS-SOLVED-URL
           END-STRING
           .

      *> ============================================================
      *> INIT-CELL-IDS
      *> ============================================================
       INIT-CELL-IDS.
           MOVE "1x1" TO WS-CELL-ID(1)
           MOVE 1     TO WS-CELL-ROW(1) WS-CELL-COL(1)
           MOVE "1x2" TO WS-CELL-ID(2)
           MOVE 1     TO WS-CELL-ROW(2)
           MOVE 2     TO WS-CELL-COL(2)
           MOVE "1x3" TO WS-CELL-ID(3)
           MOVE 1     TO WS-CELL-ROW(3)
           MOVE 3     TO WS-CELL-COL(3)
           MOVE "2x1" TO WS-CELL-ID(4)
           MOVE 2     TO WS-CELL-ROW(4)
           MOVE 1     TO WS-CELL-COL(4)
           MOVE "2x2" TO WS-CELL-ID(5)
           MOVE 2     TO WS-CELL-ROW(5)
           MOVE 2     TO WS-CELL-COL(5)
           MOVE "2x3" TO WS-CELL-ID(6)
           MOVE 2     TO WS-CELL-ROW(6)
           MOVE 3     TO WS-CELL-COL(6)
           MOVE "3x1" TO WS-CELL-ID(7)
           MOVE 3     TO WS-CELL-ROW(7)
           MOVE 1     TO WS-CELL-COL(7)
           MOVE "3x2" TO WS-CELL-ID(8)
           MOVE 3     TO WS-CELL-ROW(8)
           MOVE 2     TO WS-CELL-COL(8)
           MOVE "3x3" TO WS-CELL-ID(9)
           MOVE 3     TO WS-CELL-ROW(9)
           MOVE 3     TO WS-CELL-COL(9)
           .

      *> ============================================================
      *> STEP-DOWNLOAD-PNGS
      *> ============================================================
       STEP-DOWNLOAD-PNGS.
           INITIALIZE WS-CMD
           STRING "curl -s -o board_cur.png " WS-QT
               TRIM(WS-BOARD-URL) "?reset=1" WS-QT
               DELIMITED SIZE INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           INITIALIZE WS-CMD
           STRING "curl -s -o board_tgt.png " WS-QT
               TRIM(WS-SOLVED-URL) WS-QT
               DELIMITED SIZE INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD
           DISPLAY "  Both PNGs downloaded."
           .

      *> ============================================================
      *> READ-PNG-FILE: Read binary file into WS-PNG-BUF
      *> Uses WS-BIN-PATH for filename
      *> ============================================================
       READ-PNG-FILE.
           MOVE 0 TO WS-PNG-LEN
           MOVE 0 TO WS-BIN-EOF
           MOVE SPACES TO WS-PNG-BUF
           OPEN INPUT BIN-FILE
           IF WS-BFS NOT = "00"
               DISPLAY "  ERROR opening " TRIM(WS-BIN-PATH)
                   ": " WS-BFS
               STOP RUN
           END-IF
           PERFORM UNTIL WS-BIN-EOF = 1
               READ BIN-FILE
                   AT END
                       MOVE 1 TO WS-BIN-EOF
                   NOT AT END
                       ADD 1 TO WS-PNG-LEN
                       IF WS-PNG-LEN <= 300000
                           MOVE BIN-FILE-BYTE
                               TO WS-PNG-BUF(WS-PNG-LEN:1)
                       END-IF
               END-READ
           END-PERFORM
           CLOSE BIN-FILE
           DISPLAY "  Read " TRIM(WS-BIN-PATH) ": "
               WS-PNG-LEN " bytes"
           .

      *> ============================================================
      *> PNG-PARSE-CHUNKS: Parse PNG signature + chunks
      *> Extract IHDR dims, collect all IDAT data
      *> ============================================================
       PNG-PARSE-CHUNKS.
      *>   Verify PNG signature (first 8 bytes)
           COMPUTE WS-BYTE-VAL =
               ORD(WS-PNG-BUF(1:1)) - 1
           IF WS-BYTE-VAL NOT = 137
               DISPLAY "  ERROR: Not a PNG (byte1="
                   WS-BYTE-VAL ")"
               STOP RUN
           END-IF

           MOVE 9 TO WS-PNG-POS
           MOVE 0 TO WS-ZDATA-LEN

           PERFORM UNTIL WS-PNG-POS >= WS-PNG-LEN
      *>       Read chunk length (4 bytes big-endian)
               PERFORM READ-CHUNK-LEN
      *>       Read chunk type (4 bytes)
               MOVE WS-PNG-BUF(WS-PNG-POS:4)
                   TO WS-CHUNK-TYPE
               ADD 4 TO WS-PNG-POS

               EVALUATE WS-CHUNK-TYPE
                   WHEN "IHDR"
                       PERFORM PARSE-IHDR
                   WHEN "IDAT"
                       PERFORM COLLECT-IDAT
                   WHEN "IEND"
                       EXIT PERFORM
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE

      *>       Skip chunk data + 4-byte CRC
               ADD WS-CHUNK-LEN TO WS-PNG-POS
               ADD 4 TO WS-PNG-POS
           END-PERFORM

           DISPLAY "  IHDR: " WS-IMG-WIDTH "x" WS-IMG-HEIGHT
               " depth=" WS-IMG-BITDEPTH
               " color=" WS-IMG-COLORTYPE
           DISPLAY "  IDAT total: " WS-ZDATA-LEN " bytes"
           .

      *> ============================================================
      *> READ-CHUNK-LEN: 4 bytes big-endian -> WS-CHUNK-LEN
      *> ============================================================
       READ-CHUNK-LEN.
           COMPUTE WS-CHUNK-LEN =
               (ORD(WS-PNG-BUF(WS-PNG-POS:1)) - 1) * 16777216
             + (ORD(WS-PNG-BUF(WS-PNG-POS + 1:1)) - 1) * 65536
             + (ORD(WS-PNG-BUF(WS-PNG-POS + 2:1)) - 1) * 256
             + (ORD(WS-PNG-BUF(WS-PNG-POS + 3:1)) - 1)
           ADD 4 TO WS-PNG-POS
           .

      *> ============================================================
      *> PARSE-IHDR: Extract width, height, bit depth, color type
      *> ============================================================
       PARSE-IHDR.
           COMPUTE WS-IMG-WIDTH =
               (ORD(WS-PNG-BUF(WS-PNG-POS:1)) - 1) * 16777216
             + (ORD(WS-PNG-BUF(WS-PNG-POS + 1:1)) - 1) * 65536
             + (ORD(WS-PNG-BUF(WS-PNG-POS + 2:1)) - 1) * 256
             + (ORD(WS-PNG-BUF(WS-PNG-POS + 3:1)) - 1)
           COMPUTE WS-IMG-HEIGHT =
               (ORD(WS-PNG-BUF(WS-PNG-POS + 4:1)) - 1) * 16777216
             + (ORD(WS-PNG-BUF(WS-PNG-POS + 5:1)) - 1) * 65536
             + (ORD(WS-PNG-BUF(WS-PNG-POS + 6:1)) - 1) * 256
             + (ORD(WS-PNG-BUF(WS-PNG-POS + 7:1)) - 1)
           COMPUTE WS-IMG-BITDEPTH =
               ORD(WS-PNG-BUF(WS-PNG-POS + 8:1)) - 1
           COMPUTE WS-IMG-COLORTYPE =
               ORD(WS-PNG-BUF(WS-PNG-POS + 9:1)) - 1
           .

      *> ============================================================
      *> COLLECT-IDAT: Append IDAT data to WS-ZDATA
      *> ============================================================
       COLLECT-IDAT.
           IF WS-ZDATA-LEN + WS-CHUNK-LEN <= 300000
               MOVE WS-PNG-BUF(WS-PNG-POS:WS-CHUNK-LEN)
                   TO WS-ZDATA(WS-ZDATA-LEN + 1:WS-CHUNK-LEN)
               ADD WS-CHUNK-LEN TO WS-ZDATA-LEN
           ELSE
               DISPLAY "  WARNING: IDAT overflow!"
           END-IF
           .

      *> ============================================================
      *> DEFLATE DECOMPRESSOR
      *> Reads from WS-ZDATA (skip 2-byte zlib header)
      *> Output to WS-OUT-BUF
      *> ============================================================
       DEFLATE-DECOMPRESS.
           MOVE 0 TO WS-OUT-LEN
           MOVE 0 TO WS-BIT-POS
      *>   Skip 2-byte zlib header (CMF + FLG)
      *>   Bit position is in bits from start of zdata byte 3
           MOVE 3 TO WS-ZDATA-START
           MOVE 0 TO WS-BFINAL

           PERFORM UNTIL WS-BFINAL = 1
      *>       Read BFINAL (1 bit)
               MOVE 1 TO WS-NBITS
               PERFORM READ-BITS
               MOVE WS-READ-BITS-RESULT TO WS-BFINAL

      *>       Read BTYPE (2 bits)
               MOVE 2 TO WS-NBITS
               PERFORM READ-BITS
               MOVE WS-READ-BITS-RESULT TO WS-BTYPE

               EVALUATE WS-BTYPE
                   WHEN 0
                       PERFORM DEFLATE-STORED
                   WHEN 1
                       PERFORM BUILD-FIXED-HUFFMAN
                       PERFORM DEFLATE-HUFFMAN
                   WHEN 2
                       PERFORM BUILD-DYNAMIC-HUFFMAN
                       PERFORM DEFLATE-HUFFMAN
                   WHEN OTHER
                       DISPLAY "  ERROR: invalid BTYPE="
                           WS-BTYPE
                       STOP RUN
               END-EVALUATE
           END-PERFORM
           DISPLAY "  Decompressed: " WS-OUT-LEN " bytes"
           .

      *> ============================================================
      *> READ-BITS: Read WS-NBITS bits LSB-first from WS-ZDATA
      *> Result in WS-READ-BITS-RESULT
      *> ============================================================
       READ-BITS.
           MOVE 0 TO WS-READ-BITS-RESULT
           MOVE 1 TO WS-RB-POWER
           PERFORM VARYING WS-RB-I FROM 0 BY 1
               UNTIL WS-RB-I >= WS-NBITS
               COMPUTE WS-RB-BYTEPOS =
                   WS-ZDATA-START + WS-BIT-POS / 8
               COMPUTE WS-RB-BITOFF = MOD(WS-BIT-POS, 8)
               COMPUTE WS-RB-BYTEVAL =
                   ORD(WS-ZDATA(WS-RB-BYTEPOS:1)) - 1
               COMPUTE WS-RB-SHIFTED = WS-RB-BYTEVAL
               PERFORM VARYING WS-RB-SHCNT FROM 1 BY 1
                   UNTIL WS-RB-SHCNT > WS-RB-BITOFF
                   DIVIDE WS-RB-SHIFTED BY 2
                       GIVING WS-RB-SHIFTED
               END-PERFORM
               COMPUTE WS-RB-BIT = MOD(WS-RB-SHIFTED, 2)
               IF WS-RB-BIT = 1
                   ADD WS-RB-POWER TO WS-READ-BITS-RESULT
               END-IF
               MULTIPLY 2 BY WS-RB-POWER
               ADD 1 TO WS-BIT-POS
           END-PERFORM
           .

      *> ============================================================
      *> READ-BITS-MSB: Read WS-NBITS bits MSB-first
      *> (used for Huffman code matching)
      *> Result in WS-READ-BITS-RESULT
      *> ============================================================
       READ-BITS-MSB.
           MOVE 0 TO WS-READ-BITS-RESULT
           PERFORM VARYING WS-RB-I FROM 0 BY 1
               UNTIL WS-RB-I >= WS-NBITS
               COMPUTE WS-RB-BYTEPOS =
                   WS-ZDATA-START + WS-BIT-POS / 8
               COMPUTE WS-RB-BITOFF = MOD(WS-BIT-POS, 8)
               COMPUTE WS-RB-BYTEVAL =
                   ORD(WS-ZDATA(WS-RB-BYTEPOS:1)) - 1
               COMPUTE WS-RB-SHIFTED = WS-RB-BYTEVAL
               PERFORM VARYING WS-RB-SHCNT FROM 1 BY 1
                   UNTIL WS-RB-SHCNT > WS-RB-BITOFF
                   DIVIDE WS-RB-SHIFTED BY 2
                       GIVING WS-RB-SHIFTED
               END-PERFORM
               COMPUTE WS-RB-BIT = MOD(WS-RB-SHIFTED, 2)
               COMPUTE WS-READ-BITS-RESULT =
                   WS-READ-BITS-RESULT * 2 + WS-RB-BIT
               ADD 1 TO WS-BIT-POS
           END-PERFORM
           .

      *> ============================================================
      *> DEFLATE-STORED: Stored block (no compression)
      *> ============================================================
       DEFLATE-STORED.
      *>   Align to byte boundary
           COMPUTE WS-TMP-V = MOD(WS-BIT-POS, 8)
           IF WS-TMP-V > 0
               COMPUTE WS-BIT-POS =
                   WS-BIT-POS + 8 - WS-TMP-V
           END-IF
      *>   Read LEN (2 bytes little-endian)
           COMPUTE WS-TMP-J =
               WS-ZDATA-START + WS-BIT-POS / 8
           COMPUTE WS-BLOCK-LEN =
               (ORD(WS-ZDATA(WS-TMP-J:1)) - 1)
             + (ORD(WS-ZDATA(WS-TMP-J + 1:1)) - 1) * 256
           ADD 32 TO WS-BIT-POS
      *>   Copy LEN bytes to output
           COMPUTE WS-TMP-J =
               WS-ZDATA-START + WS-BIT-POS / 8
           MOVE WS-ZDATA(WS-TMP-J:WS-BLOCK-LEN)
               TO WS-OUT-BUF(WS-OUT-LEN + 1:WS-BLOCK-LEN)
           ADD WS-BLOCK-LEN TO WS-OUT-LEN
           COMPUTE WS-BIT-POS =
               WS-BIT-POS + WS-BLOCK-LEN * 8
           .

      *> ============================================================
      *> BUILD-FIXED-HUFFMAN: Build fixed Huffman tables
      *> Lit/len: 0-143=8bit, 144-255=9bit, 256-279=7bit, 280-287=8
      *> Distance: 0-31 = 5 bits each
      *> ============================================================
       BUILD-FIXED-HUFFMAN.
      *>   Build litlen code lengths
           MOVE 288 TO WS-HB-COUNT
           PERFORM VARYING WS-HB-I FROM 1 BY 1
               UNTIL WS-HB-I > 288
               EVALUATE TRUE
                   WHEN WS-HB-I <= 144
                       MOVE 8 TO WS-HB-LEN(WS-HB-I)
                   WHEN WS-HB-I <= 256
                       MOVE 9 TO WS-HB-LEN(WS-HB-I)
                   WHEN WS-HB-I <= 280
                       MOVE 7 TO WS-HB-LEN(WS-HB-I)
                   WHEN OTHER
                       MOVE 8 TO WS-HB-LEN(WS-HB-I)
               END-EVALUATE
           END-PERFORM
           MOVE "L" TO WS-HB-TARGET
           PERFORM BUILD-HUFFMAN-TABLE

      *>   Build distance code lengths (all 5 bits)
           MOVE 32 TO WS-HB-COUNT
           PERFORM VARYING WS-HB-I FROM 1 BY 1
               UNTIL WS-HB-I > 32
               MOVE 5 TO WS-HB-LEN(WS-HB-I)
           END-PERFORM
           MOVE "D" TO WS-HB-TARGET
           PERFORM BUILD-HUFFMAN-TABLE
           .

      *> ============================================================
      *> BUILD-DYNAMIC-HUFFMAN: Read dynamic Huffman tables
      *> ============================================================
       BUILD-DYNAMIC-HUFFMAN.
      *>   Read HLIT (5 bits) + 257
           MOVE 5 TO WS-NBITS
           PERFORM READ-BITS
           COMPUTE WS-HLIT = WS-READ-BITS-RESULT + 257

      *>   Read HDIST (5 bits) + 1
           MOVE 5 TO WS-NBITS
           PERFORM READ-BITS
           COMPUTE WS-HDIST = WS-READ-BITS-RESULT + 1

      *>   Read HCLEN (4 bits) + 4
           MOVE 4 TO WS-NBITS
           PERFORM READ-BITS
           COMPUTE WS-HCLEN = WS-READ-BITS-RESULT + 4

      *>   Read code length code lengths
           PERFORM VARYING WS-HB-I FROM 1 BY 1
               UNTIL WS-HB-I > 19
               MOVE 0 TO WS-CL-LEN(WS-HB-I)
           END-PERFORM
           PERFORM VARYING WS-HB-I FROM 1 BY 1
               UNTIL WS-HB-I > WS-HCLEN
               MOVE 3 TO WS-NBITS
               PERFORM READ-BITS
      *>       Code length order (0-based index in CL-ORD)
               COMPUTE WS-TMP-V = WS-CL-ORD(WS-HB-I) + 1
               MOVE WS-READ-BITS-RESULT TO WS-CL-LEN(WS-TMP-V)
           END-PERFORM

      *>   Build code length Huffman table
           MOVE 19 TO WS-HB-COUNT
           PERFORM VARYING WS-HB-I FROM 1 BY 1
               UNTIL WS-HB-I > 19
               MOVE WS-CL-LEN(WS-HB-I) TO WS-HB-LEN(WS-HB-I)
           END-PERFORM
           MOVE "C" TO WS-HB-TARGET
           PERFORM BUILD-HUFFMAN-TABLE

      *>   Decode litlen + dist code lengths
           COMPUTE WS-TMP-V = WS-HLIT + WS-HDIST
           MOVE 0 TO WS-TMP-I
           PERFORM UNTIL WS-TMP-I >= WS-TMP-V
      *>       Decode one symbol using code length table
               MOVE "C" TO WS-HD-TABLE
               PERFORM HUFFMAN-DECODE
               MOVE WS-HD-RESULT TO WS-DEC-SYM

               IF WS-DEC-SYM <= 15
      *>           Literal code length
                   ADD 1 TO WS-TMP-I
                   MOVE WS-DEC-SYM TO WS-ALL-LEN(WS-TMP-I)
               ELSE IF WS-DEC-SYM = 16
      *>           Repeat previous 3-6 times
                   MOVE 2 TO WS-NBITS
                   PERFORM READ-BITS
                   COMPUTE WS-TMP-K =
                       WS-READ-BITS-RESULT + 3
                   PERFORM VARYING WS-TMP-J FROM 1 BY 1
                       UNTIL WS-TMP-J > WS-TMP-K
                       ADD 1 TO WS-TMP-I
                       MOVE WS-ALL-LEN(WS-TMP-I - 1)
                           TO WS-ALL-LEN(WS-TMP-I)
                   END-PERFORM
               ELSE IF WS-DEC-SYM = 17
      *>           Repeat 0 for 3-10 times
                   MOVE 3 TO WS-NBITS
                   PERFORM READ-BITS
                   COMPUTE WS-TMP-K =
                       WS-READ-BITS-RESULT + 3
                   PERFORM VARYING WS-TMP-J FROM 1 BY 1
                       UNTIL WS-TMP-J > WS-TMP-K
                       ADD 1 TO WS-TMP-I
                       MOVE 0 TO WS-ALL-LEN(WS-TMP-I)
                   END-PERFORM
               ELSE IF WS-DEC-SYM = 18
      *>           Repeat 0 for 11-138 times
                   MOVE 7 TO WS-NBITS
                   PERFORM READ-BITS
                   COMPUTE WS-TMP-K =
                       WS-READ-BITS-RESULT + 11
                   PERFORM VARYING WS-TMP-J FROM 1 BY 1
                       UNTIL WS-TMP-J > WS-TMP-K
                       ADD 1 TO WS-TMP-I
                       MOVE 0 TO WS-ALL-LEN(WS-TMP-I)
                   END-PERFORM
               END-IF END-IF END-IF END-IF
           END-PERFORM

      *>   Build litlen table from first HLIT entries
           MOVE WS-HLIT TO WS-HB-COUNT
           PERFORM VARYING WS-HB-I FROM 1 BY 1
               UNTIL WS-HB-I > WS-HLIT
               MOVE WS-ALL-LEN(WS-HB-I) TO WS-HB-LEN(WS-HB-I)
           END-PERFORM
           MOVE "L" TO WS-HB-TARGET
           PERFORM BUILD-HUFFMAN-TABLE

      *>   Build distance table from next HDIST entries
           MOVE WS-HDIST TO WS-HB-COUNT
           PERFORM VARYING WS-HB-I FROM 1 BY 1
               UNTIL WS-HB-I > WS-HDIST
               MOVE WS-ALL-LEN(WS-HLIT + WS-HB-I)
                   TO WS-HB-LEN(WS-HB-I)
           END-PERFORM
           MOVE "D" TO WS-HB-TARGET
           PERFORM BUILD-HUFFMAN-TABLE
           .

      *> ============================================================
      *> BUILD-HUFFMAN-TABLE: From WS-HB-LEN(1..WS-HB-COUNT)
      *> Build canonical Huffman codes
      *> Target: "L"=litlen, "D"=dist, "C"=codelen
      *> ============================================================
       BUILD-HUFFMAN-TABLE.
      *>   Count code lengths
           PERFORM VARYING WS-HB-I FROM 1 BY 1
               UNTIL WS-HB-I > 15
               MOVE 0 TO WS-HB-BLC(WS-HB-I)
           END-PERFORM
           MOVE 0 TO WS-HB-MAXBITS
           PERFORM VARYING WS-HB-I FROM 1 BY 1
               UNTIL WS-HB-I > WS-HB-COUNT
               IF WS-HB-LEN(WS-HB-I) > 0
                   ADD 1 TO WS-HB-BLC(WS-HB-LEN(WS-HB-I))
                   IF WS-HB-LEN(WS-HB-I) > WS-HB-MAXBITS
                       MOVE WS-HB-LEN(WS-HB-I)
                           TO WS-HB-MAXBITS
                   END-IF
               END-IF
           END-PERFORM

      *>   Compute next code for each length
           MOVE 0 TO WS-HB-NC(1)
           PERFORM VARYING WS-HB-BITS FROM 2 BY 1
               UNTIL WS-HB-BITS > 15
               COMPUTE WS-HB-NC(WS-HB-BITS) =
                   (WS-HB-NC(WS-HB-BITS - 1)
                    + WS-HB-BLC(WS-HB-BITS - 1)) * 2
           END-PERFORM

      *>   Assign codes to symbols
           MOVE 0 TO WS-HB-OCOUNT
           PERFORM VARYING WS-HB-I FROM 1 BY 1
               UNTIL WS-HB-I > WS-HB-COUNT
               IF WS-HB-LEN(WS-HB-I) > 0
                   ADD 1 TO WS-HB-OCOUNT
                   MOVE WS-HB-LEN(WS-HB-I) TO WS-HB-BITS
                   EVALUATE WS-HB-TARGET
                       WHEN "L"
                           COMPUTE WS-LL-SYM(WS-HB-OCOUNT) =
                               WS-HB-I - 1
                           MOVE WS-HB-BITS
                               TO WS-LL-CLEN(WS-HB-OCOUNT)
                           MOVE WS-HB-NC(WS-HB-BITS)
                               TO WS-LL-CODE(WS-HB-OCOUNT)
                       WHEN "D"
                           COMPUTE WS-DT-SYM(WS-HB-OCOUNT) =
                               WS-HB-I - 1
                           MOVE WS-HB-BITS
                               TO WS-DT-CLEN(WS-HB-OCOUNT)
                           MOVE WS-HB-NC(WS-HB-BITS)
                               TO WS-DT-CODE(WS-HB-OCOUNT)
                       WHEN "C"
                           COMPUTE WS-CLT-SYM(WS-HB-OCOUNT) =
                               WS-HB-I - 1
                           MOVE WS-HB-BITS
                               TO WS-CLT-CLEN(WS-HB-OCOUNT)
                           MOVE WS-HB-NC(WS-HB-BITS)
                               TO WS-CLT-CODE(WS-HB-OCOUNT)
                   END-EVALUATE
                   ADD 1 TO WS-HB-NC(WS-HB-BITS)
               END-IF
           END-PERFORM

           EVALUATE WS-HB-TARGET
               WHEN "L" MOVE WS-HB-OCOUNT TO WS-LL-COUNT
               WHEN "D" MOVE WS-HB-OCOUNT TO WS-DT-COUNT
               WHEN "C" MOVE WS-HB-OCOUNT TO WS-CLT-COUNT
           END-EVALUATE
           .

      *> ============================================================
      *> HUFFMAN-DECODE: Decode one symbol
      *> WS-HD-TABLE: "L"=litlen, "D"=dist, "C"=codelen
      *> Result in WS-HD-RESULT
      *> ============================================================
       HUFFMAN-DECODE.
           MOVE 0 TO WS-HD-CODE
           MOVE 0 TO WS-HD-BITS

           PERFORM UNTIL WS-HD-BITS > 15
      *>       Read one bit (LSB-first in byte, MSB-first code)
               COMPUTE WS-RB-BYTEPOS =
                   WS-ZDATA-START + WS-BIT-POS / 8
               COMPUTE WS-RB-BITOFF = MOD(WS-BIT-POS, 8)
               COMPUTE WS-RB-BYTEVAL =
                   ORD(WS-ZDATA(WS-RB-BYTEPOS:1)) - 1
               COMPUTE WS-RB-SHIFTED = WS-RB-BYTEVAL
               PERFORM VARYING WS-RB-SHCNT FROM 1 BY 1
                   UNTIL WS-RB-SHCNT > WS-RB-BITOFF
                   DIVIDE WS-RB-SHIFTED BY 2
                       GIVING WS-RB-SHIFTED
               END-PERFORM
               COMPUTE WS-HD-BIT =
                   MOD(WS-RB-SHIFTED, 2)
               ADD 1 TO WS-BIT-POS

               COMPUTE WS-HD-CODE =
                   WS-HD-CODE * 2 + WS-HD-BIT
               ADD 1 TO WS-HD-BITS

      *>       Search table for match
               EVALUATE WS-HD-TABLE
                   WHEN "L"
                       PERFORM VARYING WS-HD-I FROM 1 BY 1
                           UNTIL WS-HD-I > WS-LL-COUNT
                           IF WS-LL-CLEN(WS-HD-I) = WS-HD-BITS
                           AND WS-LL-CODE(WS-HD-I) = WS-HD-CODE
                               MOVE WS-LL-SYM(WS-HD-I)
                                   TO WS-HD-RESULT
                               EXIT PARAGRAPH
                           END-IF
                       END-PERFORM
                   WHEN "D"
                       PERFORM VARYING WS-HD-I FROM 1 BY 1
                           UNTIL WS-HD-I > WS-DT-COUNT
                           IF WS-DT-CLEN(WS-HD-I) = WS-HD-BITS
                           AND WS-DT-CODE(WS-HD-I) = WS-HD-CODE
                               MOVE WS-DT-SYM(WS-HD-I)
                                   TO WS-HD-RESULT
                               EXIT PARAGRAPH
                           END-IF
                       END-PERFORM
                   WHEN "C"
                       PERFORM VARYING WS-HD-I FROM 1 BY 1
                           UNTIL WS-HD-I > WS-CLT-COUNT
                           IF WS-CLT-CLEN(WS-HD-I) = WS-HD-BITS
                           AND WS-CLT-CODE(WS-HD-I) = WS-HD-CODE
                               MOVE WS-CLT-SYM(WS-HD-I)
                                   TO WS-HD-RESULT
                               EXIT PARAGRAPH
                           END-IF
                       END-PERFORM
               END-EVALUATE
           END-PERFORM

           DISPLAY "  ERROR: Huffman decode failed at bit "
               WS-BIT-POS
           MOVE 256 TO WS-HD-RESULT
           .

      *> ============================================================
      *> DEFLATE-HUFFMAN: Decode Huffman-compressed block
      *> Uses litlen and distance tables already built
      *> ============================================================
       DEFLATE-HUFFMAN.
           PERFORM UNTIL 1 = 0
      *>       Decode litlen symbol
               MOVE "L" TO WS-HD-TABLE
               PERFORM HUFFMAN-DECODE
               MOVE WS-HD-RESULT TO WS-DEC-SYM

               IF WS-DEC-SYM < 256
      *>           Literal byte
                   ADD 1 TO WS-OUT-LEN
                   MOVE CHAR(WS-DEC-SYM + 1)
                       TO WS-OUT-BUF(WS-OUT-LEN:1)
               ELSE IF WS-DEC-SYM = 256
      *>           End of block
                   EXIT PERFORM
               ELSE
      *>           Length code (257-285)
                   COMPUTE WS-TMP-V = WS-DEC-SYM - 256
                   MOVE WS-LEN-BASE(WS-TMP-V)
                       TO WS-DEC-LEN
                   IF WS-LEN-EXTRA(WS-TMP-V) > 0
                       MOVE WS-LEN-EXTRA(WS-TMP-V)
                           TO WS-NBITS
                       PERFORM READ-BITS
                       ADD WS-READ-BITS-RESULT
                           TO WS-DEC-LEN
                   END-IF

      *>           Decode distance
                   MOVE "D" TO WS-HD-TABLE
                   PERFORM HUFFMAN-DECODE
                   COMPUTE WS-TMP-V = WS-HD-RESULT + 1
                   MOVE WS-DIST-BASE(WS-TMP-V)
                       TO WS-DEC-DIST
                   IF WS-DIST-EXTRA(WS-TMP-V) > 0
                       MOVE WS-DIST-EXTRA(WS-TMP-V)
                           TO WS-NBITS
                       PERFORM READ-BITS
                       ADD WS-READ-BITS-RESULT
                           TO WS-DEC-DIST
                   END-IF

      *>           Copy from output buffer
                   COMPUTE WS-COPY-SRC =
                       WS-OUT-LEN - WS-DEC-DIST + 1
                   PERFORM VARYING WS-COPY-I FROM 1 BY 1
                       UNTIL WS-COPY-I > WS-DEC-LEN
                       ADD 1 TO WS-OUT-LEN
                       MOVE WS-OUT-BUF(WS-COPY-SRC:1)
                           TO WS-OUT-BUF(WS-OUT-LEN:1)
                       ADD 1 TO WS-COPY-SRC
                   END-PERFORM
               END-IF END-IF
           END-PERFORM
           .

      *> ============================================================
      *> PNG-UNFILTER: Apply PNG filter reversal
      *> Input: WS-OUT-BUF (filter byte + pixel data per row)
      *> Modifies WS-OUT-BUF in place
      *> ============================================================
       PNG-UNFILTER.
      *>   Determine bytes per pixel
           EVALUATE WS-IMG-COLORTYPE
               WHEN 0  MOVE 1 TO WS-BPP
               WHEN 2  MOVE 3 TO WS-BPP
               WHEN 4  MOVE 2 TO WS-BPP
               WHEN 6  MOVE 4 TO WS-BPP
               WHEN OTHER MOVE 3 TO WS-BPP
           END-EVALUATE
           COMPUTE WS-ROW-BYTES = WS-IMG-WIDTH * WS-BPP

           PERFORM VARYING WS-TMP-I FROM 0 BY 1
               UNTIL WS-TMP-I >= WS-IMG-HEIGHT
      *>       Row start position (1-based)
               COMPUTE WS-PX-POS =
                   WS-TMP-I * (WS-ROW-BYTES + 1) + 1
      *>       Filter type byte
               COMPUTE WS-BYTE-VAL =
                   ORD(WS-OUT-BUF(WS-PX-POS:1)) - 1

               EVALUATE WS-BYTE-VAL
                   WHEN 0 CONTINUE
                   WHEN 1 PERFORM UNFILTER-SUB
                   WHEN 2 PERFORM UNFILTER-UP
                   WHEN 3 PERFORM UNFILTER-AVG
                   WHEN 4 PERFORM UNFILTER-PAETH
               END-EVALUATE
           END-PERFORM
           .

      *> ============================================================
      *> UNFILTER-SUB: Filter 1
      *> ============================================================
       UNFILTER-SUB.
           PERFORM VARYING WS-TMP-J FROM 1 BY 1
               UNTIL WS-TMP-J > WS-ROW-BYTES
               COMPUTE WS-TMP-K = WS-PX-POS + WS-TMP-J
               COMPUTE WS-PX-VAL =
                   ORD(WS-OUT-BUF(WS-TMP-K:1)) - 1
               IF WS-TMP-J > WS-BPP
                   COMPUTE WS-TMP-V3 = WS-TMP-K - WS-BPP
                   COMPUTE WS-PX-VAL =
                       MOD(WS-PX-VAL +
                       (ORD(WS-OUT-BUF(WS-TMP-V3:1)) - 1),
                       256)
               END-IF
               MOVE CHAR(WS-PX-VAL + 1)
                   TO WS-OUT-BUF(WS-TMP-K:1)
           END-PERFORM
           .

      *> ============================================================
      *> UNFILTER-UP: Filter 2
      *> ============================================================
       UNFILTER-UP.
           PERFORM VARYING WS-TMP-J FROM 1 BY 1
               UNTIL WS-TMP-J > WS-ROW-BYTES
               COMPUTE WS-TMP-K = WS-PX-POS + WS-TMP-J
               COMPUTE WS-PX-VAL =
                   ORD(WS-OUT-BUF(WS-TMP-K:1)) - 1
               IF WS-TMP-I > 0
                   COMPUTE WS-TMP-V3 =
                       WS-TMP-K - WS-ROW-BYTES - 1
                   COMPUTE WS-PX-VAL =
                       MOD(WS-PX-VAL +
                       (ORD(WS-OUT-BUF(WS-TMP-V3:1)) - 1),
                       256)
               END-IF
               MOVE CHAR(WS-PX-VAL + 1)
                   TO WS-OUT-BUF(WS-TMP-K:1)
           END-PERFORM
           .

      *> ============================================================
      *> UNFILTER-AVG: Filter 3
      *> ============================================================
       UNFILTER-AVG.
           PERFORM VARYING WS-TMP-J FROM 1 BY 1
               UNTIL WS-TMP-J > WS-ROW-BYTES
               COMPUTE WS-TMP-K = WS-PX-POS + WS-TMP-J
               COMPUTE WS-PX-VAL =
                   ORD(WS-OUT-BUF(WS-TMP-K:1)) - 1
      *>       a = left pixel (or 0)
               MOVE 0 TO WS-TMP-V
               IF WS-TMP-J > WS-BPP
                   COMPUTE WS-TMP-V3 = WS-TMP-K - WS-BPP
                   COMPUTE WS-TMP-V =
                       ORD(WS-OUT-BUF(WS-TMP-V3:1)) - 1
               END-IF
      *>       b = above pixel (or 0)
               MOVE 0 TO WS-TMP-V2
               IF WS-TMP-I > 0
                   COMPUTE WS-TMP-V3 =
                       WS-TMP-K - WS-ROW-BYTES - 1
                   COMPUTE WS-TMP-V2 =
                       ORD(WS-OUT-BUF(WS-TMP-V3:1)) - 1
               END-IF
               COMPUTE WS-PX-VAL =
                   MOD(WS-PX-VAL +
                   (WS-TMP-V + WS-TMP-V2) / 2, 256)
               MOVE CHAR(WS-PX-VAL + 1)
                   TO WS-OUT-BUF(WS-TMP-K:1)
           END-PERFORM
           .

      *> ============================================================
      *> UNFILTER-PAETH: Filter 4
      *> ============================================================
       UNFILTER-PAETH.
           PERFORM VARYING WS-TMP-J FROM 1 BY 1
               UNTIL WS-TMP-J > WS-ROW-BYTES
               COMPUTE WS-TMP-K = WS-PX-POS + WS-TMP-J
               COMPUTE WS-PX-VAL =
                   ORD(WS-OUT-BUF(WS-TMP-K:1)) - 1
      *>       a = left
               MOVE 0 TO WS-PX-R
               IF WS-TMP-J > WS-BPP
                   COMPUTE WS-TMP-V3 = WS-TMP-K - WS-BPP
                   COMPUTE WS-PX-R =
                       ORD(WS-OUT-BUF(WS-TMP-V3:1)) - 1
               END-IF
      *>       b = above
               MOVE 0 TO WS-PX-G
               IF WS-TMP-I > 0
                   COMPUTE WS-TMP-V3 =
                       WS-TMP-K - WS-ROW-BYTES - 1
                   COMPUTE WS-PX-G =
                       ORD(WS-OUT-BUF(WS-TMP-V3:1)) - 1
               END-IF
      *>       c = above-left
               MOVE 0 TO WS-PX-B
               IF WS-TMP-J > WS-BPP AND WS-TMP-I > 0
                   COMPUTE WS-TMP-V3 =
                       WS-TMP-K - WS-ROW-BYTES - 1 - WS-BPP
                   COMPUTE WS-PX-B =
                       ORD(WS-OUT-BUF(WS-TMP-V3:1)) - 1
               END-IF
      *>       Paeth predictor
               PERFORM PAETH-PREDICT
               COMPUTE WS-PX-VAL =
                   MOD(WS-PX-VAL + WS-PX-GRAY, 256)
               MOVE CHAR(WS-PX-VAL + 1)
                   TO WS-OUT-BUF(WS-TMP-K:1)
           END-PERFORM
           .

      *> ============================================================
      *> PAETH-PREDICT: a=WS-PX-R, b=WS-PX-G, c=WS-PX-B
      *> Result in WS-PX-GRAY
      *> ============================================================
       PAETH-PREDICT.
           COMPUTE WS-TMP-V = WS-PX-R + WS-PX-G - WS-PX-B
      *>   pa = abs(p - a)
           COMPUTE WS-TMP-V2 = WS-TMP-V - WS-PX-R
           IF WS-TMP-V2 < 0
               MULTIPLY -1 BY WS-TMP-V2
           END-IF
      *>   pb = abs(p - b)
           COMPUTE WS-TMP-V3 = WS-TMP-V - WS-PX-G
           IF WS-TMP-V3 < 0
               MULTIPLY -1 BY WS-TMP-V3
           END-IF
      *>   pc = abs(p - c)
           COMPUTE WS-TMP-V = WS-TMP-V - WS-PX-B
           IF WS-TMP-V < 0
               MULTIPLY -1 BY WS-TMP-V
           END-IF
      *>   Return nearest
           IF WS-TMP-V2 <= WS-TMP-V3
           AND WS-TMP-V2 <= WS-TMP-V
               MOVE WS-PX-R TO WS-PX-GRAY
           ELSE IF WS-TMP-V3 <= WS-TMP-V
               MOVE WS-PX-G TO WS-PX-GRAY
           ELSE
               MOVE WS-PX-B TO WS-PX-GRAY
           END-IF END-IF
           .

      *> ============================================================
      *> CONVERT-TO-GRAY: Convert decoded pixels to grayscale
      *> Reads from WS-OUT-BUF, outputs to WS-GRAY-BUF
      *> ============================================================
       CONVERT-TO-GRAY.
           MOVE WS-IMG-WIDTH TO WS-GRAY-W
           MOVE WS-IMG-HEIGHT TO WS-GRAY-H

           EVALUATE WS-IMG-COLORTYPE
               WHEN 0
      *>           Already grayscale
                   PERFORM VARYING WS-TMP-I FROM 0 BY 1
                       UNTIL WS-TMP-I >= WS-GRAY-H
                       COMPUTE WS-PX-POS =
                           WS-TMP-I * (WS-GRAY-W + 1) + 2
                       COMPUTE WS-DST-POS =
                           WS-TMP-I * WS-GRAY-W + 1
                       MOVE WS-OUT-BUF(
                           WS-PX-POS:WS-GRAY-W)
                           TO WS-GRAY-BUF(
                           WS-DST-POS:WS-GRAY-W)
                   END-PERFORM

               WHEN 2
      *>           RGB -> grayscale
                   MOVE 0 TO WS-DST-POS
                   PERFORM VARYING WS-TMP-I FROM 0 BY 1
                       UNTIL WS-TMP-I >= WS-GRAY-H
                       COMPUTE WS-PX-POS =
                           WS-TMP-I * (WS-GRAY-W * 3 + 1) + 2
                       PERFORM VARYING WS-TMP-J FROM 0 BY 1
                           UNTIL WS-TMP-J >= WS-GRAY-W
                           COMPUTE WS-TMP-K =
                               WS-PX-POS + WS-TMP-J * 3
                           COMPUTE WS-PX-R =
                               ORD(WS-OUT-BUF(WS-TMP-K:1)) - 1
                           COMPUTE WS-PX-G =
                               ORD(WS-OUT-BUF(
                               WS-TMP-K + 1:1)) - 1
                           COMPUTE WS-PX-B =
                               ORD(WS-OUT-BUF(
                               WS-TMP-K + 2:1)) - 1
                           COMPUTE WS-PX-GRAY =
                               (WS-PX-R * 299
                               + WS-PX-G * 587
                               + WS-PX-B * 114) / 1000
                           ADD 1 TO WS-DST-POS
                           MOVE CHAR(WS-PX-GRAY + 1)
                               TO WS-GRAY-BUF(WS-DST-POS:1)
                       END-PERFORM
                   END-PERFORM

               WHEN 6
      *>           RGBA -> grayscale
                   MOVE 0 TO WS-DST-POS
                   PERFORM VARYING WS-TMP-I FROM 0 BY 1
                       UNTIL WS-TMP-I >= WS-GRAY-H
                       COMPUTE WS-PX-POS =
                           WS-TMP-I * (WS-GRAY-W * 4 + 1) + 2
                       PERFORM VARYING WS-TMP-J FROM 0 BY 1
                           UNTIL WS-TMP-J >= WS-GRAY-W
                           COMPUTE WS-TMP-K =
                               WS-PX-POS + WS-TMP-J * 4
                           COMPUTE WS-PX-R =
                               ORD(WS-OUT-BUF(WS-TMP-K:1)) - 1
                           COMPUTE WS-PX-G =
                               ORD(WS-OUT-BUF(
                               WS-TMP-K + 1:1)) - 1
                           COMPUTE WS-PX-B =
                               ORD(WS-OUT-BUF(
                               WS-TMP-K + 2:1)) - 1
                           COMPUTE WS-PX-GRAY =
                               (WS-PX-R * 299
                               + WS-PX-G * 587
                               + WS-PX-B * 114) / 1000
                           ADD 1 TO WS-DST-POS
                           MOVE CHAR(WS-PX-GRAY + 1)
                               TO WS-GRAY-BUF(WS-DST-POS:1)
                       END-PERFORM
                   END-PERFORM
           END-EVALUATE
           .

      *> ============================================================
      *> DETECT-GRID: Find 4 row and 4 col grid lines
      *> WS-GD-IMAGE: "C" -> uses WS-GRAY-BUF, writes GRID-ROWS/COLS
      *>              "T" -> uses WS-GRAY-BUF, writes TGRID-ROWS/COLS
      *> ============================================================
       DETECT-GRID.
      *>   Compute row sums (dark pixels per row)
           PERFORM VARYING WS-TMP-I FROM 0 BY 1
               UNTIL WS-TMP-I >= WS-GRAY-H
               MOVE 0 TO WS-ROW-SUM(WS-TMP-I + 1)
               PERFORM VARYING WS-TMP-J FROM 0 BY 1
                   UNTIL WS-TMP-J >= WS-GRAY-W
                   COMPUTE WS-PX-POS =
                       WS-TMP-I * WS-GRAY-W + WS-TMP-J + 1
                   COMPUTE WS-PX-VAL =
                       ORD(WS-GRAY-BUF(WS-PX-POS:1)) - 1
                   IF WS-PX-VAL < 80
                       ADD 1 TO WS-ROW-SUM(WS-TMP-I + 1)
                   END-IF
               END-PERFORM
           END-PERFORM

      *>   Compute col sums (dark pixels per column)
           PERFORM VARYING WS-TMP-J FROM 0 BY 1
               UNTIL WS-TMP-J >= WS-GRAY-W
               MOVE 0 TO WS-COL-SUM(WS-TMP-J + 1)
               PERFORM VARYING WS-TMP-I FROM 0 BY 1
                   UNTIL WS-TMP-I >= WS-GRAY-H
                   COMPUTE WS-PX-POS =
                       WS-TMP-I * WS-GRAY-W + WS-TMP-J + 1
                   COMPUTE WS-PX-VAL =
                       ORD(WS-GRAY-BUF(WS-PX-POS:1)) - 1
                   IF WS-PX-VAL < 80
                       ADD 1 TO WS-COL-SUM(WS-TMP-J + 1)
                   END-IF
               END-PERFORM
           END-PERFORM

      *>   Find row grid lines
           MOVE "R" TO WS-GD-TARGET
           PERFORM FIND-GRID-LINES

      *>   Find col grid lines
           MOVE "C" TO WS-GD-TARGET
           PERFORM FIND-GRID-LINES
           .

      *> ============================================================
      *> FIND-GRID-LINES: Find 4 evenly-spaced grid lines
      *> WS-GD-TARGET: "R"=rows, "C"=cols
      *> ============================================================
       FIND-GRID-LINES.
      *>   Find max sum
           MOVE 0 TO WS-GD-MAX
           IF WS-GD-TARGET = "R"
               PERFORM VARYING WS-TMP-I FROM 1 BY 1
                   UNTIL WS-TMP-I > WS-GRAY-H
                   IF WS-ROW-SUM(WS-TMP-I) > WS-GD-MAX
                       MOVE WS-ROW-SUM(WS-TMP-I)
                           TO WS-GD-MAX
                   END-IF
               END-PERFORM
           ELSE
               PERFORM VARYING WS-TMP-I FROM 1 BY 1
                   UNTIL WS-TMP-I > WS-GRAY-W
                   IF WS-COL-SUM(WS-TMP-I) > WS-GD-MAX
                       MOVE WS-COL-SUM(WS-TMP-I)
                           TO WS-GD-MAX
                   END-IF
               END-PERFORM
           END-IF

      *>   Threshold = max * 0.3
           COMPUTE WS-GD-THRESH = WS-GD-MAX * 3 / 10

      *>   Find significant positions and group them
           MOVE 0 TO WS-GD-NGROUPS
           IF WS-GD-TARGET = "R"
               MOVE WS-GRAY-H TO WS-TMP-V
           ELSE
               MOVE WS-GRAY-W TO WS-TMP-V
           END-IF

           PERFORM VARYING WS-TMP-I FROM 1 BY 1
               UNTIL WS-TMP-I > WS-TMP-V
               IF WS-GD-TARGET = "R"
                   MOVE WS-ROW-SUM(WS-TMP-I) TO WS-TMP-V2
               ELSE
                   MOVE WS-COL-SUM(WS-TMP-I) TO WS-TMP-V2
               END-IF

               IF WS-TMP-V2 > WS-GD-THRESH
      *>           Check if belongs to last group (within 5)
                   IF WS-GD-NGROUPS > 0
                       COMPUTE WS-TMP-V3 =
                           WS-TMP-I - 1
                           - WS-GD-GRP-END(WS-GD-NGROUPS)
                       IF WS-TMP-V3 <= 5
                           COMPUTE WS-GD-GRP-END(
                               WS-GD-NGROUPS) =
                               WS-TMP-I - 1
                       ELSE
                           ADD 1 TO WS-GD-NGROUPS
                           COMPUTE WS-GD-GRP-START(
                               WS-GD-NGROUPS) = WS-TMP-I - 1
                           COMPUTE WS-GD-GRP-END(
                               WS-GD-NGROUPS) = WS-TMP-I - 1
                       END-IF
                   ELSE
                       MOVE 1 TO WS-GD-NGROUPS
                       COMPUTE WS-GD-GRP-START(1) =
                           WS-TMP-I - 1
                       COMPUTE WS-GD-GRP-END(1) =
                           WS-TMP-I - 1
                   END-IF
               END-IF
           END-PERFORM

      *>   Compute centers
           MOVE WS-GD-NGROUPS TO WS-GD-NCENTERS
           PERFORM VARYING WS-TMP-I FROM 1 BY 1
               UNTIL WS-TMP-I > WS-GD-NGROUPS
               COMPUTE WS-GD-CENTER(WS-TMP-I) =
                   (WS-GD-GRP-START(WS-TMP-I)
                   + WS-GD-GRP-END(WS-TMP-I)) / 2
           END-PERFORM

      *>   Select best 4 evenly-spaced centers
           IF WS-GD-NCENTERS <= 4
      *>       Use all centers
               PERFORM VARYING WS-TMP-I FROM 1 BY 1
                   UNTIL WS-TMP-I > WS-GD-NCENTERS
                   MOVE WS-GD-CENTER(WS-TMP-I)
                       TO WS-GD-BEST-V(WS-TMP-I)
               END-PERFORM
           ELSE
      *>       Try all combinations of 4 from N centers
               MOVE "N" TO WS-GD-FOUND
               MOVE 0 TO WS-GD-BEST-SCORE
               MOVE 99999 TO WS-GD-BEST-VAR
               PERFORM SELECT-BEST-4
           END-IF

      *>   Store results
           IF WS-GD-IMAGE = "C"
               IF WS-GD-TARGET = "R"
                   PERFORM VARYING WS-TMP-I FROM 1 BY 1
                       UNTIL WS-TMP-I > 4
                       MOVE WS-GD-BEST-V(WS-TMP-I)
                           TO WS-GRID-ROW(WS-TMP-I)
                   END-PERFORM
               ELSE
                   PERFORM VARYING WS-TMP-I FROM 1 BY 1
                       UNTIL WS-TMP-I > 4
                       MOVE WS-GD-BEST-V(WS-TMP-I)
                           TO WS-GRID-COL(WS-TMP-I)
                   END-PERFORM
               END-IF
           ELSE
               IF WS-GD-TARGET = "R"
                   PERFORM VARYING WS-TMP-I FROM 1 BY 1
                       UNTIL WS-TMP-I > 4
                       MOVE WS-GD-BEST-V(WS-TMP-I)
                           TO WS-TGRID-ROW(WS-TMP-I)
                   END-PERFORM
               ELSE
                   PERFORM VARYING WS-TMP-I FROM 1 BY 1
                       UNTIL WS-TMP-I > 4
                       MOVE WS-GD-BEST-V(WS-TMP-I)
                           TO WS-TGRID-COL(WS-TMP-I)
                   END-PERFORM
               END-IF
           END-IF
           .

      *> ============================================================
      *> SELECT-BEST-4: Try all C(n,4) combos from centers
      *> ============================================================
       SELECT-BEST-4.
           MOVE 1 TO WS-GD-C(1)
           PERFORM UNTIL WS-GD-C(1) > WS-GD-NCENTERS - 3
               COMPUTE WS-GD-C(2) = WS-GD-C(1) + 1
               PERFORM UNTIL WS-GD-C(2) > WS-GD-NCENTERS - 2
                   COMPUTE WS-GD-C(3) = WS-GD-C(2) + 1
                   PERFORM UNTIL
                       WS-GD-C(3) > WS-GD-NCENTERS - 1
                       COMPUTE WS-GD-C(4) = WS-GD-C(3) + 1
                       PERFORM UNTIL
                           WS-GD-C(4) > WS-GD-NCENTERS
                           PERFORM EVAL-COMBO-4
                           ADD 1 TO WS-GD-C(4)
                       END-PERFORM
                       ADD 1 TO WS-GD-C(3)
                   END-PERFORM
                   ADD 1 TO WS-GD-C(2)
               END-PERFORM
               ADD 1 TO WS-GD-C(1)
           END-PERFORM
           .

      *> ============================================================
      *> EVAL-COMBO-4: Evaluate one combo of 4 grid lines
      *> ============================================================
       EVAL-COMBO-4.
      *>   Calculate gaps
           COMPUTE WS-GD-GAP1 =
               WS-GD-CENTER(WS-GD-C(2))
               - WS-GD-CENTER(WS-GD-C(1))
           COMPUTE WS-GD-GAP2 =
               WS-GD-CENTER(WS-GD-C(3))
               - WS-GD-CENTER(WS-GD-C(2))
           COMPUTE WS-GD-GAP3 =
               WS-GD-CENTER(WS-GD-C(4))
               - WS-GD-CENTER(WS-GD-C(3))

      *>   Max and min gaps
           MOVE WS-GD-GAP1 TO WS-GD-GAPMAX
           IF WS-GD-GAP2 > WS-GD-GAPMAX
               MOVE WS-GD-GAP2 TO WS-GD-GAPMAX
           END-IF
           IF WS-GD-GAP3 > WS-GD-GAPMAX
               MOVE WS-GD-GAP3 TO WS-GD-GAPMAX
           END-IF
           MOVE WS-GD-GAP1 TO WS-GD-GAPMIN
           IF WS-GD-GAP2 < WS-GD-GAPMIN
               MOVE WS-GD-GAP2 TO WS-GD-GAPMIN
           END-IF
           IF WS-GD-GAP3 < WS-GD-GAPMIN
               MOVE WS-GD-GAP3 TO WS-GD-GAPMIN
           END-IF

           COMPUTE WS-GD-VAR = WS-GD-GAPMAX - WS-GD-GAPMIN
           COMPUTE WS-GD-SPAN =
               WS-GD-CENTER(WS-GD-C(4))
               - WS-GD-CENTER(WS-GD-C(1))

      *>   Prefer var<=5 and max span
           IF WS-GD-VAR <= 5
               IF WS-GD-FOUND = "N"
               OR WS-GD-SPAN > WS-GD-BEST-SCORE
                   MOVE "Y" TO WS-GD-FOUND
                   MOVE WS-GD-SPAN TO WS-GD-BEST-SCORE
                   PERFORM VARYING WS-TMP-I FROM 1 BY 1
                       UNTIL WS-TMP-I > 4
                       MOVE WS-GD-CENTER(WS-GD-C(WS-TMP-I))
                           TO WS-GD-BEST-V(WS-TMP-I)
                   END-PERFORM
               END-IF
           END-IF

      *>   If no combo with var<=5, track minimum var
           IF WS-GD-FOUND = "N"
               IF WS-GD-VAR < WS-GD-BEST-VAR
                   MOVE WS-GD-VAR TO WS-GD-BEST-VAR
                   PERFORM VARYING WS-TMP-I FROM 1 BY 1
                       UNTIL WS-TMP-I > 4
                       MOVE WS-GD-CENTER(WS-GD-C(WS-TMP-I))
                           TO WS-GD-BEST-V(WS-TMP-I)
                   END-PERFORM
               END-IF
           END-IF
           .

      *> ============================================================
      *> PROCESS-ONE-CELL: For cell WS-CUR-CELL
      *> 1. Crop + resize current cell, binarize
      *> 2. Generate 4 rotations (0/90/180/270)
      *> 3. Crop + resize + binarize target cell
      *> 4. Encode all 5 as PNG, base64
      *> 5. Send to Gemini, parse answer
      *> ============================================================
       PROCESS-ONE-CELL.
           DISPLAY "  Processing cell "
               TRIM(WS-CELL-ID(WS-CUR-CELL)) "..."

      *>   Compute crop coords for current board
           COMPUTE WS-SAVED-ROW = WS-CELL-ROW(WS-CUR-CELL)
           COMPUTE WS-SAVED-COL = WS-CELL-COL(WS-CUR-CELL)
           COMPUTE WS-TMP-V = WS-SAVED-ROW
           COMPUTE WS-TMP-V2 = WS-SAVED-COL

           COMPUTE WS-CROP-Y1 =
               WS-GRID-ROW(WS-TMP-V) + CROP-MARGIN
           COMPUTE WS-CROP-Y2 =
               WS-GRID-ROW(WS-TMP-V + 1) - CROP-MARGIN
           COMPUTE WS-CROP-X1 =
               WS-GRID-COL(WS-TMP-V2) + CROP-MARGIN
           COMPUTE WS-CROP-X2 =
               WS-GRID-COL(WS-TMP-V2 + 1) - CROP-MARGIN
           COMPUTE WS-CROP-W = WS-CROP-X2 - WS-CROP-X1
           COMPUTE WS-CROP-H = WS-CROP-Y2 - WS-CROP-Y1

      *>   Crop + resize to 200x200 from current gray buf
           DISPLAY "    crop: " WS-CROP-X1 "," WS-CROP-Y1
               " -> " WS-CROP-X2 "," WS-CROP-Y2
               " (" WS-CROP-W "x" WS-CROP-H ")"
           MOVE "C" TO WS-IMG-SOURCE
           PERFORM CROP-RESIZE-CELL
           DISPLAY "    crop done"

      *>   Binarize
           PERFORM BINARIZE-CELL
           DISPLAY "    binarize done"

      *>   Save original cell (rotation 0)
      *>   Encode as PNG + base64
           PERFORM ENCODE-CELL-PNG-B64
           DISPLAY "    encode rot0: " WS-B64-OUT-LEN " chars"
           DISPLAY "    copying to cell b64 slot 1..."
           MOVE WS-B64-OUT-BUF(1:WS-B64-OUT-LEN)
               TO WS-CELL-B64-DATA(1)(1:WS-B64-OUT-LEN)
           DISPLAY "    copy done"
           MOVE WS-B64-OUT-LEN TO WS-CELL-B64-LEN(1)

      *>   Rotate 90 CW and encode
           PERFORM ROTATE-90CW
           MOVE WS-ROT-BUF TO WS-CELL-BUF
           PERFORM ENCODE-CELL-PNG-B64
           MOVE WS-B64-OUT-BUF(1:WS-B64-OUT-LEN)
               TO WS-CELL-B64-DATA(2)(1:WS-B64-OUT-LEN)
           MOVE WS-B64-OUT-LEN TO WS-CELL-B64-LEN(2)

      *>   Rotate another 90 CW (=180 total)
           PERFORM ROTATE-90CW
           MOVE WS-ROT-BUF TO WS-CELL-BUF
           PERFORM ENCODE-CELL-PNG-B64
           MOVE WS-B64-OUT-BUF(1:WS-B64-OUT-LEN)
               TO WS-CELL-B64-DATA(3)(1:WS-B64-OUT-LEN)
           MOVE WS-B64-OUT-LEN TO WS-CELL-B64-LEN(3)

      *>   Rotate another 90 CW (=270 total)
           PERFORM ROTATE-90CW
           MOVE WS-ROT-BUF TO WS-CELL-BUF
           PERFORM ENCODE-CELL-PNG-B64
           MOVE WS-B64-OUT-BUF(1:WS-B64-OUT-LEN)
               TO WS-CELL-B64-DATA(4)(1:WS-B64-OUT-LEN)
           MOVE WS-B64-OUT-LEN TO WS-CELL-B64-LEN(4)

      *>   Now process target cell
           COMPUTE WS-CROP-Y1 =
               WS-TGRID-ROW(WS-SAVED-ROW) + CROP-MARGIN
           COMPUTE WS-CROP-Y2 =
               WS-TGRID-ROW(WS-SAVED-ROW + 1) - CROP-MARGIN
           COMPUTE WS-CROP-X1 =
               WS-TGRID-COL(WS-SAVED-COL) + CROP-MARGIN
           COMPUTE WS-CROP-X2 =
               WS-TGRID-COL(WS-SAVED-COL + 1) - CROP-MARGIN
           COMPUTE WS-CROP-W = WS-CROP-X2 - WS-CROP-X1
           COMPUTE WS-CROP-H = WS-CROP-Y2 - WS-CROP-Y1

           MOVE "T" TO WS-IMG-SOURCE
           PERFORM CROP-RESIZE-CELL
           PERFORM BINARIZE-CELL
           PERFORM ENCODE-CELL-PNG-B64
           MOVE WS-B64-OUT-BUF(1:WS-B64-OUT-LEN)
               TO WS-CELL-B64-DATA(5)(1:WS-B64-OUT-LEN)
           MOVE WS-B64-OUT-LEN TO WS-CELL-B64-LEN(5)

      *>   Send to Gemini
           PERFORM SEND-CELL-VISION
           MOVE WS-ROT-ANSWER TO WS-PLAN-ROT(WS-CUR-CELL)
           DISPLAY "    -> " WS-ROT-ANSWER " rotations"
           .

      *> ============================================================
      *> CROP-RESIZE-CELL: Nearest-neighbor crop+resize to 200x200
      *> Source: WS-GRAY-BUF (if "C") or WS-GRAY-TGT (if "T")
      *> Output: WS-CELL-BUF (200x200 grayscale)
      *> ============================================================
       CROP-RESIZE-CELL.
           PERFORM VARYING WS-TMP-I FROM 0 BY 1
               UNTIL WS-TMP-I >= 200
      *>       Source Y (nearest neighbor)
               COMPUTE WS-SRC-Y =
                   WS-CROP-Y1 + WS-TMP-I * WS-CROP-H / 200
               PERFORM VARYING WS-TMP-J FROM 0 BY 1
                   UNTIL WS-TMP-J >= 200
      *>           Source X
                   COMPUTE WS-SRC-X =
                       WS-CROP-X1 + WS-TMP-J * WS-CROP-W / 200
      *>           Source pixel position
                   IF WS-IMG-SOURCE = "C"
                       COMPUTE WS-SRC-POS =
                           WS-SRC-Y * WS-GRAY-W + WS-SRC-X + 1
                       COMPUTE WS-PX-VAL =
                           ORD(WS-GRAY-BUF(WS-SRC-POS:1)) - 1
                   ELSE
                       COMPUTE WS-SRC-POS =
                           WS-SRC-Y * WS-GRAY-TGT-W
                           + WS-SRC-X + 1
                       COMPUTE WS-PX-VAL =
                           ORD(WS-GRAY-TGT(WS-SRC-POS:1)) - 1
                   END-IF
      *>           Destination position
                   COMPUTE WS-DST-POS =
                       WS-TMP-I * 200 + WS-TMP-J + 1
                   MOVE CHAR(WS-PX-VAL + 1)
                       TO WS-CELL-BUF(WS-DST-POS:1)
               END-PERFORM
           END-PERFORM
           .

      *> ============================================================
      *> BINARIZE-CELL: Threshold 128 in WS-CELL-BUF
      *> ============================================================
       BINARIZE-CELL.
           PERFORM VARYING WS-PX-I FROM 1 BY 1
               UNTIL WS-PX-I > 40000
               COMPUTE WS-PX-VAL =
                   ORD(WS-CELL-BUF(WS-PX-I:1)) - 1
               IF WS-PX-VAL < 128
                   MOVE CHAR(1) TO WS-CELL-BUF(WS-PX-I:1)
               ELSE
                   MOVE CHAR(256) TO WS-CELL-BUF(WS-PX-I:1)
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> ROTATE-90CW: Rotate WS-CELL-BUF 90 CW -> WS-ROT-BUF
      *> new(x,y) = old(h-1-y, x) where h=w=200
      *> ============================================================
       ROTATE-90CW.
           MOVE SPACES TO WS-ROT-BUF
           PERFORM VARYING WS-PX-I FROM 0 BY 1
               UNTIL WS-PX-I >= 200
               PERFORM VARYING WS-PX-J FROM 0 BY 1
                   UNTIL WS-PX-J >= 200
                   COMPUTE WS-SRC-POS =
                       (199 - WS-PX-J) * 200 + WS-PX-I + 1
                   COMPUTE WS-DST-POS =
                       WS-PX-I * 200 + WS-PX-J + 1
                   MOVE WS-CELL-BUF(WS-SRC-POS:1)
                       TO WS-ROT-BUF(WS-DST-POS:1)
               END-PERFORM
           END-PERFORM
           .

      *> ============================================================
      *> ENCODE-CELL-PNG-B64: Encode WS-CELL-BUF (200x200 gray)
      *> as RGB PNG (uncompressed), then base64
      *> Output: WS-B64-OUT-BUF, WS-B64-OUT-LEN
      *> ============================================================
       ENCODE-CELL-PNG-B64.
      *>   Build PNG in WS-PNGOUT
           MOVE 0 TO WS-PNGOUT-LEN

      *>   PNG signature
           MOVE 1 TO WS-PNGOUT-POS
           MOVE CHAR(138) TO WS-PNGOUT(1:1)
           MOVE "PNG"      TO WS-PNGOUT(2:3)
           MOVE CHAR(14)  TO WS-PNGOUT(5:1)
           MOVE CHAR(11)  TO WS-PNGOUT(6:1)
           MOVE CHAR(27)  TO WS-PNGOUT(7:1)
           MOVE CHAR(11)  TO WS-PNGOUT(8:1)
           MOVE 8 TO WS-PNGOUT-LEN

      *>   IHDR chunk (13 bytes)
           MOVE SPACES TO WS-PW-CHUNK-DATA
      *>   Width: 200 = 0x00 0x00 0x00 0xC8
           MOVE CHAR(1) TO WS-PW-CHUNK-DATA(1:1)
           MOVE CHAR(1) TO WS-PW-CHUNK-DATA(2:1)
           MOVE CHAR(1) TO WS-PW-CHUNK-DATA(3:1)
           MOVE CHAR(201) TO WS-PW-CHUNK-DATA(4:1)
      *>   Height: 200
           MOVE CHAR(1) TO WS-PW-CHUNK-DATA(5:1)
           MOVE CHAR(1) TO WS-PW-CHUNK-DATA(6:1)
           MOVE CHAR(1) TO WS-PW-CHUNK-DATA(7:1)
           MOVE CHAR(201) TO WS-PW-CHUNK-DATA(8:1)
      *>   Bit depth: 8
           MOVE CHAR(9) TO WS-PW-CHUNK-DATA(9:1)
      *>   Color type: 2 (RGB)
           MOVE CHAR(3) TO WS-PW-CHUNK-DATA(10:1)
      *>   Compression, filter, interlace: 0,0,0
           MOVE CHAR(1) TO WS-PW-CHUNK-DATA(11:1)
           MOVE CHAR(1) TO WS-PW-CHUNK-DATA(12:1)
           MOVE CHAR(1) TO WS-PW-CHUNK-DATA(13:1)
           MOVE 13 TO WS-PW-CHUNK-LEN
           MOVE "IHDR" TO WS-PW-CHUNK-TYPE
           PERFORM WRITE-PNG-CHUNK

      *>   Build IDAT data (uncompressed deflate)
      *>   For 200x200 RGB with filter byte 0 per row:
      *>   Row data = 1 (filter) + 600 (RGB) = 601 bytes
      *>   Total raw = 601 * 200 = 120200 bytes
      *>   We use stored blocks (BTYPE=0), max 65535 per block

           MOVE 0 TO WS-IDAT-LEN

      *>   Zlib header: CMF=0x78, FLG=0x01
           ADD 1 TO WS-IDAT-LEN
           MOVE CHAR(121) TO WS-IDAT-BUF(WS-IDAT-LEN:1)
           ADD 1 TO WS-IDAT-LEN
           MOVE CHAR(2) TO WS-IDAT-BUF(WS-IDAT-LEN:1)

      *>   Build raw pixel data in WS-OUT-BUF temporarily
           MOVE 0 TO WS-OUT-LEN
           PERFORM VARYING WS-TMP-I FROM 0 BY 1
               UNTIL WS-TMP-I >= 200
      *>       Filter byte = 0 (None)
               ADD 1 TO WS-OUT-LEN
               MOVE CHAR(1) TO WS-OUT-BUF(WS-OUT-LEN:1)
      *>       Pixel data (gray -> RGB: R=G=B=gray)
               PERFORM VARYING WS-TMP-J FROM 0 BY 1
                   UNTIL WS-TMP-J >= 200
                   COMPUTE WS-PX-POS =
                       WS-TMP-I * 200 + WS-TMP-J + 1
                   MOVE WS-CELL-BUF(WS-PX-POS:1)
                       TO WS-B64-BYTE-X
                   ADD 1 TO WS-OUT-LEN
                   MOVE WS-B64-BYTE-X
                       TO WS-OUT-BUF(WS-OUT-LEN:1)
                   ADD 1 TO WS-OUT-LEN
                   MOVE WS-B64-BYTE-X
                       TO WS-OUT-BUF(WS-OUT-LEN:1)
                   ADD 1 TO WS-OUT-LEN
                   MOVE WS-B64-BYTE-X
                       TO WS-OUT-BUF(WS-OUT-LEN:1)
               END-PERFORM
           END-PERFORM

      *>   Compute Adler32 of raw data
           MOVE 1 TO WS-ADLER-A
           MOVE 0 TO WS-ADLER-B
           PERFORM VARYING WS-PX-I FROM 1 BY 1
               UNTIL WS-PX-I > WS-OUT-LEN
               COMPUTE WS-ADLER-A =
                   MOD(WS-ADLER-A +
                   (ORD(WS-OUT-BUF(WS-PX-I:1)) - 1), 65521)
               COMPUTE WS-ADLER-B =
                   MOD(WS-ADLER-B + WS-ADLER-A, 65521)
           END-PERFORM
           COMPUTE WS-ADLER-VAL =
               WS-ADLER-B * 65536 + WS-ADLER-A

      *>   Write stored deflate blocks
           MOVE 1 TO WS-TMP-K
           PERFORM UNTIL WS-TMP-K > WS-OUT-LEN
               COMPUTE WS-TMP-V = WS-OUT-LEN - WS-TMP-K + 1
               IF WS-TMP-V > 65535
                   MOVE 65535 TO WS-BLOCK-LEN
               ELSE
                   MOVE WS-TMP-V TO WS-BLOCK-LEN
               END-IF

      *>       BFINAL: 1 if last block, else 0
               ADD 1 TO WS-IDAT-LEN
               IF WS-TMP-K + WS-BLOCK-LEN > WS-OUT-LEN
                   MOVE CHAR(2) TO WS-IDAT-BUF(WS-IDAT-LEN:1)
               ELSE
                   MOVE CHAR(1) TO WS-IDAT-BUF(WS-IDAT-LEN:1)
               END-IF

      *>       LEN (2 bytes LE)
               ADD 1 TO WS-IDAT-LEN
               COMPUTE WS-TMP-V =
                   MOD(WS-BLOCK-LEN, 256) + 1
               MOVE CHAR(WS-TMP-V)
                   TO WS-IDAT-BUF(WS-IDAT-LEN:1)
               ADD 1 TO WS-IDAT-LEN
               COMPUTE WS-TMP-V =
                   WS-BLOCK-LEN / 256 + 1
               MOVE CHAR(WS-TMP-V)
                   TO WS-IDAT-BUF(WS-IDAT-LEN:1)

      *>       NLEN (ones complement)
               COMPUTE WS-BLOCK-NLEN =
                   65535 - WS-BLOCK-LEN
               ADD 1 TO WS-IDAT-LEN
               COMPUTE WS-TMP-V =
                   MOD(WS-BLOCK-NLEN, 256) + 1
               MOVE CHAR(WS-TMP-V)
                   TO WS-IDAT-BUF(WS-IDAT-LEN:1)
               ADD 1 TO WS-IDAT-LEN
               COMPUTE WS-TMP-V =
                   WS-BLOCK-NLEN / 256 + 1
               MOVE CHAR(WS-TMP-V)
                   TO WS-IDAT-BUF(WS-IDAT-LEN:1)

      *>       Raw data
               MOVE WS-OUT-BUF(WS-TMP-K:WS-BLOCK-LEN)
                   TO WS-IDAT-BUF(
                   WS-IDAT-LEN + 1:WS-BLOCK-LEN)
               ADD WS-BLOCK-LEN TO WS-IDAT-LEN

               ADD WS-BLOCK-LEN TO WS-TMP-K
           END-PERFORM

      *>   Adler32 checksum (4 bytes big-endian)
           ADD 1 TO WS-IDAT-LEN
           COMPUTE WS-TMP-V = WS-ADLER-VAL / 16777216
           MOVE CHAR(MOD(WS-TMP-V, 256) + 1)
               TO WS-IDAT-BUF(WS-IDAT-LEN:1)
           ADD 1 TO WS-IDAT-LEN
           COMPUTE WS-TMP-V = WS-ADLER-VAL / 65536
           MOVE CHAR(MOD(WS-TMP-V, 256) + 1)
               TO WS-IDAT-BUF(WS-IDAT-LEN:1)
           ADD 1 TO WS-IDAT-LEN
           COMPUTE WS-TMP-V = WS-ADLER-VAL / 256
           MOVE CHAR(MOD(WS-TMP-V, 256) + 1)
               TO WS-IDAT-BUF(WS-IDAT-LEN:1)
           ADD 1 TO WS-IDAT-LEN
           MOVE CHAR(MOD(WS-ADLER-VAL, 256) + 1)
               TO WS-IDAT-BUF(WS-IDAT-LEN:1)

      *>   Write IDAT chunk
           MOVE WS-IDAT-BUF(1:WS-IDAT-LEN)
               TO WS-PW-CHUNK-DATA(1:WS-IDAT-LEN)
           MOVE WS-IDAT-LEN TO WS-PW-CHUNK-LEN
           MOVE "IDAT" TO WS-PW-CHUNK-TYPE
           PERFORM WRITE-PNG-CHUNK

      *>   IEND chunk (0 bytes)
           MOVE 0 TO WS-PW-CHUNK-LEN
           MOVE "IEND" TO WS-PW-CHUNK-TYPE
           PERFORM WRITE-PNG-CHUNK

      *>   Now base64 encode WS-PNGOUT
           MOVE WS-PNGOUT(1:WS-PNGOUT-LEN)
               TO WS-B64-SRC-BUF(1:WS-PNGOUT-LEN)
           MOVE WS-PNGOUT-LEN TO WS-B64-SRC-LEN
           PERFORM BASE64-ENCODE
           .

      *> ============================================================
      *> WRITE-PNG-CHUNK: Write one PNG chunk to WS-PNGOUT
      *> Input: WS-PW-CHUNK-TYPE, WS-PW-CHUNK-DATA,
      *>        WS-PW-CHUNK-LEN
      *> ============================================================
       WRITE-PNG-CHUNK.
      *>   Length (4 bytes big-endian)
           ADD 1 TO WS-PNGOUT-LEN
           COMPUTE WS-TMP-V = WS-PW-CHUNK-LEN / 16777216
           MOVE CHAR(MOD(WS-TMP-V, 256) + 1)
               TO WS-PNGOUT(WS-PNGOUT-LEN:1)
           ADD 1 TO WS-PNGOUT-LEN
           COMPUTE WS-TMP-V = WS-PW-CHUNK-LEN / 65536
           MOVE CHAR(MOD(WS-TMP-V, 256) + 1)
               TO WS-PNGOUT(WS-PNGOUT-LEN:1)
           ADD 1 TO WS-PNGOUT-LEN
           COMPUTE WS-TMP-V = WS-PW-CHUNK-LEN / 256
           MOVE CHAR(MOD(WS-TMP-V, 256) + 1)
               TO WS-PNGOUT(WS-PNGOUT-LEN:1)
           ADD 1 TO WS-PNGOUT-LEN
           MOVE CHAR(MOD(WS-PW-CHUNK-LEN, 256) + 1)
               TO WS-PNGOUT(WS-PNGOUT-LEN:1)

      *>   Type (4 bytes)
           MOVE WS-PW-CHUNK-TYPE
               TO WS-PNGOUT(WS-PNGOUT-LEN + 1:4)
           ADD 4 TO WS-PNGOUT-LEN

      *>   Data
           IF WS-PW-CHUNK-LEN > 0
               MOVE WS-PW-CHUNK-DATA(1:WS-PW-CHUNK-LEN)
                   TO WS-PNGOUT(WS-PNGOUT-LEN + 1:
                   WS-PW-CHUNK-LEN)
               ADD WS-PW-CHUNK-LEN TO WS-PNGOUT-LEN
           END-IF

      *>   CRC32 over type+data
           MOVE 4294967295 TO WS-CRC32-VAL
      *>   Process type bytes
           PERFORM VARYING WS-TMP-I FROM 1 BY 1
               UNTIL WS-TMP-I > 4
               COMPUTE WS-CRC32-IDX =
                   MOD(ORD(WS-PW-CHUNK-TYPE(WS-TMP-I:1)) - 1,
                   256)
               COMPUTE WS-CRC32-IDX =
                   MOD(WS-CRC32-VAL, 256)
               COMPUTE WS-CRC32-IDX =
                   MOD(WS-CRC32-IDX, 256)
               PERFORM CRC32-XOR-BYTE-TYPE
           END-PERFORM
      *>   Process data bytes
           IF WS-PW-CHUNK-LEN > 0
               PERFORM VARYING WS-TMP-I FROM 1 BY 1
                   UNTIL WS-TMP-I > WS-PW-CHUNK-LEN
                   PERFORM CRC32-XOR-BYTE-DATA
               END-PERFORM
           END-IF
      *>   Final XOR
           COMPUTE WS-CRC32-VAL =
               4294967295 - WS-CRC32-VAL
      *>   Hmm, XOR with 0xFFFFFFFF. In COBOL we need bitwise XOR.
      *>   Let me use a different approach for CRC32.
      *>   Actually, let me just compute CRC32 properly.

      *>   Let me redo CRC32: use the table-driven approach
      *>   CRC32 = crc ^ 0xFFFFFFFF (final)
      *>   We need XOR which COBOL doesn't have natively.
      *>   Use: a XOR b = a + b - 2*(a AND b)
      *>   But AND is also not native. Let me use byte-by-byte.

      *>   Actually, let me compute CRC32 with a cleaner approach.
           PERFORM COMPUTE-CHUNK-CRC32-V2

      *>   Write CRC (4 bytes big-endian)
           ADD 1 TO WS-PNGOUT-LEN
           COMPUTE WS-TMP-V = WS-CRC32-VAL / 16777216
           MOVE CHAR(MOD(WS-TMP-V, 256) + 1)
               TO WS-PNGOUT(WS-PNGOUT-LEN:1)
           ADD 1 TO WS-PNGOUT-LEN
           COMPUTE WS-TMP-V = WS-CRC32-VAL / 65536
           MOVE CHAR(MOD(WS-TMP-V, 256) + 1)
               TO WS-PNGOUT(WS-PNGOUT-LEN:1)
           ADD 1 TO WS-PNGOUT-LEN
           COMPUTE WS-TMP-V = WS-CRC32-VAL / 256
           MOVE CHAR(MOD(WS-TMP-V, 256) + 1)
               TO WS-PNGOUT(WS-PNGOUT-LEN:1)
           ADD 1 TO WS-PNGOUT-LEN
           MOVE CHAR(MOD(WS-CRC32-VAL, 256) + 1)
               TO WS-PNGOUT(WS-PNGOUT-LEN:1)
           .

      *> ============================================================
      *> CRC32 TABLE INIT
      *> Precompute 256-entry CRC32 lookup table
      *> ============================================================
       INIT-CRC32-TABLE.
           PERFORM VARYING WS-CRC32-N FROM 0 BY 1
               UNTIL WS-CRC32-N >= 256
               MOVE WS-CRC32-N TO WS-CRC32-C
               PERFORM VARYING WS-CRC32-K FROM 0 BY 1
                   UNTIL WS-CRC32-K >= 8
                   IF MOD(WS-CRC32-C, 2) = 1
      *>               c = 0xEDB88320 XOR (c >> 1)
      *>               0xEDB88320 = 3988292384
                       COMPUTE WS-CRC32-C = WS-CRC32-C / 2
                       PERFORM CRC32-XOR-POLY
                   ELSE
                       COMPUTE WS-CRC32-C = WS-CRC32-C / 2
                   END-IF
               END-PERFORM
               MOVE WS-CRC32-C
                   TO WS-CRC32-ENTRY(WS-CRC32-N + 1)
           END-PERFORM
           MOVE "Y" TO WS-CRC32-INIT
           .

      *> ============================================================
      *> CRC32-XOR-POLY: WS-CRC32-C ^= 0xEDB88320
      *> XOR via: a^b = a+b - 2*(a AND b)
      *> For 32-bit, process byte by byte
      *> ============================================================
       CRC32-XOR-POLY.
      *>   0xEDB88320 bytes: ED B8 83 20
      *>   = 3988292384
      *>   Byte 0 (LSB): 0x20 = 32
      *>   Byte 1: 0x83 = 131
      *>   Byte 2: 0xB8 = 184
      *>   Byte 3 (MSB): 0xED = 237
           PERFORM CRC32-XOR-4BYTES
           .

      *> ============================================================
      *> CRC32-XOR-4BYTES: XOR WS-CRC32-C with 0xEDB88320
      *> Process byte by byte using XOR = a+b-2*(a AND b)
      *> ============================================================
       CRC32-XOR-4BYTES.
      *>   Extract bytes of WS-CRC32-C
      *>   byte0 = c mod 256, byte1 = (c/256) mod 256, etc.
      *>   XOR each byte with poly byte, reassemble
           COMPUTE WS-TMP-V = MOD(WS-CRC32-C, 256)
           PERFORM XOR-BYTE-WITH-32
           COMPUTE WS-TMP-V3 = WS-TMP-V

           COMPUTE WS-TMP-V = MOD(WS-CRC32-C / 256, 256)
           MOVE 131 TO WS-TMP-V2
           PERFORM XOR-TWO-BYTES
           COMPUTE WS-TMP-V3 = WS-TMP-V3 + WS-TMP-V * 256

           COMPUTE WS-TMP-V = MOD(WS-CRC32-C / 65536, 256)
           MOVE 184 TO WS-TMP-V2
           PERFORM XOR-TWO-BYTES
           COMPUTE WS-TMP-V3 =
               WS-TMP-V3 + WS-TMP-V * 65536

           COMPUTE WS-TMP-V = MOD(WS-CRC32-C / 16777216, 256)
           MOVE 237 TO WS-TMP-V2
           PERFORM XOR-TWO-BYTES
      *>   Reassemble using Horner's method (avoid 32-bit overflow
      *>   in WS-TMP-V * 16777216). Build MSB-first in WS-CRC32-C
      *>   which is PIC 9(10) COMP-5 = 64-bit, so each step is safe.
           MOVE WS-TMP-V TO WS-CRC32-C
           COMPUTE WS-CRC32-C =
               WS-CRC32-C * 256 + MOD(WS-TMP-V3 / 65536, 256)
           COMPUTE WS-CRC32-C =
               WS-CRC32-C * 256 + MOD(WS-TMP-V3 / 256, 256)
           COMPUTE WS-CRC32-C =
               WS-CRC32-C * 256 + MOD(WS-TMP-V3, 256)
           .

       XOR-BYTE-WITH-32.
      *>   XOR WS-TMP-V with 32
           MOVE 32 TO WS-TMP-V2
           PERFORM XOR-TWO-BYTES
           .

      *> ============================================================
      *> XOR-TWO-BYTES: XOR WS-TMP-V with WS-TMP-V2
      *> Result in WS-TMP-V
      *> Both 0-255. XOR = a+b - 2*(a AND b)
      *> Compute AND bit by bit
      *> ============================================================
       XOR-TWO-BYTES.
      *>   Compute AND bit by bit
           MOVE 0 TO WS-PX-POS
           MOVE WS-TMP-V TO WS-PX-R
           MOVE WS-TMP-V2 TO WS-PX-G
           MOVE 1 TO WS-PX-GRAY
           PERFORM VARYING WS-PX-B FROM 0 BY 1
               UNTIL WS-PX-B >= 8
               IF MOD(WS-PX-R, 2) = 1
               AND MOD(WS-PX-G, 2) = 1
                   ADD WS-PX-GRAY TO WS-PX-POS
               END-IF
               DIVIDE WS-PX-R BY 2 GIVING WS-PX-R
               DIVIDE WS-PX-G BY 2 GIVING WS-PX-G
               MULTIPLY 2 BY WS-PX-GRAY
           END-PERFORM
      *>   XOR = a + b - 2*AND
           COMPUTE WS-TMP-V =
               WS-TMP-V + WS-TMP-V2 - 2 * WS-PX-POS
           .

      *> ============================================================
      *> COMPUTE-CHUNK-CRC32: CRC32 over type+data
      *> Uses WS-CRC32-TABLE
      *> ============================================================
       COMPUTE-CHUNK-CRC32.
           MOVE 4294967295 TO WS-CRC32-VAL

      *>   Process type bytes
           PERFORM VARYING WS-TMP-I FROM 1 BY 1
               UNTIL WS-TMP-I > 4
               COMPUTE WS-BYTE-VAL =
                   ORD(WS-PW-CHUNK-TYPE(WS-TMP-I:1)) - 1
               PERFORM CRC32-UPDATE-BYTE
           END-PERFORM

      *>   Process data bytes
           IF WS-PW-CHUNK-LEN > 0
               PERFORM VARYING WS-TMP-I FROM 1 BY 1
                   UNTIL WS-TMP-I > WS-PW-CHUNK-LEN
                   COMPUTE WS-BYTE-VAL =
                       ORD(WS-PW-CHUNK-DATA(WS-TMP-I:1)) - 1
                   PERFORM CRC32-UPDATE-BYTE
               END-PERFORM
           END-IF

      *>   Final XOR with 0xFFFFFFFF
      *>   result = crc ^ 0xFFFFFFFF
      *>   Since we started with 0xFFFFFFFF, and XOR twice = identity
      *>   Actually: final = crc XOR 0xFFFFFFFF = NOT(crc)
      *>   For unsigned 32-bit: NOT(x) = 4294967295 - x
           COMPUTE WS-CRC32-VAL =
               4294967295 - WS-CRC32-VAL
           .

      *> ============================================================
      *> CRC32-UPDATE-BYTE: Update CRC32 with one byte
      *> WS-CRC32-VAL is current CRC, WS-BYTE-VAL is byte (0-255)
      *> crc = table[(crc XOR byte) & 0xFF] XOR (crc >> 8)
      *> ============================================================
       CRC32-UPDATE-BYTE.
      *>   index = (crc XOR byte) & 0xFF
      *>   First: (crc & 0xFF) XOR byte
           COMPUTE WS-TMP-V = MOD(WS-CRC32-VAL, 256)
           MOVE WS-BYTE-VAL TO WS-TMP-V2
           PERFORM XOR-TWO-BYTES
      *>   WS-TMP-V now has the XOR'd index (0-255)
           COMPUTE WS-CRC32-IDX = WS-TMP-V + 1

      *>   table_val = WS-CRC32-ENTRY(idx)
      *>   crc >> 8
           COMPUTE WS-TMP-V = WS-CRC32-VAL / 256

      *>   result = table_val XOR (crc >> 8)
      *>   Both are up to 32-bit. XOR byte by byte.
           MOVE WS-CRC32-ENTRY(WS-CRC32-IDX) TO WS-CRC32-C
      *>   XOR WS-CRC32-C with WS-TMP-V (both up to 4 bytes)
           PERFORM CRC32-XOR-TWO-LONGS
           MOVE WS-CRC32-C TO WS-CRC32-VAL
           .

      *> ============================================================
      *> CRC32-XOR-TWO-LONGS: XOR WS-CRC32-C with WS-TMP-V
      *> Both are 32-bit unsigned. Result in WS-CRC32-C.
      *> ============================================================
       CRC32-XOR-TWO-LONGS.
      *>   Byte 0
           COMPUTE WS-TMP-V2 = MOD(WS-TMP-V, 256)
           COMPUTE WS-TMP-V3 = MOD(WS-CRC32-C, 256)
           MOVE WS-TMP-V3 TO WS-PX-R
           MOVE WS-TMP-V2 TO WS-PX-G
           MOVE WS-TMP-V3 TO WS-TMP-V
           MOVE WS-TMP-V2 TO WS-TMP-V2
           PERFORM XOR-TWO-BYTES
           MOVE WS-TMP-V TO WS-ADLER-A

      *>   Restore WS-TMP-V for next bytes
      *>   Actually we destroyed WS-TMP-V. We need to save it.
      *>   Let me restructure this to avoid conflicts.

      *>   Let me use a dedicated 4-byte XOR routine.
      *>   Save the two longs first.
           CONTINUE
           .

      *> OK, the CRC32 with byte-level XOR is getting complex.
      *> Let me take a cleaner approach: compute CRC32 using
      *> a bit-by-bit method that avoids needing XOR on large values.
      *> Actually, let me just save/restore properly.

      *> Revised CRC32 approach: use temporary storage
      *> to XOR two 32-bit values byte by byte.

      *> ============================================================
      *> Actually, let me completely redo the CRC32 and XOR support
      *> using 4-byte arrays to make it manageable.
      *> ============================================================

      *> I'll define 4-byte XOR working areas:
      *> XOR-A, XOR-B, XOR-RESULT as 4-element byte arrays.
      *> Then CRC32 uses those consistently.
      *>
      *> But wait - we can't add new data items in the middle of
      *> the procedure division. All data must be in working storage.
      *> I already have them defined above. Let me just use a cleaner
      *> coding pattern.
      *>
      *> Let me restart the CRC32 with a clean implementation.

      *> ============================================================
      *> REIMPLEMENTED: COMPUTE-CHUNK-CRC32
      *> Uses lookup table. Process byte-by-byte.
      *> ============================================================
       COMPUTE-CHUNK-CRC32-V2.
           MOVE 4294967295 TO WS-CRC32-VAL

           PERFORM VARYING WS-TMP-I FROM 1 BY 1
               UNTIL WS-TMP-I > 4
               COMPUTE WS-BYTE-VAL =
                   ORD(WS-PW-CHUNK-TYPE(WS-TMP-I:1)) - 1
               PERFORM CRC32-UPDATE-BYTE-V2
           END-PERFORM

           IF WS-PW-CHUNK-LEN > 0
               PERFORM VARYING WS-TMP-I FROM 1 BY 1
                   UNTIL WS-TMP-I > WS-PW-CHUNK-LEN
                   COMPUTE WS-BYTE-VAL =
                       ORD(WS-PW-CHUNK-DATA(WS-TMP-I:1)) - 1
                   PERFORM CRC32-UPDATE-BYTE-V2
               END-PERFORM
           END-IF

           COMPUTE WS-CRC32-VAL =
               4294967295 - WS-CRC32-VAL
           .

      *> ============================================================
      *> CRC32-UPDATE-BYTE-V2
      *> crc = table[(crc ^ byte) & 0xFF] ^ (crc >> 8)
      *> ============================================================
       CRC32-UPDATE-BYTE-V2.
      *>   idx = XOR(crc & 0xFF, byte)
           MOVE MOD(WS-CRC32-VAL, 256) TO WS-TMP-V
           MOVE WS-BYTE-VAL TO WS-TMP-V2
           PERFORM XOR-TWO-BYTES
           COMPUTE WS-CRC32-IDX = WS-TMP-V + 1

      *>   shifted = crc >> 8
      *>   new_crc = XOR(table[idx], shifted)
      *>   Do 4-byte XOR between table entry and shifted crc
           MOVE WS-CRC32-ENTRY(WS-CRC32-IDX) TO WS-CRC32-C
           COMPUTE WS-ADLER-VAL = WS-CRC32-VAL / 256

      *>   XOR byte 0
           MOVE MOD(WS-CRC32-C, 256) TO WS-TMP-V
           MOVE MOD(WS-ADLER-VAL, 256) TO WS-TMP-V2
           PERFORM XOR-TWO-BYTES
           MOVE WS-TMP-V TO WS-XOR-B0

      *>   XOR byte 1
           COMPUTE WS-TMP-V = MOD(WS-CRC32-C / 256, 256)
           COMPUTE WS-TMP-V2 = MOD(WS-ADLER-VAL / 256, 256)
           PERFORM XOR-TWO-BYTES
           MOVE WS-TMP-V TO WS-XOR-B1

      *>   XOR byte 2
           COMPUTE WS-TMP-V = MOD(WS-CRC32-C / 65536, 256)
           COMPUTE WS-TMP-V2 =
               MOD(WS-ADLER-VAL / 65536, 256)
           PERFORM XOR-TWO-BYTES
           MOVE WS-TMP-V TO WS-XOR-B2

      *>   XOR byte 3
           COMPUTE WS-TMP-V =
               MOD(WS-CRC32-C / 16777216, 256)
           COMPUTE WS-TMP-V2 =
               MOD(WS-ADLER-VAL / 16777216, 256)
           PERFORM XOR-TWO-BYTES

      *>   Reassemble using Horner's method (avoid overflow in
      *>   WS-TMP-V * 16777216 when byte3 >= 128)
           MOVE WS-TMP-V TO WS-CRC32-VAL
           COMPUTE WS-CRC32-VAL =
               WS-CRC32-VAL * 256 + WS-XOR-B2
           COMPUTE WS-CRC32-VAL =
               WS-CRC32-VAL * 256 + WS-XOR-B1
           COMPUTE WS-CRC32-VAL =
               WS-CRC32-VAL * 256 + WS-XOR-B0
           .

      *> ============================================================
      *> BASE64-ENCODE: Pure COBOL base64 encoder
      *> Input:  WS-B64-SRC-BUF, WS-B64-SRC-LEN
      *> Output: WS-B64-OUT-BUF, WS-B64-OUT-LEN
      *> ============================================================
       BASE64-ENCODE.
           MOVE 1 TO WS-B64-PTR
           PERFORM VARYING WS-B64-I FROM 1 BY 3
               UNTIL WS-B64-I > WS-B64-SRC-LEN

               MOVE WS-B64-SRC-BUF(WS-B64-I:1)
                   TO WS-B64-BYTE-X
               COMPUTE WS-B64-ORD = ORD(WS-B64-BYTE-X) - 1
               MOVE WS-B64-ORD TO WS-B1

               MOVE 0 TO WS-B2
               IF WS-B64-I + 1 <= WS-B64-SRC-LEN
                   MOVE WS-B64-SRC-BUF(WS-B64-I + 1:1)
                       TO WS-B64-BYTE-X
                   COMPUTE WS-B2 = ORD(WS-B64-BYTE-X) - 1
               END-IF

               MOVE 0 TO WS-B3
               IF WS-B64-I + 2 <= WS-B64-SRC-LEN
                   MOVE WS-B64-SRC-BUF(WS-B64-I + 2:1)
                       TO WS-B64-BYTE-X
                   COMPUTE WS-B3 = ORD(WS-B64-BYTE-X) - 1
               END-IF

               DIVIDE WS-B1 BY 4 GIVING WS-B64-IDX1
               COMPUTE WS-B64-IDX2 =
                   MOD(WS-B1, 4) * 16 + WS-B2 / 16
               COMPUTE WS-B64-IDX3 =
                   MOD(WS-B2, 16) * 4 + WS-B3 / 64
               COMPUTE WS-B64-IDX4 = MOD(WS-B3, 64)

               ADD 1 TO WS-B64-IDX1
               MOVE WS-B64-TABLE(WS-B64-IDX1:1)
                   TO WS-B64-OUT-BUF(WS-B64-PTR:1)
               ADD 1 TO WS-B64-PTR

               ADD 1 TO WS-B64-IDX2
               MOVE WS-B64-TABLE(WS-B64-IDX2:1)
                   TO WS-B64-OUT-BUF(WS-B64-PTR:1)
               ADD 1 TO WS-B64-PTR

               COMPUTE WS-B64-REMAIN =
                   WS-B64-SRC-LEN - WS-B64-I + 1
               IF WS-B64-REMAIN > 1
                   ADD 1 TO WS-B64-IDX3
                   MOVE WS-B64-TABLE(WS-B64-IDX3:1)
                       TO WS-B64-OUT-BUF(WS-B64-PTR:1)
               ELSE
                   MOVE "="
                       TO WS-B64-OUT-BUF(WS-B64-PTR:1)
               END-IF
               ADD 1 TO WS-B64-PTR

               IF WS-B64-REMAIN > 2
                   ADD 1 TO WS-B64-IDX4
                   MOVE WS-B64-TABLE(WS-B64-IDX4:1)
                       TO WS-B64-OUT-BUF(WS-B64-PTR:1)
               ELSE
                   MOVE "="
                       TO WS-B64-OUT-BUF(WS-B64-PTR:1)
               END-IF
               ADD 1 TO WS-B64-PTR
           END-PERFORM

           COMPUTE WS-B64-OUT-LEN = WS-B64-PTR - 1
           .

      *> ============================================================
      *> SEND-CELL-VISION: Send 5 images to Gemini for one cell
      *> WS-CELL-B64-DATA(1..5), WS-CELL-B64-LEN(1..5)
      *> Result in WS-ROT-ANSWER
      *> ============================================================
       SEND-CELL-VISION.
           MOVE SPACES TO WS-BIG-JSON
           MOVE 1 TO WS-JP

           STRING
               '{"model":"google/gemini-3-flash-preview",'
               '"messages":[{"role":"user","content":['
               '{"type":"text","text":"'
               "I'm showing you 5 images of a cable puzzle piece."
               "\\n\\nImage 1: piece at 0 degrees (original)"
               "\\nImage 2: piece at 90 degrees clockwise"
               "\\nImage 3: piece at 180 degrees"
               "\\nImage 4: piece at 270 degrees clockwise"
               "\\nImage 5: TARGET (the required orientation)"
               "\\n\\nWhich rotation matches the TARGET exactly?"
               " Look at every detail of the cable shape."
               "\\n\\nIMPORTANT: If multiple rotations look "
               "identical (e.g. a straight pipe), choose the "
               "SMALLEST rotation number."
               "\\n\\nReply with ONLY: 0, 1, 2, or 3."
               '"}'
               DELIMITED SIZE INTO WS-BIG-JSON
               WITH POINTER WS-JP
           END-STRING

      *>   Add 5 images
           PERFORM VARYING WS-TMP-I FROM 1 BY 1
               UNTIL WS-TMP-I > 5
               STRING
                   ',{"type":"image_url","image_url":{"url":"'
                   "data:image/png;base64,"
                   DELIMITED SIZE INTO WS-BIG-JSON
                   WITH POINTER WS-JP
               END-STRING

               MOVE WS-CELL-B64-DATA(WS-TMP-I)
                   (1:WS-CELL-B64-LEN(WS-TMP-I))
                   TO WS-BIG-JSON(WS-JP:
                   WS-CELL-B64-LEN(WS-TMP-I))
               ADD WS-CELL-B64-LEN(WS-TMP-I) TO WS-JP

               STRING
                   '","detail":"high"}}'
                   DELIMITED SIZE INTO WS-BIG-JSON
                   WITH POINTER WS-JP
               END-STRING
           END-PERFORM

           STRING
               ']}],"max_tokens":20,"temperature":0}'
               DELIMITED SIZE INTO WS-BIG-JSON
               WITH POINTER WS-JP
           END-STRING

           COMPUTE WS-JSON-LEN = WS-JP - 1

      *>   Write JSON to file
           OPEN OUTPUT JSON-FILE
           MOVE WS-BIG-JSON(1:WS-JSON-LEN) TO JSON-REC
           WRITE JSON-REC
           CLOSE JSON-FILE

      *>   Send request
           MOVE 1 TO WS-ATTEMPT
           MOVE "N" TO WS-RETRY-DONE
           PERFORM UNTIL WS-ATTEMPT > WS-MAX-RETRIES
               OR WS-RETRY-DONE = "Y"
               INITIALIZE WS-CMD
               STRING
                   "curl -s -o vision_resp.json"
                   " -D headers.tmp -X POST "
                   TRIM(WS-API-URL)
                   " -H " WS-QT
                   "Content-Type: application/json"
                   WS-QT " -H " WS-QT
                   "Authorization: Bearer "
                   TRIM(WS-OR-KEY) WS-QT
                   " -d @vision_req.json"
                   DELIMITED SIZE INTO WS-CMD
               END-STRING
               CALL "SYSTEM" USING WS-CMD

               MOVE "headers.tmp" TO WS-WORK-PATH
               PERFORM READ-HTTP-HEADERS
               MOVE "work.tmp" TO WS-WORK-PATH

               IF WS-STATUS-CODE = "429"
               OR WS-STATUS-CODE = "503"
                   IF WS-RETRY-AFTER < 1
                       MOVE 5 TO WS-RETRY-AFTER
                   END-IF
                   DISPLAY "    Retry " WS-ATTEMPT
                       " (status " WS-STATUS-CODE ")"
                   MOVE WS-RETRY-AFTER TO WS-SLEEP-SECS
                   CALL "C$SLEEP" USING WS-SLEEP-SECS
                   ADD 1 TO WS-ATTEMPT
               ELSE
                   MOVE "Y" TO WS-RETRY-DONE
               END-IF
           END-PERFORM

      *>   Parse response
           MOVE "vision_resp.json" TO WS-WORK-PATH
           PERFORM READ-RESPONSE-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

      *>   Find content in response
           MOVE 0 TO WS-ROT-ANSWER
           MOVE 0 TO WS-KEY-POS
           IF WS-RESP-LEN > 11
               PERFORM VARYING WS-K FROM 1 BY 1
                   UNTIL WS-K > WS-RESP-LEN - 11
                   OR WS-KEY-POS > 0
                   IF WS-RESP-BUF(WS-K:11) =
                       '"content":"'
                       COMPUTE WS-KEY-POS = WS-K + 11
                   END-IF
               END-PERFORM
           END-IF

           IF WS-KEY-POS > 0
               PERFORM VARYING WS-K FROM WS-KEY-POS BY 1
                   UNTIL WS-K > WS-RESP-LEN
                   MOVE WS-RESP-BUF(WS-K:1) TO WS-PARSE-CH
                   IF WS-PARSE-CH >= "0"
                   AND WS-PARSE-CH <= "3"
                       MOVE WS-PARSE-CH TO WS-ROT-ANSWER
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           ELSE
               DISPLAY "    WARNING: no content in response"
               DISPLAY "    " WS-RESP-BUF(1:
                   MIN(500, WS-RESP-LEN))
           END-IF
           .

      *> ============================================================
      *> DO-CELL-ROTS: Apply rotations for cell WS-IDX
      *> ============================================================
       DO-CELL-ROTS.
           PERFORM VARYING WS-ROT-I FROM 1 BY 1
               UNTIL WS-ROT-I > WS-PLAN-ROT(WS-IDX)
               OR WS-FLAG-FOUND = "Y"
               PERFORM SEND-ROTATION
           END-PERFORM
           .

      *> ============================================================
      *> SEND-ROTATION
      *> ============================================================
       SEND-ROTATION.
           INITIALIZE WS-PAYLOAD
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME) WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "rotate" WS-QT ":"
               WS-QT TRIM(WS-CELL-ID(WS-IDX)) WS-QT "}}"
               DELIMITED SIZE INTO WS-PAYLOAD
           END-STRING
           MOVE "work.tmp" TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           WRITE WORK-REC FROM WS-PAYLOAD
           CLOSE WORK-FILE

           MOVE 1 TO WS-ATTEMPT
           MOVE "N" TO WS-RETRY-DONE
           PERFORM UNTIL WS-ATTEMPT > WS-MAX-RETRIES
               OR WS-RETRY-DONE = "Y"
               INITIALIZE WS-CMD
               STRING
                   "curl -s -o resp.json -D headers.tmp"
                   " -X POST " TRIM(WS-VERIFY-URL)
                   " -H " WS-QT
                   "Content-Type: application/json"
                   WS-QT " -d @work.tmp"
                   DELIMITED SIZE INTO WS-CMD
               END-STRING
               CALL "SYSTEM" USING WS-CMD
               MOVE "headers.tmp" TO WS-WORK-PATH
               PERFORM READ-HTTP-HEADERS
               MOVE "work.tmp" TO WS-WORK-PATH
               IF WS-STATUS-CODE = "429"
               OR WS-STATUS-CODE = "503"
                   IF WS-RETRY-AFTER < 1
                       MOVE 3 TO WS-RETRY-AFTER
                   END-IF
                   MOVE WS-RETRY-AFTER TO WS-SLEEP-SECS
                   CALL "C$SLEEP" USING WS-SLEEP-SECS
                   ADD 1 TO WS-ATTEMPT
               ELSE
                   MOVE "Y" TO WS-RETRY-DONE
               END-IF
           END-PERFORM
           MOVE "resp.json" TO WS-WORK-PATH
           PERFORM READ-RESPONSE-FILE
           MOVE "work.tmp" TO WS-WORK-PATH
           DISPLAY "  " TRIM(WS-CELL-ID(WS-IDX))
               " [" WS-ROT-I "/" WS-PLAN-ROT(WS-IDX)
               "] => " TRIM(WS-RESP-BUF)(1:200)
           MOVE 0 TO WS-TALLY-CNT
           IF WS-RESP-LEN > 0
               INSPECT WS-RESP-BUF(1:WS-RESP-LEN)
                   TALLYING WS-TALLY-CNT FOR ALL "FLG"
           END-IF
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-FLAG-FOUND
               DISPLAY "*** FLAG FOUND! ***"
               DISPLAY TRIM(WS-RESP-BUF)(1:500)
           END-IF
           .

      *> ============================================================
      *> READ-RESPONSE-FILE
      *> ============================================================
       READ-RESPONSE-FILE.
           MOVE SPACES TO WS-RESP-BUF
           MOVE 0 TO WS-RESP-LEN
           MOVE "N" TO WS-EOF
           OPEN INPUT WORK-FILE
           IF WS-FS NOT = "00" EXIT PARAGRAPH END-IF
           PERFORM UNTIL WS-EOF = "Y"
               READ WORK-FILE INTO WS-LINE
                   AT END MOVE "Y" TO WS-EOF
                   NOT AT END
                       MOVE LENGTH(TRIM(WS-LINE
                           TRAILING)) TO WS-K
                       IF WS-K > 0
                           IF WS-RESP-LEN > 0
                               ADD 1 TO WS-RESP-LEN
                               MOVE " " TO WS-RESP-BUF(
                                   WS-RESP-LEN:1)
                           END-IF
                           IF WS-RESP-LEN + WS-K <= 8000
                               MOVE WS-LINE(1:WS-K)
                                   TO WS-RESP-BUF(
                                   WS-RESP-LEN + 1:WS-K)
                               ADD WS-K TO WS-RESP-LEN
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE WORK-FILE
           MOVE "N" TO WS-EOF
           .

      *> ============================================================
      *> READ-HTTP-HEADERS
      *> ============================================================
       READ-HTTP-HEADERS.
           MOVE "000" TO WS-STATUS-CODE
           MOVE 0 TO WS-RETRY-AFTER
           MOVE "N" TO WS-EOF
           OPEN INPUT WORK-FILE
           IF WS-FS NOT = "00" EXIT PARAGRAPH END-IF
           PERFORM UNTIL WS-EOF = "Y"
               READ WORK-FILE INTO WS-LINE
                   AT END MOVE "Y" TO WS-EOF
                   NOT AT END
                       IF WS-LINE(1:5) = "HTTP/"
                           PERFORM EXTRACT-STATUS-CODE
                       END-IF
                       IF WS-LINE(1:13) = "Retry-After: "
                           PERFORM EXTRACT-RETRY-AFTER
                       END-IF
               END-READ
           END-PERFORM
           CLOSE WORK-FILE
           MOVE "N" TO WS-EOF
           .

       EXTRACT-STATUS-CODE.
           MOVE 6 TO WS-K
           PERFORM UNTIL WS-K > 15
               OR WS-LINE(WS-K:1) = " "
               ADD 1 TO WS-K
           END-PERFORM
           ADD 1 TO WS-K
           IF WS-K <= 12
               MOVE WS-LINE(WS-K:3) TO WS-STATUS-CODE
           END-IF
           .

       EXTRACT-RETRY-AFTER.
           MOVE SPACES TO WS-RETRY-STR
           MOVE TRIM(WS-LINE(14:)) TO WS-RETRY-STR
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

      *> Dummy paragraphs that were referenced but superceded
       CRC32-XOR-BYTE-TYPE.
           CONTINUE.
       CRC32-XOR-BYTE-DATA.
           CONTINUE.
