       IDENTIFICATION DIVISION.
       PROGRAM-ID. S01E02-FINDHIM.
      *> ============================================================
      *> S01E02 - Pure COBOL
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
           SELECT REQ-BODY-FILE ASSIGN TO "request_body.tmp"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  WORK-FILE.
       01  WORK-REC                PIC X(4000).

       FD  REQ-BODY-FILE.
       01  REQ-BODY-REC            PIC X(4000).

       WORKING-STORAGE SECTION.
      *> -- Config --
       01  WS-HUB-KEY              PIC X(50).
       01  WS-QT                   PIC X(1) VALUE '"'.
       01  WS-FS                   PIC XX.
       01  WS-WORK-PATH            PIC X(100) VALUE "work.tmp".

      *> -- URLs --
       01  WS-HUB-URL              PIC X(100).
       01  WS-VERIFY-URL           PIC X(200).
       01  WS-LOCATION-URL         PIC X(200).
       01  WS-ACCESS-URL           PIC X(200).

      *> -- Large JSON buffer (read whole file) --
       01  WS-JBUF                 PIC X(32000).
       01  WS-JLEN                 PIC 9(5).
       01  WS-JPOS                 PIC 9(5).
       01  WS-JVAL                 PIC X(500).
       01  WS-JNUM                 PIC X(20).

      *> -- Suspects (max 20) --
       01  WS-SUSP-CT              PIC 9(2) VALUE 0.
       01  WS-SUSPS.
           05 WS-SUSP OCCURS 20 TIMES.
              10 WS-SU-NAME        PIC X(50).
              10 WS-SU-SURNAME     PIC X(50).
              10 WS-SU-BYEAR       PIC 9(4).

      *> -- Plants (max 20) --
       01  WS-PLANT-CT             PIC 9(2) VALUE 0.
       01  WS-PLANTS.
           05 WS-PL OCCURS 20 TIMES.
              10 WS-PL-CITY        PIC X(50).
              10 WS-PL-CODE        PIC X(20).
              10 WS-PL-LAT         PIC S9(3)V9(6).
              10 WS-PL-LON         PIC S9(3)V9(6).

      *> -- Best match --
       01  WS-B-NAME               PIC X(50).
       01  WS-B-SURNAME            PIC X(50).
       01  WS-B-BYEAR              PIC 9(4).
       01  WS-B-PCODE              PIC X(20).
       01  WS-B-PCITY              PIC X(50).
       01  WS-B-DIST               PIC 9(5)V9(2) VALUE 99999.
       01  WS-B-FOUND              PIC X VALUE "N".
       01  WS-B-ACCESS             PIC 9(4) VALUE 0.

      *> -- Haversine --
       01  WS-PI-V                 PIC S9(1)V9(10)
                                   VALUE 3.1415926536.
       01  WS-RADF                 PIC S9(1)V9(10).
       01  WS-H-LAT1              PIC S9(3)V9(6).
       01  WS-H-LON1              PIC S9(3)V9(6).
       01  WS-H-LAT2              PIC S9(3)V9(6).
       01  WS-H-LON2              PIC S9(3)V9(6).
       01  WS-H-DIST              PIC S9(5)V9(2).
       01  WS-H-DLAT              PIC S9(3)V9(10).
       01  WS-H-DLON              PIC S9(3)V9(10).
       01  WS-H-A                 PIC S9(3)V9(10).
       01  WS-H-SIN1              PIC S9(3)V9(10).
       01  WS-H-SIN2              PIC S9(3)V9(10).
       01  WS-H-COS1              PIC S9(3)V9(10).
       01  WS-H-COS2              PIC S9(3)V9(10).

      *> -- Geocoding Polish char variants --
       01  WS-GEO-CITY             PIC X(100).
       01  WS-GEO-ALT              PIC X(100).
       01  WS-GEO-OK               PIC X VALUE "N".

      *> -- JSON parsing temps --
       01  WS-KEY-SEARCH           PIC X(50).
       01  WS-KEY-POS              PIC 9(5).
       01  WS-VAL-START            PIC 9(5).
       01  WS-VAL-END              PIC 9(5).
       01  WS-SCAN-POS             PIC 9(5).
       01  WS-BRACKET-DEPTH        PIC 9(2).
       01  WS-CH                   PIC X.
       01  WS-IN-STRING            PIC X VALUE "N".
       01  WS-ARR-START            PIC 9(5).
       01  WS-ARR-END              PIC 9(5).
       01  WS-OBJ-START            PIC 9(5).
       01  WS-OBJ-END              PIC 9(5).
       01  WS-OBJ-BUF              PIC X(2000).

      *> -- FIND-JSON-VAL internal scan pos (avoid clobbering) --
       01  WS-FJV-POS              PIC 9(5).

      *> -- Loop/misc --
       01  WS-I                    PIC 9(2).
       01  WS-J                    PIC 9(2).
       01  WS-K                    PIC 9(3).
       01  WS-EOF                  PIC X VALUE "N".
       01  WS-LINE                 PIC X(4000).
       01  WS-TMP                  PIC X(500).
       01  WS-TMP2                 PIC X(500).
       01  WS-NUM-STR              PIC X(10).
       01  WS-RESP                 PIC X(4000).
       01  WS-JBUF-SAVE            PIC X(32000).
       01  WS-JLEN-SAVE            PIC 9(5).

      *> -- Search patterns --
       01  WS-CODE-PAT             PIC X(6).
      *> -- Polish city variant table (ASCII -> UTF8) --
       01  WS-VARIANT-CT           PIC 9(1) VALUE 0.
       01  WS-VARIANTS.
           05 WS-VAR OCCURS 10 TIMES.
              10 WS-VAR-ASCII      PIC X(50).
              10 WS-VAR-UTF8       PIC X(50).

      *> -- System command buffer for curl binary --
       01  WS-CMD                  PIC X(4000).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S01E02 FINDHIM - COBOL ==="

      *>   Read config from environment variables
           ACCEPT WS-HUB-KEY FROM ENVIRONMENT "HUB_API_KEY"
           ACCEPT WS-HUB-URL FROM ENVIRONMENT "HUB_API_URL"

           IF WS-HUB-KEY = SPACES
               DISPLAY "ERROR: HUB_API_KEY not set"
               STOP RUN
           END-IF
           IF WS-HUB-URL = SPACES
               DISPLAY "ERROR: HUB_API_URL not set"
               STOP RUN
           END-IF

      *>   Construct URLs from base URL
           INITIALIZE WS-VERIFY-URL
           STRING TRIM(WS-HUB-URL) "/verify"
               DELIMITED SIZE INTO WS-VERIFY-URL
           END-STRING

           INITIALIZE WS-LOCATION-URL
           STRING TRIM(WS-HUB-URL) "/api/location"
               DELIMITED SIZE INTO WS-LOCATION-URL
           END-STRING

           INITIALIZE WS-ACCESS-URL
           STRING TRIM(WS-HUB-URL) "/api/accesslevel"
               DELIMITED SIZE INTO WS-ACCESS-URL
           END-STRING

           COMPUTE WS-RADF = WS-PI-V / 180

           PERFORM LOAD-SUSPECTS
           PERFORM FETCH-AND-GEOCODE-PLANTS
           PERFORM FIND-SUSPECT-NEAR-PLANT
           PERFORM GET-ACCESS-LEVEL
           PERFORM SUBMIT-ANSWER

           DISPLAY " "
           DISPLAY "=== ZAKONCZONO ==="
           STOP RUN.

      *> ============================================================
      *> READ-JSON-FILE: Read entire file into WS-JBUF
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
      *>               Append trimmed line to buffer
                       MOVE TRIM(WS-LINE) TO WS-LINE
                       MOVE LENGTH(TRIM(WS-LINE)) TO WS-K
                       IF WS-K > 0
                           IF WS-JLEN > 0
                               ADD 1 TO WS-JLEN
                               MOVE " " TO WS-JBUF(WS-JLEN:1)
                           END-IF
                           MOVE WS-LINE(1:WS-K)
                               TO WS-JBUF(WS-JLEN + 1:WS-K)
                           ADD WS-K TO WS-JLEN
                       END-IF
               END-READ
           END-PERFORM
           CLOSE WORK-FILE
           MOVE "N" TO WS-EOF
           .

      *> ============================================================
      *> WRITE-REQ-BODY: Write WS-TMP to request_body.tmp
      *> ============================================================
       WRITE-REQ-BODY.
           OPEN OUTPUT REQ-BODY-FILE
           MOVE WS-TMP TO REQ-BODY-REC
           WRITE REQ-BODY-REC
           CLOSE REQ-BODY-FILE
           .

      *> ============================================================
      *> FIND-JSON-VAL: Find "key":"value" in WS-JBUF from WS-JPOS
      *> Input: WS-KEY-SEARCH = key name, WS-JPOS = start pos
      *> Output: WS-JVAL = extracted value, WS-JPOS updated
      *> ============================================================
       FIND-JSON-VAL.
           MOVE SPACES TO WS-JVAL
           MOVE SPACES TO WS-TMP
           STRING WS-QT TRIM(WS-KEY-SEARCH) WS-QT
               DELIMITED SIZE INTO WS-TMP
           END-STRING

      *>   Find the key (uses WS-FJV-POS to avoid clobbering)
           MOVE 0 TO WS-KEY-POS
           PERFORM VARYING WS-FJV-POS FROM WS-JPOS BY 1
               UNTIL WS-FJV-POS > WS-JLEN
               OR WS-KEY-POS > 0
               IF WS-JBUF(WS-FJV-POS:LENGTH(TRIM(WS-TMP)))
                   = TRIM(WS-TMP)
                   MOVE WS-FJV-POS TO WS-KEY-POS
               END-IF
           END-PERFORM

           IF WS-KEY-POS = 0
               EXIT PARAGRAPH
           END-IF

      *>   Skip past key and colon
           COMPUTE WS-FJV-POS =
               WS-KEY-POS + LENGTH(TRIM(WS-TMP))
           PERFORM UNTIL WS-FJV-POS > WS-JLEN
               OR WS-JBUF(WS-FJV-POS:1) = ":"
               ADD 1 TO WS-FJV-POS
           END-PERFORM
           ADD 1 TO WS-FJV-POS

      *>   Skip whitespace
           PERFORM UNTIL WS-FJV-POS > WS-JLEN
               OR WS-JBUF(WS-FJV-POS:1) NOT = " "
               ADD 1 TO WS-FJV-POS
           END-PERFORM

      *>   Check if string value (starts with quote)
           IF WS-JBUF(WS-FJV-POS:1) = WS-QT
               ADD 1 TO WS-FJV-POS
               MOVE WS-FJV-POS TO WS-VAL-START
      *>       Find closing quote
               PERFORM UNTIL WS-FJV-POS > WS-JLEN
                   OR WS-JBUF(WS-FJV-POS:1) = WS-QT
                   ADD 1 TO WS-FJV-POS
               END-PERFORM
               COMPUTE WS-VAL-END = WS-FJV-POS - 1
               IF WS-VAL-END >= WS-VAL-START
                   MOVE WS-JBUF(WS-VAL-START:
                       WS-VAL-END - WS-VAL-START + 1)
                       TO WS-JVAL
               END-IF
               ADD 1 TO WS-FJV-POS
           ELSE
      *>       Numeric value: read until comma/brace/bracket
               MOVE WS-FJV-POS TO WS-VAL-START
               PERFORM UNTIL WS-FJV-POS > WS-JLEN
                   OR WS-JBUF(WS-FJV-POS:1) = ","
                   OR WS-JBUF(WS-FJV-POS:1) = "}"
                   OR WS-JBUF(WS-FJV-POS:1) = "]"
                   OR WS-JBUF(WS-FJV-POS:1) = " "
                   ADD 1 TO WS-FJV-POS
               END-PERFORM
               COMPUTE WS-VAL-END = WS-FJV-POS - 1
               IF WS-VAL-END >= WS-VAL-START
                   MOVE WS-JBUF(WS-VAL-START:
                       WS-VAL-END - WS-VAL-START + 1)
                       TO WS-JVAL
               END-IF
           END-IF
           MOVE WS-FJV-POS TO WS-JPOS
           .

      *> ============================================================
      *> FIND-JSON-ARRAY: Find [...] in WS-JBUF after key
      *> Sets WS-ARR-START, WS-ARR-END
      *> ============================================================
       FIND-JSON-ARRAY.
           MOVE 0 TO WS-ARR-START WS-ARR-END

      *>   From current WS-JPOS find opening [
           PERFORM VARYING WS-SCAN-POS FROM WS-JPOS BY 1
               UNTIL WS-SCAN-POS > WS-JLEN
               OR WS-ARR-START > 0
               IF WS-JBUF(WS-SCAN-POS:1) = "["
                   MOVE WS-SCAN-POS TO WS-ARR-START
               END-IF
           END-PERFORM

           IF WS-ARR-START = 0
               EXIT PARAGRAPH
           END-IF

      *>   Find matching ]
           MOVE 1 TO WS-BRACKET-DEPTH
           COMPUTE WS-SCAN-POS = WS-ARR-START + 1
           PERFORM UNTIL WS-SCAN-POS > WS-JLEN
               OR WS-BRACKET-DEPTH = 0
               IF WS-JBUF(WS-SCAN-POS:1) = "["
                   ADD 1 TO WS-BRACKET-DEPTH
               END-IF
               IF WS-JBUF(WS-SCAN-POS:1) = "]"
                   SUBTRACT 1 FROM WS-BRACKET-DEPTH
               END-IF
               IF WS-BRACKET-DEPTH = 0
                   MOVE WS-SCAN-POS TO WS-ARR-END
               END-IF
               ADD 1 TO WS-SCAN-POS
           END-PERFORM
           .

      *> ============================================================
      *> NEXT-JSON-OBJ: Get next {...} from array
      *> Input: WS-SCAN-POS (start scanning from)
      *> Output: WS-OBJ-BUF, WS-OBJ-START, WS-OBJ-END
      *> ============================================================
       NEXT-JSON-OBJ.
           MOVE SPACES TO WS-OBJ-BUF
           MOVE 0 TO WS-OBJ-START WS-OBJ-END

      *>   Find opening {
           PERFORM UNTIL WS-SCAN-POS > WS-ARR-END
               OR WS-OBJ-START > 0
               IF WS-JBUF(WS-SCAN-POS:1) = "{"
                   MOVE WS-SCAN-POS TO WS-OBJ-START
               END-IF
               ADD 1 TO WS-SCAN-POS
           END-PERFORM

           IF WS-OBJ-START = 0
               EXIT PARAGRAPH
           END-IF

      *>   Find matching }
           MOVE 1 TO WS-BRACKET-DEPTH
           PERFORM UNTIL WS-SCAN-POS > WS-ARR-END
               OR WS-BRACKET-DEPTH = 0
               IF WS-JBUF(WS-SCAN-POS:1) = "{"
                   ADD 1 TO WS-BRACKET-DEPTH
               END-IF
               IF WS-JBUF(WS-SCAN-POS:1) = "}"
                   SUBTRACT 1 FROM WS-BRACKET-DEPTH
               END-IF
               IF WS-BRACKET-DEPTH = 0
                   MOVE WS-SCAN-POS TO WS-OBJ-END
               END-IF
               ADD 1 TO WS-SCAN-POS
           END-PERFORM

           IF WS-OBJ-END > 0
               MOVE WS-JBUF(WS-OBJ-START:
                   WS-OBJ-END - WS-OBJ-START + 1)
                   TO WS-OBJ-BUF
           END-IF
           .

      *> ============================================================
      *> LOAD SUSPECTS from ../S01E01/suspects.json
      *> ============================================================
       LOAD-SUSPECTS.
           DISPLAY " "
           DISPLAY "--- Krok 1: Ladowanie podejrzanych ---"

           MOVE "../S01E01/suspects.json" TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           IF WS-JLEN = 0
               DISPLAY "  BLAD: Brak suspects.json!"
               STOP RUN
           END-IF

      *>   Find the top-level array
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-ARRAY

           MOVE 0 TO WS-SUSP-CT
           MOVE WS-ARR-START TO WS-SCAN-POS

           PERFORM UNTIL WS-SCAN-POS >= WS-ARR-END
               PERFORM NEXT-JSON-OBJ
               IF WS-OBJ-START = 0
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-SUSP-CT

      *>       Parse object fields using OBJ-BUF as mini-JSON
               MOVE WS-OBJ-BUF TO WS-TMP2
               MOVE WS-JBUF TO WS-JBUF-SAVE
               MOVE WS-JLEN TO WS-JLEN-SAVE
               MOVE WS-OBJ-BUF TO WS-JBUF
               MOVE LENGTH(TRIM(WS-OBJ-BUF)) TO WS-JLEN

               MOVE "name" TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               MOVE WS-JVAL TO WS-SU-NAME(WS-SUSP-CT)

               MOVE "surname" TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               MOVE WS-JVAL TO WS-SU-SURNAME(WS-SUSP-CT)

               MOVE "birthYear" TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               COMPUTE WS-SU-BYEAR(WS-SUSP-CT) =
                   NUMVAL(TRIM(WS-JVAL))

      *>       Restore main buffer
               MOVE WS-JBUF-SAVE TO WS-JBUF
               MOVE WS-JLEN-SAVE TO WS-JLEN
           END-PERFORM

           DISPLAY "  Zaladowano " WS-SUSP-CT " podejrzanych"
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-SUSP-CT
               DISPLAY "    " TRIM(WS-SU-NAME(WS-I))
                   " " TRIM(WS-SU-SURNAME(WS-I))
                   " (ur. " WS-SU-BYEAR(WS-I) ")"
           END-PERFORM
           .

      *> ============================================================
      *> FETCH + GEOCODE PLANTS
      *> ============================================================
       FETCH-AND-GEOCODE-PLANTS.
           DISPLAY " "
           DISPLAY "--- Krok 2: Elektrownie + geokodowanie ---"

      *>   Fetch plants JSON via curl binary GET
           INITIALIZE WS-CMD
           STRING "curl -s -o plants.json "
               WS-QT TRIM(WS-HUB-URL) "/data/"
               TRIM(WS-HUB-KEY) "/findhim_locations.json" WS-QT
               DELIMITED SIZE INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Parse plants JSON
           MOVE "plants.json" TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

      *>   Find "power_plants" key, then iterate its object keys
           MOVE 0 TO WS-PLANT-CT
           MOVE SPACES TO WS-TMP
           STRING WS-QT "power_plants" WS-QT
               DELIMITED SIZE INTO WS-TMP
           END-STRING

      *>   Locate "power_plants" in buffer
           MOVE 0 TO WS-KEY-POS
           PERFORM VARYING WS-SCAN-POS FROM 1 BY 1
               UNTIL WS-SCAN-POS > WS-JLEN
               OR WS-KEY-POS > 0
               IF WS-SCAN-POS + LENGTH(TRIM(WS-TMP)) - 1
                   <= WS-JLEN
               AND WS-JBUF(WS-SCAN-POS:
                   LENGTH(TRIM(WS-TMP))) = TRIM(WS-TMP)
                   MOVE WS-SCAN-POS TO WS-KEY-POS
               END-IF
           END-PERFORM

           IF WS-KEY-POS = 0
               DISPLAY "  BLAD: brak power_plants!"
               EXIT PARAGRAPH
           END-IF

      *>   Skip past "power_plants" : {
           COMPUTE WS-SCAN-POS =
               WS-KEY-POS + LENGTH(TRIM(WS-TMP))
           PERFORM UNTIL WS-SCAN-POS > WS-JLEN
               OR WS-JBUF(WS-SCAN-POS:1) = "{"
               ADD 1 TO WS-SCAN-POS
           END-PERFORM
           ADD 1 TO WS-SCAN-POS

      *>   Iterate city keys within the power_plants object
           PERFORM UNTIL WS-SCAN-POS > WS-JLEN
      *>       Skip whitespace and commas
               PERFORM UNTIL WS-SCAN-POS > WS-JLEN
                   OR (WS-JBUF(WS-SCAN-POS:1) NOT = " "
                   AND WS-JBUF(WS-SCAN-POS:1) NOT = ","
                   AND WS-JBUF(WS-SCAN-POS:1) NOT = X"0A"
                   AND WS-JBUF(WS-SCAN-POS:1) NOT = X"0D")
                   ADD 1 TO WS-SCAN-POS
               END-PERFORM

      *>       End of power_plants object?
               IF WS-JBUF(WS-SCAN-POS:1) = "}"
                   EXIT PERFORM
               END-IF

      *>       Expect opening quote of city key
               IF WS-JBUF(WS-SCAN-POS:1) NOT = WS-QT
                   ADD 1 TO WS-SCAN-POS
                   EXIT PERFORM
               END-IF

      *>       Extract city name between quotes
               ADD 1 TO WS-SCAN-POS
               MOVE WS-SCAN-POS TO WS-VAL-START
               PERFORM UNTIL WS-SCAN-POS > WS-JLEN
                   OR WS-JBUF(WS-SCAN-POS:1) = WS-QT
                   ADD 1 TO WS-SCAN-POS
               END-PERFORM
               COMPUTE WS-VAL-END = WS-SCAN-POS - 1
               ADD 1 TO WS-PLANT-CT

               IF WS-VAL-END >= WS-VAL-START
                   MOVE WS-JBUF(WS-VAL-START:
                       WS-VAL-END - WS-VAL-START + 1)
                       TO WS-PL-CITY(WS-PLANT-CT)
               END-IF
               ADD 1 TO WS-SCAN-POS

      *>       Skip to opening { of this city's sub-object
               PERFORM UNTIL WS-SCAN-POS > WS-JLEN
                   OR WS-JBUF(WS-SCAN-POS:1) = "{"
                   ADD 1 TO WS-SCAN-POS
               END-PERFORM
               MOVE WS-SCAN-POS TO WS-OBJ-START

      *>       Find matching } (track nesting depth)
               MOVE 1 TO WS-BRACKET-DEPTH
               ADD 1 TO WS-SCAN-POS
               PERFORM UNTIL WS-SCAN-POS > WS-JLEN
                   OR WS-BRACKET-DEPTH = 0
                   IF WS-JBUF(WS-SCAN-POS:1) = "{"
                       ADD 1 TO WS-BRACKET-DEPTH
                   END-IF
                   IF WS-JBUF(WS-SCAN-POS:1) = "}"
                       SUBTRACT 1 FROM WS-BRACKET-DEPTH
                   END-IF
                   ADD 1 TO WS-SCAN-POS
               END-PERFORM
               COMPUTE WS-OBJ-END = WS-SCAN-POS - 1

      *>       Extract "code" from this sub-object
               COMPUTE WS-K =
                   WS-OBJ-END - WS-OBJ-START + 1
               MOVE WS-JBUF(WS-OBJ-START:WS-K)
                   TO WS-OBJ-BUF

               MOVE WS-JBUF TO WS-JBUF-SAVE
               MOVE WS-JLEN TO WS-JLEN-SAVE
               MOVE WS-OBJ-BUF TO WS-JBUF
               MOVE WS-K TO WS-JLEN

               MOVE "code" TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               MOVE TRIM(WS-JVAL) TO
                   WS-PL-CODE(WS-PLANT-CT)

               MOVE WS-JBUF-SAVE TO WS-JBUF
               MOVE WS-JLEN-SAVE TO WS-JLEN
           END-PERFORM

           DISPLAY "  Znaleziono " WS-PLANT-CT " elektrowni"

      *>   Geocode each plant
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-PLANT-CT
               PERFORM GEOCODE-PLANT
               DISPLAY "    " TRIM(WS-PL-CITY(WS-I))
                   " [" TRIM(WS-PL-CODE(WS-I)) "]"
                   " lat=" WS-PL-LAT(WS-I)
                   " lon=" WS-PL-LON(WS-I)
           END-PERFORM
           .

      *> ============================================================
      *> GEOCODE-PLANT: Geocode city for plant WS-I
      *> Tries original name, then Polish-char variant
      *> ============================================================
       GEOCODE-PLANT.
           MOVE TRIM(WS-PL-CITY(WS-I)) TO WS-GEO-CITY
           MOVE "N" TO WS-GEO-OK
           MOVE +0 TO WS-PL-LAT(WS-I) WS-PL-LON(WS-I)

      *>   Try original city name (most have UTF-8 Polish chars)
           PERFORM GEOCODE-QUERY
           IF WS-GEO-OK = "Y"
               EXIT PARAGRAPH
           END-IF

      *>   Build Polish variant using SUBSTITUTE for known patterns
      *>   Chelmno -> Chełmno, Zarnowiec -> Żarnowiec, etc.
           MOVE SUBSTITUTE(TRIM(WS-PL-CITY(WS-I))
               "Chelmno" "Chełmno"
               "Zarnowiec" "Żarnowiec"
               "Grudziadz" "Grudziądz"
               "Piotrkow" "Piotrków"
               ) TO WS-GEO-CITY
           IF TRIM(WS-GEO-CITY) NOT =
               TRIM(WS-PL-CITY(WS-I))
               PERFORM GEOCODE-QUERY
           END-IF
           .

       GEOCODE-QUERY.
      *>   URL-encode Polish UTF-8 chars + space for geocoding
           MOVE SUBSTITUTE(TRIM(WS-GEO-CITY)
               " " "+"
               "ą" "%C4%85"
               "ć" "%C4%87"
               "ę" "%C4%99"
               "ł" "%C5%82"
               "ń" "%C5%84"
               "ó" "%C3%B3"
               "ś" "%C5%9B"
               "ź" "%C5%BA"
               "ż" "%C5%BC"
               "Ż" "%C5%BB"
               "Ą" "%C4%84"
               "Ł" "%C5%81"
               ) TO WS-TMP

      *>   GET via curl binary
           INITIALIZE WS-CMD
           STRING "curl -s -o geo.json "
               WS-QT "https://geocoding-api.open-meteo.com"
               "/v1/search?name="
               TRIM(WS-TMP)
               "&count=1" WS-QT
               DELIMITED SIZE INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Parse response
           MOVE "geo.json" TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           IF WS-JLEN = 0
               EXIT PARAGRAPH
           END-IF

      *>   Find "results" array (use KEY-POS so we don't skip [)
           MOVE "results" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE WS-KEY-POS TO WS-JPOS
           PERFORM FIND-JSON-ARRAY

           IF WS-ARR-START = 0
               EXIT PARAGRAPH
           END-IF

      *>   Iterate results, find first with country_code=PL
           MOVE WS-ARR-START TO WS-SCAN-POS
           PERFORM UNTIL WS-SCAN-POS >= WS-ARR-END
               PERFORM NEXT-JSON-OBJ
               IF WS-OBJ-START = 0
                   EXIT PERFORM
               END-IF

      *>       Check country_code
               MOVE WS-JBUF TO WS-JBUF-SAVE
               MOVE WS-JLEN TO WS-JLEN-SAVE
               MOVE WS-OBJ-BUF TO WS-JBUF
               MOVE LENGTH(TRIM(WS-OBJ-BUF)) TO WS-JLEN

               MOVE "country_code" TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL

               IF TRIM(WS-JVAL) = "PL"
      *>           Extract lat/lon
                   MOVE "latitude" TO WS-KEY-SEARCH
                   MOVE 1 TO WS-JPOS
                   PERFORM FIND-JSON-VAL
                   COMPUTE WS-PL-LAT(WS-I) =
                       NUMVAL(TRIM(WS-JVAL))

                   MOVE "longitude" TO WS-KEY-SEARCH
                   MOVE 1 TO WS-JPOS
                   PERFORM FIND-JSON-VAL
                   COMPUTE WS-PL-LON(WS-I) =
                       NUMVAL(TRIM(WS-JVAL))

                   MOVE "Y" TO WS-GEO-OK
               END-IF

               MOVE WS-JBUF-SAVE TO WS-JBUF
               MOVE WS-JLEN-SAVE TO WS-JLEN

               IF WS-GEO-OK = "Y"
                   EXIT PERFORM
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> FIND SUSPECT NEAR PLANT
      *> ============================================================
       FIND-SUSPECT-NEAR-PLANT.
           DISPLAY " "
           DISPLAY "--- Krok 3: Szukanie podejrzanego ---"

           MOVE "N" TO WS-B-FOUND
           MOVE 99999 TO WS-B-DIST

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-SUSP-CT
               PERFORM CHECK-ONE-SUSPECT
           END-PERFORM

           IF WS-B-FOUND = "Y"
               DISPLAY "  >>> " TRIM(WS-B-NAME) " "
                   TRIM(WS-B-SURNAME) " przy "
                   TRIM(WS-B-PCITY) " ("
                   TRIM(WS-B-PCODE) ") "
                   WS-B-DIST "km"
           ELSE
               DISPLAY "  NIE ZNALEZIONO!"
               STOP RUN
           END-IF
           .

       CHECK-ONE-SUSPECT.
           DISPLAY "  " TRIM(WS-SU-NAME(WS-I)) " "
               TRIM(WS-SU-SURNAME(WS-I))

      *>   Build location request JSON in memory
           INITIALIZE WS-TMP
           STRING "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "name" WS-QT ":"
               WS-QT TRIM(WS-SU-NAME(WS-I)) WS-QT ","
               WS-QT "surname" WS-QT ":"
               WS-QT TRIM(WS-SU-SURNAME(WS-I)) WS-QT "}"
               DELIMITED SIZE INTO WS-TMP
           END-STRING

      *>   Write request body to file and POST via curl binary
           PERFORM WRITE-REQ-BODY
           INITIALIZE WS-CMD
           STRING "curl -s -o locs.json -X POST "
               TRIM(WS-LOCATION-URL)
               " -H " WS-QT "Content-Type: application/json"
               WS-QT
               " -d @request_body.tmp"
               DELIMITED SIZE INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Parse locations response (array of objects)
           MOVE "locs.json" TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           IF WS-JLEN = 0
               DISPLAY "    brak lokalizacji"
               EXIT PARAGRAPH
           END-IF

      *>   Find the array (might be top-level)
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-ARRAY

           IF WS-ARR-START = 0
               DISPLAY "    brak tablicy lokalizacji"
               EXIT PARAGRAPH
           END-IF

      *>   Iterate location objects
           MOVE 0 TO WS-K
           MOVE WS-ARR-START TO WS-SCAN-POS
           PERFORM UNTIL WS-SCAN-POS >= WS-ARR-END
               PERFORM NEXT-JSON-OBJ
               IF WS-OBJ-START = 0
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-K

      *>       Extract lat/lon from this object
               MOVE WS-JBUF TO WS-JBUF-SAVE
               MOVE WS-JLEN TO WS-JLEN-SAVE
               MOVE WS-OBJ-BUF TO WS-JBUF
               MOVE LENGTH(TRIM(WS-OBJ-BUF)) TO WS-JLEN

               MOVE "latitude" TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               COMPUTE WS-H-LAT1 = NUMVAL(TRIM(WS-JVAL))

               MOVE "longitude" TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               COMPUTE WS-H-LON1 = NUMVAL(TRIM(WS-JVAL))

               MOVE WS-JBUF-SAVE TO WS-JBUF
               MOVE WS-JLEN-SAVE TO WS-JLEN

      *>       Check distance to each plant
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > WS-PLANT-CT
                   MOVE WS-PL-LAT(WS-J) TO WS-H-LAT2
                   MOVE WS-PL-LON(WS-J) TO WS-H-LON2
                   PERFORM HAVERSINE
                   IF WS-H-DIST < 20
                   AND WS-H-DIST < WS-B-DIST
                       MOVE WS-H-DIST TO WS-B-DIST
                       MOVE WS-SU-NAME(WS-I) TO WS-B-NAME
                       MOVE WS-SU-SURNAME(WS-I) TO
                           WS-B-SURNAME
                       MOVE WS-SU-BYEAR(WS-I) TO WS-B-BYEAR
                       MOVE WS-PL-CODE(WS-J) TO WS-B-PCODE
                       MOVE WS-PL-CITY(WS-J) TO WS-B-PCITY
                       MOVE "Y" TO WS-B-FOUND
                       DISPLAY "    MATCH! "
                           TRIM(WS-PL-CITY(WS-J))
                           " " WS-H-DIST "km"
                   END-IF
               END-PERFORM
           END-PERFORM

           DISPLAY "    " WS-K " lokalizacji"
           .

      *> ============================================================
      *> HAVERSINE
      *> ============================================================
       HAVERSINE.
           COMPUTE WS-H-DLAT =
               (WS-H-LAT2 - WS-H-LAT1) * WS-RADF
           COMPUTE WS-H-DLON =
               (WS-H-LON2 - WS-H-LON1) * WS-RADF

           COMPUTE WS-H-SIN1 = SIN(WS-H-DLAT / 2)
           COMPUTE WS-H-SIN1 = WS-H-SIN1 * WS-H-SIN1

           COMPUTE WS-H-SIN2 = SIN(WS-H-DLON / 2)
           COMPUTE WS-H-SIN2 = WS-H-SIN2 * WS-H-SIN2

           COMPUTE WS-H-COS1 =
               COS(WS-H-LAT1 * WS-RADF)
           COMPUTE WS-H-COS2 =
               COS(WS-H-LAT2 * WS-RADF)

           COMPUTE WS-H-A = WS-H-SIN1 +
               WS-H-COS1 * WS-H-COS2 * WS-H-SIN2

           COMPUTE WS-H-DIST =
               6371 * 2 * ASIN(SQRT(WS-H-A))
           .

      *> ============================================================
      *> GET ACCESS LEVEL
      *> ============================================================
       GET-ACCESS-LEVEL.
           DISPLAY " "
           DISPLAY "--- Krok 4: Poziom dostepu ---"

      *>   Build request JSON in memory
           INITIALIZE WS-TMP
           STRING "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "name" WS-QT ":"
               WS-QT TRIM(WS-B-NAME) WS-QT ","
               WS-QT "surname" WS-QT ":"
               WS-QT TRIM(WS-B-SURNAME) WS-QT ","
               WS-QT "birthYear" WS-QT ":"
               WS-B-BYEAR "}"
               DELIMITED SIZE INTO WS-TMP
           END-STRING

      *>   Write request body to file and POST via curl binary
           PERFORM WRITE-REQ-BODY
           INITIALIZE WS-CMD
           STRING "curl -s -o access.json -X POST "
               TRIM(WS-ACCESS-URL)
               " -H " WS-QT "Content-Type: application/json"
               WS-QT
               " -d @request_body.tmp"
               DELIMITED SIZE INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Parse response
           MOVE "access.json" TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           MOVE "accessLevel" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           COMPUTE WS-B-ACCESS = NUMVAL(TRIM(WS-JVAL))
           DISPLAY "  " TRIM(WS-B-NAME) " "
               TRIM(WS-B-SURNAME)
               " -> accessLevel: " WS-B-ACCESS
           .

      *> ============================================================
      *> SUBMIT ANSWER
      *> ============================================================
       SUBMIT-ANSWER.
           DISPLAY " "
           DISPLAY "--- Krok 5: Wysylanie odpowiedzi ---"

      *>   Format access level without leading zeros
           MOVE WS-B-ACCESS TO WS-NUM-STR
           INSPECT WS-NUM-STR REPLACING LEADING "0" BY " "

           INITIALIZE WS-TMP
           STRING "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT "findhim" WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "name" WS-QT ":"
               WS-QT TRIM(WS-B-NAME) WS-QT ","
               WS-QT "surname" WS-QT ":"
               WS-QT TRIM(WS-B-SURNAME) WS-QT ","
               WS-QT "accessLevel" WS-QT ":"
               TRIM(WS-NUM-STR) ","
               WS-QT "powerPlant" WS-QT ":"
               WS-QT TRIM(WS-B-PCODE) WS-QT
               "}}"
               DELIMITED SIZE INTO WS-TMP
           END-STRING

           DISPLAY "  Payload: " TRIM(WS-TMP)

      *>   Write request body to file and POST via curl binary
           PERFORM WRITE-REQ-BODY
           INITIALIZE WS-CMD
           STRING "curl -s -o submit.json -X POST "
               TRIM(WS-VERIFY-URL)
               " -H " WS-QT "Content-Type: application/json"
               WS-QT
               " -d @request_body.tmp"
               DELIMITED SIZE INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           DISPLAY "  Wysylam..."

      *>   Read response
           MOVE "submit.json" TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH
           DISPLAY "  Odpowiedz: " TRIM(WS-JBUF)
           .
