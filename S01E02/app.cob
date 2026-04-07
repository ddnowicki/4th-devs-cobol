       IDENTIFICATION DIVISION.
       PROGRAM-ID. S01E02-FINDHIM.
      *> ============================================================
      *> S01E02 - Stable AI Agent with 5 Tools
      *> Deterministic matching: COBOL does all haversine work.
      *> LLM only drives flow via function calling (5 roundtrips).
      *> Tools: get_suspects, get_power_plants,
      *>   get_closest_suspect_to_plant, get_access_level,
      *>   submit_answer
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
       01  WORK-REC                PIC X(64000).

       FD  REQ-BODY-FILE.
       01  REQ-BODY-REC            PIC X(4000).

       WORKING-STORAGE SECTION.
      *> === Environment (via copybook) ===
       COPY ENVLOAD-WS.

      *> === Constants ===
       01  WS-MAX-RETRIES          PIC 9(2) VALUE 10.
       01  WS-RETRY-DELAY          PIC 9(2) VALUE 15.
       01  WS-SUB-RETRIES          PIC 9(1) VALUE 5.
       01  WS-SUB-RETRY-DELAY      PIC 9(2) VALUE 10.

      *> === Shared WS (via copybooks) ===
       COPY JSONPARSE-WS.
       COPY JSONREAD-WS.
       COPY JSONESCAPE-WS.
       COPY TOOLPARSE-WS.

      *> === File I/O ===
       01  WS-FS                   PIC XX.
       01  WS-WORK-PATH            PIC X(100)
                                   VALUE "work.tmp".
       01  WS-SUSPECTS-PATH        PIC X(200).

      *> === HTTP ===
       01  WS-CMD                  PIC X(4000).
       01  WS-LOCATION-URL         PIC X(200).
       01  WS-ACCESS-URL           PIC X(200).

      *> === JSON Parsing (task-specific) ===
       01  WS-SCAN-POS             PIC 9(5).
       01  WS-BRACKET-DEPTH        PIC 9(2).
       01  WS-ARR-START            PIC 9(5).
       01  WS-ARR-END              PIC 9(5).
       01  WS-OBJ-START            PIC 9(5).
       01  WS-OBJ-END              PIC 9(5).
       01  WS-OBJ-BUF              PIC X(2000).

      *> === Task Data - Suspects ===
       01  WS-SUSP-CT              PIC 9(2) VALUE 0.
       01  WS-SUSPS.
           05 WS-SUSP OCCURS 20 TIMES.
              10 WS-SU-NAME        PIC X(50).
              10 WS-SU-SURNAME     PIC X(50).
              10 WS-SU-BYEAR       PIC 9(4).

      *> === Task Data - Plants ===
       01  WS-PLANT-CT             PIC 9(2) VALUE 0.
       01  WS-PLANTS.
           05 WS-PL OCCURS 20 TIMES.
              10 WS-PL-CITY        PIC X(50).
              10 WS-PL-CODE        PIC X(20).
              10 WS-PL-LAT         PIC S9(3)V9(6).
              10 WS-PL-LON         PIC S9(3)V9(6).

      *> === Haversine ===
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

      *> === Geocoding ===
       01  WS-GEOCODE-URL          PIC X(200).
       01  WS-GEO-CITY             PIC X(100).
       01  WS-GEO-OK               PIC X VALUE "N".

      *> === Task Data - Closest Match ===
       01  WS-BEST-NAME            PIC X(50).
       01  WS-BEST-SURNAME         PIC X(50).
       01  WS-BEST-BYEAR           PIC 9(4).
       01  WS-BEST-DIST            PIC S9(5)V9(2)
                                   VALUE +99999.
       01  WS-BEST-CITY            PIC X(50).
       01  WS-BEST-CODE            PIC X(20).
       01  WS-BEST-FOUND           PIC X VALUE "N".
       01  WS-SU-BEST-DIST         PIC S9(5)V9(2).
       01  WS-SU-BEST-CITY         PIC X(50).
       01  WS-SU-BEST-CODE         PIC X(20).
       01  WS-SU-HAS-MATCH         PIC X.

      *> === Task Data - Location Parsing ===
       01  WS-LOC-LAT              PIC S9(3)V9(6).
       01  WS-LOC-LON              PIC S9(3)V9(6).
       01  WS-LOC-JBUF             PIC X(8000).
       01  WS-LOC-JLEN             PIC 9(5).
       01  WS-LOC-SCAN             PIC 9(5).
       01  WS-LOC-ARR-START        PIC 9(5).
       01  WS-LOC-ARR-END          PIC 9(5).
       01  WS-LOC-OBJ-START        PIC 9(5).
       01  WS-LOC-OBJ-END          PIC 9(5).
       01  WS-LOC-DEPTH            PIC 9(2).
       01  WS-LOC-OBJ-BUF          PIC X(500).

      *> === Agent Loop ===
       01  WS-REQ-JSON             PIC X(64000).
       01  WS-CONV-BUF             PIC X(64000).
       01  WS-CONV-PTR             PIC 9(5).
       01  WS-AG-STEP              PIC 9(2) VALUE 00.
       01  WS-AG-DONE              PIC X VALUE "N".
       01  WS-AG-CONTENT           PIC X(2000).
       01  WS-NUDGE-CT             PIC 9(1) VALUE 0.

      *> === Tool Call (task-specific) ===
       01  WS-TOOL-RESULT          PIC X(8000).
       01  WS-TOOL-RESULT-LEN      PIC 9(5).
       01  WS-TA-NAME              PIC X(50).
       01  WS-TA-SURNAME           PIC X(50).
       01  WS-TA-BYEAR             PIC 9(4).
       01  WS-TA-ANSWER            PIC X(2000).

      *> === Control Flow ===
       01  WS-PTR                  PIC 9(5).
       01  WS-I                    PIC 9(2).
       01  WS-J                    PIC 9(2).
       01  WS-DISP-LAT             PIC -(3)9.9(6).
       01  WS-DISP-LON             PIC -(3)9.9(6).
       01  WS-DISP-DIST            PIC -(5)9.9(2).
       01  WS-NUM-STR              PIC X(10).
       01  WS-TALLY-CNT            PIC 9(4).
       01  WS-RETRY-CT             PIC 9(2) VALUE 0.
       01  WS-SUMMARY-BUF          PIC X(4000).
       01  WS-SUMMARY-PTR          PIC 9(5).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S01E02 FINDHIM - Stable 5-Tool ==="

           PERFORM LOAD-ENV-VARS

           ACCEPT WS-GEOCODE-URL
               FROM ENVIRONMENT "GEOCODING_API_URL"

      *>   Construct URLs
           INITIALIZE WS-LOCATION-URL
           STRING TRIM(WS-HUB-URL) "/api/location"
               DELIMITED SIZE INTO WS-LOCATION-URL
           END-STRING

           INITIALIZE WS-ACCESS-URL
           STRING TRIM(WS-HUB-URL) "/api/accesslevel"
               DELIMITED SIZE INTO WS-ACCESS-URL
           END-STRING

           COMPUTE WS-RADF = WS-PI-V / 180

      *>   Suspects path
           ACCEPT WS-SUSPECTS-PATH
               FROM ENVIRONMENT "SUSPECTS_PATH"
           IF WS-SUSPECTS-PATH = SPACES
               MOVE "../S01E01/suspects.json"
                   TO WS-SUSPECTS-PATH
           END-IF

      *>   Pre-load suspects and plants
           PERFORM LOAD-SUSPECTS
           PERFORM LOAD-PLANTS

      *>   Run the agent
           PERFORM RUN-AGENT

           DISPLAY " "
           DISPLAY "=== ZAKONCZONO ==="
           STOP RUN.

      *> ============================================================
      *> WRITE-REQ-BODY: Write WS-TMP to request_body.tmp
      *> ============================================================
       WRITE-REQ-BODY.
           OPEN OUTPUT REQ-BODY-FILE
           IF WS-FS NOT = "00"
               DISPLAY "ERR: OPEN request_body.tmp"
                   " FS=" WS-FS
               STOP RUN
           END-IF
           MOVE WS-TMP TO REQ-BODY-REC
           WRITE REQ-BODY-REC
           CLOSE REQ-BODY-FILE
           .

      *> ============================================================
      *> FIND-JSON-ARRAY: Find [...] after key
      *> ============================================================
       FIND-JSON-ARRAY.
           MOVE 0 TO WS-ARR-START WS-ARR-END

           PERFORM VARYING WS-SCAN-POS
               FROM WS-JPOS BY 1
               UNTIL WS-SCAN-POS > WS-JLEN
               OR WS-ARR-START > 0
               IF WS-JBUF(WS-SCAN-POS:1) = "["
                   MOVE WS-SCAN-POS
                       TO WS-ARR-START
               END-IF
           END-PERFORM

           IF WS-ARR-START = 0
               EXIT PARAGRAPH
           END-IF

           MOVE 1 TO WS-BRACKET-DEPTH
           COMPUTE WS-SCAN-POS =
               WS-ARR-START + 1
           PERFORM UNTIL WS-SCAN-POS > WS-JLEN
               OR WS-BRACKET-DEPTH = 0
               IF WS-JBUF(WS-SCAN-POS:1) = "["
                   ADD 1 TO WS-BRACKET-DEPTH
               END-IF
               IF WS-JBUF(WS-SCAN-POS:1) = "]"
                   SUBTRACT 1
                       FROM WS-BRACKET-DEPTH
               END-IF
               IF WS-BRACKET-DEPTH = 0
                   MOVE WS-SCAN-POS
                       TO WS-ARR-END
               END-IF
               ADD 1 TO WS-SCAN-POS
           END-PERFORM
           .

      *> ============================================================
      *> NEXT-JSON-OBJ: Get next {...} from array
      *> ============================================================
       NEXT-JSON-OBJ.
           MOVE SPACES TO WS-OBJ-BUF
           MOVE 0 TO WS-OBJ-START WS-OBJ-END

           PERFORM UNTIL WS-SCAN-POS > WS-ARR-END
               OR WS-OBJ-START > 0
               IF WS-JBUF(WS-SCAN-POS:1) = "{"
                   MOVE WS-SCAN-POS
                       TO WS-OBJ-START
               END-IF
               ADD 1 TO WS-SCAN-POS
           END-PERFORM

           IF WS-OBJ-START = 0
               EXIT PARAGRAPH
           END-IF

           MOVE 1 TO WS-BRACKET-DEPTH
           PERFORM UNTIL WS-SCAN-POS > WS-ARR-END
               OR WS-BRACKET-DEPTH = 0
               IF WS-JBUF(WS-SCAN-POS:1) = "{"
                   ADD 1 TO WS-BRACKET-DEPTH
               END-IF
               IF WS-JBUF(WS-SCAN-POS:1) = "}"
                   SUBTRACT 1
                       FROM WS-BRACKET-DEPTH
               END-IF
               IF WS-BRACKET-DEPTH = 0
                   MOVE WS-SCAN-POS
                       TO WS-OBJ-END
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
      *> LOAD-SUSPECTS
      *> ============================================================
       LOAD-SUSPECTS.
           DISPLAY " "
           DISPLAY "--- Loading suspects ---"
           DISPLAY "  Path: "
               TRIM(WS-SUSPECTS-PATH)

           MOVE WS-SUSPECTS-PATH TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           IF WS-JLEN = 0
               DISPLAY "  ERROR: No suspects!"
               STOP RUN
           END-IF

           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-ARRAY

           MOVE 0 TO WS-SUSP-CT
           MOVE WS-ARR-START TO WS-SCAN-POS

           PERFORM UNTIL WS-SCAN-POS
               >= WS-ARR-END
               PERFORM NEXT-JSON-OBJ
               IF WS-OBJ-START = 0
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-SUSP-CT

               MOVE WS-JBUF TO WS-JBUF-SAVE
               MOVE WS-JLEN TO WS-JLEN-SAVE
               MOVE WS-OBJ-BUF TO WS-JBUF
               MOVE LENGTH(TRIM(WS-OBJ-BUF))
                   TO WS-JLEN

               MOVE "name" TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               MOVE WS-JVAL
                   TO WS-SU-NAME(WS-SUSP-CT)

               MOVE "surname" TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               MOVE WS-JVAL
                 TO WS-SU-SURNAME(WS-SUSP-CT)

               MOVE "birthYear" TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               COMPUTE WS-SU-BYEAR(WS-SUSP-CT)
                   = NUMVAL(TRIM(WS-JVAL))

               MOVE WS-JBUF-SAVE TO WS-JBUF
               MOVE WS-JLEN-SAVE TO WS-JLEN
           END-PERFORM

           DISPLAY "  Loaded " WS-SUSP-CT
               " suspects"
           .

      *> ============================================================
      *> LOAD-PLANTS: Fetch plants JSON + geocode each
      *> ============================================================
       LOAD-PLANTS.
           DISPLAY " "
           DISPLAY "--- Loading power plants ---"

      *>   Fetch via curl with retry
           MOVE 0 TO WS-RETRY-CT
           PERFORM UNTIL WS-RETRY-CT >= WS-MAX-RETRIES
               INITIALIZE WS-CMD
               STRING "curl -s -o plants.json "
                   "--retry 3 --retry-delay 5 "
                   WS-QT TRIM(WS-HUB-URL)
                   "/data/"
                   TRIM(WS-HUB-KEY)
                   "/findhim_locations.json"
                   WS-QT
                   DELIMITED SIZE INTO WS-CMD
               END-STRING
               CALL "SYSTEM" USING WS-CMD

               MOVE "plants.json"
                   TO WS-WORK-PATH
               PERFORM READ-JSON-FILE
               MOVE "work.tmp" TO WS-WORK-PATH

               MOVE 0 TO WS-TALLY-CNT
               IF WS-JLEN > 10
                   INSPECT WS-JBUF(1:WS-JLEN)
                       TALLYING WS-TALLY-CNT
                       FOR ALL "power_plants"
               END-IF
               IF WS-TALLY-CNT > 0
                   EXIT PERFORM
               END-IF

               ADD 1 TO WS-RETRY-CT
               DISPLAY "  Retry " WS-RETRY-CT
                   " (rate limit?)..."
               CALL "C$SLEEP" USING WS-RETRY-DELAY
           END-PERFORM

      *>   Parse power_plants object keys
           MOVE 0 TO WS-PLANT-CT
           MOVE SPACES TO WS-TMP2
           STRING WS-QT "power_plants" WS-QT
               DELIMITED SIZE INTO WS-TMP2
           END-STRING

           MOVE 0 TO WS-KEY-POS
           PERFORM VARYING WS-SCAN-POS
               FROM 1 BY 1
               UNTIL WS-SCAN-POS > WS-JLEN
               OR WS-KEY-POS > 0
               IF WS-SCAN-POS
                   + LENGTH(TRIM(WS-TMP2)) - 1
                   <= WS-JLEN
               AND WS-JBUF(WS-SCAN-POS:
                   LENGTH(TRIM(WS-TMP2)))
                   = TRIM(WS-TMP2)
                   MOVE WS-SCAN-POS
                       TO WS-KEY-POS
               END-IF
           END-PERFORM

           IF WS-KEY-POS = 0
               DISPLAY "  ERROR: no power_plants!"
               EXIT PARAGRAPH
           END-IF

      *>   Skip past "power_plants" : {
           COMPUTE WS-SCAN-POS =
               WS-KEY-POS
               + LENGTH(TRIM(WS-TMP2))
           PERFORM UNTIL WS-SCAN-POS > WS-JLEN
               OR WS-JBUF(WS-SCAN-POS:1) = "{"
               ADD 1 TO WS-SCAN-POS
           END-PERFORM
           ADD 1 TO WS-SCAN-POS

      *>   Iterate city keys
           PERFORM UNTIL WS-SCAN-POS > WS-JLEN
      *>       Skip whitespace/commas
               PERFORM UNTIL WS-SCAN-POS > WS-JLEN
                   OR (WS-JBUF(WS-SCAN-POS:1)
                       NOT = " "
                   AND WS-JBUF(WS-SCAN-POS:1)
                       NOT = ","
                   AND WS-JBUF(WS-SCAN-POS:1)
                       NOT = X"0A"
                   AND WS-JBUF(WS-SCAN-POS:1)
                       NOT = X"0D")
                   ADD 1 TO WS-SCAN-POS
               END-PERFORM

               IF WS-JBUF(WS-SCAN-POS:1) = "}"
                   EXIT PERFORM
               END-IF

               IF WS-JBUF(WS-SCAN-POS:1)
                   NOT = WS-QT
                   ADD 1 TO WS-SCAN-POS
                   EXIT PERFORM
               END-IF

      *>       Extract city name
               ADD 1 TO WS-SCAN-POS
               MOVE WS-SCAN-POS TO WS-VAL-START
               PERFORM UNTIL WS-SCAN-POS > WS-JLEN
                   OR WS-JBUF(WS-SCAN-POS:1)
                       = WS-QT
                   ADD 1 TO WS-SCAN-POS
               END-PERFORM
               COMPUTE WS-VAL-END =
                   WS-SCAN-POS - 1
               ADD 1 TO WS-PLANT-CT

               IF WS-VAL-END >= WS-VAL-START
                   MOVE WS-JBUF(WS-VAL-START:
                       WS-VAL-END - WS-VAL-START
                       + 1)
                       TO WS-PL-CITY(WS-PLANT-CT)
               END-IF
               ADD 1 TO WS-SCAN-POS

      *>       Skip to { of sub-object
               PERFORM UNTIL WS-SCAN-POS
                   > WS-JLEN
                   OR WS-JBUF(WS-SCAN-POS:1)
                       = "{"
                   ADD 1 TO WS-SCAN-POS
               END-PERFORM
               MOVE WS-SCAN-POS TO WS-OBJ-START

      *>       Find matching }
               MOVE 1 TO WS-BRACKET-DEPTH
               ADD 1 TO WS-SCAN-POS
               PERFORM UNTIL WS-SCAN-POS
                   > WS-JLEN
                   OR WS-BRACKET-DEPTH = 0
                   IF WS-JBUF(WS-SCAN-POS:1) = "{"
                       ADD 1
                           TO WS-BRACKET-DEPTH
                   END-IF
                   IF WS-JBUF(WS-SCAN-POS:1) = "}"
                       SUBTRACT 1
                           FROM WS-BRACKET-DEPTH
                   END-IF
                   ADD 1 TO WS-SCAN-POS
               END-PERFORM
               COMPUTE WS-OBJ-END =
                   WS-SCAN-POS - 1

      *>       Extract "code"
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

           DISPLAY "  Loaded " WS-PLANT-CT
               " plants"

      *>   Geocode each plant city at load time
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-PLANT-CT
               DISPLAY "  Geocoding "
                   TRIM(WS-PL-CITY(WS-I)) "..."
               MOVE TRIM(WS-PL-CITY(WS-I))
                   TO WS-GEO-CITY
               MOVE "N" TO WS-GEO-OK
               MOVE +0 TO WS-H-LAT1 WS-H-LON1

               PERFORM GEOCODE-QUERY
               IF WS-GEO-OK NOT = "Y"
      *>           Try Polish variant
                   MOVE SUBSTITUTE(
                       TRIM(WS-PL-CITY(WS-I))
                       "Chelmno" "Chełmno"
                       "Zarnowiec" "Żarnowiec"
                       "Grudziadz" "Grudziądz"
                       "Piotrkow" "Piotrków"
                       ) TO WS-GEO-CITY
                   IF TRIM(WS-GEO-CITY) NOT =
                       TRIM(WS-PL-CITY(WS-I))
                       PERFORM GEOCODE-QUERY
                   END-IF
               END-IF

               MOVE WS-H-LAT1
                   TO WS-PL-LAT(WS-I)
               MOVE WS-H-LON1
                   TO WS-PL-LON(WS-I)
               MOVE WS-H-LAT1 TO WS-DISP-LAT
               MOVE WS-H-LON1 TO WS-DISP-LON
               DISPLAY "    -> "
                   TRIM(WS-DISP-LAT) ", "
                   TRIM(WS-DISP-LON)
           END-PERFORM
           .

      *> ============================================================
      *> GEOCODE-QUERY: Geocode WS-GEO-CITY -> H-LAT1/LON1
      *> ============================================================
       GEOCODE-QUERY.
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
               "Ó" "%C3%93"
               "ą" "%C4%85"
               "Ś" "%C5%9A"
               ) TO WS-TMP2

           INITIALIZE WS-CMD
           STRING "curl -s -o geo.json "
               WS-QT
               TRIM(WS-GEOCODE-URL)
               "?name="
               TRIM(WS-TMP2)
               "&count=10" WS-QT
               DELIMITED SIZE INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           MOVE "geo.json" TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           IF WS-JLEN = 0
               EXIT PARAGRAPH
           END-IF

      *>   Find "results" array
           MOVE "results" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE WS-KEY-POS TO WS-JPOS
           PERFORM FIND-JSON-ARRAY

           IF WS-ARR-START = 0
               EXIT PARAGRAPH
           END-IF

      *>   Find first PL result
           MOVE WS-ARR-START TO WS-SCAN-POS
           PERFORM UNTIL WS-SCAN-POS
               >= WS-ARR-END
               PERFORM NEXT-JSON-OBJ
               IF WS-OBJ-START = 0
                   EXIT PERFORM
               END-IF

               MOVE WS-JBUF TO WS-JBUF-SAVE
               MOVE WS-JLEN TO WS-JLEN-SAVE
               MOVE WS-OBJ-BUF TO WS-JBUF
               MOVE LENGTH(TRIM(WS-OBJ-BUF))
                   TO WS-JLEN

               MOVE "country_code"
                   TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL

               IF TRIM(WS-JVAL) = "PL"
                   MOVE "latitude"
                       TO WS-KEY-SEARCH
                   MOVE 1 TO WS-JPOS
                   PERFORM FIND-JSON-VAL
                   COMPUTE WS-H-LAT1 =
                       NUMVAL(TRIM(WS-JVAL))

                   MOVE "longitude"
                       TO WS-KEY-SEARCH
                   MOVE 1 TO WS-JPOS
                   PERFORM FIND-JSON-VAL
                   COMPUTE WS-H-LON1 =
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
      *> RUN-AGENT: 5-tool agent loop
      *> ============================================================
       RUN-AGENT.
           DISPLAY " "
           DISPLAY "--- AI Agent starting ---"

           MOVE SPACES TO WS-CONV-BUF
           MOVE 1 TO WS-CONV-PTR

           STRING
               "[{" WS-QT "role" WS-QT ":"
               WS-QT "system" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               "You investigate which suspect "
               "was seen near a nuclear power "
               "plant in Poland." WS-NL WS-NL
               "Call tools in this order:"
               WS-NL
               "1. get_suspects" WS-NL
               "2. get_power_plants" WS-NL
               "3. get_closest_suspect_to_plant"
               " (computes all distances, "
               "returns the CLOSEST match)"
               WS-NL
               "4. get_access_level for the "
               "CLOSEST person" WS-NL
               "5. submit_answer with name, "
               "surname, access_level, "
               "power_plant" WS-NL WS-NL
               "Always call the next tool. "
               "Never explain in text."
               WS-QT "},"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "{" WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT "Start the investigation."
               WS-QT "}]"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           MOVE "N" TO WS-AG-DONE
           MOVE 0 TO WS-AG-STEP

           PERFORM UNTIL WS-AG-DONE = "Y"
               OR WS-AG-STEP >= 20

               ADD 1 TO WS-AG-STEP
               DISPLAY " "
               DISPLAY "  --- Step " WS-AG-STEP
                   " ---"

               PERFORM SEND-AGENT-REQUEST

               MOVE "agent_resp.json"
                   TO WS-WORK-PATH
               PERFORM READ-JSON-FILE
               MOVE "work.tmp" TO WS-WORK-PATH

               IF WS-JLEN = 0
                   DISPLAY "  ERROR: Empty resp!"
                   MOVE "Y" TO WS-AG-DONE
                   EXIT PERFORM
               END-IF

      *>       Check API error
               MOVE 0 TO WS-TALLY-CNT
               INSPECT WS-JBUF(1:WS-JLEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL '"error"'
               IF WS-TALLY-CNT > 0
                   DISPLAY "  API ERROR: "
                       WS-JBUF(1:500)
                   MOVE "Y" TO WS-AG-DONE
                   EXIT PERFORM
               END-IF

      *>       Check for tool_calls
               MOVE 0 TO WS-TALLY-CNT
               INSPECT WS-JBUF(1:WS-JLEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL '"tool_calls"'

               IF WS-TALLY-CNT > 0
                   PERFORM PARSE-TOOL-CALL
                   MOVE 0 TO WS-NUDGE-CT

                   DISPLAY "  Tool: "
                       TRIM(WS-TOOL-NAME)
                   DISPLAY "  Args: "
                       TRIM(WS-TOOL-ARGS)

                   PERFORM DISPATCH-TOOL
                   PERFORM APPEND-TOOL-EXCHANGE
               ELSE
      *>           Text response
                   MOVE "content"
                       TO WS-KEY-SEARCH
                   MOVE 1 TO WS-JPOS
                   PERFORM FIND-JSON-VAL
                   DISPLAY "  Agent: "
                       TRIM(WS-JVAL)

      *>           Final?
                   MOVE 0 TO WS-TALLY-CNT
                   INSPECT TRIM(WS-JVAL)
                       TALLYING WS-TALLY-CNT
                       FOR ALL "ubmitted"
                   IF WS-TALLY-CNT = 0
                       INSPECT TRIM(WS-JVAL)
                           TALLYING WS-TALLY-CNT
                           FOR ALL "omplete"
                   END-IF
                   IF WS-TALLY-CNT = 0
                       INSPECT TRIM(WS-JVAL)
                           TALLYING WS-TALLY-CNT
                           FOR ALL "FLG:"
                   END-IF
                   IF WS-TALLY-CNT > 0
                       MOVE "Y" TO WS-AG-DONE
                   ELSE
                       ADD 1 TO WS-NUDGE-CT
                       IF WS-NUDGE-CT > 3
                           DISPLAY
                               "  Agent stuck"
                           MOVE "Y"
                               TO WS-AG-DONE
                       ELSE
                           PERFORM
                             APPEND-TEXT-AND-NUDGE
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> SEND-AGENT-REQUEST: Build request with 5 tools
      *> ============================================================
       SEND-AGENT-REQUEST.
           MOVE SPACES TO WS-REQ-JSON
           MOVE 1 TO WS-PTR

           STRING
               "{"
               WS-QT "model" WS-QT ":"
               WS-QT "gpt-4.1-mini" WS-QT ","
               WS-QT "messages" WS-QT ":"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Copy conversation
           COMPUTE WS-K = WS-CONV-PTR - 1
           MOVE WS-CONV-BUF(1:WS-K)
               TO WS-REQ-JSON(WS-PTR:WS-K)
           ADD WS-K TO WS-PTR

      *>   Tool definitions
           STRING ","
               WS-QT "tools" WS-QT ":["
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Tool 1: get_suspects
           STRING
               "{" WS-QT "type" WS-QT ":"
               WS-QT "function" WS-QT ","
               WS-QT "function" WS-QT ":{"
               WS-QT "name" WS-QT ":"
               WS-QT "get_suspects" WS-QT ","
               WS-QT "description" WS-QT ":"
               WS-QT "Get list of suspects"
               WS-QT ","
               WS-QT "parameters" WS-QT ":"
               "{" WS-QT "type" WS-QT ":"
               WS-QT "object" WS-QT ","
               WS-QT "properties" WS-QT ":{},"
               WS-QT "required" WS-QT ":[]"
               "}}},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Tool 2: get_power_plants
           STRING
               "{" WS-QT "type" WS-QT ":"
               WS-QT "function" WS-QT ","
               WS-QT "function" WS-QT ":{"
               WS-QT "name" WS-QT ":"
               WS-QT "get_power_plants" WS-QT ","
               WS-QT "description" WS-QT ":"
               WS-QT "Get power plants with "
               "city and code" WS-QT ","
               WS-QT "parameters" WS-QT ":"
               "{" WS-QT "type" WS-QT ":"
               WS-QT "object" WS-QT ","
               WS-QT "properties" WS-QT ":{},"
               WS-QT "required" WS-QT ":[]"
               "}}},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Tool 3: get_closest_suspect_to_plant
           STRING
               "{" WS-QT "type" WS-QT ":"
               WS-QT "function" WS-QT ","
               WS-QT "function" WS-QT ":{"
               WS-QT "name" WS-QT ":"
               WS-QT "get_closest_suspect_"
               "to_plant" WS-QT ","
               WS-QT "description" WS-QT ":"
               WS-QT "Finds closest suspect "
               "to any plant using GPS "
               "and haversine. Returns "
               "the CLOSEST match." WS-QT ","
               WS-QT "parameters" WS-QT ":"
               "{" WS-QT "type" WS-QT ":"
               WS-QT "object" WS-QT ","
               WS-QT "properties" WS-QT ":{},"
               WS-QT "required" WS-QT ":[]"
               "}}},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Tool 4: get_access_level
           STRING
               "{" WS-QT "type" WS-QT ":"
               WS-QT "function" WS-QT ","
               WS-QT "function" WS-QT ":{"
               WS-QT "name" WS-QT ":"
               WS-QT "get_access_level" WS-QT ","
               WS-QT "description" WS-QT ":"
               WS-QT "Get access level for "
               "a suspect" WS-QT ","
               WS-QT "parameters" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "object" WS-QT ","
               WS-QT "properties" WS-QT ":{"
               WS-QT "name" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "string" WS-QT "},"
               WS-QT "surname" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "string" WS-QT "},"
               WS-QT "birth_year" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "integer" WS-QT "}},"
               WS-QT "required" WS-QT ":["
               WS-QT "name" WS-QT ","
               WS-QT "surname" WS-QT ","
               WS-QT "birth_year" WS-QT
               "]}}},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Tool 5: submit_answer
           STRING
               "{" WS-QT "type" WS-QT ":"
               WS-QT "function" WS-QT ","
               WS-QT "function" WS-QT ":{"
               WS-QT "name" WS-QT ":"
               WS-QT "submit_answer" WS-QT ","
               WS-QT "description" WS-QT ":"
               WS-QT "Submit the final answer"
               WS-QT ","
               WS-QT "parameters" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "object" WS-QT ","
               WS-QT "properties" WS-QT ":{"
               WS-QT "name" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "string" WS-QT "},"
               WS-QT "surname" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "string" WS-QT "},"
               WS-QT "access_level" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "integer" WS-QT "},"
               WS-QT "power_plant" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "string" WS-QT "}},"
               WS-QT "required" WS-QT ":["
               WS-QT "name" WS-QT ","
               WS-QT "surname" WS-QT ","
               WS-QT "access_level" WS-QT ","
               WS-QT "power_plant" WS-QT
               "]}}}],"
               WS-QT "tool_choice" WS-QT ":"
               WS-QT "auto" WS-QT "}"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Write and send
           MOVE "agent_req.json"
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

           INITIALIZE WS-CMD
           STRING
               "curl -s -o agent_resp.json"
               " -X POST "
               TRIM(WS-OPENAI-URL)
               " -H " WS-QT
               "Content-Type: "
               "application/json" WS-QT
               " -H " WS-QT
               "Authorization: Bearer "
               TRIM(WS-OPENAI-KEY) WS-QT
               " -d @agent_req.json"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD
           .

      *> ============================================================
      *> DISPATCH-TOOL
      *> ============================================================
       DISPATCH-TOOL.
           MOVE SPACES TO WS-TOOL-RESULT
           MOVE 0 TO WS-TOOL-RESULT-LEN

           EVALUATE TRIM(WS-TOOL-NAME)
           WHEN "get_suspects"
               PERFORM TOOL-GET-SUSPECTS
           WHEN "get_power_plants"
               PERFORM TOOL-GET-POWER-PLANTS
           WHEN "get_closest_suspect_to_plant"
               PERFORM TOOL-GET-CLOSEST
           WHEN "get_access_level"
               PERFORM TOOL-GET-ACCESS-LEVEL
           WHEN "submit_answer"
               PERFORM TOOL-SUBMIT-ANSWER
           WHEN OTHER
               MOVE '{"error":"Unknown tool"}'
                   TO WS-TOOL-RESULT
               MOVE 23 TO WS-TOOL-RESULT-LEN
           END-EVALUATE
           .

      *> ============================================================
      *> TOOL-GET-SUSPECTS: Return suspects JSON
      *> ============================================================
       TOOL-GET-SUSPECTS.
           MOVE SPACES TO WS-TOOL-RESULT
           MOVE 1 TO WS-PTR
           STRING "[" DELIMITED SIZE
               INTO WS-TOOL-RESULT
               WITH POINTER WS-PTR
           END-STRING

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-SUSP-CT

               IF WS-I > 1
                   STRING "," DELIMITED SIZE
                       INTO WS-TOOL-RESULT
                       WITH POINTER WS-PTR
                   END-STRING
               END-IF

               MOVE TRIM(WS-SU-NAME(WS-I))
                   TO WS-ESC-IN
               PERFORM JSON-ESCAPE-STR
               MOVE WS-ESC-OUT TO WS-TMP2
               MOVE WS-ESC-OLEN TO WS-K

               STRING
                   "{" WS-QT "name" WS-QT ":"
                   WS-QT WS-TMP2(1:WS-K) WS-QT
                   ","
                   DELIMITED SIZE
                   INTO WS-TOOL-RESULT
                   WITH POINTER WS-PTR
               END-STRING

               MOVE TRIM(WS-SU-SURNAME(WS-I))
                   TO WS-ESC-IN
               PERFORM JSON-ESCAPE-STR

               STRING
                   WS-QT "surname" WS-QT ":"
                   WS-QT WS-ESC-OUT(1:
                       WS-ESC-OLEN) WS-QT ","
                   WS-QT "birthYear" WS-QT ":"
                   WS-SU-BYEAR(WS-I) "}"
                   DELIMITED SIZE
                   INTO WS-TOOL-RESULT
                   WITH POINTER WS-PTR
               END-STRING
           END-PERFORM

           STRING "]" DELIMITED SIZE
               INTO WS-TOOL-RESULT
               WITH POINTER WS-PTR
           END-STRING
           COMPUTE WS-TOOL-RESULT-LEN =
               WS-PTR - 1
           .

      *> ============================================================
      *> TOOL-GET-POWER-PLANTS: Return plants JSON
      *> ============================================================
       TOOL-GET-POWER-PLANTS.
           MOVE SPACES TO WS-TOOL-RESULT
           MOVE 1 TO WS-PTR
           STRING "[" DELIMITED SIZE
               INTO WS-TOOL-RESULT
               WITH POINTER WS-PTR
           END-STRING

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-PLANT-CT

               IF WS-I > 1
                   STRING "," DELIMITED SIZE
                       INTO WS-TOOL-RESULT
                       WITH POINTER WS-PTR
                   END-STRING
               END-IF

               STRING
                   "{" WS-QT "city" WS-QT ":"
                   WS-QT
                   TRIM(WS-PL-CITY(WS-I))
                   WS-QT ","
                   WS-QT "code" WS-QT ":"
                   WS-QT
                   TRIM(WS-PL-CODE(WS-I))
                   WS-QT "}"
                   DELIMITED SIZE
                   INTO WS-TOOL-RESULT
                   WITH POINTER WS-PTR
               END-STRING
           END-PERFORM

           STRING "]" DELIMITED SIZE
               INTO WS-TOOL-RESULT
               WITH POINTER WS-PTR
           END-STRING
           COMPUTE WS-TOOL-RESULT-LEN =
               WS-PTR - 1
           .

      *> ============================================================
      *> TOOL-GET-CLOSEST: The KEY tool. Does ALL work:
      *>  - Fetches GPS for every suspect (Hub API)
      *>  - Computes haversine vs every plant (geocoded)
      *>  - Returns summary with CLOSEST match
      *> ============================================================
       TOOL-GET-CLOSEST.
           DISPLAY "  == Computing closest =="
           MOVE "N" TO WS-BEST-FOUND
           MOVE +99999 TO WS-BEST-DIST
           MOVE SPACES TO WS-SUMMARY-BUF
           MOVE 1 TO WS-SUMMARY-PTR

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-SUSP-CT

               DISPLAY "    Suspect "
                   TRIM(WS-SU-NAME(WS-I)) " "
                   TRIM(WS-SU-SURNAME(WS-I))

      *>       Fetch locations for this suspect
               PERFORM FETCH-SUSPECT-LOCS

      *>       If no locations, skip
               IF WS-LOC-JLEN = 0
                   DISPLAY "      No locations"
                   STRING
                       TRIM(WS-SU-NAME(WS-I))
                       " "
                       TRIM(WS-SU-SURNAME(WS-I))
                       " | no locations"
                       WS-NL
                       DELIMITED SIZE
                       INTO WS-SUMMARY-BUF
                       WITH POINTER
                           WS-SUMMARY-PTR
                   END-STRING
               ELSE
      *>           Parse locations array and match
                   PERFORM MATCH-SUSPECT-PLANTS
               END-IF
           END-PERFORM

      *>   Build tool result
           MOVE SPACES TO WS-TOOL-RESULT
           MOVE 1 TO WS-PTR

           IF WS-BEST-FOUND = "Y"
               MOVE WS-BEST-DIST TO WS-DISP-DIST
               STRING
                   "CLOSEST: name="
                   TRIM(WS-BEST-NAME)
                   " surname="
                   TRIM(WS-BEST-SURNAME)
                   " birthYear="
                   WS-BEST-BYEAR
                   " distance="
                   TRIM(WS-DISP-DIST)
                   "km plant="
                   TRIM(WS-BEST-CITY)
                   " plantCode="
                   TRIM(WS-BEST-CODE)
                   WS-NL WS-NL
                   "All results:" WS-NL
                   DELIMITED SIZE
                   INTO WS-TOOL-RESULT
                   WITH POINTER WS-PTR
               END-STRING
           ELSE
               STRING
                   "NO MATCH FOUND" WS-NL
                   WS-NL
                   "All results:" WS-NL
                   DELIMITED SIZE
                   INTO WS-TOOL-RESULT
                   WITH POINTER WS-PTR
               END-STRING
           END-IF

      *>   Append summary
           COMPUTE WS-K = WS-SUMMARY-PTR - 1
           IF WS-K > 0
               MOVE WS-SUMMARY-BUF(1:WS-K)
                   TO WS-TOOL-RESULT(WS-PTR:WS-K)
               ADD WS-K TO WS-PTR
           END-IF

           COMPUTE WS-TOOL-RESULT-LEN =
               WS-PTR - 1

           DISPLAY "  == Result: "
               WS-TOOL-RESULT(
                   1:WS-TOOL-RESULT-LEN)
           .

      *> ============================================================
      *> FETCH-SUSPECT-LOCS: Fetch GPS locs for suspect WS-I
      *> Result in WS-LOC-JBUF / WS-LOC-JLEN
      *> ============================================================
       FETCH-SUSPECT-LOCS.
           MOVE SPACES TO WS-LOC-JBUF
           MOVE 0 TO WS-LOC-JLEN

      *>   Build request body
           MOVE TRIM(WS-SU-NAME(WS-I))
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR
           MOVE WS-ESC-OUT TO WS-TMP2
           MOVE WS-ESC-OLEN TO WS-K

           INITIALIZE WS-TMP
           STRING "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "name" WS-QT ":"
               WS-QT WS-TMP2(1:WS-K) WS-QT ","
               DELIMITED SIZE INTO WS-TMP
           END-STRING

           MOVE TRIM(WS-SU-SURNAME(WS-I))
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

           MOVE SPACES TO WS-TMP2
           STRING TRIM(WS-TMP)
               WS-QT "surname" WS-QT ":"
               WS-QT WS-ESC-OUT(1:WS-ESC-OLEN)
               WS-QT "}"
               DELIMITED SIZE INTO WS-TMP2
           END-STRING
           MOVE WS-TMP2 TO WS-TMP

      *>   POST with retry
           MOVE 0 TO WS-RETRY-CT
           PERFORM UNTIL WS-RETRY-CT >= WS-SUB-RETRIES
               PERFORM WRITE-REQ-BODY
               INITIALIZE WS-CMD
               STRING
                   "curl -s -o locs.json"
                   " -X POST "
                   TRIM(WS-LOCATION-URL)
                   " -H " WS-QT
                   "Content-Type: "
                   "application/json" WS-QT
                   " -d @request_body.tmp"
                   DELIMITED SIZE INTO WS-CMD
               END-STRING
               CALL "SYSTEM" USING WS-CMD

               MOVE "locs.json"
                   TO WS-WORK-PATH
               PERFORM READ-JSON-FILE
               MOVE "work.tmp" TO WS-WORK-PATH

      *>       Check for valid location data
               IF WS-JLEN > 0
                   MOVE 0 TO WS-TALLY-CNT
                   INSPECT WS-JBUF(1:WS-JLEN)
                       TALLYING WS-TALLY-CNT
                       FOR ALL "latitude"
                   IF WS-TALLY-CNT > 0
                       MOVE WS-JBUF(1:WS-JLEN)
                           TO WS-LOC-JBUF
                       MOVE WS-JLEN
                           TO WS-LOC-JLEN
                       EXIT PERFORM
                   END-IF
      *>           Empty array [] is valid
                   IF WS-JBUF(1:2) = "[]"
                       MOVE WS-JBUF(1:WS-JLEN)
                           TO WS-LOC-JBUF
                       MOVE WS-JLEN
                           TO WS-LOC-JLEN
                       EXIT PERFORM
                   END-IF
               END-IF

               ADD 1 TO WS-RETRY-CT
               IF WS-RETRY-CT < WS-SUB-RETRIES
                   DISPLAY "      Retry "
                       WS-RETRY-CT " locs..."
                   CALL "C$SLEEP" USING WS-SUB-RETRY-DELAY
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> MATCH-SUSPECT-PLANTS: Parse location array,
      *> compute distance vs each plant, track best
      *> Uses WS-LOC-JBUF, updates WS-BEST-*
      *> ============================================================
       MATCH-SUSPECT-PLANTS.
           MOVE +99999 TO WS-SU-BEST-DIST
           MOVE SPACES TO WS-SU-BEST-CITY
           MOVE SPACES TO WS-SU-BEST-CODE
           MOVE "N" TO WS-SU-HAS-MATCH

      *>   Save main JBUF, work with LOC buf
           MOVE WS-JBUF TO WS-JBUF-SAVE
           MOVE WS-JLEN TO WS-JLEN-SAVE
           MOVE WS-LOC-JBUF TO WS-JBUF
           MOVE WS-LOC-JLEN TO WS-JLEN

      *>   Find the array
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-ARRAY

           IF WS-ARR-START = 0
               MOVE WS-JBUF-SAVE TO WS-JBUF
               MOVE WS-JLEN-SAVE TO WS-JLEN
               STRING
                   TRIM(WS-SU-NAME(WS-I))
                   " "
                   TRIM(WS-SU-SURNAME(WS-I))
                   " | no valid locations"
                   WS-NL
                   DELIMITED SIZE
                   INTO WS-SUMMARY-BUF
                   WITH POINTER WS-SUMMARY-PTR
               END-STRING
               EXIT PARAGRAPH
           END-IF

      *>   Save arr bounds, restore main buf
           MOVE WS-ARR-START TO WS-LOC-ARR-START
           MOVE WS-ARR-END TO WS-LOC-ARR-END
           MOVE WS-ARR-START TO WS-LOC-SCAN

      *>   Iterate location objects
           PERFORM UNTIL WS-LOC-SCAN
               >= WS-LOC-ARR-END

      *>       Find next { in LOC buf
               MOVE 0 TO WS-LOC-OBJ-START
               PERFORM UNTIL WS-LOC-SCAN
                   > WS-LOC-ARR-END
                   OR WS-LOC-OBJ-START > 0
                   IF WS-LOC-JBUF(
                       WS-LOC-SCAN:1) = "{"
                       MOVE WS-LOC-SCAN
                           TO WS-LOC-OBJ-START
                   END-IF
                   ADD 1 TO WS-LOC-SCAN
               END-PERFORM

               IF WS-LOC-OBJ-START = 0
                   EXIT PERFORM
               END-IF

      *>       Find matching }
               MOVE 1 TO WS-LOC-DEPTH
               PERFORM UNTIL WS-LOC-SCAN
                   > WS-LOC-ARR-END
                   OR WS-LOC-DEPTH = 0
                   IF WS-LOC-JBUF(
                       WS-LOC-SCAN:1) = "{"
                       ADD 1 TO WS-LOC-DEPTH
                   END-IF
                   IF WS-LOC-JBUF(
                       WS-LOC-SCAN:1) = "}"
                       SUBTRACT 1
                           FROM WS-LOC-DEPTH
                   END-IF
                   ADD 1 TO WS-LOC-SCAN
               END-PERFORM
               COMPUTE WS-LOC-OBJ-END =
                   WS-LOC-SCAN - 1

      *>       Extract lat/lon from this obj
               COMPUTE WS-K = WS-LOC-OBJ-END
                   - WS-LOC-OBJ-START + 1
               IF WS-K > 500
                   MOVE 500 TO WS-K
               END-IF
               MOVE WS-LOC-JBUF(
                   WS-LOC-OBJ-START:WS-K)
                   TO WS-LOC-OBJ-BUF

      *>       Parse lat/lon using JBUF
               MOVE WS-LOC-OBJ-BUF TO WS-JBUF
               MOVE WS-K TO WS-JLEN

               MOVE "latitude" TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               IF TRIM(WS-JVAL) = SPACES
                   MOVE WS-LOC-JBUF TO WS-JBUF
                   MOVE WS-LOC-JLEN TO WS-JLEN
                   EXIT PERFORM
               END-IF
               COMPUTE WS-LOC-LAT =
                   NUMVAL(TRIM(WS-JVAL))

               MOVE "longitude"
                   TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               COMPUTE WS-LOC-LON =
                   NUMVAL(TRIM(WS-JVAL))

      *>       Restore LOC buf for continued
      *>       scanning
               MOVE WS-LOC-JBUF TO WS-JBUF
               MOVE WS-LOC-JLEN TO WS-JLEN

      *>       Compare vs each plant
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > WS-PLANT-CT
                   MOVE WS-LOC-LAT
                       TO WS-H-LAT1
                   MOVE WS-LOC-LON
                       TO WS-H-LON1
                   MOVE WS-PL-LAT(WS-J)
                       TO WS-H-LAT2
                   MOVE WS-PL-LON(WS-J)
                       TO WS-H-LON2
                   PERFORM HAVERSINE

                   IF WS-H-DIST < 20
                   AND WS-H-DIST
                       < WS-SU-BEST-DIST
                       MOVE WS-H-DIST
                           TO WS-SU-BEST-DIST
                       MOVE WS-PL-CITY(WS-J)
                           TO WS-SU-BEST-CITY
                       MOVE WS-PL-CODE(WS-J)
                           TO WS-SU-BEST-CODE
                       MOVE "Y"
                           TO WS-SU-HAS-MATCH
                   END-IF

      *>           Track global best
                   IF WS-H-DIST < 20
                   AND WS-H-DIST < WS-BEST-DIST
                       MOVE WS-H-DIST
                           TO WS-BEST-DIST
                       MOVE WS-SU-NAME(WS-I)
                           TO WS-BEST-NAME
                       MOVE WS-SU-SURNAME(WS-I)
                           TO WS-BEST-SURNAME
                       MOVE WS-SU-BYEAR(WS-I)
                           TO WS-BEST-BYEAR
                       MOVE WS-PL-CITY(WS-J)
                           TO WS-BEST-CITY
                       MOVE WS-PL-CODE(WS-J)
                           TO WS-BEST-CODE
                       MOVE "Y"
                           TO WS-BEST-FOUND
                   END-IF
               END-PERFORM
           END-PERFORM

      *>   Restore main JBUF
           MOVE WS-JBUF-SAVE TO WS-JBUF
           MOVE WS-JLEN-SAVE TO WS-JLEN

      *>   Add to summary
           IF WS-SU-HAS-MATCH = "Y"
               MOVE WS-SU-BEST-DIST
                   TO WS-DISP-DIST
               STRING
                   TRIM(WS-SU-NAME(WS-I))
                   " "
                   TRIM(WS-SU-SURNAME(WS-I))
                   " | "
                   TRIM(WS-SU-BEST-CITY)
                   " | "
                   TRIM(WS-DISP-DIST)
                   "km | "
                   TRIM(WS-SU-BEST-CODE)
                   WS-NL
                   DELIMITED SIZE
                   INTO WS-SUMMARY-BUF
                   WITH POINTER WS-SUMMARY-PTR
               END-STRING
               DISPLAY "      MATCH "
                   TRIM(WS-SU-BEST-CITY)
                   " " TRIM(WS-DISP-DIST) "km"
           ELSE
               STRING
                   TRIM(WS-SU-NAME(WS-I))
                   " "
                   TRIM(WS-SU-SURNAME(WS-I))
                   " | no match within 20km"
                   WS-NL
                   DELIMITED SIZE
                   INTO WS-SUMMARY-BUF
                   WITH POINTER WS-SUMMARY-PTR
               END-STRING
               DISPLAY "      No match"
           END-IF
           .

      *> ============================================================
      *> TOOL-GET-ACCESS-LEVEL: Call Hub /api/accesslevel
      *> ============================================================
       TOOL-GET-ACCESS-LEVEL.
           MOVE WS-JBUF TO WS-JBUF-SAVE
           MOVE WS-JLEN TO WS-JLEN-SAVE
           MOVE WS-TOOL-ARGS TO WS-JBUF
           MOVE LENGTH(TRIM(WS-TOOL-ARGS))
               TO WS-JLEN

           MOVE "name" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL) TO WS-TA-NAME

           MOVE "surname" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL)
               TO WS-TA-SURNAME

           MOVE "birth_year" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           COMPUTE WS-TA-BYEAR =
               NUMVAL(TRIM(WS-JVAL))

           MOVE WS-JBUF-SAVE TO WS-JBUF
           MOVE WS-JLEN-SAVE TO WS-JLEN

           DISPLAY "    -> access for "
               TRIM(WS-TA-NAME) " "
               TRIM(WS-TA-SURNAME)
               " born " WS-TA-BYEAR

      *>   Build request body
           MOVE TRIM(WS-TA-NAME)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR
           MOVE WS-ESC-OUT TO WS-TMP2
           MOVE WS-ESC-OLEN TO WS-K

           INITIALIZE WS-TMP
           STRING "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "name" WS-QT ":"
               WS-QT WS-TMP2(1:WS-K) WS-QT ","
               DELIMITED SIZE INTO WS-TMP
           END-STRING

           MOVE TRIM(WS-TA-SURNAME)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

           MOVE SPACES TO WS-TMP2
           STRING TRIM(WS-TMP)
               WS-QT "surname" WS-QT ":"
               WS-QT WS-ESC-OUT(1:WS-ESC-OLEN)
               WS-QT ","
               WS-QT "birthYear" WS-QT ":"
               WS-TA-BYEAR "}"
               DELIMITED SIZE INTO WS-TMP2
           END-STRING
           MOVE WS-TMP2 TO WS-TMP

      *>   POST with retry
           MOVE 0 TO WS-RETRY-CT
           PERFORM UNTIL WS-RETRY-CT >= WS-SUB-RETRIES
               PERFORM WRITE-REQ-BODY
               INITIALIZE WS-CMD
               STRING
                   "curl -s -o access.json"
                   " -X POST "
                   TRIM(WS-ACCESS-URL)
                   " -H " WS-QT
                   "Content-Type: "
                   "application/json" WS-QT
                   " -d @request_body.tmp"
                   DELIMITED SIZE INTO WS-CMD
               END-STRING
               CALL "SYSTEM" USING WS-CMD

               MOVE "access.json"
                   TO WS-WORK-PATH
               PERFORM READ-JSON-FILE
               MOVE "work.tmp" TO WS-WORK-PATH

               IF WS-JLEN > 0
                   MOVE 0 TO WS-TALLY-CNT
                   INSPECT WS-JBUF(1:WS-JLEN)
                       TALLYING WS-TALLY-CNT
                       FOR ALL "ccessLevel"
                   IF WS-TALLY-CNT > 0
                       MOVE WS-JBUF(1:WS-JLEN)
                           TO WS-TOOL-RESULT
                       MOVE WS-JLEN
                           TO WS-TOOL-RESULT-LEN
                       EXIT PERFORM
                   END-IF
               END-IF

               ADD 1 TO WS-RETRY-CT
               IF WS-RETRY-CT < WS-SUB-RETRIES
                   DISPLAY "      Retry "
                       WS-RETRY-CT " access..."
                   CALL "C$SLEEP" USING WS-SUB-RETRY-DELAY
               END-IF
           END-PERFORM

           IF WS-TOOL-RESULT-LEN = 0
               MOVE '{"error":"no access data"}'
                   TO WS-TOOL-RESULT
               MOVE 26 TO WS-TOOL-RESULT-LEN
           END-IF
           .

      *> ============================================================
      *> TOOL-SUBMIT-ANSWER: Submit to Hub /verify
      *> ============================================================
       TOOL-SUBMIT-ANSWER.
           MOVE WS-JBUF TO WS-JBUF-SAVE
           MOVE WS-JLEN TO WS-JLEN-SAVE
           MOVE WS-TOOL-ARGS TO WS-JBUF
           MOVE LENGTH(TRIM(WS-TOOL-ARGS))
               TO WS-JLEN

           MOVE "name" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL) TO WS-TA-NAME

           MOVE "surname" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL)
               TO WS-TA-SURNAME

           MOVE "access_level" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL) TO WS-NUM-STR

           MOVE "power_plant" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL) TO WS-TA-ANSWER

           MOVE WS-JBUF-SAVE TO WS-JBUF
           MOVE WS-JLEN-SAVE TO WS-JLEN

      *>   Build submit payload
           MOVE TRIM(WS-TA-NAME)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR
           MOVE WS-ESC-OUT TO WS-AG-CONTENT
           MOVE WS-ESC-OLEN TO WS-K

           MOVE SPACES TO WS-TOOL-RESULT
           MOVE 1 TO WS-PTR
           STRING "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT "findhim" WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "name" WS-QT ":"
               WS-QT WS-AG-CONTENT(1:WS-K)
               WS-QT ","
               DELIMITED SIZE
               INTO WS-TOOL-RESULT
               WITH POINTER WS-PTR
           END-STRING

      *>   Escape surname
           MOVE TRIM(WS-TA-SURNAME)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

           STRING
               WS-QT "surname" WS-QT ":"
               WS-QT WS-ESC-OUT(1:WS-ESC-OLEN)
               WS-QT ","
               WS-QT "accessLevel" WS-QT ":"
               TRIM(WS-NUM-STR) ","
               WS-QT "powerPlant" WS-QT ":"
               WS-QT TRIM(WS-TA-ANSWER) WS-QT
               "}}"
               DELIMITED SIZE
               INTO WS-TOOL-RESULT
               WITH POINTER WS-PTR
           END-STRING

           COMPUTE WS-TOOL-RESULT-LEN =
               WS-PTR - 1

           DISPLAY "  Submit: "
               WS-TOOL-RESULT(
                   1:WS-TOOL-RESULT-LEN)

      *>   Write and POST
           MOVE WS-TOOL-RESULT(
               1:WS-TOOL-RESULT-LEN)
               TO WS-TMP
           PERFORM WRITE-REQ-BODY

           INITIALIZE WS-CMD
           STRING
               "curl -s -o submit.json"
               " -X POST "
               TRIM(WS-VERIFY-URL)
               " -H " WS-QT
               "Content-Type: "
               "application/json" WS-QT
               " -d @request_body.tmp"
               DELIMITED SIZE INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Read response
           MOVE "submit.json" TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH
           DISPLAY "  Response: "
               TRIM(WS-JBUF)

           MOVE SPACES TO WS-TOOL-RESULT
           MOVE TRIM(WS-JBUF)
               TO WS-TOOL-RESULT
           MOVE LENGTH(TRIM(WS-JBUF))
               TO WS-TOOL-RESULT-LEN

      *>   Check for flag
           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-JBUF(1:WS-JLEN)
               TALLYING WS-TALLY-CNT
               FOR ALL "FLG:"
           IF WS-TALLY-CNT > 0
               DISPLAY "  >>> FLAG FOUND <<<"
               MOVE "Y" TO WS-AG-DONE
           END-IF
           .

      *> ============================================================
      *> APPEND-TOOL-EXCHANGE
      *> ============================================================
       APPEND-TOOL-EXCHANGE.
           SUBTRACT 1 FROM WS-CONV-PTR

      *>   Escape tool args
           MOVE TRIM(WS-TOOL-ARGS) TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

           STRING ","
               "{" WS-QT "role" WS-QT ":"
               WS-QT "assistant" WS-QT ","
               WS-QT "content" WS-QT ":null,"
               WS-QT "tool_calls" WS-QT ":["
               "{" WS-QT "id" WS-QT ":"
               WS-QT TRIM(WS-TOOL-CALL-ID)
               WS-QT ","
               WS-QT "type" WS-QT ":"
               WS-QT "function" WS-QT ","
               WS-QT "function" WS-QT ":{"
               WS-QT "name" WS-QT ":"
               WS-QT TRIM(WS-TOOL-NAME)
               WS-QT ","
               WS-QT "arguments" WS-QT ":"
               WS-QT
               WS-ESC-OUT(1:WS-ESC-OLEN)
               WS-QT
               "}}]},"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

      *>   Escape tool result
           MOVE WS-TOOL-RESULT(
               1:WS-TOOL-RESULT-LEN)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

           STRING
               "{" WS-QT "role" WS-QT ":"
               WS-QT "tool" WS-QT ","
               WS-QT "tool_call_id" WS-QT ":"
               WS-QT TRIM(WS-TOOL-CALL-ID)
               WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               WS-ESC-OUT(1:WS-ESC-OLEN)
               WS-QT "}]"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING
           .

      *> ============================================================
      *> APPEND-TEXT-AND-NUDGE
      *> ============================================================
       APPEND-TEXT-AND-NUDGE.
           SUBTRACT 1 FROM WS-CONV-PTR

           STRING ","
               "{" WS-QT "role" WS-QT ":"
               WS-QT "assistant" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT "OK" WS-QT "},"
               "{" WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT "Call the next tool now."
               WS-QT "}]"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING
           .

      *> ============================================================
      *> HAVERSINE: Distance between two GPS points
      *> ============================================================
       HAVERSINE.
           COMPUTE WS-H-DLAT =
               (WS-H-LAT2 - WS-H-LAT1)
               * WS-RADF
           COMPUTE WS-H-DLON =
               (WS-H-LON2 - WS-H-LON1)
               * WS-RADF

           COMPUTE WS-H-SIN1 =
               SIN(WS-H-DLAT / 2)
           COMPUTE WS-H-SIN1 =
               WS-H-SIN1 * WS-H-SIN1

           COMPUTE WS-H-SIN2 =
               SIN(WS-H-DLON / 2)
           COMPUTE WS-H-SIN2 =
               WS-H-SIN2 * WS-H-SIN2

           COMPUTE WS-H-COS1 =
               COS(WS-H-LAT1 * WS-RADF)
           COMPUTE WS-H-COS2 =
               COS(WS-H-LAT2 * WS-RADF)

           COMPUTE WS-H-A = WS-H-SIN1 +
               WS-H-COS1 * WS-H-COS2
               * WS-H-SIN2

           COMPUTE WS-H-DIST =
               6371 * 2 * ASIN(SQRT(WS-H-A))
           .

       COPY ENVLOAD-PROC.
       COPY JSONPARSE-PROC.
       COPY JSONREAD-PROC.
       COPY JSONESCAPE-PROC.
       COPY JSONUNESCAPE-PROC.
       COPY TOOLPARSE-PROC.
