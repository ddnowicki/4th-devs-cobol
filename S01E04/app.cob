       IDENTIFICATION DIVISION.
       PROGRAM-ID. S01E04-SENDIT.
      *> ============================================================
      *> S01E04 - SPK Transport Declaration (Pure COBOL)
      *> 1. Build prompt with SPK docs, rules, template
      *> 2. Call OpenAI API to generate filled declaration
      *> 3. Submit to Hub /verify with retry on errors
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
       01  WORK-REC                PIC X(16000).

       WORKING-STORAGE SECTION.
      *> -- Config --
       01  WS-HUB-KEY              PIC X(50).
       01  WS-OPENAI-KEY           PIC X(200).
       01  WS-MODEL                PIC X(20) VALUE "gpt-4.1-mini".
       01  WS-QT                   PIC X(1) VALUE '"'.
       01  WS-FS                   PIC XX.
       01  WS-WORK-PATH            PIC X(100) VALUE "work.tmp".

      *> -- URLs --
       01  WS-HUB-URL              PIC X(100).
       01  WS-VERIFY-URL           PIC X(200).
       01  WS-OPENAI-URL           PIC X(200).

      *> -- System command --
       01  WS-CMD                  PIC X(4000).

      *> -- Date --
       01  WS-CURR-DATE            PIC X(21).
       01  WS-DATE-STR             PIC X(10).

      *> -- JSON newline: backslash (0x5C) + n --
       01  WS-NL                   PIC X(2).

      *> -- Separator lines (54 chars each) --
       01  WS-SEP                  PIC X(54) VALUE
           "------------------------------------------------------".
       01  WS-DBL                  PIC X(54) VALUE
           "======================================================".

      *> -- Prompt building --
       01  WS-SYS-PROMPT           PIC X(2000).
       01  WS-USER-PROMPT          PIC X(4000).
       01  WS-PTR                  PIC 9(5).

      *> -- OpenAI request / response --
       01  WS-REQ-JSON             PIC X(16000).
       01  WS-DECL                 PIC X(2000).
       01  WS-PAYLOAD              PIC X(4000).

      *> -- JSON buffer for parsing --
       01  WS-JBUF                 PIC X(16000).
       01  WS-JLEN                 PIC 9(5).
       01  WS-JPOS                 PIC 9(5).
       01  WS-JVAL                 PIC X(2000).

      *> -- JSON parsing temps (from S01E02) --
       01  WS-KEY-SEARCH           PIC X(50).
       01  WS-KEY-POS              PIC 9(5).
       01  WS-VAL-START            PIC 9(5).
       01  WS-VAL-END              PIC 9(5).
       01  WS-FJV-POS              PIC 9(5).
       01  WS-TMP                  PIC X(500).

      *> -- Retry loop --
       01  WS-ATTEMPT              PIC 9(1).
       01  WS-SUCCESS              PIC X VALUE "N".
       01  WS-ERROR-MSG            PIC X(500).

      *> -- Response reading --
       01  WS-EOF                  PIC X VALUE "N".
       01  WS-LINE                 PIC X(4000).
       01  WS-K                    PIC 9(5).
       01  WS-TALLY-CNT            PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S01E04 SENDIT - COBOL ==="

           ACCEPT WS-HUB-KEY FROM ENVIRONMENT "HUB_API_KEY"
           ACCEPT WS-OPENAI-KEY FROM ENVIRONMENT "OPENAI_API_KEY"
           ACCEPT WS-HUB-URL FROM ENVIRONMENT "HUB_API_URL"
           ACCEPT WS-OPENAI-URL FROM ENVIRONMENT "OPENAI_API_URL"

           IF WS-HUB-KEY = SPACES
               DISPLAY "BLAD: Ustaw HUB_API_KEY!"
               STOP RUN
           END-IF
           IF WS-OPENAI-KEY = SPACES
               DISPLAY "BLAD: Ustaw OPENAI_API_KEY!"
               STOP RUN
           END-IF
           IF WS-HUB-URL = SPACES
               DISPLAY "BLAD: Ustaw HUB_API_URL!"
               STOP RUN
           END-IF
           IF WS-OPENAI-URL = SPACES
               DISPLAY "BLAD: Ustaw OPENAI_API_URL!"
               STOP RUN
           END-IF

      *>   Build verify URL from hub base URL
           MOVE SPACES TO WS-VERIFY-URL
           STRING TRIM(WS-HUB-URL) "/verify"
               DELIMITED SIZE INTO WS-VERIFY-URL
           END-STRING

      *>   Init JSON newline: backslash (0x5C) + n
           MOVE X"5C" TO WS-NL(1:1)
           MOVE "n" TO WS-NL(2:1)

           PERFORM GET-CURRENT-DATE
           PERFORM BUILD-SYSTEM-PROMPT

           MOVE SPACES TO WS-ERROR-MSG
           MOVE "N" TO WS-SUCCESS

           PERFORM VARYING WS-ATTEMPT FROM 1 BY 1
               UNTIL WS-ATTEMPT > 5
               OR WS-SUCCESS = "Y"
               DISPLAY " "
               DISPLAY "--- Proba " WS-ATTEMPT "/5 ---"

               PERFORM BUILD-USER-PROMPT
               PERFORM CALL-OPENAI-API
               PERFORM EXTRACT-DECLARATION

               IF TRIM(WS-DECL) = SPACES
                   DISPLAY "  BLAD: Brak deklaracji!"
                   EXIT PERFORM
               END-IF

               PERFORM SUBMIT-DECLARATION
               PERFORM CHECK-RESPONSE
           END-PERFORM

           IF WS-SUCCESS NOT = "Y"
               DISPLAY " "
               DISPLAY "  NIEPOWODZENIE."
           END-IF

           DISPLAY " "
           DISPLAY "=== ZAKONCZONO ==="
           STOP RUN.

      *> ============================================================
      *> Get today's date as YYYY-MM-DD
      *> ============================================================
       GET-CURRENT-DATE.
           MOVE CURRENT-DATE TO WS-CURR-DATE
           INITIALIZE WS-DATE-STR
           STRING WS-CURR-DATE(1:4) "-"
                  WS-CURR-DATE(5:2) "-"
                  WS-CURR-DATE(7:2)
                  DELIMITED SIZE
                  INTO WS-DATE-STR
           END-STRING
           DISPLAY "  Data: " TRIM(WS-DATE-STR)
           .

      *> ============================================================
      *> BUILD-SYSTEM-PROMPT: Rules for the LLM
      *> ============================================================
       BUILD-SYSTEM-PROMPT.
           MOVE SPACES TO WS-SYS-PROMPT
           STRING
               "You fill SPK transport declarations."
               " Output ONLY the filled declaration"
               " text. No explanations, no markdown"
               " code blocks, no extra text."
               " Match template format EXACTLY."
               " Category A (Strategic) is EXEMPT"
               " from ALL fees: base, weight,"
               " route, extra wagons."
               " Reactor fuel cassettes ="
               " Category A."
               " Categories A and B CAN use"
               " blocked (X-xx) routes per"
               " section 8.3."
               " WDP = ceil((mass_kg - 1000)"
               " / 500)."
               " For 2800 kg: WDP = 4."
               " For cat A, WDP fee NOT charged."
               " KWOTA DO ZAPŁATY for cat A"
               " = 0 PP."
               " UWAGI SPECJALNE: brak."
               " Do not add any uwagi."
               DELIMITED SIZE
               INTO WS-SYS-PROMPT
           END-STRING
           .

      *> ============================================================
      *> BUILD-USER-PROMPT: Task data + template + docs
      *> ============================================================
       BUILD-USER-PROMPT.
           MOVE SPACES TO WS-USER-PROMPT
           MOVE 1 TO WS-PTR

      *>   Task data
           STRING
               "Fill the declaration:"
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL
               "- DATA: " TRIM(WS-DATE-STR)
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL
               "- NADAWCA: 450202122"
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL
               "- PUNKT NADAWCZY: "
               "Gdańsk"
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL
               "- PUNKT DOCELOWY: "
               "Żarnowiec"
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL
               "- Zawartość: "
               "kasety z paliwem do reaktora"
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL
               "- Masa: 2800 kg"
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL
               "- Budżet: 0 PP"
               " (free, financed by System)"
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL
               "- Uwagi specjalne: brak"
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING

      *>   Template header
           STRING WS-NL WS-NL "TEMPLATE:" WS-NL
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING

      *>   Template content with \n separators
           PERFORM APPEND-TEMPLATE

      *>   Blocked routes info
           STRING WS-NL WS-NL
               "BLOCKED ROUTES:"
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL
               "X-01: Gdańsk - Żarnowiec"
               " (allowed for cat A/B"
               " per section 8.3)"
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING

      *>   Wagons info
           STRING WS-NL WS-NL
               "ADDITIONAL WAGONS: "
               "Base = 2 wagons x 500kg"
               " = 1000kg. Extra wagons"
               " at 500kg each, 55 PP each."
               " Cat A and B:"
               " wagon fee NOT charged."
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING

      *>   Error feedback (if retry)
           IF TRIM(WS-ERROR-MSG) NOT = SPACES
               STRING WS-NL WS-NL
                   "PREVIOUS ATTEMPT REJECTED:"
                   DELIMITED SIZE
                   INTO WS-USER-PROMPT
                   WITH POINTER WS-PTR
               END-STRING
               STRING WS-NL
                   TRIM(WS-ERROR-MSG)
                   DELIMITED SIZE
                   INTO WS-USER-PROMPT
                   WITH POINTER WS-PTR
               END-STRING
               STRING WS-NL
                   "Fix the declaration."
                   DELIMITED SIZE
                   INTO WS-USER-PROMPT
                   WITH POINTER WS-PTR
               END-STRING
           END-IF
           .

      *> -- Append declaration template to user prompt --
       APPEND-TEMPLATE.
           STRING
               "SYSTEM PRZESYŁEK KONDUKTORSKICH"
               " - DEKLARACJA ZAWARTOŚCI"
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL WS-DBL
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL
               "DATA: [YYYY-MM-DD]"
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL
               "PUNKT NADAWCZY: [miasto]"
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL WS-SEP
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL
               "NADAWCA: [identyfikator]"
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL
               "PUNKT DOCELOWY: [miasto]"
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL
               "TRASA: [kod trasy]"
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL WS-SEP
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL
               "KATEGORIA PRZESYŁKI: A/B/C/D/E"
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL WS-SEP
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL
               "OPIS ZAWARTOŚCI"
               " (max 200 "
               "znaków): [...]"
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL WS-SEP
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL
               "DEKLAROWANA MASA (kg): [...]"
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL WS-SEP
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL
               "WDP: [liczba]"
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL WS-SEP
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL
               "UWAGI SPECJALNE: [...]"
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL WS-SEP
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL
               "KWOTA DO ZAPŁATY: [PP]"
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL WS-SEP
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL
               "OŚWIADCZAM, ŻE PODANE"
               " INFORMACJE SĄ PRAWDZIWE."
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL
               "BIORĘ NA SIEBIE "
               "KONSEKWENCJĘ"
               " ZA FAŁSZYWE "
               "OŚWIADCZENIE."
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           STRING WS-NL WS-DBL
               DELIMITED SIZE INTO WS-USER-PROMPT
               WITH POINTER WS-PTR
           END-STRING
           .

      *> ============================================================
      *> CALL-OPENAI-API: Build request JSON, POST via curl
      *> ============================================================
       CALL-OPENAI-API.
           DISPLAY "  Calling OpenAI ("
               TRIM(WS-MODEL) ")..."

      *>   Build JSON request body
           MOVE SPACES TO WS-REQ-JSON
           STRING
               "{"
               WS-QT "model" WS-QT ":"
               WS-QT TRIM(WS-MODEL) WS-QT ","
               WS-QT "messages" WS-QT ":["
               "{"
               WS-QT "role" WS-QT ":"
               WS-QT "system" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT TRIM(WS-SYS-PROMPT) WS-QT
               "},"
               "{"
               WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT TRIM(WS-USER-PROMPT) WS-QT
               "}"
               "],"
               WS-QT "temperature" WS-QT ":0"
               "}"
               DELIMITED SIZE
               INTO WS-REQ-JSON
           END-STRING

      *>   Write to file
           OPEN OUTPUT WORK-FILE
           WRITE WORK-REC FROM WS-REQ-JSON
           CLOSE WORK-FILE

      *>   Call OpenAI via curl
           INITIALIZE WS-CMD
           STRING
               "curl -s -o llm_resp.json"
               " -X POST "
               TRIM(WS-OPENAI-URL)
               " -H " WS-QT
               "Content-Type: application/json"
               WS-QT
               " -H " WS-QT
               "Authorization: Bearer "
               TRIM(WS-OPENAI-KEY) WS-QT
               " -d @work.tmp"
               DELIMITED SIZE INTO WS-CMD
           END-STRING

           CALL "SYSTEM" USING WS-CMD
           .

      *> ============================================================
      *> EXTRACT-DECLARATION: Parse OpenAI response, get content
      *> ============================================================
       EXTRACT-DECLARATION.
           MOVE SPACES TO WS-DECL

      *>   Read response file
           MOVE "llm_resp.json" TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           IF WS-JLEN = 0
               DISPLAY "  BLAD: Pusta odpowiedz!"
               EXIT PARAGRAPH
           END-IF

      *>   Check for API error
           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-JBUF(1:WS-JLEN)
               TALLYING WS-TALLY-CNT FOR ALL '"error"'
           IF WS-TALLY-CNT > 0
               DISPLAY "  API ERROR: "
                   TRIM(WS-JBUF)(1:300)
               EXIT PARAGRAPH
           END-IF

      *>   Extract "content" value
           MOVE "content" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL

           IF TRIM(WS-JVAL) = SPACES
           OR TRIM(WS-JVAL) = "null"
               DISPLAY "  BLAD: Brak content!"
               EXIT PARAGRAPH
           END-IF

           MOVE WS-JVAL TO WS-DECL
           DISPLAY "  LLM zwrocil deklaracje ("
               LENGTH(TRIM(WS-DECL)) " zn.)"
           .

      *> ============================================================
      *> SUBMIT-DECLARATION: Send to Hub /verify
      *> ============================================================
       SUBMIT-DECLARATION.
           DISPLAY "  Wysylanie do Hub..."

      *>   Build payload JSON
           MOVE SPACES TO WS-PAYLOAD
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT "sendit" WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "declaration" WS-QT ":"
               WS-QT TRIM(WS-DECL) WS-QT
               "}}"
               DELIMITED SIZE
               INTO WS-PAYLOAD
           END-STRING

      *>   Write to file
           OPEN OUTPUT WORK-FILE
           WRITE WORK-REC FROM WS-PAYLOAD
           CLOSE WORK-FILE

      *>   POST via curl
           INITIALIZE WS-CMD
           STRING
               "curl -s -o hub_resp.json"
               " -X POST "
               TRIM(WS-VERIFY-URL)
               " -H " WS-QT
               "Content-Type: application/json"
               WS-QT
               " -d @work.tmp"
               DELIMITED SIZE INTO WS-CMD
           END-STRING

           CALL "SYSTEM" USING WS-CMD
           .

      *> ============================================================
      *> CHECK-RESPONSE: Check Hub result, store error for retry
      *> ============================================================
       CHECK-RESPONSE.
      *>   Read response
           MOVE "hub_resp.json" TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           DISPLAY "  Hub: " TRIM(WS-JBUF)(1:500)

      *>   Check for FLG
           MOVE 0 TO WS-TALLY-CNT
           IF WS-JLEN > 0
               INSPECT WS-JBUF(1:WS-JLEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL "FLG"
           END-IF
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-SUCCESS
               DISPLAY " "
               DISPLAY "  >>> SUKCES! Flaga znaleziona!"
               EXIT PARAGRAPH
           END-IF

      *>   Check for code:0
           MOVE "code" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           IF TRIM(WS-JVAL) = "0"
               MOVE "Y" TO WS-SUCCESS
               DISPLAY "  >>> SUKCES!"
               EXIT PARAGRAPH
           END-IF

      *>   Extract error message for retry feedback
           MOVE "message" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL) TO WS-ERROR-MSG
           DISPLAY "  Blad - ponawiam..."
           .

      *> ============================================================
      *> READ-JSON-FILE: Read file into WS-JBUF (S01E02 pattern)
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
                       MOVE LENGTH(TRIM(WS-LINE
                           TRAILING))
                           TO WS-K
                       IF WS-K > 0
                           IF WS-JLEN > 0
                               ADD 1 TO WS-JLEN
                               MOVE " "
                                   TO WS-JBUF(
                                   WS-JLEN:1)
                           END-IF
                           MOVE WS-LINE(1:WS-K)
                               TO WS-JBUF(
                               WS-JLEN + 1:WS-K)
                           ADD WS-K TO WS-JLEN
                       END-IF
               END-READ
           END-PERFORM

           CLOSE WORK-FILE
           MOVE "N" TO WS-EOF
           .

      *> ============================================================
      *> FIND-JSON-VAL: Find "key":"value" in WS-JBUF
      *> Input: WS-KEY-SEARCH, WS-JPOS (start)
      *> Output: WS-JVAL, WS-KEY-POS, WS-JPOS (updated)
      *> (from S01E02 pattern)
      *> ============================================================
       FIND-JSON-VAL.
           MOVE SPACES TO WS-JVAL
           MOVE SPACES TO WS-TMP
           STRING WS-QT TRIM(WS-KEY-SEARCH) WS-QT
               DELIMITED SIZE INTO WS-TMP
           END-STRING

      *>   Find the key
           MOVE 0 TO WS-KEY-POS
           PERFORM VARYING WS-FJV-POS FROM WS-JPOS BY 1
               UNTIL WS-FJV-POS > WS-JLEN
               OR WS-KEY-POS > 0
               IF WS-FJV-POS + LENGTH(TRIM(WS-TMP))
                   - 1 <= WS-JLEN
               AND WS-JBUF(WS-FJV-POS:
                   LENGTH(TRIM(WS-TMP)))
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
      *>       Find closing quote (skip escaped \")
               PERFORM UNTIL WS-FJV-POS > WS-JLEN
                   IF WS-JBUF(WS-FJV-POS:1) = WS-QT
                       IF WS-FJV-POS > 1
                       AND WS-JBUF(
                           WS-FJV-POS - 1:1)
                           = X"5C"
                           ADD 1 TO WS-FJV-POS
                       ELSE
                           EXIT PERFORM
                       END-IF
                   ELSE
                       ADD 1 TO WS-FJV-POS
                   END-IF
               END-PERFORM
               COMPUTE WS-VAL-END =
                   WS-FJV-POS - 1
               IF WS-VAL-END >= WS-VAL-START
               AND WS-VAL-END - WS-VAL-START
                   + 1 <= 2000
                   MOVE WS-JBUF(WS-VAL-START:
                       WS-VAL-END - WS-VAL-START
                       + 1) TO WS-JVAL
               END-IF
               ADD 1 TO WS-FJV-POS
           ELSE
      *>       Numeric: read until delimiter
               MOVE WS-FJV-POS TO WS-VAL-START
               PERFORM UNTIL WS-FJV-POS > WS-JLEN
                   OR WS-JBUF(WS-FJV-POS:1) = ","
                   OR WS-JBUF(WS-FJV-POS:1) = "}"
                   OR WS-JBUF(WS-FJV-POS:1) = "]"
                   OR WS-JBUF(WS-FJV-POS:1) = " "
                   ADD 1 TO WS-FJV-POS
               END-PERFORM
               COMPUTE WS-VAL-END =
                   WS-FJV-POS - 1
               IF WS-VAL-END >= WS-VAL-START
                   MOVE WS-JBUF(WS-VAL-START:
                       WS-VAL-END - WS-VAL-START
                       + 1) TO WS-JVAL
               END-IF
           END-IF
           MOVE WS-FJV-POS TO WS-JPOS
           .
