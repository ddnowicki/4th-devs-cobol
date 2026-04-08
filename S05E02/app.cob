       IDENTIFICATION DIVISION.
       PROGRAM-ID. S05E02-PHONECALL.
      *> ============================================================
      *> S05E02 - Phonecall: multi-turn audio conversation
      *> Pipeline per turn:
      *> 1. STT (Whisper) - decode operator audio -> text
      *> 2. LLM (gpt-4.1-mini) - decide response
      *> 3. TTS (gpt-4o-mini-tts) - text -> MP3 -> b64
      *> 4. Send audio to hub, parse response
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
           SELECT B64-FILE ASSIGN TO WS-B64-PATH
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS2.

       DATA DIVISION.
       FILE SECTION.
       FD  WORK-FILE.
       01  WORK-REC                PIC X(900000).

       FD  B64-FILE.
       01  B64-REC                 PIC X(4100).

       WORKING-STORAGE SECTION.
      *> === Environment (via copybook) ===
       COPY ENVLOAD-WS.

      *> === File I/O ===
       01  WS-BS                   PIC X(1)
                                   VALUE X"5C".
       01  WS-FS                   PIC XX.
       01  WS-FS2                  PIC XX.
       01  WS-WORK-PATH            PIC X(100)
                                   VALUE "work.tmp".
       01  WS-B64-PATH             PIC X(100)
                                   VALUE
                                   "b64_input.tmp".

      *> === URLs (task-specific) ===
       01  WS-WHISPER-URL          PIC X(200).
       01  WS-TTS-URL              PIC X(200).

      *> -- STRING pointer --
       01  WS-PTR                  PIC 9(6).

      *> -- System command --
       01  WS-CMD                  PIC X(4000).

      *> -- Request JSON buffer --
       01  WS-REQ-JSON             PIC X(200000).

      *> -- Conversation buffer --
       01  WS-CONV-BUF             PIC X(100000).
       01  WS-CONV-PTR             PIC 9(6).

      *> -- Hub API request body --
       01  WS-HUB-BODY             PIC X(16000).

      *> -- Task constants --
       01  WS-TASK-NAME            PIC X(20)
                                   VALUE
                                   "phonecall".

      *> -- Turn/attempt counters --
       01  WS-TURN                 PIC 9(2)
                                   VALUE 0.
       01  WS-MAX-TURNS            PIC 9(2)
                                   VALUE 15.
       01  WS-ATTEMPT              PIC 9(1)
                                   VALUE 0.
       01  WS-MAX-ATTEMPTS         PIC 9(1)
                                   VALUE 2.
       01  WS-FLAG-FOUND           PIC X
                                   VALUE "N".
       01  WS-SESSION-ERR          PIC X
                                   VALUE "N".
       01  WS-TALLY-CNT            PIC 9(4).

      *> -- Operator / our text --
       01  WS-OPERATOR-TXT         PIC X(4000).
       01  WS-OUR-TEXT             PIC X(2000).
       01  WS-CLEAN-TXT            PIC X(2000).
       01  WS-CLEAN-LEN            PIC 9(4).

      *> -- Base64 extraction --
       01  WS-B64-VAL-START        PIC 9(6).
       01  WS-B64-VAL-END          PIC 9(6).
       01  WS-B64-LEN              PIC 9(6).

      *> -- Chunk write vars --
       01  WS-CHK-POS              PIC 9(6).
       01  WS-CHK-REM              PIC 9(6).
       01  WS-CHK-LEN              PIC 9(6).

      *> -- Response code --
       01  WS-RESP-CODE            PIC X(10).

      *> -- Skip flag --
       01  WS-SKIP                 PIC 9(1)
                                   VALUE 0.

      *> -- Loop/misc vars --
       01  WS-I                    PIC 9(6).
       01  WS-K                    PIC 9(6).

      *> -- Temp variables --
       01  WS-TMP                  PIC X(4000).
       01  WS-TMP2                 PIC X(500).

      *> === JSONPARSE-WS (inline, large) ===
       01  WS-JBUF                 PIC X(900000).
       01  WS-JLEN                 PIC 9(6).
       01  WS-JPOS                 PIC 9(6).
       01  WS-JVAL                 PIC X(8000).
       01  WS-KEY-SEARCH           PIC X(50).
       01  WS-KEY-POS              PIC 9(6).
       01  WS-VAL-START            PIC 9(6).
       01  WS-VAL-END              PIC 9(6).
       01  WS-FJV-POS              PIC 9(6).

      *> === JSONESCAPE-WS (inline) ===
       01  WS-ESC-IN               PIC X(8000).
       01  WS-ESC-OUT              PIC X(16000).
       01  WS-ESC-ILEN             PIC 9(6).
       01  WS-ESC-OLEN             PIC 9(6).
       01  WS-ESC-I                PIC 9(6).

      *> === JSONREAD-WS (inline, large) ===
       01  WS-EOF                  PIC X VALUE "N".
       01  WS-LINE                 PIC X(900000).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S05E02 PHONECALL ==="

           PERFORM LOAD-ENV-VARS

      *>   Build API URLs
           PERFORM BUILD-WHISPER-URL
           PERFORM BUILD-TTS-URL

      *>   Attempt loop
           MOVE 0 TO WS-ATTEMPT
           MOVE "N" TO WS-FLAG-FOUND

           PERFORM UNTIL
               WS-FLAG-FOUND = "Y"
               OR WS-ATTEMPT
               >= WS-MAX-ATTEMPTS

               ADD 1 TO WS-ATTEMPT
               DISPLAY " "
               DISPLAY "=== ATTEMPT "
                   WS-ATTEMPT
                   " of " WS-MAX-ATTEMPTS
                   " ==="

      *>       Clean temp files
               INITIALIZE WS-CMD
               STRING
                   "rm -f hub_resp.json "
                   "llm_resp.json "
                   "llm_req.json "
                   "whisper_resp.json "
                   "tts_resp.mp3 "
                   "tts_b64.tmp "
                   "b64_input.tmp "
                   "audio_in.mp3 "
                   "hub_req.tmp "
                   "hub_prefix.tmp "
                   "hub_suffix.tmp "
                   "work.tmp"
                   DELIMITED SIZE
                   INTO WS-CMD
               END-STRING
               CALL "SYSTEM" USING WS-CMD

      *>       Reset state
               MOVE SPACES TO WS-CONV-BUF
               MOVE 1 TO WS-CONV-PTR
               MOVE 0 TO WS-TURN
               MOVE "N" TO WS-SESSION-ERR

               PERFORM RUN-CONVERSATION

               IF WS-FLAG-FOUND = "Y"
                   EXIT PERFORM
               END-IF

               DISPLAY " "
               DISPLAY "  No flag, retrying..."
               CALL "C$SLEEP" USING 2
           END-PERFORM

           DISPLAY " "
           IF WS-FLAG-FOUND = "Y"
               DISPLAY ">>> FLAG FOUND <<<"
           ELSE
               DISPLAY "No flag found after "
                   WS-MAX-ATTEMPTS
                   " attempts."
           END-IF
           DISPLAY "=== DONE ==="
           STOP RUN.

      *> ============================================================
      *> BUILD-WHISPER-URL
      *> Replace /chat/completions with
      *> /audio/transcriptions in OpenAI URL
      *> ============================================================
       BUILD-WHISPER-URL.
           MOVE SPACES TO WS-WHISPER-URL
           MOVE LENGTH(TRIM(WS-OPENAI-URL))
               TO WS-K

      *>   Find "chat/completions" (16 ch)
           MOVE 0 TO WS-I
           PERFORM VARYING WS-FJV-POS
               FROM 1 BY 1
               UNTIL WS-FJV-POS > WS-K
               OR WS-I > 0
               IF WS-FJV-POS + 15
                   <= WS-K
               AND WS-OPENAI-URL(
                   WS-FJV-POS:16)
                   = "chat/completions"
                   MOVE WS-FJV-POS
                       TO WS-I
               END-IF
           END-PERFORM

           IF WS-I > 0
               COMPUTE WS-K = WS-I - 1
               IF WS-K > 0
                   STRING
                     WS-OPENAI-URL(
                       1:WS-K)
                     "audio/transcriptions"
                     DELIMITED SIZE
                     INTO WS-WHISPER-URL
                   END-STRING
               ELSE
                   STRING
                     "audio/transcriptions"
                     DELIMITED SIZE
                     INTO WS-WHISPER-URL
                   END-STRING
               END-IF
           ELSE
      *>       Fallback: last slash
               MOVE LENGTH(
                   TRIM(WS-OPENAI-URL))
                   TO WS-K
               MOVE 0 TO WS-I
               PERFORM VARYING WS-FJV-POS
                   FROM WS-K BY -1
                   UNTIL WS-FJV-POS < 1
                   OR WS-I > 0
                   IF WS-OPENAI-URL(
                       WS-FJV-POS:1)
                       = "/"
                       MOVE WS-FJV-POS
                           TO WS-I
                   END-IF
               END-PERFORM
               IF WS-I > 0
                   STRING
                     WS-OPENAI-URL(
                       1:WS-I)
                     "audio/transcriptions"
                     DELIMITED SIZE
                     INTO WS-WHISPER-URL
                   END-STRING
               ELSE
                   MOVE WS-OPENAI-URL
                       TO WS-WHISPER-URL
               END-IF
           END-IF

           DISPLAY "  Whisper URL: "
               TRIM(WS-WHISPER-URL)
           .

      *> ============================================================
      *> BUILD-TTS-URL
      *> Replace /chat/completions with
      *> /audio/speech in OpenAI URL
      *> ============================================================
       BUILD-TTS-URL.
           MOVE SPACES TO WS-TTS-URL
           MOVE LENGTH(TRIM(WS-OPENAI-URL))
               TO WS-K

      *>   Find "chat/completions" (16 ch)
           MOVE 0 TO WS-I
           PERFORM VARYING WS-FJV-POS
               FROM 1 BY 1
               UNTIL WS-FJV-POS > WS-K
               OR WS-I > 0
               IF WS-FJV-POS + 15
                   <= WS-K
               AND WS-OPENAI-URL(
                   WS-FJV-POS:16)
                   = "chat/completions"
                   MOVE WS-FJV-POS
                       TO WS-I
               END-IF
           END-PERFORM

           IF WS-I > 0
               COMPUTE WS-K = WS-I - 1
               IF WS-K > 0
                   STRING
                     WS-OPENAI-URL(
                       1:WS-K)
                     "audio/speech"
                     DELIMITED SIZE
                     INTO WS-TTS-URL
                   END-STRING
               ELSE
                   STRING
                     "audio/speech"
                     DELIMITED SIZE
                     INTO WS-TTS-URL
                   END-STRING
               END-IF
           ELSE
      *>       Fallback: last slash
               MOVE LENGTH(
                   TRIM(WS-OPENAI-URL))
                   TO WS-K
               MOVE 0 TO WS-I
               PERFORM VARYING WS-FJV-POS
                   FROM WS-K BY -1
                   UNTIL WS-FJV-POS < 1
                   OR WS-I > 0
                   IF WS-OPENAI-URL(
                       WS-FJV-POS:1)
                       = "/"
                       MOVE WS-FJV-POS
                           TO WS-I
                   END-IF
               END-PERFORM
               IF WS-I > 0
                   STRING
                     WS-OPENAI-URL(
                       1:WS-I)
                     "audio/speech"
                     DELIMITED SIZE
                     INTO WS-TTS-URL
                   END-STRING
               ELSE
                   MOVE WS-OPENAI-URL
                       TO WS-TTS-URL
               END-IF
           END-IF

           DISPLAY "  TTS URL: "
               TRIM(WS-TTS-URL)
           .

      *> ============================================================
      *> RUN-CONVERSATION
      *> Init conversation, start session, turn loop
      *> ============================================================
       RUN-CONVERSATION.
           DISPLAY " "
           DISPLAY "--- Starting conversation ---"

      *>   Build system prompt in conv buffer
           PERFORM INIT-CONVERSATION

      *>   Start session with hub
           PERFORM START-SESSION

      *>   Check for flag in start response
           MOVE "message"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-JVAL
               TALLYING WS-TALLY-CNT
               FOR ALL "{FLG:"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-FLAG-FOUND
               DISPLAY "  Flag in start!"
               DISPLAY "  "
                   TRIM(WS-JVAL)
               EXIT PARAGRAPH
           END-IF

      *>   Check if start has audio
           MOVE 0 TO WS-TALLY-CNT
           IF WS-JLEN > 0
               INSPECT WS-JBUF(1:WS-JLEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL '"audio"'
           END-IF

           IF WS-TALLY-CNT > 0
      *>       Extract and transcribe audio
               PERFORM EXTRACT-OPERATOR-AUDIO
               IF WS-B64-LEN > 0
                   PERFORM TRANSCRIBE-AUDIO
                   DISPLAY "  [OPERATOR] "
                       TRIM(WS-OPERATOR-TXT)
                       (1:200)
                   PERFORM APPEND-USER-MSG
               END-IF
           ELSE
      *>       Check for text message
               MOVE "message"
                   TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               IF WS-JVAL NOT = SPACES
                   MOVE WS-JVAL
                       TO WS-OPERATOR-TXT
                   DISPLAY "  [OPERATOR] "
                       TRIM(WS-OPERATOR-TXT)
                       (1:200)
                   PERFORM APPEND-USER-MSG
               END-IF
           END-IF

      *>   Turn loop
           MOVE 0 TO WS-TURN
           PERFORM UNTIL
               WS-TURN >= WS-MAX-TURNS
               OR WS-FLAG-FOUND = "Y"
               OR WS-SESSION-ERR = "Y"

               ADD 1 TO WS-TURN
               DISPLAY " "
               DISPLAY "--- Turn "
                   WS-TURN " of "
                   WS-MAX-TURNS " ---"

      *>       Call LLM for response
               PERFORM CALL-LLM

               IF WS-OUR-TEXT = SPACES
                   DISPLAY "  LLM empty!"
                   MOVE "Y"
                       TO WS-SESSION-ERR
                   EXIT PERFORM
               END-IF

      *>       Strip markdown
               PERFORM STRIP-MARKDOWN

               DISPLAY "  [US] "
                   TRIM(WS-CLEAN-TXT)
                   (1:200)

      *>       Add to conversation
               MOVE WS-CLEAN-TXT
                   TO WS-OUR-TEXT
               PERFORM APPEND-ASST-MSG

      *>       Convert to speech
               PERFORM TEXT-TO-SPEECH

      *>       Send audio to hub
               PERFORM SEND-AUDIO-TO-HUB

      *>       Read hub response
               MOVE "hub_resp.json"
                   TO WS-WORK-PATH
               PERFORM READ-JSON-FILE
               MOVE "work.tmp"
                   TO WS-WORK-PATH

               IF WS-JLEN = 0
                   DISPLAY "  Empty hub resp!"
                   MOVE "Y"
                       TO WS-SESSION-ERR
                   EXIT PERFORM
               END-IF

               DISPLAY "  [HUB] "
                   WS-JBUF(1:500)

      *>       Check for flag in message
               MOVE "message"
                   TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               MOVE 0 TO WS-TALLY-CNT
               INSPECT WS-JVAL
                   TALLYING WS-TALLY-CNT
                   FOR ALL "{FLG:"
               IF WS-TALLY-CNT > 0
                   MOVE "Y"
                       TO WS-FLAG-FOUND
                   DISPLAY "  >>> FLAG <<<"
                   DISPLAY "  "
                       TRIM(WS-JVAL)
                   EXIT PERFORM
               END-IF

      *>       Check for error code
               MOVE "code"
                   TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               MOVE WS-JVAL
                   TO WS-RESP-CODE
               IF WS-RESP-CODE
                   NOT = SPACES
                   MOVE 0 TO WS-TALLY-CNT
                   INSPECT WS-RESP-CODE
                       TALLYING WS-TALLY-CNT
                       FOR ALL "-"
                   IF WS-TALLY-CNT > 0
                       DISPLAY
                           "  Session error!"
                       MOVE "Y"
                           TO WS-SESSION-ERR
                       EXIT PERFORM
                   END-IF
               END-IF

      *>       Extract operator response
               MOVE 0 TO WS-TALLY-CNT
               INSPECT WS-JBUF(1:WS-JLEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL '"audio"'

               IF WS-TALLY-CNT > 0
                   PERFORM
                       EXTRACT-OPERATOR-AUDIO
                   IF WS-B64-LEN > 0
                       PERFORM
                           TRANSCRIBE-AUDIO
                       DISPLAY
                           "  [OPERATOR] "
                           TRIM(
                           WS-OPERATOR-TXT)
                           (1:200)
                       PERFORM
                           APPEND-USER-MSG
                   ELSE
      *>               Try text message
                       MOVE "message"
                           TO WS-KEY-SEARCH
                       MOVE 1 TO WS-JPOS
                       PERFORM FIND-JSON-VAL
                       IF WS-JVAL
                           NOT = SPACES
                           MOVE WS-JVAL TO
                             WS-OPERATOR-TXT
                           DISPLAY
                             "  [OPERATOR] "
                             TRIM(
                             WS-OPERATOR-TXT
                             )(1:200)
                           PERFORM
                             APPEND-USER-MSG
                       ELSE
                           DISPLAY
                             "  No operator"
                             " response"
                       END-IF
                   END-IF
               ELSE
      *>           Check text message
                   MOVE "message"
                       TO WS-KEY-SEARCH
                   MOVE 1 TO WS-JPOS
                   PERFORM FIND-JSON-VAL
                   IF WS-JVAL NOT = SPACES
                       MOVE WS-JVAL
                           TO WS-OPERATOR-TXT
                       DISPLAY
                           "  [OPERATOR] "
                           TRIM(
                           WS-OPERATOR-TXT)
                           (1:200)
                       PERFORM
                           APPEND-USER-MSG
                   ELSE
                       DISPLAY
                           "  No operator"
                           " response"
                   END-IF
               END-IF

           END-PERFORM

           IF WS-TURN >= WS-MAX-TURNS
           AND WS-FLAG-FOUND NOT = "Y"
               DISPLAY
                   "  Max turns reached!"
           END-IF
           .

      *> ============================================================
      *> INIT-CONVERSATION
      *> Build system prompt in WS-CONV-BUF
      *> ============================================================
       INIT-CONVERSATION.
           MOVE SPACES TO WS-CONV-BUF
           MOVE 1 TO WS-CONV-PTR

           STRING
               "[{" WS-QT "role" WS-QT ":"
               WS-QT "system" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "Jestes Tymonem "
               "Gajewskim. Dzwonisz "
               "do operatora systemu."
               WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "STRATEGIA ROZMOWY:"
               WS-NL WS-NL
               "KROK 1 (powitanie):"
               WS-NL
               "Powiedz: Dzien dobry,"
               " tu Tymon Gajewski."
               WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "KROK 2 (gdy operator "
               "zapyta w jakiej "
               "sprawie):" WS-NL
               "Powiedz naturalnie, "
               "ze organizujesz tajny"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               " transport do bazy "
               "Zygfryda i "
               "potrzebujesz znalezc"
               " przejezdna droge. "
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "Zapytaj o status "
               "drog RD224, RD472 "
               "i RD820. Jedno "
               "naturalne zdanie."
               WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "KROK 3 (po otrzymaniu"
               " statusu drog):"
               WS-NL
               "Podaj haslo, "
               "uzasadnij tajna "
               "operacje zlecona "
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "przez Zygfryda i "
               "popros o wylaczenie "
               "monitoringu OKO na "
               "przejezdnej drodze. "
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "Wszystko w jednym "
               "zdaniu." WS-NL
               "Przyklad: Haslo "
               "BARBAKAN. Zygfryd "
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "zlecil tajna operacje"
               " na drodze RD-820, "
               "potrzebuje wylaczenia"
               " monitoringu OKO."
               WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "KROK 4 (gdy operator "
               "pyta o powod lub "
               "dlaczego wylaczyc "
               "monitoring):" WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "To transport "
               "zywnosci do tajnej "
               "bazy, nie moze byc "
               "odnotowany w logach."
               WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "KROK 5 (inne pytania "
               "operatora):" WS-NL
               "Odpowiadaj krotko i "
               "naturalnie, jedno "
               "zdanie." WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "DANE:" WS-NL
               "- Tozsamosc: Tymon "
               "Gajewski" WS-NL
               "- Drogi: RD224, "
               "RD472, RD820" WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "- Haslo: BARBAKAN"
               WS-NL
               "- System monitoringu:"
               " OKO" WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "ZASADY:" WS-NL
               "- Mow naturalnie po "
               "polsku, jak normalny "
               "czlowiek przez "
               "telefon" WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "- Kazda wypowiedz to "
               "jedno, maksymalnie "
               "dwa krotkie zdania"
               WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "- W KROKU 3 ZAWSZE "
               "podaj haslo, powolaj "
               "sie na tajna operacje"
               " Zygfryda i popros o"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               " wylaczenie OKO"
               WS-NL
               "- NIE uzywaj "
               "formatowania "
               "(gwiazdki, "
               "myslniki)" WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "- Nie uzywaj slowa "
               "kod - zawsze haslo"
               WS-QT "}]"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING
           .

      *> ============================================================
      *> START-SESSION
      *> POST start action to hub
      *> ============================================================
       START-SESSION.
           DISPLAY "  Starting session..."

           MOVE SPACES TO WS-HUB-BODY
           MOVE 1 TO WS-PTR
           STRING
               "{"
               WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT
               ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME) WS-QT
               ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT "start" WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

      *>   Write request body
           OPEN OUTPUT WORK-FILE
           WRITE WORK-REC FROM WS-HUB-BODY
           CLOSE WORK-FILE

      *>   Send to hub
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
               " -d @work.tmp"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           DISPLAY "  Hub cmd: "
               TRIM(WS-CMD)(1:200)
           CALL "SYSTEM" USING WS-CMD

      *>   Read response into JBUF
           MOVE "hub_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp"
               TO WS-WORK-PATH

           DISPLAY "  Hub start resp: "
               WS-JBUF(1:500)
           .

      *> ============================================================
      *> EXTRACT-OPERATOR-AUDIO
      *> Find "audio" key in JBUF, extract b64
      *> value to b64_input.tmp
      *> Don't use FIND-JSON-VAL (value too big)
      *> ============================================================
       EXTRACT-OPERATOR-AUDIO.
           MOVE 0 TO WS-B64-LEN

      *>   Find "audio" key
           MOVE SPACES TO WS-TMP2
           STRING WS-QT "audio" WS-QT
               DELIMITED SIZE INTO WS-TMP2
           END-STRING

           MOVE 0 TO WS-KEY-POS
           MOVE LENGTH(TRIM(WS-TMP2))
               TO WS-K
           PERFORM VARYING WS-FJV-POS
               FROM 1 BY 1
               UNTIL WS-FJV-POS > WS-JLEN
               OR WS-KEY-POS > 0
               IF WS-FJV-POS + WS-K - 1
                   <= WS-JLEN
               AND WS-JBUF(
                   WS-FJV-POS:WS-K)
                   = TRIM(WS-TMP2)
                   MOVE WS-FJV-POS
                       TO WS-KEY-POS
               END-IF
           END-PERFORM

           IF WS-KEY-POS = 0
               DISPLAY "    No audio key!"
               EXIT PARAGRAPH
           END-IF

      *>   Skip past key to colon
           COMPUTE WS-FJV-POS =
               WS-KEY-POS + WS-K
           PERFORM UNTIL
               WS-FJV-POS > WS-JLEN
               OR WS-JBUF(
               WS-FJV-POS:1) = ":"
               ADD 1 TO WS-FJV-POS
           END-PERFORM
           ADD 1 TO WS-FJV-POS

      *>   Skip whitespace
           PERFORM UNTIL
               WS-FJV-POS > WS-JLEN
               OR WS-JBUF(
               WS-FJV-POS:1)
               NOT = " "
               ADD 1 TO WS-FJV-POS
           END-PERFORM

      *>   Check for null value
           IF WS-FJV-POS + 3 <= WS-JLEN
           AND WS-JBUF(
               WS-FJV-POS:4) = "null"
               DISPLAY "    Audio is null"
               EXIT PARAGRAPH
           END-IF

      *>   Skip opening quote
           IF WS-JBUF(
               WS-FJV-POS:1) = WS-QT
               ADD 1 TO WS-FJV-POS
           ELSE
               DISPLAY "    No audio quote!"
               EXIT PARAGRAPH
           END-IF

      *>   Start of b64 data
           MOVE WS-FJV-POS
               TO WS-B64-VAL-START

      *>   Find closing quote
           PERFORM UNTIL
               WS-FJV-POS > WS-JLEN
               OR WS-JBUF(
               WS-FJV-POS:1) = WS-QT
               ADD 1 TO WS-FJV-POS
           END-PERFORM
           COMPUTE WS-B64-VAL-END =
               WS-FJV-POS - 1

           COMPUTE WS-B64-LEN =
               WS-B64-VAL-END
               - WS-B64-VAL-START + 1
           DISPLAY "    Audio b64 len: "
               WS-B64-LEN

      *>   Write b64 to file in chunks
           IF WS-B64-LEN > 0
           AND WS-B64-LEN <= 900000
               OPEN OUTPUT B64-FILE
               IF WS-FS2 NOT = "00"
                   DISPLAY
                       "    B64 open err: "
                       WS-FS2
                   MOVE 0 TO WS-B64-LEN
                   EXIT PARAGRAPH
               END-IF

               MOVE WS-B64-VAL-START
                   TO WS-CHK-POS
               MOVE WS-B64-LEN
                   TO WS-CHK-REM

               PERFORM UNTIL
                   WS-CHK-REM = 0
                   IF WS-CHK-REM > 4000
                       MOVE 4000
                           TO WS-CHK-LEN
                   ELSE
                       MOVE WS-CHK-REM
                           TO WS-CHK-LEN
                   END-IF
                   MOVE SPACES TO B64-REC
                   MOVE WS-JBUF(
                       WS-CHK-POS:
                       WS-CHK-LEN)
                       TO B64-REC(
                       1:WS-CHK-LEN)
                   WRITE B64-REC
                   ADD WS-CHK-LEN
                       TO WS-CHK-POS
                   SUBTRACT WS-CHK-LEN
                       FROM WS-CHK-REM
               END-PERFORM

               CLOSE B64-FILE
           END-IF
           .

      *> ============================================================
      *> TRANSCRIBE-AUDIO
      *> Decode b64 -> MP3, call Whisper, parse
      *> Sets WS-OPERATOR-TXT
      *> ============================================================
       TRANSCRIBE-AUDIO.
           MOVE SPACES TO WS-OPERATOR-TXT

      *>   Decode b64 to MP3 (strip CRLF + \)
      *>   Write decode script to file
           MOVE "decode.sh" TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           MOVE SPACES TO WORK-REC
           STRING
               "cat "
               TRIM(WS-B64-PATH)
               " | tr -d '"
               WS-BS "n" WS-BS "r"
               WS-BS WS-BS
               "' | base64 -d > "
               "audio_in.mp3"
               DELIMITED SIZE
               INTO WORK-REC
           END-STRING
           WRITE WORK-REC
           CLOSE WORK-FILE
           MOVE "work.tmp" TO WS-WORK-PATH
      *>   Run decode script
           INITIALIZE WS-CMD
           STRING
               "bash decode.sh"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           DISPLAY "    Decode cmd: "
               TRIM(WS-CMD)
           CALL "SYSTEM" USING WS-CMD

      *>   Call Whisper API
           INITIALIZE WS-CMD
           STRING
               "rm -f whisper_resp.json"
               DELIMITED SIZE INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           INITIALIZE WS-CMD
           STRING
               "curl -s "
               "-o whisper_resp.json"
               " -X POST "
               TRIM(WS-WHISPER-URL)
               " -H " WS-QT
               "Authorization: "
               "Bearer "
               TRIM(WS-OPENAI-KEY)
               WS-QT
               " -F " WS-QT
               "model=whisper-1"
               WS-QT
               " -F " WS-QT
               "language=pl"
               WS-QT
               " -F " WS-QT
               "file=@audio_in.mp3"
               WS-QT
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Parse whisper response
           MOVE "whisper_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp"
               TO WS-WORK-PATH

           DISPLAY "    Whisper resp: "
               WS-JBUF(1:300)

           MOVE "text" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE WS-JVAL TO WS-OPERATOR-TXT

           DISPLAY "    Transcribed: "
               TRIM(WS-OPERATOR-TXT)
               (1:200)
           .

      *> ============================================================
      *> CALL-LLM
      *> Build request with conversation history,
      *> call gpt-4.1-mini, parse content response
      *> Sets WS-OUR-TEXT
      *> ============================================================
       CALL-LLM.
           MOVE SPACES TO WS-OUR-TEXT

      *>   Build request JSON
           MOVE SPACES TO WS-REQ-JSON
           MOVE 1 TO WS-PTR

           STRING
               "{"
               WS-QT "model" WS-QT ":"
               WS-QT "gpt-4.1-mini" WS-QT
               ","
               WS-QT "temperature" WS-QT
               ":0.2,"
               WS-QT "max_tokens" WS-QT
               ":200,"
               WS-QT "messages" WS-QT ":"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Copy conversation buffer
           COMPUTE WS-K = WS-CONV-PTR - 1
           IF WS-K > 0
               MOVE WS-CONV-BUF(1:WS-K)
                   TO WS-REQ-JSON(
                   WS-PTR:WS-K)
               ADD WS-K TO WS-PTR
           END-IF

           STRING "}"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Write request to file
           MOVE "llm_req.json"
               TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "    LLM req open err"
               EXIT PARAGRAPH
           END-IF
           WRITE WORK-REC FROM WS-REQ-JSON
           CLOSE WORK-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

      *>   Send to LLM
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

      *>   Parse response
           MOVE "llm_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp"
               TO WS-WORK-PATH

           IF WS-JLEN = 0
               DISPLAY "    Empty LLM resp!"
               EXIT PARAGRAPH
           END-IF

           DISPLAY "    LLM resp: "
               WS-JBUF(1:500)

      *>   Find "content" after "message"
      *>   to avoid metadata content fields
           MOVE "message"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL

      *>   Now search for content from here
           MOVE "content"
               TO WS-KEY-SEARCH
           PERFORM FIND-JSON-VAL

           IF WS-JVAL NOT = SPACES
      *>       Unescape JSON string
               MOVE WS-JVAL TO WS-ESC-IN
               PERFORM JSON-UNESCAPE-STR
               MOVE WS-ESC-OUT
                   TO WS-OUR-TEXT
           END-IF

           DISPLAY "    LLM text: "
               TRIM(WS-OUR-TEXT)(1:200)
           .

      *> ============================================================
      *> STRIP-MARKDOWN
      *> Remove formatting from WS-OUR-TEXT
      *> Result in WS-CLEAN-TXT
      *> ============================================================
       STRIP-MARKDOWN.
           MOVE SPACES TO WS-CLEAN-TXT
           MOVE 0 TO WS-CLEAN-LEN
           MOVE LENGTH(TRIM(WS-OUR-TEXT))
               TO WS-K

           IF WS-K = 0
               EXIT PARAGRAPH
           END-IF

           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > WS-K

      *>       Check ** (double asterisk)
               IF WS-I + 1 <= WS-K
               AND WS-OUR-TEXT(WS-I:2)
                   = "**"
                   ADD 2 TO WS-I
               ELSE
      *>       Check __ (double underscore)
               IF WS-I + 1 <= WS-K
               AND WS-OUR-TEXT(WS-I:2)
                   = "__"
                   ADD 2 TO WS-I
               ELSE
      *>       Check ~~ (strikethrough)
               IF WS-I + 1 <= WS-K
               AND WS-OUR-TEXT(WS-I:2)
                   = "~~"
                   ADD 2 TO WS-I
               ELSE
      *>       Check single * _ ` #
               IF WS-OUR-TEXT(WS-I:1)
                   = "*"
               OR WS-OUR-TEXT(WS-I:1)
                   = "_"
               OR WS-OUR-TEXT(WS-I:1)
                   = "`"
               OR WS-OUR-TEXT(WS-I:1)
                   = "#"
                   ADD 1 TO WS-I
               ELSE
      *>           Keep character
                   ADD 1 TO WS-CLEAN-LEN
                   MOVE WS-OUR-TEXT(
                       WS-I:1)
                       TO WS-CLEAN-TXT(
                       WS-CLEAN-LEN:1)
                   ADD 1 TO WS-I
               END-IF
               END-IF
               END-IF
               END-IF
           END-PERFORM

      *>   Strip leading/trailing quotes
           IF WS-CLEAN-LEN > 2
           AND WS-CLEAN-TXT(1:1) = WS-QT
           AND WS-CLEAN-TXT(
               WS-CLEAN-LEN:1) = WS-QT
               MOVE WS-CLEAN-TXT(
                   2:WS-CLEAN-LEN - 2)
                   TO WS-CLEAN-TXT
               SUBTRACT 2
                   FROM WS-CLEAN-LEN
           END-IF
           .

      *> ============================================================
      *> TEXT-TO-SPEECH
      *> Call OpenAI TTS API, encode to base64
      *> Output: tts_b64.tmp
      *> ============================================================
       TEXT-TO-SPEECH.
           DISPLAY "    TTS generating..."

      *>   Escape text for JSON
           MOVE SPACES TO WS-ESC-IN
           MOVE WS-CLEAN-TXT
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

      *>   Build TTS request JSON
           MOVE SPACES TO WS-REQ-JSON
           MOVE 1 TO WS-PTR

           STRING
               "{"
               WS-QT "model" WS-QT ":"
               WS-QT "gpt-4o-mini-tts"
               WS-QT ","
               WS-QT "voice" WS-QT ":"
               WS-QT "coral" WS-QT ","
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "input" WS-QT ":"
               WS-QT
               TRIM(WS-ESC-OUT)
               WS-QT ","
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "instructions" WS-QT
               ":" WS-QT
               "Mow naturalnie po "
               "polsku, jak zwykly "
               "facet przez telefon."
               " Krotko i na temat."
               WS-QT ","
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "response_format"
               WS-QT ":"
               WS-QT "mp3" WS-QT "}"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Write TTS request to file
           MOVE "tts_req.json"
               TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY
                   "    TTS req open err"
               EXIT PARAGRAPH
           END-IF
           WRITE WORK-REC FROM WS-REQ-JSON
           CLOSE WORK-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

      *>   Call TTS API - output is raw MP3
           INITIALIZE WS-CMD
           STRING
               "rm -f tts_resp.mp3"
               DELIMITED SIZE INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           INITIALIZE WS-CMD
           STRING
               "curl -s "
               "-o tts_resp.mp3"
               " -X POST "
               TRIM(WS-TTS-URL)
               " -H " WS-QT
               "Content-Type: "
               "application/json"
               WS-QT
               " -H " WS-QT
               "Authorization: "
               "Bearer "
               TRIM(WS-OPENAI-KEY)
               WS-QT
               " -d @tts_req.json"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Encode MP3 to base64
           INITIALIZE WS-CMD
           STRING
               "base64 -w 0"
               " tts_resp.mp3"
               " > tts_b64.tmp"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           DISPLAY "    TTS done."
           .

      *> ============================================================
      *> SEND-AUDIO-TO-HUB
      *> Build hub request via shell concat
      *> (b64 too large for COBOL var)
      *> ============================================================
       SEND-AUDIO-TO-HUB.
           DISPLAY "    Sending audio..."

      *>   Write JSON prefix to file
           MOVE SPACES TO WS-HUB-BODY
           MOVE 1 TO WS-PTR
           STRING
               "{"
               WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY) WS-QT
               ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME) WS-QT
               ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "audio" WS-QT ":"
               WS-QT
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           MOVE "hub_prefix.tmp"
               TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY
                   "    Prefix open err"
               EXIT PARAGRAPH
           END-IF
           WRITE WORK-REC FROM WS-HUB-BODY
           CLOSE WORK-FILE

      *>   Write JSON suffix to file
           MOVE SPACES TO WS-HUB-BODY
           MOVE 1 TO WS-PTR
           STRING
               WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           MOVE "hub_suffix.tmp"
               TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           WRITE WORK-REC FROM WS-HUB-BODY
           CLOSE WORK-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

      *>   Concatenate: prefix + b64 + suffix
           INITIALIZE WS-CMD
           STRING
               "bash -c " WS-QT
               "tr -d "
               WS-BS WS-QT
               WS-BS "n" WS-BS "r"
               WS-BS WS-QT
               " < hub_prefix.tmp"
               " > hub_req.tmp"
               " && cat tts_b64.tmp"
               " >> hub_req.tmp"
               " && tr -d "
               WS-BS WS-QT
               WS-BS "n" WS-BS "r"
               WS-BS WS-QT
               " < hub_suffix.tmp"
               " >> hub_req.tmp"
               WS-QT
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Send to hub
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

           DISPLAY "    Audio sent."
           .

      *> ============================================================
      *> APPEND-USER-MSG
      *> Append operator text as user message
      *> to WS-CONV-BUF
      *> Uses WS-OPERATOR-TXT
      *> ============================================================
       APPEND-USER-MSG.
      *>   Escape operator text for JSON
           MOVE SPACES TO WS-ESC-IN
           MOVE WS-OPERATOR-TXT
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

      *>   Back up over closing "]"
           SUBTRACT 1 FROM WS-CONV-PTR

      *>   Append comma + user message + "]"
           STRING ","
               "{" WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING
           STRING
               TRIM(WS-ESC-OUT)
               WS-QT "}]"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING
           .

      *> ============================================================
      *> APPEND-ASST-MSG
      *> Append our text as assistant message
      *> to WS-CONV-BUF
      *> Uses WS-OUR-TEXT
      *> ============================================================
       APPEND-ASST-MSG.
      *>   Escape our text for JSON
           MOVE SPACES TO WS-ESC-IN
           MOVE WS-OUR-TEXT TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

      *>   Back up over closing "]"
           SUBTRACT 1 FROM WS-CONV-PTR

      *>   Append comma + asst message + "]"
           STRING ","
               "{" WS-QT "role" WS-QT ":"
               WS-QT "assistant" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING
           STRING
               TRIM(WS-ESC-OUT)
               WS-QT "}]"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING
           .

      *> ============================================================
      *> CHECK-FLAG-IN-JBUF
      *> INSPECT for "FLG" pattern in JBUF
      *> ============================================================
       CHECK-FLAG-IN-JBUF.
           MOVE "message"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-JVAL
               TALLYING WS-TALLY-CNT
               FOR ALL "{FLG:"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-FLAG-FOUND
               DISPLAY "  "
                   TRIM(WS-JVAL)
           END-IF
           .

      *> ============================================================
      *> Copybook procedures
      *> ============================================================
       COPY ENVLOAD-PROC.
       COPY JSONPARSE-PROC.
       COPY JSONESCAPE-PROC.
       COPY JSONUNESCAPE-PROC.

      *> ============================================================
      *> READ-JSON-FILE (inline, large buffer)
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
                           TRIM(WS-LINE
                           TRAILING))
                           TO WS-K
                       IF WS-K > 0
                           IF WS-JLEN > 0
                               ADD 1
                                 TO WS-JLEN
                               MOVE " "
                                 TO WS-JBUF(
                                 WS-JLEN:1)
                           END-IF
                           IF WS-K > 900000
                               MOVE 900000
                                   TO WS-K
                           END-IF
                           IF WS-JLEN + WS-K
                               <= 900000
                               MOVE WS-LINE(
                                   1:WS-K)
                                   TO WS-JBUF(
                                   WS-JLEN
                                   + 1:WS-K)
                               ADD WS-K
                                   TO WS-JLEN
                           END-IF
                       END-IF
               END-READ
           END-PERFORM

           CLOSE WORK-FILE
           MOVE "N" TO WS-EOF
           .
