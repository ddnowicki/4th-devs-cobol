       IDENTIFICATION DIVISION.
       PROGRAM-ID. S05E01-RADIOMONITOR.
      *> ============================================================
      *> S05E01 - Radio Monitoring
      *> Agent loop with LLM function calling:
      *> 1. call_start  - start radio session
      *> 2. call_listen - listen for signals (auto-process
      *>    attachments: vision for images, whisper for
      *>    audio, base64 decode for text/json)
      *> 3. call_transmit - send final report
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
      *> -- Config --
       01  WS-HUB-KEY              PIC X(100).
       01  WS-OPENAI-KEY           PIC X(200).
       01  WS-QT                   PIC X(1) VALUE '"'.
       01  WS-BS                   PIC X(1)
                                   VALUE X"5C".
       01  WS-FS                   PIC XX.
       01  WS-FS2                  PIC XX.
       01  WS-WORK-PATH            PIC X(100)
                                   VALUE "work.tmp".
       01  WS-B64-PATH             PIC X(100)
                                   VALUE
                                   "b64_data.tmp".

      *> -- URLs --
       01  WS-HUB-URL              PIC X(200).
       01  WS-OPENAI-URL           PIC X(200).
       01  WS-VERIFY-URL           PIC X(200).
       01  WS-WHISPER-URL          PIC X(200).

      *> -- JSON newline: backslash + n --
       01  WS-NL                   PIC X(2).

      *> -- STRING pointer --
       01  WS-PTR                  PIC 9(6).

      *> -- System command --
       01  WS-CMD                  PIC X(4000).

      *> -- Request JSON buffer --
       01  WS-REQ-JSON             PIC X(900000).

      *> -- Hub API request body --
       01  WS-HUB-BODY             PIC X(8000).

      *> -- JSON buffer for parsing --
       01  WS-JBUF                 PIC X(900000).
       01  WS-JLEN                 PIC 9(6).
       01  WS-JPOS                 PIC 9(6).
       01  WS-JVAL                 PIC X(4000).

      *> -- JSON parsing temps --
       01  WS-KEY-SEARCH           PIC X(50).
       01  WS-KEY-POS              PIC 9(6).
       01  WS-VAL-START            PIC 9(6).
       01  WS-VAL-END              PIC 9(6).
       01  WS-FJV-POS              PIC 9(6).
       01  WS-TMP                  PIC X(4000).
       01  WS-TMP2                 PIC X(500).

      *> -- Task constants --
       01  WS-TASK-NAME            PIC X(20)
                                   VALUE
                                   "radiomonitoring".

      *> -- JSON escape I/O --
       01  WS-ESC-IN               PIC X(16000).
       01  WS-ESC-OUT              PIC X(32000).
       01  WS-ESC-ILEN             PIC 9(6).
       01  WS-ESC-OLEN             PIC 9(6).
       01  WS-ESC-I                PIC 9(6).

      *> -- Agent conversation buffer --
       01  WS-CONV-BUF             PIC X(200000).
       01  WS-CONV-PTR             PIC 9(6).

      *> -- Agent loop --
       01  WS-AG-STEP              PIC 9(3)
                                   VALUE 0.
       01  WS-AG-DONE              PIC X
                                   VALUE "N".

      *> -- Tool call parsing --
       01  WS-TOOL-NAME            PIC X(50).
       01  WS-TOOL-CALL-ID         PIC X(100).
       01  WS-TOOL-ARGS            PIC X(4000).
       01  WS-TOOL-RESULT          PIC X(16000).
       01  WS-TOOL-RESULT-LEN      PIC 9(6).

      *> -- Tool arg values --
       01  WS-TA-CITY              PIC X(200).
       01  WS-TA-AREA              PIC X(50).
       01  WS-TA-WAREHOUSES        PIC X(50).
       01  WS-TA-PHONE             PIC X(50).

      *> -- Listen response fields --
       01  WS-RESP-CODE            PIC X(10).
       01  WS-RESP-MSG             PIC X(2000).
       01  WS-RESP-TRANS           PIC X(4000).
       01  WS-RESP-META            PIC X(100).
       01  WS-RESP-FSIZE           PIC X(20).

      *> -- Attachment processing --
       01  WS-AUDIO-EXT            PIC X(10).
       01  WS-VISION-DESC          PIC X(4000).
       01  WS-WHISPER-TXT          PIC X(4000).
       01  WS-DECODED-TXT          PIC X(8000).

      *> -- Loop/misc vars --
       01  WS-I                    PIC 9(6).
       01  WS-K                    PIC 9(6).
       01  WS-EOF                  PIC X VALUE "N".
       01  WS-LINE                 PIC X(900000).
       01  WS-TALLY-CNT            PIC 9(4).

      *> -- Success/flag tracking --
       01  WS-FLAG-FOUND           PIC X VALUE "N".

      *> -- Nudge counter --
       01  WS-NUDGE-CT             PIC 9(1)
                                   VALUE 0.

      *> -- Retry counter --
       01  WS-RETRY-CT             PIC 9(2).

      *> -- Transmit fail counter --
       01  WS-XMIT-FAIL            PIC 9(1)
                                   VALUE 0.

      *> -- Session retry counter --
       01  WS-SESSION-RETRY         PIC 9(1)
                                   VALUE 0.
       01  WS-SESSION-MAX           PIC 9(1)
                                   VALUE 5.

      *> -- JBUF save/restore --
       01  WS-JBUF-SAVE            PIC X(900000).
       01  WS-JLEN-SAVE            PIC 9(6).

      *> -- Has attachment flag --
       01  WS-HAS-ATTACH           PIC X.
      *> -- Has transcription flag --
       01  WS-HAS-TRANS            PIC X.
      *> -- Session ended flag --
       01  WS-SESSION-END          PIC X.

      *> -- B64 extraction state --
       01  WS-B64-STARTED          PIC X.
       01  WS-B64-LINE             PIC X(900000).
       01  WS-B64-LLEN             PIC 9(6).

      *> -- Chunk write vars --
       01  WS-CHK-POS              PIC 9(6).
       01  WS-CHK-REM              PIC 9(6).
       01  WS-CHK-LEN              PIC 9(6).

      *> -- Vision request file --
       01  WS-VIS-PATH             PIC X(100)
                                   VALUE
                                   "vis_req.json".

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S05E01 RADIOMONITOR ==="

           ACCEPT WS-HUB-KEY
               FROM ENVIRONMENT "HUB_API_KEY"
           ACCEPT WS-OPENAI-KEY
               FROM ENVIRONMENT "OPENAI_API_KEY"
           ACCEPT WS-HUB-URL
               FROM ENVIRONMENT "HUB_API_URL"
           ACCEPT WS-OPENAI-URL
               FROM ENVIRONMENT "OPENAI_API_URL"

           IF WS-HUB-KEY = SPACES
               DISPLAY "ERR: HUB_API_KEY!"
               STOP RUN
           END-IF
           IF WS-OPENAI-KEY = SPACES
               DISPLAY "ERR: OPENAI_API_KEY!"
               STOP RUN
           END-IF
           IF WS-HUB-URL = SPACES
               DISPLAY "ERR: HUB_API_URL!"
               STOP RUN
           END-IF
           IF WS-OPENAI-URL = SPACES
               DISPLAY "ERR: OPENAI_API_URL!"
               STOP RUN
           END-IF

           MOVE SPACES TO WS-VERIFY-URL
           STRING TRIM(WS-HUB-URL)
               "/verify"
               DELIMITED SIZE
               INTO WS-VERIFY-URL
           END-STRING

      *>   Build whisper URL from OpenAI URL
      *>   Replace chat/completions with
      *>   audio/transcriptions
           PERFORM BUILD-WHISPER-URL

      *>   Init JSON newline
           MOVE X"5C" TO WS-NL(1:1)
           MOVE "n"    TO WS-NL(2:1)

      *>   Session retry loop
           MOVE 0 TO WS-SESSION-RETRY
           MOVE "N" TO WS-FLAG-FOUND

           PERFORM UNTIL
               WS-FLAG-FOUND = "Y"
               OR WS-SESSION-RETRY
               >= WS-SESSION-MAX

               ADD 1 TO WS-SESSION-RETRY
               DISPLAY " "
               DISPLAY "=== SESSION "
                   WS-SESSION-RETRY
                   " of " WS-SESSION-MAX
                   " ==="

      *>       Clean temp files before each
               INITIALIZE WS-CMD
               STRING
                   "rm -f agent_resp.json "
                   "hub_resp.json "
                   "whisper_resp.json "
                   "vis_resp.json "
                   "vis_req.json "
                   "agent_req.json "
                   "hub_req.tmp "
                   "b64_data.tmp "
                   "decoded_text.tmp "
                   "audio_decoded.* "
                   "cookies.txt "
                   "work.tmp"
                   DELIMITED SIZE
                   INTO WS-CMD
               END-STRING
               CALL "SYSTEM" USING WS-CMD

      *>       Reset state for new session
               MOVE SPACES TO WS-CONV-BUF
               MOVE 1 TO WS-CONV-PTR
               MOVE 0 TO WS-AG-STEP
               MOVE "N" TO WS-AG-DONE
               MOVE 0 TO WS-NUDGE-CT
               MOVE 0 TO WS-XMIT-FAIL
               MOVE SPACES
                   TO WS-TOOL-RESULT
               MOVE 0
                   TO WS-TOOL-RESULT-LEN
               MOVE SPACES
                   TO WS-VISION-DESC
               MOVE SPACES
                   TO WS-WHISPER-TXT
               MOVE SPACES
                   TO WS-DECODED-TXT
               MOVE "N"
                   TO WS-SESSION-END

               DISPLAY " "
               DISPLAY "[1] Agent loop"
               PERFORM RUN-AGENT-LOOP

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
                   WS-SESSION-MAX
                   " sessions."
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

           DISPLAY "  OpenAI URL: "
               TRIM(WS-OPENAI-URL)
           DISPLAY "  URL len: " WS-K

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
      *>       Found - replace
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
      *>       Not found - try to find
      *>       last / and replace after
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

      *>   Sanitize double slashes in URL
      *>   (except the :// in https://)
           MOVE LENGTH(TRIM(WS-WHISPER-URL))
               TO WS-K
           IF WS-K > 8
               MOVE 9 TO WS-I
               PERFORM UNTIL WS-I >= WS-K
                   IF WS-WHISPER-URL(
                       WS-I:1) = "/"
                   AND WS-WHISPER-URL(
                       WS-I + 1:1) = "/"
                       MOVE SPACES
                           TO WS-TMP
                       IF WS-I > 1
                           STRING
                             WS-WHISPER-URL(
                               1:WS-I)
                             WS-WHISPER-URL(
                               WS-I + 2:
                               WS-K
                               - WS-I - 1)
                             DELIMITED SIZE
                             INTO WS-TMP
                           END-STRING
                       END-IF
                       MOVE SPACES
                           TO WS-WHISPER-URL
                       MOVE TRIM(WS-TMP)
                           TO WS-WHISPER-URL
                       MOVE LENGTH(
                           TRIM(
                           WS-WHISPER-URL))
                           TO WS-K
                   ELSE
                       ADD 1 TO WS-I
                   END-IF
               END-PERFORM
           END-IF

           DISPLAY "  Whisper URL: "
               TRIM(WS-WHISPER-URL)
           .

      *> ============================================================
      *> RUN-AGENT-LOOP: LLM function calling loop
      *> ============================================================
       RUN-AGENT-LOOP.
           DISPLAY " "
           DISPLAY "--- Agent starting ---"

           MOVE SPACES TO WS-CONV-BUF
           MOVE 1 TO WS-CONV-PTR

      *>   System message
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
               "You are analyzing "
               "intercepted radio "
               "communications from "
               "rebels. The rebels "
               "refer to a city using "
               "the codename Syjon "
               "(Zion)."
               WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "## Pipeline" WS-NL
               "1. Call call_start to "
               "begin the session."
               WS-NL
               "2. Call call_listen "
               "repeatedly to collect "
               "radio transmissions."
               WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "   Each call returns "
               "processed text (images "
               "and audio already "
               "transcribed for you)."
               WS-NL
               "   Keep calling listen "
               "until SESSION_ENDED."
               WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "3. After ALL signals, "
               "determine:" WS-NL
               "   a) The REAL name of"
               " the city called "
               "Syjon" WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "   b) The area of that"
               " city (round to 2 "
               "decimal places using "
               "math rounding, format "
               "12.34)" WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "   c) The number of "
               "warehouses in Syjon"
               WS-NL
               "   d) The phone number"
               " of the contact person"
               " from Syjon" WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "4. Call call_transmit."
               WS-NL WS-NL
               "## RULES" WS-NL
               "- Listen ALL signals "
               "before analyzing."
               WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "- Syjon is a CODE NAME"
               " for a real Polish "
               "city. Cross-reference "
               "ALL data to find it."
               WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "- For cityArea, use "
               "the occupiedArea value"
               " from the JSON data, "
               "rounded to 2 decimal "
               "places (km2)."
               WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "- The PHONE NUMBER is "
               "on the sticky note "
               "IMAGE (format XXX-XXX"
               "-XXX). It is NOT a "
               "radio callsign."
               WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "- Think step by step."
               WS-NL
               "- Do NOT guess. Use "
               "ONLY values from "
               "intercepted data."
               WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "- Audio/transcription "
               "data is MORE RECENT "
               "than JSON/XML data. "
               "If warehouse count "
               "from audio differs "
               "from JSON, trust "
               "audio."
               WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "- The phone number "
               "appears on a handwrit"
               "ten sticky note image"
               ". Look for it in "
               "image descriptions."
               WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "- cityName must NOT "
               "contain Polish "
               "diacritics (no ą ć ę"
               " ł ń ó ś ź ż). "
               "Use only ASCII."
               WS-NL
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

           STRING
               "- Call one tool at a "
               "time." WS-NL
               "- Start now."
               WS-QT "},"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

      *>   User message
           STRING
               "{" WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT
               "Begin radio monitoring. "
               "Call call_start first."
               WS-QT "}]"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING

      *>   Agent loop
           MOVE "N" TO WS-AG-DONE
           MOVE 0 TO WS-AG-STEP
           MOVE 0 TO WS-NUDGE-CT

           PERFORM UNTIL WS-AG-DONE = "Y"

               ADD 1 TO WS-AG-STEP
               DISPLAY " "
               DISPLAY "  --- Step "
                   WS-AG-STEP " ---"

               IF WS-AG-STEP > 55
                   DISPLAY "  Max steps!"
                   MOVE "Y" TO WS-AG-DONE
                   EXIT PERFORM
               END-IF

               PERFORM SEND-AGENT-REQUEST

               MOVE "agent_resp.json"
                   TO WS-WORK-PATH
               PERFORM READ-JSON-FILE
               MOVE "work.tmp"
                   TO WS-WORK-PATH

               IF WS-JLEN = 0
                   DISPLAY "  Empty resp!"
                   MOVE "Y" TO WS-AG-DONE
                   EXIT PERFORM
               END-IF

      *>       Check API error
               MOVE 0 TO WS-TALLY-CNT
               INSPECT WS-JBUF(1:WS-JLEN)
                   TALLYING WS-TALLY-CNT
                   FOR ALL '"error"'
               IF WS-TALLY-CNT > 0
                   DISPLAY "  API ERR: "
                       WS-JBUF(1:500)
                   EXIT PERFORM CYCLE
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

                   PERFORM DISPATCH-TOOL
                   PERFORM
                       APPEND-TOOL-EXCHANGE

      *>           Check flag in result
                   IF TRIM(WS-TOOL-NAME)
                       = "call_transmit"
                   AND WS-SESSION-END = "Y"
                       MOVE 0
                           TO WS-TALLY-CNT
                       IF WS-TOOL-RESULT-LEN
                           > 0
                           INSPECT
                             WS-TOOL-RESULT(
                             1:
                             WS-TOOL-RESULT-LEN
                             )
                             TALLYING
                             WS-TALLY-CNT
                             FOR ALL "FLG"
                       END-IF
                       IF WS-TALLY-CNT > 0
                           MOVE "Y"
                               TO WS-FLAG-FOUND
                           DISPLAY
                             "  >>> FLAG <<<"
                           MOVE "Y"
                               TO WS-AG-DONE
                       ELSE
      *>                   No flag - count
                           ADD 1
                             TO WS-XMIT-FAIL
                           DISPLAY
                             "  Xmit fail #"
                             WS-XMIT-FAIL
                           IF WS-XMIT-FAIL
                               >= 5
                               DISPLAY
                                 "  Max xmit!"
                               MOVE "Y"
                                 TO WS-AG-DONE
                           ELSE
                             IF WS-CONV-PTR
                               < 185000
                               SUBTRACT 1
                                 FROM
                                 WS-CONV-PTR
                               STRING ","
                                 "{"
                                 WS-QT "role"
                                 WS-QT ":"
                                 WS-QT "user"
                                 WS-QT ","
                                 WS-QT
                                 "content"
                                 WS-QT ":"
                                 WS-QT
                                 "Transmit "
                                 "returned no"
                                 " flag. Read"
                                 " the error."
                                 " Fix your "
                                 "values and "
                                 "try again."
                                 WS-QT "}]"
                                 DELIMITED
                                 SIZE
                                 INTO
                                 WS-CONV-BUF
                                 WITH POINTER
                                 WS-CONV-PTR
                               END-STRING
                             END-IF
                           END-IF
                       END-IF
                   END-IF
               ELSE
      *>           Text response - nudge
                   MOVE "content"
                       TO WS-KEY-SEARCH
                   MOVE 1 TO WS-JPOS
                   PERFORM FIND-JSON-VAL
                   DISPLAY "  Agent: "
                       WS-JVAL(1:200)

      *>           Check flag in text
                   MOVE 0 TO WS-TALLY-CNT
                   INSPECT TRIM(WS-JVAL)
                       TALLYING WS-TALLY-CNT
                       FOR ALL "FLG"
                   IF WS-TALLY-CNT > 0
                       MOVE "Y"
                           TO WS-FLAG-FOUND
                       MOVE "Y"
                           TO WS-AG-DONE
                   ELSE
                       ADD 1 TO WS-NUDGE-CT
                       PERFORM
                         APPEND-TEXT-NUDGE
                   END-IF
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> SEND-AGENT-REQUEST: Build + send to OpenAI
      *> ============================================================
       SEND-AGENT-REQUEST.
           MOVE SPACES TO WS-REQ-JSON
           MOVE 1 TO WS-PTR

           STRING
               "{"
               WS-QT "model" WS-QT ":"
               WS-QT "gpt-4.1-mini" WS-QT ","
               WS-QT "temperature" WS-QT
               ":0,"
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

      *>   Tool 1: call_start
           STRING
               "{" WS-QT "type" WS-QT ":"
               WS-QT "function" WS-QT ","
               WS-QT "function" WS-QT ":{"
               WS-QT "name" WS-QT ":"
               WS-QT "call_start" WS-QT ","
               WS-QT "description" WS-QT ":"
               WS-QT "Start radio session"
               WS-QT ","
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "parameters" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "object" WS-QT ","
               WS-QT "properties" WS-QT
               ":{},"
               WS-QT "required" WS-QT
               ":[]}}},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Tool 2: call_listen
           STRING
               "{" WS-QT "type" WS-QT ":"
               WS-QT "function" WS-QT ","
               WS-QT "function" WS-QT ":{"
               WS-QT "name" WS-QT ":"
               WS-QT "call_listen" WS-QT ","
               WS-QT "description" WS-QT ":"
               WS-QT "Listen for next radio"
               " signal. Returns "
               "processed text."
               WS-QT ","
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "parameters" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "object" WS-QT ","
               WS-QT "properties" WS-QT
               ":{},"
               WS-QT "required" WS-QT
               ":[]}}},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Tool 3: call_transmit
           STRING
               "{" WS-QT "type" WS-QT ":"
               WS-QT "function" WS-QT ","
               WS-QT "function" WS-QT ":{"
               WS-QT "name" WS-QT ":"
               WS-QT "call_transmit" WS-QT ","
               WS-QT "description" WS-QT ":"
               WS-QT "Transmit final report"
               " with city info."
               WS-QT ","
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "parameters" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "object" WS-QT ","
               WS-QT "properties" WS-QT ":{"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "cityName" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "string" WS-QT ","
               WS-QT "description" WS-QT ":"
               WS-QT "Real Polish city name"
               WS-QT "},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "cityArea" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "string" WS-QT ","
               WS-QT "description" WS-QT ":"
               WS-QT "Area in km2, format "
               "XX.XX" WS-QT "},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "warehousesCount"
               WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "integer" WS-QT ","
               WS-QT "description" WS-QT ":"
               WS-QT "Exact number of "
               "warehouses from "
               "intercepted data"
               WS-QT "},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "phoneNumber" WS-QT ":{"
               WS-QT "type" WS-QT ":"
               WS-QT "string" WS-QT ","
               WS-QT "description" WS-QT ":"
               WS-QT "Phone from sticky "
               "note image, XXX-XXX"
               "-XXX format"
               WS-QT "}},"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING
           STRING
               WS-QT "required" WS-QT ":["
               WS-QT "cityName" WS-QT ","
               WS-QT "cityArea" WS-QT ","
               WS-QT "warehousesCount"
               WS-QT ","
               WS-QT "phoneNumber" WS-QT
               "]}}}],"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

           STRING
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
           WRITE WORK-REC FROM WS-REQ-JSON
           CLOSE WORK-FILE

      *>   Delete stale response before curl
           INITIALIZE WS-CMD
           STRING "rm -f agent_resp.json"
               DELIMITED SIZE INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           INITIALIZE WS-CMD
           STRING
               "curl -s "
               "-o agent_resp.json"
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
               " -d @agent_req.json"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD
           .

      *> ============================================================
      *> DISPATCH-TOOL: Execute the called tool
      *> ============================================================
       DISPATCH-TOOL.
           MOVE SPACES TO WS-TOOL-RESULT
           MOVE 0 TO WS-TOOL-RESULT-LEN

           EVALUATE TRIM(WS-TOOL-NAME)
           WHEN "call_start"
               PERFORM TOOL-START
           WHEN "call_listen"
               PERFORM TOOL-LISTEN
           WHEN "call_transmit"
               IF WS-SESSION-END = "N"
                   MOVE SPACES
                       TO WS-TOOL-RESULT
                   STRING
                       "ERROR: Cannot "
                       "transmit yet. "
                       "Session is still "
                       "active. You MUST "
                       "keep calling "
                       "call_listen until "
                       "you receive "
                       "SESSION_ENDED."
                       DELIMITED SIZE
                       INTO WS-TOOL-RESULT
                   END-STRING
                   MOVE LENGTH(
                       TRIM(
                       WS-TOOL-RESULT))
                       TO WS-TOOL-RESULT-LEN
               ELSE
                   PERFORM TOOL-TRANSMIT
               END-IF
           WHEN OTHER
               MOVE
                   '{"error":"Unknown tool"}'
                   TO WS-TOOL-RESULT
               MOVE 24
                   TO WS-TOOL-RESULT-LEN
           END-EVALUATE

      *>   After call_listen, if session not
      *>   ended, append keep-listening hint
           IF TRIM(WS-TOOL-NAME)
               = "call_listen"
           AND WS-SESSION-END = "N"
           AND WS-TOOL-RESULT-LEN > 0
           AND WS-TOOL-RESULT-LEN < 15900
               MOVE WS-TOOL-RESULT-LEN
                   TO WS-PTR
               ADD 1 TO WS-PTR
               STRING
                   " [STATUS: session "
                   "active, more signals"
                   " remain. Call "
                   "call_listen again.]"
                   DELIMITED SIZE
                   INTO WS-TOOL-RESULT
                   WITH POINTER WS-PTR
               END-STRING
               COMPUTE WS-TOOL-RESULT-LEN
                   = WS-PTR - 1
           END-IF
           .

      *> ============================================================
      *> TOOL-START: Start radio session
      *> ============================================================
       TOOL-START.
           MOVE SPACES TO WS-HUB-BODY
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY)
               WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME)
               WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT "start" WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
           END-STRING

           PERFORM SEND-HUB-REQUEST
           PERFORM STORE-TOOL-RESULT
           .

      *> ============================================================
      *> TOOL-LISTEN: Listen for signal + process
      *> ============================================================
       TOOL-LISTEN.
           MOVE SPACES TO WS-HUB-BODY
           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY)
               WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME)
               WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT "listen" WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
           END-STRING

           PERFORM SEND-HUB-REQUEST

      *>   JBUF now has hub response
      *>   Parse code
           MOVE "N" TO WS-SESSION-END
           MOVE "N" TO WS-HAS-TRANS
           MOVE "N" TO WS-HAS-ATTACH

           MOVE "code" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL)
               TO WS-RESP-CODE

           DISPLAY "    Code: "
               TRIM(WS-RESP-CODE)

      *>   Check code != 100
           IF TRIM(WS-RESP-CODE)
               NOT = "100"
               MOVE "Y" TO WS-SESSION-END
           END-IF

      *>   Parse message
           MOVE "message" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE WS-JVAL TO WS-RESP-MSG

           DISPLAY "    Msg: "
               TRIM(WS-RESP-MSG)(1:200)

      *>   Check end keywords in message
           MOVE 0 TO WS-TALLY-CNT
           INSPECT
               LOWER-CASE(
               TRIM(WS-RESP-MSG))
               TALLYING WS-TALLY-CNT
               FOR ALL "enough"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-SESSION-END
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT
               LOWER-CASE(
               TRIM(WS-RESP-MSG))
               TALLYING WS-TALLY-CNT
               FOR ALL "koniec"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-SESSION-END
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT
               LOWER-CASE(
               TRIM(WS-RESP-MSG))
               TALLYING WS-TALLY-CNT
               FOR ALL "no more"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-SESSION-END
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT
               LOWER-CASE(
               TRIM(WS-RESP-MSG))
               TALLYING WS-TALLY-CNT
               FOR ALL "wystarczaj"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-SESSION-END
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT
               LOWER-CASE(
               TRIM(WS-RESP-MSG))
               TALLYING WS-TALLY-CNT
               FOR ALL "all materials"
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-SESSION-END
           END-IF

           IF WS-SESSION-END = "Y"
               DISPLAY "    Session ended"
               MOVE SPACES TO WS-TOOL-RESULT
               STRING
                   "SESSION_ENDED - No "
                   "more signals. Analyze "
                   "collected data now."
                   DELIMITED SIZE
                   INTO WS-TOOL-RESULT
               END-STRING
               MOVE LENGTH(
                   TRIM(WS-TOOL-RESULT))
                   TO WS-TOOL-RESULT-LEN
               EXIT PARAGRAPH
           END-IF

      *>   Check for transcription
           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-JBUF(1:WS-JLEN)
               TALLYING WS-TALLY-CNT
               FOR ALL '"transcription"'
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-HAS-TRANS
           END-IF

      *>   Check for attachment
           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-JBUF(1:WS-JLEN)
               TALLYING WS-TALLY-CNT
               FOR ALL '"attachment"'
           IF WS-TALLY-CNT > 0
               MOVE "Y" TO WS-HAS-ATTACH
           END-IF

           IF WS-HAS-TRANS = "Y"
      *>       Extract transcription text
               MOVE "transcription"
                   TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               MOVE WS-JVAL
                   TO WS-RESP-TRANS

               DISPLAY "    Transcription: "
                   TRIM(WS-RESP-TRANS)
                   (1:150)

      *>       Return transcription as result
               MOVE SPACES TO WS-TOOL-RESULT
               STRING
                   "[TRANSCRIPTION]: "
                   TRIM(WS-RESP-TRANS)
                   DELIMITED SIZE
                   INTO WS-TOOL-RESULT
               END-STRING
               MOVE LENGTH(
                   TRIM(WS-TOOL-RESULT))
                   TO WS-TOOL-RESULT-LEN
               IF WS-TOOL-RESULT-LEN
                   > 16000
                   MOVE 16000
                       TO WS-TOOL-RESULT-LEN
               END-IF
               EXIT PARAGRAPH
           END-IF

           IF WS-HAS-ATTACH = "Y"
      *>       Parse meta (MIME type)
               MOVE "meta" TO WS-KEY-SEARCH
               MOVE 1 TO WS-JPOS
               PERFORM FIND-JSON-VAL
               MOVE TRIM(WS-JVAL)
                   TO WS-RESP-META

      *>       Unescape \/ to / in MIME
      *>       JSON may have audio\/mpeg
               MOVE WS-RESP-META
                   TO WS-ESC-IN
               PERFORM JSON-UNESCAPE-STR
               IF WS-ESC-OLEN > 0
                   MOVE SPACES
                       TO WS-RESP-META
                   MOVE WS-ESC-OUT(
                       1:WS-ESC-OLEN)
                       TO WS-RESP-META
               END-IF

               DISPLAY "    Meta raw: "
                   TRIM(WS-JVAL)(1:50)
               DISPLAY "    Meta: "
                   TRIM(WS-RESP-META)

      *>       Save JBUF before extraction
               DISPLAY "    JLEN: "
                   WS-JLEN
               MOVE WS-JBUF(1:WS-JLEN)
                   TO WS-JBUF-SAVE
               MOVE WS-JLEN TO WS-JLEN-SAVE

      *>       Extract base64 to file
               PERFORM EXTRACT-B64-TO-FILE

      *>       Restore JBUF
               MOVE WS-JBUF-SAVE(
                   1:WS-JLEN-SAVE)
                   TO WS-JBUF
               MOVE WS-JLEN-SAVE
                   TO WS-JLEN

      *>       Process based on MIME type
               MOVE 0 TO WS-TALLY-CNT
               INSPECT TRIM(WS-RESP-META)
                   TALLYING WS-TALLY-CNT
                   FOR ALL "image/"
               IF WS-TALLY-CNT > 0
                   PERFORM PROCESS-IMAGE
                   EXIT PARAGRAPH
               END-IF

               MOVE 0 TO WS-TALLY-CNT
               INSPECT TRIM(WS-RESP-META)
                   TALLYING WS-TALLY-CNT
                   FOR ALL "audio/"
               IF WS-TALLY-CNT > 0
                   PERFORM PROCESS-AUDIO
                   EXIT PARAGRAPH
               END-IF

      *>       Text or JSON - decode b64
               PERFORM PROCESS-TEXT-ATTACH
               EXIT PARAGRAPH
           END-IF

      *>   No transcription, no attachment
           MOVE SPACES TO WS-TOOL-RESULT
           STRING
               "[NOISE]: Empty signal, "
               "no data received."
               DELIMITED SIZE
               INTO WS-TOOL-RESULT
           END-STRING
           MOVE LENGTH(
               TRIM(WS-TOOL-RESULT))
               TO WS-TOOL-RESULT-LEN
           .

      *> ============================================================
      *> EXTRACT-B64-TO-FILE
      *> Extract "attachment" base64 value from
      *> hub_resp.json directly via shell
      *> ============================================================
       EXTRACT-B64-TO-FILE.
      *>   Use shell to extract attachment val
      *>   from JSON - the b64 has no quotes or
      *>   backslashes so we can grep it out
      *>   Actually, use a simple approach:
      *>   read the hub response, find attachment
      *>   field, extract b64 string
      *>
      *>   Simplest approach: use tr/sed to
      *>   extract from JSON file.
      *>   Actually we must use pure COBOL.
      *>   But the b64 data is in hub_resp.json.
      *>
      *>   Strategy: Read hub_resp.json line by
      *>   line. Find the b64 data between
      *>   "attachment":" and the closing ".
      *>   Write just the b64 to b64_data.tmp.
      *>
      *>   Actually the JSON is on a single line
      *>   so we can use the JBUF which already
      *>   has it. But JBUF might be too small.
      *>
      *>   Better: use CALL SYSTEM with a
      *>   portable approach. Since the b64 is
      *>   between "attachment":" and the next ",
      *>   we can use grep/sed. But CLAUDE.md
      *>   says only curl for CALL SYSTEM.
      *>
      *>   Correct approach: read the raw JSON
      *>   file line by line in COBOL and extract
      *>   the attachment value. Since JSON is
      *>   usually one line from curl, we need a
      *>   big buffer. Let's read hub_resp.json
      *>   and write just the b64 part.
      *>
      *>   Since JBUF(64000) already has the
      *>   full response, and b64 data could be
      *>   large, let's find start/end positions
      *>   and write that range to a file.

      *>   Find "attachment" key in JBUF
      *>   Use FIND-JSON-VAL approach: find
      *>   key, skip colon, skip spaces, then
      *>   quote - handles "k":"v" and "k": "v"
           MOVE SPACES TO WS-TMP2
           STRING WS-QT "attachment" WS-QT
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
               DISPLAY "    No attachment!"
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

      *>   Skip whitespace after colon
           PERFORM UNTIL
               WS-FJV-POS > WS-JLEN
               OR WS-JBUF(
               WS-FJV-POS:1)
               NOT = " "
               ADD 1 TO WS-FJV-POS
           END-PERFORM

      *>   Skip opening quote
           IF WS-JBUF(
               WS-FJV-POS:1) = WS-QT
               ADD 1 TO WS-FJV-POS
           END-IF

      *>   Start of b64 data
           MOVE WS-FJV-POS TO WS-VAL-START

      *>   Find closing quote
           MOVE WS-VAL-START TO WS-FJV-POS
           PERFORM UNTIL
               WS-FJV-POS > WS-JLEN
               OR WS-JBUF(
               WS-FJV-POS:1) = WS-QT
               ADD 1 TO WS-FJV-POS
           END-PERFORM
           COMPUTE WS-VAL-END =
               WS-FJV-POS - 1

           COMPUTE WS-K =
               WS-VAL-END - WS-VAL-START + 1
           DISPLAY "    B64 length: " WS-K

      *>   Debug: show first/last chars
           IF WS-K > 20
               DISPLAY "    B64 start: "
                   WS-JBUF(
                   WS-VAL-START:20)
               COMPUTE WS-I =
                   WS-VAL-END - 19
               DISPLAY "    B64 end: "
                   WS-JBUF(WS-I:20)
           END-IF

      *>   Write b64 data to file in chunks
      *>   via B64-FILE. Each chunk is one
      *>   LINE SEQUENTIAL record (newline
      *>   appended). base64 -d ignores
      *>   whitespace so this is safe.
           IF WS-K > 0 AND WS-K <= 900000
               OPEN OUTPUT B64-FILE

               MOVE WS-VAL-START
                   TO WS-CHK-POS
               MOVE WS-K TO WS-CHK-REM

               PERFORM UNTIL
                   WS-CHK-REM = 0
                   IF WS-CHK-REM > 4000
                       MOVE 4000
                           TO WS-CHK-LEN
                   ELSE
                       MOVE WS-CHK-REM
                           TO WS-CHK-LEN
                   END-IF
                   MOVE SPACES
                       TO B64-REC
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
      *> PROCESS-IMAGE: Send to GPT vision
      *> Build entire vision JSON in WORK-REC
      *> (200KB buffer fits header+68K b64+footer)
      *> WS-VAL-START/END set by EXTRACT-B64
      *> ============================================================
       PROCESS-IMAGE.
           DISPLAY "    Processing image..."

           COMPUTE WS-B64-LLEN =
               WS-VAL-END - WS-VAL-START + 1
           IF WS-B64-LLEN < 1
               DISPLAY "    No b64 data!"
               MOVE SPACES TO WS-TOOL-RESULT
               STRING
                   "[IMAGE DESCRIPTION]: "
                   "(no image data)"
                   DELIMITED SIZE
                   INTO WS-TOOL-RESULT
               END-STRING
               MOVE LENGTH(
                   TRIM(WS-TOOL-RESULT))
                   TO WS-TOOL-RESULT-LEN
               EXIT PARAGRAPH
           END-IF

           DISPLAY "    B64 for vision: "
               WS-B64-LLEN

      *>   Build JSON header in WORK-REC
           MOVE SPACES TO WORK-REC
           MOVE 1 TO WS-PTR
           STRING
               "{" WS-QT "model" WS-QT ":"
               WS-QT "gpt-4.1-mini" WS-QT
               "," WS-QT "messages" WS-QT
               ":[{" WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT ":["
               DELIMITED SIZE
               INTO WORK-REC
               WITH POINTER WS-PTR
           END-STRING
           STRING
               "{" WS-QT "type" WS-QT ":"
               WS-QT "text" WS-QT ","
               WS-QT "text" WS-QT ":"
               WS-QT "Describe this "
               "image in detail. "
               "Extract ALL text, "
               "numbers, names, "
               "addresses, phone "
               "numbers visible."
               WS-QT "},"
               DELIMITED SIZE
               INTO WORK-REC
               WITH POINTER WS-PTR
           END-STRING
           STRING
               "{" WS-QT "type" WS-QT ":"
               WS-QT "image_url" WS-QT ","
               WS-QT "image_url" WS-QT ":"
               "{" WS-QT "url" WS-QT ":"
               WS-QT "data:"
               TRIM(WS-RESP-META)
               ";base64,"
               DELIMITED SIZE
               INTO WORK-REC
               WITH POINTER WS-PTR
           END-STRING

      *>   Copy b64 data from JBUF directly
      *>   200K buffer fits 68K b64 easily
           IF WS-PTR + WS-B64-LLEN
               < 900000
               MOVE WS-JBUF(
                   WS-VAL-START:
                   WS-B64-LLEN)
                   TO WORK-REC(
                   WS-PTR:WS-B64-LLEN)
               ADD WS-B64-LLEN TO WS-PTR
           ELSE
               DISPLAY "    B64 too large!"
               DISPLAY "    PTR="
                   WS-PTR " LEN="
                   WS-B64-LLEN
           END-IF

      *>   Close the JSON
           STRING
               WS-QT "}}]}],"
               WS-QT "max_tokens" WS-QT
               ":1000}"
               DELIMITED SIZE
               INTO WORK-REC
               WITH POINTER WS-PTR
           END-STRING

      *>   Write single record to file
           COMPUTE WS-K = WS-PTR - 1
           DISPLAY "    Vision JSON len: "
               WS-K
           MOVE WS-VIS-PATH
               TO WS-WORK-PATH
           OPEN OUTPUT WORK-FILE
           WRITE WORK-REC
           CLOSE WORK-FILE
           MOVE "work.tmp"
               TO WS-WORK-PATH

      *>   Send to OpenAI vision
           INITIALIZE WS-CMD
           STRING "rm -f vis_resp.json"
               DELIMITED SIZE INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

           INITIALIZE WS-CMD
           STRING
               "curl -s "
               "-o vis_resp.json"
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
               " -d @vis_req.json"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Parse vision response
           MOVE "vis_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp"
               TO WS-WORK-PATH

           DISPLAY "    Vision resp: "
               WS-JBUF(1:300)

      *>   Extract content from response
           MOVE "content" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE WS-JVAL TO WS-VISION-DESC

           DISPLAY "    Vision: "
               TRIM(WS-VISION-DESC)
               (1:200)

      *>   Build tool result
           MOVE SPACES TO WS-TOOL-RESULT
           STRING
               "[IMAGE DESCRIPTION]: "
               TRIM(WS-VISION-DESC)
               DELIMITED SIZE
               INTO WS-TOOL-RESULT
           END-STRING
           MOVE LENGTH(
               TRIM(WS-TOOL-RESULT))
               TO WS-TOOL-RESULT-LEN
           IF WS-TOOL-RESULT-LEN > 16000
               MOVE 16000
                   TO WS-TOOL-RESULT-LEN
           END-IF
           .

      *> ============================================================
      *> PROCESS-AUDIO: Decode + send to Whisper
      *> ============================================================
       PROCESS-AUDIO.
           DISPLAY "    Processing audio..."

      *>   Determine file extension from MIME
           MOVE ".mp3" TO WS-AUDIO-EXT

           MOVE 0 TO WS-TALLY-CNT
           INSPECT TRIM(WS-RESP-META)
               TALLYING WS-TALLY-CNT
               FOR ALL "audio/wav"
           IF WS-TALLY-CNT > 0
               MOVE ".wav" TO WS-AUDIO-EXT
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT TRIM(WS-RESP-META)
               TALLYING WS-TALLY-CNT
               FOR ALL "audio/ogg"
           IF WS-TALLY-CNT > 0
               MOVE ".ogg" TO WS-AUDIO-EXT
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT TRIM(WS-RESP-META)
               TALLYING WS-TALLY-CNT
               FOR ALL "audio/flac"
           IF WS-TALLY-CNT > 0
               MOVE ".flac" TO WS-AUDIO-EXT
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT TRIM(WS-RESP-META)
               TALLYING WS-TALLY-CNT
               FOR ALL "audio/webm"
           IF WS-TALLY-CNT > 0
               MOVE ".webm" TO WS-AUDIO-EXT
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT TRIM(WS-RESP-META)
               TALLYING WS-TALLY-CNT
               FOR ALL "audio/mp4"
           IF WS-TALLY-CNT > 0
               MOVE ".mp4" TO WS-AUDIO-EXT
           END-IF

           DISPLAY "    Audio ext: "
               TRIM(WS-AUDIO-EXT)

      *>   Decode base64 to binary audio file
      *>   Remove backslash (JSON \/),
      *>   spaces, and CR from b64 data
           INITIALIZE WS-CMD
           STRING
               "cat "
               TRIM(WS-B64-PATH)
               " | tr -d "
               WS-QT
               WS-BS WS-BS WS-BS WS-BS
               " "
               WS-BS "r"
               WS-QT
               " | base64 -d"
               " > audio_decoded"
               TRIM(WS-AUDIO-EXT)
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           DISPLAY "    Decode cmd: "
               TRIM(WS-CMD)
           CALL "SYSTEM" USING WS-CMD

      *>   Send to Whisper API
           INITIALIZE WS-CMD
           STRING "rm -f whisper_resp.json"
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
               "file=@audio_decoded"
               TRIM(WS-AUDIO-EXT)
               WS-QT
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Parse whisper response
           MOVE "whisper_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           DISPLAY "    Whisper resp: "
               WS-JBUF(1:300)

      *>   Extract text from response
           MOVE "text" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE WS-JVAL TO WS-WHISPER-TXT

           DISPLAY "    Whisper: "
               TRIM(WS-WHISPER-TXT)(1:200)

      *>   Build tool result
           MOVE SPACES TO WS-TOOL-RESULT
           STRING
               "[AUDIO TRANSCRIPTION]: "
               TRIM(WS-WHISPER-TXT)
               DELIMITED SIZE
               INTO WS-TOOL-RESULT
           END-STRING
           MOVE LENGTH(
               TRIM(WS-TOOL-RESULT))
               TO WS-TOOL-RESULT-LEN
           IF WS-TOOL-RESULT-LEN > 16000
               MOVE 16000
                   TO WS-TOOL-RESULT-LEN
           END-IF
           .

      *> ============================================================
      *> PROCESS-TEXT-ATTACH: Decode b64 text
      *> ============================================================
       PROCESS-TEXT-ATTACH.
           DISPLAY "    Processing text..."

      *>   Decode base64 to text
      *>   Remove backslash, spaces, CR
           INITIALIZE WS-CMD
           STRING
               "cat "
               TRIM(WS-B64-PATH)
               " | tr -d "
               WS-QT
               WS-BS WS-BS WS-BS WS-BS
               " "
               WS-BS "r"
               WS-QT
               " | base64 -d"
               " > decoded_text.tmp"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD

      *>   Read decoded text
           MOVE "decoded_text.tmp"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp" TO WS-WORK-PATH

           IF WS-JLEN > 0
               IF WS-JLEN > 8000
                   MOVE 8000 TO WS-JLEN
               END-IF
               MOVE WS-JBUF(1:WS-JLEN)
                   TO WS-DECODED-TXT
           ELSE
               MOVE "(empty)" TO WS-DECODED-TXT
           END-IF

           DISPLAY "    Decoded: "
               TRIM(WS-DECODED-TXT)(1:200)

      *>   Build tool result
           MOVE SPACES TO WS-TOOL-RESULT
           STRING
               "[DATA ("
               TRIM(WS-RESP-META)
               ")]: "
               TRIM(WS-DECODED-TXT)
               DELIMITED SIZE
               INTO WS-TOOL-RESULT
           END-STRING
           MOVE LENGTH(
               TRIM(WS-TOOL-RESULT))
               TO WS-TOOL-RESULT-LEN
           IF WS-TOOL-RESULT-LEN > 16000
               MOVE 16000
                   TO WS-TOOL-RESULT-LEN
           END-IF
           .

      *> ============================================================
      *> TOOL-TRANSMIT: Send final report
      *> ============================================================
       TOOL-TRANSMIT.
      *>   Parse args
           MOVE WS-TOOL-ARGS TO WS-JBUF
           MOVE LENGTH(TRIM(WS-TOOL-ARGS))
               TO WS-JLEN

           MOVE "cityName" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL)
               TO WS-TA-CITY

           MOVE "cityArea" TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL)
               TO WS-TA-AREA

           MOVE "warehousesCount"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL)
               TO WS-TA-WAREHOUSES

           MOVE "phoneNumber"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL)
               TO WS-TA-PHONE

           DISPLAY "    City: "
               TRIM(WS-TA-CITY)
           DISPLAY "    Area: "
               TRIM(WS-TA-AREA)
           DISPLAY "    Warehouses: "
               TRIM(WS-TA-WAREHOUSES)
           DISPLAY "    Phone: "
               TRIM(WS-TA-PHONE)

      *>   Build hub request
           MOVE SPACES TO WS-HUB-BODY
           MOVE 1 TO WS-PTR

      *>   Escape city name
           MOVE TRIM(WS-TA-CITY)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

           STRING
               "{" WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY)
               WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT TRIM(WS-TASK-NAME)
               WS-QT ","
               WS-QT "answer" WS-QT ":{"
               WS-QT "action" WS-QT ":"
               WS-QT "transmit" WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "cityName" WS-QT ":"
               WS-QT
               WS-ESC-OUT(1:WS-ESC-OLEN)
               WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "cityArea" WS-QT ":"
               WS-QT TRIM(WS-TA-AREA)
               WS-QT ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           STRING
               WS-QT "warehousesCount"
               WS-QT ":"
               TRIM(WS-TA-WAREHOUSES) ","
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

      *>   Escape phone
           MOVE TRIM(WS-TA-PHONE)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

           STRING
               WS-QT "phoneNumber" WS-QT ":"
               WS-QT
               WS-ESC-OUT(1:WS-ESC-OLEN)
               WS-QT "}}"
               DELIMITED SIZE
               INTO WS-HUB-BODY
               WITH POINTER WS-PTR
           END-STRING

           PERFORM SEND-HUB-REQUEST
           PERFORM STORE-TOOL-RESULT

           DISPLAY "  Transmit result: "
               WS-TOOL-RESULT(1:500)
           .

      *> ============================================================
      *> SEND-HUB-REQUEST: Write body + curl POST
      *> with 503 retry
      *> ============================================================
       SEND-HUB-REQUEST.
           MOVE 0 TO WS-RETRY-CT

           PERFORM UNTIL WS-RETRY-CT > 7

               MOVE "hub_req.tmp"
                   TO WS-WORK-PATH
               OPEN OUTPUT WORK-FILE
               WRITE WORK-REC
                   FROM WS-HUB-BODY
               CLOSE WORK-FILE

               INITIALIZE WS-CMD
               STRING
                   "rm -f hub_resp.json"
                   DELIMITED SIZE
                   INTO WS-CMD
               END-STRING
               CALL "SYSTEM" USING WS-CMD

               INITIALIZE WS-CMD
               STRING
                   "curl -s "
                   "-o hub_resp.json"
                   " -w " WS-QT
                   "%{http_code}"
                   WS-QT
                   " -X POST "
                   TRIM(WS-VERIFY-URL)
                   " -H " WS-QT
                   "Content-Type: "
                   "application/json"
                   WS-QT
                   " -H " WS-QT
                   "Accept: */*"
                   WS-QT
                   " -H " WS-QT
                   "User-Agent: "
                   "python-requests"
                   "/2.31.0"
                   WS-QT
                   " -b cookies.txt"
                   " -c cookies.txt"
                   " -d @hub_req.tmp"
                   DELIMITED SIZE
                   INTO WS-CMD
               END-STRING
               CALL "SYSTEM" USING WS-CMD

               MOVE "hub_resp.json"
                   TO WS-WORK-PATH
               PERFORM READ-JSON-FILE
               MOVE "work.tmp"
                   TO WS-WORK-PATH

      *>       Check for 503 in first 200
      *>       bytes only (avoid false match
      *>       in base64 attachment data)
               MOVE 0 TO WS-TALLY-CNT
               IF WS-JLEN > 0
                   IF WS-JLEN > 200
                       INSPECT
                           WS-JBUF(1:200)
                           TALLYING
                           WS-TALLY-CNT
                           FOR ALL "503"
                   ELSE
                       INSPECT
                           WS-JBUF(
                           1:WS-JLEN)
                           TALLYING
                           WS-TALLY-CNT
                           FOR ALL "503"
                   END-IF
               END-IF
               IF WS-TALLY-CNT > 0
                   ADD 1 TO WS-RETRY-CT
                   DISPLAY "    503, retry "
                       WS-RETRY-CT
                   CALL "C$SLEEP" USING 2
                   EXIT PERFORM CYCLE
               END-IF

      *>       Success - exit retry loop
               EXIT PERFORM
           END-PERFORM

           DISPLAY "    Hub: "
               WS-JBUF(1:500)
           .

      *> ============================================================
      *> STORE-TOOL-RESULT: Copy JBUF to result
      *> ============================================================
       STORE-TOOL-RESULT.
           COMPUTE WS-TOOL-RESULT-LEN =
               LENGTH(TRIM(WS-JBUF))
           IF WS-TOOL-RESULT-LEN > 16000
               MOVE 16000
                   TO WS-TOOL-RESULT-LEN
           END-IF
           IF WS-TOOL-RESULT-LEN > 0
               MOVE WS-JBUF(
                   1:WS-TOOL-RESULT-LEN)
                   TO WS-TOOL-RESULT
           END-IF
           .

      *> ============================================================
      *> APPEND-TOOL-EXCHANGE
      *> ============================================================
       APPEND-TOOL-EXCHANGE.
      *>   Guard: if conv buf > 190000, skip
      *>   to avoid overflow (200000 limit)
           IF WS-CONV-PTR > 190000
               DISPLAY "  Conv buf full!"
               EXIT PARAGRAPH
           END-IF

           SUBTRACT 1 FROM WS-CONV-PTR

      *>   Escape tool args
           MOVE TRIM(WS-TOOL-ARGS)
               TO WS-ESC-IN
           PERFORM JSON-ESCAPE-STR

           STRING ","
               "{" WS-QT "role" WS-QT ":"
               WS-QT "assistant" WS-QT ","
               WS-QT "content" WS-QT
               ":null,"
               WS-QT "tool_calls" WS-QT
               ":["
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

      *>   Escape tool result (cap at 4000
      *>   for conv buf to avoid overflow)
           IF WS-TOOL-RESULT-LEN > 0
               IF WS-TOOL-RESULT-LEN
                   > 4000
                   MOVE 4000
                       TO WS-TOOL-RESULT-LEN
               END-IF
               MOVE WS-TOOL-RESULT(
                   1:WS-TOOL-RESULT-LEN)
                   TO WS-ESC-IN
           ELSE
               MOVE "{}" TO WS-ESC-IN
           END-IF
           PERFORM JSON-ESCAPE-STR

           STRING
               "{" WS-QT "role" WS-QT ":"
               WS-QT "tool" WS-QT ","
               WS-QT "tool_call_id" WS-QT
               ":"
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
      *> APPEND-TEXT-NUDGE
      *> ============================================================
       APPEND-TEXT-NUDGE.
      *>   Guard: if conv buf > 185000, skip
           IF WS-CONV-PTR > 185000
               DISPLAY "  Conv buf full!"
               EXIT PARAGRAPH
           END-IF

           SUBTRACT 1 FROM WS-CONV-PTR

           STRING ","
               "{" WS-QT "role" WS-QT ":"
               WS-QT "assistant" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT "OK" WS-QT "},"
               "{" WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT ":"
               WS-QT "Use the tools. Call"
               " the next tool now."
               WS-QT "}]"
               DELIMITED SIZE
               INTO WS-CONV-BUF
               WITH POINTER WS-CONV-PTR
           END-STRING
           .

      *> ============================================================
      *> PARSE-TOOL-CALL
      *> ============================================================
       PARSE-TOOL-CALL.
           MOVE SPACES TO WS-TOOL-NAME
           MOVE SPACES TO WS-TOOL-CALL-ID
           MOVE SPACES TO WS-TOOL-ARGS

           MOVE SPACES TO WS-TMP2
           STRING WS-QT "tool_calls" WS-QT
               DELIMITED SIZE INTO WS-TMP2
           END-STRING

           MOVE 0 TO WS-KEY-POS
           PERFORM VARYING WS-FJV-POS
               FROM 1 BY 1
               UNTIL WS-FJV-POS > WS-JLEN
               OR WS-KEY-POS > 0
               IF WS-JBUF(WS-FJV-POS:
                   LENGTH(TRIM(WS-TMP2)))
                   = TRIM(WS-TMP2)
                   MOVE WS-FJV-POS
                       TO WS-KEY-POS
               END-IF
           END-PERFORM

           IF WS-KEY-POS = 0
               EXIT PARAGRAPH
           END-IF

      *>   Save JBUF state, parse within
           MOVE WS-JBUF(1:WS-JLEN)
               TO WS-JBUF-SAVE
           MOVE WS-JLEN TO WS-JLEN-SAVE

      *>   Extract id, name, arguments
           MOVE "id" TO WS-KEY-SEARCH
           MOVE WS-KEY-POS TO WS-JPOS
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL)
               TO WS-TOOL-CALL-ID

           MOVE "name" TO WS-KEY-SEARCH
           PERFORM FIND-JSON-VAL
           MOVE TRIM(WS-JVAL)
               TO WS-TOOL-NAME

           MOVE "arguments"
               TO WS-KEY-SEARCH
           PERFORM FIND-JSON-VAL
      *>   Unescape arguments JSON
           MOVE WS-JVAL TO WS-ESC-IN
           PERFORM JSON-UNESCAPE-STR
           MOVE WS-ESC-OUT(1:WS-ESC-OLEN)
               TO WS-TOOL-ARGS

      *>   Restore JBUF
           MOVE WS-JBUF-SAVE(1:WS-JLEN-SAVE)
               TO WS-JBUF
           MOVE WS-JLEN-SAVE TO WS-JLEN

           DISPLAY "  Tool ID: "
               TRIM(WS-TOOL-CALL-ID)
           DISPLAY "  Args: "
               TRIM(WS-TOOL-ARGS)(1:300)
           .

      *> ============================================================
      *> JSON-ESCAPE-STR
      *> ============================================================
       JSON-ESCAPE-STR.
           MOVE SPACES TO WS-ESC-OUT
           MOVE 0 TO WS-ESC-OLEN
           MOVE LENGTH(TRIM(WS-ESC-IN))
               TO WS-ESC-ILEN

           IF WS-ESC-ILEN = 0
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-ESC-I
               FROM 1 BY 1
               UNTIL WS-ESC-I
               > WS-ESC-ILEN
               EVALUATE TRUE
               WHEN WS-ESC-IN(
                   WS-ESC-I:1) = WS-QT
                   ADD 1 TO WS-ESC-OLEN
                   MOVE X"5C"
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
                   ADD 1 TO WS-ESC-OLEN
                   MOVE WS-QT
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
               WHEN WS-ESC-IN(
                   WS-ESC-I:1) = X"5C"
                   ADD 1 TO WS-ESC-OLEN
                   MOVE X"5C"
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
                   ADD 1 TO WS-ESC-OLEN
                   MOVE X"5C"
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
               WHEN WS-ESC-IN(
                   WS-ESC-I:1) = X"0A"
                   ADD 1 TO WS-ESC-OLEN
                   MOVE X"5C"
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
                   ADD 1 TO WS-ESC-OLEN
                   MOVE "n"
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
               WHEN WS-ESC-IN(
                   WS-ESC-I:1) = X"0D"
                   ADD 1 TO WS-ESC-OLEN
                   MOVE X"5C"
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
                   ADD 1 TO WS-ESC-OLEN
                   MOVE "r"
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
               WHEN WS-ESC-IN(
                   WS-ESC-I:1) = X"09"
                   ADD 1 TO WS-ESC-OLEN
                   MOVE X"5C"
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
                   ADD 1 TO WS-ESC-OLEN
                   MOVE "t"
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
               WHEN OTHER
                   ADD 1 TO WS-ESC-OLEN
                   MOVE WS-ESC-IN(
                       WS-ESC-I:1)
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
               END-EVALUATE
           END-PERFORM
           .

      *> ============================================================
      *> JSON-UNESCAPE-STR
      *> ============================================================
       JSON-UNESCAPE-STR.
           MOVE SPACES TO WS-ESC-OUT
           MOVE 0 TO WS-ESC-OLEN
           MOVE LENGTH(TRIM(WS-ESC-IN))
               TO WS-ESC-ILEN

           IF WS-ESC-ILEN = 0
               EXIT PARAGRAPH
           END-IF

           MOVE 1 TO WS-ESC-I
           PERFORM UNTIL
               WS-ESC-I > WS-ESC-ILEN
               IF WS-ESC-IN(
                   WS-ESC-I:1) = X"5C"
               AND WS-ESC-I
                   < WS-ESC-ILEN
                   ADD 1 TO WS-ESC-I
                   ADD 1 TO WS-ESC-OLEN
                   MOVE WS-ESC-IN(
                       WS-ESC-I:1)
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
               ELSE
                   ADD 1 TO WS-ESC-OLEN
                   MOVE WS-ESC-IN(
                       WS-ESC-I:1)
                     TO WS-ESC-OUT(
                     WS-ESC-OLEN:1)
               END-IF
               ADD 1 TO WS-ESC-I
           END-PERFORM
           .

      *> ============================================================
      *> READ-JSON-FILE
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
               READ WORK-FILE
                   INTO WS-LINE
                   AT END
                       MOVE "Y"
                           TO WS-EOF
                   NOT AT END
                       MOVE 0
                           TO WS-I
                       INSPECT WS-LINE
                           TALLYING
                           WS-I
                           FOR TRAILING
                           SPACES
                       COMPUTE WS-K =
                           900000
                           - WS-I
                       IF WS-K > 0
                           IF WS-JLEN + WS-K
                               > 900000
                               COMPUTE WS-K =
                                   900000
                                   - WS-JLEN
                           END-IF
                           IF WS-K > 0
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

      *> ============================================================
      *> FIND-JSON-VAL
      *> ============================================================
       FIND-JSON-VAL.
           MOVE SPACES TO WS-JVAL
           MOVE SPACES TO WS-TMP2
           STRING WS-QT
               TRIM(WS-KEY-SEARCH)
               WS-QT
               DELIMITED SIZE
               INTO WS-TMP2
           END-STRING

           MOVE 0 TO WS-KEY-POS
           PERFORM VARYING WS-FJV-POS
               FROM WS-JPOS BY 1
               UNTIL WS-FJV-POS
                   > WS-JLEN
               OR WS-KEY-POS > 0
               IF WS-FJV-POS
                   + LENGTH(
                   TRIM(WS-TMP2))
                   - 1 <= WS-JLEN
               AND WS-JBUF(
                   WS-FJV-POS:
                   LENGTH(
                   TRIM(WS-TMP2)))
                   = TRIM(WS-TMP2)
                   MOVE WS-FJV-POS
                       TO WS-KEY-POS
               END-IF
           END-PERFORM

           IF WS-KEY-POS = 0
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-FJV-POS =
               WS-KEY-POS
               + LENGTH(
               TRIM(WS-TMP2))
           PERFORM UNTIL
               WS-FJV-POS > WS-JLEN
               OR WS-JBUF(
               WS-FJV-POS:1) = ":"
               ADD 1 TO WS-FJV-POS
           END-PERFORM
           ADD 1 TO WS-FJV-POS

           PERFORM UNTIL
               WS-FJV-POS > WS-JLEN
               OR WS-JBUF(
               WS-FJV-POS:1)
               NOT = " "
               ADD 1 TO WS-FJV-POS
           END-PERFORM

           IF WS-JBUF(
               WS-FJV-POS:1) = WS-QT
               ADD 1 TO WS-FJV-POS
               MOVE WS-FJV-POS
                   TO WS-VAL-START
               PERFORM UNTIL
                   WS-FJV-POS
                   > WS-JLEN
                   IF WS-JBUF(
                       WS-FJV-POS:1)
                       = X"5C"
                   AND WS-FJV-POS
                       < WS-JLEN
                       ADD 2
                       TO WS-FJV-POS
                   ELSE
                       IF WS-JBUF(
                           WS-FJV-POS:1)
                           = WS-QT
                           EXIT PERFORM
                       END-IF
                       ADD 1
                       TO WS-FJV-POS
                   END-IF
               END-PERFORM
               COMPUTE WS-VAL-END =
                   WS-FJV-POS - 1
               IF WS-VAL-END
                   >= WS-VAL-START
               AND WS-VAL-END
                   - WS-VAL-START
                   + 1 <= 4000
                   MOVE WS-JBUF(
                       WS-VAL-START:
                       WS-VAL-END
                       - WS-VAL-START
                       + 1) TO WS-JVAL
               END-IF
               ADD 1 TO WS-FJV-POS
           ELSE
               MOVE WS-FJV-POS
                   TO WS-VAL-START
               PERFORM UNTIL
                   WS-FJV-POS
                   > WS-JLEN
                   OR WS-JBUF(
                   WS-FJV-POS:1)
                   = ","
                   OR WS-JBUF(
                   WS-FJV-POS:1)
                   = "}"
                   OR WS-JBUF(
                   WS-FJV-POS:1)
                   = "]"
                   OR WS-JBUF(
                   WS-FJV-POS:1)
                   = " "
                   ADD 1
                   TO WS-FJV-POS
               END-PERFORM
               COMPUTE WS-VAL-END =
                   WS-FJV-POS - 1
               IF WS-VAL-END
                   >= WS-VAL-START
                   MOVE WS-JBUF(
                       WS-VAL-START:
                       WS-VAL-END
                       - WS-VAL-START
                       + 1) TO WS-JVAL
               END-IF
           END-IF
           MOVE WS-FJV-POS
               TO WS-JPOS
           .
