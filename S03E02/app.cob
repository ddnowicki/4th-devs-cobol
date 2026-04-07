       IDENTIFICATION DIVISION.
       PROGRAM-ID. S03E02-FIRMWARE.
      *> ============================================================
      *> S03E02 - Firmware Fixer (Pure COBOL)
      *> 1. Reboot VM via Shell API
      *> 2. Scripted exploration to gather filesystem data
      *> 3. ONE GPT-4.1-mini call to find the password
      *> 4. Deterministic config fix + binary execution
      *> 5. Submit ECCS code to Hub /verify
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
      *> === Environment (via copybook) ===
       COPY ENVLOAD-WS.

      *> === Constants ===
       01  WS-MAX-RETRIES          PIC 9(1) VALUE 8.
       01  WS-RETRY-DELAY          PIC 9(2) VALUE 5.

      *> === File I/O ===
       01  WS-FS                   PIC XX.
       01  WS-WORK-PATH            PIC X(200)
                                   VALUE "work.tmp".

      *> === HTTP ===
       01  WS-CMD                  PIC X(4000).
       01  WS-REQ-JSON             PIC X(16000).
       01  WS-PAYLOAD              PIC X(4000).
       01  WS-SHELL-URL            PIC X(200).

      *> === JSON Parsing (copybook) ===
       COPY JSONPARSE-WS.
       COPY JSONREAD-WS.

      *> === Task Data ===
       01  WS-SHELL-CMD            PIC X(500).
       01  WS-SHELL-DATA           PIC X(8000).
       01  WS-SHELL-CODE           PIC X(20).
       01  WS-SHELL-RETRY          PIC 9(2).
       01  WS-PASSWORD             PIC X(100).
       01  WS-ECCS-CODE            PIC X(100).
       01  WS-GATHERED             PIC X(8000).
       01  WS-GATH-LEN             PIC 9(5).

      *> === Control Flow ===
       01  WS-PTR                  PIC 9(5).
       01  WS-I                    PIC 9(5).
       01  WS-TALLY-CNT            PIC 9(5).
       01  WS-SUCCESS              PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== S03E02 FIRMWARE ==="

           PERFORM INIT-ENV
           PERFORM REBOOT-VM
           PERFORM EXPLORE-VM
           PERFORM AI-FIND-PASSWORD
           PERFORM FIX-AND-RUN

           IF TRIM(WS-ECCS-CODE)
               NOT = SPACES
               PERFORM SUBMIT-ECCS
           ELSE
               DISPLAY "  No ECCS found!"
           END-IF

           IF WS-SUCCESS = "Y"
               DISPLAY " "
               DISPLAY "=== SUCCESS ==="
           ELSE
               DISPLAY " "
               DISPLAY "=== FAILED ==="
           END-IF
           STOP RUN.

      *> ============================================================
      *> INIT-ENV: Load env vars, build URLs
      *> ============================================================
       INIT-ENV.
           PERFORM LOAD-ENV-VARS

           MOVE SPACES TO WS-SHELL-URL
           STRING TRIM(WS-HUB-URL)
               "/api/shell"
               DELIMITED SIZE
               INTO WS-SHELL-URL
           END-STRING

           DISPLAY "  Shell: "
               TRIM(WS-SHELL-URL)
           .

      *> ============================================================
      *> REBOOT-VM: Reset VM to clean state
      *> ============================================================
       REBOOT-VM.
           DISPLAY " "
           DISPLAY "--- REBOOT ---"
           MOVE "reboot" TO WS-SHELL-CMD
           PERFORM SHELL-EXEC
           CALL "C$SLEEP" USING 1
           .

      *> ============================================================
      *> EXPLORE-VM: Gather filesystem data
      *> ============================================================
       EXPLORE-VM.
           DISPLAY " "
           DISPLAY "--- EXPLORE ---"
           MOVE 0 TO WS-GATH-LEN
           MOVE SPACES TO WS-GATHERED

      *>   Read settings.ini
           MOVE SPACES TO WS-SHELL-CMD
           STRING
               "cat /opt/firmware/"
               "cooler/settings.ini"
               DELIMITED SIZE
               INTO WS-SHELL-CMD
           END-STRING
           PERFORM SHELL-EXEC
           PERFORM APPEND-DATA

      *>   Explore /home
           MOVE "ls /home"
               TO WS-SHELL-CMD
           PERFORM SHELL-EXEC

           MOVE "ls /home/operator"
               TO WS-SHELL-CMD
           PERFORM SHELL-EXEC

      *>   Read password file
           MOVE SPACES TO WS-SHELL-CMD
           STRING
               "ls /home/operator/"
               "notes"
               DELIMITED SIZE
               INTO WS-SHELL-CMD
           END-STRING
           PERFORM SHELL-EXEC

           MOVE SPACES TO WS-SHELL-CMD
           STRING
               "cat /home/operator/"
               "notes/pass.txt"
               DELIMITED SIZE
               INTO WS-SHELL-CMD
           END-STRING
           PERFORM SHELL-EXEC
           PERFORM APPEND-DATA

      *>   Read bash_history
           MOVE SPACES TO WS-SHELL-CMD
           STRING
               "cat /home/operator/"
               ".bash_history"
               DELIMITED SIZE
               INTO WS-SHELL-CMD
           END-STRING
           PERFORM SHELL-EXEC
           PERFORM APPEND-DATA

      *>   Check /tmp
           MOVE "ls /tmp"
               TO WS-SHELL-CMD
           PERFORM SHELL-EXEC

           MOVE "cat /tmp/aidevs4.txt"
               TO WS-SHELL-CMD
           PERFORM SHELL-EXEC

           DISPLAY "  Gathered "
               WS-GATH-LEN " chars"
           .

      *> ============================================================
      *> APPEND-DATA: Add WS-SHELL-DATA to gathered buf
      *> ============================================================
       APPEND-DATA.
           IF TRIM(WS-SHELL-DATA) = SPACES
               EXIT PARAGRAPH
           END-IF
           MOVE LENGTH(TRIM(WS-SHELL-DATA))
               TO WS-K
           IF WS-K = 0
               EXIT PARAGRAPH
           END-IF
      *>   Add separator
           IF WS-GATH-LEN > 0
               ADD 1 TO WS-GATH-LEN
               MOVE X"5C"
                   TO WS-GATHERED(
                   WS-GATH-LEN:1)
               ADD 1 TO WS-GATH-LEN
               MOVE "n"
                   TO WS-GATHERED(
                   WS-GATH-LEN:1)
               ADD 1 TO WS-GATH-LEN
               MOVE "-"
                   TO WS-GATHERED(
                   WS-GATH-LEN:1)
               ADD 1 TO WS-GATH-LEN
               MOVE "-"
                   TO WS-GATHERED(
                   WS-GATH-LEN:1)
               ADD 1 TO WS-GATH-LEN
               MOVE "-"
                   TO WS-GATHERED(
                   WS-GATH-LEN:1)
               ADD 1 TO WS-GATH-LEN
               MOVE X"5C"
                   TO WS-GATHERED(
                   WS-GATH-LEN:1)
               ADD 1 TO WS-GATH-LEN
               MOVE "n"
                   TO WS-GATHERED(
                   WS-GATH-LEN:1)
           END-IF
      *>   Append data
           IF WS-GATH-LEN + WS-K < 7900
               MOVE WS-SHELL-DATA(1:WS-K)
                   TO WS-GATHERED(
                   WS-GATH-LEN + 1:WS-K)
               ADD WS-K TO WS-GATH-LEN
           END-IF
           .

      *> ============================================================
      *> AI-FIND-PASSWORD: ONE call to GPT-4.1-mini
      *> ============================================================
       AI-FIND-PASSWORD.
           DISPLAY " "
           DISPLAY "--- AI ANALYSIS ---"

           MOVE SPACES TO WS-REQ-JSON
           MOVE 1 TO WS-PTR

      *>   Build JSON request
           STRING
               "{"
               WS-QT "model" WS-QT ":"
               WS-QT "gpt-4.1-mini"
               WS-QT ","
               WS-QT "messages"
               WS-QT ":["
               "{"
               WS-QT "role" WS-QT ":"
               WS-QT "user" WS-QT ","
               WS-QT "content" WS-QT
               ":" WS-QT
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Prompt text
           STRING
               "Files from a Linux"
               " VM:" WS-NL WS-NL
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Gathered data
           IF WS-GATH-LEN > 0
               STRING
                   WS-GATHERED(
                   1:WS-GATH-LEN)
                   DELIMITED SIZE
                   INTO WS-REQ-JSON
                   WITH POINTER WS-PTR
               END-STRING
           END-IF

      *>   Question + close JSON
           STRING
               WS-NL WS-NL
               "What is the password"
               " to run cooler.bin?"
               " Reply ONLY the "
               "password string, "
               "nothing else."
               WS-QT "}],"
               WS-QT "temperature"
               WS-QT ":0,"
               WS-QT "max_tokens"
               WS-QT ":50}"
               DELIMITED SIZE
               INTO WS-REQ-JSON
               WITH POINTER WS-PTR
           END-STRING

      *>   Write request to file
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
           PERFORM CALL-OPENAI

      *>   Extract password
           MOVE "llm_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp"
               TO WS-WORK-PATH

           IF WS-JLEN = 0
               DISPLAY "  Empty AI resp!"
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           INSPECT WS-JBUF(1:WS-JLEN)
               TALLYING WS-TALLY-CNT
               FOR ALL '"error"'
           IF WS-TALLY-CNT > 0
               DISPLAY "  AI ERR: "
                   TRIM(WS-JBUF)(1:300)
               EXIT PARAGRAPH
           END-IF

           MOVE "content"
               TO WS-KEY-SEARCH
           MOVE 1 TO WS-JPOS
           PERFORM FIND-JSON-VAL

           MOVE TRIM(WS-JVAL)
               TO WS-PASSWORD
           DISPLAY "  Password: "
               TRIM(WS-PASSWORD)
           .

      *> ============================================================
      *> CALL-OPENAI: POST work.tmp to OpenAI
      *> ============================================================
       CALL-OPENAI.
           DISPLAY "  Calling OpenAI..."
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
               " -d @work.tmp"
               DELIMITED SIZE
               INTO WS-CMD
           END-STRING
           CALL "SYSTEM" USING WS-CMD
           .

      *> ============================================================
      *> FIX-AND-RUN: Apply config fixes, run binary
      *> ============================================================
       FIX-AND-RUN.
           DISPLAY " "
           DISPLAY "--- FIX & RUN ---"

           IF TRIM(WS-PASSWORD) = SPACES
               DISPLAY "  No password!"
               EXIT PARAGRAPH
           END-IF

      *>   Remove lock file
           MOVE SPACES TO WS-SHELL-CMD
           STRING
               "rm /opt/firmware/"
               "cooler/"
               "cooler-is-blocked"
               ".lock"
               DELIMITED SIZE
               INTO WS-SHELL-CMD
           END-STRING
           PERFORM SHELL-EXEC

      *>   Edit line 2: uncomment SAFETY_CHECK
           MOVE SPACES TO WS-SHELL-CMD
           STRING
               "editline /opt/"
               "firmware/cooler/"
               "settings.ini 2 "
               "SAFETY_CHECK=pass"
               DELIMITED SIZE
               INTO WS-SHELL-CMD
           END-STRING
           PERFORM SHELL-EXEC

      *>   Edit line 6: disable test_mode
           MOVE SPACES TO WS-SHELL-CMD
           STRING
               "editline /opt/"
               "firmware/cooler/"
               "settings.ini 6 "
               "enabled=false"
               DELIMITED SIZE
               INTO WS-SHELL-CMD
           END-STRING
           PERFORM SHELL-EXEC

      *>   Edit line 10: enable cooling
           MOVE SPACES TO WS-SHELL-CMD
           STRING
               "editline /opt/"
               "firmware/cooler/"
               "settings.ini 10 "
               "enabled=true"
               DELIMITED SIZE
               INTO WS-SHELL-CMD
           END-STRING
           PERFORM SHELL-EXEC

      *>   cd to firmware dir
           MOVE SPACES TO WS-SHELL-CMD
           STRING
               "cd /opt/firmware/"
               "cooler"
               DELIMITED SIZE
               INTO WS-SHELL-CMD
           END-STRING
           PERFORM SHELL-EXEC

      *>   Run binary with password
           MOVE SPACES TO WS-SHELL-CMD
           STRING
               "./cooler.bin "
               TRIM(WS-PASSWORD)
               DELIMITED SIZE
               INTO WS-SHELL-CMD
           END-STRING
           PERFORM SHELL-EXEC

      *>   Extract ECCS from output
           PERFORM EXTRACT-ECCS
           .

      *> ============================================================
      *> EXTRACT-ECCS: Find ECCS code in shell data
      *> ============================================================
       EXTRACT-ECCS.
           MOVE SPACES TO WS-ECCS-CODE

           IF TRIM(WS-SHELL-DATA) = SPACES
               DISPLAY "  No binary output!"
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-TALLY-CNT
           MOVE LENGTH(
               TRIM(WS-SHELL-DATA))
               TO WS-K
           INSPECT WS-SHELL-DATA(1:WS-K)
               TALLYING WS-TALLY-CNT
               FOR ALL "ECCS-"

           IF WS-TALLY-CNT = 0
               DISPLAY "  No ECCS found!"
               DISPLAY "  Data: "
                   WS-SHELL-DATA(1:200)
               EXIT PARAGRAPH
           END-IF

      *>   Find ECCS- position
           PERFORM VARYING WS-I
               FROM 1 BY 1
               UNTIL WS-I > WS-K - 4
               IF WS-SHELL-DATA(
                   WS-I:5) = "ECCS-"
                   MOVE WS-SHELL-DATA(
                       WS-I:45)
                       TO WS-ECCS-CODE
                   DISPLAY "  ECCS: "
                       TRIM(WS-ECCS-CODE)
                   EXIT PERFORM
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> SUBMIT-ECCS: Submit to /verify
      *> ============================================================
       SUBMIT-ECCS.
           DISPLAY " "
           DISPLAY "--- SUBMIT ---"

           MOVE SPACES TO WS-PAYLOAD
           STRING
               "{"
               WS-QT "apikey" WS-QT ":"
               WS-QT TRIM(WS-HUB-KEY)
               WS-QT ","
               WS-QT "task" WS-QT ":"
               WS-QT "firmware"
               WS-QT ","
               WS-QT "answer" WS-QT
               ":{"
               WS-QT "confirmation"
               WS-QT ":"
               WS-QT
               TRIM(WS-ECCS-CODE)
               WS-QT "}}"
               DELIMITED SIZE
               INTO WS-PAYLOAD
           END-STRING

           OPEN OUTPUT WORK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "ERR: OPEN "
                   TRIM(WS-WORK-PATH)
                   " FS=" WS-FS
               STOP RUN
           END-IF
           WRITE WORK-REC
               FROM WS-PAYLOAD
           CLOSE WORK-FILE

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
           CALL "SYSTEM" USING WS-CMD

      *>   Check response
           MOVE "hub_resp.json"
               TO WS-WORK-PATH
           PERFORM READ-JSON-FILE
           MOVE "work.tmp"
               TO WS-WORK-PATH

           DISPLAY "  Hub: "
               TRIM(WS-JBUF)(1:300)

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
               DISPLAY " "
               DISPLAY "  >>> FLAG FOUND!"
           END-IF
           .

      *> ============================================================
      *> SHELL-EXEC: Execute cmd via Shell API
      *> ============================================================
       SHELL-EXEC.
           MOVE SPACES TO WS-SHELL-DATA
           MOVE SPACES TO WS-SHELL-CODE

           PERFORM VARYING WS-SHELL-RETRY
               FROM 1 BY 1
               UNTIL WS-SHELL-RETRY
               > WS-MAX-RETRIES

      *>       Build payload
               MOVE SPACES TO WS-PAYLOAD
               STRING
                   "{"
                   WS-QT "apikey"
                   WS-QT ":"
                   WS-QT
                   TRIM(WS-HUB-KEY)
                   WS-QT ","
                   WS-QT "cmd"
                   WS-QT ":"
                   WS-QT
                   TRIM(WS-SHELL-CMD)
                   WS-QT "}"
                   DELIMITED SIZE
                   INTO WS-PAYLOAD
               END-STRING

      *>       Write payload to file
               OPEN OUTPUT WORK-FILE
               IF WS-FS NOT = "00"
                   DISPLAY "ERR: OPEN "
                       TRIM(WS-WORK-PATH)
                       " FS=" WS-FS
                   STOP RUN
               END-IF
               WRITE WORK-REC
                   FROM WS-PAYLOAD
               CLOSE WORK-FILE

      *>       curl POST
               INITIALIZE WS-CMD
               STRING
                   "curl -s "
                   "-o shell_resp.json"
                   " -X POST "
                   TRIM(WS-SHELL-URL)
                   " -H " WS-QT
                   "Content-Type: "
                   "application/json"
                   WS-QT
                   " -d @work.tmp"
                   DELIMITED SIZE
                   INTO WS-CMD
               END-STRING
               CALL "SYSTEM"
                   USING WS-CMD

      *>       Read response
               MOVE "shell_resp.json"
                   TO WS-WORK-PATH
               PERFORM READ-JSON-FILE
               MOVE "work.tmp"
                   TO WS-WORK-PATH

               IF WS-JLEN = 0
                   DISPLAY "  Empty resp!"
                   CALL "C$SLEEP"
                       USING WS-RETRY-DELAY
               ELSE
      *>           Parse code
                   MOVE "code"
                       TO WS-KEY-SEARCH
                   MOVE 1 TO WS-JPOS
                   PERFORM FIND-JSON-VAL
                   MOVE TRIM(WS-JVAL)
                       TO WS-SHELL-CODE
                   DISPLAY "  ["
                       TRIM(WS-SHELL-CMD)
                       (1:30) "] code="
                       TRIM(WS-SHELL-CODE)

      *>           Check code
                   EVALUATE TRUE
                   WHEN TRIM(WS-SHELL-CODE)
                       = "-9999"
                       DISPLAY
                           "  [RATE] wait 10s"
                       CALL "C$SLEEP"
                           USING 10
                   WHEN TRIM(WS-SHELL-CODE)
                       = "-735"
                       DISPLAY
                           "  [BAN] wait 20s"
                       CALL "C$SLEEP"
                           USING 20
                   WHEN TRIM(WS-SHELL-CODE)
                       = "-733"
                       DISPLAY
                           "  [BAN!] wait 20s"
                       CALL "C$SLEEP"
                           USING 20
                       EXIT PERFORM
                   WHEN OTHER
      *>               Success - get data
                       MOVE "data"
                           TO WS-KEY-SEARCH
                       MOVE 1 TO WS-JPOS
                       PERFORM FIND-JSON-VAL
                       MOVE WS-JVAL
                           TO WS-SHELL-DATA
                       EXIT PERFORM
                   END-EVALUATE
               END-IF
           END-PERFORM
           .

       COPY JSONREAD-PROC.

       COPY JSONPARSE-PROC.

       COPY ENVLOAD-PROC.
