<div align="center">

# AI_devs 4 Builders: The COBOL Edition

**25 AI tasks. 43,000+ lines. One language from 1959.**

[![COBOL CI](https://img.shields.io/github/actions/workflow/status/ddnowicki/4th-devs-cobol/cobol-tasks.yml?label=25%2F25%20tasks%20pass&style=for-the-badge&logo=github)](https://github.com/ddnowicki/4th-devs-cobol/actions/workflows/cobol-tasks.yml)
[![COBOL](https://img.shields.io/badge/language-COBOL-blue?style=for-the-badge)](https://gnucobol.sourceforge.io/)
[![Lines](https://img.shields.io/badge/lines_of_COBOL-43%2C196-brightgreen?style=for-the-badge)](#)

TCP servers, PNG decoders, LLM agent loops with function calling,
rocket navigation, time travel. No Python. Just COBOL.

</div>

---

## Why COBOL?

At the start of the AI_devs 4 course, co-founder **[@mateuszchrobok](https://github.com/mateuszchrobok)** (Mateusz Chrobok) threw down a challenge: he would personally come to the home of anyone who completes **the entire course in COBOL**... and make them a **pineapple pizza** 🍕🍍

This repository is the answer to that challenge.

25 tasks. 43 thousand lines of COBOL. TCP servers, PNG decoding, ZIP extraction, LLM agent loops with function calling, rocket navigation with radar avoidance, time travel. All in pure COBOL.

## What is AI_devs?

[AI_devs 4](https://www.aidevs.pl/) is a 5-week intensive program focused on building production-ready AI solutions. Over 10,000 graduates. Topics span LLM programming, AI agents, function calling, multimodal AI, RAG, context engineering, and prompt optimization.

Created by **Adam Gospodarczyk**, **Jakub Mrugalski**, and **Mateusz Chrobok**.

## Technical Highlights

- **Pure COBOL** — every line of logic is GnuCOBOL. Zero Python, zero shell scripts, zero other languages
- **TCP HTTP server** built from POSIX sockets via GnuCOBOL FFI
- **PNG image processing** — parse chunks, inflate/deflate, unfilter scanlines, crop, resize, binarize, rotate, re-encode, and base64 encode
- **ZIP extraction** — decompress and extract archives natively
- **LLM function calling** with agent loops — the LLM orchestrates via tool_use, COBOL executes deterministically
- **Deterministic computation** — haversine distance, pathfinding, anomaly detection, and keyword scoring all in COBOL
- **13 shared copybooks** — reusable modules for env loading, JSON parsing/escaping/unescaping, hub submission, and tool response parsing
- **Full CI/CD** — 25 sequential GitHub Actions jobs with rate limiting, flag detection, and secret masking

## Credits

This project was built as part of the [AI_devs 4](https://www.aidevs.pl/) course.

Built by [Daniel Nowicki](https://github.com/ddnowicki) with the help of Opus 4.6.

---

<div align="center">
<sub>No mainframes were harmed in the making of this project.</sub>
</div>
