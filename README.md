<h1 align="center">
  🧭 Declarative Programming — Deep Robotics @ NSU
</h1>

<p align="center">
  <em>Hands‑on guide & assignment solutions for Haskell + SQLite adventures in the Deep Robotics curriculum at Novosibirsk State University.</em>
</p>

<p align="center">
  <img src="https://img.shields.io/badge/status-%F0%9F%9A%A7%20work&nbsp;in&nbsp;progress-yellow?style=for-the-badge"/>
  <img src="https://img.shields.io/badge/language-Haskell-purple?style=for-the-badge"/>
  <img src="https://img.shields.io/badge/language-SQL%20%28SQLite%29-lightgrey?style=for-the-badge"/>
  <img src="https://img.shields.io/badge/university-NSU-blue?style=for-the-badge"/>
  <img src="https://img.shields.io/badge/track-Deep&nbsp;Robotics-critical?style=for-the-badge"/>
</p>

---

## Table of Contents

1. ❓ [Why this repo?](#why-this-repo)
2. 🗂️ [Repository layout](#repository-layout)
3. 🚀 [Getting started](#getting-started)
4. 📚 [Semester breakdown](#semester-breakdown)
5. 🛠️ [Key resources](#key-resources)
6. 📄 [License](#license)

---

<h2 id="why-this-repo">❓ Why this repo?</h2>

> *“Declarative programming teaches you to tell the computer **what** you want, not **how** to do it.”*
>
> — every functional‑programming TA, probably.

* 🎯 **Single source of truth.** Lecture slides, lesson snippets, and all graded homeworks sit side‑by‑side.
* 🧑‍💻 **Real student code.** Browse working solutions that actually passed the NSU autograder.
* 🏗️ **Step‑by‑step evolution.** From first `map` in Haskell to complex JOIN cascades in SQL — follow the commit history.
* 🗺️ **Deep Robotics context.** Examples geared toward robotics data‑flows (sensor streams, kinematics tables, etc.).

---

<h2 id="repository-layout">🗂️ Repository layout</h2>

```text
DeclarativeProgramming/
├── 1_sem/                  # semester 1 — pure Haskell
│   ├── homeworks/
│   ├── lectures/
│   ├── lessons/
│   └── tests/
├── 2_sem/                  # semester 2 — Haskell ⇄ SQLite combo
│   ├── homeworks/
│   │   ├── haskell/
│   │   └── sql/
│   ├── lectures/
│   │   ├── haskell/
│   │   └── sql/
│   └── lessons/
│       ├── haskell/
│       └── sql/
├── haskell_book.pdf        # friendly 1k‑page door‑stopper
├── LICENSE
└── README.md               # you’re reading it
```

---

<h2 id="getting-started">🚀 Getting started</h2>

### 0. Prerequisites

| Tool           | Tested version | Notes                              |
| -------------- | -------------- | ---------------------------------- |
| **GHC**        | 9.6+           | via `ghcup` or Stack               |
| **Stack**      | 2.15+          | easiest way to build & run Haskell |
| **SQLite**     | 3.46+          | bundled on most \*nix distros      |

### 1. Clone the repo

```bash
git clone https://github.com/McAshesha/DeclarativeProgramming.git
cd DeclarativeProgramming
```

### 2. Explore a Haskell task

```bash
cd 1_sem/homeworks/1-simple_function
runhaskell solution.hs   # or :l in ghci
```
or
```
ghci
:l solution.hs
```

### 3. Run an SQL script

```bash
cd 2_sem/homeworks/sql/03-joins
sqlite3 wares.sqlite3 < solution.sql
```

---

<h2 id="semester-breakdown">📚 Semester breakdown</h2>

| Semester | Focus                                   | Highlights                                                               |
| -------- | --------------------------------------- | ------------------------------------------------------------------------ |
| **1**    | Functional programming with **Haskell** | recursion 💫 • higher‑order funcs 🧩 • typeclasses 🔠                    |
| **2**    | Data wrangling in **Haskell + SQLite**  | monad ⇄ database bridges 🔗 • window functions 🪟 • query optimisation ⚡ |

---

<h2 id="key-resources">🛠️ Key resources</h2>

| Topic            | Link                                                 | Why it matters                        |
| ---------------- | -----------------------------------------------------| ------------------------------------- |
| Hoogle           | [`Hoogle`](https://hoogle.haskell.org/)              | Instant type‑directed search engine   |
| SQLite docs      | [`Docs`](https://www.sqlite.org/docs.html)           | Official SQL grammar & pragmas        |
| Learn You        | [`Haskell`](http://learnyouahaskell.com/)            | Beginner‑friendly FP walkthrough      |
| Haskell Book     | [`Book`](hakell_book.pdf)                            | Minimal Haskell ↔ SQLite bridge (WIP) |

---

<h2 id="license">📄 License</h2>

This repository is released under the **GNU License**.
See [`LICENSE`](LICENSE) for the full text.

---

> *Found a typo or have a smarter query plan? File an issue or drop a PR!* ✨
