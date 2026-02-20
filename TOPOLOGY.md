<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- TOPOLOGY.md — Project architecture map and completion dashboard -->
<!-- Last updated: 2026-02-20 -->

# Games & Trivia — Project Topology

## System Architecture

```
                        ┌─────────────────────────────────────────┐
                        │              PLAYER / CLIENT            │
                        │        (Web Browser, Desktop GUI)       │
                        └───────────────────┬─────────────────────┘
                                            │
                                            ▼
                        ┌─────────────────────────────────────────┐
                        │           GAME PROJECTS HUB             │
                        │  ┌───────────┐  ┌───────────┐  ┌───────┐│
                        │  │ airborne- │  │ befunge93-│  │ candy-││
                        │  │ submarine │  │ vault     │  │ crash ││
                        │  └───────────┘  └───────────┘  └───────┘│
                        │  ┌───────────┐  ┌───────────┐  ┌───────┐│
                        │  │ pow-the-  │  │ phantom-  │  │ safe- ││
                        │  │ game      │  │ metal     │  │ brute ││
                        │  └───────────┘  └───────────┘  └───────┘│
                        └───────────────────┬─────────────────────┘
                                            │
                                            ▼
                        ┌─────────────────────────────────────────┐
                        │             ASSET LAYER                 │
                        │      (Sprites, Levels, Narrative)       │
                        └─────────────────────────────────────────┘

                        ┌─────────────────────────────────────────┐
                        │          REPO INFRASTRUCTURE            │
                        │  Justfile / Nix     .machine_readable/  │
                        │  .bot_directives/   0-AI-MANIFEST.a2ml  │
                        └─────────────────────────────────────────┘
```

NOTE: IDApTIK game engine projects (idaptiky, IDApixiTIK) have been moved
to the dedicated `idaptik/` monorepo as of 2026-02-20.

## Completion Dashboard

```
COMPONENT                          STATUS              NOTES
─────────────────────────────────  ──────────────────  ─────────────────────────────────
GAME PROJECTS
  airborne-submarine-squadron       ██████████ 100%    Flight-sub physics stable
  pow-the-game                      ██████████ 100%    Impact mechanics verified
  befunge93-vault-cracker           ██████████ 100%    Esoteric logic stable
  phantom-metal-taste               ████░░░░░░  40%    Atmospheric layer in progress

REPO INFRASTRUCTURE
  Justfile Automation               ██████████ 100%    Build/Package tasks
  .machine_readable/                ██████████ 100%    STATE tracking active
  0-AI-MANIFEST.a2ml                ██████████ 100%    AI agent entry point

─────────────────────────────────────────────────────────────────────────────
OVERALL:                            ████████░░  ~80%   Stable games collection
```

## Update Protocol

This file is maintained by both humans and AI agents. When updating:

1. **After completing a component**: Change its bar and percentage
2. **After adding a component**: Add a new row in the appropriate section
3. **After architectural changes**: Update the ASCII diagram
4. **Date**: Update the `Last updated` comment at the top of this file

Progress bars use: `█` (filled) and `░` (empty), 10 characters wide.
Percentages: 0%, 10%, 20%, ... 100% (in 10% increments).
