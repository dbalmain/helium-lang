# Helium

The implementation of the Helium language — a small, embeddable, typed scripting language built chapter-by-chapter in [the book](https://github.com/dbalmain/helium-book).

The `main` branch is a skeleton. The real code lives on the chapter branches below.

## Chapters

| Branch | Chapter |
| --- | --- |
| [`chapter/001`](https://github.com/dbalmain/helium-lang/tree/chapter/001) | 1 — Arithmetic expressions: parse and evaluate `1 + 2 * 3` |
| [`chapter/002`](https://github.com/dbalmain/helium-lang/tree/chapter/002) | 2 — Variables and let bindings: `let x = 5 in x + 1` |

## Toolchain

Haskell, built with Cabal and Nix. With [direnv](https://direnv.net/) installed, `cd`-ing into any chapter branch drops you into a working dev environment. Without direnv, `nix develop` does the same. Then `cabal run` starts the REPL.
