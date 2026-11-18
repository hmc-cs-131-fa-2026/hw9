# CS 131 HW 9: Raskell

Starter code for HW 9. The full assignment is on the course site: <https://www.cs.hmc.edu/~bang/cs131/assignments/hw09/>.

Raskell is a small, Racket-like language, and this repository holds its interpreter, written in Haskell. In this
assignment you extend the interpreter, then write a library and a program in Raskell itself.

## Files you edit

| File | What goes in it |
|---|---|
| `RaskEval.hs` | The evaluator. Everything to fill in is marked `TODO (Stage N)`. |
| `rasklib/list-library.rask` | `reverse` and `filter`, written in Raskell (Stage 5). |
| `programs/collatz.rask` | Your Collatz program (Stage 6). |

## Provided files

| File | What it is |
|---|---|
| `RaskAST.hs` | The `SExpr` and `Value` datatypes and printing. |
| `RaskParser.hs`, `Rask.hs` | The parser, and the REPL and file runner. |
| `ParserCombinators.hs`, `ParserBase.hs` | **PROVIDED: DO NOT EDIT.** The course's parser-combinator library. |
| `rask`, `run-tests` | Run Raskell (compiling it when needed), and run the tests. |
| `tests/` | **DO NOT EDIT.** The tests, in one folder per stage. |
| `programs/swap.rask`, `programs/square.rask` | Two small sample programs. `swap.rask` runs on the starter; `square.rask` needs Stage 1. |

## Running (on cs131.cs.hmc.edu)

```bash
./rask                                    # the REPL (:q to quit)
./rask --file program.rask                # run a program
./rask --lib rasklib/list-library.rask --file programs/collatz.rask 6
                                          # load your library, run collatz.rask on 6
```

## Testing

```bash
./run-tests                    # all the tests
./run-tests 02                 # one stage
./run-tests --reference        # the same tests, with the reference interpreter instead of yours
./run-tests > testing.log      # save the results to submit
```

`rask-reference` is the finished interpreter, installed on the course server. It takes the same arguments as `./rask`.

## Submitting

On Gradescope, under **HW 09: Raskell**: paste the whole of `testing.log`, upload `RaskEval.hs`,
`rasklib/list-library.rask`, and `programs/collatz.rask`, and answer the questions.
