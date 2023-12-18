# Connect Four

Members:
Jino Tesauro,
Caleb Aguiar,
Samuel Vader,
Natalie Marie Tiglao,
Clarissa Garcia

This is our implementation of Connect Four in Haskell.

# Project Grade:         76/100
## Functionality               57/73
* Game mechanics:              20
  * Reading the first character for the player, rather than first line, is deeply confusing.
  * Hard to evaluate things with only one test game!
* Exact game solver:           15
  * Seems to work well[
* Cut-off depth solver:        6/13
  * fromMaybe instead of catMaybes when using makeMove in whoMightWin
  * doesn't check for the game to be over
  * you always use maximum on the top level, but minimax in the recursive function
* Evaluation function:         0/2
  * you just filter == color, you don't check for adjacency or interruption by other colors!
* Avoiding unnecessary work:   0/3
* Command-line interface:      9/10
  * Crashes on some inputs, doesn't print quite up to specs.
* Move and verbose flags:      7/5
  * -m and -v seem to interact oddly: by default you pretty print the input and output,  and -v turns OFF
    pretty printing for output but not printing hte input board.
  * Interactive flag works, but can't block a win. Doesn't support -w flag. (+3/5)
* Error-handling:              0/5
  * None on readGame, bestMove, whoMightWin. Nice safeIndexing.
  * Refutable patterns in assignment [one, two, three, four]
  * Missing a filename crashes the program

## Design                      19/27
* Well-designed data types:    8
  * I like the Rating.
* Well-decomposed functions:   10
  * way too many top-level helper functions, resulting in some very long names. 
  * A lot of repeated code verticalWin/diagonalWin/horizontalWin are all essentially identical.
  * gameWin and unsafeGameWin are reversed in names, or at least confusing.
* Good module decomposition:   2
  * Game.hs is VERY overcrowded, especially with test boards.
* Good variable names:         2
* Efficient/idiomatic code:    5
  * diagonals are shlemiely.
