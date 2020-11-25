# Antichess
An interface for playing the chess variant "Antichess", together with multiple AI implementations.

For general information about antichess, see [Wikipedia](https://en.wikipedia.org/wiki/Losing_chess).

As of now this project consists of two implementations, one in Python and one in Haskell.

Common features:
* Ability to play antichess against a human or computer opponent.
* A minimax AI for the computer player.

Python:
* Built up out of an older Python 2 chess program.
* Includes a graphical representation of the chessboard in the terminal.
* Gives more details about the AI's thinking process as it runs.

Haskell:
* Adapted from: http://www.cs.columbia.edu/~sedwards/classes/2019/4995-fall/reports/suicide-chess.pdf
* Chess modules copied from Stackage to enable access to hidden functions
* A slightly better implementation of the minimax AI.

Features to add:
* Better AIs based on neural networks.
