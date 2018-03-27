# Pandemic for Haskell

This is a toy implementation of the Pandemic game by Matt Learcock.

It was essentially motivated by the need to learn Lenses.


## How to play

    $ cabal build
    $ ghci Pandemic

    -- There are three difficulties available: Tutorial, Normal and Epic
    -- you need between 2 and 4 players. You can hand-pick roles or use "Nothing" to let it decide for you
    *Pandemic> g <- new Normal [("Alice",Just Medic), ("Bob",Nothing), ("Charlie", Just Researcher)]

    -- g is now an IORef to the game state. You can use "disp" to show the game board:
    *Pandemic> disp g
    .....

    -- use the "play" function to run actions over the state of the game:

    *Pandemic> play g $ moveTo newyork
    *Pandemic> play g $ giveCardTo "Alice" newyork

