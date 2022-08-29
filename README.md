# How to use this

## Step 1: Load this project into GHCI

You'll need to install `GHC` (the Glascow Haskell Compiler, i.e. Haskell)
and `cabal-install` (a dependency manager for Haskell).
That's described here:

https://www.haskell.org/downloads/

Once that works, download this project,
and run `cabal repl` from the root of the project.
That will drop you into GHCI.


## Step 2: Once you've loaded the project in GHCI

Evaluate `go edo right_step downright_step`, where
* `edo` is your chosen division of the octave
* `right_step` is the interval when moving from left to right
* `downright_step` is the interval when moving from up-left to down-right
If any of those numbers is negative, you'll need to wrap it in parentheses.

Check out the new file in the `output/` folder.
Its name will correspond to the arguments you provided to `go`.

This program only defines midi notes, channels and colors.
I don't understand the other voodoo in an .ltn file,
so I just append the data in `data/tail.txt`.
You can use the Lumatone editor to change those things.


# The examples in the `sample-output/` folder

`31edo-5r-3dl.ltn` is 31-edo Bosanquet: Moving right raises the pitch by 5\31, and moving down-right raises it by 3\31.

`41edo-7r-3dl.ltn` is 41-edo Bosanquet: Moving right raises the pitch by 7\41, and moving down-right raises it by 3\41.

`41edo-6r-1dl.ltn` is the reason I wrote this program.
It's almost as compact as Bosanquet but a little wider and a little flatter,
to avoid the need for weird and/or impossible voicings.
