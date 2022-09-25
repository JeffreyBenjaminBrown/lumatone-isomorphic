# Purpose

Make isomorphic Lumatone layouts,
by providing only the following four arguments, in order:
* edo :: A positive integer. The number of divisions of the octave you want.
* right_step :: An integer. The difference in edo steps from any button to its right neighbor.
* downright_step :: An integer. The difference in edo steps from any button to its down-left neighbor.
* note_shift :: A nonnegative integer. A number of MIDI notes to add to every value. If this is zero, the top-left key will send MIDI note zero.
* channel_shift = A nonnegative integer. A number of MIDI channels to add to every value. If this is zero, the lowest channel will be 1.

You can also define colors to assign to each EDO value,
rather than to each key,
by editing the definitions of `default_color` and `color_map`
in the file `app/Colors.hs`.

## A limitation

This program only defines midi notes, channels and colors.
I don't understand the other voodoo in an .ltn file,
so I just append the data in `data/tail.txt`.
You can use the Lumatone editor to change those things,
or you can edit that file itself.


# How to use it

## Step 1: Load this project into GHCI

You'll need to install `GHC` (the Glascow Haskell Compiler, i.e. Haskell)
and `cabal-install` (a dependency manager for Haskell).
That's described here:

https://www.haskell.org/downloads/

Once that works, download this project,
and run `cabal repl` from its root folder.
That will drop you into GHCI.

(There are other ways to run Haskell, too, most notably Stack.)

## Step 2: Make a layout with it

Evaluate the expression `go edo right_step downright_step note_shift channel_shift`. Write the word `go` literally, but substitute numbers for each of the other terms, as described in the introduction to this README. Any negative number will need to be wrapped in parenthesees.

Some data will print to the screen. Ignore it.

Check out the new file in the `output/` folder.
Its name will correspond to the arguments you provided to `go`.


# There's some example output in sample-output.

See the README in that folder to know more about those files.


# A surprising problem that looks like a bug but isn't

## The problem

Consider two nearly identical layouts, differing only in the `note_shift` value:
```
go 41 7 3 0 0
go 41 7 3 9 0
```

One of these has 9 added to every MIDI note, and the other doesn't.
9 steps of 41-edo is less than a minor third,
so you might expect the first layout to be a minor third below the second.
Instead it's an octave higher than that.

## Why that happens

It results from the following facts:

* By default (i.e., if you set channel_shift to 0),
  the lowest MIDI channel will be 0.
* By default (i.e. if you set note_shift to 0),
  the key in the top-left corner will be assigned note 0.
* The key in the top-left corner is not necessarily
  the lowest note on the keyboard.

## The solution is easy.

If you run into a problem like this,
just use the `channel_shift` argument
to add 1 to all the channel values
in the layout that was an octave too low.
