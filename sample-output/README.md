# What these layouts are

## The 31-edo Bosanquet layout

It's called `31edo.5right.3downleft.ltn`.
It doesn't do anything the Lumatone's builtin
31-edo Bosanquet layout doesn't do,
but it was a useful proof of concept.

This layout offers the minor advantage of
being readable than that builtin layout,
because each octave is on a separate MIDI channel,
and its MIDI note values lie exclusively in the interval [0,30].

## The two 41-edo Bosanquet layouts

They have these names:
```
41edo.7right.3downleft.+1channels.ltn
41edo.7right.3downleft.+9notes.ltn
```

and were generated via these commands:
```
go 41 7 3 0 0
go 41 7 3 9 1
```

If something is hard to play in one of them
(because your hand is up against the top or bottom edge of the keyboard),
it should be easier to play in the other,
which is shifted vertically by about half a keyboard.
(9 EDO steps not always half a keyboard's width --
that depends on both the EDO and the layout.
In the case of 41-edo Bosanquet, it is.)

One of the layouts is shifted 9 MIDI notes and 1 MIDI channel up.
That's so the two layouts are in the same octave.
The reason that's necessary is described in the main README.


## `41edo.6right.-1downleft.ltn` is a wide, flat 41-edo layout.

I thought it would be cool. Maybe it is.
I'm happy enough so far with 41-edo's Bosanquet, though,
that I've barely used it, so I won't try to sell you on it.

This also comes in two versions,
shifted by a vertical half of a keyboard.
No channel-shift necessary in this case.

```
go 41 6 (-1) 0  0
go 41 6 (-1) 22 0
```

## The 46-edo layout

```
go 46 5 7 0 0
```

Wider and flatter than Bosanquet.
Very slightly diagonal (half a button every octave).


## The two 50-edo Bosanquet layouts

Again in two versions,
shifted by a vertical half of a keyboard.
No channel-shift necessary in this case.

```
go 50 8 5 0  0
go 50 8 5 20 0
```

## The two 53-edo Bosanquet layouts

```
go 53 9 4 0  0
go 53 9 4 11 1
```

# In brief, how I generated the latest data

(The passage below uses "00" instead of "0"
so that it can be sensibly line-sorted in Emacs.)

```
-- go edo right_step downright_step note_shift channel_shift
go 12 4    5  60 00
go 24 5    4  00 00
go 31 5    3  00 00
go 41 6 (-1)  00 00
go 41 6 (-1)  22 00
go 41 7    3   9  1
go 41 7    3  00 00
go 46 5    7  00 00
go 46 5    7  13 00
go 50 8    5  00 00
go 50 8    5  20 00
go 53 9    4  00 00
go 53 9    4  11  1
go 58 6    11 00 00
go 58 6    11 15 00
```
