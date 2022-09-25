# What these layouts are

## `31edo.5right.3downleft.ltn` is standard 31-edo Bosanquet.

It's more readable than
the 31-edo layout that comes with the Lumatone,
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
go 41 7 3 0 1
go 41 7 3 9 0
```

If something is hard to play in one of them
(because your hand is up against the top or bottom edge of the keyboard),
it should be easier to play in the other,
which is shifted vertically by about half a keyboard.
(9 EDO steps not always half a keyboard's width --
that depends on both the EDO and the layout.
In the case of 41-edo Bosanquet, it is.)

One of the layouts is shifted 9 MIDI notes up.
The other is shifted 1 MIDI channel up,
so that they are in the same octave.
The reason that was necessary is described in the main README.


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
go 53 9 4 0  1
go 53 9 4 11 0
```
