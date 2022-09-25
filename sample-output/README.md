# What these layouts are

## 31edo-5r-3dl.ltn is standard 31-edo Bosanquet.

It's a little different from the one that comes with the Lumatone,
because each octave is on a separate MIDI channel,
and the MIDI note values lie entirely in the interval [0,30].

## There are two 41-edo Bosanquet layouts

They have these names:
```
41edo-7r-3dl+9.ltn
41edo-7r-3dl.ltn
```

and were generated via these commands:
```
go 41 7 3 9 0
go 41 7 3 0 1
```

They differ only by a midi_shift value --
0 in one of them, +9 in the other.
If something is hard to play in one of them
(because your hand is up against the top or bottom edge of the keyboard),
it should be easier to play in the other,
which is shifted vertically by about half a keyboard.
(9 is not always half a keyboard's width --
that depends on the Edo and the layout.
In the case of 41-edo Bosanquet, it is.)

## `41edo-6r--1dl.ltn` is a wide, flat 41-edo layout.

I thought it would be cool. Maybe it is.
I'm happy enough so far with 41-edo's Bosanquet
that I haven't really tried it much.
