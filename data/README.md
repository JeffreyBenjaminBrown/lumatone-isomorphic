# What this data is for
Each .ltn file has some data resembling `tail.txt` at the end of it.
To me, most of those parameters are gibberish,
but `LumaTouchConfig` is important.
The file `LumaTouchConfig_choices.txt`
lists some ways to configure it that I like,
along with code to generate each.

# How to generate it
See `app/Touch.hs`.

# Why I changed it
The 64 least velocity values of the Lumatone
(which are inputs to the function that `LumaTouchConfig` defines)
are hard for my fingers to distinguish between,
so I'm only allocating around 10 output slots to those inputs.
The harder 64 input values are spread smoothly across the remaining range,
from 11 to 127.

# What I seem to like most
The two parameters I use to control the exponential curve
are `e` and `c`. It turns out `c` isn't very important,
so I've just left it at 1.
I've tried value for `e` of 3, 3.5 and 4,
and so far I like 4 the best.
