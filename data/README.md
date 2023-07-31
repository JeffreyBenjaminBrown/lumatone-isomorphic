Each .ltn file has some data resembling `tail.txt` at the end of it.
To me, most of those parameters are gibberish,
but `LumaTouchConfig` is important.
The file `LumaTouchConfig_choices.txt`
lists some ways to configure it that I like,
along with code to generate each.

The 64 least velocity values of the Lumatone
(which are inputs to the function that `LumaTouchConfig` defines)
are hard for my fingers to distinguish between,
so I'm only allocating around 10 output slots to those inputs.
The harder 64 input values are spread smoothly across the remaining range,
from 11 to 127.
