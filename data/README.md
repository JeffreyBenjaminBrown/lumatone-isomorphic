The velocity curve I'm using,
encoded in the part of `tail.txt` that looks like this:
```
LumaTouchConfig=1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 4 4 4 4 4 5 5 5 5 6 6 6 7 7 8 8 8 9 9 10 10 11 11 12 12 13 14 14 15 16 17 18 18 19 20 21 22 23 24 25 26 27 29 30 31 33 34 35 37 38 40 42 43 45 47 49 50 52 54 56 59 61 63 65 68 70 73 75 78 81 83 86 89 92 95 99 102 105 109 112 116 119 123 127
```
I generated using the command `polynomial 4 0.07`, which results in a curve with the following properties:
```
ghci> polynomial 4 0.07
[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
,3,3,3,3,3,3,3,3,4,4,4,4,4,5,5,5,5,6,6,6,7,7,8,8,8,9,9,10,10,11,11,12,12,13,14,14,15,16,17,18,18,19,20,21,22,23,24,25,26,27,29,30,31,33,34,35,37,38,40,42,43,45,47,49,50,52,54,56,59,61,63,65,68,70,73,75,78,81,83,86,89,92,95,99,102,105,109,112,116,119,123,127]
ghci> polynomial 4 0.07 !! 64
10
ghci> polynomial 4 0.07 !! 65
11
```

I like it that way because the 64 least velocity values of the Lumatone (inputs to this function) are really hard for me to distinguish between, so I'm only allocating 10 output slots to them. The harder 64 input values are spread smoothly across the remaining range, from 11 to 127.
