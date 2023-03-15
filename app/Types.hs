module Types where


type Board = Int -- ^ The Lumatone has five Boards, numbered 0 through 4.

type Key = Int -- ^ Each Board has 56 keys, numbered 0 to 55.
-- Refer to the file "data/2022-08-27-stl4uf7J.one-lumatone-octave.png"
-- to see which button falls where.

data KeyData = KeyData {
  keyChannel :: MidiChannel,
  keyNote :: MidiNote,
  keyColorString :: ColorString }
  deriving (Show, Eq, Ord)

-- | These are computer graphics-style coordinates:
-- x runs left to right, and y runs *up* to *down*.
type X = Int
type Y = Int

type Edo         = Int
type EdoNote     = Int -- ^ Unbounded int.
type MidiNote    = Int -- ^ A value in [0,127].
type MidiChannel = Int -- ^ A value in [0,15], I guess.
                       -- TODO : Right? Or is it 1-16?

type ColorString = String
type Filename = String
