module Types where


type Board = Int -- ^ The Lumatone has five boards, numbered 0 through 4.

type Key = Int -- ^ Each Board has 56 keys, numbered 0 to 55.
-- Refer to this picture on p. 28 of the Lumatone manual for which is where:
-- [[~/many-small/music-making/lumatone/2022-08-27-stl4uf7J.one-lumatone-octave.png]]

data KeyData = KeyData {
  keyChannel :: MidiChannel,
  keyNote :: MidiNote,
  keyColor :: Color }
  deriving (Show, Eq, Ord)

-- | These are computer graphics-style coordinates:
-- x runs left to right, and y runs *up* to *down*.
type X = Int
type Y = Int

type Edo         = Int
type EdoNote     = Int -- ^ Unbounded int.
type MidiNote    = Int -- ^ A value in [0,127].
type MidiChannel = Int -- ^ A value in [0,15], I guess.
                       -- TODO : Right?

type Color = String
type Filename = String
