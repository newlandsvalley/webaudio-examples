module Audio.Example.Filter (example) where

import Prelude (Unit, bind, pure, unit, ($), (-), (+), (*), (/), (>))
import Data.Maybe (Maybe(..))
import Audio.WebAudio.Types (AudioContext, AudioBuffer, AudioBufferSourceNode, BiquadFilterNode,  connect)
import Audio.WebAudio.BaseAudioContext (newAudioContext, createBufferSource, createBiquadFilter,
  currentTime, destination, sampleRate)
import Audio.WebAudio.AudioBufferSourceNode (StartOptions, setBuffer, startBufferSource, stopBufferSource, setLoop)
import Audio.WebAudio.AudioParam (setValue)
import Audio.WebAudio.BiquadFilterNode (BiquadFilterType(..), filterFrequency, setFilterType)
import Audio.Util
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay)
import Effect (Effect)
import Effect.Class (liftEffect)

import Math (log, ln2, pow)

-- | Filter example
-- | for the original webaudioapi javascript that influenced this example see
-- | https://github.com/borismus/webaudioapi.com/blob/master/content/posts/filter/filter-sample.js

-- | the controller
type FilterController =
  { source :: AudioBufferSourceNode
  , filter :: BiquadFilterNode
  }

-- | load the buffer and complete the configuration
setup ::
  AudioContext
  -> Aff FilterController
setup ctx = do
  buffer <- loadSoundBuffer ctx "wav/techno.wav"
  filterController <- liftEffect $ configure ctx buffer
  pure filterController

-- | configure the nodes
configure ::
     AudioContext
  -> AudioBuffer
  -> Effect   FilterController
configure ctx buf = do
  src <- createBufferSource ctx
  _ <- setLoop true src
  filter <- createBiquadFilter ctx
  _ <- setFilterType Highpass filter
  freqParam <- filterFrequency filter
  _ <- setValue 5000.0 freqParam
  dst <- destination ctx
  _ <- connect src filter
  _ <- connect filter dst
  _ <- setBuffer buf src
  pure { source : src, filter : filter}

-- | change the frequency
changeFrequency ::
     AudioContext
  -> FilterController
  -> Number
  -> Effect Unit
changeFrequency ctx controller setting = do
  sRate <- sampleRate ctx
  let
    -- Clamp the frequency between the minimum value (40 Hz) and half of the
    -- sampling rate (the Nyquist frequency).
    minValue = 40.0
    maxValue = sRate / 2.0
    -- Logarithm (base 2) to compute how many octaves fall in the range.
    numberOfOctaves = log(maxValue / minValue) / ln2
    -- Compute a multiplier from 0 to 1 based on an exponential scale.
    multiplier = pow 2.0 (numberOfOctaves * (setting - 1.0))
  filterValue <- filterFrequency controller.filter
  -- Get back to the frequency value between min and max.
  _ <- setValue (maxValue * multiplier) filterValue
  pure unit

-- | start the playback
start ::
     AudioContext
  -> FilterController
  -> Effect Unit
start ctx filterController = do
  now <- currentTime ctx
  let
    whenOption :: StartOptions
    whenOption = { when: Just now,  offset: Nothing, duration: Nothing }
  startBufferSource whenOption filterController.source

-- | stop the playback
stop ::
     AudioContext
  -> FilterController
  -> Effect Unit
stop ctx filterController = do
  now <- currentTime ctx
  stopBufferSource now filterController.source


-- | This assumes we start with a setting of 0.0 (no filter) and gradually
-- | increase it until it reaches 1.0 (most frequencies filtered) after which we stop
-- | the filtered frequencies start at the low end of the sp
moveFilterFreq ::
     AudioContext
  -> FilterController
  -> Number
  -> Aff Unit
moveFilterFreq ctx filterController fraction =
  if fraction > 1.0 then do
    liftEffect $ stop ctx filterController
  else do
    -- we'll mimic user input of pressing a 'change filter frequency' key with delays
    _ <- delay (Milliseconds $ 1000.0)
    _ <- liftEffect $ changeFrequency ctx filterController fraction
    moveFilterFreq ctx filterController (fraction + 0.05)

-- | the complete example
example :: Aff Unit
example = do
  ctx <- liftEffect newAudioContext
  filterController <- setup ctx
  -- start it off
  _ <- liftEffect $ start ctx filterController
  -- (play for a second unfiltered)
  -- _ <- delay (Milliseconds $ 1000.0)
  -- play about with the filter frequency
  moveFilterFreq ctx filterController 0.0
