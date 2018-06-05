module Audio.Example.Volume (example) where

import Prelude (Unit, bind, pure, unit, ($), (+), (-), (*), (<))
import Data.Maybe (Maybe(..))
import Audio.WebAudio.Types (AudioContext, AudioBuffer, AudioBufferSourceNode, GainNode, connect)
import Audio.WebAudio.BaseAudioContext (newAudioContext, createBufferSource, createGain, currentTime, destination)
import Audio.WebAudio.AudioBufferSourceNode (StartOptions, setBuffer, startBufferSource, stopBufferSource, setLoop)
import Audio.WebAudio.GainNode (setGain)
import Audio.Util
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay)
import Effect (Effect)
import Effect.Class (liftEffect)


-- | Volume example
-- | for the original webaudioapi javascript that influenced this example see
-- | https://github.com/borismus/webaudioapi.com/tree/master/content/posts/volume
-- | The JavaScript obviously does a good deal of mutation in place.
-- | Also it mentions noteOn and noteOff which I can no longer find in the spec -
-- | presumably these names were a precursor to start and stop and I've ignored them.

-- | this is the state that we'll pass through the various funtions
type VolumeController =
  { source :: AudioBufferSourceNode
  , gain :: GainNode
  }

-- | load the buffer and complete the configuration
setup ::
  AudioContext
  -> Aff VolumeController
setup ctx = do
  buffer <- loadSoundBuffer ctx "wav/techno.wav"
  liftEffect $ configure ctx buffer

-- | configure the nodes
configure ::
     AudioContext
  -> AudioBuffer
  -> Effect
        { source :: AudioBufferSourceNode
        , gain :: GainNode
        }
configure ctx buf = do
  src <- createBufferSource ctx
  _ <- setLoop true src
  gain <- createGain ctx
  dst <- destination ctx
  _ <- connect src gain
  _ <- connect gain dst
  _ <- setBuffer buf src
  startTime <- currentTime ctx
  let
    whenOption :: StartOptions
    whenOption = { when: Just (startTime + 0.1),  offset: Nothing, duration: Nothing }
  _ <- startBufferSource whenOption src
  pure { source : src, gain : gain}

-- | change the volume
changeVolume ::
     AudioContext
  -> VolumeController
  -> Number
  -> Effect Unit
changeVolume ctx controller fraction = do
  -- Let's use an x*x curve (x-squared) since simple linear (x) does not
  -- sound as good.
  setGain (fraction * fraction) controller.gain

-- | stop the sound immediately
stop ::
     AudioContext
  -> VolumeController
  -> Effect Unit
stop ctx controller = do
  startTime <- currentTime ctx
  stopBufferSource (startTime) controller.source

-- | gradually reduce the volume level every second (or so)
quieten ::
     AudioContext
  -> VolumeController
  -> Number
  -> Aff Unit
quieten ctx controller fraction =
  if fraction < 0.0 then do
    _ <- liftEffect $ stop ctx controller
    pure unit
  else do
    -- we'll mimic user input of pressing a 'quieten' key with delays
    _ <- delay (Milliseconds $ 1000.0)
    _ <- liftEffect $ changeVolume ctx controller fraction
    quieten ctx controller (fraction - 0.1)

-- | the complete example
example :: Aff Unit
example = do
  ctx <- liftEffect newAudioContext
  controller <- setup ctx
  quieten ctx controller 1.0
