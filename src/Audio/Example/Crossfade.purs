module Audio.Example.Crossfade (example) where

import Prelude (Unit, bind, pure, ($), (+), (-), (*), (>))
import Data.Array.Partial (head, last)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (Maybe(..))
import Audio.WebAudio.Types (AudioContext, AudioBuffer, AudioBufferSourceNode, GainNode,  connect)
import Audio.WebAudio.BaseAudioContext (newAudioContext, createBufferSource, createGain, currentTime, destination)
import Audio.WebAudio.AudioBufferSourceNode (StartOptions, setBuffer, startBufferSource, stopBufferSource, setLoop)
import Audio.WebAudio.GainNode (setGain)
import Audio.Util
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay)
import Effect (Effect)
import Effect.Class (liftEffect)
import Math (cos, pi)

-- | Crossfade example
-- | for the original webaudioapi javascript that influenced this example see
-- | https://github.com/borismus/webaudioapi.com/tree/master/content/posts/crossfade


-- | the controller for one instrument
type VolumeController =
  { source :: AudioBufferSourceNode
  , gain :: GainNode
  }

type ControllerPair =
  { left :: VolumeController
  , right :: VolumeController
  }

-- | load the buffer and complete the configuration
setup ::
  AudioContext
  -> Aff ControllerPair
setup ctx = do
  buffers <- loadSoundBuffers ctx ["wav/drums.wav", "wav/organ.wav" ]
  let
    drums = unsafePartial $ head buffers
    organ = unsafePartial $ last buffers
  left <- liftEffect $ configureSource ctx drums
  right <- liftEffect $ configureSource ctx organ
  pure { left : left, right : right }

-- | configurew the nodes
configureSource ::
     AudioContext
  -> AudioBuffer
  -> Effect
        { source :: AudioBufferSourceNode
        , gain :: GainNode
        }
configureSource ctx buf = do
  src <- createBufferSource ctx
  _ <- setLoop true src
  gain <- createGain ctx
  dst <- destination ctx
  _ <- connect src gain
  _ <- connect gain dst
  _ <- setBuffer buf src
  pure { source : src, gain : gain}

-- | change the volume
changeVolume ::
     AudioContext
  -> VolumeController
  -> Number
  -> Effect Unit
changeVolume ctx controller fraction = do
  setGain (fraction * fraction) controller.gain


-- | start the playback
start ::
     AudioContext
  -> ControllerPair
  -> Effect Unit
start ctx controllerPair = do
  now <- currentTime ctx
  let
    whenOption :: StartOptions
    whenOption = { when: Just (now + 0.1),  offset: Nothing, duration: Nothing }
  _ <- startBufferSource whenOption controllerPair.left.source
  startBufferSource whenOption controllerPair.right.source

-- | stop the sound immediately
stop ::
     AudioContext
  -> ControllerPair
  -> Effect  Unit
stop ctx controllerPair = do
  now <- currentTime ctx
  _ <- stopBufferSource now controllerPair.left.source
  stopBufferSource now controllerPair.right.source

-- | This assumes we start with a fraction of 0.0 (drums only) and gradually
-- | increase it until it reaches 1.0 (organ only) after which we stop
crossfade ::
     AudioContext
  -> ControllerPair
  -> Number
  -> Aff  Unit
crossfade ctx controllerPair fraction =
  if fraction > 1.0 then do
    liftEffect $ stop ctx controllerPair
  else do
    -- Use an equal-power crossfading curve:
    let
      gainl = cos (fraction * 0.5 * pi)
      gainr = cos ((1.0 - fraction) * 0.5 * pi)
    -- we'll mimic user input of pressing a 'crossfade' key with delays
    _ <- delay (Milliseconds $ 1000.0)
    _ <- liftEffect $ changeVolume ctx controllerPair.left gainl
    _ <- liftEffect $ changeVolume ctx controllerPair.right gainr
    crossfade ctx controllerPair (fraction + 0.05)

-- | the complete example
example :: Aff  Unit
example = do
  ctx <- liftEffect newAudioContext
  controllerPair <- setup ctx
  -- mute the second source
  _ <- liftEffect $ changeVolume ctx controllerPair.right  0.0
  -- start them off
  _ <- liftEffect $ start ctx controllerPair
  -- crossfade between them
  crossfade ctx controllerPair 0.0
