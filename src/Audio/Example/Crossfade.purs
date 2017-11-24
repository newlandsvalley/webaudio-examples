module Audio.Example.Crossfade (example) where

import Prelude (Unit, bind, pure, ($), (+), (-), (*), (>))
import Data.Array.Partial (head, last)
import Partial.Unsafe (unsafePartial)
import Audio.WebAudio.Types (AudioContext, AudioBuffer, AudioBufferSourceNode, GainNode, WebAudio)
import Audio.WebAudio.AudioContext (makeAudioContext, createBufferSource, createGain, connect, currentTime, destination)
import Audio.WebAudio.AudioBufferSourceNode (setBuffer, startBufferSource, stopBufferSource, setLoop)
import Audio.WebAudio.GainNode (setGain)
import Audio.Util
import Control.Monad.Aff (Aff, delay)
import Data.Time.Duration (Milliseconds(..))
import Network.HTTP.Affjax (AJAX)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
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
setup :: ∀ eff.
  AudioContext
  -> Aff
      ( ajax :: AJAX
      , wau :: WebAudio
      | eff
      )
      ControllerPair
setup ctx = do
  buffers <- loadSoundBuffers ctx ["wav/drums.wav", "wav/organ.wav" ]
  let
    drums = unsafePartial $ head buffers
    organ = unsafePartial $ last buffers
  left <- liftEff $ configureSource ctx drums
  right <- liftEff $ configureSource ctx organ
  pure { left : left, right : right }

-- | configurew the nodes
configureSource :: ∀ eff.
     AudioContext
  -> AudioBuffer
  -> Eff
      ( wau :: WebAudio
      | eff
      )
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
changeVolume :: ∀ eff.
     AudioContext
  -> VolumeController
  -> Number
  -> Eff
      ( wau :: WebAudio
      | eff
      )
      Unit
changeVolume ctx controller fraction = do
  setGain (fraction * fraction) controller.gain


-- | start the playback
start :: ∀ eff.
     AudioContext
  -> ControllerPair
  -> Eff
      ( wau :: WebAudio
      | eff
      )
      Unit
start ctx controllerPair = do
  now <- currentTime ctx
  _ <- startBufferSource now controllerPair.left.source
  startBufferSource now controllerPair.right.source

-- | stop the sound immediately
stop :: ∀ eff.
     AudioContext
  -> ControllerPair
  -> Eff
      ( wau :: WebAudio
      | eff
      )
        Unit
stop ctx controllerPair = do
  now <- currentTime ctx
  _ <- stopBufferSource now controllerPair.left.source
  stopBufferSource now controllerPair.right.source

-- | This assumes we start with a fraction of 0.0 (drums only) and gradually
-- | increade it until it reaches 1.0 (organ only) after which we stop
crossfade :: ∀ eff.
     AudioContext
  -> ControllerPair
  -> Number
  -> Aff
     ( ajax :: AJAX
     , wau :: WebAudio
     | eff
     )
     Unit
crossfade ctx controllerPair fraction =
  if fraction > 1.0 then do
    liftEff $ stop ctx controllerPair
  else do
    -- Use an equal-power crossfading curve:
    let
      gainl = cos (fraction * 0.5 * pi)
      gainr = cos ((1.0 - fraction) * 0.5 * pi)
    -- we'll mimic user input of pressing a 'crossfade' key with delays
    _ <- delay (Milliseconds $ 1000.0)
    _ <- liftEff $ changeVolume ctx controllerPair.left gainl
    _ <- liftEff $ changeVolume ctx controllerPair.right gainr
    crossfade ctx controllerPair (fraction + 0.05)

-- | the complete example
example :: ∀ eff.
  Aff
    ( ajax :: AJAX
    , wau :: WebAudio
    | eff
    )
    Unit
example = do
  ctx <- liftEff makeAudioContext
  controllerPair <- setup ctx
  -- mute the second source
  _ <- liftEff $ changeVolume ctx controllerPair.right  0.0
  -- start them off
  _ <- liftEff $ start ctx controllerPair
  -- crossfade between them
  crossfade ctx controllerPair 0.0
