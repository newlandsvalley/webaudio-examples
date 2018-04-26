module Audio.Example.Volume (example) where

import Prelude (Unit, bind, pure, unit, ($), (+), (-), (*), (<))
import Audio.WebAudio.Types (AudioContext, AudioBuffer, AudioBufferSourceNode, GainNode, AUDIO, connect)
import Audio.WebAudio.BaseAudioContext (newAudioContext, createBufferSource, createGain, currentTime, destination)
import Audio.WebAudio.AudioBufferSourceNode (setBuffer, startBufferSource, stopBufferSource, setLoop)
import Audio.WebAudio.GainNode (setGain)
import Audio.Util
import Control.Monad.Aff (Aff, delay)
import Data.Time.Duration (Milliseconds(..))
import Network.HTTP.Affjax (AJAX)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)

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
setup :: ∀ eff.
  AudioContext
  -> Aff
      ( ajax :: AJAX
      , audio :: AUDIO
      | eff
      )
      VolumeController
setup ctx = do
  buffer <- loadSoundBuffer ctx "wav/techno.wav"
  liftEff $ configure ctx buffer

-- | configure the nodes
configure :: ∀ eff.
     AudioContext
  -> AudioBuffer
  -> Eff
      ( audio :: AUDIO
      | eff
      )
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
  _ <- startBufferSource (startTime + 0.1) src
  pure { source : src, gain : gain}

-- | change the volume
changeVolume :: ∀ eff.
     AudioContext
  -> VolumeController
  -> Number
  -> Eff
      ( audio :: AUDIO
      | eff
      )
      Unit
changeVolume ctx controller fraction = do
  -- Let's use an x*x curve (x-squared) since simple linear (x) does not
  -- sound as good.
  setGain (fraction * fraction) controller.gain

-- | stop the sound immediately
stop :: ∀ eff.
     AudioContext
  -> VolumeController
  -> Eff
      ( audio :: AUDIO
      | eff
      )
        Unit
stop ctx controller = do
  startTime <- currentTime ctx
  stopBufferSource (startTime) controller.source

-- | gradually reduce the volume level every second (or so)
quieten :: ∀ eff.
     AudioContext
  -> VolumeController
  -> Number
  -> Aff
     ( ajax :: AJAX
     , audio :: AUDIO
     | eff
     )
     Unit
quieten ctx controller fraction =
  if fraction < 0.0 then do
    _ <- liftEff $ stop ctx controller
    pure unit
  else do
    -- we'll mimic user input of pressing a 'quieten' key with delays
    _ <- delay (Milliseconds $ 1000.0)
    _ <- liftEff $ changeVolume ctx controller fraction
    quieten ctx controller (fraction - 0.1)

-- | the complete example
example :: ∀ eff.
  Aff
    ( ajax :: AJAX
    , audio :: AUDIO
    | eff
    )
    Unit
example = do
  ctx <- liftEff newAudioContext
  controller <- setup ctx
  quieten ctx controller 1.0
