module Audio.Example.Feedback (example) where

import Prelude (Unit, bind, pure, ($), (+))
import Audio.WebAudio.Types (AudioContext, AudioBuffer, AudioBufferSourceNode, GainNode, WebAudio)
import Audio.WebAudio.AudioContext (makeAudioContext, createBufferSource, createDelay,
      createGain, connect, currentTime, destination)
import Audio.WebAudio.AudioBufferSourceNode (setBuffer, startBufferSource, stopBufferSource, setLoop)
import Audio.WebAudio.AudioParam (setValue)
import Audio.WebAudio.GainNode (gain)
import Audio.WebAudio.DelayNode (delayTime)
import Audio.Util
import Control.Monad.Aff (Aff, delay)
import Data.Time.Duration (Milliseconds(..))
import Network.HTTP.Affjax (AJAX)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)


-- | feedback example illustrating DelayNode inspired by
-- | http://blog.chrislowis.co.uk/2014/07/23/dub-delay-web-audio-api.html

type VolumeController =
  { source :: AudioBufferSourceNode
  , gain :: GainNode
  }

-- | load the buffer and complete the configuration
setup :: ∀ eff.
  AudioContext
  -> Aff
      ( ajax :: AJAX
      , wau :: WebAudio
      | eff
      )
      VolumeController
setup ctx = do
  buffer <- loadSoundBuffer ctx "ogg/chop.ogg"
  liftEff $ configure ctx buffer

-- | configurew the nodes
configure :: ∀ eff.
     AudioContext
  -> AudioBuffer
  -> Eff
      ( wau :: WebAudio
      | eff
      )
      VolumeController
configure ctx buf = do
  src <- createBufferSource ctx
  _ <- setBuffer buf src
  _ <- setLoop true src
  delay <- createDelay ctx
  delayParam <- delayTime delay
  -- delay by half a second
  _ <- setValue 0.5 delayParam
  feedback <- createGain ctx
  gainValue <- gain feedback
  -- create a diminishing echo effect each time we feed back
  _ <- setValue 0.8 gainValue
  dst <- destination ctx

  -- connect the feedback loop
  _ <- connect delay feedback
  _ <- connect feedback delay
  _ <- connect src delay
  -- connect both the original source and the delayed feedback to the destination
  _ <- connect src dst
  _ <- connect delay dst
  startTime <- currentTime ctx
  _ <- startBufferSource (startTime + 0.1) src
  pure { source : src, gain : feedback}

-- | start the sound
start :: ∀ eff.
     AudioContext
  -> VolumeController
  -> Eff
      ( wau :: WebAudio
      | eff
      )
        Unit
start ctx controller = do
  startTime <- currentTime ctx
  startBufferSource (startTime) controller.source

-- | stop the sound
stop :: ∀ eff.
     AudioContext
  -> VolumeController
  -> Eff
      ( wau :: WebAudio
      | eff
      )
        Unit
stop ctx controller = do
  now <- currentTime ctx
  stopBufferSource now controller.source

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
  controller <- setup ctx
  _ <- liftEff $ start ctx controller
  -- let it run for 5 seconds
  _ <- delay (Milliseconds 5000.0)
  -- why doesn't this stop ?? - presumably because the feedback carries on
  -- autonomously from the source once started.  I need disconnect!
  liftEff $ stop ctx controller
