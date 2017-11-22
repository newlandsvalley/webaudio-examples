module Audio.Example.Feedback (example) where

import Prelude (Unit, bind, pure, ($))
import Audio.WebAudio.Types (AudioContext, AudioBuffer, AudioBufferSourceNode, GainNode,
     DelayNode, DestinationNode, WebAudio)
import Audio.WebAudio.AudioContext (makeAudioContext, createBufferSource, createDelay,
      createGain, connect, currentTime, destination, disconnect)
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

type FeedbackController =
  { source :: AudioBufferSourceNode
  , delay :: DelayNode
  , gain :: GainNode
  , destination :: DestinationNode
  }

-- | load the buffer and complete the configuration
setup :: ∀ eff.
  AudioContext
  -> Aff
      ( ajax :: AJAX
      , wau :: WebAudio
      | eff
      )
      FeedbackController
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
      FeedbackController
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
  pure { source : src, delay : delay, gain : feedback, destination : dst}

-- | start the sound
start :: ∀ eff.
     AudioContext
  -> FeedbackController
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
  -> FeedbackController
  -> Eff
      ( wau :: WebAudio
      | eff
      )
        Unit
stop ctx controller = do
  now <- currentTime ctx
  -- these disconnects seem to be necessary to enable stopping
  -- presumably if we just kill the source, the feedback loop will still be active
  _ <- disconnect controller.delay controller.gain
  _ <- disconnect controller.gain controller.delay
  _ <- disconnect controller.delay controller.destination
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
  -- let it run for 10 seconds
  _ <- delay (Milliseconds 10000.0)
  -- why doesn't this stop ??
  liftEff $ stop ctx controller
