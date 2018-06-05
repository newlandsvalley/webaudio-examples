module Audio.Example.Feedback (example) where

import Prelude (Unit, bind, pure, ($), (+))
import Data.Maybe (Maybe(..))
import Audio.WebAudio.Types (AudioContext, AudioBuffer, AudioBufferSourceNode, GainNode,
     DelayNode, DestinationNode, connect, disconnect)
import Audio.WebAudio.BaseAudioContext (newAudioContext, createBufferSource, createDelay,
      createGain, currentTime, destination)
import Audio.WebAudio.AudioBufferSourceNode (StartOptions, setBuffer, startBufferSource, stopBufferSource, setLoop)
import Audio.WebAudio.AudioParam (setValue)
import Audio.WebAudio.GainNode (setGain)
import Audio.WebAudio.DelayNode (delayTime)
import Audio.Util
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay)
import Effect (Effect)
import Effect.Class (liftEffect)



-- | feedback example illustrating DelayNode inspired by
-- | http://blog.chrislowis.co.uk/2014/07/23/dub-delay-web-audio-api.html

type FeedbackController =
  { source :: AudioBufferSourceNode
  , delay :: DelayNode
  , gain :: GainNode
  , destination :: DestinationNode
  }

-- | load the buffer and complete the configuration
setup ::
  AudioContext
  -> Aff  FeedbackController
setup ctx = do
  buffer <- loadSoundBuffer ctx "ogg/chop.ogg"
  liftEffect $ configure ctx buffer

-- | configure the nodes
configure ::
     AudioContext
  -> AudioBuffer
  -> Effect  FeedbackController
configure ctx buf = do
  src <- createBufferSource ctx
  _ <- setBuffer buf src
  _ <- setLoop true src
  delay <- createDelay ctx
  delayParam <- delayTime delay
  -- delay by half a second
  _ <- setValue 0.5 delayParam
  feedback <- createGain ctx
  -- create a diminishing echo effect each time we feed back
  _ <- setGain 0.8 feedback
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
start ::
     AudioContext
  -> FeedbackController
  -> Effect Unit
start ctx controller = do
  startTime <- currentTime ctx
  let
    whenOption :: StartOptions
    whenOption = { when: Just (startTime + 0.1),  offset: Nothing, duration: Nothing }
  startBufferSource whenOption controller.source

-- | stop the sound
stop ::
     AudioContext
  -> FeedbackController
  -> Effect Unit
stop ctx controller = do
  now <- currentTime ctx
  -- these disconnects seem to be necessary to enable stopping
  -- presumably if we just kill the source, the feedback loop will still be active
  _ <- disconnect controller.delay controller.gain
  _ <- disconnect controller.gain controller.delay
  _ <- disconnect controller.delay controller.destination
  stopBufferSource now controller.source

-- | the complete example
example ::  Aff Unit
example = do
  ctx <- liftEffect newAudioContext
  controller <- setup ctx
  _ <- liftEffect $ start ctx controller
  -- let it run for 10 seconds
  _ <- delay (Milliseconds 10000.0)
  -- why doesn't this stop ??
  liftEffect $ stop ctx controller
