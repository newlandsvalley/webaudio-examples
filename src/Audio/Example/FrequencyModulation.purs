module Audio.Example.FrequencyModulation (example) where

import Prelude (Unit, bind, pure, ($))
import Audio.WebAudio.Types (AudioContext, GainNode, OscillatorNode, connect, connectParam)
import Audio.WebAudio.BaseAudioContext (newAudioContext, createOscillator,
      createGain, currentTime, destination)
import Audio.WebAudio.Oscillator (frequency, setFrequency, startOscillator, stopOscillator)
import Audio.WebAudio.GainNode (setGain)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay)
import Effect (Effect)
import Effect.Class (liftEffect)


-- | Frequency modulation example
-- | examples exist on https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API
-- | this is simply an illutration of connecting a node to an Audio Param
-- | (not another node)

type FMController =
  { modulator :: OscillatorNode
  , carrier :: OscillatorNode
  , modGain :: GainNode
  }

-- | configure the nodes
configure ::
     AudioContext
  -> Effect FMController
configure ctx = do
  -- the modulating oscillator
  modulator <- createOscillator ctx
  _ <- setFrequency 0.8 modulator
  -- the carrier oscillator (the basic note)
  carrier <- createOscillator ctx
  _ <- setFrequency 300.0 carrier
  cfreqParam <- frequency carrier
  -- the gain node
  modGainNode <- createGain ctx
  _ <- setGain 30.0 modGainNode
  dst <- destination ctx
  -- connect it all up
  _ <- connect modulator modGainNode
  _ <- connectParam modGainNode carrier "frequency"
  _ <- connect carrier dst
  pure { modulator : modulator, carrier : carrier, modGain : modGainNode}

-- | start the oscillator
start ::
     AudioContext
  -> FMController
  -> Effect Unit
start ctx controller = do
  now <- currentTime ctx
  _ <- startOscillator now controller.carrier
  startOscillator now controller.modulator

-- | stop the oscillator immediately
stop ::
     AudioContext
  -> FMController
  -> Effect Unit
stop ctx controller = do
  now <- currentTime ctx
  _ <- stopOscillator now controller.carrier
  stopOscillator now controller.modulator

-- | the complete example
example ::  Aff Unit
example = do
  ctx <- liftEffect newAudioContext
  controller <- liftEffect $ configure ctx
  _ <- liftEffect $ start ctx controller
  -- let it run for about 5 seconds
  _ <- delay (Milliseconds 5000.0)
  liftEffect $ stop ctx controller
