module Audio.Example.Cowbell (example) where

import Prelude (Unit, bind, pure, ($), (+))
import Audio.WebAudio.Types (AudioContext, GainNode, OscillatorNode, connect)
import Audio.WebAudio.BaseAudioContext (newAudioContext, createOscillator,
      createGain, createBiquadFilter, currentTime, destination)
import Audio.WebAudio.Oscillator (OscillatorType(..), setFrequency, setOscillatorType, startOscillator, stopOscillator)
import Audio.WebAudio.GainNode (gain)
import Audio.WebAudio.BiquadFilterNode (BiquadFilterType(..), filterFrequency, setFilterType)
import Audio.WebAudio.AudioParam (setValue, setValueAtTime, exponentialRampToValueAtTime)
import Effect.Aff (Aff)
import Effect (Effect)
import Effect.Class (liftEffect)

-- | Cowbell - illustrates rampToValue
-- | example taken from
-- | http://outputchannel.com/post/tr-808-cowbell-web-audio/

type CowbellController =
  { osc1 :: OscillatorNode
  , osc2 :: OscillatorNode
  , gain :: GainNode
  }

-- | configure the nodes

configure ::
     AudioContext
  -> Effect CowbellController
configure ctx = do
  now <- currentTime ctx
  -- we make the basic bell sound from a couple of frequencies
  -- the first oscillator
  osc1 <- createOscillator ctx
  _ <- setOscillatorType Square osc1
  _ <- setFrequency 800.0 osc1

  -- the second oscillator
  osc2 <- createOscillator ctx
  _ <- setOscillatorType Square osc2
  _ <- setFrequency 540.0 osc2

  -- the gain
  gainNode <- createGain ctx
  gainParam <- gain gainNode
  _ <- setValue 0.5 gainParam
  -- set the start gain
  _ <- setValueAtTime 0.5 now gainParam
  -- gradually reduce the amplitude to a minimum value of 0.01
  _ <- exponentialRampToValueAtTime 0.01 (now + 1.0) gainParam

  -- the filter
  filter <- createBiquadFilter ctx
  _ <- setFilterType Bandpass filter
  -- the bandpass filter concentrates on passing through a narrow band of
  -- frequencies centered around the one supplied, attenuating the others
  freqParam <- filterFrequency filter
  _ <- setValue 800.0 freqParam

  dst <- destination ctx

  -- connect it all up
  _ <- connect osc1 gainNode
  _ <- connect osc2 gainNode
  _ <- connect gainNode filter
  _ <- connect filter dst
  pure { osc1 : osc1, osc2 : osc2, gain : gainNode}

-- | start the oscillator
start ::
     Number
  -> CowbellController
  -> Effect Unit
start time controller = do
  _ <- startOscillator time controller.osc1
  startOscillator time controller.osc2

-- | stop the oscillator immediately
stop ::
     Number
  -> CowbellController
  -> Effect  Unit
stop time controller = do
  _ <- stopOscillator time controller.osc1
  stopOscillator time controller.osc2

-- | the complete example
example :: Aff Unit
example = do
  ctx <- liftEffect newAudioContext
  now <- liftEffect $ currentTime ctx
  controller <- liftEffect $ configure ctx
  _ <- liftEffect $ start now controller
  liftEffect $ stop (now + 1.1) controller
