module Audio.Example.FrequencyModulation (example) where

import Prelude (Unit, bind, pure, ($))
import Audio.WebAudio.Types (AudioContext, GainNode, OscillatorNode, AUDIO, connect, connectParam)
import Audio.WebAudio.BaseAudioContext (newAudioContext, createOscillator,
      createGain, currentTime, destination)
import Audio.WebAudio.Oscillator (frequency, setFrequency, startOscillator, stopOscillator)
import Audio.WebAudio.GainNode (setGain)
import Control.Monad.Aff (Aff, delay)
import Data.Time.Duration (Milliseconds(..))
import Network.HTTP.Affjax (AJAX)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)

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
configure :: ∀ eff.
     AudioContext
  -> Eff
      ( audio :: AUDIO
      | eff
      )
      FMController
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
start :: ∀ eff.
     AudioContext
  -> FMController
  -> Eff
      ( audio :: AUDIO
      | eff
      )
        Unit
start ctx controller = do
  now <- currentTime ctx
  _ <- startOscillator now controller.carrier
  startOscillator now controller.modulator

-- | stop the oscillator immediately
stop :: ∀ eff.
     AudioContext
  -> FMController
  -> Eff
      ( audio :: AUDIO
      | eff
      )
        Unit
stop ctx controller = do
  now <- currentTime ctx
  _ <- stopOscillator now controller.carrier
  stopOscillator now controller.modulator

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
  controller <- liftEff $ configure ctx
  _ <- liftEff $ start ctx controller
  -- let it run for about 5 seconds
  _ <- delay (Milliseconds 5000.0)
  liftEff $ stop ctx controller
