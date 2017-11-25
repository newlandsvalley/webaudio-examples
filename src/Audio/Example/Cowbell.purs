module Audio.Example.Cowbell (example) where

import Prelude (Unit, bind, pure, ($), (+))
import Audio.WebAudio.Types (AudioContext, GainNode, OscillatorNode, WebAudio)
import Audio.WebAudio.AudioContext (makeAudioContext, createOscillator,
      createGain, createBiquadFilter, connect, currentTime, destination)
import Audio.WebAudio.Oscillator (OscillatorType(..), setFrequency, setOscillatorType, startOscillator, stopOscillator)
import Audio.WebAudio.GainNode (gain)
import Audio.WebAudio.BiquadFilterNode (BiquadFilterType(..), filterFrequency, setFilterType)
import Audio.WebAudio.AudioParam (setValue, setValueAtTime, exponentialRampToValueAtTime)
import Control.Monad.Aff (Aff)
import Network.HTTP.Affjax (AJAX)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)

-- | Cowbell - illustrates rampToValue
-- | example taken from
-- | http://outputchannel.com/post/tr-808-cowbell-web-audio/

type CowbellController =
  { osc1 :: OscillatorNode
  , osc2 :: OscillatorNode
  , gain :: GainNode
  }

-- | configure the nodes

configure :: ∀ eff.
     AudioContext
  -> Eff
      ( wau :: WebAudio
      | eff
      )
      CowbellController
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
start :: ∀ eff.
     Number
  -> CowbellController
  -> Eff
      ( wau :: WebAudio
      | eff
      )
        Unit
start time controller = do
  _ <- startOscillator time controller.osc1
  startOscillator time controller.osc2

-- | stop the oscillator immediately
stop :: ∀ eff.
     Number
  -> CowbellController
  -> Eff
      ( wau :: WebAudio
      | eff
      )
        Unit
stop time controller = do
  _ <- stopOscillator time controller.osc1
  stopOscillator time controller.osc2

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
  now <- liftEff $ currentTime ctx
  controller <- liftEff $ configure ctx
  _ <- liftEff $ start now controller
  liftEff $ stop (now + 1.1) controller
