module Audio.Example.FrequencyModulation (example) where

import Prelude (Unit, bind, pure, ($))
import Audio.WebAudio.Types (AudioContext, GainNode, DestinationNode, OscillatorNode, WebAudio)
import Audio.WebAudio.AudioContext (makeAudioContext, createOscillator,
      createGain, connect, connectParam, currentTime, destination, disconnect, disconnectParam)
import Audio.WebAudio.Oscillator (frequency, startOscillator, stopOscillator)
import Audio.WebAudio.AudioParam (getValue, setValue)
import Audio.WebAudio.GainNode (gain)
import Control.Monad.Aff (Aff, delay)
import Data.Time.Duration (Milliseconds(..))
import Network.HTTP.Affjax (AJAX)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)

-- | this doesn't work and I don't know why ! It ignores the modulation.
-- | Also, it puts into very clear perspective the impedence mismatch
-- | between pure functions, no variable assignment on the one hand
-- | and updating state in situ on the other
-- | So, an AudioParam can be updated by means of an unsafe assignment
-- | (which is bad enough) but then you need this facility in order to
-- | vary the modulator frequency (an AudioParam) and feed it to the carrier

{-
var mod, modGain, osc;

var out = context.destination;

var startTest = function(){
    mod = context.createOscillator();
    mod.frequency.value = 8;

    modGain = context.createGain();
    modGain.gain.value = 30;

    osc = context.createOscillator();
    osc.frequency.value = 300;

    mod.connect(modGain);
    modGain.connect(osc.frequency);
    osc.connect(out);

    osc.start(0);
    mod.start(0);
};

var stopTest = function(){
    osc.stop(0);
    mod.stop(0);
    mod = modGain = osc = null;
};
-}

type FMController =
  { modulator :: OscillatorNode
  , carrier :: OscillatorNode
  , modGain :: GainNode
  }

-- | configurew the nodes
configure :: ∀ eff.
     AudioContext
  -> Eff
      ( wau :: WebAudio
      | eff
      )
      FMController
configure ctx = do
  -- the modulating oscillator
  modulator <- createOscillator ctx
  mfreqParam <- frequency modulator
  _ <- setValue 8.0 mfreqParam
  -- modulationFrequency <- getValue mfreqParam
  -- the carrier oscillator (the basic note)
  carrier <- createOscillator ctx
  cfreqParam <- frequency carrier
  _ <- setValue 300.0 cfreqParam
  -- cfreqParam' <- frequency carrier
  -- the gain node
  modGainNode <- createGain ctx
  gainParam <- gain modGainNode
  _ <- setValue 30.0 gainParam
  dst <- destination ctx
  -- connect it all up
  _ <- connect modulator modGainNode
  _ <- connectParam modGainNode cfreqParam
  _ <- connect carrier dst
  pure { modulator : modulator, carrier : carrier, modGain : modGainNode}

-- | start the oscillator
start :: ∀ eff.
     AudioContext
  -> FMController
  -> Eff
      ( wau :: WebAudio
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
      ( wau :: WebAudio
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
    , wau :: WebAudio
    | eff
    )
    Unit
example = do
  ctx <- liftEff makeAudioContext
  controller <- liftEff $ configure ctx
  _ <- liftEff $ start ctx controller
  -- let it run for about 5 seconds
  _ <- delay (Milliseconds 5000.0)
  liftEff $ stop ctx controller
