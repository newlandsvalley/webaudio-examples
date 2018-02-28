module Audio.Example.Oscillator (example) where

import Prelude (Unit, bind, pure, negate, unit, ($), (*), (+), (-), (>), (<), (/))
import Audio.WebAudio.Types (AudioContext, OscillatorNode, GainNode, WebAudio)
import Audio.WebAudio.AudioContext (makeAudioContext, createOscillator, createGain, connect, currentTime, destination)
import Audio.WebAudio.Oscillator (OscillatorType(..), setFrequency, setDetune, setOscillatorType, startOscillator, stopOscillator)
import Audio.WebAudio.GainNode (setGain)
import Control.Monad.Aff (Aff, delay)
import Data.Time.Duration (Milliseconds(..))
import Network.HTTP.Affjax (AJAX)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Math (pow)

-- | Volume example
-- | for the original webaudioapi javascript that influenced this example see
-- | https://github.com/borismus/webaudioapi.com/blob/master/content/posts/oscillator/oscillator-sample.js

-- | this is the state that we'll pass through the various funtions
type OscillatorController =
  { oscillator :: OscillatorNode
  , gain :: GainNode
  }

-- | frequency and detune over a given octace range are essentially equivalent.
-- | frequency works in logarithmic units whilst detune works in decimal units
-- | (over a given octave range both up and down)
-- | to go up a semitone, multiply the frquency by 2^1/2
-- | or equivalently, increase the detune by 100 cents

-- | let's define an octave range for our experiments
-- | the 60 roughly means we should also play the final note in the octave
startFrequency :: Number
startFrequency = 440.0

endFrequency :: Number
endFrequency = (2.0 * startFrequency) + 60.0

-- | and to go back down again, we'll use detune
-- | which is only defined in the range -1200 to +1200
-- | where 1 semitone is 100 units (cents)
startDetune :: Number
startDetune = 1200.0

endDetune :: Number
endDetune =  -110.0

-- | configure the nodes
configure :: ∀ eff.
     AudioContext
  -> OscillatorType
  -> Eff
      ( wau :: WebAudio
      | eff
      )
      OscillatorController
configure ctx oscillatorType = do
  osc <- createOscillator ctx
  _ <- setOscillatorType oscillatorType osc
  -- let's not make it too loud - oscillators can be annoying
  gainNode <- createGain ctx
  _ <- setGain 0.1 gainNode
  dst <- destination ctx
  _ <- connect osc gainNode
  _ <- connect gainNode dst
  pure { oscillator : osc, gain : gainNode}

-- | change the frequency
changeFrequency :: ∀ eff.
     AudioContext
  -> OscillatorController
  -> Number
  -> Eff
      ( wau :: WebAudio
      | eff
      )
      Unit
changeFrequency ctx controller hz = do
  setFrequency hz controller.oscillator

-- | change the detune
changeDetune :: ∀ eff.
     AudioContext
  -> OscillatorController
  -> Number
  -> Eff
      ( wau :: WebAudio
      | eff
      )
      Unit
changeDetune ctx controller cents = do
  setDetune cents controller.oscillator

-- | start the oscillator
start :: ∀ eff.
     AudioContext
  -> OscillatorController
  -> Eff
      ( wau :: WebAudio
      | eff
      )
        Unit
start ctx controller = do
  now <- currentTime ctx
  startOscillator now controller.oscillator

-- | stop the oscillator immediately
stop :: ∀ eff.
     AudioContext
  -> OscillatorController
  -> Eff
      ( wau :: WebAudio
      | eff
      )
        Unit
stop ctx controller = do
  now <- currentTime ctx
  stopOscillator now controller.oscillator

-- | gradually increase the frequency by a semitone every quarter second (or so)
stepFreq :: ∀ eff.
     AudioContext
  -> OscillatorController
  -> Number
  -> Aff
     ( ajax :: AJAX
     , wau :: WebAudio
     | eff
     )
     Unit
stepFreq ctx controller hz =
  if hz > endFrequency then do
    _ <- liftEff $ stop ctx controller
    pure unit
  else do
    let
      semitoneUp = hz * pow 2.0 (1.0 / 12.0)
    -- we'll mimic user input of pressing a 'semitone up' key with delays
    _ <- delay (Milliseconds 250.0)
    _ <- liftEff $ changeFrequency ctx controller hz
    stepFreq ctx controller semitoneUp

-- | gradually increase the detune by 10% every quarter second or so
stepDetune :: ∀ eff.
     AudioContext
  -> OscillatorController
  -> Number
  -> Aff
     ( ajax :: AJAX
     , wau :: WebAudio
     | eff
     )
     Unit
stepDetune ctx controller cents =
  if cents < endDetune then do
    _ <- liftEff $ stop ctx controller
    pure unit
  else do
    -- we'll mimic user input of pressing a 'detune up' key with delays
    _ <- delay (Milliseconds 250.0)
    _ <- liftEff $ changeDetune ctx controller cents
    stepDetune ctx controller (cents - 100.0)

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
  controller <- liftEff $ configure ctx Square
  _ <- liftEff $ start ctx controller
  _ <- stepFreq ctx controller startFrequency
  -- as we stopped the controller, we need to start a new one
  -- let's use a triangular wave ocillator
  controller' <- liftEff $ configure ctx Triangle
  _ <- liftEff $ start ctx controller'
  stepDetune ctx controller' startDetune
