module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Fiber, launchAff)
import Audio.WebAudio.Types (WebAudio)
import Network.HTTP.Affjax (AJAX)
import Audio.Example.Rhythm as Rhythm
import Audio.Example.Volume as Volume
import Audio.Example.Crossfade as Crossfade
import Audio.Example.Filter as Filter
import Audio.Example.Oscillator as Oscillator
import Audio.Example.Feedback as Feedback
import Audio.Example.FrequencyModulation as FrequencyModulation


main :: ∀ eff.
  Eff
    ( ajax :: AJAX
    , wau :: WebAudio
    | eff
    )
    (Fiber
       ( ajax :: AJAX
       , wau :: WebAudio
       | eff
       )
       Unit
    )
main =
  -- launchAff Rhythm.example
  -- launchAff Volume.example
  -- launchAff Crossfade.example
  -- launchAff Filter.example
  -- launchAff Oscillator.example
  -- launchAff Feedback.example
  launchAff FrequencyModulation.example
