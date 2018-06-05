module Main where

import Prelude
import Effect (Effect)
import Effect.Aff (Fiber, launchAff)
import Audio.Example.Rhythm as Rhythm
import Audio.Example.Volume as Volume
import Audio.Example.Crossfade as Crossfade
import Audio.Example.Filter as Filter
import Audio.Example.Oscillator as Oscillator
import Audio.Example.Feedback as Feedback
import Audio.Example.FrequencyModulation as FrequencyModulation
import Audio.Example.Cowbell as Cowbell


main ::  Effect (Fiber Unit)
main =
  -- launchAff Rhythm.example
  -- launchAff Volume.example
  -- launchAff Crossfade.example
  -- launchAff Filter.example
  -- launchAff Oscillator.example
  -- launchAff Feedback.example
  -- launchAff FrequencyModulation.example
  launchAff Cowbell.example
