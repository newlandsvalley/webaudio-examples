module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Fiber, launchAff)
import Audio.WebAudio.Types (WebAudio)
import Network.HTTP.Affjax (AJAX)
import Audio.Example.Rhythm as Rhythm
import Audio.Example.Volume as Volume
import Audio.Example.Crossfade as Crossfade

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
  launchAff Crossfade.example
