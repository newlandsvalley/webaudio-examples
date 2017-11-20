module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Fiber, launchAff)
import Audio.WebAudio.Types (WebAudio)
import Network.HTTP.Affjax (AJAX)
import Audio.Example.Rhythm as Rhythm

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
  launchAff Rhythm.example
