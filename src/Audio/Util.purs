module Audio.Util where

import Prelude (Unit, ($), (+), bind, pure, unit)
import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Data.HTTP.Method (Method(..))
import Control.Monad.Eff (Eff)
import Data.Traversable (traverse)
import Control.Parallel (parallel, sequential)
import Audio.WebAudio.AudioBufferSourceNode (setBuffer, startBufferSource)
import Audio.WebAudio.BaseAudioContext (createBufferSource, currentTime, decodeAudioDataAsync, destination, newAudioContext)
import Audio.WebAudio.Types (AudioContext, AudioBuffer, AUDIO, connect)


type ElapsedTime = Number

-- | load a single sound buffer resource and decode it
loadSoundBuffer :: ∀ eff.
  AudioContext
  -> String
  -> Aff
     ( ajax :: AJAX
     , audio :: AUDIO
     | eff
     )
     AudioBuffer
loadSoundBuffer ctx fileName = do
  res <- affjax $ defaultRequest { url = fileName, method = Left GET }
  buffer <- decodeAudioDataAsync ctx res.response
  pure buffer

-- | load and decode an array of audio buffers from a set of resources
loadSoundBuffers :: ∀ e.
  AudioContext
  -> (Array String)
  -> Aff
     ( ajax :: AJAX
     , audio :: AUDIO
     | e
     )
     (Array AudioBuffer)
loadSoundBuffers ctx fileNames =
  sequential $ traverse (\name -> parallel (loadSoundBuffer ctx name)) fileNames

-- | Play a sound at a specified elapsed time
playSoundAt  :: ∀ eff.
     AudioContext
  -> Maybe AudioBuffer
  -> ElapsedTime
  -> Eff
      ( audio :: AUDIO
      | eff )
      Unit
playSoundAt ctx mbuffer elapsedTime =
  case mbuffer of
    Just buffer ->
      do
        startTime <- currentTime ctx
        src <- createBufferSource ctx
        dst <- destination ctx
        _ <- connect src dst
        _ <- setBuffer buffer src
        -- // We'll start playing the sound 100 milliseconds from "now"
        startBufferSource (startTime + elapsedTime + 0.1) src
    _ ->
      pure unit

{-}
example :: ∀ eff.
  Aff
    ( ajax :: AJAX
    , audio :: AUDIO
    | eff
    )
    Unit
example = do
  ctx <- liftEff newAudioContext
  buffers <- loadSoundBuffers ctx ["wav/hihat.wav", "wav/kick.wav", "wav/snare.wav"]
  _ <- liftEff $ playSoundAt ctx (buffers !! 0) 0.0
  _ <- liftEff $ playSoundAt ctx (buffers !! 1) 0.5
  _ <- liftEff $ playSoundAt ctx (buffers !! 2) 1.0
  _ <- liftEff $ playSoundAt ctx (buffers !! 0) 1.5
  _ <- liftEff $ playSoundAt ctx (buffers !! 1) 2.0
  _ <- liftEff $ playSoundAt ctx (buffers !! 2) 2.5
  pure unit
-}
