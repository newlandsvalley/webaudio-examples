module Audio.Util where

import Prelude (Unit, ($), (+), bind, pure, unit)
import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect.Aff (Aff, delay)
import Effect (Effect)
import Effect.Class (liftEffect)
import Network.HTTP.Affjax (affjax, defaultRequest)
import Network.HTTP.Affjax.Response as Response
import Data.HTTP.Method (Method(..))
import Data.Traversable (traverse)
import Control.Parallel (parallel, sequential)
import Audio.WebAudio.AudioBufferSourceNode (StartOptions, setBuffer, startBufferSource)
import Audio.WebAudio.BaseAudioContext (createBufferSource, currentTime, decodeAudioDataAsync, destination, newAudioContext)
import Audio.WebAudio.Types (AudioContext, AudioBuffer, connect)


type ElapsedTime = Number

-- | load a single sound buffer resource and decode it
loadSoundBuffer ::
  AudioContext
  -> String
  -> Aff AudioBuffer
loadSoundBuffer ctx fileName = do
  res <- affjax Response.arrayBuffer $ defaultRequest { url = fileName, method = Left GET }
  buffer <- decodeAudioDataAsync ctx res.response
  pure buffer

-- | load and decode an array of audio buffers from a set of resources
loadSoundBuffers ::
  AudioContext
  -> (Array String)
  -> Aff (Array AudioBuffer)
loadSoundBuffers ctx fileNames =
  sequential $ traverse (\name -> parallel (loadSoundBuffer ctx name)) fileNames

-- | Play a sound at a specified elapsed time
playSoundAt  ::
     AudioContext
  -> Maybe AudioBuffer
  -> ElapsedTime
  -> Effect Unit
playSoundAt ctx mbuffer elapsedTime =
  case mbuffer of
    Just buffer ->
      do
        startTime <- currentTime ctx
        src <- createBufferSource ctx
        let
          whenOption :: StartOptions
          whenOption = { when: Just (startTime + elapsedTime + 0.1),  offset: Nothing, duration: Nothing }
        dst <- destination ctx
        _ <- connect src dst
        _ <- setBuffer buffer src
        -- // We'll start playing the sound 100 milliseconds from "now"
        startBufferSource  whenOption src
    _ ->
      pure unit
