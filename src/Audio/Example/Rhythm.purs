module Audio.Example.Rhythm (example) where

import Prelude (Unit, bind, pure, unit, ($), (+), (-), (*), (/), (>))
import Data.Array ((!!))
import Data.Maybe (Maybe)
import Data.Int (toNumber)
import Audio.WebAudio.Types (AudioContext, AudioBuffer)
import Audio.WebAudio.BaseAudioContext (newAudioContext)
import Audio.Util
import Effect.Aff (Aff)
import Effect (Effect)
import Effect.Class (liftEffect)

-- | Rhythm example
-- | for the original webaudioapi javascript see
-- | https://github.com/borismus/webaudioapi.com/blob/master/content/posts/rhythm/rhythm-sample.js

aBar ::
     AudioContext
  -> Array AudioBuffer
  -> Int
  -> Effect Unit
aBar ctx buffers bar =
  let
    tempo = 80 -- BPM (beats per minute)
    eighthNoteTime :: Number
    eighthNoteTime = (toNumber 60 / toNumber tempo) / 2.0
    time :: Number
    time = (toNumber bar) * 8.0 * eighthNoteTime
    hihat = buffers !! 0
    kick = buffers !! 1
    snare = buffers !! 2
  in
    do
      -- Play the bass (kick) drum on beats 1, 5
      _ <- playSoundAt ctx kick time
      _ <- playSoundAt ctx kick (time + 4.0 * eighthNoteTime)

      -- Play the snare drum on beats 3, 7
      _ <- playSoundAt ctx snare (time + 2.0 * eighthNoteTime)
      _ <- playSoundAt ctx snare (time + 6.0 * eighthNoteTime)

      -- Play the hi-hat every eighth note.
      playLoopPattern ctx hihat time eighthNoteTime 8

-- | Play the hi-hat every eighth
-- | we use recursion to emulate the for loop
playLoopPattern ::
     AudioContext
  -> Maybe AudioBuffer
  -> Number
  -> Number
  -> Int
  -> Effect Unit
playLoopPattern ctx buffer time eighthNoteTime count =
  if (count > 0) then
    do
      _ <- playSoundAt ctx buffer (time + (toNumber count) * eighthNoteTime)
      playLoopPattern ctx buffer time eighthNoteTime (count -1)
  else
    do
      pure unit

example :: Aff Unit
example = do
  ctx <- liftEffect newAudioContext
  buffers <- loadSoundBuffers ctx ["wav/hihat.wav", "wav/kick.wav", "wav/snare.wav"]
  _ <- liftEffect $ aBar ctx buffers 0
  _ <- liftEffect $ aBar ctx buffers 1
  pure unit
