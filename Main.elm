module Main where

import Http
import Html.Events exposing (..)
import Json.Decode as Json exposing (..)
import Task exposing (..)
import Effects exposing (..)
import Char
import Keyboard
import Time
import StartApp
import TypeTest 

fetchWords : Effects TypeTest.Action
fetchWords =
  Http.get words "http://www.randomtext.me/api/gibberish/p/1000"
  |> Task.toMaybe
  |> Task.map TypeTest.UpdateWords
  |> Effects.task

words : Json.Decoder String
words =
  "text_out" := Json.string

keyboardToAction : Int -> TypeTest.Action
keyboardToAction keyCode =
  case keyCode of
    -- Enter key
    13 -> TypeTest.NoOp                
    -- Space key
    32 -> TypeTest.FinishOrSkipCurrentWord
    -- The rest
    code -> TypeTest.UpdateTypedText (Char.fromCode code)

app =
  StartApp.start
    { init = (TypeTest.init 120, fetchWords)
    , update = TypeTest.update
    , view = TypeTest.view
    , inputs = [ Signal.map keyboardToAction Keyboard.presses
               , Signal.map (\_ -> TypeTest.Tick) (Time.fps 1)
               , Signal.map (\_ -> TypeTest.RemoveLastCharInTypedText) backspace ]
    }

main = 
  app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

port backspace : Signal ()