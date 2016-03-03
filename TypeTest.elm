module TypeTest where

import Effects exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
import String
import Char

type alias WordStatus =
  { text : String
  , typedText : String
}

type alias Model =
  { words : List String
  , running : Bool
  , currentWordIndex : Int
  , currentWord : WordStatus
  , hasTypingError : Bool
  , pastWords : List WordStatus
  , count : Int
  , errorCount : Int
  , streak : Int
  , highestStreak : Int
  , completedWordCount : Int
  , skippedWordCount : Int
  , timeElapsed : Int
  }

type Action
  = NoOp
  | Tick
  | UpdateWords (Maybe String)
  | SkipCurrentWord
  | UpdateTypedText Char

appendTypedText : WordStatus -> Char -> WordStatus
appendTypedText word typedChar =
  { word | typedText = word.typedText ++ (String.fromChar typedChar) }

getNextWord : List String -> WordStatus
getNextWord words =
  case List.head words of
    Nothing -> { text = "", typedText = "" }

    Just word -> { text = word, typedText = "" }

removeFirstWord : List String -> List String
removeFirstWord words =
  case List.tail words of
    Nothing -> []

    Just newWords -> newWords

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none)

    Tick ->
      ({ model | timeElapsed = if model.running then model.timeElapsed + 1 else model.timeElapsed }, Effects.none)

    UpdateWords wordText ->
      let
        parsedWords = parseWords wordText
        words = removeFirstWord parsedWords
        currentWord = getNextWord parsedWords
      in
        ({ model | words = words
                 , currentWord = currentWord }, Effects.none)

    SkipCurrentWord ->
      let
        newWord = getNextWord model.words
        newWords = removeFirstWord model.words
      in
        ({ model | words = newWords
                 , currentWord = newWord
                 , hasTypingError = False
                 , count = model.count + 1
                 , errorCount = model.errorCount + 1
                 , skippedWordCount = model.skippedWordCount + 1
                 , streak = 0 }, Effects.none)

    UpdateTypedText char ->
      let
        currentWord = appendTypedText model.currentWord (Char.toLower char)
        hasTypingError = not (typedTextMatches currentWord)
        newWordNeeded = wordFinished currentWord
        newWord = if newWordNeeded then getNextWord model.words else currentWord
        words = if newWordNeeded then removeFirstWord model.words else model.words
        pastWords = if newWordNeeded then currentWord :: model.pastWords else model.pastWords
        errorCount = if hasTypingError then model.errorCount + 1 else model.errorCount
        streak = if hasTypingError then 0 else model.streak + 1
        highestStreak = if streak > model.highestStreak then streak else model.highestStreak
        completedWordCount = if newWordNeeded then model.completedWordCount + 1 else model.completedWordCount
      in
        ({ model | currentWord = newWord
                 , running = True
                 , count = model.count + 1
                 , errorCount = errorCount
                 , hasTypingError = hasTypingError
                 , words = words
                 , pastWords = pastWords
                 , streak = streak
                 , highestStreak = highestStreak
                 , completedWordCount = completedWordCount }, Effects.none)

parseWords : Maybe String -> List String
parseWords wordText =
  case wordText of
    Nothing -> []

    Just wordText ->
      String.slice 3 -5 wordText 
      |> String.toLower
      |> String.words
      |> List.filter (\word -> (String.length word) > 2)

typedTextMatches : WordStatus -> Bool
typedTextMatches word =
  String.startsWith word.typedText word.text

wordFinished : WordStatus -> Bool
wordFinished word =
  word.text == word.typedText

init : List String -> Model
init words =
  { words = words
  , running = False
  , currentWordIndex = 0
  , currentWord = { text = "", typedText = "" }
  , hasTypingError = False
  , pastWords = []
  , count = 0
  , errorCount = 0
  , streak = 0
  , highestStreak = 0
  , completedWordCount = 0
  , skippedWordCount = 0
  , timeElapsed = 0 
  }

view address model =
  div [ baseStyle ]
  [ text ("typed letters: " ++ (toString model.count))
  , text (", errors: " ++ (toString model.errorCount))
  , text (", current streak: " ++ (toString model.streak))
  , text (", highest streak: " ++ (toString model.highestStreak))
  , text (", completed words: " ++ (toString model.completedWordCount))
  , text (", skipped words: " ++ (toString model.skippedWordCount))
  , hr [] []
  , text ("Time: " ++ (String.padLeft 3 '0' (toString model.timeElapsed)))
  , div [ if model.hasTypingError then errorStyle else noErrorStyle ] 
    [ text model.currentWord.typedText
    , text (if model.hasTypingError then (" [ TYPING ERROR (press space to skip word) ]") else "")
    ]
  , hr [] []
  , div [ currentWordStyle ] [ text model.currentWord.text ]
  , hr [] []
  , div [ awaitingWordsStyle ]
    [ text (String.join ", " (List.take 5 model.words))
    ]
  ]

lineHeight = "25px"

baseStyle =
  style 
    [ ("display", "inline-block")
    , ("margin", "10px")
    ]

currentWordStyle = 
  style 
    [ ("color", "blue")
    , ("font-weight", "bold") ]

noErrorStyle =
  style 
    [ ("color", "green") 
    , ("font-weight", "bold")
    , ("height", lineHeight)
    , ("line-height", lineHeight)
    ]

errorStyle = 
  style 
    [ ("color", "red") 
    , ("font-weight", "bold")
    , ("height", lineHeight)
    , ("line-height", lineHeight)
    ]

awaitingWordsStyle =
  style
    [ ("opacity", "0.5")]