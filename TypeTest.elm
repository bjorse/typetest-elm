module TypeTest where

import Effects exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
import String
import Char
import Calculations

type alias WordStatus =
  { text : String
  , typedText : String
}

type alias TypeTestResult =
  { wpmTotal : Int
  , wpmCorrect : Int
  , cpmTotal : Int
  , cpmCorrect : Int
  }

type alias Model =
  { totalTime : Int 
  , timeLeft : Int
  , words : List String
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
  , result : TypeTestResult
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

getNextTimeLeft : Model -> Int
getNextTimeLeft model =
  if model.running && model.timeLeft > 0 
    then model.timeLeft - 1 
    else model.timeLeft

getNextTypeStatus : Char -> Model -> Model
getNextTypeStatus char model =
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
    { model | currentWord = newWord
            , running = True
            , count = model.count + 1
            , errorCount = errorCount
            , hasTypingError = hasTypingError
            , words = words
            , pastWords = pastWords
            , streak = streak
            , highestStreak = highestStreak
            , completedWordCount = completedWordCount 
    }

getTypeTestResult : Model -> TypeTestResult
getTypeTestResult model =
  let
    timeSpent = model.totalTime - model.timeLeft
    totalCharactersTyped = model.count + model.errorCount
  in
    { wpmTotal = Calculations.calculateWpm timeSpent totalCharactersTyped
    , wpmCorrect = Calculations.calculateWpm timeSpent model.count
    , cpmTotal = Calculations.calculateCpm timeSpent totalCharactersTyped
    , cpmCorrect = Calculations.calculateCpm timeSpent model.count
    }

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none)

    Tick ->
      let
        newTimeLeft = getNextTimeLeft model
        newResult = if model.timeLeft > 0 then getTypeTestResult model else model.result 
      in
        ({ model | timeLeft = newTimeLeft
                 , result = newResult }, Effects.none)

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
      if model.timeLeft == 0
        then ( model, Effects.none )
        else ( getNextTypeStatus char model, Effects.none)

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

initResult : TypeTestResult
initResult =
  { wpmTotal = 0
  , wpmCorrect = 0
  , cpmTotal = 0
  , cpmCorrect = 0
  }

init : Int -> Model
init totalTime =
  { totalTime = totalTime 
  , timeLeft = totalTime
  , words = []
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
  , result = initResult
  }

boldSpan value =
  span [ boldStyle ] [ text (toString value) ]

view address model =
  div [ baseStyle ]
  [ text "typed characters: "
  , boldSpan model.count
  , text ", errors: "
  , boldSpan model.errorCount
  , text ", current streak: "
  , boldSpan model.streak
  , text ", highest streak: "
  , boldSpan model.highestStreak
  , text ", completed words: "
  , boldSpan model.completedWordCount
  , text ", skipped words: "
  , boldSpan model.skippedWordCount
  , hr [] []
  , span []
    [ text "Time: "
    , span [ timeStyle ]
      [ text (String.padLeft 3 '0' (toString model.timeLeft)) ]
    ]
  , hr [] []
  , div [ fixedHeightStyle, (if model.hasTypingError then errorStyle else noErrorStyle) ] 
    [ text model.currentWord.typedText
    , text (if model.hasTypingError then (" [ TYPING ERROR (press space to skip word) ]") else "")
    ]
  , hr [] []
  , div [ fixedHeightStyle, currentWordStyle ] 
    [ text model.currentWord.text ]
  , hr [] []
  , span [ awaitingWordsStyle ]
    [ text (String.join ", " (List.take 5 model.words))
    ]
  , hr [] []
  , div []
    [ text "WPM (total): " 
    , boldSpan model.result.wpmTotal
    , text ", WPM (correct): "
    , boldSpan model.result.wpmCorrect
    , text ", CPM (total): "
    , boldSpan model.result.cpmTotal
    , text ", CPM (correct): "
    , boldSpan model.result.cpmCorrect
    ]
  ]

lineHeight = "20px"

bold = ("font-weight", "bold")

baseStyle =
  style 
    [ ("display", "inline-block")
    , ("margin", "10px")
    ]

boldStyle =
  style
    [ bold ]

fixedHeightStyle =
  style
    [ ("display", "block") ]

timeStyle =
  style
    [ ("color", "green")
    , bold
    ]

currentWordStyle = 
  style 
    [ ("color", "blue")
    , bold
    , ("height", lineHeight)
    ]

noErrorStyle =
  style 
    [ ("color", "green") 
    , bold
    , ("height", lineHeight)
    , ("line-height", lineHeight)
    ]

errorStyle = 
  style 
    [ ("color", "red") 
    , bold
    , ("height", lineHeight)
    , ("line-height", lineHeight)
    ]

awaitingWordsStyle =
  style
    [ ("opacity", "0.5") ]