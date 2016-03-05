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
  | FinishOrSkipCurrentWord
  | UpdateTypedText Char
  | RemoveLastCharInTypedText

appendTypedText : WordStatus -> Char -> WordStatus
appendTypedText word typedChar =
  { word | typedText = if String.length word.typedText < 45 
                         then word.typedText ++ (String.fromChar typedChar) 
                         else word.typedText }

removeLastCharInTypedText : WordStatus -> WordStatus
removeLastCharInTypedText word =
  { word | typedText = if String.length word.typedText > 0 
                         then String.slice 0 -1 word.typedText 
                         else word.typedText }

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
    errorCount = if hasTypingError then model.errorCount + 1 else model.errorCount
    streak = if hasTypingError then 0 else model.streak + 1
    highestStreak = if streak > model.highestStreak then streak else model.highestStreak
  in
    { model | currentWord = currentWord
            , running = True
            , count = model.count + 1
            , errorCount = errorCount
            , hasTypingError = hasTypingError
            , streak = streak
            , highestStreak = highestStreak
    }

getTypeTestResult : Model -> TypeTestResult
getTypeTestResult model =
  let
    timeSpent = model.totalTime - model.timeLeft
    correctCharactersTyped = model.count - model.errorCount
  in
    { wpmTotal = Calculations.calculateWpm timeSpent model.count
    , wpmCorrect = Calculations.calculateWpm timeSpent correctCharactersTyped
    , cpmTotal = Calculations.calculateCpm timeSpent model.count
    , cpmCorrect = Calculations.calculateCpm timeSpent correctCharactersTyped
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

    FinishOrSkipCurrentWord ->
      let
        wordSkipped = not (wordFinished model.currentWord)
        newWord = getNextWord model.words
        newWords = removeFirstWord model.words
        pastWords = model.currentWord :: model.pastWords
        completedWordCount = if wordSkipped then model.completedWordCount else model.completedWordCount + 1
        skippedWordCount = if wordSkipped then model.skippedWordCount + 1 else model.skippedWordCount
      in
        ({ model | words = newWords
                 , currentWord = newWord
                 , hasTypingError = False
                 , count = model.count + 1
                 , errorCount = if wordSkipped then model.errorCount + 1 else model.errorCount
                 , skippedWordCount = skippedWordCount
                 , completedWordCount = completedWordCount
                 , streak = if wordSkipped then 0 else model.streak + 1
                 , pastWords = pastWords }, Effects.none)

    UpdateTypedText char ->
      if model.timeLeft == 0
        then ( model, Effects.none )
        else ( getNextTypeStatus char model, Effects.none)

    RemoveLastCharInTypedText ->
      let
        currentWord = removeLastCharInTypedText model.currentWord
        hasTypingError = not (typedTextMatches currentWord)
      in
        ({ model | currentWord = currentWord
                 , hasTypingError = hasTypingError
                 , streak = 0 }, Effects.none)

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
  , div [ lineHeightStyle ]
    [ div [ pullLeftStyle ]
      [ div [ if model.hasTypingError then errorStyle else noErrorStyle ] 
        [ text model.currentWord.typedText ]
      ]
    , div [ pullRightStyle ]
      [ div [ errorStyle ]
        [ text (if model.hasTypingError then ("press space to skip word") else "") ]
      ]
    ]
  , hr [ clearStyle ] []
  , div []
    [ span [ currentWordStyle ] 
      [ text model.currentWord.text ]
    , span [ awaitingWordsStyle ]
      [ text (String.join " " (List.take 5 model.words)) ]
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

lineHeight = ("height", "24px")

bold = ("font-weight", "bold")

bigFontSize = ("font-size", "24px")

fixedHeight = ("display", "block")

pullLeftStyle = 
  style
    [ ("float", "left") ]

pullRightStyle =
  style
    [ ("float", "right") ]

clearStyle =
  style
    [ ("float", "clear") ]

baseStyle =
  style 
    [ ("display", "inline-block")
    , ("margin", "20px")
    ]

boldStyle =
  style
    [ bold ]

timeStyle =
  style
    [ ("color", "green")
    , bold
    ]

lineHeightStyle =
  style
    [ lineHeight ]

currentWordStyle = 
  style 
    [ ("color", "blue")
    , ("display", "inline-block")
    , bold
    , bigFontSize
    , lineHeight
    ]

noErrorStyle =
  style 
    [ ("color", "green") 
    , bold
    , bigFontSize
    , fixedHeight
    , lineHeight
    ]

errorStyle = 
  style 
    [ ("color", "red") 
    , bold
    , bigFontSize
    , fixedHeight
    , lineHeight
    ]

awaitingWordsStyle =
  style
    [ ("opacity", "0.5")
    , ("display", "inline-block")
    , ("word-spacing", "10px")
    , ("margin-left", "15px")
    , bigFontSize
    , lineHeight
    ]
