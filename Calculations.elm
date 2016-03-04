module Calculations where

calculateWpm : Int -> Int -> Int
calculateWpm totalSeconds charCount =
  if totalSeconds == 0
    then 0
    else 
      let
        minutes = (toFloat totalSeconds) / 60
        words = (toFloat charCount) / 5
        result = words / minutes
      in
        round result

calculateCpm : Int -> Int -> Int
calculateCpm totalSeconds charCount =
  if totalSeconds == 0
    then 0
    else
      let
        minutes = (toFloat totalSeconds) / 60
        result = (toFloat charCount) / minutes
      in
        round result