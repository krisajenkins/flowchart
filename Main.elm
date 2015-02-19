module Main where

import Signal (..)
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import List
import List (head,filter,(::))

type Answer
  = Yes
  | No
  | NotReally
  | Reset
  | NoOp

type Question
  = DoYouKnowWhatYouAreDoing
  | AreYouSure
  | DoIt
  | DoNotDoIt

flowchartStart : Question
flowchartStart = DoYouKnowWhatYouAreDoing

flowchart : List (Question, (List (Answer, Question)))
flowchart =
  [(DoYouKnowWhatYouAreDoing, [(Yes, AreYouSure)
                              ,(No, DoNotDoIt)])
  ,(AreYouSure, [(Yes, DoIt)
                ,(No, DoNotDoIt)
                ,(NotReally, DoNotDoIt)])
  ,(DoIt, [(Reset, flowchartStart)])
  ,(DoNotDoIt, [(Reset, flowchartStart)])]

questionString : Question -> String
questionString q =
  case q of
    DoYouKnowWhatYouAreDoing -> "Do you know what you are doing?"
    AreYouSure -> "Are you sure?"
    DoIt -> "Do It"
    DoNotDoIt -> "Don't Do It"

answerString : Answer -> String
answerString a =
  case a of
    Yes -> "Yes"
    No -> "No"
    NotReally -> "Not Really"
    Reset -> "Start Again"

------------------------------------------------------------

answers : Question -> List Answer
answers q =
  let (_,answers) = head (filter (\ (q', _) -> q' == q) flowchart)
  in List.map fst answers

answerQuestion : Question -> Answer -> Question
answerQuestion q a =
  let (_,answers) = head (filter (\ (q', _) -> q' == q) flowchart)
      (_,response) = (head (filter (\ (a', _) -> a' == a) answers))
  in response

uiChannel : Channel Answer
uiChannel = channel NoOp

uiSignal : Signal Answer
uiSignal = subscribe uiChannel

step : Answer -> Question -> Question
step a q = answerQuestion q a

model : Signal Question
model = foldp step flowchartStart uiSignal

rootView : Question -> Html
rootView q =
  div [id "main"]
      [div [class "container"]
           [h1 [] [text "Should I Do It?"]
           ,h2 [] [text (questionString q)]
           ,div [class "answers"]
                (List.map (\a -> button [onClick (send uiChannel a)
                                        ,class "btn btn-lg btn-info"]
                                        [text (answerString a)])
                          (answers q))]]

main : Signal Html
main = rootView <~ model
