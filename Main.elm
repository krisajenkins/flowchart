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

uiChannel : Channel Answer
uiChannel = channel NoOp

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

------------------------------------------------------------

answers : Question -> List Answer
answers q = List.map fst (lookup q flowchart)

{-| Lookup the value in an association list structure. -}
lookup : a -> List (a,b) -> b
lookup x xs = snd (head (filter ((==) x << fst) xs))

answerQuestion : Question -> Answer -> Question
answerQuestion q a = lookup q flowchart |> lookup a

step : Answer -> Question -> Question
step = flip answerQuestion

model : Signal Question
model = foldp step flowchartStart (subscribe uiChannel)

main : Signal Html
main = rootView <~ model
