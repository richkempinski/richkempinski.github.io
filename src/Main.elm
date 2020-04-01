module Main exposing (..)

-- simple chess timer
-- 
-- includes:
--    one button for each player's clock, button face is the clock time
--    a pause button to stop time on both clocks
--    a reset button to start over - sets time on both clocks to default time
--    a increase time button to add more time to both clocks
--    a decrease time button to subtract time from both clocks

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Time

-- MAIN

import Html exposing (..)
import Html.Attributes exposing (..)

main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view  }

-- MODEL

type Player = He | She | Neither

startTimeDefault = 300
deltaTime = 30
            
type alias Model =
    {
        hisTime : Int,
        herTime : Int,
        currentPlayer : Player
    }

init : () -> (Model, Cmd Msg)
init _ = 
  ( Model startTimeDefault startTimeDefault Neither, Cmd.none )

-- UPDATE

type Msg
  = IncrementTimeLeft
  | DecrementTimeLeft
  | Tick Time.Posix
  | Pause
  | Reset
  | HeStops
  | SheStops

lostTime t = if t <= 0 then 0 else t

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
  case msg of
    HeStops ->
      ({ model | currentPlayer = She }, Cmd.none)
          
    SheStops ->
      ({ model | currentPlayer = He }, Cmd.none)
          
    IncrementTimeLeft ->
      ({ model | hisTime = model.hisTime + deltaTime, herTime = model.herTime + deltaTime }, Cmd.none)

    DecrementTimeLeft ->
      ({ model | hisTime = lostTime (model.hisTime - deltaTime), herTime = lostTime (model.herTime - deltaTime) }, Cmd.none)

    Tick _ ->
        case model.currentPlayer of
            She ->
               ({ model | herTime = lostTime (model.herTime - 1) }, Cmd.none)
            He ->
               ({ model | hisTime = lostTime (model.hisTime - 1) }, Cmd.none)
            Neither ->
               (model, Cmd.none)
    Pause ->
      ({ model | currentPlayer = Neither }, Cmd.none)

    Reset ->
      ({ model | currentPlayer = Neither, hisTime = startTimeDefault, herTime = startTimeDefault }, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick

-- VIEW

activeBtnColor currPlayer isThisPerson = if currPlayer == isThisPerson then "yellow" else "white"

btnColor m isThisPerson = if getTimeForPlayer m isThisPerson == 0 then "red" else activeBtnColor m.currentPlayer isThisPerson

minsec seconds = String.padLeft 2 '0' (String.fromInt (seconds // 60)) ++ ":" ++  String.padLeft 2 '0' (String.fromInt (remainderBy 60 seconds))

getTimeForPlayer model player = case player of
                                    He -> model.hisTime
                                    She -> model.herTime
                                    Neither -> 1

view : Model -> Html Msg
view model =
  div [style "background-color" "white"
     , style "height" "90px"
     , style "width" "100%"
     , style "font-size" "50px"]

     [
      button [ onClick HeStops, style "font-size" "200px", style "background-color" (btnColor model He)]
             [ text (minsec model.hisTime) ],

      button [ onClick DecrementTimeLeft]
             [ text "-" ],

      button [ onClick IncrementTimeLeft]
             [ text "+" ],

      button [ onClick Reset] 
             [ text "Reset" ],

      button [ onClick Pause] 
             [ text "Pause" ],

      button [ onClick SheStops, style "font-size" "200px", style "background-color" (btnColor model She)]
             [ text (minsec model.herTime) ]
    ]
