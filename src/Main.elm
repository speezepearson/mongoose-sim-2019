import Browser
import Html exposing (Html, ul, li, text, button, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random

type alias World =
    { snake : Bool
    , seed : Random.Seed
    , snakeProbability : Float
    }

type Action = Go String | Fight | Flee | Speak

step : Action -> World -> World
step action world =
    case action of
        Go dir ->
            let
                (snakeFloat, seed) = Random.step (Random.float 0 1) world.seed
                snake = (snakeFloat < world.snakeProbability)
            in
                { world | snake=snake, seed=seed }

        Fight ->
            { world | snake=False }

        Flee -> world
        Speak -> world

legalActions : World -> List Action
legalActions {snake} =
    if
        snake
    then
        [Fight, Flee, Speak]
    else
        ["north", "east", "south", "west"] |> List.map Go

type alias History =
    { current : World
    , previous : List (World, Action)
    }



type alias Model = History
type alias Msg = Action

init : Model
init =
    { current = {snake=False, snakeProbability=0.2, seed=Random.initialSeed 0}
    , previous = []
    }

update : Msg -> Model -> Model
update msg model =
    { model
        | previous = model.previous |> (::) (model.current, msg)
        , current = step msg model.current
        }

view : Model -> Html Msg
view model =
    let
        actionText a =
            case a of
                Go dir -> "Go " ++ dir
                Fight -> "Fight"
                Flee -> "Flee"
                Speak -> "Speak"

        previous =
            model.previous
            |> List.reverse
            |> List.map (\(w, a) -> li [] [describeAction w a])
            |> ul
                [ style "color" "gray"
                ]
            |> List.singleton
            |> div
                [ style "height" "20em"
                , style "overflow" "scroll"
                , style "display" "flex"
                , style "flex-direction" "column-reverse"
                ]

        buttons =
            legalActions model.current
            |> List.map (\a -> button [onClick a] [text <| actionText a])

        current = div [] [text "What do?", div [] buttons]
    in
        div [] [previous, current]

describeAction : World -> Action -> Html Msg
describeAction world action =
    (\s -> div [] [text s]) <| case action of
        Go dir ->
            if (step action world).snake then
                "You go " ++ dir ++ ".\nA SNAKE approaches; it looks like it wants to EAT your BABIES!"
            else
                "You go " ++ dir ++ ". You are still a MONGOOSE."

        Fight -> "You slay the SNAKE easily, because it is only a SNAKE, and you are a MONGOOSE."

        Flee -> "You cannot FLEE -- what if the SNAKE EATS your BABIES?"

        Speak -> "MONGOOSES cannot speak."

main = Browser.sandbox { init=init, update=update, view=view }
