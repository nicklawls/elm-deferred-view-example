module OrderFruit exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { name : String
    , state : State
    }


type State
    = Confirming
    | Paying
    | Done


type Msg
    = Confirm
    | Pay


init : String -> Model
init name =
    { name = name, state = Confirming }


update : Msg -> Model -> Model
update msg model =
    case model.state of
        Confirming ->
            case msg of
                Confirm ->
                    { model | state = Paying }

                _ ->
                    model

        Paying ->
            case msg of
                Pay ->
                    { model | state = Done }

                _ ->
                    model

        Done ->
            model


view : Model -> Html Msg
view model =
    div [ class "dashboard" ] <|
        case model.state of
            Confirming ->
                [ h1 [] [ text model.name ]
                , button [ onClick Confirm ] [ text "Confirm" ]
                ]

            Paying ->
                [ h1 [] [ text model.name ]
                , button [ onClick Pay ] [ text "Pay" ]
                ]

            Done ->
                [ h2 [] [ text "Done" ] ]
