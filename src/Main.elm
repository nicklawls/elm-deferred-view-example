module Main exposing (..)

import Dashboard exposing (Modal(..))
import Html exposing (..)
import Html.Attributes exposing (class)
import OrderFruit


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }


type Page
    = Dashboard Dashboard.Model
    | OrderFruit OrderFruit.Model


type alias Model =
    { page : Page
    , modal : Modal
    }


type Msg
    = ToDashboard
    | ToOrderFruit String
    | ProfileMsg OrderFruit.Msg
    | DashboardMsg Dashboard.Msg


init : Model
init =
    { page = Dashboard Dashboard.init
    , modal = Closed
    }


update : Msg -> Model -> Model
update msg model =
    case ( msg, model.page ) of
        ( ToDashboard, OrderFruit _ ) ->
            { model | page = Dashboard Dashboard.init }

        ( ProfileMsg profileMsg, OrderFruit profileModel ) ->
            { model | page = OrderFruit <| OrderFruit.update profileMsg profileModel }

        ( ToOrderFruit fruit, Dashboard _ ) ->
            { model | page = OrderFruit (OrderFruit.init fruit), modal = Closed }

        ( DashboardMsg dashboardMsg, Dashboard dashboardModel ) ->
            let
                ( dashboard, modal ) =
                    Dashboard.update dashboardMsg dashboardModel
            in
                { model | page = Dashboard dashboard, modal = modal }

        _ ->
            model


view : Model -> Html Msg
view model =
    div [ class "page" ]
        [ case model.page of
            Dashboard modelDashboard ->
                Dashboard.view
                    { orderFruit = ToOrderFruit
                    , toMsg = DashboardMsg
                    }
                    modelDashboard

            OrderFruit modelProfile ->
                OrderFruit.view modelProfile
                    |> Html.map ProfileMsg
        , Dashboard.viewModal model.modal
            |> Html.map DashboardMsg
        ]
