module Main exposing (..)

import Dashboard exposing (Fruit, Msg(..))
import Dialog
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
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
    { page : Page }


type Msg
    = ToDashboard
    | ToOrderFruit String
    | ProfileMsg OrderFruit.Msg
    | DashboardMsg Dashboard.Msg


init : Model
init =
    { page = Dashboard Dashboard.init }


update : Msg -> Model -> Model
update msg model =
    case ( msg, model.page ) of
        ( ToDashboard, OrderFruit _ ) ->
            { model | page = Dashboard Dashboard.init }

        ( ProfileMsg profileMsg, OrderFruit profileModel ) ->
            { model | page = OrderFruit <| OrderFruit.update profileMsg profileModel }

        ( ToOrderFruit fruit, Dashboard _ ) ->
            { model | page = OrderFruit (OrderFruit.init fruit) }

        ( DashboardMsg dashboardMsg, Dashboard dashboardModel ) ->
            let
                dashboard =
                    Dashboard.update dashboardMsg dashboardModel
            in
                { model | page = Dashboard dashboard }

        _ ->
            model


view : Model -> Html Msg
view model =
    case model.page of
        Dashboard dashboardModel ->
            div [ class "page" ]
                [ div [ class "dashboard" ]
                    [ h1 [] [ text "Fruits" ]
                    , ol []
                        (List.map
                            (\fruit ->
                                div []
                                    [ Html.map DashboardMsg (viewFruit fruit)
                                    , button [ onClick (ToOrderFruit fruit.name) ] [ text "Order" ]
                                    ]
                            )
                            dashboardModel.fruits
                        )
                    ]
                , dashboardModel.selectedFruit
                    |> Maybe.andThen
                        (\selectedName ->
                            dashboardModel.fruits
                                |> List.filter (\fruit -> fruit.name == selectedName)
                                |> List.head
                        )
                    |> Maybe.map
                        (\fruit ->
                            { closeMessage = Just ExitDetails
                            , body =
                                Just
                                    (div []
                                        [ viewFruit fruit
                                        , button [ onClick (UpvoteFruit fruit.name) ] [ text "Upvote" ]
                                        ]
                                    )
                            , header = Just (h2 [] [ text fruit.name ])
                            , footer = Nothing
                            , containerClass = Just "fruit-modal"
                            }
                        )
                    |> Dialog.view
                    |> Html.map DashboardMsg
                ]

        OrderFruit modelProfile ->
            div [ class "page" ]
                [ OrderFruit.view modelProfile
                    |> Html.map ProfileMsg
                ]


viewFruit : Fruit -> Html Dashboard.Msg
viewFruit fruit =
    li [ onClick (SeeDetails fruit.name) ]
        [ text (fruit.name ++ " " ++ toString fruit.votes) ]
