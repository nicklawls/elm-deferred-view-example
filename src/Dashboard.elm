module Dashboard exposing (Model, Msg, init, update, view, Modal, viewModal, Modal(..))

import Dialog
import Html exposing (..)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { fruits : List Fruit
    , selectedFruit : Maybe String
    }


type alias Fruit =
    { name : String
    , votes : Int
    }


type Msg
    = UpvoteFruit String
    | SeeDetails String
    | ExitDetails


init : Model
init =
    { fruits =
        [ { name = "Apple", votes = 0 }
        , { name = "Orange", votes = 32 }
        , { name = "Banana", votes = 3 }
        ]
    , selectedFruit = Nothing
    }


update : Msg -> Model -> ( Model, Modal )
update msg model =
    case model.selectedFruit of
        Just selected ->
            ( case msg of
                UpvoteFruit fruitName ->
                    { model
                        | fruits =
                            List.map
                                (\fruit ->
                                    if fruit.name == fruitName then
                                        { fruit | votes = fruit.votes + 1 }
                                    else
                                        fruit
                                )
                                model.fruits
                    }

                ExitDetails ->
                    { model | selectedFruit = Nothing }

                _ ->
                    model
            , model.fruits
                |> List.filter ((==) selected << .name)
                |> List.head
                |> Maybe.map Open
                |> Maybe.withDefault Closed
            )

        Nothing ->
            ( case msg of
                SeeDetails fruit ->
                    { model | selectedFruit = Just fruit }

                _ ->
                    model
            , Closed
            )


view : { orderFruit : String -> msg, toMsg : Msg -> msg } -> Model -> Html msg
view { orderFruit, toMsg } model =
    div [ class "dashboard" ]
        [ h1 [] [ text "Fruits" ]
        , ol []
            (List.map
                (\fruit ->
                    div []
                        [ Html.map toMsg (viewFruit fruit)
                        , button [ onClick (orderFruit fruit.name) ] [ text "Order" ]
                        ]
                )
                model.fruits
            )
        ]


type Modal
    = Open Fruit
    | Closed


viewModal : Modal -> Html Msg
viewModal modal =
    Dialog.view <|
        case modal of
            Open fruit ->
                Just
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

            Closed ->
                Nothing


viewFruit : Fruit -> Html Msg
viewFruit fruit =
    li [ onClick (SeeDetails fruit.name) ]
        [ text (fruit.name ++ " " ++ toString fruit.votes) ]
