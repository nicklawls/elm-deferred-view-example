module Dashboard exposing (Model, State, Msg, init, update, view, Fruit)

import Html exposing (Html)


type alias State =
    { fruits : List Fruit
    , selectedFruit : Maybe String
    }


type Model
    = Model State


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
    Model
        { fruits =
            [ { name = "Apple", votes = 0 }
            , { name = "Orange", votes = 32 }
            , { name = "Banana", votes = 3 }
            ]
        , selectedFruit = Nothing
        }


update : Msg -> Model -> Model
update msg (Model state) =
    Model <|
        case state.selectedFruit of
            Just selected ->
                case msg of
                    UpvoteFruit fruitName ->
                        { state
                            | fruits =
                                List.map
                                    (\fruit ->
                                        if fruit.name == fruitName then
                                            { fruit | votes = fruit.votes + 1 }
                                        else
                                            fruit
                                    )
                                    state.fruits
                        }

                    ExitDetails ->
                        { state | selectedFruit = Nothing }

                    _ ->
                        state

            Nothing ->
                case msg of
                    SeeDetails fruit ->
                        { state | selectedFruit = Just fruit }

                    _ ->
                        state


type alias Msgs =
    { seeDetails : String -> Msg
    , upvoteFruit : String -> Msg
    , exitDetails : Msg
    }


view : Model -> (Msgs -> State -> Html msg) -> Html msg
view (Model state) f =
    f
        { seeDetails = SeeDetails
        , upvoteFruit = UpvoteFruit
        , exitDetails = ExitDetails
        }
        state
