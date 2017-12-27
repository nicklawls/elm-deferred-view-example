module Dashboard exposing (Model, Msg(..), init, update, Fruit)


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


update : Msg -> Model -> Model
update msg model =
    case model.selectedFruit of
        Just selected ->
            case msg of
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

        Nothing ->
            case msg of
                SeeDetails fruit ->
                    { model | selectedFruit = Just fruit }

                _ ->
                    model
