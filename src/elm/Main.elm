port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
import Debug


port generationRequest : List Int -> Cmd msg


port generationResponse : (List Int -> msg) -> Sub msg


type alias Person =
    { name : String, id : Int }


type alias Model =
    { people : List Person, result : Dict Int Int, newPerson : String }


init : ( Model, Cmd Msg )
init =
    ( (Model [ Person "Florian" 1, Person "Jochen" 2, Person "Korneel" 3 ] Dict.empty ""), Cmd.none )


type Msg
    = UpdateNewPerson String
    | AddPerson
    | RemovePerson Int
    | Generate
    | Result (List Int)


view : Model -> Html Msg
view model =
    div []
        [ createForm model.newPerson
        , createPeopleTable model.people
        , createGenerateButton model.people
        ]


formButtonClass : String -> String
formButtonClass currentName =
    if String.isEmpty currentName then
        "button is-info is-disabled"
    else
        "button is-info"


createForm : String -> Html Msg
createForm currentName =
    Html.form [ class "entry box", onSubmit AddPerson ]
        [ h4 [ class "subtitle" ] [ text "Personen toevoegen" ]
        , p [ class "control has-addons" ]
            [ input [ class "input", type_ "text", placeholder "Vul een nieuwe naam in", onInput UpdateNewPerson, value currentName ] []
            , a [ class (formButtonClass currentName), onClick AddPerson ] [ text "Toevoegen" ]
            ]
        ]


createPeopleTable : List Person -> Html Msg
createPeopleTable people =
    table [ class "table is-striped" ]
        [ thead []
            [ tr []
                [ th [] [ text "Naam" ]
                , th [ colspan 3 ] [ text "Verwijderen" ]
                ]
            ]
        , tbody [] (List.map createPeopleTableRow people)
        ]


createPeopleTableRow : Person -> Html Msg
createPeopleTableRow person =
    tr []
        [ td [] [ text person.name ]
        , td [ class "is-icon" ]
            [ div [ class "button is-danger is-small", onClick (RemovePerson person.id) ]
                [ i
                    [ class "fa fa-times" ]
                    []
                ]
            ]
        ]


createGenerateButton : List Person -> Html Msg
createGenerateButton people =
    let
        disabledClass =
            if List.length people <= 1 then
                "is-disabled"
            else
                ""
    in
        button [ class ("button is-primary is-large " ++ disabledClass), onClick Generate ] [ text "Genereer" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateNewPerson newValue ->
            ( { model | newPerson = newValue }, Cmd.none )

        AddPerson ->
            addPerson model

        RemovePerson id ->
            ( { model | people = (List.filter (\p -> p.id /= id) model.people) }, Cmd.none )

        Generate ->
            ( model, generationRequest (List.map (\p -> p.id) model.people) )

        Result ids ->
            let
                foo =
                    Debug.log "from JS" ids
            in
                ( model, Cmd.none )


addPerson : Model -> ( Model, Cmd Msg )
addPerson model =
    let
        nextId =
            List.map (\p -> p.id) model.people
                |> List.maximum
                |> Maybe.withDefault 0
                |> (+) 1

        nextPerson =
            Person model.newPerson nextId
    in
        ( { model | people = nextPerson :: model.people, newPerson = "" }, Cmd.none )


generateOrder : Model -> ( Model, Cmd Msg )
generateOrder model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    generationResponse Result


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
