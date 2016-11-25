port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)


port generationRequest : List Int -> Cmd msg


generate : Model -> Cmd Msg
generate model =
    generationRequest (List.map (\p -> p.id) model.people)


port generationResponse : (List Int -> msg) -> Sub msg


type alias Person =
    { name : String, id : Int }


type alias Model =
    { people : List Person, result : Dict Int Int, newPerson : String }


init : ( Model, Cmd Msg )
init =
    ( (Model [] Dict.empty ""), Cmd.none )


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
        , createResultTable model
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
        button [ class ("button is-primary is-large " ++ disabledClass), onClick Generate, id "generate" ] [ text "Genereer" ]


createResultTable : Model -> Html Msg
createResultTable model =
    let
        resultList =
            Dict.toList model.result

        createRow =
            createResultRow model
    in
        table [ class "table is-striped" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Resultaat" ]
                    , th [] []
                    , th [] []
                    ]
                ]
            , tbody [] (resultList |> List.map createRow)
            ]


createResultRow : Model -> ( Int, Int ) -> Html Msg
createResultRow model ( id, for ) =
    let
        subject =
            getNameById model id

        target =
            getNameById model for
    in
        tr []
            [ td [] [ text subject ]
            , td [] [ text "koopt voor" ]
            , td [] [ text target ]
            ]


getNameById : Model -> Int -> String
getNameById model id =
    List.filter (\p -> p.id == id) model.people
        |> List.head
        |> Maybe.map (\p -> p.name)
        |> Maybe.withDefault "???"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateNewPerson newValue ->
            ( { model | newPerson = newValue }, Cmd.none )

        AddPerson ->
            addPerson model

        RemovePerson id ->
            removePerson model id

        Generate ->
            ( model, generate model )

        Result ids ->
            processResult model ids


removePerson : Model -> Int -> ( Model, Cmd Msg )
removePerson model id =
    let
        nextModel =
            { model | people = (List.filter (\p -> p.id /= id) model.people) }
    in
        if (List.length nextModel.people) > 1 then
            ( nextModel, generate nextModel )
        else
            ( { nextModel | result = Dict.empty }, Cmd.none )


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


processResult : Model -> List Int -> ( Model, Cmd Msg )
processResult model ids =
    let
        personIds =
            List.map (\p -> p.id) model.people

        merged =
            List.map2 (,) personIds ids
    in
        ( { model | result = Dict.fromList merged }, Cmd.none )


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
