port module Main exposing (..)
 
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (decodeString)
import Json.Encode as Encode
import Browser
import Random exposing (Seed, initialSeed, step)
import Uuid
import Dict exposing (update)
import Json.Decode.Pipeline as Pipeline
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (spanishLocale, usLocale)

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
    
port idbGet : String -> Cmd msg
port idbAdd : RecvModel -> Cmd msg
port idbDelete : DeleteModel -> Cmd msg
port idbRecv : (RecvModel -> msg) -> Sub msg

type alias Model =
  { test : String
  , boms : List Bom
  , items: List Item
  , selectedBom : Maybe Bom
  , newBom : Bom
  , newItem : Item
  , currentSeed : Seed
  , currentUuid : Maybe Uuid.Uuid
  }

type alias DeleteModel =
  { name : String
  , uuid : String
  }

type alias RecvModel =
  { name : String
  , contents: String
  }

recvModelEncoder : RecvModel -> Encode.Value
recvModelEncoder recvModel =
  Encode.object
    [ ( "name", Encode.string recvModel.name )
    , ( "contents", Encode.string recvModel.contents )
    ]

type alias Bom =
  { uuid : String
  , name : String
  } 

bomDecoder : Decode.Decoder Bom
bomDecoder =
  Decode.succeed Bom
    |> Pipeline.required "uuid" Decode.string
    |> Pipeline.required "name" Decode.string

bomEncoder : Bom -> Encode.Value
bomEncoder bom =
  Encode.object
    [ ( "uuid", Encode.string bom.uuid )
    , ( "name", Encode.string bom.name )
    ]

initialBom : Bom
initialBom =
  { uuid = ""
  , name = ""
  } 

type alias Item =
  { uuid : String
  , name : String
  , price : Int
  , qty : Int
  , link : String
  , bomUuid: String
  }
itemEncoder : Item -> Encode.Value
itemEncoder item =
  Encode.object
    [ ( "uuid", Encode.string item.uuid )
    , ( "name", Encode.string item.name )
    , ( "price", Encode.int item.price )
    , ( "qty", Encode.int item.qty )
    , ( "link", Encode.string item.link )
    , ( "bomUuid", Encode.string item.bomUuid )
    ]

itemDecoder : Decode.Decoder Item
itemDecoder =
  Decode.succeed Item
    |> Pipeline.required "uuid" Decode.string
    |> Pipeline.required "name" Decode.string
    |> Pipeline.required "price" Decode.int
    |> Pipeline.required "qty" Decode.int
    |> Pipeline.required "link" Decode.string
    |> Pipeline.required "bomUuid" Decode.string

initialItem : Item
initialItem =
  { uuid = ""
  , name = ""
  , price = 0
  , qty = 0
  , link = ""
  , bomUuid = ""
  }



init : Int -> ( Model, Cmd Msg )
init seed =
  let
    initialModel : Model
    initialModel =
      { test = ""
      , boms = []
      , items = []
      , selectedBom = Nothing
      , newItem = initialItem
      , newBom = initialBom
      , currentSeed = initialSeed seed
      , currentUuid = Nothing
      } 
  in
  

  ( initialModel
  , Cmd.batch 
      [ idbGet "boms"
      , idbGet "items"
      ]
  )

type Msg
  = MyMsg
  | SelectBom String
  | Recv RecvModel
  | AddNewItem
  | AddNewBom
  | InputItemName String
  | InputItemQty String
  | InputBomName String
  | InputItemPrice String
  | SelectBomListItem Bom
  | DeleteItem String
  | DeleteBom String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    MyMsg ->
      ( model, Cmd.none )

    SelectBom bomName ->
      (Debug.log <| "BOM name:" ++ bomName)
      ( model, Cmd.none )

    Recv recvData -> 
      case recvData.name of
        "boms" -> 
          let
            decodedBoms = decodeString (Decode.list bomDecoder) recvData.contents
          in
            case decodedBoms of
              Ok boms ->
                ( { model | boms = boms }, Cmd.none )    
              _ ->
                ( model, Cmd.none )

        "items" ->
          let
            decodedItems = decodeString (Decode.list itemDecoder) recvData.contents
          in
            case decodedItems of
              Ok items ->
                ( { model | items = items }, Cmd.none )    
              _ ->
                ( model, Cmd.none )

        _ ->
          ( model, Cmd.none )

    AddNewItem ->
      let
        ( newUuid, newSeed ) = Random.step Uuid.uuidGenerator model.currentSeed
        item = model.newItem 
        newItem = 
          { item 
          | uuid = Uuid.toString newUuid 
          , bomUuid =
              case model.selectedBom of
                Just bom ->
                  bom.uuid
                
                _ ->
                  ""
          }
        newItemModel = { item | name = "" }
      in
        if (model.newItem.name /= "" && model.newItem.price /= 0) then
          ( { model 
            | currentUuid = Just newUuid
            , currentSeed = newSeed
            , newItem = newItemModel
            }
          , idbAdd { name = "items", contents = Encode.encode 0 (itemEncoder newItem) } 
          )
        else 
          ( model, Cmd.none )

    AddNewBom -> 
      let
        ( newUuid, newSeed ) = Random.step Uuid.uuidGenerator model.currentSeed
        bom = model.newBom
        newBom = { bom | uuid = Uuid.toString newUuid }
        newBomModel = { bom | name = "" }
      in
        if (model.newBom.name /= "") then
          ( { model 
            | currentUuid = Just newUuid
            , currentSeed = newSeed
            , newBom = newBomModel
            }
          , idbAdd { name = "boms", contents = Encode.encode 0 (bomEncoder newBom) } 
          )
        else
          ( model, Cmd.none )
      

    InputItemName itemName ->
      let
        item = model.newItem
        newItem = { item | name = itemName }
      in
        ( { model | newItem = newItem }, Cmd.none )

    InputItemQty itemQty ->
      let
        item = model.newItem
        newItem = { item | qty = Maybe.withDefault 0 (String.toInt itemQty) }
      in
        ( { model | newItem = newItem }, Cmd.none )

    InputItemPrice itemPrice ->
      let
        item = model.newItem
        newItem = { item | price = Maybe.withDefault 0 (String.toInt itemPrice) }
      in
        ( { model | newItem = newItem }, Cmd.none )

    InputBomName bomName ->
      let
        bom = model.newBom
        newBom = { bom | name = bomName }
      in
        ( { model | newBom = newBom }, Cmd.none )

    SelectBomListItem bom ->
      ( { model | selectedBom = Just bom }, Cmd.none )

    DeleteItem uuid ->
      ( model
      , idbDelete { name="items", uuid=uuid } 
      )

    DeleteBom uuid ->
      ( { model | selectedBom = Nothing }
      , idbDelete { name="boms", uuid=uuid }
      )

subscriptions : Model -> Sub Msg
subscriptions _ =
  idbRecv Recv

makeBomListItem : Bom -> Html Msg
makeBomListItem bom =
  div
    [ style "display" "flex"
    , style "justify-content" "space-between"
    , style "align-items" "center"
    , style "padding-left" "5px"
    , style "border" "1px solid gray"
    ]
    [ div
        [ style "color" "blue"
        -- , style "text-decoration" "underline"
        , style "margin-top" "1em"
        , style "margin-bottom" "1em"
        , style "cursor" "pointer"
        , onClick (SelectBomListItem bom)
        ]
        [ text bom.name ]
    , button
        [ onClick (DeleteBom bom.uuid)
        ]
        [ text "Delete" ]
    ]
  
makeItemListItem : Item -> Html Msg
makeItemListItem item =
  tr 
    [ onClick (DeleteItem item.uuid)
    ]
    [ td [] [ text item.name ]
    , td [] [ text <| String.fromInt item.qty ]
    , td [] [ text <| "Rp" ++ format spanishLocale (toFloat item.price) ]
    ]
  


view : Model -> Html Msg
view model =
  div [ style "margin" "1em", style "padding" "0" ]
    [ div 
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "flex-direction" "column"
        ]
        [ h1 [] [ text "Summer" ]
        , div 
            [ style "font-style" "oblique" 
            , style "margin-bottom" "1em"
            , style "display" "flex"
            , style "align-items" "center"
            , style "justify-content" "center"
            , style "text-align" "center"
            ] 
            [ text "A program to save and sum BOM lists in your browser storage" ]
        ]
    , div 
        [ style "display" "flex"
        , style "justify-content" "space-between"
        ]
        [ button [] [ text "Import (WIP)" ]
        , button [] [ text "Export (WIP)" ]
        ]
    , div 
        [ style "margin-top" "1em"
        , style "margin-bottom" "1em"
        ]
        [ text "Select BOM" ]
    , div []
        (List.map makeBomListItem model.boms)
    , div
        [ style "margin-top" "1em" 
        , style "margin-bottom" "1em"
        , style "display" "flex"
        , style "flex-direction" "column"
        ]
        [ text "Add new BOM:"
        , div 
            [ style "display" "flex"
            , style "flex-direction" "column"
            ]
            [ input 
                [ placeholder "New BOM Name..."
                , value model.newBom.name
                , onInput InputBomName
                , style "flex-grow" "1"
                ] 
                []
            , button 
                [ style "flex-grow" "1"
                , onClick AddNewBom
                ] 
                [ text "Add" ]

            ]
        ]
    , case model.selectedBom of
        Nothing ->
          div [] []
        
        Just bom ->
          div []
            [ div
                [ style "margin-top" "1em" 
                , style "margin-bottom" "1em"
                ]
                [ h3
                    []
                    [ text <| "Item List for " ++ bom.name ]
                ]
            , div
                [ style "display" "flex"
                , style "flex-direction" "column"
                ]
                [ div [] [ text "Insert new Item..." ]
                , div []
                    [ input 
                        [ placeholder "New Item Name..."
                        , value model.newItem.name
                        , onInput InputItemName
                        , style "width" "100%"
                        ]
                        []
                    , div 
                        [ style "display" "flex" 
                        , style "align-items" "center"
                        ]
                        [ text "Qty"
                        , input 
                            [ placeholder "Qty..."
                            , value (String.fromInt model.newItem.qty)
                            , onInput InputItemQty
                            , type_ "number"
                            , style "width" "100%"
                            , style "margin-left" "1em"
                            ]
                            []
                        ] 
                    , div 
                        [ style "display" "flex" 
                        , style "align-items" "center"
                        ]
                        [ div [] [ text "IDR" ]
                        , input 
                            [ type_ "number"
                            , placeholder "New Item Price/Unit..."
                            , value (String.fromInt model.newItem.price)
                            , onInput InputItemPrice
                            , style "width" "100%"
                            , style "margin-left" "1em"
                            ]
                            []
                        ]
                    ]
                ]
            , div 
                [ style "display" "flex"
                ]
                [ button 
                    [ style "width" "100%" 
                    , onClick AddNewItem
                    ] 
                    [ text "Insert" ] ]
            , div 
                [ style "margin-top" "1em" ] 
                [ div []
                    [ text "(Click to delete)" ]
                , table 
                    [ attribute "border" "1" 
                    , style "width" "100%"
                    ]
                    ([ tr []
                        [ th [] [ text "Name" ]
                        , th [] [ text "Qty" ]
                        , th [] [ text "Price" ]
                        ]
                    ]
                    ++
                    (model.items
                      |> List.filter (\item -> item.bomUuid == bom.uuid)
                      |> List.map makeItemListItem)
                    )
                ]
            , div [ style "margin-top" "1em" ]
                [ strong [] [ text "Total:" ]
                , div 
                    [] 
                    [ h3
                        [ style "color" "green" ]
                        [ text <| "IDR" ++ format spanishLocale 
                            (model.items
                              |> List.filter (\item -> item.bomUuid == bom.uuid)
                              |> List.foldl (\item acc -> acc + item.qty * item.price) 0
                              |> toFloat
                            ) 
                        ]
                    ]
                ]
            ]
    ]
