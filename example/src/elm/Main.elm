module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import FancyTable exposing (..)

-- APP
-- main : Program Never Int Msg
-- main =
--   Html.beginnerProgram { model = model, view = view, update = update }
main : Program Never Model Msg
main =
  Html.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map FancyTableMsg <| FancyTable.subscriptions model.fancyTable
    ]

init : (Model, Cmd Msg)
init =
  let
    tableData =
      [ ["1", "2", "3", "4", "5", "6"]
      , ["7", "8", "9", "10", "11", "12"]
      , ["13", "14", "15", "16", "17" , "18"]
      ]
    headerColors = ["Red", "Blue", "Yellow", "Green", "Pink", "Purple"]
    headers =
      List.map (\c -> (c, Html.span [ style [ ("background-color", c), ("display", "block"), ("height", "100%") ] ] [ Html.text c ])
               ) headerColors
    fancyTable 
      = FancyTable.init
        |> setSettings { resizeColumns = True, reorderColumns = True, hideColumns = True }
        |> setTableHeadersFromHtml { defaultHeaderSettings | minWidth = 75 } headers
        |> setTableRows (List.map(\d -> getTableRowFromStrings d) tableData)
      
    model =
      { fancyTable = fancyTable
      }
  in
      (model, Cmd.none)

-- MODEL
type alias Model = 
  { fancyTable : FancyTable
  }

model : number
model = 0


-- UPDATE
type Msg 
  = FancyTableMsg FancyTable.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FancyTableMsg msg ->
            let
                ( updatedModelFancyTable, cmd ) =
                    FancyTable.update model.fancyTable msg
            in
                ( { model | fancyTable = updatedModelFancyTable }, Cmd.map FancyTableMsg cmd )

-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib
view : Model -> Html Msg
view model =
  div []
    [ Html.map FancyTableMsg <| FancyTable.view model.fancyTable
    ]
