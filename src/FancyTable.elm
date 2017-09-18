module FancyTable
    exposing 
        ( FancyTable
        , Settings
        , Header
        , HeaderSettings
        , Msg
        , defaultSettings
        , setSettings
        , defaultHeaderSettings
        , setHeaderMinWidth
        , setHeaderMaxWidth
        , setHeaderWidth
        , setTableHeaders
        , setTableHeadersFromStrings
        , setTableHeadersFromHtml
        , setTableRows
        , getTableRowFromStrings
        , getTableRowFromHtml
        , subscriptions
        , update
        , init
        , view
        )

{-| An fancy table that allows you to resize and reorder columns

# Model
@docs FancyTable, Header, Settings, HeaderSettings

# Update
@docs Msg, update, subscriptions

# Configuring the table
@docs init, setTableHeaders, setTableHeadersFromStrings, setTableHeadersFromHtml, setTableRows, getTableRowFromStrings, getTableRowFromHtml, setSettings, defaultSettings, defaultHeaderSettings, setHeaderMinWidth, setHeaderMaxWidth, setHeaderWidth

# View
@docs view

-}

import Html exposing (Html, Attribute)
import Html.Attributes exposing (style, class)
import Html.Events exposing (on)
import Mouse exposing (Position)
import Json.Decode as Decode
import ContextMenu exposing (..)
import Color exposing (Color)
import FontAwesome

{-| The base model of the table
-}
type FancyTable
    = FancyTable Model

{-| Represents your table as a model
-}
type alias Model =
    { headers : List Header
    , rows : List TableRow
    , resizeHeader : Maybe Drag
    , reorderHeader : Maybe Drag
    , settings : Settings
    , contextMenu : ContextMenuModel
    }

{-| Represents the table settings to be able to resize/reorder columns
-}
type alias Settings =
    { resizeColumns : Bool
    , reorderColumns : Bool
    , hideColumns : Bool
    }

{-| Default Header Settings
-}
defaultSettings : Settings
defaultSettings =
    { resizeColumns = True
    , reorderColumns = True
    , hideColumns = True
    }

{-| Set the table settings
-}
setSettings : Settings -> FancyTable -> FancyTable
setSettings s (FancyTable ({ settings } as model)) =
    FancyTable { model | settings = s }

{-| Represents the content that goes into your THead
# index = order indes
# name = the string name for the column
# content = html inside the header
# settings = settings for this header
-}
type alias Header =
    { index : Int
    , name : String
    , content : Html Msg
    , settings : HeaderSettings
    }

{-| Represents the settings for the Header
-}
type alias HeaderSettings =
    { minWidth : Int
    , maxWidth : Int
    , width : Int
    , isVisible : Bool
    }

{-| Default Header Settings
-}
defaultHeaderSettings : HeaderSettings
defaultHeaderSettings =
    { minWidth = 140
    , maxWidth = 1000
    , width = 140
    , isVisible = True
    }

type alias TableRow = List TableData

type alias TableData =
    { index : Int
    , content : Html Msg
    , isVisible : Bool
    }

{-| Keeps track of the Header that is being dragged
and its position
-}
type alias Drag =
    { index : Int
    , startX : Int
    , currentX : Int
    }

{-| The basic type accepted by the update
-}
type Msg
    = ResizeDragStart Int Position
    | ResizeDragAt Position
    | ResizeDragEnd Position
    | ReorderDragStart Int Position
    | ReorderDragAt Position
    | ReorderDragEnd Position
    | ToggleHeader Int
    | ContextMenuMsg (ContextMenu.Msg Context)

type Context
    = HeaderContext

type alias ContextMenuModel =
    { menu : ContextMenu Context
    , config : ContextMenu.Config
    }

{-| Context Menu Config for winChrome
-}
winChrome : ContextMenu.Config
winChrome =
  { defaultConfig
  | direction = RightBottom
  , overflowX = Shift
  , overflowY = Mirror
  , containerColor = Color.white
  , hoverColor = Color.lightGray
  , invertText = False
  , cursor = Arrow
  , rounded = False
  }

{-| Sets the min width of the header when rendered.
-}
setHeaderMinWidth : Int -> Header -> Header
setHeaderMinWidth minWidth header =
    let
        settings = header.settings
    in
        { header | settings = { settings | minWidth = minWidth } }

{-| Sets the max width of the header when rendered.
-}
setHeaderMaxWidth : Int -> Header -> Header
setHeaderMaxWidth maxWidth header =
    let
        settings = header.settings
    in
        { header | settings = { settings | maxWidth = maxWidth } }

{-| Sets the width of the header when rendered.
-}
setHeaderWidth : Int -> Header -> Header
setHeaderWidth width header =
    let
        settings = header.settings
    in
        { header | settings = { settings | width = width } }

{-| Sets the talbe headers from a list of Header
-}
setTableHeaders : List Header -> FancyTable -> FancyTable
setTableHeaders h (FancyTable ({ headers } as model)) =
    FancyTable { model | headers = h }

{-| Sets the table headers from a list of strings (content)
-}
setTableHeadersFromStrings : HeaderSettings -> List String -> FancyTable -> FancyTable
setTableHeadersFromStrings settings headerStrings (FancyTable ({ headers } as model)) =
    let
        tableHeaders =
            List.indexedMap (\index content -> { index = index, name = content, content = Html.text content, settings = settings }) headerStrings
    in
        FancyTable { model | headers = tableHeaders }

{-| Sets the table headers from a list of (String, Html Msg) (content)
where the String is the header name and the Html Msg is the header content
-}
setTableHeadersFromHtml : HeaderSettings -> List (String, (Html Msg)) -> FancyTable -> FancyTable
setTableHeadersFromHtml settings headerList (FancyTable ({ headers } as model)) =
    let
        tableHeaders =
            List.indexedMap (\index content -> { index = index, name = (Tuple.first content), content = (Tuple.second content), settings = settings }) headerList
    in
        FancyTable { model | headers = tableHeaders }

{-| Sets the table rows from a list of TableData
-}
setTableRows : List TableRow -> FancyTable -> FancyTable
setTableRows tableRows (FancyTable ({ rows } as model)) =
    FancyTable { model | rows = tableRows }

{-| get table row from a list of strings (content)
-}
getTableRowFromStrings : List String -> TableRow
getTableRowFromStrings colStrings =
    let
        columns =
            List.indexedMap (\index content -> { index = index, content = (Html.text content), isVisible = True }) colStrings
    in
        columns

{-| get table row from a list of Html Msg (content)
-}
getTableRowFromHtml : List (Html Msg) -> TableRow
getTableRowFromHtml colStrings =
    let
        columns =
            List.indexedMap (\index content -> { index = index, content = content, isVisible = True }) colStrings
    in
        columns

{-| Returns the subscriptions necessary to run
-}
subscriptions : FancyTable -> Sub Msg
subscriptions (FancyTable model) =
    let
        resizeCmds =
            case model.resizeHeader of
                Nothing ->
                    Sub.none
                _ ->
                    Sub.batch [ Mouse.moves ResizeDragAt, Mouse.ups ResizeDragEnd ]
        reorderCmds =
            case model.reorderHeader of
                Nothing ->
                    Sub.none
                _ ->
                    Sub.batch [ Mouse.moves ReorderDragAt, Mouse.ups ReorderDragEnd ]
        hideCmds =
            Sub.map ContextMenuMsg (ContextMenu.subscriptions model.contextMenu.menu)
    in
        Sub.batch [ resizeCmds, reorderCmds, hideCmds]

{-| takes a model and a message and applies it to create an updated model
-}
update : FancyTable -> Msg -> (FancyTable, Cmd Msg)
update (FancyTable model) msg =
    case msg of
        ResizeDragStart index position ->
            ( FancyTable { model | resizeHeader = Just (Drag index position.x position.x) }
            , Cmd.none
            )
        ResizeDragAt position ->
            let
                newModel = { model | resizeHeader = updateDrag model.resizeHeader position }
            in
                ( FancyTable (updateWidth newModel)
                , Cmd.none
                )
        ResizeDragEnd _ ->
            ( FancyTable { model | resizeHeader = Nothing }
            , Cmd.none
            )
        ReorderDragStart index position ->
            ( FancyTable { model | reorderHeader = Just (Drag index position.x position.x) }
            , Cmd.none
            )
        ReorderDragAt position ->
            let
                newModel = { model | reorderHeader = updateDrag model.reorderHeader position }
            in
                ( FancyTable newModel
                , Cmd.none
                )
        ReorderDragEnd _ ->
            let
                newModel = reorderHeaders model
            in
                ( FancyTable newModel
                , Cmd.none
                )
        ContextMenuMsg msg ->
            let
                (contextMenu, cmd) =
                    ContextMenu.update msg model.contextMenu.menu
                cm = model.contextMenu
                newContextMenu = { cm | menu = contextMenu }
                newModel = { model | contextMenu = newContextMenu }
            in
                ( FancyTable newModel
                , Cmd.map ContextMenuMsg cmd
                )
        ToggleHeader index ->
            let
                newModel = updateHeaderVisibility index model
            in
                ( FancyTable newModel
                , Cmd.none
                )

updateHeaderVisibility : Int -> Model -> Model
updateHeaderVisibility index model =
    let
        headers = model.headers
        rows = model.rows
        visible =
            List.filter (\h -> h.settings.isVisible == True) headers
        (updatedHeaders, updatedRows) =
            case List.length visible of
                1 ->
                    let
                        newHeaders =
                            List.map (\h ->
                                        if h.index == index then
                                            let
                                                settings = h.settings
                                            in
                                                { h | settings = { settings | isVisible = True } }
                                        else
                                            h
                                    ) headers
                        newRows =
                            List.map (\r ->
                                        List.map (\td ->
                                                    if td.index == index then
                                                        { td | isVisible = True }
                                                    else
                                                        td  
                                                ) r
                                    ) rows
                    in
                        (newHeaders, newRows)
                _ ->
                    let
                        newHeaders =
                            List.map (\h ->
                                        if h.index == index then
                                            let
                                                settings = h.settings
                                            in
                                                { h | settings = { settings | isVisible = not settings.isVisible } }
                                        else
                                            h
                                    ) headers
                        newRows =
                            List.map (\r ->
                                        List.map (\td ->
                                                    if td.index == index then
                                                        { td | isVisible = not td.isVisible }
                                                    else
                                                        td  
                                                ) r
                                    ) rows
                    in
                        (newHeaders, newRows)
    in
        { model | headers = updatedHeaders, rows = updatedRows }

updateDrag : Maybe Drag -> Position -> Maybe Drag
updateDrag headerDrag position =
    case headerDrag of
        Just a ->
            Just (Drag a.index a.startX position.x)
        _ -> Nothing

updateWidth : Model -> Model
updateWidth model =
    let
        d = model.resizeHeader
        (index, difference, updatedResizeHeader) =
            case d of
                Just a -> 
                    let
                        currentX = a.currentX
                        updatedResizeHeader = Just { a | startX = currentX }
                    in
                        (a.index, a.currentX - a.startX, updatedResizeHeader )
                _ -> (0,0,Nothing)
        updatedHeaders = 
            List.map (\h ->
                        if h.index == index
                        then setHeaderWidth (getNewWidth h difference) h
                        else h
                     ) model.headers
    in
        { model | headers = updatedHeaders, resizeHeader = updatedResizeHeader }

reorderHeaders : Model -> Model
reorderHeaders model =
    case model.reorderHeader of
        Nothing ->
            { model | reorderHeader = Nothing }
        Just { index, startX, currentX } ->
            let
                sortedHeaders = (List.sortBy .index model.headers)
                offset = findOffset index (currentX - startX) sortedHeaders
                headerOrder = updateHeaderOrder index offset sortedHeaders
                updatedHeaders = List.indexedMap (\index header -> { header | index = index }) headerOrder
                sortedColumns = List.map (\r -> (List.sortBy .index r)) model.rows
                columnOrder = List.map (\r -> (updateColumnOrder index offset r)) sortedColumns
                updatedRows = List.map (\r -> List.indexedMap (\index column -> { column | index = index }) r) columnOrder
            in
                { model | headers = updatedHeaders, rows = updatedRows, reorderHeader = Nothing }

findOffset : Int -> Int -> List Header -> Int
findOffset index pickles headers =
    let
        newHeaders =
            if pickles > 0 then
                List.filter (\h -> h.index > index) headers
            else
                List.reverse (List.filter (\h -> h.index < index) headers)
        newPickles =
            if pickles > 0 then
                pickles
            else
                pickles * (-1)
        moves = countMoves newPickles headers 0
    in
        if pickles > 0 then
            moves
        else
            moves * (-1)

countMoves : Int -> List Header -> Int -> Int
countMoves pickles headers moves =
    case List.head headers of
        Just a ->
            let
                halfWayPickles = floor ((toFloat a.settings.width) / (2.0))
            in
                if (pickles >= halfWayPickles) then
                    countMoves (pickles - a.settings.width) (List.drop 1 headers) (moves + 1)
                else
                    moves
        Nothing ->
            moves

updateHeaderOrder : Int -> Int -> List Header -> List Header
updateHeaderOrder fromIndex offset headers =
    let
        headersWithoutMovedColumn =
            List.take fromIndex headers ++ List.drop (fromIndex + 1) headers
        moved =
            List.take 1 <| List.drop fromIndex headers
    in
        List.take (fromIndex + offset) headersWithoutMovedColumn ++ moved ++ List.drop (fromIndex + offset) headersWithoutMovedColumn

updateColumnOrder : Int -> Int -> TableRow -> TableRow
updateColumnOrder fromIndex offset columns =
    let
        columnsWithoutMovedColumn =
            List.take fromIndex columns ++ List.drop (fromIndex + 1) columns
        moved =
            List.take 1 <| List.drop fromIndex columns
    in
        List.take (fromIndex + offset) columnsWithoutMovedColumn ++ moved ++ List.drop (fromIndex + offset) columnsWithoutMovedColumn
        
getNewWidth : Header -> Int -> Int
getNewWidth header diff =
    let
        newWidth = header.settings.width + diff
    in
        if newWidth > header.settings.maxWidth then
            header.settings.maxWidth
        else if newWidth < header.settings.minWidth then
            header.settings.minWidth
        else
            newWidth

onDrag : (Mouse.Position -> Msg) -> Attribute Msg
onDrag msg =
    on "mousedown" (Decode.map msg Mouse.position)

-- updateContextMenu : FancyTable -> ContextMenu
-- updateContextMenu (FancyTable model) = ContextMenu.init

toItemGroups : Model -> Context ->  List (List (ContextMenu.Item, Msg))
toItemGroups model context =
    case context of
        HeaderContext ->
           [ List.map getHeaderContextItem model.headers ]

getHeaderContextItem : Header -> (ContextMenu.Item, Msg)
getHeaderContextItem header =
    ((ContextMenu.item header.name) |> if header.settings.isVisible then ContextMenu.icon FontAwesome.check Color.blue else ContextMenu.icon FontAwesome.eye Color.white
    , ToggleHeader header.index
    )

{-| Returns default fancy table which is an empty table
-}
init : FancyTable
init =
    let
        (contextMenu, msg) = ContextMenu.init
        contextMenuModel =
            { menu = contextMenu
            , config = winChrome
            }
        model = 
            { headers = []
            , rows = []
            , resizeHeader = Nothing
            , reorderHeader = Nothing
            , settings = defaultSettings
            , contextMenu = contextMenuModel
            }
    in
        FancyTable model

{-| Displays the table
-}
view : FancyTable -> Html Msg
view (FancyTable model) =
    let
        thead = getTHead model
        tbody = getTbody model
        tableWidth = List.foldr (\h -> (+) h.settings.width) 0 model.headers
    in
        Html.div [ class "fancy-table-wrapper" ]
                 [ Html.table [ class "fancy-table-table", style [ ("width", toString tableWidth ++ "px")] ] 
                              [ thead
                              , tbody
                              ]
                 , Html.div [ class "fancy-table-context-menu" ] [ (ContextMenu.view model.contextMenu.config ContextMenuMsg (toItemGroups model) model.contextMenu.menu) ]
                 ]

getTHead : Model -> Html Msg
getTHead model =
    Html.thead [ class "fancy-table-thead", ContextMenu.open ContextMenuMsg HeaderContext ]
            <| List.map (\h -> getTableHeader model h) (List.filter (\h -> h.settings.isVisible == True ) model.headers)

getTbody : Model -> Html Msg
getTbody model =
    Html.tbody [ class "fancy-table-tbody" ] <|
        List.map (\r -> getTableRow model r) model.rows

getTableRow : Model -> TableRow -> Html Msg
getTableRow model rows =
    Html.tr [ class "fancy-table-tr" ] <|
        List.map (\td -> getTableTd model td) (List.filter (\td -> td.isVisible == True) rows)

getTableTd : Model -> TableData -> Html Msg
getTableTd model td = 
    let
        moveStyle =
            case model.reorderHeader of
                Nothing -> 
                    []
                Just { index, startX, currentX } ->
                    if index == td.index then
                        [ ("transform","translateX(" ++ toString (currentX - startX) ++ "px)")
                        , ("box-shadow", "0 3px 6px #000000")
                        , ("willChange", "transform")
                        ]
                    else
                        []
    in
        Html.td [ class "fancy-table-td dragable"
                , style <| moveStyle
                ] [ td.content ]
                
getTableHeader : Model -> Header -> Html Msg
getTableHeader model header =
    let
        moveStyle =
            case model.reorderHeader of
                Nothing -> 
                    []
                Just { index, startX, currentX } ->
                    if index == header.index then
                        [ ("transform","translateX(" ++ toString (currentX - startX) ++ "px)")
                        , ("box-shadow", "0 3px 6px #000000")
                        , ("willChange", "transform")
                        ]
                    else
                        []
        fancyTableHeaderStyle =
            [ ("max-width", (toString header.settings.maxWidth ++ "px") )
            , ("min-width", (toString header.settings.minWidth ++ "px") )
            , ("width", (toString header.settings.width ++ "px") )
            ]
        
        headerContentDiv =
            case model.settings.reorderColumns of
                True ->
                    Html.div [ class "fancy-table-header-content reorder", onDrag <| ReorderDragStart header.index ] [ header.content ]
                False ->
                    Html.div [ class "fancy-table-header-content no-reorder" ] [ header.content ]
        headerGripDiv =
            case model.settings.resizeColumns of
                True ->
                    Html.div [ class "fancy-table-header-grip resize", onDrag <| ResizeDragStart header.index ] []
                False ->
                    Html.div [ class "fancy-table-header-grip no-resize" ] []
    in
        Html.th [ class "fancy-table-th dragable"
                , style <| moveStyle ++ fancyTableHeaderStyle
                ]
                [ headerContentDiv
                , headerGripDiv
                ]