module Main exposing (..)

import Array
import Browser
import Browser.Dom as Dom
import Browser.Events as E
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as EEvents
import Element.Font as Font
import Element.Input as ElementI
import EvolutionPlans exposing (..)
import EvolutionPlansDecoder as Decode
import Html exposing (Html)
import Html.Attributes as HtmlA
import Http
import List
import List.Extra as List
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Svg.Events as SvgEvents
import Task


type alias Color =
    { dark : String
    , medium : String
    , light : String
    , lightPrimary : String
    , darkPrimary : String
    , warning : String
    , alert : String
    , white : Element.Color
    , navbar :
        { topBackground : Element.Color
        , topHover : Element.Color
        , bottomBackground : Element.Color
        , bottomHover : Element.Color
        , background : Element.Color
        , selectedTimePoint : Element.Color
        }
    }


colorScheme : Color
colorScheme =
    { dark = "#43464C"
    , medium = "#9197A6"
    , light = "#D3DCF2"
    , lightPrimary = "#7690CF"
    , darkPrimary = "#48577D"
    , warning = "#FFCF59"
    , alert = "#F56F67"
    , white = Element.rgb255 255 255 255
    , navbar =
        { topBackground = Element.rgb255 165 165 165
        , topHover = Element.rgb255 179 179 179
        , bottomBackground = Element.rgb255 196 196 196
        , bottomHover = Element.rgb255 208 208 208
        , background = Element.rgb255 246 246 246
        , selectedTimePoint = Element.rgb255 98 118 189
        }
    }


calcWidth : ComputedTree -> Float
calcWidth (ComputedNode tree) =
    tree.computedDimentions.treeWidth


calcHeight : ComputedTree -> Float
calcHeight tree =
    let
        treeDepth =
            toFloat <| depth tree

        nodeTotalHeight =
            treeDepth * nodeHeight

        spacingTotalHeight =
            (treeDepth - 1) * spacingY
    in
    nodeTotalHeight + spacingTotalHeight


depth : ComputedTree -> Int
depth (ComputedNode node) =
    case node.children |> List.map depth |> List.maximum of
        Nothing ->
            1

        Just n ->
            n + 1



-- extra paddyo


nodeHeight : Float
nodeHeight =
    30


spacingX : Float
spacingX =
    30


spacingY : Float
spacingY =
    80


type Msg
    = NoOp
    | NewWindowSize Int Int
    | GotViewport Dom.Viewport
    | GotMergeResult (Result Http.Error MergeResult)
    | NodeHoverEntry String
    | NodeHoverExit
    | NewEvolutionPlanIndex Int
    | NewFeatureModelIndex Int


type alias Flags =
    {}


type Model
    = UnInitialized SomeFields
    | Initialized Fields


type alias SomeFields =
    { mDimentions : Maybe ( Int, Int )
    , mMergeResult : Maybe MergeResult
    }


type alias Fields =
    { device : Element.Device
    , width : Int
    , height : Int
    , mergeResult : MergeResult
    , hoverData : Maybe String
    , chosenEvolutionPlanIndex : Int
    , chosenFeatureModelIndex : Int
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        getViewport =
            Task.perform GotViewport Dom.getViewport

        getMergeResult =
            Http.get
                { url = "./elm-input.json"
                , expect =
                    Http.expectJson
                        GotMergeResult
                        Decode.mergeResult
                }
    in
    ( UnInitialized
        { mMergeResult = Nothing
        , mDimentions = Nothing
        }
    , Cmd.batch [ getViewport, getMergeResult ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewWindowSize w h ->
            ( updateWindowSize w h model, Cmd.none )

        GotViewport viewport ->
            ( updateViewport viewport model, Cmd.none )

        GotMergeResult mergeResultErr ->
            case mergeResultErr of
                Err _ ->
                    ( model, Cmd.none )

                Ok t ->
                    ( updateMergeResult t model, Cmd.none )

        NodeHoverEntry hoverData ->
            ( updateIfInitialized (\f -> { f | hoverData = Just hoverData }) model, Cmd.none )

        NodeHoverExit ->
            ( updateIfInitialized (\f -> { f | hoverData = Nothing }) model, Cmd.none )

        NewEvolutionPlanIndex epIndex ->
            ( updateIfInitialized
                (\f ->
                    { f
                        | chosenEvolutionPlanIndex = epIndex

                        -- , chosenFeatureModelIndex = 0
                    }
                )
                model
            , Cmd.none
            )

        NewFeatureModelIndex fmIndex ->
            ( updateIfInitialized (\f -> { f | chosenFeatureModelIndex = fmIndex }) model, Cmd.none )


updateIfInitialized : (Fields -> Fields) -> Model -> Model
updateIfInitialized fieldTransformer model =
    case model of
        Initialized fields ->
            Initialized (fieldTransformer fields)

        _ ->
            model


updateWindowSize : Int -> Int -> Model -> Model
updateWindowSize w h model =
    case model of
        UnInitialized someFields ->
            convertIfInitialized
                { someFields | mDimentions = Just ( w, h ) }

        Initialized fields ->
            Initialized
                { fields
                    | width = w
                    , height = h
                    , device =
                        Element.classifyDevice
                            { width = w, height = h }
                }


convertIfInitialized : SomeFields -> Model
convertIfInitialized someFields =
    case ( someFields.mDimentions, someFields.mMergeResult ) of
        ( Just ( w, h ), Just mergeResult ) ->
            Initialized
                { width = w
                , height = h
                , device =
                    Element.classifyDevice
                        { width = w, height = h }
                , mergeResult = mergeResult
                , hoverData = Nothing
                , chosenEvolutionPlanIndex = 0
                , chosenFeatureModelIndex = 0
                }

        _ ->
            UnInitialized someFields


updateViewport : Dom.Viewport -> Model -> Model
updateViewport viewport model =
    let
        w =
            round viewport.viewport.width

        h =
            round viewport.viewport.height
    in
    case model of
        UnInitialized someFields ->
            convertIfInitialized
                { someFields | mDimentions = Just ( w, h ) }

        Initialized fields ->
            model


updateMergeResult : MergeResult -> Model -> Model
updateMergeResult mergeResult model =
    case model of
        UnInitialized someFields ->
            convertIfInitialized { someFields | mMergeResult = Just mergeResult }

        Initialized fields ->
            model


subscriptions : Model -> Sub Msg
subscriptions model =
    E.onResize NewWindowSize


tempToTree : Feature -> Tree
tempToTree (Feature fields) =
    Node
        { value = fields.name
        , metaData = Just fields.featureType
        , children =
            fields.groups
                |> List.concatMap (\(Group gFields) -> gFields.features)
                |> List.map tempToTree
        }


noOutlineButton : List (Element.Attribute msg) -> { onPress : Maybe msg, label : Element msg } -> Element msg
noOutlineButton attributes fields =
    ElementI.button (Element.htmlAttribute (HtmlA.style "box-shadow" "none") :: attributes) fields


view : Model -> Html Msg
view model =
    let
        content =
            case model of
                UnInitialized someFields ->
                    Element.text <| Debug.toString someFields

                Initialized fields ->
                    case Array.get fields.chosenEvolutionPlanIndex fields.mergeResult.evolutionPlans of
                        Nothing ->
                            Element.text "Evolution Plan index out of bounds"

                        Just currentEP ->
                            case Array.get fields.chosenFeatureModelIndex currentEP.timePoints of
                                Nothing ->
                                    Element.text "Feature Model index out of bounds"

                                Just currentFM ->
                                    viewInitialized fields currentEP currentFM
    in
    Element.layout [ Background.color colorScheme.navbar.background ] content


viewInitialized : Fields -> EvolutionPlan -> TimePoint -> Element Msg
viewInitialized fields currentEP currentFM =
    Element.column
        [ Element.height Element.fill
        , Element.width Element.fill
        ]
        [ viewNavbar fields currentEP
        , viewTree fields currentEP currentFM
        ]


viewNavbar : Fields -> EvolutionPlan -> Element Msg
viewNavbar fields currentEP =
    Element.column
        [ Element.width Element.fill ]
        [ viewEvolutionPlanBar fields
        , viewNavbarSpacer fields
        , viewFeatureModelBar fields currentEP
        ]


viewEvolutionPlanBar : Fields -> Element Msg
viewEvolutionPlanBar fields =
    Element.row
        [ Element.width Element.fill
        ]
        (fields.mergeResult.evolutionPlans
            |> Array.indexedMap (viewEvolutionPlanButton fields)
            |> Array.toList
        )


viewEvolutionPlanButton : Fields -> Int -> EvolutionPlan -> Element Msg
viewEvolutionPlanButton fields epIndex ep =
    noOutlineButton
        ([ EEvents.onClick (NewEvolutionPlanIndex epIndex)
         , Element.width Element.fill
         , Element.padding 3
         ]
            ++ (if fields.chosenEvolutionPlanIndex == epIndex then
                    [ Background.color colorScheme.navbar.bottomBackground ]

                else
                    [ Background.color colorScheme.navbar.topBackground
                    , Element.mouseOver [ Background.color colorScheme.navbar.topHover ]
                    ]
               )
        )
        { onPress = Just (NewEvolutionPlanIndex epIndex)
        , label =
            Element.el
                [ Element.centerX
                , Font.color colorScheme.white
                ]
            <|
                Element.text ep.name
        }


viewNavbarSpacer : Fields -> Element Msg
viewNavbarSpacer fields =
    Element.el
        [ Element.height <| Element.px 16
        , Element.width Element.fill
        , Background.color colorScheme.navbar.bottomBackground
        ]
        Element.none


viewFeatureModelBar : Fields -> EvolutionPlan -> Element Msg
viewFeatureModelBar fields currentEP =
    Element.row [ Element.width Element.fill ]
        (currentEP.timePoints
            |> Array.indexedMap (viewFeatureModelButton fields)
            |> Array.toList
        )


viewFeatureModelButton : Fields -> Int -> TimePoint -> Element Msg
viewFeatureModelButton fields fmIndex fm =
    noOutlineButton
        ([ EEvents.onClick (NewFeatureModelIndex fmIndex)
         , Element.width Element.fill
         , Element.padding 3
         ]
            ++ (if fields.chosenFeatureModelIndex == fmIndex then
                    [ Background.color colorScheme.navbar.background
                    ]

                else
                    [ Background.color colorScheme.navbar.bottomBackground
                    , Element.mouseOver [ Background.color colorScheme.navbar.bottomHover ]
                    , Border.widthEach { bottom = 0, left = 1, right = 1, top = 0 }
                    , Border.solid
                    , Border.color colorScheme.navbar.bottomHover
                    ]
               )
        )
        { onPress = Just (NewFeatureModelIndex fmIndex)
        , label =
            Element.el
                [ Element.centerX
                , Font.color <|
                    if fields.chosenFeatureModelIndex == fmIndex then
                        colorScheme.navbar.selectedTimePoint

                    else
                        colorScheme.white
                ]
            <|
                Element.text (String.fromInt fm.time)
        }


viewTree : Fields -> EvolutionPlan -> TimePoint -> Element Msg
viewTree fields currentEP currentFM =
    let
        computedTree =
            computeTree <| tempToTree <| currentFM.featureModel.rootFeature

        width =
            calcWidth computedTree

        height =
            calcHeight computedTree
    in
    Element.el
        [ Element.clip
        , Element.scrollbars
        , Element.width Element.fill
        , Element.height Element.fill
        ]
    <|
        Element.el [ Element.padding 16 ] <|
            Element.html
                (Svg.svg
                    [ SvgA.width <| String.fromFloat width
                    , SvgA.height <| String.fromFloat height
                    , SvgA.viewBox <|
                        "0 0 "
                            ++ String.fromFloat width
                            ++ " "
                            ++ String.fromFloat height
                    ]
                    (computedTree |> drawTree 0 0)
                )


computeTree : Tree -> ComputedTree
computeTree (Node tree) =
    let
        computedChildren =
            List.map computeTree tree.children

        childrenTotalWidth =
            computedChildren
                |> List.map (\(ComputedNode t) -> t.computedDimentions.treeWidth)
                |> List.intersperse spacingX
                |> List.sum

        approxNodeWidth =
            toFloat (String.length tree.value) * 15
    in
    ComputedNode
        { value = tree.value
        , metaData = tree.metaData
        , children = computedChildren
        , computedDimentions =
            { approxNodeWidth = approxNodeWidth
            , treeWidth = Basics.max approxNodeWidth childrenTotalWidth
            }
        }


drawTree : Float -> Float -> ComputedTree -> List (Svg Msg)
drawTree xStart yStart (ComputedNode tree) =
    let
        childYLevel : Float
        childYLevel =
            yStart + nodeHeight + spacingY

        nodeXLevel : Float
        nodeXLevel =
            xStart + (tree.computedDimentions.treeWidth / 2)

        drawChildren : Float -> List ComputedTree -> List (Svg Msg)
        drawChildren childX children =
            case children of
                [] ->
                    []

                (ComputedNode child) :: rest ->
                    [ drawLine
                        nodeXLevel
                        (yStart + nodeHeight)
                        (childX + (child.computedDimentions.treeWidth / 2))
                        childYLevel
                    ]
                        ++ drawTree childX childYLevel (ComputedNode child)
                        ++ drawChildren
                            (childX
                                + child.computedDimentions.treeWidth
                                + spacingX
                            )
                            rest
    in
    drawNode nodeXLevel yStart (ComputedNode tree)
        :: drawChildren xStart tree.children


drawNode : Float -> Float -> ComputedTree -> Svg Msg
drawNode x y (ComputedNode node) =
    Svg.g
        [ SvgA.transform <|
            "translate("
                ++ String.fromFloat x
                ++ ", "
                ++ String.fromFloat y
                ++ ")"
        , SvgEvents.onMouseOver
            (NodeHoverEntry <| Maybe.withDefault "-" node.metaData)
        , SvgEvents.onMouseOut NodeHoverExit
        , SvgA.cursor "pointer"
        ]
        [ Svg.rect
            [ SvgA.width <|
                String.fromFloat
                    node.computedDimentions.approxNodeWidth
            , SvgA.height <| String.fromFloat nodeHeight
            , SvgA.x <|
                String.fromFloat <|
                    -(node.computedDimentions.approxNodeWidth / 2)
            , SvgA.fill colorScheme.darkPrimary
            , SvgA.strokeWidth "2"
            , SvgA.stroke colorScheme.dark
            ]
            []
        , Svg.text_
            [ SvgA.x "0"
            , SvgA.y <| String.fromFloat <| nodeHeight / 2
            , SvgA.dominantBaseline "middle"
            , SvgA.textAnchor "middle"
            , SvgA.fill colorScheme.light
            ]
            [ Svg.text node.value ]
        ]


drawLine : Float -> Float -> Float -> Float -> Svg msg
drawLine x1 y1 x2 y2 =
    Svg.line
        [ SvgA.x1 <| String.fromFloat x1
        , SvgA.y1 <| String.fromFloat y1
        , SvgA.x2 <| String.fromFloat x2
        , SvgA.y2 <| String.fromFloat y2
        , SvgA.strokeWidth "3"
        , SvgA.stroke colorScheme.dark
        ]
        []
