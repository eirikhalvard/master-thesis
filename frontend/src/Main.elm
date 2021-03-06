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
        , rightMenuBackground : Element.Color
        , rightMenuTextColor : Element.Color
        , rightMenuSelectedColor : Element.Color
        , rightMenuHoverColor : Element.Color
        , accent : Element.Color
        , error : Element.Color
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
        , selectedTimePoint = Element.rgb255 255 184 123
        , rightMenuBackground = Element.rgb255 72 87 125
        , rightMenuTextColor = Element.rgb255 211 220 242
        , rightMenuSelectedColor = Element.rgb255 211 220 242
        , rightMenuHoverColor = Element.rgb255 211 220 242
        , accent = Element.rgb255 255 184 123
        , error = Element.rgb255 138 0 0
        }
    }


calcFeatureWidth : Feature ComputedDimentions ComputedDimentions -> Float
calcFeatureWidth (Feature fields) =
    fields.extra.treeWidth


calcFeatureHeight : Feature ComputedDimentions ComputedDimentions -> Float
calcFeatureHeight (Feature fields) =
    case fields.groups |> List.map calcGroupHeight |> List.maximum of
        Nothing ->
            featureHeight

        Just maxGroupHeight ->
            featureHeight + spacingY + maxGroupHeight


calcGroupHeight : Group ComputedDimentions ComputedDimentions -> Float
calcGroupHeight (Group fields) =
    case fields.features |> List.map calcFeatureHeight |> List.maximum of
        Nothing ->
            groupHeight

        Just maxFeatureHeight ->
            groupHeight + spacingY + maxFeatureHeight



-- extra paddyo


featureHeight : Float
featureHeight =
    30


groupHeight : Float
groupHeight =
    25


spacingX : Float
spacingX =
    30


spacingY : Float
spacingY =
    50


type Msg
    = NoOp
    | NewWindowSize Int Int
    | GotViewport Dom.Viewport
    | GotDataExamples (Result Http.Error (DataExamples () ()))
    | NodeHoverEntry NodeInformation
    | NodeHoverExit
    | NewExampleIndex Int
    | NewEvolutionPlanIndex Int
    | NewFeatureModelIndex Int


type alias Flags =
    {}


type Model
    = UnInitialized SomeFields
    | Initialized Fields


type alias SomeFields =
    { mDimentions : Maybe ( Int, Int )
    , mDataExamples : Maybe (DataExamples () ())
    }


type alias Fields =
    { device : Element.Device
    , width : Int
    , height : Int
    , dataExamples : DataExamples () ()
    , hoverData : Maybe NodeInformation
    , chosenExampleIndex : Int
    , chosenEvolutionPlanIndex : Int
    , chosenFeatureModelIndex : Int
    }


type alias NodeInformation =
    { node : String
    , id : String
    , nodeType : String
    , name : Maybe String
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

        getDataExamples =
            Http.get
                { url = "./data/elm-input.json"
                , expect =
                    Http.expectJson
                        GotDataExamples
                        Decode.dataExamples
                }
    in
    ( UnInitialized
        { mDataExamples = Nothing
        , mDimentions = Nothing
        }
    , Cmd.batch [ getViewport, getDataExamples ]
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

        GotDataExamples dataExamplesErr ->
            case dataExamplesErr of
                Err _ ->
                    ( model, Cmd.none )

                Ok t ->
                    ( updateDataExamples t model, Cmd.none )

        NodeHoverEntry hoverData ->
            ( updateIfInitialized (\f -> { f | hoverData = Just hoverData }) model, Cmd.none )

        NodeHoverExit ->
            ( updateIfInitialized (\f -> { f | hoverData = Nothing }) model, Cmd.none )

        NewExampleIndex exIndex ->
            ( updateIfInitialized
                (\f ->
                    { f
                        | chosenExampleIndex = exIndex
                        , chosenEvolutionPlanIndex = 0
                        , chosenFeatureModelIndex = 0
                    }
                )
                model
            , Cmd.none
            )

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
            ( updateIfInitialized
                (\f ->
                    { f | chosenFeatureModelIndex = fmIndex }
                )
                model
            , Cmd.none
            )


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
    case ( someFields.mDimentions, someFields.mDataExamples ) of
        ( Just ( w, h ), Just dataExamples ) ->
            Initialized
                { width = w
                , height = h
                , device =
                    Element.classifyDevice
                        { width = w, height = h }
                , dataExamples = dataExamples
                , hoverData = Nothing
                , chosenExampleIndex = 0
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


updateDataExamples : DataExamples () () -> Model -> Model
updateDataExamples dataExamples model =
    case model of
        UnInitialized someFields ->
            convertIfInitialized { someFields | mDataExamples = Just dataExamples }

        Initialized fields ->
            model


subscriptions : Model -> Sub Msg
subscriptions model =
    E.onResize NewWindowSize


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
                    case Array.get fields.chosenExampleIndex fields.dataExamples.examples of
                        Nothing ->
                            Element.text "Example index out of bounds"

                        Just mergeExample ->
                            case Array.get fields.chosenEvolutionPlanIndex mergeExample.evolutionPlans of
                                Nothing ->
                                    Element.text "Evolution Plan index out of bounds"

                                Just currentEP ->
                                    case currentEP.mergeData of
                                        Err errStr ->
                                            viewInitialized fields currentEP (Err errStr)

                                        Ok timePoints ->
                                            case Array.get fields.chosenFeatureModelIndex timePoints of
                                                Nothing ->
                                                    Element.text "Feature Model index out of bounds"

                                                Just currentFM ->
                                                    viewInitialized fields currentEP (Ok currentFM)
    in
    Element.layout
        [ Background.color colorScheme.navbar.background
        , Element.height Element.fill
        , Element.width Element.fill
        ]
        content


viewInitialized : Fields -> EvolutionPlan () () -> Result String (TimePoint () ()) -> Element Msg
viewInitialized fields currentEP currentFMOrErr =
    let
        fmOrErr =
            case currentFMOrErr of
                Err errStr ->
                    viewError fields errStr

                Ok currentFM ->
                    viewTree fields currentEP currentFM
    in
    Element.row
        [ Element.height Element.fill
        , Element.width Element.fill
        ]
        [ Element.column
            [ Element.height Element.fill
            , Element.width <| Element.fillPortion 4
            , Element.clip
            , Element.scrollbars
            ]
            [ viewNavbar fields currentEP
            , Element.el
                [ Element.clip
                , Element.scrollbars
                , Element.width Element.fill
                , Element.height Element.fill
                ]
                fmOrErr
            ]
        , viewRightMenu fields
        ]


viewError : Fields -> String -> Element Msg
viewError fields errStr =
    Element.el
        [ Element.padding 30
        , Element.centerY
        ]
    <|
        Element.el
            [ Element.centerX
            , Element.centerY
            ]
            (Element.text errStr)


viewRightMenu : Fields -> Element Msg
viewRightMenu fields =
    Element.el
        [ Element.height Element.fill
        , Background.color colorScheme.navbar.rightMenuBackground
        , Font.color <| colorScheme.navbar.rightMenuTextColor
        , Element.width (Element.px 270)
        , Element.padding 10
        , Element.clipY
        , Element.scrollbarY
        ]
    <|
        Element.column
            [ Element.spaceEvenly
            , Element.clipY
            , Element.height Element.fill
            ]
            [ viewTopBox fields
            , viewExamples fields
            , viewInformation fields
            ]


viewTopBox : Fields -> Element Msg
viewTopBox fields =
    let
        githubLink =
            Element.newTabLink []
                { url = "https://github.com/eirikhalvard/master-thesis/"
                , label =
                    Element.image
                        [ Element.width <| Element.px 24
                        , Element.height <| Element.px 24
                        ]
                        { src = "./data/githublogo.png", description = "Github link" }
                }
    in
    Element.column
        [ Element.height <| Element.px 60
        , Element.width Element.fill
        ]
        [ Element.el [ Element.centerX, Font.size 24 ] <| Element.text "Evolution Plan Merger"
        , Element.row
            [ Element.centerX
            , Font.size 16
            , Element.centerY
            , Element.spaceEvenly
            , Element.width Element.fill
            , Element.paddingXY 16 0
            ]
            [ Element.text "By Eirik Sæther"
            , githubLink
            ]
        ]


viewExamples : Fields -> Element Msg
viewExamples fields =
    let
        exampleList =
            fields.dataExamples.examples
                |> Array.indexedMap
                    (\i example ->
                        Element.el
                            [ EEvents.onClick (NewExampleIndex i)
                            , Element.pointer
                            , Element.centerX
                            , Font.size 16
                            , if i == fields.chosenExampleIndex then
                                Font.color colorScheme.navbar.accent

                              else
                                Font.color <| colorScheme.navbar.rightMenuTextColor
                            ]
                            (Element.paragraph [ Font.center ] [ Element.text example.name ])
                    )
                |> Array.toList
    in
    Element.column
        [ Element.height Element.fill
        , Element.width Element.fill
        , Element.spacing 10
        , Element.paddingXY 0 40
        ]
        [ Element.el [ Element.centerX, Font.size 22 ] (Element.text "Examples")
        , Element.column [ Element.width Element.fill, Element.spacing 4 ] exampleList
        ]


viewInformation : Fields -> Element Msg
viewInformation fields =
    let
        nodeInfo node id nodeType name =
            [ infoLine "Node" node
            , infoLine "Id " id
            , infoLine "Type" nodeType
            , infoLine "Name" name
            ]

        infoLine infoType infoContent =
            Element.paragraph [ Font.center, Element.width Element.fill ]
                [ Element.el [ Font.semiBold ] <| Element.text infoType
                , Element.text ": "
                , Element.text infoContent
                ]
    in
    Element.column
        [ Element.height Element.fill
        , Element.width Element.fill
        , Element.spacing 10
        , Element.paddingXY 0 40
        ]
        [ Element.el
            [ Element.centerX, Font.size 22 ]
          <|
            Element.text "Node Information"
        , Element.column
            [ Font.size 16, Element.width Element.fill, Element.spacing 4 ]
          <|
            case fields.hoverData of
                Nothing ->
                    nodeInfo "-" "-" "-" "-"

                Just hoverData ->
                    nodeInfo hoverData.node
                        hoverData.id
                        hoverData.nodeType
                        (Maybe.withDefault "-" hoverData.name)
        ]


viewNavbar : Fields -> EvolutionPlan () () -> Element Msg
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
        (case Array.get fields.chosenExampleIndex fields.dataExamples.examples of
            Nothing ->
                [ Element.none ]

            Just mergeExample ->
                mergeExample.evolutionPlans
                    |> Array.indexedMap (viewEvolutionPlanButton fields)
                    |> Array.toList
        )


viewEvolutionPlanButton : Fields -> Int -> EvolutionPlan () () -> Element Msg
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


viewFeatureModelBar : Fields -> EvolutionPlan () () -> Element Msg
viewFeatureModelBar fields currentEP =
    Element.row [ Element.width Element.fill ]
        (case currentEP.mergeData of
            Err _ ->
                [ Element.el
                    [ Background.color colorScheme.navbar.bottomBackground
                    , Border.widthEach { bottom = 0, left = 1, right = 1, top = 0 }
                    , Border.solid
                    , Element.width Element.fill
                    , Element.padding 3
                    ]
                  <|
                    Element.el
                        [ Font.color colorScheme.navbar.error
                        , Element.centerX
                        ]
                    <|
                        Element.text "Error in Evolution Plan"
                ]

            Ok timePoints ->
                timePoints
                    |> Array.indexedMap (viewFeatureModelButton fields)
                    |> Array.toList
        )


viewFeatureModelButton : Fields -> Int -> TimePoint () () -> Element Msg
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


viewTree : Fields -> EvolutionPlan () () -> TimePoint () () -> Element Msg
viewTree fields currentEP currentFM =
    let
        strokeOffset =
            10

        computedTree =
            computeFeature <| currentFM.featureModel.rootFeature

        width =
            calcFeatureWidth computedTree + strokeOffset

        height =
            calcFeatureHeight computedTree + strokeOffset
    in
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
                (computedTree |> drawFeature (strokeOffset / 2) (strokeOffset / 2))
            )


computeFeature : Feature () () -> Feature ComputedDimentions ComputedDimentions
computeFeature (Feature fields) =
    let
        computedGroups =
            List.map computeGroup fields.groups

        childrenTotalWidth =
            computedGroups
                |> List.map (\(Group groupFields) -> groupFields.extra.treeWidth)
                |> List.intersperse spacingX
                |> List.sum

        approxFeatureWidth =
            toFloat (String.length fields.name) * 15

        extra =
            { approxNodeWidth = approxFeatureWidth
            , treeWidth = Basics.max approxFeatureWidth childrenTotalWidth
            }
    in
    Feature
        { extra = extra
        , id = fields.id
        , featureType = fields.featureType
        , name = fields.name
        , groups = computedGroups
        }


computeGroup : Group () () -> Group ComputedDimentions ComputedDimentions
computeGroup (Group fields) =
    let
        computedFeatures =
            List.map computeFeature fields.features

        childrenTotalWidth =
            computedFeatures
                |> List.map (\(Feature featureFields) -> featureFields.extra.treeWidth)
                |> List.intersperse spacingX
                |> List.sum

        approxGroupWidth =
            20.0

        --TODO: find right value
        extra =
            { approxNodeWidth = approxGroupWidth
            , treeWidth = Basics.max approxGroupWidth childrenTotalWidth
            }
    in
    Group
        { extra = extra
        , id = fields.id
        , groupType = fields.groupType
        , features = computedFeatures
        }


drawFeature : Float -> Float -> Feature ComputedDimentions ComputedDimentions -> List (Svg Msg)
drawFeature xStart yStart (Feature fields) =
    let
        childYLevel : Float
        childYLevel =
            yStart + featureHeight + spacingY

        nodeXLevel : Float
        nodeXLevel =
            xStart + (fields.extra.treeWidth / 2)

        drawChildren : Float -> List (Group ComputedDimentions ComputedDimentions) -> List (Svg Msg)
        drawChildren childX children =
            case children of
                [] ->
                    []

                (Group groupFields) :: rest ->
                    [ drawLine
                        nodeXLevel
                        (yStart + featureHeight)
                        (childX + groupFields.extra.treeWidth / 2)
                        childYLevel
                    ]
                        ++ drawGroup childX childYLevel (Group groupFields)
                        ++ drawChildren
                            (childX
                                + groupFields.extra.treeWidth
                                + spacingX
                            )
                            rest
    in
    drawFeatureNode nodeXLevel yStart (Feature fields)
        :: drawChildren xStart fields.groups


drawGroup : Float -> Float -> Group ComputedDimentions ComputedDimentions -> List (Svg Msg)
drawGroup xStart yStart (Group fields) =
    let
        childYLevel : Float
        childYLevel =
            yStart + groupHeight + spacingY

        nodeXLevel : Float
        nodeXLevel =
            xStart + (fields.extra.treeWidth / 2)

        drawChildren : List (Feature ComputedDimentions ComputedDimentions) -> List (Svg Msg)
        drawChildren children =
            let
                childWidths =
                    children
                        |> List.map (\(Feature featureFields) -> featureFields.extra.treeWidth)

                subTreePositions =
                    childWidths
                        |> List.scanl (\treeWidth accWidth -> treeWidth + spacingX + accWidth) xStart
                        |> List.init
                        |> Maybe.withDefault []

                branchPositions =
                    List.map2 (\pos width -> pos + width / 2) subTreePositions childWidths

                branches =
                    drawBranches nodeXLevel branchPositions (yStart + groupHeight) spacingY

                subTrees =
                    List.concat <| List.map2 (\childX feature -> drawFeature childX childYLevel feature) subTreePositions children
            in
            branches ++ subTrees
    in
    drawGroupNode nodeXLevel yStart (Group fields)
        :: drawChildren fields.features


drawBranches : Float -> List Float -> Float -> Float -> List (Svg Msg)
drawBranches xStart xStops yStart yStep =
    case xStops of
        [] ->
            []

        [ xStop ] ->
            [ drawLine xStart yStart xStop (yStart + yStep) ]

        _ ->
            let
                rootLine =
                    drawLine xStart yStart xStart (yStart + yStep / 2)

                horizontalLine =
                    case ( List.head xStops, List.last xStops ) of
                        ( Just firstX, Just lastX ) ->
                            [ drawLine firstX (yStart + yStep / 2) lastX (yStart + yStep / 2) ]

                        _ ->
                            []

                childrenLines =
                    List.map (\xStop -> drawLine xStop (yStart + yStep / 2) xStop (yStart + yStep)) xStops
            in
            [ rootLine ] ++ horizontalLine ++ childrenLines


drawGroupNode : Float -> Float -> Group ComputedDimentions ComputedDimentions -> Svg Msg
drawGroupNode x y (Group fields) =
    let
        groupSymbol =
            case fields.groupType of
                "And" ->
                    "⋀"

                "Or" ->
                    "⋁"

                "Alternative" ->
                    "⊕"

                _ ->
                    "_"
    in
    Svg.g
        [ SvgA.transform <|
            "translate("
                ++ String.fromFloat x
                ++ ", "
                ++ String.fromFloat y
                ++ ")"
        , SvgEvents.onMouseOver
            (NodeHoverEntry <|
                NodeInformation "Group" fields.id fields.groupType Nothing
            )
        , SvgEvents.onMouseOut NodeHoverExit
        , SvgA.cursor "pointer"
        ]
        [ Svg.circle
            [ SvgA.width <|
                String.fromFloat
                    fields.extra.approxNodeWidth
            , SvgA.height <| String.fromFloat featureHeight
            , SvgA.r <| String.fromFloat (groupHeight / 2)
            , SvgA.cy <| String.fromFloat (groupHeight / 2)

            -- , SvgA.cx <|
            --     String.fromFloat <|
            --         -(fields.extra.approxNodeWidth / 2)
            , SvgA.fill colorScheme.darkPrimary
            , SvgA.strokeWidth "2"
            , SvgA.stroke colorScheme.dark
            ]
            []
        , Svg.text_
            [ SvgA.x "0"
            , SvgA.y <| String.fromFloat <| groupHeight / 2
            , SvgA.dominantBaseline "middle"
            , SvgA.textAnchor "middle"
            , SvgA.fill colorScheme.light
            ]
            [ Svg.text groupSymbol ]
        ]


drawFeatureNode : Float -> Float -> Feature ComputedDimentions ComputedDimentions -> Svg Msg
drawFeatureNode x y (Feature fields) =
    let
        textColor =
            if fields.featureType == "Mandatory" then
                colorScheme.light

            else
                colorScheme.darkPrimary

        backgroundColor =
            if fields.featureType == "Mandatory" then
                colorScheme.darkPrimary

            else
                colorScheme.light
    in
    Svg.g
        [ SvgA.transform <|
            "translate("
                ++ String.fromFloat x
                ++ ", "
                ++ String.fromFloat y
                ++ ")"
        , SvgEvents.onMouseOver
            (NodeHoverEntry <|
                NodeInformation
                    "Feature"
                    fields.id
                    fields.featureType
                    (Just fields.name)
            )
        , SvgEvents.onMouseOut NodeHoverExit
        , SvgA.cursor "pointer"
        ]
        [ Svg.rect
            [ SvgA.width <|
                String.fromFloat
                    fields.extra.approxNodeWidth
            , SvgA.height <| String.fromFloat featureHeight
            , SvgA.x <|
                String.fromFloat <|
                    -(fields.extra.approxNodeWidth / 2)
            , SvgA.fill backgroundColor
            , SvgA.strokeWidth "2"
            , SvgA.stroke colorScheme.dark
            ]
            []
        , Svg.text_
            [ SvgA.x "0"
            , SvgA.y <| String.fromFloat <| featureHeight / 1.9 -- dividing by half plus a little offset
            , SvgA.dominantBaseline "middle"
            , SvgA.textAnchor "middle"
            , SvgA.fill textColor
            ]
            [ Svg.text fields.name ]
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
        , SvgA.strokeLinecap "square"
        ]
        []
