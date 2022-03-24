module Test.FormParserTests exposing (all)

import Expect exposing (Expectation)
import Html exposing (Html, a, div, footer, h1, header, img, li, section, span, ul)
import Html.Attributes as Attr exposing (href)
import Test exposing (..)
import Test.Html.FormParser as FormParser
import Test.Html.Query as Query exposing (Single)
import Test.Html.Selector exposing (..)


all : Test
all =
    let
        divWithAttribute attr =
            Html.div [ attr ] []
    in
    describe "FormParser"
        [ test "matching a string attribute" <|
            \() ->
                divWithAttribute (Attr.title "test")
                    |> Query.fromHtml
                    |> FormParser.getAttribute "title"
                    |> Expect.equal (Just "test")
        , test "extract value" <|
            \() ->
                divWithAttribute (Attr.value "thisismyvalue")
                    |> Query.fromHtml
                    |> FormParser.getAttribute "value"
                    |> Expect.equal (Just "thisismyvalue")
        , test "extract form" <|
            \() ->
                Html.form [ Attr.action "/logout", Attr.method "post" ] []
                    |> Query.fromHtml
                    |> FormParser.getAttribute "action"
                    |> Expect.equal (Just "/logout")
        , test "extract full form data given a list of selectors for a button to click" <|
            \() ->
                Html.form [ Attr.action "/delete", Attr.method "post" ]
                    [ Html.input [ Attr.name "id", Attr.value "123" ] []
                    , Html.button
                        [ Attr.type_ "submit"
                        ]
                        [ Html.text "Delete" ]
                    ]
                    |> Query.fromHtml
                    |> FormParser.parseForm
                        [ tag "button"
                        , containing [ text "Delete" ]
                        ]
                    |> Expect.equal
                        (Just
                            { action = Just "/delete"
                            , method = Just "post"
                            , inputs = [ ( "id", "123" ) ]
                            }
                        )
        ]
