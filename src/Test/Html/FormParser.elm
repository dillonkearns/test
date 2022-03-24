module Test.Html.FormParser exposing (getAttribute, parseForm)

import Dict
import Test.Html.Internal.ElmHtml.InternalTypes exposing (ElmHtml)
import Test.Html.Query exposing (Single)
import Test.Html.Query.Internal as Internal exposing (QueryError(..))
import Test.Html.Selector
import Test.Html.Selector.Internal as Selector exposing (Selector)


getAttribute : String -> Single msg -> Maybe String
getAttribute attributeName (Internal.Single showTrace query) =
    case Internal.traverse query of
        Ok [ singleMatch ] ->
            case singleMatch of
                Test.Html.Internal.ElmHtml.InternalTypes.CustomNode record ->
                    Nothing

                Test.Html.Internal.ElmHtml.InternalTypes.NodeEntry record ->
                    record.facts.stringAttributes |> Dict.get attributeName

                _ ->
                    Nothing

        _ ->
            Nothing


type alias ParsedForm =
    { method : Maybe String, action : Maybe String, inputs : List ( String, String ) }


parseForm :
    List Selector
    -> Single msg
    -> Maybe ParsedForm
parseForm selectors (Internal.Single showTrace query) =
    {-
       Steps:

       ## Find the form

       1. Check if matching button is submit button - if not then Nothing
       2. If the button has a `form=` (https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input#attr-form), then find the form with matching `id`
       3. Otherwise, find either a <form> where the submit button is a child
       4. If the button has name/value attributes, include them in the `inputs` result

       ## Parse the form
       1. Gather all of the name/value pairs for all `input` and `textarea` elements (except `input type=button`) (TODO: that's it, right? Buttons are taken care of above)
       2. Get the form action if present
       3. Get the form method if any

    -}
    case
        query
            |> Internal.traverse
            |> Result.map (Selector.queryAll selectors)
    of
        -- TODO handle when there is more than 1 form
        Ok [ singleMatch ] ->
            let
                allForms : List (ElmHtml msg)
                allForms =
                    Internal.traverse query
                        |> Result.map (Selector.queryAll [ Test.Html.Selector.tag "form" ])
                        |> Result.withDefault []
            in
            case singleMatch of
                Test.Html.Internal.ElmHtml.InternalTypes.NodeEntry record ->
                    let
                        gatherInputs : ElmHtml msg -> List ( String, String )
                        gatherInputs parent =
                            case parent of
                                Test.Html.Internal.ElmHtml.InternalTypes.NodeEntry fields ->
                                    if fields.tag == "input" then
                                        case
                                            Maybe.map2 Tuple.pair
                                                (fields.facts.stringAttributes |> Dict.get "name")
                                                (fields.facts.stringAttributes |> Dict.get "value")
                                        of
                                            Just foundField ->
                                                [ foundField ]

                                            Nothing ->
                                                []

                                    else
                                        List.concatMap gatherInputs
                                            fields.children

                                _ ->
                                    []
                    in
                    case allForms of
                        [ (Test.Html.Internal.ElmHtml.InternalTypes.NodeEntry singleForm) as formNode ] ->
                            Just
                                { method = singleForm.facts.stringAttributes |> Dict.get "method"
                                , action = singleForm.facts.stringAttributes |> Dict.get "action"
                                , inputs = gatherInputs formNode
                                }

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing
