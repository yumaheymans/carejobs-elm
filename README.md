# carejobs-elm

Elm client for the [CareJobs.Now](https://carejobs.now) healthcare jobs API. Search open US healthcare jobs, see the hourly pay employers post, and look up employers and states.

- Free, no API key. Elm 0.19.
- Dependencies: `elm/core`, `elm/http`, `elm/json`.
- Every request is a `Cmd`. Answers are typed records.
- The API sends open CORS headers, so it works from the browser.
- The same API is a remote MCP server at <https://carejobs.now/api/mcp>, so any MCP client can use it too.

## Install

```sh
elm install yumaheymans/carejobs-elm
```

## Quick start

```elm
module Main exposing (main)

import Browser
import CareJobs exposing (Kind(..), emptySearch)
import Html exposing (Html, div, li, text, ul)
import Http


type Model
    = Loading
    | Failed String
    | Loaded CareJobs.SearchResult


type Msg
    = GotJobs (Result Http.Error CareJobs.SearchResult)


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( Loading
                , CareJobs.searchJobs CareJobs.defaultConfig
                    { emptySearch
                        | query = Just "ICU"
                        , kind = Just Nurses
                        , state = Just "TX"
                        , limit = Just 5
                    }
                    GotJobs
                )
        , update =
            \(GotJobs result) _ ->
                case result of
                    Ok jobs ->
                        ( Loaded jobs, Cmd.none )

                    Err _ ->
                        ( Failed "The jobs could not be loaded.", Cmd.none )
        , view = view
        , subscriptions = \_ -> Sub.none
        }


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            text "Loading..."

        Failed message ->
            text message

        Loaded result ->
            div []
                [ text (String.fromInt result.total ++ " jobs match")
                , ul [] (List.map (\job -> li [] [ text (job.title ++ " | " ++ job.employer) ]) result.jobs)
                ]
```

## Functions

| Function | What it answers | Result |
| --- | --- | --- |
| `searchJobs config params toMsg` | Open jobs by query, kind, state and limit | `SearchResult` |
| `payByKind config kind toMsg` | Posted hourly pay for one kind of job, and the employers posting the most | `PayResult` |
| `employers config query limit toMsg` | The employers, AI-training programs and job boards the site reads | `EmployerList` |
| `states config toMsg` | Open jobs in every US state | `List StateSummary` |
| `kinds` | The 17 kinds of job (a plain `List Kind`, no request) | `List Kind` |

`kindToString`, `kindFromString` and `kindLabel` convert a `Kind` to and from the slug the API uses (`"nurses"`, `"nurse-practitioners"`, and so on).

`searchJobs` takes a `SearchParams` record. Start from `emptySearch` and set only the filters you want:

```elm
{ emptySearch | kind = Just Physicians, state = Just "Ohio", limit = Just 10 }
```

Every `Job` carries `url`, its page on CareJobs.Now, where a person applies. The server has no separate job lookup by id: a job is described by the search answer.

```elm
CareJobs.payByKind CareJobs.defaultConfig Nurses GotPay
-- GotPay (Ok { pay = Just { median, typicalLow, typicalHigh }, bestPaying = ... })

CareJobs.employers CareJobs.defaultConfig (Just "Kaiser") (Just 5) GotEmployers

CareJobs.states CareJobs.defaultConfig GotStates
```

### Configuration and errors

`defaultConfig` points at `https://carejobs.now` with a 60 second timeout. The server answers a query it has not seen before in up to about 25 seconds (its database may be waking up) and repeats of it in a fraction of one. Change `baseUrl` or `timeout` (milliseconds, `Nothing` for none) with record update syntax:

```elm
{ CareJobs.defaultConfig | timeout = Just 120000 }
```

Errors arrive as `Http.Error`. A JSON-RPC error or a tool the server runs and refuses arrives as `Http.BadBody` with the server's message.

The decoders (`searchResultDecoder`, `payResultDecoder`, `employerListDecoder`, `stateListDecoder`) are exposed to test or to use with your own requests.

## Tests

```sh
npm install -g elm elm-test
elm-test
```

The tests decode answers recorded from the live API.

## Data

Data from [CareJobs.Now](https://carejobs.now), CC BY 4.0. Docs and downloads: <https://carejobs.now/data>.

## License

Code: MIT, copyright O-mega Enterprise, Inc. See [LICENSE](LICENSE).
