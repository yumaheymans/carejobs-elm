module CareJobs exposing
    ( Config, defaultConfig
    , SearchParams, emptySearch, searchJobs
    , SearchResult, SearchPay, Job
    , payByKind, PayResult, PayStats, EmployerPay
    , employers, EmployerList, Employer
    , states, StateSummary, KindCount
    , Kind(..), kinds, kindToString, kindFromString, kindLabel
    , searchResultDecoder, payResultDecoder, employerListDecoder, stateListDecoder
    )

{-| Client for the [CareJobs.Now](https://carejobs.now) healthcare jobs API.
Search open US healthcare jobs, see the hourly pay employers post, and look up
employers and states. The API is free and needs no key.

Every request is a `Cmd` that posts one JSON-RPC call to the CareJobs.Now MCP
endpoint (`https://carejobs.now/api/mcp`), which allows browser requests from
any origin.

Data from CareJobs.Now (<https://carejobs.now>), CC BY 4.0.


# Configuration

@docs Config, defaultConfig


# Search jobs

@docs SearchParams, emptySearch, searchJobs
@docs SearchResult, SearchPay, Job


# Pay

@docs payByKind, PayResult, PayStats, EmployerPay


# Employers

@docs employers, EmployerList, Employer


# States

@docs states, StateSummary, KindCount


# Kinds of job

@docs Kind, kinds, kindToString, kindFromString, kindLabel


# Decoders

The decoders read the `structuredContent` of a tool answer. They are exposed
so you can test them or reuse them with your own requests.

@docs searchResultDecoder, payResultDecoder, employerListDecoder, stateListDecoder

-}

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- CONFIGURATION


{-| Where to send requests. `baseUrl` has no trailing slash; the MCP endpoint
is `baseUrl ++ "/api/mcp"`. `timeout` is in milliseconds, `Nothing` for none.
-}
type alias Config =
    { baseUrl : String
    , timeout : Maybe Float
    }


{-| The live API at `https://carejobs.now`, with a 60 second timeout. The server
answers a query it has not seen before in up to about 25 seconds, then repeats
of it in a fraction of one.
-}
defaultConfig : Config
defaultConfig =
    { baseUrl = "https://carejobs.now"
    , timeout = Just 60000
    }



-- KINDS


{-| A kind of healthcare job, the `kind` filter of the API.
-}
type Kind
    = Nurses
    | NursePractitioners
    | Physicians
    | NursingAssistants
    | Caregivers
    | TherapyRehab
    | MentalHealth
    | Pharmacy
    | Imaging
    | Lab
    | SurgicalTechs
    | EmtsParamedics
    | MedicalAssistants
    | Dental
    | Admin
    | AlliedHealth
    | AiTraining


{-| All 17 kinds of job, in the order the API documents them.
-}
kinds : List Kind
kinds =
    [ Nurses
    , NursePractitioners
    , Physicians
    , NursingAssistants
    , Caregivers
    , TherapyRehab
    , MentalHealth
    , Pharmacy
    , Imaging
    , Lab
    , SurgicalTechs
    , EmtsParamedics
    , MedicalAssistants
    , Dental
    , Admin
    , AlliedHealth
    , AiTraining
    ]


{-| The slug the API uses for a kind, for example `"nurses"` or
`"nurse-practitioners"`.
-}
kindToString : Kind -> String
kindToString kind =
    case kind of
        Nurses ->
            "nurses"

        NursePractitioners ->
            "nurse-practitioners"

        Physicians ->
            "physicians"

        NursingAssistants ->
            "nursing-assistants"

        Caregivers ->
            "caregivers"

        TherapyRehab ->
            "therapy-rehab"

        MentalHealth ->
            "mental-health"

        Pharmacy ->
            "pharmacy"

        Imaging ->
            "imaging"

        Lab ->
            "lab"

        SurgicalTechs ->
            "surgical-techs"

        EmtsParamedics ->
            "emts-paramedics"

        MedicalAssistants ->
            "medical-assistants"

        Dental ->
            "dental"

        Admin ->
            "admin"

        AlliedHealth ->
            "allied-health"

        AiTraining ->
            "ai-training"


{-| Read a kind from its API slug. `Nothing` for a slug this version does not
know.
-}
kindFromString : String -> Maybe Kind
kindFromString slug =
    kinds
        |> List.filter (\kind -> kindToString kind == slug)
        |> List.head


{-| A readable name for a kind, for example `"Nurses"` or `"Doctors"`.
-}
kindLabel : Kind -> String
kindLabel kind =
    case kind of
        Nurses ->
            "Nurses"

        NursePractitioners ->
            "NPs and PAs"

        Physicians ->
            "Doctors"

        NursingAssistants ->
            "CNAs and patient care techs"

        Caregivers ->
            "Caregivers"

        TherapyRehab ->
            "Therapy and rehab"

        MentalHealth ->
            "Mental health"

        Pharmacy ->
            "Pharmacy"

        Imaging ->
            "Imaging"

        Lab ->
            "Lab"

        SurgicalTechs ->
            "Surgical techs"

        EmtsParamedics ->
            "EMS"

        MedicalAssistants ->
            "Medical assistants"

        Dental ->
            "Dental"

        Admin ->
            "Front desk and admin"

        AlliedHealth ->
            "Allied health"

        AiTraining ->
            "AI training"



-- SEARCH


{-| Filters for [`searchJobs`](#searchJobs). Every field is optional and
`Nothing` leaves that filter off.

  - `query`: words every job must carry in its title, place, department or
    schedule, for example `"ICU"`, `"night"` or `"Houston"`
  - `kind`: one kind of job
  - `state`: a US state, its two-letter code (`"TX"`) or its name (`"Texas"`)
  - `limit`: how many jobs to return, 1 to 25 (the server default is 10)

-}
type alias SearchParams =
    { query : Maybe String
    , kind : Maybe Kind
    , state : Maybe String
    , limit : Maybe Int
    }


{-| A search with every filter off.
-}
emptySearch : SearchParams
emptySearch =
    { query = Nothing
    , kind = Nothing
    , state = Nothing
    , limit = Nothing
    }


{-| Search the open healthcare jobs.

    searchJobs defaultConfig
        { emptySearch
            | query = Just "ICU"
            , kind = Just Nurses
            , state = Just "TX"
            , limit = Just 5
        }
        GotJobs

-}
searchJobs : Config -> SearchParams -> (Result Http.Error SearchResult -> msg) -> Cmd msg
searchJobs config params toMsg =
    callTool config
        "search_healthcare_jobs"
        (Encode.object
            (List.filterMap identity
                [ params.query |> Maybe.map (\q -> ( "query", Encode.string q ))
                , params.kind |> Maybe.map (\k -> ( "kind", Encode.string (kindToString k) ))
                , params.state |> Maybe.map (\s -> ( "state", Encode.string s ))
                , params.limit |> Maybe.map (\n -> ( "limit", Encode.int n ))
                ]
            )
        )
        searchResultDecoder
        toMsg


{-| The answer of [`searchJobs`](#searchJobs).

  - `total`: how many open jobs match, all of them and not only the ones in `jobs`
  - `pay`: the posted pay across all matching jobs, `Nothing` when none post a rate
  - `jobs`: the jobs returned, at most the limit
  - `more`: the same search on the CareJobs.Now site

-}
type alias SearchResult =
    { total : Int
    , pay : Maybe SearchPay
    , jobs : List Job
    , more : Maybe String
    }


{-| The posted hourly pay in US dollars across the jobs a search matches.
`priced` is how many matching jobs post an hourly rate, `open` how many match.
-}
type alias SearchPay =
    { typicalLow : Float
    , typicalHigh : Float
    , median : Float
    , rateSamples : Int
    , priced : Int
    , open : Int
    }


{-| One open job. `url` is the job's page on CareJobs.Now, where the person
applies. `pay` is the posted pay as text, for example `"$42.43-$86.19/hr"`.
`via` names the job board a job was read through, when it is another
employer's job.
-}
type alias Job =
    { title : String
    , employer : String
    , via : Maybe String
    , kind : String
    , pay : Maybe String
    , schedule : List String
    , facility : Maybe String
    , location : Maybe String
    , listed : String
    , url : String
    , employerPage : Maybe String
    }



-- PAY


{-| What open jobs of one kind post as hourly pay, measured from the posted
hourly rates (before shift differentials and bonuses), plus the employers
posting the most for it.

    payByKind defaultConfig Nurses GotPay

-}
payByKind : Config -> Kind -> (Result Http.Error PayResult -> msg) -> Cmd msg
payByKind config kind toMsg =
    callTool config
        "get_healthcare_pay"
        (Encode.object [ ( "kind", Encode.string (kindToString kind) ) ])
        payResultDecoder
        toMsg


{-| The answer of [`payByKind`](#payByKind). `pay` is `Nothing` when too few
open jobs post an hourly rate to measure.
-}
type alias PayResult =
    { asOf : String
    , kind : String
    , openJobs : Int
    , pay : Maybe PayStats
    , bestPaying : List EmployerPay
    }


{-| Posted hourly pay in US dollars. `typicalLow` is the 25th percentile of what
jobs pay, `typicalHigh` the 90th percentile advertised ceiling. `noRate` is how
many of the `openJobs` post no hourly rate. `marketRate` is true only where at
least three employers post a rate.
-}
type alias PayStats =
    { median : Float
    , typicalLow : Float
    , typicalHigh : Float
    , rateSamples : Int
    , employers : Int
    , openJobs : Int
    , noRate : Int
    , marketRate : Bool
    }


{-| The pay one employer posts for a kind of job.
-}
type alias EmployerPay =
    { slug : String
    , name : String
    , stats : PayStats
    }



-- EMPLOYERS


{-| The employers, AI-training programs and job boards CareJobs.Now reads, most
open jobs first. Pass `Nothing` for the whole list or words from a name
(`Just "Kaiser"`), and `Just n` to limit how many come back (1 to 100, the
server default is 20).

    employers defaultConfig (Just "children") (Just 10) GotEmployers

-}
employers : Config -> Maybe String -> Maybe Int -> (Result Http.Error EmployerList -> msg) -> Cmd msg
employers config query limit toMsg =
    callTool config
        "list_healthcare_employers"
        (Encode.object
            (List.filterMap identity
                [ query |> Maybe.map (\q -> ( "query", Encode.string q ))
                , limit |> Maybe.map (\n -> ( "limit", Encode.int n ))
                ]
            )
        )
        employerListDecoder
        toMsg


{-| The answer of [`employers`](#employers). `total` counts every match, and
`entries` holds the ones returned.
-}
type alias EmployerList =
    { total : Int
    , entries : List Employer
    }


{-| One employer, AI-training program or job board. `slug` is the name the API
takes elsewhere, `type_` is `"employer"`, `"ai-program"` or `"job-board"`, and
`postedMedianHourly` is the median posted hourly rate when it can be measured.
-}
type alias Employer =
    { slug : String
    , name : String
    , type_ : String
    , model : Maybe String
    , care : Maybe String
    , hq : Maybe String
    , states : List String
    , openJobs : Int
    , postedMedianHourly : Maybe Float
    , url : String
    }



-- STATES


{-| Open jobs for every US state, most first, with the kinds of job each state
has the most of.

    states defaultConfig GotStates

-}
states : Config -> (Result Http.Error (List StateSummary) -> msg) -> Cmd msg
states config toMsg =
    callTool config
        "get_healthcare_jobs_by_state"
        (Encode.object [])
        stateListDecoder
        toMsg


{-| One US state: its two-letter `code`, how many jobs are open, at how many
employers, and the kinds of job it has the most of.
-}
type alias StateSummary =
    { code : String
    , name : String
    , openJobs : Int
    , employers : Int
    , topKinds : List KindCount
    , url : String
    }


{-| The open jobs of one kind of job inside a state. `kind` is the API slug (see
[`kindFromString`](#kindFromString)).
-}
type alias KindCount =
    { kind : String
    , label : String
    , openJobs : Int
    , url : Maybe String
    }



-- DECODERS


{-| Decode the `structuredContent` of a `search_healthcare_jobs` answer.
-}
searchResultDecoder : Decoder SearchResult
searchResultDecoder =
    Decode.map4 SearchResult
        (Decode.field "total" Decode.int)
        (nullable "pay" searchPayDecoder)
        (Decode.field "jobs" (Decode.list jobDecoder))
        (nullable "more" Decode.string)


searchPayDecoder : Decoder SearchPay
searchPayDecoder =
    Decode.map6 SearchPay
        (Decode.field "typical_low" Decode.float)
        (Decode.field "typical_high" Decode.float)
        (Decode.field "median" Decode.float)
        (Decode.field "rate_samples" Decode.int)
        (Decode.field "priced" Decode.int)
        (Decode.field "open" Decode.int)


jobDecoder : Decoder Job
jobDecoder =
    Decode.map8
        (\title employer via kind pay schedule facility location ->
            \listed url employerPage ->
                Job title employer via kind pay schedule facility location listed url employerPage
        )
        (Decode.field "title" Decode.string)
        (Decode.field "employer" Decode.string)
        (nullable "via" Decode.string)
        (Decode.field "kind" Decode.string)
        (nullable "pay" Decode.string)
        (Decode.map (Maybe.withDefault []) (nullable "schedule" (Decode.list Decode.string)))
        (nullable "facility" Decode.string)
        (nullable "location" Decode.string)
        |> Decode.andThen
            (\partial ->
                Decode.map3 partial
                    (Decode.field "listed" Decode.string)
                    (Decode.field "url" Decode.string)
                    (nullable "employer_page" Decode.string)
            )


{-| Decode the `structuredContent` of a `get_healthcare_pay` answer for one kind.
-}
payResultDecoder : Decoder PayResult
payResultDecoder =
    Decode.map5 PayResult
        (Decode.field "as_of" Decode.string)
        (Decode.field "kind" Decode.string)
        (Decode.field "open_jobs" Decode.int)
        (nullable "pay" payStatsDecoder)
        (Decode.map (Maybe.withDefault []) (nullable "best_paying" (Decode.list employerPayDecoder)))


payStatsDecoder : Decoder PayStats
payStatsDecoder =
    Decode.map8 PayStats
        (Decode.field "median" Decode.float)
        (Decode.field "typical_low" Decode.float)
        (Decode.field "typical_high" Decode.float)
        (Decode.field "rate_samples" Decode.int)
        (Decode.field "employers" Decode.int)
        (Decode.field "open_jobs" Decode.int)
        (Decode.field "no_rate" Decode.int)
        (Decode.field "market_rate" Decode.bool)


employerPayDecoder : Decoder EmployerPay
employerPayDecoder =
    Decode.map3 EmployerPay
        (Decode.field "slug" Decode.string)
        (Decode.field "name" Decode.string)
        payStatsDecoder


{-| Decode the `structuredContent` of a `list_healthcare_employers` answer.
-}
employerListDecoder : Decoder EmployerList
employerListDecoder =
    Decode.map2 EmployerList
        (Decode.field "total" Decode.int)
        (Decode.field "entries" (Decode.list employerDecoder))


employerDecoder : Decoder Employer
employerDecoder =
    Decode.map8
        (\slug name type_ model care hq states_ openJobs ->
            \postedMedianHourly url ->
                Employer slug name type_ model care hq states_ openJobs postedMedianHourly url
        )
        (Decode.field "slug" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "type" Decode.string)
        (nullable "model" Decode.string)
        (nullable "care" Decode.string)
        (nullable "hq" Decode.string)
        (Decode.map (Maybe.withDefault []) (nullable "states" (Decode.list Decode.string)))
        (Decode.field "open_jobs" Decode.int)
        |> Decode.andThen
            (\partial ->
                Decode.map2 partial
                    (nullable "posted_median_hourly" Decode.float)
                    (Decode.field "url" Decode.string)
            )


{-| Decode the `structuredContent` of a `get_healthcare_jobs_by_state` answer
that covers every state.
-}
stateListDecoder : Decoder (List StateSummary)
stateListDecoder =
    Decode.field "states" (Decode.list stateDecoder)


stateDecoder : Decoder StateSummary
stateDecoder =
    Decode.map6 StateSummary
        (Decode.field "code" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "open_jobs" Decode.int)
        (Decode.field "employers" Decode.int)
        (Decode.map (Maybe.withDefault []) (nullable "top_kinds" (Decode.list kindCountDecoder)))
        (Decode.field "url" Decode.string)


kindCountDecoder : Decoder KindCount
kindCountDecoder =
    Decode.map4 KindCount
        (Decode.field "kind" Decode.string)
        (Decode.field "label" Decode.string)
        (Decode.field "open_jobs" Decode.int)
        (nullable "url" Decode.string)



-- HELPERS


{-| A field that may be missing or null.
-}
nullable : String -> Decoder a -> Decoder (Maybe a)
nullable name decoder =
    Decode.maybe (Decode.field name decoder)


{-| Post one JSON-RPC `tools/call` and decode the `structuredContent` of the
result. A JSON-RPC error, or a tool that answers with `isError`, fails the
decoder with the server's message, so it arrives as `Http.BadBody`.
-}
callTool : Config -> String -> Encode.Value -> Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
callTool config tool arguments decoder toMsg =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = config.baseUrl ++ "/api/mcp"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "jsonrpc", Encode.string "2.0" )
                    , ( "id", Encode.int 1 )
                    , ( "method", Encode.string "tools/call" )
                    , ( "params"
                      , Encode.object
                            [ ( "name", Encode.string tool )
                            , ( "arguments", arguments )
                            ]
                      )
                    ]
                )
        , expect = Http.expectJson toMsg (answerDecoder decoder)
        , timeout = config.timeout
        , tracker = Nothing
        }


answerDecoder : Decoder a -> Decoder a
answerDecoder decoder =
    Decode.maybe (Decode.at [ "error", "message" ] Decode.string)
        |> Decode.andThen
            (\rpcError ->
                case rpcError of
                    Just message ->
                        Decode.fail message

                    Nothing ->
                        Decode.field "result" (resultDecoder decoder)
            )


resultDecoder : Decoder a -> Decoder a
resultDecoder decoder =
    Decode.maybe (Decode.field "isError" Decode.bool)
        |> Decode.andThen
            (\isError ->
                if isError == Just True then
                    Decode.at [ "content", "0", "text" ] Decode.string
                        |> Decode.andThen Decode.fail

                else
                    Decode.field "structuredContent" decoder
            )
