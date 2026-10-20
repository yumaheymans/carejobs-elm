module DecoderTests exposing (suite)

import CareJobs
import Expect
import Json.Decode as Decode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "CareJobs decoders (recorded answers of the live API)"
        [ test "search result" <|
            \_ ->
                case Decode.decodeString CareJobs.searchResultDecoder searchJson of
                    Ok result ->
                        Expect.all
                            [ \r -> Expect.equal 2 (List.length r.jobs)
                            , \r -> Expect.equal True (r.total >= 2)
                            , \r -> Expect.equal (Just 45) (Maybe.map .median r.pay)
                            , \r -> Expect.equal [ "Nurses" ] (List.map .kind (List.take 1 r.jobs))
                            , \r -> Expect.equal [ Nothing ] (List.map .via (List.take 1 r.jobs))
                            ]
                            result

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "pay of one kind" <|
            \_ ->
                case Decode.decodeString CareJobs.payResultDecoder payJson of
                    Ok result ->
                        Expect.all
                            [ \r -> Expect.equal "nurses" r.kind
                            , \r -> Expect.equal (Just 45) (Maybe.map .median r.pay)
                            , \r -> Expect.equal 2 (List.length r.bestPaying)
                            ]
                            result

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "employers" <|
            \_ ->
                case Decode.decodeString CareJobs.employerListDecoder employersJson of
                    Ok result ->
                        Expect.all
                            [ \r -> Expect.equal 2 (List.length r.entries)
                            , \r -> Expect.equal [ "cross-country-healthcare" ] (List.map .slug (List.take 1 r.entries))
                            ]
                            result

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "states" <|
            \_ ->
                case Decode.decodeString CareJobs.stateListDecoder statesJson of
                    Ok list ->
                        Expect.equal [ "TX", "FL" ] (List.map .code list)

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "kinds round trip through their slugs" <|
            \_ ->
                CareJobs.kinds
                    |> List.map (\k -> CareJobs.kindFromString (CareJobs.kindToString k))
                    |> Expect.equal (List.map Just CareJobs.kinds)
        , test "there are 17 kinds" <|
            \_ ->
                Expect.equal 17 (List.length CareJobs.kinds)
        ]


searchJson : String
searchJson =
    """{"total":562,"pay":{"typical_low":45,"typical_high":65,"median":45,"rate_samples":91,"priced":92,"open":562},"jobs":[{"title":"New Grad Registered Nurse (RN) - ICU","employer":"Tenet Healthcare","via":null,"kind":"Nurses","pay":"$55 to $95/hr (typical of registered nurse and LPN jobs at Tenet Healthcare, measured from its open jobs; not a rate this posting states)","schedule":["Full-time"],"facility":null,"location":"San Antonio, TX","listed":"2026-10-19","url":"https://carejobs.now/jobs/tenet-healthcare/new-grad-registered-nurse-rn-icu","employer_page":"https://carejobs.now/employers/tenet-healthcare"},{"title":"Registered Nurse ICU","employer":"Prime Healthcare","via":null,"kind":"Nurses","pay":"$50 to $65/hr (typical of registered nurse and LPN jobs at Prime Healthcare, measured from its open jobs; not a rate this posting states)","schedule":[],"facility":null,"location":"Dallas, TX","listed":"2026-10-19","url":"https://carejobs.now/jobs/prime-healthcare/registered-nurse-icu","employer_page":"https://carejobs.now/employers/prime-healthcare"}],"more":"https://carejobs.now/?work=nurses&q=ICU&state=TX&country=all#live-roles"}"""


payJson : String
payJson =
    """{"as_of":"2026-10-20","kind":"nurses","open_jobs":97460,"pay":{"median":45,"typical_low":40,"typical_high":80,"rate_samples":21465,"employers":515,"open_jobs":97460,"no_rate":70317,"market_rate":true},"best_paying":[{"slug":"the-helper-bees","name":"The Helper Bees","median":125,"typical_low":125,"typical_high":125,"rate_samples":17,"employers":1,"open_jobs":17,"no_rate":0,"market_rate":false},{"slug":"stanford-health-care","name":"Stanford Health Care","median":105,"typical_low":100,"typical_high":115,"rate_samples":32,"employers":1,"open_jobs":34,"no_rate":0,"market_rate":false}]}"""


employersJson : String
employersJson =
    """{"total":2017,"entries":[{"slug":"cross-country-healthcare","name":"Cross Country Healthcare","type":"employer","model":"Travel, locum tenens and per diem staffing","care":"Travel nursing & staffing agencies","hq":"Boca Raton, FL","states":["AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"],"open_jobs":7061,"posted_median_hourly":null,"url":"https://carejobs.now/employers/cross-country-healthcare"},{"slug":"cvs-health","name":"CVS Health","type":"employer","model":"Pharmacy and retail health","care":"Pharmacy & retail health","hq":"Woonsocket, RI","states":["PA","CA","TX","OH","FL","MA","NY","IL","NJ","NC","VA","IN","MI","CT","GA","MD","AZ","SC","MO","LA","MN","TN","WA","RI","AL","WI","KS","KY","NV","WV","CO","IA","NH","OK","NE","OR","DE","MS","ME","HI","AR","DC","UT","NM","ND","VT","ID","MT","SD","WY","AK"],"open_jobs":6267,"posted_median_hourly":30,"url":"https://carejobs.now/employers/cvs-health"}]}"""


statesJson : String
statesJson =
    """{"states":[{"code":"TX","name":"Texas","open_jobs":11319,"employers":224,"top_kinds":[{"kind":"nurses","label":"Nurses","open_jobs":3997},{"kind":"therapy-rehab","label":"Therapy & rehab","open_jobs":1497},{"kind":"physicians","label":"Doctors","open_jobs":1068}],"url":"https://carejobs.now/states/texas"},{"code":"FL","name":"Florida","open_jobs":10944,"employers":201,"top_kinds":[{"kind":"nurses","label":"Nurses","open_jobs":3744},{"kind":"therapy-rehab","label":"Therapy & rehab","open_jobs":1195},{"kind":"imaging","label":"Imaging","open_jobs":959}],"url":"https://carejobs.now/states/florida"}]}"""
