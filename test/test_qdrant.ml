(** Qdrant Client Unit Tests *)

module Q = Qdrant

(* Test helpers *)
let distance_testable = Alcotest.testable
  (fun fmt d -> Format.fprintf fmt "%s" (Q.distance_to_string d))
  (=)

(* Distance type tests *)
let test_distance_cosine () =
  Alcotest.(check string) "cosine" "Cosine" (Q.distance_to_string Q.Cosine)

let test_distance_euclid () =
  Alcotest.(check string) "euclid" "Euclid" (Q.distance_to_string Q.Euclid)

let test_distance_dot () =
  Alcotest.(check string) "dot" "Dot" (Q.distance_to_string Q.Dot)

let test_distance_manhattan () =
  Alcotest.(check string) "manhattan" "Manhattan" (Q.distance_to_string Q.Manhattan)

(* Error type tests *)
let test_error_connection () =
  let err = Q.ConnectionError "timeout" in
  Alcotest.(check string) "connection error"
    "Connection error: timeout"
    (Q.error_to_string err)

let test_error_api () =
  let err = Q.ApiError (404, "not found") in
  Alcotest.(check string) "api error"
    "API error 404: not found"
    (Q.error_to_string err)

let test_error_parse () =
  let err = Q.ParseError "invalid json" in
  Alcotest.(check string) "parse error"
    "Parse error: invalid json"
    (Q.error_to_string err)

let test_error_timeout () =
  Alcotest.(check string) "timeout"
    "Request timeout"
    (Q.error_to_string Q.Timeout)

let test_error_notfound () =
  let err = Q.NotFound "collection" in
  Alcotest.(check string) "not found"
    "Not found: collection"
    (Q.error_to_string err)

(* Config tests *)
let test_default_config () =
  let config = Q.default_config in
  Alcotest.(check int) "timeout" 30 (int_of_float config.timeout_s)

(* Vector config tests *)
let test_vector_config () =
  let vc = Q.{ size = 1024; distance = Cosine } in
  Alcotest.(check int) "size" 1024 vc.size;
  Alcotest.(check distance_testable) "distance" Q.Cosine vc.distance

(* Point creation tests *)
let test_point_creation () =
  let p = Q.{
    id = "test-1";
    vector = [| 0.1; 0.2; 0.3 |];
    payload = [("key", `String "value")];
  } in
  Alcotest.(check string) "id" "test-1" p.id;
  Alcotest.(check int) "vector length" 3 (Array.length p.vector);
  Alcotest.(check int) "payload length" 1 (List.length p.payload)

(* Search result tests *)
let test_search_result () =
  let sr = Q.{
    id = "result-1";
    score = 0.95;
    payload = `Assoc [("title", `String "test")];
    vector = Some [| 0.1; 0.2 |];
  } in
  Alcotest.(check string) "id" "result-1" sr.id;
  Alcotest.(check (float 0.01)) "score" 0.95 sr.score;
  Alcotest.(check bool) "has vector" true (Option.is_some sr.vector)

(* Collection info tests *)
let test_collection_info () =
  let ci = Q.{
    status = "green";
    vectors_count = 1000;
    points_count = 1000;
    segments_count = 2;
  } in
  Alcotest.(check string) "status" "green" ci.status;
  Alcotest.(check int) "vectors" 1000 ci.vectors_count;
  Alcotest.(check int) "points" 1000 ci.points_count;
  Alcotest.(check int) "segments" 2 ci.segments_count

(* Test suites *)
let () =
  Alcotest.run "Qdrant" [
    "distance", [
      ("cosine", `Quick, test_distance_cosine);
      ("euclid", `Quick, test_distance_euclid);
      ("dot", `Quick, test_distance_dot);
      ("manhattan", `Quick, test_distance_manhattan);
    ];
    "error", [
      ("connection", `Quick, test_error_connection);
      ("api", `Quick, test_error_api);
      ("parse", `Quick, test_error_parse);
      ("timeout", `Quick, test_error_timeout);
      ("notfound", `Quick, test_error_notfound);
    ];
    "config", [
      ("default", `Quick, test_default_config);
    ];
    "types", [
      ("vector_config", `Quick, test_vector_config);
      ("point", `Quick, test_point_creation);
      ("search_result", `Quick, test_search_result);
      ("collection_info", `Quick, test_collection_info);
    ];
  ]
