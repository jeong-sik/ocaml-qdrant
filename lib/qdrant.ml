(** Qdrant Vector Database Client for OCaml

    Pure OCaml client for Qdrant REST API.
    Supports vector search, collection management, and point operations.

    Reference: https://qdrant.tech/documentation/
*)

open Lwt.Syntax

(** {1 Configuration} *)

type config = {
  base_url: string;
  api_key: string option;
  timeout_s: float;
}

let default_config = {
  base_url = Sys.getenv_opt "QDRANT_URL" |> Option.value ~default:"http://localhost:6333";
  api_key = Sys.getenv_opt "QDRANT_API_KEY";
  timeout_s = 30.0;
}

(** {1 Types} *)

(** Distance metrics for vector similarity *)
type distance =
  | Cosine
  | Euclid
  | Dot
  | Manhattan

let distance_to_string = function
  | Cosine -> "Cosine"
  | Euclid -> "Euclid"
  | Dot -> "Dot"
  | Manhattan -> "Manhattan"

(** Vector configuration *)
type vector_config = {
  size: int;
  distance: distance;
}

(** Point with vector and payload *)
type point = {
  id: string;
  vector: float array;
  payload: (string * Yojson.Safe.t) list;
}

(** Search result *)
type search_result = {
  id: string;
  score: float;
  payload: Yojson.Safe.t;
  vector: float array option;
}

(** Collection info *)
type collection_info = {
  status: string;
  vectors_count: int;
  points_count: int;
  segments_count: int;
}

(** {1 Errors} *)

type error =
  | ConnectionError of string
  | ApiError of int * string
  | ParseError of string
  | Timeout
  | NotFound of string

let error_to_string = function
  | ConnectionError msg -> Printf.sprintf "Connection error: %s" msg
  | ApiError (code, msg) -> Printf.sprintf "API error %d: %s" code msg
  | ParseError msg -> Printf.sprintf "Parse error: %s" msg
  | Timeout -> "Request timeout"
  | NotFound name -> Printf.sprintf "Not found: %s" name

(** {1 Internal HTTP} *)

let make_request ~config ~meth ~path ?(body=`Null) () =
  let uri = Uri.of_string (config.base_url ^ path) in
  let headers =
    let base = Cohttp.Header.init () in
    let base = Cohttp.Header.add base "Content-Type" "application/json" in
    match config.api_key with
    | Some key -> Cohttp.Header.add base "api-key" key
    | None -> base
  in
  let body_str = match body with
    | `Null -> ""
    | json -> Yojson.Safe.to_string json
  in

  try%lwt
    let* (resp, resp_body) = match meth with
      | `POST ->
        let body = Cohttp_lwt.Body.of_string body_str in
        Cohttp_lwt_unix.Client.post ~headers ~body uri
      | `PUT ->
        let body = Cohttp_lwt.Body.of_string body_str in
        Cohttp_lwt_unix.Client.put ~headers ~body uri
      | `DELETE ->
        Cohttp_lwt_unix.Client.delete ~headers uri
      | `GET ->
        Cohttp_lwt_unix.Client.get ~headers uri
      | `PATCH ->
        let body = Cohttp_lwt.Body.of_string body_str in
        Cohttp_lwt_unix.Client.patch ~headers ~body uri
    in
    let status = Cohttp.Response.status resp in
    let code = Cohttp.Code.code_of_status status in
    let* body_str = Cohttp_lwt.Body.to_string resp_body in

    if Cohttp.Code.is_success code then
      Lwt.return_ok body_str
    else if code = 404 then
      Lwt.return_error (NotFound path)
    else
      Lwt.return_error (ApiError (code, body_str))
  with
  | Unix.Unix_error (err, _, _) ->
    Lwt.return_error (ConnectionError (Unix.error_message err))
  | exn ->
    Lwt.return_error (ConnectionError (Printexc.to_string exn))

(** {1 Health & Info} *)

(** Check if Qdrant is healthy *)
let health ?(config=default_config) () =
  let* result = make_request ~config ~meth:`GET ~path:"/" () in
  match result with
  | Ok _ -> Lwt.return_ok true
  | Error (ConnectionError _) -> Lwt.return_ok false
  | Error e -> Lwt.return_error e

(** Get Qdrant version *)
let version ?(config=default_config) () =
  let* result = make_request ~config ~meth:`GET ~path:"/" () in
  match result with
  | Error e -> Lwt.return_error e
  | Ok body ->
    try
      let json = Yojson.Safe.from_string body in
      let version = json
        |> Yojson.Safe.Util.member "version"
        |> Yojson.Safe.Util.to_string
      in
      Lwt.return_ok version
    with exn ->
      Lwt.return_error (ParseError (Printexc.to_string exn))

(** {1 Collections} *)

(** List all collections *)
let list_collections ?(config=default_config) () =
  let* result = make_request ~config ~meth:`GET ~path:"/collections" () in
  match result with
  | Error e -> Lwt.return_error e
  | Ok body ->
    try
      let json = Yojson.Safe.from_string body in
      let collections = json
        |> Yojson.Safe.Util.member "result"
        |> Yojson.Safe.Util.member "collections"
        |> Yojson.Safe.Util.to_list
        |> List.map (fun c ->
          c |> Yojson.Safe.Util.member "name" |> Yojson.Safe.Util.to_string)
      in
      Lwt.return_ok collections
    with exn ->
      Lwt.return_error (ParseError (Printexc.to_string exn))

(** Check if collection exists *)
let collection_exists ?(config=default_config) ~name () =
  let path = Printf.sprintf "/collections/%s/exists" name in
  let* result = make_request ~config ~meth:`GET ~path () in
  match result with
  | Error (NotFound _) -> Lwt.return_ok false
  | Error e -> Lwt.return_error e
  | Ok body ->
    try
      let json = Yojson.Safe.from_string body in
      let exists = json
        |> Yojson.Safe.Util.member "result"
        |> Yojson.Safe.Util.member "exists"
        |> Yojson.Safe.Util.to_bool
      in
      Lwt.return_ok exists
    with _ ->
      Lwt.return_ok true  (* If we got a response, it exists *)

(** Get collection info *)
let get_collection ?(config=default_config) ~name () =
  let path = Printf.sprintf "/collections/%s" name in
  let* result = make_request ~config ~meth:`GET ~path () in
  match result with
  | Error e -> Lwt.return_error e
  | Ok body ->
    try
      let json = Yojson.Safe.from_string body in
      let result = json |> Yojson.Safe.Util.member "result" in
      let status = result
        |> Yojson.Safe.Util.member "status"
        |> Yojson.Safe.Util.to_string
      in
      let vectors_count = result
        |> Yojson.Safe.Util.member "vectors_count"
        |> Yojson.Safe.Util.to_int_option
        |> Option.value ~default:0
      in
      let points_count = result
        |> Yojson.Safe.Util.member "points_count"
        |> Yojson.Safe.Util.to_int_option
        |> Option.value ~default:0
      in
      let segments_count = result
        |> Yojson.Safe.Util.member "segments_count"
        |> Yojson.Safe.Util.to_int_option
        |> Option.value ~default:0
      in
      Lwt.return_ok { status; vectors_count; points_count; segments_count }
    with exn ->
      Lwt.return_error (ParseError (Printexc.to_string exn))

(** Create a collection *)
let create_collection ?(config=default_config) ~name ~vector_config () =
  let path = Printf.sprintf "/collections/%s" name in
  let body = `Assoc [
    ("vectors", `Assoc [
      ("size", `Int vector_config.size);
      ("distance", `String (distance_to_string vector_config.distance));
    ]);
  ] in
  let* result = make_request ~config ~meth:`PUT ~path ~body () in
  match result with
  | Ok _ -> Lwt.return_ok ()
  | Error e -> Lwt.return_error e

(** Delete a collection *)
let delete_collection ?(config=default_config) ~name () =
  let path = Printf.sprintf "/collections/%s" name in
  let* result = make_request ~config ~meth:`DELETE ~path () in
  match result with
  | Ok _ -> Lwt.return_ok ()
  | Error (NotFound _) -> Lwt.return_ok ()  (* Already deleted *)
  | Error e -> Lwt.return_error e

(** {1 Points} *)

(** Upsert points into a collection *)
let upsert ?(config=default_config) ~collection ~(points:point list) () =
  let path = Printf.sprintf "/collections/%s/points?wait=true" collection in
  let points_json = List.map (fun (p:point) ->
    `Assoc [
      ("id", `String p.id);
      ("vector", `List (Array.to_list (Array.map (fun f -> `Float f) p.vector)));
      ("payload", `Assoc p.payload);
    ]
  ) points in
  let body = `Assoc [("points", `List points_json)] in
  let* result = make_request ~config ~meth:`PUT ~path ~body () in
  match result with
  | Ok _ -> Lwt.return_ok (List.length points)
  | Error e -> Lwt.return_error e

(** Delete points by IDs *)
let delete_points ?(config=default_config) ~collection ~ids () =
  let path = Printf.sprintf "/collections/%s/points/delete?wait=true" collection in
  let body = `Assoc [
    ("points", `List (List.map (fun id -> `String id) ids));
  ] in
  let* result = make_request ~config ~meth:`POST ~path ~body () in
  match result with
  | Ok _ -> Lwt.return_ok ()
  | Error e -> Lwt.return_error e

(** Get point by ID *)
let get_point ?(config=default_config) ~collection ~id () =
  let path = Printf.sprintf "/collections/%s/points/%s" collection id in
  let* result = make_request ~config ~meth:`GET ~path () in
  match result with
  | Error e -> Lwt.return_error e
  | Ok body ->
    try
      let json = Yojson.Safe.from_string body in
      let result = json |> Yojson.Safe.Util.member "result" in
      let id = result
        |> Yojson.Safe.Util.member "id"
        |> (fun x -> match x with
          | `String s -> s
          | `Int i -> string_of_int i
          | _ -> "unknown")
      in
      let vector = result
        |> Yojson.Safe.Util.member "vector"
        |> Yojson.Safe.Util.to_list
        |> List.map Yojson.Safe.Util.to_float
        |> Array.of_list
      in
      let payload = result |> Yojson.Safe.Util.member "payload" in
      Lwt.return_ok { id; score = 1.0; payload; vector = Some vector }
    with exn ->
      Lwt.return_error (ParseError (Printexc.to_string exn))

(** {1 Search} *)

(** Search for similar vectors *)
let search ?(config=default_config) ~collection ~vector ~limit
    ?(score_threshold=0.0) ?(with_vector=false) () =
  let path = Printf.sprintf "/collections/%s/points/search" collection in
  let body = `Assoc [
    ("vector", `List (Array.to_list (Array.map (fun f -> `Float f) vector)));
    ("limit", `Int limit);
    ("score_threshold", `Float score_threshold);
    ("with_payload", `Bool true);
    ("with_vector", `Bool with_vector);
  ] in
  let* result = make_request ~config ~meth:`POST ~path ~body () in
  match result with
  | Error e -> Lwt.return_error e
  | Ok body ->
    try
      let json = Yojson.Safe.from_string body in
      let results = json
        |> Yojson.Safe.Util.member "result"
        |> Yojson.Safe.Util.to_list
        |> List.map (fun item ->
          let id = item
            |> Yojson.Safe.Util.member "id"
            |> (fun x -> match x with
              | `String s -> s
              | `Int i -> string_of_int i
              | _ -> "unknown")
          in
          let score = item
            |> Yojson.Safe.Util.member "score"
            |> Yojson.Safe.Util.to_float
          in
          let payload = item |> Yojson.Safe.Util.member "payload" in
          let vector = if with_vector then
            Some (item
              |> Yojson.Safe.Util.member "vector"
              |> Yojson.Safe.Util.to_list
              |> List.map Yojson.Safe.Util.to_float
              |> Array.of_list)
          else None
          in
          { id; score; payload; vector }
        )
      in
      Lwt.return_ok results
    with exn ->
      Lwt.return_error (ParseError (Printexc.to_string exn))

(** Search with filter *)
let search_with_filter ?(config=default_config) ~collection ~vector ~limit
    ~filter ?(score_threshold=0.0) () =
  let path = Printf.sprintf "/collections/%s/points/search" collection in
  let body = `Assoc [
    ("vector", `List (Array.to_list (Array.map (fun f -> `Float f) vector)));
    ("limit", `Int limit);
    ("score_threshold", `Float score_threshold);
    ("with_payload", `Bool true);
    ("filter", filter);
  ] in
  let* result = make_request ~config ~meth:`POST ~path ~body () in
  match result with
  | Error e -> Lwt.return_error e
  | Ok body ->
    try
      let json = Yojson.Safe.from_string body in
      let results = json
        |> Yojson.Safe.Util.member "result"
        |> Yojson.Safe.Util.to_list
        |> List.map (fun item ->
          let id = item
            |> Yojson.Safe.Util.member "id"
            |> (fun x -> match x with
              | `String s -> s
              | `Int i -> string_of_int i
              | _ -> "unknown")
          in
          let score = item
            |> Yojson.Safe.Util.member "score"
            |> Yojson.Safe.Util.to_float
          in
          let payload = item |> Yojson.Safe.Util.member "payload" in
          { id; score; payload; vector = None }
        )
      in
      Lwt.return_ok results
    with exn ->
      Lwt.return_error (ParseError (Printexc.to_string exn))

(** {1 Scroll} *)

(** Scroll through all points *)
let scroll ?(config=default_config) ~collection ?(limit=10) ?offset () =
  let path = Printf.sprintf "/collections/%s/points/scroll" collection in
  let body = `Assoc (
    [("limit", `Int limit); ("with_payload", `Bool true); ("with_vector", `Bool false)]
    @ (match offset with Some o -> [("offset", `String o)] | None -> [])
  ) in
  let* result = make_request ~config ~meth:`POST ~path ~body () in
  match result with
  | Error e -> Lwt.return_error e
  | Ok body ->
    try
      let json = Yojson.Safe.from_string body in
      let result_obj = json |> Yojson.Safe.Util.member "result" in
      let points = result_obj
        |> Yojson.Safe.Util.member "points"
        |> Yojson.Safe.Util.to_list
        |> List.map (fun item ->
          let id = item
            |> Yojson.Safe.Util.member "id"
            |> (fun x -> match x with
              | `String s -> s
              | `Int i -> string_of_int i
              | _ -> "unknown")
          in
          let payload = item |> Yojson.Safe.Util.member "payload" in
          { id; score = 0.0; payload; vector = None }
        )
      in
      let next_offset = result_obj
        |> Yojson.Safe.Util.member "next_page_offset"
        |> (fun x -> match x with
          | `String s -> Some s
          | `Int i -> Some (string_of_int i)
          | _ -> None)
      in
      Lwt.return_ok (points, next_offset)
    with exn ->
      Lwt.return_error (ParseError (Printexc.to_string exn))

(** Count points in collection *)
let count ?(config=default_config) ~collection () =
  let path = Printf.sprintf "/collections/%s/points/count" collection in
  let body = `Assoc [("exact", `Bool true)] in
  let* result = make_request ~config ~meth:`POST ~path ~body () in
  match result with
  | Error e -> Lwt.return_error e
  | Ok body ->
    try
      let json = Yojson.Safe.from_string body in
      let count = json
        |> Yojson.Safe.Util.member "result"
        |> Yojson.Safe.Util.member "count"
        |> Yojson.Safe.Util.to_int
      in
      Lwt.return_ok count
    with exn ->
      Lwt.return_error (ParseError (Printexc.to_string exn))
