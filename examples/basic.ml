(** Basic Qdrant usage example *)

let () = Lwt_main.run begin
  let open Lwt.Syntax in
  let open Qdrant in

  (* Check health *)
  let* health_result = health () in
  (match health_result with
   | Ok true -> print_endline "✅ Qdrant is healthy"
   | Ok false -> print_endline "❌ Qdrant is not responding"
   | Error e -> print_endline ("❌ " ^ error_to_string e));

  (* List collections *)
  let* collections = list_collections () in
  (match collections with
   | Ok cols ->
     Printf.printf "📦 Collections: %d\n" (List.length cols);
     List.iter (Printf.printf "  - %s\n") cols
   | Error e ->
     print_endline ("❌ " ^ error_to_string e));

  (* Create a test collection *)
  let collection_name = "ocaml_test" in
  let vector_config = { size = 4; distance = Cosine } in

  let* _ = delete_collection ~name:collection_name () in
  let* create_result = create_collection ~name:collection_name ~vector_config () in
  (match create_result with
   | Ok () -> Printf.printf "✅ Created collection: %s\n" collection_name
   | Error e -> print_endline ("❌ " ^ error_to_string e));

  (* Insert some points *)
  let points = [
    { id = "1"; vector = [| 0.1; 0.2; 0.3; 0.4 |]; payload = [("text", `String "hello")] };
    { id = "2"; vector = [| 0.2; 0.3; 0.4; 0.5 |]; payload = [("text", `String "world")] };
    { id = "3"; vector = [| 0.9; 0.8; 0.7; 0.6 |]; payload = [("text", `String "different")] };
  ] in

  let* upsert_result = upsert ~collection:collection_name ~points () in
  (match upsert_result with
   | Ok n -> Printf.printf "✅ Inserted %d points\n" n
   | Error e -> print_endline ("❌ " ^ error_to_string e));

  (* Search *)
  let query_vector = [| 0.15; 0.25; 0.35; 0.45 |] in
  let* search_result = search ~collection:collection_name ~vector:query_vector ~limit:2 () in
  (match search_result with
   | Ok results ->
     Printf.printf "🔍 Search results: %d\n" (List.length results);
     List.iter (fun r ->
       Printf.printf "  - id=%s score=%.4f\n" r.id r.score
     ) results
   | Error e ->
     print_endline ("❌ " ^ error_to_string e));

  (* Cleanup *)
  let* _ = delete_collection ~name:collection_name () in
  print_endline "🧹 Cleaned up test collection";

  Lwt.return ()
end
