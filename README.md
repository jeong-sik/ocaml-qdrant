# ocaml-qdrant

Pure OCaml client for [Qdrant](https://qdrant.tech/) vector database.

## Features

- **REST API** client (no gRPC dependency)
- Collection management (create, delete, info)
- Point operations (upsert, delete, get)
- Vector search with filters
- Async I/O with Lwt

## Install

```bash
opam install qdrant
```

## Usage

```ocaml
let () = Lwt_main.run begin
  let open Lwt.Syntax in
  let open Qdrant in

  (* Create collection *)
  let* _ = create_collection
    ~name:"my_collection"
    ~vector_config:{ size = 1024; distance = Cosine }
    () in

  (* Insert points *)
  let* _ = upsert ~collection:"my_collection" ~points:[
    { id = "1"; vector = my_embedding; payload = [("text", `String "hello")] }
  ] () in

  (* Search *)
  let* results = search
    ~collection:"my_collection"
    ~vector:query_embedding
    ~limit:10
    () in

  Lwt.return ()
end
```

## Config

```bash
export QDRANT_URL="http://localhost:6333"
export QDRANT_API_KEY="your-api-key"  # optional
```

## API

- `health` - Check server status
- `list_collections` - List all collections
- `create_collection` - Create with vector config
- `delete_collection` - Delete collection
- `get_collection` - Get collection info
- `upsert` - Insert/update points
- `delete_points` - Delete by IDs
- `search` - Vector similarity search
- `search_with_filter` - Search with payload filter
- `scroll` - Paginate through points
- `count` - Count points

## License

MIT
