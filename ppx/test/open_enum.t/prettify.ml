(* [@json.open_enum] marks the catch-all constructor. The constructor's
   argument must be a record (inline on sum types, named on polyvariants)
   with fields { tag : string; payload : Melange_json.t list option }. *)

(* Sum type: inline record form. *)
type sum_enum =
  | Alpha [@json.name "alpha"]
  | Beta of int [@json.name "beta"]
  | Other of { tag : string; payload : Melange_json.t list option }
    [@json.open_enum]
[@@deriving json] [@@json.compact_variants]

(* Polyvariant: named record form (inline records aren't allowed on
   polyvariants). The user defines the record with the standard fields
   in the same recursive group via `and`, so the polyvariant can
   reference it. *)
type poly_enum = [
  | `Alpha [@json.name "alpha"]
  | `Beta of int [@json.name "beta"]
  | `Other of poly_open_enum [@json.open_enum]
] [@@json.compact_variants]
and poly_open_enum = { tag : string; payload : Melange_json.t list option }
[@@deriving json]

let pp = function
  | Alpha -> "Alpha"
  | Beta n -> Printf.sprintf "Beta(%d)" n
  | Other { tag; payload = None } -> Printf.sprintf "Other(%s,bare)" tag
  | Other { tag; payload = Some xs } ->
      Printf.sprintf "Other(%s,[%s])" tag
        (String.concat ";" (List.map Yojson.Basic.to_string xs))

let pp_poly (v : poly_enum) = match v with
  | `Alpha -> "Alpha"
  | `Beta n -> Printf.sprintf "Beta(%d)" n
  | `Other { tag; payload = None } -> Printf.sprintf "Other(%s,bare)" tag
  | `Other { tag; payload = Some xs } ->
      Printf.sprintf "Other(%s,[%s])" tag
        (String.concat ";" (List.map Yojson.Basic.to_string xs))

let () =
  let json = Yojson.Basic.from_string Sys.argv.(1) in
  let kind = Sys.argv.(2) in
  let in_s = Yojson.Basic.to_string json in
  match kind with
  | "sum" ->
      let v = sum_enum_of_json json in
      Printf.printf "got %s\n" (pp v);
      Printf.printf "round-trip %s -> %s\n" in_s
        (Yojson.Basic.to_string (sum_enum_to_json v))
  | "poly" ->
      let v = poly_enum_of_json json in
      Printf.printf "got %s\n" (pp_poly v);
      Printf.printf "round-trip %s -> %s\n" in_s
        (Yojson.Basic.to_string (poly_enum_to_json v))
  | _ -> failwith "kind must be sum or poly"
