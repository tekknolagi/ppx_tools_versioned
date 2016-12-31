open Migrate_parsetree_def

type filename = string

let magics = [
  OCaml402.ast_intf_magic_number,
  (fun x -> OCaml402 (OCaml402.Intf (Obj.obj x)));
  OCaml402.ast_impl_magic_number,
  (fun x -> OCaml402 (OCaml402.Impl (Obj.obj x)));
  OCaml403.ast_intf_magic_number,
  (fun x -> OCaml403 (OCaml403.Intf (Obj.obj x)));
  OCaml403.ast_impl_magic_number,
  (fun x -> OCaml403 (OCaml403.Impl (Obj.obj x)));
  OCaml404.ast_intf_magic_number,
  (fun x -> OCaml404 (OCaml404.Intf (Obj.obj x)));
  OCaml404.ast_impl_magic_number,
  (fun x -> OCaml404 (OCaml404.Impl (Obj.obj x)));
]

let magic_number = function
  | OCaml402 (OCaml402.Intf _) -> OCaml402.ast_intf_magic_number
  | OCaml402 (OCaml402.Impl _) -> OCaml402.ast_impl_magic_number
  | OCaml403 (OCaml403.Intf _) -> OCaml403.ast_intf_magic_number
  | OCaml403 (OCaml403.Impl _) -> OCaml403.ast_impl_magic_number
  | OCaml404 (OCaml404.Intf _) -> OCaml404.ast_intf_magic_number
  | OCaml404 (OCaml404.Impl _) -> OCaml404.ast_impl_magic_number

let payload = function
  | OCaml402 (OCaml402.Intf x) -> Obj.repr x
  | OCaml402 (OCaml402.Impl x) -> Obj.repr x
  | OCaml403 (OCaml403.Intf x) -> Obj.repr x
  | OCaml403 (OCaml403.Impl x) -> Obj.repr x
  | OCaml404 (OCaml404.Intf x) -> Obj.repr x
  | OCaml404 (OCaml404.Impl x) -> Obj.repr x

let magic_length = String.length OCaml402.ast_impl_magic_number

let read_magic ic = really_input_string ic magic_length

exception Unknown_magic_number of string

let from_channel ic =
  let magic = read_magic ic in
  match List.assoc magic magics with
  | inj ->
      let filename : filename = input_value ic in
      let payload = inj (input_value ic) in
      (filename, payload)
  | exception Not_found -> raise (Unknown_magic_number magic)

let from_bytes bytes pos =
  let len = min magic_length (Bytes.length bytes - pos) in
  let magic = Bytes.to_string (Bytes.sub bytes pos len) in
  match List.assoc magic magics with
  | inj ->
      let filename_pos = pos + magic_length in
      let filename : filename = Marshal.from_bytes bytes filename_pos in
      let payload_pos = filename_pos + Marshal.total_size bytes filename_pos in
      let payload = inj (Marshal.from_bytes bytes payload_pos) in
      (filename, payload)
  | exception Not_found -> raise (Unknown_magic_number magic)

let to_channel oc (filename : filename) x =
  output_string oc (magic_number x);
  output_value oc filename;
  output_value oc x

let to_bytes (filename : filename) x =
  Bytes.cat (
    Bytes.cat
      (Bytes.of_string (magic_number x))
      (Marshal.to_bytes filename [])
  ) (Marshal.to_bytes (payload x) [])

let tree_version = function
  | OCaml402 _ -> `OCaml402
  | OCaml403 _ -> `OCaml403
  | OCaml404 _ -> `OCaml404

let migrate_to_402 = function
  | OCaml402 x -> x
  | OCaml403 (OCaml403.Impl x) ->
      OCaml402.Impl
        (Migrate_parsetree_403_402.copy_OCamlFrontend403_Parsetree_structure x)
  | OCaml403 (OCaml403.Intf x) ->
      OCaml402.Intf
        (Migrate_parsetree_403_402.copy_OCamlFrontend403_Parsetree_signature x)
  | OCaml404 (OCaml404.Impl x) ->
      OCaml402.Impl
        (Migrate_parsetree_403_402.copy_OCamlFrontend403_Parsetree_structure
           (Migrate_parsetree_404_403.copy_OCamlFrontend404_Parsetree_structure x))
  | OCaml404 (OCaml404.Intf x) ->
      OCaml402.Intf
        (Migrate_parsetree_403_402.copy_OCamlFrontend403_Parsetree_signature
           (Migrate_parsetree_404_403.copy_OCamlFrontend404_Parsetree_signature x))

let migrate_to_403 = function
  | OCaml403 x -> x
  | OCaml402 (OCaml402.Impl x) ->
      OCaml403.Impl
        (Migrate_parsetree_402_403.copy_OCamlFrontend402_Parsetree_structure x)
  | OCaml402 (OCaml402.Intf x) ->
      OCaml403.Intf
        (Migrate_parsetree_402_403.copy_OCamlFrontend402_Parsetree_signature x)
  | OCaml404 (OCaml404.Impl x) ->
      OCaml403.Impl
        (Migrate_parsetree_404_403.copy_OCamlFrontend404_Parsetree_structure x)
  | OCaml404 (OCaml404.Intf x) ->
      OCaml403.Intf
        (Migrate_parsetree_404_403.copy_OCamlFrontend404_Parsetree_signature x)

let migrate_to_404 = function
  | OCaml404 x -> x
  | OCaml402 (OCaml402.Impl x) ->
      OCaml404.Impl
        (Migrate_parsetree_403_404.copy_OCamlFrontend403_Parsetree_structure
           (Migrate_parsetree_402_403.copy_OCamlFrontend402_Parsetree_structure x))
  | OCaml402 (OCaml402.Intf x) ->
      OCaml404.Intf
        (Migrate_parsetree_403_404.copy_OCamlFrontend403_Parsetree_signature
           (Migrate_parsetree_402_403.copy_OCamlFrontend402_Parsetree_signature x))
  | OCaml403 (OCaml403.Impl x) ->
      OCaml404.Impl
        (Migrate_parsetree_403_404.copy_OCamlFrontend403_Parsetree_structure x)
  | OCaml403 (OCaml403.Intf x) ->
      OCaml404.Intf
        (Migrate_parsetree_403_404.copy_OCamlFrontend403_Parsetree_signature x)

let migrate_to_version tree = function
  | `OCaml402 -> OCaml402 (migrate_to_402 tree)
  | `OCaml403 -> OCaml403 (migrate_to_403 tree)
  | `OCaml404 -> OCaml404 (migrate_to_404 tree)
