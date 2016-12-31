module OCaml402Config = struct
  let ast_impl_magic_number = "Caml1999M016"
  let ast_intf_magic_number = "Caml1999N015"
end

module OCaml403Config = struct
  let ast_impl_magic_number = "Caml1999M019"
  let ast_intf_magic_number = "Caml1999N018"
end

module OCaml404Config = struct
  let ast_impl_magic_number = "Caml1999M020"
  let ast_intf_magic_number = "Caml1999N018"
end

type filename = string

type tree =
  | OCaml402_intf of OCamlFrontend402.Parsetree.signature
  | OCaml402_impl of OCamlFrontend402.Parsetree.structure
  | OCaml403_intf of OCamlFrontend403.Parsetree.signature
  | OCaml403_impl of OCamlFrontend403.Parsetree.structure
  | OCaml404_intf of OCamlFrontend404.Parsetree.signature
  | OCaml404_impl of OCamlFrontend404.Parsetree.structure

let magics = [
  OCaml402Config.ast_intf_magic_number, (fun x -> OCaml402_intf (Obj.obj x));
  OCaml402Config.ast_impl_magic_number, (fun x -> OCaml402_impl (Obj.obj x));
  OCaml403Config.ast_intf_magic_number, (fun x -> OCaml403_intf (Obj.obj x));
  OCaml403Config.ast_impl_magic_number, (fun x -> OCaml403_impl (Obj.obj x));
  OCaml404Config.ast_intf_magic_number, (fun x -> OCaml404_intf (Obj.obj x));
  OCaml404Config.ast_impl_magic_number, (fun x -> OCaml404_impl (Obj.obj x));
]

let magic_number = function
  | OCaml402_intf _ -> OCaml402Config.ast_intf_magic_number
  | OCaml402_impl _ -> OCaml402Config.ast_impl_magic_number
  | OCaml403_intf _ -> OCaml403Config.ast_intf_magic_number
  | OCaml403_impl _ -> OCaml403Config.ast_impl_magic_number
  | OCaml404_intf _ -> OCaml404Config.ast_intf_magic_number
  | OCaml404_impl _ -> OCaml404Config.ast_impl_magic_number

let payload = function
  | OCaml402_intf x -> Obj.repr x
  | OCaml402_impl x -> Obj.repr x
  | OCaml403_intf x -> Obj.repr x
  | OCaml403_impl x -> Obj.repr x
  | OCaml404_intf x -> Obj.repr x
  | OCaml404_impl x -> Obj.repr x

let magic_length = String.length OCaml402Config.ast_impl_magic_number

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
