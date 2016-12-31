module OCaml402Config : sig
  val ast_impl_magic_number : string
  val ast_intf_magic_number : string
end

module OCaml403Config : sig
  val ast_impl_magic_number : string
  val ast_intf_magic_number : string
end

module OCaml404Config : sig
  val ast_impl_magic_number : string
  val ast_intf_magic_number : string
end

type filename = string

type tree =
  | OCaml402_intf of OCamlFrontend402.Parsetree.signature
  | OCaml402_impl of OCamlFrontend402.Parsetree.structure
  | OCaml403_intf of OCamlFrontend403.Parsetree.signature
  | OCaml403_impl of OCamlFrontend403.Parsetree.structure
  | OCaml404_intf of OCamlFrontend404.Parsetree.signature
  | OCaml404_impl of OCamlFrontend404.Parsetree.structure

exception Unknown_magic_number of string
val from_channel : in_channel -> filename * tree
val from_bytes : bytes -> int -> filename * tree

val to_channel : out_channel -> filename -> tree -> unit
val to_bytes : filename -> tree -> bytes
