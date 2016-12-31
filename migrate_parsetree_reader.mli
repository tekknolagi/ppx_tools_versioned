module OCaml402 : sig
  val ast_impl_magic_number : string
  val ast_intf_magic_number : string
  type tree =
    | Intf of OCamlFrontend402.Parsetree.signature
    | Impl of OCamlFrontend402.Parsetree.structure
end

module OCaml403 : sig
  val ast_impl_magic_number : string
  val ast_intf_magic_number : string
  type tree =
    | Intf of OCamlFrontend403.Parsetree.signature
    | Impl of OCamlFrontend403.Parsetree.structure
end

module OCaml404 : sig
  val ast_impl_magic_number : string
  val ast_intf_magic_number : string
  type tree =
    | Intf of OCamlFrontend404.Parsetree.signature
    | Impl of OCamlFrontend404.Parsetree.structure
end

type filename = string

type tree =
  | OCaml402 of OCaml402.tree
  | OCaml403 of OCaml403.tree
  | OCaml404 of OCaml404.tree

exception Unknown_magic_number of string
val from_channel : in_channel -> filename * tree
val from_bytes : bytes -> int -> filename * tree

val to_channel : out_channel -> filename -> tree -> unit
val to_bytes : filename -> tree -> bytes

val tree_version : tree -> [> `OCaml402 | `OCaml403 | `OCaml404 ]
val migrate_to_402 : tree -> OCaml402.tree
val migrate_to_403 : tree -> OCaml403.tree
val migrate_to_404 : tree -> OCaml404.tree
val migrate_to_version : tree -> [< `OCaml402 | `OCaml403 | `OCaml404 ] -> tree
