open Migrate_parsetree_def

type filename = string

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
