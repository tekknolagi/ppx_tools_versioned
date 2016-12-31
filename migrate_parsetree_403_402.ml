let migration_error = Migrate_parsetree_def.migration_error

let rec copy_OCamlFrontend403_Parsetree_expression :
  OCamlFrontend403.Parsetree.expression ->
    OCamlFrontend402.Parsetree.expression
  =
  fun
    { OCamlFrontend403.Parsetree.pexp_desc = pexp_desc;
      OCamlFrontend403.Parsetree.pexp_loc = pexp_loc;
      OCamlFrontend403.Parsetree.pexp_attributes = pexp_attributes }
     ->
    {
      OCamlFrontend402.Parsetree.pexp_desc =
        (copy_OCamlFrontend403_Parsetree_expression_desc pexp_desc);
      OCamlFrontend402.Parsetree.pexp_loc =
        (copy_OCamlFrontend403_Location_t pexp_loc);
      OCamlFrontend402.Parsetree.pexp_attributes =
        (copy_OCamlFrontend403_Parsetree_attributes pexp_attributes)
    }

and copy_OCamlFrontend403_Parsetree_expression_desc :
  OCamlFrontend403.Parsetree.expression_desc ->
    OCamlFrontend402.Parsetree.expression_desc
  =
  function
  | OCamlFrontend403.Parsetree.Pexp_ident x0 ->
      OCamlFrontend402.Parsetree.Pexp_ident
        (copy_OCamlFrontend403_Asttypes_loc copy_OCamlFrontend403_Longident_t
           x0)
  | OCamlFrontend403.Parsetree.Pexp_constant x0 ->
      OCamlFrontend402.Parsetree.Pexp_constant
        (copy_OCamlFrontend403_Parsetree_constant x0)
  | OCamlFrontend403.Parsetree.Pexp_let (x0,x1,x2) ->
      OCamlFrontend402.Parsetree.Pexp_let
        ((copy_OCamlFrontend403_Asttypes_rec_flag x0),
          (List.map copy_OCamlFrontend403_Parsetree_value_binding x1),
          (copy_OCamlFrontend403_Parsetree_expression x2))
  | OCamlFrontend403.Parsetree.Pexp_function x0 ->
      OCamlFrontend402.Parsetree.Pexp_function
        (List.map copy_OCamlFrontend403_Parsetree_case x0)
  | OCamlFrontend403.Parsetree.Pexp_fun (x0,x1,x2,x3) ->
      OCamlFrontend402.Parsetree.Pexp_fun
        ((copy_OCamlFrontend403_Asttypes_arg_label x0),
          (copy_option copy_OCamlFrontend403_Parsetree_expression x1),
          (copy_OCamlFrontend403_Parsetree_pattern x2),
          (copy_OCamlFrontend403_Parsetree_expression x3))
  | OCamlFrontend403.Parsetree.Pexp_apply (x0,x1) ->
      OCamlFrontend402.Parsetree.Pexp_apply
        ((copy_OCamlFrontend403_Parsetree_expression x0),
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_OCamlFrontend403_Asttypes_arg_label x0),
                  (copy_OCamlFrontend403_Parsetree_expression x1))) x1))
  | OCamlFrontend403.Parsetree.Pexp_match (x0,x1) ->
      OCamlFrontend402.Parsetree.Pexp_match
        ((copy_OCamlFrontend403_Parsetree_expression x0),
          (List.map copy_OCamlFrontend403_Parsetree_case x1))
  | OCamlFrontend403.Parsetree.Pexp_try (x0,x1) ->
      OCamlFrontend402.Parsetree.Pexp_try
        ((copy_OCamlFrontend403_Parsetree_expression x0),
          (List.map copy_OCamlFrontend403_Parsetree_case x1))
  | OCamlFrontend403.Parsetree.Pexp_tuple x0 ->
      OCamlFrontend402.Parsetree.Pexp_tuple
        (List.map copy_OCamlFrontend403_Parsetree_expression x0)
  | OCamlFrontend403.Parsetree.Pexp_construct (x0,x1) ->
      OCamlFrontend402.Parsetree.Pexp_construct
        ((copy_OCamlFrontend403_Asttypes_loc
            copy_OCamlFrontend403_Longident_t x0),
          (copy_option copy_OCamlFrontend403_Parsetree_expression x1))
  | OCamlFrontend403.Parsetree.Pexp_variant (x0,x1) ->
      OCamlFrontend402.Parsetree.Pexp_variant
        ((copy_OCamlFrontend403_Asttypes_label x0),
          (copy_option copy_OCamlFrontend403_Parsetree_expression x1))
  | OCamlFrontend403.Parsetree.Pexp_record (x0,x1) ->
      OCamlFrontend402.Parsetree.Pexp_record
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               ((copy_OCamlFrontend403_Asttypes_loc
                   copy_OCamlFrontend403_Longident_t x0),
                 (copy_OCamlFrontend403_Parsetree_expression x1))) x0),
          (copy_option copy_OCamlFrontend403_Parsetree_expression x1))
  | OCamlFrontend403.Parsetree.Pexp_field (x0,x1) ->
      OCamlFrontend402.Parsetree.Pexp_field
        ((copy_OCamlFrontend403_Parsetree_expression x0),
          (copy_OCamlFrontend403_Asttypes_loc
             copy_OCamlFrontend403_Longident_t x1))
  | OCamlFrontend403.Parsetree.Pexp_setfield (x0,x1,x2) ->
      OCamlFrontend402.Parsetree.Pexp_setfield
        ((copy_OCamlFrontend403_Parsetree_expression x0),
          (copy_OCamlFrontend403_Asttypes_loc
             copy_OCamlFrontend403_Longident_t x1),
          (copy_OCamlFrontend403_Parsetree_expression x2))
  | OCamlFrontend403.Parsetree.Pexp_array x0 ->
      OCamlFrontend402.Parsetree.Pexp_array
        (List.map copy_OCamlFrontend403_Parsetree_expression x0)
  | OCamlFrontend403.Parsetree.Pexp_ifthenelse (x0,x1,x2) ->
      OCamlFrontend402.Parsetree.Pexp_ifthenelse
        ((copy_OCamlFrontend403_Parsetree_expression x0),
          (copy_OCamlFrontend403_Parsetree_expression x1),
          (copy_option copy_OCamlFrontend403_Parsetree_expression x2))
  | OCamlFrontend403.Parsetree.Pexp_sequence (x0,x1) ->
      OCamlFrontend402.Parsetree.Pexp_sequence
        ((copy_OCamlFrontend403_Parsetree_expression x0),
          (copy_OCamlFrontend403_Parsetree_expression x1))
  | OCamlFrontend403.Parsetree.Pexp_while (x0,x1) ->
      OCamlFrontend402.Parsetree.Pexp_while
        ((copy_OCamlFrontend403_Parsetree_expression x0),
          (copy_OCamlFrontend403_Parsetree_expression x1))
  | OCamlFrontend403.Parsetree.Pexp_for (x0,x1,x2,x3,x4) ->
      OCamlFrontend402.Parsetree.Pexp_for
        ((copy_OCamlFrontend403_Parsetree_pattern x0),
          (copy_OCamlFrontend403_Parsetree_expression x1),
          (copy_OCamlFrontend403_Parsetree_expression x2),
          (copy_OCamlFrontend403_Asttypes_direction_flag x3),
          (copy_OCamlFrontend403_Parsetree_expression x4))
  | OCamlFrontend403.Parsetree.Pexp_constraint (x0,x1) ->
      OCamlFrontend402.Parsetree.Pexp_constraint
        ((copy_OCamlFrontend403_Parsetree_expression x0),
          (copy_OCamlFrontend403_Parsetree_core_type x1))
  | OCamlFrontend403.Parsetree.Pexp_coerce (x0,x1,x2) ->
      OCamlFrontend402.Parsetree.Pexp_coerce
        ((copy_OCamlFrontend403_Parsetree_expression x0),
          (copy_option copy_OCamlFrontend403_Parsetree_core_type x1),
          (copy_OCamlFrontend403_Parsetree_core_type x2))
  | OCamlFrontend403.Parsetree.Pexp_send (x0,x1) ->
      OCamlFrontend402.Parsetree.Pexp_send
        ((copy_OCamlFrontend403_Parsetree_expression x0), x1)
  | OCamlFrontend403.Parsetree.Pexp_new x0 ->
      OCamlFrontend402.Parsetree.Pexp_new
        (copy_OCamlFrontend403_Asttypes_loc copy_OCamlFrontend403_Longident_t
           x0)
  | OCamlFrontend403.Parsetree.Pexp_setinstvar (x0,x1) ->
      OCamlFrontend402.Parsetree.Pexp_setinstvar
        ((copy_OCamlFrontend403_Asttypes_loc (fun x  -> x) x0),
          (copy_OCamlFrontend403_Parsetree_expression x1))
  | OCamlFrontend403.Parsetree.Pexp_override x0 ->
      OCamlFrontend402.Parsetree.Pexp_override
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_OCamlFrontend403_Asttypes_loc (fun x  -> x) x0),
                (copy_OCamlFrontend403_Parsetree_expression x1))) x0)
  | OCamlFrontend403.Parsetree.Pexp_letmodule (x0,x1,x2) ->
      OCamlFrontend402.Parsetree.Pexp_letmodule
        ((copy_OCamlFrontend403_Asttypes_loc (fun x  -> x) x0),
          (copy_OCamlFrontend403_Parsetree_module_expr x1),
          (copy_OCamlFrontend403_Parsetree_expression x2))
  | OCamlFrontend403.Parsetree.Pexp_assert x0 ->
      OCamlFrontend402.Parsetree.Pexp_assert
        (copy_OCamlFrontend403_Parsetree_expression x0)
  | OCamlFrontend403.Parsetree.Pexp_lazy x0 ->
      OCamlFrontend402.Parsetree.Pexp_lazy
        (copy_OCamlFrontend403_Parsetree_expression x0)
  | OCamlFrontend403.Parsetree.Pexp_poly (x0,x1) ->
      OCamlFrontend402.Parsetree.Pexp_poly
        ((copy_OCamlFrontend403_Parsetree_expression x0),
          (copy_option copy_OCamlFrontend403_Parsetree_core_type x1))
  | OCamlFrontend403.Parsetree.Pexp_object x0 ->
      OCamlFrontend402.Parsetree.Pexp_object
        (copy_OCamlFrontend403_Parsetree_class_structure x0)
  | OCamlFrontend403.Parsetree.Pexp_newtype (x0,x1) ->
      OCamlFrontend402.Parsetree.Pexp_newtype
        (x0, (copy_OCamlFrontend403_Parsetree_expression x1))
  | OCamlFrontend403.Parsetree.Pexp_pack x0 ->
      OCamlFrontend402.Parsetree.Pexp_pack
        (copy_OCamlFrontend403_Parsetree_module_expr x0)
  | OCamlFrontend403.Parsetree.Pexp_open (x0,x1,x2) ->
      OCamlFrontend402.Parsetree.Pexp_open
        ((copy_OCamlFrontend403_Asttypes_override_flag x0),
          (copy_OCamlFrontend403_Asttypes_loc
             copy_OCamlFrontend403_Longident_t x1),
          (copy_OCamlFrontend403_Parsetree_expression x2))
  | OCamlFrontend403.Parsetree.Pexp_extension x0 ->
      OCamlFrontend402.Parsetree.Pexp_extension
        (copy_OCamlFrontend403_Parsetree_extension x0)
  | OCamlFrontend403.Parsetree.Pexp_unreachable  ->
      migration_error `Pexp_unreachable

and copy_OCamlFrontend403_Asttypes_direction_flag :
  OCamlFrontend403.Asttypes.direction_flag ->
    OCamlFrontend402.Asttypes.direction_flag
  =
  function
  | OCamlFrontend403.Asttypes.Upto  -> OCamlFrontend402.Asttypes.Upto
  | OCamlFrontend403.Asttypes.Downto  -> OCamlFrontend402.Asttypes.Downto

and copy_OCamlFrontend403_Parsetree_case :
  OCamlFrontend403.Parsetree.case -> OCamlFrontend402.Parsetree.case =
  fun
    { OCamlFrontend403.Parsetree.pc_lhs = pc_lhs;
      OCamlFrontend403.Parsetree.pc_guard = pc_guard;
      OCamlFrontend403.Parsetree.pc_rhs = pc_rhs }
     ->
    {
      OCamlFrontend402.Parsetree.pc_lhs =
        (copy_OCamlFrontend403_Parsetree_pattern pc_lhs);
      OCamlFrontend402.Parsetree.pc_guard =
        (copy_option copy_OCamlFrontend403_Parsetree_expression pc_guard);
      OCamlFrontend402.Parsetree.pc_rhs =
        (copy_OCamlFrontend403_Parsetree_expression pc_rhs)
    }

and copy_OCamlFrontend403_Parsetree_value_binding :
  OCamlFrontend403.Parsetree.value_binding ->
    OCamlFrontend402.Parsetree.value_binding
  =
  fun
    { OCamlFrontend403.Parsetree.pvb_pat = pvb_pat;
      OCamlFrontend403.Parsetree.pvb_expr = pvb_expr;
      OCamlFrontend403.Parsetree.pvb_attributes = pvb_attributes;
      OCamlFrontend403.Parsetree.pvb_loc = pvb_loc }
     ->
    {
      OCamlFrontend402.Parsetree.pvb_pat =
        (copy_OCamlFrontend403_Parsetree_pattern pvb_pat);
      OCamlFrontend402.Parsetree.pvb_expr =
        (copy_OCamlFrontend403_Parsetree_expression pvb_expr);
      OCamlFrontend402.Parsetree.pvb_attributes =
        (copy_OCamlFrontend403_Parsetree_attributes pvb_attributes);
      OCamlFrontend402.Parsetree.pvb_loc =
        (copy_OCamlFrontend403_Location_t pvb_loc)
    }

and copy_OCamlFrontend403_Parsetree_pattern :
  OCamlFrontend403.Parsetree.pattern -> OCamlFrontend402.Parsetree.pattern =
  fun
    { OCamlFrontend403.Parsetree.ppat_desc = ppat_desc;
      OCamlFrontend403.Parsetree.ppat_loc = ppat_loc;
      OCamlFrontend403.Parsetree.ppat_attributes = ppat_attributes }
     ->
    {
      OCamlFrontend402.Parsetree.ppat_desc =
        (copy_OCamlFrontend403_Parsetree_pattern_desc ppat_desc);
      OCamlFrontend402.Parsetree.ppat_loc =
        (copy_OCamlFrontend403_Location_t ppat_loc);
      OCamlFrontend402.Parsetree.ppat_attributes =
        (copy_OCamlFrontend403_Parsetree_attributes ppat_attributes)
    }

and copy_OCamlFrontend403_Parsetree_pattern_desc :
  OCamlFrontend403.Parsetree.pattern_desc ->
    OCamlFrontend402.Parsetree.pattern_desc
  =
  function
  | OCamlFrontend403.Parsetree.Ppat_any  ->
      OCamlFrontend402.Parsetree.Ppat_any
  | OCamlFrontend403.Parsetree.Ppat_var x0 ->
      OCamlFrontend402.Parsetree.Ppat_var
        (copy_OCamlFrontend403_Asttypes_loc (fun x  -> x) x0)
  | OCamlFrontend403.Parsetree.Ppat_alias (x0,x1) ->
      OCamlFrontend402.Parsetree.Ppat_alias
        ((copy_OCamlFrontend403_Parsetree_pattern x0),
          (copy_OCamlFrontend403_Asttypes_loc (fun x  -> x) x1))
  | OCamlFrontend403.Parsetree.Ppat_constant x0 ->
      OCamlFrontend402.Parsetree.Ppat_constant
        (copy_OCamlFrontend403_Parsetree_constant x0)
  | OCamlFrontend403.Parsetree.Ppat_interval (x0,x1) ->
      OCamlFrontend402.Parsetree.Ppat_interval
        ((copy_OCamlFrontend403_Parsetree_constant x0),
          (copy_OCamlFrontend403_Parsetree_constant x1))
  | OCamlFrontend403.Parsetree.Ppat_tuple x0 ->
      OCamlFrontend402.Parsetree.Ppat_tuple
        (List.map copy_OCamlFrontend403_Parsetree_pattern x0)
  | OCamlFrontend403.Parsetree.Ppat_construct (x0,x1) ->
      OCamlFrontend402.Parsetree.Ppat_construct
        ((copy_OCamlFrontend403_Asttypes_loc
            copy_OCamlFrontend403_Longident_t x0),
          (copy_option copy_OCamlFrontend403_Parsetree_pattern x1))
  | OCamlFrontend403.Parsetree.Ppat_variant (x0,x1) ->
      OCamlFrontend402.Parsetree.Ppat_variant
        ((copy_OCamlFrontend403_Asttypes_label x0),
          (copy_option copy_OCamlFrontend403_Parsetree_pattern x1))
  | OCamlFrontend403.Parsetree.Ppat_record (x0,x1) ->
      OCamlFrontend402.Parsetree.Ppat_record
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               ((copy_OCamlFrontend403_Asttypes_loc
                   copy_OCamlFrontend403_Longident_t x0),
                 (copy_OCamlFrontend403_Parsetree_pattern x1))) x0),
          (copy_OCamlFrontend403_Asttypes_closed_flag x1))
  | OCamlFrontend403.Parsetree.Ppat_array x0 ->
      OCamlFrontend402.Parsetree.Ppat_array
        (List.map copy_OCamlFrontend403_Parsetree_pattern x0)
  | OCamlFrontend403.Parsetree.Ppat_or (x0,x1) ->
      OCamlFrontend402.Parsetree.Ppat_or
        ((copy_OCamlFrontend403_Parsetree_pattern x0),
          (copy_OCamlFrontend403_Parsetree_pattern x1))
  | OCamlFrontend403.Parsetree.Ppat_constraint (x0,x1) ->
      OCamlFrontend402.Parsetree.Ppat_constraint
        ((copy_OCamlFrontend403_Parsetree_pattern x0),
          (copy_OCamlFrontend403_Parsetree_core_type x1))
  | OCamlFrontend403.Parsetree.Ppat_type x0 ->
      OCamlFrontend402.Parsetree.Ppat_type
        (copy_OCamlFrontend403_Asttypes_loc copy_OCamlFrontend403_Longident_t
           x0)
  | OCamlFrontend403.Parsetree.Ppat_lazy x0 ->
      OCamlFrontend402.Parsetree.Ppat_lazy
        (copy_OCamlFrontend403_Parsetree_pattern x0)
  | OCamlFrontend403.Parsetree.Ppat_unpack x0 ->
      OCamlFrontend402.Parsetree.Ppat_unpack
        (copy_OCamlFrontend403_Asttypes_loc (fun x  -> x) x0)
  | OCamlFrontend403.Parsetree.Ppat_exception x0 ->
      OCamlFrontend402.Parsetree.Ppat_exception
        (copy_OCamlFrontend403_Parsetree_pattern x0)
  | OCamlFrontend403.Parsetree.Ppat_extension x0 ->
      OCamlFrontend402.Parsetree.Ppat_extension
        (copy_OCamlFrontend403_Parsetree_extension x0)

and copy_OCamlFrontend403_Parsetree_core_type :
  OCamlFrontend403.Parsetree.core_type ->
    OCamlFrontend402.Parsetree.core_type
  =
  fun
    { OCamlFrontend403.Parsetree.ptyp_desc = ptyp_desc;
      OCamlFrontend403.Parsetree.ptyp_loc = ptyp_loc;
      OCamlFrontend403.Parsetree.ptyp_attributes = ptyp_attributes }
     ->
    {
      OCamlFrontend402.Parsetree.ptyp_desc =
        (copy_OCamlFrontend403_Parsetree_core_type_desc ptyp_desc);
      OCamlFrontend402.Parsetree.ptyp_loc =
        (copy_OCamlFrontend403_Location_t ptyp_loc);
      OCamlFrontend402.Parsetree.ptyp_attributes =
        (copy_OCamlFrontend403_Parsetree_attributes ptyp_attributes)
    }

and copy_OCamlFrontend403_Parsetree_core_type_desc :
  OCamlFrontend403.Parsetree.core_type_desc ->
    OCamlFrontend402.Parsetree.core_type_desc
  =
  function
  | OCamlFrontend403.Parsetree.Ptyp_any  ->
      OCamlFrontend402.Parsetree.Ptyp_any
  | OCamlFrontend403.Parsetree.Ptyp_var x0 ->
      OCamlFrontend402.Parsetree.Ptyp_var x0
  | OCamlFrontend403.Parsetree.Ptyp_arrow (x0,x1,x2) ->
      OCamlFrontend402.Parsetree.Ptyp_arrow
        ((copy_OCamlFrontend403_Asttypes_arg_label x0),
          (copy_OCamlFrontend403_Parsetree_core_type x1),
          (copy_OCamlFrontend403_Parsetree_core_type x2))
  | OCamlFrontend403.Parsetree.Ptyp_tuple x0 ->
      OCamlFrontend402.Parsetree.Ptyp_tuple
        (List.map copy_OCamlFrontend403_Parsetree_core_type x0)
  | OCamlFrontend403.Parsetree.Ptyp_constr (x0,x1) ->
      OCamlFrontend402.Parsetree.Ptyp_constr
        ((copy_OCamlFrontend403_Asttypes_loc
            copy_OCamlFrontend403_Longident_t x0),
          (List.map copy_OCamlFrontend403_Parsetree_core_type x1))
  | OCamlFrontend403.Parsetree.Ptyp_object (x0,x1) ->
      OCamlFrontend402.Parsetree.Ptyp_object
        ((List.map
            (fun x  ->
               let (x0,x1,x2) = x  in
               (x0, (copy_OCamlFrontend403_Parsetree_attributes x1),
                 (copy_OCamlFrontend403_Parsetree_core_type x2))) x0),
          (copy_OCamlFrontend403_Asttypes_closed_flag x1))
  | OCamlFrontend403.Parsetree.Ptyp_class (x0,x1) ->
      OCamlFrontend402.Parsetree.Ptyp_class
        ((copy_OCamlFrontend403_Asttypes_loc
            copy_OCamlFrontend403_Longident_t x0),
          (List.map copy_OCamlFrontend403_Parsetree_core_type x1))
  | OCamlFrontend403.Parsetree.Ptyp_alias (x0,x1) ->
      OCamlFrontend402.Parsetree.Ptyp_alias
        ((copy_OCamlFrontend403_Parsetree_core_type x0), x1)
  | OCamlFrontend403.Parsetree.Ptyp_variant (x0,x1,x2) ->
      OCamlFrontend402.Parsetree.Ptyp_variant
        ((List.map copy_OCamlFrontend403_Parsetree_row_field x0),
          (copy_OCamlFrontend403_Asttypes_closed_flag x1),
          (copy_option
             (fun x  -> List.map copy_OCamlFrontend403_Asttypes_label x) x2))
  | OCamlFrontend403.Parsetree.Ptyp_poly (x0,x1) ->
      OCamlFrontend402.Parsetree.Ptyp_poly
        ((List.map (fun x  -> x) x0),
          (copy_OCamlFrontend403_Parsetree_core_type x1))
  | OCamlFrontend403.Parsetree.Ptyp_package x0 ->
      OCamlFrontend402.Parsetree.Ptyp_package
        (copy_OCamlFrontend403_Parsetree_package_type x0)
  | OCamlFrontend403.Parsetree.Ptyp_extension x0 ->
      OCamlFrontend402.Parsetree.Ptyp_extension
        (copy_OCamlFrontend403_Parsetree_extension x0)

and copy_OCamlFrontend403_Parsetree_package_type :
  OCamlFrontend403.Parsetree.package_type ->
    OCamlFrontend402.Parsetree.package_type
  =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_OCamlFrontend403_Asttypes_loc copy_OCamlFrontend403_Longident_t x0),
      (List.map
         (fun x  ->
            let (x0,x1) = x  in
            ((copy_OCamlFrontend403_Asttypes_loc
                copy_OCamlFrontend403_Longident_t x0),
              (copy_OCamlFrontend403_Parsetree_core_type x1))) x1))

and copy_OCamlFrontend403_Parsetree_row_field :
  OCamlFrontend403.Parsetree.row_field ->
    OCamlFrontend402.Parsetree.row_field
  =
  function
  | OCamlFrontend403.Parsetree.Rtag (x0,x1,x2,x3) ->
      OCamlFrontend402.Parsetree.Rtag
        ((copy_OCamlFrontend403_Asttypes_label x0),
          (copy_OCamlFrontend403_Parsetree_attributes x1), (copy_bool x2),
          (List.map copy_OCamlFrontend403_Parsetree_core_type x3))
  | OCamlFrontend403.Parsetree.Rinherit x0 ->
      OCamlFrontend402.Parsetree.Rinherit
        (copy_OCamlFrontend403_Parsetree_core_type x0)

and copy_OCamlFrontend403_Parsetree_attributes :
  OCamlFrontend403.Parsetree.attributes ->
    OCamlFrontend402.Parsetree.attributes
  = fun x  -> List.map copy_OCamlFrontend403_Parsetree_attribute x

and copy_OCamlFrontend403_Parsetree_attribute :
  OCamlFrontend403.Parsetree.attribute ->
    OCamlFrontend402.Parsetree.attribute
  =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_OCamlFrontend403_Asttypes_loc (fun x  -> x) x0),
      (copy_OCamlFrontend403_Parsetree_payload x1))

and copy_OCamlFrontend403_Parsetree_payload :
  OCamlFrontend403.Parsetree.payload -> OCamlFrontend402.Parsetree.payload =
  function
  | OCamlFrontend403.Parsetree.PStr x0 ->
      OCamlFrontend402.Parsetree.PStr
        (copy_OCamlFrontend403_Parsetree_structure x0)
  | OCamlFrontend403.Parsetree.PSig _x0 ->
      migration_error `PSig
  | OCamlFrontend403.Parsetree.PTyp x0 ->
      OCamlFrontend402.Parsetree.PTyp
        (copy_OCamlFrontend403_Parsetree_core_type x0)
  | OCamlFrontend403.Parsetree.PPat (x0,x1) ->
      OCamlFrontend402.Parsetree.PPat
        ((copy_OCamlFrontend403_Parsetree_pattern x0),
          (copy_option copy_OCamlFrontend403_Parsetree_expression x1))

and copy_OCamlFrontend403_Parsetree_structure :
  OCamlFrontend403.Parsetree.structure ->
    OCamlFrontend402.Parsetree.structure
  = fun x  -> List.map copy_OCamlFrontend403_Parsetree_structure_item x

and copy_OCamlFrontend403_Parsetree_structure_item :
  OCamlFrontend403.Parsetree.structure_item ->
    OCamlFrontend402.Parsetree.structure_item
  =
  fun
    { OCamlFrontend403.Parsetree.pstr_desc = pstr_desc;
      OCamlFrontend403.Parsetree.pstr_loc = pstr_loc }
     ->
    {
      OCamlFrontend402.Parsetree.pstr_desc =
        (copy_OCamlFrontend403_Parsetree_structure_item_desc pstr_desc);
      OCamlFrontend402.Parsetree.pstr_loc =
        (copy_OCamlFrontend403_Location_t pstr_loc)
    }

and copy_OCamlFrontend403_Parsetree_structure_item_desc :
  OCamlFrontend403.Parsetree.structure_item_desc ->
    OCamlFrontend402.Parsetree.structure_item_desc
  =
  function
  | OCamlFrontend403.Parsetree.Pstr_eval (x0,x1) ->
      OCamlFrontend402.Parsetree.Pstr_eval
        ((copy_OCamlFrontend403_Parsetree_expression x0),
          (copy_OCamlFrontend403_Parsetree_attributes x1))
  | OCamlFrontend403.Parsetree.Pstr_value (x0,x1) ->
      OCamlFrontend402.Parsetree.Pstr_value
        ((copy_OCamlFrontend403_Asttypes_rec_flag x0),
          (List.map copy_OCamlFrontend403_Parsetree_value_binding x1))
  | OCamlFrontend403.Parsetree.Pstr_primitive x0 ->
      OCamlFrontend402.Parsetree.Pstr_primitive
        (copy_OCamlFrontend403_Parsetree_value_description x0)
  | OCamlFrontend403.Parsetree.Pstr_type (x0,x1) ->
      OCamlFrontend402.Parsetree.Pstr_type (type_declarations x0 x1)
  | OCamlFrontend403.Parsetree.Pstr_typext x0 ->
      OCamlFrontend402.Parsetree.Pstr_typext
        (copy_OCamlFrontend403_Parsetree_type_extension x0)
  | OCamlFrontend403.Parsetree.Pstr_exception x0 ->
      OCamlFrontend402.Parsetree.Pstr_exception
        (copy_OCamlFrontend403_Parsetree_extension_constructor x0)
  | OCamlFrontend403.Parsetree.Pstr_module x0 ->
      OCamlFrontend402.Parsetree.Pstr_module
        (copy_OCamlFrontend403_Parsetree_module_binding x0)
  | OCamlFrontend403.Parsetree.Pstr_recmodule x0 ->
      OCamlFrontend402.Parsetree.Pstr_recmodule
        (List.map copy_OCamlFrontend403_Parsetree_module_binding x0)
  | OCamlFrontend403.Parsetree.Pstr_modtype x0 ->
      OCamlFrontend402.Parsetree.Pstr_modtype
        (copy_OCamlFrontend403_Parsetree_module_type_declaration x0)
  | OCamlFrontend403.Parsetree.Pstr_open x0 ->
      OCamlFrontend402.Parsetree.Pstr_open
        (copy_OCamlFrontend403_Parsetree_open_description x0)
  | OCamlFrontend403.Parsetree.Pstr_class x0 ->
      OCamlFrontend402.Parsetree.Pstr_class
        (List.map copy_OCamlFrontend403_Parsetree_class_declaration x0)
  | OCamlFrontend403.Parsetree.Pstr_class_type x0 ->
      OCamlFrontend402.Parsetree.Pstr_class_type
        (List.map copy_OCamlFrontend403_Parsetree_class_type_declaration x0)
  | OCamlFrontend403.Parsetree.Pstr_include x0 ->
      OCamlFrontend402.Parsetree.Pstr_include
        (copy_OCamlFrontend403_Parsetree_include_declaration x0)
  | OCamlFrontend403.Parsetree.Pstr_attribute x0 ->
      OCamlFrontend402.Parsetree.Pstr_attribute
        (copy_OCamlFrontend403_Parsetree_attribute x0)
  | OCamlFrontend403.Parsetree.Pstr_extension (x0,x1) ->
      OCamlFrontend402.Parsetree.Pstr_extension
        ((copy_OCamlFrontend403_Parsetree_extension x0),
          (copy_OCamlFrontend403_Parsetree_attributes x1))

and copy_OCamlFrontend403_Parsetree_include_declaration :
  OCamlFrontend403.Parsetree.include_declaration ->
    OCamlFrontend402.Parsetree.include_declaration
  =
  fun x  ->
    copy_OCamlFrontend403_Parsetree_include_infos
      copy_OCamlFrontend403_Parsetree_module_expr x

and copy_OCamlFrontend403_Parsetree_class_declaration :
  OCamlFrontend403.Parsetree.class_declaration ->
    OCamlFrontend402.Parsetree.class_declaration
  =
  fun x  ->
    copy_OCamlFrontend403_Parsetree_class_infos
      copy_OCamlFrontend403_Parsetree_class_expr x

and copy_OCamlFrontend403_Parsetree_class_expr :
  OCamlFrontend403.Parsetree.class_expr ->
    OCamlFrontend402.Parsetree.class_expr
  =
  fun
    { OCamlFrontend403.Parsetree.pcl_desc = pcl_desc;
      OCamlFrontend403.Parsetree.pcl_loc = pcl_loc;
      OCamlFrontend403.Parsetree.pcl_attributes = pcl_attributes }
     ->
    {
      OCamlFrontend402.Parsetree.pcl_desc =
        (copy_OCamlFrontend403_Parsetree_class_expr_desc pcl_desc);
      OCamlFrontend402.Parsetree.pcl_loc =
        (copy_OCamlFrontend403_Location_t pcl_loc);
      OCamlFrontend402.Parsetree.pcl_attributes =
        (copy_OCamlFrontend403_Parsetree_attributes pcl_attributes)
    }

and copy_OCamlFrontend403_Parsetree_class_expr_desc :
  OCamlFrontend403.Parsetree.class_expr_desc ->
    OCamlFrontend402.Parsetree.class_expr_desc
  =
  function
  | OCamlFrontend403.Parsetree.Pcl_constr (x0,x1) ->
      OCamlFrontend402.Parsetree.Pcl_constr
        ((copy_OCamlFrontend403_Asttypes_loc
            copy_OCamlFrontend403_Longident_t x0),
          (List.map copy_OCamlFrontend403_Parsetree_core_type x1))
  | OCamlFrontend403.Parsetree.Pcl_structure x0 ->
      OCamlFrontend402.Parsetree.Pcl_structure
        (copy_OCamlFrontend403_Parsetree_class_structure x0)
  | OCamlFrontend403.Parsetree.Pcl_fun (x0,x1,x2,x3) ->
      OCamlFrontend402.Parsetree.Pcl_fun
        ((copy_OCamlFrontend403_Asttypes_arg_label x0),
          (copy_option copy_OCamlFrontend403_Parsetree_expression x1),
          (copy_OCamlFrontend403_Parsetree_pattern x2),
          (copy_OCamlFrontend403_Parsetree_class_expr x3))
  | OCamlFrontend403.Parsetree.Pcl_apply (x0,x1) ->
      OCamlFrontend402.Parsetree.Pcl_apply
        ((copy_OCamlFrontend403_Parsetree_class_expr x0),
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_OCamlFrontend403_Asttypes_arg_label x0),
                  (copy_OCamlFrontend403_Parsetree_expression x1))) x1))
  | OCamlFrontend403.Parsetree.Pcl_let (x0,x1,x2) ->
      OCamlFrontend402.Parsetree.Pcl_let
        ((copy_OCamlFrontend403_Asttypes_rec_flag x0),
          (List.map copy_OCamlFrontend403_Parsetree_value_binding x1),
          (copy_OCamlFrontend403_Parsetree_class_expr x2))
  | OCamlFrontend403.Parsetree.Pcl_constraint (x0,x1) ->
      OCamlFrontend402.Parsetree.Pcl_constraint
        ((copy_OCamlFrontend403_Parsetree_class_expr x0),
          (copy_OCamlFrontend403_Parsetree_class_type x1))
  | OCamlFrontend403.Parsetree.Pcl_extension x0 ->
      OCamlFrontend402.Parsetree.Pcl_extension
        (copy_OCamlFrontend403_Parsetree_extension x0)

and copy_OCamlFrontend403_Parsetree_class_structure :
  OCamlFrontend403.Parsetree.class_structure ->
    OCamlFrontend402.Parsetree.class_structure
  =
  fun
    { OCamlFrontend403.Parsetree.pcstr_self = pcstr_self;
      OCamlFrontend403.Parsetree.pcstr_fields = pcstr_fields }
     ->
    {
      OCamlFrontend402.Parsetree.pcstr_self =
        (copy_OCamlFrontend403_Parsetree_pattern pcstr_self);
      OCamlFrontend402.Parsetree.pcstr_fields =
        (List.map copy_OCamlFrontend403_Parsetree_class_field pcstr_fields)
    }

and copy_OCamlFrontend403_Parsetree_class_field :
  OCamlFrontend403.Parsetree.class_field ->
    OCamlFrontend402.Parsetree.class_field
  =
  fun
    { OCamlFrontend403.Parsetree.pcf_desc = pcf_desc;
      OCamlFrontend403.Parsetree.pcf_loc = pcf_loc;
      OCamlFrontend403.Parsetree.pcf_attributes = pcf_attributes }
     ->
    {
      OCamlFrontend402.Parsetree.pcf_desc =
        (copy_OCamlFrontend403_Parsetree_class_field_desc pcf_desc);
      OCamlFrontend402.Parsetree.pcf_loc =
        (copy_OCamlFrontend403_Location_t pcf_loc);
      OCamlFrontend402.Parsetree.pcf_attributes =
        (copy_OCamlFrontend403_Parsetree_attributes pcf_attributes)
    }

and copy_OCamlFrontend403_Parsetree_class_field_desc :
  OCamlFrontend403.Parsetree.class_field_desc ->
    OCamlFrontend402.Parsetree.class_field_desc
  =
  function
  | OCamlFrontend403.Parsetree.Pcf_inherit (x0,x1,x2) ->
      OCamlFrontend402.Parsetree.Pcf_inherit
        ((copy_OCamlFrontend403_Asttypes_override_flag x0),
          (copy_OCamlFrontend403_Parsetree_class_expr x1),
          (copy_option (fun x  -> x) x2))
  | OCamlFrontend403.Parsetree.Pcf_val x0 ->
      OCamlFrontend402.Parsetree.Pcf_val
        (let (x0,x1,x2) = x0  in
         ((copy_OCamlFrontend403_Asttypes_loc (fun x  -> x) x0),
           (copy_OCamlFrontend403_Asttypes_mutable_flag x1),
           (copy_OCamlFrontend403_Parsetree_class_field_kind x2)))
  | OCamlFrontend403.Parsetree.Pcf_method x0 ->
      OCamlFrontend402.Parsetree.Pcf_method
        (let (x0,x1,x2) = x0  in
         ((copy_OCamlFrontend403_Asttypes_loc (fun x  -> x) x0),
           (copy_OCamlFrontend403_Asttypes_private_flag x1),
           (copy_OCamlFrontend403_Parsetree_class_field_kind x2)))
  | OCamlFrontend403.Parsetree.Pcf_constraint x0 ->
      OCamlFrontend402.Parsetree.Pcf_constraint
        (let (x0,x1) = x0  in
         ((copy_OCamlFrontend403_Parsetree_core_type x0),
           (copy_OCamlFrontend403_Parsetree_core_type x1)))
  | OCamlFrontend403.Parsetree.Pcf_initializer x0 ->
      OCamlFrontend402.Parsetree.Pcf_initializer
        (copy_OCamlFrontend403_Parsetree_expression x0)
  | OCamlFrontend403.Parsetree.Pcf_attribute x0 ->
      OCamlFrontend402.Parsetree.Pcf_attribute
        (copy_OCamlFrontend403_Parsetree_attribute x0)
  | OCamlFrontend403.Parsetree.Pcf_extension x0 ->
      OCamlFrontend402.Parsetree.Pcf_extension
        (copy_OCamlFrontend403_Parsetree_extension x0)

and copy_OCamlFrontend403_Parsetree_class_field_kind :
  OCamlFrontend403.Parsetree.class_field_kind ->
    OCamlFrontend402.Parsetree.class_field_kind
  =
  function
  | OCamlFrontend403.Parsetree.Cfk_virtual x0 ->
      OCamlFrontend402.Parsetree.Cfk_virtual
        (copy_OCamlFrontend403_Parsetree_core_type x0)
  | OCamlFrontend403.Parsetree.Cfk_concrete (x0,x1) ->
      OCamlFrontend402.Parsetree.Cfk_concrete
        ((copy_OCamlFrontend403_Asttypes_override_flag x0),
          (copy_OCamlFrontend403_Parsetree_expression x1))

and copy_OCamlFrontend403_Parsetree_module_binding :
  OCamlFrontend403.Parsetree.module_binding ->
    OCamlFrontend402.Parsetree.module_binding
  =
  fun
    { OCamlFrontend403.Parsetree.pmb_name = pmb_name;
      OCamlFrontend403.Parsetree.pmb_expr = pmb_expr;
      OCamlFrontend403.Parsetree.pmb_attributes = pmb_attributes;
      OCamlFrontend403.Parsetree.pmb_loc = pmb_loc }
     ->
    {
      OCamlFrontend402.Parsetree.pmb_name =
        (copy_OCamlFrontend403_Asttypes_loc (fun x  -> x) pmb_name);
      OCamlFrontend402.Parsetree.pmb_expr =
        (copy_OCamlFrontend403_Parsetree_module_expr pmb_expr);
      OCamlFrontend402.Parsetree.pmb_attributes =
        (copy_OCamlFrontend403_Parsetree_attributes pmb_attributes);
      OCamlFrontend402.Parsetree.pmb_loc =
        (copy_OCamlFrontend403_Location_t pmb_loc)
    }

and copy_OCamlFrontend403_Parsetree_module_expr :
  OCamlFrontend403.Parsetree.module_expr ->
    OCamlFrontend402.Parsetree.module_expr
  =
  fun
    { OCamlFrontend403.Parsetree.pmod_desc = pmod_desc;
      OCamlFrontend403.Parsetree.pmod_loc = pmod_loc;
      OCamlFrontend403.Parsetree.pmod_attributes = pmod_attributes }
     ->
    {
      OCamlFrontend402.Parsetree.pmod_desc =
        (copy_OCamlFrontend403_Parsetree_module_expr_desc pmod_desc);
      OCamlFrontend402.Parsetree.pmod_loc =
        (copy_OCamlFrontend403_Location_t pmod_loc);
      OCamlFrontend402.Parsetree.pmod_attributes =
        (copy_OCamlFrontend403_Parsetree_attributes pmod_attributes)
    }

and copy_OCamlFrontend403_Parsetree_module_expr_desc :
  OCamlFrontend403.Parsetree.module_expr_desc ->
    OCamlFrontend402.Parsetree.module_expr_desc
  =
  function
  | OCamlFrontend403.Parsetree.Pmod_ident x0 ->
      OCamlFrontend402.Parsetree.Pmod_ident
        (copy_OCamlFrontend403_Asttypes_loc copy_OCamlFrontend403_Longident_t
           x0)
  | OCamlFrontend403.Parsetree.Pmod_structure x0 ->
      OCamlFrontend402.Parsetree.Pmod_structure
        (copy_OCamlFrontend403_Parsetree_structure x0)
  | OCamlFrontend403.Parsetree.Pmod_functor (x0,x1,x2) ->
      OCamlFrontend402.Parsetree.Pmod_functor
        ((copy_OCamlFrontend403_Asttypes_loc (fun x  -> x) x0),
          (copy_option copy_OCamlFrontend403_Parsetree_module_type x1),
          (copy_OCamlFrontend403_Parsetree_module_expr x2))
  | OCamlFrontend403.Parsetree.Pmod_apply (x0,x1) ->
      OCamlFrontend402.Parsetree.Pmod_apply
        ((copy_OCamlFrontend403_Parsetree_module_expr x0),
          (copy_OCamlFrontend403_Parsetree_module_expr x1))
  | OCamlFrontend403.Parsetree.Pmod_constraint (x0,x1) ->
      OCamlFrontend402.Parsetree.Pmod_constraint
        ((copy_OCamlFrontend403_Parsetree_module_expr x0),
          (copy_OCamlFrontend403_Parsetree_module_type x1))
  | OCamlFrontend403.Parsetree.Pmod_unpack x0 ->
      OCamlFrontend402.Parsetree.Pmod_unpack
        (copy_OCamlFrontend403_Parsetree_expression x0)
  | OCamlFrontend403.Parsetree.Pmod_extension x0 ->
      OCamlFrontend402.Parsetree.Pmod_extension
        (copy_OCamlFrontend403_Parsetree_extension x0)

and copy_OCamlFrontend403_Parsetree_module_type :
  OCamlFrontend403.Parsetree.module_type ->
    OCamlFrontend402.Parsetree.module_type
  =
  fun
    { OCamlFrontend403.Parsetree.pmty_desc = pmty_desc;
      OCamlFrontend403.Parsetree.pmty_loc = pmty_loc;
      OCamlFrontend403.Parsetree.pmty_attributes = pmty_attributes }
     ->
    {
      OCamlFrontend402.Parsetree.pmty_desc =
        (copy_OCamlFrontend403_Parsetree_module_type_desc pmty_desc);
      OCamlFrontend402.Parsetree.pmty_loc =
        (copy_OCamlFrontend403_Location_t pmty_loc);
      OCamlFrontend402.Parsetree.pmty_attributes =
        (copy_OCamlFrontend403_Parsetree_attributes pmty_attributes)
    }

and copy_OCamlFrontend403_Parsetree_module_type_desc :
  OCamlFrontend403.Parsetree.module_type_desc ->
    OCamlFrontend402.Parsetree.module_type_desc
  =
  function
  | OCamlFrontend403.Parsetree.Pmty_ident x0 ->
      OCamlFrontend402.Parsetree.Pmty_ident
        (copy_OCamlFrontend403_Asttypes_loc copy_OCamlFrontend403_Longident_t
           x0)
  | OCamlFrontend403.Parsetree.Pmty_signature x0 ->
      OCamlFrontend402.Parsetree.Pmty_signature
        (copy_OCamlFrontend403_Parsetree_signature x0)
  | OCamlFrontend403.Parsetree.Pmty_functor (x0,x1,x2) ->
      OCamlFrontend402.Parsetree.Pmty_functor
        ((copy_OCamlFrontend403_Asttypes_loc (fun x  -> x) x0),
          (copy_option copy_OCamlFrontend403_Parsetree_module_type x1),
          (copy_OCamlFrontend403_Parsetree_module_type x2))
  | OCamlFrontend403.Parsetree.Pmty_with (x0,x1) ->
      OCamlFrontend402.Parsetree.Pmty_with
        ((copy_OCamlFrontend403_Parsetree_module_type x0),
          (List.map copy_OCamlFrontend403_Parsetree_with_constraint x1))
  | OCamlFrontend403.Parsetree.Pmty_typeof x0 ->
      OCamlFrontend402.Parsetree.Pmty_typeof
        (copy_OCamlFrontend403_Parsetree_module_expr x0)
  | OCamlFrontend403.Parsetree.Pmty_extension x0 ->
      OCamlFrontend402.Parsetree.Pmty_extension
        (copy_OCamlFrontend403_Parsetree_extension x0)
  | OCamlFrontend403.Parsetree.Pmty_alias x0 ->
      OCamlFrontend402.Parsetree.Pmty_alias
        (copy_OCamlFrontend403_Asttypes_loc copy_OCamlFrontend403_Longident_t
           x0)

and copy_OCamlFrontend403_Parsetree_with_constraint :
  OCamlFrontend403.Parsetree.with_constraint ->
    OCamlFrontend402.Parsetree.with_constraint
  =
  function
  | OCamlFrontend403.Parsetree.Pwith_type (x0,x1) ->
      OCamlFrontend402.Parsetree.Pwith_type
        ((copy_OCamlFrontend403_Asttypes_loc
            copy_OCamlFrontend403_Longident_t x0),
          (copy_OCamlFrontend403_Parsetree_type_declaration x1))
  | OCamlFrontend403.Parsetree.Pwith_module (x0,x1) ->
      OCamlFrontend402.Parsetree.Pwith_module
        ((copy_OCamlFrontend403_Asttypes_loc
            copy_OCamlFrontend403_Longident_t x0),
          (copy_OCamlFrontend403_Asttypes_loc
             copy_OCamlFrontend403_Longident_t x1))
  | OCamlFrontend403.Parsetree.Pwith_typesubst x0 ->
      OCamlFrontend402.Parsetree.Pwith_typesubst
        (copy_OCamlFrontend403_Parsetree_type_declaration x0)
  | OCamlFrontend403.Parsetree.Pwith_modsubst (x0,x1) ->
      OCamlFrontend402.Parsetree.Pwith_modsubst
        ((copy_OCamlFrontend403_Asttypes_loc (fun x  -> x) x0),
          (copy_OCamlFrontend403_Asttypes_loc
             copy_OCamlFrontend403_Longident_t x1))

and copy_OCamlFrontend403_Parsetree_signature :
  OCamlFrontend403.Parsetree.signature ->
    OCamlFrontend402.Parsetree.signature
  = fun x  -> List.map copy_OCamlFrontend403_Parsetree_signature_item x

and copy_OCamlFrontend403_Parsetree_signature_item :
  OCamlFrontend403.Parsetree.signature_item ->
    OCamlFrontend402.Parsetree.signature_item
  =
  fun
    { OCamlFrontend403.Parsetree.psig_desc = psig_desc;
      OCamlFrontend403.Parsetree.psig_loc = psig_loc }
     ->
    {
      OCamlFrontend402.Parsetree.psig_desc =
        (copy_OCamlFrontend403_Parsetree_signature_item_desc psig_desc);
      OCamlFrontend402.Parsetree.psig_loc =
        (copy_OCamlFrontend403_Location_t psig_loc)
    }

and copy_OCamlFrontend403_Parsetree_signature_item_desc :
  OCamlFrontend403.Parsetree.signature_item_desc ->
    OCamlFrontend402.Parsetree.signature_item_desc
  =
  function
  | OCamlFrontend403.Parsetree.Psig_value x0 ->
      OCamlFrontend402.Parsetree.Psig_value
        (copy_OCamlFrontend403_Parsetree_value_description x0)
  | OCamlFrontend403.Parsetree.Psig_type (x0,x1) ->
      OCamlFrontend402.Parsetree.Psig_type (type_declarations x0 x1)
  | OCamlFrontend403.Parsetree.Psig_typext x0 ->
      OCamlFrontend402.Parsetree.Psig_typext
        (copy_OCamlFrontend403_Parsetree_type_extension x0)
  | OCamlFrontend403.Parsetree.Psig_exception x0 ->
      OCamlFrontend402.Parsetree.Psig_exception
        (copy_OCamlFrontend403_Parsetree_extension_constructor x0)
  | OCamlFrontend403.Parsetree.Psig_module x0 ->
      OCamlFrontend402.Parsetree.Psig_module
        (copy_OCamlFrontend403_Parsetree_module_declaration x0)
  | OCamlFrontend403.Parsetree.Psig_recmodule x0 ->
      OCamlFrontend402.Parsetree.Psig_recmodule
        (List.map copy_OCamlFrontend403_Parsetree_module_declaration x0)
  | OCamlFrontend403.Parsetree.Psig_modtype x0 ->
      OCamlFrontend402.Parsetree.Psig_modtype
        (copy_OCamlFrontend403_Parsetree_module_type_declaration x0)
  | OCamlFrontend403.Parsetree.Psig_open x0 ->
      OCamlFrontend402.Parsetree.Psig_open
        (copy_OCamlFrontend403_Parsetree_open_description x0)
  | OCamlFrontend403.Parsetree.Psig_include x0 ->
      OCamlFrontend402.Parsetree.Psig_include
        (copy_OCamlFrontend403_Parsetree_include_description x0)
  | OCamlFrontend403.Parsetree.Psig_class x0 ->
      OCamlFrontend402.Parsetree.Psig_class
        (List.map copy_OCamlFrontend403_Parsetree_class_description x0)
  | OCamlFrontend403.Parsetree.Psig_class_type x0 ->
      OCamlFrontend402.Parsetree.Psig_class_type
        (List.map copy_OCamlFrontend403_Parsetree_class_type_declaration x0)
  | OCamlFrontend403.Parsetree.Psig_attribute x0 ->
      OCamlFrontend402.Parsetree.Psig_attribute
        (copy_OCamlFrontend403_Parsetree_attribute x0)
  | OCamlFrontend403.Parsetree.Psig_extension (x0,x1) ->
      OCamlFrontend402.Parsetree.Psig_extension
        ((copy_OCamlFrontend403_Parsetree_extension x0),
          (copy_OCamlFrontend403_Parsetree_attributes x1))

and copy_OCamlFrontend403_Parsetree_class_type_declaration :
  OCamlFrontend403.Parsetree.class_type_declaration ->
    OCamlFrontend402.Parsetree.class_type_declaration
  =
  fun x  ->
    copy_OCamlFrontend403_Parsetree_class_infos
      copy_OCamlFrontend403_Parsetree_class_type x

and copy_OCamlFrontend403_Parsetree_class_description :
  OCamlFrontend403.Parsetree.class_description ->
    OCamlFrontend402.Parsetree.class_description
  =
  fun x  ->
    copy_OCamlFrontend403_Parsetree_class_infos
      copy_OCamlFrontend403_Parsetree_class_type x

and copy_OCamlFrontend403_Parsetree_class_type :
  OCamlFrontend403.Parsetree.class_type ->
    OCamlFrontend402.Parsetree.class_type
  =
  fun
    { OCamlFrontend403.Parsetree.pcty_desc = pcty_desc;
      OCamlFrontend403.Parsetree.pcty_loc = pcty_loc;
      OCamlFrontend403.Parsetree.pcty_attributes = pcty_attributes }
     ->
    {
      OCamlFrontend402.Parsetree.pcty_desc =
        (copy_OCamlFrontend403_Parsetree_class_type_desc pcty_desc);
      OCamlFrontend402.Parsetree.pcty_loc =
        (copy_OCamlFrontend403_Location_t pcty_loc);
      OCamlFrontend402.Parsetree.pcty_attributes =
        (copy_OCamlFrontend403_Parsetree_attributes pcty_attributes)
    }

and copy_OCamlFrontend403_Parsetree_class_type_desc :
  OCamlFrontend403.Parsetree.class_type_desc ->
    OCamlFrontend402.Parsetree.class_type_desc
  =
  function
  | OCamlFrontend403.Parsetree.Pcty_constr (x0,x1) ->
      OCamlFrontend402.Parsetree.Pcty_constr
        ((copy_OCamlFrontend403_Asttypes_loc
            copy_OCamlFrontend403_Longident_t x0),
          (List.map copy_OCamlFrontend403_Parsetree_core_type x1))
  | OCamlFrontend403.Parsetree.Pcty_signature x0 ->
      OCamlFrontend402.Parsetree.Pcty_signature
        (copy_OCamlFrontend403_Parsetree_class_signature x0)
  | OCamlFrontend403.Parsetree.Pcty_arrow (x0,x1,x2) ->
      OCamlFrontend402.Parsetree.Pcty_arrow
        ((copy_OCamlFrontend403_Asttypes_arg_label x0),
          (copy_OCamlFrontend403_Parsetree_core_type x1),
          (copy_OCamlFrontend403_Parsetree_class_type x2))
  | OCamlFrontend403.Parsetree.Pcty_extension x0 ->
      OCamlFrontend402.Parsetree.Pcty_extension
        (copy_OCamlFrontend403_Parsetree_extension x0)

and copy_OCamlFrontend403_Parsetree_class_signature :
  OCamlFrontend403.Parsetree.class_signature ->
    OCamlFrontend402.Parsetree.class_signature
  =
  fun
    { OCamlFrontend403.Parsetree.pcsig_self = pcsig_self;
      OCamlFrontend403.Parsetree.pcsig_fields = pcsig_fields }
     ->
    {
      OCamlFrontend402.Parsetree.pcsig_self =
        (copy_OCamlFrontend403_Parsetree_core_type pcsig_self);
      OCamlFrontend402.Parsetree.pcsig_fields =
        (List.map copy_OCamlFrontend403_Parsetree_class_type_field
           pcsig_fields)
    }

and copy_OCamlFrontend403_Parsetree_class_type_field :
  OCamlFrontend403.Parsetree.class_type_field ->
    OCamlFrontend402.Parsetree.class_type_field
  =
  fun
    { OCamlFrontend403.Parsetree.pctf_desc = pctf_desc;
      OCamlFrontend403.Parsetree.pctf_loc = pctf_loc;
      OCamlFrontend403.Parsetree.pctf_attributes = pctf_attributes }
     ->
    {
      OCamlFrontend402.Parsetree.pctf_desc =
        (copy_OCamlFrontend403_Parsetree_class_type_field_desc pctf_desc);
      OCamlFrontend402.Parsetree.pctf_loc =
        (copy_OCamlFrontend403_Location_t pctf_loc);
      OCamlFrontend402.Parsetree.pctf_attributes =
        (copy_OCamlFrontend403_Parsetree_attributes pctf_attributes)
    }

and copy_OCamlFrontend403_Parsetree_class_type_field_desc :
  OCamlFrontend403.Parsetree.class_type_field_desc ->
    OCamlFrontend402.Parsetree.class_type_field_desc
  =
  function
  | OCamlFrontend403.Parsetree.Pctf_inherit x0 ->
      OCamlFrontend402.Parsetree.Pctf_inherit
        (copy_OCamlFrontend403_Parsetree_class_type x0)
  | OCamlFrontend403.Parsetree.Pctf_val x0 ->
      OCamlFrontend402.Parsetree.Pctf_val
        (let (x0,x1,x2,x3) = x0  in
         (x0, (copy_OCamlFrontend403_Asttypes_mutable_flag x1),
           (copy_OCamlFrontend403_Asttypes_virtual_flag x2),
           (copy_OCamlFrontend403_Parsetree_core_type x3)))
  | OCamlFrontend403.Parsetree.Pctf_method x0 ->
      OCamlFrontend402.Parsetree.Pctf_method
        (let (x0,x1,x2,x3) = x0  in
         (x0, (copy_OCamlFrontend403_Asttypes_private_flag x1),
           (copy_OCamlFrontend403_Asttypes_virtual_flag x2),
           (copy_OCamlFrontend403_Parsetree_core_type x3)))
  | OCamlFrontend403.Parsetree.Pctf_constraint x0 ->
      OCamlFrontend402.Parsetree.Pctf_constraint
        (let (x0,x1) = x0  in
         ((copy_OCamlFrontend403_Parsetree_core_type x0),
           (copy_OCamlFrontend403_Parsetree_core_type x1)))
  | OCamlFrontend403.Parsetree.Pctf_attribute x0 ->
      OCamlFrontend402.Parsetree.Pctf_attribute
        (copy_OCamlFrontend403_Parsetree_attribute x0)
  | OCamlFrontend403.Parsetree.Pctf_extension x0 ->
      OCamlFrontend402.Parsetree.Pctf_extension
        (copy_OCamlFrontend403_Parsetree_extension x0)

and copy_OCamlFrontend403_Parsetree_extension :
  OCamlFrontend403.Parsetree.extension ->
    OCamlFrontend402.Parsetree.extension
  =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_OCamlFrontend403_Asttypes_loc (fun x  -> x) x0),
      (copy_OCamlFrontend403_Parsetree_payload x1))

and copy_OCamlFrontend403_Parsetree_class_infos :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 OCamlFrontend403.Parsetree.class_infos ->
        'g0 OCamlFrontend402.Parsetree.class_infos
  =
  fun f0  ->
    fun
      { OCamlFrontend403.Parsetree.pci_virt = pci_virt;
        OCamlFrontend403.Parsetree.pci_params = pci_params;
        OCamlFrontend403.Parsetree.pci_name = pci_name;
        OCamlFrontend403.Parsetree.pci_expr = pci_expr;
        OCamlFrontend403.Parsetree.pci_loc = pci_loc;
        OCamlFrontend403.Parsetree.pci_attributes = pci_attributes }
       ->
      {
        OCamlFrontend402.Parsetree.pci_virt =
          (copy_OCamlFrontend403_Asttypes_virtual_flag pci_virt);
        OCamlFrontend402.Parsetree.pci_params =
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_OCamlFrontend403_Parsetree_core_type x0),
                  (copy_OCamlFrontend403_Asttypes_variance x1))) pci_params);
        OCamlFrontend402.Parsetree.pci_name =
          (copy_OCamlFrontend403_Asttypes_loc (fun x  -> x) pci_name);
        OCamlFrontend402.Parsetree.pci_expr = (f0 pci_expr);
        OCamlFrontend402.Parsetree.pci_loc =
          (copy_OCamlFrontend403_Location_t pci_loc);
        OCamlFrontend402.Parsetree.pci_attributes =
          (copy_OCamlFrontend403_Parsetree_attributes pci_attributes)
      }

and copy_OCamlFrontend403_Asttypes_virtual_flag :
  OCamlFrontend403.Asttypes.virtual_flag ->
    OCamlFrontend402.Asttypes.virtual_flag
  =
  function
  | OCamlFrontend403.Asttypes.Virtual  -> OCamlFrontend402.Asttypes.Virtual
  | OCamlFrontend403.Asttypes.Concrete  -> OCamlFrontend402.Asttypes.Concrete

and copy_OCamlFrontend403_Parsetree_include_description :
  OCamlFrontend403.Parsetree.include_description ->
    OCamlFrontend402.Parsetree.include_description
  =
  fun x  ->
    copy_OCamlFrontend403_Parsetree_include_infos
      copy_OCamlFrontend403_Parsetree_module_type x

and copy_OCamlFrontend403_Parsetree_include_infos :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 OCamlFrontend403.Parsetree.include_infos ->
        'g0 OCamlFrontend402.Parsetree.include_infos
  =
  fun f0  ->
    fun
      { OCamlFrontend403.Parsetree.pincl_mod = pincl_mod;
        OCamlFrontend403.Parsetree.pincl_loc = pincl_loc;
        OCamlFrontend403.Parsetree.pincl_attributes = pincl_attributes }
       ->
      {
        OCamlFrontend402.Parsetree.pincl_mod = (f0 pincl_mod);
        OCamlFrontend402.Parsetree.pincl_loc =
          (copy_OCamlFrontend403_Location_t pincl_loc);
        OCamlFrontend402.Parsetree.pincl_attributes =
          (copy_OCamlFrontend403_Parsetree_attributes pincl_attributes)
      }

and copy_OCamlFrontend403_Parsetree_open_description :
  OCamlFrontend403.Parsetree.open_description ->
    OCamlFrontend402.Parsetree.open_description
  =
  fun
    { OCamlFrontend403.Parsetree.popen_lid = popen_lid;
      OCamlFrontend403.Parsetree.popen_override = popen_override;
      OCamlFrontend403.Parsetree.popen_loc = popen_loc;
      OCamlFrontend403.Parsetree.popen_attributes = popen_attributes }
     ->
    {
      OCamlFrontend402.Parsetree.popen_lid =
        (copy_OCamlFrontend403_Asttypes_loc copy_OCamlFrontend403_Longident_t
           popen_lid);
      OCamlFrontend402.Parsetree.popen_override =
        (copy_OCamlFrontend403_Asttypes_override_flag popen_override);
      OCamlFrontend402.Parsetree.popen_loc =
        (copy_OCamlFrontend403_Location_t popen_loc);
      OCamlFrontend402.Parsetree.popen_attributes =
        (copy_OCamlFrontend403_Parsetree_attributes popen_attributes)
    }

and copy_OCamlFrontend403_Asttypes_override_flag :
  OCamlFrontend403.Asttypes.override_flag ->
    OCamlFrontend402.Asttypes.override_flag
  =
  function
  | OCamlFrontend403.Asttypes.Override  -> OCamlFrontend402.Asttypes.Override
  | OCamlFrontend403.Asttypes.Fresh  -> OCamlFrontend402.Asttypes.Fresh

and copy_OCamlFrontend403_Parsetree_module_type_declaration :
  OCamlFrontend403.Parsetree.module_type_declaration ->
    OCamlFrontend402.Parsetree.module_type_declaration
  =
  fun
    { OCamlFrontend403.Parsetree.pmtd_name = pmtd_name;
      OCamlFrontend403.Parsetree.pmtd_type = pmtd_type;
      OCamlFrontend403.Parsetree.pmtd_attributes = pmtd_attributes;
      OCamlFrontend403.Parsetree.pmtd_loc = pmtd_loc }
     ->
    {
      OCamlFrontend402.Parsetree.pmtd_name =
        (copy_OCamlFrontend403_Asttypes_loc (fun x  -> x) pmtd_name);
      OCamlFrontend402.Parsetree.pmtd_type =
        (copy_option copy_OCamlFrontend403_Parsetree_module_type pmtd_type);
      OCamlFrontend402.Parsetree.pmtd_attributes =
        (copy_OCamlFrontend403_Parsetree_attributes pmtd_attributes);
      OCamlFrontend402.Parsetree.pmtd_loc =
        (copy_OCamlFrontend403_Location_t pmtd_loc)
    }

and copy_OCamlFrontend403_Parsetree_module_declaration :
  OCamlFrontend403.Parsetree.module_declaration ->
    OCamlFrontend402.Parsetree.module_declaration
  =
  fun
    { OCamlFrontend403.Parsetree.pmd_name = pmd_name;
      OCamlFrontend403.Parsetree.pmd_type = pmd_type;
      OCamlFrontend403.Parsetree.pmd_attributes = pmd_attributes;
      OCamlFrontend403.Parsetree.pmd_loc = pmd_loc }
     ->
    {
      OCamlFrontend402.Parsetree.pmd_name =
        (copy_OCamlFrontend403_Asttypes_loc (fun x  -> x) pmd_name);
      OCamlFrontend402.Parsetree.pmd_type =
        (copy_OCamlFrontend403_Parsetree_module_type pmd_type);
      OCamlFrontend402.Parsetree.pmd_attributes =
        (copy_OCamlFrontend403_Parsetree_attributes pmd_attributes);
      OCamlFrontend402.Parsetree.pmd_loc =
        (copy_OCamlFrontend403_Location_t pmd_loc)
    }

and copy_OCamlFrontend403_Parsetree_type_extension :
  OCamlFrontend403.Parsetree.type_extension ->
    OCamlFrontend402.Parsetree.type_extension
  =
  fun
    { OCamlFrontend403.Parsetree.ptyext_path = ptyext_path;
      OCamlFrontend403.Parsetree.ptyext_params = ptyext_params;
      OCamlFrontend403.Parsetree.ptyext_constructors = ptyext_constructors;
      OCamlFrontend403.Parsetree.ptyext_private = ptyext_private;
      OCamlFrontend403.Parsetree.ptyext_attributes = ptyext_attributes }
     ->
    {
      OCamlFrontend402.Parsetree.ptyext_path =
        (copy_OCamlFrontend403_Asttypes_loc copy_OCamlFrontend403_Longident_t
           ptyext_path);
      OCamlFrontend402.Parsetree.ptyext_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_OCamlFrontend403_Parsetree_core_type x0),
                (copy_OCamlFrontend403_Asttypes_variance x1))) ptyext_params);
      OCamlFrontend402.Parsetree.ptyext_constructors =
        (List.map copy_OCamlFrontend403_Parsetree_extension_constructor
           ptyext_constructors);
      OCamlFrontend402.Parsetree.ptyext_private =
        (copy_OCamlFrontend403_Asttypes_private_flag ptyext_private);
      OCamlFrontend402.Parsetree.ptyext_attributes =
        (copy_OCamlFrontend403_Parsetree_attributes ptyext_attributes)
    }

and copy_OCamlFrontend403_Parsetree_extension_constructor :
  OCamlFrontend403.Parsetree.extension_constructor ->
    OCamlFrontend402.Parsetree.extension_constructor
  =
  fun
    { OCamlFrontend403.Parsetree.pext_name = pext_name;
      OCamlFrontend403.Parsetree.pext_kind = pext_kind;
      OCamlFrontend403.Parsetree.pext_loc = pext_loc;
      OCamlFrontend403.Parsetree.pext_attributes = pext_attributes }
     ->
    {
      OCamlFrontend402.Parsetree.pext_name =
        (copy_OCamlFrontend403_Asttypes_loc (fun x  -> x) pext_name);
      OCamlFrontend402.Parsetree.pext_kind =
        (copy_OCamlFrontend403_Parsetree_extension_constructor_kind pext_kind);
      OCamlFrontend402.Parsetree.pext_loc =
        (copy_OCamlFrontend403_Location_t pext_loc);
      OCamlFrontend402.Parsetree.pext_attributes =
        (copy_OCamlFrontend403_Parsetree_attributes pext_attributes)
    }

and copy_OCamlFrontend403_Parsetree_extension_constructor_kind :
  OCamlFrontend403.Parsetree.extension_constructor_kind ->
    OCamlFrontend402.Parsetree.extension_constructor_kind
  =
  function
  | OCamlFrontend403.Parsetree.Pext_decl (x0,x1) ->
      OCamlFrontend402.Parsetree.Pext_decl
        ((copy_OCamlFrontend403_Parsetree_constructor_arguments x0),
          (copy_option copy_OCamlFrontend403_Parsetree_core_type x1))
  | OCamlFrontend403.Parsetree.Pext_rebind x0 ->
      OCamlFrontend402.Parsetree.Pext_rebind
        (copy_OCamlFrontend403_Asttypes_loc copy_OCamlFrontend403_Longident_t
           x0)

and copy_OCamlFrontend403_Parsetree_type_declaration :
  OCamlFrontend403.Parsetree.type_declaration ->
    OCamlFrontend402.Parsetree.type_declaration
  =
  fun
    { OCamlFrontend403.Parsetree.ptype_name = ptype_name;
      OCamlFrontend403.Parsetree.ptype_params = ptype_params;
      OCamlFrontend403.Parsetree.ptype_cstrs = ptype_cstrs;
      OCamlFrontend403.Parsetree.ptype_kind = ptype_kind;
      OCamlFrontend403.Parsetree.ptype_private = ptype_private;
      OCamlFrontend403.Parsetree.ptype_manifest = ptype_manifest;
      OCamlFrontend403.Parsetree.ptype_attributes = ptype_attributes;
      OCamlFrontend403.Parsetree.ptype_loc = ptype_loc }
     ->
    {
      OCamlFrontend402.Parsetree.ptype_name =
        (copy_OCamlFrontend403_Asttypes_loc (fun x  -> x) ptype_name);
      OCamlFrontend402.Parsetree.ptype_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_OCamlFrontend403_Parsetree_core_type x0),
                (copy_OCamlFrontend403_Asttypes_variance x1))) ptype_params);
      OCamlFrontend402.Parsetree.ptype_cstrs =
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              ((copy_OCamlFrontend403_Parsetree_core_type x0),
                (copy_OCamlFrontend403_Parsetree_core_type x1),
                (copy_OCamlFrontend403_Location_t x2))) ptype_cstrs);
      OCamlFrontend402.Parsetree.ptype_kind =
        (copy_OCamlFrontend403_Parsetree_type_kind ptype_kind);
      OCamlFrontend402.Parsetree.ptype_private =
        (copy_OCamlFrontend403_Asttypes_private_flag ptype_private);
      OCamlFrontend402.Parsetree.ptype_manifest =
        (copy_option copy_OCamlFrontend403_Parsetree_core_type ptype_manifest);
      OCamlFrontend402.Parsetree.ptype_attributes =
        (copy_OCamlFrontend403_Parsetree_attributes ptype_attributes);
      OCamlFrontend402.Parsetree.ptype_loc =
        (copy_OCamlFrontend403_Location_t ptype_loc)
    }

and copy_OCamlFrontend403_Asttypes_private_flag :
  OCamlFrontend403.Asttypes.private_flag ->
    OCamlFrontend402.Asttypes.private_flag
  =
  function
  | OCamlFrontend403.Asttypes.Private  -> OCamlFrontend402.Asttypes.Private
  | OCamlFrontend403.Asttypes.Public  -> OCamlFrontend402.Asttypes.Public

and copy_OCamlFrontend403_Parsetree_type_kind :
  OCamlFrontend403.Parsetree.type_kind ->
    OCamlFrontend402.Parsetree.type_kind
  =
  function
  | OCamlFrontend403.Parsetree.Ptype_abstract  ->
      OCamlFrontend402.Parsetree.Ptype_abstract
  | OCamlFrontend403.Parsetree.Ptype_variant x0 ->
      OCamlFrontend402.Parsetree.Ptype_variant
        (List.map copy_OCamlFrontend403_Parsetree_constructor_declaration x0)
  | OCamlFrontend403.Parsetree.Ptype_record x0 ->
      OCamlFrontend402.Parsetree.Ptype_record
        (List.map copy_OCamlFrontend403_Parsetree_label_declaration x0)
  | OCamlFrontend403.Parsetree.Ptype_open  ->
      OCamlFrontend402.Parsetree.Ptype_open

and copy_OCamlFrontend403_Parsetree_constructor_declaration :
  OCamlFrontend403.Parsetree.constructor_declaration ->
    OCamlFrontend402.Parsetree.constructor_declaration
  =
  fun
    { OCamlFrontend403.Parsetree.pcd_name = pcd_name;
      OCamlFrontend403.Parsetree.pcd_args = pcd_args;
      OCamlFrontend403.Parsetree.pcd_res = pcd_res;
      OCamlFrontend403.Parsetree.pcd_loc = pcd_loc;
      OCamlFrontend403.Parsetree.pcd_attributes = pcd_attributes }
     ->
    {
      OCamlFrontend402.Parsetree.pcd_name =
        (copy_OCamlFrontend403_Asttypes_loc (fun x  -> x) pcd_name);
      OCamlFrontend402.Parsetree.pcd_args =
        (copy_OCamlFrontend403_Parsetree_constructor_arguments pcd_args);
      OCamlFrontend402.Parsetree.pcd_res =
        (copy_option copy_OCamlFrontend403_Parsetree_core_type pcd_res);
      OCamlFrontend402.Parsetree.pcd_loc =
        (copy_OCamlFrontend403_Location_t pcd_loc);
      OCamlFrontend402.Parsetree.pcd_attributes =
        (copy_OCamlFrontend403_Parsetree_attributes pcd_attributes)
    }

and copy_OCamlFrontend403_Parsetree_constructor_arguments :
  OCamlFrontend403.Parsetree.constructor_arguments ->
    OCamlFrontend402.Parsetree.core_type list
  =
  function
  | OCamlFrontend403.Parsetree.Pcstr_tuple x0 ->
      List.map copy_OCamlFrontend403_Parsetree_core_type x0
  | OCamlFrontend403.Parsetree.Pcstr_record _x0 ->
      migration_error `Pcstr_record

and copy_OCamlFrontend403_Parsetree_label_declaration :
  OCamlFrontend403.Parsetree.label_declaration ->
    OCamlFrontend402.Parsetree.label_declaration
  =
  fun
    { OCamlFrontend403.Parsetree.pld_name = pld_name;
      OCamlFrontend403.Parsetree.pld_mutable = pld_mutable;
      OCamlFrontend403.Parsetree.pld_type = pld_type;
      OCamlFrontend403.Parsetree.pld_loc = pld_loc;
      OCamlFrontend403.Parsetree.pld_attributes = pld_attributes }
     ->
    {
      OCamlFrontend402.Parsetree.pld_name =
        (copy_OCamlFrontend403_Asttypes_loc (fun x  -> x) pld_name);
      OCamlFrontend402.Parsetree.pld_mutable =
        (copy_OCamlFrontend403_Asttypes_mutable_flag pld_mutable);
      OCamlFrontend402.Parsetree.pld_type =
        (copy_OCamlFrontend403_Parsetree_core_type pld_type);
      OCamlFrontend402.Parsetree.pld_loc =
        (copy_OCamlFrontend403_Location_t pld_loc);
      OCamlFrontend402.Parsetree.pld_attributes =
        (copy_OCamlFrontend403_Parsetree_attributes pld_attributes)
    }

and copy_OCamlFrontend403_Asttypes_mutable_flag :
  OCamlFrontend403.Asttypes.mutable_flag ->
    OCamlFrontend402.Asttypes.mutable_flag
  =
  function
  | OCamlFrontend403.Asttypes.Immutable  ->
      OCamlFrontend402.Asttypes.Immutable
  | OCamlFrontend403.Asttypes.Mutable  -> OCamlFrontend402.Asttypes.Mutable

and copy_OCamlFrontend403_Asttypes_variance :
  OCamlFrontend403.Asttypes.variance -> OCamlFrontend402.Asttypes.variance =
  function
  | OCamlFrontend403.Asttypes.Covariant  ->
      OCamlFrontend402.Asttypes.Covariant
  | OCamlFrontend403.Asttypes.Contravariant  ->
      OCamlFrontend402.Asttypes.Contravariant
  | OCamlFrontend403.Asttypes.Invariant  ->
      OCamlFrontend402.Asttypes.Invariant

and copy_OCamlFrontend403_Parsetree_value_description :
  OCamlFrontend403.Parsetree.value_description ->
    OCamlFrontend402.Parsetree.value_description
  =
  fun
    { OCamlFrontend403.Parsetree.pval_name = pval_name;
      OCamlFrontend403.Parsetree.pval_type = pval_type;
      OCamlFrontend403.Parsetree.pval_prim = pval_prim;
      OCamlFrontend403.Parsetree.pval_attributes = pval_attributes;
      OCamlFrontend403.Parsetree.pval_loc = pval_loc }
     ->
    {
      OCamlFrontend402.Parsetree.pval_name =
        (copy_OCamlFrontend403_Asttypes_loc (fun x  -> x) pval_name);
      OCamlFrontend402.Parsetree.pval_type =
        (copy_OCamlFrontend403_Parsetree_core_type pval_type);
      OCamlFrontend402.Parsetree.pval_prim =
        (List.map (fun x  -> x) pval_prim);
      OCamlFrontend402.Parsetree.pval_attributes =
        (copy_OCamlFrontend403_Parsetree_attributes pval_attributes);
      OCamlFrontend402.Parsetree.pval_loc =
        (copy_OCamlFrontend403_Location_t pval_loc)
    }

and copy_OCamlFrontend403_Asttypes_arg_label :
  OCamlFrontend403.Asttypes.arg_label -> string
  =
  function
  | OCamlFrontend403.Asttypes.Nolabel  -> ""
  | OCamlFrontend403.Asttypes.Labelled x0 -> x0
  | OCamlFrontend403.Asttypes.Optional x0 -> "?" ^ x0

and copy_OCamlFrontend403_Asttypes_closed_flag :
  OCamlFrontend403.Asttypes.closed_flag ->
    OCamlFrontend402.Asttypes.closed_flag
  =
  function
  | OCamlFrontend403.Asttypes.Closed  -> OCamlFrontend402.Asttypes.Closed
  | OCamlFrontend403.Asttypes.Open  -> OCamlFrontend402.Asttypes.Open

and copy_OCamlFrontend403_Asttypes_label :
  OCamlFrontend403.Asttypes.label -> OCamlFrontend402.Asttypes.label =
  fun x  -> x

and copy_OCamlFrontend403_Asttypes_rec_flag :
  OCamlFrontend403.Asttypes.rec_flag -> OCamlFrontend402.Asttypes.rec_flag =
  function
  | OCamlFrontend403.Asttypes.Nonrecursive  ->
      OCamlFrontend402.Asttypes.Nonrecursive
  | OCamlFrontend403.Asttypes.Recursive  ->
      OCamlFrontend402.Asttypes.Recursive

and copy_OCamlFrontend403_Parsetree_constant :
  OCamlFrontend403.Parsetree.constant -> OCamlFrontend402.Asttypes.constant
  =
  function
  | OCamlFrontend403.Parsetree.Pconst_integer (x0,x1) ->
     begin match x1 with
     | None -> OCamlFrontend402.Asttypes.Const_int (int_of_string x0)
     | Some 'l' ->
         OCamlFrontend402.Asttypes.Const_int32 (Int32.of_string x0)
     | Some 'L' ->
         OCamlFrontend402.Asttypes.Const_int64 (Int64.of_string x0)
     | Some 'n' ->
         OCamlFrontend402.Asttypes.Const_nativeint (Nativeint.of_string x0)
     | Some _ -> migration_error `Pconst_integer
     end
  | OCamlFrontend403.Parsetree.Pconst_char x0 ->
      OCamlFrontend402.Asttypes.Const_char x0
  | OCamlFrontend403.Parsetree.Pconst_string (x0,x1) ->
      OCamlFrontend402.Asttypes.Const_string (x0,x1)
  | OCamlFrontend403.Parsetree.Pconst_float (x0,x1) ->
      begin match x1 with
      | None -> OCamlFrontend402.Asttypes.Const_float x0
      | Some _ -> migration_error `Pconst_float
      end

and copy_option : 'f0 'g0 . ('f0 -> 'g0) -> 'f0 option -> 'g0 option =
  fun f0  -> function | None  -> None | Some x0 -> Some (f0 x0)

and copy_OCamlFrontend403_Longident_t :
  OCamlFrontend403.Longident.t -> OCamlFrontend402.Longident.t =
  function
  | OCamlFrontend403.Longident.Lident x0 ->
      OCamlFrontend402.Longident.Lident x0
  | OCamlFrontend403.Longident.Ldot (x0,x1) ->
      OCamlFrontend402.Longident.Ldot
        ((copy_OCamlFrontend403_Longident_t x0), x1)
  | OCamlFrontend403.Longident.Lapply (x0,x1) ->
      OCamlFrontend402.Longident.Lapply
        ((copy_OCamlFrontend403_Longident_t x0),
          (copy_OCamlFrontend403_Longident_t x1))

and copy_OCamlFrontend403_Asttypes_loc :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 OCamlFrontend403.Asttypes.loc -> 'g0 OCamlFrontend402.Asttypes.loc
  =
  fun f0  ->
    fun
      { OCamlFrontend403.Asttypes.txt = txt;
        OCamlFrontend403.Asttypes.loc = loc }
       ->
      {
        OCamlFrontend402.Asttypes.txt = (f0 txt);
        OCamlFrontend402.Asttypes.loc =
          (copy_OCamlFrontend403_Location_t loc)
      }

and copy_OCamlFrontend403_Location_t :
  OCamlFrontend403.Location.t -> OCamlFrontend402.Location.t =
  fun
    { OCamlFrontend403.Location.loc_start = loc_start;
      OCamlFrontend403.Location.loc_end = loc_end;
      OCamlFrontend403.Location.loc_ghost = loc_ghost }
     ->
    {
      OCamlFrontend402.Location.loc_start = (copy_Lexing_position loc_start);
      OCamlFrontend402.Location.loc_end = (copy_Lexing_position loc_end);
      OCamlFrontend402.Location.loc_ghost = (copy_bool loc_ghost)
    }

and copy_bool : bool -> bool = function | false  -> false | true  -> true

and copy_Lexing_position : Lexing.position -> Lexing.position =
  fun
    { Lexing.pos_fname = pos_fname; Lexing.pos_lnum = pos_lnum;
      Lexing.pos_bol = pos_bol; Lexing.pos_cnum = pos_cnum }
     ->
    {
      Lexing.pos_fname = pos_fname;
      Lexing.pos_lnum = pos_lnum;
      Lexing.pos_bol = pos_bol;
      Lexing.pos_cnum = pos_cnum
    }

and type_declarations recflag types =
  match
    (recflag, List.map copy_OCamlFrontend403_Parsetree_type_declaration types)
  with
  | OCamlFrontend403.Asttypes.Recursive, types -> types
  | OCamlFrontend403.Asttypes.Nonrecursive, [] -> []
  | OCamlFrontend403.Asttypes.Nonrecursive, (x :: xs) ->
      let open OCamlFrontend402 in
      let ptype_attributes =
        ({Asttypes. txt = "nonrec"; loc = Location.none}, Parsetree.PStr [])
        :: x.Parsetree.ptype_attributes in
      {x with Parsetree.ptype_attributes} :: xs
