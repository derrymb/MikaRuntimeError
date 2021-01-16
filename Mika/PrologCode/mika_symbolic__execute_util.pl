%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                 Copyright 2020 Dr Christophe Meudec 
%%                                     <http://www.echancrure.eu/>
%% This file is part of Mika.
%% Mika is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by
%%   the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
%% Mika is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
%%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
%% You should have received a copy of the GNU General Public License along with Mika.  If not, see <https://www.gnu.org/licenses/>.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mika_symbolic__execute_util.pl
% module mika_symbolic
% utilitarian predicates used during the symbolic execution of intermediate Ada statements
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%modelled on match_renamed_params from mika_symbolic__execute_subprogram_call
check_params_matching([], []) :-
        !.
check_params_matching([], [param([], _, _, _)]) :-
        !.
check_params_matching([param([], _, _, _)|Rest_params], Params_renames) :-
        !,
        check_params_matching(Rest_params, Params_renames).
check_params_matching([param([Param|Rest_ids], _, _, _)|Rest_params], Params_renames) :-
        !,
        check_params_matching_single(Params_renames, Param, Params_renames_rest),
        check_params_matching([param(Rest_ids, _, _, _)|Rest_params], Params_renames_rest).

check_params_matching_single([param([], _, _, _)|Rest_params_renames], Param, Params_renames_rest) :-
        !,
        check_params_matching_single(Rest_params_renames, Param, Params_renames_rest).
check_params_matching_single([param([_|Rest_ids], _, _, _)|Rest_params_renames], _, [param(Rest_ids, _, _, _)|Rest_params_renames]) :-
        !.   %matching of the params
check_params_matching_single(_, _, _) :-
        fail.

%%%
create_atom([], Tmp, Tmp).
create_atom([F|R], Current, Out):-
        number_codes(F, L1), atom_codes(F_atom, L1),
        atom_concat(Current, '_', Tmp),
        atom_concat(Tmp, F_atom, Tmp2),
        create_atom(R, Tmp2, Out).

%%%
%check for global unhandled variables (features/entities) in Term
check_for_unhandled(Term, Has_unhandled) :-
        term_variables_bag(Term, All_vars),
        cfu_has_unhandled(All_vars, Has_unhandled).

cfu_has_unhandled([], no).
cfu_has_unhandled([First|Rest], Has_unhandled) :-
        (mika_unhandled_atts:mika_unhandled_atts__is_unhandled_atts(First) ->
                Has_unhandled = yes
        ;
                cfu_has_unhandled(Rest, Has_unhandled)
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_unhandled(Var, Type_info, Message) :-
        (mika_unhandled_atts:mika_unhandled_atts__is_unhandled_atts(Var) ->
                mika_unhandled_atts:mika_unhandled_atts__get(name, Var, Name)
        ;
                ((mika_name_atts:mika_name_atts__is_name_atts(Var) ->
                        mika_name_atts:mika_name_atts__unput(Var, Name)
                 ;
                  mika_seav_atts:mika_seav_atts__is_seav(Var) ->
                        mika_seav_atts:mika_seav_atts__unput(Var, Name)

                 ;
                  midoan_type:midoan_type__is_type(Var) ->
                        midoan_type:midoan_type__unput(Var, Name)
                 ;
                        Name = lost %changed 15/04/09 common_util__error(10, "Variable's type is unknown", "Cannot proceed", [(var, Var)], 1075114, mika_symbolic, create_unhandled, no_localisation, "Should never happen")
                 ),
                 mika_unhandled_atts:mika_unhandled_atts__create(Var, Name, Type_info)
                )
       ),
       append("Unhandled entity creation :", Message, New_message),
       common_util__error(2, New_message, no_error_consequences, [(name, Name), (type_info, Type_info)], 280128, mika_symbolic, create_unhandled, no_localisation, no_extra_info).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_unhandled_list([]).
create_unhandled_list([Next|Rest], Type_info, Message) :-
        create_unhandled(Next, Type_info, Message),
        create_unhandled_list(Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%initialise a list of enumeration literals name_atts vars to a lits of seavs and returns a list of input_values
% ready to be sent to the solver to be declared as enumeration literals and transformed in mika_enums
initialise_literals_to_seavs([], _Type_var, []).
initialise_literals_to_seavs([Lit_var|Rest], Type_var, [(Lit_name, Lit_value)|Rest2]) :-
        mika_name_atts:mika_name_atts__unput(Lit_var, Lit_name),
        mika_seav_atts:mika_seav_atts__create(Lit_var, Lit_name, object(not_qualified, Type_var, no_init)),
        mika_seav_atts:mika_seav_atts__update(mode, Lit_var, constant),
        mika_seav_atts:mika_seav_atts__get(input_value, Lit_var, Lit_value),   %Yes get...to be instantiated by the midoan_solver on return : here it will be a ground enum literal
        mika_seav_atts:mika_seav_atts__update(symbolic, Lit_var, Lit_name),
        initialise_literals_to_seavs(Rest, Type_var, Rest2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%converts a list of field names (from a record declaration)
% input is a list of (Field_var_namesL, component_definition(Aliased, Subtype_indication), Init_exp)
prepares_field_list([Last], []) :-
        !,
        (Last == no_variant ->
                true
        ;
         Last == variant ->
                common_util__error(10, "VARIANT part not yet implemented for records", "Cannot proceed", no_arguments, 10111107, mika_symbolic, prepares_field_list, no_localisation, "Mika limitation")
        ;
                common_util__error(10, "Unknown record component", "Cannot proceed", [(last, Last)], 10113107, mika_symbolic, prepares_field_list, no_localisation, "Should never happen")
        ).
prepares_field_list([(Field_varsL, component_definition(Aliased, Subtype_indication), Init)|Rest], [(Field_namesL, Field_type_var, Exp)|Rest_field_list]) :-
        pfl_prepares_name_atts_list(Field_varsL, Field_namesL),
        handle_intermediate_types(Subtype_indication, Field_type_var, _Field_type_name),
        (Init == no_init ->
                Exp = no_init
        ;
                Exp = tic(Field_type_var, Init)   %to allow symbolic interpretation when a record var is declared
        ),
        (Aliased == not_aliased ->
                true
        ;
                common_util__error(4, "Aliased record type field is ignored", "e.g. Access will not work", [(field_namesL, Field_namesL)], 4129153, mika_symbolic, prepares_field_list, no_localisation, no_extra_info)
        ),
        prepares_field_list(Rest, Rest_field_list).

%%%
%converts list of mika_name_atts into a list of atom names
        pfl_prepares_name_atts_list([], []).
        pfl_prepares_name_atts_list([Name_atts|Rest], [Literal|Rest_LiteralL]) :-
                mika_name_atts:mika_name_atts__get(name, Name_atts, Literal),
                pfl_prepares_name_atts_list(Rest, Rest_LiteralL).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%handle list of discrete_subtype_definition
%Ada rule 3.6
%used for a list of index from a name array declaration and in for loops
%the output should be a list of Type_names and a list of Type_vars
% can be (discrete_)'subtype_indication' or a 'range'
%discrete ranges are problematic
% -if it is a subtype_indication : no problem
% -if it is an anonymous range over enumeration literals we have a problem e.g. :
%         type month_t is (jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec);
%         type summer_t is (jun, jul, aug);
%         type fun_t is array(jun..aug) of integer; --not allowed
%         dhhd : array (jan..aug) of integer; --allowed
handle_discrete_range_list([], []).
handle_discrete_range_list([F_in|Rest_in], [Dummy_subtype_var|Rest_vars]) :-
        common_util__create_dummy_name(Dummy_subtype_name),
        mika_name_atts:mika_name_atts__create(Dummy_subtype_var, Dummy_subtype_name),
        (F_in = subtype_indication(Null_exclusion, Name_exp, Constraint) ->
                %problem below ??? : may be it should not be the const version of the values
                exec(subtype(Dummy_subtype_var, subtype_indication(Null_exclusion, Name_exp, Constraint)), carry_on)
        ;
         F_in = Range_exp ->
                        (handle_range(Range_exp, _Range_symb, range([Min_const, Max_const])),
                         midoan_solver__get_type_from_value(Min_const, Type),
                         ((Type == 'ground' ; Type == 'standard.ads:integer') -> %an integer anonymous discrete range
                                (mika_name_atts:mika_name_atts__unput(Dummy_subtype_var, _),
                                 midoan_type:standard_type('integer', Integer_type_var),
                                 midoan_type:midoan_type__create_subtype(Dummy_subtype_name, Integer_type_var, range_bounds(Min_const, Max_const), Dummy_subtype_var)
                                )
                         ;
                          Type == 'base_enumeration' ->
                                (midoan_enum:midoan_enum__get(type, Min_const, Basetype),
                                 mika_name_atts:mika_name_atts__unput(Dummy_subtype_var, _),
                                 midoan_type:midoan_type__create_subtype(Dummy_subtype_name, Basetype, range_bounds(Min_const, Max_const), Dummy_subtype_var)
                                )
                         ;
                                common_util__error(10, "TODO variable is not known or an enumeration literal", "Cannot proceed", [(min_const, Min_const)], 1018239, mika_symbolic, handle_discrete_range_list, no_localisation, "Should never happen")
                         )
                        )
        ),
        handle_discrete_range_list(Rest_in, Rest_vars).
%%%
%handle list of index_subtype_definition
%Ada rule 3.6
%used for unconstrained arrays only
%  returns a list of type_vars
handle_unconstrained_array_indexes([], []).
handle_unconstrained_array_indexes([range(Subtype_var, box)|Rest_in], [Type_var|Rest_out]) :-
        symbolically_interpret(Subtype_var, _, Type_var, Type, _Exception_),
        (Type == unhandled_expression ->
                common_util__error(10, "Unhandled entities in unconstrained array index", no_error_consequences, no_arguments, 1019342, mika_symbolic, handle_unconstrained_array_indexes, no_localisation, no_extra_info)
        ;
         Type == type ->
                handle_unconstrained_array_indexes(Rest_in, Rest_out)
        ;
                common_util__error(10, "Unconstrained array index does not evaluate to a type", no_error_consequences, no_arguments, 1019847, mika_symbolic, handle_unconstrained_array_indexes, no_localisation, no_extra_info)
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%the array aggregate is restricted by Ada semantics (no others clause, single choice in list)
%Rep_clause is a list of symbolically executed expression of the form [Exp, ..., Exp]: positional
%                                                                  or [(Lit, Exp), ..., (Lit, Exp)]: named
update_representation_enum([First|Rest], Rep_clause) :-
        ((compound(First), First = named(_Names, _Value)) ->
                update_representation_clause_enum_named([First|Rest], Rep_clause)
        ;
                update_representation_clause_enum_positional([First|Rest], Rep_clause)
        ).

update_representation_clause_enum_positional([], []).
update_representation_clause_enum_positional([Exp|R], [Exp_cons|R1]) :-
        symbolically_interpret(Exp, _, Exp_cons, Type, _Exception_),            %can be a complex expression
        (Type == unhandled_expression ->
                common_util__error(10, "Unhandled entities in positional representation clause", no_error_consequences, no_arguments, 10216128, mika_symbolic, update_representation_clause_enum_positional, no_localisation, no_extra_info)
        ;
                update_representation_enum(R, R1)
        ).

update_representation_clause_enum_named([], []).
update_representation_clause_enum_named([named([Seav_var], Exp)|R], [named(Seav_name, Exp_cons)|R1]) :-
        symbolically_interpret(Exp, _, Exp_cons, Type, _Exception_),            %can be a complex expression
        (Type == unhandled_expression ->
                common_util__error(10, "Unhandled entities in named representation clause", no_error_consequences, no_arguments, 1022516, mika_symbolic, update_representation_clause_enum_named, no_localisation, no_extra_info)
        ;
                (mika_seav_atts:mika_seav_atts__get(name, Seav_var, Seav_name),
                 update_representation_clause_enum_named(R, R1)
                )
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%deal with Type_var which could be of type name_atts (1st time seen this type)
%                                       or type_atts (2nd time seen this type because it has already been declared private or as an incomplete type)
handle_intermediate_type_var(Type_var, Type_name) :-
        (mika_name_atts:mika_name_atts__is_name_atts(Type_var) ->
                mika_name_atts:mika_name_atts__unput(Type_var, Type_name)
        ;
         midoan_type:midoan_type__is_type(Type_var) ->
                midoan_type:midoan_type__unput(Type_var, Type_name)

        ;
                common_util:common_util__error(10, "Type is of the wrong type", "Cannot proceed", [(type_var, Type_var)], 10233181, mika_symbolic, handle_intermediate_type_var, no_localisation, "Should never happen: Type_var is not a name_atts (1st time seen) or a type_atts (2nd time seen because private or incomplete)")
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%deal with compound_names : we return the last identifier only (gnatxref will ensure its unicity)
%can also be a simple variable name
%not currently needed : compound(_, _) is no longer outputed by the parser (changed 10/12/07) keeping it for the moment just in case we need to roll back
handle_compound_name(Compound, Name) :-
        compound(Compound),
        nonvar(Compound),       %to be sure
        !,
        Compound = compound(_, Name).   %why is it not recursif?
handle_compound_name(Compound, Name) :-
        var(Compound),
        !,
        Compound = Name.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%deal with selected_names for subprogram calls: we return the last identifier only (gnatxref will ensure its unicity)
%can also be a simple functor variable (which can be an operator symbol)
handle_selected_name(Selected_exp, Functor) :-
        (var(Selected_exp) ->
                Selected_exp = Functor
        ;
         Selected_exp = selected(_Selector, Exp) ->
                handle_selected_name(Exp, Functor)
        ;
                Selected_exp = Functor
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%creates a new subprogram Subprogram_var based on the generic instantiation by matching the generic_formal_part with the parameters and attaching its body
%Subprogram_var is a name_atts var
%Generic_var is a generic_atts var
handle_generic_subprogram_instantiation(Subprogram_var, Generic_inst) :-
        mika_name_atts:mika_name_atts__get(name, Subprogram_var, New_subprogram_name),
        %Generic_inst is similar to a procedure call see 'exec(procedure_call(Call), carry_on)'
        (Generic_inst = indexed(Name_exp, Parameters) ->           %Name_exp can be an indentifier, an operator symbol or a sequence of select(...,...)
                handle_selected_name(Name_exp, Generic_var)
        ;
         Generic_inst = selected(_, _) ->                       %a selected subprogram instantiation without parameters
                (handle_selected_name(Generic_inst, Generic_var),
                 Parameters = []
                )
        ;
         Generic_inst = Generic_var ->                              %a non selected subprogram instantiation without parameters
                 Parameters = []
        ),
        mika_generic_atts:mika_generic_atts__get(name, Generic_var, _Generic_name),
        mika_generic_atts:mika_generic_atts__get(generic, Generic_var, Generic_formal_part),
        %mika_generic_atts__get(spec, Generic_var, _Generic_specification),      %probably not needed
        mika_generic_atts:mika_generic_atts__get(body, Generic_var, Generic_body),
        (Generic_body = procedure_body(_Generic_subprogram_var, no_return, parameters(Params), local_decl(Decls), Body) ->
                (%we should rewrite Params, Decls and Body by replacing the generic formal parameters in Generic_formal_part by the passed Parameters
                 % How to do it? making a copy? actual matching, or actual parsing and replacing?
                 %  -making a copy we need to copy everything (parameters, local variables) except global variables: this is similar to subprogram call
                 my_copy_term('locals', a(Generic_formal_part, Params, Decls, Body), a(Generic_formal_part_c, Params_c, Decls_c, Body_c)),
                 Generic_formal_part_c = [generic|Generic_paramL],
                 hgsi_match_generic_params(Generic_paramL, Parameters),
                 exec(subprogram_body(nothing, procedure_body(Subprogram_var, no_return, parameters(Params_c), local_decl(Decls_c), Body_c)), carry_on)   %the subprogram will be declared in the normal way
                )
        ;
         Generic_body = function_body(_Generic_subprogram_var, Return_var, Return_type, parameters(Params), local_decl(Decls), Body) ->
                (my_copy_term('locals', a(Generic_formal_part, Return_var, Return_type, Params, Decls, Body), a(Generic_formal_part_c, Return_var_c, Return_type_c, Params_c, Decls_c, Body_c)),
                 Generic_formal_part_c = [generic|Generic_paramL],
                 hgsi_match_generic_params(Generic_paramL, Parameters),
                 exec(subprogram_body(nothing, function_body(Subprogram_var, Return_var_c, Return_type_c, parameters(Params_c), local_decl(Decls_c), Body_c)), carry_on)   %the subprogram will be declared in the normal way
                )
        ;
                common_util__error(10, "This kind of generic subprogram instantiation is not yet handled", "Cannot proceed", [(new_subprogram_name, New_subprogram_name), (generic_body, Generic_body)], 10306207, mika_symbolic, handle_generic_subprogram_instantiation, no_localisation, "Mika limitation")
        ).

%named or positional matching allowed
hgsi_match_generic_params([], []).
hgsi_match_generic_params([Generic_param|Generic_paramL], [Arg|ArgL]) :-
        %there are many kinds of generic formal parameters : for the moment all we need is the name of the formal parameter
        (Generic_param = type(Generic_name, _Generic_info, private, _Tagged_opt, _Limited_opt) ->
                (mika_name_atts:mika_name_atts__unput(Generic_name, _),
                 Generic_name = Arg
                )
        ;
         Generic_param = type(Generic_name, no_discriminant, access, subtype_indication(_Null_exclusion, _Type, no_constraint)) ->
                (mika_name_atts:mika_name_atts__unput(Generic_name, _),
                 Generic_name = Arg
                )
        ;
                common_util__error(10, "The generic parameter is not recognised", "Cannot proceed", [(generic_param, Generic_param)], 1032346, mika_symbolic, hgsi_match_generic_params, no_localisation, no_extra_info)
        ),
        hgsi_match_generic_params(Generic_paramL, ArgL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%deal with subtype_indication
%Ada Rule 3.2.2
%Note that the name_exp will gobble up the index and discriminant constraint
% name_cons could be a direct_name, a selected_component or an indexed_component with an index_contraint (for an unconstrained array)
%   or discriminant_constraint (for unknown discriminant parts)
%possible forms:
% name_cons no_constraint : name_cons could be a direct_name, a selected_component or an indexed_component with an index_contraint
%   or discriminant_constraint
% name_cons range([Min_cons, Max_cons])
% name_cons digits(Digits_cons, no_range)
% name_cons digits(Digits_cons, range([Min_cons, Max_cons]))
% name_cons delta(Delta_cons, no_range)
% name_cons delta(Delta_cons, range([Min_cons, Max_cons]))
handle_subtype_indication(subtype_indication(_Null_exclusion, Name_exp, Constraint_exp), Name_const, Cons_const) :-
        ((nonvar(Name_exp) , Name_exp = indexed(Something, Indexes) , Constraint_exp == no_constraint) ->       %used below too in hit__is_anonymous_type/2
                (symbolically_interpret(Something, _, Name_const, Type, _Exception_),
                 (Type == unhandled_expression ->
                        common_util__error(10, "Unhandled entities in subtype indication", no_error_consequences, no_arguments, 1035818, mika_symbolic, handle_subtype_indication, no_localisation, no_extra_info)
                 ;
                  Type == type ->
                        (midoan_type:midoan_type__obtain_basetype(Name_const, Basetype),
                         (Basetype = unconst_array(_) ->        %an unconstrained array bounds instantiation
                                (handle_discrete_range_list(Indexes, IndexL_out),      %IndexL_out is a list of Type_vars
                                 Cons_const = unconst_array(IndexL_out)
                                )
                         ;
                                common_util__error(10, "The subtype indication is not yet handled", "Cannot proceed", [(name_exp, Name_exp), (constraint_exp, Constraint_exp)], 10351174, mika_symbolic, handle_subtype_indication, no_localisation, "Mika limitation")
                         )
                        )
                 ;
                        common_util__error(10, "Unexpected type in subtype indication", no_error_consequences, [(type, Type)], 1037221, mika_symbolic, handle_subtype_indication, no_localisation, no_extra_info)
                 )
                )
        ;
                (symbolically_interpret(Name_exp, _, Name_const, Name_type, _Exception_),     %Name_const is the name of the subtype on which a constraint is applied
                 (Name_type == unhandled_expression ->
                        common_util__error(10, "Unhandled entities in subtype indication", no_error_consequences, no_arguments, 1037823, mika_symbolic, handle_subtype_indication, no_localisation, no_extra_info)
                 ;
                        (
                         (Constraint_exp == no_constraint ->
                                Cons_const = no_constraint
                         ;
                          Constraint_exp = constraint(range(Range_exp)) ->
                                handle_range(Range_exp, _Range_symb, Cons_const)
                         ;
                          Constraint_exp = constraint(digits(Digits_exp, Range_constraint_opt)) ->
                                handle_digits_constraint(digits(Digits_exp, Range_constraint_opt), Cons_const)
                         ;
                          Constraint_exp = constraint(delta(Delta_exp, Range_constraint_opt)) ->
                                handle_delta_constraint(delta(Delta_exp, Range_constraint_opt), Cons_const)
                         )
                        )
                 )
                )
        ).

%27/01/09 : use this instead of directly call exec(subtype(Type_var, Subtype_indication), carry_on) : fewer dummy types created
handle_intermediate_types(Subtype_indication_or_access_definition, Type_var, Type_name) :-
        (Subtype_indication_or_access_definition = access_definition(_Null_Exclusion, _Kind, _Name) ->
                (create_unhandled(Type_var, intermediate_type, "Unhandled entity in handle_intermediate_types: array component type is an access type"),
                 mika_unhandled_atts:mika_unhandled_atts__get(name, Type_var, Type_name)
                )
        ;
         Subtype_indication_or_access_definition = access_definition(_Null_Exclusion, _Kind, _Protection, _Parameters) ->
                (create_unhandled(Type_var, intermediate_type, "Unhandled entity in handle_intermediate_types: array component type is an access type"),
                 mika_unhandled_atts:mika_unhandled_atts__get(name, Type_var, Type_name)
                )
        ;
         Subtype_indication_or_access_definition = access_definition(_Null_Exclusion, _Kind, _Protection, _Parameters) ->
                (create_unhandled(Type_var, intermediate_type, "Unhandled entity in handle_intermediate_types: array component type is an access type"),
                 mika_unhandled_atts:mika_unhandled_atts__get(name, Type_var, Type_name)
                )
        ;
                (Subtype_indication = Subtype_indication_or_access_definition,
                 check_for_unhandled(Subtype_indication, Has_unhandled),
                 (Has_unhandled == no ->
                        (hit__is_anonymous_type(Subtype_indication, Is_anonymous_type),
                         (Is_anonymous_type == yes ->
                                (common_util__create_dummy_name(Dummy_type_name),
                                 mika_name_atts:mika_name_atts__create(Type_var, Dummy_type_name),
                                 exec(subtype(Type_var, Subtype_indication), carry_on),
                                 midoan_type:midoan_type__get_typemark(Type_var, Type_name)     %needed? surely Type_name == Dummy_type_name
                                )
                         ;
                                (Subtype_indication = subtype_indication(_Null_exclusion, Type_var_given, no_constraint),
                                 symbolically_interpret(Type_var_given, Type_name, Type_var, Type_type, _Exception_),
                                 (Type_type == unhandled_expression ->
                                        (create_unhandled(Type_var, intermediate_type, "Unhandled entity in handle_intermediate_types"),
                                         mika_unhandled_atts:mika_unhandled_atts__get(name, Type_var, Type_name)
                                        )
                                 ;
                                        true
                                 )
                                )
                         )
                        )
                 ;
                        (create_unhandled(Type_var, intermediate_type, "Unhandled entity in handle_intermediate_types"),
                         mika_unhandled_atts:mika_unhandled_atts__get(name, Type_var, Type_name)
                        )
                 )
                )
        ).

%26/01/09: to address 'dummy types' issue
hit__is_anonymous_type(subtype_indication(_Null_exclusion, Name_exp, Constraint_exp), Is_anonymous) :-
        ((nonvar(Name_exp) , Name_exp = indexed(_Something, _Indexes) , Constraint_exp == no_constraint) ->       %from above
                Is_anonymous = yes
        ;
         Constraint_exp == no_constraint ->
                Is_anonymous = no
        ;
                Is_anonymous = yes
        ).
%%%
%deal with range: always returns something like range([Min_const, Max_const])
%Ada Rule 3.5
handle_range([Min_exp, Max_exp], range([Min_symb, Max_symb]), range([Min_const, Max_const])) :-
        !,
        symbolically_interpret(Min_exp, Min_symb, Min_const, Min_type, _Exception_),
        symbolically_interpret(Max_exp, Max_symb, Max_const, Max_type, _Exception_),
        ((Min_type == unhandled_expression ; Max_type == unhandled_expression) ->
                common_util__error(10, "Unhandled entities in range", no_error_consequences, no_arguments, 1046428, mika_symbolic, handle_range, no_localisation, no_extra_info)
        ;
                true
        ).

%Name_exp must be interpreted only once (may have side effects)
handle_range(tic(Name_exp, range), range([tic(Name, first), tic(Name, last)]), range([Min_const, Max_const])) :-
        !,
        syntactically_denote(Name_exp, Name, Entity, Type_or_object),
        (Type_or_object == object ->
                (mika_seav_atts:mika_seav_atts__get(type, Entity, Array_type_var),
                 midoan_solver__interpret(first(Array_type_var, 1), types(type), Min_const, Min_type, 'no_exception'),
                 midoan_solver__interpret(last(Array_type_var, 1), types(type), Max_const, Max_type, 'no_exception'),
                 ((Min_type == unhandled_expression ; Max_type == unhandled_expression) ->
                        %23/09/09 (see electronic diary) "instead of returning range([Min_const, Max_const]) if unhandled_expression is returned we could create an unhandled var : but then we would have to deal with that everywhere handle_range is used ... and check and propagate the unhandled feature upwards."
                        common_util__error(10, "Unhandled entities in range", no_error_consequences, no_arguments, 1047832, mika_symbolic, handle_range, no_localisation, no_extra_info)
                 ;
                        true
                 )
                )
        ;
         Type_or_object == type ->
                (midoan_solver__interpret(first(Name), types(_), Min_const, Min_type, 'no_exception'),
                 midoan_solver__interpret(last(Name), types(_), Max_const, Max_type, 'no_exception'),
                 ((Min_type == unhandled_expression ; Max_type == unhandled_expression) ->
                        %23/09/09 (see electronic diary) "instead of returning range([Min_const, Max_const]) if unhandled_expression is returned we could create an unhandled var : but then we would have to deal with that everywhere handle_range is used ... and check and propagate the unhandled feature upwards."
                        common_util__error(10, "Unhandled entities in range", no_error_consequences, no_arguments, 1048835, mika_symbolic, handle_range, no_localisation, no_extra_info)
                 ;
                        true
                 )
                )
        ;
         Type_or_object == unhandled_expression ->
                common_util__error(10, "Unhandled entities in range", no_error_consequences, no_arguments, 1048529, mika_symbolic, handle_range, no_localisation, no_extra_info)
        ;
                common_util__error(10, "Unexpected type in range", no_error_consequences, no_arguments, 1048730, mika_symbolic, handle_range, no_localisation, no_extra_info)
        ).

%Name_exp must evaluate to an array object, an array type or a constrained array subtype
%Name_exp should be interpreted only once (may have side effects)
handle_range(tic(Name_exp, range, Exp), range([Min_symb, Max_symb]), range([Min_const, Max_const])) :-
        !,
        %to transform into (e.g.) indexed(tic(Var_Seav(AA), last), [2])
        symbolically_interpret(indexed(tic(Name_exp, first), [Exp]), Min_symb, Min_const, Min_type, _Exception_),
        symbolically_interpret(indexed(tic(Name_exp, last), [Exp]), Max_symb, Max_const, Max_type, _Exception_),
        ((Min_type == unhandled_expression ; Max_type == unhandled_expression) ->
                common_util__error(10, "Unhandled entities in tic range", no_error_consequences, no_arguments, 1050836, mika_symbolic, handle_range, no_localisation, no_extra_info)
        ;
                true
        ).

handle_range(Type_var, Type_name, range([Min_const, Max_const])) :-
        midoan_type:midoan_type__is_type(Type_var),
        !,
        midoan_type:midoan_type__get_typemark(Type_var, Type_name),
        symbolically_interpret(tic(Type_var, first), _Min_symb, Min_const, Min_type, _Exception_),
        symbolically_interpret(tic(Type_var, last), _Max_symb, Max_const, Max_type, _Exception_),
        ((Min_type == unhandled_expression ; Max_type == unhandled_expression) ->
                common_util__error(10, "Unhandled entities in range", no_error_consequences, no_arguments, 1052036, mika_symbolic, handle_range, no_localisation, no_extra_info)
        ;
                true
        ).

handle_range(Expr, _, _) :-
        !,
        common_util__error(10, "Unknown first argument", "Cannot proceed", [(expr, Expr)], 1041197, mika_symbolic, handle_range, no_localisation, "Should never happen").
%%%
%deal with digits_constraint
%Ada Rule 3.5.9
handle_digits_constraint(digits(Digits_exp, Range_constraint_opt), Digits_out) :-
        symbolically_interpret(Digits_exp, _Symb, Digits_const, Type, _Exception_),
        (Type == unhandled_expression ->
                common_util__error(10, "Unhandled entities in digit constraint", no_error_consequences, no_arguments, 1053438, mika_symbolic, handle_digits_constraint, no_localisation, no_extra_info)
        ;
                ((Range_constraint_opt == empty_range ->
                        Digits_out = digits(Digits_const, no_range)
                ;
                        (Range_constraint_opt = range(Range_exp),
                         handle_range(Range_exp, _Range_symb, Range_out),
                         Digits_out = digits(Digits_const, Range_out)
                        )
                 )
                )
        ).

%%%
%deal with delta_constraint
%Ada Rule J.3
handle_delta_constraint(delta(Delta_exp, Range_constraint_opt), Delta_out) :-
        symbolically_interpret(Delta_exp, _Symb, Delta_const, Type, _Exception_),
        (Type == unhandled_expression ->
                common_util__error(10, "Unhandled entities in delta constraint", no_error_consequences, no_arguments, 1055341, mika_symbolic, handle_delta_constraint, no_localisation, no_extra_info)
        ;
                ((Range_constraint_opt == empty_range ->
                        Delta_out = delta(Delta_const, no_range)
                ;
                        (Range_constraint_opt = range(Range_exp),
                         handle_range(Range_exp, _Range_symb, Range_out),
                         Delta_out = delta(Delta_const, Range_out)
                        )
                 )
                )
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%declare the objects as Seav variables
%deal with Var which could be of type name_atts (1st time seen this object)
%                                  or seav_atts (2nd time seen this object because it has been declared as private or incomplete or re-entering a loop (for the identifier))
objects([], _).
objects([Var|R], Decl) :-
        (mika_name_atts:mika_name_atts__is_name_atts(Var) ->
                 mika_name_atts:mika_name_atts__unput(Var, Var_name)
        ;
         mika_seav_atts:mika_seav_atts__is_seav(Var) ->                        %i.e. was already declared (e.g. the identifier in a loop, blocks in a loop?)
                 mika_seav_atts:mika_seav_atts__unput(Var, Var_name)
        ;
                common_util__error(10, "Variable is not of a valid type", "Cannot proceed", [(var, Var)], 10456167, mika_symbolic, objects, no_localisation, "Should never happen: name_atts (1st time seen) nor a seav_atts (2nd time seen because private or incomplete)")
        ),
        Decl = object(Qualifier, Type_var, Init),
        (Init == no_init ->     %below, we check if the type contains, default initialised, record fields
                (midoan_type:midoan_type__get_attribute(Type_var, has_fields_with_default, Has_fields_with_default),
                 (Has_fields_with_default == no ->
                        mika_seav_atts:mika_seav_atts__create(Var, Var_name, object(Qualifier, Type_var, no_init))
                 ;
                        (midoan_type:midoan_type__created_variable_with_default_initialised_fields(Default_initialised_variable, Type_var),
                         mika_seav_atts:mika_seav_atts__create(Var, Var_name, object(Qualifier, Type_var, init(default_initialised_variable, Default_initialised_variable)))
                        )
                 )
                )
        ;
                (symbolically_interpret(Init, Symbolic_init, Constraint_init, Type_init, _Exception_),
                 (Type_init == unhandled_expression ->
                        (common_util__error(4, "Unhandled expression in initialisation of object", no_error_consequences, [(name, Var_name)], 4596, mika_symbolic, objects, no_localisation, no_extra_info),
                          mika_unhandled_atts:mika_unhandled_atts__create(Var, Var_name, Symbolic_init)
                        )
                 ;
                        (midoan_type:midoan_type__variable_declaration(Template, Type_var),
                         uvoa_check_range(Constraint_init, Template, Var_name),              %may fail if out of range
                         %for unconst array 'variable' (e.g. in Next_work_day: W := (tue, Wed, ..., Mon); see Barne p 123)
                         % the type of the var will change to the dummy subtype of Constraint_init
                         midoan_type:midoan_type__obtain_basetype(Type_var, Basetype),
                         (Basetype = unconst_array(_) ->
                                (%trace,
                                 midoan_array:midoan_array__get_type_var(Constraint_init, New_type_var)
                                )
                         ;
                                New_type_var = Type_var
                         ),
                         mika_seav_atts:mika_seav_atts__create(Var, Var_name, object(Qualifier, New_type_var, init(Symbolic_init, Constraint_init)))
                        )
                 )
                )
        ),
        objects(R, Decl).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%must be passed an seav variable (since they are the structure that allow us to perform the symbolic interpretation
%in particular record field and array element assignement need to be preprocessed prior to being sent to this predicate
%updates the attributes of an SEAV(can be simple variable or record, or array) on assignement
update_var_on_assignement(Seav_var, Symbolic, Constraint) :-
        mika_seav_atts:mika_seav_atts__update(symbolic, Seav_var, Symbolic),
        mika_seav_atts:mika_seav_atts__get(mode, Seav_var, Mode),
        mika_seav_atts:mika_seav_atts__get(type, Seav_var, Type_var),
        (Mode == unused ->
                (mika_seav_atts:mika_seav_atts__update(mode, Seav_var, out),
                 midoan_type:midoan_type__variable_declaration(Template, Type_var),
                 mika_seav_atts:mika_seav_atts__update(output_value, Seav_var, Template)
                )
        ;
         Mode == init_elab ->
                (mika_globals:mika_globals__get_NBT(phase, Info),
                 (Info == post_elaboration ->   %04/04/08
                        mika_seav_atts:mika_seav_atts__update(mode, Seav_var, out_elab)
                 ;
                        (mika_seav_atts:mika_seav_atts__update(input_value, Seav_var, Constraint),
                         mika_seav_atts:mika_seav_atts__update(symbolic, Seav_var, Symbolic)
                        )
                 ),
                 midoan_type:midoan_type__variable_declaration(Template, Type_var),
                 mika_seav_atts:mika_seav_atts__update(output_value, Seav_var, Template)
                )
        ;
         Mode == in ->
                (mika_seav_atts:mika_seav_atts__update(mode, Seav_var, in_out),
                 midoan_type:midoan_type__variable_declaration(Template, Type_var),
                 mika_seav_atts:mika_seav_atts__update(output_value, Seav_var, Template)
                )
        ;
         (Mode == in_out ; Mode == out ; Mode == out_elab) ->
                true
        ),
        uvoa_perform_update(Seav_var, Constraint),
        ((midoan_array:midoan_array__is_unconst_array(Type_var), midoan_array:midoan_array__is_array(Constraint), \+ midoan_array:midoan_array__get_all_index_elements(Constraint, unconst_array(_, _))) ->
                (%an unconstrained array was assigned a constrained array value
                 midoan_array:midoan_array__get_type_var(Constraint, Constrained_type_var),
                 mika_seav_atts:mika_seav_atts__update(type, Seav_var, Constrained_type_var)   %we update its type
                )
        ;
                true
        ),
        !.

%ensures variable range compliance via uvoa_check_range
% in other words we ensure that the expression that we update the Seav_var with is constrained by the definition
% range of the variable
%this way we never generate test inputs that will lead to constraint errors being raised.
uvoa_perform_update(Seav_var, Constraint) :-
        mika_seav_atts:mika_seav_atts__get(output_value, Seav_var, Output_value),
        copy_term(Output_value, Output_value_copy),             %a copy because we don't want to change the output_value (it is just used a template) : should ok, because no delayed constraints
        mika_seav_atts:mika_seav_atts__get(name, Seav_var, Name),              %just for error message
        uvoa_check_range(Constraint, Output_value_copy, Name),    %may fail if out of range
        mika_seav_atts:mika_seav_atts__update(constraint, Seav_var, Output_value_copy).

%note that we only check for SYSTEMATIC range error within the current subpath
% in other words, we do not check for the POSSIBILITY of a range error
% that feature could be added later 1st within the current subpath [easy] and 2nd later for all subpath that lead to a given assignment [hard]
uvoa_check_range(Constraint, Template_var, Name) :-    %may fail if out of range
        (midoan_solver__controlled_unification(Constraint, Template_var)  ->            %may fail if out of range
                true
        ;
                (%this may occur if the path is actually infeasible too (example of tomorrow for decision): some constraints are awaken and found contradictory
                 % may be we should try to label and give an example ...
                 common_util__error(10, "CONSTRAINT_ERROR raised systematically in this path (if it is feasible) for variable being assigned this value", "Systematic exception will be raised in this path", [(name, Name), (constraint, Constraint)], 10555243, mika_symbolic, uvoa_check_range, no_localisation, no_extra_info)
                )
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%debug only : upper limit for the number of iterations
for_body(Iteration, _Loop_name, _, _, _, _, _, 'carry_on') :-
        maxForIterationReached(Iteration),     %upper limit for the number of iterations
        common_util__error(4, "We may be in an infinite loop or run the risk of reaching mika's limitations", "the solution given for this path (if any) may not be correct (limitations reached) or lead to infinite processing (infinite loop)", no_arguments, 4563271, mika_symbolic, for_body, no_localisation, no_extra_info),
        !,
        fail.   %not logically correct! we force backtrack

for_body(Iteration, Loop_name, bran(Id_bran, deci(Id_deci, cond(Id_cond, Identifier))), Direction, Range_symb, Range_cons, Stmts, Flow) :-
%this is similar to the choose_truth /2 predicate, but it cannot be used because the range must be symbolically interpreted only once
%  in addition we are dealing with a range constraint (similar to choice being a range within a case statement;
%  see combine_or_else_case(not_last, ...) predicate above)
%      garbage_collect,
%        statistics(global_stack, [GS_used, _]),
%        common_util:common_util__error(1, "Info: GS used", no_error_consequences, [(gs_used, GS_used)], 157192, mika_symbolic, for_body, no_localisation, no_extra_info),
%        statistics(program, [PS_used, _]),
%        common_util:common_util__error(1, "Info: PS used", no_error_consequences, [(ps_used, PS_used)], 157392, mika_symbolic, for_body, no_localisation, no_extra_info),
        symbolically_interpret(Identifier, Identifier_symb, Identifier_cons, Identifier_type, _Exception_),    %need to get its new value
        (Identifier_type == 'unhandled_expression' ->
                common_util__error(10, "Unhandled entities in for body", no_error_consequences, no_arguments, 1071358, mika_symbolic, for_body, no_localisation, no_extra_info)
        ;
                (Choice_symb = is_in(Identifier_symb, Range_symb),
                 Choice_cons = is_in(Identifier_cons, Range_cons),
                 CombinationsL = [combination(cond(Id_cond, Choice_symb, Choice_cons, 'true'), 'true'), combination(cond(Id_cond, Choice_symb, Choice_cons, 'false'), 'false')],
                 single_combine(CombinationsL, 'deci', Id_deci, CombinationsL3),
                 single_combine(CombinationsL3, 'bran', Id_bran, CombinationsL_out),
                 mika_globals:mika_globals__get_NBT('driver', Driver),
                 (Driver == 'no_driver' ->                              %normal test inputs generation
                        (mika_globals:mika_globals__get_NBT('strategy', Kind),                  %global branch, decision or condition coverage desired
                         choose_combination(CombinationsL_out, Kind, Chosen_combination, Outcome)        %pick the best choice
                        )
                 ;
                  Driver = driver(_Target_subprogram_name_xref) ->      %we are in a driver mode
                        get_next_combination(CombinationsL_out, Chosen_combination, Outcome)        %pick the next combination, may have to be redone
                 ;
                        common_util:common_util__error(10, "Driver's value is unexpected", "Should never happen", [('driver', Driver)], 10135147, 'mika_symbolic', 'choose_truth', 'no_localisation', 'no_extra_info')
                 ),
                 traverse(Chosen_combination),
                 (Outcome == 'false' ->
                         Flow = 'carry_on'
                 ;
                  Outcome == 'true' ->
                        (exec(Stmts, Intermediate_flow),
                         (Intermediate_flow == 'exit' ->
                                Flow = 'carry_on'       %Exit indicates return statements within the for loop: so we 'carry_on' after the loop
                         ;
                          Intermediate_flow == 'return' ->
                                Flow = Intermediate_flow
                         ;
                          common_util:common_util__is_a_goto(Intermediate_flow) ->    %goto out of the loop
                                Flow = Intermediate_flow
                         ;
                          common_util:common_util__is_a_exit_named_loop(Intermediate_flow) ->
                                (Intermediate_flow = exit_named_loop(To_name),
                                 (To_name == Loop_name ->
                                        Flow = 'carry_on'
                                 ;
                                        Flow = Intermediate_flow
                                 )
                                )
                         ;
                          Intermediate_flow == 'carry_on' ->
                                %we need to increment the loop variable, that implies checking its own definition range first.
                                (copy_term(a(Range_cons), a(Range_consc)),      %21/-6/10 we make a copy because it is just a check : we do not wan tto change the range (only applies if it is dynamic)
                                 Range_consc = [Min_consc, Max_consc],

                                 (Direction == 'normal' ->
                                        Constraint = <(Identifier_cons, Max_consc)
                                 ;
                                  Direction == 'reverse' ->
                                        Constraint = >(Identifier_cons, Min_consc)
                                 ),
                                 (midoan_solver__sdl(Constraint) ->
                                        (mika_seav_atts:mika_seav_atts:mika_seav_atts__get(type, Identifier, Type_var),
                                         (Direction == 'normal' ->
                                                exec(assign(Identifier, indexed(tic(Type_var, 'succ'), [Identifier])), carry_on)
                                         ;
                                          Direction == 'reverse' ->
                                                exec(assign(Identifier, indexed(tic(Type_var, 'pred'), [Identifier])), carry_on)
                                         ),
                                         Iteration_1 is Iteration + 1,   %counting the number of iterations
                                         for_body(Iteration_1, Loop_name, bran(Id_bran, deci(Id_deci, cond(Id_cond, Identifier))), Direction, Range_symb, Range_cons, Stmts, Flow)
                                        )
                                ;
                                        (%We are out of the range : end of for loop
                                         mika_coverage:mika_coverage__add_to_current_path('condition', (Id_cond, 'false')),
                                         mika_coverage:mika_coverage__add_to_current_path('decision', (Id_deci, 'false')),
                                         mika_coverage:mika_coverage__add_to_current_path('branch', (Id_bran, 'false')),
                                         Flow = 'carry_on'
                                        )
                                 )
                                )
                         )
                        )
                 )
                )
        ).
%%%
loop_body(Iteration, _Loop_name, _, carry_on) :-
        maxLoopIterationReached(Iteration),     %upper limit for the number of iterations
        common_util__error(4, "We may be in an infinite loop or run the risk of reaching mika's limitations", "the solution given for this path (if any) may not be correct (limitations reached) or lead to infinite processing (infinite loop)", no_arguments, 4634271, mika_symbolic, loop_body, no_localisation, no_extra_info),
        !,
        fail.   % see [20/10/06] not logically correct! we force backtrack
loop_body(Iteration, Loop_name, Stmts, Flow) :-
        exec(Stmts, Intermediate_flow),
        (Intermediate_flow == 'exit' ->
                Flow = 'carry_on'
        ;
         Intermediate_flow == 'return' ->
                Flow = Intermediate_flow
        ;
         common_util:common_util__is_a_goto(Intermediate_flow) ->    %goto out of the loop
                Flow = Intermediate_flow
        ;
         common_util:common_util__is_a_exit_named_loop(Intermediate_flow) ->
                (Intermediate_flow = exit_named_loop(To_name),
                 (To_name == Loop_name ->
                        Flow = 'carry_on'
                 ;
                        Flow = Intermediate_flow
                 )
                )
        ;
         Intermediate_flow == 'carry_on' ->
                (Iteration_1 is Iteration + 1,
                 loop_body(Iteration_1, Loop_name, Stmts, Flow)
                )
        ).
%%%
while_body(Iteration, _Loop_name, _, _, 'carry_on') :-
        maxWhileIterationReached(Iteration),    %upper limit for the number of iterations
        %trace,
        common_util__error(4, "We may be in an infinite while loop or run the risk of reaching mika's limitations", "the solution given for this path (if any) may not be correct (limitations reached) or lead to infinite processing (infinite loop)", no_arguments, 4653271, mika_symbolic, while_body, no_localisation, no_extra_info),
        !,
        fail.   %not logically correct! we force backtrack
while_body(Iteration, Loop_name, bran(Id_bran, deci(Id_deci, Expression)), Stmts, Flow) :-
        choose_truth(bran(Id_bran, deci(Id_deci, Expression)), Outcome),
        (Outcome == 'true' ->
                (exec(Stmts, Intermediate_flow),
                 (Intermediate_flow == 'exit' ->
                        Flow = 'carry_on'
                 ;
                  Intermediate_flow == 'return' ->
                        Flow = Intermediate_flow
                 ;
                  common_util:common_util__is_a_goto(Intermediate_flow) ->    %goto out of the loop
                        Flow = Intermediate_flow
                 ;
                  common_util:common_util__is_a_exit_named_loop(Intermediate_flow) ->
                        (Intermediate_flow = exit_named_loop(To_name),
                         (To_name == Loop_name ->
                                Flow = 'carry_on'
                         ;
                                Flow = Intermediate_flow
                         )
                        )
                 ;
                  Intermediate_flow == 'carry_on' ->
                        (NIteration is Iteration +1,
                         while_body(NIteration, Loop_name, bran(Id_bran, deci(Id_deci, Expression)), Stmts, Flow)
                        )
                 )
                )
        ;
         Outcome == 'false' ->
                Flow = 'carry_on'               %end of iteration
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     END    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%