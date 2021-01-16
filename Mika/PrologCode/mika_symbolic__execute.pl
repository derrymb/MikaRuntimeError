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
% mika_symbolic__execute.pl
% module mika_symbolic
% implements the exec/2 predicate : symbolic execution of intermediate Ada code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%handles sequences of statement respecting the flow of the previously covered statement
exec_sequences_of_statements([], 'carry_on', _Local_labels).      %if we have reach beyond the last statement Flow is carry_on
exec_sequences_of_statements([Next|Rest], Flow, Local_labels) :-
        exec(Next, Intermediate_flow),
        (Intermediate_flow == 'carry_on' ->
                exec_sequences_of_statements(Rest, Flow, Local_labels)
        ;
         common_util:common_util__is_a_goto(Intermediate_flow) ->
                (Intermediate_flow = goto(Label),
                 (mika_coverage:mika_coverage__is_local_label(Local_labels, Label, Target_statements) ->
                        exec_sequences_of_statements(Target_statements, Flow, Local_labels)
                 ;
                        Flow = Intermediate_flow      %propagated upwards: cover_sequences_of_statements terminates and so does the initial enclosing cover(stmts(Statements))  which will therefore return control here
                 )
                )
        ;
                Flow = Intermediate_flow    %exit|return|exit_named_loop(To_name)|exception(Name, Arguments) and the rest is not covered
        ).
%%%
%The second argument of exec/2 is an indication of the control flow.
%  it can have the following values : carry_on|goto(Label)|exit|return|exit_named_loop(To_name)|exception(Name, Arguments)
%symbolic interpretation of declarations and statements: it is used during elaboration and during 'normal' execution
%%%
exec([mika_ref(_)], 'carry_on') :-    %all the cross-references : used during elaboration only
        !.      %needed
%%%
%this is needed 21/01/11; handles sequences things not necessarily statements : of top level entries in foo.pl, declarations etc. only
exec([], 'carry_on').
exec([Next|Rest], Flow) :-
        exec(Next, Intermediate_flow),
        (Intermediate_flow == 'carry_on' ->
                exec(Rest, Flow)           %we carry on
        ;
                common_util__error(10, "Intermediate flow is not carry_on", "Should never happen", [(intermediate_flow, Intermediate_flow)], 102608, mika_symbolic, exec, no_localisation, "Contact Midoan today")
        ).

exec(stmts(Statements), Flow) :-
        mika_coverage:mika_coverage__search_local_labeled_statements(Statements, Local_labels),
        exec_sequences_of_statements(Statements, Flow, Local_labels).

%%%
exec('nothing', 'carry_on').                        %nothing done as nothing to do
exec('empty_body', 'carry_on').                     %nothing done as nothing to do
exec('empty_declarative_part', 'carry_on').         %nothing done as nothing to do
exec(atomic_pragma(_Pragma), 'carry_on').         %we currently do nothing

%Local_entity's behaviour needs to be defined here by us because it is imported
exec(pragma('import', [_Convention, Local_entity|Rest]), 'carry_on') :-   %we only support positional version of this pragma
        !,
        (mika_sub_atts:mika_sub_atts__is_sub_atts(Local_entity) ->
                 mika_sub_atts:mika_sub_atts__update('status', Local_entity, imported)    %imported subprogram will be handled the first time it is called; that's a better way [13/03/08]
        ;
         mika_seav_atts:mika_seav_atts__is_seav(Local_entity) ->
                (mika_seav_atts:mika_seav_atts__get(name, Local_entity, Name),
                 (Rest = [string(External_name)|_] ->
                        true
                 ;
                        common_util:common_util__error(10, "Import pragma format is not cupported", 'no_error_consequences', [('name', Name)], 446931, 'mika_symbolic', 'exec', 'exec(pragma(import, b))', 'no_extra_info')
                 ),
                 atom_codes(External_name_atom, External_name),
                 (External_name_atom == '__gl_zero_cost_exceptions' ->         %would be better if that could be read at run time in the same way as gnat's specific attributes are
                        (common_util__error(4, "Imported object zero_cost_exceptions given guessed value", "May be given wrong value", [(value_given, 1)], 44818, mika_symbolic, exec, exec(pragma(import, b)), no_extra_info),
                         exec(assign(Local_entity, 1), 'carry_on')
                        )
                 ;
                  External_name_atom == '__gl_leap_seconds_support' ->         %would be better if that could be read at run time in the same way as gnat's specific attributes are
                        (common_util__error(4, "Imported object leap_seconds_support given guessed value", "May be given wrong value", [(value_given, 0)], 44818, mika_symbolic, exec, exec(pragma(import, b)), no_extra_info),
                         exec(assign(Local_entity, 0), 'carry_on')
                        )
/*                  ;
                  Id == wc_encoding ->         %would be better if that could be read at run time in the same way as gnat's specific attributes are
                      todo  (common_util__error(4, "Imported object wc_encoding given guessed value", "May be given wrong value", [(value_given, 'b')], 44818, mika_symbolic, exec, exec(pragma(import, b)), no_extra_info),
                         exec(assign(Local_entity, 1), carry_on)
                        )
*/                 ;
                        (common_util__error(4, "Unhandled entity during execution: imported object is unknown", "Entity will be unhandled", [(entity, Name)], 44540, mika_symbolic, exec, exec(pragma(import, b)), no_extra_info),
                         mika_seav_atts:mika_seav_atts__unput(Local_entity, _),
                         mika_unhandled_atts:mika_unhandled_atts__create(Local_entity, Name, unidentified_imported_object)
                        )
                 )
                )
        ;
                common_util__error(10, "Imported entity is unknown: not a subprogram nor an object", no_error_consequences, [(entity, Local_entity)], 105930, mika_symbolic, exec, exec(pragma(import, ...)), "Is unexpected")
        ).

exec(pragma(_Pragma, _ArgumentsL), carry_on).   %we currently do nothing for other pragmas

exec(use_clause, carry_on).			%nothing done as nothing to do
%28/01/08 we handle intrinsics here
%handling them here is quite limiting: only works because there are no external entities mentionned in their definition and body; also no automatic cfg for it ...
% we could have tried to write an intrinsic.adb and .abs and parse it etc.: that solution would be more powerful as would allow crossreferencing of external entities.
%04/11/09 but at least in the case of unchecked_conversion : the semantics cannot be written in Ada ... hence we 'break on through' (the door ...)
exec(is_intrinsic(Name, Intrinsic_variable), carry_on) :-       %is_intrinsic/2 is generated during parsing
        (Name == unchecked_conversion ->
                (%should be a generic variable
                 %do exec of generic and exe of subprogram etc.
                 mika_name_atts:mika_name_atts__create(Source, source_unchecked_conversion_intrinsic),
                 mika_name_atts:mika_name_atts__create(Target, target_unchecked_conversion_intrinsic),
                 mika_name_atts:mika_name_atts__create(S, s_unchecked_conversion_intrinsic),
                 %Return vars are not xrefed : not a name_atts var
                 exec(generic_subprogram_specification([generic,
                                                        type(Source, box, private, not_tagged, limited),
                                                        type(Target, box, private, not_tagged, limited)],
                                                        function_body(Intrinsic_variable, Return_Unchecked_conversion, subtype_indication(may_be_null, Target, no_constraint),
                                                                      parameters([param([S], in, subtype_indication(may_be_null, Source, no_constraint), no_init)])
                                                                    )
                                                      )
                      , carry_on),
                 exec(subprogram_body(nothing,
                                      function_body(Intrinsic_variable, Return_Unchecked_conversion, subtype_indication(may_be_null, Target, no_constraint), parameters([param([S], in, subtype_indication(may_be_null, Source, no_constraint), no_init)]),
                                      local_decl([empty_declarative_part]),
                                      body(stmts([break_on_through(unchecked_conversion(S, Source, Return_Unchecked_conversion, Target))]), no_exceptions)      %[04/11/09] semantics cannot be written in Ada : use break_on_through to allow pure Prolog code to be executed
                                     ))
                      , carry_on)
                )
        ;
         Name == unchecked_deallocation ->
                (%should be a generic variable
                 %do exec of generic and exe of subprogram etc.
                 mika_name_atts:mika_name_atts__create(Object, object_unchecked_deallocation_intrinsic),
                 mika_name_atts:mika_name_atts__create(Name_2, name_unchecked_deallocation_intrinsic),
                 mika_name_atts:mika_name_atts__create(X, x_unchecked_deallocation_intrinsic),
                 %Return vars are not xrefed : not a name_atts var
                 exec(generic_subprogram_specification([generic,
                                                        type(Object, box, private, not_tagged, limited),
                                                        type(Name_2, no_discriminant, access, subtype_indication(may_be_null, Object, no_constraint))],
                                                       procedure_body(Intrinsic_variable, no_return,
                                                                      parameters([param([X], in_out, subtype_indication(may_be_null, Name_2, no_constraint), no_init)])
                                                                     )
                                                      )
                      , carry_on),
                 exec(subprogram_body(nothing,
                                      procedure_body(Intrinsic_variable, no_return, parameters([param([X], in_out, subtype_indication(may_be_null, Name_2, no_constraint), no_init)]),
                                      local_decl([empty_declarative_part]),
                                      body(stmts([null]), no_exceptions)
                                     ))
                      , carry_on)
                )
        ;
                common_util__error(10, "Unhandled intrinsic", "Cannot proceed", [(name, Name)], 10113102, mika_symbolic, exec, exec(is_intrinsic(a, b)), "Should never happen")
        ).

%%%
%to handle non Ada semantics to be executed (e.g. for the intrinsic unchecked_conversion function)
exec(break_on_through(Compound), Flow) :-
        (Compound = unchecked_conversion(Source, _Source_typemark, Target, Target_typevar) ->
                (%we are within a call to unchecked_conversion : Source has been declared as a SEAV of type Source_type and Target has been declared as a SEAV of type Target_type
                 symbolically_interpret(Source, Source_symb, Source_const, Source_type, _Exception_),
                 symbolically_interpret(Target_typevar, _Target_typemark, Const, 'type', _Exception_),         %obtaining the actual typevar of the type
                 midoan_solver__interpret(unchecked_conversion(Const, Source_const), types(Source_type), Constraint, _Type, _Exception_),
                 update_var_on_assignement(Target, unchecked_conversion(Source_symb), Constraint),      %but really this is incorrect: subsequent uvoa_check_range should not be performed
                 Flow = return
                )
        ;
                common_util__error(10, "Unhandled break on through", "Cannot proceed", [(compound, Compound)], 1015715, mika_symbolic, exec, exec(break_on_through(a)), "Should never happen")
        ).

%%%
%for Ada rule 11.1
exec(exception_declaration(Exception_idL), carry_on) :-
        %commented ou 18/04/08 !,
        common_util__error(4, "Exception declarations are currently ignored", no_error_consequences, [(exception_idL, Exception_idL)], 4120143, mika_symbolic, exec, no_localisation, no_extra_info).

%%%
%for Ada Rule 8.5.1
%!!!! see ada.y for remarks regarding parsing : more tolerant than the Ada RM
exec(rename(Object_id, subtype_indication(_Null_exclusion, Subtype_var, no_constraint), Exp), carry_on) :-
        %we cannot treat this as an assignment (e.g. by calling exec(object(constant, [Object_id], Subtype_indication, Exp), carry_on))
        % because if it stands for a variable and be assigned later on the change should be propagated to the original expression
        % as in 'RenameOfJ := RenameOfJ + 2;' where J should be changed
        %cannot directly call symbolically_interpret on Exp either because if it is known it reduces to the expression an we loose th reference of the original variable
        % e.g. 'RenameOfJ : Integer renames ch12_6.J;' here Exp will symbolically interpret to '0' if J has that value but later in
        % 'RenameOfJ := RenameOfJ + 2;' RenameOfJ will simply be 2, the reference back to J will be lost
        %probably need something like syntactically_denote or similar to in_out parameter passing in procedures
        (mika_name_atts:mika_name_atts__is_name_atts(Object_id) ->
                 mika_name_atts:mika_name_atts__unput(Object_id, Object_id_name)			%the attribute is removed
        ;
         mika_seav_atts:mika_seav_atts__is_seav(Object_id) ->
                 mika_seav_atts:mika_seav_atts__unput(Object_id, Object_id_name)			%the attribute is removed
        ;
                common_util__error(10, "Renamed variable is of the wrong type", "Cannot proceed", [(object_id, Object_id)], 10137102, mika_symbolic, exec, exec(rename(a, b)), "Should never happen:  not a name_atts nor a seav_atts (2nd time seen because private or incomplete)")
        ),
        symbolically_interpret(Subtype_var, Subtype_name, _, Subtype_type, _Exception_),
        (Subtype_type == type ->
                (freeze_arguments([Exp], [Exp_constraint]),
                 check_for_unhandled(Exp_constraint, Has_unhandled),
                 (Has_unhandled == no ->
                        Object_id = Exp_constraint
                 ;
                        create_unhandled(Object_id, rename, "Renamed object contains unhandled entities")
                 )
                )
        ;
                common_util__error(10, "Renaming declaration for objects : the subtype given is not a type", "Cannot proceed", [(subtype_name, Subtype_name)], 10147171, mika_symbolic, exec, exec(rename(a, b)), "Should never happen")
        ).
%%%
%for Ada rule 8.5.2
exec(rename_exception(Exception_idL, New_name), carry_on) :-
        common_util__error(4, "Exception renaming declarations are currently ignored", no_error_consequences, [(exception_idL, Exception_idL), (new_name, New_name)], 4154173, mika_symbolic, exec, no_localisation, no_extra_info).

%%%
exec(generic_package_rename(Generic_compound_name_exp, New_name), carry_on) :-
        common_util__error(4, "Generic package renaming are currently ignored", no_error_consequences, [(generic_compound_name_exp, Generic_compound_name_exp), (new_name, New_name)], 415828, mika_symbolic, exec, no_localisation, no_extra_info).

%%%
exec(generic_subprogram_rename(Generic_compound_name_exp, New_name), carry_on) :-
        common_util__error(4, "Generic subprogram renaming are currently ignored", no_error_consequences, [(generic_compound_name_exp, Generic_compound_name_exp), (new_name, New_name)], 416234, mika_symbolic, exec, no_localisation, no_extra_info).

%%%
%for Ada Rule 8.5.3
exec(package_rename(Compound_name_exp, Package_exp), carry_on) :-
        handle_selected_name(Compound_name_exp, New_package_var),
        mika_name_atts:mika_name_atts__unput(New_package_var, _),                 %the attribute is removed
        handle_selected_name(Package_exp, Old_package_var),
        (mika_package_atts:mika_package_atts__is_package_atts(Old_package_var) ->
                (mika_package_atts:mika_package_atts__get(Old_package_var, 'name', Xref_name),      %the xref ada name of the old package
                 mika_package_atts:mika_package_atts__create(New_package_var, Xref_name)          %a package atts var is created just for subsequent tracking with the old package name attached
                )
        ;
                common_util__error(10, "In package renaming, the old package is not a package", "Cannot proceed", [(package, package_rename(Compound_name_exp, Package_exp))], 10167180, mika_symbolic, exec, exec(package_rename(a, b)), "Should never happen")
        ).
%%%
%for Ada Rule 8.5.4, see Barnes 12.6
exec(subprogram_rename(_Overriding_indicator_opt, Decl, Name_exp), carry_on) :-
        %commented ou 18/04/08 !,
        %30/01/08
        %two different kinds of subprogram renaming: renaming as spec (the subprogram was not previously declared),
        %                                            renaming as body (the subprogram was previously declared: the renaming provides the body)
        %to differentiate between the two we use the Compound_name
        handle_selected_name(Name_exp, Old_sub_var),
        (Decl = procedure_body(Compound_name, no_return, parameters(Params)) ->
                (Return_var = no_return,
                 Return_type = no_return
                )
        ;
         Decl = function_body(Compound_name, _Return, Return_type, parameters(Params)) ->
                true
        ),
        handle_compound_name(Compound_name, Sub_var),   %needed?
        (mika_name_atts:mika_name_atts__is_name_atts(Sub_var) ->      %it has not been declared: not part of the specification : renaming as spec
                (mika_name_atts:mika_name_atts__unput(Sub_var, Name),
                 (Return_var == no_return ->
                        true
                 ;
                        (atom_concat(Name, '_return', Name_atom),
		         mika_name_atts:mika_name_atts__create(Return_var, Name_atom)	%messy but we need the artificial return variable to be a mika_name_atts variable
                        )
                 ),
                 (mika_sub_atts:mika_sub_atts__is_sub_atts(Old_sub_var) ->
                        (mika_sub_atts:mika_sub_atts__get(params, Old_sub_var, Old_params),
                         (check_params_matching(Old_params, Params) ->    %06/05/08 and 08/03/10     %may not match due to problem in .ali which seems to occur only for unary, binary couples (i.e. +, and -)
                                mika_sub_atts:mika_sub_atts__create(Sub_var, Name, rename_of(Old_sub_var), Return_var, Return_type, Params, not_available, not_available, [])        %the body is not yet available for renaming as spec : will be set during handle_subprogram_call by create_subprogram_rename
                         ;
                                (common_util__error(2, "In subprogram rename, parameters do not match: .ali confuses binary and unary operators", "Logically the predefined operator is used", [(name, Name)], 226406, mika_symbolic, exec, exec(subprogram_rename(a, b, c)), no_extra_info),
                                 %we have a failure in the parameter matching of the Old_sub_var with the new Sub_var: the operator is actually predefined
                                 mika_symbolic__parse(Name, _, _, _, _, Id),
                                 user:nsn_transform_for_operators(_, Id, Operator),   %e.g. "-"
                                 atom_codes(Operator, [34|Op_start]),
                                 append(Op_code, [34], Op_start),
                                 !,
                                 atom_codes(Op, Op_code),
                                 mika_sub_atts:mika_sub_atts__create(Sub_var, Name, rename_of_default(Op), _Return_var, _Return_type, _Params, not_available, not_available, [])        %default operator : will be set during handle_subprogram_call by create_subprogram_rename
                                )
                         )
                        )
                 ;
                  mika_seav_atts:mika_seav_atts__is_seav(Old_sub_var) ->       %14/01/09 could be the rename of an enumeration literal (in this case the function is parameterless)
                         Sub_var = Old_sub_var                  %simple unification here
                 )
                )
        ;
         mika_sub_atts:mika_sub_atts__is_sub_atts(Sub_var) ->        %it has been previously declared : renaming as body
                (mika_sub_atts:mika_sub_atts__get(name, Sub_var, Name),
                 mika_sub_atts:mika_sub_atts__get(params, Sub_var, Declaration_params),       %[10/03/08] we retrieve the original Params as declared (because due to a gnatxref quirk) it is those that are used during named parameter subprogram call
                 mika_sub_atts:mika_sub_atts__unput(Sub_var),                                 %[28/04/09] but newer versions of gnatxref do not seem to have this bug so we modified match_renamed_params/2
                 (Return_var == no_return ->
                        true
                 ;
                        (atom_concat(Name, '_return', Name_atom),
		         mika_name_atts:mika_name_atts__create(Return_var, Name_atom)	%messy but we need the artificial return variable to be a mika_name_atts variable
                        )
                 ),
                 match_renamed_params(Declaration_params, Params),
                 mika_sub_atts:mika_sub_atts__create(Sub_var, Name, renaming_as_body, Return_var, Return_type, Declaration_params, not_available, not_available, []),            %the body will be immediately set by create_subprogram_rename
                 create_subprogram_rename(Sub_var, Old_sub_var)
                )
        ).
%%%
exec(generic_package_specification(Generic_formal_part, Package_specification), carry_on) :-
        Package_specification = package_specification(Compound_name, _Locals, _Private),
        handle_compound_name(Compound_name, Generic_var),
        mika_name_atts:mika_name_atts__unput(Generic_var, Name),
        mika_generic_atts:mika_generic_atts__create(Generic_var, Name, package, Generic_formal_part, Package_specification).
%%%
exec(generic_package_instantiation(Compound_name, _Generic_inst), carry_on) :-
        handle_compound_name(Compound_name, Package_name),
        mika_name_atts:mika_name_atts__unput(Package_name, Name),
        mika_unhandled_atts:mika_unhandled_atts__create(Package_name, Name, generic_package),
        common_util__error(4, "Generic package instantiation are currently ignored", no_error_consequences, [(name, Name)], 4259123, mika_symbolic, exec, no_localisation, no_extra_info).
%%%
%e.g. from
%generic
%  type Item is private;
%  procedure sSwap(X, Y : in out Item);
% we get:
%generic_subprogram_specification([generic,
%                                  type(Item_360, no_discriminant, private, not_tagged, not_limited)
%                                 ],
%                                 procedure_body(
%                                   Sswap_368, no_return,
%                                   parameters([param([X_376, Y_379], in_out, Item_360, no_init)])
%                                 )
%                                )
%Generic_formal part is of the form [generic, param([ids], mode, mark, init)*, type(name, discriminant, generic_type_definition)*]
%incomplete : possibly other things.
%Subprogram_specification is of the normal form
exec(generic_subprogram_specification(Generic_formal_part, Subprogram_specification), carry_on) :-
        (Subprogram_specification = procedure_body(Compound_name, no_return, _Params) ->
		true
        ;
         Subprogram_specification = function_body(Compound_name, _Return, _Return_type, _Params) ->
		true
        ),
        handle_compound_name(Compound_name, Generic_var),
        mika_name_atts:mika_name_atts__unput(Generic_var, Name),
        mika_generic_atts:mika_generic_atts__create(Generic_var, Name, subprogram, Generic_formal_part, Subprogram_specification).

%%%
%e.g. from 'procedure swap_fl is new sswap(my_float);' in ada
%generic_subprogram_instantiation(
%  procedure_body(
%    Swap_fl_369,
%    no_return,
%    parameters([])
%  ),
%  indexed(Sswap_368, [My_float_365])
%),
exec(generic_subprogram_instantiation(_Overriding_indicator_opt, Subprogram_specification, Generic_inst), carry_on) :-
        %trace,
        (Subprogram_specification = procedure_body(Compound_name, no_return, parameters([])) ->
		true
        ;
         Subprogram_specification = Compound_name ->    %it is a function
		true
        ),
	handle_compound_name(Compound_name, New_subprogram),
        (mika_name_atts:mika_name_atts__is_name_atts(New_subprogram) ->
                mika_name_atts:mika_name_atts__get(name, New_subprogram, _New_subprogram_name)
        ;
                common_util__error(10, "The new subprogram is not a name", "Cannot proceed", [(subprogram_specification, Subprogram_specification)], 10311155, mika_symbolic, exec, exec(generic_subprogram_instantiation(a, b, c)), "Should never happen")
        ),
        handle_generic_subprogram_instantiation(New_subprogram, Generic_inst). %creates a new subprogram based on the generic instantiation

%%%
exec(package_specification(Compound_name, local_decl(Local_decls), Private), 'carry_on') :-
        handle_compound_name(Compound_name, Package_var),
        mika_name_atts:mika_name_atts__unput(Package_var, Xref_name),
        mika_globals:mika_globals__set_NBT('debug_info', package_specification(Xref_name)),
        %(Xref_name = 's-stalib.ads:56:16:standard_library' ->
	%	trace
	%;
	%	true
	%),
        mika_package_atts:mika_package_atts__create(Package_var, Xref_name),          %a package atts var is created just for subsequent tracking
        exec(Local_decls, 'carry_on'),    %must be carry on : no return nor exit allowed in declarations
        (Private == 'no_private' ->
                true            %nothing done as nothing to do
        ;
         Private = private(Private_decls) ->
                 exec(Private_decls, 'carry_on')  %must be carry on : no return nor exit allowed in declarations
        ).

%%%
exec(package_body(Compound_name, local_decl(Local_decls), Body), 'carry_on') :-
        handle_compound_name(Compound_name, Package_var),
        (mika_generic_atts:mika_generic_atts__is_generic_atts(Package_var) -> %it is the body of a generic package
                mika_generic_atts:mika_generic_atts__set_body(Package_var, package_body(Package_var, local_decl(Local_decls), Body))
                %!! care in the above Package_var is recursively mentionned !!
                %that's all there is to do at elaboration time for a generic package
        ;
         mika_package_atts:mika_package_atts__is_package_atts(Package_var) ->    %elaboration takes place
                (mika_package_atts:mika_package_atts__get(Package_var, 'name', Xref_name),
                 mika_globals:mika_globals__set_NBT('debug_info', package_body(Xref_name)),
                 exec(Local_decls, 'carry_on'),   %must be carry on : no return or exit allowed in declarations
                 exec(Body, 'carry_on')           %actual elaboration of a package body : the statements are executed
                )
        ;
                common_util__error(10, "The package body has an unknown name", "Cannot proceed", [('name', Compound_name)], 10351128, mika_symbolic, exec, exec(package_body(a, b, c)), "Should never happen")
        ).
%%%
%Exception_handler_part can be 'no_exceptions' or 'exception_handler(HandlerL)'
exec(body(Stmts, Exception_handler_part), Flow) :-
        (Exception_handler_part = exception_handler(HandlerL)  ->
                common_util__error(4, "Exception handlers are currently ignored", no_error_consequences, [(handlerl, HandlerL)], 4358145, mika_symbolic, exec, no_localisation, no_extra_info)
        ;
         Exception_handler_part = no_exceptions ->
                true    %nothing done as nothing to do
        ),
        exec(Stmts, Flow).

%%%
%creates a mika_sub_atts variable
exec(subprogram_body(_Overriding_indicator_opt, procedure_body(Compound_name, no_return, parameters(Params), local_decl(Decls), Body)), carry_on) :-
        !,      %just for efficiency : the indexing is not perfect
        handle_compound_name(Compound_name, Sub_var),
        (mika_generic_atts:mika_generic_atts__is_generic_atts(Sub_var) -> %it is the body of a generic procedure
                mika_generic_atts:mika_generic_atts__set_body(Sub_var, procedure_body(Sub_var, no_return, parameters(Params), local_decl(Decls), Body))
                %!! care in the above Name is recursively mentionned !!
                %that's all there is to do at elaboration time for a generic procedure
        ;
                ((mika_name_atts:mika_name_atts__is_name_atts(Sub_var) ->      %it has not been declared: not part of the specification
                        (mika_name_atts:mika_name_atts__unput(Sub_var, Name),
                         mika_sub_atts:mika_sub_atts__create(Sub_var, Name, 'bodied', 'no_return', 'no_return', Params, Decls, Body, [])
                        )
                 ;
                  mika_sub_atts:mika_sub_atts__is_sub_atts(Sub_var) ->        %it has been previously declared
                        (mika_sub_atts:mika_sub_atts__get('name', Sub_var, Name),
                         mika_sub_atts:mika_sub_atts__get('delayed_calls', Sub_var, Delayed_calls),
                         mika_sub_atts:mika_sub_atts__unput(Sub_var),
                         mika_sub_atts:mika_sub_atts__create(Sub_var, Name, 'bodied', 'no_return', 'no_return', Params, Decls, Body, []),
                         execute_delayed_calls(Delayed_calls, Sub_var)
                        )
                 )
                )
        ).

%%%
%creates a mika_sub_atts variable
exec(subprogram_body(_Overriding_indicator_opt, function_body(Compound_name, Return, Return_type, parameters(Params), local_decl(Decls), Body)), carry_on) :-
        !,      %just for efficiency : the indexing is not perfect
        handle_compound_name(Compound_name, Sub_var),
        (mika_generic_atts:mika_generic_atts__is_generic_atts(Sub_var) -> %it is the body of a generic function
                mika_generic_atts:mika_generic_atts__set_body(Sub_var, function_body(Sub_var, Return, Return_type, parameters(Params), local_decl(Decls), Body))
                %!! care in the above Name is recursively mentionned !!
                %that's all there is to do at elaboration time for a generic procedure
        ;
                ((mika_name_atts:mika_name_atts__is_name_atts(Sub_var) ->      %it has not been declared: not part of the specification
                        (mika_name_atts:mika_name_atts__unput(Sub_var, Name),
                         atom_concat(Name, '_return', Name_atom),
		         mika_name_atts:mika_name_atts__create(Return, Name_atom),	%messy but we need the artificial return variable to be a mika_name_atts variable
                         mika_sub_atts:mika_sub_atts__create(Sub_var, Name, 'bodied', Return, Return_type, Params, Decls, Body, [])
                        )
                 ;
                  mika_sub_atts:mika_sub_atts__is_sub_atts(Sub_var) ->        %it has been previously declared
                        (mika_sub_atts:mika_sub_atts__get(name, Sub_var, Name),
                         atom_concat(Name, '_return', Name_atom),
		         mika_name_atts:mika_name_atts__create(Return, Name_atom),	%messy but we need the artificial return variable to be a mika_name_atts variable
                         mika_sub_atts:mika_sub_atts__get('delayed_calls', Sub_var, Delayed_calls),
                         mika_sub_atts:mika_sub_atts__unput(Sub_var),
                         mika_sub_atts:mika_sub_atts__create(Sub_var, Name, 'bodied', Return, Return_type, Params, Decls, Body, []),
                         execute_delayed_calls(Delayed_calls, Sub_var)
                        )
                 )
                )
        ).
%%%
%changed 30/01/08
exec(subprogram_declaration(_Overriding_indicator_opt, Decl), carry_on) :-
        (Decl = procedure_body(Compound_name, no_return, parameters(Params)) ->
                (Return_var = no_return,
                 Return_type = no_return
                )
        ;
         Decl = function_body(Compound_name, Return_var, Return_type, parameters(Params)) ->
                true
        ),
        handle_compound_name(Compound_name, Sub_var),
        mika_name_atts:mika_name_atts__unput(Sub_var, Name),
        mika_sub_atts:mika_sub_atts__create(Sub_var, Name, declared, Return_var, Return_type, Params, _, _, []).  %status is set to declared, we keep track of Params [10/03/08] for re-use in case will be renamed as boby later
                                                                                                % we keep track of Return_var and Return_type [13/03/08] for re-use in case it will be an imported subprogram later

exec(abstract_subprogram_declaration(_Overriding_indicator_opt, _), carry_on) :-
        common_util__error(6, "Abstract subprogram declarations are ignored", "Cannot be relied upon", no_arguments, 643330, mika_symbolic, exec, exec(abstract_subprogram_declaration(a, b), c), "Not yet implemented: should probably create an unhandled var in the mean time").

exec(null_prodecure_declaration(_Overriding_indicator_opt, _Decl), carry_on) :-
        common_util__error(6, "Null procedure declarations are ignored", "Cannot be relied upon", no_arguments, 6456117, mika_symbolic, exec, exec(null_prodecure_declaration(a, b), c), "Not yet implemented: should probably create an unhandled var in the mean time").

%%%
%a derived type
%For Ada rule 3.4
%New_type_var is a simple non_defining_identifier
%uses subtype_indication
exec(type(New_type_var, no_discriminant, New_or_abstract_new), carry_on) :-
        (New_or_abstract_new = new(derived_type_definition(_Private_type_kind, Subtype_indication)) ; New_or_abstract_new = abstract_new(derived_type_definition(_Private_type_kind, Subtype_indication))),
        !,      %just for efficiency : the indexing is not perfect
        handle_intermediate_types(Subtype_indication, Intermediate_type_var, Intermediate_type_name),
        check_for_unhandled(type(New_type_var, _Private_type_kind, Intermediate_type_var), Has_unhandled),
        (Has_unhandled == no ->
                (handle_intermediate_type_var(New_type_var, _New_type_name),
                 midoan_type:midoan_type__create_derived(Intermediate_type_name, Intermediate_type_var, New_type_var)
                )
        ;
                create_unhandled(New_type_var, derived_type, "Unhandled entity in exec/1: derived type declaration has unhandled entities")
        ).

% For Ada Rule 7.3
exec(type(New_type_var, no_discriminant, New_or_abstract_new), carry_on) :-
        (New_or_abstract_new = new(derived_type_definition(_Private_type_kind, Subtype_indication, _Interface_list_item, with_private)) ; New_or_abstract_new = abstract_new(derived_type_definition(_Private_type_kind, Subtype_indication, _Interface_list_item, with_private))),
        !,
        check_for_unhandled(type(New_type_var, no_discriminant, new(derived_type_definition(_Private_type_kind, Subtype_indication, _Interface_list_item, with_private))), Has_unhandled),
        (Has_unhandled == no ->
	        create_unhandled(New_type_var, derived_type_with_private, "Warning: derived type declaraction with private in exec/1 are currently ignored (declared an unhandled)")
        ;
                create_unhandled(New_type_var, derived_type_with_private, "Unhandled entity in exec/1: derived type with private declaration has unhandled entities")
        ).

%For Ada rule 3.4
exec(type(New_type_var, no_discriminant, New_or_abstract_new), carry_on) :-
        (New_or_abstract_new = new(derived_type_definition(_Private_type_kind, Subtype_indication, _Interface_list_item, record(Component_list))) ; New_or_abstract_new = abstract_new(derived_type_definition(_Private_type_kind, Subtype_indication, _Interface_list_item, record(Component_list)))),
        !,
        check_for_unhandled(type(New_type_var, no_discriminant, new(derived_type_definition(_Private_type_kind, Subtype_indication, _Interface_list_item, record(Component_list)))), Has_unhandled),
        (Has_unhandled == no ->
	        create_unhandled(New_type_var, derived_type_record, "Warning: derived record type declaraction in exec/1 are currently ignored (declared an unhandled)")
        ;
                create_unhandled(New_type_var, derived_type_record, "Unhandled entity in exec/1: derived record type declaration has unhandled entities")
        ).

%%%
%30/01/08: special catch type(Sa_12727, no_discriminant, integer, Integer_258, range([ - (2 ** Asiz_1559), indexed(String_45_468,[2 ** Asiz_1559, 1])])),
% due to gnatxref error: total hack, in 'doc_issues.odt'; very inneficient way to catch it
%24/05/10 : toco check if this is still relevant
exec(type(Type_var, no_discriminant, 'integer', Integer_type_var, range([Min_exp, indexed(_, [L, R])])), carry_on) :-
        mika_name_atts:mika_name_atts__is_name_atts(Type_var),
        mika_name_atts:mika_name_atts__get(name, Type_var, Name),
        mika_symbolic__parse(Name, 's-auxdec', 'adb', _, _, 'sa'),       %any line any column
        !,              %absolutely necessary here
        exec(type(Type_var, no_discriminant, integer, Integer_type_var, range([Min_exp, L - R])), carry_on).

%range is static Cf rule 3.5.4 Ada RM (static expression is explained in 4.9 of Ada RM)
%10/03/05 the range must be of integers
exec(type(Type_var, 'no_discriminant', 'integer', _Integer_type_var, range([Min_exp, Max_exp])), carry_on) :-
        !,
        check_for_unhandled(type(Type_var, Min_exp, Max_exp), Has_unhandled),
        (Has_unhandled == no ->
	        (handle_intermediate_type_var(Type_var, Type_name),
                 symbolically_interpret(Min_exp, _, Min_cons, Type_min, _Exception_),
                 symbolically_interpret(Max_exp, _, Max_cons, Type_max, _Exception_),
	         ((Type_min == unhandled_expression ; Type_max == unhandled_expression) ->
                        create_unhandled(Type_var, integer_type, "Integer type declaration range has unhandled entities")
                  ;
                        midoan_type:midoan_type__create_type(Type_name, 'integer', range_bounds(Min_cons, Max_cons), Type_var)
                  )
                )
        ;
                create_unhandled(Type_var, integer_type, "Unhandled entity in exec/1: integer type declaration has unhandled entities")
        ).

%
exec(type(Type_var, no_discriminant, integer, Standard_integer_type_var, mod(Mod_exp)), carry_on) :-
        !,
        check_for_unhandled(type(Type_var, Standard_integer_type_var, Mod_exp), Has_unhandled),
        (Has_unhandled == no ->
	        (handle_intermediate_type_var(Type_var, Type_name),
                 symbolically_interpret(Mod_exp, _, Mod_cons, Type_mod, _Exception_),
                 (Type_mod == unhandled_expression ->
                        create_unhandled(Type_var, modular_integer_type, "Modular integer type declaration expression has unhandled entities")
                 ;
                        midoan_type:midoan_type__create_type(Type_name, modular_integer, Standard_integer_type_var, Mod_cons, Type_var)
                 )
                )
        ;
                create_unhandled(Type_var, modular_integer_type, "Unhandled entity in exec/1: modular integer type declaration has unhandled entities")
        ).

%for Ada rule 3.5.7
exec(type(Type_var, no_discriminant, float, _Standard_float_type_var, digits(Digits_exp, range([Min_exp, Max_exp]))), carry_on) :-
        !,
        check_for_unhandled(type(Type_var, Digits_exp, Min_exp, Max_exp), Has_unhandled),
        (Has_unhandled == no ->
	        (handle_intermediate_type_var(Type_var, Type_name),
	         symbolically_interpret(Min_exp, _, Min_cons, Type_min, _Exception_),
	         symbolically_interpret(Max_exp, _, Max_cons, Type_max, _Exception_),
                 symbolically_interpret(Digits_exp, _, Digits_cons, Type_digits, _Exception_),
	         ((Type_min == unhandled_expression ; Type_max == unhandled_expression ; Type_digits == unhandled_expression) ->
                        create_unhandled(Type_var, float_type_with_digits_and_range, "Float type with digits and range declaration has unhandled entities")
                 ;
	                midoan_type:midoan_type__create_type(Type_name, float, digits(Digits_cons), range_bounds(Min_cons, Max_cons), Type_var)
                 )
                )
        ;
                create_unhandled(Type_var, float_type_with_digits_and_range, "Unhandled entity in exec/1: float type with digits and range declaration has unhandled entities")
        ).

%for Ada rule 3.5.7
exec(type(Type_var, no_discriminant, float, Standard_float_type_var, digits(Digits_exp, empty_range)), Flow) :-
        !,
        symbolically_interpret(tic(Standard_float_type_var, first), _, Min_exp, _, _Exception_),
        symbolically_interpret(tic(Standard_float_type_var, last), _, Max_exp, _, _Exception_),
        exec(type(Type_var, no_discriminant, float, Standard_float_type_var, digits(Digits_exp, range([Min_exp, Max_exp]))), Flow).

%for Ada rule 3.5.9
exec(type(Type_var, no_discriminant, fixed, _Standard_float_type_var, delta(Delta_exp, range([Min_exp, Max_exp]))), carry_on) :-
	!,
        check_for_unhandled(type(Type_var, Delta_exp, Min_exp, Max_exp), Has_unhandled),
        (Has_unhandled == no ->
	        (handle_intermediate_type_var(Type_var, Type_name),
                 common_util__error(4, "Fixed type declaration are only approximately implemented", no_error_consequences, [(type_name, Type_name)], 4627157, mika_symbolic, exec, no_localisation, no_extra_info),
	         symbolically_interpret(Min_exp, _, Min_cons, Type_min, _Exception_),
	         symbolically_interpret(Max_exp, _, Max_cons, Type_max, _Exception_),
                 symbolically_interpret(Delta_exp, _, Delta_cons, Type_delta, _Exception_),
                 ((Type_min == unhandled_expression ; Type_max == unhandled_expression ; Type_delta == unhandled_expression) ->
                        create_unhandled(Type_var, fixed_float_type_with_range, "Fixed float type with range declaration has unhandled entities")
                 ;
	                midoan_type:midoan_type__create_type(Type_name, fixed, delta(Delta_cons), range_bounds(Min_cons, Max_cons), Type_var)
                 )
                )
        ;
                create_unhandled(Type_var, fixed_float_type_with_range, "Unhandled entity in exec/1: fixed float type with range declaration has unhandled entities")
        ).

%for Ada rule 3.5.9
exec(type(Type_var, no_discriminant, fixed, _Standard_float_type_var, delta_digits(Delta_exp, Digits_exp, range([Min_exp, Max_exp]))), carry_on) :-
	!,
        check_for_unhandled(type(Type_var, Delta_exp, Digits_exp, Min_exp, Max_exp), Has_unhandled),
        (Has_unhandled == no ->
	        (handle_intermediate_type_var(Type_var, Type_name),
                 common_util__error(4, "Fixed type declarations are only approximately implemented", no_error_consequences, [(type_name, Type_name)], 4651158, mika_symbolic, exec, no_localisation, no_extra_info),
	         symbolically_interpret(Min_exp, _, Min_cons, Type_min, _Exception_),
	         symbolically_interpret(Max_exp, _, Max_cons, Type_max, _Exception_),
                 symbolically_interpret(Delta_exp, _, Delta_cons, Type_delta, _Exception_),
                 symbolically_interpret(Digits_exp, _, Digits_cons, Type_digits, _Exception_),
                 ((Type_min == unhandled_expression ; Type_max == unhandled_expression ; Type_delta == unhandled_expression ; Type_digits == unhandled_expression) ->
	                create_unhandled(Type_var, fixed_float_type_with_digits_and_range, "Fixed float type with digits and range declaration has unhandled entities")
                 ;
                        midoan_type:midoan_type__create_type(Type_name, fixed, delta(Delta_cons), digits(Digits_cons), range_bounds(Min_cons, Max_cons), Type_var)
                 )
                )
        ;
                create_unhandled(Type_var, fixed_float_type_with_digits_and_range, "Unhandled entity in exec/1: fixed float type with digits and range declaration has unhandled entities")
        ).

%for Ada rule 3.5.9
exec(type(Type_var, no_discriminant, fixed, Standard_float_type_var, delta_digits(Delta_exp, Digits_exp)), Flow) :-
	!,
        symbolically_interpret(tic(Standard_float_type_var, first), _, Min_exp, _, _Exception_),
        symbolically_interpret(tic(Standard_float_type_var, last), _, Max_exp, _, _Exception_),
        exec(type(Type_var, no_discriminant, fixed, Standard_float_type_var, delta_digits(Delta_exp, Digits_exp, range([Min_exp, Max_exp]))), Flow).

%for Ada rule 7.3
%Type_var must be a name_atts vars
exec(type(Type_var, Discriminant, 'private', Tag, Limitation), carry_on) :-
        !,
        check_for_unhandled(type(Type_var, Discriminant, private, Tag, Limitation), Has_unhandled),
        handle_intermediate_type_var(Type_var, Type_name),
        (Has_unhandled == 'no' ->
                (Discriminant == 'no_discriminant' ->
                        (Tag == not_tagged ->
	                        ((Limitation == limited ->       %assignment for objects of that type are impossible outside the package
                                        common_util__error(2, "Type limitation is not enforced", no_error_consequences, [(type_name, Type_name)], 2695138, mika_symbolic, exec, no_localisation, no_extra_info)
                                 ;
                                        true
                                 ),
                                 midoan_type:midoan_type__create_type(Type_name, private, Type_var)    %will be fully declared later (when elaborating the private part)
                                )
                        ;
                                (mika_unhandled_atts:mika_unhandled_atts__create(Type_var, Type_name, type(record)),
                                 common_util__error(4, "Unhandled entity: Private tagged type are not handled (are ignored)", no_error_consequences, [(type_name, Type_name)], 4736181, mika_symbolic, exec, no_localisation, no_extra_info)
                                )
                        )
                ;
                        (mika_unhandled_atts:mika_unhandled_atts__create(Type_var, Type_name, type(record)),
                         common_util__error(4, "Unhandled entity: Private discriminated type are not handled (are ignored)", no_error_consequences, [(type_name, Type_name)], 4742181, mika_symbolic, exec, no_localisation, no_extra_info)
                        )
                )
        ;
                create_unhandled(Type_var, private_type, "Unhandled entity in exec/1: private type declaration has unhandled entities")
        ).
%%%
%e.g. type(Boolean_104, no_discriminant, enumeration, [False_148, True_255])
exec(type(Type_var, no_discriminant, enumeration, LiteralL), carry_on) :-
        !,      %just for efficiency : the indexing is not perfect
        check_for_unhandled(type(Type_var, LiteralL), Has_unhandled),
        (Has_unhandled == no ->
	        (handle_intermediate_type_var(Type_var, Type_name),
	         initialise_literals_to_seavs(LiteralL, Type_var, Literals_name_valueL),       %initialise all literals as SEAVs and returns list of input values ready for transformation into midoan_enum variables by the solver
	         midoan_type:midoan_type__create_type(Type_name, enumeration, Literals_name_valueL, Type_var)
                )
        ;
                create_unhandled(Type_var, enumeration_type, "Unhandled entity in exec/1: enumeration type declaration has unhandled entities")
        ).

%%%
exec(type(Type_var, Discriminant, Tag, Limitation, record(FieldL_in)), carry_on) :-
%FieldL_in is of the form a list of (Field_namesL, component_definition(Aliased, Subtype_indication), Init(no_init))
%terminated by  	             Variant(variant�no_variant)
	!,
        check_for_unhandled(type(Type_var, Discriminant, not_tagged, Limitation, record(FieldL_in)), Has_unhandled),
        (Has_unhandled == no ->
	        (handle_intermediate_type_var(Type_var, Type_name),
                 (Limitation == limited ->       %assignment for objects of that type are impossible outside the package
                        common_util__error(2, "Warning: type limitation is not enforced", no_error_consequences, [(type_name, Type_name)], 2736138, mika_symbolic, exec, no_localisation, no_extra_info)
                 ;
                        true
                 ),
                 (Discriminant == no_discriminant ->
                        (Tag == not_tagged ->
                                ((FieldL_in == null_record ->
                                        midoan_type:midoan_type__create_type(Type_name, null_record, Type_var)
                                 ;
                                        (prepares_field_list(FieldL_in, FieldL_out),	%symbolically interpret (could contain initialisation information)
	                                 midoan_type:midoan_type__create_type(Type_name, record, FieldL_out, Type_var)
                                        )
                                 )
                                )
                        ;
                                (mika_unhandled_atts:mika_unhandled_atts__create(Type_var, Type_name, type(record)),
                                 common_util__error(4, "Unhandled entity: Tagged record type are not handled (are ignored)", no_error_consequences, [(type_name, Type_name)], 4795182, mika_symbolic, exec, no_localisation, no_extra_info)
                                )
                        )
                 ;
                        (mika_unhandled_atts:mika_unhandled_atts__create(Type_var, Type_name, type(record)),
                         common_util__error(4, "Unhandled entity: Discriminated record type are not handled (are ignored)", no_error_consequences, [(type_name, Type_name)], 4753181, mika_symbolic, exec, no_localisation, no_extra_info)
                        )
                 )
                )
        ;
                create_unhandled(Type_var, record_type, "Unhandled entity in exec/1: record type declaration has unhandled entities")
        ).
%%%
%unconstrained arrays are currently not handled
%things to consider are unification, and sliding for whole array assignment (also for ordinary arrays)
%Array_kind : array|unconst_array
exec(type(Type_var, no_discriminant, Array_kind, IndexL, component_definition(Alias, Component_subtype_indication_or_access_definition)), carry_on) :-
	!,
        handle_intermediate_types(Component_subtype_indication_or_access_definition, Component_subtype_var, _Component_subtype_name),
        check_for_unhandled(type(Type_var, no_discriminant, Array_kind, IndexL, component_definition(Alias, Component_subtype_var)), Has_unhandled),
        (Has_unhandled == no ->
	        (handle_intermediate_type_var(Type_var, Type_name),
                 (Alias == aliased ->    %the memory address of its components maybe retrieved via the 'Access attribute
                        common_util__error(4, "Aliased array type component is ignored", "e.g. Access will not work", [(type_name, Type_name)], 4770172, mika_symbolic, exec, no_localisation, no_extra_info)
                 ;
                        true
                 ),
                 (Array_kind == unconst_array ->
                        handle_unconstrained_array_indexes(IndexL, IndexL_out) %IndexL_out should be a list of Type_vars
                 ;
                  Array_kind == array ->
                        handle_discrete_range_list(IndexL, IndexL_out)      %IndexL_out is a list of Type_vars
                 ;
                        common_util__error(10, "Array kind in type definition is not 'array' nor 'unconst_array'", "Cannot proceed", [(type_name, Type_name), (array_kind, Array_kind)], 1082342, mika_symbolic, exec, no_localisation, "A Mika bug")
                 ),
                 midoan_type:midoan_type__create_type(Type_name, Array_kind, IndexL_out, Component_subtype_var, Type_var)
                )
        ;
                create_unhandled(Type_var, Array_kind, "Unhandled entity in exec/1: array type declaration has unhandled entities")
        ).

exec(type(Type_var, no_discriminant, interface, _Kind, _Interface_list), carry_on) :-
        !,
        check_for_unhandled(type(Type_var, no_discriminant, interface, _Kind, _Interface_list), Has_unhandled),
        (Has_unhandled == no ->
                create_unhandled(Type_var, interface, "Warning: interface declarations in exec/1 are currently ignored (declared an unhandled)")
        ;
                create_unhandled(Type_var, access_type, "Unhandled entity in exec/1: interface declaration has unhandled entities")
        ).

exec(type(Type_var, no_discriminant, _Null_Exclusion, access, Subtype_indication), carry_on) :-
        !,
        check_for_unhandled(type(Type_var, no_discriminant, _Null_Exclusion, access, Subtype_indication), Has_unhandled),
        (Has_unhandled == no ->
	        create_unhandled(Type_var, access_type, "Warning: access type declaration in exec/1 are currently ignored (declared an unhandled)")
        ;
                create_unhandled(Type_var, access_type, "Unhandled entity in exec/1: access type declaration has unhandled entities")
        ).
exec(type(Type_var, no_discriminant, _Null_Exclusion, access_constant, Subtype_indication), carry_on) :-
        !,
        check_for_unhandled(type(Type_var, no_discriminant, _Null_Exclusion, access_constant, Subtype_indication), Has_unhandled),
        (Has_unhandled == no ->
	        create_unhandled(Type_var, access_constant_type, "Warning: access constant type declaraction in exec/1 are currently ignored (declared an unhandled)")
        ;
                create_unhandled(Type_var, access_constant_type, "Unhandled entity in exec/1: access constant type declaration has unhandled entities")
        ).

exec(type(Type_var, no_discriminant, _Null_Exclusion, access_all, Subtype_indication), carry_on) :-
        !,
        check_for_unhandled(type(Type_var, no_discriminant, _Null_Exclusion, access_all, Subtype_indication), Has_unhandled),
        (Has_unhandled == no ->
	        create_unhandled(Type_var, access_all_type, "Warning: access all type declaraction in exec/1 are currently ignored (declared an unhandled)")
        ;
                create_unhandled(Type_var, access_all_type, "Unhandled entity in exec/1: access all type declaration has unhandled entities")
        ).

exec(type(Type_var, no_discriminant, _Null_Exclusion, access_procedure, Protected, Formal_part), carry_on) :-
        !,
        check_for_unhandled(type(Type_var, no_discriminant, _Null_Exclusion, access_procedure, Protected, Formal_part), Has_unhandled),
        (Has_unhandled == no ->
	        create_unhandled(Type_var, access_procedure_type, "Warning: access procedure type declaraction in exec/1 are currently ignored (declared an unhandled)")
        ;
                create_unhandled(Type_var, access_procedure_type, "Unhandled entity in exec/1: access procedure type declaration has unhandled entities")
        ).

exec(type(Type_var, no_discriminant, _Null_Exclusion, access_function, Protected, Formal_part, Mark), carry_on) :-
        !,
        check_for_unhandled(type(Type_var, no_discriminant, _Null_Exclusion, access_function, Protected, Formal_part, Mark), Has_unhandled),
        (Has_unhandled == no ->
	        create_unhandled(Type_var, access_function_type, "Warning: access function type declaraction in exec/1 are currently ignored (declared an unhandled)")
        ;
                create_unhandled(Type_var, access_function_type, "access function type declaration has unhandled entities")
        ).

%Empty_type_completion is either is_tagged or empty_type_completion
exec(type(Type_var, Discriminant, Empty_type_completion), carry_on) :-
        !,
        %Type_var will be completely declared later see 15/10/07
        check_for_unhandled(type(Type_var, Discriminant, Empty_type_completion), Has_unhandled),
        (Has_unhandled == no ->
                (Discriminant == no_discriminant ->
                        (Empty_type_completion == empty_type_completion ->
                                (handle_intermediate_type_var(Type_var, Type_name),
                                 midoan_type:midoan_type__create_type(Type_name, incomplete_type, Type_var)    %will be fully declared later
                                )
                        ;
                         Empty_type_completion == is_tagged ->
                                create_unhandled(Type_var, type(tagged_incomplete_type), "Incomplete tagged types are not handled")
                        ;
                                common_util__error(10, "Empty type completion is not empty nor tagged", "Should never happen", [(type_name, Type_name)], 10970833, mika_symbolic, exec, no_localisation, no_extra_info)
                        )
                ;
                        create_unhandled(Type_var, type(discriminated_incomplete_type), "Incomplete discriminated types are not handled")
                )
        ;
                create_unhandled(Type_var, empty_type_completion, "Unhandled entity in exec/1: empty completion type declaration has unhandled entities")
        ).

%%%
%For Ada Rule 3.2.2
%Type_var is just a non-defining identifier here
exec(subtype(Subtype_var, Subtype_indication), carry_on) :-
        mika_name_atts:mika_name_atts__unput(Subtype_var, Subtype_name),
        check_for_unhandled(Subtype_indication, Has_unhandled),
        (Has_unhandled == no ->
	        (handle_subtype_indication(Subtype_indication, Parent_type_var, Cons_const),
                 %we are currently ignoring some of the name constructs : incomplete
                 (Cons_const == no_constraint ->
		        midoan_type:midoan_type__create_subtype(Subtype_name, Parent_type_var, Subtype_var)
	         ;
	          Cons_const = range([Min_const, Max_const]) ->
		        midoan_type:midoan_type__create_subtype(Subtype_name, Parent_type_var, range_bounds(Min_const, Max_const), Subtype_var)
	         ;
                  Cons_const = unconst_array(IndexL)     ->
                        midoan_type:midoan_type__create_subtype(Subtype_name, Parent_type_var, unconst_array(IndexL), Subtype_var)    %the type_name unconstrained array type is being instantiated by IndexL
                 ;
	          Cons_const = digits(_Digits_cons, _Range_opt) ->
		        common_util__error(10, "In subtype declaration, digits constraint are not handled", "Cannot proceed", [(constraint, Cons_const)], 10909160, mika_symbolic, exec, exec(subtype(a, digits(b, c))), "Not yet implemented: Mika limitation. Should probably create an unhandled var in the mean time")
	         ;
                  Cons_const = delta(_Delta_cons, _Range_opt) ->
		        common_util__error(4, "In subtype declaration, delta constraints are not handled", no_error_consequences, [(cons_const, Cons_const)], 4912166, mika_symbolic, exec, no_localisation, no_extra_info)
                 )
                )
        ;
                (mika_unhandled_atts:mika_unhandled_atts__create(Subtype_var, Subtype_name, subtype),
                 common_util__error(4, "Subtype has unhandled entities", no_error_consequences, [(subtype_name, Subtype_name)], 4917130, mika_symbolic, exec, no_localisation, no_extra_info)
                )
        ).

%%%
exec(number(VarsL, Exp), carry_on) :-
        symbolically_interpret(Exp, _, Exp_cons, Type_exp, _Exception_),    %only interpreted once as per Ada RM
        exec(number2(VarsL, Exp_cons, Type_exp), carry_on).

exec(number2([], _, _), carry_on) :-
        !.      %just for efficiency : the indexing is not perfect
exec(number2([Name_atts|Rest], Var, Type), carry_on) :-
        !,      %just for efficiency : the indexing is not perfect
	(Type == unhandled_expression ->
                (mika_name_atts:mika_name_atts__unput(Name_atts, Name),
                 mika_unhandled_atts:mika_unhandled_atts__create(Name_atts, Name, constant),
                 common_util__error(4, "Constant is assigned an expression with unhandled entities", no_error_consequences, [(name, Name)], 4103026, mika_symbolic, exec, no_localisation, no_extra_info)
                )
        ;
                (mika_name_atts:mika_name_atts__unput(Name_atts, _),
                 Name_atts = Var
                )
        ),
        exec(number2(Rest, Var, Type), carry_on).

exec(null, carry_on).
%%%
%for Ada rules 13.3 (an attribute change)
%              13.4 (enumeration)
%local_name is direct_name | direct_name'attribute | library_unit_name where direct_name is an identifier or an operator_symbol : it static
exec(representation_clause(Target_exp, Exp), carry_on) :-
        check_for_unhandled((Target_exp, Exp), Has_unhandled),
        (Has_unhandled == no ->
                (Exp = agg(Array_agg) ->       %Ada rule 13.4: a representation clause for an enumerated subtype
                        (syntactically_denote(Target_exp, Name_target, Entity_target, _Type_target),                %not actual execution ...
                         update_representation_enum(Array_agg, Rep_clause),             %see mika_symbolic__init also symbolically interpret the expressions
		         midoan_type:midoan_type__update_enum(Entity_target, Name_target, Rep_clause)
                        )
                ;
                 Target_exp = tic(Local_name, Attribute) ->      %Ada rule 13.3: an attribute definition clause
                        ((Attribute == address ; Attribute == size ; Attribute == component_size ; Attribute == alignment ; Attribute == external_tag ; Attribute == small ;
                         Attribute == bit_order ; Attribute == storage_pool ; Attribute == storage_size ; Attribute == write ;
                         Attribute == output ; Attribute == read ; Attribute == input ; Attribute == machine_radix) ->  %only allowed attributes : see 13.3 in Ada RM
                                (symbolically_interpret(Exp, _Symbolic_exp, Constraint_exp, Type_exp, _Exception_),
                                 (Type_exp == unhandled_expression ->
                                        common_util__error(4, "Representation clause with unhandled entities is ignored", no_error_consequences, [(representation_clause(target_exp, exp), representation_clause(Target_exp, Exp))], 4020213, mika_symbolic, exec, no_localisation, no_extra_info)
                                 ;
                                        (syntactically_denote(Local_name, Name_local_name, Entity, Type_local_name),                %not actual execution ...
                                         (Type_local_name == type ->
                                                midoan_type:midoan_type__update_type_attribute(Entity, Attribute, Constraint_exp)
                                         ;
                                          Type_local_name == object ->
                                                common_util__error(4, "In representation clause attributes to object are ignored", no_error_consequences, [(tic(name_local_name, attribute), tic(Name_local_name, Attribute))], 4953224, mika_symbolic, exec, no_localisation, no_extra_info) %could use assert or better add an extra attribute to seavs containing a list of attributes values for objects
                                         ;
                                                common_util__error(10, "Representation clause for something else than a type or an object is not handled", "Cannot proceed", [(tic(name_local_name, attribute), tic(Name_local_name, Attribute))], 10955241, mika_symbolic, exec, exec(representation_clause(a, b)), "Not yet implemented: Mika limitation. Should probably create an unhandled var in the mean time")
                                         )
                                        )
                                 )
                                )
                        ;
                                common_util__error(10, "Invalid attribute in representation clause", "Cannot proceed", [(tic(local_name, attribute), tic(Local_name, Attribute))], 1095944, mika_symbolic, exec, exec(representation_clause(a, b)), "See 13.3 in Ada RM")
                        )
                ;
                        common_util__error(10, "Representation clause not handled", "Cannot proceed", [(target, Target_exp)], 1096246, mika_symbolic, exec, exec(representation_clause(a, b)), "Not yet implemented: Mika limitation.")
                )
        ;
                common_util__error(4, "Representation clause with unhandled entities is ignored", no_error_consequences, [(representation_clause(target_exp, exp), representation_clause(Target_exp, Exp))], 4965213, mika_symbolic, exec, no_localisation, no_extra_info)
        ).

%Ada rule 13.5.1: a representation clause for a record type
exec(record_representation_clause(Target_record, Mod, ClauseL), carry_on) :-
        check_for_unhandled((Target_record, Mod, ClauseL), Has_unhandled),
        (Has_unhandled == no ->
                (syntactically_denote(Target_record, Name_target_record, _Entity_target, _Type_local_name),                %not actual execution ...
                 common_util__error(4, "Record representation clauses are ignored", no_error_consequences, [(name_target_record, Name_target_record)], 4973159, mika_symbolic, exec, no_localisation, no_extra_info)
                )
        ;
                common_util__error(4, "Record representation clause with unhandled entities is ignored", no_error_consequences, [(target_record, Target_record), (mod, Mod), (clausel, ClauseL)], 4976202, mika_symbolic, exec, no_localisation, no_extra_info)
        ).

exec(at_representation_clause(Target_exp, Exp), carry_on) :-
        check_for_unhandled((Target_exp, Exp), Has_unhandled),
        (Has_unhandled == no ->
                (syntactically_denote(Target_exp, Name_target_exp, _Entity_target, _Type_local_name),                %not actual execution ...
                 common_util__error(4, "At representation clauses are ignored", no_error_consequences, [(name_target_exp, Name_target_exp)], 4983149, mika_symbolic, exec, no_localisation, no_extra_info)
                )
        ;
                common_util__error(4, "At representation clause with unhandled entities is ignored", no_error_consequences, [(target_exp, Target_exp), (exp, Exp)], 4986172, mika_symbolic, exec, no_localisation, no_extra_info)
        ).
%%%
% For Ada rule 3.3.1
%object declaration
%Qualifier can be not_qualified, aliased, constant or aliased_constant
%Init can be no_init or an expression
exec(object(Qualifier, NameL, Subtype_indication_or_access_definition, Init), carry_on) :-
        handle_intermediate_types(Subtype_indication_or_access_definition, Intermediate_type_var, _Intermediate_type_name),
        check_for_unhandled(object(Qualifier, NameL, Intermediate_type_var, Init), Has_unhandled),
        (Has_unhandled == no ->
                objects(NameL, object(Qualifier, Intermediate_type_var, Init))
        ;
                create_unhandled_list(NameL, object, "Unhandled entity in exec/1: object declaration has unhandled entities")
        ).

%Array_indication : can be 'array' or 'unconst_array'
exec(object(Qualifier, NameL, Array_indication, IndexL_in, Component_definition, Init), carry_on) :-
        common_util__create_dummy_name(Dummy_type_name),
        mika_name_atts:mika_name_atts__create(Dummy_type_var, Dummy_type_name),        %we create a dummy type to allow straightforward declaration
        exec(type(Dummy_type_var, no_discriminant, Array_indication, IndexL_in, Component_definition), carry_on),
        exec(object(Qualifier, NameL, subtype_indication(may_be_null, Dummy_type_var, no_constraint), Init), carry_on).

%%%
exec(task_body, carry_on).              %nothing done as nothing to do
exec(protected_body, carry_on).         %nothing done as nothing to do
exec(body_stub(Stub_name), carry_on) :-      %added 04/07/09 %was matched to its body during the fix_separates pre-processing
        exec(Stub_name, carry_on).
exec(match_body(_Stub_name, _Body), carry_on).        %added 05/07/09 %nothing done as nothing to do: has already been parsed during body_stub(Stub_name)
%%%
%labeled statements are handled in exec(stmts ...
exec(labeled_statement(_Label, Stmt), Flow) :-
        exec(Stmt, Flow).

%Flow becomes goto(Label) will be handled by cover_sequences_of_statements predicate
exec(goto(Label), goto(Label)).

%similar pattern to subprogramm call execution
exec(block(_Label, local_decl(Local_decls), Body), Flow) :-     %block labels can be totally ignored : only used for scope resolution (not for goto nor exit statements)
        my_copy_term('locals', a(Local_decls, Body), a(Local_decls_c, Body_c)),
        exec(Local_decls_c, carry_on),
        exec(Body_c, Flow).
exec(return(_Return), return) :-	%for procedures
        !.
exec(return(Return, Expression), return) :- %for functions
        exec(assign(Return, Expression), carry_on).

exec(extended_return(_Identifier, _Constant_opt, _Subtype_indication_or_access_definition, _Init_opt, _Stmts), return) :-
        common_util__error(6, "Extended returns are currently unhandled", "coverage inforamtion will be wrong", no_arguments, 101094, mika_symbolic, exec, no_localisation, no_extra_info).

exec(exit_when(Loop_name, no_when), Flow) :-      %a normal 'exit' statement
	!,      %just for efficiency : the indexing is not perfect
        (Loop_name == 'no_name' ->
                Flow = 'exit'
        ;
                Flow = exit_named_loop(Loop_name)
        ).

exec(exit_when(Loop_name, bran(Id_bran, deci(Id_deci, Expression))), Flow) :-
	!,
        choose_truth(bran(Id_bran, deci(Id_deci, Expression)), Outcome),
        (Outcome == 'true' ->
                (Loop_name == 'no_name' ->
                        Flow = 'exit'
                ;
                        Flow = exit_named_loop(Loop_name)
                )
        ;
         Outcome == 'false' ->
                Flow = 'carry_on'
        ).
%%%
%for Ada rule 11.3
exec(raise_statement(Exception_name, String_Expression), exception_raised(Exception_name, String_Expression)) :-      %changed 20/11/07 : what should we do here? see issue 'exception'
        fail.
        %common_util__error(6, "Raise statements are currently ignored", no_error_consequences, [(exception_name, Exception_name)], 6104840, mika_symbolic, exec, no_localisation, no_extra_info).

exec(requeue_statement, carry_on) :-
        common_util__error(6, "Requeue statements are currently ignored", no_error_consequences, no_arguments, 6111200, mika_symbolic, exec, no_localisation, no_extra_info).

exec(requeue_abort_statement, carry_on) :-
        common_util__error(6, "Requeue abort statements are currently ignored", no_error_consequences, no_arguments, 6111501, mika_symbolic, exec, no_localisation, no_extra_info).

%%%
% 04/03/05 TODO : assign is complex it needs to be thoroughly tested (create a separate test file)
% any change here should also influence the freeze_arguments/2 predicate in mika_symbolic__execute_util.pl
exec(assign(Name, Expression), Flow) :-
        (mika_unhandled_atts:mika_unhandled_atts__is_unhandled_atts(Name) ->
                (!,              %absolutely necessary here
                 common_util__error(4, "Unhandled entity is assigned", no_error_consequences, [(name, Name)], 4105620, mika_symbolic, exec, no_localisation, no_extra_info),
                 Flow = 'carry_on'
                )
        ;
                (mika_seav_atts:mika_seav_atts__is_seav(Name),
                 !,              %absolutely necessary here
                 symbolically_interpret(Expression, Symbolic, Constraint, Type, Exception),
                 (common_util:common_util__is_an_exception(Exception) ->
                        Flow = Exception
                 ;
                  Type == 'unhandled_expression' ->
                        (mika_seav_atts:mika_seav_atts__unput(Name, Name_name),
                         mika_unhandled_atts:mika_unhandled_atts__create(Name, Name_name, object),
                         common_util__error(4, "Object is assigned an expression with unhandled entities", no_error_consequences, [(name, Name_name)], 4106927, mika_symbolic, exec, no_localisation, no_extra_info)
                        )
                 ;
                        (update_var_on_assignement(Name, Symbolic, Constraint),		%name is an Seav
                         Flow = 'carry_on'
                        )
                 )
                )
        ).
%%%%%
%below are all the possible combinations of array and record access on the left hand side of an assignement
%-selected(...)                 -> up_rec
%-indexed(...)                  -> up_arr
%-selected(selected(...), ...)  -> assign(selected(...), up_rec ...)
%-indexed(selected(...), ...)   -> assign(selected(...), up_arr ...)
%-selected(indexed(...), ...)   -> assign(indexed(...), up_rec ...)
%-indexed(indexed(...), ...)    -> assign(indexed(...), up_arr ...)
%%%%%
%e.g. "date.year := 1998" is transformed into up_rec(date, year, 1998)
exec(assign(selected(Name, Field), Exp), carry_on) :-
        (\+ mika_name_atts:mika_name_atts__is_name_atts(Field) ->      %this is not a record access but a prefix
                (!,              %absolutely necessary here
                 exec(assign(Field, Exp), carry_on)
                )
        ;
         (mika_unhandled_atts:mika_unhandled_atts__is_unhandled_atts(Name) ; check_for_unhandled(Field, yes)) ->
                (!,              %absolutely necessary here
                 common_util__error(4, "Unhandled entity is assigned", no_error_consequences, [(selected(name, field), selected(Name, Field))], 4109353, mika_symbolic, exec, no_localisation, no_extra_info)
                )
        ;
                (%a function that returns a record (e.g. f(x).year := 1998) : this is impossible "must be a variable"
                 %check name can be a compound
                 mika_seav_atts:mika_seav_atts__is_seav(Name),            %Name is a seav being assigned
                 !,              %absolutely necessary here
                 check_for_unhandled(Exp, Has_unhandled),
                 (Has_unhandled == no ->
                        (mika_seav_atts:mika_seav_atts__get(type, Name, Type_var),
	                 midoan_type:midoan_type__obtain_basetype(Type_var, record), %must be a record seav variable
	                 symbolically_interpret(Field, Field_symb, Field_const, Field_type, _Exception_),
	                 symbolically_interpret(Exp, Exp_symb, Exp_const, Exp_type, _Exception_),
                         symbolically_interpret(Name, Name_symb, Name_const, Name_type, _Exception_),
                         ((Field_type == unhandled_expression ; Exp_type == unhandled_expression ; Name_type == unhandled_expression) ->
                                (mika_seav_atts:mika_seav_atts__unput(Name, Name_name),
                                 mika_unhandled_atts:mika_unhandled_atts__create(Name, Name_name, object),
                                 common_util__error(4, "Object is assigned an expression with unhandled entities", no_error_consequences, [(name, Name_name)], 4119855, mika_symbolic, exec, no_localisation, no_extra_info)
                                )
                          ;
                                (
	                         %09/01/09
                                 %there is a problem here for partially assigned arrays (and records) they should really become 'out' but then this applies to all its components even those who after should become 'in'
                                 %because we are treating arrays and records as a whole we have no choice but but symbolically interpret the entire variable which will become 'in_out' if unused even if in fact it is not 'in'
                                 %mika_seav_atts__get(mode, Name, Mode),
                                 %(Mode == unused ->
		                 %       (%special case to deal with misidentified mode in simple case such as "date.year := 1998"
		                 %        mika_seav_atts__get(name, Name, Name_symb),
		                 %        midoan_type:midoan_type__variables_declaration([Name_const], variable, Type_mark)
		                 %        %Name is still unused here as it should be
		                 %       )
	                         %;
	                         %),
                                 midoan_solver__interpret(up_rec(Name_const, Field_const, Exp_const), types(record, _, Exp_type), Constraint, Type, _Exception_),
                                 (Type == unhandled_expression ->
                                        (mika_seav_atts:mika_seav_atts__unput(Name, Name_name),
                                         mika_unhandled_atts:mika_unhandled_atts__create(Name, Name_name, object),
                                         common_util__error(4, "Record could not be updated", no_error_consequences, [(name, Name_name)], 4124749, mika_symbolic, exec, no_localisation, no_extra_info)
                                        )
                                 ;
                                        update_var_on_assignement(Name, up_rec(Name_symb, Field_symb, Exp_symb), Constraint)   %name above is an SEAV
                                 )
                                )
                         )
                        )
                 ;
                        (mika_seav_atts:mika_seav_atts__unput(Name, Name_name),
                         mika_unhandled_atts:mika_unhandled_atts__create(Name, Name_name, object),
                         common_util__error(4, "Object is assigned an expression with unhandled entities", no_error_consequences, [(name, Name_name)], 4112355, mika_symbolic, exec, no_localisation, no_extra_info)
                        )
                 )
                )
        ).
%e.g. "birthdays(chris) := 311070" is transformed into up_arr(birthdays, chris, 311070)
%TODO could be a function returning a variable (e.g. record filed or and array element) that is assigned a value ...
exec(assign(indexed(Name, Index), Exp), carry_on) :-
        mika_seav_atts:mika_seav_atts__is_seav(Name),
        !,              %absolutely necessary here
        symbolically_interpret(Index, Index_symb, Index_const, Index_type, _Exception_),
	symbolically_interpret(Exp, Exp_symb, Exp_const, Exp_type, _Exception_),
        symbolically_interpret(Name, Name_symb, Name_const, Name_type, _Exception_),
        (is_unhandled_type([Index_type, Exp_type, Name_type]) ->
                (mika_seav_atts:mika_seav_atts__unput(Name, Name_name),
                 mika_unhandled_atts:mika_unhandled_atts__create(Name, Name_name, object),
                 common_util__error(4, "Object is assigned an expression with unhandled entities", no_error_consequences, [(name, Name_name)], 4123955, mika_symbolic, exec, no_localisation, no_extra_info)
                )
        ;
                (%mika_seav_atts__get(type, Name, Type_mark),           %removed 23/11/09
	         %midoan_solver__obtain_basetype(Type_mark, array(_)),  %removed 23/11/09
	         %09/01/09
                 %there is a problem here for partially assigned arrays (and records) they should really become 'out' but then this applies to all its components even those who after should become 'in'
                 %because we are treating arrays and records as a whole we have no choice but but symbolically interpret the entire variable which will become 'in_out' if unused even if in fact it is not 'in'
                 %mika_seav_atts__get(mode, Name, Mode),
                 %(Mode == unused ->
	         %	(%special case to deal with misidentified mode in simple case such as "birthdays(chris) := 311070"
                 %	 mika_seav_atts__get(name, Name, Name_symb),
                 %	 midoan_type:midoan_type__variables_declaration([Name_const], variable, Type_mark)
                 %	 %Name is still unused here as it should be
                 %	)
                 %;
        	 %),
                 %Index could be a 1 dimentional range in which case it is a slice
                 ((Index_const = [The_index], nonvar(The_index), The_index = [_From, _To]) ->
                        midoan_solver__interpret(up_arr_slice(Name_const, Index_const, Exp_const), types(array, _, Exp_type), Constraint, Type, _Exception_)
	         ;
                        midoan_solver__interpret(up_arr(Name_const, Index_const, Exp_const), types(array, _, Exp_type), Constraint, Type, _Exception_)
                 ),
                 (Type == unhandled_expression ->
                        (mika_seav_atts:mika_seav_atts__unput(Name, Name_name),
                         mika_unhandled_atts:mika_unhandled_atts__create(Name, Name_name, object),
                         common_util__error(4, "Array could not be updated", no_error_consequences, [(name, Name_name)], 4130051, mika_symbolic, exec, no_localisation, no_extra_info)
                        )
                 ;
	                update_var_on_assignement(Name, up_arr(Name_symb, Index_symb, Exp_symb), Constraint)
                 )
                )
        ).

%e.g. "date.year.unit := 8" is transformed into "date.year := up_rec(date.year, unit, 8)"
%                       which is equivalent to up_rec(date, year, up_rec(date.year, unit, 8))
%multy-level selection needs to be unravelled at least for records (see 17 and 18-09-03 notes)
%exec needs to be re-used because Record_exp can be a SEAV record variable or something that evaluates to a record variable
%  (i.e. a field selection, an array element)
exec(assign(selected(selected(Record, Field1), Field2), Expression), carry_on) :-
        !,      %just for efficiency : the indexing is not perfect
        (\+ mika_name_atts:mika_name_atts__is_name_atts(Field1) ->      %this is not a record access but a prefix
                exec(assign(selected(Field1, Field2), Expression), carry_on)
        ;
         \+ mika_name_atts:mika_name_atts__is_name_atts(Field2) ->      %this is not a record access but a prefix
                exec(assign(Field2, Expression), carry_on)
        ;
	        (my_copy_term('seavs', Record, Record_copy),
	         exec(assign(selected(Record, Field1), up_rec(selected(Record_copy, Field1), Field2, Expression)), carry_on)
                )
        ).

%e.g. "date.year(2) := 9" is transformed into "date.year := up_arr(date.year, 2, 9)"
%		       which is equivalent to up_rec(date, year, up_arr(selected(date, year), 2, 9))
exec(assign(indexed(selected(Record, Field), Index), Expression), carry_on) :-
        !,      %just for efficiency : the indexing is not perfect
        (\+ mika_name_atts:mika_name_atts__is_name_atts(Field) ->      %this is not a record access but a prefix
                exec(assign(indexed(Field, Index), Expression), carry_on)
        ;
	        (my_copy_term('seavs', Record, Record_copy),
                 exec(assign(selected(Record, Field), up_arr(selected(Record_copy, Field), Index, Expression)), carry_on)
                )
        ).

%e.g. "birthdays(chris)(2) := Exp" is transformed into "birthdays(chris) := up_arr(birthdays(chris), 2, Exp)"
%				which is equivalent to up_arr(birthdays, chris, up_arr(indexed(birthdays, chris), 2, Exp)
exec(assign(indexed(indexed(Array, Index1), Index2), Expression), carry_on) :-
        !,
        my_copy_term('seavs', Array, Array_copy),
        exec(assign(indexed(Array, Index1), up_arr(indexed(Array_copy, Index1), Index2, Expression)), carry_on).

%the field of an array element is being assigned
%e.g. "birthdays(chris).year := 1970" is transformed into "birthdays(chris) := up_rec(birthdays(chris), year, 1970)"
%			           which is equivalent to up_arr(birthdays, chris, up_rec(indexed(birthdays, chris), year, 1970))
%29/03/07 it is not entirely equivalent as far as the mode of the seav 'birthdays' is concerned ...
exec(assign(selected(indexed(Array, Index), Field), Expression), carry_on) :-
        !,      %just for efficiency : the indexing is not perfect
        (\+ mika_name_atts:mika_name_atts__is_name_atts(Field) ->      %this is not a record access but a prefix
                common_util__error(10, "Prefix does not make sense", "Cannot proceed", [(assign, assign(selected(indexed(Array, Index), Field), Expression))], 10119949, mika_symbolic, exec, exec(assign(selected(indexed(a, b), c), d)), "Should never happen")
        ;
	        (my_copy_term('seavs', Array, Array_copy),
                 exec(assign(indexed(Array, Index), up_rec(indexed(Array_copy, Index), Field, Expression)), carry_on)
                )
        ).
%%%
%For Ada Rule 6.4
exec(procedure_call(Call), Flow) :-
        check_for_unhandled(Call, Has_unhandled),
        (Has_unhandled == 'no' ->
                ((Call = indexed(Name_exp, Operand) ->           %Name_exp can be an indentifier, an operator symbol or a sequence of select(...,...)
                        handle_selected_name(Name_exp, Functor)
                 ;
                  Call = selected(_, _) ->                       %a selected procedure call without parameters
                        (handle_selected_name(Call, Functor),
                         Operand = []
                        )
                 ;
                  Call = Functor ->                              %a non selected procedure call without parameters
                        (Functor = Call,
                         Operand = []
                        )
                 ),
                 (mika_sub_atts:mika_sub_atts__is_sub_atts(Functor)    ->
                        (handle_subprogram_call(Functor, Operand, _Symbolic, _Constraint, _Type, Exception),
                         (common_util:common_util__is_an_exception(Exception) ->
                                Flow = Exception
                         ;
                                Flow = 'carry_on'
                         )
                        )
                 ;
                        (common_util__error(4, "Unhandled procedure (procedure not found) call for", no_error_consequences, [(functor, Functor), (call, Call)], 4122768, mika_symbolic, exec, no_localisation, no_extra_info),
                         Flow = 'carry_on'
                        )
                 )
                )
        ;
                (common_util__error(4, "Procedure call with unhandled entities is ignored", no_error_consequences, [(call, Call)], 4123139, mika_symbolic, exec, no_localisation, no_extra_info),
                 Flow = 'carry_on'
                )
        ).
%%%
exec(if_stmt([if_clause(Bran, Stmts)|Rest_if_clauses], else(Else_stmts)), Flow) :-
        check_for_unhandled(Bran, Has_unhandled),
        (Has_unhandled == 'no' ->
                (choose_truth(Bran, Outcome),
                 (Outcome == 'true' ->
                        exec(Stmts, Flow)
                 ;
                  Outcome == 'false' ->
                        (Rest_if_clauses == [] ->
                                exec(Else_stmts, Flow)
                        ;
                                exec(if_stmt(Rest_if_clauses, else(Else_stmts)), Flow)
                        )
                 ;
                  common_util:common_util__is_an_exception(Outcome) ->
                        Flow = Outcome                  %the exception is propagated upwards
                 )
                )
        ;
                (Flow = 'carry_on',
                 common_util__error(6, "If statement decision contains unhandled entities: it is skipped", "Soundness and completelness impaired", [(bran, Bran)], 6931027, mika_symbolic, exec, no_localisation, no_extra_info)
                )
        ).
%%%
exec(case_stmt(Exp, AlternativeL), Flow) :-
	symbolically_interpret(Exp, Exp_symb, Exp_cons, Type, _Exception_),	%only done once: the exp should be evaluated only once [ada RM 5.4]
        (Type == 'unhandled_expression' ->
                (Flow = 'carry_on',
                 common_util__error(6, "Case statement expression contains unhandled entities: it is skipped", "Soundness and completelness impaired", [(expression, Exp)], 613747, mika_symbolic, exec, no_localisation, no_extra_info)
                )
        ;
	        exec(case_stmt2(exp(Exp_symb, Exp_cons), AlternativeL), Flow)
        ).

exec(case_stmt2(_Exp, [alternative('others', Stmts)]), Flow) :-
	!,      %just for efficiency : the indexing is not perfect
	exec(Stmts, Flow).

exec(case_stmt2(Exp, [alternative(bran(Id_bran, deci(Id_deci, Discrete_choicesL)), Stmts)|Rest_alternatives]), Flow) :-
	choose_truth(case_alternative_discrete_choices(Exp, bran(Id_bran, deci(Id_deci, Discrete_choicesL))), Outcome),
	(Outcome == 'true' ->
                exec(Stmts, Flow)
        ;
         Outcome == 'false' ->
                (Rest_alternatives == [] ->
                        common_util__error(10, "Last alternative in a case statement must be taken: constraint error raised in your code", "Cannot proceed", [('Branch id', Id_bran)], 10129928, mika_symbolic, exec, exec(case_stmt2(a, [alternative(b, c)])), "Should never happen")
                ;
                        exec(case_stmt2(Exp, Rest_alternatives), Flow)
                )
        ).
%%%
%Identifier must be declared as an SEAV so that symbolic interpretation can take place (it gets assigned at every iteration)
%CHECK : I am not sure this works because the range may not be static (e.g. an input var): what happen then?
exec(loop_stmt(Loop_name, 'for', bran(Id_bran, deci(Id_deci, cond(Id_cond, Identifier))), Direction, Discrete_range, Stmts), Flow) :-
        !,
        %trace,
        handle_discrete_range_list([Discrete_range], [Discrete_type_var]),
	symbolically_interpret(tic(Discrete_type_var, 'first'), Min_symb, Min_cons, Type_min, _Exception_),
	symbolically_interpret(tic(Discrete_type_var, 'last'), Max_symb, Max_cons, Type_max, _Exception_),
        (
                (midoan_solver__sdl(>(Min_cons, Max_cons)), %null range
                 mika_coverage:mika_coverage__add_to_current_path('condition', (Id_cond, false)),
                 mika_coverage:mika_coverage__add_to_current_path('decision', (Id_deci, false)),
                 mika_coverage:mika_coverage__add_to_current_path('branch', (Id_bran, false)),
                 Flow = 'carry_on'
                )
        ;
                (midoan_solver__sdl(<=(Min_cons, Max_cons)), %null range
                 my_copy_term('locals', a(Identifier, Stmts), a(NIdentifier, NStmts)),
                 mika_globals:mika_globals__get_NBT(phase, Info),
                 mika_globals:mika_globals__set_NBT(phase, post_elaboration), %hack 21/04/08 to ensure that NIdentifier is actually identified as 'out' even if during we are during elaborations where the context is ignored
                 %SEAV declaration
                 (Direction == 'normal' ->
                        exec(object(not_qualified, [NIdentifier], subtype_indication(may_be_null, Discrete_type_var, no_constraint), tic(Discrete_type_var, first)), carry_on)
                 ;
                  Direction == 'reverse' ->
                        exec(object(not_qualified, [NIdentifier], subtype_indication(may_be_null, Discrete_type_var, no_constraint), tic(Discrete_type_var, last)), carry_on)
                 ),
                 mika_globals:mika_globals__set_NBT(phase, Info),
                 ((Type_min == 'unhandled_expression' ; Type_max == 'unhandled_expression') ->
                        (Flow = 'carry_on',
                         common_util__error(6, "For statement range contains unhandled entities: it is skipped", "Soundness and completelness impaired", no_arguments, 614162, mika_symbolic, exec, no_localisation, no_extra_info)
                        )
                 ;
                        for_body(0, Loop_name, bran(Id_bran, deci(Id_deci, cond(Id_cond, NIdentifier))), Direction, [Min_symb, Max_symb], [Min_cons, Max_cons], NStmts, Flow)
                 )
                )
        ).
%%%
exec(loop_stmt(Loop_name, 'loop', Stmts), Flow) :-
        !,
        loop_body(0, Loop_name, Stmts, Flow).
%%%
exec(loop_stmt(Loop_name, 'while', bran(Id_bran, deci(Id_deci, Expression)), Stmts), Flow) :-
        !,
        while_body(0, Loop_name, bran(Id_bran, deci(Id_deci, Expression)), Stmts, Flow).
%for debugging add directly in foo.pl file
exec('trace', 'carry_on') :- trace.
exec('cover_trace', 'carry_on').
%%%
%we cannot use what is below because we need to backtrack if coverage is not finished: and below catches a backtracking exec/1
%exec(Something_else) :-
%        common_util__error(10, "(Catch all) Unhandled execution of ", "Cannot proceed", [(something_else, Something_else)], 10130601, mika_symbolic, exec, exec(a), "Should never happen")
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     END    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%