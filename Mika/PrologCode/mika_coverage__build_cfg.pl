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
% mika_coverage__cover.pl
% module mika_coverage
% builds the cfg using cover/2 for statements and cover_exp/1 for expressions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% What follows is the construction of the control flow graph from the source code
%   and the overall branches, decisions and conditions
% using create_arc/2 and the global variables current_arc_bran|deci (true or false) and
% the global variables current_node_bran|deci (an id)
% CHECK : the entire Ada syntax needs to be handled (parser -> coverage -> symbolic interpretation)

:- dynamic is_a_procedure/2.
:- dynamic is_a_function/2.

add_procedure(Subprogram_name, Info) :-
        (is_a_procedure(Subprogram_name, _) ->
                (retract(is_a_procedure(Subprogram_name, _)),
                 !      %needed because 'retract' above is non deterministic
                )
        ;
                true
        ),
        assert(is_a_procedure(Subprogram_name, Info)).

add_function(Subprogram_name, Info) :-
        (is_a_function(Subprogram_name, _) ->
                (retract(is_a_function(Subprogram_name, _)),
                 !      %needed because 'retract' above is non deterministic
                )
        ;
                true
        ),
        assert(is_a_function(Subprogram_name, Info)).

check_rename_of_rename(Name, Original_name) :-
        (is_a_rename(Name, Previous_name) ->
                check_rename_of_rename(Previous_name, Original_name)
        ;
                Original_name = Name
        ).

%remove 'selected' in fornt of names (prefix removal)
handle_selected_name(Selected_exp, Functor) :-
        (var(Selected_exp) ->
                Selected_exp = Functor
        ;
         Selected_exp = selected(_Selector, Exp) ->
                handle_selected_name(Exp, Functor)
        ;
                Selected_exp = Functor
        ).
%%%
%collects all the labeled statements in a sequence of statements
mika_coverage__search_local_labeled_statements([], []) :-
        !.
mika_coverage__search_local_labeled_statements([labeled_statement(Label, Statement)|Rest], [label(Label, [labeled_statement(Label, Statement)|Rest])|Rest_labels]) :-
        !,
        mika_coverage__search_local_labeled_statements([Statement|Rest], Rest_labels).
mika_coverage__search_local_labeled_statements([_|Rest], Rest_labels) :-
        !,
        mika_coverage__search_local_labeled_statements(Rest, Rest_labels).
%%%
%looks for a specific label and returns the target statements
mika_coverage__is_local_label([], _Label_sought, _Target_statements) :-
        !,
        fail.
mika_coverage__is_local_label([label(Label, Statements)|Rest_labels], Label_sought, Target_statements) :-
        !,
        (Label == Label_sought ->
                Target_statements = Statements
        ;
                mika_coverage__is_local_label(Rest_labels, Label_sought, Target_statements)
        ).
%%%
%handles sequences of statement respecting the flow of the previously covered statement
cover_sequences_of_statements([], carry_on, _Local_labels) :-  %if we have reach beyond the last statement Flow is carry_on
        !.
cover_sequences_of_statements([Next|Rest], Flow, Local_labels) :-
        !,
        cover(Next, Intermediate_flow),
        (Intermediate_flow == 'carry_on' ->
                cover_sequences_of_statements(Rest, Flow, Local_labels)
        ;
         common_util:common_util__is_a_goto(Intermediate_flow) ->
                (Intermediate_flow = goto(Label),
                 (mika_coverage__is_local_label(Local_labels, Label, Target_statements) ->
                        cover_sequences_of_statements(Target_statements, Flow, Local_labels)
                 ;
                        Flow = Intermediate_flow      %propagated upwards: cover_sequences_of_statements terminates and so does the initial enclosing cover(stmts(Statements))  which will therefore return control here
                 )
                )
        ;
                Flow = Intermediate_flow    %(exit, return, exit_named_loop) and the rest is not covered
        ).

%%%
% Flow is an out parameter, it can take the following values : 'carry_on', 'exit' (from exit statements) or 'return' (from return statements) ; it is necessary for proper cfg building
% Labels is an in parameter ; it is necessary to handle gotos
cover([mika_ref(_)], carry_on) :-
        !.
%handles sequences things : of top level entries in foo.pl, declarations etc.
cover([], carry_on) :-
        !.
cover([Entry|R], Flow) :-
        !,
        cover(Entry, Intermediate_flow),
        (Intermediate_flow == carry_on ->
                cover(R, Flow)
        ;
                common_util__error(10, "Intermediate flow is not carry_on", "Should never happen", [(intermediate_flow, Intermediate_flow)], 106536, mika_coverage, cover, cover([_], c), "Investigate problem asap")
        ).

%goto statements and labeled statements are caught here
%return values from exit statements ('exit') and return statements ('return') are handled here and will be caught in loop statements and subprogram_bodyies respectively
cover(stmts(Statements), Flow) :-
        !,
        mika_coverage__search_local_labeled_statements(Statements, Local_labels),
        cover_sequences_of_statements(Statements, Flow, Local_labels).

%labeled statements are handled in cover(stmts ...
cover(labeled_statement(_Label, Statement), Flow) :-
        !,
        cover(Statement, Flow).
%Flow becomes goto(Label) will be handled by cover_sequences_of_statements predicate
cover(goto(Label), goto(Label)) :-
        !.
cover(nothing, carry_on) :-                     %nothing done as nothing to do
        !.
cover(generic, carry_on) :-                     %nothing done as nothing to do
        !.
cover(is_intrinsic(Name, Intrinsic_variable), carry_on) :-
        !,
        ((nonvar(Name), memberchk(Name, [unchecked_conversion, unchecked_deallocation])) ->
                (%unchecked_conversion and unchecked_deallocation are generic : do the same as for cover(procedure_body in generic case
                 mika_name_atts:mika_name_atts__get(name, Intrinsic_variable, Subprogram_name),        %the xrefed name
                 (memberchk(Name, [unchecked_conversion]) ->    %intrinsic functions
                        add_function(Subprogram_name, generic)
                 ;
                  memberchk(Name, [unchecked_deallocation]) ->    %intrinsic procedure
                        add_procedure(Subprogram_name, generic)
                 ),
                 \+     (mika_globals:mika_globals__set_BT(current_node_bran, start(Subprogram_name)),
			 mika_globals:mika_globals__set_BT(current_node_deci, start(Subprogram_name)),
			 mika_globals:mika_globals__set_BT(current_arc_bran, true),
			 mika_globals:mika_globals__set_BT(current_arc_deci, true),
			 create_arc(branch_decision, end, end),
			 fail		%to cover entire subprogram
                        )
                )
        ;
                common_util__error(10, "Intrinsic entity is unknown during cfg building", no_error_consequences, [(subprogram_name, Subprogram_name)], 108022, mika_coverage, cover, cover(is_intrinsic(_), carry_on), no_extra_info)
        ).
cover(empty_body, carry_on) :-                  %nothing done as nothing to do
        !.
cover(empty_declarative_part, carry_on) :-      %nothing done as nothing to do
        !.
cover(atomic_pragma(_), carry_on) :-            %nothing done as nothing to do
        !.
cover(pragma(_, _), carry_on) :-                %nothing done as nothing to do
        !.
cover(use_clause, carry_on) :-                          %nothing done as nothing to do
        !.
cover(exception_declaration(_), carry_on) :-          %nothing done as nothing to do
        !.
cover(rename(_Object_id, Subtype_name_exp, Name_exp), carry_on) :-
        !,
        cover_exp(Subtype_name_exp),
        cover_exp(Name_exp).
cover(rename_exception(_, _), carry_on) :-              %nothing done as nothing to do
        !.
cover(generic_package_rename(_, _), carry_on) :-        %nothing done as nothing to do
        !.
cover(generic_subprogram_rename(_, _), carry_on) :-     %nothing done as nothing to do
        !.
cover(package_rename(_Compound_name_exp, Package_exp), carry_on) :-
        !,
        cover_exp(Package_exp).
cover(subprogram_rename(_Overriding_indicator_opt, Decl, Name_exp), carry_on) :-
        !,
        mika_symbolic:handle_selected_name(Name_exp, Original_sub_var),
        (mika_name_atts:mika_name_atts__is_name_atts(Original_sub_var) ->      %should always be the case
                mika_name_atts:mika_name_atts__get(name, Original_sub_var, Original_name_tmp)
        ;
                (%02/05/08 but sometimes e.g. "<=" in boxtempeartures_data.ads is renamed to something that is not explicitly defined ... [could issue a warning : but not really our role ...]
                 Original_name_tmp = Original_sub_var
                )
        ),
        check_rename_of_rename(Original_name_tmp, Original_name),
        (Decl = procedure_body(Compound_name, _, _) ->
                true
        ;
         Decl = function_body(Compound_name, _, _, _) ->
                true
        ),
        mika_symbolic:handle_compound_name(Compound_name, Sub_var),     %needed?
        mika_name_atts:mika_name_atts__get(name, Sub_var, Name),
        (Decl = procedure_body(_, _, _) ->
                add_procedure(Name, rename)
        ;
         Decl = function_body(_, _, _, _) ->
                add_function(Name, rename)
        ),
        (is_a_rename(Name, Original_name) ->
                true
        ;
                assert(is_a_rename(Name, Original_name))                %Name is an instance of Original_name
        ).

cover(generic_package_specification(Generic_formal_part, _Package_specification), carry_on) :-
        !,
        %trace,
        cover(Generic_formal_part, carry_on).   %may contain generic subprograms declarations
cover(generic_package_instantiation(_Compound_name, _Generic_inst), carry_on) :-
        !.

cover(generic_subprogram_specification(_Generic_formal_part, Subprogram_specification), carry_on) :-
        !,
        (Subprogram_specification = procedure_body(Compound_name, _, _) ->
		true
        ;
         Subprogram_specification = function_body(Compound_name, _, _, _) ->
		true
        ),
        mika_symbolic:handle_compound_name(Compound_name, Generic_var),
        mika_name_atts:mika_name_atts__get(name, Generic_var, Name),       %the xref ada name.
        (Subprogram_specification = procedure_body(_, _, _) ->
		add_procedure(Name, generic)
        ;
         Subprogram_specification = function_body(_, _, _, _) ->
		add_function(Name, generic)
        ).
cover(generic_subprogram_instantiation(_Overriding_indicator_opt, Subprogram_specification, Generic_inst), carry_on) :-
        !,
        %trace,
        (Subprogram_specification = procedure_body(Compound_name, no_return, parameters([])) ->
		true
        ;
         Subprogram_specification = Compound_name ->    %it is a function
		true
        ),
        mika_name_atts:mika_name_atts__get(name, Compound_name, Subprogram_name),      %since 10/12/07 compound_names do not exist any more (see ada.y)
        (Subprogram_specification = procedure_body(_, _, _) ->
		add_procedure(Subprogram_name, instance)
        ;
		add_function(Subprogram_name, instance)
        ),
        %we need to obtain the name of the generic subprogram
        %Generic_inst is similar to a procedure call see 'exec(procedure_call(Call), carry_on)'
        (Generic_inst = indexed(Name_exp, _) ->           %Name_exp can be an indentifier, an operator symbol or a sequence of select(...,...)
                handle_selected_name(Name_exp, Generic_var)     %from mika_symbolic__execute_util.pl : removes prefix
        ;
         Generic_inst = selected(_, _) ->                       %a selected subprogram instantiation without parameters
                handle_selected_name(Generic_inst, Generic_var) %from mika_symbolic__execute_util.pl : removes prefix
        ;
         Generic_inst = Generic_var ->                              %a non selected subprogram instantiation without parameters
                true
        ),
        mika_name_atts:mika_name_atts__get(name, Generic_var, Generic_name),
        (is_an_instance(Subprogram_name, Generic_name) ->
                true
        ;
                assert(is_an_instance(Subprogram_name, Generic_name))           %Subprogram_name is an instance of Generic_name
        ).

cover(package_specification(_Name, local_decl(Local_decls), Private), carry_on) :-
	!,
        cover(Local_decls, carry_on),
                 (Private == no_private ->
                        true            %nothing done as nothing to do
                 ;
                  Private = private(Private_decls) ->
                        cover(Private_decls, carry_on)
                 ).
/*        (\+
                (cover(Local_decls, carry_on),
                 (Private == no_private ->
                        true            %nothing done as nothing to do
                 ;
                  Private = private(Private_decls) ->
                        cover(Private_decls, carry_on)
                 ),
                 create_arc(branch_decision, end, end),
                 fail		%to cover the entire package specification
                )
        ).
*/

cover(package_body(_Compound_name, local_decl(Local_decls), Body), carry_on) :-
        !,
        cover(Local_decls, carry_on),
        cover(Body, carry_on).

/*        (\+
                (cover(Local_decls, carry_on),
                 cover(Body, carry_on),
                 create_arc(branch_decision, end, end),
                 fail		%to cover the entire package body
                )
        ).
*/
%block statement
cover(block(_Label, local_decl(Local_decls), Body), Flow) :-    %the label provided for a block can be totally ignored : only used for scope resolution (not for goto nor exit statements)
        !,
        cover(Local_decls, carry_on),
        cover(Body, Flow).

%can be part of a block statement or subprogram body
cover(body(Stmts, _Exception_handler_part), Flow) :-
        !,
        cover(Stmts, Flow).

cover(subprogram_body(_Overriding_indicator_opt, procedure_body(Compound_name, _, _, local_decl(Decls), Body)), carry_on) :-
        !,
        %trace,
        mika_symbolic:handle_compound_name(Compound_name, Sub_var), %needed?
	(mika_name_atts:mika_name_atts__is_name_atts(Sub_var) ->
		(mika_name_atts:mika_name_atts__get(name, Sub_var, Subprogram_name),
                 add_procedure(Subprogram_name, bodied),
		 (\+    (mika_globals:mika_globals__set_BT(current_node_bran, start(Subprogram_name)),
			 mika_globals:mika_globals__set_BT(current_node_deci, start(Subprogram_name)),
			 mika_globals:mika_globals__set_BT(current_arc_bran, true),
			 mika_globals:mika_globals__set_BT(current_arc_deci, true),
			 cover(Decls, carry_on),
			 cover(Body, Flow),
			 ((Flow == carry_on ; Flow == return) ->
				true
			 ;
                                common_util__error(10, "Flow is wrong during cfg building", no_error_consequences, [(subprogram_name, Subprogram_name), (flow, Flow)], 1019556, mika_coverage, cover, cover(procedure_body), "a procedure must end with 'carry_on' or 'return'")
			 ),
			 create_arc(branch_decision, end, end),
			 fail		%to cover entire subprogram
			)
		 )
		)
).

cover(subprogram_body(_Overriding_indicator_opt, function_body(Compound_name, _, _, _, local_decl(Decls), Body)), carry_on) :-
        !,
        mika_symbolic:handle_compound_name(Compound_name, Sub_var), %needed?
	(mika_name_atts:mika_name_atts__is_name_atts(Sub_var) ->
		(mika_name_atts:mika_name_atts__get(name, Sub_var, Subprogram_name),
                 add_function(Subprogram_name, bodied),
                 (\+    (mika_globals:mika_globals__set_BT(current_node_bran, start(Subprogram_name)),
		         mika_globals:mika_globals__set_BT(current_node_deci, start(Subprogram_name)),
                         mika_globals:mika_globals__set_BT(current_arc_bran, true),
		         mika_globals:mika_globals__set_BT(current_arc_deci, true),
                         cover(Decls, carry_on),
                         cover(Body, Flow),
                         (Flow == return ->     %should always be the case
                                true
                         ;
                                common_util__error(8, "Not all path end with a return statement in function during cfg building", no_error_consequences, [(subprogram_name, Subprogram_name)], 224149, mika_coverage, cover, cover(function_body), no_extra_info)
                         ),
		         create_arc(branch_decision, end, end),
                         fail		%to cover entire subprogram
                        )
	         )
                )
).

cover(subprogram_declaration(_Overriding_indicator_opt, Decl), carry_on) :-
        !,
        (Decl = procedure_body(Compound_name, _, _) ->
                true
        ;
         Decl = function_body(Compound_name, _, _, _) ->
                true
        ),
        mika_symbolic:handle_compound_name(Compound_name, Sub_var),
        mika_name_atts:mika_name_atts__get(name, Sub_var, Name),
        (Decl = procedure_body(_, _, _) ->
		add_procedure(Name, declared)
        ;
         Decl = function_body(_, _, _, _) ->
		add_function(Name, declared)
        ).

cover(trace, carry_on) :-
        !.
cover(cover_trace, carry_on) :- !, trace.
cover(abstract_subprogram_declaration(_Overriding_indicator_opt, _), carry_on) :-
        !.
cover(null_prodecure_declaration(_Overriding_indicator_opt, _Decl), carry_on) :-
        !.
cover(type(_New_type_var, _Discriminant_part, new(Derived_type_definition)), carry_on) :-
        !,
        cover_exp(Derived_type_definition).
cover(type(_New_type_var, _Discriminant_part, abstract_new(Derived_type_definition)), carry_on) :-
        !,
        cover_exp(Derived_type_definition).
cover(type(_Type_var, _Discriminant_part, integer, _Integer_type_mark, range([Min_exp, Max_exp])), carry_on) :-
        !,
        cover_exp(Min_exp),
        cover_exp(Max_exp).
cover(type(_Type_var, _Discriminant_part, integer, _Integer_type_mark, mod(Mod_exp)), carry_on) :-
        !,
        cover_exp(Mod_exp).
cover(type(_Type_var, _Discriminant_part, float, _Float_type_mark, digits(Digits_exp, range([Min_exp, Max_exp]))), carry_on) :-
        !,
        cover_exp(Digits_exp),
        cover_exp(Min_exp),
        cover_exp(Max_exp).
cover(type(_Type_var, _Discriminant_part, float, _Float_type_mark, digits(Digits_exp, empty_range)), carry_on) :-
        !,
        cover_exp(Digits_exp).
cover(type(_Type_var, _Discriminant_part, fixed, _Float_type_mark, delta(Delta_exp, range([Min_exp, Max_exp]))), carry_on) :-
        !,
        cover_exp(Delta_exp),
        cover_exp(Min_exp),
        cover_exp(Max_exp).
cover(type(_Type_var, _Discriminant_part, fixed, _Float_type_mark, delta_digits(Delta_exp, Digits_exp, range([Min_exp, Max_exp]))), carry_on) :-
        !,
        cover_exp(Delta_exp),
        cover_exp(Digits_exp),
        cover_exp(Min_exp),
        cover_exp(Max_exp).
cover(type(_Type_var, _Discriminant_part, fixed, _Float_type_mark, delta_digits(Delta_exp, Digits_exp)), carry_on) :-
        !,
        cover_exp(Delta_exp),
        cover_exp(Digits_exp).
cover(type(_Type_var, _Discriminant_part, private, _Tag, _Limitation), carry_on) :-
        !.
cover(type(_Type_var, _Discriminant_part, enumeration, _LiteralL), carry_on) :-
        !.
cover(type(_Type_var, _Discriminant_part, _Tag, _Limitation, record(FieldL_in)), carry_on) :-
	!,
        cover_fieldL(FieldL_in).
cover(type(_Type_var, _Discriminant_part, unconst_array, _Index_subtypeL, Component_definition), carry_on) :-
        !,
        cover_exp(Component_definition).
cover(type(_Type_var, _Discriminant_part, array, _IndexL_in, Component_definition), carry_on) :-
        !,
        cover_exp(Component_definition).
cover(type(_Type_var, _Discriminant_part, interface, _Kind, _Interface_list), carry_on) :-
        !.
%not handled or nothing todo : to be checked
cover(type(_Type_var, _Discriminant_part, _Null_Exclusion, access, _Subtype_indication), carry_on) :-
        !.
cover(type(_Type_var, _Discriminant_part, _Null_Exclusion, access_all, _Subtype_indication), carry_on) :-
        !.
cover(type(_Type_var, _Discriminant_part, _Null_Exclusion, access_constant, _Subtype_indication), carry_on) :-
        !.
cover(type(_Type_var, _Discriminant_part, _Null_Exclusion, access_procedure, _Protected, _Formal_part), carry_on) :-
        !.
cover(type(_Type_var, _Discriminant_part, _Null_Exclusion, access_function, _Protected, _Formal_part, _Mark), carry_on) :-
        !.
cover(type(_Type_var, _Discriminant_part, empty_type_completion), carry_on) :-
        !.
cover(type(_Type_var, _Discriminant_part, is_tagged), carry_on) :-
        !.
cover(type(_Type_var, _Generic_discriminant, box), carry_on) :-
        !.
cover(type(_Type_var, _Generic_discriminant, range_box), carry_on) :-
        !.
cover(type(_Type_var, _Generic_discriminant, mod_box), carry_on) :-
        !.
cover(type(_Type_var, _Generic_discriminant, delta_box), carry_on) :-
        !.
cover(type(_Type_var, _Generic_discriminant, delta_digits), carry_on) :-
        !.
cover(type(_Type_var, _Generic_discriminant, digits_box), carry_on) :-
        !.

%%%
cover(param(_, in, _, no_init), carry_on) :-
        !.
cover(with_subprogram(Subprogram_body, _), carry_on) :-
        !,
        (Subprogram_body = procedure_body(Compound_name, _, _) ->
                true
        ;
         Subprogram_body = function_body(Compound_name, _, _, _) ->
                true
        ),
        mika_symbolic:handle_compound_name(Compound_name, Sub_var),
        mika_name_atts:mika_name_atts__get(name, Sub_var, Subprogram_name),
        add_procedure(Subprogram_name, generic),
        %29/04/08 we create a fake cfg because it will not have a body but will be called within the generic body
        (\+     (mika_globals:mika_globals__set_BT(current_node_bran, start(Subprogram_name)),
                 mika_globals:mika_globals__set_BT(current_node_deci, start(Subprogram_name)),
                 mika_globals:mika_globals__set_BT(current_arc_bran, true),
                 mika_globals:mika_globals__set_BT(current_arc_deci, true),
                 create_arc(branch_decision, end, end),
                 fail
                )
        ).
%%%
cover(subtype(_Type_var, Subtype_indication), carry_on) :-
        !,
        cover_exp(Subtype_indication).

cover(number([], _), carry_on) :-
        !.
cover(number([_Name_atts|_Rest], Exp), carry_on) :-
        !,
        cover_exp(Exp). %only one coverage necessary
%null statement
cover(null, carry_on) :-
        !.
cover(representation_clause(_Type_var, Exp), carry_on) :-
	!,
        cover_exp(Exp).
cover(record_representation_clause(_Target_record, _Mod, _ClauseL), carry_on) :-
	!.
cover(at_representation_clause(_Type_var, Exp), carry_on) :-
	!,
        cover_exp(Exp).

cover(object(_Qualifier, _NameL, Subtype_indication_or_access_definition, Init), carry_on) :-
        !,
        cover_exp(Subtype_indication_or_access_definition),
        cover_exp(Init).
%Array_kind : array|unconst_array
cover(object(_Qualifier, _NameL, _Array_kind, IndexL_in, Component_definition, Init), carry_on) :-
        !,
        cover_exp(IndexL_in),
        cover_exp(Component_definition),
        cover_exp(Init).

cover(task_body, carry_on) :-                   %nothing done as nothing to do
        !.
cover(protected_body, carry_on) :-              %nothing done as nothing to do
        !.
cover(body_stub(_Stub_name), carry_on) :-     %added 04/07/09
        !.
cover(match_body(_Stub_name, Body), carry_on) :-     %added 04/07/09
        !,
        cover(Body, carry_on).
cover(return(_Return), return) :-
        !.
cover(return(_Return, Expression), return) :-
        !,
        cover_exp(Expression).
cover(extended_return(_Identifier, _Constant_opt, Subtype_indication_or_access_definition, Init_opt, Stmts), return) :-
        !,
        cover_exp(Subtype_indication_or_access_definition),
        cover_exp(Init_opt),
        cover(Stmts, _Flow).    %should always be return but to be checked (e.g. could it be exit?), could we have gotos to labels in the enclosing sequence of statements in extended_returns? should be checked

%30/08/09 we should take the name into consideration ... the name should resolved to an enclosing loop statement
cover(exit_when(Loop_name, 'no_when'), Flow) :-
        !,
        (Loop_name == 'no_name' ->
                Flow = 'exit'
        ;
                Flow = exit_named_loop(Loop_name)
        ).
%30/08/09 we should take the name into consideration ... the name should resolved to an enclosing loop statement
cover(exit_when(Loop_name, bran(Id_bran, deci(Id_deci, Expression))), Flow) :-
        !,
        add_decision_to_overall_mcdc_deci(Id_deci),
	cover_exp(Expression),		%28/11/06 put before create_arc (to allow correct coverage of subprogram calls in the expression)
        create_arc(branch_decision, Id_bran, Id_deci),
        (
                (mika_globals:mika_globals__set_BT(current_arc_bran, true),
		 mika_globals:mika_globals__set_BT(current_arc_deci, true),
                 (Loop_name == 'no_name' ->
                        Flow = 'exit'
                 ;
                        Flow = exit_named_loop(Loop_name)
                 )
                )
        ;
                (mika_globals:mika_globals__set_BT(current_arc_bran, false),
		 mika_globals:mika_globals__set_BT(current_arc_deci, false),
                 Flow = carry_on
                )
        ).
cover(assign(_Name, Expression), carry_on) :-
        !,
        cover_exp(Expression).
cover(procedure_call(Call), carry_on) :-
        !,
        (Call = indexed(Name_exp, Operand) ->
                (handle_selected_name(Name_exp, Functor),       %imported from mika_symbolic__execute_util.pl
                 cover_exp(Operand)
                )
        ;
         Call = selected(_, _) ->
                handle_selected_name(Call, Functor)
        ;
         Call = Functor ->     %a procedure call without parameters
                true    %see below
        ),
        (mika_name_atts:mika_name_atts__is_name_atts(Functor) ->
                (mika_name_atts:mika_name_atts__get(name, Functor, Subprogram_name),
                 mika_globals:mika_globals__get_BT(current_node_bran, N_bran),
	         mika_globals:mika_globals__get_BT(current_node_deci, N_deci),
                 mika_globals:mika_globals__get_BT(current_arc_bran, B_bran),        %true or false
	         mika_globals:mika_globals__get_BT(current_arc_deci, B_deci),        %true or false
	         (call_bran(N_bran, Subprogram_name, B_bran) ->
		        true
	         ;
		        assert(call_bran(N_bran, Subprogram_name, B_bran))	%and the call is not followed
	         ),
	         (call_deci(N_deci, Subprogram_name, B_deci) ->
		        true
	         ;
		        assert(call_deci(N_deci, Subprogram_name, B_deci))	%and the call is not followed
	         )
                )
        ;
         Functor == all ->
                (%explicit deference : a pointer to a procedure
                 common_util__error(4, "Explicit deferences of a pointer to a procedure are not handled during cfg buidling", "the cfg may be incomplete", [(call, Call)], 448359, mika_coverage, cover, cover(procedure_call(_), carry_on), "How can this be handled??")
                )
        ;
                common_util__error(8, "Expected a procedure here but found something else during cfg building", no_error_consequences, [(call, Call), (functor, Functor)], 848841, mika_coverage, cover, cover(procedure_call(Call), carry_on), "Investigate problem")
        ).
%copied to cover_expr(if_expr
cover(if_stmt([if_clause(bran(Id_bran, deci(Id_deci, Expression)), Stmts)|Rest_if_clauses], else(Else_stmts)), Flow) :-
        !,
        add_decision_to_overall_mcdc_deci(Id_deci),
        cover_exp(Expression),        %27/10/06 put before create_arc (to allow correct coverage of subprogram calls in the expression)
	create_arc(branch_decision, Id_bran, Id_deci),
        (
                (mika_globals:mika_globals__set_BT(current_arc_bran, true),
		 mika_globals:mika_globals__set_BT(current_arc_deci, true),
                 cover(Stmts, Flow)
                )
        ;
                (mika_globals:mika_globals__set_BT(current_arc_bran, false),
		 mika_globals:mika_globals__set_BT(current_arc_deci, false),
                 (Rest_if_clauses == [] ->
                        cover(Else_stmts, Flow)
                 ;
                        cover(if_stmt(Rest_if_clauses, else(Else_stmts)), Flow)
                 )
                )
        ).

%Expression is covered only once
cover(case_stmt(Expression, Alternatives), Flow) :-
	!,
	cover_exp(Expression),
	cover(case_stmt2(Alternatives), Flow).
%31/05/06 & 16/10/06
%the last alternative, if it is reached must be followed (cannot be false) but
cover(case_stmt2([alternative('others', Stmts)]), Flow) :-
	!,
	cover(Stmts, Flow).
cover(case_stmt2([alternative(bran(Id_bran, deci(Id_deci, Choices)), Stmts)|Rest_alternatives]), Flow) :-
	!,
        add_decision_to_overall_mcdc_deci(Id_deci),
	cover_exp(Choices),		%28/11/06 put before create_arc (to allow correct coverage of subprogram calls in the expression)
	create_arc(branch_decision, Id_bran, Id_deci),
	(
                (mika_globals:mika_globals__set_BT(current_arc_bran, true),
		 mika_globals:mika_globals__set_BT(current_arc_deci, true),
                 cover(Stmts, Flow)
                )
        ;
                (mika_globals:mika_globals__set_BT(current_arc_bran, false),
		 mika_globals:mika_globals__set_BT(current_arc_deci, false),
                 (Rest_alternatives == [] ->
                        Flow = carry_on
                 ;
                        cover(case_stmt2(Rest_alternatives), Flow)
                 )
                )
        ).

%the three loop forms are treated in a very similar manner
cover(loop_stmt(Loop_name, for, bran(Id_bran, deci(Id_deci, cond(_Id_cond, _))), _Direction, Discrete_range, Stmts), Flow) :-
	!,
        add_decision_to_overall_mcdc_deci(Id_deci),
	cover_exp(Discrete_range),	%28/11/06 put before create_arc (to allow correct coverage of subprogram calls in the expression)
	create_arc(branch_decision, Id_bran, Id_deci),
	(
                (mika_globals:mika_globals__set_BT(current_arc_bran, true),
		 mika_globals:mika_globals__set_BT(current_arc_deci, true),
                 cover(Stmts, Intermediate_flow),
                 (Intermediate_flow == 'exit' ->		%this is correct
                        Flow = 'carry_on'			%the statements have exited, 1 level of exit only
                 ;
                  Intermediate_flow == 'return' ->
                        Flow = Intermediate_flow
                 ;
                  common_util:common_util__is_a_goto(Intermediate_flow) ->
                        Flow = Intermediate_flow
                 ;
                  common_util:common_util__is_a_exit_named_loop(Intermediate_flow) ->
                        (Intermediate_flow = exit_named_loop(To_name),
                         (To_name == Loop_name ->
                                Flow = 'carry_on'
                         ;
                                Flow = exit_named_loop(To_name)
                         )
                        )
                 ;
                  Intermediate_flow == 'carry_on' ->
                        (create_arc('branch_decision', Id_bran, Id_deci),	%to create the backward arcs
			 Flow = 'carry_on',		%will be undone on backtracking but left for style reasons
                         fail	%this is needed but not too sure why 29/05/05 why???
                        )
                 )
                )
        ;
                (mika_globals:mika_globals__set_BT('current_arc_bran', 'false'),
		 mika_globals:mika_globals__set_BT('current_arc_deci', 'false'),
                 Flow = 'carry_on'
                )
        ).

cover(loop_stmt(Loop_name, loop, Stmts), Flow) :-
	!,
        cover(Stmts, Intermediate_flow),
        (Intermediate_flow == 'exit' ->		%this is correct
                Flow = 'carry_on'			%the statements have exited, 1 level of exit only
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
                (%20/10/06 we have taken out the fake id for basic loops but we still need to create the backward arc, so we call
		 %    the loop recursively.
		 %  the reason we do not end up in an infinite loop here is due to the fails within create_arc whenever we try to
		 %    create duplicate arcs.
		 cover(loop_stmt(Loop_name, loop, Stmts), Flow)	%so this works. [01/09/09 : this is very weird ... :but correct because we have no Id_bran, nor Id_deci]
                )
        ).
cover(loop_stmt(Loop_name, while, bran(Id_bran, deci(Id_deci, Expression)), Stmts), Flow) :-
	!,
        add_decision_to_overall_mcdc_deci(Id_deci),
	cover_exp(Expression),		%28/11/06 put before create_arc (to allow correct coverage of subprogram calls in the expression)
	create_arc(branch_decision, Id_bran, Id_deci),
        (
                (mika_globals:mika_globals__set_BT(current_arc_bran, true),
		 mika_globals:mika_globals__set_BT(current_arc_deci, true),
                 cover(Stmts, Intermediate_flow),
                 (Intermediate_flow == 'exit'->		%this is correct
                        Flow = carry_on			%the statements have exited, 1 level of exit only
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
                  Intermediate_flow == carry_on ->
                        (create_arc(branch_decision, Id_bran, Id_deci),	%to create the backward arcs
			 Flow = carry_on, 		%will be undone on backtracking but left for style reasons
                         fail	%this is needed but not too sure why 29/05/05 why???
                        )
                 )
                )
        ;
                (mika_globals:mika_globals__set_BT(current_arc_bran, false),
		 mika_globals:mika_globals__set_BT(current_arc_deci, false),
                 Flow = carry_on
                )
        ).

cover(raise_statement(Exception_name, String_expression), return) :-    %08/01/10 was carry_on
        !,
        cover_exp(Exception_name),
        cover_exp(String_expression).

cover(requeue_statement, carry_on) :-
        !.
cover(requeue_abort_statement, carry_on) :-
        !.

cover(Unknown, Flow) :-
        %trace,
        %fail.
        common_util__error(10, "Unexpected statement during cfg building", no_error_consequences, [(statement, Unknown), (flow, Flow)], 1063637, mika_coverage, cover, cover(unknown), "Investigate why this knind of statement occurs during cfg building").

%%%
%covers the list of fields in a record declaration in case of initialisations
cover_fieldL(null_record) :-
        !.      %needed : indexing is not clever enough : 29/04/08
cover_fieldL([variant]) :-
        !.      %needed : indexing is not clever enough : 29/04/08
cover_fieldL([no_variant]) :-
        !.      %needed : indexing is not clever enough : 29/04/08
cover_fieldL([(_Field_varsL, component_definition(_Aliased, Subtype_indication), Init)|Rest]) :-
	cover_exp(Subtype_indication),
        (Init == no_init ->
		true
	;
		cover_exp(Init)
	),
	cover_fieldL(Rest).

%%%
%scanning expressions : necessary for constructing CFG due to function calls
%follows the structure symbolically_execute/3 in midoan_symbolic__execute.pl
cover_exp(Var) :-
	var(Var),
	!,
	(mika_name_atts:mika_name_atts__is_name_atts(Var) ->
	        (mika_name_atts:mika_name_atts__get(name, Var, Subprogram_name),
                 (is_a_function(Subprogram_name, _) ->
                        (%so, we have a function call
                         mika_globals:mika_globals__get_BT(current_node_bran, N_bran),
                         mika_globals:mika_globals__get_BT(current_arc_bran, B_bran),        %true or false
                         (call_bran(N_bran, Subprogram_name, B_bran) ->
			        true
		         ;
			        assert(call_bran(N_bran, Subprogram_name, B_bran))	%and the call is not followed
		         ),
		         mika_globals:mika_globals__get_BT(current_node_deci, N_deci),
		         mika_globals:mika_globals__get_BT(current_arc_deci, B_deci),        %true or false
		         (call_deci(N_deci, Subprogram_name, B_deci) ->
			        true
		         ;
			        assert(call_deci(N_deci, Subprogram_name, B_deci))	%and the call is not followed
		         )
	                )
                 ;
                        %not a function could be a type, a field name, a package etc.
                        true
                 )
                )
        ;
         mika_unhandled_atts:mika_unhandled_atts__is_unhandled_atts(Var) ->
                (mika_unhandled_atts:mika_unhandled_atts__get(name, Var, Name),
                 common_util__error(4, "Unhandled variable during cfg building", "contents, if any, will not be covered", [(name, Name)], 468619, mika_coverage, cover_exp, cover_exp(var), no_extra_info)
                )
        ;
                common_util__error(10, "Unexpected variable during cfg building", no_error_consequences, [(var, Var)], 1068941, mika_coverage, cover_exp, cover_exp(var), "Investigate why this kind of variable appears during cfg building")
        ).

%e.g. A:=x>6
cover_exp(deci(Id_deci, Expression)) :-
	!,
        add_decision_to_overall_mcdc_deci(Id_deci),
	cover_exp(Expression),	%28/11/06 put before create_arc (to allow correct coverage of subprogram calls in the expression)
	create_arc(decision, _, Id_deci),
	(
		mika_globals:mika_globals__set_BT(current_arc_deci, true)
	;                                                       %choice point
                mika_globals:mika_globals__set_BT(current_arc_deci, false)
	).

cover_exp(cond(_Id_cond, Expression)) :-
	!,
	cover_exp(Expression).

%copy of cover(if_stmt(
cover_exp(if_expr([if_expr_clause(bran(Id_bran, deci(Id_deci, Expression)), Expresssion_expression)|Rest_if_expr_clauses], else_expression(Else_expression))) :-
        !,
        add_decision_to_overall_mcdc_deci(Id_deci),
        cover_exp(Expression),
	create_arc(branch_decision, Id_bran, Id_deci),
        (
                (mika_globals:mika_globals__set_BT(current_arc_bran, true),
		 mika_globals:mika_globals__set_BT(current_arc_deci, true),
                 cover_exp(Expresssion_expression)
                )
        ;
                (mika_globals:mika_globals__set_BT(current_arc_bran, false),
		 mika_globals:mika_globals__set_BT(current_arc_deci, false),
                 (Rest_if_expr_clauses == [] ->
                        (Else_expression == true ->     %i.e. none
                                true
                        ;
                                cover_exp(Else_expression)
                        )
                 ;
                        cover_exp(if_expr(Rest_if_expr_clauses, else_expression(Else_expression)))
                 )
                )
        ).

cover_exp(discrete_with_range(Exp)) :-
        !,
        cover_exp(Exp).

cover_exp(subtype_indication(_Null_exclusion, Name_exp, Constraint_exp)) :-
        !,
        cover_exp(Name_exp),
        (Constraint_exp = no_constraint ->
                true
        ;
         Constraint_exp = constraint(range(Range_exp)) ->
                cover_exp(Range_exp)
        ;
         Constraint_exp = constraint(digits(Digits_exp, Range_constraint_opt)) ->
                (cover_exp(Digits_exp),
                 (Range_constraint_opt == empty_range ->
                        true
                 ;
                        (Range_constraint_opt = range(Range_exp),
                         cover_exp(Range_exp)
                        )
                 )
                )
        ;
         Constraint_exp = constraint(delta(Delta_exp, Range_constraint_opt)) ->
                (cover_exp(Delta_exp),
                 (Range_constraint_opt == empty_range ->
                        true
                 ;
                        (Range_constraint_opt = range(Range_exp),
                         cover_exp(Range_exp)
                        )
                 )
                )
        ).
cover_exp(tic(Name, Range_Attribute_or_Expression)) :-
	!,
        cover_exp(Name),
        cover_exp(Range_Attribute_or_Expression).
cover_exp(tic(Name, range, Exp)) :-
	!,
	cover_exp(Name),
	cover_exp(Exp).

cover_exp(range(Type_var, box)) :-
        !,
        cover_exp(Type_var).
%Functor can be a 'mark', a 'name'
%operand can be a 'direct_name', a 'character_literal', an 'operator_symbol', the terminal 'all'
cover_exp(selected(Functor, Operand)) :-
	!,
	cover_exp(Operand),
	cover_exp(Functor).

cover_exp(indexed(Functor, Operand)) :-
	!,
        cover_exp(Operand),
	cover_exp(Functor).

cover_exp(named(Choices, Expression)) :-
        !,
        cover_exp(Choices),
        cover_exp(Expression).

cover_exp(allocator(Name)) :-
	!,
        cover_exp(Name).

cover_exp(agg(Agg_exp)) :-
        !,
        cover_exp(Agg_exp).

cover_exp(rat(_, _)) :-
	!.

cover_exp(N) :-
	atomic(N),      %an atom or a number: catches others, no_discriminant, box, range etc.
	!.

cover_exp(string(_)) :-
        !.

cover_exp(access_definition(_Null_Exclusion, _Kind, _Name)) :-
        !.
cover_exp(access_definition(_Null_Exclusion, _Kind, _Protection, _Parameters)) :-
        !.
cover_exp(access_definition(_Null_Exclusion, _Kind, _Protection, _Parameters, _Return)) :-
        !.
cover_exp(derived_type_definition(_Kind, Subtype_indication)) :-
        !,
        cover_exp(Subtype_indication).
cover_exp(derived_type_definition(_Kind, Subtype_indication, _Interface_list, with_private)) :-
        !,
        cover_exp(Subtype_indication).
cover_exp(derived_type_definition(_Kind, Subtype_indication, _Interface_list, record(_))) :-
        !,
        cover_exp(Subtype_indication).
cover_exp(component_definition(_Alias_opt, Subtype_indication_or_access_definition)) :-
        !,
        cover_exp(Subtype_indication_or_access_definition).
cover_exp((A, B)) :-
        !,
        cover_exp(A),
        cover_exp(B).
cover_exp([]) :-
        !.
cover_exp([F|Rest]) :-	%e.g. for choices
	!,
	cover_exp(F),
	cover_exp(Rest).
cover_exp(and(Gate_nb, Le_exp, Ri_exp)) :-
        !,
        add_gate_to_overall_mcdc_deci(Gate_nb, and),
        cover_exp(Le_exp),
        cover_exp(Ri_exp).
cover_exp(or(Gate_nb, Le_exp, Ri_exp)) :-
        !,
        add_gate_to_overall_mcdc_deci(Gate_nb, or),
        cover_exp(Le_exp),
        cover_exp(Ri_exp).
cover_exp(and_then(Gate_nb, Le_exp, Ri_exp)) :-
        !,
        add_gate_to_overall_mcdc_deci(Gate_nb, and_then),
        cover_exp(Le_exp),
        cover_exp(Ri_exp).
cover_exp(or_else(Gate_nb, Le_exp, Ri_exp)) :-
        !,
        add_gate_to_overall_mcdc_deci(Gate_nb, or_else),
        cover_exp(Le_exp),
        cover_exp(Ri_exp).
cover_exp(xor(Gate_nb, Le_exp, Ri_exp)) :-
        !,
        add_gate_to_overall_mcdc_deci(Gate_nb, xor),
        cover_exp(Le_exp),
        cover_exp(Ri_exp).
cover_exp(not(Gate_nb, Le_exp)) :-
        !,
        add_gate_to_overall_mcdc_deci(Gate_nb, not),
        cover_exp(Le_exp).
%dealing with binary expressions
cover_exp(Bin_exp) :-
        Bin_exp =.. [Functor, L, R],
        binary_functor(Functor, _),
        !,
        cover_exp(L),
        cover_exp(R).

%dealing with unary expressions
cover_exp(Una_exp) :-
        Una_exp =.. [Functor, L],
        unary_functor(Functor, _),
        !,
        cover_exp(L).

%dealing with ternary expressions
cover_exp(Ter_exp) :-
        Ter_exp =.. [Functor, One, Two, Three],
        ternary_functor(Functor, _),
        !,
        cover_exp(One),
        cover_exp(Two),
        cover_exp(Three).

cover_exp(Unknown) :-
        !,
        %trace,
        %fail.
        common_util__error(10, "Unexpected expression during cfg building", no_error_consequences, [(expression, Unknown)], 1082812, mika_coverage, cover_exp, cover_exp(unknown), "Investigate why this kind of expression occurs during cfg building").

%%%
%used within cover_exp/1
binary_functor(=, =).
binary_functor(<>, <>).
binary_functor(<, <).
binary_functor(<=, <=).
binary_functor(>, >).
binary_functor(>=, >=).
binary_functor(is_in, is_in).
binary_functor(is_not_in, is_not_in).
binary_functor(+, +).
binary_functor(-, -).
binary_functor(&, &).
binary_functor(*, *).
binary_functor(/, /).
binary_functor(mod, mod).
binary_functor(rem, rem).
binary_functor(**, **).
binary_functor(pos, pos).
binary_functor(val, val).
%
binary_functor(field, field).
binary_functor(element, element).
binary_functor(conversion, conversion).

unary_functor(+, +).
unary_functor(-, -).
unary_functor(abs, abs).
%
unary_functor(succ, succ).
unary_functor(pred, pred).
unary_functor(first, first).
unary_functor(last, last).

ternary_functor(up_rec, up_rec).
ternary_functor(up_arr, up_arr).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%END%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%