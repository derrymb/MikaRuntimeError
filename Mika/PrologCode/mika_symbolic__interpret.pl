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
% mika_symbolic__interpret.pl
% module mika_symbolic
% symbolic interpretation of intermediate Ada expressions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%
symbolically_interpret(Var, Symbolic, Constraint, Type, Exception) :-
	var(Var),
	!,
	(mika_seav_atts:mika_seav_atts__is_seav(Var) ->
		(mika_seav_atts:mika_seav_atts__get(mode, Var, Mode),
		 (Mode == unused ->
			(!,
                         mika_seav_atts:mika_seav_atts__get('type', Var, Type_mark),
			 midoan_type:midoan_type__variable_declaration(Constraint, Type_mark), %just to return Constraint
                         mika_seav_atts:mika_seav_atts__get('name', Var, Symbolic),	                %just to return Symbolic
                         mika_seav_atts:mika_seav_atts__update('mode', Var, in),
			 mika_seav_atts:mika_seav_atts__update('input_value', Var, Constraint)
                        )
		 ;
		  Mode == in ->
			(!,
                         mika_seav_atts:mika_seav_atts__get(name, Var, Symbolic),
			 mika_seav_atts:mika_seav_atts__get(input_value, Var, Constraint)
			)
		 ;
		  (Mode == in_out ; Mode == out ; Mode == out_elab ; Mode == init_elab) ->
			(!,
                         mika_seav_atts:mika_seav_atts__get(symbolic, Var, Symbolic),
			 mika_seav_atts:mika_seav_atts__get(constraint, Var, Constraint)
			)
		 ;
		  Mode == constant ->
			(!,
                         mika_seav_atts:mika_seav_atts__get(symbolic, Var, Symbolic),
			 mika_seav_atts:mika_seav_atts__get(input_value, Var, Constraint)
			)
		 ),
		 midoan_solver__interpret(Constraint, types(_), _, Type, _Exception_)	%only to retrieve the basic type of the variable
	        )
	;
	 mika_sub_atts:mika_sub_atts__is_sub_atts(Var) ->
		(%so, we have a function (must be a function since a result is expected) with no parameters called during symbolic interpretation ...)
                 !,
                 handle_subprogram_call(Var, [], Symbolic, Constraint, Type, Exception)
		)
        ;
	 mika_name_atts:mika_name_atts__is_name_atts(Var) ->
		(%can only be a record field or a label at this stage all other variables are transformed during their declaration into SEAVs, Type_vars or Sub_vars
                 !,
                 mika_name_atts:mika_name_atts__get('name', Var, Name),
		 Symbolic = Name,
		 Constraint = Name,
		 Type = 'name'
	        )
	;
         midoan_type:midoan_type__is_type(Var) ->
		(!,
                 midoan_type:midoan_type__get_typemark(Var, Type_mark),
		 Symbolic = Type_mark,
		 Constraint = Var,
		 Type = 'type'
		)
        ;
         mika_package_atts:mika_package_atts__is_package_atts(Var) ->
		(!,
                 mika_package_atts:mika_package_atts__get(Var, 'name', Xref_name),
		 Symbolic = Xref_name,
		 Constraint = Var,
		 Type = 'package'
		)
        ;
         mika_unhandled_atts:mika_unhandled_atts__is_unhandled_atts(Var) ->
                (!,
                 Symbolic = 'unhandled_expression',
                 Constraint = Var,
                 Type = 'unhandled_expression'
                )
        ;
                common_util__error(10, "Interpretation of an unknown variable", "Cannot proceed", [(var, Var)], 1090126, mika_symbolic, symbolically_interpret, symbolically_interpret(var), "Should never happen")
        ).

symbolically_interpret(trace, trace, trace, trace, _) :- !, trace.

%10/03/05 these come from operand freezing in freeze_arguments/2 from mika_symbolic__execute_util.pl
%Solver_var is a solver var, or a list of solver_vars (for array access) or a number
%basically these have already been symbolically interpreted (frozen) so hence, danger!; do not re-do it.
%see issue 'Freezing subprogram calls arguments'
symbolically_interpret(frozen(Solver_var), lost, Solver_var, Type, _Exception_) :-
        var(Solver_var),
	!,
        midoan_solver:get_var_type(Solver_var, Type).
symbolically_interpret(frozen(A_number), lost, A_number, Type, _Exception_) :-
        number(A_number),
	!,
        midoan_solver:get_var_type(A_number, Type).
symbolically_interpret(frozen(An_enum), lost, An_enum, e, _Exception_) :-
        midoan_enum:midoan_enum__is_enum(An_enum),
	!.
symbolically_interpret(frozen(A_modular_integer), lost, A_modular_integer, modular_integer, _Exception_) :-
        midoan_modular_integer:midoan_modular_integer__is_modular_integer(A_modular_integer),
	!.
symbolically_interpret(frozen([]), [], [], [], _Exception_) :-
	!.
symbolically_interpret(frozen([Solver_var|Rest_in]), [Symb|Rest_symb], [Solver_var|Rest_out], [Type|Rest_type], _Exception_) :-
        !,
	symbolically_interpret(frozen(Solver_var), Symb, Solver_var, Type, _Exception_),
        symbolically_interpret(frozen(Rest_in), Rest_symb, Rest_out, Rest_type, _Exception_).
symbolically_interpret(frozen(Expression), lost, Expression, lost, _Exception) :-     %added 14/05/2010
        !.
%%%
symbolically_interpret(N, N, Const_exp, Type, Exception) :-
	number(N),	%a number
	!,
	midoan_solver__interpret(N, types(_), Const_exp, Type, Exception).
%07/03/05 necessary during assign transformation in exec(assign(...))
symbolically_interpret(up_rec(Record_exp, Field, Exp), Symb, Const, Type, _Exception_) :-
        symbolically_interpret(Exp, Symbolic_exp, Constraint_exp, Type_exp, _Exception_),
	symbolically_interpret(Field, Symbolic_field, Constraint_field, Type_field, _Exception_),
	symbolically_interpret(Record_exp, Symbolic_record_exp, Constraint_record_exp, Type_record_exp, _Exception_),
        ((Type_exp == unhandled_expression ; Type_field == unhandled_expression ; Type_record_exp == unhandled_expression) ->
                (common_util__error(6, "Unhandled record update", "Will propagate upwards", [(up_rec(record_exp, field, exp), up_rec(Record_exp, Field, Exp))], 612812, mika_symbolic, symbolically_interpret, no_localisation, "Subset needs enlarging"),
                 Symb = unhandled_expression,
                 Type = unhandled_expression,
                 mika_unhandled_atts:mika_unhandled_atts__create(Const, unhandled_expression, up_rec(Record_exp, Field, Exp))
                )
        ;
                (midoan_solver__interpret(up_rec(Constraint_record_exp, Constraint_field, Constraint_exp), types(record, _, _), Const, Type, _Exception_),
                 Symb = up_rec(Symbolic_record_exp, Symbolic_field, Symbolic_exp)
                )
        ).
%10/03/05 necessary during assign transformation in exec(assign(...))
symbolically_interpret(up_arr(Array_exp, Index, Exp), Symb, Const, Type, _Exception_) :-
        symbolically_interpret(Exp, Symbolic_exp, Constraint_exp, Type_exp, _Exception_),
	symbolically_interpret(Index, Symbolic_index, Constraint_index, Type_index, _Exception_),
	symbolically_interpret(Array_exp, Symbolic_array_exp, Constraint_array_exp, Type_array_exp, _Exception_),
        (is_unhandled_type([Type_exp, Type_index, Type_array_exp]) ->
                (common_util__error(6, "Unhandled array update", "Will propagate upwards", [(up_arr(array_exp, index, exp), up_arr(Array_exp, Index, Exp))], 614534, mika_symbolic, symbolically_interpret, no_localisation, "Subset needs enlarging"),
                 Symb = unhandled_expression,
                 Type = unhandled_expression,
                 mika_unhandled_atts:mika_unhandled_atts__create(Const, unhandled_expression, up_arr(Array_exp, Index, Exp))
                )
        ;
                ((Constraint_index = [The_index], nonvar(The_index), The_index = [_From, _To]) ->
                        (midoan_solver__interpret(up_arr_slice(Constraint_array_exp, Constraint_index, Constraint_exp), types(array, _, Type_exp), Const, Type, _Exception_),
                         Symb = up_arr_slice(Symbolic_array_exp, Symbolic_index, Symbolic_exp)
                        )
	         ;
                        (midoan_solver__interpret(up_arr(Constraint_array_exp, Constraint_index, Constraint_exp), types(array, _, Type_exp), Const, Type, _Exception_),
                         Symb = up_arr(Symbolic_array_exp, Symbolic_index, Symbolic_exp)
                        )
                )
        ).
symbolically_interpret(selected(Functor, Operand), Symb, Const, Type, _Exception_) :-
        remove_prefix_util(selected(Functor, Operand), Prefix_less),    %could be a prefix or a record access
        symbolically_interpret(Prefix_less, Symb, Const, Type, _Exception_).
symbolically_interpret(record_access(Functor, Operand), Symb, Const, Type, _Exception_) :-
        symbolically_interpret(Functor, Symbolic_functor, Constraint_functor, Type_functor, _Exception_),
	symbolically_interpret(Operand, Symbolic_operand, Constraint_operand, Type_operand, _Exception_),
        ((Type_functor == unhandled_expression ; Type_operand == unhandled_expression) ->
                (common_util__error(6, "Unhandled record access", "Will propagate upwards", [(record_access(functor, operand), record_access(Functor, Operand))], 617037, mika_symbolic, symbolically_interpret, no_localisation, "Subset needs enlarging"),
                 Symb = unhandled_expression,
                 Type = unhandled_expression,
                 mika_unhandled_atts:mika_unhandled_atts__create(Const, unhandled_expression, record_access(Functor, Operand))
                )
        ;
	        ((midoan_record:midoan_record__is_record(Constraint_functor) -> %this is a record access
		        (midoan_solver__interpret(field(Constraint_functor, Constraint_operand), types(record, _), Const, Type, _Exception_),
		         Symb = field(Symbolic_functor, Symbolic_operand)
		        )
	         ;
                        common_util__error(10, "Unhandled symbolic interpretation", "Cannot proceed", [(record_access(functor, operand), record_access(Functor, Operand))], 10257170, mika_symbolic, symbolically_interpret, symbolically_interpret(record_access(a, b)), "Should never happen")
                 )
                )
        ).
%Functor is <name> in the grammar
symbolically_interpret(indexed(Functor, Operand), Symb, Const, Type, Exception) :-
        %problem here : we need to know if Functor represents a function, an array or etc.
        %the logical thing is to call symbolically_interpret on Functor to get back Constraint_functor which we can test
	%  for type membership
        % BUT if it represents, a function is will be executed by symbolically_interpret as a function with no parameters (which fails)
	%  ... which is obvious wrong here since there are parameters
        % the Functor can itself be complex as it could be another array access (indexed(indexed(Array, [1]), [42]) a type_conversion etc
	%  all nested
        %so if Functor represents a function it should not be executed during symbolically_interpret otherwise it safe and necessary
	%  to call symbolically_interpret
        %If Functor is a  mika_sub_atts__is_sub_atts then we deal we it here and then
        %   otherwise do as usual (will cope with <package_name>.<function_name>)

        ((var(Functor), mika_sub_atts:mika_sub_atts__is_sub_atts(Functor)) -> %a function with parameters call
                (Operand == [] ->                               %was already identified as parameterless
                        handle_subprogram_call(Functor, [], Symb, Const, Type, Exception)
                ;
                        (mika_sub_atts:mika_sub_atts__get(params, Functor, Params),   %just to check if it is a parameterless function
                         (Params == [] ->                       %a parameterless function whose result is indexed
                                symbolically_interpret(indexed(indexed(Functor, []), Operand), Symb, Const, Type, Exception)       %the parameterless function will be called first
                         ;
                                handle_subprogram_call(Functor, Operand, Symb, Const, Type, Exception)	%leads to exec/2
                         )
                        )
                )
        ;
         Functor = selected(_, _) ->              %added 11/03/08
                (remove_prefix_util(Functor, Prefix_less),      %could be a prefix or a record access
                 symbolically_interpret(indexed(Prefix_less, Operand), Symb, Const, Type, _Exception_)
                )
        ;
         (Functor = tic(Array_type_or_var, Attribute), (Attribute == 'last' ; Attribute == 'first' ; Attribute == 'length')) ->         %e.g. AA'last(2) is indexed(tic(Var_Seav(AA), last), [2])
                (symbolically_interpret(Operand, [Symbolic_operand], [Constraint_operand], Type_operand, _Exception_),
                 (is_unhandled_type([Type_operand]) ->
                        (common_util__error(6, "Unhandled tic expression operand", "Will propagate upwards", [(operand, Operand)], 622222, mika_symbolic, symbolically_interpret, no_localisation, "Subset needs enlarging"),
                         Symb = unhandled_expression,
                         Type = unhandled_expression,
                         mika_unhandled_atts:mika_unhandled_atts__create(Const, unhandled_expression, cantbebothered)
                        )
                 ;
                        (syntactically_denote(Array_type_or_var, Array_type_or_var_name, Entity, Type_or_object),
                         (Type_or_object == type ->
                                Array_type_var = Entity
                         ;
                          Type_or_object == object ->
                                mika_seav_atts:mika_seav_atts__get(type, Entity, Array_type_var)
                         ),
                         (Attribute == last ->
                                (midoan_solver__interpret(last(Array_type_var, Constraint_operand), types(type), Const, Type, 'no_exception'),
                                 Symb = last(Array_type_or_var_name, Symbolic_operand)
                                )
                         ;
                         Attribute == first ->
                                (midoan_solver__interpret(first(Array_type_var, Constraint_operand), types(type), Const, Type, 'no_exception'),
                                 Symb = first(Array_type_or_var_name, Symbolic_operand)
                                )
                         ;
                         Attribute == length ->
                                (midoan_solver__interpret(length(Array_type_var, Constraint_operand), types(type), Const, Type, _Exception_),
                                 Symb = length(Array_type_or_var_name, Symbolic_operand)
                                )
                         )
                        )
                 )
                )
        ;
                (symbolically_interpret(Functor, Symbolic_functor, Constraint_functor, Type_functor, _Exception_),
                 symbolically_interpret(Operand, Symbolic_operand, Constraint_operand, Type_operand, _Exception_),
                 (is_unhandled_type([Type_functor, Type_operand]) ->
                        (common_util__error(6, "Unhandled indexed expression", "Will propagate upwards", [(indexed(symbolic_functor, symbolic_operand), indexed(Symbolic_functor, Symbolic_operand))], 622559, mika_symbolic, symbolically_interpret, symbolically_interpret(indexed(a, b)), "Subset needs enlarging"),
                         Symb = unhandled_expression,
                         Type = unhandled_expression,
                         mika_unhandled_atts:mika_unhandled_atts__create(Const, unhandled_expression, indexed(Symbolic_functor, Symbolic_operand))
                        )
                 ;
                  mika_sub_atts:mika_sub_atts__is_sub_atts(Constraint_functor) ->
                        symbolically_interpret(indexed(Functor, Operand), Symb, Const, Type, Exception)	%we start again ...:will be handled by above
                 ;
                  midoan_array:midoan_array__is_array(Constraint_functor) -> %array access or slice
		        (((Constraint_operand = [The_index], nonvar(The_index), The_index = [_From, _To]) ->
                                (midoan_solver__interpret(slice(Constraint_functor, Constraint_operand), types('array', _), Const, Type, Exception),
                                 Symb = slice(Symbolic_functor, Symbolic_operand)
                                )
                         ;
                                (midoan_solver__interpret(element(Constraint_functor, Constraint_operand), types('array', _), Const, Type, Exception),
                                 Symb = element(Symbolic_functor, Symbolic_operand)
                                )
                         )
		        )
	         ;
                  Type_functor == 'type' ->    %a type conversion: the operand has to be an array (not an aggregate, nor a string_literal), a float or an integer
                        (Symbolic_operand = [Symbolic_operand2],        %only one operand in the list
                         Constraint_operand = [Constraint_operand2],
                         midoan_solver__interpret(conversion(Constraint_functor, Constraint_operand2), types(_, _), Const, Type, _Exception_),
		         Symb = conversion(Symbolic_functor, Symbolic_operand2)
                        )
                 ;
                  (Constraint_functor = tic(Type_var, Attribute), (Attribute == 'pos' ; Attribute == 'val' ; Attribute == 'pred' ; Attribute == 'succ')) ->
                        (Symbolic_operand = [Symbolic_operand2],        %only one operand in the list
                         Constraint_operand = [Constraint_operand2],
                         midoan_type:midoan_type__get_typemark(Type_var, Typemark),
                         Type_operand = [Type_operand2],
                         (Attribute == 'pos' ->
		                (midoan_solver__interpret(pos(Type_var, Constraint_operand2), types(_, Type_operand2), Const, Type, _Exception_),
		                 Symb = pos(Typemark, Symbolic_operand2)
                                )
                         ;
                          Attribute == 'val' ->
		                (midoan_solver__interpret(val(Type_var, Constraint_operand2), types(_, Type_operand2), Const, Type, _Exception_),
		                 Symb = val(Typemark, Symbolic_operand2)
                                )
                         ;
                          Attribute == 'pred' ->
		                (midoan_solver__interpret(pred(Constraint_operand2), types(Type_operand2), Const, Type, _Exception_),
		                 Symb = pred(Symbolic_operand2)
                                )
                         ;
                          Attribute == 'succ' ->
		                (midoan_solver__interpret(succ(Constraint_operand2), types(Type_operand2), Const, Type, _Exception_),
		                 Symb = succ(Symbolic_operand2)
                                )
                         )
                        )
	         ;
                  Constraint_functor = tic(Type_var, 'min') ->
                        (midoan_type:midoan_type__get_typemark(Type_var, Typemark),
                         Symbolic_operand = [Symbolic_operand1, Symbolic_operand2],        %only two operand in the list
                         Constraint_operand = [Constraint_operand1, Constraint_operand2],
                         Type_operand = [Type_operand1, Type_operand2],
                         midoan_solver__interpret(min(Type_var, Constraint_operand1, Constraint_operand2), types(_, Type_operand1, Type_operand2), Const, Type, _Exception_),
                         Symb = min(Typemark, Symbolic_operand1, Symbolic_operand2)
                        )
                 ;
                  Constraint_functor = tic(Type_var, 'max') ->
                        (midoan_type:midoan_type__get_typemark(Type_var, Typemark),
                         Symbolic_operand = [Symbolic_operand1, Symbolic_operand2],        %only two operand in the list
                         Constraint_operand = [Constraint_operand1, Constraint_operand2],
                         Type_operand = [Type_operand1, Type_operand2],
                         midoan_solver__interpret(max(Type_var, Constraint_operand1, Constraint_operand2), types(_, Type_operand1, Type_operand2), Const, Type, _Exception_),
                         Symb = max(Typemark, Symbolic_operand1, Symbolic_operand2)
                        )
                 ;
                  midoan_string_literal:midoan_string_literal__is_string_literal(Constraint_functor) ->
                        (%a string literal e.g. from selected(Standard_341, string([45])) standard."+"
                         midoan_string_literal:midoan_string_literal__get_ascii_codes(Constraint_functor, Codes),       %just retrieving the attribute
                         (Constraint_operand = [Le, Ri] ->
                                (find_string_operator(binary, Codes, Op),
                                 Operation =.. [Op, Le, Ri],
                                 symbolically_interpret(Operation, Symb, Const, Type, _Exception_)
                                )
                         ;
                          Constraint_operand = [Le] ->
                                (find_string_operator(unary, Codes, Op),
                                 Operation =.. [Op, Le],
                                 symbolically_interpret(Operation, Symb, Const, Type, _Exception_)
                                )
                         ;
                                common_util__error(10, "Calling an undefined standard string operator", "Cannot proceed", [(codes, Codes)], 10397146, mika_symbolic, symbolically_interpret, symbolically_interpret(indexed(a, b)), "Should never happen")
                         )
                        )
                 ;
                        common_util__error(10, "Unhandled interpretation", "Cannot proceed", [(indexed(functor, operand), indexed(Functor, Operand))], 10401166, mika_symbolic, symbolically_interpret, symbolically_interpret(indexed(a, b)), "Should never happen")
                 )
                )
        ).
%could be a record or array aggregate
%it is at the moment anonymous so no array or record can be created
symbolically_interpret(agg(Agg_exp), Symb, Const, Type, _Exception_) :-
        symbolically_interpret(Agg_exp, Agg_exp_symb, Agg_exp_const, _Type_agg, _Exception_),
        check_for_unhandled(Agg_exp_const, Has_unhandled),
        (Has_unhandled == no ->
                (midoan_anon_aggregate:midoan_anon_aggregate__create_anon_aggregate(Agg_exp_const, Const),
                 Type = aggregate,
                 Symb = agg(Agg_exp_symb)
                )
        ;
                (common_util__error(6, "Aggregate contains unhandled entities", "Will propagate upwards", [(agg(agg_exp), agg(Agg_exp))], 636030, mika_symbolic, symbolically_interpret, no_localisation, "Subset needs enlarging"),
                 Symb = unhandled_expression,
                 Type = unhandled_expression,
                 mika_unhandled_atts:mika_unhandled_atts__create(Const, unhandled_expression, agg(Agg_exp_symb))
                )
        ).
%string literal
% a string literal is not an array of characters
% a string literal is not a positional aggregate of characters
% the basic problem with string literals is that they may not be made of characters (from standard) they could be made of roman digits ...
% so until it can be determined from its context (assignment, qualification ...) what kind of character literals the string is made up of, it has to stay a string literal
symbolically_interpret(string(CodeL_in), string(CodeL_out), Const, string_literal, _Exception_) :-
        !,
        clean_up_double_quotes(CodeL_in, CodeL_out),    %a 2 double quotes have to interpreted as 1 double quote in a string
        midoan_string_literal:midoan_string_literal__create_string_literal(CodeL_out, Const).
symbolically_interpret(tic(Prefix, Designator), Symb, Const, Type, Exception) :-
        symbolically_interpret(Designator, Symbolic_designator, Constraint_designator, Type_designator, _Exception_),
        (Type_designator == unhandled_expression ->
                (common_util__error(6, "Unhandled tic expression", "Will propagate upwards", [(tic(prefix, designator), tic(prefix, designator))], 638040, mika_symbolic, symbolically_interpret, no_localisation, "Subset needs enlarging"),
                 Symb = unhandled_expression,
                 Type = unhandled_expression,
                 mika_unhandled_atts:mika_unhandled_atts__create(Const, unhandled_expression, tic(Prefix, Designator))
                )
        ;
         Type_designator == 'valid' ->
                symbolically_interpret_boolean(tic(Prefix, 'valid'), Symb, Const, Type, Exception)
        ;
                (syntactically_denote(Prefix, Symbolic_prefix, Entity_prefix, Type_prefix),
                 (Type_designator == address ->  %for object, program unit or label
                        (common_util__error(5, "'Address expressions always evaluate to address '10'", no_error_consequences, [(expression, Symbolic_designator)], 5426163, mika_symbolic, symbolically_interpret, no_localisation, no_extra_info),
                         symbolically_interpret(10, _Symb, Const, Type, _Exception_),      %static address : 10 is returned
                         Symb = tic(Symbolic_prefix, Symbolic_designator)
                        )
                 ;
                  Type_prefix == object ->
                        (mika_seav_atts:mika_seav_atts__get(type, Entity_prefix, Array_type_var),
                         (Type_designator == first ->
			        (midoan_solver__interpret(first(Array_type_var, 1), types(type), Const, Type, 'no_exception'),      %for the first dimension
			         Symb = first(Symbolic_prefix, 1)
                                )
		         ;
		          Type_designator == last ->
			        (midoan_solver__interpret(last(Array_type_var, 1), types(type), Const, Type, 'no_exception'),       %for the first dimension
			         Symb = last(Symbolic_prefix, 1)
                                )
                         ;
                          Type_designator == length ->
			        (midoan_solver__interpret(length(Array_type_var, 1), types(type), Const, Type, _Exception_),     %for the first dimension
			         Symb = length(Symbolic_prefix, 1)
                                )
                         ;
                          Type_designator == 'dimensions' ->
			        (midoan_solver__interpret(dimensions(Array_type_var), types(type), Const, Type, _Exception_),
			         Symb = dimensions(Symbolic_prefix)
                                )
                         ;
                                common_util__error(10, "Unhandled interpretation (array tic)", "Cannot proceed", [(tic(array_type_var, designator), tic(Array_type_var, Designator))], 10442192, mika_symbolic, symbolically_interpret, symbolically_interpret(tic(a, b)), "Should never happen")
                         )
                        )
                 ;
                  Type_prefix == type ->
                        (Type_designator == modulus ->
                                (midoan_solver__interpret(modulus(Entity_prefix), types(_), Const, Type, _Exception_),
                                 Symb = modulus(Symbolic_prefix, Symbolic_designator)
                                )
                        ;
                         Type_designator == component_size ->
                                (midoan_solver__interpret(component_size(Entity_prefix), types(_), Const, Type, _Exception_),
                                 Symb = component_size(Symbolic_prefix, Symbolic_designator)
                                )
                        ;
                         Type_designator == first ->
		                (midoan_solver__interpret(first(Entity_prefix), types(_), Const, Type, 'no_exception'),
			         Symb = first(Symbolic_prefix, Symbolic_designator)
                                )
	                ;
		         Type_designator == last ->
		                (midoan_solver__interpret(last(Entity_prefix), types(_), Const, Type, 'no_exception'),
			         Symb = last(Symbolic_prefix, Symbolic_designator)
                                )
                        ;
                         Type_designator == length ->
		                (midoan_solver__interpret(length(Entity_prefix, 1), types(type), Const, Type, _Exception_),
		                 Symb = length(Symbolic_prefix, Symbolic_designator)
                                )
                        ;
                         Type_designator == delta ->
                                (midoan_solver__interpret(delta(Entity_prefix), types(_), Const, Type, _Exception_),
                                 Symb = delta(Symbolic_prefix, Symbolic_designator)
                                )
                        ;
                         Type_designator == digits ->
                                (midoan_solver__interpret(digits(Entity_prefix), types(_), Const, Type, _Exception_),
                                 Symb = digits(Symbolic_prefix, Symbolic_designator)
                                )
                        ;
                         Type_designator == max_size_in_storage_elements ->
                                (midoan_solver__interpret(max_size_in_storage_elements(Entity_prefix), types(_), Const, Type, _Exception_),
                                 Symb = max_size_in_storage_elements(Symbolic_prefix, Symbolic_designator)
                                )
                        ;
                         Type_designator == size ->
                                (midoan_solver__interpret(size(Entity_prefix), types(_), Const, Type, _Exception_),
                                 Symb = size(Symbolic_prefix, Symbolic_designator)
                                )
                        ;
                         Type_designator == small ->
                                (midoan_solver__interpret(small(Entity_prefix), types(_), Const, Type, _Exception_),
                                 Symb = small(Symbolic_prefix, Symbolic_designator)
                                )
		        ;
		         (Type_designator == pos ; Type_designator == val ; Type_designator == pred ; Type_designator == succ ;
                          Type_designator == min ; Type_designator == max) ->	%cannot be handled here
			        (Symb = tic(Symbolic_prefix, Type_designator),
			         Const = tic(Entity_prefix, Type_designator)
			        )
                        ;
		         Type_designator == range ->	%cannot be handled here
		                (Symb = range(Symbolic_prefix),
			         Const = range(Entity_prefix)
			        )
                        ;
                         Type_designator == aggregate ->        %qualification of an aggregate (could be a record or array aggregate, the type_mark can be a record, constrained_array or unconstrained array)
                                (midoan_type:midoan_type__variable_declaration(Const, Entity_prefix),
                                 midoan_solver__controlled_unification(Const, Constraint_designator),
                                 Symb = Symbolic_designator
                                )
                        ;
                         Type_designator == string_literal ->        %qualification of a string literal (the type_mark could be a constrained array of characters or an unconstrained array of characters)
                                (midoan_type:midoan_type__variable_declaration(Const, Entity_prefix),
                                 midoan_solver__controlled_unification(Const, Constraint_designator),
                                 Symb = Symbolic_designator
                                )
                        ;
                         Type_designator == i ->                %qualification of an integer expression
                                (midoan_type:midoan_type__obtain_basetype(Entity_prefix, 'standard.ads:integer') ->           %has to be an integer
                                        (midoan_solver__interpret(first(Entity_prefix), types(_), Min, Type_first, 'no_exception'),
                                         midoan_solver__interpret(last(Entity_prefix), types(_), Max, Type_last, 'no_exception'),
                                         ((Type_first == unhandled_expression ; Type_last == unhandled_expression) ->
                                                (common_util__error(9, "In integer expression qualification bounds are unknown", "Will propagate upwards", [(qual(prefix, designator), qual(Symbolic_prefix, Symbolic_designator))], 951322, mika_symbolic, symbolically_interpret, symbolically_interpret(tic(a, b)), "Should never happen"),
                                                 Symb = unhandled_expression,
                                                 Type = unhandled_expression,
                                                 mika_unhandled_atts:mika_unhandled_atts__create(Const, unhandled_expression, tic(Symbolic_prefix, Symbolic_designator))
                                                )
                                         ;
                                                ((midoan_solver__sdl(>=(Constraint_designator, Min)), midoan_solver__sdl(<=(Constraint_designator, Max))) ->
                                                        (Const = Constraint_designator,
                                                         Symb = qual(Symbolic_prefix, Symbolic_designator),
                                                         Type = Type_designator
                                                        )
                                                ;
                                                        (common_util__error(3, "CONSTRAINT_ERROR raised systematically in this path in integer qualified expression", "Systematic exception in your code will raised for this path", [(qual(symbolic_prefix, symbolic_designator), qual(Symbolic_prefix, Symbolic_designator))], 3548325, mika_symbolic, symbolically_interpret, no_localisation, no_extra_info),
		                                         fail	%to induce backtracking : we have to try to go through this assignment via a different path
		                                        )
                                                )
                                         )
                                        )
                                ;
                                        (common_util__error(3, "CONSTRAINT_ERROR raised systematically in this path in integer qualified expression because the type mark is not an integer", "Systematic exception in your code will raised for this path", [(qual(symbolic_prefix, symbolic_designator), qual(Symbolic_prefix, Symbolic_designator))], 3554733, mika_symbolic, symbolically_interpret, no_localisation, no_extra_info),
		                         fail	%to induce backtracking : we have to try to go through this assignment via a different path
		                        )
                                )
                        ;
                         Type_designator == modular_integer ->                %qualification of a modular_integer expression
                                (midoan_type:midoan_type__obtain_basetype(Entity_prefix, modular_integer) ->           %has to be a modular integer
                                        (midoan_solver__interpret(first(Entity_prefix), types(_), Min, Type_first, 'no_exception'),
                                         midoan_solver__interpret(last(Entity_prefix), types(_), Max, Type_last, 'no_exception'),
                                         ((Type_first == unhandled_expression ; Type_last == unhandled_expression) ->
                                                (common_util__error(9, "In integer expression qualification bounds are unknown", "Will propagate upwards", [(qual(prefix, designator), qual(Symbolic_prefix, Symbolic_designator))], 954650, mika_symbolic, symbolically_interpret, symbolically_interpret(tic(a, b)), "Should never happen"),
                                                 Symb = unhandled_expression,
                                                 Type = unhandled_expression,
                                                 mika_unhandled_atts:mika_unhandled_atts__create(Const, unhandled_expression, tic(Symbolic_prefix, Symbolic_designator))
                                                )
                                         ;
                                                ((midoan_solver__sdl(>=(Constraint_designator, Min)), midoan_solver__sdl(<=(Constraint_designator, Max))) ->
                                                        (Const = Constraint_designator,
                                                         Symb = qual(Symbolic_prefix, Symbolic_designator),
                                                         Type = Type_designator
                                                        )
                                                ;
                                                        (common_util__error(3, "CONSTRAINT_ERROR raised systematically in this path in modular integer qualified expression", "Systematic exception in your code will raised for this path", [(qual(symbolic_prefix, symbolic_designator), qual(Symbolic_prefix, Symbolic_designator))], 355851, mika_symbolic, symbolically_interpret, no_localisation, no_extra_info),
		                                         fail	%to induce backtracking : we have to try to go through this assignment via a different path
		                                        )
                                                )
                                         )
                                        )
                                ;
                                        (common_util__error(3, "CONSTRAINT_ERROR raised systematically in this path in modular integer qualified expression because the type mark is not a modular integer", "Systematic exception in your code will raised for this path", [(qual(symbolic_prefix, symbolic_designator), qual(Symbolic_prefix, Symbolic_designator))], 356550, mika_symbolic, symbolically_interpret, no_localisation, no_extra_info),
		                         fail	%to induce backtracking : we have to try to go through this assignment via a different path
		                        )
                                )
                        ;
                         Type_designator == r ->                %qualification of a float expression
                                (midoan_type:midoan_type__obtain_basetype(Entity_prefix, 'standard.ads:float') ->           %has to be a float
                                        (midoan_solver__interpret(first(Entity_prefix), types(_), Min, Type_first, 'no_exception'),
                                         midoan_solver__interpret(last(Entity_prefix), types(_), Max, Type_last, 'no_exception'),
                                         ((Type_first == unhandled_expression ; Type_last == unhandled_expression) ->
                                                (common_util__error(9, "In float expression qualification bounds are unknown", "Will propagate upwards", [(qual(prefix, designator), qual(Symbolic_prefix, Symbolic_designator))], 954234, mika_symbolic, symbolically_interpret, symbolically_interpret(tic(a, b)), "Should never happen"),
                                                 Symb = unhandled_expression,
                                                 Type = unhandled_expression,
                                                 mika_unhandled_atts:mika_unhandled_atts__create(Const, unhandled_expression, tic(Symbolic_prefix, Symbolic_designator))
                                                )
                                         ;
                                                ((midoan_solver__sdl(>=(Constraint_designator, Min)), midoan_solver__sdl(<=(Constraint_designator, Max))) ->
                                                        (Const = Constraint_designator,
                                                         Symb = qual(Symbolic_prefix, Symbolic_designator),
                                                         Type = Type_designator
                                                        )
                                                ;
                                                        (common_util__error(3, "CONSTRAINT_ERROR raised systematically in this path float qualified expression", "Systematic exception in your code will raised for this path", [(qual(symbolic_prefix, symbolic_designator), qual(Symbolic_prefix, Symbolic_designator))], 3568316, mika_symbolic, symbolically_interpret, no_localisation, no_extra_info),
		                                         fail	%to induce backtracking : we have to try to go through this assignment via a different path
		                                        )
                                                )
                                         )
                                        )
                                ;
                                        (common_util__error(3, "CONSTRAINT_ERROR raised systematically in this path in float qualified expression because the type mark is not a float", "Systematic exception in your code will raised for this path", [(qual(symbolic_prefix, symbolic_designator), qual(Symbolic_prefix, Symbolic_designator))], 3574347, mika_symbolic, symbolically_interpret, no_localisation, no_extra_info),
		                         fail	%to induce backtracking : we have to try to go through this assignment via a different path
		                        )
                                )
                        ;
                         Type_designator == e ->                %qualification of a enumeration expression
                                (midoan_type:midoan_type__obtain_basetype(Entity_prefix, 'base_enumeration') ->           %has to be an enumeration type
                                        (midoan_solver__interpret(first(Entity_prefix), types(_), Min, Type_first, 'no_exception'),
                                         midoan_solver__interpret(last(Entity_prefix), types(_), Max, Type_last, 'no_exception'),
                                         ((Type_first == unhandled_expression ; Type_last == unhandled_expression) ->
                                                (common_util__error(9, "In enumeration expression qualification bounds are unknown", "Will propagate upwards", [(qual(prefix, designator), qual(Symbolic_prefix, Symbolic_designator))], 957042, mika_symbolic, symbolically_interpret, symbolically_interpret(tic(a, b)), "Should never happen"),
                                                 Symb = unhandled_expression,
                                                 Type = unhandled_expression,
                                                 mika_unhandled_atts:mika_unhandled_atts__create(Const, unhandled_expression, tic(Symbolic_prefix, Symbolic_designator))
                                                )
                                         ;
                                                ((midoan_solver__sdl(>=(Constraint_designator, Min)), midoan_solver__sdl(<=(Constraint_designator, Max))) ->
                                                        (Const = Constraint_designator,
                                                         Symb = qual(Symbolic_prefix, Symbolic_designator),
                                                         Type = Type_designator
                                                        )
                                                ;
                                                        (common_util__error(3, "CONSTRAINT_ERROR raised systematically in this path in enumeration qualified expression because the type mar is not an enumeration", "Systematic exception in your code will raised for this path", [(qual(symbolic_prefix, symbolic_designator), qual(Symbolic_prefix, Symbolic_designator))], 3588322, mika_symbolic, symbolically_interpret, no_localisation, no_extra_info),
		                                         fail	%to induce backtracking : we have to try to go through this assignment via a different path
		                                        )
                                                )
                                         )
                                        )
                                ;
                                        (common_util__error(3, "CONSTRAINT_ERROR raised systematically in this path in enumeration qualified expression because the type mark is not an enumeration", "Systematic exception in your code will raised for this path", [(qual(symbolic_prefix, symbolic_designator), qual(Symbolic_prefix, Symbolic_designator))], 3343346, mika_symbolic, symbolically_interpret, no_localisation, no_extra_info),
		                         fail	%to induce backtracking : we have to try to go through this assignment via a different path
		                        )
                                )
                        ;
                         Type_designator == record ->           %qualification of a record expression
                                (%08/01/09 added : todo if the types are not identical we should do qualification of each individual field
                                 Symb = qual(Symbolic_prefix, Symbolic_designator),
                                 Const = Constraint_designator,
                                 Type = record
                                )
                        ;
                         Type_designator == array ->           %added 22/04/09 qualification of an array expression
                                (Symb = qual(Symbolic_prefix, Symbolic_designator),
                                 Const = Constraint_designator,
                                 Type = array
                                )
                        ;
                                (common_util__error(6, "Unhandled tic expression", "Will propagate upwards", [print(tic(prefix, designator), tic(Symbolic_prefix, Symbolic_designator))], 652578, mika_symbolic, symbolically_interpret, symbolically_interpret(tic(a, b)), "Subset needs enlarging"),
                                 Symb = unhandled_expression,
                                 Type = unhandled_expression,
                                 mika_unhandled_atts:mika_unhandled_atts__create(Const, unhandled_expression, tic(Symbolic_prefix, Symbolic_designator))
                                )
                        )
                ;
                 (Type_prefix == package, Symbolic_prefix == 'standard.ads:standard') ->
                        (user:attr(Symbolic_designator, Value) ->       %define in attr.mika
                                (Symb = tic(Symbolic_prefix, Symbolic_designator),
                                 symbolically_interpret(Value, _, Const, Type, _Exception_)
                                )
                        ;
                                common_util__error(10, "GNAT package attribute is unknown", "Cannot proceed", [(tic(symbolic_prefix, symbolic_designator), tic(Symbolic_prefix, Symbolic_designator))], 10608199, mika_symbolic, symbolically_interpret, symbolically_interpret(tic(a, b)), "Should never happen")
                        )
                 ;
                        common_util__error(10, "Unhandled tic expression", "Cannot proceed", [(tic(prefix, designator), tic(Prefix, Designator))], 10611154, mika_symbolic, symbolically_interpret, symbolically_interpret(tic(a, b)), "Should never happen")
                 )
                )
        ).
symbolically_interpret(tic(Name, range, Exp), _Symb, _Const, _Type, _Exception_) :-
        common_util__error(10, "Unhandled interpretation", "Cannot proceed", [(tic(name, range, exp), tic(Name, range, Exp))], 1062239, mika_symbolic, symbolically_interpret, symbolically_interpret(tic(a, range, b)), "Should never happen").

symbolically_interpret(if_expr([if_expr_clause(Bran, Expresssion_expression)|Rest_if_expr_clauses], else_expression(Else_expression)), Symb, Const, Type, _Exception_) :-    %used exec(if_stmt ... as template
        check_for_unhandled(Bran, Has_unhandled),
        (Has_unhandled == 'no' ->
                (choose_truth(Bran, Outcome),
                 (Outcome == 'true' ->
                        symbolically_interpret(Expresssion_expression, Symb, Const, Type, _Exception_)
                 ;
                  Outcome == 'false' ->
                        (Rest_if_expr_clauses == [] ->
                                symbolically_interpret(Else_expression, Symb, Const, Type, _Exception_)
                        ;
                                symbolically_interpret(if_expr(Rest_if_expr_clauses, else_expression(Else_expression)), Symb, Const, Type, _Exception_)
                        )
                 )
                )
        ;
                (common_util__error(6, "Conditional expression contains unhandled entities: it is skipped", "Soundness and completelness impaired", [(bran, Bran)], 6931027, mika_symbolic, exec, no_localisation, no_extra_info),
                 Symb = 'unhandled_expression',
                 Type = 'unhandled_expression',
                 mika_unhandled_atts:mika_unhandled_atts__create(Const, 'unhandled_expression', if_expr([if_expr_clause(Bran)]))
                )
        ).


symbolically_interpret(allocator(Name), _Symb, _Const, _Type, _Exception_) :-
        common_util__error(10, "Unhandled interpretation", "Cannot proceed", [(allocator(name), allocator(Name))], 1062540, mika_symbolic, symbolically_interpret, symbolically_interpret(allocator(a)), "Should never happen").

%a decision needs to be interpreted
%e.g. as part of an assignment assign(Var_seav(bool.adb:4:4:b, unused), deci(3, cond(5, Var_seav(bool.adb:5:1:m, in) = -99)))
%will create choice points since it is a Boolean expression
%HACK! But works because we need to return the enumeration variable for the literal 'true' or 'false'
symbolically_interpret(deci(Id_deci, Expression), Symb, Const, Type, _Exception_) :-
	choose_truth(deci(Id_deci, Expression), Outcome),
        midoan_type:standard_type('boolean', Boolean_type_var),
	(Outcome == 'true' ->
		(midoan_type:midoan_type__get_attribute(Boolean_type_var, 'last', Const),
                 Type = 'b',
                 Symb = 'standard.ads:true'
		)
	;
	 Outcome == 'false' ->
		(midoan_type:midoan_type__get_attribute(Boolean_type_var, 'first', Const),
                 Type = 'b',
                 Symb = 'standard.ads:false'
		)
        ;
         Outcome = bitwise_deci(Id_deci, _Id_conds, arg(Symb, Const)) ->  %the entire decision was a bitwise expression
                Type = 'modular_integer'
	).
symbolically_interpret(+(Le), Symb, Const_exp, Type, _Exception_) :-
	symbolically_interpret(Le, Le_symb, Le_const, Le_type, _Exception_),
        (Le_type == unhandled_expression ->
                (common_util__error(6, "Unhandled unary plus expression", "Will propagate upwards", [(+(le_symb), +(Le_symb))], 661159, mika_symbolic, symbolically_interpret, symbolically_interpret(+(a)), "Subset needs enlarging"),
                 Symb = unhandled_expression,
                 Type = unhandled_expression,
                 mika_unhandled_atts:mika_unhandled_atts__create(Const_exp, unhandled_expression, +(Le_symb))
                )
        ;
	        (midoan_solver__interpret(Le_const, types(Le_type), Const_exp, Type, _Exception_),
                 Symb = +(Le_symb)
                )
        ).
symbolically_interpret(-(Le), Symb, Const_exp, Type, _Exception_) :-
	symbolically_interpret(Le, Le_symb, Le_const, Le_type, _Exception_),
        (Le_type == unhandled_expression ->
                (common_util__error(6, "Unhandled unary minus expression", "Will propagate upwards", [(-(le_symb), -(Le_symb))], 662259, mika_symbolic, symbolically_interpret, symbolically_interpret(-(a)), "Subset needs enlarging"),
                 Symb = unhandled_expression,
                 Type = unhandled_expression,
                 mika_unhandled_atts:mika_unhandled_atts__create(Const_exp, unhandled_expression, -(Le_symb))
                )
        ;
	        (midoan_solver__interpret(-(Le_const), types(Le_type), Const_exp, Type, _Exception_),
                 Symb = -(Le_symb)
                )
        ).
symbolically_interpret(abs(Le), Symb, Const_exp, Type, _Exception_) :-
	symbolically_interpret(Le, Le_symb, Le_const, Le_type, _Exception_),
        (Le_type == unhandled_expression ->
                (common_util__error(6, "Unhandled unary abs expression", "Will propagate upwards", [(abs(le_symb), abs(Le_symb))], 663959, mika_symbolic, symbolically_interpret, symbolically_interpret(abs(a)), "Subset needs enlarging"),
                 Symb = unhandled_expression,
                 Type = unhandled_expression,
                 mika_unhandled_atts:mika_unhandled_atts__create(Const_exp, unhandled_expression, abs(Le_symb))
                )
        ;
	        (midoan_solver__interpret(abs(Le_const), types(Le_type), Const_exp, Type, _Exception_),
                 Symb = abs(Le_symb)
                )
        ).
symbolically_interpret(+(Le, Ri), Symb, Const_exp, Type, _Exception_) :-
        symbolically_interpret(Le, Le_symb, Le_const, Le_type, _Exception_),
	symbolically_interpret(Ri, Ri_symb, Ri_const, Ri_type, _Exception_),
        ((Le_type == unhandled_expression ; Ri_type == unhandled_expression) ->
                (common_util__error(6, "Unhandled binary plus expression", "Will propagate upwards", [(+(le_symb, ri_symb), +(Le_symb, Ri_symb))], 660102, mika_symbolic, symbolically_interpret, symbolically_interpret(+(a, b)), "Subset needs enlarging"),
                 Symb = unhandled_expression,
                 Type = unhandled_expression,
                 mika_unhandled_atts:mika_unhandled_atts__create(Const_exp, unhandled_expression, +(Le_symb, Ri_symb))
                )
        ;
	        (midoan_solver__interpret(+(Le_const, Ri_const), types(Le_type, Ri_type), Const_exp, Type, _Exception_),
                 Symb = +(Le_symb, Ri_symb)
                )
        ).
symbolically_interpret(-(Le, Ri), Symb, Const_exp, Type, _Exception_) :-
        symbolically_interpret(Le, Le_symb, Le_const, Le_type, _Exception_),
	symbolically_interpret(Ri, Ri_symb, Ri_const, Ri_type, _Exception_),
        ((Le_type == unhandled_expression ; Ri_type == unhandled_expression) ->
                (common_util__error(6, "Unhandled binary minus expression", "Will propagate upwards", [(-(le_symb, ri_symb), -(Le_symb, Ri_symb))], 663958, mika_symbolic, symbolically_interpret, symbolically_interpret(-(a, b)), "Subset needs enlarging"),
                 Symb = unhandled_expression,
                 Type = unhandled_expression,
                 mika_unhandled_atts:mika_unhandled_atts__create(Const_exp, unhandled_expression, -(Le_symb, Ri_symb))
                )
        ;
	        (midoan_solver__interpret(-(Le_const, Ri_const), types(Le_type, Ri_type), Const_exp, Type, _Exception_),
                 Symb = -(Le_symb, Ri_symb)
                )
        ).
symbolically_interpret(/(Le, Ri), Symb, Const_exp, Type, _Exception_) :-
	symbolically_interpret(Le, Le_symb, Le_const, Le_type, _Exception_),
	symbolically_interpret(Ri, Ri_symb, Ri_const, Ri_type, _Exception_),
        ((Le_type == 'unhandled_expression' ; Ri_type == 'unhandled_expression') ->
                (common_util__error(6, "Unhandled binary div expression", "Will propagate upwards", [(/(le_symb, ri_symb), /(Le_symb, Ri_symb))], 665400, mika_symbolic, symbolically_interpret, symbolically_interpret(/(a, b)), "Subset needs enlarging"),
                 Symb = 'unhandled_expression',
                 Type = 'unhandled_expression',
                 mika_unhandled_atts:mika_unhandled_atts__create(Const_exp, 'unhandled_expression', /(Le_symb, Ri_symb))
                )
        ;
	        (midoan_solver__interpret(/(Le_const, Ri_const), types(Le_type, Ri_type), Const_exp, Type, _Exception_),
                 Symb = /(Le_symb, Ri_symb)
                )
        ).
symbolically_interpret(*(Le, Ri), Symb, Const_exp, Type, _Exception_) :-
	symbolically_interpret(Le, Le_symb, Le_const, Le_type, _Exception_),
	symbolically_interpret(Ri, Ri_symb, Ri_const, Ri_type, _Exception_),
	((Le_type == unhandled_expression ; Ri_type == unhandled_expression) ->
                (common_util__error(6, "Unhandled binary mult expression", "Will propagate upwards", [(*(le_symb, ri_symb), *(Le_symb, Ri_symb))], 666902, mika_symbolic, symbolically_interpret, symbolically_interpret(*(a, b)), "Subset needs enlarging"),
                 Symb = unhandled_expression,
                 Type = unhandled_expression,
                 mika_unhandled_atts:mika_unhandled_atts__create(Const_exp, unhandled_expression, *(Le_symb, Ri_symb))
                )
        ;
	        (midoan_solver__interpret(*(Le_const, Ri_const), types(Le_type, Ri_type), Const_exp, Type, _Exception_),
                 Symb = *(Le_symb, Ri_symb)
                )
        ).
symbolically_interpret(**(Le, Ri), Symb, Const_exp, Type, _Exception_) :-
	symbolically_interpret(Le, Le_symb, Le_const, Le_type, _Exception_),
	symbolically_interpret(Ri, Ri_symb, Ri_const, Ri_type, _Exception_),
        ((Le_type == unhandled_expression ; Ri_type == unhandled_expression) ->
                (common_util__error(6, "Unhandled binary power expression", "Will propagate upwards", [(**(le_symb, ri_symb), **(Le_symb, Ri_symb))], 668503, mika_symbolic, symbolically_interpret, symbolically_interpret(**(a, b)), "Subset needs enlarging"),
                 Symb = unhandled_expression,
                 Type = unhandled_expression,
                 mika_unhandled_atts:mika_unhandled_atts__create(Const_exp, unhandled_expression, **(Le_symb, Ri_symb))
                )
        ;
                (Symb = **(Le_symb, Ri_symb),
                 midoan_solver__interpret(**(Le_const, Ri_const), types(Le_type, Ri_type), Const_exp, Type, _Exception_)
                )
        ).
symbolically_interpret(mod(Le, Ri), Symb, Const_exp, Type, _Exception_) :-
	symbolically_interpret(Le, Le_symb, Le_const, Le_type, _Exception_),
	symbolically_interpret(Ri, Ri_symb, Ri_const, Ri_type, _Exception_),
	((Le_type == unhandled_expression ; Ri_type == unhandled_expression) ->
                (common_util__error(6, "Unhandled binary mod expression", "Will propagate upwards", [(mod(le_symb, ri_symb), mod(Le_symb, Ri_symb))], 671606, mika_symbolic, symbolically_interpret, symbolically_interpret(mod(a, b)), "Subset needs enlarging"),
                 Symb = unhandled_expression,
                 Type = unhandled_expression,
                 mika_unhandled_atts:mika_unhandled_atts__create(Const_exp, unhandled_expression, mod(Le_symb, Ri_symb))
                )
        ;
                (midoan_solver__interpret(mod(Le_const, Ri_const), types(Le_type, Ri_type), Const_exp, Type, _Exception_),
                 Symb = mod(Le_symb, Ri_symb)
                )
        ).
symbolically_interpret(rem(Le, Ri), Symb, Const_exp, Type, _Exception_) :-
	symbolically_interpret(Le, Le_symb, Le_const, Le_type, _Exception_),
	symbolically_interpret(Ri, Ri_symb, Ri_const, Ri_type, _Exception_),
        ((Le_type == unhandled_expression ; Ri_type == unhandled_expression) ->
                (common_util__error(6, "Unhandled binary rem expression", "Will propagate upwards", [(rem(le_symb, ri_symb), rem(Le_symb, Ri_symb))], 673531, mika_symbolic, symbolically_interpret, symbolically_interpret(rem(a, b)), "Subset needs enlarging"),
                 Symb = unhandled_expression,
                 Type = unhandled_expression,
                 mika_unhandled_atts:mika_unhandled_atts__create(Const_exp, unhandled_expression, rem(Le_symb, Ri_symb))
                )
        ;
	        (midoan_solver__interpret(rem(Le_const, Ri_const), types(Le_type, Ri_type), Const_exp, Type, _Exception_),
                 Symb = rem(Le_symb, Ri_symb)
                )
        ).
%%%
%one-dimensional array concatenation
%one or both(!) operand may be of the component type( typical example : "dshdjds" & 'l'
symbolically_interpret(&(Le, Ri), Symb, Array_var, Type, _Exception_) :-
	%trace,
        symbolically_interpret(Le, Le_symb, Le_const, Le_type, _Exception_),
	symbolically_interpret(Ri, Ri_symb, Ri_const, Ri_type, _Exception_),
        ((Le_type == unhandled_expression ; Ri_type == unhandled_expression) ->
                (common_util__error(6, "Unhandled binary one dimensional array concatenation expression", "Will propagate upwards", [(&(le_symb, ri_symb), &(Le_symb, Ri_symb))], 687255, mika_symbolic, symbolically_interpret, symbolically_interpret(&(a, b)), "Subset needs enlarging"),
                 Symb = unhandled_expression,
                 Type = unhandled_expression,
                 mika_unhandled_atts:mika_unhandled_atts__create(Array_var, unhandled_expression, &(Le_symb, Ri_symb))
                )
        ;
                (Symb = &(Le_symb, Ri_symb),
                 midoan_solver__interpret(&(Le_const, Ri_const), types(Le_type, Ri_type), Array_var, Type, _Exception_)      %22/09/09 used to return array(_) as Type
                )
        ).
%indexes for example and aggregates
symbolically_interpret(named(A, B), named(A_symb, B_symb), named(A_const, B_const), named(A_type, B_type), _Exception_) :-
        symbolically_interpret(A, A_symb, A_const, A_type, _Exception_),
        symbolically_interpret(B, B_symb, B_const, B_type, _Exception_).
symbolically_interpret((A, B), (A_symb, B_symb), (A_const, B_const), (A_type, B_type), _Exception_) :-
        symbolically_interpret(A, A_symb, A_const, A_type, _Exception_),
        symbolically_interpret(B, B_symb, B_const, B_type, _Exception_).
symbolically_interpret([], [], [], [], _Exception_).
symbolically_interpret([F|Rest], [F_symb|Rest_symb], [F_const|Rest_const], [F_type|Rest_type], _Exception_) :-
        symbolically_interpret(F, F_symb, F_const, F_type, _Exception_),
	symbolically_interpret(Rest, Rest_symb, Rest_const, Rest_type, _Exception_).

%%%
%for Ada attributes (see Annex K of Ada RM)
symbolically_interpret(access, access, access, access, no_exception).
symbolically_interpret(address, address, address, address, no_exception).
symbolically_interpret(adjacent, adjacent, adjacent, adjacent, no_exception).
symbolically_interpret(aft, aft, aft, aft, no_exception).
symbolically_interpret(alignment, alignment, alignment, alignment, no_exception).
symbolically_interpret(base, base, base, base, no_exception).
symbolically_interpret(bit_order, bit_order, bit_order, bit_order, no_exception).
symbolically_interpret(body_version, body_version, body_version, body_version, no_exception).
symbolically_interpret(callable, callable, callable, callable, no_exception).
symbolically_interpret(caller, caller, caller, caller, no_exception).
symbolically_interpret(ceiling, ceiling, ceiling, ceiling, no_exception).
symbolically_interpret(class, class, class, class, no_exception).
symbolically_interpret(component_size, component_size, component_size, component_size, no_exception).
symbolically_interpret(compose, compose, compose, compose, no_exception).
symbolically_interpret(constrained, constrained, constrained, constrained, no_exception).
symbolically_interpret(copy_sign, copy_sign, copy_sign, copy_sign, no_exception).
symbolically_interpret(count, count, count, count, no_exception).
symbolically_interpret(definite, definite, definite, definite, no_exception).
symbolically_interpret(delta, delta, delta, delta, no_exception).
symbolically_interpret(denorm, denorm, denorm, denorm, no_exception).
symbolically_interpret(digits, digits, digits, digits, no_exception).
symbolically_interpret(dimensions, dimensions, dimensions, dimensions, no_exception). %our own attributes : dimensions of an array
symbolically_interpret(exponent, exponent, exponent, exponent, no_exception).
symbolically_interpret(external_tag, external_tag, external_tag, external_tag, no_exception).
symbolically_interpret(first, first, first, first, no_exception).
%A'First(N) is treated as a function with A'First the name of the function
symbolically_interpret(first_bit, first_bit, first_bit, first_bit, no_exception).
symbolically_interpret(floor, floor, floor, floor, no_exception).
symbolically_interpret(fore, fore, fore, fore, no_exception).
symbolically_interpret(fraction, fraction, fraction, fraction, no_exception).
symbolically_interpret(identity, identity, identity, identity, no_exception).
symbolically_interpret(image, image, image, image, no_exception).
%S'Class'Input how is this parsed?
symbolically_interpret(input, input, input, input, no_exception).
symbolically_interpret(last, last, last, last, no_exception).
%A'Last(N) is treated as a function with A'Last the name of the function
symbolically_interpret(last_bit, last_bit, last_bit, last_bit, no_exception).
symbolically_interpret(leading_part, leading_part, leading_part, leading_part, no_exception).
symbolically_interpret(length, length, length, length, no_exception).
%A'Length(N) is treated as a function with A'Length the name of the function
symbolically_interpret(machine, machine, machine, machine, no_exception).
symbolically_interpret(machine_emax, machine_emax, machine_emax, machine_emax, no_exception).
symbolically_interpret(machine_emin, machine_emin, machine_emin, machine_emin, no_exception).
symbolically_interpret(machine_mantissa, machine_mantissa, machine_mantissa, machine_mantissa, no_exception).
symbolically_interpret(machine_overflows, machine_overflows, machine_overflows, machine_overflows, no_exception).
symbolically_interpret(machine_radix, machine_radix, machine_radix, machine_radix, no_exception).
symbolically_interpret(machine_rounding, machine_rounding, machine_rounding, machine_rounding, no_exception). %new in Ada 05
symbolically_interpret(machine_rounds, machine_rounds, machine_rounds, machine_rounds, no_exception).
symbolically_interpret(max, max, max, max, no_exception).
symbolically_interpret(max_size_in_storage_elements, max_size_in_storage_elements, max_size_in_storage_elements, max_size_in_storage_elements, no_exception).
symbolically_interpret(min, min, min, min, no_exception).
symbolically_interpret(mod, mod, mod, mod, no_exception).     %new in Ada 05
symbolically_interpret(model, model, model, model, no_exception).
symbolically_interpret(model_emin, model_emin, model_emin, model_emin, no_exception).
symbolically_interpret(model_epsilon, model_epsilon, model_epsilon, model_epsilon, no_exception).
symbolically_interpret(model_mantissa, model_mantissa, model_mantissa, model_mantissa, no_exception).
symbolically_interpret(model_small, model_small, model_small, model_small, no_exception).
symbolically_interpret(modulus, modulus, modulus, modulus, no_exception).
symbolically_interpret(null_record, null_record, null_record, null_record, no_exception).
symbolically_interpret(output, output, output, output, no_exception).
symbolically_interpret(others, others, others, others, no_exception).
symbolically_interpret(partition_id, partition_id, partition_id, partition_id, no_exception).
symbolically_interpret(pos, pos, pos, pos, no_exception).
symbolically_interpret(position, position, position, position, no_exception).
symbolically_interpret(pred, pred, pred, pred, no_exception).
symbolically_interpret(priority, priority, priority, priority, no_exception). %new in Ada 05
symbolically_interpret(range, range, range, range, no_exception).
%A'Range(N) is treated as a function with A'Range the name of the function
symbolically_interpret(read, read, read, read, no_exception).
symbolically_interpret(remainder, remainder, remainder, remainder, no_exception).
symbolically_interpret(round, round, round, round, no_exception).
symbolically_interpret(rounding, rounding, rounding, rounding, no_exception).
symbolically_interpret(safe_first, safe_first, safe_first, safe_first, no_exception).
symbolically_interpret(safe_last, safe_last, safe_last, safe_last, no_exception).
symbolically_interpret(scale, scale, scale, scale, no_exception).
symbolically_interpret(scaling, scaling, scaling, scaling, no_exception).
symbolically_interpret(signed_zeros, signed_zeros, signed_zeros, signed_zeros, no_exception).
symbolically_interpret(size, size, size, size, no_exception).
symbolically_interpret(small, small, small, small, no_exception).
symbolically_interpret(storage_pool, storage_pool, storage_pool, storage_pool, no_exception).
symbolically_interpret(storage_unit, storage_unit, storage_unit, storage_unit, no_exception).
symbolically_interpret(storage_size, storage_size, storage_size, storage_size, no_exception).
symbolically_interpret(stream_size, stream_size, stream_size, stream_size, no_exception).     %new in Ada 05
symbolically_interpret(succ, succ, succ, succ, no_exception).
symbolically_interpret(tag, tag, tag, tag, no_exception).
symbolically_interpret(terminated, terminated, terminated, terminated, no_exception).
symbolically_interpret(truncation, truncation, truncation, truncation, no_exception).
symbolically_interpret(unbiased_rounding, unbiased_rounding, unbiased_rounding, unbiased_rounding, no_exception).
symbolically_interpret(unchecked_access, unchecked_access, unchecked_access, unchecked_access, no_exception).
symbolically_interpret(val, val, val, val, no_exception).
symbolically_interpret(valid, valid, valid, valid, no_exception).
symbolically_interpret(value, value, value, value, no_exception).
symbolically_interpret(version, version, version, version, no_exception).
symbolically_interpret(wide_image, wide_image, wide_image, wide_image, no_exception).
symbolically_interpret(wide_value, wide_value, wide_value, wide_value, no_exception).
symbolically_interpret(wide_width, wide_width, wide_width, wide_width, no_exception).
symbolically_interpret(write, write, write, write, no_exception).

%for Gnat defined attributes (see GNAT RM)
symbolically_interpret(abort_signal, abort_signal, abort_signal, abort_signal, no_exception).         %not handled
symbolically_interpret(address_size, address_size, address_size, address_size, no_exception).         %handled Standard is the only allowed prefix
symbolically_interpret(asm_input, asm_input, asm_input, asm_input, no_exception).                     %not handled
symbolically_interpret(asm_output, asm_output, asm_output, asm_output, no_exception).                 %not handled
symbolically_interpret(ast_entry, ast_entry, ast_entry, ast_entry, no_exception).                     %not handled
symbolically_interpret(bit, bit, bit, bit, no_exception).                                             %not handled
symbolically_interpret(bit_position, bit_position, bit_position, bit_position, no_exception).         %not handled
symbolically_interpret(code_address, code_address, code_address, code_address, no_exception).         %not handled
symbolically_interpret(default_bit_order, default_bit_order, default_bit_order, default_bit_order, no_exception).     %handled Standard is the only allowed prefix
symbolically_interpret(elaborated, elaborated, elaborated, elaborated, no_exception).                 %not handled
symbolically_interpret(elab_body, elab_body, elab_body, elab_body, no_exception).                     %not handled
symbolically_interpret(elab_spec, elab_spec, elab_spec, elab_spec, no_exception).                     %not handled
symbolically_interpret(emax, emax, emax, emax, no_exception).                                         %not handled
symbolically_interpret(enabled, enabled, enabled, enabled, no_exception).                             %not handled
symbolically_interpret(enum_rep, enum_rep, enum_rep, enum_rep, no_exception).                         %not handled
symbolically_interpret(enum_val, enum_val, enum_val, enum_val, no_exception).                         %not handled
symbolically_interpret(epsilon, epsilon, epsilon, epsilon, no_exception).                             %not handled
symbolically_interpret(fixed_value, fixed_value, fixed_value, fixed_value, no_exception).             %not handled
symbolically_interpret(has_access_values, has_access_values, has_access_values, has_access_values, no_exception).     %not handled
symbolically_interpret(has_discriminants, has_discriminants, has_discriminants, has_discriminants, no_exception).     %not handled
symbolically_interpret(img, img, img, img, no_exception).                                             %not handled
symbolically_interpret(integer_value, integer_value, integer_value, integer_value, no_exception).     %not handled
symbolically_interpret(invalid_value, invalid_value, invalid_value, invalid_value, no_exception).     %not handled
symbolically_interpret(large, large, large, large, no_exception).                                     %not handled
symbolically_interpret(machine_size, machine_size, machine_size, machine_size, no_exception).         %not handled
symbolically_interpret(mantissa, mantissa, mantissa, mantissa, no_exception).                         %not handled
symbolically_interpret(max_interrupt_priority, max_interrupt_priority, max_interrupt_priority, max_interrupt_priority, no_exception). %handled Standard is the only allowed prefix
symbolically_interpret(max_priority, max_priority, max_priority, max_priority, no_exception).         %handled Standard is the only allowed prefix
symbolically_interpret(maximum_alignment, maximum_alignment, maximum_alignment, maximum_alignment, no_exception).     %handled Standard is the only allowed prefix
symbolically_interpret(mechanism_code, mechanism_code, mechanism_code, mechanism_code, no_exception). %not handled
symbolically_interpret(null_parameter, null_parameter, null_parameter, null_parameter, no_exception). %not handled
symbolically_interpret(object_size, object_size, object_size, object_size, no_exception).             %not handled
symbolically_interpret(old, old, old, old, no_exception).                                             %not handled
symbolically_interpret(passed_by_reference, passed_by_reference, passed_by_reference, passed_by_reference, no_exception).     %not handled
symbolically_interpret(pool_address, pool_address, pool_address, pool_address, no_exception).         %not handled
symbolically_interpret(range_length, range_length, range_length, range_length, no_exception).         %not handled
symbolically_interpret(safe_emax, safe_emax, safe_emax, safe_emax, no_exception).                     %not handled
symbolically_interpret(safe_large, safe_large, safe_large, safe_large, no_exception).                 %not handled
%already a standard ada attribute symbolically_interpret(small, small, small, small, no_exception).                                     %not handled
%already a standard ada attribute symbolically_interpret(storage_unit, storage_unit, storage_unit, storage_unit, no_exception).         %handled Standard is the only allowed prefix
symbolically_interpret(stub_type, stub_type, stub_type, stub_type, no_exception).                     %not handled
symbolically_interpret(target_name, target_name, target_name, target_name, no_exception).             %handled Standard is the only allowed prefix
symbolically_interpret(tick, tick, tick, tick, no_exception).                                         %handled Standard is the only allowed prefix
symbolically_interpret(to_address, to_address, to_address, to_address, no_exception).                 %not handled
symbolically_interpret(type_class, type_class, type_class, type_class, no_exception).                 %not handled
symbolically_interpret(uet_address, uet_address, uet_address, uet_address, no_exception).             %not handled
symbolically_interpret(unconstrained_array, unconstrained_array, unconstrained_array, unconstrained_array, no_exception).     %not handled
symbolically_interpret(universal_literal_string, universal_literal_string, universal_literal_string, universal_literal_string, no_exception). %not handled
symbolically_interpret(unrestricted_access, unrestricted_access, unrestricted_access, unrestricted_access, no_exception).     %not handled
symbolically_interpret(vads_size, vads_size, vads_size, vads_size, no_exception).                     %not handled
symbolically_interpret(value_size, value_size, value_size, value_size, no_exception).                 %not handled
symbolically_interpret(wchar_t_size, wchar_t_size, wchar_t_size, wchar_t_size, no_exception).         %handled Standard is the only allowed prefix
symbolically_interpret(word_size, word_size, word_size, word_size, no_exception).                     %handled Standard is the only allowed prefix
%%%
symbolically_interpret_boolean(Boolean_expression, Symb, Const_exp, Type, Exception) :-
        ((compound(Boolean_expression),
          Boolean_expression =.. [Op|[Le, Ri]],
          (Op == '=' ->
                Message = "equal"
          ;
           Op == '<>' ->
                Message = "different"
          ;
           Op == '<' ->
                Message = "less than"
          ;
           Op == '<=' ->
                Message = "less than or equal"
          ;
           Op == '>' ->
                Message = "greater than"
          ;
           Op == '>=' ->
                Message = "greater than or equal"
          ;
           Op == 'is_in' ->
                Message = "is in"
          ;
           Op == 'is_not_in' ->
                Message = "is not in"
          )
         ) ->
                (symbolically_interpret(Le, Le_symb, Le_const, Le_type, Exception),
                 (common_util:common_util__is_an_exception(Exception) ->
                        (Symb = Le_symb,
                         Const_exp = Le_const,
                         Type = Le_type
                        )
                 ;
                        (symbolically_interpret(Ri, Ri_symb, Ri_const, Ri_type, Exception),
                         (common_util:common_util__is_an_exception(Exception) ->
                                (Symb = Ri_symb,
                                 Const_exp = Ri_const,
                                 Type = Ri_type
                                )
                         ;
                                (
                                 (Le_type == 'unhandled_expression' ; Ri_type == 'unhandled_expression') ->
                                        (Unhandled_symb =.. [Op|[Le_symb, Ri_symb]],    %e.g. >(Le_symb, Ri_symb)
                                         common_util__error(6, "Unhandled binary equal expression", "Will propagate upwards", [(unhandled_symb, Unhandled_symb)], 677336, 'mika_symbolic', 'symbolically_interpret_boolean', symbolically_interpret(=(a, b)), "Subset needs enlarging"),
                                         Symb = 'unhandled_expression',
                                         Type = 'unhandled_expression',
                                         mika_unhandled_atts:mika_unhandled_atts__create(Const_exp, 'unhandled_expression', Unhandled_symb)
                                        )
                                ;
                                        (Symb =.. [Op|[Le_symb, Ri_symb]],              %e.g. >(Le_symb, Ri_symb)
                                         Const_exp =.. [Op|[Le_const, Ri_const]],       %e.g. >(Le_const, Ri_const)
                                         Type = 'b'
                                        )
                                )
                         )
                        )
                 )
                )
        ;
         Boolean_expression = tic(Value, 'valid') ->
                (syntactically_denote(Value, Name, Entity, Entity_type),
                 (Entity_type == 'object' ->
                        (symbolically_interpret(Entity, Value_symb, Value_const, Value_type, Exception),
                         (Value_type == 'unhandled_expression' ->
                                (common_util__error(6, "Unhandled value in valid expression", "Will propagate upwards", [(tic(value_symb, valid), tic(Value_symb, valid))], 685245, mika_symbolic, symbolically_interpret, symbolically_interpret(tic(a, valid)), "Subset needs enlarging"),
                                 Symb = unhandled_expression,
                                 Type = unhandled_expression,
                                 mika_unhandled_atts:mika_unhandled_atts__create(Const_exp, unhandled_expression, valid(Value_symb))
                                )
                         ;
                          mika_seav_atts:mika_seav_atts__is_seav(Entity) ->
                                (mika_seav_atts:mika_seav_atts__get(type, Entity, Value_typevar),
                                 Symb = valid(Value_symb, Value_typevar),
                                 Const_exp = valid(Value_const, Value_typevar),
                                 Type = 'b'
                                )
                         ;
                                common_util__error(10, "In valid expression, the expression is not a value", "Cannot proceed", [(tic(value_symb, valid), tic(Value_symb, valid))], 10115945, mika_symbolic, symbolically_interpret, symbolically_interpret(tic(a, valid)), "Should never happen")
                         )
                        )
                ;
                        common_util__error(10, "In valid expression, the expression is not an object", "Cannot proceed", [((name, entity_type), (Name, Entity_type))], 10116445, mika_symbolic, symbolically_interpret, symbolically_interpret(tic(a, valid)), "Should never happen")
                 )
                )
        ;
                % e.g. a function call, or could actually be part of a bitwise operator that does not return a boolean
                symbolically_interpret(Boolean_expression, Symb, Const_exp, Type, Exception)
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     END    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%