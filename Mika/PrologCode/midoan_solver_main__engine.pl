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
% midoan_solver_main__engine.pl
% part of the midoan_solver module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Integer is converted to a modular integer 'Modular_result' using Modular_var as a template
%but because of static expressions such as Ymod8 + (7 + 2) we may get a larger than expected integer literal so we cannot check its range as in (apply_relation(>=, Integer, i, 0, i), apply_relation(<=, Integer, i, Modulo_minus_one, i))
convert_to_modulo(Modular_var, Integer, Modular_result) :-
        midoan_modular_integer:midoan_modular_integer__get(modulo, Modular_var, Modulo),
        midoan_solver__interpret(mod(Integer, Modulo), types(i, i), Result_value, 'i', _Exception), %need to apply the modulo (e.g. 128 + 128 : for the solver there is no modular context to it gets evaluated to 256 not 0 at it should)
        midoan_modular_integer:midoan_modular_integer__create_modular_integer(Modulo, Modular_result),
        midoan_modular_integer:midoan_modular_integer__get(value, Modular_result, Result_value).

%prepares 2 modular variables for arithmetic operator
extract_integers_from_modular(Le, Ri, Le_value, Ri_value, Modular_var, Modulo, Modular_value) :-
        midoan_modular_integer:midoan_modular_integer__get(value, Le, Le_value),
        midoan_modular_integer:midoan_modular_integer__get(value, Ri, Ri_value),
        midoan_modular_integer:midoan_modular_integer__get(modulo, Le, Modulo),
        midoan_modular_integer:midoan_modular_integer__create_modular_integer(Modulo, Modular_var),
        midoan_modular_integer:midoan_modular_integer__get(value, Modular_var, Modular_value).

%applies to all scalar types
%[29/10/09] : this seems to be broken for floats (especially is_not_in)
relation_constraint('is_in', X, Min, Max) :-	%interval constraint (may legitimately fail)
	get_var_type(X, X_type),
	get_var_type(Min, Min_type),
	get_var_type(Max, Max_type),
	!,
	apply_relation(>=, X, X_type, Min, Min_type),
        apply_relation(<=, X, X_type, Max, Max_type).

relation_constraint(is_not_in, X, Min, Max) :- %interval constraint
	get_var_type(X, X_type),
	get_var_type(Min, Min_type),
	get_var_type(Max, Max_type),
	!,
        (X_type == e ->
	        (%care here
	         midoan_enum:midoan_enum__get(position, X, PosX),
		 (midoan_enum:midoan_enum__pred(Min, Pred_min) ->	%this will fail for the first enum
		        (midoan_enum:midoan_enum__get(position, Pred_min, Pos_pred_min),
	                 (midoan_enum:midoan_enum__succ(Max, Succ_max) ->	%this will fail for the first enum
 				(midoan_enum:midoan_enum__get(position, Succ_max, Pos_succ_max),
			         !,
			         PosX #=< Pos_pred_min #\/ PosX #>= Pos_succ_max
				)
		         ;
				apply_relation(<, X, X_type, Min, Min_type)
			 )
			)
		 ;
			apply_relation(>, X, X_type, Max, Max_type)
		 )
		)
        ;
         X_type == r ->
                common_util__error(10, "'is not in' expression over real numbers is not yet implmented", no_error_consequences, [(x_type, X_type), (min_type, Min_type), (max_type, Max_type)], 1066197, midoan_solver, relation_constraint, no_localisation, no_extra_info)
        ;
         (X_type == i ; X_type == modular_integer) ->
		((X_type == i ->
                        X_value = X
                 ;
                        midoan_modular_integer:midoan_modular_integer__get(value, X, X_value)
                 ),
                 (Min_type == i ->
                        Min_value = Min
                 ;
                        midoan_modular_integer:midoan_modular_integer__get(value, Min, Min_value)
                 ),
                 (Max_type == i ->
                        Max_value = Max
                 ;
                        midoan_modular_integer:midoan_modular_integer__get(value, Max, Max_value)
                 ),
                 !,
                 X_value #< Min_value #\/ X_value #> Max_value		%see 19/10/06
	        )
	;
		common_util__error(10, "Typing error in 'is not in' expression", no_error_consequences, [(x_type, X_type), (min_type, Min_type), (max_type, Max_type)], 105350, midoan_solver, relation_constraint, no_localisation, no_extra_info)
	).
relation_constraint('valid', X, Type_var) :-
        !,
        midoan_solver__interpret(conversion_may_fail(Type_var, X), types(_, _), _Const_exp, _Type, _Exception_).
relation_constraint('is_not_valid', X, Type_var) :-
        !,
        \+ midoan_solver__interpret(conversion_may_fail(Type_var, X), types(_, _), _Const_exp, _Type, _Exception_). %must fail to be true
relation_constraint(Rel, Le, Ri):-
	get_var_type(Le, Le_type),
	get_var_type(Ri, Ri_type),
	!,
	apply_relation(Rel, Le, Le_type, Ri, Ri_type).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
apply_relation(=, L, TL, R, TR) :-
        ((      (TL == array, TR == array) ;
                (TL == array, TR == anonymous_aggregate) ;
                (TL == anonymous_aggregate, TR == array) ;
                (TL == array, TR == string_literal) ;
                (TL == string_literal, TR == array) ;
                (TL == record, TR == record)
         )
         ->
                midoan_solver__controlled_unification(L, R)
        ;
         (TL == r, TR == r) ->
                L = R
        ;
         (TL == e, TR == e) ->
                (midoan_enum:midoan_enum__get(position, L, L_pos),
                 midoan_enum:midoan_enum__get(position, R, R_pos),
                 midoan_solver__controlled_unification(L_pos, R_pos)
                )
        ;
         (TL == i, TR == i) ->
                midoan_solver__controlled_unification(L, R)
        ;
         (TL == modular_integer, TR == modular_integer) ->
                (midoan_modular_integer:midoan_modular_integer__get(value, R, R_value),
                 midoan_modular_integer:midoan_modular_integer__get(value, L, L_value),
                 apply_relation(=, L_value, i, R_value, i)
                )
        ;
         (TL == i, TR == modular_integer) ->
                (midoan_modular_integer:midoan_modular_integer__get(value, R, R_value),
                 apply_relation(=, L, TL, R_value, i)
                )
        ;
         (TL == modular_integer, TR == i) ->
                (midoan_modular_integer:midoan_modular_integer__get(value, L, L_value),
                 apply_relation(=, L_value, i, R, TR)
                )
        ;
                common_util__error(10, "Typing error in '=' expression", no_error_consequences, [(l, L), (tl, TL), (r, R), (tr, TR)], 108552, midoan_solver, apply_relation, no_localisation, no_extra_info)
        ).

apply_relation(<>, L, TL, R, TR) :-
        ((TL == array, TR == array) ->
                midoan_array:midoan_array__inequality(L, R)
        ;
         (TL == record, TR == record) ->
                midoan_record:midoan_record__inequality(L, R)
        ;
         (TL == anonymous_aggregate, TR == array) ->
                (midoan_array:midoan_array__get_type_var(R, Type_var),
                 midoan_type:midoan_type__variable_declaration(Template, Type_var),
                 midoan_solver__controlled_unification(Template, L),
                 midoan_array:midoan_array__inequality(L, R)
                )
        ;
         (TL == array, TR == anonymous_aggregate) ->
                (midoan_array:midoan_array__get_type_var(L, Type_var),
                 midoan_type:midoan_type__variable_declaration(Template, Type_var),
                 midoan_solver__controlled_unification(Template, R),
                 midoan_array:midoan_array__inequality(L, R)
                )
        ;
         (TL == string_literal, TR == array) ->
                (midoan_array:midoan_array__get_type_var(R, Type_var),
                 midoan_string_literal:midoan_string_literal__get_ascii_codes(L, CodeL),
                 midoan_array:midoan_array__create_array_from_string_literal(Type_var, CodeL, Tmp),
                 midoan_array:midoan_array__inequality(Tmp, R)
                )
        ;
         (TL == array, TR == string_literal) ->
                (midoan_array:midoan_array__get_type_var(L, Type_var),
                 midoan_string_literal:midoan_string_literal__get_ascii_codes(R, CodeR),
                 midoan_array:midoan_array__create_array_from_string_literal(Type_var, CodeR, Tmp),
                 midoan_array:midoan_array__inequality(L, Tmp)
                )
        ;
         (TL == e, TR == e) ->
                (midoan_enum:midoan_enum__get(position, L, PosL),
		 midoan_enum:midoan_enum__get(position, R, PosR),
                 apply_relation(<>, PosL, i, PosR, i)
                )
        ;
         (TL == i, TR == i) ->
                midoan_extensions:midoan_extensions__infinity_different(L, R)
        ;
         (TL == r, TR == r) ->
                {L =\= R}
        ;
         (TL == modular_integer, TR == modular_integer) ->
                (midoan_modular_integer:midoan_modular_integer__get(value, R, R_value),
                 midoan_modular_integer:midoan_modular_integer__get(value, L, L_value),
                 apply_relation(<>, L_value, i, R_value, i)
                )
        ;
         (TL == i, TR == modular_integer) ->
                (midoan_modular_integer:midoan_modular_integer__get(value, R, R_value),
                 apply_relation(<>, L, TL, R_value, i)
                )
        ;
         (TL == modular_integer, TR == i) ->
                (midoan_modular_integer:midoan_modular_integer__get(value, L, L_value),
                 apply_relation(<>, L_value, i, R, TR)
                )
        ;
                common_util__error(10, "Typing error in '<>' expression", no_error_consequences, [(l, L), (tl, TL), (r, R), (tr, TR)], 1013553, midoan_solver, apply_relation, no_localisation, no_extra_info)
        ).

apply_relation(>, L, TL, R, TR) :-
        ((TL == e, TR == e) ->
                (midoan_enum:midoan_enum__succ(R, SuccR),
		 midoan_enum:midoan_enum__get(position, SuccR, Pos_succR),
		 midoan_enum:midoan_enum__get(position, L, PosL),
		 apply_relation(>=, PosL, i, Pos_succR, i)
		)
        ;
         (TL == i, TR == i) ->
                midoan_extensions__infinity_greater(L, R)
        ;
         (TL == r, TR == r) ->
                {L > R}
        ;
         (TL == modular_integer, TR == modular_integer) ->
                (midoan_modular_integer:midoan_modular_integer__get(value, R, R_value),
                 midoan_modular_integer:midoan_modular_integer__get(value, L, L_value),
                 apply_relation(>, L_value, i, R_value, i)
                )
        ;
         (TL == i, TR == modular_integer) ->
                (midoan_modular_integer:midoan_modular_integer__get(value, R, R_value),
                 apply_relation(>, L, TL, R_value, i)
                )
        ;
         (TL == modular_integer, TR == i) ->
                (midoan_modular_integer:midoan_modular_integer__get(value, L, L_value),
                 apply_relation(>, L_value, i, R, TR)
                )
        ;
                common_util__error(10, "Typing error in '>' expression", no_error_consequences, [(l, L), (tl, TL), (r, R), (tr, TR)], 1015253, midoan_solver, apply_relation, no_localisation, no_extra_info)
        ).

apply_relation(<, L, TL, R, TR) :-
        ((TL == e, TR == e) ->
                (midoan_enum:midoan_enum__pred(R, PredR),
		 midoan_enum:midoan_enum__get(position, PredR, Pos_predR),
		 midoan_enum:midoan_enum__get(position, L, PosL),
		 apply_relation(<=, PosL, i, Pos_predR, i)
		)
        ;
         (TL == i, TR == i) ->
                midoan_extensions__infinity_less(L, R)
        ;
         (TL == r, TR == r) ->
                {L < R}
        ;
         (TL == modular_integer, TR == modular_integer) ->
                (midoan_modular_integer:midoan_modular_integer__get(value, R, R_value),
                 midoan_modular_integer:midoan_modular_integer__get(value, L, L_value),
                 apply_relation(<, L_value, i, R_value, i)
                )
        ;
         (TL == i, TR == modular_integer) ->
                (midoan_modular_integer:midoan_modular_integer__get(value, R, R_value),
                 apply_relation(<, L, TL, R_value, i)
                )
        ;
         (TL == modular_integer, TR == i) ->
                (midoan_modular_integer:midoan_modular_integer__get(value, L, L_value),
                 apply_relation(<, L_value, i, R, TR)
                )
        ;
                common_util__error(10, "Typing error in '<' expression", no_error_consequences, [(l, L), (tl, TL), (r, R), (tr, TR)], 1016954, midoan_solver, apply_relation, no_localisation, no_extra_info)
        ).
apply_relation(<=, L, TL, R, TR) :-
        ((TL == e, TR == e) ->
                (midoan_enum:midoan_enum__get(position, L, PosL),
		 midoan_enum:midoan_enum__get(position, R, PosR),
		 apply_relation(<=, PosL, i, PosR, i)
		)
        ;
         (TL == i, TR == i) ->
                midoan_extensions__infinity_less_or_equal(L, R)
        ;
         (TL == r, TR == r) ->
                {L =< R}
        ;
         (TL == modular_integer, TR == modular_integer) ->
                (midoan_modular_integer:midoan_modular_integer__get(value, R, R_value),
                 midoan_modular_integer:midoan_modular_integer__get(value, L, L_value),
                 apply_relation(<=, L_value, i, R_value, i)
                )
        ;
         (TL == i, TR == modular_integer) ->
                (midoan_modular_integer:midoan_modular_integer__get(value, R, R_value),
                 apply_relation(<=, L, TL, R_value, i)
                )
        ;
         (TL == modular_integer, TR == i) ->
                (midoan_modular_integer:midoan_modular_integer__get(value, L, L_value),
                 apply_relation(<=, L_value, i, R, TR)
                )
        ;
                common_util__error(10, "Typing error in '<=' expression", no_error_consequences, [(l, L), (tl, TL), (r, R), (tr, TR)], 1018454, midoan_solver, apply_relation, no_localisation, no_extra_info)
        ).
apply_relation(>=, L, TL, R, TR) :-
        ((TL == e, TR == e) ->
                (midoan_enum:midoan_enum__get(position, L, PosL),
		 midoan_enum:midoan_enum__get(position, R, PosR),
		 apply_relation(>=, PosL, i, PosR, i)
		)
        ;
         (TL == i, TR == i) ->
                midoan_extensions__infinity_greater_or_equal(L, R)
        ;
         (TL == r, TR == r) ->
                {L >= R}
        ;
         (TL == modular_integer, TR == modular_integer) ->
                (midoan_modular_integer:midoan_modular_integer__get(value, R, R_value),
                 midoan_modular_integer:midoan_modular_integer__get(value, L, L_value),
                 apply_relation(>=, L_value, i, R_value, i)
                )
        ;
         (TL == i, TR == modular_integer) ->
                (midoan_modular_integer:midoan_modular_integer__get(value, R, R_value),
                 apply_relation(>=, L, TL, R_value, i)
                )
        ;
         (TL == modular_integer, TR == i) ->
                (midoan_modular_integer:midoan_modular_integer__get(value, L, L_value),
                 apply_relation(>=, L_value, i, R, TR)
                )
        ;
                common_util__error(10, "Typing error in '>=' expression", no_error_consequences, [(l, L), (tl, TL), (r, R), (tr, TR)], 1019955, midoan_solver, apply_relation, no_localisation, no_extra_info)
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
real_conversion(X, Type, R) :-
	(Type = r ->
		R = X		%no change (already a real type variable)
	;
	 Type = i ->
		(var(X) ->            %the integer is not instantiated
			(update_ghost_real(X, R),
			 %below linking Xeval and R so that updates from one variable are reflected to the other variable
			 update_ghost_integer(X, R)
			)
		;                               %Xeval is ground
			R is X*1.0              %return a float
		)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- multifile clpfd:dispatch_global/4.
clpfd:dispatch_global(update_ghost_real_blocked(I, R), state(I, R), state(_, _), Actions) :-
       Actions = [call(midoan_solver:update_ghost_real_blocked(I, R))].

% update the associated real variable R according to I
update_ghost_real(I, R) :-
        (update_ghost_real_ground(I, R) ->
                true    %we have finished
        ;
                fd_global(update_ghost_real_blocked(I, R), state(I, R), [dom(I)])
        ).

update_ghost_real_ground(I, R) :-
        (ground(I) ->
                {R = I}
        ;
         ground(R) ->
                true    %not our task here to update I : will be done by update_ghost_integer
        ).

update_ghost_real_blocked(I, R) :-
	(update_ghost_real_ground(I, R) ->
	        true
        ;
                (fd_min(I, Min),
                 fd_max(I, Max),
                 {R >= Min, R =< Max}
                )
	).

% update the associated integer variable I according to R
update_ghost_integer(I, R) :-
        update_ghost_integer_blocked(I, R).

update_ghost_integer_blocked(I, R) :-
	sup(R, SupR),
%calculating MaxI taking into account the edge effects around Sup
	((
	     (SupR >= 0, bound_integer_not_taken(SupR, R))
	 ;
	     (SupR < 0 , bound_real_taken(SupR, R))
	 ) ->
	     MaxI is integer(SupR) - 1
	 ;
	     MaxI is integer(SupR)
	),
	inf(R, InfR),
%calculating MinI taking into account the edge effects around Inf
	((
	     (InfR =< 0, bound_integer_not_taken(InfR, R))
	 ;
	     (InfR > 0 , bound_real_taken(InfR, R))
	 ) ->
	     MinI is integer(InfR) + 1
	 ;
	     MinI is integer(InfR)
	),
        %assigning them to I,
        midoan_extensions:max_clpfd(Max_clpfd),
        midoan_extensions:min_clpfd(Min_clpfd),
        (MaxI > Max_clpfd ->
                MaxI_checked = Max_clpfd
        ;
                MaxI_checked = MaxI
        ),
        (MinI < Min_clpfd ->
                MinI_checked = Min_clpfd
        ;
                MinI_checked = MinI
        ),
        apply_relation(<=, I, i, MaxI_checked, i),
        apply_relation(>=, I, i, MinI_checked, i),
        (ground(I) ->                    %no further improvements
	        true
	;                                %delay on R until instantiation of R (best we can do with reals)
                when((ground(R);ground(I)), update_ghost_integer_blocked(I, R))
        ).

%called from update_ghost_integer/2 by bound_integer_not_taken(InfR, R) [or with (SupR, R)]
%Bound is the sup or inf of R
%R is a real variable
%indicate that Bound is an integer and that R cannot be equal to it
bound_integer_not_taken(Bound, R) :-
	0.0 is float_fractional_part(abs(Bound)), %i.e. Bound is an integer
	entailed(R =\= Bound).       %trick should be equivalent to not entailed(R = Bound)

%called from update_ghost_integer/2 by bound_real_taken(InfR, R) [or with (SupR, R)]
%Bound is the sup or inf of R
%R is a real variable
%indicate that Bound is not an integer and that R can be equal to it
bound_real_taken(Bound, R) :-
	\+ (0.0 is float_fractional_part(abs(Bound)), %i.e. Bound is not an integer (i.e. a real)
	    \+ entailed(R =\= Bound)
	   ). %trick should be equivalent to entailed(R = Bound)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%get the type of a solver variable or a number
get_var_type(X, Type) :-
	((midoan_solver__is_integer(X); integer(X)) ->
		Type = 'i'
	;
         midoan_enum:midoan_enum__is_enum(X) ->
		Type = 'e'
	;
	 midoan_array:midoan_array__is_array(X) ->
		Type = 'array'
	;
	 midoan_record:midoan_record__is_record(X) ->
		Type = 'record'
	;
	 (midoan_solver__is_float(X); float(X)) ->
		Type = 'r'
        ;
         midoan_anon_aggregate:midoan_anon_aggregate__is_anon_aggregate(X) ->
                Type = 'anonymous_aggregate'
        ;
         midoan_string_literal:midoan_string_literal__is_string_literal(X) ->
                Type = 'string_literal'
        ;
         midoan_modular_integer:midoan_modular_integer__is_modular_integer(X) ->
                Type = 'modular_integer'
        ;
                common_util__error(10, "Unknown variable type", no_error_consequences, [(x, X)], 1033857, midoan_solver, get_var_type, no_localisation, no_extra_info)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check for variables
midoan_solver__interpret(Xin, types(_), Const_exp, Type, 'no_exception'):-
        var(Xin),
	!,
	get_var_type(Xin, Type),
        (Type == 'i' ->
                (midoan_extensions__constrain_domain(Const_exp),
                 Const_exp #= Xin
                )
        ;
         Type == 'r' ->
                {Const_exp = Xin}
        ;
         Type == 'array' ->
                Const_exp = Xin
        ;
         Type == 'record' ->
                Const_exp = Xin
        ;
         Type == 'modular_integer' ->
                Const_exp = Xin
        ;
                common_util__error(10, "Unknown variable type in interpreter", no_error_consequences, [(xin, Xin), (type, Type)], 1036158, midoan_solver, midoan_solver__interpret, no_localisation, no_extra_info)
        ).

midoan_solver__interpret(N, types(_), Neval, Type, 'no_exception') :-
	number(N),
	!,
        (integer(N) ->
                (Type = 'i',
                 Neval is N
                )
	;
	        (Type = 'r',
	         {Neval = N}
	        )
	).

%23/09/09 see diary : unification of Enum and Enum_const is only made if we are dealing with enums : strange error otherwise
midoan_solver__interpret(Enum, types(_), Enum_const, 'e', 'no_exception') :-
        midoan_enum:midoan_enum__is_enum(Enum),
        !,
        Enum = Enum_const.

midoan_solver__interpret(Modular, types(_), Modular_const, 'modular_integer', 'no_exception') :-
        midoan_modular_integer:midoan_modular_integer__is_modular_integer(Modular),
        !,
        Modular = Modular_const.

%Type must be modular_integer
midoan_solver__interpret(bitwise('not', Le_const), types(Le_type), Const_exp, 'modular_integer', _Exception_) :-
        !,
        midoan_modular_integer:midoan_modular_integer__unary_bitwise('not', Le_const, types(Le_type), Const_exp).

midoan_solver__interpret(bitwise(Operator, Le_const, Ri_const), types(Le_type, Ri_type), Const_exp, 'modular_integer', _Exception_) :-
        !,
        midoan_modular_integer:midoan_modular_integer__binary_bitwise(Operator, Le_const, Ri_const, types(Le_type, Ri_type), Const_exp).

midoan_solver__interpret(-(Le_const), types(Le_type), Const_exp, Type, _Exception_) :-
	!,
        (Le_type == i ->
                (Type = i,
                 midoan_extensions__infinity_unary_minus(Le_const, Const_exp)
                )
        ;
         Le_type == modular_integer ->
                (midoan_modular_integer:midoan_modular_integer__get(value, Le_const, Le_value),
                 midoan_modular_integer:midoan_modular_integer__get(modulo, Le_const, Modulo),
                 midoan_modular_integer:midoan_modular_integer__create_modular_integer(Modulo, Const_exp),
                 midoan_modular_integer:midoan_modular_integer__get(value, Const_exp, Result_value),
                 midoan_solver__interpret(-(Le_value), types(i), Raw_result, _, _),
                 midoan_solver__interpret(mod(Raw_result, Modulo), types(i, i), Result_value, 'i', _Exception_mod),
                 Type = modular_integer
                )
        ;
	        ({Const_exp = -Le_const},
                 Type = r
                )
        ).

midoan_solver__interpret(+(Le_const, Ri_const), types(Le_type, Ri_type), Const_exp, Type, _Exception_) :-
        !,
        ((Le_type == i, Ri_type == i) ->
                 midoan_extensions__infinity_binary_plus(Le_const, Ri_const, Const_exp, Type)
	;
         (Le_type == modular_integer, Ri_type == modular_integer) ->
                (extract_integers_from_modular(Le_const, Ri_const, Le_value, Ri_value, Const_exp, Modulo, Result_value),
                 midoan_solver__interpret(+(Le_value, Ri_value), types(i, i), Raw_result, i, _),
                 midoan_solver__interpret(mod(Raw_result, Modulo), types(i, i), Result_value, i, _Exception_mod),
                 Type = modular_integer
                )
        ;
         (Le_type == modular_integer, Ri_type == i) ->
                (convert_to_modulo(Le_const, Ri_const, Ri_modular),
                 midoan_solver__interpret(+(Le_const, Ri_modular), types(modular_integer, modular_integer), Const_exp, Type, _Exception_)
                )
        ;
         (Le_type == i, Ri_type == modular_integer) ->
                (convert_to_modulo(Ri_const, Le_const, Le_modular),
                 midoan_solver__interpret(+(Le_modular, Ri_const), types(modular_integer, modular_integer), Const_exp, Type, _Exception_)
                )
        ;
	        (real_conversion(Le_const, Le_type, New_le),
	         real_conversion(Ri_const, Ri_type, New_ri),
	         {Const_exp = New_le + New_ri},
	         Type = r
	        )
	).

midoan_solver__interpret(-(Le_const, Ri_const), types(Le_type, Ri_type), Const_exp, Type, _Exception_) :-
	!,
	((Le_type == i, Ri_type == i) ->
                 midoan_extensions__infinity_binary_minus(Le_const, Ri_const, Const_exp, Type)
	;
         (Le_type == modular_integer, Ri_type == modular_integer) ->
                (extract_integers_from_modular(Le_const, Ri_const, Le_value, Ri_value, Const_exp, Modulo, Result_value),
                 midoan_solver__interpret(-(Le_value, Ri_value), types(i, i), Raw_result, i, _),
                 midoan_solver__interpret(mod(Raw_result, Modulo), types(i, i), Result_value, i, _Exception_),
                 Type = modular_integer
                )
        ;
         (Le_type == modular_integer, Ri_type == i) ->
                (convert_to_modulo(Le_const, Ri_const, Ri_modular),
                 midoan_solver__interpret(-(Le_const, Ri_modular), types(modular_integer, modular_integer), Const_exp, Type, _Exception_)
                )
        ;
         (Le_type == i, Ri_type == modular_integer) ->
                (convert_to_modulo(Ri_const, Le_const, Le_modular),
                 midoan_solver__interpret(-(Le_modular, Ri_const), types(modular_integer, modular_integer), Const_exp, Type, _Exception_)
                )
        ;
                (real_conversion(Le_const, Le_type, New_le),
	         real_conversion(Ri_const, Ri_type, New_ri),
	         {Const_exp = New_le - New_ri},
	         Type = r
	        )
	).

midoan_solver__interpret(/(Le_const, Ri_const), types(Le_type, Ri_type), Const_exp, Type, _Exception_) :-
	!,
	((Le_type == 'i', Ri_type == 'i') ->
                (midoan_extensions__div(Le_const, Ri_const, Const_exp),     %user defined operator
                 Type = 'i'
                )
	;
         (Le_type == modular_integer, Ri_type == modular_integer) ->
                (extract_integers_from_modular(Le_const, Ri_const, Le_value, Ri_value, Const_exp, Modulo, Result_value),
                 midoan_solver__interpret(/(Le_value, Ri_value), types(i, i), Raw_result, i, _Exception_div),
                 midoan_solver__interpret(mod(Raw_result, Modulo), types(i, i), Result_value, i, _Exception_mod),
                 Type = modular_integer
                )
        ;
         (Le_type == modular_integer, Ri_type == i) ->
                (convert_to_modulo(Le_const, Ri_const, Ri_modular),
                 midoan_solver__interpret(/(Le_const, Ri_modular), types(modular_integer, modular_integer), Const_exp, Type, _Exception_div)
                )
        ;
         (Le_type == i, Ri_type == modular_integer) ->
                (convert_to_modulo(Ri_const, Le_const, Le_modular),
                 midoan_solver__interpret(/(Le_modular, Ri_const), types(modular_integer, modular_integer), Const_exp, Type, _Exception_div)
                )
        ;
	        (real_conversion(Le_const, Le_type, New_le),
	         real_conversion(Ri_const, Ri_type, New_ri),
	         {New_le / New_ri = Const_exp},
	         Type = r
	        )
	).

midoan_solver__interpret(*(Le_const, Ri_const), types(Le_type, Ri_type), Const_exp, Type, _Exception_) :-
	!,
	((Le_type == i, Ri_type == i) ->
                midoan_extensions__infinity_multiply(Le_const, Ri_const, Const_exp, Type)
	;
         (Le_type == modular_integer, Ri_type == modular_integer) ->
                (extract_integers_from_modular(Le_const, Ri_const, Le_value, Ri_value, Const_exp, Modulo, Result_value),
                 midoan_solver__interpret(*(Le_value, Ri_value), types(i, i), Raw_result, i, _Exception_mult),
                 midoan_solver__interpret(mod(Raw_result, Modulo), types(i, i), Result_value, i, _Exception_mod),
                 Type = modular_integer
                )
        ;
         (Le_type == modular_integer, Ri_type == i) ->
                (convert_to_modulo(Le_const, Ri_const, Ri_modular),
                 midoan_solver__interpret(*(Le_const, Ri_modular), types(modular_integer, modular_integer), Const_exp, Type, _Exception_mult)
                )
        ;
         (Le_type == i, Ri_type == modular_integer) ->
                (convert_to_modulo(Ri_const, Le_const, Le_modular),
                 midoan_solver__interpret(*(Le_modular, Ri_const), types(modular_integer, modular_integer), Const_exp, Type, _Exception_mult)
                )
        ;
	        (real_conversion(Le_const, Le_type, New_le),
	         real_conversion(Ri_const, Ri_type, New_ri),
	         {Const_exp = New_le * New_ri},
	         Type = r
	        )
	).

midoan_solver__interpret(**(Le_const, Ri_const), types(Le_type, Ri_type), Const_exp, Type, _Exception_) :- %to type checking midoan_solver__sdl(>=(Ri_const, 0)) too
        !,
        ((Le_type == i , Ri_type == i) ->
                (midoan_extensions__pow(Le_const, Ri_const, Const_exp),  %user defined operator
	         Type = i
	        )
        ;
         (Le_type == modular_integer, Ri_type == modular_integer) ->
                (extract_integers_from_modular(Le_const, Ri_const, Le_value, Ri_value, Const_exp, Modulo, Result_value),
                 midoan_solver__interpret(**(Le_value, Ri_value), types(i, i), Raw_result, i, _Exception_power),
                 midoan_solver__interpret(mod(Raw_result, Modulo), types(i, i), Result_value, i, _Exception_mod),
                 Type = modular_integer
                )
        ;
         (Le_type == modular_integer, Ri_type == i) ->
                (convert_to_modulo(Le_const, Ri_const, Ri_modular),
                 midoan_solver__interpret(**(Le_const, Ri_modular), types(modular_integer, modular_integer), Const_exp, Type, _Exception_power)
                )
        ;
         (Le_type == i, Ri_type == modular_integer) ->
                (convert_to_modulo(Ri_const, Le_const, Le_modular),
                 midoan_solver__interpret(**(Le_modular, Ri_const), types(modular_integer, modular_integer), Const_exp, Type, _Exception_power)
                )
        ;
         (Le_type == r, Ri_type == modular_integer) ->
                (midoan_modular_integer:midoan_modular_integer__get(value, Ri_const, Ri_value),
                 midoan_solver__interpret(**(Le_const, Ri_value), types(Le_type, i), Const_exp, Type, _Exception_power)
                )
        ;
         (Le_type == r, Ri_type == i) ->
	        (real_conversion(Ri_const, Ri_type, New_ri),
	         {Const_exp = pow(Le_const, New_ri)},
	         Type = r
	        )
	).

midoan_solver__interpret(component_size(Subtype_var), types(_), R, i, _) :-
	!,
        midoan_extensions__constrain_domain(R),
	midoan_type:midoan_type__get_attribute(Subtype_var, component_size, R).

midoan_solver__interpret(delta(Subtype_var), types(_), R, r, _) :-
	!,
	midoan_type:midoan_type__get_attribute(Subtype_var, delta, R).

midoan_solver__interpret(digits(Subtype_var), types(_), R, i, _) :-
	!,
	midoan_type:midoan_type__get_attribute(Subtype_var, digits, R).

midoan_solver__interpret(first(Scalar_type_var), types(_), R, T, 'no_exception') :-
	!,
        (Scalar_type_var == 'standard.ads:integer' ->
                midoan_extensions:min_clpfd_expression(Exp)
        ;
         Scalar_type_var == 'standard.ads:float' ->
                Exp = -1.790000e308
        ;
	        midoan_type:midoan_type__get_attribute(Scalar_type_var, 'first', Exp)
        ),
	midoan_solver__interpret(Exp, types(_), R, T, _).
midoan_solver__interpret(modulus(Modular_type_var), types(_), R, i, _Exception_) :-
        !,
        midoan_type:midoan_type__get_attribute(Modular_type_var, modulus, R).
midoan_solver__interpret(last(Scalar_type_var), types(_), R, T, 'no_exception') :-
	!,
        (Scalar_type_var == 'standard.ads:integer' ->
                midoan_extensions:max_clpfd_expression(Exp)
        ;
         Scalar_type_var == 'standard.ads:float' ->
                Exp = 1.790000e308
        ;
	        midoan_type:midoan_type__get_attribute(Scalar_type_var, 'last', Exp)
	),
        midoan_solver__interpret(Exp, types(_), R, T, _Exception_).
midoan_solver__interpret(first(Array_type_var, Dim), types(type), R, T, 'no_exception') :-
	!,
	midoan_type:midoan_type__get_attribute(Array_type_var, first(Dim), Exp),
	midoan_solver__interpret(Exp, types(_), R, T, 'no_exception').

midoan_solver__interpret(last(Array_type_var, Dim), types(type), R, T, 'no_exception') :-
	!,
	midoan_type:midoan_type__get_attribute(Array_type_var, last(Dim), Exp),
	midoan_solver__interpret(Exp, types(_), R, T, 'no_exception').

midoan_solver__interpret(length(Array_type_var, Dim), types(type), R, i, _Exception_) :-
	!,
	midoan_type:midoan_type__get_attribute(Array_type_var, length(Dim), R).
midoan_solver__interpret(dimensions(Array_type_var), types(type), R, i, _Exception_) :-
	!,
	midoan_type:midoan_type__get_attribute(Array_type_var, 'dimensions', R).
midoan_solver__interpret(max_size_in_storage_elements(Subtype_var), types(_), R, i, _Exception_) :-
	!,
	midoan_type:midoan_type__get_attribute(Subtype_var, max_size_in_storage_elements, R).

midoan_solver__interpret(size(Subtype_var), types(_), R, i, _Exception_) :-
	!,
	midoan_type:midoan_type__get_attribute(Subtype_var, size, R).

midoan_solver__interpret(small(Subtype_var), types(_), R, r, _Exception_) :-
	!,
	midoan_type:midoan_type__get_attribute(Subtype_var, small, R).

midoan_solver__interpret(abs(Ri), types(Ri_type), Const_exp, Type, _Exception_) :-
	!,
	(Ri_type == i ->
                (midoan_extensions__abs(Ri, Const_exp),
                 Type = i
                )
        ;
         Ri_type == modular_integer ->
                (midoan_modular_integer:midoan_modular_integer__get(value, Ri, Ri_value),
                 Result_value = Ri_value,
                 midoan_modular_integer:midoan_modular_integer__get(modulo, Ri, Modulo),
                 midoan_modular_integer:midoan_modular_integer__create_modular_integer(Modulo, Const_exp),
                 midoan_modular_integer:midoan_modular_integer__get(value, Const_exp, Result_value),
                 Type = modular_integer
                )
        ;
         Ri_type == r ->
	        ({Const_exp = abs(Ri)},
                 Type = r
                )
        ).

%mod can only apply to integer operands and the resulting type is always integer
midoan_solver__interpret(mod(Le, Ri), types(Le_type, Ri_type), Const_exp, Type, _Exception_) :-
	!,
        ((Le_type == i, Ri_type == i) ->
                (midoan_extensions__mod(Le, Ri, Const_exp),
                 Type = i
                )
        ;
         (Le_type == modular_integer, Ri_type == modular_integer) ->
                (extract_integers_from_modular(Le, Ri, Le_value, Ri_value, Const_exp, Modulo, Result_value),
                 midoan_solver__interpret(mod(Le_value, Ri_value), types(i, i), Raw_result, i, _Exception_),
                 midoan_solver__interpret(mod(Raw_result, Modulo), types(i, i), Result_value, i, _Exception_),
                 Type = modular_integer
                )
        ;
         (Le_type == modular_integer, Ri_type == i) ->
                (convert_to_modulo(Le, Ri, Ri_modular),
                 midoan_solver__interpret(mod(Le, Ri_modular), types(modular_integer, modular_integer), Const_exp, Type, _Exception_)
                )
        ;
         (Le_type == i, Ri_type == modular_integer) ->
                (convert_to_modulo(Ri, Le, Le_modular),
                 midoan_solver__interpret(mod(Le_modular, Ri), types(modular_integer, modular_integer), Const_exp, Type, _Exception_)
                )
        ;
                common_util__error(10, "Failed mod operator: invalid types", no_error_consequences, [(le, Le), (ri, Ri)], 10587128, midoan_solver, midoan_solver__interpret, no_localisation, no_extra_info)
        ).

%rem can only apply to integer operands and the resulting type is always integer
midoan_solver__interpret(rem(Le, Ri), types(Le_type, Ri_type), Const_exp, Type, _Exception_) :-
	!,
        ((Le_type == i, Ri_type == i) ->
                (midoan_extensions__infinity_rem(Le, Ri, Const_exp, Type)     %will constrain Ri to <> 0
                )
        ;
         (Le_type == modular_integer, Ri_type == modular_integer) ->
                (extract_integers_from_modular(Le, Ri, Le_value, Ri_value, Const_exp, Modulo, Result_value),
                 midoan_solver__interpret(rem(Le_value, Ri_value), types(i, i), Raw_result, i, _Exception_),
                 midoan_solver__interpret(mod(Raw_result, Modulo), types(i, i), Result_value, i, _Exception_),
                 Type = modular_integer
                )
        ;
         (Le_type == modular_integer, Ri_type == i) ->
                (convert_to_modulo(Le, Ri, Ri_modular),
                 midoan_solver__interpret(rem(Le, Ri_modular), types(modular_integer, modular_integer), Const_exp, Type, _Exception_)
                )
        ;
         (Le_type == i, Ri_type == modular_integer) ->
                (convert_to_modulo(Ri, Le, Le_modular),
                 midoan_solver__interpret(rem(Le_modular, Ri), types(modular_integer, modular_integer), Const_exp, Type, _Exception_)
                )
        ;
                common_util__error(10, "Failed rem operator: invalid types", no_error_consequences, [(le, Le), (ri, Ri)], 10573441, midoan_solver, midoan_solver__interpret, no_localisation, no_extra_info)
        ).


%type conversion of an expression
midoan_solver__interpret(conversion(Type_var, Exp), types(_, _), Const_exp, Type, _Exception_) :-
        !,
        (midoan_solver__interpret(conversion_may_fail(Type_var, Exp), types(_, _), Const_exp, Type, _Exception_) ->
                true
        ;
                ((midoan_type:midoan_type__is_type(Type_var) ->
                        midoan_type:midoan_type__get_typemark(Type_var, Type_mark)
                 ;
                        Type_mark = Type_var       %to deal, notably, with view conversion from controlled unification
                 ),
                 common_util__error(10, "Systematic run-time error in your code due to numeric type conversion", no_error_consequences, [(exp, Exp), (type_mark, Type_mark)], 1085244, midoan_solver, midoan_solver__interpret, no_localisation, no_extra_info)
                )
        ).
midoan_solver__interpret(conversion_may_fail(Type_var, Exp), types(_, _), Const_exp, Type, _Exception_) :-
        !,
        get_var_type(Exp, From_type),	%local predicate can be i, e, array, record, r
        ((From_type == 'i' ; From_type == 'r' ; From_type == 'modular_integer') ->   %integer and float type conversion
	        (midoan_solver__interpret(first(Type_var), types(_), Min, To_type_1, 'no_exception'),
	         midoan_solver__interpret(last(Type_var), types(_), Max, To_type_2, 'no_exception'),
                 (midoan_type:midoan_type__is_type(Type_var) ->
                        midoan_type:midoan_type__obtain_basetype(Type_var, Basic_basetype)    %05/11/10 modified (was not working for modular_integer To_type
                 ;
                        Basic_basetype = Type_var       %to deal, notably, with view conversion from controlled unification
                 ),
                 (Basic_basetype == 'standard.ads:integer' ->
                        To_type = 'i'
                 ;
                  Basic_basetype == 'standard.ads:float' ->
                        To_type = 'r'
                 ;
                  Basic_basetype == 'modular_integer' ->
                        To_type = 'modular_integer'
                 ),
                 ((To_type_1  == 'unhandled_expression' ; To_type_2 == 'unhandled_expression') ->
                        (Type = 'unhandled_expression',           %and propagates upward
                         common_util__error(4, "Unhandled type", no_error_consequences, [(exp, Exp), (to_type_1, To_type_1), (to_type_2, To_type_2)], 456705, midoan_solver, midoan_solver__interpret, no_localisation, no_extra_info)
                        )
                  ;
                   To_type_1 \= To_type_2 ->
                        common_util__error(10, "Un-identical type types in conversion.", no_error_consequences, [(exp, Exp), (to_type, To_type), (to_type_2, To_type_2)], 1057113, midoan_solver, midoan_solver__interpret, no_localisation, no_extra_info)
                  ;
                        (((From_type == 'i', To_type == 'modular_integer') -> %conversion to a modular type : the in value must lay within the base range of the modular type
                                (midoan_solver__interpret(modulus(Type_var), types(_), Modulo, i, _Exception_),
                                 midoan_modular_integer:midoan_modular_integer__create_modular_integer(Modulo, Const_exp),
                                 midoan_modular_integer:midoan_modular_integer__get('value', Const_exp, Const_value),
                                 apply_relation(>=, Exp, 'i', Min, 'i'),        %note that modulo must not be applied
                                 apply_relation(<=, Exp, 'i', Max, 'i'),
                                 midoan_solver__controlled_unification(Const_value, Exp),        %both integers
                                 Type = To_type
                                )
                         ;
                          (From_type == 'modular_integer', To_type == 'modular_integer') ->
                                (midoan_solver__interpret(modulus(Type_var), types(_), Modulo, i, _Exception_),
                                 midoan_modular_integer:midoan_modular_integer__create_modular_integer(Modulo, Const_exp),
                                 midoan_modular_integer:midoan_modular_integer__get('value', Const_exp, Const_value),
                                 midoan_modular_integer:midoan_modular_integer__get('value', Exp, Exp_value),
                                 apply_relation(>=, Exp_value, 'i', Min, 'i'),        %note that modulo must not be applied
                                 apply_relation(<=, Exp_value, 'i', Max, 'i'),
                                 midoan_solver__controlled_unification(Const_value, Exp_value),        %both integers
                                 Type = To_type
                                )
                         ;
                          (From_type == 'r', To_type == 'modular_integer') ->
                                (midoan_solver__interpret(conversion('standard.ads:integer', Exp), types(_, _), Intermediate_integer, _Intermediate_type, _Exception_),
                                 midoan_solver__interpret(conversion(Type_var, Intermediate_integer), types(_, _), Const_exp, Type, _Exception_)
                                )
                         ;
                          (From_type == 'modular_integer', To_type == 'i') ->
                                (midoan_extensions__constrain_domain(Const_exp),
                                 midoan_modular_integer:midoan_modular_integer__get('value', Exp, Const_exp),   %unification
                                 apply_relation(>=, Const_exp, i, Min, To_type),
                                 apply_relation(<=, Const_exp, i, Max, To_type),
                                 Type = To_type
                                )
                         ;
                          (From_type == 'modular_integer', To_type == 'r') ->
                                (midoan_solver__interpret(conversion('standard.ads:integer', Exp), types(_, _), Intermediate_integer, _Intermediate_type, _Exception_),
                                 midoan_solver__interpret(conversion(Type_var, Intermediate_integer), types(_, _), Const_exp, Type, _Exception_)
                                )
                         ;
                          (From_type == 'i', To_type == 'i') ->
                                (midoan_extensions__constrain_domain(Const_exp),
                                 apply_relation(>=, Const_exp, From_type, Min, To_type),
                                 apply_relation(<=, Const_exp, From_type, Max, To_type),
                                 midoan_solver__controlled_unification(Const_exp, Exp),
                                 Type = To_type
                                )
                         ;
                          (From_type == 'r', To_type == 'r') ->
                                (Const_exp = Exp,
                                 apply_relation(>=, Const_exp, From_type, Min, To_type),
                                 apply_relation(<=, Const_exp, From_type, Max, To_type),
                                 Type = To_type
                                )
                         ;
                          (From_type == 'i', To_type == 'r') ->
                                (real_conversion(Exp, i, Const_exp),
                                 apply_relation(>=, Const_exp, To_type, Min, To_type),
                                 apply_relation(<=, Const_exp, To_type, Max, To_type),
                                 Type = To_type
                                )
                         ;
                          (From_type == 'r', To_type == 'i') ->
                                (midoan_extensions__constrain_domain(Const_exp), %constraint to default fd domain
                                 real_conversion(Min, i, Min_r),
                                 real_conversion(Max, i, Max_r),
                                 apply_relation(>=, Exp, r, Min_r, r),
                                 apply_relation(<=, Exp, r, Max_r, r),
                                 midoan_extensions__round(Exp, Const_exp),
                                 Type = To_type
                                )
                         ;
                                common_util__error(10, "Unexpected type during conversion", no_error_consequences, [(from_type, From_type), (to_type, To_type)], 1060053, midoan_solver, midoan_solver__interpret, no_localisation, no_extra_info)
                         ) ->
                                true
                         ;
                                fail
                        )
                 )
                )
        ;
         From_type == 'e' ->
                (midoan_type:midoan_type__obtain_basetype(Type_var, Basetype),
                 (Basetype == base_enumeration ->
                        (midoan_enum:midoan_enum__get(position, Exp, Position_origin),
                         midoan_type:midoan_type__variable_declaration(Const_exp, Type_var),
                         midoan_enum:midoan_enum__get(position, Const_exp, Position_target),
                         Type = e,
                         (midoan_solver__sdl(=(Position_origin, Position_target)) ->
                                true
                         ;
                                fail
                         )
                        )
                 ;
                        common_util__error(10, "Unexpected target type during enumeration type conversion", no_error_consequences, [(from_type, From_type), (to_type, Type_var)], 1094736, midoan_solver, midoan_solver__interpret, no_localisation, no_extra_info)
                 )
                )
        ;
         From_type == 'array' ->  %array type conversion: at this level the operand cannot be anything else than an array i.e. cannot be an aggregate
                (midoan_type:midoan_type__obtain_basetype(Type_var, Kind_of_array),
                 (Kind_of_array = unconst_array(_) ->   %e.g. Row(V) : has the same type characteristics (length, first, last, actual bounds etc.) as V's type since Row is unconstrained
                        Const_exp = Exp                 %no change
                 ;
                        (%e.g. Row_5(V) : has the same characteristics (length, first, last, actual bounds etc.) as Row_5 since Row_5 is constrained
                         %so Row_5(V) is a Row_5 array var with V's elements
                         midoan_type:midoan_type__variable_declaration(Const_exp, Type_var),
                         midoan_solver__controlled_unification(Exp, Const_exp)
                        )
                 ),
                 Type = 'array'
                )
        ;
                common_util__error(10, "Failed type conversion: from type is unknown", no_error_consequences, [(type_var, Type_var), (exp, Exp), (from_type, From_type)], 1061101, midoan_solver, midoan_solver__interpret, no_localisation, no_extra_info)
        ).

midoan_solver__interpret(field(Record, Field), types(record, _), R, T, _Exception_) :-
	!,
	midoan_record:midoan_record__get_field(Record, Field, R, T).

midoan_solver__interpret(element(Array, Index), types('array', _), Const_exp, Type, Exception) :-
	!,
	midoan_array:midoan_array__get_element(Array, Index, Const_exp, Type, Exception).

midoan_solver__interpret(slice(Array, [Range]), types('array', _), Const_exp, 'array', _Exception_) :-
        !,
        midoan_array:midoan_array__get_slice(Array, Range, Const_exp).

midoan_solver__interpret(up_arr(Array, Index, Exp), types('array', _, _Type_exp), Const_exp, Type, Exception) :-
	!,
	midoan_array:midoan_array__up_array(Array, Index, Exp, Const_exp, Type, Exception).

midoan_solver__interpret(up_arr_slice(Array, [Range], Exp), types(array, _, _Type_exp), Const_exp, Type, Exception) :-
        !,
        midoan_array:midoan_array__up_array_slice(Array, Range, Exp, Const_exp, Type, Exception).

midoan_solver__interpret(&(Le, Ri), types(_, _), Const_exp, Const_exp, _Exception_) :-
        !,
        get_var_type(Le, Le_type),
        get_var_type(Ri, Ri_type),
        midoan_array:midoan_array__concatenate(Le_type, Ri_type, Le, Ri, Const_exp).

midoan_solver__interpret(up_rec(Record, Field, Exp), types(record, _, _Type_exp), Const_exp, record, _Exception_) :-
	!,
	midoan_record:midoan_record__up_record(Record, Field, Exp, Const_exp).

%succ works on the base type of Ri
midoan_solver__interpret(succ(Ri), types(Ri_type), Const_exp, Ri_type, _Exception_) :-
	!,
	(Ri_type == i ->
	    (midoan_extensions__constrain_domain(Const_exp),
             midoan_solver__interpret(+(Ri, 1), types(i, i), Const_exp, Ri_type, _Exception_)
            )
        ;
         Ri_type == modular_integer ->
	    midoan_modular_integer:midoan_modular_integer__succ(Ri, Const_exp)
	;
         Ri_type == e ->
	    midoan_enum:midoan_enum__succ(Ri, Const_exp)
	).

%pred works on the base type of Ri
midoan_solver__interpret(pred(Ri), types(Ri_type), Const_exp, Ri_type, _Exception_) :-
	!,
	(Ri_type == i ->
	    (midoan_extensions__constrain_domain(Const_exp),
             midoan_solver__interpret(-(Ri, 1), types(i, i), Const_exp, Ri_type, _Exception_)
            )
	;
         Ri_type == modular_integer ->
	    midoan_modular_integer:midoan_modular_integer__pred(Ri, Const_exp)
        ;
         Ri_type == e ->
	    midoan_enum:midoan_enum__pred(Ri, Const_exp)
	).

%S'pos returns universal_integer
midoan_solver__interpret(pos(Type_var, Ri), types(_, Ri_type), Const_exp, 'i', _Exception_) :-
	!,
	(Ri_type == 'modular_integer' ->
	    (midoan_modular_integer:midoan_modular_integer__get('value', Ri, Const_exp),
             midoan_extensions__constrain_domain(Const_exp),
             Const_exp = Ri
            )
	;
         Ri_type == 'i' ->
	    (midoan_extensions__constrain_domain(Const_exp),
             Const_exp = Ri
            )
	;
         Ri_type == 'e' ->
	    midoan_enum:midoan_enum__pos(Type_var, Ri, Const_exp)
        ;
                common_util__error(10, "Invalid type in pos expression", no_error_consequences, [(ri_type, Ri_type), (type_var, Type_var)], 10104352, midoan_solver, midoan_solver__interpret, no_localisation, no_extra_info)
	).

midoan_solver__interpret(val(Type_var, Ri), types(_, Op_type), Const_exp, Type, _Exception_) :-
	!,
	(midoan_type:midoan_type__obtain_basetype(Type_var, base_enumeration) ->      %Type_var is an enumeration type
	    (Type = e,
	     midoan_enum:midoan_enum__val(Type_var, Ri, Const_exp)
	    )
	;
         Op_type == i ->
	    (midoan_extensions__constrain_domain(Const_exp),
             Type = i,
	     Const_exp = Ri
	    )
        ;
         Op_type == modular_integer ->
	    (Type = modular_integer,
	     Const_exp = Ri
	    )
	).

%the minimum of the 2 parameters
midoan_solver__interpret(min(_Basetype, Le, Ri), types(_, Le_type, Ri_type), Const_exp, Type, _Exception_) :-
        !,
        ((Le_type == modular_integer, Ri_type == modular_integer) ->
                 (midoan_modular_integer:midoan_modular_integer__get(modulo, Le, Modulo),
                  midoan_modular_integer:midoan_modular_integer__create_modular_integer(Modulo, Const_exp),
                  midoan_modular_integer:midoan_modular_integer__get(value, Le, Le_value),
                  midoan_modular_integer:midoan_modular_integer__get(value, Ri, Ri_value),
                  midoan_modular_integer:midoan_modular_integer__get(value, Const_exp, Const_exp_value),
                  midoan_solver__interpret(min(_Basetype, Le_value, Ri_value), types(_, i, i), Const_exp_value, i, _Exception_),
                  Type = modular_integer
                 )
        ;
         (Le_type == modular_integer, Ri_type == i) ->          %a modular integer and a ground integer
                (convert_to_modulo(Le, Ri, Ri_modular),
                 midoan_solver__interpret(min(_Basetype, Le, Ri_modular), types(_, modular_integer, modular_integer), Const_exp, Type, _Exception_)
                )
        ;
         (Le_type == i, Ri_type == modular_integer) ->
                (convert_to_modulo(Ri, Le, Le_modular),
                 midoan_solver__interpret(min(_Basetype, Le_modular, Ri), types(_, modular_integer, modular_integer), Const_exp, Type, _Exception_)
                )
        ;
         Le_type == i ->
                (midoan_extensions__constrain_domain(Const_exp),
                 midoan_extensions__min(Le, Ri, Const_exp),
                 Type = i
                )
        ;
         Le_type == r ->
                ({Const_exp = min(Le, Ri)},
                 Type = r
                )
        ;
         Le_type == e ->
                (midoan_enum:midoan_enum__get(type, Le, Type_var),
                 midoan_type:midoan_type__variable_declaration(Const_exp, Type_var),
                 midoan_enum:midoan_enum__get(position, Le, Le_position),
                 midoan_enum:midoan_enum__get(position, Ri, Ri_position),
                 midoan_enum:midoan_enum__get(position, Const_exp, Const_exp_position),
                 midoan_solver__interpret(min(_Basetype, Le_position, Ri_position), types(_, i, i), Const_exp_position, i, _Exception_),
                 Type = e
                )
        ).

%the maximum of the 2 parameters
midoan_solver__interpret(max(_Basetype, Le, Ri), types(_, Le_type, Ri_type), Const_exp, Type, _Exception_) :-
        !,
        ((Le_type == modular_integer, Ri_type == modular_integer) ->
                 (midoan_modular_integer:midoan_modular_integer__get(modulo, Le, Modulo),
                  midoan_modular_integer:midoan_modular_integer__create_modular_integer(Modulo, Const_exp),
                  midoan_modular_integer:midoan_modular_integer__get(value, Le, Le_value),
                  midoan_modular_integer:midoan_modular_integer__get(value, Ri, Ri_value),
                  midoan_modular_integer:midoan_modular_integer__get(value, Const_exp, Const_exp_value),
                  midoan_solver__interpret(max(_Basetype, Le_value, Ri_value), types(_, i, i), Const_exp_value, i, _Exception_),
                  Type = modular_integer
                 )
        ;
         (Le_type == modular_integer, Ri_type == i) ->
                (convert_to_modulo(Le, Ri, Ri_modular),
                 midoan_solver__interpret(max(_Basetype, Le, Ri_modular), types(_, modular_integer, modular_integer), Const_exp, Type, _Exception_)
                )
        ;
         (Le_type == i, Ri_type == modular_integer) ->
                (convert_to_modulo(Ri, Le, Le_modular),
                 midoan_solver__interpret(max(_Basetype, Le_modular, Ri), types(_, modular_integer, modular_integer), Const_exp, Type, _Exception_)
                )
        ;
         Le_type == i ->
                (midoan_extensions__constrain_domain(Const_exp),
                 midoan_extensions__max(Le, Ri, Const_exp),
                 Type = i
                )
        ;
         Le_type == r ->
                ({Const_exp = max(Le, Ri)},
                 Type = r
                )
        ;
         Le_type == e ->
                (midoan_enum:midoan_enum__get(type, Le, Type_var),
                 midoan_type:midoan_type__variable_declaration(Const_exp, Type_var),
                 midoan_enum:midoan_enum__get(position, Le, Le_position),
                 midoan_enum:midoan_enum__get(position, Ri, Ri_position),
                 midoan_enum:midoan_enum__get(position, Const_exp, Const_exp_position),
                 midoan_solver__interpret(max(_Basetype, Le_position, Ri_position), types(_, i, i), Const_exp_position, i, _Exception_),
                 Type = e
                )
        ).

%unchecked conversion is problematic : e.g. from byte to packed array of oolean etc. : will have to be done on a case by caes basis ...
%and of course, ranges should not be checked
midoan_solver__interpret(unchecked_conversion(Target_type_var, Le), types(Le_type), Const_exp, Type, _Exception_) :-
        !,
        midoan_type:midoan_type__obtain_basetype(Target_type_var, Target_basetype),
        ((Le_type == 'i', Target_basetype == 'standard.ads:integer') ->
                (Const_exp = Le,                                        %no range checkecking performed on purpose (may not be a valid integer)
                 Type = 'i'
                )
        ;
         (Le_type == 'i', Target_basetype == 'modular_integer') ->
                (midoan_type:midoan_type__variable_declaration(Const_exp, Target_type_var),
                 (ground(Le) ->
                        (midoan_modular_integer:midoan_modular_integer__get('modulo', Const_exp, Modulo),
                         midoan_solver__interpret(mod(Le, Modulo), types('i', 'i'), Const_value, 'i', _Exception_), %to ensure -10 (e.g.) gets converted
                         midoan_modular_integer:midoan_modular_integer__get('value', Const_exp, Const_value)
                        )
                 ;
                        midoan_modular_integer:midoan_modular_integer__get('value', Const_exp, Le)
                 ),
                 Type = 'modular_integer'
                )
        ;
         (Le_type == 'i', Target_basetype == 'base_enumeration') ->
                (midoan_type:midoan_type__variable_declaration(Const_exp, Target_type_var),
                 midoan_enum:midoan_enum__get('position', Const_exp, Le),             %no range checking perfomed on purpose (may not be a valid enumeration literal)
                 Type = 'e'
                )
        ;
         Le_type == 'modular_integer' ->
                (midoan_modular_integer:midoan_modular_integer__get('value', Le, Le_value),
                 midoan_solver__interpret(unchecked_conversion(Target_type_var, Le_value), types('i'), Const_exp, Type, _Exception_)
                )
        ;
         (Le_type == 'r', Target_basetype == 'standard.ads:float') ->
                (Const_exp = Le,                                        %no range checkecking performed on purpose (may not be a valid float)
                 Type = 'r'
                )
        ;
         (Le_type == 'r', Target_basetype == 'standard.ads:integer') ->
                ((ground(Le) ->
                        (Fractional is abs(integer(Le) - Le),
                         (Fractional = 0.0 ->
                                (Const_exp is integer(Le),      %may obviously be out of range for target type : that's normal behaviour for unchecked conversions
                                 Type = 'i'
                                )
                         ;
                                (common_util__error(1, "Unchecked conversion from r to i is incorrect for ground real: should return an invalid integer", 'no_error_consequences', [('target_basetype', Basetype)], 10122037, 'midoan_solver', 'midoan_solver__interpret', 'no_localisation', 'no_extra_info'),
                                 Type = 'unhandled_expression',
                                 Const_exp = 'invalid'
                                )
                         )
                        )
                 ;
                        (common_util__error(1, "Unchecked conversion from r to i is incorrect for variable: should delay until ground instead of converting", 'no_error_consequences', [('target_basetype', Basetype)], 10122017, 'midoan_solver', 'midoan_solver__interpret', 'no_localisation', 'no_extra_info'),
                         midoan_solver__interpret(conversion_may_fail(Target_type_var, Le), types(_, _), Const_exp, Type, _Exception_)
                        )
                 )
                )
        ;
                common_util__error(10, "Unhandled unchecked conversion", 'no_error_consequences', [('le_type', Le_type), ('target_basetype', Target_basetype)], 10114520, midoan_solver, midoan_solver__interpret, no_localisation, no_extra_info)
        ).
%not a variable, not a number, not a compound, must be an atom i.e. a literal e.g. 'monday'
midoan_solver__interpret(Exp, _, _, _, _Exception) :-
	!,
        common_util__error(10, "Unknown expression in interpreter", "Cannot proceed: should never happen", [(exp, Exp)], 1073302, midoan_solver, midoan_solver__interpret, no_localisation, no_extra_info).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_solver__sdl(Cond) :-
	(midoan_enum:midoan_enum__is_enum(Cond) ->                    %Cond must be a boolean variable that must be true
                (midoan_enum:midoan_enum__get(type, Cond, Type_var),
                 midoan_type:midoan_type__get_attribute(Type_var, last, True_enum),        %retrieve the true enumeration literal
                 relation_constraint(=, Cond, True_enum)                                            %if it cannot be constrained to true we fail
                )
        ;
		boolean(Cond)
	).

boolean(not(Bool)) :-
	(midoan_enum:midoan_enum__is_enum(Bool) ->            %Bool must be a boolean variable that must be false
                (midoan_enum:midoan_enum__get(type, Bool, Type_var),
                 midoan_type:midoan_type__get_attribute(Type_var, first, False_enum),        %retrieve the false enumeration literal
                 relation_constraint(=, Bool, False_enum)                               %if it cannot be constrained to false we fail
                )
        ;
	        negate(Bool)
	).

boolean(=(X, Y)) :-
	relation_constraint(=, X, Y).
boolean(<>(X, Y)) :-
	relation_constraint(<>, X, Y).
boolean(<(X, Y)) :-
	relation_constraint(<, X, Y).
boolean(>(X, Y)) :-
	relation_constraint(>, X, Y).
boolean(<=(X, Y)) :-
	relation_constraint(<=, X, Y).
boolean(>=(X, Y)) :-
	relation_constraint(>=, X, Y).
boolean(is_in(X, [Min, Max])) :-
	relation_constraint('is_in', X, Min, Max).
boolean(is_not_in(X, [Min, Max])) :-
	relation_constraint('is_not_in', X, Min, Max).
boolean(valid(X, Type_var)) :-
	relation_constraint('valid', X, Type_var).
%%%
negate(not(Rel)) :-                 %not not Rel == Rel
	midoan_solver__sdl(Rel).
negate(=(X, Y)) :-
	relation_constraint(<>, X, Y).
negate(<>(X, Y)) :-
	relation_constraint(=, X, Y).
negate(<(X, Y)) :-
	relation_constraint(>=, X, Y).
negate(>(X, Y)) :-
	relation_constraint(<=, X, Y).
negate(<=(X, Y)) :-
	relation_constraint(>, X, Y).
negate(>=(X, Y)) :-
	relation_constraint(<, X, Y).
negate(is_in(X, [Min, Max])) :-
	relation_constraint('is_not_in', X, Min, Max).
negate(is_not_in(X, [Min, Max])) :-
	relation_constraint('is_in', X, Min, Max).
negate(valid(X, Type_var)) :-
	relation_constraint('is_not_valid', X, Type_var).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%