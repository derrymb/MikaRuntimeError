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
% midoan_solver_modular_integer.pl
% defines module modular_integer for modular integer meta variables handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a modular integer variable is of the form midoan_modular_integer(Modulo, Value) where Value is not a ground fd integer
% a ground modular integer value is of the form midoan_modular_integer(Modulo, Value) where Value is a ground integer
% Value is an fd variable
:- module(midoan_modular_integer, []).

:- use_module([	library(clpfd), %for domain/3
                library(lists)  %for append
	     ]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%check if a variable is a modular integer variable or a ground modular integer
midoan_modular_integer__is_modular_integer(Modular_integer) :-
	(nonvar(Modular_integer) ->
	        Modular_integer = midoan_modular_integer(_Modulo, _Value)       %may fail
        ;
                fail
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%create a modular integer variable or a ground modular integer (if the modulo is 1)
midoan_modular_integer__create_modular_integer(Modulo, midoan_modular_integer(Modulo, Value)) :-
        (integer(Modulo) ->      %a ground integer
                (midoan_extensions:max_clpfd(Max_clpfd),
                 (Modulo >= Max_clpfd ->
                        domain([Value], 0, Max_clpfd)           %the maximum we can provide
                 ;
                  Modulo > 0 ->
                        (Modulo_minus_one is Modulo - 1,
	                 domain([Value], 0, Modulo_minus_one)           %constrain the position as an fd variable
                        )
                 ;
                        common_util:common_util__error(10, "Creation of a modular integer variable with non positive modulo", "should never happen", [(modulo, Modulo)], 108759, midoan_modular_integer, midoan_modular_integer__create_modular_integer, no_localisation, no_extra_info)
                 )
                )
        ;
                common_util:common_util__error(10, "Creation of a modular integer variable with non known integer as modulo", "should never happen", [(modulo, Modulo)], 1085555, midoan_modular_integer, midoan_modular_integer__create_modular_integer, no_localisation, no_extra_info)
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_modular_integer__get('value', midoan_modular_integer(_Modulo, Value), Value).
midoan_modular_integer__get('modulo', midoan_modular_integer(Modulo, _Value), Modulo).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_modular_integer__ground(Modular_integer) :-
        midoan_modular_integer__is_modular_integer(Modular_integer),
        midoan_modular_integer__get('value', Modular_integer, Value),
        ground(Value).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%apply the constraint 'R is the predecessor of X'
midoan_modular_integer__pred(X, R) :-
	midoan_modular_integer__succ(R, X).         %as simple as that
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% apply the constraint 'R' is the successor of X'
midoan_modular_integer__succ(X, R) :-
        (((\+ midoan_modular_integer__is_modular_integer(X)) , (\+ midoan_modular_integer__is_modular_integer(R))) ->
                common_util:common_util__error(10, "No modular type available in this context", "should never happen", [(x, X), (r, R)], 1010314, midoan_modular_integer, midoan_modular_integer__succ, no_localisation, no_extra_info)
        ;
                ((\+ midoan_modular_integer__is_modular_integer(X) ->  %this happens for somes call from the engine
                        (midoan_modular_integer__get('modulo', R, Modulo),
                         midoan_modular_integer__create_modular_integer(Modulo, X)
                        )
                 ;
                  \+ midoan_modular_integer__is_modular_integer(R) ->  %this happens for somes call from the engine
                        (midoan_modular_integer__get('modulo', X, Modulo),
                         midoan_modular_integer__create_modular_integer(Modulo, R)
                        )
                 ;
                        true
                 ),
                 midoan_modular_integer__get('modulo', X, Modulo),
                 midoan_modular_integer__get('value', X, ValueX),
                 midoan_modular_integer__get('value', R, ValueR),
                 ValueR #= (ValueX + 1) mod Modulo
                )
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_modular_integer__unary_bitwise(Operator, Le_const, types(Le_type), Const_exp) :-
        (Le_type == 'modular_integer' ->
                midoan_modular_integer:midoan_modular_integer__get('modulo', Le_const, Modulo)
        ;
                common_util:common_util__error(10, "Unary bitwise expressions must involve a modular integer", 'no_error_consequences', [('le_type', Le_type)], 10851604, 'midoan_modular_integer', 'midoan_modular_integer__unary_bitwise', 'no_localisation', 'no_extra_info')
        ),
        (Operator == 'not' ->
                (midoan_modular_integer__create_modular_integer(Modulo, Const_exp),
                 midoan_modular_integer__get('value', Const_exp, Cons_exp_value),
                 midoan_modular_integer__get('value', Le_const, Le_const_value),
                 Top_bound is Modulo - 1,                %Ada RM "The result of the operator not for a modular type is defined as the difference between the high bound of the base range of the type and the value of the operand."
                 midoan_solver:midoan_solver__interpret(-(Top_bound, Le_const_value), types(i, i), Cons_exp_value, i, _Exception_)
                )
        ;
                common_util:common_util__error(10, "Unary bitwise operator is not valid", 'no_error_consequences', [('operator', Operator)], 10951257, 'midoan_modular_integer', 'midoan_modular_integer__unary_bitwise', 'no_localisation', "The only valid unary bitwise operator is: not")
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_modular_integer__binary_bitwise(Operator, Le_const, Ri_const, types(Le_type, Ri_type), Const_exp) :-
        ((Le_type == 'i', Ri_type == 'modular_integer') ->
                midoan_modular_integer:midoan_modular_integer__get('modulo', Ri_const, Modulo)
        ;
         (Le_type == 'modular_integer', Ri_type == 'i') ->
                midoan_modular_integer:midoan_modular_integer__get('modulo', Le_const, Modulo)
        ;
         (Le_type == 'modular_integer', Ri_type == 'modular_integer') ->        %should be the same modulo
                midoan_modular_integer:midoan_modular_integer__get('modulo', Le_const, Modulo)
        ;
                common_util:common_util__error(10, "Binary bitwise expressions must involve a modular integer", 'no_error_consequences', [('le_type', Le_type), ('ri_type', Ri_type)], 1038941, 'midoan_modular_integer', 'midoan_modular_integer__binary_bitwise', 'no_localisation', 'no_extra_info')
        ),
        midoan_modular_integer__create_modular_integer(Modulo, Const_exp),
        midoan_modular_integer__get('value', Const_exp, Cons_exp_value),
        build_suspension_all_ground([Le_const, Ri_const], [Le_type, Ri_type], Ground, Susp, (Le_value, Ri_value)),
        (Ground = (true , true) ->      %this is quite basic we could do a better job especially if the result is actually ground
                binary_bitwise(Operator, Le_value, Ri_value, Modulo, Cons_exp_value)
        ;
                when(Susp, binary_bitwise(Operator, Le_value, Ri_value, Modulo, Cons_exp_value))
        ).
%%%
        build_suspension_all_ground([V|Rest_var], [Type|Rest_type], Ground, Susp, Values) :-
                build_suspension_single(V, Type, Ground_single, Susp_single, Value_single),
                (Rest_var == [] ->
                        (Ground = Ground_single,
                         Susp = Susp_single,
                         Values = Value_single
                        )
                ;
                        (build_suspension_all_ground(Rest_var, Rest_type, Ground_rest, Susp_rest, Values_rest),
                         Ground = (Ground_single , Ground_rest),
                         Susp = (Susp_single , Susp_rest),
                         Values = (Value_single , Values_rest)
                        )
                ).
%%%
                build_suspension_single(V, Type, Ground, Susp, Values) :-
                        (Type == 'i' ->
                                ((ground(V) ->
                                        Ground = 'true'
                                 ;
                                        Ground = 'false'
                                 ),
                                 Susp = ground(V),
                                 Values = V
                                )
                        ;
                         Type == 'modular_integer' ->
                                ((midoan_modular_integer__ground(V) ->
                                        Ground = 'true'
                                 ;
                                        Ground = 'false'
                                 ),
                                 midoan_modular_integer__get('value', V, Value),
                                 Susp = ground(Value),
                                 Values = Value
                                )
                        ).
%%%
binary_bitwise(Operator, Le_value, Ri_value, Modulo, Result_decimal) :-
        to_binary(Le_value, Le_bin),
        to_binary(Ri_value, Ri_bin),
        list_op(Le_bin, Ri_bin, Result_bin, Operator),
        to_decimal(Result_bin, Raw_decimal),
        midoan_solver:midoan_solver__interpret(mod(Raw_decimal, Modulo), types(i, i), Result_decimal, _, _Exception_).

%%%
        list_op([], [], [], _Operator).
        list_op([B|R], [C|S], [D|T], Operator) :-
                op_bit(Operator, B, C, D),
                list_op(R, S, T, Operator).
%%%
                op_bit('and', 0, 0, 0) :- !.
                op_bit('and', 0, 1, 0) :- !.
                op_bit('and', 1, 0, 0) :- !.
                op_bit('and', 1, 1, 1) :- !.
                op_bit('or', 0, 0, 0) :- !.
                op_bit('or', 0, 1, 1) :- !.
                op_bit('or', 1, 0, 1) :- !.
                op_bit('or', 1, 1, 1) :- !.
                op_bit('xor', 0, 0, 0) :- !.
                op_bit('xor', 0, 1, 1) :- !.
                op_bit('xor', 1, 0, 1) :- !.
                op_bit('xor', 1, 1, 0) :- !.
                op_bit(Operator, _, _, _) :-
                        common_util:common_util__error(10, "Binary bitwise operator is invalid", 'no_error_consequences', [('operator', Operator)], 101511230, 'midoan_modular_integer', 'op_bit', 'no_localisation', "Valid binary bitwise operators are and, or, xor.").

%%%
to_binary(Value, BinaryL) :-
        to_binary2(Value, 64, BinaryL).  %use 64 bits
%%%
        to_binary2(Value, Len, BinaryL) :-
                New_value is Value // 2,
                Bit is Value mod 2,
                Len2 is Len - 1,
                (Len2 == 0 ->   %if the length is 0 but the Value isn't we need more bits : generate an error
                        BinaryL_rest = []
                ;
                        to_binary2(New_value, Len2, BinaryL_rest)
                ),
                append(BinaryL_rest, [Bit], BinaryL).
%%%
to_decimal(Binary, Decimal) :-
        to_decimal2(Binary, _, Decimal).
%%%
        to_decimal2([Bit], 1, Decimal) :-
                !,
                Decimal is Bit*integer(2**0).
        to_decimal2([Bit|L], N1, Decimal) :-
                to_decimal2(L, N, Decimal2),
                Decimal is Decimal2 + Bit*integer(2**N),
                N1 is N + 1.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
