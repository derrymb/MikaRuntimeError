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
% mika_symbolic__interpret_util.pl
% module mika_symbolic
% utilitarian predicates used during the symbolic interpretation of intermediate Ada expression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%added 10/03/08
remove_prefix_util(selected(Functor, Operand), Prefix_less) :-
        (mika_name_atts:mika_name_atts__is_name_atts(Operand) ->
                Prefix_less = record_access(Functor, Operand)
        ;
                Prefix_less = Operand   %Functor could be a package, a subprogram, a named loop or block [when a subprogram the call is not made]
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_string_operator(Arity, List, Operator) :-
        fso(List, Arity, Operator),
        !.
find_string_operator(Arity, List, _) :-
        common_util__error(10, "The binary operator is unknown", "Cannot proceed", [(arity, Arity), (list, List)], 1025120, mika_symbolic, find_string_operator, no_localisation, "Should never happen").

fso([43] , binary, '+') :- !.
fso([45] , binary, '-') :- !.
fso([42] , binary, '*') :- !.
fso([47] , binary, '/') :- !.
fso([42, 42] , binary, '**') :- !.
fso([38] , binary, '&') :- !.
fso([61] , binary, '=') :- !.
fso([60, 62] , binary, '<>') :- !.
fso([60] , binary, '<') :- !.
fso([60, 61] , binary, '<=') :- !.
fso([62] , binary, '>') :- !.
fso([62, 61] , binary, '>=') :- !.
fso(L, binary, Operator) :-
        !,
        fso_put_upper_case(L, L_upper),
        (L_upper == [65, 78, 68] ->
                Operator = 'and'
        ;
         L_upper == [77, 79, 68] ->
                Operator = 'mod'
        ;
         L_upper == [79, 82] ->
                Operator = 'or'
        ;
         L_upper == [82, 69, 77] ->
                Operator = 'rem'
        ;
         L_upper == [88, 79, 82] ->
                Operator = 'xor'
        ).

fso([43] , unary, '+') :- !.
fso([45] , unary, '-') :- !.
fso(L, unary, Operator) :-
        !,
        fso_put_upper_case(L, L_upper),
        (L_upper == [65, 66, 83] ->
                Operator = 'abs'
        ;
         L_upper == [78, 79, 84] ->
                Operator = 'not'
        ).


fso_put_upper_case([], []).
fso_put_upper_case([Next|Rest], [Upper|Rest_upper]) :-
        (Next > 96 ->
                Upper is Next - 32
        ;
                Upper is Next
        ),
        fso_put_upper_case(Rest, Rest_upper).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clean_up_double_quotes(CodeL_in, CodeL_out) :-
        (append(Start, [34, 34|Rest], CodeL_in) ->
                (clean_up_double_quotes(Rest, Rest_out),
                 append(Start, [34|Rest_out], CodeL_out)
                )
        ;
                CodeL_in = CodeL_out
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_unhandled_type([]) :-
        fail.
is_unhandled_type([First|Rest]) :-
        (First == unhandled_expression ->
                true
        ;
         (nonvar(First), First = [_]) ->
                (is_unhandled_type(First) ->
                        true
                ;
                        is_unhandled_type(Rest)
                )
        ;
                is_unhandled_type(Rest)
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%