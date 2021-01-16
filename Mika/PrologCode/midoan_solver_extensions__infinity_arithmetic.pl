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
% midoan_solver_extensions__infinity_arithmetic.pl
% part of the midoan_extensions module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_extensions__infinity_different(L, R) :-
        ((\+ ground(L), \+ ground(R)) ->
                L #\= R
        ;
                (midoan_extensions:max_clpfd(Max_int),
                 midoan_extensions:min_clpfd(Min_int),
                 ((ground(L), \+ ground(R)) ->
                        ((L >= Max_int ; L =< Min_int) ->
                                true            %will be unsound if later R is unified with a very large/small integer
                        ;
                                L #\= R
                        )
                 ;
                  (\+ ground(L), ground(R)) ->
                        ((R >= Max_int ; R =< Min_int) ->
                                true            %will be unsound if later R is unified with a very large/small integer
                        ;
                                L #\= R
                        )
                 ;
                        L \= R
                 )
                )
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_extensions__infinity_greater(L, R) :-
        ((\+ ground(L), \+ ground(R)) ->
                L #> R
        ;
                (midoan_extensions:max_clpfd(Max_int),
                 midoan_extensions:min_clpfd(Min_int),
                 ((ground(L), \+ ground(R)) ->
                        (L >= Max_int ->
                                true            %not entirely correct: R and L could both be Maxint which should lead to NaN, or R could be unified to an even larger integer than L
                        ;
                         L =< Min_int ->
                                fail            %not entirely correct: similar to above
                        ;
                                L #> R
                        )
                 ;
                  (\+ ground(L), ground(R)) ->
                        (R >= Max_int ->
                                fail            %not entirely correct: similar to above
                        ;
                         R =< Min_int ->
                                true            %not entirely correct: similar to above
                        ;
                                L #> R
                        )
                 ;
                  (ground(L), ground(R)) ->
                        (L == Max_int ->
                                (R == Max_int ->
                                        common_util:common_util__error(10, "NaN in greater than due to integer overflow", no_error_consequences, [(l, L), (r, R)], 1081170, midoan_extensions, midoan_extensions__infinity_greater, no_localisation, no_extra_info)
                                ;
                                        true
                                )
                        ;
                         L == Min_int ->
                                (R == Min_int ->
                                        common_util:common_util__error(10, "NaN in greater than due to integer overflow", no_error_consequences, [(l, L), (r, R)], 1081170, midoan_extensions, midoan_extensions__infinity_greater, no_localisation, no_extra_info)
                                ;
                                        fail
                                )
                        ;
                         R == Min_int ->
                                true
                        ;
                         R == Max_int ->
                                fail
                        ;
                                L > R
                        )
                 )
                )
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_extensions__infinity_greater_or_equal(L, R) :-
        ((\+ ground(L), \+ ground(R)) ->
                L #>= R
        ;
                (midoan_extensions:max_clpfd(Max_int),
                 midoan_extensions:min_clpfd(Min_int),
                 ((ground(L), \+ ground(R)) ->
                        (L >= Max_int ->
                                true            %not entirely correct: similar to above
                        ;
                         L =< Min_int ->
                                fail            %not entirely correct: similar to above
                        ;
                                L #>= R
                        )
                 ;
                  (\+ ground(L), ground(R)) ->
                        (R >= Max_int ->
                                fail            %not entirely correct: similar to above
                        ;
                         R =< Min_int ->
                                true            %not entirely correct: similar to above
                        ;
                                L #>= R
                        )
                 ;
                  (ground(L), ground(R)) ->
                        (L == Max_int ->
                                (R == Max_int ->
                                        true    %(allowed because of the =)
                                ;
                                        true
                                )
                        ;
                         L == Min_int ->
                                (R == Min_int ->
                                        true    %(allowed because of the =)
                                ;
                                        fail
                                )
                        ;
                         R == Min_int ->
                                true
                        ;
                         R == Max_int ->
                                fail
                        ;
                                L >= R
                        )
                 )
                )
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_extensions__infinity_less(L, R) :-
        ((\+ ground(L), \+ ground(R)) ->
                L #< R
        ;
                (midoan_extensions:max_clpfd(Max_int),
                 midoan_extensions:min_clpfd(Min_int),
                 ((ground(L), \+ ground(R)) ->
                        (L >= Max_int ->
                                fail            %not entirely correct: similar to above
                        ;
                         L =< Min_int ->
                                true            %not entirely correct: similar to above
                        ;
                                L #< R
                        )
                 ;
                  (\+ ground(L), ground(R)) ->
                        (R >= Max_int ->
                                true            %not entirely correct: similar to above
                        ;
                         R =< Min_int ->
                                fail            %not entirely correct: similar to above
                        ;
                                L #< R
                        )
                 ;
                  (ground(L), ground(R)) ->
                        (L == Max_int ->
                                (R == Max_int ->
                                        common_util:common_util__error(10, "NaN in greater than due to integer overflow", no_error_consequences, [(l, L), (r, R)], 1081170, midoan_extensions, midoan_extensions__infinity_greater, no_localisation, no_extra_info)
                                ;
                                        fail
                                )
                        ;
                         L == Min_int ->
                                (R == Min_int ->
                                        common_util:common_util__error(10, "NaN in greater than due to integer overflow", no_error_consequences, [(l, L), (r, R)], 1081170, midoan_extensions, midoan_extensions__infinity_greater, no_localisation, no_extra_info)
                                ;
                                        true
                                )
                        ;
                         R == Min_int ->
                                fail
                        ;
                         R == Max_int ->
                                true
                        ;
                                L < R
                        )
                 )
                )
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_extensions__infinity_less_or_equal(L, R) :-
        ((\+ ground(L), \+ ground(R)) ->
                L #=< R
        ;
                (midoan_extensions:max_clpfd(Max_int),
                 midoan_extensions:min_clpfd(Min_int),
                 ((ground(L), \+ ground(R)) ->
                        (L >= Max_int ->
                                fail            %not entirely correct: similar to above
                        ;
                         L =< Min_int ->
                                true            %not entirely correct: similar to above
                        ;
                                L #=< R
                        )
                 ;
                  (\+ ground(L), ground(R)) ->
                        (R >= Max_int ->
                                true            %not entirely correct: similar to above
                        ;
                         R =< Min_int ->
                                fail            %not entirely correct: similar to above
                        ;
                                L #=< R
                        )
                 ;
                  (ground(L), ground(R)) ->
                        (L == Max_int ->
                                (R == Max_int ->
                                        true    %(allowed because of the =)
                                ;
                                        fail
                                )
                        ;
                         L == Min_int ->
                                (R == Min_int ->
                                        true    %(allowed because of the =)
                                ;
                                        true
                                )
                        ;
                         R == Min_int ->
                                fail
                        ;
                         R == Max_int ->
                                true
                        ;
                                L =< R
                        )
                 )
                )
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_extensions__infinity_unary_minus(Le_const, Const_exp) :-
        (ground(Le_const) ->
                (midoan_extensions:max_clpfd(Max_int),
                 midoan_extensions:min_clpfd(Min_int),
                 (Le_const == Max_int ->
                        Const_exp = Min_int
                 ;
                  Le_const == Min_int ->
                        Const_exp = Max_int
                 ;
                        (Tmp is -Le_const,      %we cannot use Const_exp #= -Le_const, because Le_const maybe very large
                         midoan_solver:midoan_solver__controlled_unification(Const_exp, Tmp)
                        )
                 )
                )
        ;
                (midoan_extensions__constrain_domain(Const_exp),
                 Const_exp #= Le_const * (1-2)       %weird fut clpfd does not have unary minus ; also, unsound if Le_const is unified with a very large (or small) integer later

                )
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_extensions__infinity_binary_plus(Le_const, Ri_const, Const_exp, Type) :-
        ((\+ ground(Le_const), \+ ground(Ri_const)) ->
                (midoan_extensions__constrain_domain(Const_exp),
                 Const_exp #= Le_const + Ri_const       %unsound if Le_const and/or Ri_const is unified with a very large (or small) integer later
	        )
        ;
                (midoan_extensions:max_clpfd(Max_int),
                 midoan_extensions:min_clpfd(Min_int),
                 ((ground(Le_const), \+ ground(Ri_const)) ->
                        (Le_const >= Max_int ->
                                Const_exp = Max_int     %unsound if Ri_const is unified with a large(or small) integer later and Le_const is actually > Max_int
                        ;
                         Le_const =< Min_int ->
                                Const_exp = Min_int     %unsound ...
                        ;
                                (midoan_extensions__constrain_domain(Const_exp),
                                 Const_exp #= Le_const + Ri_const       %unsound ...
                                )
                        )
                 ;
                  (\+ ground(Le_const), ground(Ri_const)) ->
                        (Ri_const >= Max_int ->
                                Const_exp = Max_int             %unsound ...
                        ;
                         Ri_const =< Min_int ->
                                Const_exp = Min_int             %unsound ...
                        ;
                                (midoan_extensions__constrain_domain(Const_exp),
                                 Const_exp #= Le_const + Ri_const       %unsound ...
                                )
                        )
                 ;
                  (ground(Le_const), ground(Ri_const)) ->
                        (Le_const == Max_int ->
                                (Ri_const == Min_int ->
                                        (common_util:common_util__error(4, "NaN in addition due to integer overflow", no_error_consequences, [(le_const, Le_const), (ri_const, Ri_const)], 481170, midoan_extensions, midoan_extensions__infinity_binary_plus, no_localisation, no_extra_info),
                                         Type = unhandled_expression
                                        )
                                ;
                                        Const_exp = Max_int     %unsound ...
                                )
                        ;
                         Ri_const == Max_int ->
                                (Le_const == Min_int ->
                                        (common_util:common_util__error(4, "NaN in addition due to integer overflow", no_error_consequences, [(le_const, Le_const), (ri_const, Ri_const)], 490170, midoan_extensions, midoan_extensions__infinity_binary_plus, no_localisation, no_extra_info),
                                         Type = 'unhandled_expression'
                                        )
                                ;
                                        Const_exp = Max_int     %unsound ...
                                )
                        ;
                         Le_const == Min_int ->
                                Const_exp = Min_int             %unsound ...
                        ;
                         Ri_const == Min_int ->
                                Const_exp = Min_int             %unsound ...
                        ;
                                (Tmp is Le_const + Ri_const,
                                 midoan_solver:midoan_solver__controlled_unification(Const_exp, Tmp)
                                )
                        )
                 )
                )
        ),
        (Type == 'unhandled_expression' ->
                true
        ;
                Type = 'i'        %that's the default in all successful cases
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_extensions__infinity_binary_minus(Le_const, Ri_const, Const_exp, Type) :-
        ((\+ ground(Le_const), \+ ground(Ri_const)) ->
                (midoan_extensions__constrain_domain(Const_exp),
                 Const_exp #= Le_const - Ri_const       %unsound ...
	        )
        ;
                (midoan_extensions:max_clpfd(Max_int),
                 midoan_extensions:min_clpfd(Min_int),
                 ((ground(Le_const), \+ ground(Ri_const)) ->
                        (Le_const >= Max_int ->
                                Const_exp = Max_int     %unsound ...
                        ;
                         Le_const =< Min_int ->
                                Const_exp = Min_int     %unsound ...
                        ;
                                (midoan_extensions__constrain_domain(Const_exp),
                                 Const_exp #= Le_const - Ri_const       %unsound ...
                                )
                        )
                  ;
                  (\+ ground(Le_const), ground(Ri_const)) ->
                        (Ri_const >= Max_int ->
                                Const_exp = Min_int     %unsound ...
                        ;
                         Ri_const =< Min_int ->
                                Const_exp = Max_int     %unsound ...
                        ;
                                (midoan_extensions__constrain_domain(Const_exp),
                                 Const_exp #= Le_const - Ri_const       %unsound ...
                                )
                        )
                  ;
                  (ground(Le_const), ground(Ri_const)) ->
                        (Le_const == Max_int ->
                                (Ri_const == Max_int ->
                                        (common_util:common_util__error(4, "NaN in substraction due to integer overflow", no_error_consequences, [(le_const, Le_const), (ri_const, Ri_const)], 417222, midoan_extensions, midoan_extensions__infinity_binary_minus, no_localisation, no_extra_info),
                                         Type = unhandled_expression
                                        )
                                ;
                                        Const_exp = Max_int     %unsound ...
                                )
                        ;
                         Ri_const == Max_int ->
                                        Const_exp = Min_int     %unsound ...
                        ;
                         Le_const == Min_int ->
                                (Ri_const == Min_int ->
                                        (common_util:common_util__error(4, "NaN in substraction due to integer overflow", no_error_consequences, [(le_const, Le_const), (ri_const, Ri_const)], 419023, midoan_extensions, midoan_extensions__infinity_binary_minus, no_localisation, no_extra_info),
                                         Type = unhandled_expression
                                        )
                                ;
                                        Const_exp = Min_int     %unsound ...
                                )
                        ;
                         Ri_const == Min_int ->
                                        Const_exp = Max_int     %unsound ...
                        ;
                                (Tmp is Le_const - Ri_const,
                                 midoan_solver:midoan_solver__controlled_unification(Const_exp, Tmp)
                                )
                        )
                 )
                )
        ),
        (Type == unhandled_expression ->
                true
        ;
                Type = 'i'        %that's the default in all successful cases
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_extensions__infinity_multiply(Le_const, Ri_const, Const_exp, Type) :-
        ((\+ ground(Le_const), \+ ground(Ri_const)) ->
                (midoan_extensions__constrain_domain(Const_exp),
                 Const_exp #= Le_const * Ri_const       %unsound ...
	        )
        ;
                (midoan_extensions:max_clpfd(Max_int),
                 midoan_extensions:min_clpfd(Min_int),
                 ((ground(Le_const), \+ ground(Ri_const)) ->
                        (Le_const >= Max_int ->
                                (midoan_extensions__constrain_domain(Const_exp),
                                 Const_exp #= Max_int * Ri_const        %unsound ...
                                )
                        ;
                         Le_const =< Min_int ->         %Ri_const can only be 0 ,1
                                (midoan_extensions__constrain_domain(Const_exp),
                                 Const_exp #= Min_int * Ri_const        %unsound ...
                                )
                        ;
                                (midoan_extensions__constrain_domain(Const_exp),
                                 Const_exp #= Le_const * Ri_const       %unsound ...
                                )
                        )
                 ;
                  (\+ ground(Le_const), ground(Ri_const)) ->
                        (Ri_const >= Max_int ->
                                (midoan_extensions__constrain_domain(Const_exp),
                                 Const_exp #= Le_const * Max_int        %unsound ...
                                )
                        ;
                         Ri_const =< Min_int ->
                                (midoan_extensions__constrain_domain(Const_exp),
                                 Const_exp #= Le_const * Min_int        %unsound ...
                                )
                        ;
                                (midoan_extensions__constrain_domain(Const_exp),
                                 Const_exp #= Le_const * Ri_const       %unsound ...
                                )
                        )
                 ;
                  (ground(Le_const), ground(Ri_const)) ->
                        (Le_const == Max_int ->
                                (Ri_const == 0 ->
                                        (common_util:common_util__error(4, "NaN in multiplication due to integer overflow", no_error_consequences, [(le_const, Le_const), (ri_const, Ri_const)], 427030, midoan_extensions, midoan_extensions__infinity_binary_minus, no_localisation, no_extra_info),
                                         Type = unhandled_expression
                                        )
                                ;
                                 Ri_const < 0 ->
                                        Const_exp = Min_int
                                ;
                                 Ri_const > 0 ->
                                        Const_exp = Max_int
                                )
                        ;
                         Ri_const == Max_int ->
                                (Le_const == 0 ->
                                        (common_util:common_util__error(4, "NaN in multiplication due to integer overflow", no_error_consequences, [(le_const, Le_const), (ri_const, Ri_const)], 428630, midoan_extensions, midoan_extensions__infinity_binary_minus, no_localisation, no_extra_info),
                                         Type = unhandled_expression
                                        )
                                ;
                                 Le_const < 0 ->
                                        Const_exp = Min_int
                                ;
                                 Le_const > 0 ->
                                        Const_exp = Max_int
                                )
                        ;
                         Le_const == Min_int ->
                                (Ri_const == 0 ->
                                        (common_util:common_util__error(4, "NaN in multiplication due to integer overflow", no_error_consequences, [(le_const, Le_const), (ri_const, Ri_const)], 430230, midoan_extensions, midoan_extensions__infinity_binary_minus, no_localisation, no_extra_info),
                                         Type = unhandled_expression
                                        )
                                ;
                                 Ri_const < 0 ->
                                        Const_exp = Max_int
                                ;
                                 Ri_const > 0 ->
                                        Const_exp = Min_int
                                )
                        ;
                         Ri_const == Min_int ->
                                (Le_const == 0 ->
                                        (common_util:common_util__error(4, "NaN in multiplication due to integer overflow", no_error_consequences, [(le_const, Le_const), (ri_const, Ri_const)], 431830, midoan_extensions, midoan_extensions__infinity_binary_minus, no_localisation, no_extra_info),
                                         Type = unhandled_expression
                                        )
                                ;
                                 Le_const < 0 ->
                                        Const_exp = Max_int
                                ;
                                 Le_const > 0 ->
                                        Const_exp = Min_int
                                )
                        ;
                                (Tmp is Le_const * Ri_const,
                                 midoan_solver:midoan_solver__controlled_unification(Const_exp, Tmp)
                                )
                        )
                 )
                )
        ),
        (Type == unhandled_expression ->
                true
        ;
                Type = i        %that's the default in all successful cases
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_extensions__infinity_rem(X, Y, R, R_type) :-
	((ground(X), ground(Y)) ->
                (Remed is X rem Y,
                 midoan_solver:midoan_solver__controlled_unification(R, Remed),
                 R_type = 'i'
                )
        ;
                (cons_same_sign(X, R),   %constrain X and R to be of the same sign (always hold)
                 midoan_extensions__abs(R, R_abs),
                 midoan_extensions__abs(Y, Y_abs),
                 midoan_solver:midoan_solver__sdl(>(Y_abs, R_abs)),    %added 18/05/2010
	         midoan_extensions__div(X, Y, XDivY),
                 midoan_solver:midoan_solver__interpret(*(XDivY, Y), types(i, i), Inter_result, Inter_type, _Exception_inter),   %i.e. X - (XDivY*Y) #= R
                 (Inter_type == 'unhandled_expression' ->
                        R_type = 'unhandled_expression'
                 ;
                        midoan_solver:midoan_solver__interpret(-(X, Inter_result), types(i, Inter_type), R, R_type, _Exception_R)
                 )
                )
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%