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
% midoan_solver_extensions.pl
% module for user defined arithmetic constraints
:-module(midoan_extensions, [   midoan_extensions__round/2,
                                midoan_extensions__abs/2,
                                midoan_extensions__div/3,
                                midoan_extensions__mod/3,
                                midoan_extensions__pow/3,
                                midoan_extensions__min/3,
                                midoan_extensions__max/3,
                                midoan_extensions__constrain_domain/1,
                                midoan_extensions__infinity_unary_minus/2,
                                midoan_extensions__infinity_binary_minus/4,
                                midoan_extensions__infinity_binary_plus/4,
                                midoan_extensions__infinity_multiply/4,
                                midoan_extensions__infinity_greater/2,
                                midoan_extensions__infinity_greater_or_equal/2,
                                midoan_extensions__infinity_less/2,
                                midoan_extensions__infinity_less_or_equal/2,
                                midoan_extensions__infinity_rem/4
                            ]
        ).

:- use_module([library(lists), library(clpfd), library(clpr)]).

:- compile([midoan_solver_extensions__infinity_arithmetic]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%notes on clpfd limimitations : updated 25/06/2010
%we use max_clpfd(Max) and min_clpfd(Min) to denote positive and negative infinity respectively
%we also handle larger integer (and smaller) arithmetic
%this can of course lead to unsoundness ... but hopefully no overflow
%also especially in the 'infinity versions' unsoundness may arise later in many, many places if a var is unifified with a very large (or very small) integer
%most of our arithmetic constraints are unsound with very large integers
%%%
%unsafe settings
max_clpfd(33554431).
min_clpfd(-33554432).           %used for individual clpfd variable
%safe settings : max_clpfd(16777215) and min_clpfd(-16777216).
%max_clpfd(16777215).
%min_clpfd(-16777216).

max_clpfd_expression(33554431). %only used for standard domain of integer variables
min_clpfd_expression(-33554432).

midoan_extensions__constrain_domain(V) :-
        max_clpfd_expression(Max_clpfd_exp),
        min_clpfd_expression(Min_clpfd_exp),
        V in Min_clpfd_exp .. Max_clpfd_exp.

%constraints defined in this module
:- multifile clpfd:dispatch_global/4.
clpfd:dispatch_global(round_blocked(X, R), state(X, R), state(_, _), Actions) :-
        Actions = [call(midoan_extensions:round_blocked(X, R))].
clpfd:dispatch_global(abs_blocked(X, R), state(X, R), state(_, _), Actions) :-
        Actions = [call(midoan_extensions:abs_blocked(X, R))].
clpfd:dispatch_global(div_blocked(X, Y, R), state(X, Y, R), state(_, _, _), Actions) :-
        Actions = [call(midoan_extensions:div_blocked(X, Y, R))].
clpfd:dispatch_global(mod_blocked(X, Y, R), state(X, Y, R), state(_, _, _), Actions) :-
        Actions = [call(midoan_extensions:mod_blocked(X, Y, R))].
clpfd:dispatch_global(pow_blocked(X, Y, R), state(X, Y, R), state(_, _, _), Actions) :-
        Actions = [call(midoan_extensions:pow_blocked(X, Y, R))].
clpfd:dispatch_global(cons_same_sign_blocked(X, Y), state(X, Y), state(_, _), Actions) :-
        Actions = [call(midoan_extensions:cons_same_sign_blocked(X, Y))].
clpfd:dispatch_global(min_blocked(X, Y, R), state(X, Y, R), state(_, _, _), Actions) :-
        Actions = [call(midoan_extensions:min_blocked(X, Y, R))].
clpfd:dispatch_global(max_blocked(X, Y, R), state(X, Y, R), state(_, _, _), Actions) :-
        Actions = [call(midoan_extensions:max_blocked(X, Y, R))].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%updated June 2010
midoan_extensions__round(Real, Integer) :-
        (round_ground(Real, Integer) ->         %deals with the case where X or Integer is ground
                true                            %we have finished
        ;
                (% not Real nor Integer are ground
		 midoan_extensions__constrain_domain(Integer),
                 % freeze on the real Real in case it becomes ground, or on the integer Integer to release the constraint round_ground placed on Real
		 when((ground(Real); ground(Integer)), round_ground(Real, Integer)),
                 fd_global(round_blocked(Real, Integer), state(Real, Integer), [dom(Integer)])       %delays on the integer only at this level
                )
	).
%%%
        round_ground(Real, Integer) :-
                (ground(Real) ->
                        ada_round(Real, Integer)                        %yields a ground integer
                ;
                 ground(Integer) ->
                        ada_round_inverse(Integer, Integer, Real)       %yields a real interval
                ).
%%%
        %a delay occurs
        round_blocked(X, R) :-
                (round_ground(X, R) ->          %nothing else to do the blocked call disapears
                        true
                ;                               %nor X nor R are ground
                        (%updating the real X according to the integer R
                         util_fd_min(R, MinR),
                         util_fd_max(R, MaxR),
                         ada_round_inverse(MinR, MaxR, X),
                         %updating the integer R according to the real X
                         inf(X, Inf),
                         ada_round(Inf, Inf_round),
                         ((0.5 is float_fractional_part(abs(Inf)), %decimal part is 0.5 or -0.5
                           Inf < 0,                     %Inf is negative
                           entailed(X=\= Inf)           %Inf is not taken
                          ) ->
                                Min is Inf_round + 1    %special case when Inf in {-0.5, -1.5, -2.5 ...} and X \= Inf
                          ;
                                Min is Inf_round
                         ),
                         sup(X, Sup),
                         ada_round(Sup, Sup_round),
                         ((0.5 is float_fractional_part(abs(Inf)), %decimal part is 0.5 or -0.5
                           Sup > 0,                     %Sup is positive
                           entailed(X =\= Sup)          %Sup is not taken
                          ) ->
                                Max is Sup_round - 1    %special case when Sup in {0.5, 1.5, 2.5 ...} and X \= Sup
                         ;
                                Max is Sup_round
                         ),
                         midoan_solver:midoan_solver__sdl(>=(R, Min)),
                         midoan_solver:midoan_solver__sdl(<=(R, Max))
                        )
                ).
%%%
        %calculate round(X) = R according to Ada95 semantics whenever X is a ground real
        ada_round(Real, Integer) :-
                FixReal is integer(Real),
                (abs(FixReal - Real) >= 0.5 ->
                        (Rounded is integer(FixReal + sign(Real)),
                         midoan_solver:midoan_solver__controlled_unification(Integer, Rounded)               %round away from 0 when decimal part >= 0.5
                        )
                ;
                        midoan_solver:midoan_solver__controlled_unification(Integer, FixReal)
                ).
%%%
        %constrain X to an interval according to MinR and MaxR
        %when MinR = MaxR = R then behaves as the inverse of round
        ada_round_inverse(MinInteger, MaxInteger, Real) :-
                (MinInteger > 0 ->
                        {Real >= MinInteger - 0.5}
                ;
                        {Real > MinInteger - 0.5}          %when MinR <= 0
                ),
                (MaxInteger < 0 ->
                        {Real =< MaxInteger + 0.5}
                ;
                        {Real < MaxInteger + 0.5}          %when MinR >= 0
                ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%updated June 2010
midoan_extensions__abs(X, R) :-
	(abs_ground(X, R) ->            %deals with the case X or R are ground
	        true                    %we have finished
        ;
                (% not X nor R are ground
                 midoan_extensions__constrain_domain(X),
                 midoan_extensions__constrain_domain(R),
                 midoan_solver:midoan_solver__sdl(>=(R, 0)),
                 fd_global(abs_blocked(X, R), state(X, R), [dom(X), dom(R)]) %delays
                )
        ).
%%%
abs_ground(X, R):-
	(ground(X) ->   %but R may, or may not be an fd_var
                (Absed is abs(X),
                 midoan_solver:midoan_solver__controlled_unification(R, Absed)             %uses prolog abs, R becomes ground
                )
	;
	 ground(R) ->                   %X is either -R or R, nonlinear domain
	        (NegX is -R,
	         list_to_fdset([NegX, R], X_set),
	         X in_set X_set
	        )
        ).
%%%
%a delay occurs
abs_blocked(X, R) :-
        (abs_ground(X, R) ->    %nothing else to do the blocked call disapears
                true
        ;                        %nor X nor R are ground
	    (util_fd_min(X, MinX),
	     util_fd_max(X, MaxX),
	     (MinX >= 0 ->       %X is positive
		 R #= X
	     ;
	      MaxX =< 0 ->       %X is negative
	         R #= X*(1-2)   %trick
	     ;                   %the domain of X crosses 0 (nor positive, nor negative)
	         (%updating R according to X
		  Max_abs_X is max(-MinX, MaxX),      %the maximum absolute value of X
                  midoan_solver:midoan_solver__sdl(<=(R, Max_abs_X)), %i.e. R is positif and <= Max_abs_X
		  %updating X according to R
	          util_fd_min(R, MinR),
	          util_fd_max(R, MaxR),
		  %2 tricks, should be equivalent to X::[-MaxR..-MinR, MinR..MaxR] in all cases
		  Neg_minR is -MaxR,
		  (MinR = 0 ->                          %MinR = 0 causes overlapping which generates an error
		        (fdset_interval(X_set2, Neg_minR, MaxR),
	                 X in_set X_set2
		        )
		  ;
		        (Neg_maxR is -MinR,
		         fdset_interval(X_set3, Neg_minR, Neg_maxR),
		         fdset_interval(X_set4, MinR, MaxR),
		         fdset_union(X_set3, X_set4, X_set5),
	                 X in_set X_set5                  %nonlinear domain
		        )
		  )
	         )
	     )
	    )
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%updated June 2010
midoan_extensions__min(X, Y, R) :-
        (min_ground(X, Y, R) ->
	        true                    %we have finished
        ;
                fd_global(min_blocked(X, Y, R), state(X, Y, R), [dom(X), dom(Y), dom(R)])       %delays
	).
min_ground(X, Y, R) :-
        ((ground(X), ground(Y)) ->    %covers the case when X, Y and R are ground
                (midoan_solver:midoan_solver__sdl(>(X, Y)) ->
                        midoan_solver:midoan_solver__controlled_unification(R, Y)
                ;
                        midoan_solver:midoan_solver__controlled_unification(R, X)
                )
        ;
                 (X #> Y #<=> B,        %reification (except it is not very powerful e.g. X #> Y, X #> Y #<=> B. returns  B unknown)
                  (B == 1 ->
                        R #= Y
                  ;
                   B == 0 ->
                        R #= X
                  ;
                        (%seeing if X and Y's domain do not overlap (is this useful ever?)
                         fd_set(X, X_set),
                         fd_set(Y, Y_set),
                         (fdset_disjoint(X_set, Y_set) ->       %they do not overlap we can conclude
                                (midoan_solver:midoan_solver__sdl(<(X, Y)) ->
                                        R #= X
                                ;
                                        R #= Y
                                )
                         )      %otherwise fails
                        )
                  )
                 )
        ).

%a delays occurs
min_blocked(X, Y, R) :-
        (min_ground(X, Y, R) ->
	        true                    %we have finished: the constraint will be automatically removed when X and Y will be ground
	;
	        constrain_subset(X, Y, R)
        ).

constrain_subset(X, Y, R) :-
        fd_set(X, X_set),
	fd_set(Y, Y_set),
        fd_set(R, R_set),
        fdset_union(X_set, Y_set, XY_union),
        fdset_intersection(XY_union, R_set, New_R_set),
        R in_set New_R_set.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%updated June 2010
midoan_extensions__max(X, Y, R) :-
        (max_ground(X, Y, R) ->
	        true                    %we have finished
        ;
                fd_global(max_blocked(X, Y, R), state(X, Y, R), [dom(X), dom(Y), dom(R)])       %delays
	).
max_ground(X, Y, R) :-
        ((ground(X), ground(Y)) ->    %covers the case when X, Y and R are ground
                (midoan_solver:midoan_solver__sdl(>(X, Y)) ->
                        midoan_solver:midoan_solver__controlled_unification(R, X)
                ;
                        midoan_solver:midoan_solver__controlled_unification(R, Y)
                )
        ;
                (X #> Y #<=> B,        %reification (except it is not very powerful e.g. X #> Y, X #> Y #<=> B. returns  B unknown)
                 (B == 1 ->
                        R #= X
                 ;
                  B == 0 ->
                        R #= Y
                 ;
                        (%seeing if X and Y's domain do not overlap
	                 fd_set(X, X_set),
	                 fd_set(Y, Y_set),
                         (fdset_disjoint(X_set, Y_set) ->       %they do not overlap we can conclude
                          (midoan_solver:midoan_solver__sdl(<(X, Y)) ->
                                R #= Y
                          ;
                                R #= X
                          )
                         )
                        )
                 )
                )
        ).

%a delays occurs
max_blocked(X, Y, R) :-
        (max_ground(X, Y, R) ->
	        true                    %we have finished: the constraint will be automatically removed when X and Y will be ground
	;
	        constrain_subset(X, Y, R)
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   DIV/3   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%clpfd:dispatch_global(div_blocked(X, Y, R), state(X, Y, R), state(_, _, _), Actions) :-
%        Actions = [call(midoan_extensions:div_blocked(X, Y, R))].
%user defined integer div constraint, remark: X div Y = truncate(X / Y)
%called from arithmetic/3 by div(X, Y, R)
%X, Y and R are integer expressions
midoan_extensions__div(X, Y, R) :-
	(ground(Y) ->
                (Y == 0 ->
                        common_util:common_util__error(10, "Division by a ground 0", "Should never happen", [('x', X)], 1024904, 'midoan_extensions', 'midoan_extensions__div', 'no_localisation', 'no_extra_info')
                ;
                        true
                )
        ;
                midoan_extensions__infinity_different(Y, 0)                    %Y \= 0 always hold
        ),
	(div_ground(X, Y, R) ->
	        true                    %we have finished
        ;
                fd_global(div_blocked(X, Y, R), state(X, Y, R), [dom(X), dom(Y), dom(R)])       %delays
	).

div_ground(X, Y, R) :-
        ((ground(X), ground(Y)) ->    %covers the case when X, Y and R are ground
	        (Dived is X // Y,
                 midoan_solver:midoan_solver__controlled_unification(R, Dived)
                )
	;
	 X == 0 ->                    %special case, 0 div Y = 0 always hold
	        midoan_solver:midoan_solver__controlled_unification(R, 0)                   %Y is free
	;
	 R == 0 ->                    %special case, abs(Y) > abs(X) holds
	    (midoan_extensions__abs(Y, AbsY),   %as we are not exactly interested in the absolute value of Y
	     midoan_extensions__abs(X, AbsX),    %idem
             midoan_extensions__infinity_greater(AbsY, AbsX)
	    )
	;
	 R == 1 ->                    %special case, X div X = 1 always hold
	    X #= Y
	;
	 R == -1 ->                   %special case, X div -X = -1 always hold
	    X #= Y*(1-2)
	;
	 Y == 1 ->                    %special case, X div 1 = X always hold
	    X #= R
	;
	 Y == -1 ->                   %special case, X div -1 = -X always hold
	    X #= R*(1-2)
        ).

%a delays occurs
div_blocked(X, Y, R) :-
        (div_ground(X, Y, R) ->
	        true                    %we have finished
	;
	    ((ground(X) ->
                true
             ;
                (%calculating the bounds of X according to Y and R
	         util_fd_min(Y, MinY),
	         util_fd_max(Y, MaxY),
	         util_fd_min(R, MinR),
	         util_fd_max(R, MaxR),
	         X1 is MinY*(MinR + sign(MinR)) - sign(MinY)*sign(MinR), %X bounds similar to Y*R
	         X11 is MinY*MinR,
	         X2 is MinY*(MaxR + sign(MaxR)) - sign(MinY)*sign(MaxR),
	         X22 is MinY*MaxR,
	         X3 is MaxY*(MinR + sign(MinR)) - sign(MaxY)*sign(MinR),
	         X33 is MaxY*MinR,
	         X4 is MaxY*(MaxR + sign(MaxR)) - sign(MaxY)*sign(MaxR),
	         X44 is MaxY*MaxR,
	         mum([X1, X11, X2, X22, X3, X33, X4, X44], >, _, New_maxX),
	         mum([X1, X11, X2, X22, X3, X33, X4, X44], <, _, New_minX),
	         constrain(New_minX, New_maxX, X)
                )
             ),
	     (ground(R) ->
                true
             ;
                (%calculate the bounds of R according to X and Y
                 util_fd_min(X, MinXLatest),
	         util_fd_max(X, MaxXLatest),
                 util_fd_min(Y, MinYLatest),
	         util_fd_max(Y, MaxYLatest),
	         bounds_XdivY(Y, MinXLatest, MaxXLatest, MinYLatest, MaxYLatest, New_minR, New_maxR),       %19/01/09 MinY nor MaxY will not be 0
	         constrain(New_minR, New_maxR, R)
                )
             ),
	     (ground(Y) ->
                true
             ;
                (%calculate the bounds of Y according to X and R
	         fd_set(R, SetR),
	         (fdset_member(0, SetR) ->
		        true                  %Y is nearly free : cannot be further constrained
	         ;
	                (util_fd_min(R, MinRLatest),
	                 util_fd_max(R, MaxRLatest),
                         util_fd_min(X, MinXLatest2),
	                 util_fd_max(X, MaxXLatest2),
                         bounds_XdivY(R, MinXLatest2, MaxXLatest2, MinRLatest, MaxRLatest, New_minY, New_maxY), %19/01/09 MinRLatest nor MaxRLatest will not be 0
		         constrain(New_minY, New_maxY, Y)
	                )
	         )
	        )
             )
            )
	).

constrain(New_min, New_max, Var) :-
        max_clpfd(Max_clpfd),
        min_clpfd(Min_clpfd),
        (New_min > Min_clpfd ->
                Var #>= New_min
        ;
	        Var #>= Min_clpfd                 %to avoid overflow
        ),
	(New_max < Max_clpfd ->
	        Var #=< New_max
        ;
	        Var #=< Max_clpfd                %to avoid overflow
        ).

%called from div_blocked/3 by bounds_XdivY(Y, MinX, MaxX, MinY, MaxY, New_minR, New_maxR) etc
%Y is an integer variable
%MinX, MaxX, MinY, MaxY are the minimum and maximum of X and Y respectively
%New_minR, New_maxR are the out bounds of X div Y
%calculate the bound of X div Y
bounds_XdivY(Y, MinX, MaxX, MinY, MaxY, New_minR, New_maxR) :-
	dom_near_0(Y, Max_negY, Min_posY), %obtain the closest numbers around 0
	(ground(Min_posY) ->
	    (R1 is MaxX // Min_posY,
	     R11 is MinX // Min_posY
	    )
	;
	    (R1 = nc,                     %nc indicates that the bound are unknown
	     R11 = nc
	    )
	),
	(ground(Max_negY) ->
	    (R2 is MinX // Max_negY,
	     R12 is MaxX // Max_negY
	    )
	;
	    (R2 = nc,
	     R12 = nc
	    )
	),
	R3 is MaxX // MinY,
	R4 is MinX // MaxY,
	mum([R1, R2, R3, R4], >, _, New_maxR),          %the maximum of the list
	mum([R11, R12, R3, R4], <, _, New_minR).        %the minimum of the list

%called by div_blocked/3 and bound_XdivY/7
%Op is either '>' or '<'
%the second argument is a list of numbers or 'nc' terms
%the third argument is the result so far
%the fourth argument is the out maximum (if Op is '>') or minimum (if Op is '<') of the list of numbers
mum([], _, M, M).
mum([I|IL], Op, M, Res) :-
	((ground(M), M \= nc) ->
	   (Op = > ->
	       ((I \= nc, I > M) ->
		   mum(IL, Op, I, Res)
	       ;
	           mum(IL, Op, M, Res)
	       )
	   ;
	    Op = < ->
	       ((I \= nc, I < M) ->
		   mum(IL, Op, I, Res)
	       ;
	           mum(IL, Op, M, Res)
	       )
	   )
        ;
	   mum(IL, Op, I, Res)
       ).

%called from bounds_XdivY/7 by dom_near_0(Y, Max_negY, Min_posY)
%Var is an integer variable
%Max_neg is the maximum negative of Var
%Min_pos is the minimum positive of Var
dom_near_0(Var, Max_neg, Min_pos) :-
	fd_set(Var, Set),
	V2 in_set Set,
	V3 in_set Set,
	(V2 #> 0 ->                     %V2 is similar to Var but positif
                util_fd_min(V2, Min_pos)
	;
	        true
	),
	(V3 #< 0 ->                     %V3 is similar to Var but negatif
                util_fd_max(V3, Max_neg)
	;
	        true
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_extensions__mod(X, Y, R) :-      %cannot use Prolog's mod see Barnes for weird behaviour with negative arguments
	cons_same_sign(Y, R),  %constrain Y and R to be of the same sign (always hold)
	((extract_sign(X, SignX), extract_sign(Y, SignY)) ->
	        mod_equivalent(X, SignX, Y, SignY, R)
        ;
                (midoan_extensions__constrain_domain(R),
                 fd_global(mod_blocked(X, Y, R), state(X, Y, R), [dom(X), dom(Y), dom(R)])   %delays
                )
        ).
%%%
        %we know an equivalent constraint expressed in terms of 'rem'
        mod_equivalent(X, SignX, Y, SignY, R) :-
                (SignX == SignY ->                      %X and Y are of the same sign
                        midoan_extensions__infinity_rem(X, Y, R, _Type)    %X mod Y = X rem Y
                    ;
                        (midoan_extensions__infinity_rem(X, Y, R1, _Type),  %X mod Y = X rem Y + Y
                         (ground(R1) ->
                                (R1 == 0 ->
                                        R #= 0
                                ;
                                        R #= R1 + Y    %X and Y are of opposite sign
                                )
                         ;
                                true    %nothing happens the constraint is still alive
                         )
                        )
                ).
%%%
        %a delays occurs
        mod_blocked(X, Y, R) :-
                ((extract_sign(X, SignX), extract_sign(Y, SignY)) ->
                        mod_equivalent(X, SignX, Y, SignY, R)
                ;
                        true            %nothing to be done:the constraint is still alive
                ).
%%%
%clpfd:dispatch_global(cons_same_sign_blocked(X, Y), state(X, Y), state(_, _), Actions) :-
%       Actions = [call(midoan_extensions:cons_same_sign_blocked(X, Y))].
%called from mod/3 by cons_same_sign(Yeval, Reval) and from rem/3 by cons_same_sign(X, R)
%X and Y are integer variables
%constrain X and Y to be of the same sign
cons_same_sign(X, Y) :-
        (cons_same_sign_ground(X, Y) ->
                true                    %we have finished
        ;
                fd_global(cons_same_sign_blocked(X, Y), state(X, Y), [dom(X), dom(Y)])  %delays
        ).

cons_same_sign_ground(X, Y) :-
	(extract_sign(X, SignX) ->      %the sign of X is known
	    (SignX = 1 ->
		Y #>= 0
	    ;
	     SignX = -1 ->
	        Y #=< 0
	    )
	;                               %the sign of X is unknown
	extract_sign(Y, SignY) ->       %the sign of Y is known
	    (SignY = 1 ->
		X #>= 0
	    ;
	     SignY = -1 ->
	        X #=< 0
	    )
        ).

%the sign of X and the sign of Y are both unknown
cons_same_sign_blocked(X, Y) :-
        (cons_same_sign_ground(X, Y) ->
                true            %nothing to be done:the constraint will be automatically when X and Y will be ground
        ;
                true            %nothing to be done:the constraint is still alive
        ).

%%%
%called from mod_ground/3 twice and from cons_same_sign_ground/2 twice
%SignX is the out sign of the integer variable X, 1 denote that X is positive, -1 negative
%fail when the sign is unknown
extract_sign(X, SignX) :-
	util_fd_min(X, MinX),
	util_fd_max(X, MaxX),
	(MinX >= 0 ->
	    SignX = 1
	;
	 MaxX =< 0 ->
	    SignX = -1
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   POW/3   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%X, Y and R are integer expressions
%user defined power operator
midoan_extensions__pow(X, Y, R) :-
        (pow_ground(X, Y, R) ->
                true
        ;
                (Y #>= 0,        %added 29/10/09
                 fd_global(pow_blocked(X, Y, R), state(X, Y, R), [dom(X), dom(Y), dom(R)])   %delays
                )
	).

pow_ground(X, Y, R) :-
	(Y == 0 ->
		R = 1                   %X is free
	;
         X == 1 ->
	        R #= 1          %Y is free
	;
	 X == 0 ->
	        R #= 0          %Y is free
	;
	 X == -1 ->
	        (R = 1 ->
		    util_odd(Y)          %Y must be odd
		;
		 R = -1 ->
		    util_even(Y)         %Y must be even
		)
	;
	 Y == 1 ->
	        R #= X
        ;
	 R == 0 ->
	        X = 0                    %Y is free
        ;
	 R == 1 ->
	        X #= 1                  %Y is free
	;
        (ground(X), ground(Y)) ->
	        R is integer(X**Y)
	;
	(ground(Y), ground(R)) ->
	        (Tmp is sign(R)*abs(R)**float(1/Y),     %weird but leave it like that
		 util_real_is_integer(Tmp),             %Tmp must be an integer
		 Tmp_int is integer(Tmp),
		 (util_even(Y) ->
		        (TmpMin is Tmp_int*(1-2),  %weird but leave it like that (compilation error if -1*Y for example)
                         list_to_fdset([TmpMin, Tmp_int], X_set),
                         X in_set X_set
		        )
		 ;
		         X = Tmp_int
		 )
	        )
	;
	(ground(X), ground(R)) ->
	        (Tmp is log(abs(R))/log(abs(X)),
		 util_real_is_integer(Tmp),   %Tmp must be an integer
		 Y is integer(Tmp)
	        )
        ).

pow_blocked(X, Y, R) :-
        max_clpfd(Max_clpfd),
        min_clpfd(Min_clpfd),
        (pow_ground(X, Y, R) ->
                true    %nothing else to do the blocked call disapears
        ;
	 ground(X) ->
	        (X = -1 ->
                        (list_to_fdset([-1, 1], R_set),
	                 R in_set R_set
	                )
                ;
	         X > 0 ->
	                R #> 0
                ;
	                true    %the constraint remains
	        )
	;
	 ground(Y) ->
                (Y == 0 ->
		        (list_to_fdset([0, 1], R_set),
		         R in_set R_set
		        )
	        ;
	         util_even(Y) ->                % Y is even, constrain R to be positive
	                (R #>= 0,               % for efficiency only
		         %updating R according to X and Y
		         util_fd_min(X, MinX),
		         util_fd_max(X, MaxX),
		         fd_set(X, SetX),
		         (fdset_member(0, SetX) ->
		                New_MinR = 0
		         ;
		          MinX > 0 ->
		                New_MinR is min(integer(MinX**Y), Max_clpfd)
		         ;
		          MaxX < 0 ->
		                New_MinR is min(integer(-MaxX**Y), Max_clpfd)
		         ;
	                        (dom_near_0(X, Max_neg, Min_pos),
		                 New_MinR is min(integer(min(-Max_neg, Min_pos)**Y), Max_clpfd)
		                )
		         ),
		         New_MaxR is min(integer(max(abs(MinX), abs(MaxX))**Y), Max_clpfd),
		         %we want to preserve the bounds of R when they indicate unboundness
		         util_fd_max(R, MaxRcheck),
		         R #>= New_MinR,

		         (MaxRcheck = Max_clpfd ->
		                true
		         ;
		                R #=< New_MaxR
		         ),
		         %updating X according to R and Y
		         util_fd_min(R, MinR),
		         util_fd_max(R, MaxR),
                         (MaxR = Max_clpfd ->
		                Pos_max_X2 = Max_clpfd
		         ;
		                (N2 is sign(MaxR)*abs(MaxR)**float(1/Y),
		                 rg_max(N2, Pos_max_X2)
		                )
		         ),
		         N1 is sign(MinR)*abs(MinR)**float(1/Y),
		         rg_min(N1, Pos_min_X2),
		         Neg_min_X2 is -Pos_max_X2,
		         Neg_max_X2 is -Pos_min_X2,
		         (Pos_min_X2 = 0 ->
		                (fdset_interval(X_set, Neg_min_X2, Pos_max_X2),
		                 X in_set X_set
		                )
		         ;
	                        (fdset_interval(X_set1, Neg_min_X2, Neg_max_X2),
	                         fdset_interval(X_set2, Pos_min_X2, Pos_max_X2),
	                         fdset_union(X_set1, X_set2, X_set3),
	                         X in_set X_set3
	                        )
		         )
		        )
                ;             %Y is odd
	                (%updating R according to X and Y
		         util_fd_min(X, MinX),
		         util_fd_max(X, MaxX),
		         util_fd_min(R, MinRcheck),
		         util_fd_max(R, MaxRcheck),
		         (MinRcheck = Min_clpfd ->
		                New_MinR = Min_clpfd
		         ;
		                New_MinR is min(integer(abs(MinX)**Y), Max_clpfd)*sign(MinX)
		         ),
		         (MaxRcheck = Max_clpfd ->
		                New_MaxR = Max_clpfd
		         ;
		                New_MaxR is min(integer(abs(MaxX)**Y), Max_clpfd)*sign(MaxX)
		         ),
		         fdset_interval(R_set, New_MinR, New_MaxR),
		         R in_set R_set,
		         util_fd_min(R, MinR),
		         util_fd_max(R, MaxR),
		         (MinR =  Min_clpfd ->
		                New_minX = Min_clpfd
		         ;
		                (N3 is sign(MinR)*abs(MinR)**float(1/Y),
		                 rg_min(N3, New_minX)
		                )
		         ),
		         (MaxR = Max_clpfd ->
		                New_maxX = Max_clpfd
		         ;
		                (N4 is sign(MaxR)*abs(MaxR)**float(1/Y),
		                 rg_max(N4, New_maxX)
		                )
		         ),
		         fdset_interval(X_set, New_minX, New_maxX),
		         X in_set X_set
	                )
	        )
	;
	 ground(R) ->
                (R = -1 ->
		        (list_to_fdset([-1, 1], X_set),
		         X in_set X_set
		        )
	        ;
	         R < 0 ->
	                (X #< 0,
	                 util_odd(Y)
	                )
	        ;
	         true
                )
	;       %nor X nor Y nor R are ground
	        (util_fd_min(X, MinX),
	                (MinX > 0 ->               %X is positif
		                R #> 0
	                ;
	                        true
	                ),
		 util_fd_max(R, MaxR),
	                (MaxR < 0 ->              %R is negatif
		                (X #< 0,
		                 util_odd(Y)
	                        )
	                ;
	                        true
	                )
	        )
	).

%
rg_min(X, R) :-
	(util_real_is_integer(X) ->
	    R is integer(X)
	;
	 X >= 0 ->
	    R is integer(X)+1
	;
	    R is integer(X)
	).

%
rg_max(X, R) :-
	(util_real_is_integer(X) ->
	    R is integer(X)
	;
	 X >= 0 ->
	    R is integer(X)
	;
	    R is integer(X)-1
	).

%%%
%avoids to deal with inf [08/11/07: these should not ne needed any more since we constrain every fd variable to be in a safe range]
util_fd_min(X, Min) :-
        fd_min(X, MinX),
        (MinX = inf ->
                (min_clpfd(Min_clpfd),
                 Min = Min_clpfd
                )
        ;
                Min = MinX
        ).

%%%
%avoids to deal with sup
util_fd_max(X, Max) :-
        fd_max(X, MaxX),
        (MaxX = sup ->
                (max_clpfd(Max_clpfd),
                 Max = Max_clpfd
                )
        ;
                Max = MaxX
        ).

%%%
% does a real represent an integer? [precisions problems?]
util_real_is_integer(X) :-
	0.0 is integer(X) - X.

%%%
util_even(N) :-
	0 is N mod 2.

util_odd(N) :-
	1 is N mod 2.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%