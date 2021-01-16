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
% midoan_solver_labeling.pl
% defines the labeling module
% takes care of the labeling of variables in the solver's format
% various strategy could be implemented including the use of local search algorithms (Hill climbing, Tabu Search etc.)
%  see ECLiPSe sample programs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module('midoan_labeling', []).

%%%
:- use_module([	library('clpfd'),
		library('clpr'),
		library('random'),
		library('lists'),
		library('terms')
	     ]).

:- use_module('midoan_solver_main').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%IL is a list of integer variables or integers
midoan_labeling__integers(IL) :-
        %see clpfd doc, the problem with this is that labelling start at the minimum
        %of the var rather than the middle of the domain
        %we can write our own enumerator predicate
        %clpfd:labeling([ffc, bisect, all], IL).
        clpfd:labeling([ffc, value(midoan_labeling:enumerator), all], IL).

enumerator(Var, _Rest, BB0, BB) :- %unfinished see 3.9.1. documentation which is mileasding below is 3.8.6 version
        fd_min(Var, Min),
        fd_max(Var, Max),
        % Mid is the mid point of the domain of I
        % another way to chose Mid to avoid results which look the same is to choose a cutting point at random
        % random returns a float in [0, 1[
        random(N),
        Mid is integer((Max - Min) * N) + Min,
        (
                (Var #= Mid,
                 first_bound(BB0, BB)
                )
        ;
                (Var #< Mid,
                 later_bound(BB0, BB)
                )
        ;
                (Var #> Mid,
                 later_bound(BB0, BB)
                )
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%EL is a list of original enumeration variables or ground terms
midoan_labeling__enums([]).
midoan_labeling__enums([E|EL]) :-
	(midoan_enum:midoan_enum__ground(E) ->
                true
        ;
	        midoan_enum:midoan_enum__sample(E)            %take a sample, can be ressatisfied
	),
	midoan_labeling__enums(EL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%RL is a of real variables or reals
%19/11/07 : used to be very slow for large number of reals (e.g. 400), the version below is much faster than previously whilst still
%this is still not very good : hard to find the right ordering for the vars
%                              still slow
midoan_labeling__reals(RL) :-
        term_variables(RL, RL_nonground),
        dump(RL_nonground, New_list, Constraints),      %clpr predicate, this is time consuming so: do it for all vars, as here, but not for individual variables
        get_vars(Constraints, Var_list),                %obtain the list of lists of vars for each mentionned constraint
        count_occurences(RL_nonground, New_list, Var_list, Result),     %count the number of times the variables are mentionned
        sort(Result, Ordered),                          %order the vars in decreasing oder of occurences in the constraints
        labeling_reals2(Ordered).

get_vars([], []).
get_vars([Cons|Constraints], [List|Var_list]) :-
        term_variables(Cons, List),
        get_vars(Constraints, Var_list).

%result is of the form [(nb, var) .. (nb, var)]
count_occurences([], [], _, []).
count_occurences([V|RestV], [N|RestN], Var_list, [(Nb, V)|Next_result]) :-
        countV(Var_list, N, 0, Nb),
        count_occurences(RestV, RestN, Var_list, Next_result).

%how many times does C represented by N appears in Constrains?
countV([], _, Nb, Nb).
countV([Var_list|Rest], N, Nb_in, Nb_out) :-
        (match(Var_list, N) ->  %N appears in Var_list
                Next_Nb is Nb_in - 1       %for reverse order
        ;
                Next_Nb is Nb_in
        ),
        countV(Rest, N, Next_Nb, Nb_out).

match([], _) :-
        fail.
match([V|Rest], N) :-
        (N == V ->
                true
        ;
                match(Rest, N)
        ).

labeling_reals2([]).
labeling_reals2([(Nb, V)|Rest]) :-
        (ground(V) ->
                labeling_reals2(Rest)
        ;
                (%garbage_collect,
                 %statistics(global_stack, [GS_Used, _]),
                 %common_util__error(1, "", no_error_consequences, [(gs_used, GS_Used)], 112488, midoan_labeling, labeling_reals2, no_localisation, no_extra_info),
	         %trace,
                 select_r([(Nb, V)|Rest], R, Rest_vars),      %select R from RL_delayed
	         sample_real(R),
                 labeling_reals2(Rest_vars)
                )
        ).

select_r(RL, R, Rest_vars) :-
        (RL = [(_, R)] ->
                true
        ;
                (
                        RL = [(_, R)|Rest_vars]                 %pick the most constrained first
                ;
                        %select((_, R), RL, Rest_vars)                  %random select
                        (length(RL, Nb_reals),
                         random(0, Nb_reals, Position),
                         %format(user_error, "select_r: ~w~n", [Position]),
                         nth0(Position, RL, (_, R), Rest_vars)
                        )
                )
        ).

%called from midoan_labeling__reals/1 by sample_real(R)
%can fail
%R is the real variable to sample
%constrain R to a value or fail
%the sampling is not exhaustif i.e. a failure does not indicate an absence of solution
% (contrast with sample_integer/1)
%remark: bias towards 0.0
sample_real(R) :-
	inf(R, InfR),
	sup(R, SupR),
	SizeR is SupR - InfR,               %the size of the domaim of R
%same as for integers: to introduce a bit of randomness in the result generated
	random(N),      %a float between 0 and 1
	Ran is InfR + (SizeR/2) + N,	%new version June 2007, below +/- 5% on a very large number e.g. float 3.8E38 is way too big and may give a lot of silent overflows ...
        %Ran is InfR + (SizeR/2)*(0.95+N/10),    %changed end of Jan/Feb 2004
        %Ran is (SizeR/2 + InfR)*(0.95+N/20),   --problems as Mid then is not always>inf
        %Ran is SizeR * N + InfR
        Limit is 0.1,
	(
                sample_real_around_start(R, 1.0, 'mid', SizeR, Limit) %trying different samples around 1.0 (nice value for multiplication or division)
        ;
                sample_real_around_start(R, -1.0, 'mid', SizeR, Limit) %trying different samples around -1.0 (nice value for multiplication or division)
	;
                {R = Ran}
        ;
	        {R = InfR}
	;
	        {R = SupR}
	;
	        sample_real_around_start(R, Ran, 'mid', SizeR, Limit) %trying different samples around Random start
        ;
	        sample_real_around_start(R, InfR, 'inf', SizeR, Limit) %trying different samples around Inf
	;
	        sample_real_around_start(R, SupR, 'sup', SizeR, Limit) %trying different samples around Sup
        ).

%called from sample_real/1 by sample_real_around_start(R, Start, Type, SizeR, Limit)
%is recursif; can fail; choice points
%R is the real variable to sample
%Start is the staring point for the sampling
%Type is either inf, mid or sup to indicate whether Start is the inf, mid or sup of the domain of R
%SizeR is the size of the domain of R
%the last argument, Limit, is a float which is increased after each recursion set at 10 percent of the size of the domain
% it is used to stop the recursion as well as to increase the gap between the start point
% and the sample value generated after each recursion
sample_real_around_start(R, Start, Type, SizeR, Limit) :-
        (Limit > SizeR/2 -> %arbritrary stop of the recursion: the limit has been reached
                (!,
	         fail
	        )
        ;
	        (random(1, 100000, N1),
	         random(1, 100000, N2),
                 %generate a standard deviation between -1 and +1
	         (N1 > N2 ->
		        Deviation is N2/N1
	         ;
	                Deviation is -N1/N2
	         ),
                 %generate a sample value around the start point, with limit increasing after each recursive call
                 (Type == 'inf' ->
                        Sample is Limit*abs(Deviation) + Start      %Start = Inf
                 ;
                  Type == 'mid' ->
        	        Sample is Limit*Deviation + Start           %Start = Mid
	         ;
	          Type == 'sup' ->                                      %Start = Sup
	                Sample is Start - Limit*abs(Deviation)
	         ),
	         (
                        {R = Sample}			%try the sample
	         ;					% choice point
	                (Next_limit is Limit*2,		%increase the limit: 0.1 0.2 0.4 0.8 Stop
		         sample_real_around_start(R, Start, Type, SizeR, Next_limit)      %try again
	                )
	         )
	        )
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%