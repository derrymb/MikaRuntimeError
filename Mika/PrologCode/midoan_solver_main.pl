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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% midoan_solver_main.pl
% define the midoan_solver module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%This module is a little bit messy. Its function is to provide an interface for the solver.
%It could be simplified via syntactic rationalisation of the solver per se.
%%%
:- module(midoan_solver, [      midoan_solver__interpret/5,
				midoan_solver__sdl/1,
                                midoan_solver__arithmetic/3,
                                midoan_solver__label_reals/1,
                                midoan_solver__real_min/3,
                                midoan_solver__real_max/3,
                                midoan_solver__integer_range/3,
                                midoan_solver__modular_integer_range/3,
                                midoan_solver__create_derived_type/3,
				midoan_solver__get_type_from_value/2,
                                midoan_solver__controlled_unification/2,
                                midoan_solver__inequality_constraint/4,
                                find_lowest_largest/3
                         ]
         ).

%%%
:- use_module([library(clpfd), library(clpr), library(lists), library(random)]).

:- compile([midoan_solver_main__engine,
            midoan_solver_main__controlled_unification,
            midoan_solver_main__inequality]).

:- use_module([midoan_solver_anon_aggregate, midoan_solver_array, midoan_solver_enum, midoan_solver_modular_integer, midoan_solver_record, midoan_solver_string_literal, midoan_solver_type]).

:- use_module(midoan_solver_extensions, [       midoan_extensions__round/2,
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

:- use_module(midoan_solver_labeling).

:- use_module(common_util, [    common_util__error/9
                           ]
             ).
%%%
%quick and dirty: sometimes necessary because array and record elements are not SEAV variables their types must be infered
%  from their actual value
midoan_solver__get_type_from_value(Value, Type) :-
        (midoan_enum:midoan_enum__is_enum(Value) ->
                Type = 'base_enumeration'         %may be ground or not
        ;
         midoan_modular_integer:midoan_modular_integer__is_modular_integer(Value) ->
                Type = 'modular_integer'          %may be ground or not
        ;
         ground(Value) ->	%will only catch ground integers and floats (which is what we want: see where it is used)
                Type = 'ground'
        ;
         midoan_solver__is_integer(Value) ->
                Type = 'standard.ads:integer'   %an integer var
        ;
         midoan_array:midoan_array__is_array(Value) ->
                Type = array(_)
        ;
         midoan_record:midoan_record__is_record(Value) ->
                Type = 'record'
        ;
         midoan_solver__is_float(Value) ->
                Type = 'standard.ads:float'     %a float var
        ;
         midoan_anon_aggregate:midoan_anon_aggregate__is_anon_aggregate(Value) ->
                Type = 'anonymous_aggregate'
        ;
         midoan_string_literal:midoan_string_literal__is_string_literal(Value) ->
                Type = 'string_literal'
        ;
         midoan_type:midoan_type__is_type(Value) ->
                Type = type
        ;
                Type = 'free'
        ).
%%%
midoan_solver__label_reals(RL) :-
        midoan_labeling:midoan_labeling__reals(RL).
%%%
%the next two predicates are really ugly
%they might not work all the time, in particular we must ensure that all clpr we manipulate
% have been imposed a linear constraint

%ensure fd variable by Var #= Var*1
midoan_solver__is_integer(Var) :-
	var(Var),
        clpfd:get_atts(Var, fd_attribute(_, _, _)).

%massive hack
midoan_solver__is_float(Var) :-
	var(Var),
        (clpr:get_atts(Var, lin(_)) ->          %this only works for a clpr variable with linear constraints on it
                true
        ;
         geler_r:get_atts(Var, goals(_)) ->       %works for non-linear
                true
        ).

%%%
midoan_solver__modular_integer_range(Var, Min, Max) :-
        midoan_modular_integer:midoan_modular_integer__get(value, Var, Value),
        midoan_solver__integer_range(Value, Min, Max).
%%%
midoan_solver__real_min(Real_var, Inf, Taken) :-
        inf(Real_var, Inf),
        (\+ entailed(Real_var =\= Inf) ->      %trick should be equivalent to entailed(Value = Inf)
	    Taken = taken                       %lower bound included
	;
	    Taken = not_taken                   %lower bound excluded
	).

midoan_solver__real_max(Real_var, Sup, Taken) :-
        sup(Real_var, Sup),
        (\+ entailed(Real_var =\= Sup) ->      %trick should be equivalent to entailed(Value = Sup)
	    Taken = taken                       %upper bound included
	;
	    Taken = not_taken                   %upper bound excluded
	).

midoan_solver__integer_range(Integer_var, Min, Max) :-
        fd_min(Integer_var, Min),
        fd_max(Integer_var, Max).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%END%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%