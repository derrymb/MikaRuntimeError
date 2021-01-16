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
% midoan_solver_anon_aggregate.pl
% defines the module anon_aggregate for anonymous aggregates meta variables
%could be record or array anonymous aggregates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% an anon_aggregate variable A is of the form: A{midoan_anon_aggregate(Stuff)}, where Stuff is a symblically executed record or array aggregate
:- module(midoan_anon_aggregate, []).

:- attribute midoan_anon_aggregate/1.   %name of the attribute
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%internal predicate (see atts library documentation)
%called prior to unification
%Value is a non-variable or another attributed variable (if Value is a plain variable verify_attributes is not called)
%!!! this is no longer used (since September 2007) : all done via : midoan_solver__controlled_unification
verify_attributes(Var, Value, Goals) :-
        (get_atts(Var, midoan_anon_aggregate(_Stuff)) ->
                (var(Value) ->  %unification of a anonymous aggregate variable with a variable (should always be the case)
                        (midoan_anon_aggregate__is_anon_aggregate(Value) ->
                                common_util:common_util__error(10, "Unification Error", no_error_consequences, no_arguments, 1030118, midoan_anon_aggregate, verify_attributes, no_localisation, "Unification of an an anonymous aggregate with another non anonymous aggregate variable: should use midoan_solver__controlled_unification")
                        ;
                                common_util:common_util__error(10, "Unification Error", no_error_consequences, no_arguments, 103241, midoan_anon_aggregate, verify_attributes, no_localisation, "Unification of an an anonymous aggregate with a non anonymous aggregate variabl: should use midoan_solver__controlled_unification")
                        )
                ;
                        fail    %unification of an anonymous aggregate with a non variable
                )
        ;
                Goals = []     %no actions as we are not concerned : not an anon aggregate variable
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%check if a variable is a record variable
midoan_anon_aggregate__is_anon_aggregate(Var) :-
	var(Var),
	get_atts(Var, midoan_anon_aggregate(_)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%create an anonymous aggregate variable
midoan_anon_aggregate__create_anon_aggregate(Stuff, Anon_aggregate_var) :-
	put_atts(Anon_aggregate_var, midoan_anon_aggregate(Stuff)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_anon_aggregate__get_anon_aggregate_stuff(Anon_aggregate_var, Stuff) :-
	get_atts(Anon_aggregate_var, midoan_anon_aggregate(Stuff)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_anon_aggregate__unput(Anon_aggregate_var) :-
        put_atts(Anon_aggregate_var, -midoan_anon_aggregate(_)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%