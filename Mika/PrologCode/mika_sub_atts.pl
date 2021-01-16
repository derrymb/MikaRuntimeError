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
% mika_sub_atts.pl
% defines the module for attributed subprogram variables
% 1st argument  name            is the full atomic xref ada name of the subprogram (e.g. bool.ads:35:13:rec)
% 2nd argument  status          indicate current status of the subprogram var (e.g. declared, bodied, rename_of(Orig_sub_var), imported)   added 30/01/08
% 3rd argument	return		is the return variable used in the code of the function subprogram (see parsing)
% 4th argument  return_type     is the <name> return type for functions (only used in setup_and_run)
% 5th argument  parameters      the list of parameters
% 6th argument  local_decl      the list of local declarations to the subprogram
% 7th argument  body            the body of the subprogram
% 8th argument  delayed calls   16/11/09 delayed call
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(mika_sub_atts, []).

:- use_module([library(atts), common_util]).

:- attribute mika_sub_atts/8.   %name of the attribute
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%internal predicate
%called prior to unification
%Value is a non-variable or another attributed variable (if Value is a plain variable verify_attributes is not called)
% verify_attributes mostly gives rise to error messages except with an atom in which case it should fail
verify_attributes(Var, Value, Goals) :-
        (get_atts(Var, mika_sub_atts(N, S, R, RT, P, D, B, DC)) ->
                (atomic(Value) ->       %unification of a sub_atts variable with an atom, can happen
                        fail            % but it is a failure
                ;
                 compound(Value) ->     %unification of a sub_atts variable with a compound, can happen
                        fail            % but it is a failure
                ;
                 var(Value) ->          %unification of a sub_atts variable with another attributed variable
                        (get_atts(Value, mika_sub_atts(N2, S2, R2, RT2, P2, D2, B2, DC2)) ->
                                Goals = [a(N, S, R, RT, P, D, B, DC) = a(N2, S2, R2, RT2, P2, D2, B2, DC2)]
                         ;
                                common_util:common_util__error(10, "Unification error", "Clearly never allowed", [(name, N)], 105227, mika_sub_atts, verify_attributes, no_localisation, "Unification of a sub_atts var with another attributed variable")
                        )
                ;
                        %should never happen: not expected what is Value?
                        common_util:common_util__error(10, "Unification error", "Clearly never allowed", [(name, N)], 105629, mika_sub_atts, verify_attributes, no_localisation, "Unification of a sub_atts variable with a non expected term")
                )
        ;
                Goals = []     %no actions as we are not concerned here : not a sub_atts variable
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mika_sub_atts__is_sub_atts(Var) :-
        var(Var),
        get_atts(Var, mika_sub_atts(_, _, _, _, _, _, _, _)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mika_sub_atts__create(Sub_atts, Name, Status, Return, Return_type, Params, Decls, Body, Delayed_calls) :-
        put_atts(Sub_atts, mika_sub_atts(Name, Status, Return, Return_type, Params, Decls, Body, Delayed_calls)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%remove the attribute
mika_sub_atts__unput(Sub_atts) :-
        put_atts(Sub_atts, -mika_sub_atts(_, _, _, _, _, _, _, _)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mika_sub_atts__get('name', Sub_atts, Name) :-
        get_atts(Sub_atts, mika_sub_atts(Name, _, _, _, _, _, _, _)).
mika_sub_atts__get('status', Sub_atts, Status) :-
        get_atts(Sub_atts, mika_sub_atts(_, Status, _, _, _, _, _, _)).
mika_sub_atts__get('return', Sub_atts, Return) :-
        get_atts(Sub_atts, mika_sub_atts(_, _, Return, _, _, _, _, _)).
mika_sub_atts__get('return_type', Sub_atts, Return_type) :-
        get_atts(Sub_atts, mika_sub_atts(_, _, _, Return_type, _, _, _, _)).
mika_sub_atts__get('params', Sub_atts, Params) :-
        get_atts(Sub_atts, mika_sub_atts(_, _, _, _, Params, _, _, _)).
mika_sub_atts__get('decls', Sub_atts, Decls) :-
        get_atts(Sub_atts, mika_sub_atts(_, _, _, _, _, Decls, _, _)).
mika_sub_atts__get('body', Sub_atts, Body) :-
        get_atts(Sub_atts, mika_sub_atts(_, _, _, _, _, _, Body, _)).
mika_sub_atts__get('delayed_calls', Sub_atts, Delayed_calls) :-
        get_atts(Sub_atts, mika_sub_atts(_, _, _, _, _, _, _, Delayed_calls)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mika_sub_atts__update('status', Sub_atts, Status) :-
        get_atts(Sub_atts, mika_sub_atts(Name, _, Return, Return_type, Params, Decls, Body, Delayed_calls)),
	put_atts(Sub_atts, mika_sub_atts(Name, Status, Return, Return_type, Params, Decls, Body, Delayed_calls)).
mika_sub_atts__update('delayed_calls', Sub_atts, Delayed_calls) :-
        get_atts(Sub_atts, mika_sub_atts(Name, Status, Return, Return_type, Params, Decls, Body, _)),
	put_atts(Sub_atts, mika_sub_atts(Name, Status, Return, Return_type, Params, Decls, Body, Delayed_calls)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%