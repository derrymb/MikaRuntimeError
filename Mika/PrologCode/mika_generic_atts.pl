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
% mika_generic_atts.pl
% defines the module for attributed generic variables
% This is not much more than a stub at present (13/02/07)
% 1st argument  name            is the full atomic xref ada name of the generic declaration
% 2nd argument	type		is the type of generic declaration (package|subprogram)
% 3rd argument  generic         is the generic formal part of the declaration
% 4th argument  spec            the actual specification of the generic entity (incl. generic_formal_part)
% 5th argument  body            the actual body of the generic entity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(mika_generic_atts, []).

:- use_module([library(atts)]).	%Sicstus attribute library

:- attribute mika_generic_atts/5.   %name of the attribute
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%internal predicate
%called prior to unification
%Value is a non-variable or another attributed variable (if Value is a plain variable verify_attributes is not called)
% verify_attributes mostly gives rise to error messages except with an atom in which case it should fail
verify_attributes(Var, Value, Goals) :-
        (get_atts(Var, mika_generic_atts(_Name, _Type, _Generic, _Spec, _Body)) ->
                (atomic(Value) ->       %unification of a generic_atts variable with an atom, can happen
                        common_util:common_util__error(10, "Unification Error", no_error_consequences, [(value, Value)], 104012, mika_generic_atts, verify_attributes, no_localisation, "Unification of a generic_atts with an atom")
                ;
                 compound(Value) ->     %unification of a generic_atts variable with a compound
                        common_util:common_util__error(10, "Unification Error", no_error_consequences, [(value, Value)], 104313, mika_generic_atts, verify_attributes, no_localisation, "Unification of a generic_atts with a compound")
                ;
                 var(Value) ->          %unification of a generic_atts variable with another attributed variable
                        common_util:common_util__error(10, "Unification Error", no_error_consequences, [(value, Value)], 104614, mika_generic_atts, verify_attributes, no_localisation, "Unification of a generic_atts with another attributed variable")
                ;
                        %should never happen: not expected what is Value?
                        common_util:common_util__error(10, "Unification Error", no_error_consequences, [(value, Value)], 105015, mika_generic_atts, verify_attributes, no_localisation, "Unification of a generic_atts variable with a non expected term")
                )
        ;
                Goals = []     %no actions as we are not concerned here : not a generic_atts variable
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mika_generic_atts__is_generic_atts(Var) :-
        var(Var),
        get_atts(Var, mika_generic_atts(_, _, _, _, _)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%during elaboration the specification of a generic entity has been found: the body will be found later
mika_generic_atts__create(Generic_atts, Name, Type, Generic, Spec) :-
        put_atts(Generic_atts, mika_generic_atts(Name, Type, Generic, Spec, _)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%during elaboration : the body of a previously declared as generic entity has been found
mika_generic_atts__set_body(Generic_atts, Body) :-
        get_atts(Generic_atts, mika_generic_atts(Name, Type, Generic, Spec, _)),
        put_atts(Generic_atts, mika_generic_atts(Name, Type, Generic, Spec, Body)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mika_generic_atts__get(name, Generic_atts, Name) :-
        get_atts(Generic_atts, mika_generic_atts(Name, _, _, _, _)).
mika_generic_atts__get(type, Generic_atts, Type) :-
	get_atts(Generic_atts, mika_generic_atts(_, Type, _, _, _)).
mika_generic_atts__get(generic, Generic_atts, Generic) :-
	get_atts(Generic_atts, mika_generic_atts(_, _, Generic, _, _)).
mika_generic_atts__get(spec, Generic_atts, Spec) :-
	get_atts(Generic_atts, mika_generic_atts(_, _, _, Spec, _)).
mika_generic_atts__get(body, Generic_atts, Body) :-
	get_atts(Generic_atts, mika_generic_atts(_, _, _, _, Body)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%