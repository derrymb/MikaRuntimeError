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
% mika_package_atts.pl
% defines the module for attributed package variables
% 1st argument name is the full atomic xref ada name of the package e.g. 'bool.ads:1:9:bool'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(mika_package_atts, []).

:- use_module([library(atts)]).	%Sicstus attribute library

:- attribute mika_package_atts/1.   %name and arity of the attribute
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%internal predicate
%called prior to unification
%Value is a non-variable or another attributed variable (if Value is a plain variable verify_attributes is not called)
% verify_attributes mostly gives rise to error messages except with an atom in which case it should fail
verify_attributes(Var, Value, Goals) :-
        (get_atts(Var, mika_package_atts(Name)) ->
                (atomic(Value) ->       %unification of a package_atts variable with an atom, can happen
                        fail            % but it is a failure
                ;
                 compound(Value) ->     %unification of a package_atts variable with a compound
                        fail
                ;
                 var(Value) ->          %unification of a package_atts variable with another attributed variable
                        (get_atts(Value, mika_package_atts(Name2)) ->
				Goals = [a(Name) = a(Name2)]        %Unification update is deferred
                        ;
                                common_util:common_util__error(10, "Unification error", no_error_consequences, [(value, Value)], 104334, mika_package_atts, verify_attributes, no_localisation, "Unification of a package_atts with a non package_atts attributed variable")
                        )
                ;
                        %should never happen: not expected what would Value be?
                        common_util:common_util__error(10, "Unification error", no_error_consequences, [(value, Value)], 104736, mika_package_atts, verify_attributes, no_localisation, "Unification of a package_atts variable with a non expected term")
                )
        ;
		Goals = []     %no actions as we are not concerned here : not a package_atts variable
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mika_package_atts__is_package_atts(Var) :-
        var(Var),
        get_atts(Var, mika_package_atts(_)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mika_package_atts__create(Package_atts, Name) :-
        put_atts(Package_atts, mika_package_atts(Name)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mika_package_atts__get(Package_atts, 'name', Name) :-   %the Xref name
        get_atts(Package_atts, mika_package_atts(Name)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%