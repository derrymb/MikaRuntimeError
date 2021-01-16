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
% mika_name_atts.pl
% defines the module for attributed named variables
% 1st argument is the Ada source name of all identifiers in the Ada source code as per the output of the cross referencer (GNATXREF)
%  E.g. Date_3 has for name "inter.ads:13:19:date" a string, mika_name_atts stores the atom 'inter.ads:13:19:date' (not the string)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(mika_name_atts, []).

:- use_module([library(atts)]).	%Sicstus attribute library

:- attribute mika_name_atts/1.	%name of the attribute
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%internal predicate (see atts library documentation)
%called prior to unification
%Value is a non-variable or another attributed variable (if Value is a plain variable verify_attributes is not called)
% verify_attributes mostly gives rise to error messages except with an atom in which case it should fail
verify_attributes(Var, Value, Goals) :-
        (get_atts(Var, mika_name_atts(Name)) ->
                (atomic(Value) ->       %unification of a name_atts variable with an atom
                        Goals = [Var = Value]  %e.g. during a number declaration in Ada
                ;
                 compound(Value) ->     %unification of a name_atts variable with a compound
                        fail
                ;
                 var(Value) ->          %unification of a name_atts variable with another attributed variable
                        (get_atts(Value, mika_name_atts(Name2)) ->
                                Goals = [Name == Name2] %when does this occur?
                        ;
                                common_util:common_util__error(10, "Unification error", no_error_consequences, [(value, Value)], 104930, mika_name_atts, verify_attributes, no_localisation, "Unification of a name_atts with a non name_atts variable")
                        )
                ;
			%should never happen: not expected what is Value?
                        common_util:common_util__error(10, "Unification error", no_error_consequences, [(var, Var)], 105332, mika_name_atts, verify_attributes, no_localisation, "Unification of a name_atts variable with a non expected term")
                )
        ;
                Goals = []     %no actions as we are not concerned here : not a name_atts variable
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mika_name_atts__is_name_atts(Var) :-
        var(Var),
        get_atts(Var, mika_name_atts(_)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mika_name_atts__create(Name_atts, Name) :-
        put_atts(Name_atts, mika_name_atts(Name)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mika_name_atts__get(name, Name_atts, Name) :-
        get_atts(Name_atts, mika_name_atts(Name)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%create a list of name_atts variables from a list of a(Var, Name_string) generated during parsing in foo.pl
%  e.g. a(X_353, "bool.ads:4:3:x")
%06-03-08 : also tries to find a match for unxrefed variables using : unxrefed(Var, Name_string, Matching_varL)
%unxrefed are generated due to gnatxref bugs (they are generated are parsing time in double_tst.c)
%Matching_varL is a list of Prolog vars that matches the name of the unxrefed entity
mika_name_atts__initL([]).
mika_name_atts__initL([unxrefed(Var, Name_atom, Matching_varL)|R]) :-
        !,
        (Matching_varL == [] ->
                (common_util:common_util__error(6, "Entity found with no match at all during initialisations of variables: will be unhandled", no_error_consequences, [(name, Name_atom)], 688621, mika_name_atts, mika_name_atts__initL, no_localisation, "A referencing problem"),
                 mika_unhandled_atts:mika_unhandled_atts__create(Var, Name_atom, unxrefed)
                )
        ;
         Matching_varL = [Single_match] ->
                Var = Single_match      %not properly referenced but ambiguity is not possible, so there's no problem
        ;
                (%more than one match ... take the first ... may not be correct
                 common_util:common_util__error(6, "Entity found with more than one match during initialisations of variables: choice taken may not be the correct one", no_error_consequences, [(name, Name_atom)], 68534, mika_name_atts, mika_name_atts__initL, no_localisation, "A referencing problem"),
                 Matching_varL = [First_match|_],
                 Var = First_match
                )
        ),
        mika_name_atts__initL(R).
mika_name_atts__initL([a(Var, Name_atom)|R]) :-
        mika_name_atts__create(Var, Name_atom),
        mika_name_atts__initL(R).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%remove the attribute
mika_name_atts__unput(Name_atts, Name) :-
        mika_name_atts__get(name, Name_atts, Name),
        put_atts(Name_atts, -mika_name_atts(_)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%