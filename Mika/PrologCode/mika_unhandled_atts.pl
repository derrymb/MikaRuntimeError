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
% mika_unhandled_atts.pl (14-06-07)
% defines the module for attributed unhandled variables (i.e. to represent unhandled entities during symbolic execution)
% 1st argument is hopefully the xrefed name
% 2nd argument is a comment (may not be used)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(mika_unhandled_atts, []).

:- use_module([library(atts)]).	%Sicstus attribute library

:- attribute mika_unhandled_atts/2.	%name of the attribute
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%internal predicate (see atts library documentation)
%called prior to unification
%Value is a non-variable or another attributed variable (if Value is a plain variable verify_attributes is not called)
verify_attributes(Var, Value, Goals) :-
        (get_atts(Var, mika_unhandled_atts(Name, Comment)) ->
                (atomic(Value) ->       %unification of a name_atts variable with an atom
                        common_util:common_util__error(10, "Unification error", "Clearly never allowed", [(name, Name)], 103443, mika_unhandled_atts, verify_attributes, no_localisation, "Unification of an unhandled_atts variable with an atom")
                ;
                 compound(Value) ->     %unification of an unhandled_atts variable with a compound
                        common_util:common_util__error(10, "Unification error", "Clearly never allowed", [(name, Name)], 103744, mika_unhandled_atts, verify_attributes, no_localisation, "Unification of an unhandled_atts variable with a compound")
                ;
                 var(Value) ->          %unification of a unhandled_atts variable with another attributed variable
                                        %may occur during copy_term of a subprogram body during a subprogram call
                        (get_atts(Value, mika_unhandled_atts(Name2, Comment2)) ->       %changed 20/05/09
                                (Name = Name2,
                                 Comment = Comment2,
                                 Goals = []
                                )
                        ;
                                common_util:common_util__error(10, "Unification error", "Clearly never allowed", [(name, Name)], 104637, mika_unhandled_atts, verify_attributes, no_localisation, "Unification of an unhandled_atts variable with a non unhandled attributed variable")
                        )
                ;
                        common_util:common_util__error(10, "Unification error", "Clearly never allowed", [(name, Name)], 104244, mika_unhandled_atts, verify_attributes, no_localisation, "Unification of an unhandled_atts variable with a non expected term")
                )
        ;
                Goals = []     %no actions as we are not concerned here : not an unhandled_atts variable
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mika_unhandled_atts__is_unhandled_atts(Var) :-
        var(Var),
        get_atts(Var, mika_unhandled_atts(_, _)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mika_unhandled_atts__create(Unhandled_atts, Name, Comment) :-
        put_atts(Unhandled_atts, mika_unhandled_atts(Name, Comment)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mika_unhandled_atts__get('name', Unhandled_atts, Name) :-
        get_atts(Unhandled_atts, mika_unhandled_atts(Name, _)).
mika_unhandled_atts__get('comment', Unhandled_atts, Comment) :-
        get_atts(Unhandled_atts, mika_unhandled_atts(_, Comment)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%