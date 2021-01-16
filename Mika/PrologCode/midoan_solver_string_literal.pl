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
% midoan_solver_string_literal.pl
% defines the module string_literal for string_literals meta variables
% remember :
% a string literal is not an array of characters
% a string literal is not a positional aggregate of characters
% the basic problem with string literals is that they may not be made of characters (from standard) they could be made of roman digits ...
% so until it can be determined from its context (assignment, qualification ...) what kind of character literals the string is made up of, it has to stay a string literal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a string literal variable A is of the form: A{midoan_string_literal(CodeL)},
% where CodeL is the list of the ASCII codes that makes up the string
:- module(midoan_string_literal, []).

:- attribute midoan_string_literal/1.   %name of the attribute
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
verify_attributes(Var, Value, Goals) :-
        (get_atts(Var, midoan_string_literal(_CodeL)) ->
                (var(Value) ->  %unification of a string literal with a variable
                        (midoan_string_literal__is_string_literal(Value) ->
                                common_util:common_util__error(10, "Unification Error", no_error_consequences, no_arguments, 1036118, midoan_string_literal, verify_attributes, no_localisation, "Unification of a string literal with another string literal")
                        ;
                                common_util:common_util__error(10, "Unification Error", no_error_consequences, no_arguments, 1038118, midoan_string_literal, verify_attributes, no_localisation, "Unification of a string literal with a non string literal variable")
                        )
                ;
                        fail    %unification of a string_literal var with a nonvar (e.g 'others')
                )
        ;
                Goals = []
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_string_literal__is_string_literal(Var) :-
	var(Var),
	get_atts(Var, midoan_string_literal(_)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_string_literal__create_string_literal(CodeL, String_literal) :-
	put_atts(String_literal, midoan_string_literal(CodeL)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_string_literal__get_ascii_codes(String_literal, CodeL) :-
	get_atts(String_literal, midoan_string_literal(CodeL)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_string_literal__unput(String_literal) :-
        put_atts(String_literal, -midoan_string_literal(_)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%END%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%