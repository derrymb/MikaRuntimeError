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
% midoan_solver_enum.pl
% defines module enum for enumeration meta variables handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% an enum variable is of the form midoan_enum(Type_var, Position) where Position is not a ground fd integer
% an ground enumeration literal is midoan_enum(Type_var, Position) where Position is a ground integer
% Position is an fd variable, the literal_positions list [(Lit_name, Position), ...] of an enumeration type is an attribute of Type_var
%second argument is name_position(Type_name, Subtype_name, [(Lit_name, Position), ...]) where Lit_name is the name of the literal and position is a ground integer
:- module(midoan_enum, []).

:- use_module([	library(lists), %for member/2, last/2, nextto/2
		library(clpfd), %for domain/3
		library(random)	%for random/3
	     ]).

:- multifile clpfd:dispatch_global/4.
clpfd:dispatch_global(succ_blocked(X, R), state(X, R), state(_, _), Actions) :-
        Actions = [call(midoan_enum:succ_blocked(X, R))].
clpfd:dispatch_global(pos_blocked(Type_var, Enum_var, FD_var), state(Type_var, Enum_var, FD_var), state(_, _, _), Actions) :-
        Actions = [call(midoan_enum:pos_blocked(Type_var, Enum_var, FD_var))].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%a representation clause updates the earlier declaration of an enumeration type
%Rep_clause is of the form : [Exp, ..., Exp]: positional
%                         or [(Lit, Exp), ..., (Lit, Exp)]: named
%returns the first and last literals and updates midoan_enum__dynamic_list
midoan_enum__update_enum(Type_var, [First|Rest], midoan_enum(Type_var, Pos_first), midoan_enum(Type_var, Pos_last), New_literal_positionL):-
        midoan_type:midoan_type__get_attribute(Type_var, literal_positions, Literal_positionL),
	((compound(First), First = named(_Names, _Value)) ->
                match_rep_clause_named(Literal_positionL, [First|Rest], New_literal_positionL)	%Rep_clause is list of (Name, Exp)
        ;
                match_rep_clause_positional(Literal_positionL, [First|Rest], New_literal_positionL)	%Rep_clause is a list of Exp
        ),
        New_literal_positionL = [(_Name_first, Pos_first)|_],
	last(New_literal_positionL, (_Name_last, Pos_last)).
%%%
        %Rep_clause is of the form (Name, Exp)
        match_rep_clause_named([], _, []).
        match_rep_clause_named([(Lit_name, _Pos)|R], Rep_clause, [(Lit_name, New_position)|R2]) :-
                memberchk(named(Lit_name, New_position), Rep_clause),
                match_rep_clause_named(R, Rep_clause, R2).
%%%
        %Rep_clause is a list of Exp
        match_rep_clause_positional([], [], []).
        match_rep_clause_positional([(Lit_name, _Pos)|R], [New_position|R1], [(Lit_name, New_position)|R2]) :-
                match_rep_clause_positional(R, R1, R2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%returns the first and last literals
midoan_enum__get_literal_positions(Type_var, Literals_name_valueL, midoan_enum(Type_var, Pos_first), midoan_enum(Type_var, Pos_last), Literal_positionL) :-
        enum_representation(Literals_name_valueL, Type_var, Literal_positionL, 0),	%assign default positions to literals
	Literal_positionL = [(_Name_first, Pos_first)|_],
	last(Literal_positionL, (_Name_last, Pos_last)).
midoan_enum__get_literal_positions(_New_subtype_var, range(midoan_enum(Type_var, Pos_first), midoan_enum(Type_var, Pos_last)), New_literal_positionL) :-
        midoan_type:midoan_type__get_attribute(Type_var, literal_positions, Literal_positionL),
        get_sublist(Literal_positionL, Pos_first, Pos_last, New_literal_positionL).
%%%
        %default integer representation for enumeration types
        %Literals_name_valueL is a list of (Lit_name, Literal) where Literal is initially the input attribute of an SEAV
        enum_representation([], _, [], _).
        enum_representation([(Lit_name, midoan_enum(Type_var, Position))|RestL], Type_var, [(Lit_name, Position)|Rest2], N) :-
                N_1 is N+1,
                domain([Position], N, N),          %constrain the position as an fd variable
                enum_representation(RestL, Type_var, Rest2, N_1).
%%%
        get_sublist([], _, _, []) :-
                common_util:common_util__error(10, "Min in range is invalid in enumeration", no_error_consequences, no_arguments, 10126109, midoan_enum, get_sublist, no_localisation, no_extra_info).
        get_sublist([(Lit_name, Pos)|Rest], Pos_first, Pos_last, New_lit_posL) :-
                (Pos = Pos_first ->
                        get_sublist_rest([(Lit_name, Pos)|Rest], Pos_last, New_lit_posL)
                ;
                        get_sublist(Rest, Pos_first, Pos_last, New_lit_posL)
                ).
                %%%
                get_sublist_rest([], _, []) :-
                        common_util:common_util__error(10, "Max in range is invalid in enumeration", no_error_consequences, no_arguments, 10135116, midoan_enum, get_sublist_rest, no_localisation, no_extra_info).
                get_sublist_rest([(Lit_name, Pos)|Rest], Pos_last, [(Lit_name, Pos)|Rest_lit_posL]) :-
                        (Pos = Pos_last ->
                                Rest_lit_posL = []      %end of the recursion
                        ;
                                get_sublist_rest(Rest, Pos_last, Rest_lit_posL)
                        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%check if a variable is an enumeration variable or a ground literal
midoan_enum__is_enum(Enum) :-
	(nonvar(Enum) ->
	        Enum = midoan_enum(_Type_var, _Position)       %may fail
        ;
                fail
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%create an enumeration variable or ground literal
midoan_enum__create_enum(Type_var, _Subtype, midoan_enum(_, Pos_first), midoan_enum(_, Pos_last), Enum) :-
	domain([Position], Pos_first, Pos_last),          %constrain the position as an fd variable
        Enum = midoan_enum(Type_var, Position).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%always called from a string_literal
midoan_enum__generate_list_of_enums(Var_or_CodeL, Character_type_var, Enum_list) :-
        (midoan_string_literal:midoan_string_literal__is_string_literal(Var_or_CodeL) ->
                midoan_string_literal:midoan_string_literal__get_ascii_codes(Var_or_CodeL, CodeL)
        ;
                CodeL = Var_or_CodeL
        ),
        get_codes_for_type(Character_type_var, Codes_for_type),
        generate_list_of_enums_from_codes(CodeL, Codes_for_type, Enum_list).
%%%
        %matches the true ascii code with a ground literal within character type with the same ascii code
        %Code is the ascii code
        %Codes_for_type is the list of (Character_code, Ground_enum)
        get_codes_for_type(Character_type_var, PrintableL) :-
                midoan_type:midoan_type__get_attribute(Character_type_var, printable, Printable_attribute), %the list for types that contain, actual, printable, characters the list of [(Code, Lit_name, Pos)]
                (Printable_attribute == not_built ->
                        (midoan_type:midoan_type__get_attribute(Character_type_var, literal_positions, Literal_positionL),
                         build_printableL(Literal_positionL, Character_type_var, PrintableL),
                         midoan_type:midoan_type__update_type_attribute(Character_type_var, printable, PrintableL)      %if it does not exist it is recorded to improve efficiency next time
                        )
                ;
                        PrintableL = Printable_attribute
                ).
%%%
                build_printableL([], _Character_type_var, []).
                build_printableL([(Lit_name, Pos)|Rest], Character_type_var, PrintableL) :-
                        (get_character_ascii(Lit_name, Character_code) ->   %may fail if the 'character' is not printable
                                (build_printableL(Rest, Character_type_var, PrintableL2),
                                 append([(Character_code, midoan_enum(Character_type_var, Pos))], PrintableL2, PrintableL)
                                )
                        ;
                                build_printableL(Rest, Character_type_var, PrintableL)
                        ).
%%%
                        get_character_ascii(Lit_name, Code) :-
                                mika_symbolic:mika_symbolic__parse(Lit_name, _File_name, _File_extension, _Line, _Column, Id),
                                atom_codes(Id, Id_code),
                                append([99, 104, 97, 114, 95, 95], Char_code, Id_code),     %[99, 104, 97, 114, 95, 95] represents 'char__'
                                number_codes(Code, Char_code).  %may fail because some 'characters' cannot be displayed (are not char__something because they are not character literals such as 'a'
%%%
        generate_list_of_enums_from_codes([], _Codes_for_type, []).
        generate_list_of_enums_from_codes([Next|Rest], Codes_for_type, [Char_enum|Rest_agg]) :-
                match_ascii(Next, Codes_for_type, Char_enum), %matches the true ascii code with an enum value within character type with the same ascii code
                generate_list_of_enums_from_codes(Rest, Codes_for_type, Rest_agg).
%%%
                match_ascii(Character_code, Codes_for_type, Char_enum) :-
                        (memberchk((Character_code, Char_enum), Codes_for_type) ->
                                true
                        ;
                                common_util:common_util__error(10, "Could not find character code", no_error_consequences, [(character_code, Character_code), (codes_for_type, Codes_for_type)], 10205171, midoan_enum, match_ascii, no_localisation, no_extra_info)
                        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%sample an enumeration variable
midoan_enum__sample(midoan_enum(Type_var, Position)) :-
        (ground(Position) ->
                true    %already ground
        ;
	        (midoan_type:midoan_type__get_attribute(Type_var, literal_positions, Literal_positionL),
	         pick_one_at_random(Literal_positionL, Position)
	        )
        ).
%%%
        %May be inefficient for large enumeration types with long Literal_positionL
        % where the Position is very constrained
        pick_one_at_random([], _Pos) :-
                !,
                fail.	%no suitable ground literal
        pick_one_at_random(Lit_posL, Position) :-
                length(Lit_posL, Length),
                Length_1 is Length + 1,
                random(1, Length_1, Choice),
                nth(Choice, Lit_posL, (_Lit_name, Pos), Rest),
                (Position = Pos ->         %Position is suitable
                        true
                ;
                        pick_one_at_random(Rest, Position)
                ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_enum__get('position', midoan_enum(_Type_var, Position), Position).
midoan_enum__get('name', midoan_enum(Type_var, Position), Name) :-
        (ground(Position) ->
                (midoan_type:midoan_type__get_attribute(Type_var, 'literal_positions', Literal_positionL),
                 memberchk((Name, Position), Literal_positionL)
                )
        ;
                Name = _        %the Enum is an Enum_var its Name could be anything
        ).
midoan_enum__get('type', midoan_enum(Type_var, _Position), Type_var).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%get the min literal and max literal of an enumeration variable
midoan_enum__get_range_literal(Enum, midoan_enum(Type_var, Pos_first), midoan_enum(Type_var, Pos_last)) :-
	midoan_enum__get('type', Enum, Type_var),
        midoan_enum__get('position', Enum, Position),
        (var(Position) ->
                (midoan_type:midoan_type__get_attribute(Type_var, 'literal_positions', Literal_positionL),
                 util_find_valid_positions(Position, Literal_positionL, Pos_first, Pos_last)     %since Position is not ground we find the range of valid positions
                )
        ;
                (Pos_first = Position,
                 Pos_last = Position
                )
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_enum__ground(midoan_enum(_Typevar, Position)) :-
        ground(Position).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%apply the constraint 'R is the predecessor of X'
midoan_enum__pred(X, R) :-
	midoan_enum__succ(R, X).         %as simple as that
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% apply the constraint 'R is the successor of X'
midoan_enum__succ(X, R) :-
        (succ_ground(X, R) ->
                true
        ;
                ((\+ midoan_enum__is_enum(X) ->  %this happens for somes call from the engine
                        (midoan_enum__get('type', R, Type_var),
                         midoan_type:midoan_type__variable_declaration(X, Type_var)
                        )
                 ;
                  \+ midoan_enum__is_enum(R) ->  %this happens for somes call from the engine
                        (midoan_enum__get('type', X, Type_var),
                         midoan_type:midoan_type__variable_declaration(R, Type_var)
                        )
                 ),
                 succ_constrain(X, R),  %e.g. that X cannot be the last, may result in X or R becoming ground literal
                 (succ_ground(X, R) ->
                        true
                 ;
                        (midoan_enum__get('position', X, PositionX),
		         midoan_enum__get('position', R, PositionR),
                         fd_global(succ_blocked(X, R), state(X, R), [dom(PositionX), dom(PositionR)]) %apply global constraint
                        )
                 )
                )
        ).

succ_ground(X, R) :-
        (midoan_enum__ground(X) ->
                (X = midoan_enum(Type_var, Position_first),
                 midoan_type:midoan_type__get_attribute(Type_var, 'literal_positions', Literal_positionL),
                 nextto((_Name_first, Position_first), (_Name_second, Position_second), Literal_positionL),
                 R = midoan_enum(Type_var, Position_second)       %verify_attributes may be called since R may not be ground
                )
        ;
         midoan_enum__ground(R) ->
                (R = midoan_enum(Type_var, Position_second),
                 midoan_type:midoan_type__get_attribute(Type_var, 'literal_positions', Literal_positionL),
                 nextto((_Name_first, Position_first), (_Name_second, Position_second), Literal_positionL),
                 X = midoan_enum(Type_var, Position_first)        %verify_attributes will be called since X is not ground
                )
        ;
                fail
        ).

succ_constrain(X, R) :-
        midoan_enum__get('position', X, PositionX),
        midoan_enum__get('position', R, PositionR),
        midoan_enum__get('type', X, Type_var),
        midoan_type:midoan_type__get_attribute(Type_var, 'literal_positions', Literal_positionL),
        append(_, [(_Penul_name, Penul_pos), (_Last_name, _Last_Pos)], Literal_positionL),       %leaves choice points
        !,
        midoan_solver:midoan_solver__sdl(<=(PositionX, Penul_pos)),           %and we constrain PositionX to be have valid max position
        midoan_solver:midoan_solver__sdl(<(PositionX, PositionR)),            %we ensure that R is greater than X
        util_find_valid_positions(PositionX, Lit_posL, _Min_posX, _Max_posX),
        util_find_valid_positions(PositionR, Lit_posL, _Min_posR, _Max_posR).

succ_blocked(X, R) :-
        (succ_ground(X, R) ->
                true
        ;
                succ_constrain(X, R)
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_enum__val(Type_var, X, R) :-
	midoan_enum__pos(Type_var, R, X).             %as simple as that
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%X is an enumeration variable
%R is an integer, an integer variable
midoan_enum__pos(Type_var, X, R) :-
        (pos_ground(Type_var, X, R) ->
                true
	;
                (pos_constrain(Type_var, X, R),
                 (pos_ground(Type_var, X, R) ->
                        true
                 ;
                        (midoan_enum__get('position', X, Position),
                         fd_global(pos_blocked(Type_var, X, R), state(Type_var, X, R), [dom(Position), dom(R)]) %apply global constraint
                        )
                 )
                )
        ).

pos_ground(Type_var, X, R) :-
        (midoan_enum__ground(X) ->
                X = midoan_enum(_Type_var, R)
	;
	 ground(R) ->
		(midoan_type:midoan_type__get_attribute(Type_var, 'literal_positions', Literal_positionL),
                 memberchk((_Lit_name, R), Literal_positionL),   %we ensure that R is a valid position
                 X = midoan_enum(Type_var, R)        %verify_attributes will be called since X is not ground
                )
        ;
                fail
        ).

pos_constrain(Type_var, X, R) :-
	midoan_type:midoan_type__get_attribute(Type_var, 'literal_positions', Literal_positionL),
        midoan_enum__get('position', X, Position),
	R #= Position,                        %actually performs the intersection of the domains
	util_find_valid_positions(Position, Literal_positionL, _Min_pos, _Max_pos).

pos_blocked(Type_var, X, R) :-
        (pos_ground(Type_var, X, R) ->
                true
        ;
                pos_constrain(Type_var, X, R)
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
util_find_valid_positions(Position, Lit_posL, Min_pos, Max_pos) :-
        midoan_solver:midoan_solver__integer_range(Position, Min, Max),
	(memberchk((_, Min), Lit_posL) ->               %the minimum of R has a literal equivalent
		 Min_pos = Min
	     ;
	         (!,
		  find_min_pos(Lit_posL, Min, Min_pos),         %find the supremum of Min
                  midoan_solver:midoan_solver__sdl(>=(Position, Min_pos))      %and we constrain Position to be have valid min position
	         )
	),
	(memberchk((_, Max), Lit_posL) ->               %the maximum of R has a literal equivalent
		 Max_pos = Max
	     ;
	         (!,
		  find_max_pos(Lit_posL, Max, Max_pos),         %find the infinimum of Max
                  midoan_solver:midoan_solver__sdl(<=(Position, Max_pos))     %and we constrain Position to be have valid max position
	         )
	).
%New_min_pos is the supremum of Min_pos
find_min_pos([], _, _) :-
	fail.
find_min_pos([(_, Pos)|Rest], Min, Min_pos) :-
	(Min < Pos ->
	    Min_pos = Pos
	;
	    find_min_pos(Rest, Min, Min_pos)
	).
%New_max_pos is the infimum of Max_pos
find_max_pos([(_, Pos)], Max, Max_pos) :-
	!,
	(Pos < Max ->
		Max_pos = Pos
	;
		fail
	).
find_max_pos([(_, Pos), Next|Rest], Max, Max_pos) :-
	(find_max_pos([Next|Rest], Max, Max_pos) ->
		true
	;
	        (Pos < Max ->
		        Max_pos = Pos
		;
			fail
		)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_enum__get_length_dynamic_list(Type_var, Length) :-
        midoan_type:midoan_type__get_attribute(Type_var, 'literal_positions', Literal_positionL),
        midoan_type:midoan_type__get_attribute(Type_var, 'first', Min),
        midoan_type:midoan_type__get_attribute(Type_var, 'last', Max),
        midoan_enum__get('position', Min, Pos_min),
        midoan_enum__get('position', Max, Pos_max),
        get_sublist(Literal_positionL, Pos_min, Pos_max, New_literal_positionL),        %11/11/10 this is necessary for subtypes of enumeration types (same Literal_positionL as the parten type but with a restricted range)
        length(New_literal_positionL, Length).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%