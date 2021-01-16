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
% mika_coverage__check.pl
% module mika_coverage
% for debugging purposes only
% predicate check_well_formedness checks for the well-formedness of cfgs but does NOT check for:
%	-repeated arcs
%	-that nodes are not shared between subprograms
%	(these are assumed to be impossible by construction)
% test driver below means we can check this predicate independently
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
:- use_module([mika_globals], [ mika_globals__set_BT/2,
                                mika_globals__get_BT/2,
				mika_globals__set_NBT/2,
                                mika_globals__get_NBT/2
                              ]
             ).
:- use_module(common_util, [	common_util__error/9
                           ]
             ).
arc_bran(start('bool.adb:6:11:loopstmts'),	1,	true).
arc_bran(1,	2,	true).
arc_bran(2,	end,	true).
arc_bran(2,	3,	false).
arc_bran(3,	end,	true).
arc_bran(3,	1,	false).
arc_bran(1,	end,	false).
arc_bran(start('bool.adb:16:11:casestmt'),	4,	true).
arc_bran(4,	end,	true).
arc_bran(4,	5,	false).
arc_bran(5,	end,	true).
arc_bran(5,	6,	false).
arc_bran(6,	end,	true).
arc_bran(6,	7,	false).
arc_bran(7,	end,	true).
arc_bran(7,	end,	false).
arc_bran(start('bool.ads:4:13:exchange'),	end,	true).
arc_bran(start('bool.adb:37:11:maximum'),	8,	true).
arc_bran(8,	end,	true).
arc_bran(8,	end,	false).
arc_bran(start('bool.adb:48:10:increment'),	end,	true).
arc_bran(start('bool.adb:53:11:test'),	9,	true).
arc_bran(9,	end,	true).
arc_bran(9,	end,	false).
arc_bran(start('bool.adb:64:11:wasmain'),	10,	true).
arc_bran(10,	end,	true).
arc_bran(10,	end,	false).
arc_bran(start('bool.adb:77:11:problem'),	11,	true).
arc_bran(11,	end,	true).
arc_bran(11,	end,	false).
arc_bran(start('bool.adb:87:11:allpath'),	12,	true).
arc_bran(12,	13,	true).
arc_bran(13,	end,	true).
arc_bran(13,	end,	false).
arc_bran(12,	13,	false).
arc_bran(start('elaboration'),	end,	true).
call_bran(8,	'bool.ads:4:13:exchange',	false).
call_bran(start('bool.adb:53:11:test'),	'bool.adb:48:10:increment',	true).
arc_deci(start('bool.adb:6:11:loopstmts'),	1,	true).
arc_deci(1,	2,	true).
arc_deci(2,	end,	true).
arc_deci(2,	3,	false).
arc_deci(3,	end,	true).
arc_deci(3,	1,	false).
arc_deci(1,	end,	false).
arc_deci(start('bool.adb:16:11:casestmt'),	4,	true).
arc_deci(4,	end,	true).
arc_deci(4,	5,	false).
arc_deci(5,	end,	true).
arc_deci(5,	6,	false).
arc_deci(6,	end,	true).
arc_deci(6,	7,	false).
arc_deci(7,	end,	true).
arc_deci(7,	end,	false).
arc_deci(start('bool.ads:4:13:exchange'),	end,	true).
arc_deci(start('bool.adb:37:11:maximum'),	8,	true).
arc_deci(8,	end,	true).
arc_deci(8,	end,	false).
arc_deci(start('bool.adb:48:10:increment'),	end,	true).
arc_deci(start('bool.adb:53:11:test'),	9,	true).
arc_deci(9,	10,	true).
arc_deci(10,	11,	true).
arc_deci(11,	12,	true).
arc_deci(12,	end,	true).
arc_deci(12,	end,	false).
arc_deci(11,	end,	false).
arc_deci(10,	11,	false).
arc_deci(9,	10,	false).
arc_deci(start('bool.adb:64:11:wasmain'),	13,	true).
arc_deci(13,	14,	true).
arc_deci(14,	15,	true).
arc_deci(15,	16,	true).
arc_deci(16,	end,	true).
arc_deci(16,	end,	false).
arc_deci(15,	16,	false).
arc_deci(14,	15,	false).
arc_deci(13,	14,	false).
arc_deci(start('bool.adb:77:11:problem'),	17,	true).
arc_deci(17,	end,	true).
arc_deci(17,	end,	false).
arc_deci(start('bool.adb:87:11:allpath'),	18,	true).
arc_deci(18,	19,	true).
arc_deci(19,	end,	true).
arc_deci(19,	end,	false).
arc_deci(18,	19,	false).
arc_deci(start('elaboration'),	end,	true).
call_deci(8,	'bool.ads:4:13:exchange',	false).
call_deci(10,	'bool.adb:48:10:increment',	true).
call_deci(10,	'bool.adb:48:10:increment',	false).
*/

:- dynamic has_been_followed/2.

%%%30/05/06
%checks for the well-formedness of cfgs but does NOT check for:
%	-repeated arcs
%	-that nodes are not shared between subprograms
%	(these are assumed to be impossible by construction)
check_well_formedness :-
	mika_globals:mika_globals__set_NBT(current_check, branch),
	(\+ check_well_formedness2),
	mika_globals:mika_globals__set_NBT(current_check, decision),
	(\+ check_well_formedness2).
check_well_formedness2 :-
	check_arc(start(From_subprogram), To, Tv),
	(check_subprogram(From_subprogram, To, Tv) ->
		fail
	;
		(mika_globals:mika_globals__get_NBT(current_check, Current),
                 common_util__error(10, "Subprogram not well formed", no_error_consequences, [(check, check_subprogram(From_subprogram, To, Tv)), (current_check, Current)], 1013416, mika_coverage, check_well_formedness2, no_localisation, "the cfg is wrong")
                )
	).
check_arc(From, To, Tv) :-
	mika_globals:mika_globals__get_NBT(current_check, Current),
	(Current == branch ->
		(!,
		 arc_bran(From, To, Tv)
		)
	;
	 Current == decision ->
		(!,
		 arc_deci(From, To, Tv)
		)
	).
check_subprogram(From_subprogram, To, Tv) :-
	Tv == true,
	is_unique_arc(start(From_subprogram), To, _),
	follow(To, true),
	follow(To, false).

follow(end, _):-
	!,
        does_not_exist(end, _, _),
	!.

follow(From, Tv) :-
        (has_been_followed(From, Tv) ->
                true
        ;
                (is_unique_arc(From, To, Tv),
	         (To == end ->
		        (does_not_exist(end, _, _)
		        )
	         ;
	          To > From ->	%it isn't a back arc created for a loop
		        (follow(To, true),
		         follow(To, false)
		        )
	         ;
		        true
	         ),
                 assert((has_been_followed(From, Tv) :- !))
                )
        ).


does_not_exist(From, To, Tv) :-
	(check_arc(From, To, Tv) ->
                (mika_globals:mika_globals__get_NBT(current_check, Current),
                 common_util__error(10, "arc does not exist", no_error_consequences, [(call, does_not_exist(From, To, Tv)), (current_check, Current)], 1017719, mika_coverage, does_not_exist, no_localisation, "An arc is missing in built cfg")
                )
	;
		true
	).
is_unique_arc(From, To, Tv) :-
	%if it does not exist or is not unique we abort
	(check_arc(From, To, Tv) ->
		true
	;
		(mika_globals:mika_globals__get_NBT(current_check, Current),
                 common_util__error(10, "arc does not exist", no_error_consequences, [(call, not_unique_arc(From, To, Tv)), (current_check, Current)], 1018823, mika_coverage, is_unique_arc, no_localisation, "An arc is missing in built cfg")
                )
	),
	(not_unique_arc(From, To, Tv) ->
		(mika_globals:mika_globals__get_NBT(current_check, Current),
                 common_util__error(10, "arc is not unique", no_error_consequences, [(call, not_unique_arc(From, To, Tv)), (current_check, Current)], 1019326, mika_coverage, is_unique_arc, no_localisation, "An arc is not unique in built cfg")
                )
	;
		true
	).
not_unique_arc(From, To, Tv) :-
	check_arc(From, To2, Tv),
	To \= To2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%END%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%