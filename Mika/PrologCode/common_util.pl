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
% common_util.pl
% defines module common_util.pl
% common utilities to Mika and the Solver
%  in an ideal world the solver should be completely separate : but here are a few exceptions for expediency
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(common_util, [ common_util__create_dummy_name/1,
                         common_util__error/9
		       ]
         ).
:- use_module(library(lists)).  %for append/3
:- use_module([mika_globals]).
:- mika_globals:mika_globals__set_NBT(dummy_nb, 1).		%dummy counter used to create dummy variables (e.g. dummy types)
:- mika_globals:mika_globals__set_NBT(already_printed, []).  %list of already printed error messages : used in release mode
%%%
%The idea behind this complex error message predicate is to allow filtering of warning messages according to severity and allow fine control over the information displayed (e.g. different details for developpers and users)
%any optional argument can be ommitted by appending 'no_' in front of the corresponding parameter name
%e.g. common_util__error(7, "Generic package instantiation to be implemented", "The package is unhandled", [("package's name", Name)], 709, mika_symbolic, exec, exec(generic_package_instantiation(...)), "scheduled to be fixed in version 2.4")
%e.g. common_util__error(7, "Generic package instantiation to be implemented", no_error_consequences, no_arguments, 709, mika_symbolic, exec, no_localisation, no_extra_info)
%TEMPLATE : common_util__error(Error_severity, Error_message, no_error_consequences, no_arguments, Error_code, From_module, From_predicate, no_localisation, no_extra_info)

%Error_severity between 0 to 10 : 10 is the highest error level and denotes a fatal error (cannot proceed any further : abort),
%                                      the arguments are always printed out at level 10
%                                    at level 0 is for debug only : e.g. Path info messages that indicate the path that is being followed
%                                    at level 4 and above soundness may be affected or efficiency impeded
%                                       from level 1 to 9 the arguments are only printed in release mode if surrounded by 'print'
%                                    1 is the lowest warning level with no consequences at all
%Error_message : a string to explain to developers and users what the error is and why it was raised
%Error_consequences : optional, a string to explain potential consequences
%ArgumentsL : optional, a list of pairs (Description, Item)
%Error_code : an integer code starting with the Error_severity that is unique per call to common_util__error (to be used in 'find in files')
%From_module : an atom denoting the originating module
%From_predicate : an atom denoting the originating predicate
%Localisation : optional, an atom or a compound term providing further localisation information
%Extra_info : optional, a string of anything you want
common_util__error(Error_severity, Error_message, Error_consequences, ArgumentsL, Error_code, From_module, From_predicate, Localisation, Extra_info) :-
        mika_globals:mika_globals__get_NBT(message_mode, Message_mode),
        (Message_mode == debug ->
                (mika_globals:mika_globals__get_NBT(errorMessageNb, ErrorNb),
                 ErrorNb1 is ErrorNb + 1,
                 %(ErrorNb1 == 188 -> trace ; true),
                 format(user_error, "Error Nb: ~w", [ErrorNb1]),
                 mika_globals:mika_globals__set_NBT(errorMessageNb, ErrorNb1),
                 common_util__error2(Error_severity, Error_message, Error_consequences, ArgumentsL, Error_code, From_module, From_predicate, Localisation, Extra_info)
                )
        ;
                (mika_globals:mika_globals__get_NBT(already_printed, Already_printed),
                 (memberchk(Error_code, Already_printed) ->
                        true
                 ;
                        ((Error_severity == 1 ->
                                true
                         ;
                                mika_globals:mika_globals__set_NBT(already_printed, [Error_code|Already_printed])    %only done if non debug and for errors severity > 1
                         ),
                         common_util__error2(Error_severity, Error_message, Error_consequences, ArgumentsL, Error_code, From_module, From_predicate, Localisation, Extra_info)
                        )
                 )
                )
        ).

common_util__error2(10, Error_message, Error_consequences, ArgumentsL, Error_code, From_module, From_predicate, Localisation, Extra_info) :-
        !,
        mika_globals:mika_globals__get_NBT(message_mode, Message_mode),
        (Message_mode == debug ->
                (format(user_error, "~2n###################################~n", []),
                 format(user_error, "=>MIKA: a fatal error has occurred~n", []),
                 format(user_error, "        Error Code: ~w~n", [Error_code]),
                 format(user_error, "        Message: ~s~n", [Error_message]),
                 (ArgumentsL == no_arguments ->
                        true
                 ;
                        (format(user_error, "        Arguments: ", []),
                         cue_print_error_arguments(ArgumentsL, debug)
                        )
                 ),
                 (Error_consequences == no_error_consequences ->
                        true
                 ;
                        format(user_error, "        Consequences: ~s~n", [Error_consequences])
                 ),
                 format(user_error, "        Localisation: in ~w module, at ~w predicate", [From_module, From_predicate]),
                 (Localisation == no_localisation ->
                        format(user_error, "~n", [])
                 ;
                        format(user_error, ", for ~w case~n", [Localisation])
                 ),
                 (Extra_info == no_extra_info ->
                        true
                 ;
                        format(user_error, "        Extra Info: ~s~n", [Extra_info])
                 ),
                 %debugging information
                 format(user_error, "        Debugging Info: ~n", []),
                 mika_globals:mika_globals__get_NBT(debug_info, Current),
                 format(user_error, "            Was processing the ~w entity when error occurred~n", [Current]),
	         format(user_error, "            To help debugging, path information prior to error follows ...~n", []),
	         mika_globals:mika_globals__get_BT_path(current_path_bran, Current_path_bran),
	         format(user_error, "            Branches followed prior to error : ~w~n", [Current_path_bran]),
	         mika_globals:mika_globals__get_BT_path(current_path_deci, Current_path_deci),
	         format(user_error, "            Decisions followed prior to error : ~w~n", [Current_path_deci]),
                 mika_globals:mika_globals__get_BT_path(current_path_mcdc_gate, Current_path_mcdc_gate),
	         format(user_error, "            Gates followed prior to error : ~w~n", [Current_path_mcdc_gate]),
	         mika_globals:mika_globals__get_BT_path(current_path_cond, Current_path_cond),
	         format(user_error, "            Conditions followed prior to error : ~w~n", [Current_path_cond]),
                 abort
                )
        ;
                (format(user_error, "~2n###################################~n", []),
                 format(user_error, "=>MIKA: a fatal error has occurred~n", []),
                 format(user_error, "        Error Code: ~w~n", [Error_code]),
                 format(user_error, "        Message: ~s~n", [Error_message]),
                 (ArgumentsL = no_arguments ->
                        true
                 ;
                        (format(user_error, "        Arguments: ", []),
                         cue_print_error_arguments(ArgumentsL, release)
                        )
                 ),
                 format(user_error, "=>Report error to http://www.midoan.com/support.html to have it addressed.~n", []),
                 halt(Error_code)
                )
        ).

common_util__error2(0, Error_message, Error_consequences, ArgumentsL, _Error_code, From_module, From_predicate, Localisation, Extra_info) :-
        !,
        mika_globals:mika_globals__get_NBT(message_mode, Message_mode),
        (Message_mode == debug ->
                (format(user_error, "~s", [Error_message]),
                 (ArgumentsL == no_arguments ->
                        true
                 ;
                        cue_print_warning_arguments(ArgumentsL, debug)
                 ),
                 (Error_consequences == no_error_consequences ->
                        true
                 ;
                        format(user_error, ", ~s", [Error_consequences])
                 ),
                 format(user_error, " in ~w module, at ~w predicate", [From_module, From_predicate]),
                 (Localisation == no_localisation ->
                        true
                 ;
                        format(user_error, ", for ~w case", Localisation)
                 ),
                 (Extra_info == no_extra_info ->
                        format(user_error, "~n", [])
                 ;
                        format(user_error, ", ~s~n", [Extra_info])
                 )
                )
        ;
                true
        ).

common_util__error2(Error_severity, Error_message, Error_consequences, ArgumentsL, Error_code, From_module, From_predicate, Localisation, Extra_info) :-
        mika_globals:mika_globals__get_NBT(message_mode, Message_mode),
        (Message_mode == debug ->
                (format(user_error, "MIKA warning level ~w, ~w, ~s", [Error_severity, Error_code, Error_message]),
                 (ArgumentsL == no_arguments ->
                        true
                 ;
                        cue_print_warning_arguments(ArgumentsL, debug)
                 ),
                 (Error_consequences == no_error_consequences ->
                        true
                 ;
                        format(user_error, ", ~s", [Error_consequences])
                 ),
                 format(user_error, " in ~w module, at ~w predicate", [From_module, From_predicate]),
                 (Localisation == no_localisation ->
                        true
                 ;
                        format(user_error, ", for ~w case", Localisation)
                 ),
                 (Extra_info == no_extra_info ->
                        format(user_error, "~n", [])
                 ;
                        format(user_error, ", ~s~n", [Extra_info])
                 )
                )
        ;
                ((Error_severity == 1 ->
                        format(user_error, "~s~n", [Error_message])
                 ;
                        format(user_error, "MIKA warning level ~w ~w ~s~n", [Error_severity, Error_code, Error_message])
                 ),
                 (ArgumentsL == no_arguments ->
                        true
                 ;
                        cue_print_warning_arguments(ArgumentsL, release)
                 )
                )
        ),
        flush_output(user_error).


cue_print_warning_arguments(Warning_list, Debug) :-
        (Warning_list == [] ->
                true
        ;
                (Warning_list = [Warning|Rest],
                 (((Debug == debug , Warning = (Entry, Argument)) ; Warning = print(Entry, Argument)) ->
                        format(user_error, ", ~w : ~p", [Entry, portray(Argument)])
                 ;
                        true    %ignored
                 ),
                 cue_print_warning_arguments(Rest, Debug)
                )
        ).

cue_print_error_arguments([(Entry, Argument)|R], Debug) :-
        !,
        (Debug == debug ->
                format(user_error, "~w : ~p~n", [Entry, portray(Argument)])
        ;
                format(user_error, "~w : ~w~n", [Entry, Argument])
        ),
        cuepea_print_error_arguments_rest(R, Debug).

cuepea_print_error_arguments_rest([], _).
cuepea_print_error_arguments_rest([(Entry, Argument)|R], Debug) :-
        (Debug == debug ->
                format(user_error, "                   ~w : ~p~n", [Entry, portray(Argument)])
        ;
                format(user_error, "                   ~w : ~w~n", [Entry, Argument])
        ),
        cuepea_print_error_arguments_rest(R, Debug).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%generate a unique dummy name (e.g. for dummy types)
common_util__create_dummy_name(Dummy_name) :-
        mika_globals:mika_globals__get_NBT(dummy_nb, Dummy_nb),
        atom_codes(dummy, K),
        number_codes(Dummy_nb, L),
        append(K, L, M),
        atom_codes(Dummy_name, M),
        New_dummy_nb is Dummy_nb + 1,
        mika_globals:mika_globals__set_NBT(dummy_nb, New_dummy_nb).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%checks whether a term represents an exception (of the form exception(ExceptionName, ListOfCoumpounds] e.g. exception(Exception, [type(Result_type), array(Array), index(Index), predicate(get_element_ground2), message("Run-time constraint error in array access")]) )
common_util__is_an_exception(T) :-
        compound(T),
        T =.. ['exception'|_Arguments].
%checks whether a term represents a goto (of the form goto(Label))
common_util__is_a_goto(T) :-
        compound(T),
        T =.. ['goto'|_Arguments].
%checks whether a term represents a exit_named_loop (of the form exit_named_loop(To_name))
common_util__is_a_exit_named_loop(T) :-
        compound(T),
        T =.. ['exit_named_loop'|_Arguments].