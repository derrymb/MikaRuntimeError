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
% mika_print.pl
% defines the module mika_print
% printing module implements the report printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ATTENTION
% This module is much more difficult than it seems; changing things can affect the run-time dramatically
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(mika_print, []).

:- use_module([library(lists),	%Sicstus list library
               library(random)	%Sicstus random library
             ]).
:- mika_globals:mika_globals__set_NBT('comment_out', 'no').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mika_print__unhandled([]) :-
        !.
mika_print__unhandled(VarL) :-
        mika_globals:mika_globals__get_NBT('debug_mode', Debug_mode),  %debug or release
        mp_print_unhandled_list(VarL, Debug_mode).
%%%
        mp_print_unhandled_list([U|Rest], Debug_mode) :-
                mika_unhandled_atts:mika_unhandled_atts__get('name', U, Name),
                (Name == 'unhandled_expression' ->        %can happen in symbolically_interpret
                        true
                ;
                 Debug_mode == 'debug' ->
                        format('answer_output', "~s", [Name])
                ;
                        (mika_symbolic:mika_symbolic__parse(Name, _File_name, _File_extension, _Line, _Column, Id),
                         format('answer_output', "~s", [Id])
                        )
                ),
                (Rest == [] ->
                        format('answer_output', "\n", [])
                ;
                        (format('answer_output', ", ", []),
                         mp_print_unhandled_list(Rest, Debug_mode)
                        )
                ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%used for list of unused seav variables in prepare_mika_report predicate
mika_print__list_of_vars([]).
mika_print__list_of_vars([Next|Rest]) :-
        print_seav(Next),
        (Rest == [] ->
                format('answer_output', "\n", [])
        ;
                (format('answer_output', ", ", []),
                 mika_print__list_of_vars(Rest)
                )
        ).
%%%
        print_seav(V) :-
                (mika_seav_atts:mika_seav_atts__is_seav(V) ->
                        (mika_seav_atts:mika_seav_atts__get(name, V, Name),
                         format('answer_output', "~s", [Name])
                        )
                ;
                        common_util:common_util__error(10, "Printing a non SEAV var", "Cannot proceed", no_arguments, 1078103, mika_print, print_seav, no_localisation, "Should never occur")
                ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%used for list of branch, decision or condition not covered
mika_print__list([]).   %may be called with an empty list
mika_print__list([Next|Rest]) :-
        format('answer_output', "~w", Next),
        (Rest == [] ->
                format('answer_output', "\n", [])
        ;
                (format('answer_output', ", ", []),
                 mika_print__list(Rest)
                )
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Kind is either 'input_value' or 'constraint' denoting the range we want to print out
mika_print__ranges([], _).
mika_print__ranges([SEAV|Rest], Kind) :-
        %%DEBUG!!!!
        %(Kind = constraint ->
        %       spy [print_range/2]
        %;
        %       nospy [print_range/2]
        %),
        print_seav(SEAV),
        format('answer_output', " = ", []),
        mika_seav_atts:mika_seav_atts__get(Kind, SEAV, Value),
        midoan_solver:midoan_solver__get_type_from_value(Value, Type),	%because they may well be not SEAV variables (e.g. solver variables from array components)
        print_range(Type, Value),               %prints the actual range
        format('answer_output', "\n", []),
        mika_print__ranges(Rest, Kind).

print_range('ground', Value) :-
        !,
        format('answer_output', "~w", [Value]).
print_range('base_enumeration', Value) :-
        !,
        (midoan_enum:midoan_enum__ground(Value) ->
                (midoan_enum:midoan_enum__get('name', Value, Name),
                 format('answer_output', "~w", [Name])
                )
        ;
                (midoan_enum:midoan_enum__get_range_literal(Value, Min_lit, Max_lit),
	         midoan_enum:midoan_enum__get('name', Min_lit, Min_name),
	         midoan_enum:midoan_enum__get('name', Max_lit, Max_name),
                 format('answer_output', "~a..~a", [Min_name, Max_name])
                )
        ).
print_range(array(_), Value) :-
        !,
        format('answer_output', "[", []),
        midoan_array:midoan_array__get_all_index_elements(Value, Indice_elements),
        print_array_range(Indice_elements),
        format('answer_output', "]", []).
print_range('record', Value) :-
        !,
        format('answer_output', "(", []),
        midoan_record:midoan_record__get_all_field_values(Value, Field_values),
        print_record_range(Field_values),
        format('answer_output', ")", []).
print_range('standard.ads:integer', Value) :-   %an integer var
        !,
        midoan_solver:midoan_solver__integer_range(Value, Min, Max),
        format('answer_output', "~d..~d", [Min, Max]).
print_range('standard.ads:float', Value) :-     %a float var
        !,
        midoan_solver:midoan_solver__real_min(Value, Inf, T1),
        (T1 == 'taken' ->
                format('answer_output', '[', [])       %lower bound included
        ;
                format('answer_output', ']', [])       %lower bound excluded
        ),
        midoan_solver:midoan_solver__real_max(Value, Sup, T2),
        format('answer_output', "~f..~f", [Inf, Sup]),
        (T2 == 'taken' ->
                format('answer_output', ']', [])       %upper bound included
        ;
                format('answer_output', '[', [])       %upper bound excluded
        ).
print_range('modular_integer', Value) :-   %a modular integer var
        !,
        midoan_solver:midoan_solver__modular_integer_range(Value, Min, Max),
        format('answer_output', "~d..~d", [Min, Max]).
print_range(Type, _) :-
        !,
        common_util:common_util__error(10, "Trying to print the range of a variable of unknown type", "Cannot proceed", [(type, Type)], 1019846, mika_print, print_range, no_localisation, "Should never occur").
%%%
        %prints the range of records
        %identical structure to print_record_solution/1
        print_record_range([]) :-
                format('answer_output', "null record", []).
        print_record_range([(_, Element)|Rest]) :-
                midoan_solver:midoan_solver__get_type_from_value(Element, Type),      %because they may well be not SEAV variables (e.g. solver variables from array components)
                print_range(Type, Element),
                (Rest == [] ->
                        true
                ;
                        (format('answer_output', ", ", []),
                         print_record_range(Rest)
                        )
                ).
%%%
        %prints the range of arrays (the type of the elements is identical for all elements)
        %identical structure to print_array_solution/1
        print_array_range([]) :-        %an empty array
                format('answer_output', " ", []).
        print_array_range([(_, Element)|Rest]) :-
                midoan_solver:midoan_solver__get_type_from_value(Element, Type),
                print_array_range2([(_, Element)|Rest], Type).
%%%
                print_array_range2([(_, Element)|Rest], Type) :-
                        print_range(Type, Element),
                        (Rest == [] ->
                                true
                        ;
                                (format('answer_output', ", ", []),
                                 print_array_range2(Rest, Type)
                                )
                        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%prints the effect 'symbolic' or 'constraint' of traversal (actions) on the list of output variables
mika_print__effects([], _).
mika_print__effects([SEAV|Rest], Kind) :-
        print_seav(SEAV),
        mika_seav_atts:mika_seav_atts__get(Kind, SEAV, Value_kind),
	%Value_Kind can be enormous (100000s of characters) even for simple examples
	%  ~w prints it our entirely and on a single line ... and therefore may not display properly in your text editor [try to ensure that word wrap is enabled]
	%  a solution could be to use ~p which goes via portray (could be done for everything printed out)
	%    this would allow us to control the way things are printed
        format('answer_output', " := ~w\n", [Value_kind]),
        mika_print__effects(Rest, Kind).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Kind is either 'input_value' (vars representing the test inputs) or 'constraint' (vars representing the expected outputs)
%  print the solution which is, by definition, composed of ground variables (but possibly composite such as array or records)
%  write to foo_mika_tests.txt and to the appropriate test point
%It is the ONLY mika_print predicate that also prints to the test driver
%it is called twice : once for the test inputs then for the expected outputs
%for expected outputs ('Kind == constraint') the SEAV may be uninitialised, and may be floats (which requires printing out individual composite types elements)
%%%
%REM this is quite a complex predicate ...
%REM about 300 lines of code ...
%REM could be simplified : atom_concats, a lot of repeated code,
%REM but be careful before breaking it!
mika_print__solutions([], _, _, _).
mika_print__solutions([SEAV|Rest], Kind, Subprogram_name, ParamL_driver) :-
	mika_seav_atts:mika_seav_atts__get('name', SEAV, Name),
        %(Name == 'ch8_6.ads:15:1:len2' -> trace ; true),
        mika_symbolic:mika_symbolic__parse(Name, File_name, _File_extension, _Line, _Column, Id),
        mika_globals:mika_globals__get_NBT('debug_mode', Debug_mode),  %debug or release
        (Debug_mode == 'debug' ->
                format('answer_output', "~s", [Name])
        ;
                format('answer_output', "~s", [Id])
        ),
	(memberchk(driver(Name, Driver_name, _), ParamL_driver) ->      %it is a parameter
                user:file_name_without_extension_to_package_name(File_name, Current_TP_stream)  %see issue "Does not work with krunched file names"
	;
		(Driver_name = Id,
                 (user:file_name_denotes_a_RTL_compilation_unit(File_name) ->
                        (common_util:common_util__error(8, "RTL variable needed for test driver: test driver cannot set or check RTL variables (see contents of the file 'dummy_mika_test_point.txt'); ignoring the calling context of the subprogram under test should not raise this warning", "Some expected output will not be checked or the test driver will not compile", [(kind, Kind), (id, Id), (File_name, file_name)], 1024732, mika_print, mika_print__solutions, no_localisation, no_extra_info),
                         (stream_property(_, alias(dummy_mika_test_point)) ->	%to detect if our dummy_mika_test_point stream was created
		                true            %it already exists
   	                 ;
        	                user:create_dummy_mika_test_point
   	                 ),
                         Current_TP_stream = 'dummy_mika_test_point'       %the dummy test point for RTL variables
                        )
                 ;
                        user:file_name_without_extension_to_package_name(File_name, Current_TP_stream)  %see issue "Does not work with krunched file names"
                 )
		)
	),
        mika_globals:mika_globals__set_NBT('current_TP_stream', Current_TP_stream),
        %%%Jan/09
        mika_seav_atts:mika_seav_atts__get('type', SEAV, Seav_type),
        is_special_type(Seav_type, Is_special_type),    %true(Wrap, Unwrap)|false
        %%%
        mika_seav_atts:mika_seav_atts__get(Kind, SEAV, Value),
        (Kind == 'constraint' ->
                (format(Current_TP_stream, "      --     ", []),	%commented out non initialiased value (still printed for expidency)
                        mika_globals:mika_globals__set_NBT('comment_out', 'yes'),			%ugly non logical var
                        print_solution_allinit(Value, Kind, Driver_name, Is_special_type, Current_TP_stream),
                        mika_globals:mika_globals__set_NBT('comment_out', 'no'),
                        format(Current_TP_stream, ";\n", []),
                        print_solution_noninit_or_floats(Value, Driver_name, Is_special_type, Current_TP_stream)		%individualises the elements of composite types
                )
        ;
         Kind == 'input_value' ->
                (format(Current_TP_stream, "      ", []),	%for indentation purposes only
                        print_solution_allinit(Value, Kind, Driver_name, Is_special_type, Current_TP_stream),
                        format(Current_TP_stream, ";\n", [])
                )
        ),
        format('answer_output', "\n", []),
        mika_print__solutions(Rest, Kind, Subprogram_name, ParamL_driver).
%%%
        % this is only necessary for the test driver in the case of RR code at the moment
        % don't know how general the problem is ...added to issue document 23/04/09
        is_special_type(Type_var, Special) :-
                midoan_type:midoan_type__get_typemark(Type_var, Type_name),
                (mika_symbolic:mika_symbolic__parse(Type_name, 'system', 'ads', _, _, 'address') ->       %any line any column
                        Special = true('To_Address', 'to_integer')      %was System.To_Address
                ;
                        Special = 'false'
                ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the entire Value has been initialised (e.g. all the elements of an array are ground)
print_solution_allinit(Value, Kind, Driver_name, Is_special_type, Current_TP_stream) :-
	midoan_solver:midoan_solver__get_type_from_value(Value, Type),
        format('answer_output', " = ", []),
        format(Current_TP_stream, "~w", [Driver_name]),
        (Kind == 'input_value' ->
	        format(Current_TP_stream, " := ", [])
        ;
         Kind == 'constraint' ->	%for output values
	        format(Current_TP_stream, " = ", [])
        ),
        print_solution(Type, Value, Is_special_type, Current_TP_stream).

print_solution(array(_), Value, Is_special_type, Current_TP_stream) :-
        !,                      % an array
        midoan_array:midoan_array__get_all_index_elements(Value, Indice_elements),
        midoan_array:midoan_array__get_type_var(Value, Type_var),
        midoan_type:midoan_type__obtain_basetype(Type_var, Array_basetype),
        (Array_basetype = array(Component_type_var) ->
                true    %just to retrieve the Component_type_var
        ;
         Array_basetype = unconst_array(Component_type_var) ->
                true    %just to retrieve the Component_type_var
        ;
                common_util:common_util__error(10, "Unknow array basetype", no_error_consequences, [(array_basetype, Array_basetype)], 1086895, mika_print, print_solution, no_localisation, no_extra_info)
        ),
        is_special_type(Component_type_var, Is_special_type),                  %true|false
        (Indice_elements == [] ->       %an empty array: we have a problem here we have to build an empty aggregate of the form
                                        % '(array_type'first..array_type'last [, repeated for all dimensions] => an_element)'
                (format(Current_TP_stream, "(", []),
                 format(answer_output, "(", []),
                 ps_print_indexes_range(Type_var, Current_TP_stream),                             %prints 'array_type'first..array_type'last'
                 format(Current_TP_stream, " => ", []),
                 format(answer_output, " => ", []),
                 midoan_type:midoan_type__obtain_parenttype(Component_type_var, Component_basetype_var),        %changed 26/11/09 was obtain_basetype
                 midoan_type:midoan_type__variable_declaration(Element_var, Component_basetype_var),       %this is a free variable
                 mika_symbolic:partition_atomic([Element_var], IL, RL, EL),                                             %it needs to be given a random, valid, value
                 midoan_labeling:midoan_labeling__enums(EL),
                 midoan_labeling:midoan_labeling__integers(IL),
                 midoan_labeling:midoan_labeling__reals(RL),
                 !,
                 midoan_solver:midoan_solver__get_type_from_value(Element_var, Type_element_var),
                 print_solution(Type_element_var, Element_var, Is_special_type, Current_TP_stream),
                 format(Current_TP_stream, ")", []),
                 format(answer_output, ")", [])
                )
        ;
                ps_print_array_solution(Indice_elements, Is_special_type, Current_TP_stream)
        ).
print_solution('record', Value, Is_special_type, Current_TP_stream) :-
        !,                      % a record
        format('answer_output', "(", []),
        (Is_special_type = true(Wrap, _Unwrap) ->
                format(Current_TP_stream, "~w(", Wrap)
        ;
        	format(Current_TP_stream, "(", [])
        ),
        retrieve_type_field_values(Value, Type_field_values),
        (Type_field_values == [] ->
                (format('answer_output', "null record", []),
	         format(Current_TP_stream, "null record", [])
                )
        ;
                print_record_solution(Type_field_values, Current_TP_stream)
        ),
        format('answer_output', ")", []),
	format(Current_TP_stream, ")", []).
print_solution('base_enumeration', Value, Is_special_type, Current_TP_stream) :-
	!,			%an enumeration literal
	(midoan_enum:midoan_enum__ground(Value) ->
		(get_enum_representation(Value, Is_a_char, Full_name, Representation),  %e.g. Full_name is date.ads:2:49:sun, Representation is Sun
                 (Is_a_char == 'yes' ->
                        (format('answer_output', "'~w'", Representation),
		         (Is_special_type = true(Wrap, _Unwrap) ->
                                format(Current_TP_stream, "~w('~w')", [Wrap, Representation])
                         ;
                                format(Current_TP_stream, "'~w'", Representation)
                         )
                        )
                 ;
                        (mika_globals:mika_globals__get_NBT(debug_mode, Debug_mode),  %debug or release
                         (Debug_mode == 'debug' ->
                                format('answer_output', "~w", Full_name)
                         ;
                                format('answer_output', "~w", Representation)
                         ),
		         (Is_special_type = true(Wrap, _Unwrap) ->
                                format(Current_TP_stream, "~w(~w)", [Wrap, Representation])
                         ;
                                format(Current_TP_stream, "~w", Representation)
                         )
                        )
                 )
                )
	;
		(%an unknown enumeration output value (non-initialised variable)
		 format('answer_output', "_", []),
		 format(Current_TP_stream, "_", [])
		)
	).
print_solution('modular_integer', Value, Is_special_type, Current_TP_stream) :-
	!,
	(midoan_modular_integer:midoan_modular_integer__ground(Value) ->
		(midoan_modular_integer:midoan_modular_integer__get('value', Value, Value_value),
		 format('answer_output', "~w", Value_value),
                 (Is_special_type = true(Wrap, _Unwrap) ->
                        format(Current_TP_stream, "~w(~w)", [Wrap, Value_value])
                 ;
                        format(Current_TP_stream, "~w", Value_value)
                 )
                )
	;
		(%an unknown modular integer output value (non-initialised variable)
		 format('answer_output', "_", []),
		 format(Current_TP_stream, "_", [])
		)
	).
print_solution('ground', Value, Is_special_type, Current_TP_stream) :-
        !,                      %a number
        format('answer_output', "~w", Value),
        (Is_special_type = true(Wrap, _Unwrap) ->
                format(Current_TP_stream, "~w(~w)", [Wrap, Value])
        ;
                format(Current_TP_stream, "~w", Value)
        ).
%an unknown output value (non-initialised variable)
print_solution(_, _, _Is_special_type, Current_TP_stream) :-
	!,
	format('answer_output', "_ ", []),
	format(Current_TP_stream, "_ ", []).

%%%
        %the out Type_field_values is of the form [(Type_name, Field_name, Value)*]
        retrieve_type_field_values(Value, Type_field_values) :-
                midoan_record:midoan_record__get_type(Value, Type_var),
                midoan_type:midoan_type__get_attribute(Type_var, field_types, Template_FieldL),         %Template_FieldL is of the form [(Field_namesL, Field_type_name, no_init)*]
                midoan_record:midoan_record__get_all_field_values(Value, Field_values),                        %Field_values is of the form [(Field_name, Value)*]
                build_type_field_values(Template_FieldL, Field_values, Type_field_values).
%%%
                build_type_field_values([], [], []).
                build_type_field_values([(Field_namesL, Field_type_name, _)|Rest_Template_FieldL], Field_values, Type_field_values) :-
                        build_type_field_values2(Field_namesL, Field_type_name, Field_values, Field_values_Rest, Type_field_values1),
                        build_type_field_values(Rest_Template_FieldL, Field_values_Rest, Type_field_values2),
                        append(Type_field_values1, Type_field_values2, Type_field_values).
%%%
                        build_type_field_values2([], _Field_type_name, Field_values_Rest, Field_values_Rest, []).
                        build_type_field_values2([Field_name|Rest_Field_names], Field_type_name, [(Field_name, Value)|Rest_Field_values], Field_values_Rest, [(Field_type_name, Field_name, Value)|Type_field_values_Rest]) :-
                                build_type_field_values2(Rest_Field_names, Field_type_name, Rest_Field_values, Field_values_Rest, Type_field_values_Rest).
%%%
        %prints the values of records
        %identical structure to print_record_range/1
        print_record_solution([(Type_name, Field, Element)], Current_TP_stream) :-    %single component record
                !,
                mika_symbolic:mika_symbolic__parse(Field, _File_name, _File_extension, _Line, _Column, Field_id),
                format('answer_output', "~w => ", [Field_id]),
                format(Current_TP_stream, "~w => ", [Field_id]),
                midoan_solver:midoan_solver__get_type_from_value(Element, Type),
                is_special_type(Type_name, Is_special_type),
                print_solution(Type, Element, Is_special_type, Current_TP_stream).
        print_record_solution(Type_field_valuesL, Current_TP_stream) :-
                !,
                print_multi_record_solution(Type_field_valuesL, Current_TP_stream).
%%%
                print_multi_record_solution([(Type_name, _, Element)|Rest], Current_TP_stream) :-
                        is_special_type(Type_name, Is_special_type),
                        midoan_solver:midoan_solver__get_type_from_value(Element, Type),
                        print_solution(Type, Element, Is_special_type, Current_TP_stream),
                        (Rest == [] ->
                                true
                        ;
                                (format('answer_output', ", ", []),
                                 format(Current_TP_stream, ", ", []),
                                 break_lines(Current_TP_stream),
                                 print_multi_record_solution(Rest, Current_TP_stream)
                                )
                        ).
%%%
        ps_print_indexes_range(Type_var, Current_TP_stream) :-
                midoan_type:midoan_type__get_attribute(Type_var, 'dimensions', Dimensions),
                pspir_print_indexes_range(Type_var, 1, Dimensions, Current_TP_stream).
%%%
                pspir_print_indexes_range(Type_var, Dim, Dimensions, Current_TP_stream) :-
                        (Dim =< Dimensions ->
                                (is_special_type(Type_var, Is_special_type),
                                 midoan_type:midoan_type__get_attribute(Type_var, first(Dim), Min),
                                 midoan_type:midoan_type__get_attribute(Type_var, last(Dim), Max),
                                 midoan_solver:midoan_solver__get_type_from_value(Min, Type_index),
                                 print_solution(Type_index, Min, Is_special_type, Current_TP_stream),
                                 format(Current_TP_stream, "..", []),
                                 format('answer_output', "..", []),
                                 print_solution(Type_index, Max, Is_special_type, Current_TP_stream),
                                 (Dim == Dimensions ->
                                        true
                                 ;
                                        (Dim_1 is Dim + 1,
                                         format(Current_TP_stream, ", ", []),
                                         format('answer_output', ", ", []),
                                         pspir_print_indexes_range(Type_var, Dim_1, Current_TP_stream)
                                        )
                                 )
                                )
                        ;
                                true
                        ).
%%%
        %even works for multidimentional single element array
        print_single_array_aggregate([F|Rest], Element, Is_special_type, Current_TP_stream) :-
                psaa_print_choice(F, Current_TP_stream),
                (Rest == [] ->
                        (midoan_solver:midoan_solver__get_type_from_value(Element, Type),
                         print_solution(Type, Element, Is_special_type, Current_TP_stream)
                        )
                ;
                        print_single_array_aggregate(Rest, Element, Is_special_type, Current_TP_stream)
                ),
                print_scope(1, [0')], Current_TP_stream).
%%%
                psaa_print_choice(F, Current_TP_stream) :-
                        print_scope(1, [0'(], Current_TP_stream),
                        midoan_solver:midoan_solver__get_type_from_value(F, Type_indice),
                        (Type_indice == 'ground' ->
                                Choice = F
                        ;
                         Type_indice == 'modular_integer' ->
                                midoan_modular_integer:midoan_modular_integer__get('value', F, Choice)
                        ;
                                (midoan_enum:midoan_enum__get('name', F, Name),
                                 mika_symbolic:mika_symbolic__parse(Name, _File_name, _File_extension, _Line, _Column, Choice)
                                )
                        ),
                        format(Current_TP_stream, "~w => ", Choice).
%%%
        %prints the solution of arrays (the type of the elements is identical for all elements)
        %printing multi-dimentional array aggregates modified 26/03/07
        %print_array_solution([([1,1], 1.1), ([1,2], 1.2), ([1,3], 1.3), ([2,1], 2.1), ([2,2], 2.2), ([2,3], 2.3)]).
        %print_array_solution([([1,1,1], 1.11), ([1,1,2], 1.12), ([1,2,1], 1.21), ([1,2,2], 1.22), ([1,3,1], 1.31), ([1,3,2], 1.32), ([2,1,1], 2.11), ([2,1,2], 2.12), ([2,2,1], 2.21), ([2,2,2], 2.22), ([2,3,1], 2.31), ([2,3,2], 2.32)]).
        %quite difficult to achieve ... but below works fine
        %  another solution would be to write a recursive parser in Prolog for this with action rules 'a la' yacc ...
        ps_print_array_solution([(First_inds, Element)], Is_special_type, Current_TP_stream) :-
                !,      %needed
                %a single element array : cannot use a single positional array aggregate (illegal in Ada) as the test driver will not compile
                %cannot even write (others => ...)
                print_single_array_aggregate(First_inds, Element, Is_special_type, Current_TP_stream).
        ps_print_array_solution([(First_inds, Element), (Next_inds, Next_element)|Rest], Is_special_type, Current_TP_stream) :-        %to help indexing we ensure at least two elements
                length(First_inds, Scope),
                print_scope(Scope, [0'(], Current_TP_stream),	%Prints first element of aggregate : opening brackets outputed
                midoan_solver:midoan_solver__get_type_from_value(Element, Type),
                print_solution(Type, Element, Is_special_type, Current_TP_stream),	%prints the actual element of the aggregate
                %29/03/07 normally we used to pass the type of the first element around since all the elements of an array are of the same type
                % but for outputs some elements of the array may actually be unitialised
                %so we check all elements ...
                pspas_print_array_solution2([(Next_inds, Next_element)|Rest], First_inds, Is_special_type, Current_TP_stream).
%%%
                pspas_print_array_solution2([(Inds, Element)], _First_inds, Is_special_type, Current_TP_stream) :- %Prints last element of aggregate
                        !,      %needed
                        format(Current_TP_stream, ",", []),
                        format('answer_output', ",", []),
                        midoan_solver:midoan_solver__get_type_from_value(Element, Type),
                        print_solution(Type, Element, Is_special_type, Current_TP_stream),	%prints the actual element of the aggregate
                        length(Inds, Scope),
                        print_scope(Scope, [0')], Current_TP_stream).
                pspas_print_array_solution2([(Inds, Element)|Rest], Previous_inds, Is_special_type, Current_TP_stream) :-
                        pspaspas_print_array_solution3((Inds, Element), Previous_inds, 0, Is_special_type, Current_TP_stream),
                        pspas_print_array_solution2(Rest, Inds, Is_special_type, Current_TP_stream).
%%%
                        pspaspas_print_array_solution3(([_Last], Element), _Previous_inds, Scope, Is_special_type, Current_TP_stream) :-
                                !,      %needed
                                print_scope(Scope, [0')], Current_TP_stream),	%prints 'Scope' closing brackets
                                format('answer_output', ",", []),
                                break_lines(Current_TP_stream),
                                format(Current_TP_stream, ",", []),
                                print_scope(Scope, [0'(], Current_TP_stream),	%prints 'Scope' opening brackets
                                midoan_solver:midoan_solver__get_type_from_value(Element, Type),
                                print_solution(Type, Element, Is_special_type, Current_TP_stream).	%prints the actual element of the aggregate
                        pspaspas_print_array_solution3(([First, Second|Rest_inds], Element), [Previous_first|Rest_previous], Scope, Is_special_type, Current_TP_stream) :-   %to help indexing we ensure at least two elements
                                (First == Previous_first ->
                                        pspaspas_print_array_solution3(([Second|Rest_inds], Element), Rest_previous, Scope, Is_special_type, Current_TP_stream)
                                ;
                                        (Scope_out is Scope + 1,
                                         pspaspas_print_array_solution3(([Second|Rest_inds], Element), Rest_previous, Scope_out, Is_special_type, Current_TP_stream)
                                        )
                                ).
%%%
        %prints the passed character C N times (format/2  ~Nc specificfier could not be used...)
        %for openning brackets C == [0'(]
        %for closing brackets C == [0')]
        print_scope(0, _, _Current_TP_stream) :-
                !.
        print_scope(N, C, Current_TP_stream) :-
                format(Current_TP_stream, "~c", C),
                format('answer_output', "~c", C),
                N1 is N - 1,
                print_scope(N1, C, Current_TP_stream).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%print an uninitialised value or one that contains floats to the test point
%only happens whenever Kind == 'constraint'
%only has to print to the test_point_output stream
print_solution_noninit_or_floats(Value, Driver_name, Is_special_type, Current_TP_stream) :-
        %trace,
	midoan_solver:midoan_solver__get_type_from_value(Value, Type),
	(Type = array(_) ->
		(midoan_array:midoan_array__get_all_index_elements(Value, Indice_elements),
                 midoan_array:midoan_array__get_type_var(Value, Type_var),
                 midoan_type:midoan_type__obtain_basetype(Type_var, Array_basetype),
                 (Array_basetype = array(Component_type_var) ->
                        true    %just to retrieve the Component_type_var
                 ;
                  Array_basetype = unconst_array(Component_type_var) ->
                        true    %just to retrieve the Component_type_var
                 ;
                        common_util:common_util__error(10, "Unknow array basetype", no_error_consequences, [(array_basetype, Array_basetype)], 1086894, mika_print, print_solution_noninit_or_floats, no_localisation, no_extra_info)
                 ),
                 is_special_type(Component_type_var, Is_special_type_component),                  %true(Wrap, Unwrap)|false
                 (Indice_elements == [] ->                                              %an empty array: this is messy we print : len2 = (1..0 => 'x')
                        ((Is_special_type = true(Wrap, _Unwrap) ->
                                true
                         ;
                                Wrap = ''
                         ),
                         format(Current_TP_stream, "      compare_expected(~w = ~w((", [Driver_name, Wrap]),
                         ps_print_indexes_range(Type_var, Current_TP_stream),        %prints 'array_type'first..array_type'last'
                         midoan_type:midoan_type__obtain_parenttype(Component_type_var, Component_basetype_var),          %[26/11/09] was obtain_basetype
                         midoan_type:midoan_type__variable_declaration(Element_var, Component_basetype_var),       %this is a free variable
                         mika_symbolic:partition_atomic([Element_var], IL, RL, EL),                                             %it needs to be given a random, valid, value
                         midoan_labeling:midoan_labeling__enums(EL),
                         midoan_labeling:midoan_labeling__integers(IL),
                         midoan_labeling:midoan_labeling__reals(RL),
                         !,
                         midoan_solver:midoan_solver__get_type_from_value(Element_var, Type_element_var),
                         format(Current_TP_stream, " => ", []),
                         print_solution(Type_element_var, Element_var, Is_special_type_component, Current_TP_stream),      %should only print to Current_TP_stream but does not
                         format(Current_TP_stream, ")), ""~w.~w"", "" a non empty array"", ""empty array"");\n", [Current_TP_stream, Driver_name])
                        )
                 ;
		        print_array_decomposed(Indice_elements, Driver_name, Value, Is_special_type_component, Current_TP_stream)
		 )
                )
	;
	 Type == 'record' ->
		(retrieve_type_field_values(Value, Type_field_values),
		 print_record_decomposed(Type_field_values, Driver_name, Current_TP_stream)
		)
	;
	 Type == 'base_enumeration' ->
		((midoan_enum:midoan_enum__ground(Value) ->
			(get_enum_representation(Value, Is_a_char, _Full_name, Representation),
                         midoan_enum:midoan_enum__get(type, Value, Enum_type),
                         midoan_type:midoan_type__obtain_parenttype(Enum_type, Parenttype),
                         midoan_type:midoan_type__get_typemark(Parenttype, Parenttype_typemark),
                         mika_symbolic:mika_symbolic__parse(Parenttype_typemark, _, _, _, _, Parenttype_id),
                         (Is_a_char == 'yes' ->
                                (Is_special_type = true(Wrap, Unwrap) ->
                                        format(Current_TP_stream, "      compare_expected(~w = ~w('~w'), ""~w.~w"", ~w'image(~w(~w(~w))), ""'~w'"");\n", [Driver_name, Wrap, Representation, Current_TP_stream, Driver_name, Parenttype_id, Parenttype_id, Unwrap, Driver_name, Representation])
                                ;
                                        format(Current_TP_stream, "      compare_expected(~w = '~w', ""~w.~w"", ~w'image(~w(~w)), ""'~w'"");\n", [Driver_name, Representation, Current_TP_stream, Driver_name, Parenttype_id, Parenttype_id, Driver_name, Representation])
                                )
                         ;
                                (Is_special_type = true(Wrap, Unwrap) ->
                                        format(Current_TP_stream, "      compare_expected(~w = ~w(~w), ""~w.~w"", ~w'image(~w(~w(~w))), ""~w"");\n", [Driver_name, Wrap, Representation, Current_TP_stream, Driver_name, Parenttype_id, Parenttype_id, Unwrap, Driver_name, Representation])
                                ;
                                        format(Current_TP_stream, "      compare_expected(~w = ~w, ""~w.~w"", ~w'image(~w(~w)), ""~w"");\n", [Driver_name, Representation, Current_TP_stream, Driver_name, Parenttype_id, Parenttype_id, Driver_name, Representation])
                                )
                         )
                        )
                 ;
                        format(Current_TP_stream, "      -- ~w is a free : no expected result : comparison is meaningless\n", [Driver_name])
                 )
                )
	;
	 Type == 'ground' ->	% a number
		((Is_special_type = true(Wrap, Unwrap) ->
                                (Actual_result =.. [Unwrap, Driver_name],
                                 Expected_value =.. [Wrap, Value]
                                )
                         ;
                                (Actual_result = Driver_name,
                                 Expected_value = Value
                                )
                 ),
                 (integer(Value) ->
                        (midoan_extensions:max_clpfd(Max_int),
                         midoan_extensions:min_clpfd(Min_int),
                         (Value == Max_int ->
                                format(Current_TP_stream, "      compare_expected(~w >= ~w, ""~w.~w"", Integer'image(integer(~w)), ""~w"");\n", [Driver_name, Expected_value, Current_TP_stream, Driver_name, Actual_result, Value])
                         ;
                          Value == Min_int ->
                                format(Current_TP_stream, "      compare_expected(~w <= ~w, ""~w.~w"", Integer'image(integer(~w)), ""~w"");\n", [Driver_name, Expected_value, Current_TP_stream, Driver_name, Actual_result, Value])
                         ;
                                format(Current_TP_stream, "      compare_expected(~w = ~w, ""~w.~w"", Integer'image(integer(~w)), ""~w"");\n", [Driver_name, Expected_value, Current_TP_stream, Driver_name, Actual_result, Value])
                         )
                        )
                 ;
                  float(Value) ->
                        format(Current_TP_stream, "      compare_expected(mika_TP_util.is_nearly_equal(float(~w), float(~w)), ""~w.~w"", Float'image(float(~w)), ""~w"");\n", [Driver_name, Expected_value, Current_TP_stream, Driver_name, Actual_result, Value])
		 ;
                  Value == 'null record' ->
                        format(Current_TP_stream, "      compare_expected(~w = (null record), ""~w.~w"", ~w, ""null record"");\n", [Driver_name, Current_TP_stream, Driver_name, Actual_result])
                 )
		)
	;
         Type == 'modular_integer' ->
                (midoan_modular_integer:midoan_modular_integer__get('value', Value, Value_value),
                 print_solution_noninit_or_floats(Value_value, Driver_name, Is_special_type, Current_TP_stream)
                )
        ;
	 (Type = 'standard.ads:integer' ; Type = 'standard.ads:float') ->		% a var
                format(Current_TP_stream, "      -- ~w is a free : no expected result : comparison is meaningless\n", [Driver_name])
        ;
         Type = 'anonymous_aggregate' ->
                common_util:common_util__error(10, "Trying to print the expected value of a 'anonymous_aggregate' variable", "Cannot proceed", [(current_TP_stream, Current_TP_stream), (driver_name, Driver_name), (value, Value)], 10103148, mika_print, print_solution_noninit_or_floats, no_localisation, "Should never occur")
        ;
         Type = 'string_literal' ->
                common_util:common_util__error(10, "Trying to print the expected value of a 'string_literal' variable", "Cannot proceed", [(current_TP_stream, Current_TP_stream), (driver_name, Driver_name), (value, Value)], 10103449, mika_print, print_solution_noninit_or_floats, no_localisation, "Should never occur")
        ;
         Type = 'free' ->
                common_util:common_util__error(10, "Trying to print the expected value of a 'free' variable", "Cannot proceed", [(current_TP_stream, Current_TP_stream), (driver_name, Driver_name), (value, Value)], 10103750, mika_print, print_solution_noninit_or_floats, no_localisation, "Should never occur")
        ;
         Type = Unhandled_type ->
                common_util:common_util__error(10, "Trying to print the expected value of a unhandled type variable", "Cannot proceed", [(unhandled_type, Unhandled_type), (current_TP_stream, Current_TP_stream), (driver_name, Driver_name), (value, Value)], 10104051, mika_print, print_solution_noninit_or_floats, no_localisation, "Should never occur")

	).
%%%
        print_array_decomposed([(Inds, Element)|Rest], Array_name, Value, Is_special_type_component, Current_TP_stream) :-
                util_create_indexed_name(Inds, Array_name, Updated_name),       %e.g. Updated_name is now 'array_name(3,2)'
                print_solution_noninit_or_floats(Element, Updated_name, Is_special_type_component, Current_TP_stream),
                (Rest == [] ->
                        true
                ;
                        print_array_decomposed(Rest, Array_name, Value, Is_special_type_component, Current_TP_stream)
                ).
%%%
        print_record_decomposed([(Type_name, Field, Value)|Rest], Name, Current_TP_stream) :-
                atom_concat(Name, '.', N1),
                mika_symbolic:mika_symbolic__parse(Field, _File_name, _File_extension, _Line, _Column, Id),
                atom_concat(N1, Id, Name2),
                is_special_type(Type_name, Is_special_type),
                print_solution_noninit_or_floats(Value, Name2, Is_special_type, Current_TP_stream),
                (Rest == [] ->
                        true
                ;
                        print_record_decomposed(Rest, Name, Current_TP_stream)
                ).
%%%
        break_lines(Current_TP_stream) :-
                line_position(Current_TP_stream, N),	%returns number of characters on current line for the stream
                (N > 100 ->
                        (mika_globals:mika_globals__get_NBT(comment_out, YN),
                         (YN == 'yes' ->
                                format(Current_TP_stream, "\n --", [])	%to break long lines in the test driver (must be less than 256 for gnat)
                         ;
                                format(Current_TP_stream, "\n", [])	%to break long lines in the test driver (must be less than 256 for gnat)
                         )
                        )
                ;
                        true
                ).
%%%
        %e.g. Full_name is date.ads:2:49:sun, Representation is sun
        get_enum_representation(Value, Is_a_char, Full_name, Representation) :-
                midoan_enum:midoan_enum__get('name', Value, Name),
                mika_symbolic:mika_symbolic__parse(Name, _File_name, _File_extension, _Line, _Column, Id),   %e.g. with Id == char__34
                (atom_concat('char__', Atom, Id) ->                                             % Atom == '34'
                        (%we have a character : print it rather than char__
                         %what a mess ...
                         Is_a_char = 'yes',
                         Full_name = Id,
                         atom_codes(Atom, CodeL),                                               % CodeL == [51, 52]
                         number_codes(Number, CodeL),                                           % Number == 34
                         char_code(Representation, Number)                                      % Representation == '"'
                        )
                ;
                        (Is_a_char = 'no',
                         Full_name = Name,
                         Representation = Id
                        )
                ).
%%%
        %e.g. util_create_indexed_name([3, 2], my_array, 'my_array(3,2)')
        util_create_indexed_name(Index, Start, Indexed_name) :-
                atom_concat(Start, '(', Start2),
                ucin_create_indexed_name(Index, Start2, End),
                atom_concat(End, ')', Indexed_name).
%%%
                ucin_create_indexed_name([First|Rest], Name_start, Indexed_name) :-
                        (integer(First) ->
                                util_number_atom(First, First_Atom)     %an integer
                        ;
                         midoan_enum:midoan_enum__is_enum(First) ->
                                (midoan_enum:midoan_enum__get('name', First, Enum_name),
                                 mika_symbolic:mika_symbolic__parse(Enum_name, _, _, _, _, First_Atom)
                                )
                        ;
                         midoan_modular_integer:midoan_modular_integer__is_modular_integer(First) ->
                                (midoan_modular_integer:midoan_modular_integer__get(value, First, Single_value),
                                 util_number_atom(Single_value, First_Atom)
                                )
                        ;
                                common_util:common_util__error(10, "Unexpected index", "Should never happen", [(first, First)], 10108010, mika_print, ucin_create_indexed_name, no_localisation, no_extra_info)
                        ),
                        atom_concat(Name_start, First_Atom, Tmp_name),
                        (Rest == [] ->
                                Indexed_name = Tmp_name
                        ;
                                (atom_concat(Tmp_name, ', ', New_start),
                                 ucin_create_indexed_name(Rest, New_start, Indexed_name)
                                )
                        ).
%%%
                        util_number_atom(Number, Atom) :-
                                number_codes(Number, CodeL),
                                atom_codes(Atom, CodeL).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%           END         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%