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
%midoan_solver_main__controlled_unification.pl
%
%This complex predicate is needed to replace the verify attributes predicate of the record and array modules.
%See Early September 2007 in the notes for further information
%The only reason that this predicate is necessary is to accurately implement array sliding. Since records can contain arrays its verify attribute had to change also.
%In the verify attribute of arrays if the type is within the attributed variable then during unification teh types are also unified : wrong semantics
%  (and this is important because the type is used in many places to retrieve the bounds of and array, to created new varibles etc.)
%The only alternative we tried to using this predicate iinvolves the passing of the type of the array at every step the symbolic interpretation.
%  (this is backed up in the '2007-09-10 extended type' folder) unfortunately this solution, whilst more elegant than this midoan_solver__controlled_unification predicate,
%   ran into problems (see September 10th 2007 in the notes)
:- use_module(common_util, [	common_util__create_dummy_name/1
                           ]
             ).

:- use_module([library(atts)]).

midoan_solver__controlled_unification(Le, Ri) :-
        midoan_solver__get_type_from_value(Le, Le_type),
        midoan_solver__get_type_from_value(Ri, Ri_type),
        ((Le_type = 'free' ; Ri_type = 'free') ->   %at least one is free
                Le = Ri
        ;
         Le_type = Ri_type ->
                controlled_unification_identical_types(Le_type, Le, Ri) %both are base_enumeration, ground, 'standard.ads:integer', array(_),
                                                                        % record, 'standard.ads:float', anonymous_aggregate, string_literal
        ;
         Le_type == 'ground' ->
                controlled_unification_one_ground(Ri_type, Ri, Le)      %one is ground 'standard.ads:integer' or 'standard.ads:float' only
        ;
         Ri_type == 'ground' ->
                controlled_unification_one_ground(Le_type, Le, Ri)      %one is ground 'standard.ads:integer' or 'standard.ads:float' only
        ;
         (Le_type == 'standard.ads:integer', Ri_type == 'standard.ads:float') ->        %(may occur during view conversion see 28/11/07)
                controlled_unification_numbers(Le, Ri)
        ;
         (Le_type == 'standard.ads:float', Ri_type == 'standard.ads:integer') ->        %(may occur during view conversion see 28/11/07)
                controlled_unification_numbers(Ri, Le)
        ;
         Le_type == 'anonymous_aggregate' ->
                controlled_unification_anonymous_aggregate(Ri_type, Ri, Le)
        ;
         Ri_type == 'anonymous_aggregate' ->
                controlled_unification_anonymous_aggregate(Le_type, Le, Ri)
        ;
         Le_type == 'string_literal' ->
                controlled_unification_string_literal(Ri_type, Ri, Le)
        ;
         Ri_type == 'string_literal' ->
                controlled_unification_string_literal(Le_type, Le, Ri)
        ;
                common_util__error(10, "Type mismatch during unification", no_error_consequences, [(le, Le), (le_type, Le_type), (ri, Ri), (ri_type, Ri_type)], 106047, midoan_solver, midoan_solver__controlled_unification, no_localisation, no_extra_info)
        ).

%%%
controlled_unification_identical_types(anonymous_aggregate, Le, Ri) :-
        common_util__error(10, "Type mismatch during unification: both types are anonymous aggregates", no_error_consequences, [(le, Le), (ri, Ri)], 106550, midoan_solver, controlled_unification_identical_types, no_localisation, no_extra_info).
controlled_unification_identical_types(string_literal, Le, Ri) :-
        common_util__error(10, "Type mismatch during unification: both types are string literals", no_error_consequences, [(le, Le), (ri, Ri)], 106751, midoan_solver, controlled_unification_identical_types, no_localisation, no_extra_info).
controlled_unification_identical_types(ground, Value, Value).   %'standard.ads:integer' or 'standard.ads:float' only
controlled_unification_identical_types('standard.ads:integer', Value, Value).
controlled_unification_identical_types('standard.ads:float', Value, Value).
controlled_unification_identical_types(record, Le, Ri) :-
        midoan_record:midoan_record__get_all_field_values(Le, Field_values_le),
        midoan_record:midoan_record__get_all_field_values(Ri, Field_values_ri),
        controlled_unification_record(Field_values_le, Field_values_ri).
controlled_unification_identical_types(modular_integer, Le, Ri) :-
        midoan_modular_integer:midoan_modular_integer__get(value, Le, Le_value),
        midoan_modular_integer:midoan_modular_integer__get(value, Ri, Ri_value),
        Le_value = Ri_value.
controlled_unification_identical_types(base_enumeration, Le, Ri) :-
        midoan_enum:midoan_enum__get(position, Le, Le_pos),
        midoan_enum:midoan_enum__get(position, Ri, Ri_pos),
        Le_pos = Ri_pos.
controlled_unification_identical_types(array(_), Le, Ri) :-
        midoan_array:midoan_array__get_all_index_elements(Le, Indice_elements_le),
        midoan_array:midoan_array__get_all_index_elements(Ri, Indice_elements_ri),
        (Indice_elements_le = unconst_array(_ ,_) ->
                controlled_unification_unconst_array(Le, Ri)
        ;
         Indice_elements_ri = unconst_array(_ ,_) ->
                controlled_unification_unconst_array(Ri, Le)
        ;
                array_sliding_unification(Indice_elements_le, Indice_elements_ri)
        ).
%%%
controlled_unification_one_ground('modular_integer', Ri, Le) :-           %Le is a ground integer, Ri is a modular integer var
        !,
        midoan_modular_integer:midoan_modular_integer__get('value', Ri, Ri_value),
        midoan_modular_integer:midoan_modular_integer__get('modulo', Ri, Modulo),
        midoan_solver__interpret(mod(Le, Modulo), types('i', 'i'), Result_value, 'i', _), %need to apply the modulo (e.g. 128 + 128 : for the solver there is no modular context to it gets evaluated to 256 not 0 at it should)
        midoan_solver__controlled_unification(Ri_value, Result_value).
controlled_unification_one_ground('standard.ads:integer', Ri, Le) :-    %Le is a ground, Ri is an integer var
        !,
        (integer(Le) -> %a ground integer with an integer var
                (midoan_extensions:max_clpfd(Max),
                 midoan_extensions:min_clpfd(Min),
                 ((Le >= Max ; Le =< Min) ->
                        (%this is very dangerous as it is unsound and attached constraints will not propagate! but it is necessary in order to deal with large integers
                         fd_degree(Ri, DegreeRi),       %DegreeRi is the number of constraints attached to Ri
                         (DegreeRi > 0 ->       %could lower severity down to 9 ... but we are intersted in knowing hwne it occurs ...
                                common_util__error(10, "Possible unsound integer equality due to a very large integer", "Possibly catastrophic", [('le', Le)], 1010627, 'midoan_solver', 'controlled_unification_one_ground', 'no_localisation', 'no_extra_info')
                         ;
                                true
                         ),
                         clpfd:put_atts(Ri, -fd_attribute(_, _, _)),    %the attribute is removed!
                         Ri = Le
                        )
                 ;
                        Ri #= Le
                 )
                )
        ;
         float(Le) ->   %a ground float with an integer var (may occur during view conversion see 28/11/07)
                (midoan_solver__interpret(conversion('standard.ads:integer', Le), types(_, _), Ri, Type_conversion, _Exception_conversion),
                 (Type_conversion == 'unhandled_expression' ->
                        common_util__error(10, "Unhandled type", no_error_consequences, [(type_conversion, Type_conversion)], 109906, midoan_solver, controlled_unification_one_ground, no_localisation, no_extra_info)
                 ;
                        true
                 )
                )
        ;
                common_util__error(10, "Type mismatch during unification: not a number", no_error_consequences, [(ri, Ri), (le, Le)], 109953, midoan_solver, controlled_unification_one_ground, no_localisation, no_extra_info)
        ).
controlled_unification_one_ground('standard.ads:float', Ri, Le) :-      %Le is a ground, Ri is a float var
        !,
        (float(Le) ->   %a ground float with a float var
                Ri = Le
        ;
         integer(Le) -> %a ground integer with a float var (may occur during view conversion see 28/11/07)
                (midoan_solver__interpret(conversion('standard.ads:float', Le), types(_, _), Ri, Type_conversion, _Exception_conversion),
                 (Type_conversion == unhandled_expression ->
                        common_util__error(10, "Unhandled type", no_error_consequences, [(type_conversion, Type_conversion)], 1011510, midoan_solver, controlled_unification_one_ground, no_localisation, no_extra_info)
                 ;
                        true
                 )
                )
        ;
                common_util__error(10, "Type mismatch during unification: not a number", no_error_consequences, [(ri, Ri), (le, Le)], 1010954, midoan_solver, controlled_unification_one_ground, no_localisation, no_extra_info)
        ).
controlled_unification_one_ground(Something_else, Ri, Le) :-
        !,
        common_util__error(10, "Type mismatch during unification: unknown types", no_error_consequences, [(something_else, Something_else), (ri, Ri), (le, Le)], 1011355, midoan_solver, controlled_unification_one_ground, no_localisation, no_extra_info).

%%%
controlled_unification_numbers(Var_int, Var_fl) :-      %we do both! [how crazy is that?] see [28/11/07]
        %trace,
        midoan_solver__interpret(conversion('standard.ads:integer', Var_fl), types(_, _), Var_int, Type_conversion_int, _Exception_conversion_int),
        midoan_solver__interpret(conversion('standard.ads:float', Var_int), types(_, _), Var_fl, Type_conversion_fl, _Exception_conversion_fl),
        ((Type_conversion_int == unhandled_expression ; Type_conversion_fl == unhandled_expression) ->
                common_util__error(10, "Unhandled type", no_error_consequences, [(type_conversion_int, Type_conversion_int), (type_conversion_fl, Type_conversion_fl)], 1013314, midoan_solver, controlled_unification_numbers, no_localisation, no_extra_info)
        ;
                true
        ).
%%%
controlled_unification_anonymous_aggregate(record, Record_var, Anon_aggregate) :-
        !,
        midoan_anon_aggregate:midoan_anon_aggregate__get_anon_aggregate_stuff(Anon_aggregate, AsgL),
        midoan_anon_aggregate:midoan_anon_aggregate__unput(Anon_aggregate),
        midoan_record:midoan_record__get_type(Record_var, Type_var),
        midoan_record:midoan_record__create_record_from_agg(Type_var, AsgL, Anon_aggregate),         %Anon_aggregate is now a new record var
        controlled_unification_identical_types(record, Record_var, Anon_aggregate).

controlled_unification_anonymous_aggregate(array(_), Array_var, Anon_aggregate) :-
        !,
        midoan_anon_aggregate:midoan_anon_aggregate__get_anon_aggregate_stuff(Anon_aggregate, AsgL),
        midoan_anon_aggregate:midoan_anon_aggregate__unput(Anon_aggregate),
        midoan_array:midoan_array__get_type_var(Array_var, Type_var),
        midoan_array:midoan_array__get_all_index_elements(Array_var, Indice_elements),
        (Indice_elements = unconst_array(IndexL, _Component_type_var) ->
                (%unification of an unconstrained array with an anonymous array aggregate
                 get_bounds(AsgL, IndexL, New_indexL),  %New_indexL is a list of subtype making up the new bounds
		 common_util__create_dummy_name(Subtype_name),
                 midoan_type:midoan_type__create_subtype(Subtype_name, Type_var, unconst_array(New_indexL), Subtype_var), %create a new array type based on the unconst array
                 midoan_array:midoan_array__create_array_from_agg(Subtype_var, AsgL, Anon_aggregate)        %Anon_aggregate is now a new array var
                )
        ;
                midoan_array:midoan_array__create_array_from_agg(Type_var, AsgL, Anon_aggregate)            %Anon_aggregate is now a new array var
        ),
        controlled_unification_identical_types(array(_), Array_var, Anon_aggregate).

controlled_unification_anonymous_aggregate(Another_type, Another_var, Anon_aggregate) :-
        common_util__error(10, "Type mismatch during unification: an anonymous aggregate with a non array or a non record", no_error_consequences, [(another_type, Another_type), (another_var, Another_var), (anon_aggregate, Anon_aggregate)], 1014958, midoan_solver, controlled_unification_anonymous_aggregate, no_localisation, no_extra_info).

%%%
array_sliding_unification([], []).
array_sliding_unification([(_, Value_le)|R1], [(_, Value_ri)|R2]) :-
        midoan_solver__controlled_unification(Value_le, Value_ri),
        array_sliding_unification(R1, R2).

%%%
controlled_unification_record([], []).
controlled_unification_record([(Field_le, Value_le)|Rest_le], [(Field_ri, Value_ri)|Rest_ri]) :-
        (Field_le == Field_ri ->
                (midoan_solver__controlled_unification(Value_le, Value_ri),
                 controlled_unification_record(Rest_le, Rest_ri)
                )
        ;
                common_util__error(10, "Field mismatch during record unification", no_error_consequences, [(field_le, Field_le), (value_ri, Value_ri)], 1016559, midoan_solver, controlled_unification_record, no_localisation, no_extra_info)
        ).
%%%
controlled_unification_string_literal(array(_), Array_var, String_literal) :-
        !,
        midoan_string_literal:midoan_string_literal__get_ascii_codes(String_literal, CodeL),
        midoan_string_literal:midoan_string_literal__unput(String_literal),
        midoan_array:midoan_array__get_type_var(Array_var, Type_var),
        midoan_array:midoan_array__get_all_index_elements(Array_var, Indice_elements),
        (Indice_elements = unconst_array([Type_mark_of_index], _Component_type_var) ->          %can only be one dimentional array
                (midoan_solver__interpret(first(Type_mark_of_index), _, From, Type_first, 'no_exception'),               %to get the lower bound of the dimension
                 (Type_first == unhandled_expression ->
                        common_util__error(10, "Unhandled type", no_error_consequences, [(type_first, Type_first)], 1019526, midoan_solver, controlled_unification_string_literal, no_localisation, no_extra_info)
                 ;
                        true
                 ),
                 length(CodeL, L),
                 By is L - 1,
                 midoan_array:midoan_array__shift_single_index(By, From, To),                                              %to get the upper bound of the dimension
                 common_util__create_dummy_name(Subtype_name_index),
                 midoan_type:midoan_type__create_subtype(Subtype_name_index, Type_mark_of_index, range_bounds(From, To), Subtype_var_index),
                 common_util__create_dummy_name(Subtype_name),
                 midoan_type:midoan_type__create_subtype(Subtype_name, Type_var, unconst_array([Subtype_var_index]), Subtype_var),      %create a new array type based on the unconst array
                 midoan_array:midoan_array__create_array_from_string_literal(Subtype_var, CodeL, String_literal)                          %String_literal is now a new array var
                )
        ;
                midoan_array:midoan_array__create_array_from_string_literal(Type_var, CodeL, String_literal)      %String_literal is now a new array var
        ),
        controlled_unification_identical_types(array(_), Array_var, String_literal).

controlled_unification_string_literal(Another_type, Another_var, String_literal) :-
        common_util__error(10, "Type mismatch during unification: a string literal with a non array", no_error_consequences, [(another_type, Another_type), (another_var, Another_var), (string_literal, String_literal)], 1019102, midoan_solver, controlled_unification_string_literal, no_localisation, no_extra_info).
%%%
controlled_unification_unconst_array(Unconst_array, Array_var) :-
        midoan_array:midoan_array__get_all_index_elements(Unconst_array, unconst_array(IndexL, Component_type_var)),
        midoan_array:midoan_array__get_all_index_elements(Array_var, Indice_elements),
        midoan_array:midoan_array__get_type_var(Array_var, Type_var),
        (Indice_elements = unconst_array(IndexL2, Component_type_var2) -> %2 unconstarined arrays unification : when does this occur?
                unconst_array(IndexL, Component_type_var) = unconst_array(IndexL2, Component_type_var2)
        ;
                (%we should check for run time errors regarding type of components and indices
                 midoan_array:midoan_array__update_unconst_array(Unconst_array, Type_var, Indice_elements)
                )
        ).
%%%
%an unconstrained array gets its new bounds from an aggregate
%IndexL is the unconstrained array list of index typemarks (one per dimension)
%AsgL is the aggregate
%New_indexL needs to be a list of subtype type marks (one per dimension)
%remember : there is no 'others' allowed in the aggregate (Ada rule)
get_bounds(AsgL, IndexL, New_indexL) :-
        AsgL = [First|_],
        ((compound(First), First = named(_, _)) ->
                get_bounds(named, AsgL, IndexL, New_indexL)
        ;
                get_bounds(positional, AsgL, IndexL, New_indexL)
        ).
%%%
        %need to get the max length of each dimension : we simply get the length
        get_bounds(Kind, AsgL, [Type_var|R2], [Subtype_var|R3]) :-
                (Kind == positional ->
                        (midoan_solver__interpret(first(Type_var), _, From, Type_first, 'no_exception'),         %to get the lower bound of the dimension
                         (Type_first == unhandled_expression ->
                                common_util__error(10, "Unhandled type", no_error_consequences, [(type_first, Type_first)], 1024727, midoan_solver, get_bounds, no_localisation, no_extra_info)
                         ;
                                true
                         ),
                         length(AsgL, L),
                         By is L - 1,
                         midoan_array:midoan_array__shift_single_index(By, From, To),                               %to get the upper bound of the dimension
                         AsgL = [Exp_asg|_]
                        )
                ;
                 Kind == named ->
                        (find_lowest_largest(AsgL, From, To),
                         AsgL = [named(_, Exp_asg)|_]
                        )
                ),
                common_util__create_dummy_name(Subtype_name),
                midoan_type:midoan_type__create_subtype(Subtype_name, Type_var, range_bounds(From, To), Subtype_var),
                (R2 == [] ->
                        R3 = []
                ;
                        (midoan_anon_aggregate:midoan_anon_aggregate__is_anon_aggregate(Exp_asg) ->	                %then Exp_asg is an AsgL itself (for a multidimentional array)
                                (midoan_anon_aggregate:midoan_anon_aggregate__get_anon_aggregate_stuff(Exp_asg, AsgL_inner),
                                 get_bounds(AsgL_inner, R2, R3)                         %could be positional or named
                                )
                        ;
                         midoan_string_literal:midoan_string_literal__is_string_literal(Exp_asg) ->
                                (midoan_string_literal:midoan_string_literal__get_ascii_codes(Exp_asg, CodeL),
                                 get_bounds(CodeL, R2, R3)
                                )
                        )
                ).

%applied to a named aggregate only
find_lowest_largest(AsgL, Smallest, Largest) :-
        fll_append_indices(AsgL, IndexL),
        IndexL = [First|R],
        find_lowest_largest_single_index(First, S, L),      %for the first Choice
        find_lowest_largest(R, S, L, Smallest, Largest).

%append all the indices in a named aggregate : get a list only containing choices
fll_append_indices([], []).
fll_append_indices([named(Next, _Exp)|R], IndexL) :-
        fll_append_indices(R, IndexL2),
        append(Next, IndexL2, IndexL).

%%%
find_lowest_largest_single_index(I, I, I) :-
        integer(I),
        !.
find_lowest_largest_single_index(I, I, I) :-
        midoan_enum:midoan_enum__is_enum(I),
        !.
find_lowest_largest_single_index(I, I, I) :-
        midoan_modular_integer:midoan_modular_integer__is_modular_integer(I),
        !.
find_lowest_largest_single_index([Min, Max], Min, Max) :-
        !.
find_lowest_largest_single_index(range(Type_var), Min, Max) :-
        !,
        midoan_type:midoan_type__get_attribute(Type_var, first, Min),
	midoan_type:midoan_type__get_attribute(Type_var, last, Max),
        !.
find_lowest_largest_single_index(Type_var, Min, Max) :-
        !,
        find_lowest_largest_single_index(range(Type_var), Min, Max).   %see above

%%%
find_lowest_largest([], Smallest, Largest, Smallest, Largest).
find_lowest_largest([Next|R], S, L, Smallest, Largest) :-
        find_lowest_largest_single_index(Next, S1, L1),
        (midoan_solver__sdl(<(S1, S)) ->
                S2 = S1
        ;
                S2 = S
        ),
        (midoan_solver__sdl(>(L1, L)) ->
                L2 = L1
        ;
                L2 = L
        ),
        find_lowest_largest(R, S2, L2, Smallest, Largest).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%