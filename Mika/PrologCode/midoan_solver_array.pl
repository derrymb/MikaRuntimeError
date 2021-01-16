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
% midoan_solver_array.pl
% defines module array for array meta variables handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% an array variable R is of the form: R{midoan_array(Type_var, Indice_Elements)},
%  where Indice_Elements is a list of (Indice, Element) and Type the type of the array and Component_type the type of the elements of the array
%                        or unconst_array(IndexL, Component_type_var)
%  a multi-dimentional array is represented as [([1, 1], el), [1, 2], el) etc.]
%  an array of array is represented as [([1], array), ([2], array) etc.]
%  so the internal representation follows the logical structures of arrays
%  the type of the elements of an array can be retrieved via midoan_type:midoan_type__obtain_basetype(Type_var, array(Component_type_var))
% an array meta variable never becomes ground, even if an array is fully known it is still represented as a meta array variable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(midoan_array, []).

:- use_module([midoan_solver_main, common_util]).

:- use_module([	library(atts),	%Sicstus attribute library
		library(lists)  %for append/3
	     ]).

:- attribute midoan_array/2.    %name of the attribute
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%internal predicate (see atts library documentation)
%called prior to unification
%Value is a non-variable or another attributed variable (if Value is a plain variable verify_attributes is not called)
%!!! this is no longer used (since September 2007) : all done via : midoan_solver__controlled_unification
verify_attributes(Var, Value, Goals) :-
        (get_atts(Var, midoan_array(Type_var, Indice_elements)) ->
                (Indice_elements = unconst_array(_, _) ->
                        (%an unconstrained array var gets its bounds
                         var(Value) ->  %unification of an unconstrained array variable with a variable (should always be the case)
                                (get_atts(Value, midoan_array(Type_var2, Indice_elements2)) ->
                                        common_util:common_util__error(10, "Unification Error", no_error_consequences, [(type_var, Type_var), (indice_elements, Indice_elements), (type_var2, Type_var2), (indice_elements2, Indice_elements2)], 108544, midoan_array, verify_attributes, no_localisation, "Unification error in verify_attributes in midoan_array module: should use midoan_solver__controlled_unification (unconst_array = array)")
                                ;
			         midoan_anon_aggregate:midoan_anon_aggregate__is_anon_aggregate(Value) ->
				        common_util:common_util__error(10, "Unification Error", no_error_consequences, [(type_var, Type_var), (indice_elements, Indice_elements), (value, Value)], 108848, midoan_array, verify_attributes, no_localisation, "Unification error in verify_attributes in midoan_array module: should use midoan_solver__controlled_unification (unconst_array = aggregate)")
			        ;
                                        %should never happen: arrays are only unified with other array variables
                                        common_util:common_util__error(10, "Unification Error", no_error_consequences, no_arguments, 109149, midoan_array, verify_attributes, no_localisation, "Unification of an unconstrained array variable with a non array variable or a non anonymous aggregate in midoan_array module; should use midoan_solver__controlled_unification.")
                                )
                        ;
                                common_util:common_util__error(10, "Unification Error", no_error_consequences, [(var, Var), (value, Value)], 1094134, midoan_array, verify_attributes, no_localisation, "Unification of an unconstrained array variable with a non variable in midoan_array module; should use midoan_solver__controlled_unification.")
                        )
                ;

                        (var(Value) ->  %unification of an array variable with a variable (should always be the case)
                                (get_atts(Value, midoan_array(Type_var2, Indice_elements2)) ->         %two array variables unification
                                        common_util:common_util__error(10, "Unification Error", no_error_consequences, [(type_var, Type_var), (indice_elements, Indice_elements), (type_var2, Type_var2), (indice_elements2, Indice_elements2)], 10100239, midoan_array, verify_attributes, no_localisation, "Unification error in verify_attributes in midoan_array module: should use midoan_solver__controlled_unification (array = array)")
                                ;
			         midoan_anon_aggregate:midoan_anon_aggregate__is_anon_aggregate(Value) ->
				        common_util:common_util__error(10, "Unification Error", no_error_consequences, [(type_var, Type_var), (indice_elements, Indice_elements), (value, Value)], 1010353, midoan_array, verify_attributes, no_localisation, "Unification error in verify_attributes in midoan_array module: should use midoan_solver__controlled_unification (array = aggregate)")
			        ;
                                        %should never happen: arrays are only unified with other array variables
                                        common_util:common_util__error(10, "Unification Error", no_error_consequences, no_arguments, 1010654, midoan_array, verify_attributes, no_localisation, "Unification of an array variable with a non array variable or a non anonymous aggregate in midoan_array module; should use midoan_solver__controlled_unification.")
                                )
                        ;
                                fail    %unification of a string_literal var with a nonvar (e.g 'others')
                        )
                )
        ;
                Goals = []     %success but no actions as we are not concerned : not an array variable
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%check if a variable is an array variable
midoan_array__is_array(Var) :-
	var(Var),
	get_atts(Var, midoan_array(_, _)).      %possibly unconstrained
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%[23/11/09] We have no way of knowing if a type is an unconstrained array or not, the basetype remains unconst_array even after the type has
% been bound (to reflect the fact that the underlying basetype was an unconstrained array, information which is needed in array concatenation)
% So, we have to check a declared varaible instead as done below
midoan_array__is_unconst_array(Type_var) :-
        midoan_type:midoan_type__variable_declaration(Var, Type_var),
        !,
        var(Var),       %in case it is an enum or a modular_integer and actually not a variable
	get_atts(Var, midoan_array(_, unconst_array(_, _))).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%create an array variable
%very similar to midoan_array__create_array
%string_type is a constrained array of characters (enumeration values)
midoan_array__create_array_from_string_literal(String_type_var, CodeL, R) :-      %CodeL is a list of true ascii codes
        midoan_type:midoan_type__obtain_basetype(String_type_var, Array_basetype),
        (Array_basetype = array(Character_type_var) ->
                true    %just to retrieve the Character_type
        ;
         Array_basetype = unconst_array(Character_type_var) ->
                true    %just to retrieve the Character_type
        ;
                common_util:common_util__error(10, "Unknow array basetype", no_error_consequences, [(array_basetype, Array_basetype)], 1086895, midoan_array, midoan_array__create_array_from_string_literal, no_localisation, no_extra_info)
        ),
        midoan_type:midoan_type__get_attribute(String_type_var, first(1), First_index),
        midoan_enum:midoan_enum__generate_list_of_enums(CodeL, Character_type_var, Enum_list),
        macafsl_add_index(Enum_list, First_index, Indice_elements),
        put_atts(R, midoan_array(String_type_var, Indice_elements)).
%%%
        macafsl_add_index([], _Index, []).
        macafsl_add_index([Next_enum|Rest], Index, [([Index], Next_enum)|Rest_indice_elements]) :-
                midoan_array__shift_single_index(1, Index, Next_index),
                macafsl_add_index(Rest, Next_index, Rest_indice_elements).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%create an unconstrained array variable (ready for unification with an initial value later to supply the bounds)
midoan_array__create_unconst_array(Type_var, IndexL, Component_type_var, R) :-
        put_atts(R, midoan_array(Type_var, unconst_array(IndexL, Component_type_var))).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%an unconstrained array variable is unified with a constrained array variable (called from controlled_unification_unconst_array)
midoan_array__update_unconst_array(Unconst_array, Type_var, Indice_elements) :-
        put_atts(Unconst_array, midoan_array(Type_var, Indice_elements)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_array__create_array(Type_var, IndexL, Component_type_var, Template_or_variable, R) :-
        extract_index(IndexL, Eval_indexL),    %extract the list of index to the form a list of (First, Last)
        (Template_or_variable == template ->
                Component_template_or_variable = Template_or_variable
        ;
                (midoan_type:midoan_type__get_attribute(Component_type_var, has_fields_with_default, Has_fields_with_default),
                 (Has_fields_with_default == no ->
                        Component_template_or_variable = template
                 ;
                        Component_template_or_variable = variable
                 )
                )
        ),
	generate_array(Eval_indexL, Indice_elements, Component_type_var, Component_template_or_variable),
	put_atts(R, midoan_array(Type_var, Indice_elements)).
%%%
        % IndexL is a list of type_marks
        % Eval_indexL is the out list of indexes of the form [(First0, Last0) ... (FirstN, LastN)] corresponding to IndexL
        extract_index([], []).
        extract_index([Type_var|Rest_indexL], [(First, Last)|Rest_eval_indexL]) :-
                midoan_type:midoan_type__get_attribute(Type_var, first, First),
                midoan_type:midoan_type__get_attribute(Type_var, last, Last),
                extract_index(Rest_indexL, Rest_eval_indexL).

%%%
        % IndexL is the list of indices of the form [(First0, Last0) ... (FirstN, LastN)]
        % Element is a variable for the element of the array
        %Indice_elements for an array of the form for a 1..3, 1..2 array is:
        % _[([1,1], el1.1), ([1,2], el1.2), ([2,1], el2.1), ([2,2], el2.2), ([3,1], el3.1), ([3,2], el3.2)]
        generate_array(IndexL, Indice_elements, Component_type_var, Template_or_variable) :-
                gen1(IndexL, [[]], Indice_elements, Component_type_var, Template_or_variable).
%%%
                gen1([], In, Out, Component_type_var, Template_or_variable) :-
                        (In == [] ->
                                Out = []
                        ;
                                fill_with_elements(In, Out, Component_type_var, Template_or_variable)
                        ).
                gen1([(Min, Max)|Rest], In, Out, Component_type_var, Template_or_variable) :-
                        (index_is_ground([Min, Max]) ->
                                true
                        ;
                                (common_util:common_util__error(2, "Input Dependent Array Creation : Mika is only guessing its bounds", "may create false run time errors later is this path", [('min', Min), ('max', Max)], 2164226, 'midoan_array', 'gen1', 'no_localisation', 'no_extra_info'),
                                 (
                                        midoan_solver:midoan_solver__sdl('>'(Min, Max))      %empty
                                 ;                                              %choice point either the range is empty or it isn't
                                        midoan_solver:midoan_solver__sdl('<='(Min, Max))      %not empty
                                 ),
                                 make_range_ground([Min, Max])  %ensure that the range is ground use labeling if necessary : creates choice points!!!!!!!!!
                                )
                        ),
                        gen_single_list(Single_list, Min, Max),
                        combine(In, Single_list, Out1),
                        gen1(Rest, Out1, Out, Component_type_var, Template_or_variable).
%%%
                        fill_with_elements([Index|Index_list], [(Index, Element)|Index_element_list], Component_type_var, Template_or_variable) :-
                                (Template_or_variable == template ->
                                        midoan_type:midoan_type__variable_declaration(Element, Component_type_var)
                                ;
                                        midoan_type:midoan_type__created_variable_with_default_initialised_fields(Element, Component_type_var)
                                ),
                                (Index_list == [] ->
                                        Index_element_list = []
                                ;
                                        fill_with_elements(Index_list, Index_element_list, Component_type_var, Template_or_variable)
                                ).
%%%
                        %combine a list of index with a list of indices
                        %e.g. combine([[1,1], [1,2]], [1,2,3,4,5], [[1,1,1], [1,1,2], [1,1,3], [1,1,4], [1,1,5], [1,2,1], [1,2,2], [1,2,3], [1,2,4], [1,2,5]]
                        combine([], _, []).
                        combine([Index|Index_list], Indice_L, New_Index_list) :-
                                subcombine(Indice_L, Index, Sub_Index_list),
                                append(Sub_Index_list, Rest_Index_list, New_Index_list),
                                combine(Index_list, Indice_L, Rest_Index_list).
%%%
                                %combine a list of indices with an index
                                %subcombine([1, 2], [1, 1, 1], [[1, 1, 1, 1], [1, 1, 1, 2]]).
                                subcombine([], _, []).
                                subcombine([Indice|Indice_L], Index, [New_Index|Rest]) :-
                                        append(Index, [Indice], New_Index),
                                        subcombine(Indice_L, Index, Rest).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_array__get_all_index_elements(Array_var, Index_elements) :-
	get_atts(Array_var, midoan_array(_, Index_elements)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_array__get_type_var(Array_var, Type_var) :-
        get_atts(Array_var, midoan_array(Type_var, _)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_array__get_element(Array, Index, Result, Type, Exception) :-
        midoan_array__get_type_var(Array, Array_type_var),
        midoan_type:midoan_type__get_attribute(Array_type_var, 'basetype', array(Component_type_var)),
        midoan_type:midoan_type__variable_declaration(Result, Component_type_var),
        midoan_solver:midoan_solver__interpret(Result, types(_), _, Type, 'no_exception'),           %to get the type, cannot be unhandled nor raise exception otherwise the array would not exist
        (index_is_ground(Index) ->
                get_element_ground(Array, Index, Result, Exception)
        ;
                (constrain_indices(Index, Array_type_var, Exception),             %ensures that the indices are within the dimensions of the array
                 (common_util:common_util__is_an_exception(Exception) ->
                        true
                 ;
                        (build_suspension_all_ground(Index, Susp),
                         when(Susp, get_element_ground(Array, Index, Result, _))        %Exception cannot be raised (already check prior to delaying)
                        )
                 )
                )
        ).
%%%
        get_element_ground(Array, Index, Result, Exception) :-
                midoan_array__get_all_index_elements(Array, Indice_elements),
                get_element_ground2(Indice_elements, Index, Result, Exception_raised),
                (Exception_raised == 'no_exception' ->
                        Exception = Exception_raised
                ;
                        Exception = exception(Exception_raised, [array(Array), index(Index), predicate(get_element_ground2), message("Run-time constraint error in array access")])
                ).
%%%
                get_element_ground2([], _Index, _Result, 'constraint_error').
                get_element_ground2([(Element_index, Element)|Rest], Index, Result, Exception_raised) :-
                        (index_match(Element_index, Index) ->           %could this raise an exception?
                                (midoan_solver:midoan_solver__controlled_unification(Element, Result),  %could this raise an exception?
                                 Exception_raised = 'no_exception'
                                )
                        ;
                                get_element_ground2(Rest, Index, Result, Exception_raised)
                        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_array__get_slice(Array, [From, To], New_array) :-        %may create choice points!!!!!!!!!
        %if the range is ground we need to:
        % create a subtype of the index
        % create a new array type based on it
        % create a new array var
        % do a controlled unification on the common elements
        %if the range is not ground ... not much can be deducted, not even the length of the array
        % ... making array access, update etc. pretty difficult
        % one possibility would be to delay and have an unknown array which means that we would need
        % to rewrite the other constraints on array (get_element, up_array etc) to delay too
        % another possibility ... is to label the range ! this is quite new and bold but may be
        % justified on pragmatic ground (intermediate solution, less work,
        % dynamic array slices unlikely to be common etc.)
        midoan_array__get_type_var(Array, Array_type_var),
        midoan_solver:midoan_solver__interpret(first(Array_type_var, 1), types(type), Min, Type_first, 'no_exception'),
        midoan_solver:midoan_solver__interpret(last(Array_type_var, 1), types(type), Max, Type_last, 'no_exception'),
        ((Type_first == unhandled_expression ; Type_last == unhandled_expression) ->
                %23/09/09 (see electronic diary) better would be to propagate the unhandled expression upwards ...
                common_util:common_util__error(10, "Unhandled type", no_error_consequences, [(type_first, Type_first), (type_last, Type_last)], 1031532, mika_solver_array, midoan_array__get_slice, no_localisation, no_extra_info)
        ;
                true
        ),
        (index_is_ground([From, To]) ->
                (midoan_solver:midoan_solver__sdl(From > To)  ->
                        true                                    %will be an empty array
                ;
                        ((midoan_solver:midoan_solver__sdl('<='(Min, From)),   %we check for constraint error
                          midoan_solver:midoan_solver__sdl('>='(Max, To))
                         ) ->
                                true
                         ;
                                common_util:common_util__error(10, "Run-time constraint error in this array slice index: discrete range is out of bounds of the index of the array", no_error_consequences, [(array, Array), (from, From), (to, To)], 1029628, midoan_array, midoan_array__get_slice, no_localisation, no_extra_info)
                        )
                )
        ;
                (common_util:common_util__error(2, "Input Dependent Slice : Mika is only guessing its range", "may create false run time errors later is this path", [(array, Array), (from, From), (to, To)], 2301200, midoan_array, midoan_array__get_slice, no_localisation, no_extra_info),
                 (
                        (midoan_solver:midoan_solver__sdl('<='(Min, From)),
                         midoan_solver:midoan_solver__sdl('>='(Max, To))
                        )
                 ;                                              %choice point either the range is empty or it isn't
                        midoan_solver:midoan_solver__sdl(From > To)
                 ),
                 make_range_ground([From, To])  %ensure that the range is ground use labeling if necessary : creates choice points!!!!!!!!!
                )
        ),
        common_util:common_util__create_dummy_name(Index_name),
        midoan_type:midoan_type__get_attribute(Array_type_var, index_list, Index_list),
        (Index_list = [Index_type_var] ->
                true
        ;
                common_util:common_util__error(10, "List of index in get slice is not a singleton", no_error_consequences, [(array_type_var, Array_type_var)], 1030321, midoan_array, midoan_array__get_slice, no_localisation, no_extra_info)
        ),
        midoan_type:midoan_type__create_subtype(Index_name, Index_type_var, range_bounds(From, To), Index_subtype_var),
        common_util:common_util__create_dummy_name(New_array_name),
        midoan_type:midoan_type__obtain_basetype(Array_type_var, Array_basetype),
        (Array_basetype = array(Component_type_var) ->
                midoan_type:midoan_type__create_type(New_array_name, array, [Index_subtype_var], Component_type_var, New_array_type_var)
        ;
         Array_basetype = unconst_array(Component_type_var) ->
                midoan_type:midoan_type__create_subtype(New_array_name, Array_type_var, unconst_array([Index_subtype_var]), New_array_type_var)
        ;
                common_util:common_util__error(10, "Unknow array basetype", no_error_consequences, [(array_basetype, Array_basetype)], 1086894, midoan_array, midoan_array__get_slice, no_localisation, no_extra_info)
        ),
        midoan_array__get_all_index_elements(Array, Index_elements),
        (midoan_solver:midoan_solver__sdl(From > To) ->
                New_index_elements = []
        ;
                get_sublist(Index_elements, From, To, New_index_elements)
        ),
        put_atts(New_array, midoan_array(New_array_type_var, New_index_elements)),
        midoan_type:midoan_type__variable_declaration(Template, New_array_type_var),
        midoan_solver:midoan_solver__controlled_unification(New_array, Template).
%%%
make_range_ground([]).
make_range_ground([Next|Rest]) :-
        (midoan_enum:midoan_enum__is_enum(Next) ->
                midoan_labeling:midoan_labeling__enums([Next])  %will only label if not ground
        ;
         midoan_modular_integer:midoan_modular_integer__is_modular_integer(Next) ->
                (midoan_modular_integer:midoan_modular_integer__get(value, Next, Value),
                 midoan_solver__label_modular_integers([Value])  %will only label if not ground
                )
        ;
                midoan_labeling:midoan_labeling__integers([Next])       %will only label if not ground
        ),
        make_range_ground(Rest).
%%%
get_sublist([], _, _, []) :-
        common_util:common_util__error(10, "Min is invalid in array slice", no_error_consequences, no_arguments, 1034331, midoan_array, get_sublist, no_localisation, no_extra_info).
get_sublist([([I], Value)|Rest], Min, Max, New_index_elements) :-
        (index_match([I], [Min]) ->
                get_sublist_rest([([I], Value)|Rest], Max, New_index_elements)
        ;
                get_sublist(Rest, Min, Max, New_index_elements)
        ).

get_sublist_rest([], _, []) :-
        common_util:common_util__error(10, "Max is invalid in array slice", no_error_consequences, no_arguments, 1035233, midoan_array, get_sublist, no_localisation, no_extra_info).
get_sublist_rest([([I], Value)|Rest], Max, [([I], Value)|Rest_new_list]) :-
        (index_match([I], [Max]) ->
                Rest_new_list = []      %end of the recursion
        ;
                get_sublist_rest(Rest, Max, Rest_new_list)
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_array__up_array_slice(Array, [From, To], Exp, New_array, Type, Exception) :-      %may create choice points!!!!!!!!!
        midoan_array__get_type_var(Array, Array_type_var),
        midoan_type:midoan_type__variable_declaration(New_array, Array_type_var),
        midoan_solver:midoan_solver__interpret(New_array, types(_), _, Type, 'no_exception'),           %to get the type
        (index_is_ground([From, To]) ->
                 mauas_up_array_slice_ground(Array, [From, To], Exp, New_array, Exception)
        ;
                ((midoan_array__is_array(Exp) ->
                        (midoan_array__get_all_index_elements(Exp, Index_elements),
                         length(Index_elements, L)
                        )
                 ;
                  midoan_string_literal:midoan_string_literal__is_string_literal(Exp) ->
                        (midoan_string_literal:midoan_string_literal__get_ascii_codes(Exp, Codes),
                         length(Codes, L)
                        )
                 ),
                 (L == 0 ->
                        (midoan_solver:midoan_solver__sdl(From > To) ->
                                true
                        ;
                                Exception = exception('constraint_error', [array(Array), index([From, To]), predicate(midoan_array__up_array_slice), message("Run-time constraint error in array slice assignment : the slice is non null but it is assigned a null array")])
                        )
                 ;
                        (midoan_solver:midoan_solver__interpret(first(Array_type_var, 1), types(type), Min, Type_first, 'no_exception'), %type cannot be unhandled_expression or Array would not exist
                         midoan_solver:midoan_solver__interpret(last(Array_type_var, 1), types(type), Max, _, 'no_exception'),  %type cannot be unhandled_expression or Array would not exist
                         (midoan_solver:midoan_solver__sdl('<='(Min, From)) ->
                                (midoan_solver:midoan_solver__sdl('>='(Max, To)) ->
                                        ((Type_first == 'i' ->
                                                (To1 = To,
                                                 From1 = From
                                                )
                                         ;
                                          Type_first == 'e' ->
                                                (midoan_enum:midoan_enum__get('position', From, From1),
                                                 midoan_enum:midoan_enum__get('position', To, To1)
                                                )
                                         ;
                                          Type_first == 'modular_integer' ->
                                                (midoan_modular_integer:midoan_modular_integer__get('value', From, From1),
                                                 midoan_modular_integer:midoan_modular_integer__get('value', To, To1)
                                                )
                                         ),
                                         %(To - From) + 1 = Length of the update
                                         midoan_solver:midoan_solver__interpret('-'(To1, From1), types('i', 'i'), M, _, _),     %type cannot be unhandled_expression or Array would not exist
                                         midoan_solver:midoan_solver__interpret('+'(M, 1), types('i', 'i'), M1, _, _),          %type cannot be unhandled_expression or Array would not exist
                                         midoan_solver:midoan_solver__sdl('='(M1, L))
                                        )
                                ;
                                        Exception = exception('constraint_error', [array(Array), index([From, To]), predicate(midoan_array__up_array_slice), message("Run-time constraint error in array slice assignment : the lower bound index of the slice is lower than its type definition")])
                                )
                         ;
                                Exception = exception('constraint_error', [array(Array), index([From, To]), predicate(midoan_array__up_array_slice), message("Run-time constraint error in array slice assignment : the upper bound index of the slice is greater than its type definition")])
                         )
                        )
                 ),
                 (common_util:common_util__is_an_exception(Exception) ->
                        true
                 ;
                        (build_suspension_all_ground([From, To], Susp),
                         when(Susp, mauas_up_array_slice_ground(Array, [From, To], Exp, New_array, Exception))
                        )
                 )
                )
        ).
%%%
        mauas_up_array_slice_ground(Array, [From, To], Exp, New_array, 'no_exception') :-
                (midoan_solver:midoan_solver__sdl(From > To) ->                                                           %an empty slice : basically not update
                        midoan_solver:midoan_solver__controlled_unification(New_array, Array)
                ;
                        (midoan_array__get_slice(Array, [From, To], Slice),              %we get the slice first: ground here
                         midoan_array__get_type_var(Slice, Type_mark_slice),
                         midoan_type:midoan_type__variable_declaration(Exp_slice, Type_mark_slice),
                         midoan_solver:midoan_solver__controlled_unification(Exp_slice, Exp),
                         midoan_array__get_all_index_elements(Exp_slice, Slice_elements),
                         midoan_array__get_all_index_elements(Array, Array_elements),
                         midoan_array__get_type_var(Array, Type_mark),
                         fill_array(Array_elements, From, To, Slice_elements, New_index_elements),       %we have [Min to From-1] : Array ; [From to To] : Slice ; [To+1 to Max] : Array
                         put_atts(New_array_tmp, midoan_array(Type_mark, New_index_elements)),
                         midoan_solver:midoan_solver__controlled_unification(New_array, New_array_tmp)
                        )
                ).
%%%
        fill_array([], _, _, _, []).
        fill_array([([I], Value)|Rest], From , To, Slice_elements, New_index_elements) :-
                (I = From ->
                        (fill_slice([([I], Value)|Rest], To, Slice_elements, Slice_index_elements),
                         fill_rest([([I], Value)|Rest], To, Rest_index_elements),
                         append(Slice_index_elements, Rest_index_elements, New_index_elements)
                        )
                ;
                        (fill_array(Rest, From , To, Slice_elements, Rest_index_elements),
                         append([([I], Value)], Rest_index_elements, New_index_elements)
                        )
                ).
%%%
                fill_slice([([I], _)|Rest], To, [(_, Value)|Rest_slice_elements], [([I], Value)|Rest_slice_index_elements]) :-
                        (I = To ->
                                Rest_slice_index_elements = []
                        ;
                                fill_slice(Rest, To, Rest_slice_elements, Rest_slice_index_elements)
                        ).
%%%
                fill_rest([([I], _Value)|Rest], To, Rest_index_elements) :-
                        (I = To ->
                                Rest_index_elements = Rest
                        ;
                                fill_rest(Rest, To, Rest_index_elements)
                        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%at least one of the elements must be different : easy to check for ground elements, delay otherwise
midoan_array__inequality(X, Y) :-
        midoan_array__get_all_index_elements(X, Index_elements_X),
        midoan_array__get_all_index_elements(Y, Index_elements_Y),
        !,
        midoan_solver:midoan_solver__inequality_constraint(Index_elements_X, Index_elements_Y, Outcome, Susp),        %see midoan_solver_main__inequality.pl
        (Outcome ==  yes ->     %at least one of the element is different
                true
        ;
         Outcome == dontknow ->
                (var(Susp) ->   %the suspension list is still a var: all the elements are ground (but the Outcome is dontknow so we fail)
                        fail
                ;
                        when(Susp, midoan_array__inequality(X, Y))      %all the ground elements are identical but there are some non ground elements
                )
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%is the index ground?
index_is_ground([]).
index_is_ground([Index|Rest_indexes]) :-
	(midoan_enum:midoan_enum__is_enum(Index) ->
                midoan_enum:midoan_enum__ground(Index)
        ;
         midoan_modular_integer:midoan_modular_integer__is_modular_integer(Index) ->
                midoan_modular_integer:midoan_modular_integer__ground(Index)
        ;
                ground(Index)
        ),
        index_is_ground(Rest_indexes).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% reverted to June 2008 version on 01/02/10 (problem was for partially assigned arrays)
midoan_array__up_array(Array, Index, Exp, Result, Type, Exception) :-
        midoan_array__get_type_var(Array, Array_type_var),
        midoan_type:midoan_type__variable_declaration(Result, Array_type_var),
        midoan_solver:midoan_solver__interpret(Result, types(_), _, Type, 'no_exception'),           %to get the type, cannot be unhandled nor raise exception otherwise the array would not exist
        (index_is_ground(Index) ->
                up_array_ground(Array, Index, Exp, Result, Exception)
        ;
                (constrain_indices(Index, Array_type_var, Exception),             %ensures that the indices are within the dimensions of the array
                 (common_util:common_util__is_an_exception(Exception) ->
                        true
                 ;
                        (build_suspension_all_ground(Index, Susp),
                         when(Susp, up_array_ground(Array, Index, Exp, Result, Exception))
                        )
                 )
                )
        ).
%%%
        %Eval_index is ground, the constraint disapear
        up_array_ground(Array, Index, Exp, Result, Exception) :-
                midoan_array__get_type_var(Array, Array_type_var),
                midoan_array__get_all_index_elements(Array, Indice_elements),
                up_array_ground2(Indice_elements, Index, Exp, [], New_indice_elements, Exception_raised),
                (Exception_raised == 'no_exception' ->
                        Exception = Exception_raised
                ;
                        Exception = exception(Exception_raised, [array(Array), index(Index), predicate(up_array_ground), message("Run-time constraint error in array update")])
                ),
                put_atts(New_array_tmp, midoan_array(Array_type_var, New_indice_elements)),
                midoan_solver:midoan_solver__controlled_unification(Result, New_array_tmp).     %could this raise an exception?
%%%
                up_array_ground2([], _Index, _Exp, _New_indice_elements, 'constraint_error').
                up_array_ground2([(Index1, Value)|Rest_indice_elements], Index, Exp, New_indice_elements_in, New_indice_elements_out, Exception) :-
                        (index_match(Index1, Index) ->
                                (append(New_indice_elements_in, [(Index1, Exp)|Rest_indice_elements], New_indice_elements_out),
                                 Exception = 'no_exception'
                                )
                        ;
                                (append(New_indice_elements_in, [(Index1, Value)], New_new_indice_elements_in),
                                 up_array_ground2(Rest_indice_elements, Index, Exp, New_new_indice_elements_in, New_indice_elements_out, Exception)
                                )
                        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%construst the goal (ground(I) , ground(J) ...) for all indices, wakes up iff all of the indices become ground
build_suspension_all_ground([I], ground(Var)) :-
        !,      %needed
        check_index(I, Var).
build_suspension_all_ground([I, J|Rest], (ground(Var); Rest2)) :-
        check_index(I, Var),
        build_suspension_all_ground([J|Rest], Rest2).

check_index(I, Var) :-
        (midoan_enum:midoan_enum__is_enum(I) ->
                midoan_enum:midoan_enum__get(position, I, Var)
        ;
         midoan_modular_integer:midoan_modular_integer__is_modular_integer(I) ->
                midoan_modular_integer:midoan_modular_integer__get(value, I, Var)
        ;
                Var = I
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%AsgL is the list of assignments that defines an array aggregate:
%AsgL is of the form for SPARK Ada:
%                       [exp, ..., exp]                                                 --positional
%          or           [exp, ..., exp, ([others], exp)]                                --positional
%          or           [([others], exp)]                                               --positional
%          or           [([Indices], exp), ..., ([Indices], exp)]                       --named
%          or           [([Indices], exp), ..., ([Indices], exp), ([others], exp)]      --named
%where Indices is of the form: for SPARK Ada
%                       [I1, ..., In]
%where I is of the form: for SPARK Ada
%                       exp
%          or           range_bounds(min, max)
%          or           range(typemark)
%          or           type_mark
midoan_array__create_array_from_agg(Type_var, AsgL, Array_var) :-                  %Type_var is never an unconstrained array here
        midoan_type:midoan_type__variable_declaration(Array_var, Type_var),
	midoan_array__get_all_index_elements(Array_var, Indice_elements),
        init_indice_elements(AsgL, Indice_elements).
        %put_atts(Array, midoan_array(Type_var, Indice_elements)).      %creates Array
%%%
        init_indice_elements([], []).
        init_indice_elements([First|Rest], Indice_elements) :-
                ((compound(First), First = named(_, _))->
                        init_indice_elements_named([First|Rest], Indice_elements)
                ;
                        init_indice_elements_positional([First|Rest], Indice_elements)
                ).
%%%
                init_indice_elements_positional([], []) :-      %an empty aggregate
                        !.
                init_indice_elements_positional([Element], []) :-
                        nonvar(Element),
                        Element = ([others], _Exp_asg),
                        !.
                init_indice_elements_positional([Element], [(_, Value)|Rest2]) :-
                        nonvar(Element),
                        Element = ([others], Exp_asg),
                        !,
                        midoan_solver:midoan_solver__controlled_unification(Exp_asg, Value),
                        init_indice_elements_positional([([others], Exp_asg)], Rest2).

                init_indice_elements_positional([Exp_asg|Rest], [(Index, Value)|Rest2]) :-
                          %Index \= [_] because Exp_asg could be an anonymous record !!!!
                        ((midoan_anon_aggregate:midoan_anon_aggregate__is_anon_aggregate(Exp_asg), Index \= [_])  ->	%then Exp_asg is an AsgL itself (for a multidimentional array)
                                (midoan_anon_aggregate:midoan_anon_aggregate__get_anon_aggregate_stuff(Exp_asg, AsgL_inner),
                                 AsgL_inner = [First|_],
                                 Index = [I|_],
                                 get_starters([(Index, Value)|Rest2], I, Start_of_index_elements, Rest_of_index_Elements),
                                 ((compound(First), First = named(_, _)) ->
                                        init_indice_elements_named(AsgL_inner, Start_of_index_elements)
                                 ;
                                        init_indice_elements_positional(AsgL_inner, Start_of_index_elements)
                                 ),
                                 init_indice_elements(Rest, Rest_of_index_Elements)
                                )
                        ;
                         midoan_string_literal:midoan_string_literal__is_string_literal(Exp_asg) ->   %we have a string_literal matching an array of literals representing characters (could be Roman_Digit...)
                                                                        %"cat" and [[2,1], Var_enum, [2,2], Var_enum, [2,3], Var_enum]
                                (Index = [I|_],
                                 get_starters([(Index, Value)|Rest2], I, Start_of_index_elements, Rest_of_index_Elements),
                                 %here Value may be a character or an array of characters
                                 (midoan_enum:midoan_enum__is_enum(Value) ->
                                        (midoan_enum:midoan_enum__get(type, Value, Character_type),
                                         Start_of_index_elements_inner = Start_of_index_elements
                                        )
                                 ;
                                  midoan_array__is_array(Value) ->      %for arrays of strings
                                        (midoan_array__get_all_index_elements(Value, [(_, V2)|_]),
                                         midoan_enum:midoan_enum__get(type, V2, Character_type),
                                         midoan_array__get_all_index_elements(Value, Start_of_index_elements_inner)
                                        )
                                 ),
                                 midoan_enum:midoan_enum__generate_list_of_enums(Exp_asg, Character_type, Ugly_agg),
                                 init_indice_elements_positional(Ugly_agg, Start_of_index_elements_inner),
                                 init_indice_elements(Rest, Rest_of_index_Elements)
                                )
                        ;
                                (midoan_solver:midoan_solver__controlled_unification(Exp_asg, Value),
                                 init_indice_elements(Rest, Rest2)
                                )
                        ).

%%%
init_indice_elements_named(AsgL, Index_elements) :-
        last(AsgL, Last_choice),
        (Last_choice = named([others], _Exp) ->
                init_indice_elements_named2(Index_elements, AsgL)       %all named with others at the end
        ;
                (transform_to_positional(AsgL, AsgL_positional),        %all named with no others at the end (there is a difference with respect to the bounds)
                 init_indice_elements_positional(AsgL_positional, Index_elements)
                )
        ).

transform_to_positional(AsgL, AsgL_positional) :-
        midoan_solver:find_lowest_largest(AsgL, Smallest, Largest),   %from midoan_solver_main__controlled_unification.pl
        gen_single_list(Indices, Smallest, Largest),    %e.g. [3, 4, 5, 6, 7]
        gen_positional(Indices, AsgL, AsgL_positional).

gen_positional([], _AsgL, []).
gen_positional([First|Rest], AsgL, [Exp|Rest_asg_positional]) :-
        find_named_element2(AsgL, First, Exp),
        gen_positional(Rest, AsgL, Rest_asg_positional).

init_indice_elements_named2([], _).
init_indice_elements_named2([(Indices, Value)|Rest], AsgL) :-
        Indices = [I|Rest_index],
        find_named_element2(AsgL, I, Exp),
        (Rest_index == [] ->
                (midoan_solver:midoan_solver__controlled_unification(Exp, Value),
                 init_indice_elements_named2(Rest, AsgL)
                )
        ;
                ((midoan_anon_aggregate:midoan_anon_aggregate__is_anon_aggregate(Exp) ->	%then Exp is an AsgL itself (for a multidimentional array)
                        (midoan_anon_aggregate:midoan_anon_aggregate__get_anon_aggregate_stuff(Exp, AsgL_inner),
                         AsgL_inner = [First|_],
                         get_starters([(Indices, Value)|Rest], I, Start_of_index_elements, Rest_of_index_Elements),
                         ((compound(First), First = named(_, _)) ->
                                init_indice_elements_named(AsgL_inner, Start_of_index_elements)
                         ;
                                init_indice_elements_positional(AsgL_inner, Start_of_index_elements)
                         ),
                         init_indice_elements_named2(Rest_of_index_Elements, AsgL)
                        )
                 ;
                  midoan_string_literal:midoan_string_literal__is_string_literal(Exp) ->       %we have a string_literal matching an array of literals representing characters (could be Roman_Digit...)
                                                                %"cat" and [[2,1], Var_enum, [2,2], Var_enum, [2,3], Var_enum]
                        (get_starters([(Indices, Value)|Rest], I, Start_of_index_elements, Rest_of_index_Elements),
                         %here Value may be a character or an array of characters
                         (midoan_enum:midoan_enum__is_enum(Value) ->
                                (midoan_enum:midoan_enum__get(type, Value, Character_type),
                                 Start_of_index_elements_inner = Start_of_index_elements
                                )
                         ;
                          midoan_array__is_array(Value) ->      %for arrays of strings
                                (midoan_array__get_all_index_elements(Value, [(_, V2)|_]),
                                 midoan_enum:midoan_enum__get(type, V2, Character_type),
                                 midoan_array__get_all_index_elements(Value, Start_of_index_elements_inner)
                                )
                         ),
                         midoan_enum:midoan_enum__generate_list_of_enums(Exp, Character_type, Ugly_agg),
                         init_indice_elements_positional(Ugly_agg, Start_of_index_elements_inner),
                         init_indice_elements_named2(Rest_of_index_Elements, AsgL)
                        )
                  ;
                        common_util:common_util__error(10, "Unknown expression in array initialisation", no_error_consequences, [(i, I), (exp, Exp)], 1067436, midoan_array, init_indice_elements_named2, no_localisation, no_extra_info)
                 )
                )
        ).
%%%
%s2parate list of indice_elements into 2, those who start with I as index and those who don't
get_starters([], _I, [], []).
get_starters([([I|Rest_index], Value)|Rest], I1, Start_of_index_elements, Rest_of_index_Elements) :-
        (index_match([I], [I1]) ->
                (get_starters(Rest, I1, Rest_of_start, Rest_of_index_Elements),
                 append([(Rest_index, Value)], Rest_of_start, Start_of_index_elements)
                )
        ;
                (get_starters(Rest, I1, Start_of_index_elements, Rest_of_rest),
                 append([([I|Rest_index], Value)], Rest_of_rest, Rest_of_index_Elements)
                )
        ).

%%%
%matches a list of indices (can be enums or integers)
index_match([I], [J]) :-
        !,
        midoan_solver:midoan_solver__sdl(=(I, J)).
index_match([I|Rest_I], [J|Rest_J]) :-
        index_match([I], [J]),
        index_match(Rest_I, Rest_J).

%find a particular element given a named AsgL
find_named_element2([], _, _) :-
	!,
	common_util:common_util__error(10, "Cannot find given index in array aggregate", no_error_consequences, no_arguments, 1073653, midoan_array, find_named_element2, no_localisation, no_extra_info).
find_named_element2([named(Choices, Exp)|Next], I, R) :-
        (Choices == [others] ->
                R = Exp
        ;
         in_choices(Choices, I) ->
	        R = Exp
	;
	        find_named_element2(Next, I, R)
	).
%%%
        %checking if an index is within a list of choices
        in_choices([], _) :-
                fail.
        in_choices([First|Next], I) :-
                (eq_index(First, I) ->
                    true
                ;
                    in_choices(Next, I)
                ).
%%%
                %all the different choices available
                %can fail if doesn't match
                eq_index(Choice, I) :-
                        (Choice == I ->
                                true
                        ;
                         (midoan_enum:midoan_enum__is_enum(Choice); midoan_modular_integer:midoan_modular_integer__is_modular_integer(Choice)) ->
                                midoan_solver:midoan_solver__sdl(=(Choice, I))
                        ;
                         Choice = [Min, Max] ->
                                (midoan_solver:midoan_solver__sdl(<=(Min, I)),
                                 midoan_solver:midoan_solver__sdl(<=(I, Max))
                                )
                        ;
                         Choice = range(Type_var) ->
                                (midoan_type:midoan_type__is_type(Type_var),
                                 midoan_type:midoan_type__get_attribute(Type_var, first, Min),
                                 midoan_type:midoan_type__get_attribute(Type_var, last, Max),
                                 midoan_solver:midoan_solver__sdl(<=(Min, I)),
                                 midoan_solver:midoan_solver__sdl(<=(I, Max))
                                )
                        ;
                         midoan_type:midoan_type__is_type(Choice) ->
                                eq_index(range(Choice), I)
                        ;
                                fail
                        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%one dimentional array concatenation
%both operands may be: an array, an anonymous aggregate, a string literal, a component value
midoan_array__concatenate(Type, Type, Le, Ri, Aggregate) :-
        (Type = i ; Type = e ; Type = record ; Type = r ; Type = modular_integer),
        !,
        midoan_anon_aggregate:midoan_anon_aggregate__create_anon_aggregate([Le, Ri], Aggregate).      %positional
midoan_array__concatenate(anonymous_aggregate, Type, Le, Ri, Aggregate) :-
        (Type = i ; Type = e ; Type = record ; Type = r ; Type = modular_integer),
        !,
        midoan_anon_aggregate:midoan_anon_aggregate__get_anon_aggregate_stuff(Le, AsgL),
        (AsgL == [] ->
                New_asgL = [Ri]
        ;
                (AsgL = [First|_Rest],
                 ((compound(First), First = named(_, _))->
                        concatenate_named_aggregate_with_index_elements_to_named_aggregate(AsgL, [(_, Ri)], New_asgL)
                 ;
                        concatenate_positional_aggregate_with_index_elements_to_positional_aggregate(AsgL, [(_, Ri)], New_asgL)
                 )
                )
        ),
        midoan_anon_aggregate:midoan_anon_aggregate__create_anon_aggregate(New_asgL, Aggregate).
midoan_array__concatenate(Type, anonymous_aggregate, Le, Ri, Aggregate) :-
        (Type = i ; Type = e ; Type = record ; Type = r ; Type = modular_integer),
        !,
        midoan_anon_aggregate:midoan_anon_aggregate__get_anon_aggregate_stuff(Ri, AsgL),
        (AsgL == [] ->
                AsgL_positional = []
        ;
                (AsgL = [First|_Rest],
                 ((compound(First), First = named(_, _))->
                        transform_to_positional(AsgL, AsgL_positional)  %others is not allowed by gnat here
                 ;
                        AsgL_positional = AsgL
                 )
                )
        ),
        append([Le], AsgL_positional, New_asgL),
        midoan_anon_aggregate:midoan_anon_aggregate__create_anon_aggregate(New_asgL, Aggregate).
midoan_array__concatenate(string_literal, string_literal, Le, Ri, String_literal) :-
        !,
        midoan_string_literal:midoan_string_literal__get_ascii_codes(Le, Le_codes),           %works if string_literal is empty
        midoan_string_literal:midoan_string_literal__get_ascii_codes(Ri, Ri_codes),           %works if string_literal is empty
        append(Le_codes, Ri_codes, New_codes),
        midoan_string_literal:midoan_string_literal__create_string_literal(New_codes, String_literal).
midoan_array__concatenate(string_literal, e, Le, Ri, Anon_aggregate) :-      %for a string it cannot be anything else than an enumeration literal
        !,
        midoan_enum:midoan_enum__get(type, Ri, Character_type),
        midoan_enum:midoan_enum__generate_list_of_enums(Le, Character_type, Start_agg),
        append(Start_agg, [Ri], Array_agg),
        midoan_anon_aggregate:midoan_anon_aggregate__create_anon_aggregate(Array_agg, Anon_aggregate).        %positional
midoan_array__concatenate(e, string_literal, Le, Ri, Anon_aggregate) :-         %for a string it cannot be anything else than an enumeration literal
        !,
        midoan_enum:midoan_enum__get(type, Le, Character_type),
        midoan_enum:midoan_enum__generate_list_of_enums(Ri, Character_type, End_agg),
        append([Le], End_agg, Array_agg),
        midoan_anon_aggregate:midoan_anon_aggregate__create_anon_aggregate(Array_agg, Anon_aggregate).        %positional
midoan_array__concatenate(array, array, Le, Ri, Anon_aggregate) :-
        !,
        midoan_array__get_all_index_elements(Le, Index_elementLe),
        (Index_elementLe == [] ->
                Anon_aggregate = Ri     %an array in this special case
        ;
                (midoan_array__get_all_index_elements(Ri, Index_elementRi),
                 midoan_array__get_type_var(Le, Array_type_var),
                 midoan_type:midoan_type__obtain_basetype(Array_type_var, Array_basetype),
                 (Array_basetype = array(_Component_type_var) -> %constrained use array subptype index : i.e. similar to a positional aggregate
                        concatenate_index_element_with_index_element_to_positional_aggregate(Index_elementLe, Index_elementRi, AsgL)
                 ;
                  Array_basetype = unconst_array(_Component_type_var) -> %unconstrained use left operand index : i.e. similar to a named aggregate
                        concatenate_index_element_with_index_element_to_named_aggregate(Index_elementLe, Index_elementRi, AsgL)
                 ;
                        common_util:common_util__error(10, "Unknow array basetype", no_error_consequences, [(array_basetype, Array_basetype)], 1086837, midoan_array, midoan_array__concatenate, no_localisation, no_extra_info)
                 ),
                 midoan_anon_aggregate:midoan_anon_aggregate__create_anon_aggregate(AsgL, Anon_aggregate)
                )
        ).
midoan_array__concatenate(Type, array, Le, Ri, Anon_aggregate) :-
        (Type = i ; Type = e ; Type = record ; Type = r ; Type = modular_integer),
        !,
        midoan_array__get_all_index_elements(Ri, Index_elementRi),
        midoan_array__get_type_var(Ri, Array_type_var),
        midoan_type:midoan_type__obtain_basetype(Array_type_var, Array_basetype),
        (Array_basetype = array(_Component_type_var) -> %constrained use array subptype index : i.e. similar to a positional aggregate
                concatenate_index_element_with_index_element_to_positional_aggregate([(_, Le)], Index_elementRi, AsgL)
        ;
         Array_basetype = unconst_array(_Component_type_var) -> %unconstrained use left operand index (here same as an unconstrained aggregate) : i.e. similar to a positional aggregate
                concatenate_index_element_with_index_element_to_positional_aggregate([(_, Le)], Index_elementRi, AsgL)
        ;
                common_util:common_util__error(10, "Unknow array basetype", no_error_consequences, [(array_basetype, Array_basetype)], 1086897, midoan_array, midoan_array__concatenate, no_localisation, no_extra_info)
        ),
        midoan_anon_aggregate:midoan_anon_aggregate__create_anon_aggregate(AsgL, Anon_aggregate).
midoan_array__concatenate(anonymous_aggregate, array, Le, Ri, Anon_aggregate) :-
        !,
        midoan_anon_aggregate:midoan_anon_aggregate__get_anon_aggregate_stuff(Le, AsgL),
        (AsgL == [] ->
                Anon_aggregate = Ri     %an array in this case
        ;
                (midoan_array__get_all_index_elements(Ri, Index_elementRi),
                 midoan_array__get_type_var(Ri, Array_type_var),
                 midoan_type:midoan_type__obtain_basetype(Array_type_var, Array_basetype),
                 (Array_basetype = array(_Component_type_var) -> %constrained use array subptype index : i.e. similar to a positional aggregate
                        (AsgL = [First|_Rest],
                         ((compound(First), First = named(_, _))->
                                transform_to_positional(AsgL, AsgL_positional)  %others is not allowed by gnat here
                         ;
                                AsgL_positional = AsgL
                         ),
                         concatenate_positional_aggregate_with_index_elements_to_positional_aggregate(AsgL_positional, Index_elementRi, New_asgL),
                         midoan_anon_aggregate:midoan_anon_aggregate__create_anon_aggregate(New_asgL, Anon_aggregate)
                        )
                 ;
                  Array_basetype = unconst_array(_Component_type_var) -> %unconstrained use left operand index (here same as an unconstrained aggregate) : i.e. similar to a positional aggregate
                        (AsgL = [First|_Rest],
                         ((compound(First), First = named(_, _))->
                                concatenate_named_aggregate_with_index_elements_to_named_aggregate(AsgL, Index_elementRi, New_asgL)
                         ;
                                concatenate_positional_aggregate_with_index_elements_to_positional_aggregate(AsgL, Index_elementRi, New_asgL)
                         ),
                         midoan_anon_aggregate:midoan_anon_aggregate__create_anon_aggregate(New_asgL, Anon_aggregate)
                        )
                 ;
                        common_util:common_util__error(10, "Unknow array basetype", no_error_consequences, [(array_basetype, Array_basetype)], 1086897, midoan_array, midoan_array__concatenate, no_localisation, no_extra_info)
                 )
                )
        ).
midoan_array__concatenate(array, string_literal, Le, Ri, Array_var) :-
        !,
        midoan_array__get_type_var(Le, Array_type_var),
        midoan_type:midoan_type__obtain_basetype(Array_type_var, array(Component_type_var)),
        midoan_enum:midoan_enum__generate_list_of_enums(Ri, Component_type_var, Enum_list),
        midoan_anon_aggregate:midoan_anon_aggregate__create_anon_aggregate(Enum_list, Anon_aggregate),
        midoan_array__concatenate(array, anonymous_aggregate, Le, Anon_aggregate, Array_var).

midoan_array__concatenate(string_literal, array, Le, Ri, Array_var) :-
        !,
        midoan_array__get_type_var(Ri, Array_type_var),
        midoan_type:midoan_type__obtain_basetype(Array_type_var, Array_basetype),
        (Array_basetype = array(Component_type_var) ->
                true    %just to retrieve the Component_type_var
        ;
         Array_basetype = unconst_array(Component_type_var) ->
                true    %just to retrieve the Component_type_var
        ;
                common_util:common_util__error(10, "Unknow array basetype", no_error_consequences, [(array_basetype, Array_basetype)], 1086896, midoan_array, midoan_array__concatenate, no_localisation, no_extra_info)
        ),
        midoan_enum:midoan_enum__generate_list_of_enums(Le, Component_type_var, Enum_list),
        midoan_anon_aggregate:midoan_anon_aggregate__create_anon_aggregate(Enum_list, Anon_aggregate),
        midoan_array__concatenate(anonymous_aggregate, array, Anon_aggregate, Ri, Array_var).

midoan_array__concatenate(array, Type, Le, Ri, Array_var) :-
        (Type = i ; Type = e ; Type = record ; Type = r ; Type = modular_integer),
        !,
        midoan_anon_aggregate:midoan_anon_aggregate__create_anon_aggregate([Ri], Anon_aggregate),
        midoan_array__concatenate(array, anonymous_aggregate, Le, Anon_aggregate, Array_var).

midoan_array__concatenate(array, anonymous_aggregate, Le, Ri, Anon_aggregate) :-
        !,
        midoan_array__get_all_index_elements(Le, Index_elementLe),
        midoan_anon_aggregate:midoan_anon_aggregate__get_anon_aggregate_stuff(Ri, AsgL),
        (Index_elementLe == [] ->
                Anon_aggregate = Ri
        ;
                (midoan_array__get_type_var(Le, Array_type_var),
                 midoan_type:midoan_type__obtain_basetype(Array_type_var, Array_basetype),
                 (Array_basetype = array(Component_type_var) -> %constrained use array subptype index : i.e. similar to a positional aggregate
                        ((AsgL == [] ->
                                concatenate_index_element_with_index_element_to_positional_aggregate(Index_elementLe, [], New_asgL)
                         ;
                                (AsgL = [First|_Rest],
                                 ((compound(First), First = named(_, _))->
                                        transform_to_positional(AsgL, AsgL_positional)  %others is not allowed by gnat here
                                 ;
                                        AsgL_positional = AsgL
                                 ),
                                 concatenate_index_element_with_index_element_to_positional_aggregate(Index_elementLe, [], Start_new_asgL),
                                 append(Start_new_asgL, AsgL_positional, New_asgL)
                                )
                         ),
                         midoan_anon_aggregate:midoan_anon_aggregate__create_anon_aggregate(New_asgL, Anon_aggregate)
                        )
                 ;
                  Array_basetype = unconst_array(Component_type_var) -> %unconstrained use left operand index : i.e. similar to a named aggregate
                        ((AsgL == [] ->
                                concatenate_index_element_with_positional_aggregate_to_named_aggregate(Index_elementLe, [], New_asgL)
                         ;
                                (AsgL = [First|_Rest],
                                 ((compound(First), First = named(_, _))->
                                        transform_to_positional(AsgL, AsgL_positional)  %others is not allowed by gnat here
                                 ;
                                        AsgL_positional = AsgL
                                 ),
                                 concatenate_index_element_with_positional_aggregate_to_named_aggregate(Index_elementLe, AsgL_positional, New_asgL)
                                )
                         ),
                         midoan_anon_aggregate:midoan_anon_aggregate__create_anon_aggregate(New_asgL, Anon_aggregate)
                        )
                 ;
                  common_util:common_util__error(10, "Unknow array basetype", no_error_consequences, [(array_basetype, Array_basetype)], 1086831, midoan_array, midoan_array__concatenate, no_localisation, no_extra_info)
                 )
                )
        ).

midoan_array__concatenate(anonymous_aggregate, anonymous_aggregate, Le, Ri, Anon_aggregate) :-
        !,
        midoan_anon_aggregate:midoan_anon_aggregate__get_anon_aggregate_stuff(Le, AsgLe),
        (AsgLe == [] ->
                Anon_aggregate = Ri
        ;
                (midoan_anon_aggregate:midoan_anon_aggregate__get_anon_aggregate_stuff(Ri, AsgRi),
                 (AsgRi == [] ->
                        Anon_aggregate = Le
                 ;
                        (AsgRi = [First|_Rest],
                         ((compound(First), First = named(_, _))->
                                transform_to_positional(AsgRi, AsgL_positional)
                          ;
                                AsgL_positional = AsgRi
                          ),
                          AsgLe = [First1|_Rest1],
                          ((compound(First1), First1 = named(_, _))->
                                concatenate_named_aggregate_with_positional_aggregate_to_named_aggregate(AsgLe, AsgL_positional, New_asgL)
                          ;
                                append(AsgLe, AsgL_positional, New_asgL)
                          ),
                          midoan_anon_aggregate:midoan_anon_aggregate__create_anon_aggregate(New_asgL, Anon_aggregate)
                        )
                 )
                )
        ).

%this is difficult : need to obatin the type of the elements from the aggregate
midoan_array__concatenate(anonymous_aggregate, string_literal, Le, Ri, Anon_aggregate) :-
        !,
        midoan_anon_aggregate:midoan_anon_aggregate__get_anon_aggregate_stuff(Le, AsgLe),
        (AsgLe == [] ->
                Anon_aggregate = Ri
        ;
                (obtain_enum_element_type_from_aggregate(AsgLe, Component_type_mark),
                 midoan_enum:midoan_enum__generate_list_of_enums(Ri, Component_type_mark, Enum_list),
                 midoan_anon_aggregate:midoan_anon_aggregate__create_anon_aggregate(Enum_list, Ri_anon_aggregate),
                 midoan_array__concatenate(anonymous_aggregate, anonymous_aggregate, Le, Ri_anon_aggregate, Anon_aggregate)
                )
        ).
midoan_array__concatenate(string_literal, anonymous_aggregate, Le, Ri, Anon_aggregate) :-
        !,
        midoan_anon_aggregate:midoan_anon_aggregate__get_anon_aggregate_stuff(Ri, AsgRi),
        (AsgRi == [] ->
                Anon_aggregate = Le
        ;
                (obtain_enum_element_type_from_aggregate(AsgRi, Component_type_mark),
                 midoan_enum:midoan_enum__generate_list_of_enums(Le, Component_type_mark, Enum_list),
                 midoan_anon_aggregate:midoan_anon_aggregate__create_anon_aggregate(Enum_list, Le_anon_aggregate),
                 midoan_array__concatenate(anonymous_aggregate, anonymous_aggregate, Le_anon_aggregate, Ri, Anon_aggregate)
                )
        ).

midoan_array__concatenate(Le_type, Ri_type, _Le, _Ri, unhandled_expression) :-
        common_util:common_util__error(9, "Not yet implemented: array concatenation for these types", no_error_consequences, [(le_type, Le_type), (ri_type, Ri_type)], 979742, midoan_array, midoan_array__concatenate, no_localisation, no_extra_info).

%%%
obtain_enum_element_type_from_aggregate([First|Rest], Component_type_mark) :-
        ((compound(First), First = named(_, _))->
                transform_to_positional([First|Rest], AsgL_positional)
        ;
                AsgL_positional = [First|Rest]
        ),
        AsgL_positional = [Element|_],
        midoan_enum:midoan_enum__get(type, Element, Component_type_mark).

%%%
shift_indices([], _Index_shift, []).
shift_indices([(Index, Element)|Rest], Index_shift, [(New_index, Element)|Rest_out]) :-
        midoan_array__shift_single_index(Index_shift, Index, New_index),
        shift_indices(Rest, Index_shift, Rest_out).

midoan_array__shift_single_index(By, From, To) :-
        ((midoan_solver:midoan_solver__is_integer(From); integer(From)) ->
                Type = i
	;
	 midoan_enum:midoan_enum__is_enum(From) ->
	        Type = e
        ;
         midoan_modular_integer:midoan_modular_integer__is_modular_integer(From) ->
                Type = modular_integer
        ),
        (By < 0 ->
                (midoan_solver:midoan_solver__interpret(pred(From), types(Type), To, Type_pred, _Exception_pred),
                 (Type_pred == unhandled_expression ->
                        %23/09/09 (see electronic diary) better would be to propagate the unhandled expression upwards ...
                        common_util:common_util__error(10, "Unhandled type", no_error_consequences, [(type_pred, Type_pred)], 1086053, mika_solver_array, midoan_array__shift_single_index, no_localisation, no_extra_info)
                 ;
                        true
                 )
                )
        ;
                shift_single_index(By, Type, From, To)
        ).
%%%
        shift_single_index(0, _, To, To) :-
                !.
        shift_single_index(By, Type, From, To) :-
                midoan_solver:midoan_solver__interpret(succ(From), types(Type), Next, Type_succ, _Exception_succ),
                (Type_succ == unhandled_expression ->
                        %23/09/09 (see electronic diary) better would be to propagate the unhandled expression upwards ...
                        common_util:common_util__error(10, "Unhandled type", no_error_consequences, [(type_succ, Type_succ)], 1087555, mika_solver_array, shift_single_index, no_localisation, no_extra_info)
                ;
                        (By1 is By - 1,
                         shift_single_index(By1, Type, Next, To)
                        )
                ).
%%%
concatenate_index_element_with_index_element_to_positional_aggregate([], Index_elementRi, AsgL) :-
        concatenate_index_element_with_index_element_to_positional_aggregate_rhs(Index_elementRi, AsgL).
concatenate_index_element_with_index_element_to_positional_aggregate([(_, Element)|Rest], Index_elementRi, [Element|Rest_asgL]) :-
        concatenate_index_element_with_index_element_to_positional_aggregate(Rest, Index_elementRi, Rest_asgL).

concatenate_index_element_with_index_element_to_positional_aggregate_rhs([], []).
concatenate_index_element_with_index_element_to_positional_aggregate_rhs([(_, Element)|Rest], [Element|Rest_asgL]) :-
        concatenate_index_element_with_index_element_to_positional_aggregate_rhs(Rest, Rest_asgL).

%%%
%generate a named AsgL to allow the creation of an aggregate
%we use the first index of the lhs in the named aggregate
concatenate_index_element_with_index_element_to_named_aggregate([(Last_index, Last_element)], Index_elementRi, New_asgL) :-
        !,      %needed
        Last_index = [The_index],
        midoan_solver:get_var_type(The_index, Type_index),
        midoan_solver:midoan_solver__interpret(succ(The_index), types(Type_index), Next_index, Type_succ, _Exception_succ),
        (Type_succ == unhandled_expression ->
                %23/09/09 (see electronic diary) better would be to propagate the unhandled expression upwards ...
                common_util:common_util__error(10, "Unhandled type", no_error_consequences, [(type, Type_succ)], 1095519, mika_solver_array, concatenate_index_element_with_index_element_to_named_aggregate, no_localisation, no_extra_info)
        ;
                true
        ),
        transform_index_elements_to_named_aggregate(Index_elementRi, Type_index, Next_index, Named_aggregate),
        append([named(Last_index, Last_element)], Named_aggregate, New_asgL).
concatenate_index_element_with_index_element_to_named_aggregate([(Last_index, Last_element)|Rest_index_elements], Index_elementRi, [named(Last_index, Last_element)|Rest_asgL]) :-
        concatenate_index_element_with_index_element_to_named_aggregate(Rest_index_elements, Index_elementRi, Rest_asgL).

%%%
concatenate_positional_aggregate_with_index_elements_to_positional_aggregate([], Index_elements, New_asgL) :-
        concatenate_positional_aggregate_with_index_elements_to_positional_aggregate_rhs(Index_elements, New_asgL).
concatenate_positional_aggregate_with_index_elements_to_positional_aggregate([Exp|Rest], Index_elements, [Exp|Rest_new_asgL]) :-
        concatenate_positional_aggregate_with_index_elements_to_positional_aggregate(Rest, Index_elements, Rest_new_asgL).
concatenate_positional_aggregate_with_index_elements_to_positional_aggregate_rhs([], []).
concatenate_positional_aggregate_with_index_elements_to_positional_aggregate_rhs([(_, Element)|Rest], [Element|Rest_new_asgL]) :-
        concatenate_positional_aggregate_with_index_elements_to_positional_aggregate_rhs(Rest, Rest_new_asgL).

%%%
concatenate_named_aggregate_with_index_elements_to_named_aggregate(AsgL, Index_elements, New_asgL) :-
        midoan_solver:find_lowest_largest(AsgL, _Smallest, Largest),
        midoan_solver:get_var_type(Largest, Type_index),
        midoan_solver:midoan_solver__interpret(succ(Largest), types(Type_index), Next_index, Type_succ, _Exception_succ),
        (Type_succ == unhandled_expression ->
                %23/09/09 (see electronic diary) better would be to propagate the unhandled expression upwards ...
                common_util:common_util__error(10, "Unhandled type", no_error_consequences, [(type, Type_succ)], 1095599, mika_solver_array, concatenate_named_aggregate_with_index_elements_to_named_aggregate, no_localisation, no_extra_info)
        ;
                true
        ),
        transform_index_elements_to_named_aggregate(Index_elements, Type_index, Next_index, Named_aggregate),
        append(AsgL, Named_aggregate, New_asgL).
transform_index_elements_to_named_aggregate([], _Type_index, _Next_index, []).
transform_index_elements_to_named_aggregate([(_, Last_element)], _Type_index, Next_index, [named([Next_index], Last_element)]) :-
        !.      %needed
transform_index_elements_to_named_aggregate([(_, Next_element)|Rest], Type_index, Next_index, [named([Next_index], Next_element)|Rest_asgL]) :-
        midoan_solver:midoan_solver__interpret(succ(Next_index), types(Type_index), Next_next_index, _Type_succ, _Exception_succ),
        transform_index_elements_to_named_aggregate(Rest, Type_index, Next_next_index, Rest_asgL).

%Index_elements is never [] when called
concatenate_index_element_with_positional_aggregate_to_named_aggregate([([Last_index], Last_element)], AsgL, [named([Last_index], Last_element)|Rest]) :-
        !,      %needed
        midoan_solver:get_var_type(Last_index, Type_index),
        midoan_solver:midoan_solver__interpret(succ(Last_index), types(Type_index), Next_index, Type_succ, _Exception_succ),
        (Type_succ == unhandled_expression ->
                %23/09/09 (see electronic diary) better would be to propagate the unhandled expression upwards ...
                common_util:common_util__error(10, "Unhandled type", no_error_consequences, [(type, Type_succ)], 1095597, mika_solver_array, concatenate_index_element_with_positional_aggregate_to_named_aggregate, no_localisation, no_extra_info)
        ;
                true
        ),
        transform_positional_aggregate_to_named_aggregate(AsgL, Type_index, Next_index, Rest).
concatenate_index_element_with_positional_aggregate_to_named_aggregate([([Next_index], Next_element)|Rest], AsgL, [named([Next_index], Next_element)|Rest_AsgL]) :-
        concatenate_index_element_with_positional_aggregate_to_named_aggregate(Rest, AsgL, Rest_AsgL).

transform_positional_aggregate_to_named_aggregate([], _Type_index, _Next_index, []).
transform_positional_aggregate_to_named_aggregate([Last_element], _Type_index, Next_index, [named([Next_index], Last_element)]) :-
        !.      %needed
transform_positional_aggregate_to_named_aggregate([Next_element|Rest], Type_index, Next_index, [named([Next_index], Next_element)|Rest_AsgL]) :-
        midoan_solver:midoan_solver__interpret(succ(Next_index), types(Type_index), Next_next_index, _Type_succ, _Exception_succ),
        transform_positional_aggregate_to_named_aggregate(Rest, Type_index, Next_next_index, Rest_AsgL).

concatenate_named_aggregate_with_positional_aggregate_to_named_aggregate(AsgL, AsgL_positional, New_asgL) :-
        midoan_solver:find_lowest_largest(AsgL, _Smallest, Largest),
        midoan_solver:get_var_type(Largest, Type_index),
        midoan_solver:midoan_solver__interpret(succ(Largest), types(Type_index), Next_index, Type_succ, _Exception_succ),
        (Type_succ == unhandled_expression ->
                %23/09/09 (see electronic diary) better would be to propagate the unhandled expression upwards ...
                common_util:common_util__error(10, "Unhandled type", no_error_consequences, [(type, Type_succ)], 1095593, mika_solver_array, concatenate_named_aggregate_with_positional_aggregate_to_named_aggregate, no_localisation, no_extra_info)
        ;
                true
        ),
        transform_positional_aggregate_to_named_aggregate(AsgL_positional, Type_index, Next_index, Named_aggregate),
        append(AsgL, Named_aggregate, New_asgL).
%%%
%generate a list of indices between Min and Max
%gen_single_list(1, 4, [1, 2, 3, 4]). (could be enumeration literals)
gen_single_list(List, Min, Max) :-
        (midoan_solver:midoan_solver__sdl(Min > Max) ->  %an empty range
                List = []
        ;
                (midoan_solver:midoan_solver__interpret(Min, types(_), _, Type, 'no_exception'),
                 (Type == unhandled_expression ->
                        %23/09/09 (see electronic diary) better would be to propagate the unhandled expression upwards ...
                        common_util:common_util__error(10, "Unhandled type", no_error_consequences, [(type, Type)], 1020518, mika_solver_array, gen_single_list, no_localisation, no_extra_info)
                 ;
                        true
                 ),
                 gen_single_list2(Min, Max, List, Type)
                )
        ).
%%%
        gen_single_list2(From, To, List, Type) :-
                (From == To ->
                        List = [To]
                ;
                        ((Type == i ->
                                Next is From + 1        %for efficiency
                         ;
                                (midoan_solver:midoan_solver__interpret(succ(From), types(Type), Next, Type_succ, _Exception_succ),
                                 (Type_succ == unhandled_expression ->   %23/09/09 (see electronic diary) better would be to propagate the unhandled expression upwards ...
                                         common_util:common_util__error(10, "Unhandled type", no_error_consequences, [(type, Type_succ)], 1021219, mika_solver_array, gen_single_list, no_localisation, no_extra_info)
                                 ;
                                        true
                                 )
                                )
                         ),
                         List = [From|Rest],
                         gen_single_list2(Next, To, Rest, Type)
                        )
                ).
%%%
%ensures that the indices are within the dimensions of the array
constrain_indices(Index, Array_type_var, Exception) :-
        constrain_indices(Index, Array_type_var, 1, Exception).

        constrain_indices([], _Array_type_var, _Dimension, 'no_exception').
        constrain_indices([Index|Rest_indices], Array_type_var, Dimension, Exception) :-
                ((midoan_solver:midoan_solver__interpret(first(Array_type_var, Dimension), types('type'), Min, _, 'no_exception')       %cannot be unhandled nor raise exception otherwise the array would not exist
                 , midoan_solver:midoan_solver__interpret(last(Array_type_var, Dimension), types('type'), Max, _, 'no_exception')        %cannot be unhandled nor raise exception otherwise the array would not exist
                 ) ->
                        (( midoan_solver:midoan_solver__sdl('<'(Index, Min)) ; midoan_solver:midoan_solver__sdl('>'(Index, Max)) ) -> %can we have a run time error?
                                Exception = exception('constraint_error', [array(Array_type_var), index(Index), dimension(Dimension), predicate(constrain_indices), message("Run-time constraint error in array index: index is out of range")])
                        ;
                                (Dimension_1 is Dimension + 1,
                                 constrain_indices(Rest_indices, Array_type_var, Dimension_1, Exception)
                                )
                        )
                ;
                        Exception = exception('constraint_error', [array(Array_type_var), index(Index), dimension(Dimension), predicate(constrain_indices), message("Run-time constraint error in array index: too many indices")])
                ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%