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
% midoan_solver_type.pl
% defines the module type for solver type meta variables handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a type variable R is of the form: R{midoan_type(Type_mark, Attribute_list)}, where Attribute_list is a list of (Attribute, Value)
:- module(midoan_type, []).

:- use_module([	library(atts),	%Sicstus attribute library
		library(lists),	%for append/3
                library(clpr)
	     ]).

:- attribute midoan_type/2.   %name of the attribute

:- dynamic      standard_type/2.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%internal predicate (see atts library documentation)
%called prior to unification
%Value is a non-variable or another attributed variable (if Value is a plain variable verify_attributes is not called)
verify_attributes(Var, Value, Goals) :-
        (get_atts(Var, midoan_type(Type_mark, _Attribute_list)) ->
                (var(Value) ->  %unification of a type variable with a variable (should always be the case)
                        (midoan_type__is_type(Value) ->
                                (get_atts(Value, midoan_type(Type_mark2, _Attribute_list2)),   %two type variables unification
                                 Goals = [Type_mark = Type_mark2]        %Unification update is deferred
                                )
                        ;
                                %should never happen record are only unified with other record varaibles
                                common_util:common_util__error(10, "Unification Error", no_error_consequences, no_arguments, 104758, midoan_type, verify_attributes, no_localisation, "Unification of a type variable with a non type variable in midoan_type module.")
                        )
                ;
                        fail    %unification of a type var with a nonvar
                )
        ;
                Goals = []     %no actions as we are not concerned : not a record variable
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%check if a variable is a record variable
midoan_type__is_type(Var) :-
	var(Var),
	get_atts(Var, midoan_type(_, _)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_type__unput(Type_var, Typemark) :-
        midoan_type__get_typemark(Type_var, Typemark),
        put_atts(Type_var, -midoan_type(_, _)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_type__get_typemark(Type_var, Typemark) :-
        get_atts(Type_var, midoan_type(Typemark, _Attribute_list)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%obtain the basic basetype.
midoan_type__obtain_basetype(Type_var, Basic_basetype) :-
        midoan_type__get_attribute(Type_var, basetype, Parent_type_var),
        !,
        ((Parent_type_var == 'base_enumeration' ; Parent_type_var == 'record' ; Parent_type_var == 'standard.ads:float' ; Parent_type_var == 'modular_integer'
          ; Parent_type_var == 'standard.ads:integer' ; Parent_type_var == 'private'; Parent_type_var == 'incomplete_type'; Parent_type_var = array(_) ; Parent_type_var = unconst_array(_))  ->
                Basic_basetype = Parent_type_var
        ;
                midoan_type__obtain_basetype(Parent_type_var, Basic_basetype)
        ).
midoan_type__obtain_basetype(Type_var, _Basic_basetype) :-
        common_util:common_util__error(10, "A type has no basetype", 'no_error_consequences', [(type_var, Type_var)], 1034110, midoan_type, midoan_type__obtain_basetype, no_localisation, no_extra_info).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_type__obtain_parenttype(Type_var, Parent_type_var) :-
        midoan_type__get_attribute(Type_var, basetype, Subtype_var2),
        !,
        ((Subtype_var2 == 'base_enumeration' ; Subtype_var2 == 'record' ; Subtype_var2 == 'standard.ads:float' ; Subtype_var2 == 'modular_integer'
          ; Subtype_var2 == 'standard.ads:integer' ; Subtype_var2 == 'private'; Subtype_var2 == 'incomplete_type'; Subtype_var2 = array(_) ; Subtype_var2 = unconst_array(_))  ->
                Parent_type_var = Type_var
        ;
                midoan_type__obtain_parenttype(Subtype_var2, Parent_type_var)
        ).
midoan_type__obtain_parenttype(Type_var, _Parent_type_var) :-
        common_util:common_util__error(10, "A type has no basetype", no_error_consequences, [(type_var, Type_var)], 1046111, midoan_type, midoan_type__obtain_parenttype, no_localisation, no_extra_info).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_type__get_attribute(Type_var, Attribute, Value) :-
        get_atts(Type_var, midoan_type(_Typemark, Attribute_list)),
        (memberchk((Attribute, Value_tmp), Attribute_list) ->
                (Attribute == 'declaration' ->
                        (Value_tmp = declaration(Call_declaration, Value_template),
                         mika_symbolic:my_copy_term(single(Value_template), declaration(Call_declaration, Value_template), declaration(Call_declarationc, Value)),
                         call(Call_declarationc)
                        )
                ;
                        Value = Value_tmp
                )
        ;

                (midoan_type__get_attribute(Type_var, 'basetype', Basetype_var),
                 (midoan_type__is_type(Basetype_var) ->
                        midoan_type__get_attribute(Basetype_var, Attribute, Value)      %get the same value as the parent type : quite nice -> fewer repetitions
                 ;
                        fail
                 )
                )
        ),
        !.
midoan_type__get_attribute(Type_var, Attribute, Value) :-
        (Attribute == 'has_fields_with_default' ->
                Value = 'no'     %default value
        ;
         Attribute == 'size' ->
                Value = 999     %default value
        ;
         Attribute == 'max_size_in_storage_elements' ->
                Value = 999     %default value (for enumeration types : min size should be smallest int such as 2 power size > len Literals_name_valueL)
                                %              (for record types : min size should be the sum of all field's size)
        ;
         Attribute == 'printable' ->
                Value = 'not_built'
        ;
                 common_util:common_util__error(10, "Unknow type attribute", 'no_error_consequences', [(type_var, Type_var), (attribute, Attribute)], 107812, midoan_type, midoan_type__get_attribute, no_localisation, no_extra_info)
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%updates the list of attributes of Type_var for the Attribute with Value
midoan_type__update_type_attribute(Type_var, Attribute, Value) :-
        get_atts(Type_var, midoan_type(Typemark, Attribute_list)),
        (nth(_N, Attribute_list, (Attribute, _Old_value), Rest) ->      %it already exist
                New_attribute_list = [(Attribute, Value)|Rest]          %it is changed
        ;
                New_attribute_list = [(Attribute, Value)|Attribute_list]        %first time set
        ),
        put_atts(Type_var, midoan_type(Typemark, New_attribute_list)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_type__create_type(Type_mark, 'incomplete_type', Type_var) :-
        !,
        put_atts(Type_var, midoan_type(Type_mark, [('basetype', 'incomplete_type')])).      %we know nothing else about the type
midoan_type__create_type(Type_mark, 'private', Type_var) :-
        !,
        put_atts(Type_var, midoan_type(Type_mark, [('basetype', 'private')])).              %we know nothing else about the type
midoan_type__create_type(Type_mark, 'null_record', Type_var) :-
        !,
        midoan_type__create_type(Type_mark, record, [], Type_var),
        midoan_type__update_type_attribute(Type_var, size, 0).
midoan_type__create_type(Type_mark, 'enumeration', Literals_name_valueL, Type_var) :-
        !,
        midoan_enum:midoan_enum__get_literal_positions(Type_var, Literals_name_valueL, First, Last, Literal_positionL), %internal recording enum
        declare_enum_type(Type_mark, First, Last, Literal_positionL, Type_var),
        (Type_mark == 'standard.ads:boolean' ->
                asserta(standard_type('boolean', Type_var))
        ;
                true
        ).
midoan_type__create_type(Type_mark, 'integer', range_bounds(Min, Max), Type_var) :-       %Min and Max must be static
        !,
        put_atts(Type_var, midoan_type(Type_mark, [(basetype, 'standard.ads:integer'), (first, Min), (last, Max), (declaration, declaration(constrain_integer_template(Min, Max, Var_template), Var_template))])),
        (Type_mark == 'standard.ads:integer' ->
                asserta(standard_type('integer', Type_var))
        ;
                true
        ).
midoan_type__create_type(Type_mark, 'record', Field_list, Type_var) :-    %Field_list is a list of ([FieldL], Type_var, Init|no_init)
        !,
        check_for_field_with_default_initialisations(Field_list, Has_fields_with_default),
        put_atts(Type_var, midoan_type(Type_mark, [(has_fields_with_default, Has_fields_with_default), (basetype, record), (declaration, declaration(midoan_record:midoan_record__create_record(Type_var, Field_list, template, Var_template), Var_template)), (field_types, Field_list)])).
midoan_type__create_type(Type_mark, 'modular_integer', Standard_integer_type_var, Modulo, Type_var) :-       %modulo are always static (Ada RM)
        !,
        Modulo_minus_one is Modulo - 1,
        midoan_type__get_attribute(Standard_integer_type_var, size, Size),       %getting the size of the original integer type
        put_atts(Type_var, midoan_type(Type_mark, [(basetype, modular_integer), (first, 0), (last, Modulo_minus_one), (declaration, declaration(midoan_modular_integer:midoan_modular_integer__create_modular_integer(Modulo, Var_template), Var_template)), (size, Size), (modulus, Modulo)])).
midoan_type__create_type(Type_mark, 'float', digits(Digits), range_bounds(Min, Max), Type_var) :- %Digits, Min and Max must be static
        !,
        put_atts(Type_var, midoan_type(Type_mark, [(basetype, 'standard.ads:float'), (first, Min), (last, Max), (declaration, declaration(constrain_float_template(Min, Max, Var_template), Var_template)), (digits, Digits)])).
midoan_type__create_type(Type_mark, 'fixed', delta(Delta), range_bounds(Min, Max), Type_var) :- %Delta, Min and Max must be static
        !,
        put_atts(Type_var, midoan_type(Type_mark, [(basetype, 'standard.ads:float'), (first, Min), (last, Max), (declaration, declaration(constrain_float_template(Min, Max, Var_template), Var_template)), (delta, Delta), (small, 998)])).   %small should be (see 3.5.9 point 8 in Ada RM)
midoan_type__create_type(Type_mark, 'array', Index_list, Component_type_var, Type_var) :- %Index_list should be a list of Type_vars
        !,
        midoan_type__get_attribute(Component_type_var, size, Size),
        midoan_type__get_attribute(Component_type_var, has_fields_with_default, Has_fields_with_default),
        length(Index_list, Dimensions),         %used for our own internal dimensions attribute
        put_atts(Type_var, midoan_type(Type_mark, [(dimensions, Dimensions), (has_fields_with_default, Has_fields_with_default), (basetype, array(Component_type_var)), (declaration, declaration(midoan_array:midoan_array__create_array(Type_var, Index_list, Component_type_var, template, Var_template), Var_template)), (component_size, Size), (index_list, Index_list)])),
        add_array_dimensions_attributes(Index_list, Type_var, 1).      %e.g. AA'First(2) and also last and length attribute
midoan_type__create_type(Type_mark, 'unconst_array', Index_list, Component_type_var, Type_var) :-   %Index_list should be a list of Type_vars
        !,
          %create an unconst array
        midoan_type__get_attribute(Component_type_var, size, Size),
        put_atts(Type_var, midoan_type(Type_mark, [(basetype, unconst_array(Component_type_var)), (declaration, declaration(midoan_array:midoan_array__create_unconst_array(Type_var, Index_list, Component_type_var, Var_template), Var_template)), (component_size, Size), (unconst_array_indexes, Index_list)])).
midoan_type__create_type(Type_mark, 'fixed', delta(Delta), digits(Digits), range_bounds(Min, Max), Type_var) :- %Delta, Digits, Min and Max must be static
        !,
        midoan_type__create_type(Type_mark, fixed, delta(Delta), range_bounds(Min, Max), Type_var),     %see above
        midoan_type__update_type_attribute(Type_var, digits, Digits).
%%%
        check_for_field_with_default_initialisations([], no) :-
                !.
        check_for_field_with_default_initialisations([(_, _, no_init)|Rest], Has_fields_with_default) :-
                !,
                check_for_field_with_default_initialisations(Rest, Has_fields_with_default).
        check_for_field_with_default_initialisations(_, yes).
%%%
        %for every index in turn
        add_array_dimensions_attributes([], _Array_type_var, _Dim).
        add_array_dimensions_attributes([Type_var_index|Rest_indexL], Array_type_var, Dim) :-
                midoan_type__get_attribute(Type_var_index, first, Min),
                midoan_type__update_type_attribute(Array_type_var, first(Dim), Min),
                midoan_type__get_attribute(Type_var_index, last, Max),
                midoan_type__update_type_attribute(Array_type_var, last(Dim), Max),
                midoan_type__obtain_basetype(Type_var_index, Basetype_of_index),
                ((Basetype_of_index == 'standard.ads:integer' ; Basetype_of_index == 'modular_integer') ->
                         (%coding of Length #= Max - Min + 1; calling midoan_solver__interpret is necessary because if the numbers are huge we will get and overflow, also cannot use is because the range may be dynamic
                          midoan_solver:midoan_solver__interpret(-(Max, Min), types(i, i), Tmp1, _, _Exception_),
                          midoan_solver:midoan_solver__interpret(+(Tmp1, 1), types(i, i), Length, _, _Exception_)
                         )
                ;
                 Basetype_of_index == 'base_enumeration' ->
                        (midoan_enum:midoan_enum__get_length_dynamic_list(Type_var_index, Length1),     %because of possible representation clauses for enumerated index, length is not necessarily Max'pos-Min'Pos+1 ...
                         midoan_solver:midoan_solver__interpret(+(Length1, 0), types(i, i), Length, _, _Exception_)
                        )
                ),
                midoan_type__update_type_attribute(Array_type_var, length(Dim), Length),
                Dim1 is Dim + 1,
                add_array_dimensions_attributes(Rest_indexL, Array_type_var, Dim1).
%%%
        constrain_float_template(Min, Max, Var_template) :-
                (midoan_solver:midoan_solver__sdl(<=(Min, Max)) ->                    %the range is not empty: static check because Min and Max are static for type declarations
                        ({_ = Var_template*1.0},                        %trick to ensure that Var is a clpr variable
                         midoan_solver:midoan_solver__sdl(>=(Var_template, Min)),
                         midoan_solver:midoan_solver__sdl(<=(Var_template, Max))
                        )
                ;
                        Var_template = 42.42                    %a random or special value
                ).
        constrain_integer_template(Min, Max, Var_template) :-
                (midoan_solver:midoan_solver__sdl(<=(Min, Max)) ->                    %the range is not empty: static check because Min and Max are static for type declarations
                        (midoan_solver:midoan_extensions__constrain_domain(Var_template),
                         midoan_solver:midoan_solver__sdl(>=(Var_template, Min)),
                         midoan_solver:midoan_solver__sdl(<=(Var_template, Max))
                        )
                ;
                        Var_template = 42                       %a random or special value
                ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_type__create_subtype(Subtype_mark, Parent_type_var, Type_var) :-
        !,
        midoan_type__obtain_basetype(Parent_type_var, Basic_basetype),
        ((Basic_basetype == 'standard.ads:integer' ; Basic_basetype == 'standard.ads:float' ; Basic_basetype == 'modular_integer') ->
                put_atts(Type_var, midoan_type(Subtype_mark, [(basetype, Parent_type_var)]))    %all other attributes (including declaration) are those of the parent type
        ;
         (Basic_basetype == 'private' ; Basic_basetype == 'incomplete_type') ->
                put_atts(Type_var, midoan_type(Subtype_mark, [(basetype, Parent_type_var)]))
        ;
         Basic_basetype == 'base_enumeration' ->
                put_atts(Type_var, midoan_type(Subtype_mark, [(basetype, Parent_type_var)]))    %all other attributes (including declaration which may not be accurate here since a record of the subtype will have its parent type as type_var) are those of the parent type
        ;
         Basic_basetype == 'record' ->
                put_atts(Type_var, midoan_type(Subtype_mark, [(basetype, Parent_type_var)]))    %all other attributes (including declaration which may not be accurate here since a record of the subtype will have its parent type as type_var) are those of the parent type
        ;
         Basic_basetype = unconst_array(_) ->
                (midoan_type__get_attribute(Parent_type_var, basetype, unconst_array(Component_type_var)),
                 midoan_type__get_attribute(Parent_type_var, unconst_array_indexes, Index_list),
                 put_atts(Type_var, midoan_type(Type_mark, [(basetype, Parent_type_var), (declaration, declaration(midoan_array:midoan_array__create_unconst_array(Type_var, Index_list, Component_type_var, Var_template), Var_template))]))
                )
        ;
         Basic_basetype = array(_) ->
                (midoan_type__get_attribute(Parent_type_var, basetype, array(Component_type_var)),
                 midoan_type__get_attribute(Parent_type_var, index_list, Index_list),
                 put_atts(Type_var, midoan_type(Type_mark, [(basetype, Parent_type_var), (declaration, declaration(midoan_array:midoan_array__create_array(Type_var, Index_list, Component_type_var, template, Var_template), Var_template))]))
                )
        ;
                common_util:common_util__error(10, "Unknown basic type in subtype creation", no_error_consequences, [(basic_basetype, Basic_basetype), (subtype_mark, Subtype_mark)], 1015602, midoan_type, midoan_type__create_subtype, no_localisation, no_extra_info)
        ).
midoan_type__create_subtype(Subtype_mark, Parent_type_var, range_bounds(Min, Max), Type_var) :-         %Min and Max may not be static
        !,
        midoan_type__obtain_basetype(Parent_type_var, Basic_basetype),
        (Basic_basetype == 'standard.ads:integer' ->            %is the subtype of an integer type
                (Declaration = ((integer(Min), integer(Max)) -> %static range
                        constrain_integer_template(Min, Max, Var_template)
                 ;
                        (       (midoan_solver:midoan_solver__sdl(<=(Min, Max)),           %the range is not empty
                                 midoan_solver:midoan_extensions__constrain_domain(Var_template),
                                 midoan_solver:midoan_solver__sdl(>=(Var_template, Min)),
                                 midoan_solver:midoan_solver__sdl(<=(Var_template, Max))
                                )
                        ;              %deliberate choice point
                                (midoan_solver:midoan_solver__sdl(>(Min, Max)),
                                 Var_template = 42       %a random or special value in case of an empty range
                                )
                        )
                )
               )
        ;
         Basic_basetype == 'standard.ads:float' ->           %is the subtype of a float type
                (Declaration = ((float(Min), float(Max)) ->            %static range
                        constrain_float_template(Min, Max, Var_template)
                 ;
                        (       (midoan_solver:midoan_solver__sdl(<=(Min, Max)),           %the range is not empty
                                 {_ = Var_template*1.0},                        %trick to ensure that Var is a clpr variable
                                 midoan_solver:midoan_solver__sdl(>=(Var_template, Min)),
                                 midoan_solver:midoan_solver__sdl(<=(Var_template, Max))
                                )
                        ;               %deliberate choice point
                                (midoan_solver:midoan_solver__sdl(>(Min, Max)),
                                 Var_template = 42.42       %a random or special value in case of an empty range
                                )
                        )
                )
                )
        ;
         Basic_basetype == 'modular_integer' ->        %the subtype of a modular integer (!)
                (Declaration = (((integer(Min) ; midoan_modular_integer:midoan_modular_integer__ground(Min)) , (integer(Max) ; midoan_modular_integer:midoan_modular_integer__ground(Max))) -> %static range
                        (midoan_solver:midoan_solver__sdl(<=(Min, Max)) ->           %the range is not empty
                                (midoan_type__variable_declaration(Var_template, Parent_type_var),
                                 midoan_solver:midoan_solver__sdl(>=(Var_template, Min)),
                                 midoan_solver:midoan_solver__sdl(<=(Var_template, Max))
                                )
                        ;
                                Var_template = 42       %a random or special value in case of an empty range
                        )
                ;
                        (       (midoan_solver:midoan_solver__sdl(<=(Min, Max)),           %the range is not empty
                                 midoan_type__variable_declaration(Var_template, Parent_type_var),
                                 midoan_solver:midoan_solver__sdl(>=(Var_template, Min)),
                                 midoan_solver:midoan_solver__sdl(<=(Var_template, Max))
                                )
                        ;               %deliberate choice point
                                (midoan_solver:midoan_solver__sdl(>(Min, Max)),
                                 Var_template = 42       %a random or special value in case of an empty range
                                )
                        )
                )
                )
        ;
         Basic_basetype == 'base_enumeration' ->
                (Declaration = ((midoan_enum:midoan_enum__ground(Min), midoan_enum:midoan_enum__ground(Max)) -> %static range
                        (midoan_solver:midoan_solver__sdl(<=(Min, Max)) ->           %the range is not empty
                                midoan_enum:midoan_enum__create_enum(Type_var, _, Min, Max, Var_template)     %create an enum var template
                        ;
                                midoan_type:midoan_type__get_attribute(Parent_type_var, first, Var_template)   %a random or special value
                        )
                ;
                        (       (midoan_solver:midoan_solver__sdl(<=(Min, Max)),
                                 midoan_enum:midoan_enum__create_enum(Type_var, _, Min, Max, Var_template)
                                )
                        ;               %deliberate choice point
                                (midoan_solver:midoan_solver__sdl(<(Min, Max)),
                                 midoan_type:midoan_type__get_attribute(Parent_type_var, first, Var_template)   %a random or special value
                                )
                        )
                )
                )
        ;
                common_util:common_util__error(10, "Unknown basic type in subtype creation", no_error_consequences, [(basic_basetype, Basic_basetype), (subtype_mark, Subtype_mark)], 1015601, midoan_type, midoan_type__create_subtype, no_localisation, no_extra_info)
        ),
        put_atts(Type_var, midoan_type(Subtype_mark, [(basetype, Parent_type_var), (first, Min), (last, Max), (declaration, declaration(Declaration, Var_template))])).
%%%
%unconstrained array instantiation
midoan_type__create_subtype(Subtype_mark, Parent_type_var, unconst_array(Bounds), Type_var) :-
        midoan_type:midoan_type__obtain_basetype(Parent_type_var, unconst_array(Component_type_var)),
        midoan_type:midoan_type__create_type(Subtype_mark, array, Bounds, Component_type_var, Type_var),
        midoan_type:midoan_type__update_type_attribute(Type_var, basetype, array(Component_type_var)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%a representation clause is being recorded
%Rep_clause is of the form : [Exp, ..., Exp]: positional
%                         or [(Lit, Exp), ..., (Lit, Exp)]: named
midoan_type__update_enum(Type_var, Type_mark, Rep_clause) :-
        midoan_enum:midoan_enum__update_enum(Type_var, Rep_clause, First, Last, Literal_positionL),   %updates midoan_enum__dynamic_list
        declare_enum_type(Type_mark, First, Last, Literal_positionL, Type_var).    %the type needs to be redeclared (because First and Last have changed)
%%%
declare_enum_type(Type_mark, First, Last, Literal_positionL, Type_var) :-
        put_atts(Type_var, midoan_type(Type_mark, [(basetype, base_enumeration), (first, First), (last, Last), (declaration, declaration(midoan_enum:midoan_enum__create_enum(Type_var, _, First, Last, Enum_var_template), Enum_var_template)), (literal_positions, Literal_positionL)])).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_type__variable_declaration(Variable, Type_var) :-
        midoan_type__get_attribute(Type_var, 'declaration', Variable).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_type__created_variable_with_default_initialised_fields(Default_initialised_variable, Type_var) :-
        midoan_type__obtain_basetype(Type_var, Basic_basetype),
        (Basic_basetype == 'record' ->
                (midoan_type__get_attribute(Type_var, field_types, Field_list),
                 midoan_record:midoan_record__create_record(Type_var, Field_list, variable, Default_initialised_variable)
                )
        ;
         (Basic_basetype = array(Component_type_var) ; Basic_basetype = unconst_array(Component_type_var)) ->
                (midoan_type__get_attribute(Type_var, index_list, Index_list),
                 midoan_array:midoan_array__create_array(Type_var, Index_list, Component_type_var, variable, Default_initialised_variable)
                )
        ;
                common_util:common_util__error(10, "Unexpected basic type", no_error_consequences, [(basic_basetype, Basic_basetype)], 10368, midoan_type, midoan_type__created_variable_with_default_initialised_fields, no_localisation, no_extra_info)
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%used for derived types : creates a new type based on an old one
midoan_type__create_derived(New_type_name, Old_type_var, New_type_var) :-
        !,
        put_atts(New_type_var, midoan_type(New_type_name, [(basetype, Old_type_var)])).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%