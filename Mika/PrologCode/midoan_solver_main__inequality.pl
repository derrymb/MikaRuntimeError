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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% midoan_solver_main__inequality.pl
% part of the midoan_solver module : used to impose inequality constraint on arrays or records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%called from midoan_solver_array.pl and midoan_solver_record.pl
%the first two arguments can be an index_elements list (for arrays) or a field_value list (for records)
midoan_solver__inequality_constraint(L_Le, L_Ri, Outcome, Susp) :-
        length(L_Le, Length_Le),
        length(L_Ri, Length_Ri),
        (Length_Le == Length_Ri ->
                midoan_solver__inequality_constraint2(L_Le, L_Ri, Outcome, Susp)
        ;
                Outcome = yes
        ).

midoan_solver__inequality_constraint2([], [], dontknow, _).
midoan_solver__inequality_constraint2([(_, Value_le)|R1], [(_, Value_ri)|R2], Outcome, Susp) :-
        midoan_solver__get_type_from_value(Value_le, Type_le),
        midoan_solver__get_type_from_value(Value_ri, Type_ri),
        ((Type_le == ground , Type_ri == ground) ->             %for ground integers and floats
                (Value_le == Value_ri ->
                        midoan_solver__inequality_constraint2(R1, R2, Outcome, Susp)
                ;
                        Outcome = yes                           %one difference found : success
                )
        ;
         Type_le == modular_integer ->
                (midoan_modular_integer:midoan_modular_integer__get(value, Value_le, Le_value),
                 midoan_solver__inequality_constraint2([(_, Le_value)|R1], [(_, Value_ri)|R2], Outcome, Susp)
                )
        ;
         Type_ri == modular_integer ->
                (midoan_modular_integer:midoan_modular_integer__get(value, Value_ri, Ri_value),
                 midoan_solver__inequality_constraint2([(_, Value_le)|R1], [(_, Ri_value)|R2], Outcome, Susp)
                )
        ;
         Type_le == base_enumeration ->
                (midoan_enum:midoan_enum__get(position, Value_le, Le_pos),
                 midoan_solver__inequality_constraint2([(_, Le_pos)|R1], [(_, Value_ri)|R2], Outcome, Susp)
                )
        ;
         Type_ri == base_enumeration ->
                (midoan_enum:midoan_enum__get(position, Value_ri, Ri_pos),
                 midoan_solver__inequality_constraint2([(_, Value_le)|R1], [(_, Ri_pos)|R2], Outcome, Susp)
                )
        ;
         ((Type_le == 'standard.ads:integer' ; Type_ri == 'standard.ads:integer') ;
          (Type_le == 'standard.ads:float' ; Type_ri == 'standard.ads:float')) ->     %at least one of them is not ground
                (Susp1 = (ground(eq(Value_le, Value_ri))),
                 midoan_solver__inequality_constraint2(R1, R2, Outcome, Susp2),
                 (var(Susp2) ->                         %it has not been instantiated
                        Susp = Susp1
                 ;
                        Susp = (Susp1 ; Susp2)          % ; 'or constraint' in sicstus coroutining mechanism
                 )
                )
        ;
         (Type_le = array(_) ; Type_ri = array(_)) ->
                (midoan_array:midoan_array__get_all_index_elements(Value_le, Index_elements_le),
                 midoan_array:midoan_array__get_all_index_elements(Value_ri, Index_elements_ri),
                 inequality_constraint_of_component(Index_elements_le, Index_elements_ri, R1, R2, Outcome, Susp)
                )
        ;
         (Type_le == record ; Type_ri == record) ->
                (midoan_record:midoan_record__get_all_field_values(Value_le, Field_values_le),
                 midoan_record:midoan_record__get_all_field_values(Value_ri, Field_values_ri),
                 inequality_constraint_of_component(Field_values_le, Field_values_ri, R1, R2, Outcome, Susp)
                )
        ;
                 common_util__error(10, "Unexpected types in inequality", no_error_consequences, [(value_le, Value_le), (type_le, Type_le), (value_ri, Value_ri), (type_ri, Type_ri)], 1076188, midoan_solver, midoan_solver__inequality_constraint2, no_localisation, no_extra_info)
        ).

%works on Index_elements from array or from field_values from records
inequality_constraint_of_component(Component_le, Component_ri, R1, R2, Outcome, Susp) :-
                midoan_solver__inequality_constraint2(Component_le, Component_ri, Outcome_inner, Susp_inner),
                 (Outcome_inner == yes ->       %the two components contain at least one difference
                        Outcome = yes           %one difference found : success
                 ;
                  Outcome_inner == dontknow ->
                        (var(Susp_inner) ->   %the two components are actually identical for sure
                                midoan_solver__inequality_constraint2(R1, R2, Outcome, Susp)
                        ;
                                (midoan_solver__inequality_constraint2(R1, R2, Outcome, Susp2),
                                 (var(Susp2) ->
                                        Susp = Susp_inner
                                 ;
                                        Susp = (Susp_inner ; Susp2)  % ; 'or constraint'
                                 )
                                )
                        )
                 ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%