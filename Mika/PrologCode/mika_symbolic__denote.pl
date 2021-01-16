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
% mika_symbolic.pl
%28/03/08
%replaces symbolically_interpret when we do not want to actually execute the expression
%quite limited at the moment : deals with issue A1 and also used in representation clauses
%and quite unsure at the moment if it is sufficient
syntactically_denote(Var, Name, Entity, Type) :-
	var(Var),
	!,
        Entity = Var,
	(mika_seav_atts:mika_seav_atts__is_seav(Var) ->
		(mika_seav_atts:mika_seav_atts__get('name', Var, Name),
		 Type = 'object'
		)
	;
	 mika_sub_atts:mika_sub_atts__is_sub_atts(Var) ->
		(mika_sub_atts:mika_sub_atts__get('name', Var, Name),
		 Type = 'subprogram'
		)
	;
         midoan_type:midoan_type__is_type(Var) ->
                (midoan_type:midoan_type__get_typemark(Var, Name),
                 Type = 'type'
                )
        ;
         mika_package_atts:mika_package_atts__is_package_atts(Var) ->
		(mika_package_atts:mika_package_atts__get(Var, 'name', Name),
		 Type = 'package'
		)
        ;
         mika_unhandled_atts:mika_unhandled_atts__is_unhandled_atts(Var) ->
                (mika_unhandled_atts:mika_unhandled_atts__get('name', Var, Name),
                 mika_unhandled_atts:mika_unhandled_atts__get(comment, Var, Comment),
                 %if this error occurs the enveloping object/construct should itself become an unhandled construct
                 %we consider that unhandled things should not reach the syntactic interpretation : they should be caught before during symbolic execution
                 % otherwise we will have loads of propagation to implement in synatctically_denote for little benefit : the result will be unhandled
                 common_util__error(10, "Syntactic denotation of an unhandled type/object", "see comments in code", [(name, Name), (comment, Comment)], 103530, mika_symbolic__denote, syntactically_denote, no_localisation, no_extra_info)
                )
        ;
         mika_name_atts:mika_name_atts__is_name_atts(Var) ->
		(mika_name_atts:mika_name_atts__get(name, Var, Name),
		 Type = label_or_field
		)
        ;
                common_util__error(10, "Unknown type/object in syntactic denotation", "extend code?", [(var, Var)], 103832, mika_symbolic__denote, syntactically_denote, no_localisation, no_extra_info)
        ).

syntactically_denote(selected(Selector, Selected), Name, Entity, Type) :-
        syntactically_denote(Selector, _Inner_Name, Inner_Entity, _Inner_Type), %modified 12/10/09
        (mika_package_atts:mika_package_atts__is_package_atts(Inner_Entity) ->
                syntactically_denote(Selected, Name, Entity, Type)
        ;
                common_util__error(10, "Unhandled syntactic denotation of a selected expression", no_error_consequences, [(selector, Selector), (selected, Selected)], 104533, mika_symbolic__denote, syntactically_denote, syntactically_denote(selected(_)), no_extra_info)
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%