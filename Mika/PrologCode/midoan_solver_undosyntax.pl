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
%'necessary' in debug mode only (when recompiling several times)
% interferences between '->' in prolog and '->' in Prolog when reloading (possible interference with other operators
%  so they are also included below)

:- current_op(70, xfx, <>) ->	%i.e. Ada operators precedence are active
%we should abolish the user-defined operators, but it cannot be done in Sicstus
%maybe could save previous setting and re-establish them here (check abolish predicate in sicstus)
	(format("MIDOAN SOLVER, COMPILATION DANGER: see midoan_solver_undosyntax, close Sicstus and reload the solver from afresh", []),
	 abort
	)
	;
	true
	.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%END%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%