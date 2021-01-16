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
% operator precedence as defined in Ada
% note that in Ada 'and' & 'or' have the same priority: different than in traditional logic
% put at the end to avoid interferences with other Prolog operators
% not too sure if xfy or yfx corresponding to (right to left and left to right respectively)
% should be used, right to left seem more efficient but is different from Ada semantics

:- op(30, fy, [abs]).                              %must be fy for 'abs abs a' expressions
:- op(30, yfx, [**]).                                   %must be left to right for 'a** b ** c' expressions
:- op(40, yfx, [*, /, mod, rem]).                       %must be left to right for a*b mod c
:- op(50, fy, [+, -]).                                  %must be fy for '- -a' expressions
:- op(60, yfx, [+, -, &]).                              %could be xfy, does not matter
:- op(70, xfx, [=, <>, <, >, <=, >=]).                  %can be anything, relations never repeated
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%