/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: <TO BE FILLED IN BEFORE THE GRADING> 
%    Student user id  : <TO BE FILLED IN BEFORE THE GRADING> 
%
/* ------------------------------------------------------- */



%do not chagne the follwoing line!
:- ensure_loaded('play.pl').


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them. 
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 





% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board

initBoard([[.,.,.,.,.,.],
		   [.,.,.,.,.,.],
		   [.,.,1,2,.,.],
		   [.,.,2,1,.,.],
		   [.,.,.,.,.,.],
		   [.,.,.,.,.,.]]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 


initialize(X, 1) :-
	initBoard(X).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 

 


score(Board, Player, Score) :-
	flatten(Board, ScoreList),
	countScore(ScoreList, Player, Score).

countScore([], _, 0).
countScore([Player | RestOfBoard], Player, TotalScore) :-
	countScore(RestOfBoard, Player, Score),
	TotalScore is 1 + Score.
	
countScore([NotPlayerStone | RestOfBoard], Player, Score) :-
	Player \= NotPlayerStone,
	countScore(RestOfBoard, Player, Score).


winner(State, Plyr) :-
	terminal(State),
	score(State, 1, ScoreP1),
	score(State, 2, ScoreP2),
	(ScoreP1 < ScoreP2, Plyr = 1;
	ScoreP2 < ScoreP1, Plyr = 2).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 


tie(State) :-
	terminal(State),
	score(State, 1, ScoreP1),
	score(State, 2, ScoreP2),
	ScoreP1 = ScoreP2.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%%

showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%



getPlayerPositions(_, [], _, _, []).
getPlayerPositions(Plyr, [Head | RestOfRow], X, Y, PlayerPositions) :-
	Plyr = Head,
	PlayerCoords = [[X, Y]],
	NextX is X + 1,
	getPlayerPositions(Plyr, RestOfRow, NextX, Y, NewCoords),
	append(PlayerCoords, NewCoords, PlayerPositions).
getPlayerPositions(Plyr, [Head | RestOfRow], X, Y, PlayerPositions) :-
	Plyr \= Head,
	NextX is X + 1,
	getPlayerPositions(Plyr, RestOfRow, NextX, Y, PlayerPositions).

getAllPlayerPositions(_, [], _, []).
getAllPlayerPositions(Plyr, [FirstRow | RestOfRows], Y, Positions):-
	getPlayerPositions(Plyr, FirstRow, 0, Y, PlayerRowPositions),
	NextRow is Y + 1, 
	getAllPlayerPositions(Plyr, RestOfRows, NextRow, RestRowPositions),
	append(PlayerRowPositions, RestRowPositions, Positions).

decreaseX(X, XMinusOne) :-
	XMinusOne is X - 1.

increaseX(X, XPlusOne) :-
	XPlusOne is X + 1.

decreaseY(Y, YMinusOne) :-
	YMinusOne is Y - 1.

increaseY(Y, YPlusOne) :-
	YPlusOne is Y + 1.

increaseXY(X, Y, XPlusOne, YPlusOne):-
	XPlusOne is X + 1,
	YPlusOne is Y + 1.

decreaseXY(X, Y, XMinusOne, YMinusOne) :-
	XMinusOne is X - 1,
	YMinusOne is Y - 1.

incXDecY(X, Y, XPlusOne, YMinusOne):-
	XPlusOne is X + 1,
	YMinusOne is Y - 1.

decXIncY(X, Y, XMinusOne, YPlusOne) :-
	XMinusOne is X - 1,
	YPlusOne is Y + 1.

countEnemiesWest(Plyr, [X,Y], State, ValidDot):-
	get(State, [X,Y], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	decreaseX(X, NewX),
	countEnemiesWest(Plyr, [NewX, Y], State, ValidDot).
countEnemiesWest(_, [X,Y], _, ValidDot):-
	ValidDot = [[X, Y]].

countEnemiesEast(Plyr, [X,Y], State, ValidDot) :-
	get(State, [X,Y], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	increaseX(X, NewX),
	countEnemiesEast(Plyr, [NewX, Y], State, ValidDot).
countEnemiesEast(_, [X,Y], _, ValidDot):-
	ValidDot = [[X, Y]].

countEnemiesNorth(Plyr, [X,Y], State, ValidDot):-
	get(State, [X,Y], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	decreaseY(Y, NewY),
	countEnemiesNorth(Plyr, [X, NewY], State, ValidDot).
countEnemiesNorth(_, [X,Y], _, ValidDot):-
	ValidDot = [[X, Y]].

countEnemiesSouth(Plyr, [X, Y], State, ValidDot) :-
	get(State, [X,Y], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	increaseY(Y, NewY),
	countEnemiesNorth(Plyr, [X, NewY], State, ValidDot).
countEnemiesSouth(_, [X,Y], _, ValidDot):-
	ValidDot = [[X, Y]].

countEnemiesSE(Plyr, [X, Y], State, ValidDot) :-
	get(State, [X,Y], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	increaseXY(X,Y, NewX, NewY),
	countEnemiesSE(Plyr, [NewX, NewY], State, ValidDot).
countEnemiesSE(_, [X,Y], _, ValidDot):-
	ValidDot = [[X, Y]].

countEnemiesSW(Plyr, [X,Y], State, ValidDot) :-
	get(State, [X,Y], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	decreaseXY(X,Y, NewX, NewY),
	countEnemiesSW(Plyr, [NewX, NewY], State, ValidDot).
countEnemiesSW(_, [X,Y], _, ValidDot):-
	ValidDot = [[X, Y]].

countEnemiesNE(Plyr, [X,Y], State, ValidDot) :-
	get(State, [X,Y], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	incXDecY(X,Y, NewX, NewY),
	countEnemiesNE(Plyr, [NewX, NewY], State, ValidDot).
countEnemiesNE(_, [X,Y], _, ValidDot):-
	ValidDot = [[X, Y]].

countEnemiesNW(Plyr, [X,Y], State, ValidDot) :-
	get(State, [X,Y], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	decXIncY(X,Y, NewX, NewY),
	countEnemiesNW(Plyr, [NewX, NewY], State, ValidDot).
countEnemiesNW(_, [X,Y], _, ValidDot):-
	ValidDot = [[X, Y]].

west(_, State, [], []) :-
	showState(State).
west(Plyr, State, [[X, Y]|PlayerCoordinates], DotPosition) :-
	NextX is X-1,
	get(State, [NextX,Y], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	NextNextX is NextX - 1,
	countEnemiesWest(Plyr, [NextNextX, Y], State, PlayablePosition),
	west(Plyr, State, PlayerCoordinates, NewDot), !,
	append(PlayablePosition, NewDot, DotPosition).
west(Plyr, State, [_|PlayerCoordinates], DotPosition) :-
	west(Plyr, State, PlayerCoordinates, DotPosition).

east(_,_,[],[]).
east(Plyr, State, [[X, Y]| PlayerCoordinates], DotPosition) :-
	increaseX(X, NextX),
	get(State, [NextX, Y], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	increaseX(NextX, NextNextX),
	countEnemiesEast(Plyr, [NextNextX, Y], State, PlayablePosition),
	east(Plyr, State, PlayerCoordinates, NewDot), !,
	append(PlayablePosition, NewDot, DotPosition).
east(Plyr, State, [_|PlayerCoordinates], DotPosition) :-
	east(Plyr, State, PlayerCoordinates, DotPosition).

north(_,State,[],[]) :-
	showState(State).
north(Plyr, State, [[X,Y] | PlayerCoordinates], DotPosition) :-
	decreaseY(Y, NextY),
	get(State, [X, NextY], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	decreaseY(NextY, NextNextY),
	countEnemiesNorth(Plyr, [X, NextNextY], State, PlayablePosition),
	north(Plyr, State, PlayerCoordinates, NewDot), !,
	append(PlayablePosition, NewDot, DotPosition).
north(Plyr, State, [_|PlayerCoordinates], DotPosition) :-
	north(Plyr, State, PlayerCoordinates, DotPosition).

south(_, State, [], []) :-
	showState(State).
south(Plyr, State, [[X,Y] | PlayerCoordinates], DotPosition) :-
	increaseY(Y, NextY),
	get(State, [X, NextY], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	increaseY(NextY, NextNextY),
	countEnemiesSouth(Plyr, [X, NextNextY], State, PlayablePosition),
	south(Plyr, State, PlayerCoordinates, NewDot), !,
	append(PlayablePosition, NewDot, DotPosition).
south(Plyr, State, [_|PlayerCoordinates], DotPosition) :-
	south(Plyr, State, PlayerCoordinates, DotPosition).

southEast(_, State, [], []) :-
	showState(State).
southEast(Plyr, State, [[X,Y] | PlayerCoordinates], DotPosition) :-
	increaseXY(X, Y, NextX, NextY),
	get(State, [NextX, NextY], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	increaseXY(NextX, NextY, NextNextX, NextNextY),
	countEnemiesSE(Plyr, [NextNextX, NextNextY], State, PlayablePosition),
	southEast(Plyr, State, PlayerCoordinates, NewDot), !,
	append(PlayablePosition, NewDot, DotPosition).
southEast(Plyr, State, [_|PlayerCoordinates], DotPosition) :-
	southEast(Plyr, State, PlayerCoordinates, DotPosition).

southWest(_, State, [], []) :-
	showState(State).
southWest(Plyr, State, [[X,Y] | PlayerCoordinates], DotPosition) :-
	decreaseXY(X, Y, NextX, NextY),
	get(State, [NextX, NextY], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	decreaseXY(NextX, NextY, NextNextX, NextNextY),
	countEnemiesSW(Plyr, [NextNextX, NextNextY], State, PlayablePosition),
	southWest(Plyr, State, PlayerCoordinates, NewDot), !,
	append(PlayablePosition, NewDot, DotPosition).
southWest(Plyr, State, [_|PlayerCoordinates], DotPosition) :-
	southWest(Plyr, State, PlayerCoordinates, DotPosition).

northEast(_, State, [], []) :-
	showState(State).
northEast(Plyr, State, [[X,Y] | PlayerCoordinates], DotPosition) :-
	incXDecY(X, Y, NextX, NextY),
	get(State, [NextX, NextY], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	incXDecY(NextX, NextY, NextNextX, NextNextY),
	countEnemiesNE(Plyr, [NextNextX, NextNextY], State, PlayablePosition),
	northEast(Plyr, State, PlayerCoordinates, NewDot), !,
	append(PlayablePosition, NewDot, DotPosition).
northEast(Plyr, State, [_|PlayerCoordinates], DotPosition) :-
	northEast(Plyr, State, PlayerCoordinates, DotPosition).

northWest(_, State, [], []) :-
	showState(State).
northWest(Plyr, State, [[X,Y] | PlayerCoordinates], DotPosition) :-
	decXIncY(X, Y, NextX, NextY),
	get(State, [NextX, NextY], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	decXIncY(NextX, NextY, NextNextX, NextNextY),
	countEnemiesNW(Plyr, [NextNextX, NextNextY], State, PlayablePosition),
	northWest(Plyr, State, PlayerCoordinates, NewDot), !,
	append(PlayablePosition, NewDot, DotPosition).
northWest(Plyr, State, [_|PlayerCoordinates], DotPosition) :-
	northWest(Plyr, State, PlayerCoordinates, DotPosition).	

moves(Plyr, State, MvLst) :-
	getAllPlayerPositions(Plyr, State, 0, PlyrPositions),
	west(Plyr, State, PlyrPositions, ValidWest) ,
	east(Plyr, State, PlyrPositions, ValidEast) ,
	south(Plyr, State, PlyrPositions, ValidSouth) ,
	north(Plyr, State, PlyrPositions, ValidNorth) ,
	southEast(Plyr, State, PlyrPositions, ValidSE) ,
	southWest(Plyr,State, PlyrPositions, ValidSW) ,
	northEast(Plyr, State, PlyrPositions, ValidNE) ,
	northWest(Plyr, State, PlyrPositions, ValidNW) ,
	append(ValidWest, ValidEast, ValidWestEast),
	append(ValidWestEast, ValidSouth, ValidWES),
	append(ValidWES, ValidNorth, ValidWESN),
	append(ValidWESN, ValidSE, ValidWESNSE),
	append(ValidWESNSE, ValidSW, ValidWESNSESW),
	append(ValidWESNSESW, ValidNE, ValidWESNSESWNE),
	append(ValidWESNSESWNE, ValidNW, MvLst).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:

get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0, 
    Y1 is Y-1, 
    set( RestRows, NewRestRows, [X, Y1], Value). 

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 
