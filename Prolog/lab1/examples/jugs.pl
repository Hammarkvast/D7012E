% Written by Dave Reed 2002-02-15
% Håkan Jonsson has just written comments, added a hard coded limit, and edited a bit

% version 2, where the actions performed are explicit returned (as a list)

% a state is a pair jugs(#1,#2)
%    #1 is the current volume in the large (4 liter) jug
%    #2 is the current volume in the small (3 liter) jug

% 8 state transitions, "moves", with (old state, action performed, new state):

move(jugs(J1,J2), 'Fill L', jugs(4,J2)) :- J1 < 4.      % 1. fill large (jug)

move(jugs(J1,J2), 'Fill S', jugs(J1,3)) :- J2 < 3.      % 2. fill small (jug)

move(jugs(J1,J2), 'Empty L', jugs(0,J2)) :- J1 > 0.      % 3. empty large 

move(jugs(J1,J2), 'Empty S', jugs(J1,0)) :- J2 > 0.      % 4. empty small 

move(jugs(J1,J2), 'Fill L from S', jugs(4,N))  :-              % 5. fill large from small until large is full
    J2 > 0,                                  
    J1 + J2 >= 4,                             %    small must contain enough liquidto fill up large
    N is J2 - (4 - J1).

move(jugs(J1,J2), 'Fill S from L', jugs(N,3))  :-              % 6. fill small from large until small is full
    J1 > 0,
    J1 + J2 >= 3,                             %    large must contain enough liquid to fill up small
    N is J1 - (3 - J2).

move(jugs(J1,J2), 'Empty S into L', jugs(N,0))  :-              % 7. empty small into large
    J2 > 0,
    J1 + J2 < 4,                              %    all liquid in small must fit in large
    N is J1 + J2.

move(jugs(J1,J2), 'Empty L into S', jugs(0,N))  :-              % 8. empty large into small
    J1 > 0,
    J1 + J2 < 3,                              %    all liquid in large must fit in small
    N is J1 + J2.


dfs_no_cycles_deep(State, Goal, Path, ACTIONS) :-
    dfs_deep_help([State], Goal, RevPath, ACTIONS, 1),      % start with maximum depth 1
    reverse(RevPath, Path).


dfs_deep_help(PathSoFar, Goal, Path, ACTIONS, DepthBound) :- % helper to try increasingly larger depths
    dfs_bounded(PathSoFar, Goal, Path, ACTIONS, DepthBound). % until a solution is found - add cut to get unique solution

dfs_deep_help(PathSoFar, Goal, Path, ACTIONS, DepthBound) :- 
    NewDepth is DepthBound + 1,                     % add 1 to the maximum depth allowed
    NewDepth < 400,                                   % uncomment to set an absolute limit on depth
    dfs_deep_help(PathSoFar, Goal, Path, ACTIONS, NewDepth). % try again with the increased maximum depth


dfs_bounded([Goal|PathSoFar], Goal, [Goal|PathSoFar], [], 1).

dfs_bounded([State|PathSoFar], Goal, Path, [ACTION|ACTION2], DepthBound) :-
    DepthBound > 0,                                 % check so maximum depth is not exceeded
    move(State, ACTION, NextState),
    not(member(NextState, [State|PathSoFar])),      % check against cycles in our traversal
    NewDepth is DepthBound - 1,
    dfs_bounded([NextState,State|PathSoFar], Goal, Path, ACTION2, NewDepth).


% dfs_no_cycles_deep(jugs(0,0), jugs(2,0), Path, ACTIONS).
% this works: 6 transitions from the initial state

% dfs_no_cycles_deep(jugs(0,0), jugs(3,0), Path, ACTIONS).
% this works: 2 transitions from the initial state

% dfs_no_cycles_deep(jugs(0,0), jugs(2,2), Path, ACTIONS).
% this has no solution 
