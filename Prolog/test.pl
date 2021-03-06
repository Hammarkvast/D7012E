length1([], 0).
length1([_| Tail], N) :- length1(Tail, N1),
                          N is 1 + N1.


conc([],L,L).
conc( [X|L1], L2, [X|L3]) :- conc(L1, L2, L3).

sublist( S, L) :- conc(_, L2, L), conc(S, _, L2).

init1([_], []).
init1([X|Xs], [X|Ys]) :-
    init1(Xs, Ys).

move( state( middle, onbox, middle, hasnot),
      grasp,
      state( middle, onbox, middle, has) ).    

move( state( P, onfloor, P, H),
      climb,
      state( P, onbox, P, H) ).   

move( state( P1, onfloor, P1, H),
      push( P1, P2),
      state( P2, onfloor, P2, H) ).

move( state( P1, onfloor, B, H),
      walk( P1, P2),
      state( P2, onfloor, B, H) ).                   

canget( state( _, _, _, has), [done| []]).       

canget( State1, [Move| Trace2])  :-
    move( State1, Move, State2),
    canget( State2, Trace2). 

% canget(state(atdoor,onfloor,atwindow,hasnot), R).