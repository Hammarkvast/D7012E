length1([], 0).
length1([_| Tail], N) :- length1(Tail, N1),
                          N is 1 + N1.


conc([],L,L).
conc( [X|L1], L2, [X|L3]) :- conc(L1, L2, L3).

sublist( S, L) :- conc(_, L2, L), conc(S, _, L2).