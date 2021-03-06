%Tom Hammarkvist

init([_], []).
init([X|Xs], [X | Ys]) :-
   init(Xs, Ys).

getSum([], 0).
getSum([X | Xs], Sum) :-
   getSum(Xs, NewSum) ,
   Sum is X + NewSum.

returnSumFromTuple((Sum, _, _, _), S) :-
   S = Sum.

getSublists([], _, []).
getSublists(Lst, Index, Tup) :-
   getSum(Lst, Sum),
   I = Index,
   length(Lst, LNGTH),
   J is I + LNGTH -1,
   FirstTup = [(Sum, I, J, Lst)],
   init(Lst, ShorterLst),
   getSublists(ShorterLst, I, TupNew),
   append(FirstTup, TupNew, Tup).
    
getAllSublists([], _, []).
getAllSublists([H | T], Start, Sublists) :-
   getSublists([H | T], Start, Subs),
   Next is Start + 1,
   getAllSublists(T, Next, AllSubs),
   append(Subs, AllSubs, Sublists).

insert(X, [], [X]):- !.
insert(X, [X1|L1], [X, X1|L1]):- 
   returnSumFromTuple(X, Sum1),
   returnSumFromTuple(X1, Sum2),
   Sum1=<Sum2, !.
insert(X, [X1|L1], [X1|L]):- insert(X, L1, L).

insertionSort([], []):- !.
insertionSort([X|L], S):- insertionSort(L, S1), insert(X, S1, S).

take(0, _, []). 
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :-
   N > 0,
   M is N-1, 
   take(M, Xs, Ys).

generateSet([], _, []).
generateSet(Lst, Amount, Sublists) :-
   getAllSublists(Lst, 1, Res),
   insertionSort(Res, SortedSubs),
   take(Amount, SortedSubs, Sublists).
   
stringOutput([(Sum, I, J, Lst)| Rest]) :-
   write(Sum),
   write('\t'),
   write(I),
   write('\t'),
   write(J),
   write('\t'),
   write(Lst),
   write('\n'),
   stringOutput(Rest).
   %stringOutput(Rest).
   
smallestKset(_, 0, _) :-
   write('That was pointless'), !.

smallestKset([], _, _) :-
   write('Inputlist is empty, try another list!'), !.

smallestKset(List, Amount, Output) :-
   generateSet(List, Amount, Output),
   write('Sum\ti\tj\tSublist\n'),
   stringOutput(Output).


% range(Int Start, Int End, [Int] Ls)
range(X, X, []).
range(S, E, [S|Ls]) :-
S < E,
NextS is S+1,
range(NextS, E, Ls).

testCase1Func(X, Y) :- Y is X*(-1)^X.
testCase1(15, Y) :-
range(1, 101, Ints),
maplist(testCase1Func, Ints, Y).