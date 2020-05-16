init([_], []).
init([X|Xs], [X | Ys]) :-
   init(Xs, Ys).

getSum([], 0).
getSum([X | Xs], Sum) :-
   getSum(Xs, NewSum) ,
   Sum is X + NewSum.
    
getSublist(Lst, tup(List, TheSum)) :-
   init(Lst, List),
   getSum(List, TheSum).

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


