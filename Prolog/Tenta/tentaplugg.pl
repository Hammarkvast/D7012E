
fib(0, 0).
fib(1, 1).
fib(N, FN) :-
    N1 is N-1,
    N2 is N-2,
    fib(N1, FN1), 
    fib(N2, FN2), 
    FN is FN1 + FN2 ,!.


del(X, [X | L], L).
del(X, [A |L], [A | L1]) :-
    del(X, L, L1).

insert(X, List, BL) :-
    del(X, BL, List).

fibs(0, [0]).
fibs(N, L) :-
    fib(N, FN),
    N1 is N-1,
    fibs(N1, FN1),
    FNLIST = FN1,
    insert(FN, FNLIST, Unsorted),
    sort(0, @=<, Unsorted, L), !.

fibs4(N,L) :-
    fib(N,F), N1 is N-1, fibs(N1,R),append(R,[F],L),!. 

conc([], L, L).
conc([X | L1], L2, [X | L3]) :-
    conc(L1, L2, L3).



dele([], []) :- !.
dele([H, H | T], NoDupl) :-
    dele([H | T], NoDupl),!.
dele([H | T], [H | NoDupl]) :-
    dele(T, NoDupl), !.


mergeLists([], L, L).
mergeLists(L, [], L).
mergeLists([], [], []).
mergeLists([H | T], [H1 | T1], Sorted) :-
    ((H < H1, conc([H], [H1], L)) ; 
    (H1 < H, conc([H1], [H], L)) ;
    (H1 = H, conc([H1], [], L))),
    mergeLists(T, T1, N),
    conc(L, N, SortedWithDoubles),  
    dele(SortedWithDoubles, Sorted), !.

mergeUnq(L1, L2, L) :-
    mergeLists(L1, L2, L).

swap(t(A, B), t(C, D)) :-
    A = D,
    B = C.


factorial(0, 1).
factorial(N, FN) :-
    N1 is N -1,
    factorial(N1, FN1),
    FN is N * FN1, !.

factorials(0, [1]).
factorials(N, L) :-
    factorial(N, S),
    N1 is N-1,
    factorials(N1, L1),
    Unsorted= [S | L1],
    sort(0, @=<, Unsorted, L), !.

parent(X, Y).

siblings(X, Y) :-
    parent(Z, X),
    parent(Z, Y).

cousins(X,Y) :-
    parent(Z, X),
    parent(T, Y),
    siblings(Z, T).


insertion(X, [], [X]):- !.
insertion(X, [X1|L1], [X, X1|L1]):- 
   X=<X1, !.
insertion(X, [X1|L1], [X1|L]):- insert(X, L1, L).

insertionSort([], []):- !.
insertionSort([X|L], S):- insertionSort(L, S1), insert(X, S1, S).

f(_, [], 0).
f(X, [H | T], N) :-
    H = X,
    New is  1,
    f(X, T, N1), !,
    N is New + N1.  
f(X, [_| T], N) :-
    f(X, T, N).


fL([], []).
fL([H | T], FL) :-
    f(H, [H | T], N),
    List = [H, N],
    insertionSort([H | T], [H1 | T1]),
    dele([H1 | T1], [H2 | T2]),
    fL(T2, FL1),
    insert(List, FL1, FL), !.


freq(_,[],0) :- !.
freq(X,[X|T],Num) :- freq(X,T,N), Num is N+1,!. % ! prevents backtracking into next case
freq(X,[_|T],Num) :- freq(X,T,Num),!.
% (b)
freqList(L,FL) :- setof([S,D],(member(S,L),freq(S,L,D)),FL).


myMember(X, []) :-
    fail.
myMember(X, [X | _]).
myMember(X, [_ | T]) :-
    myMember(X, T), !.

% a) a AND b AND c
% b) a AND b or NOT(a) AND c or a AND NOT(b) AND c
% c) a AND b or  NOT(a) AND c
% d) c or NOT(c) AND a AND b
% e) c or NOT(c) AND a AND b
% f) a AND b or a AND NOT(b) AND c or NOT(a) AND c

append2([],L2,L2).
append2([H|T],L2,[H|L4]) :- append2(T,L2,L4).


mySelect(_,[],[]) :- !.
mySelect(X, [X|Xs], Xs).
mySelect(X, [Y|Ys], [Y|Zs]) :- mySelect(X, Ys, Zs), !.

pickTwoDifferent(L,E1,E2) :-
    mySelect(E1,L,L1),
    mySelect(E2,L1,_),
    \+E1=E2, !.

%a AND b or NOT(a AND b) or c
%a AND b or NOT(a) AND c
%c or NOT(c) AND a AND b,
%c
c(1).  % change these to see how p1, p2, p3, p4 are effected
a(1).
b(0).

p1 :- a(1), b(1).
p1 :- c(1).

p2 :- a(1), !, b(1).
p2 :- c(1).

p3 :- c(1).
p3 :- a(1), !, b(1).

p4 :- !, c(1).
p4 :- a(1), b(1).

subseq([], []).
subseq([_|Xs], Ys) :-
    subseq(Xs, Ys).
subseq([X|Xs], [X|Ys]) :-
    prefix_subseq(Xs, Ys).

prefix_subseq(_, []).
prefix_subseq([X|Xs], [X|Ys]) :-
    prefix_subseq(Xs, Ys).


subList(L, S) :-
    findall(X, combinations(L, X), S).
    
combinations([],[]).
combinations([V|Vs],[V|S]) :- combinations(Vs, S).
combinations([_|Vs],S) :- combinations(Vs, S).

keep([],_,[]).
keep([H|T],Max,R) :- 
    keep(T,Max,R2),
    sum_list(H,S),
    (S =< Max -> R = [H | R2]; R = R2).

arc(n1, n2).
arc(n2, n5).
arc(n2, n3).
arc(n3, n7).
arc(n3, n4).
arc(n6, n2).


path(A,B,[arc(A,B)]) :- arc(A,B).%,!.
path(A,B,[arc(A,X) | R]) :-
arc(A,X),
path(X,B,R).%,!.


add(arc(A,A)) :- !,fail. % arcs that start and end at the same node are not allowed
add(arc(A,B)) :- path(B,A,_), !,fail. % arc would cause a cycle
add(arc(A,B)) :- asserta(arc(A,B)).