move(state(robot(room1, Inventory), Steel, Brass, Package),
'GO TO ROOM 2',
state(robot(room2, Inventory), Steel, Brass, Package)) :-
    member(steelKey, Inventory).
    
    
move(state(robot(room2, Inventory), Steel, Brass, Package),
'GO TO ROOM 1',
state(robot(room1, Inventory), Steel, Brass, Package)) :-
    member(steelKey, Inventory).
    

move(state(robot(room1, Inventory), Steel, Brass, Package),
'GO TO ROOM 3',
state(robot(room3, Inventory),Steel, Brass, Package)) :-
    member(brassKey, Inventory).
    

move(state(robot(room3, Inventory), Steel, Brass, Package),
'GO TO ROOM 1',
state(robot(room1, Inventory), Steel, Brass, Package)) :-
    member(brassKey, Inventory).

move(state(robot(Pos, Inventory), Steel, Brass, Package),
'TAKE STEEL KEY',
state(robot(Pos, [steelKey | Inventory]), inHand, Brass, Package)) :-
    not(member(steelKey, Inventory)),
    Pos = Steel,
    length(Inventory, LengthOfList),
    LengthOfList < 2.

move(state(robot(Pos, Inventory), Steel, Brass, Package), 
'TAKE BRASS KEY',
state(robot(Pos, [brassKey | Inventory]), Steel, inHand, Package)):-
    not(member(brassKey, Inventory)),
    Pos = Brass,
    length(Inventory, LengthOfList),
    LengthOfList < 2.

move(state(robot(Pos, Inventory), Steel, Brass, Package),
'TAKE PACKAGE',
state(robot(Pos, [package | Inventory]), Steel, Brass, inHand)):-
    not(member(package, Inventory)),
    Pos = Package,
    length(Inventory, LengthOfList),
    LengthOfList < 2.


move(state(robot(Pos, Y), Steel, inHand, Package), 
'DROP BRASS KEY',
state(robot(Pos, Lst), Steel, Pos, Package)) :-
    member(brassKey, Y),
    delete(Y, brassKey, Lst).
    

move(state(robot(Pos, Y), inHand, Brass, Package), 
'DROP STEEL KEY',
state(robot(Pos, Lst), Pos, Brass, Package)) :-
    member(steelKey, Y),
    delete(Y, steelKey, Lst).


move(state(robot(Pos, Y), Steel, Brass, inHand), 
'DROP PACKAGE',
state(robot(Pos, Lst), Steel, Brass, Pos)) :-
    member(package, Y),
    delete(Y, package, Lst).

%solveR(state(room2, _, hasPackage), N, [done | []]).

solveR(state(robot(room2, _), _, _, room2), _, []).

solveR(State1, N, [MyCurrentMove | RestOfMoves]) :-
    N > 0,
    move(State1, MyCurrentMove, State2),
    M is N - 1,
    solveR(State2, M, RestOfMoves).
    
%solveR(state(robot(room1, []), room1, room2, room3), 12, MOVES).