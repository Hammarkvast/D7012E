move( state(room1, noSteelKey,P),
grabSteelKey,
state(room1, hasSteelKey, P)).

move( state(room1, hasSteelKey, P),
room1ToRoom2,
state(room2, hasSteelKey, P)).

move( state(room2, hasSteelKey, P),
room2ToRoom1,
state(room1, hasSteelKey, P)).

move(state(room2, hasSteelKey, noBrassKey),
grabBrassKey,
state(room2, hasSteelKey, hasBrassKey)).

move(state(room1, P, hasBrassKey),
room1ToRoom3,
state(room3, P, hasBrassKey)).

move(state(room3, P, noPackage),
grabPackage,
state(room3, P, hasPackage)).