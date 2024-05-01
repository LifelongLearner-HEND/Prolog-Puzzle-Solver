% define the colors of the board
color(r).
color(y).
color(b).

% get the state of the board at a given position
getState(Board, X, Y, Color):-
    nth0(X, Board, Row),
    nth0(Y, Row, Color).

% move through columns
getNextState(N, M, X, Y, X1, Y1):-
    Y < M - 1, !,
    X1 is X,
    Y1 is Y + 1.

% move through rows
getNextState(N, M, X, Y, X1, Y1):-
    X < N - 1,
    X1 is X + 1,
    Y1 is 0.

% print the cycle
printCycle([]).

printCycle([X|Rest]):-
    write(X), write(' -> '),
    printCycle(Rest).

% print the solution
printSolution(Cycle, Color):-
    write('Cycle: '),
    printCycle(Cycle),
    write('Color: '),
    write(Color),
    nl.

% check if the current state position is a goal
isGoal(Board, N, M, X, Y, Color, Cycle):-
    cycle(Board, N, M, X, Y, Color, [[X,Y]], 0, Cycle).

cycle(Board, N, M, X, Y, Color, Visited, Count, Cycle):-
    right(Board, N, M, X, Y, Color, Visited, Count, Cycle).

right(Board, N, M, X, Y, Color, Visited, Count, Cycle):-
    Y1 is Y + 1,
    nth0(X, Board, Row),
    nth0(Y1, Row, Color1),
    not(member([X,Y1], Visited)),
    Color = Color1,
    append(Visited, [[X,Y1]], NewVisited),
    Count1 is Count + 1,
    Count1 < 4,
    down(Board, N, M, X, Y1, Color, NewVisited, Count, Cycle).

down(Board, N, M, X, Y, Color, Visited, Count, Cycle):-
    X1 is X + 1,
    nth0(X1, Board, Row),
    nth0(Y, Row, Color1),
    not(member([X1,Y], Visited)),
    Color = Color1,
    append(Visited, [[X1,Y]], NewVisited),
    Count1 is Count + 1,
    Count1 < 4,
    left(Board, N, M, X1, Y, Color, NewVisited, Count, Cycle).

left(Board, N, M, X, Y, Color, Visited, Count, Cycle):-
    Y1 is Y - 1,
    nth0(X, Board, Row),
    nth0(Y1, Row, Color1),
    not(member([X,Y1], Visited)),
    Color = Color1,
    append(Visited, [[X,Y1]], NewVisited),
    Count1 is Count + 1,
    Count1 < 4,
    up(Board, N, M, X, Y1, Color, NewVisited, Count, Cycle).
    
up(Board, N, M, X, Y, Color, Visited, Count, Cycle):-
    append(Visited, [], Cycle).

% search for cycles in the board
search(Board, X, Y):-
    length(Board, N),
    nth0(0, Board, Row),
    length(Row, M),
    getState(Board, X, Y, Color),
    isGoal(Board, N, M, X, Y, Color, Cycle), !,
    printSolution(Cycle, Color), 
    getNextState(N, M, X, Y, X1, Y1),
    search(Board, X1, Y1). % continue searching for more cycles

search(Board, X, Y):-
    length(Board, N),
    nth0(0, Board, Row),
    length(Row, M),
    getState(Board, X, Y, _), 
    getNextState(N, M, X, Y, X1, Y1),
    search(Board, X1, Y1). % continue searching

search(_, _, _):-
    write('Search Finished!').


% test case in assignment
% search([[y, y, y, r], [b, y, b, y], [b, b, b, y], [b, b, b, y]], 0, 0).

% another test case
% search([[y, y, b], [y, y, r], [b, r, y]], 0, 0).

% another test case for no cycles
% search([[y, y, b], [b, y, r], [b, r, y]], 0, 0).
