%----------------------------------->>>>>>>>>State intialization
% grid([
%     [('red', 6), ('blue', 5), ('blue', 4), ('red', 3)],
%     [('red', 5), ('red', 4), ('blue', 3), ('blue', 2)],
%     [('red', 2), ('red', 3), ('red', 2), ('red', 1)],
%     [('blue', 3), ('blue', 2), ('blue', 1), ('red', 0)]
% ]).

grid([
    [('red', 1), ('red', 1), ('yellow', 1), ('yellow', 1)],
    [('red', 1), ('blue', 1), ('red', 1), ('red', 1)],
    [('red', 1), ('red', 1), ('red', 1), ('yellow', 1)],
    [('blue',1), ('red', 1), ('blue',1), ('yellow', 1)]
]).

% grid([
%     [('red', 6), ('blue', 5), ('blue', 4), ('red', 3)],
%     [('red', 5), ('red', 4), ('blue', 3), ('blue', 2)],
%     [('red', 4), ('red', 3), ('red', 2), ('red', 1)],
%     [('blue', 3), ('blue', 2), ('blue', 1), ('red', 0)]
% ]).


startpos((0, 0)).  
goalpos((1, 3)). 

%----------------------------------->>>>>>>>>print grid part

% Print each row of the grid
print_grid_rows([]) :- nl. % End 

print_grid_rows([Row | Rest]) :-
    print_grid_row(Row),
    print_grid_rows(Rest). % Recurse to print 

% Print a single row with color and cost
print_grid_row([]) :- nl. % End 

print_grid_row([(Color, Cost) | Rest]) :-
    write((Color, Cost)), % print node data
    write(' '), 
    print_grid_row(Rest). % recursive calling 

print_grid(Grid) :-
    writeln("Grid:"),
    print_grid_rows(Grid).
    
%----------------------------------->>>>>>>>>Data strucure operations

% Priority Queue Operations
queue_add(Items, Queue, NewQueue) :-
    append(Queue, Items, TempQueue), %push
    sort(0, @=<, TempQueue, NewQueue). % Sort to order by lowest cost

queue_pop([Head | Tail], Tail, Head). % Pop 

%----------------------------------->>>>>>>>>heuristic predicate 

% Heuristic Function: Manhattan Distance
% Calculates the estimated cost from the current position to the goal
hueristic_value((R1, C1), (R2, C2), Distance) :-
    Distance is abs(R1 - R2) + abs(C1 - C2). % Manhattan distance formula

%----------------------------------->>>>>>>>>Move predicates 

move((R, C), (R, C1)) :- 
    C1 is C + 1. % Right

move((R, C), (R, C1)) :- 
    C1 is C - 1. % Left

move((R, C), (R1, C)) :- 
    R1 is R - 1. % Up

move((R, C), (R1, C)) :- 
    R1 is R + 1. % Down

%----------------------------------->>>>>>>>>bound validation 

in_bounds((R, C), Grid) :-
    length(Grid, Rows),
    R >= 0,
    R < Rows,
    nth0(0, Grid, FirstRow),
    length(FirstRow, Cols),
    C >= 0,
    C < Cols.

%----------------------------------->>>>>>>>>color validation


valid_move((R, C), (R1, C1), Grid) :-
    in_bounds((R1, C1), Grid),
    nth0(R, Grid, Row),
    nth0(C, Row, (Color, _)), % color of given cell
    nth0(R1, Grid, Row1),
    nth0(C1, Row1, (Color1, _)), % color of target cell
    Color = Color1. % compare 

%----------------------------------->>>>>>>>>cost retrival 

get_cost((R, C), Grid, Cost) :-
    nth0(R, Grid, Row),
    nth0(C, Row, (_, Cost)).

%----------------------------------->>>>>>>>>A_star intial calling 

a_star(Grid, Start, Goal, Path) :-
    get_cost(Start, Grid, StartCost),
    hueristic_value(Start, Goal, HeuristicCost), % Calculate heuristic for Start
    queue_add([[HeuristicCost, StartCost, [(Start, StartCost)]]], [], PQ), % Initialize with heuristic and path cost
    a_star_search(Grid, PQ, Goal, [], Path).

%----------------------------------->>>>>>>>>A_star algorithm 

% A* Search Predicate with Cost-Based Priority Queue and Heuristic
a_star_search(Grid, PQ, Goal, Visited, Path) :-
    % Pop the path with the lowest Cost from the queue
    queue_pop(PQ, NewPQ, PathWithCost),
    
    % Extract priority, current cost, and path
    [Priority, CurrentCost, [(CurrentPos, CurrentCostNode) | Rest]] = PathWithCost,
    
    % Check if we've reached the goal
    (CurrentPos = Goal ->
        reverse([(CurrentPos, CurrentCostNode) | Rest], Path), % Reverse to get the correct path
        ! % Cut when reaching goal 
    ;
        % Goal not reached -> BFS on all valid moves and their updated costs
        findall(
            [NewPriority, NewCost, [(NewPos, NewCostNode), (CurrentPos, CurrentCostNode) | Rest]], % New valid path with updated cost
            (
                move(CurrentPos, NewPos), % Generate new position
                valid_move(CurrentPos, NewPos, Grid), % Check if the move is valid
                \+ member((NewPos, _), Visited), % Ensure new position isn't in visited
                get_cost(NewPos, Grid, NewCostNode), % Get the cost of the new position
                NewCost is CurrentCost + NewCostNode, % Update the actual path cost
                hueristic_value(NewPos, Goal, NewHeuristic), % Heuristic from NewPos to Goal
                NewPriority is NewCost + NewHeuristic % Total priority
            ),
            NewPathsWithCost % Collect all valid new paths with their costs and heuristics
        ),
        
        % If new valid paths are found, continue with the updated queue
        (NewPathsWithCost \= [] ->
            queue_add(NewPathsWithCost, NewPQ, UpdatedPQ), % Add new paths to the queue
            a_star_search(Grid, UpdatedPQ, Goal, [(CurrentPos, CurrentCostNode) | Visited], Path) % Recurse with updated queue
        ;
            % No valid paths found, search fails
            fail
        )
    ).

%----------------------------------->>>>>>>>>Cost print

% failor 
print_solution([]) :- writeln("No path found. :< ").
%success
print_solution(Path) :-
    % Calculate the total cost for the path
    total_path_cost(Path, TotalCost),
    length(Path, PathLength),
    writeln("Path found :OOO "),%success message 
    writeln("Path:"),
    print_path(Path), % Print the path with costs
    write("Path length: "), writeln(PathLength),
    write("Total cost: "), writeln(TotalCost).

%----------------------------------->>>>>>>>>Total cost calculation

total_path_cost([], 0). % Base case 

total_path_cost([((_, _), Cost) | T], TotalCost) :-
    total_path_cost(T, RestCost), % Recursive call
    TotalCost is Cost + RestCost.

%----------------------------------->>>>>>>>>Path print

print_path([]) :- nl. % End 

print_path([((R, C), Cost) | T]) :- 
    write((R, C, Cost)), % Print the node
    (T \= [] -> write(" -> "); true),
    print_path(T). % recursive calling 

%----------------------------------->>>>>>>>>User calling predicate

find_path :-
    grid(Grid), 
    startpos(Start),
    goalpos(Goal), 
    print_grid(Grid), 
    (a_star(Grid, Start, Goal, Path) -> print_solution(Path); print_solution([])).
