getDomino(N,M,D):-
    %horizontal domino
    LastCell is N*M,
    between(1, LastCell, I),
    SecondCell is I+1,
    Check is SecondCell mod M ,
    Check =\= 1,
    D=[I, SecondCell].

getDomino(N, M ,D):-
    %vertical domino
    LastCell is N*M,
    between(1, LastCell, I),     %I is the first cell of the domino itself  , Last cell is the bottom right corner
    SecondCell is I+M,
    SecondCell =< LastCell,
    D= [I, SecondCell].     

overlapcheckBombs(D,B1,B2):-
    \+ member(B1,D),\+ member(B2,D).

overlapcheckBoard(D,Board):-
    flatten(Board, NewBoard),
    intersection(D,NewBoard,Overlaped),
    length(Overlaped, 0).
    

    %test(N, M) :-
    %(   getDomino(N, M, D)
    %;   % If getDomino/3 fails, do something else here
     %   writeln('getDomino failed')
    %).



%fullboardcheck(N,M,Board):-
  
 getAllPossibleSteps(N,M,B1,B2,Ds):-
    findall(D,(getDomino(N,M,D),
    overlapcheckBombs(D,B1,B2)
    ), 
    Ds).


%filterPossiblePlays(PossiblePlays,D):-

filterPossiblePlays([], _ , []).
filterPossiblePlays([[X,Y]|T], ExcludeList, NewList) :-
    (   (member(X, ExcludeList); member(Y, ExcludeList)) 
    ->  filterPossiblePlays(T, ExcludeList, NewList) 
    ;   NewList = [[X,Y]|Rest], filterPossiblePlays(T, ExcludeList, Rest) 
    ).
   



getThingsReady(N , M , B1 , B2 ,Board , Solution):-
    getAllPossibleSteps(N,M,B1,B2,Ds),
    %write(Ds),nl,
    solve(N,M,B1,B2,Board,Solution,Ds).


solve(N, M, B1 , B2 , Board , Solution,PossiblePlays):- %board is database of dominos to not overlap
    getDomino(N,M,D),
    overlapcheckBombs(D,B1,B2),
    overlapcheckBoard(D,Board),
    append([D], Board, NewBoard),
    filterPossiblePlays(PossiblePlays,D,NewPossiblePlays),
    %write(PossiblePlays),nl,
    %write(NewPossiblePlays),nl,
    (NewPossiblePlays = [] -> (Solution = NewBoard,true) ; 
    solve(N,M,B1,B2,NewBoard,Solution,NewPossiblePlays)).

   %fullboardcheck(N,M,Board),
   % NewPossiblePlays = [],
   % length(NewPossiblePlays,0),
   % Solution = Board.

    
    