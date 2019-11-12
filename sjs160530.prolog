%1---------------------------------------------------------

% Predicate to check if the number is odd or not.
checkOdd(Q):-
    R is Q mod 2,
    R is 1 -> true
    ;false.

% Predicate to check if N is a multiple or 3 or not.
checkMultiple3(N):-
    R is N mod 3,
    R is 0 -> Q is N/3, checkOdd(round(Q))
    ;false.

% Predicate to check multiple of 3.
oddMultOf3(N):-
	(   not(integer(N)) ->
	print("ERROR: The given parameter is not an integer")
	;checkMultiple3(N)).
	
%2--------------------------------------------------------------

list_loop(List, [H|T], Product, Index) :-
    (   not(length(List,Index)) ->  
    	Product is Product * H,
        inc(Index).,
        list_loop(List, T, Product, Index);).

list_prod(List, Product) :-
    (   List == [] ->  Product is 0
    ;   [H|T] is List,
        Product is H,
        list_loop(List,T,Product,0)).
		
%predicate that takes a list and produces a produce of its contents
list_prod(List,Num).
%empty list base clause
list_prod([],0).
%single list base clause
list_prod([H],H).
%recursive call with multiplication logic
list_prod([H|T],Product):- list_prod(T,Prod), Product is Prod * H.

%3-------------------------------------------------------------------
%predicate that takes a list and determines if it represents a palindrome
palindrome(N):-
	reverse(N,N).
	
%4-----------------------------------------------------------------------

% predicate to check if there is an non-numeric element in the list
% prints error messsage if non-numeric
checklist([]):-true.
checklist([H|T]):-
    % Print error the element is not a number.
    not(number(H))-> print("Error: "),print(H),print(" is not a number."),false
    ;checklist(T).

mySort(List,Sorted):-
    %checking for non-numeric characters
    %if a number, bubble sort it, if not print error msg and abort
    checklist(List) -> b_sort(List,[],Sorted)
    ;abort.

%bubble sort used    
b_sort([],Acc,Acc).
b_sort([H|T],Acc,Sorted):-bubble(H,T,NT,Max),b_sort(NT,[Max|Acc],Sorted).
bubble(X,[],[],X).
bubble(X,[Y|T],[Y|NT],Max):-X>Y,bubble(X,T,NT,Max).
bubble(X,[Y|T],[X|NT],Max):-X=<Y,bubble(Y,T,NT,Max).


% % Predicate to find length of a list.
len([],0).
len([_|T],L):-
    len(T,L1),
    L is L1+1.

% Predicate to find to find minimum of first 2 numbers of the list.
findmin([],[]).
findmin([_,T|_],M2):-
    % Set the second minimum value.
    M2 is T.

% predicate to find the second minimum.
secondMin(List, M2):-
    % Sort the list and remove duplicates.
    mySort(List,List2),
    % Calculate the size of the list.
    len(List2,L),
    % Print error if the size of the list is less than 2.
    L < 2 -> print("ERROR: List has fewer than two unique elements.")
    % Otherwise, check the elements of the list.
    ;checklist(List)->
    % Sort the list and find the minimum.
    mySort(List,List2),
    findmin(List2,M2),!;!.

%5-----------------------------------------------------------------------

%predicate that takes a list on integers and generates a list for even elements and a list for off elements
%if list ele is even set to even list, else set to odd list
classify(List,Even,Odd):-
    [H|T] = List,
    0 is H mod 2, (   Even = [H|Even1],
                      classify(T,Even1,Odd)).

classify(List,Even,Odd):-
	[H|T] = List,
    1 is H mod 2, (   Odd = [H|Odd1],
                      classify(T,Even,Odd1)).

%empty list base clause
classify([],[],[]).

%6-----------------------------------------------------------------------------
%predicate that takes three lists as parameters and checks for bookends
bookends(List1,List2,List3):-
    prefix(List1,List3),
    suffix(List2,List3).

prefix([],_):-!.
prefix([H1|T1],[H2|T2]):-
    H1 == H2,
    prefix(T1,T2).

suffix(L,L):-!.
suffix(S,[_|T1]):-
    suffix(S,T1),
    !.
	
%7-----------------------------------------------------------------------------
subslice([],_):-
    !.

subslice(Sub,[H|T]):-
    prefix(Sub,[H|T]),
    !;
    subslice(Sub,T).
%8-----------------------------------------------------------------------------
shift(L1, N, L2) :-
    N < 0, !,             % this is the case for negative N
    length(L1, Len),
    N1 is Len + N,
    shift(L1, N1, L2).  

shift(L1, N, L2) :- 
    append(Lx, Ly, L1), % L1 is Lx || Ly
    append(Ly, Lx, L2), % L2 is Ly || Lx
    length(Lx, N).      % The length of Lx is N
%9-----------------------------------------------------------------------------
sum2(Num,Result):-
    N1 is mod(Num,10),
    N2 is div(Num,10),
    Result is N1+N2,!.

%Function to calculate sum of a 2 digit number.
sum(Num,Result):-
    N1 is mod(Num,10),
    N2 is div(Num,10),
    %Calculate second digit from right * 2.
    N3 is N2*2,
    %If second number from right * 2 is more than 9,
    % call the function sum2.
    N3>9 -> sum2(N3,X),Result is X+N1,!;
    %Otherwise, add the digits.
    N1 is mod(Num,10),
    N2 is div(Num,10),
    N3 is N2*2,
    Result is N1+N3,!.

%Define the base condition.
digitsum(0,Sum,Result):-
    Result is Sum,!.

%Define the function digits().
digitsum(N,Sum,Result):-
    %Extract 2 last digits from the number.
    Rem is mod(N,100),
    Ques is div(N,100),
    %Calculate the sum of 2 digits.
    sum(Rem,S2),
    NewSum is Sum+S2,
    digitsum(Ques,NewSum,Result),!.

%Define the function luhn().
luhn(Num):-
    %Call the function to add the digits.
    digitsum(Num,0,Result),
    %Calculate the value of sum mod 10.
    Rem is mod(Result,10),
    %Check if the mod value is 0 or not.
    Rem = 0.
%10----------------------------------------------------------------------------

%predicate implementing the graph predicate
edge(a,b).
edge(b,c).
edge(c,d).
edge(d,e).
edge(d,a).

path(Z,Z).
path(Node1,Node2):-
    edge(Node1, A), 
    path(A, Node2),
    !.

cycle(Node):-
    edge(Node,X),
    path(X, Node),!.