%   Title:
%         Programming Assignment #2
%         Prolog Programming Assignment
%
%   Author:
%         Stuart Small- sjs160530
%         Computer Science Undergraduate
%         sjs160530@utdallas.edu
%
%  Description:
%         This file contains multiple procedures that perform seperate tasks.
%
%    Date:
%         11/11/2019
%


% Question 1

% predicate to check if the number is odd
odd(N):-
    R is N mod 2,
    R is 1 -> true
    ;false.

% predicate to check if the number is a multiple of 3
multiple3(N):-
    R is N mod 3,
    R is 0 -> D is N/3, odd(round(D))
    ;false.

% predicate to check for odd multiple of 3.
oddMultOf3(N):-
	(   not(integer(N)) ->
	print("ERROR: The given parameter is not an integer")
	;multiple3(N)).
	

% Question 2
		
%predicate that takes a list and produces a produce of its contents

%empty list base clause
list_prod([],0).

%single list base clause
list_prod([H],H).

%recursive call with multiplication logic
list_prod([H|T],Product):- list_prod(T,P), Product is P * H.


% Question 3

%predicate that takes a list and determines if it represents a palindrome
palindrome(N):-
	reverse(N,N).
	

% Question 4

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


% predicate to find length of a list.
len([],0).
len([_|T],Len):-
    len(T,Len1),
    Len is Len1+1.

% predicate to find to find minimum of first 2 numbers of the list.
min([],[]).
min([_,T|_],M2):-
    % Set the second minimum value.
    M2 is T.

% predicate to find the second minimum.
secondMin(List, M2):-
    % Sort the list and remove duplicates.
    mySort(List,List2),
    % compute the size of the list.
    len(List2,Len),
    % Print error if the size of the list is less than 2.
    Len < 2 -> print("ERROR: List has fewer than two unique elements.")
    % Otherwise, check the elements of the list.
    ;checklist(List)->
    % Sort list 
    mySort(List,List2),
    % find minimum of list
    min(List2,M2),!;!.

	
% Question 5

%predicate that takes a list on integers and generates a list for even elements and a list for off elements
%if list ele is even set to even list, else set to odd list
%even predicate
classify(List,Even,Odd):-
    [H|T] = List,
    0 is H mod 2, (   Even = [H|Even1],
                      classify(T,Even1,Odd)).

%odd predicate
classify(List,Even,Odd):-
	[H|T] = List,
    1 is H mod 2, (   Odd = [H|Odd1],
                      classify(T,Even,Odd1)).

%empty list base clause
classify([],[],[]).


% Question 6

%predicate that takes three lists as parameters and checks for bookends
bookends(List1,List2,List3):-
    pre(List1,List3),
    suf(List2,List3).

%predicates that checks is the first passed list is a prexix of the other
pre([],_):-!.
pre([H1|T1],[H2|T2]):-
    H1 == H2,
    pre(T1,T2).

%predicates that checks is the first passed list is a suffix of the other
suf(L,L):-!.
suf(S,[_|T]):-
    suf(S,T),
    !.
	
% Question 7

%predicate that tests if the first list parameter is a contiguous series 
% of elements anywhere within the second list parameter
subslice([],_):-!.
subslice(Sub,[H|T]):-
    prefix(Sub,[H|T]),
    !;
    subslice(Sub,T).
	
% Question 8

% predicate that "shifts" or "rotates" a list N places to the left. N may be
% a negative number, i.e. rotate to the right. Your predicate should have the 
% signature shift(+List, +Integer, +List).

shift(L1, N, L2) :-
	% case for N<0
    N < 0, !,             
    length(L1, Len),
    N1 is Len + N,
    shift(L1, N1, L2).  

shift(L1, N, L2) :- 
    append(Lx, Ly, L1), % L1 is Lx || Ly
    append(Ly, Lx, L2), % L2 is Ly || Lx
    length(Lx, N).      % The length of Lx is N
	

% Question 9

% predicates that are an implementation of the Luhn Algorithm and returns
%  true if the parameter is an integer and passes the Luhn test and false otherwise

sum2(Num,Result):-
    N1 is mod(Num,10),
    N2 is div(Num,10),
    Result is N1+N2,!.

% predicate that computes sum of a 2 digit number.
sum(Num,Result):-
    N1 is mod(Num,10),
    N2 is div(Num,10),
    % compute second digit from right * 2.
    N3 is N2*2,
    % if second number from right * 2 is more than 9,
    % call the function sum2.
    N3>9 -> sum2(N3,X),Result is X+N1,!;
    % else add the digits.
    N1 is mod(Num,10),
    N2 is div(Num,10),
    N3 is N2*2,
    Result is N1+N3,!.

% base condition.
digitsum(0,Sum,Result):-
    Result is Sum,!.

% predicate digits().
digitsum(N,Sum,Result):-
    % get the 2 last digits from the number.
    Remainder is mod(N,100),
    Q is div(N,100),
    % compute the sum of 2 digits.
    sum(Remainder,S2),
    NewSum is Sum+S2,
    digitsum(Q,NewSum,Result),!.

% luhn predicate
luhn(Num):-
    % call the predicate that adds the digits.
    digitsum(Num,0,Result),
    % compute the value of sum mod 10.
    Remainder is mod(Result,10),
    % check if the mod value is 0 or not.
    Remainder = 0.
	

% Question 10

% predicates implementing the graph predicate
% knowledge base
edge(a,b).
edge(b,c).
edge(c,d).
edge(d,e).
edge(d,a).

% path predicate
path(X,X).
path(Node1,Node2):-
    edge(Node1, A), 
    path(A, Node2),
    !.
% cycle predicate
cycle(Node):-
    edge(Node,X),
    path(X, Node),!.