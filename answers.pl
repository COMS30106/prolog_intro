%%% Movies Database Queries %%%

%%% Q1 %%%
% In which year was the movie American Beauty released
% ?- movie(american_beauty,Y).
% NB. This is a query to be typed in at the Prolog prompt; 
% you can turn it into a clause by naming the query, e.g. american_beauty_year(Y) :- movie(american_beauty,Y).
% Find a movie that was released in the year 2000?
% ?- movie(M,2000).
% Find a movie that was released before 2000?
% ?- movie(M,Y),Y<2000.
% Find the name and year of a movie?
% ?- movie(M,Y).
% Find an actor who has appeared in more than one movie?
% ?- actor(M1,A,_),actor(M2,A,_),M1@<M2.  % or just M1\=M2.
% Find a director who has directed a movie in which the actress Scarlett Johansson appeared?
% ?- actress(M,scarlett_johansson,_),director(M,D).
% Find an actor who has also directed a movie?
% ?- director(M,A),actor(M,A,_).
% Find an actor or actress who has also directed a movie?
% (Hint: to do this in a single query you will need to use the ; disjunction operator)
% ?- director(M,A),(actor(M,A,_);actress(M,A,_)).

%%% Q2 %%%
% released_since(M,Y) <- movie M was released after year X
released_since(M,Y) :- movie(M,X), X>Y.
% released_between(M,Y1,Y2) <- movie M was released between year X and year Y inclusive
released_between(M,Y1,Y2) :- movie(M,Y), Y>=Y1, Y=<Y2.
% same_year_as(M1,M2) <- movie M1 was released in the same year as movie M2
same_year_as(M1,M2) :- movie(M1,Y), movie(M2,Y), M1\=M2.
% newer(M1,M2) <- movie M1 was released after movie M2
newer(M1,M2) :- movie(M1,Y1), movie(M2,Y2), Y1>Y2.
% cast_member(A,M) <- person A was an actor or actress in movie M
% (Give as a two predicates)
cast_member(A,M) :- actor(M,A,_).
cast_member(A,M) :- actress(M,A,_).
% cast_member2(A,M) <- person A was an actor or actress in movie M
% (Give as a single predicate using the ; disjunction operator)
cast_member2(A,M) :- actor(M,A,_) ; actress(M,A,_).
% directed_by(X,Y) <- person X has been in a movie directed by Y
% (Hint: re-use your cast_member/2 predicate)
directed_by(X,Y) :- director(M,Y), cast_member(X,M).

%%% Q3 %%%
% What's the difference between these queries?
% ?- actor(M1,D,_),actor(M2,D,_). -- returns every actor as M1 and M2 are not necessarily different
% ?- actor(M1,D,_),actor(M2,D,_),M1\=M2. -- succeeds twice for every pair of different movies
% ?- actor(M1,D,_),actor(M2,D,_),M1@<M2. -- succeeds once for every pair of different movies

%%% Q4 %%%
% Why do these queries return answers multiple times?
% ?- director(_,D),actor(_,D,_).
% ?- director(_,D),actress(_,D,_).
% Answer: if D directed n movies and acted in m movies, the query succeeds n*m times. 
% We will see later how to avoid this.

%%% List processing predicates %%%
%%% Q5 %%%
% element(X,Ys) <- X is an element of the list Ys
element(X,[X|Ys]).
element(X,[_|Ys]) :- 
        element(X,Ys).
% remove_one(X,Ys,Zs) <- list Zs is list Ys without one occurrence of X
remove_one(X,[X|Ys],Ys).
remove_one(X,[Y|Ys],[Y|Zs]) :- 
        remove_one(X,Ys,Zs).
% remove_one2(X,Ys,Zs) <- list Zs is list Ys without one occurrence of X
%                         if it occurs in Ys, and Zs is Ys otherwise
% NB. Not entirely correct as it will also succeed if X occurs in Ys and Zs=Ys
remove_one2(X,[],[]).
remove_one2(X,[X|Ys],Ys).
remove_one2(X,[Y|Ys],[Y|Zs]) :- 
        remove_one2(X,Ys,Zs).
% remove_all(X,Ys,Zs) <- list Zs is list Ys without any occurrences of X
% NB. Not entirely correct as it will also succeed with some occurrences of X in Zs
remove_all(X,[],[]).
remove_all(X,[X|Ys],Zs) :- 
        remove_all(X,Ys,Zs).
remove_all(X,[Y|Ys],[Y|Zs]) :- 
        remove_all(X,Ys,Zs).

%%% Q6 %%%
% subset(Xs,Ys) <- every element of list Xs occurs in list Ys
subset([],_).
subset([X|Xs],Ys) :- 
        element(X,Ys),
        subset(Xs,Ys).
% proper_subset(Xs,Ys) <- every element of list Xs occurs in list Ys, and 
%                         Ys has at least one element more than Xs
proper_subset([],[_|_]).
proper_subset([X|Xs],Ys) :- 
        remove_one(X,Ys,Zs),
        proper_subset(Xs,Zs).
% subset_ord(Xs,Ys) <- every element of list Xs occurs in the same order in list Ys
subset_ord([],_).
subset_ord([X|Xs],[X|Ys]) :- 
        subset_ord(Xs,Ys).
subset_ord(Xs,[_|Ys]) :- 
        subset_ord(Xs,Ys).

%%% Q7 %%%
% subseq1(Xs,Ys) <- every element of list Xs occurs in the same order and contiguously in list Ys
subseq1([],_).
subseq1([B|Bs],ABC) :-
        append(_,BC,ABC),
        append([B|Bs],_,BC).
% subseq2(Xs,Ys) <- every element of list Xs occurs in the same order and contiguously in list Ys
% NB. The first two clauses can be combined into subseq2(Xs,Ys) :- append(Xs,_,Ys)
% i.e., the original base case
subseq2([],_).
subseq2([X|Xs],Ys) :- 
        append([X|Xs],_,Ys).
subseq2([X|Xs],[_|Ys]) :- 
        subseq2([X|Xs],Ys).

%%% Pattern matching %%%
%%% Q8 %%%
diff(x,x,1).
diff(minus(U),x,minus(RU)) :- diff(U,x,RU).
diff(plus(U,V),x,plus(RU,RV)) :- diff(U,x,RU),diff(V,x,RV).
diff(minus(U,V),x,minus(RU,RV)) :- diff(U,x,RU),diff(V,x,RV).
diff(times(U,V),x,plus(times(U,RV),times(V,RU))) :- diff(U,x,RU),diff(V,x,RV).

%%% Q9 %%%
simp(times(U,1),V) :- simp(U,V).
simp(plus(U,0),V) :- simp(U,V).
simp(plus(U,U),times(2,V)) :- simp(U,V).
simp(minus(U,U),0).
% Some kind of base case is needed; multiple solutions could be avoided by demanding that U is atomic
simp(U,U).
% Keep top-level the same, simplify at lower level
simp(plus(U,X),plus(V,Y)) :- simp(U,V),simp(X,Y).
