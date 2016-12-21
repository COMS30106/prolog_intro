# Prolog exercises for self-study #
Below are some exercises designed to help you get up to scratch with Prolog. These exercises are for self-study, you won't get any marks for them. You are encouraged to work through them as soon as possible, to make the most out of the lectures. For those already familiar with Prolog, they should give you an idea of the basic level of Prolog programming required. Advanced programming techniques will be covered in the lectures. (NB. It would be trivial to look up the answers on the web or in a Prolog textbook, but this would entirely miss the point. A 3d year/4th year/MSc CS student should be able to work out the answers for themselves.)  

Before you start, make sure you have read the [Getting started with SWI-Prolog](1_getting_started.md?raw=true) web page.

------------------------------------------------------------------------

## Movie Database Queries ##
Download the [movies.pl](movies.pl) file and open it in SWI-Prolog using the **consult/1** predicate. This file defines a database of facts of the following format (NB. Anything following '%' is treated as a comment in Prolog):

    % movie(M,Y) <- movie M came out in year Y
    movie(american_beauty, 1999).

    % director(M,D) <- movie M was directed by director D
    director(american_beauty, sam_mendes).

    % actor(M,A,R) <- actor A played role R in movie M
    actor(american_beauty, kevin_spacey, lester_burnham).

    % actress(M,A,R) <- actress A played role R in movie M
    actress(american_beauty, annette_bening, carolyn_burnham).

### 1. ###
Write queries to answer the following questions (NB. Press **;** after each answer to see if there are any more answers in the database):

    % In which year was the movie American Beauty released?

    % Find a movie that was released in the year 2000?

    % Find a movie that was released before 2000?

    % Find the name and year of a movie?

    % Find an actor who has appeared in more than one movie?

    % Find a director who has directed a movie in which the actress Scarlett Johansson appeared?

    % Find an actor who has also directed a movie?

    % Find an actor or actress who has also directed a movie?
    % (Hint: to do this in a single query you will need to use disjunction
    % (semicolon) as well as conjunction (comma). 

### 2. ###
Write a definition in Prolog for each of the following predicates:

    % released_since(M,Y) <- movie M was released after year X

    % released_between(M,Y1,Y2) <- movie M was released between year X and year Y inclusive

    % same_year_as(M1,M2) <- movie M1 was released in the same year as movie M2

    % newer(M1,M2) <- movie M1 was released after movie M2

    % cast_member(A,M) <- person A was an actor or actress in movie M
    % (Give as a two predicates)

    % cast_member2(A,M) <- person A was an actor or actress in movie M
    % (Give as a single predicate using the ; disjunction predicate)

    % directed_by(X,Y) <- person X has been in a movie directed by Y
    % (Hint: re-use your cast_member/2 predicate)

Test your definitions by checking all answers to the appropriate query **relation(X,Y)**. In some cases you will find unwanted answers of the form **relation(X,X)**, but don't worry about these for now.

### 3. ###
What's the difference between these queries?

    ?- actor(M1,D,_),actor(M2,D,_).
    ?- actor(M1,D,_),actor(M2,D,_),M1\=M2.
    ?- actor(M1,D,_),actor(M2,D,_),M1@<M2.

### 4. ###
Why do these queries return answers multiple times?

    ?- director(_,D),actor(_,D,_).
    ?- director(_,D),actress(_,D,_).

## List processing predicates ##
The task is to modify the behaviour of some standard list processing predicates. Make sure that you understand the given predicates first. Try to think in terms of minimal changes.

### 5. ###
The following predicate tests list membership:

    % element(X,Ys) <- X is an element of the list Ys
    element(X,[X|Ys]).
    element(X,[_|Ys]):-
        element(X,Ys).

1.  Extend it with a third argument that contains the list with X removed.
2.  Next, adapt the predicate such that it succeeds if X does not occur in the list, and returns the original list in its third argument in this case. Hint: add a second base case.
3.  Next, adapt the predicate such that it removes all occurrences (if any) of X.

### 6. ###
The following predicate tests for a subset:

    % subset(Xs,Ys) <- every element of list Xs occurs in list Ys
    subset([],_).
    subset([X|Xs],Ys):-
        element(X,Ys),
        subset(Xs,Ys).

  1.  Adapt the predicate such that it only succeeds if Xs is a proper subset of Ys (i.e., Ys contains at least one element not in Xs). Hint: use one of the predicates of the previous question.
  2.  Adapt the original predicate such that it only succeeds if every element of Xs occurs in the same order (but not necessarily contiguously) in Ys. Hint: get rid of the **element/2** call, and add a second recursive clause.

### 7. ###
The following predicates both test for a subsequence (a contiguous set of elements):

    % subseq1(Xs,Ys) <- every element of list Xs occurs in the same order and contiguously in list Ys
    subseq1(B,ABC) :-
            append(_,BC,ABC),
            append(B,_,BC).

    % subseq2(Xs,Ys) <- every element of list Xs occurs in the same order and contiguously in list Ys
    subseq2(Xs,Ys):-
            append(Xs,_,Ys).
    subseq2(Xs,[_|Ys]):-
            subseq2(Xs,Ys).

  1.  Both predicates succeed several times with the empty list for queries of the form **subseq(Xs,\[1,2,3,4\])**. Fix this. Hint: treat the empty list as a separate case and make sure the above clauses only apply if Xs is non-empty.

## Pattern matching ##
Pattern matching is used in virtually every predicate definition to distinguish between different cases, often (but not always) base case vs. recursive case.

### 8. ###
Consider the following rules for symbolic differentiation (U, V are mathematical expressions, x is a variable):

    dx/dx = 1
    d(-U)/dx = -(dU/dx)
    d(U+V)/dx = dU/dx + dV/dx
    d(U-V)/dx = dU/dx - dV/dx
    d(U*V)/dx = U*(dV/dx) + V*(dU/dx)

These rules can easily be translated into Prolog, for instance, the third rule can be defined as

    diff(plus(U,V),x,plus(RU,RV)):-diff(U,x,RU),diff(V,x,RV).

Write the remaining rules. Here is a test query:

    ?- diff(plus(times(x,x),x),x,Result).

    Result = plus(plus(times(x, 1), times(x, 1)), 1) ;

    No

Note: Prolog has built-in functors such as +, - and \* that can be used in infix or prefix notation, so the given Prolog clause can also be written as

    diff(U+V,x,RU+RV):-diff(U,x,RU),diff(V,x,RV).

Keep in mind, though, that terms such as U+V are still trees with the functor at the root, and that evaluation of such terms requires additional processing (see the next question).

### 9. ###
Write a predicate that simplifies mathematical expressions as follows:

    U*1 = U
    U+0 = U
    U+U = 2*U
    U-U = 0

Hint: most of your clauses will need to be recursive. Also add a base case that leaves the expression untouched (in case none of the cases applies), and recursive clauses that take expressions of the form U+V and return X+Y, where X is the simplification of U and Y is the simplification of V.  
Here is a test query:

    ?- simp(plus(plus(times(x,1),times(x,1)),1),Result).

    Result = plus(times(2, x), 1)

Note: Most likely, you will get a variety of answers, among which the desired one. This is because your clauses are not mutually exclusive. We will see later how to achieve that.
