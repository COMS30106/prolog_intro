%%% Plotting terms as trees using Graphviz %%%

term_list_linear(false).	% change to true for plotting lists linearly

term(Term,Dot):-
	gv_start('term.dot'),
	Term =.. [Functor|Subterms],
	gv_root(Functor,0),
	term_list(Subterms,0),
	gv_stop(Dot).

terml(Term,N):-
	var(Term),!,
	gv_node(N,Term,_).
terml(Term,N):-
	term_list_linear(true),
	list(Term),!,
	gv_node(N,Term,N1),
	term_list(Term,N1).
terml([],N):-!,
	gv_node(N,'$empty_list',_).
terml(Term,N):-
	Term =.. [Functor|Subterms],
	gv_node(N,Functor,N1),
	term_list(Subterms,N1).

term_list([],_).
term_list([Term|Terms],N):-
	terml(Term,N),
	term_list(Terms,N).

% testing
term1(Dot):-term([a,b,b,a],Dot).
term2(Dot):-term(
html(head(title('Peter A. Flach')),
	body([img([align=right,src='logo.jpg']),
		img([align=left,src='peter.jpg']),
		h1('Peter Flach\'s homepage'),
		h2('Research interests'),
		ul([li('Learning from structured data'),
			bla,
			li(a([href='CV.pdf'],'Full CV'))]),
		h2('Current activities'),
		bla,
		h2('Past activities'),
		bla,
		h2('Archives'),
		bla,
		hr,address(bla)
	    ])
         )
,Dot
).


%%% Meta-interpreter plotting (part of) the SLD-tree using Graphviz %%%

%:-op(1100,fx,sld).	% can write ?-sld Goal instead of ?-sld(Goal)

sld(Goal,Dot):-
	sld(Goal,5,Dot).	% default depth bound

sld(Goal,D,_):-
	gv_start('sld.dot'),
	gv_root((?-Goal),0),
	prove_d(Goal,Goal,0,D),
	fail.	% failure-driven loop to get all solutions
sld(_,_,Dot):-
	gv_stop(Dot).

% meta-interpreter with complete resolvent and depth bound
prove_d(true,Goal,N,_):-!,
	gv_answer(N,Goal).
prove_d((A,B),Goal,N,D):-!,
	D>0, D1 is D-1,
	resolve(A,C),
	conj_append(C,B,E),
	gv_node(N,(:-E),N1),
	prove_d(E,Goal,N1,D1).
prove_d(A,Goal,N,D):-
	D>0, D1 is D-1,
	resolve(A,B),
	gv_node(N,(:-B),N1),
	prove_d(B,Goal,N1,D1).

resolve(A,true):-
	predicate_property(A,built_in),!,
	call(A).
resolve(A,B):-
	clause(A,B).

% testing
student_of(X,T):-follows(X,C),teaches(T,C).
follows(paul,computer_science).
follows(paul,expert_systems).
follows(maria,ai_techniques).
teaches(adrian,expert_systems).
teaches(peter,ai_techniques).
teaches(peter,computer_science).

brother_of(paul,peter).
brother_of(peter,adrian).
brother_of(X,Y):-brother_of(X,Z),brother_of(Z,Y).
brother_of(X,Y):-brother_of(Y,X).

sld1(Dot):-sld(student_of(_,peter),Dot).
sld2(Dot):-sld(brother_of(paul,_),Dot).


%%% Utilities %%%

list([]).
list([_|T]):-list(T).

conj_element(X,X):-	% single-element conjunction
	X \= true,
	X \= (_,_).
conj_element(X,(X,_)).
conj_element(X,(_,Ys)):-
	conj_element(X,Ys).

conj_append(true,Ys,Ys).
conj_append(X,Ys,(X,Ys)):-	% single-element conjunction
	X \= true, 
	X \= (_,_).
conj_append((X,Xs),Ys,(X,Zs)):-
	conj_append(Xs,Ys,Zs).

writes([]):-!.
writes([H|T]):-!,writes(H),writes(T).
writes((A,B)):-!,writes(A),my_assert(',\\n'),writes(B).	% break up conjunctions
writes(:-A):-!,my_assert(':-'),writes(A).
writes(?-A):-!,my_assert('?-'),writes(A).
writes('$empty_list'):-!,my_assert('[]').
writes(A):-(atom(A)->my_assert(A);term_to_atom(A,B),my_assert(B)).	% catch-all

my_assert(A):-
	assertz('$my_assert'(A)).

get_dot(A):-
	get_dot('',A).

get_dot(In,Out):-
	( retract('$my_assert'(A)) -> atom_concat(In,A,In1),get_dot(In1,Out)
	; otherwise -> Out=In
	).
	
	
%%% Graphviz utilities %%%

gv_max_id(1000).	% max number of nodes in the graph

% open file and start new graph
gv_start(FileName):-
	retractall('$my_assert'(_)),
	tell(FileName),
	writes(['digraph {']),
	%writes(['graph [size="4,6"];']),
	writes(['node [shape=plaintext, fontname=Courier, fontsize=12]']).

% next graph
gv_next:-
	writes(['}']),
	writes(['digraph {']),
	writes(['node [shape=plaintext, fontname=Courier, fontsize=12]']).

% finish graph and close file
gv_stop(Dot):-
	writes(['}']),
	told,
	get_dot(Dot).

% start new subgraph
gv_cluster_start:-
	( retract('$gv_cluster'(N)) -> N1 is N+1
	; otherwise -> N1=0
	),assert('$gv_cluster'(N1)),
	writes(['subgraph cluster_',N1,' {']),
	writes(['[style=filled, color=lightgrey];']),
	writes(['node [style=filled,color=white];']).

% finish subgraph 
gv_cluster_stop:-
	writes(['}']).

% write the root of a tree and initialise node IDs
gv_root(L,N):-
	writes([N,' [label="',L,'"];']),
	gv_init_ids(N).

% add a node with label L and parent N0
gv_node(N0,L,N):-
	gv_id(N),
	writes([N,' [label="',L,'"];']),
	writes([N0,' -> ',N,';']).

% add a specially formatted leaf
gv_answer(N0,L):-
	gv_id(N),
	writes([N,' [label="Answer:\\n',L,'", shape=ellipse, style=dotted, fontsize=10];']),
	writes([N0,' -> ',N,' [style=dotted, arrowhead=none];']).
	%writes(['{rank=same;',N0,';',N,';}']).

% generate a new node ID
gv_id(N):-
	retract('$gv_id'(N0)),
	gv_max_id(M),
	N0 < M,	% don't generate infinite graphs
	N is N0+1,
	assert('$gv_id'(N)).

% initialise node IDs, next free ID is N+1
gv_init_ids(N) :-
	retractall('$gv_id'(_)),
	assert('$gv_id'(N)).

