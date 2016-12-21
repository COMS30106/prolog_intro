%%% Plotting terms as trees using Graphviz %%%

term_list_linear(false).	% change to true for plotting lists linearly
write_to_file(false).     % write Graphviz file

term(Term, Dot):-
	gv_start('term.dot', DotOut1),
	Term =.. [Functor|Subterms],
	gv_root(Functor,0, DotOut1, DotOut2),
	term_list(Subterms,0, DotOut2, DotOut3),
	gv_stop(DotOut3, Dot).

term(Term,N, DotIn, DotOut):-
	var(Term),!,
	gv_node(N,Term,_, DotIn, DotOut).
term(Term,N, DotIn, DotOut):-
	term_list_linear(true),
	list(Term),!,
	gv_node(N,Term,N1, DotIn, DotOut1),
	term_list(Term,N1, DotOut1, DotOut).
term([],N, DotIn, DotOut):-!,
	gv_node(N,'$empty_list',_, DotIn, DotOut).
term(Term,N, DotIn, DotOut):-
	Term =.. [Functor|Subterms],
	gv_node(N,Functor,N1, DotIn, DotOut1),
	term_list(Subterms,N1, DotOut1, DotOut).

term_list([],_, Dot, Dot).
term_list([Term|Terms],N, DotIn, DotOut):-
	term(Term,N, DotIn, DotOut1),
	term_list(Terms,N, DotOut1, DotOut).

% testing
term1(Dot):-term([a,b,b,a], Dot).
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
  ),
  Dot
).


%%% Meta-interpreter plotting (part of) the SLD-tree using Graphviz %%%

%:-op(1100,fx,sld).	% can write ?-sld Goal instead of ?-sld(Goal)
:-op(200,xfx,sld).	% can write ?-Goal sld Dot instead of ?-sld(Goal, Dot)

sld(Goal, Dot):-
  sld(Goal,5, DotOut),	% default depth bound
  sld_(DotOut, Dot).

sld(Goal,D, Dot):-
	gv_start('sld.dot', DotOut),
	gv_root((?-Goal),0, DotOut, DotOut1),
	prove_d(Goal,Goal,0,D, DotOut1, Dot),
	fail.	% failure-driven loop to get all solutions
sld_(DotIn, DotOut):-
	gv_stop(DotIn, DotOut).

% meta-interpreter with complete resolvent and depth bound
prove_d(true,Goal,N,_, DotIn, DotOut):-!,
	gv_answer(N,Goal, DotIn, DotOut).
prove_d((A,B),Goal,N,D, DotIn, DotOut):-!,
	D>0, D1 is D-1,
	resolve(A,C),
	conj_append(C,B,E),
	gv_node(N,(:-E),N1, DotIn, DotOut1),
	prove_d(E,Goal,N1,D1, DotOut1, DotOut).
prove_d(A,Goal,N,D, DotIn, DotOut):-
	D>0, D1 is D-1,
	resolve(A,B),
	gv_node(N,(:-B),N1, DotIn, DotOut1),
	prove_d(B,Goal,N1,D1, DotOut1, DotOut).

resolve(A,true):-
	%predicate_property(A,built_in),!,
	!, call(A).
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

sld1(Dot):-sld( student_of(_,peter), Dot).
sld2(Dot):-sld( brother_of(paul,_), Dot).


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

writes([]):-!,nl.
writes([H|T]):-!,writes(H),writes(T).
writes((A,B)):-!,writes(A),write(',\\n'),writes(B).	% break up conjunctions
writes(:-A):-!,write(':-'),writes(A).
writes(?-A):-!,write('?-'),writes(A).
writes('$empty_list'):-!,write([]).
writes(A):-write(A).	% catch-all


%%% Graphviz utilities %%%

gv_max_id(1000).	% max number of nodes in the graph

% open file and start new graph
gv_start(_, DotOut):- % gv_start(FileName, DotOut):-
  %(  write_to_file(true)
  %-> (tell(FileName),
			%writes(['digraph {']),
			%%writes(['graph [size="4,6"];']),
			%writes(['node [shape=plaintext, fontname=Courier, fontsize=12]'])
     %)
  %; true
  %),
	writes(['digraph {'], '', DotOut1),
	writes(['node [shape=plaintext, fontname=Courier, fontsize=12]'], DotOut1, DotOut).

% next graph
gv_next(DotIn, DotOut):-
  %(  write_to_file(true)
  %-> (writes(['}']),
			%writes(['digraph {']),
			%writes(['node [shape=plaintext, fontname=Courier, fontsize=12]'])
     %)
  %;  true
  %),
  writes(['}'], DotIn, DotOut1),
	writes(['digraph {'], DotOut1, DotOut2),
	writes(['node [shape=plaintext, fontname=Courier, fontsize=12]'], DotOut2, DotOut).

% finish graph and close file
gv_stop(DotIn, DotOut):-
  %(  write_to_file(true)
  %-> (writes(['}']),
			%told)
  %;  true
  %),
  writes(['}'], DotIn, DotOut).

% start new subgraph
gv_cluster_start(DotIn, DotOut):-
	( retract('$gv_cluster'(N)) -> N1 is N+1
	; otherwise -> N1=0
	),assert('$gv_cluster'(N1)),
  %(  write_to_file(true)
  %-> (writes(['subgraph cluster_',N1,' {']),
			%writes(['[style=filled, color=lightgrey];']),
			%writes(['node [style=filled,color=white];'])
     %)
  %;  true
  %),
  atom_number(N1Atom, N1),
  writes(['subgraph cluster_',N1Atom,' {'], DotIn, DotOut1),
	writes(['[style=filled, color=lightgrey];'], DotOut1, DotOut2),
	writes(['node [style=filled,color=white];'], DotOut2, DotOut).

% finish subgraph
gv_cluster_stop(DotIn, DotOut):-
  %(  write_to_file(true)
  %-> writes(['}'])
  %;  true
  %),
  writes(['}'], DotIn, DotOut).

% write the root of a tree and initialise node IDs
gv_root(L,N, DotIn, DotOut):-
  %(  write_to_file(true)
  %-> 	writes([N,' [label="',L,'"];'])
  %;  true
  %),
  atom_number(NAtom, N),
  writes([NAtom,' [label="',L,'"];'], DotIn, DotOut),
	gv_init_ids(N).

% add a node with label L and parent N0
gv_node(N0,L,N, DotIn, DotOut):-
	gv_id(N),
  %(  write_to_file(true)
  %-> writes([N,' [label="',L,'"];']),
		 %writes([N0,' -> ',N,';'])
  %;  true
  %),
  atom_number(NAtom, N),
  atom_number(N0Atom, N0),
  writes([NAtom,' [label="',L,'"];'], DotIn, DotOut1),
	writes([N0Atom,' -> ',NAtom,';'], DotOut1, DotOut).

% add a specially formatted leaf
gv_answer(N0,L, DotIn, DotOut):-
	gv_id(N),
  %(  write_to_file(true)
  %-> (writes([N,' [label="Answer:\\n',L,'", shape=ellipse, style=dotted, fontsize=10];']),
			%writes([N0,' -> ',N,' [style=dotted, arrowhead=none];'])
			%%writes(['{rank=same;',N0,';',N,';}']).
     %)
  %;  true
  %),
  atom_number(NAtom, N),
  atom_number(N0Atom, N0),
  writes([NAtom,' [label="Answer:\n',L,'", shape=ellipse, style=dotted, fontsize=10];'], DotIn, DotOut1),
	writes([N0Atom,' -> ',NAtom,' [style=dotted, arrowhead=none];'], DotOut1, DotOut).

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
append(X, Y, XY) :-
  atom_concat(X, '\n', Xp),
  atom_concat(Xp, Y, XY).
merge(In, Out) :-
  merge_(In, '', Out).
merge_([In|RIn], Acc, Out) :-
  atom_concat(Acc, In, AccUp),
  merge_(RIn, AccUp, Out).
merge_([], Out, Out).% :-
  %atom_concat(Acc, '\n', Out).

writes([], In, Out):-!,atom_concat(In, '\n', Out).
writes([H|T], In, Out):-!,writes(H, In, Out1),writes(T, Out1, Out).
writes((A,B), In, Out):-!,writes(A, In, Out1),atom_concat(Out1, ',\n', Out2),writes(B, Out2, Out).	% break up conjunctions
writes(:-A, In, Out):-!,atom_concat(In, ':-', Out1),writes(A, Out1, Out).
writes(?-A, In, Out):-!,atom_concat(In, '?-', Out1),writes(A, Out1, Out).
writes('$empty_list', In, Out):-!,atom_concat(In, '[]', Out).
writes(A, In, Out):-% catch-all
  format(atom(AAtom), "~w", [A]),
  atom_concat(In, AAtom, Out).
