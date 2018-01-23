:- consult(scholar).

% Base URL. HAS TO END WITH */* (URL concatenation).
base_url('http://localhost:8080/').

%author('Rafal Bogacz','m7abl3ty.html').	% for testing
author('Nello Cristianini','s7c6c-u4.html').
author('Peter Flach','pnkxxyy5.html').
%author('Majid Mirmehdi','4gnre9fn.html').	% for testing
author('Nigel Smart','el2pjv7x.html').


%%% Q1 %%%

% get details for a publication for a particular author
% get_publication_details(+Author, Key, Title, Authors, CitationCount, CitingPapersURL) <-
%     retrieve details of a publication by a given author using Google Scholar
get_publication_details(Author, Key, Title, Authors, CitationCount, CitingPapersURL) :-
   author(Author, Query),
   get_term_html(Query, HTML),
   get_publication_details1(HTML, Key, Title, Authors, CitationCount, CitingPapersURL).

% get details for a publication on a particular Google Scholar page
get_publication_details1(HTML, Key, Title, Authors, CitationCount, CitingPapersURL) :-
   html_tag_list(span,HTML,Tag1,HTML1),
   html_tag_list(a,Tag1,a(Key,Title),_), % use URL behind title as key
   html_tag_list(span,HTML1,Tag2,HTML2),
   get_authors(Tag2, Authors),
   html_tag_list(a,HTML2,a(CitingPapersURL,L),_),
   get_cites(L,CitationCount).

% get number of citations from given anchor if it starts with 'Cited by '
get_cites(L, CitationCount) :-
   atom_concat('Cited by ',C,L),
   atom_number(C,CitationCount).

% get authors by flattening HTML terms and return text until first dash
get_authors(HTML, Authors) :-
   html_flatten(HTML,'',Atom),
   atom_concat(Prefix,_,Atom),
   atom_concat(Authors,' - ',Prefix),!. % don't backtrack to second dash


%%% Q2 %%%

% author_stats(+A,C,H) <- author A has C citations in total and h-index H
author_stats(Author, CitationCount, H) :-
   bagof(C,K^T^A^U^get_publication_details(Author,K,T,A,C,U),Counts),
   counts2stats(Counts,CitationCount,H).

counts2stats(Counts, CitationCount, H) :-
   sumlist(Counts,0,CitationCount),
   msort(Counts,CountsS),reverse(CountsS,CountsR),
   counts2h(CountsR,0,H).

sumlist([],N,N).
sumlist([H|T],N0,N):-
   N1 is N0+H,
   sumlist(T,N1,N).

counts2h([], N, N).
counts2h([C|Counts], N, H) :-
   ( C > N -> N1 is N+1,counts2h(Counts,N1,H)
   ; otherwise -> H=N
   ).


%%% Q3 %%%

% self_citations(+Author, Citee, Citers) <-
%      Citers is a list of publications citing Citee; all publications are by Author
%      Citee and Citers are terms of the form
%         p(Author,Key,Title,Authors,CitationCount,CitingPapersURL)
self_citations(Author, Citee, Citers) :-
   bagof(p(Author,K,T,A,C,U),get_publication_details(Author,K,T,A,C,U),AuthorPapers),
   member(Citee,AuthorPapers),
   bagof(Citer, self_citation(Author,AuthorPapers,Citee,Citer), Citers).

% self_citation(+Author, +AuthorPapers, Citee, Citer) <-
%      AuthorPapers is a list of publications by Author; Citer and Citee are both on this list,
%      and Citer cites Citee
%      Citee and Citers are terms of the form
%         p(Author,Key,Title,Authors,CitationCount,CitingPapersURL)
self_citation(Author, AuthorPapers, Citee, Citer) :-
   Citee = p(Author,K,T,A,C,U),
   member(Citee,AuthorPapers),
   get_term_html(U,CitersHTML),
   %get_publication_details1(CitersHTML,CK,CT,CA,CC,_),	% ignore CitingPapersURL
   get_publication_details1(CitersHTML,CK,CT,_,_,_),	% ignore everything except Key & Title
   Citer = p(Author,CK,CT,CA,CC,CU),
   member(Citer,AuthorPapers).

% author_stats_sc(+A,C,H) <- author A has C citations in total and h-index H
%                            (corrected for self-citattions)
author_stats_sc(Author, CitationCount, H) :-
   bagof(C,K^T^A^U^get_publication_details_sc(Author,K,T,A,C,U),Counts),
   counts2stats(Counts,CitationCount,H).

% get_publication_details_sc(+Author, Key, Title, Authors, CitationCount, CitingPapersURL) <-
%     like get_publication_details/6, but CitationCount is corrected for self-citations
get_publication_details_sc(Author, K, T, A, C, U) :-
   get_publication_details(Author, K, T, A, C0, U),
   ( self_citations(Author,p(Author,K,T,A,C0,U),SC) -> length(SC,C1),C is C0-C1
   ; otherwise -> C=C0
   ).


%%% Q4 %%%

% self_citation_graph(+Author) <- generate Graphviz self-citation graph
self_citation_graph(Author) :-
    authorname(Author,_,Last),
    atom_concat(Last,'.dot',Filename),
    gv_start(Filename),
    init_id(Author),
    self_citations(Author, p(Author,KCitee,_,_,_,_), Citers),
    member(p(Author,KCiter,_,_,_,_),Citers),
    gv_directed_edge(Author, KCiter, KCitee),
    fail.   % failure-driven loop to get all citers
self_citation_graph(_):-
    gv_stop.

self_citation_graph :-
    author(Author,_),
    writes([Author,'...']),
    self_citation_graph(Author),
    fail.
self_citation_graph.

gv_directed_edge(Author, KFrom,KTo) :-
    key2id(Author, KFrom,From),
    key2id(Author, KTo,To),
    writes(['"', From, '" -> "', To, '";']).

authorname(Author, First, Last) :-
   atom_concat(First,Rest,Author),
   atom_concat(' ',Last,Rest).

:- dynamic publicationID/3.

init_id(Author) :-
   retractall(publicationID(Author,_,_)).

key2id(Author,Key,ID) :-
   ( publicationID(Author,Key,ID) -> true
   ; otherwise -> gv_next_id(ID),assert(publicationID(Author,Key,ID))
   ).
