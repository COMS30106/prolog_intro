
:- use_module(library(sgml)).              % webpage parsing
:- use_module(library('http/http_open')).  % web page retrieval
:- use_module(library('memfile')).



%%%%%%%%%%%%%%%%%%%%%%%%
%%% Graphviz utilities
%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic gv_id/1.

% open Graphviz file and write header
gv_start(FileName):-
    retractall(gv_id(_)),assert(gv_id(0)),
    tell(FileName),
    writes(['digraph publications {']),
    writes(['graph [overlap=scale];']),
    writes(['node [fontname="Arial"];']).

% returns the next available node identifier
gv_next_id(N) :-
    retract(gv_id(N0)),N is N0+1,assert(gv_id(N)).

% close file
gv_stop:-
    writes(['}']),
    told.

writes([]):-nl.
writes([H|T]):-write(H),writes(T).


%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fetching web pages
%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic html_cache/2.

% get web page relative to base URL
% requires base_url/1 to be defined
get_term_html(DocURL, Terms) :-
   base_url(BaseURL),
   concat_atom([BaseURL, DocURL], URL),
   get_term_html0(URL, Terms).

% get web page at full URL
get_term_html0(URL, Terms) :-
   LogFile = 'parselog.txt',
   fetch_url_to_terms(URL, LogFile, Terms, 'ok').

%   fetch_url_to_terms(+URL, +LogFile, -Terms, -Result)
%
%   Attempt fetch file from URL and parse to HTML terms.
%   If the http get fails, Terms will be empty and an error is
%   returned in Result. If successful, Result contains 'ok'.

fetch_url_to_terms(URL, _, Terms, _) :-
    html_cache(URL, Terms),!.
fetch_url_to_terms(URL, LogFile, Terms, Result) :-
    tell(LogFile),
    (   catch(
            http_open(URL, _,
            [
                timeout(20)
            ]),
            E,
            (message_to_string(E, S),write('404: '),write(URL),write('\nGET FAILURE: '),write(S),fail)
        )
     ->
        URLQ = URL
    ;
        % failed to fetch file
        %Terms = [],
        %Result = E
        base_url(BURL), atom_concat(BURL, '404.html', URLQ)
    ),
    % read http stream and parse it
    http_open(URLQ, InStream, [timeout(20)]),
    dtd(html, DTD),
    load_structure(stream(InStream), Terms,
    [ dtd(DTD),
      dialect(sgml),
      shorttag(false),
      max_errors(2000),
      call(error, on_error)
    ]),
    close(InStream),
    Result = 'ok',
    %
    told,
    assert(html_cache(URL, Terms)).

on_error(Severity, Message, Parser) :-
    get_sgml_parser(Parser, line(Line)),
    get_sgml_parser(Parser, charpos(CharPos)),
    get_sgml_parser(Parser, context(Context)),
    write(Severity),write(' at line '),write(Line),write(' ('),write(CharPos),write(') '),
    writeln(Message),
    writeln(Context).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parsing web pages
%%%%%%%%%%%%%%%%%%%%%%%%

% look for a specific tag in a list of HTML terms
html_tag_list(Tag,[HTML|Rest],Result,Rest) :-
   html_tag(Tag, HTML, Result).
html_tag_list(Tag,[element(_,_,Env)|_],Result,Rest) :-
   html_tag_list(Tag,Env,Result,Rest).
html_tag_list(Tag,[_|T],Result,Rest) :-
   html_tag_list(Tag,T,Result,Rest).

html_tag_list(Tag,HTML,Result) :-
   html_tag_list(Tag,HTML,Result,_).

% deals with SWI HTML format element(Tag,Attributes,Contents)
html_tag(title, element(title,_,[T]), T).
html_tag(ul, element(ul,_,C), C).
html_tag(li, element(li,_,C), C).
html_tag(a_href, element(a,Atts,_), URL) :-
   member((href=URL),Atts). % return only the URL of a link
html_tag(a, element(a,Atts,[C]), a(URL,C)) :-
   member((href=URL),Atts). % return both the URL and the text of a link
html_tag(span, element(span,_,C), C).

html_flatten([],Ys,Ys) :- !.
html_flatten([X|Xs],Ys0,Zs) :- !,
   html_flatten(Xs,Ys0,Ys1),
   html_flatten(X,Ys1,Zs).
html_flatten(element(_,_,Xs),Ys,Zs) :- !,
   html_flatten(Xs,Ys,Zs).
html_flatten(Xs,Ys,Zs) :-
   atom_concat(Xs,Ys,Zs).

