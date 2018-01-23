% http://www.swi-prolog.org/howto/http/HTTPFile.html
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).

:- initialization
      http_server(http_dispatch, [port(8080)]).

:- multifile user:file_search_path/2.
user:file_search_path(scholar_mirror, './scholar_mirror').
:- http_handler(root(.), serve_files_in_directory(scholar_mirror), [prefix]).
:- http_handler(/, http_reply_file(scholar_mirror('index.html'), []), []).
