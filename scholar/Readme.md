# Querying Google Scholar with Prolog #
1. Run the Google Scholar mirror: `swipl server.pl`,
1. Open the example answers file: `swipl answers.pl`
1. Query the mirror.

Example queries:
``` Prolog
get_publication_details('Peter Flach',K,T,As,C,U).
self_citations('Peter Flach',Citee,Citers).
author_stats('Peter Flach',C,H).
author_stats_sc('Peter Flach',C,H).
self_citation_graph('Peter Flach').
```
Available authors:
* Peter Flach
* Rafal Bogacz
* Nello Cristianini
* Majid Mirmehdi
* Nigel Smart
