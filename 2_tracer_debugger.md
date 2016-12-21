# SWI-Prolog tracer and debugger #
SWI-Prolog offers a native text-console based tracer and debugger.

## Text-console based tracer ##
You can trace Prolog's computations by giving the command **?-trace.** This will put Prolog in trace mode, showing every single resolution step (except for the predicates that have been compiled rather than consulted). Trace mode is switched off by the command **?-notrace.**

If you only want to debug small part of the code you can set *spy* and *trace* points and use **?-debug.** command (and **?-nodebug.** to disable debugging).

For more help see SWI-Prolog API available [here](http://www.swi-prolog.org/pldoc/man?section=debugger).

### Tracing ###
We will follow the computation trace of the query **?-student\_of(S,peter).**, depicted in Figure 3.1 on page 45 of the book. Load the file [**student\_of.pl**](student_of.pl?raw=true).

    ?- trace.

    Yes
    [trace]  ?- student_of(S,peter).
       Call: (7) student_of(_G311, peter) ? creep
       Call: (8) follows(_G311, _L210) ? creep
       Exit: (8) follows(paul, computer_science) ? creep
       Call: (8) teaches(peter, computer_science) ? creep
       Exit: (8) teaches(peter, computer_science) ? creep
       Exit: (7) student_of(paul, peter) ? creep

    S = paul ;

After each line of output we can give a command; **h** lists the possible options.

    +:                  spy        -:              no spy
    /c|e|r|f|u|a goal:  find       .:              repeat find
    a:                  abort      A:              alternatives
    b:                  break      c (ret, space): creep
    [depth] d:          depth      e:              exit
    f:                  fail       [ndepth] g:     goals (backtrace)
    h (?):              help       i:              ignore
    l:                  leap       L:              listing
    n:                  no debug   p:              print
    r:                  retry      s:              skip
    u:                  up         w:              write
    m:                    exception details
    C:                  toggle show context

For the moment we are just stepping (creeping) through the computation by hitting **RETURN**. The relation with SLD-trees is as follows. A **Call** means passing through a node in the SLD-tree in downward direction; only the first literal of the resolvent is shown. **Exit** means passing upward through a node. The number to the left indicates the depth of the node in the SLD-tree, but the SWI-Prolog **pl** shell starts counting at 7 instead of 1 (why SWI does this is left as an exercise for the reader). So, for instance, **teaches(peter,computer\_science)** is called at level 8 (i.e. level 2 in the SLD-tree).

We have found our first solution, and force backtracking by typing a semi-colon as usual. We thus backtrack to the most recent choice point.

       Redo: (8) follows(_G311, _L210) ? creep
       Exit: (8) follows(paul, expert_systems) ? creep
       Call: (8) teaches(peter, expert_systems) ? creep
       Fail: (8) teaches(peter, expert_systems) ? creep
       Redo: (8) follows(_G311, _L210) ? creep
       Exit: (8) follows(maria, ai_techniques) ? creep
       Call: (8) teaches(peter, ai_techniques) ? creep
       Exit: (8) teaches(peter, ai_techniques) ? creep
       Exit: (7) student_of(maria, peter) ? creep

    S = maria ;

**Redo** indicates the search for an alternative solution; notice that the literal following **Redo** is the most recently found answer rather than the query to which we seek an alternative solution (see the **Call** at the same level). The second solution for **follows(S,C)** leads to a failure branch, because we can't solve **teaches(peter,expert\_systems)**. We thus redo, after which we find our second solution. **teaches(peter,ai\_techniques)** is not the last **teaches** fact in the program. Forced backtracking however shows that all solutions have been exhausted.

       Redo: (8) teaches(peter, ai_techniques) ? creep
       Fail: (7) student_of(_G311, peter) ? creep

    No
