student_of(S,T):-follows(S,C),teaches(T,C).

follows(paul,computer_science).
follows(paul,expert_systems).
follows(maria,ai_techniques).

teaches(adrian,expert_systems).
teaches(peter,ai_techniques).
teaches(peter,computer_science).

