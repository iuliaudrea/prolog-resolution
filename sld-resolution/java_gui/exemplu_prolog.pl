:-use_module(library(socket)).


inceput:-format('Salutare\n',[]),	flush_output,
				current_prolog_flag(argv, [PortSocket|_]), %preiau numarul portului, dat ca argument cu -a
				%portul este atom, nu constanta numerica, asa ca trebuie sa il convertim la numar
				write('port '),
				write(PortSocket),nl,
				atom_chars(PortSocket,LCifre),
				number_chars(Port,LCifre),%transforma lista de cifre in numarul din 
				tcp_socket(Socket),
				tcp_connect(Socket, localhost:Port),
				tcp_open_socket(Socket,IStream, OStream),
				proceseaza_text_primit(IStream, OStream,0).
							
				
proceseaza_text_primit(IStream, OStream,C):-
				read(IStream,CevaCitit),
				write('Am primit':CevaCitit),nl,
				proceseaza_termen_citit(IStream, OStream,CevaCitit,C).
				
proceseaza_termen_citit(IStream, OStream,salut,C):-
				write(OStream,'Salut, de la SWI!\n'),
				flush_output(OStream),
				C1 is C+1,
				proceseaza_text_primit(IStream, OStream,C1).
				
proceseaza_termen_citit(IStream, OStream,'ce mai faci?',C):-
				write(IStream, OStream,'ma plictisesc...\n'),
				flush_output(OStream),
				C1 is C+1,
				proceseaza_text_primit(IStream, OStream,C1).

proceseaza_termen_citit(IStream, OStream, (Student,Grade,CV), C) :-
    format(OStream, 'Answers: ~w, ~0f, ~1w~n', [Student, Grade, CV]),
    diagnose(Student,Grade,CV,OStream),
    flush_output(OStream),
    C1 is C + 1,
    proceseaza_text_primit(IStream, OStream, C1).
				
proceseaza_termen_citit(IStream, OStream, X + Y,C):-
				Rez is X+Y,
				write(OStream,'SWI spune':Rez),nl(OStream),
				flush_output(OStream),
				C1 is C+1,
				proceseaza_text_primit(IStream, OStream,C1).
				
oras(bucuresti, mare).
oras(constanta, mare).				
oras(pitesti, mediu).	
oras(buftea, mic).
proceseaza_termen_citit(IStream, OStream, oras(X),C):-
				oras(X,Tip),
				format(OStream,'~p este un oras ~p\n',[X,Tip]),
				flush_output(OStream),
				C1 is C+1,
				proceseaza_text_primit(IStream, OStream,C1).
				
proceseaza_termen_citit(IStream, OStream, X, _):-
				(X == end_of_file ; X == exit),
				close(IStream),
				close(OStream).
				
			
proceseaza_termen_citit(IStream, OStream, Altceva,C):-
				write(OStream,'nu inteleg ce vrei sa spui: '),write(OStream,Altceva),nl(OStream),
				flush_output(OStream),
				C1 is C+1,
				proceseaza_text_primit(IStream, OStream,C1).


%------------------------------------------------------
read_kb_and_goals(KB, Goals):- 
    open('kb_ex1.txt', read, Stream),
    read_lines(Stream, Lines),
    close(Stream),
    parse_lines(Lines, KB, Goals), !.
    
read_lines(Stream, Lines) :-
    read_line_to_string(Stream, Line),
    ( Line = end_of_file -> Lines = [] 
    ; read_lines(Stream, RestLines),
        string_lower(Line, NewString),
        split_string(NewString, " ", ",.", L ),
        maplist(atom_string, Atoms, L),
      Lines = [Atoms | RestLines]
    ).

parse_lines(Lines, KB, Goals) :-
    append(RuleLines, [QuestionLine], Lines),
    parse_rules(RuleLines, KB),  
    phrase(question(Goals), QuestionLine). 


parse_rules([], []). 
parse_rules([RuleLine | RestLines], [Clause | RestKB]) :-
    phrase(sentence(Clause), RuleLine),
    parse_rules(RestLines, RestKB). 

% -------

sentence(Clause) --> 
    [if, a, person], condition(Premises), [then, the, person], consequence(Conclusion),
    { append(Premises, [Conclusion], Clause) }, !.  


condition([n(Premise)]) --> consequence(Premise).

condition([n(Premise)|Rest]) --> consequence(Premise), [and], condition(Rest).


question(Goals) -->
    [the, person], goals(Goals), !.

goals([Goal|Rest]) --> consequence(Goal), [and] , goals(Rest), !.

goals([Goal]) --> consequence(Goal).

consequence(student) --> [is, a, student].
consequence(grade) --> [has, a, grade, greater, than, five].
consequence(passed_exam) --> [passes, the, exam].
consequence(diploma) --> [receives, a, diploma].
consequence(cv) --> [has, a, cv].
consequence(job) --> [can, apply, for, a, job].

% -------

validate_yes_no(yes).
validate_yes_no(no).

collect_facts(Student, Grade, CV, KB) :-
    validate_yes_no(Student),
    add_fact(student, Student, KB1),
    
    number(Grade),
    (Grade >= 5 -> KB2= [[grade]]; KB2 = [[n(grade)]]),

    validate_yes_no(CV),
    add_fact(cv, CV, KB3),
    
    append(KB1, KB2, KB12),
    append(KB12, KB3, KB).


add_fact(Literal, yes, [[Literal]]) :- !.
add_fact(Literal, no, [[n(Literal)]]).

% -------

backward_chain(KB, Goals) :-
    solve(KB, Goals).

solve(_, []) :- !.

solve(KB, [Goal | RestGoals]) :-
    satisfies(KB, Goal, Conditions),
    append(Conditions, RestGoals, NewGoals), !, 
    solve(KB, NewGoals).

satisfies(KB, Goal, Conditions) :-
    member(Clause, KB),
    select(Goal, Clause, NegatedLiterals),
    maplist(remove_negation, NegatedLiterals, Conditions).

remove_negation(n(P), P).


% ------

forward_chain(_, Solved, Goals) :-
	all_goals_solved(Goals, Solved),!.

forward_chain(KB, Solved, Goals) :-
    derive_fact(KB, Solved, NewFact, KBRest),
    forward_chain(KBRest, [NewFact | Solved], Goals), !.

all_goals_solved([], _).
all_goals_solved([Goal | Rest], Solved) :-
    member(Goal, Solved),
    all_goals_solved(Rest, Solved).

derive_fact(KB, Solved, NewFact, KBRest) :-
    select(Clause, KB, KBRest),
    satisfies_clause(Clause, Solved, NewFact).

satisfies_clause(Clause, Solved, NewFact) :-
    exclude(is_negated, Clause, [NewFact]),
    include(is_negated, Clause, Conditions),
    \+ member(NewFact, Solved),          
    all_true(Conditions, Solved).           

is_negated(n(_)).

all_true([], _). 
all_true([n(P) | Rest], Solved) :-
    member(P, Solved),
    all_true(Rest, Solved).


% ------


diagnose(Student, Grade, CV, OStream) :-
    read_kb_and_goals(Rules, Goals),
    collect_facts(Student, Grade, CV, Facts),
    append(Facts, Rules, KB),

     write(OStream, 'Forward chaining: '),
    (forward_chain(KB, [], Goals) ->
        write(OStream, 'The person can apply for a job.'), nl(OStream);
        write(OStream, 'The person cannot apply for a job.'), nl(OStream)),

    write(OStream, 'Backward chaining: '),
    (backward_chain(KB, Goals) ->
        write(OStream, 'The person can apply for a job.\n'), nl(OStream);
        write(OStream, 'The person cannot apply for a job.\n'), nl(OStream)).

