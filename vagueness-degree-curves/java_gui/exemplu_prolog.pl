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

proceseaza_termen_citit(IStream, OStream, (Cleanliness,Location), C) :-
    run_review(Cleanliness, Location, Review),
    format(OStream, 'Cleanliness: ~1f, Location: ~1f, Review: ~1f~n', [Cleanliness, Location, Review]),
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
read_kb(Rules):- 
    open('kb_ex2.txt', read, Stream),
    read_lines(Stream, Lines),
    close(Stream),
    parse_rules(Lines, Rules), !.
    
read_lines(Stream, Lines) :-
    read_line_to_string(Stream, Line),
    ( Line = end_of_file -> Lines = [] 
    ; read_lines(Stream, RestLines),
        string_lower(Line, NewString),
        split_string(NewString, " ", ",.", L ),
        maplist(atom_string, Atoms, L),
      Lines = [Atoms | RestLines]
    ).

parse_rules([], []). 
parse_rules([RuleLine | RestLines], [Clause | RestKB]) :-
    phrase(sentence(Clause), RuleLine),
    parse_rules(RestLines, RestKB). 

% -------

sentence([Connector, Antecedents, Consequents]) -->
    [if], condition_list(Connector, Antecedents), [then], consequences(Consequents), !.

connector(or) --> [or].
connector(and) --> [and].


condition_list(Connector, [First | Rest]) -->
    condition(First),
    connector(Connector),
    condition_list(Connector, Rest).

condition_list(_, [Last]) --> condition(Last).

condition(Var/Predicate) -->
    [Var, is, Predicate].

consequences([Var/Predicate]) -->
    [the, Var, is, Predicate].



% -- review
low(X, Y) :- X >= 1, X < 3, Y is 1 - (X / 3), !.
low(X, 0) :- X >= 3.


moderate(X, 0) :- X >= 1, X < 2, !.
moderate(X, Y) :- X >= 2, X =< 3, Y is X - 2, !.
moderate(X, Y) :- X > 3, X =< 4, Y is 4 - X, !.
moderate(X, 0) :- X > 4, X =< 5.


high(X, 0) :- X < 3, !.
high(X, Y) :- X >= 3, X =< 5, Y is (X - 3) / 2.

% --- cleanliness

poor(X, 1) :- X >= 0, X =< 3, !.
poor(X, Y) :- X > 3, X =< 5, Y is (5 - X)/2, !.
poor(X, 0) :- X > 5, X =< 10.

decent(X, 0) :- X =< 4, !.
decent(X, Y) :- X > 4, X =< 5, Y is X - 4, !.
decent(X, 1) :- X > 5, X =< 7, !.
decent(X, Y) :- X > 7, X =< 8, Y is 8 - X, !.
decent(X, 0) :- X > 8, X =< 10.

excellent(X, 0) :- X =< 7, !.
excellent(X, Y) :- X > 7, X =< 9, Y is (X - 7)/2, !.
excellent(X, 1) :- X > 9, X =< 10.

% --- location
bad(X, 1) :- X >= 0, X =< 4, !.
bad(X, Y) :- X > 4, X =< 6, Y is (6 - X)/2, !.
bad(X, 0) :- X > 6, X =< 10.


good(X, 0) :- X =< 5, !.
good(X, Y) :- X > 5, X =< 8, Y is (X - 5)/3, !.
good(X, 1) :- X > 8, X =< 10.


evaluate_predicates(FuzzyList, CleanScore, LocScore, ResultList) :-
    findall(
        Category/Predicate-Degree,
        (
            member(Category/Predicate, FuzzyList),
            get_degree(Category, Predicate, CleanScore, LocScore, Degree)
        ),
        ResultList
    ).


get_degree(cleanliness, Predicate, CleanScore, _, Degree) :-
    call(Predicate, CleanScore, Degree).

get_degree(location, Predicate, _, LocScore, Degree) :-
    call(Predicate, LocScore, Degree).


membership_degree(Category/Predicate, ResultList, Deg) :-
    member(Category/Predicate-Deg, ResultList).


combine_antecedent_degrees([], or, _, 0) :- !.
combine_antecedent_degrees([], and, _, 1) :- !.

combine_antecedent_degrees([CatPred|Rest], and, FuzzyDegrees, CombinedDeg) :-
    membership_degree(CatPred, FuzzyDegrees, DegFirst),
    combine_antecedent_degrees(Rest, and, FuzzyDegrees, DegRest),
    CombinedDeg is min(DegFirst, DegRest), !.

combine_antecedent_degrees([CatPred|Rest], or, FuzzyDegrees, CombinedDeg) :-
    membership_degree(CatPred, FuzzyDegrees, DegFirst),
    combine_antecedent_degrees(Rest, or, FuzzyDegrees, DegRest),
    CombinedDeg is max(DegFirst, DegRest).



evaluate_rule([Combiner, Antecedents, [ConclusionFuzzyPred]], FuzzyDegrees,
              ConclusionFuzzyPred, ConclusionDegree) :-

    combine_antecedent_degrees(Antecedents, Combiner, FuzzyDegrees, AntecedentDeg),
    ConclusionDegree is AntecedentDeg.


evaluate_all_rules(Rules, FuzzyDegrees, Results) :-
    findall(Conclusion-Deg,
        (
           member(Rule, Rules),
           evaluate_rule(Rule, FuzzyDegrees, Conclusion, Deg)
        ),
        Results
    ).



aggregator(X, DegLow, DegMod, DegHigh, MuAgg) :-
    low(X, LowVal),
    MLow is min(LowVal, DegLow),
    moderate(X, ModVal),
    MMod is min(ModVal, DegMod),
    high(X, HighVal),
    MHigh is min(HighVal, DegHigh),
    MuAgg is max(MLow, max(MMod, MHigh)).


aggregate_membership([], _, _, _, []).
aggregate_membership([X|Xs], DegLow, DegMod, DegHigh, [(X, MuAgg)|Rest]) :-
    aggregator(X, DegLow, DegMod, DegHigh, MuAgg),
    aggregate_membership(Xs, DegLow, DegMod, DegHigh, Rest).

defuzzify_discrete(AggPoints, CrispValue) :-
    sum_over_points(AggPoints, 0.0, 0.0, SumXMu, SumMu),
    ( SumMu =:= 0.0
      -> CrispValue = 0.0 
      ;  CrispValue is SumXMu / SumMu
    ).

sum_over_points([], SXM, SM, SXM, SM).
sum_over_points([(X, Mu)|Rest], SXM0, SM0, SXM, SM) :-
    SXM1 is SXM0 + X * Mu,
    SM1  is SM0 + Mu,
    sum_over_points(Rest, SXM1, SM1, SXM, SM).



run_review(CleanScore, LocScore, CrispReview) :-
    read_kb(Rules),
    findall(Predicate, member([_, Predicate, _], Rules), AllPred), flatten(AllPred, AllPredFlat),
    evaluate_predicates(AllPredFlat, CleanScore, LocScore, Scores),
    writeln(Scores),
    evaluate_all_rules(Rules, Scores,  [_/_-DL, _/_-DM, _/_-DH]),
    writeln(DL), writeln(DM), writeln(DH),
    aggregate_membership([1,2,3,4,5], DL, DM, DH, AggPoints),
    writeln(AggPoints),
    defuzzify_discrete(AggPoints, CrispReview).



