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



proceseaza_termen_citit(IStream, OStream, res(File), C):-
    catch(
        read_file_as_string(File, Content), 
        Error, 
        (format(OStream, "Eroare la citirea fișierului: ~w\n", [Error]), Content = "")
    ),
    /*
    format(OStream, "Resolution for file: ~w\n", [File]),
    flush_output(OStream),*/
    reset_state,
    tokenize_formula(Content, Tokens),
    process_sentence(Tokens, List),
    res(List, OStream),
    flush_output(OStream),
    C1 is C + 1,
    proceseaza_text_primit(IStream, OStream, C1).

proceseaza_termen_citit(IStream, OStream, dp(File), C):-
    catch(
        read_file_as_string(File, Content), 
        Error, 
        (format(OStream, "Eroare la citirea fisierului: ~w\n", [Error]), Content = "")
    ),
    /*
    format(OStream, "Davis Putnam for file: ~w\n", [File]),
    flush_output(OStream),*/
    tokenize_formula(Content, Tokens),
    process_sentence(Tokens, List),
    run_dp_with_strategies(List, OStream),

    flush_output(OStream),
    C1 is C + 1,
    proceseaza_text_primit(IStream, OStream, C1).


proceseaza_termen_citit(IStream, OStream, X, _):-
				(X == end_of_file ; X == exit),
				close(IStream),
				close(OStream).
				
			
proceseaza_termen_citit(IStream, OStream, Altceva,C):-
				write(OStream,'nu inteleg ce vrei sa spui: '),write(OStream,Altceva),nl(OStream),
				flush_output(OStream),
				C1 is C+1,
				proceseaza_text_primit(IStream, OStream,C1).

%------

read_file_as_string(File, Content) :-
    open(File, read, Stream),           
    read_string(Stream, _, Content),    
    close(Stream).


tokenize_formula(Formula, Tokens) :-
    string_chars(Formula, Chars),     
    tokenize(Chars, Tokens), !. 


tokenize([], []).
tokenize([C|Rest], Tokens) :-
    (   member(C, ['(', ')', ',', '¬']) -> 
            atom_chars(Atom, [C]), Tokens = [Atom|T], tokenize(Rest, T)
    ;   char_type(C, space) -> tokenize(Rest, Tokens) 
    ;   char_type(C, alnum) -> extract_atom([C|Rest], Atom, R), Tokens = [Atom|T], tokenize(R, T)
    ;   tokenize(Rest, Tokens) 
    ).


extract_atom([], '', []).
extract_atom([C|Rest], Atom, Remaining) :-
    char_type(C, alnum),
    extract_atom(Rest, A, Remaining),
    atom_concat(C, A, Atom).
extract_atom(Rest, '', Rest).


%----------------------------------------

process_sentence(S,R) :- 
    phrase(translate(R, [], _), S), !. 


get_variable(Atom, Var, Context, NewContext) :-
    (   member(Atom-Var, Context) -> 
        NewContext = Context
    ;   Var = _,                     
        NewContext = [Atom-Var | Context] 
    ).

translate([A|B], Context, NewContext) --> disjunction(A, Context, MidContext), [and], translate(B, MidContext, NewContext).
translate([A], Context, NewContext) --> disjunction(A, Context, NewContext).

disjunction([Element|Rest], Context, NewContext) --> negation(Element, Context, MidContext), [or], disjunction(Rest, MidContext, NewContext).
disjunction([Element|Rest], Context, NewContext) --> compound_term(Element, Context, MidContext), [or], disjunction(Rest, MidContext, NewContext).
disjunction([Element|Rest], Context, NewContext) --> variable_or_atom(Element, Context, MidContext), [or], disjunction(Rest, MidContext, NewContext).
disjunction([Element], Context, NewContext) --> negation(Element, Context, NewContext).
disjunction([Element], Context, NewContext) --> compound_term(Element, Context, NewContext).
disjunction([Element], Context, NewContext) --> variable_or_atom(Element, Context, NewContext).
disjunction(A, Context, NewContext) --> ['('], disjunction(A, Context, NewContext), [')'].

negation(not(Term), Context, NewContext) --> [¬], term(Term, Context, NewContext).

term(Term, Context, NewContext) --> compound_term(Term, Context, NewContext).
term(Term, Context, NewContext) --> variable_or_atom(Term, Context, NewContext).

compound_term(Term, Context, NewContext) --> 
    [Functor, '('], arguments(Args, Context, NewContext), [')'], 
    { Term =.. [Functor | Args] }.

arguments([Arg], Context, NewContext) --> term(Arg, Context, NewContext).
arguments([Arg|Args], Context, NewContext) --> 
    term(Arg, Context, MidContext), [','], arguments(Args, MidContext, NewContext).

variable_or_atom(Var, Context, NewContext) --> 
    [Atom], 
    { atom(Atom), 
      atom_length(Atom, 1),
      char_type(Atom, upper),
      get_variable(Atom, Var, Context, NewContext) }.

variable_or_atom(Atom, Context, Context) --> 
    [Atom], 
    { atomic(Atom), 
      (
          \+ (atom_length(Atom, 1), char_type(Atom, upper)) ; 
          \+ (atom_string(Atom, String), string_length(String, 1))
      )
    }.

    
%----------------- ALGORTIM REZOLUTIE -------------------

opposite_literal(Lit1, Lit2) :-
    (Lit1 = not(A), Lit2 = A);
    (Lit2 = not(A), Lit1 = A).

strip_negation(not(Literal), Literal):- !.
strip_negation(Literal, Literal).


res(KB, Out) :-
    member([], KB), 
    format(Out, 'Knowledge base is UNSATISFIABLE.~n~n', []), % Transmite mesajul în fluxul Out
    !.

res(KB, Out) :-
    eliminate_clauses(KB, CleanedKB),
    (   member([], CleanedKB) 
    ->  format(Out, 'Knowledge base is UNSATISFIABLE.~n~n', []) % Transmite mesajul în fluxul Out
    ;   resolve_step(CleanedKB, NewKB),
        (   CleanedKB == NewKB
        ->  format(Out, 'Knowledge base is SATISFIABLE.~n~n', []) % Transmite mesajul în fluxul Out
        ;   res(NewKB, Out) % Continuă recursia
        )
    ).

:- dynamic generated/1.


resolve_step(KB, NewKB) :-
    retractall(generated(_)),
    findall(NewClause,
        (member(Clause1, KB),
         member(Clause2, KB),
         Clause1 \== Clause2,
         resolve_clauses(Clause1, Clause2, TempClause),
         sort(TempClause, NewClause), 
         \+ member(NewClause, KB), 
         \+ generated(NewClause),
         asserta(generated(NewClause))
         ),
        NewClauses),
    append(KB, NewClauses, NewKB).


resolve_clauses(Clause1, Clause2, NewClause) :-
    copy_term(Clause1, RenamedClause1),
    copy_term(Clause2, RenamedClause2),

    select(Lit1, RenamedClause1, Rest1),
    select(Lit2, RenamedClause2, Rest2),
    opposite_literal(Lit1, Lit2),
    
    strip_negation(Lit1, PositiveLit1),
    strip_negation(Lit2, PositiveLit2),

    unifiable(PositiveLit1, PositiveLit2, Substitution),
    
    \+ already_processed(Clause1, Clause2, Lit1),
    mark_as_processed(Clause1, Clause2, Lit1), 

    apply_substitution_to_clause(Substitution, Rest1, FinalRest1),
    apply_substitution_to_clause(Substitution, Rest2, FinalRest2),

    append(FinalRest1, FinalRest2, CombinedClause),
    list_to_set(CombinedClause, NewClause).


:- dynamic processed/3.

already_processed(C1, C2, L) :-
    processed(C1, C2, L);
    processed(C2, C1, L).

mark_as_processed(C1, C2, L) :-
    asserta(processed(C1, C2, L)),
    asserta(processed(C2, C1, L)).



apply_substitution_to_clause(Substitution, Clause, ResultClause) :-
    maplist(apply_substitution(Substitution), Clause, ResultClause).

apply_substitution(Substitution, Literal, SubstitutedLiteral) :-
    Literal =.. [Functor | Args], 
    maplist(substitute_arg(Substitution), Args, NewArgs), 
    SubstitutedLiteral =.. [Functor | NewArgs]. 


substitute_arg(Substitution, Arg, NewArg) :-
    (   find_substitution(Arg, Substitution, Value)
    ->  NewArg = Value
    ;   NewArg = Arg
    ).

find_substitution(Arg, [Arg1=Value | _], Value) :-
    Arg == Arg1, !.

find_substitution(Arg, [_ | Rest], Value) :-
    find_substitution(Arg, Rest, Value).


% ---------


eliminate_clauses(KB, CleanedKB) :-
    remove_pure_clauses(KB, KB1),
    remove_tautologies(KB1, KB2),
    remove_subsumed_clauses(KB2, CleanedKB).

%-

remove_pure_clauses(KB, FilteredKB) :-
    find_pure_literals(KB, PureLiterals),
    exclude(contains_pure_literal(PureLiterals), KB, FilteredKB).

find_pure_literals(KB, PureLiterals) :-
    findall(Literal, (member(Clause, KB), member(Literal, Clause)), Literals),
    list_to_set(Literals, UniqueLiterals),
    include(is_pure(Literals), UniqueLiterals, PureLiterals).

is_pure(Literals, Literal) :-
    \+ ground(Literal),
    \+ (opposite_literal(Literal, Opposite), member(Opposite, Literals)).

contains_pure_literal(PureLiterals, Clause) :-
    member(Literal, Clause),
    member(Literal, PureLiterals).

%-

remove_tautologies(KB, FilteredKB) :-
    exclude(is_tautology, KB, FilteredKB).

is_tautology(Clause) :-
    member(Literal, Clause),
    member(Opposite, Clause),
    ground(Literal),
    ground(Opposite),
    opposite_literal(Literal, Opposite).


%-

ground_clause(Clause) :-
    forall(member(Literal, Clause), ground(Literal)).

is_subsumed(KB, OtherClause) :-
    ground_clause(OtherClause),
    member(Clause, KB),
    Clause \== OtherClause, 
    ground_clause(Clause),
    forall(member(Literal, Clause), member(Literal, OtherClause)).


remove_subsumed_clauses(KB, FilteredKB) :-
    exclude(is_subsumed(KB), KB, FilteredKB).


%----------------- ALGORTIM DAVIS PUTNAM ----------------

dp(_, [], []):- !. 

dp(_, L, _) :-
    member([], L), !,
    fail.


dp(Strategy, L, [C/A|S]) :-
    select_atom(Strategy, L, C), 
    (
        (
            A = true, 
            apply_operation(L, C, L1), 
            dp(Strategy, L1, S), !
        ) ; 
        (
            A = false, 
            complement(C, NotC), 
            apply_operation(L, NotC, L2), 
            dp(Strategy, L2, S)
        )
    ).


apply_operation(Clauses, Literal, SortedClauses) :-
    complement(Literal, Complement),
    exclude(member(Literal), Clauses, SatisfyingRemoved),
    findall(
        ReducedClause,
        (
            member(Clause, SatisfyingRemoved),
            select(Complement, Clause, ReducedClause)
        ),
        ComplementReduced
    ),
    findall(
        Clause,
        (
            member(Clause, SatisfyingRemoved),
            \+ member(Complement, Clause)
        ),
        UnaffectedClauses
    ),
    append(ComplementReduced, UnaffectedClauses, ReducedClauses),
    sort(ReducedClauses, SortedClauses).


complement(not(Literal), Literal) :- !.
complement(Literal, not(Literal)).

sort_by_length(Lists, SortedLists) :-
    map_list_to_pairs(length, Lists, Pairs),
    keysort(Pairs, SortedPairs),            
    pairs_values(SortedPairs, SortedLists). 


select_atom(shortest_clause, Clauses, Atom) :-
    sort_by_length(Clauses, SortedClauses),
    SortedClauses = [ShortestClause|_],
    member(Atom, ShortestClause).


select_atom(most_frequent, Clauses, Atom) :-
    flatten(Clauses, Literals),
    maplist(normalize_literal, Literals, NormalizedLiterals),
    maplist(count_occurrences(NormalizedLiterals), NormalizedLiterals, Counts),
    list_to_set(Counts, UniqueCounts),
    max_member((_,Atom), UniqueCounts).


normalize_literal(not(Literal), Literal) :- !.
normalize_literal(Literal, Literal).

count_occurrences(Literals, Literal, (Count, Literal)) :-
    include(==(Literal), Literals, Matches),
    length(Matches, Count).


reset_state :-
    retractall(processed(_, _, _)),
    retractall(generated(_)).


format_solution([], _OStream). 
format_solution([C/A | S], OStream) :-
    format(OStream, "~w/~w; ", [C, A]), 
    format_solution(S, OStream).


run_dp_with_strategies(List, OStream) :-
    (   dp(shortest_clause, List, SolutionShortest)
    ->  format(OStream, "Strategy - shortest clause: YES\n{", []),
        format_solution(SolutionShortest, OStream),
        format(OStream, "}\n\n", [])
    ;   format(OStream, "Strategy - shortest clause: NOT\n\n", [])
    ),
    (   dp(most_frequent, List, SolutionMostFrequent)
    ->  format(OStream, "Strategy - most frequent atom: YES\n{", []),
        format_solution(SolutionMostFrequent, OStream),
        format(OStream, "}\n\n", [])
    ;   format(OStream, "Strategy - most frequent atom: NOT\n\n", [])
    ).
 
