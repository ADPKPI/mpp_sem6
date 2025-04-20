% === Факти ===

% Вхідний числовий ряд (можна змінювати)
series([3.2, 7.8, 1.5, 9.0, 4.6]).
% Заданий алфавіт (можна змінювати за змістом і кількістю)
alphabet([a, b, c]).

% === Правила ===

% Знаходження мінімального елемента списку
my_min([X], X).
my_min([H|T], Min) :- my_min(T, Temp), Min is min(H, Temp).

% Знаходження максимального елемента списку
my_max([X], X).
my_max([H|T], Max) :- my_max(T, Temp), Max is max(H, Temp).

% Побудова рівномірних інтервалів [A, B)
build_intervals(Min, Max, N, Intervals) :-
    Step is (Max - Min) / N,
    build_intervals_helper(Min, Step, N, Intervals).

build_intervals_helper(_, _, 0, []) :- !.
build_intervals_helper(Min, Step, N, [[Min, Max1]|Rest]) :-
    Max1 is Min + Step,
    N1 is N - 1,
    build_intervals_helper(Max1, Step, N1, Rest).

% Пошук інтервалу, до якого належить значення
value_interval(Value, [[A,B]|_], 0) :-
    Value >= A, Value < B, !.
value_interval(Value, [_|T], Index) :-
    value_interval(Value, T, Temp),
    Index is Temp + 1.
value_interval(_, [], 0) :- !.  % fallback на праву межу

% Відображення значення у символ алфавіту
value_to_symbol(Value, Intervals, Alphabet, Symbol) :-
    value_interval(Value, Intervals, Index),
    length(Alphabet, L),
    (Index >= L -> LastIndex is L - 1 ; LastIndex is Index),
    nth0(LastIndex, Alphabet, Symbol).

% Перетворення всього числового ряду в лінгвістичний
map_series([], _, _, []).
map_series([H|T], Intervals, Alphabet, [S|Rest]) :-
    value_to_symbol(H, Intervals, Alphabet, S),
    map_series(T, Intervals, Alphabet, Rest).

% === Побудова матриці передування ===

% Формує список переходів (a->b, b->c, ...)
transitions([], []).
transitions([_], []).
transitions([A,B|T], [(A,B)|Rest]) :-
    transitions([B|T], Rest).

% Підрахунок кількості конкретного переходу в списку
count_transitions([], _, 0).
count_transitions([(A,B)|T], (A,B), N) :-
    count_transitions(T, (A,B), N1),
    N is N1 + 1.
count_transitions([(X,Y)|T], (A,B), N) :-
    (X \= A ; Y \= B),
    count_transitions(T, (A,B), N).

% Побудова одного рядка матриці передування
build_matrix_row(_, [], _, []).
build_matrix_row(From, [To|T], Transitions, [Count|Rest]) :-
    count_transitions(Transitions, (From, To), Count),
    build_matrix_row(From, T, Transitions, Rest).

% Побудова повної матриці передування
build_transition_matrix(_, [], _, []).
build_transition_matrix(Alphabet, [From|RestFrom], Transitions, [[From|Row]|MatrixRest]) :-
    build_matrix_row(From, Alphabet, Transitions, Row),
    build_transition_matrix(Alphabet, RestFrom, Transitions, MatrixRest).

% === Форматований вивід матриці ===

% Заголовки алфавіту з вирівнюванням
print_alphabet_header([]) :- nl.
print_alphabet_header([X|T]) :-
    format('~t~a~5|', [X]),
    print_alphabet_header(T).

% Вивід одного рядка матриці
print_matrix(_, []).
print_matrix(Alphabet, [[From|Row]|Rest]) :-
    format('~a: ', [From]),
    print_matrix_row(Row),
    nl,
    print_matrix(Alphabet, Rest).

% Вивід рядка з числами
print_matrix_row([]).
print_matrix_row([N|Rest]) :-
    format('~t~d~5|', [N]),
    print_matrix_row(Rest).

% === Головна функція запуску ===
run :-
    get_time(Start),
    series(Series),
    alphabet(Alphabet),
    my_min(Series, Min),
    my_max(Series, Max),
    length(Alphabet, N),
    build_intervals(Min, Max, N, Intervals),
    map_series(Series, Intervals, Alphabet, Linguistic),
    write('Лінгвістичний ряд: '), write(Linguistic), nl,
    transitions(Linguistic, Transitions),
    build_transition_matrix(Alphabet, Alphabet, Transitions, Matrix),
    nl, write('Матриця передування:'), nl,
    write('     '), print_alphabet_header(Alphabet),
    print_matrix(Alphabet, Matrix),
    get_time(End),
    Duration is (End - Start) * 1000,
    format('\nЧас виконання: ~2f мс\n', [Duration]).