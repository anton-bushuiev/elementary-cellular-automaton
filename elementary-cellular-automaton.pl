/**
 * main(-StartGen : list, -NumOfGens : integer, -Rule : integer).
 *
 * Runs an elementary cellular automaton.
 *
 * @param StartGen A starting state(generation).
 * @param NumOfGens A number of generations to run.
 * @param Rule A rule to determine a next generation based on the previous.
 *
 * Example:
 * ?- main([1, 0, 1, 0, 1], 10, 110).
 * 01 | O _ O _ O
 * 02 | O O O O O
 * 03 | O _ _ _ O
 * 04 | O _ _ O O
 * 05 | O _ O O O
 * 06 | O O O _ O
 * 07 | O _ O O O
 * 08 | O O O _ O
 * 09 | O _ O O O
 * 10 | O O O _ O
 * true
 */
main(StartGen, NumOfGens, Rule) :-
    % make a number of generations and rule global
    nb_setval(num_of_gens, NumOfGens),
    nb_setval(rule, Rule),

    live(1, StartGen).

/**
 * prettify(-Digit, +Char)
 *
 * Gives a "prettier" representation of a cell state.
 *
 * @param Digit A digit (0 or 1) representing a cell state.
 * @param Char A char to depict a state.
 */
prettify(Digit, Char) :-
    (Digit = 1, Char = 'O');
    (Digit = 0, Char = '_').

/*
 * draw_gen(-Gen : list)
 *
 * Draws a generation.
 *
 * @param Gen A generation to draw.
 */
draw_gen(Iter, Gen) :-
    maplist(prettify, Gen, Gen_pretty),
    atomic_list_concat(Gen_pretty, ' ', String),
    format('~|~`0t~d~2+ | ', [Iter]), write(String), nl.

/*
 * live(-Iter : integer, -Gen : list)
 *
 * Runs one iteration(Generates a next generation) and draws it.
 *
 * @param Iter number of iteration
 * @param Gen generation
 */
live(Iter, Gen) :-
    nb_getval(num_of_gens, NumOfGens), NumOfGens =:= Iter,
    draw_gen(Iter, Gen).

live(Iter, Gen) :-
    next_gen(Gen, Next_gen),

    draw_gen(Iter, Gen),

    Next_iter is Iter + 1,
    live(Next_iter, Next_gen).


/*
 * next_gen(?XL : list, ?YL : list)
 *
 * Defines relation between a generation and the first
 * cell of the next one.
 *
 * @param XL a currnet generation.
 * @param YL a next generation.
 *
 */
next_gen([], []).

next_gen([X1], [Y1]) :-
    next_cell(0, X1, 0, Y1).

next_gen([X1, X2 | XL], [Y1 | YL]) :-
    next_cell(0, X1, X2, Y1),
    next_gen_rec([X1, X2 | XL], YL).

/*
 * next_gen_rec(?XL : list, ?YL : list)
 *
 * Defines relation between a generation and the second or further
 * cell of the next one.
 *
 * @param XL a currnet generation.
 * @param YL a next generation.
 *
 */
next_gen_rec([X1, X2, X3 | XL], [Y2 | YL]) :-
    next_cell(X1, X2, X3, Y2),
    next_gen_rec([X2, X3 | XL], YL).

next_gen_rec([X1, X2], [Y2]) :-
    next_cell(X1, X2, 0, Y2).

/*
 * next_cell(-X1 : integer, -X2 : integer, -X3 : integer, +Y2 : integer)
 *
 * Determines a state of a cell for the next generation.
 *
 * @param X1 a left neighbour.
 * @param X2 current cell.
 * @param X3 a right neghbour.
 * @param Y2 a new state of a cell.
 *
 */
next_cell(X1, X2, X3, Y2) :-
    nb_getval(rule, Rule),
    (
		(X1 = 1, X2 = 1, X3 = 1, Y2 is (Rule >> 7) /\ 1);
		(X1 = 1, X2 = 1, X3 = 0, Y2 is (Rule >> 6) /\ 1);
		(X1 = 1, X2 = 0, X3 = 1, Y2 is (Rule >> 5) /\ 1);
		(X1 = 1, X2 = 0, X3 = 0, Y2 is (Rule >> 4) /\ 1);
		(X1 = 0, X2 = 1, X3 = 1, Y2 is (Rule >> 3) /\ 1);
		(X1 = 0, X2 = 1, X3 = 0, Y2 is (Rule >> 2) /\ 1);
		(X1 = 0, X2 = 0, X3 = 1, Y2 is (Rule >> 1) /\ 1);
		(X1 = 0, X2 = 0, X3 = 0, Y2 is (Rule >> 0) /\ 1)
    ).
