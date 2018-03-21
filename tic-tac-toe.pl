/* some matrix standard functions, maybe use another file? */

/* concat, flat: some old implemented function i found on my notes */
concat([], L, L).
concat([H | T], L2, [H | L]) :-
	concat(T, L2, L).


flat([], []) :-
	!.
	
flat(A, [A]) :-
	atomic(A).

flat([H | T], R) :-
	flat(H, V),
	flat(T, T2),
	concat(V, T2, R).
	
/* returns the number of occurance of E inside of the vector */
vector_count([], _, 0).

vector_count([E | T], E, Count) :-
	vector_count(T, E, NewCount),
	Count is NewCount + 1,
	!.
	
vector_count([H | T], E, Count) :-
	H \= E,
	vector_count(T, E, Count).
	
/* returns the length of a vector. be careful, it's not tail-recursive */
vector_size([], 0).
vector_size([_ | T], S) :-
	vector_size(T, NS),
	S is NS + 1.

/* checks if two vectors are equal: vector_equals([1,2,3], [1,2,3]) */
vector_equals([], []) :-
	!.

vector_equals([H | VT], [H | WT]) :-
	vector_equals(VT, WT).
	
/* sets the i-th element of a prolog list: vector_set(list, position, new value, new list) */
vector_set([_ | T], 1, V, [V | T]) :-
	!.

vector_set([H | T], X, V, [H | NL]) :-
	Y is X - 1,
	vector_set(T, Y, V, NL).

/* ... */
vector_append([], X, [X]) :-
	!.
	
vector_append([H | T], X, [H | NT]) :-
	vector_append(T, X, NT).

/* ... */
vector_get([H | _], 1, H) :-
	!.
	
vector_get([_ | T], I, V) :-
	J is I - 1,
	vector_get(T, J, V).

/* ... */	
vector_contains([H | _], H).

vector_contains([_ | T], H) :-
	vector_contains(T, H).
	
/* returns the shape of the matrix: matrix_shape(InputMatrix, Rows, Columns) */
matrix_shape(M, X, Y) :-
	vector_size(M, X),
	X == 0,
	!,
	Y = 0.
	
matrix_shape(M, X, Y) :-
	vector_size(M, X),
	matrix_get_row(M, 1, R),
	vector_size(R, Y).
	
/* matrix_set_row(matrix, row index, new row, new matrix) */
matrix_set_row([_ | T], 1, R, [R | T]) :-
	!.
	
matrix_set_row([H | T], I, R, [H | M]) :-
	J is I - 1,
	matrix_set_row(T, J, R, M).
	
/* matrix_get_row(matrix, row index, row) */
matrix_get_row([R | _], 1, R) :-
	!.
	
matrix_get_row([_ | M], I, R) :-
	J is I - 1,
	matrix_get_row(M, J, R).

/* ... */	
matrix_get_column([], _, []).
	
matrix_get_column([R | M], I, [C | X]) :-
	vector_get(R, I, C),
	matrix_get_column(M, I, X).
	
matrix_diag(M, D) :-
	matrix_diag(M, D, 1).

matrix_diag([], [], _).

matrix_diag([R | M], [C | D], I) :-
		vector_get(R, I, C),
		J is I + 1,
		matrix_diag(M, D, J).
		
matrix_rdiag(M, D) :-
	matrix_shape(M, Rows, _),
	matrix_rdiag(M, D, Rows).
	
matrix_rdiag([], [], _).

matrix_rdiag([R | M], [C | D], I) :-
	vector_get(R, I, C),
	J is I - 1,
	matrix_rdiag(M, D, J).
	
matrix_transpose(M, M2) :-
	matrix_shape(M, _, Columns),
	matrix_transpose(M, M2, 1, Columns).
	
matrix_transpose([], [], _, _).

matrix_transpose(_, [], ColumnIndex, MaxIndex) :-
	ColumnIndex > MaxIndex,
	!.

matrix_transpose(M, [C | X], ColumnIndex, MaxIndex) :-
	ColumnIndex =< MaxIndex,
	!,
	NextIndex is ColumnIndex + 1,
	matrix_get_column(M, ColumnIndex, C),
	matrix_transpose(M, X, NextIndex, MaxIndex).

/* sets a single cell of a matrix: matrix_set(Matrix, i, j, value, Returned New Matrix) */
matrix_set(M, X, Y, V, NM) :-
	matrix_get_row(M, X, R),
	vector_set(R, Y, V, NR),
	matrix_set_row(M, X, NR, NM).
	
/* /some matrix standard functions */

/* statically defined board matrix */
board([
	[empty, empty, empty],
	[empty, empty, empty],
	[empty, empty, empty]
]).

/* statically defined winning vectors */
win(ai, [ai, ai, ai]).
win(human, [human, human, human]). 

/* pretty print symbols */
symbol(empty, '   ').
symbol(ai, ' O ').
symbol(human, ' X ').

board_pretty_print([]) :-
	writeln(' ').

board_pretty_print([R | M]) :-
	board_pretty_print_row(R),
	writeln(' '),
	board_pretty_print(M).
	
board_pretty_print_row([]).
	
board_pretty_print_row([C | R]) :-
	symbol(C, X),
	write(X),
	write(' '),
	board_pretty_print_row(R).

board_next_turn(ai, human).
board_next_turn(human, ai).

board_make_move(B, X, Y, Value, NB) :-
	matrix_set(B, X, Y, Value, NB).
 
/* game over if all cells are filled */
board_gameover(B) :-
	flat(B, X),
	not(vector_contains(X, empty)),
	!.

/* game over if the diag is a win */
board_gameover(B) :- 
	matrix_diag(B, D),
	board_gameover_condition(D),
	!.

/* could have done so, but i've tried to keep the board functions shape-indipendent, for what it's worth
board_gameover([ [C, _, _], [_, C, _], [_, _, C] ]) :-
	C \= empty.
	*/
	
board_gameover(B) :-
	matrix_rdiag(B, D2),
	board_gameover_condition(D2),
	!.	
	
/* game over if a row is a win */
board_gameover(B) :-
	board_gameover_rows(B),
	!.

/* game over if a column is a win */
board_gameover(B) :-
	matrix_transpose(B, M),
	board_gameover_rows(M),
	!.
	
board_gameover_rows([R | _]) :-
	board_gameover_condition(R).
	
board_gameover_rows([_ | B]) :-
	board_gameover_rows(B).
	
/* checks if a vector is a `winning` one */
board_gameover_condition(R) :-
	win(human, V),
	vector_equals(R, V).
	
board_gameover_condition(R) :-
	win(ai, V),
	vector_equals(R, V).

/* returns the score (heuristic) of the board */
board_score(B, Score) :-
	matrix_diag(B, D),
	matrix_rdiag(B, D2),
	board_rows_score(B, RowsScore),
	board_columns_score(B, ColumnsScore),
	board_row_score(D, DiagScore),
	board_row_score(D2, Diag2Score),
	Score is DiagScore + Diag2Score + RowsScore + ColumnsScore.
	
board_rows_score([], 0).
	
board_rows_score([R | M], Score) :-
	board_row_score(R, S),
	board_rows_score(M, SS),
	Score is SS + S.
	
board_columns_score(B, Score) :-
	matrix_transpose(B, M),
	board_rows_score(M, Score).

board_row_score(R, Score) :-
	vector_count(R, human, 0),
	vector_count(R, ai, ScoreAI),
	ScoreAI > 0,
	Exponent is ScoreAI - 1,
	pow(10, Exponent, Score),
	!.

board_row_score(R, Score) :-
	vector_count(R, ai, 0),
	vector_count(R, human, ScoreHuman),
	ScoreHuman > 0,
	Exponent is ScoreHuman - 1,
	pow(10, Exponent, S),
	Score is -S,
	!.

board_row_score(_, 0).

/* returns a list of possible moves */
board_free_cells(B, C) :-
	matrix_shape(B, Rows, Columns),
	board_free_cells(B, Rows, Columns, C).
	
board_free_cells(_, 0, _, []) :-
	!.

board_free_cells(Board, RowIndex, 0, Cells) :-
	matrix_shape(Board, _, Columns),
	NRowIndex is RowIndex - 1,
	board_free_cells(Board, NRowIndex, Columns, Cells).

board_free_cells(Board, RowIndex, ColumnIndex, [[RowIndex, ColumnIndex] | Rest]) :-
	matrix_get_row(Board, RowIndex, Row),
	vector_get(Row, ColumnIndex, empty),
	NColumnIndex is ColumnIndex - 1,
	board_free_cells(Board, RowIndex, NColumnIndex, Rest),
	!.
	
board_free_cells(Board, RowIndex, ColumnIndex, Cells) :-
	/* we know that cell != empty */
	NColumnIndex is ColumnIndex - 1,
	board_free_cells(Board, RowIndex, NColumnIndex, Cells),
	!.
	
generate_ai_decision_max([[X, Y, Score]], [X, Y, Score]).
	
generate_ai_decision_max([[_, _, Score] | Scores], Max) :-
	generate_ai_decision_max(Scores, [Xm, Ym, ScoreM]),
	ScoreM > Score,
	Max = [Xm, Ym, ScoreM],
	!.
	
generate_ai_decision_max([[X, Y, Score] | Scores], Max) :-
	generate_ai_decision_max(Scores, [_, _, _]),
	Max = [X, Y, Score].	
	
generate_ai_decision_min([[X, Y, Score]], [X, Y, Score]).
	
generate_ai_decision_min([[_, _, Score] | Scores], Min) :-
	generate_ai_decision_min(Scores, [Xm, Ym, ScoreM]),
	ScoreM < Score,
	Min = [Xm, Ym, ScoreM],
	!.
	
generate_ai_decision_min([[X, Y, Score] | Scores], Min) :-
	generate_ai_decision_min(Scores, [_, _, _]),
	Min = [X, Y, Score].

generate_ai_decision(_, B, 0, [0, 0, S]) :-
	board_score(B, S),
	!.

generate_ai_decision(_, B, _, [0, 0, S]) :-
	board_gameover(B),
	board_score(B, S),
	!.
	
generate_ai_decision(ai, B, Depth, Decision) :-
	board_free_cells(B, Cells),
	generate_ai_decision_recursion(ai, B, Depth, Cells, Scores),
	generate_ai_decision_max(Scores, Decision),
	!.

generate_ai_decision(human, B, Depth, Decision) :-
	board_free_cells(B, Cells),
	generate_ai_decision_recursion(human, B, Depth, Cells, Scores),
	generate_ai_decision_min(Scores, Decision).
	
generate_ai_decision_recursion(_, _, _, [], []).
	
generate_ai_decision_recursion(Turn, B, Depth, [[X, Y] | T], [[X, Y, Decision] | Scores]) :-
	D is Depth - 1,
	board_make_move(B, X, Y, Turn, NB),
	board_next_turn(Turn, NextTurn),
	generate_ai_decision(NextTurn, NB, D, [_, _, Decision]),
	generate_ai_decision_recursion(Turn, B, Depth, T, Scores).

/* main loop */
main(B, _, _) :-
	board_gameover(B),
	board_pretty_print(B),
	board_score(B, S),
	writeln(S),
	!.

main(B, human, Diff) :-
	board_pretty_print(B),
	writeln('Insert your move ([X, Y].): '),
	read(Move),
	[X, Y] = Move,
	board_make_move(B, X, Y, human, NB),
	main(NB, ai, Diff).
	
main(B, ai, Diff) :-
	board_pretty_print(B),
	writeln('Ai is `thinking`...'),
	generate_ai_decision(ai, B, Diff, [X, Y, _]),
	board_make_move(B, X, Y, ai, NB),
	main(NB, human, Diff).
	
main :-
	/* ai starts and always put its mark in the middle of the board */
	board(B),
	board_make_move(B, 2, 2, ai, NB),
	Diff is 2,
	main(NB, human, Diff).
