%# GNU Prolog
%# ---------- Common helper rules -------------------------------------------
%# Check length
check_length(N, T, Top, Bottom, Left, Right) :-
    length(T, N),
    length(Top, N),
    length(Bottom, N),
    length(Left, N),
    length(Right, N).

%# Checking process
check_counts(T, T_trans, Top, Bottom, Left, Right) :-
    check_tower(T, Left),
    maplist(reverse, T, T_reverse),
    check_tower(T_reverse, Right),
    check_tower(T_trans, Top),
    maplist(reverse, T_trans, T_trans_reverse),
    check_tower(T_trans_reverse, Bottom).

check_tower([], []).
check_tower([Row|Rest_rows], [Count|Rest_counts]) :-
    increasing(Row, Count, 0, 0),
    check_tower(Rest_rows, Rest_counts).

%# idea from piazza
increasing([], Count, _, C) :-
    C = Count.

increasing([Hd|Tl], Count, Previous, C) :-
    (Hd > Previous) -> (New_count is C + 1),
    increasing(Tl, Count, Hd, New_count);
    increasing(Tl, Count, Previous, C).


%# transpose matrix implementation, got from stackflow
transpose([[]|_], []).
transpose(Matrix, [Row|Rows]) :-
    transpose_1st_col(Matrix, Row, RestMatrix),
    transpose(RestMatrix, Rows).

transpose_1st_col([], [], []).
transpose_1st_col([[H|T]|Rows], [H|Hs], [T|Ts]) :-
    transpose_1st_col(Rows, Hs, Ts).

%# ---------- Part1 tower() -----------------------------------------------
%# Use FD solver
tower(N, T, C) :-
    C = counts(Top, Bottom, Left, Right),
    check_length(N, T, Top, Bottom, Left, Right),
    unique_list_fd(T, N),
    transpose(T, T_trans),
    unique_list_fd(T_trans, N),
    maplist(fd_labeling, T),
    check_counts(T, T_trans, Top, Bottom, Left, Right).

%# Recursion rules to check each row, use fd domain solver
%# provided from discussion, generate a list of length N where each element is a unique integer between 1..N
unique_list_fd([], _).
unique_list_fd([Row|Rest], N) :-
    length(Row, N),
    fd_domain(Row, 1, N),
    fd_all_different(Row),
    unique_list_fd(Rest, N).


%# --------  Part2 plain_tower() Not fd domain solver --------------------
%# Cannot use FD solver
plain_tower(N, T, C) :-
    C = counts(Top, Bottom, Left, Right),
    check_length(N, T, Top, Bottom, Left, Right),
    maplist(unique_list_notfd(N), T),
    transpose(T, T_tr),
    maplist(unique_list_notfd(N), T_tr),

    check_counts(T, T_tr, Top, Bottom, Left, Right).

%# provided from discussion
unique_list_notfd(_, []).
unique_list_notfd(N, Row) :-
    length(Row, N),
    elements_between(Row, 1, N),
    all_unique(Row).

all_unique([]).
all_unique([H|T]) :- member(H,T), !, fail.
all_unique([_|T]) :- all_unique(T).

elements_between([],_,_).
elements_between([H|T], Min, Max) :-
    %# between(Bottom, Top, Int) is true if and only if Bottom =< Int =< Top
    between(Min, Max, H),
    elements_between(T, Min, Max).


%# ------Part3. Compare performance--------------------------
%# statstics(?atom, ?list): display statistics about memory usage and run times.
measure_tower(Time) :-
    statistics(cpu_time, [SinceStart|_]),
    tower(4, _ ,counts([1,2,2,4],[4,2,2,1],[1,2,2,4],[4,2,2,1])),
    statistics(cpu_time, [SinceLast|_]),
    Time is SinceLast - SinceStart + 1.
    
measure_plain(Time) :-
    statistics(cpu_time, [SinceStart|_]),
    plain_tower(4, _ ,counts([1,2,2,4],[4,2,2,1],[1,2,2,4],[4,2,2,1])),
    statistics(cpu_time, [SinceLast|_]),
    Time is SinceLast - SinceStart.


speedup(X) :-
    measure_tower(T),
    measure_plain(PT),
    X is PT / T.

%# ------Part4. Find ambiguous rules, ------------------------
%#  i.e. Numbers on the side can be caused by multiple tower arrangements
ambiguous(N, C, T1, T2) :-
    tower(N, T1, C),
    tower(N, T2, C),
    T1 \= T2.


