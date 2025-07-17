:- module('ex4',
        [author/2,
         genre/2,
         book/4
        ]).

/*
 * **********************************************
 * Printing result depth
 *
 * You can enlarge it, if needed.
 * **********************************************
 */




author(a, asimov).
author(h, herbert).
author(m, morris).
author(t, tolkien).

genre(s, science).
genre(l, literature).
genre(sf, science_fiction).
genre(f, fantasy).

book(inside_the_atom, a, s, s(s(s(s(s(zero)))))).
book(asimov_guide_to_shakespeare, a, l, s(s(s(s(zero))))).
book(i_robot, a, sf, s(s(s(zero)))).
book(dune, h, sf, s(s(s(s(s(zero)))))).
book(the_well_at_the_worlds_end, m, f, s(s(s(s(zero))))).
book(the_hobbit, t, f, s(s(s(zero)))).
book(the_lord_of_the_rings, t, f, s(s(s(s(s(s(zero))))))).

% You can add more facts.


% Signature: max_list(Lst, Max)/2
% Purpose: true if Max is the maximum church number in Lst, false if Lst is emoty.
max_list([], false).
max_list([X], X).
max_list([H|T], Max) :-
    max_list(T, TmpMax),
    max_church(H, TmpMax, Max).

% Signature: max_church(X, Y, Max)/3
% Purpose: True if Max is the greater of the two Church numerals X and Y.
max_church(X, false, X).
max_church(false, Y, Y).
max_church(zero, Y, Y).
max_church(X, zero, X).
max_church(s(X), s(Y), s(Z)) :-
    max_church(X, Y, Z).



% Signature: author_of_genre(GenreName, AuthorName)/2
% Purpose: true if an author by the name AuthorName has written a book belonging to the genre named GenreName.
author_of_genre(GenreName, AuthorName) :-
    genre(GenreId, GenreName),
    book(_, AuthorId, GenreId, _),
    author(AuthorId, AuthorName).



% Signature: longest_book(AuthorName, BookName)/2
% Purpose: true if the longest book that an author by the name AuthorName has written is titled BookName.
longest_book(AuthorName, BookName) :-
    author(AuthorId, AuthorName),
    findall((Length, BookName0),
            book(BookName0, AuthorId, _, Length),
            Books),
    max_pair_list(Books, (_, BookName)).

% Signature: max_pair_list(Pairs, MaxPair)/2
% Purpose: True if MaxPair is the (Length, BookName) pair in Pairs with the greatest Length (represented as a Church numeral).
max_pair_list([X], X).
max_pair_list([(L1,B1)|T], (MaxL, MaxB)) :-
    max_pair_list(T, (L2,B2)),
    max_church(L1, L2, MaxL),
    (MaxL = L1 -> MaxB = B1 ; MaxB = B2).
