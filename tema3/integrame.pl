:- ensure_loaded('checker.pl').

% Ispas Alexandra-Petrina 322CDb
% alexandra.ispas@stud.acs.upb.ro

test_mode(detailed).

% Considerăm următoarele reprezentări:
%
% O integramă este reprezentată prin structura (compusul)
% integ(H, W, Lista, Vocab), unde:
% H este înălțimea integramei
% W este lățimea integramei
% Lista este o listă de tupluri (Poz, Valoare), unde
%   Poz este un tuplu (R, C) conținând rândul și coloana (0-based)
%   Valoare este una dintre:
%     x - dacă celula este neagră (nu poate fi completată cu litere)
%     o literă, dacă celula este completată cu o literă
%     o listă de întrebări, reprezentate ca tupluri (Text, Dir, ID), cu
%       Text - un literal, textul întrebării
%       Dir - una dintre valorile j sau d, indicând direcția întrebării
%       ID - un identificator numeric al întrebării
% Vocab este o listă de literali reprezentând cuvinte disponibile
% pentru a rezolva întrebarea.
%
% În ieșirea predicatului intrebări, o întrebare este reprezentată ca
% ((R, C), Text, Dir, ID), unde
% R este rândul căsuței cu întrebarea (0-based)
% C este coloana căsuței cu întrebarea (0-based)
% Text este textul întrebării (un literal)
% Dir este j sau d, reprezentând direcția în care trebuie să fie plasat
% răspunsul (jos sau dreapta)
% ID este un identificator numeric al întrebării.

% Puteți vizualiza integramele cu:
% integrama(0, W), print_integrama(W).
% integrama(1, W), print_integrama(W).
% integrama(2, W), print_integrama(W).
% integrama(3, W), print_integrama(W).
%
% Testați cu
% vmtest.
% Testați teste individuale (vedeți predicatul tt din checker.pl) cu
% vmtest(Test).
% de exemplu cu vmtest(intrebari).


% intrebari/2
% intrebari(integ(+H, +W, +Lista, +Vocab), -Lista_intrebari)
% Este adevărat atunci când Lista_intrebari este o lista de tupluri
% ((R, C), Text, Dir, ID), fiecare tuplu corespunzând unei întrebări din
% integramă (rândul, coloana, textul întrebării, direcția (j/d),
% identificatorul).
%
% BONUS: intrebari are o singură soluție (o singură listă) pentru o
% anumită integramă.
intrebari(integ(_, _, [], _), []).
intrebari(integ(_, _, [(_, [])|Res], _), Rez) :- intrebari(integ(_, _, Res, _), Rez).
intrebari(integ(_, _, [(Poz, [(Text, Dir, ID)|T] ) | Res], _), Rez) :- 
    append([(Poz,Text, Dir, ID)], X, Rez), 
    intrebari(integ(_, _, [(Poz, T)|Res], _), X), !.
intrebari(integ(_, _, [(_, _) | Res], _), Rez) :- intrebari(integ(_, _, Res, _), Rez).

% id_intrebare/2
% id_intrebare(+Integ, ?Intrebare, ?Q_ID)
% Este adevărat dacă în integrama reprezentată ca integ(...), Intrebare
% este un text iar Q_ID este un identificator care corespund aceleași
% întrebări.

% primeste intrebarea si ii intoarce ID-ul
getId(_, [], -1).
getId(Intrebare, [(_, Text, _, Id) | _], Id) :- Intrebare = Text.
getId(Intrebare, [(_, Text, _, _) | T], X) :- Intrebare \= Text, getId(Intrebare, T, X), !.

% primeste ID-ul si intoarce textul intrebarii
getIntrebare(_, [], '').
getIntrebare(Q_ID, [(_, Text, _, Id) | _], X) :- Q_ID = Id, X = Text.
getIntrebare(Q_ID, [(_, _, _, Id) | T], X) :- Q_ID \= Id, getIntrebare(Q_ID, T, X), !.

id_intrebare(Integrama, Intrebare, Q_ID) :- var(Intrebare), intrebari(Integrama, X), 
                                            getIntrebare(Q_ID, X, Intrebare).

id_intrebare(Integrama, Intrebare, Q_ID) :- var(Q_ID), intrebari(Integrama, X), 
                                            getId(Intrebare, X, Q_ID).

% completare/3
% completare(+Integ, +Sol, -Integrama)
% Predicatul produce Integrama, o structură de forma integ(...),
% pornind de la Integ, în care au fost completate celule conform cu
% soluția Sol.
% Soluția este reprezentată ca o listă de perechi (Întrebare, Răspuns),
% unde Întrebarea este textul unei întrebări, iar Răspuns este un cuvânt
% de completat; ambele sunt atomi (literali).
% De exemplu, o soluție parțială pentru integrama 0 poate fi:
% [('Din care plouă', 'NOR'), ('Al doilea număr', 'DOI')]
%
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), solutie(0, Sol), completare(W, Sol, W2),
%   print_integrama(W2).

removeDuplicates([], []).
removeDuplicates([H | Rest], NewRest) :- member(H, Rest), removeDuplicates(Rest, NewRest).
removeDuplicates([H | Rest], [H | NewRest]) :- \+member(H, Rest), removeDuplicates(Rest, NewRest).

% cauta o intrebare in lista de intrebari
getQ(Q, Intrebari, (Poz, Q, Dir, _)):- Intrebari = [(Poz, Q, Dir, _)| _].
getQ(Q, [_| Intrebari], X):- getQ(Q, Intrebari, X).

% a terminat de adaugat toate literele din cuvant
addWord(_, _, _, [], []).

%adauga litera intr-un cuvant scris la dreapta 
addWord(Lista, (R, C), Dir, [L | Litere], [((R, C1), L) | Result]):- Dir = d, C1 is C+1,
    addWord(Lista, (R, C1), Dir, Litere, Result).  

%adauga litera intr-un cuvant scris in jos
addWord(Lista, (R, C), Dir, [L | Litere], [((R1, C), L) | Result]):- Dir = j, R1 is R+1,
    addWord(Lista, (R1, C), Dir, Litere, Result).  

% construieste lista dintr o celula
getListaInteg(_, [], []).

getListaInteg(integ(H,W, Lista, Vocab), [(Q, A) | Rest], Result):- 
    atom_chars(A, Litere),
    append(Added, Next, Result),
    intrebari(integ(H,W, Lista, Vocab), Intrebari),
    getQ(Q, Intrebari, (Poz, Q, Dir, _)),
    addWord(Lista, Poz, Dir, Litere, Added),
    append(Added, Lista, Lista2),
    getListaInteg(integ(H,W, Lista2, Vocab), Rest, Next), !.

completare(integ(H, W, Lista, Vocab), Sol, integ(H, W, Lista2, Vocab)):- 
    getListaInteg(integ(H, W, Lista, Vocab), Sol, Added), append(Added, Lista, Lista1),
    removeDuplicates(Lista1, Lista2).

% lungime_spatiu/3
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), +Intrebare, -Lungime)
% pentru Bonus:
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), ?Intrebare, ?Lungime)
%
% Returnează lungimea spațiului asociat întrebării date.
% Întrebarea este indicată prin textul ei. De exemplu:
% lungime_spatiu pentru integrama 0 și întrebarea 'Al doilea număr'
% trebuie să lege Lungime la 3.
%
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), id_intrebare(W, Text, 3), lungime_spatiu(W, Text, X).

getPoz(_, [], []).
getPoz(Poz, Lista, Ceva):- Lista = [(Poz, Ceva)| _].
getPoz(Poz, [_| Lista], X):- getPoz(Poz, Lista, X), !.

% % a ajung la vreo marine a integramei
parcurgere(integ(_, W, _, _), _, (_, W), d, 0).
parcurgere(integ(H, _, _, _), _, (H, _), j, 0).

% e celula neagra pe poz aia sau intrebare
parcurgere(integ(_, _, Lista, _), _,(R, C), j, 0) :- R1 is R+1,
    getPoz((R1, C), Lista, x).  

parcurgere(_, Lista_intrebari, (R, C), j, 0) :- R1 is R+1,
    getQ(_, Lista_intrebari, ((R1, C), _, _)).

%daca nu e nimic acolo sau e o litera
parcurgere(Integrama, Qs, (R, C), j, L1) :- R1 is R+1, L1 = L + 1,
    parcurgere(Integrama, Qs, (R1, C), j, L), !. 
%--------------------------------------------------------------------
% e celula neagra pe poz aia sau intrebare
parcurgere(integ(_, _, Lista, _), _, (R, C), d, 0) :- C1 is C+1,
    getPoz((R, C1), Lista, X), X = x. 

parcurgere(_, Lista_intrebari, (R, C), d, 0) :- C1 is C+1,
    getQ(_, Lista_intrebari, ((R, C1), _, _)).

%daca nu e nimic acolo sau e o litera
parcurgere(Integrama, Qs, (R, C), d, L1) :- C1 is C+1, L1 = L + 1,
    parcurgere(Integrama, Qs,(R, C1), d, L), !. 

lungime_spatiu(Integrama, Intrebare, L) :- 
    intrebari(Integrama, Intrebari),
    getQ(Intrebare, Intrebari, (Poz, _, Dir, _)), 
    parcurgere(Integrama, Intrebari, Poz, Dir, L1), L is L1.

% intersectie/5
% intersectie(integ(+H, +W, +Lista, +Voc), +I1, -Poz1, +I2, -Poz2)
% pentru Bonus:
% intersectie(integ(+H, +W, +Lista, +Voc), ?I1, ?Poz1, ?I2, ?Poz2)
%
% Pentru o integramă și două întrebări date prin textul lor (I1 și I2),
% al căror răspunsuri se intersectează, întoarce în Poz1 indicele din
% răspunsul la I1 la care este intersecția, și în Poz2 indicele din
% răspunsul la I2 la care este intersecția. Indecșii incep de la 0.
%
% De exemplu, în integrama 0:
%  █       █       2↓      3↓      █
%  █       0↓,1→   -       -       █
%  4→      -       -       -       █
%  5→      -       -       -       █
%  █       █       █       █       █
%
%  Întrebările 'Primii 3 din artă' și 'Afirmativ' (3, respectiv 1) se
%  intersectează la pozițiile 0, respectiv 2 (va fi litera A, de la
%  ART, respectiv DA).

outputCheck(P1, P2, Poz1, Poz2):- (P1 >= 0, P2 >= 0) -> Poz1 is P1, Poz2 is P2.

checkPoz((R1, C1), j, (R2, C2), d, L1, L2):- R11 is R1 + L1, C22 is C2 + L2,
    R11 >= R2, C22 >= C1.

checkPoz((R1, C1), d, (R2, C2), j, L1, L2):- R22 is R2 + L2, 
    C11 is C1 + L1, R22 >= R1, C11 >= C2.

getPoint((R1, C1), j, (R2, C2), d, Poz1, Poz2):- 
    Poz1 is R2 - R1 - 1, 
    Poz2 is C1 - C2 - 1.

getPoint((R1, C1), d, (R2, C2), j, Poz1, Poz2):- 
    Poz1 is C2 - C1 - 1, 
    Poz2 is R1 - R2 - 1.

intersectie(Integrama, Q1, Poz1, Q2, Poz2) :- 
    intrebari(Integrama, Intrebari),
    getQ(Q1, Intrebari, (P1, _, Dir1, _)),
    getQ(Q2, Intrebari, (P2, _, Dir2, _)),
    lungime_spatiu(Integrama, Q1, L1), lungime_spatiu(Integrama, Q2, L2), 
    checkPoz(P1, Dir1, P2, Dir2, L1, L2),
    getPoint(P1, Dir1, P2, Dir2, Aux1, Aux2),
    outputCheck(Aux1, Aux2, Poz1, Poz2).

% solutii_posibile/2
% solutii_posibile(integ(+H, +W, +Lista, +Vocabular), -Solutii)
% Formează o listă Solutii, conținând perechi de forma
% (Întrebare, Cuvinte), unde
% Întrebare este textul unei întrebări din integramă, iar Cuvinte este o
% listă de cuvinte ce sunt din Vocabular și au lungimea corectă pentru a fi
% răspuns la întrebare. Solutii conține câte o pereche pentru fiecare
% întrebare din integramă.
% Cuvintele sunt reprezentate ca liste de atomi, fiecare atom
% având lungime 1 (o singură literă).
% De exemplu, pentru integrama 0, Solutii conține 6 perechi, două dintre
% ele fiind:
% ('Afirmativ', [['D', 'A'], ['N', 'U']])
% ('Din care plouă',
% [['N','O','R'],['A','R','T'],['U','I','T'],['D','O','I']])

getWord(_, [], []).
getWord(L, [R|Vocabular], [X|W]):- atom_chars(R, X), length(X, L1), L1 = L, 
    getWord(L, Vocabular, W).
getWord(L, [R|Vocabular], W):- atom_chars(R, X), length(X, L1), L1 \= L,
    getWord(L, Vocabular, W).

getSols(_, [], []).
getSols(integ(H, W, Lista, Vocabular), [(_, Q, _, _)|Intrebari], [(Q, A)|Raspuns]):-
    lungime_spatiu(integ(H, W, Lista, Vocabular), Q, L), 
    getWord(L, Vocabular, A), 
    getSols(integ(H, W, Lista, Vocabular), Intrebari, Raspuns).

solutii_posibile(integ(H, W, Lista, Vocabular), Sol) :-
    intrebari(integ(H, W, Lista, Vocabular), Intrebari), 
    getSols(integ(H, W, Lista, Vocabular), Intrebari, Sol).

% rezolvare/2
% rezolvare(+Integ, -Solutie)
% Rezolvare produce în Solutie soluția integramei Integ. Soluția este
% reprezentată ca o listă de perechi de literali, fiecare pereche
% conținând textul unei întrebări și cuvântul (ca literal) care este
% răspunsul la întrebare.
%
% BONUS: rezolvare nu oferă soluții duplicate - numărul de soluții ale 
% predicatului este chiar numărul de completări posibile ale integramei.

removeFromList(_, [], []).
removeFromList(Elem, [Elem|L], L).
removeFromList(Elem, [H|L], [H | X]):- removeFromList(Elem, L, X).
    
remove(_, [], _, []).
remove(Solutii, [(_, Q, _, _) | Intrebari], Vocabular, [(Q, R) | Rezultat]):-
    member((Q, R), Solutii), 
    member(R, Vocabular), 
    removeFromList(R, Vocabular, Vocabular2),
    remove(Solutii, Intrebari, Vocabular2, Rezultat).

check(Solutii, Intersectii, Solutii):-
    forall(member((Q1, A1), Solutii), 
        forall(member((Q1, P1, Q2, P2), Intersectii), 
            (member((Q2, A2), Solutii), atom_chars(A1, Cuv1), atom_chars(A2, Cuv2), 
              nth0(P1, Cuv1, Litera), nth0(P2, Cuv2, Litera)))).

rezolvareHelper([], _, _, []).
rezolvareHelper(Intrebari, Intersectii, Solutii, L):-
    findall((Q1, Z), (member((_, Q1, _, _), Intrebari),
        member((Q1, A1), Solutii),
        member(Elem1, A1),

        forall(member((Q1, P1, Q, P), Intersectii), 
                (member((Q, A), Solutii), 
                member(Elem, A),
                nth0(P, Elem, Litera), 
                nth0(P1, Elem1, Litera))),
                atom_chars(Z, Elem1)), L).

rezolvare(Integrama, Solutie):- Integrama = integ(_, _, _, Vocabular),
    solutii_posibile(Integrama, SolutiiPosibile),
    intrebari(Integrama, Intrebari),
    findall((Q1, P1, Q2, P2), intersectie(Integrama, Q1, P1, Q2, P2), Intersectii), 
    rezolvareHelper(Intrebari, Intersectii, SolutiiPosibile, AuxSolutie), 
    remove(AuxSolutie, Intrebari, Vocabular, Solutie1), 
    check(Solutie1, Intersectii, Solutie).
