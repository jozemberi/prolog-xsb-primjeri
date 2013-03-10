/*
Implementirajte gramatiku koja će prepoznavati izraze zbrajanja i oduzimanja dvaju brojeva te konstruirati odgovarajuće stablo sintakse.

| ?- izraz( X, [ 2.1, '-', 55 ], [] ).

X = izraz(prvi(2.1000),operator(-),drugi(55))
no
| ?- izraz( X, [ 17, '+', 22.43 ], [] ).

X = izraz(prvi(17),operator(+),drugi(22.4300));

no

*/

izraz(izraz(X,O,Y)) --> prvi(X), operator(O), drugi(Y).
prvi(prvi(X)) --> [X], {number(X)}.
drugi(drugi(X)) --> [X], {number(X)}.

operator(operator(-)) --> ['-'].
operator(operator(+)) -->['+'].

/*
Neka je zadana sljedeća baza činjenica:

osoba( ivek ).
osoba( joza ).
osoba( bara ).

Implementirajte gramatiku koja će prepoznavati i konstruirati parove osoba.

Primjer korištenja:

| ?- par( X, [ joza, i, bara ], [] ).

X = par(joza,i,bara);

no
| ?- par( X, Y, [] ).

X = par(ivek,i,ivek)
Y = [ivek,i,ivek];

X = par(ivek,i,joza)
Y = [ivek,i,joza];

X = par(ivek,i,bara)
Y = [ivek,i,bara];

X = par(joza,i,ivek)
Y = [joza,i,ivek];

X = par(joza,i,joza)
Y = [joza,i,joza];

X = par(joza,i,bara)
Y = [joza,i,bara];

X = par(bara,i,ivek)
Y = [bara,i,ivek];

X = par(bara,i,joza)
Y = [bara,i,joza];

X = par(bara,i,bara)
Y = [bara,i,bara];

no

*/

osoba( ivek ).
osoba( joza ).
osoba( bara ).

par(par(X,i,Y)) --> p(X), [i], p(Y).
p(X) --> [X],{osoba(X)}.

/*
Implementirajte gramatiku koja će prepoznavati jednostavne rečenice oblika:

subjekt predikat objekt.

Subjekti mogu biti:
- Ivek
- Stef
- Bara

Predikati mogu biti
- voli
- jede
- gleda

Objekti mogu biti
- sladoled
- kobasice

Primjer korištenja:
| ?- recenica( X, Y, [] ).

X = recenica(subjekt(Ivek),predikat(voli),objekt(sladoled))
Y = [Ivek,voli,sladoled];

X = recenica(subjekt(Ivek),predikat(voli),objekt(kobasice))
Y = [Ivek,voli,kobasice];

X = recenica(subjekt(Ivek),predikat(jede),objekt(sladoled))
Y = [Ivek,jede,sladoled];

X = recenica(subjekt(Ivek),predikat(jede),objekt(kobasice))
Y = [Ivek,jede,kobasice];

X = recenica(subjekt(Ivek),predikat(gleda),objekt(sladoled))
Y = [Ivek,gleda,sladoled];

X = recenica(subjekt(Ivek),predikat(gleda),objekt(kobasice))
Y = [Ivek,gleda,kobasice];

X = recenica(subjekt(Stef),predikat(voli),objekt(sladoled))
Y = [Stef,voli,sladoled];

X = recenica(subjekt(Stef),predikat(voli),objekt(kobasice))
Y = [Stef,voli,kobasice];

X = recenica(subjekt(Stef),predikat(jede),objekt(sladoled))
Y = [Stef,jede,sladoled];

X = recenica(subjekt(Stef),predikat(jede),objekt(kobasice))
Y = [Stef,jede,kobasice];

X = recenica(subjekt(Stef),predikat(gleda),objekt(sladoled))
Y = [Stef,gleda,sladoled];

X = recenica(subjekt(Stef),predikat(gleda),objekt(kobasice))
Y = [Stef,gleda,kobasice];

X = recenica(subjekt(Bara),predikat(voli),objekt(sladoled))
Y = [Bara,voli,sladoled];

X = recenica(subjekt(Bara),predikat(voli),objekt(kobasice))
Y = [Bara,voli,kobasice];

X = recenica(subjekt(Bara),predikat(jede),objekt(sladoled))
Y = [Bara,jede,sladoled];

X = recenica(subjekt(Bara),predikat(jede),objekt(kobasice))
Y = [Bara,jede,kobasice];

X = recenica(subjekt(Bara),predikat(gleda),objekt(sladoled))
Y = [Bara,gleda,sladoled];

X = recenica(subjekt(Bara),predikat(gleda),objekt(kobasice))
Y = [Bara,gleda,kobasice];

no
| ?-
*/

recenica(recenica(S,P,O)) --> subjekt(S), predikat(P), objekt(O).

subjekt(subjekt(X)) --> [X],{X='Ivek';X='Stef';X='Bara'}.
/* ili
subjekt(subjekt('Ivek')) --> ['Ivek'].
subjekt(subjekt('Stef')) --> ['Stef'].
subjekt(subjekt('Bara')) --> ['Bara'].
*/
predikat(predikat(voli)) --> [voli].
predikat(predikat(jede)) --> [jede].
predikat(predikat(gleda)) --> [gleda].

objekt(objekt(sladoled)) --> [sladoled].
objekt(objekt(kobasice)) --> [kobasice].

/*

Implementirajte gramatiku koja koja prepoznaje if-then-else izraze oblika:

if ( izraz )
then { naredbe }
[else { naredbe } ]

(else je opcionalan). Gramatika treba konstruirati stablo sintakse.

Primjeri korištenja:

| ?- selekcija( S, [ if, '(', a, <, b, ')', then, '{', a, +, +, '}' ], [] ).

S = selekcija(if(uvjet([a,<,b])),then(naredbe([a,+,+])));

no
| ?- selekcija( S, [ if, '(', a, ')', then, '{', 'a', -, -, '}', else, '{', a, +, +, '}' ], [] ).

S = selekcija(if(uvjet([a])),then(naredbe([a,-,-])),else(naredbe([a,+,+]))); 

no
| ?-
*/

selekcija(selekcija(if(U), then(N))) --> [if], ['('], uvjet(U), [')'], [then], ['{'], naredbe(N), ['}'].
selekcija(selekcija(if(U), then(N), else(E))) --> [if], ['('], uvjet(U), [')'], [then], ['{'], naredbe(N), ['}'],
    [else], ['{'], naredbe(E), ['}'].

uvjet(uvjet(U)) --> niz(U).
naredbe(naredbe(N)) --> niz(N).

niz([U]) --> [U].
niz([G|R]) --> [G], niz(R), {G \=='{'}.

/*

Implementirajte predikat listaj/2 koji će na temelju zadanog naziva direktorija u izlaznoj varijabli vratiti listu običnih 
(plain) datoteka koje se u njemu nalaze, te promijeniti trenutni direktorij u zadani.

Primjer korištenja:

| ?- listaj( '/home/mschatte/myPackages' , L ).

L = [apache-tomcat-6.0.18.tar.gz,exist.war,install_flash_player_10_linux.deb,openlaszlo.war];

no
| ?- shell( pwd ).
/home/mschatte/myPackages

yes
| ?-
*/

:- [shell].

listaj(N, L) :- cd(N), findall( _X, (list_directory( '.', _X), is_plain_file(_X)), L ).
/*
listaj(N, L) :- shell_to_list('pwd', T,_E), cd(N), findall( _X, (list_directory( '.', _X), 
    is_plain_file(_X)), L ), T = [[D]], cd(D).
*/


/*
Potrebno je implementirati predikat lista_datoteka/2 koji će vratiti sortiranu listu običnih (plain) datoteka u 
 zadanom direktoriju u sljedećem obliku:

[dat( datoteka_1 ), ..., dat( datoteka_n )]

Pri tome se smijete koristiti ugrađenim predikatima.

Primjer korištenja:

| ?- lista_datoteka( '.', L ).

L = [dat(k1.P),dat(k1.P~),dat(k1.xwam)];

no
*/

lista_datoteka(D,L) :- shell_to_list('pwd', T,_Err), cd(D), setof(dat(_X), (list_directory( '.', _X), is_plain_file(_X)), L ), 
  T = [[E]], cd(E).
  
/*
Implementirajte predikat kodiraj/2 koji će listu znakova kodirati klasičnom Cezarovom šifrom s pomakom 1 
(svakom kodu slova dodaje se broj 1). Primjer korištenja:


| ?- kodiraj( "kolokvij", X ).

X = lpmplwjk;

Pri implementaciji smiju se koristiti ugrađeni predikati.

 
*/

kodiraj([], "").

kodiraj([G|R], K) :- kodiraj(R,K1), X is G+1, char_code(C,X), atom_concat(C,K1,K). 

/*

Implementirajte predikat zamijeni( UlazniNiz, TrazeniNiz, Zamjena, Izlaz ) koji ce primit ulazni niz, 
pretraziti je li on sadrzi trazeni niz, te ukoliko ga sadrzi zamijeniti ga zamjenom i rezultat vratiti u varijabli izlaz.

Primjer korištenja:

| ?- zamijeni( 'ovo je proba', 'proba', 'uspjeh', X ).

X = ovo je uspjeh;

no
| ?- zamijeni( 'ovo ne prolazi', 'jer trazim svasta', 'nezamjenjivo', X ).

no
| ?- 
 
*/

zamijeni(UlazniNiz, TrazeniNiz, Zamjena, Izlaz ) :- str_sub( TrazeniNiz, UlazniNiz, Pozicija ), 
  str_length(TrazeniNiz, Duzina), Kraj is Pozicija + Duzina,
  string_substitute( UlazniNiz, [s( Pozicija, Kraj)], [Zamjena], Izlaz ).
  
  
  
