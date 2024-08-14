
% Aquí va el código.
%Parte 1:

casa(gryffindor).
casa(slytherin).
casa(ravenclaw).
casa(hufflepuff).

%Mago, Sangre , Caracteristica.
mago(harry,mestiza,[coraje,amistoso,orgullo,inteligencia]).
mago(draco,pura,[inteligencia,orgullo]).
mago(hermione,impura,[inteligencia,orgullo,responsabilidad]).

permiteEntar(Casa,_):-
    casa(Casa),
    Casa \= slytherin.
permiteEntar(slytherin,Mago):-
    mago(Mago,TipoSangre,_),
    TipoSangre \= impura.

tieneCaracteristica(Mago,Caracteristica):-
    mago(Mago,_,Caracteristicas),
    member(Caracteristica,Caracteristicas).

tieneCaracterApropiado(Mago,Casa):-
    mago(Mago,_,_),
    casa(Casa),
    forall(caracteristicaCasa(Casa,Caracteristica),tieneCaracteristica(Mago,Caracteristica)).

caracteristicaCasa(gryffindor,coraje).
caracteristicaCasa(slytherin,orgullo).
caracteristicaCasa(slytherin,inteligencia).
caracteristicaCasa(ravenclaw,inteligencia).
caracteristicaCasa(ravenclaw,responsabilidad).
caracteristicaCasa(hufflepuff,amistoso).

%Parte 3:

odiariaEntrarCasa(harry,slytherin).
odiariaEntrarCasa(draco,hufflepuff).

puedeQuedarSeleccionado(Mago,Casa):-
    mago(Mago,_,_), casa(Casa),
    tieneCaracterApropiado(Mago,Casa),
    permiteEntar(Casa,Mago),
    not(odiariaEntrarCasa(Mago,Casa)).

puedeQuedarSeleccionado(hermione,gryffindor).

%Parte 4:

cadenaDeAmistades(Magos):-
    todosAmistosos(Magos),
    cadenaDeCasas(Magos).

todosAmistosos(Magos):-
    forall(member(Mago,Magos),tieneCaracteristica(Mago,amistoso)).

cadenaDeCasas([Mago1, Mago2|MagoSiguiente]):-
    puedeQuedarSeleccionado(Mago1,Casa),
    puedeQuedarSeleccionado(Mago2,Casa),    
    cadenaDeCasas([Mago2|MagoSiguiente]).
cadenaDeCasas([_]).
cadenaDeCasas([]).

%Parte 2:

hizo(harry, fueraDeCama).
hizo(hermione, fueA(tercerPiso)).
hizo(hermione, fueA(seccionRestringidaBiblioteca)).
hizo(harry, fueA(bosque)).
hizo(harry, fueA(tercerPiso)).
hizo(draco, fueA(mazmorras)).
hizo(ron, buenaAccion(50,ganarPartidaAjedrezMagico)).
hizo(hermione, buenaAccion(50,usoIntelectoParaSalvarAmigos)).
hizo(harry, buenaAccion(60,ganarAVoldemort)).


hizoAlgunaAccion(Mago):-
    hizo(Mago,_).
hizoAlgoMalo(Mago):-
    hizo(Mago,Accion),
    puntajeGenerado(Accion,Puntaje),
    Puntaje < 0.

lugarProhibido(bosque,50).
lugarProhibido(seccionRestringidaBiblioteca,10).
lugarProhibido(tercerPiso,75).

puntajeGenerado(fueraDeCama,-50).
puntajeGenerado(fueA(Lugar),PuntajeRestado):-
    lugarProhibido(Lugar,Puntos),
    PuntajeRestado is Puntos * -1.
puntajeGenerado(buenaAccion(Puntaje,_),Puntaje).


esBuenAlumno(Mago):-
    hizoAlgunaAccion(Mago),
    not(hizoAlgoMalo(Mago)).

accionRecurrente(Accion):-
    hizo(Mago,Accion),
    hizo(OtroMago,Accion),
    Mago \= OtroMago.

esDe(hermione,gryffindor).
esDe(ron,gryffindor).
esDe(harry,gryffindor).
esDe(draco,slytherin).
esDe(luna,ravenclaw).



puntajeTotalDeLaCasa(Casa,PuntajeTotal):-
    esDe(_,Casa),
    findall(Puntos,(esDe(Mago,Casa), puntosMago(Mago,_,Puntos)), ListaPuntos),
    sum_list(ListaPuntos, PuntajeTotal).
/*
puntajeTotalDeLaCasaV2(Casa,PuntajeTotal):-
    esDe(_,Casa),
    findall(Puntos,(esDe(Mago,Casa), hizo(Mago,Accion),puntajeGenerado(Accion,Puntos)), ListaPuntos),
    sum_list(ListaPuntos, PuntajeTotal).
*/
puntosMago(Mago,Accion,Puntos):-
    hizo(Mago,Accion),
    puntajeGenerado(Accion,Puntos).
    