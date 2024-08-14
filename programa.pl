
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

