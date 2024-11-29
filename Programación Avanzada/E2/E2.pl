%%% E2.pl %%%

%%%% VII

% Declaramos que los predicados pueden ser modificados en tiempo de ejecución.
:- dynamic hecho/1.
:- dynamic regla/2.

% Predicado para agregar un hecho
agregar_hecho(Hecho) :-
    assertz(Hecho),
    assertz(hecho(Hecho)).

% Predicado para eliminar un hecho
eliminar_hecho(Hecho) :-
    retract(Hecho),
    retract(hecho(Hecho)).

% Predicado para agregar una regla
agregar_regla(Cabeza, Cuerpo) :-
    assertz((Cabeza :- Cuerpo)),
    assertz(regla(Cabeza, Cuerpo)).

% Predicado para eliminar una regla
eliminar_regla(Cabeza, Cuerpo) :-
    retract((Cabeza :- Cuerpo)),
    retract(regla(Cabeza, Cuerpo)).

% Consultar los hechos y reglas actuales.
listar_hechos :-
    findall(H, hecho(H), Hechos),
    write('Hechos actuales: '), writeln(Hechos).

listar_reglas :-
    findall((C :- B), regla(C, B), Reglas),
    write('Reglas actuales: '), writeln(Reglas).

%%%% IX. 

prefijo([], _).
prefijo([CL|T1], [CL|T2]) :-
prefijo(T1, T2).

%%%% X. Corte

maximo(X,Y,X) :- X < Y.
maximo(X,Y,X) :- X >= Y.

max(X,Y,X) :- X < Y, !.
max(X,Y,X) :- X >= Y.

edad_categoria(Edad, joven) :-
Edad < 18, !.
edad_categoria(Edad, adulto) :-
Edad >= 18, Edad < 60, !.
edad_categoria(_, mayor).

% Main

main :- 

    % VII
    nl,writeln('VII. Hechos y reglas dinámicos'),nl,
    % Socrates es un hombre.
    agregar_hecho(hombre(socrates)),
    % Todos los hombres son mortales.
    agregar_regla(mortal(X), hombre(X)),
    % Consultar los hechos y reglas actuales.
    listar_hechos,
    listar_reglas,
    % Consultar si Socrates es mortal.
    (mortal(socrates) -> writeln('Socrates es mortal'); writeln('Socrates no es mortal')),
    % Eliminar el hecho de que Socrates es un hombre.
    writeln('Eliminando hecho de que Socrates es un hombre...'),
    eliminar_hecho(hombre(socrates)),
    % Consultar si Socrates sigue siendo mortal.
    (mortal(socrates) -> writeln('Socrates es mortal'); writeln('Socrates no es mortal')),
    % Eliminar la regla de mortalidad.
    writeln('Eliminando regla de mortalidad...'),
    eliminar_regla(mortal(X), hombre(X)),
    % Consultar los hechos y reglas actuales.
    listar_hechos,
    listar_reglas,

    % IX

    nl,writeln('IX. Prefijo'),nl,
    % Consultar si [1,2] es prefijo de [1,2,3,4].
    (prefijo([1,2], [1,2,3,4]) -> writeln('[1,2] es prefijo de [1,2,3,4]'); writeln('[1,2] no es prefijo de [1,2,3,4]')),
    % Consultar si [1,2] es prefijo de [1,3,4].
    (prefijo([1,2], [1,3,4]) -> writeln('[1,2] es prefijo de [1,3,4]'); writeln('[1,2] no es prefijo de [1,3,4]')),

    % X

    nl,writeln('X. Corte'),nl,
    % Consultar el máximo entre 5 y 10.
    maximo(5, 10, Maximo),
    write('El máximo entre 5 y 10 es: '), writeln(Maximo),
    % Consultar el máximo entre 10 y 5.
    maximo(10, 5, Maximo2),
    write('El máximo entre 10 y 5 es: '), writeln(Maximo2),

    % Consultar el máximo entre 5 y 10.
    max(5, 10, Maximo3),
    write('El máximo entre 5 y 10 es: '), writeln(Maximo3),
    % Consultar el máximo entre 10 y 5.
    max(10, 5, Maximo4),
    write('El máximo entre 10 y 5 es: '), writeln(Maximo4),

    % Consultar la categoría de edad de 15 años.
    edad_categoria(15, Categoria),
    write('La categoría de edad de 15 años es: '), writeln(Categoria),
    % Consultar la categoría de edad de 25 años.
    edad_categoria(25, Categoria2),
    write('La categoría de edad de 25 años es: '), writeln(Categoria2),
    % Consultar la categoría de edad de 65 años.
    edad_categoria(65, Categoria3),
    write('La categoría de edad de 65 años es: '), writeln(Categoria3),
    nl,

    halt.