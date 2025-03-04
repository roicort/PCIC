% Definición de la gramática
oracion(S) --> fn(S1), fv(S2), {append(S1, S2, S)}. 

fn([N]) --> n(N). 
fn([PRO]) --> pro(PRO).
fn([DET, N]) --> det(DET), n(N).

det(el) --> [el].
det(la) --> [la].
det(los) --> [los].
det(las) --> [las].

n(gato) --> [gato].
n(perro) --> [perro].
n(niño) --> [niño].
n(niña) --> [niña].

pro(él) --> [él].
pro(ella) --> [ella].
pro(ellos) --> [ellos].
pro(ellas) --> [ellas].

fv([V, FN]) --> v(V), fn(FN).
fv([V]) --> v(V).

v(ama) --> [ama].
v(come) --> [come].
v(ve) --> [ve].


parse_sentence(Sentence) :-
    oracion(Tree, Sentence, []),
    write(Tree), nl.

main :-
    parse_sentence([el, gato, come]).