# Tests passés sur Codility pour "DE"

> ☝ Les énoncés ne sont pas ceux officiels mais une retranscription de mémoire.

## Task 2

> Optimiser le placement d'un vélo type [Vélib'](https://fr.wikipedia.org/wiki/V%C3%A9lib%27_M%C3%A9tropole) dans une station d'accueil.

- La station comporte 14 racks numérotés de -1 à 12.
- Un rack peut accueillir plusieurs vélos.

En entrée : liste des vélos déposés dans la station, chaque vélo étant identifié par son numéro de rack.

Objectif : déposer un nouveau vélo, entre deux racks déjà occupés (y compris ceux-ci), le plus loin possible d'eux.

En sortie : la distance maximale possible.

Exemples :

- `compute [5; 5]` : un seul rack occupé, le n°5; le vélo ne peut être déposé qu'au rack 5, soit une distance de 0.
- `compute [5; 7]` : le vélo peut être déposé entre les racks n°5 et n°7 où il existe un seul rack inoccupé, le n°6, soit une distance de 1.

## Task 3

> Trouver la plus longue combinaison de textes ne comportant pas de lettres en double.

En entrée : une liste de textes de longueurs quelconques, comportant les caractères a..z.

Exemples :

- `compute ["a"; "ab"; "b"]` → `"ab"`
- `compute ["a"; "ab"; "ba"; "ce"; "edc"]` → `"abedc"`
