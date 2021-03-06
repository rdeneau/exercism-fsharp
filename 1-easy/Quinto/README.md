# Quinto (French)

Cinq chiffres initiaux pour obtenir un nombre à 2 chiffres
par addition `+`, soustraction `-`, multiplication `*` ou division `/`
de tous les chiffres et des nombres résultats intermédiaires :
- les 2 premiers chiffres ensemble
- puis le 1er résultat avec le 3e chiffre
- puis le 2e résultat avec le 4e chiffre
- enfin le 3e résultat avec le dernier chiffre

```txt
• 1, 2, 3, 4, 5 => 37
  • Solution 1 :
    3 * 5 = 15
      + 4 = 19
      * 2 = 38
      - 1 = 37

• 5, 6, 7, 8, 9 => 42
  • Solution 1 :
    7 - 6 = 1
      + 9 = 10
      * 5 = 50
      - 8 = 42
```

## Etapes

- A partir des 5 chiffres fournis, trouver tous les candidats i.e. toutes les combinaisons des 5 chiffres et de 4 opérateurs (parmi `+`, `-`, `*`, `/`)
  - Exemple avec 3 chiffres 1, 2, 3 : `1 + 2 + 3`, `1 + 2 - 3`, ... `2 + 1 + 3`, ...
- Trouver toutes les solutions pour les 2 exemples ci-dessous
- Les classer par ordre alphabétique
- Ignorer les solutions similaires telles que `1 + 2 * 3` et `2 + 1 * 3`
- Généraliser l'implémentation pour N chiffres
- Mode strict : ne pas accepter les solutions avec `+ 0`, `- 0`, `* 1`, `/ 1`
