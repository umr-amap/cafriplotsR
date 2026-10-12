---
title: "Formation CafriplotsR — samedi 26 septembre 2026"
subtitle: "Programme de la journée et informations pratiques"
lang: fr
toc: false
format:
  html:
    embed-resources: true
---

## L'essentiel

| | |
|---|---|
| **Date** | Samedi 26 septembre 2026 |
| **Début** | **09h00, heure de Douala** (10h00 en France) — connexion ouverte dès 08h40 |
| **Matin** | 09h00 – 12h30 |
| **Après-midi** | 14h00 – 16h30, pour ceux qui sont là |
| **Lieu** | En salle à Douala ; l'animation se fait à distance |
| **Lien Zoom** | <https://umontpellier-fr.zoom.us/j/97678683045> |
| **Contact** | Gilles Dauby — gilles.dauby@ird.fr |

Le même lien sert le matin et l'après-midi.

---

## Deux choses à préparer d'ici demain matin

### 1. Installer le package **et le faire tourner une fois**

C'est la seule condition réelle pour suivre la matinée. Et le « une fois » compte autant
que l'installation : le premier appariement télécharge le référentiel taxonomique sur
votre machine. Une fois qu'il y est, l'application fonctionne **sans connexion** — ce qui
est notre filet de sécurité collectif si la connexion de la salle est mauvaise.

```r
# 1. Laisser plus de temps au téléchargement (utile sur connexion lente)
options(timeout = max(3000, getOption("timeout")))

# 2. Installer l'assistant 'remotes' — seulement la première fois
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

# 3. Installer CafriplotsR depuis GitHub
remotes::install_github("umr-amap/cafriplotsR", upgrade = "never")
```

Puis, et c'est la partie à ne pas sauter :

```r
library(CafriplotsR)
launch_taxonomic_match_app()
```

L'application s'ouvre dans votre navigateur. Cliquez sur **Se connecter en utilisateur
public**, chargez n'importe quelle petite liste de noms — trois noms suffisent — et lancez
l'appariement. C'est tout. Vous verrez apparaître au lancement suivant un bouton
**utiliser hors ligne** : c'est le signe que le référentiel est bien sur votre machine.

Si quelque chose bloque, écrivez-moi ce soir plutôt que demain matin.

Les trois causes d'échec habituelles : une version de R antérieure à 4.1 (vérifier avec
`R.version.string`) ; sous Windows, la question « installer depuis les sources qui
nécessitent compilation ? », à laquelle il faut répondre **non** (`n`) ; une connexion
lente, ce à quoi sert la première ligne.

### 2. Apporter une liste d'espèces

Un fichier `.xlsx` ou `.csv` à vous, avec des noms d'espèces. L'heure d'atelier du matin se
fait sur vos données, pas sur un exemple.

- **Si vous pouvez me l'envoyer aujourd'hui**, faites-le : cela me permet de préparer
  l'atelier sur vos cas réels plutôt que sur des exemples fabriqués.

Ceux qui viendront sans fichier ne resteront pas spectateurs : le fichier joint,
`formation_2026_liste_especes_exemple2.xlsx`, est là pour ça — une liste de terrain de 33
noms, à traiter exactement comme la vôtre. Inutile de l'ouvrir avant samedi.

---

## Dans la salle : comment on se connecte

Trois consignes :

1. **Une connexion Zoom par personne ou binôme**
2. **Une seule machine porte le son** — celle qui est branchée au vidéoprojecteur. Toutes
   les autres rejoignent la réunion **sans audio** : micro coupé *et* haut-parleurs coupés.
3. **Caméras coupées**, des deux côtés.

Le vidéoprojecteur sert aux séquences où tout le monde regarde la même chose — la
présentation de 09h15, la démonstration de 10h00, le bilan de 12h10. Pendant l'atelier, ce
sont vos écrans qui comptent.

Prévoyez une multiprise.

---

## Matin — 09h00 à 12h30

Horaires en heure de Douala.

| Horaire | Séquence | |
|---|---|---|
| 09h00 – 09h15 | **Accueil** | Une seule question à chacun : quel est votre dernier fichier de noms d'espèces, et d'où vient-il ? |
| 09h15 – 09h40 | **À quoi sert tout ça** | Pourquoi deux listes d'espèces ne s'alignent jamais tout à fait ; ce qu'est un référentiel taxonomique ; ce qu'est le réseau Cafriplots et à quoi sert la boîte à outils CafriplotsR |
| 09h40 – 10h00 | **Mise en route** | Chacun ouvre l'application, en utilisateur public ou hors ligne. C'est le moment de régler les derniers blocages |
| 10h00 – 10h30 | **Démonstration guidée** | Sur un fichier d'exemple volontairement imparfait. Appariement automatique, puis lecture des indicateurs : méthode d'appariement, score de similarité, synonymie résolue |
| 10h30 – 10h45 | *Pause* | |
| 10h45 – 11h45 | **Atelier — sur vos données** | Votre fichier, votre appariement, et la révision à la main des noms douteux : suggestions approchantes, recherche manuelle, arbitrage. C'est la partie qui vous rendra capable de recommencer seul lundi. |
| 11h45 – 12h10 | **Traits, référentiels et export** | Densité du bois, forme de croissance, traits foliaires au niveau du taxon — avec les références bibliographiques à reprendre en méthodes. Formats de sortie et export. Et le panneau qui vous donne **le code R équivalent** de ce que vous venez de faire à la souris. |
| 12h10 – 12h30 | **Bilan** | Ce qui existe au-delà de la matinée, en mots : gérer ses propres inventaires dans l'infrastructure, l'assistant d'import, l'extraction de données de parcelles. Et ce qu'on fait de l'après-midi. |

À midi, vous repartez avec votre liste d'espèces standardisée, ses identifiants
taxonomiques stables, un tableau de traits pour les taxons couverts, les citations
associées — et un script R qui refait tout cela sur votre fichier.

---

## Après-midi — 14h00 à 16h30

**Une permanence, pas un second cours.**

On passe à la seconde application, `query_plots`, celle qui sert à interroger et extraire
des données d'inventaire.

**Sur quelles données.** Quelques inventaires sont en accès libre dans la base, ouverts à
tous, mais tous relevés une seule fois. J'ouvre en plus, **pour la journée seulement**, une
parcelle permanente d'un hectare recensée deux fois, en 2021 et en 2024 : 456 arbres
étiquetés, dont 436 mesurés aux deux dates. Rien à importer, rien à préparer : vous arrivez
et vous interrogez.

**Le point sur lequel je veux vraiment m'arrêter : la forme des données en sortie.** Les
mêmes arbres, les mêmes mesures, peuvent sortir sous trois formes :

| Format | Une ligne = | Sert à |
|---|---|---|
| **Large** | un arbre, avec une colonne par recensement (`dbh_2021`, `dbh_2024`) | lire un tableau à l'œil, comparer deux dates |
| **Long** | une mesure | la représentation la plus complète mais la moins intuitive. C'est ce qu'attendent la plupart des fonctions de R |
| **Paires de recensements** | un intervalle entre deux passages, avec `dbh0`, `dbh1` et le temps écoulé | les taux de croissance, la mortalité, le recrutement |


**Le reste, selon la demande** : les filtres et la carte pour construire une sélection,
l'export (Excel, CSV, RDS, shapefile), les traits et les référentiels pour ceux qui
veulent aller plus loin, le code R généré, et toute question restée en suspens le matin.

Un seul point de dépendance : contrairement à la matinée, cette application n'a pas de
mode hors ligne et demande une connexion.

---

## Ce que la journée n'est pas

Ce n'est pas une formation à R, et c'est délibéré. Les deux applications sont faites pour
être utilisées sans écrire de code — la seule ligne de R que vous taperez demain est celle
qui lance l'application.

Cela dit, à ceux qui sont venus pour progresser en R : le pont existe, et il est en fin de
matinée. Chaque manipulation faite à la souris produit le code R correspondant, copiable
dans un script. À 11h45, vous aurez résolu un vrai problème sur votre propre fichier, et
le panneau vous donnera le script qui le refait — avec vos noms de colonnes, vos réglages,
vos données. On n'apprend pas R en regardant du R ; on l'apprend en ayant du code qui
marche sur un problème qu'on vient de résoudre soi-même.

Ne seront pas traités : l'import de vos inventaires dans l'infrastructure (dix minutes de
présentation orale, sans démonstration — une session dédiée si la demande existe), et les
statistiques.

Aucune donnée ne vous sera demandée. La liste que vous chargez dans l'application de
standardisation reste sur votre machine et n'est déposée nulle part.

---

## Pour aller voir avant

- Le réseau Cafriplots — <https://www.cafriplot.net/>
- Présentation du package, en français — <https://umr-amap.github.io/cafriplotsR/articles/readme-fr.html>

À demain,

Gilles Dauby — gilles.dauby@ird.fr
