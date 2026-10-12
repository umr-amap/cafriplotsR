---
title: "Liste d'espèces d'exemple — corrigé animateur"
subtitle: "Fichier formation_2026_liste_especes_exemple.xlsx"
lang: fr
toc: true
format:
  html:
    embed-resources: true
---

Document interne. Le fichier `.xlsx` se distribue aux participants ; ce corrigé, non.

## À quoi sert ce fichier

Un extrait d'inventaire volontairement sale, construit pour que la démonstration
rencontre un cas de chaque situation plutôt que la seule situation facile. Sans cela, une
démonstration sur données propres ne montre rien de ce qui fait l'intérêt de l'outil.

Deux feuilles :

- `inventaire` — 38 lignes, colonnes `plot_id`, `tree_id`, `species_name`, `dbh_cm`,
  `date_releve`. C'est la feuille de travail principale.
- `genre_espece` — 12 lignes avec le genre et l'épithète dans deux colonnes séparées, pour
  démontrer le mode multi-colonnes en vingt secondes.

Les 38 lignes portent 36 noms distincts : une ligne est un doublon exact (`Gilbertiodendron
dewevrei`, deux arbres différents) et une cellule est vide.

## Ce que donne l'appariement automatique

Résultats obtenus en faisant tourner le fichier dans la vraie chaîne d'appariement, contre
le référentiel en cache, avec le seuil par défaut (0,60) et sans les auteurs.

| Résultat | Nombre |
|---|---|
| `exact` | 27 |
| `genus_constrained` (approximatif) | 2 |
| `no_match` (à réviser à la main) | 8 |
| dont synonymes résolus | 4 |

Comptes revérifiés le 25/09/2026, **sur le fichier tel qu'il est aujourd'hui et au seuil
de 0,60 qui est celui de l'application** — et non au seuil de 0,3 que la fonction
`standardize_taxonomic_batch()` prend par défaut quand on l'appelle en console. Les quatre
synonymes sont *Aningeria* → *Pouteria*, *Chlorophora* → *Milicia*, *Celtis durandii var.
ugandensis* → *Celtis gomphophylla*, *Polyalthia* → *Greenwayodendron*. Le cinquième,
`Fabaceae → Fabaceae`, a disparu avec la fusion des doublons — voir plus bas.

**Ce que l'onglet Révision montre, et ce qu'il cache.** La file de révision est construite
par `filter(is.na(idtax_n))` : elle ne contient **que les noms sans identifiant**, ici les
huit `no_match`. Deux lignes ont pourtant été appariées sous 0,95 sans que rien ne le
signale — `Entandrophragma cylindricm` à 0,88, juste, et `Uapaca guineensis var.
guineensis` à **0,68**, qui est un nom inexistant (voir plus bas). Pour les voir, il faut **trier par
`match_score` croissant**, pas ouvrir l'onglet Révision. Monter le seuil à 0,75 fait
basculer l'*Uapaca* dans la file, qui passe de 8 à 9 noms.

## Détail nom par nom

Un tiret dans la colonne « Nom apparié » signale un nom que l'application enverra dans
l'onglet de révision manuelle.

| Nom saisi | Nom apparié | match_method | score | Nom accepté |
|---|---|---|---|---|
| `Gilbertiodendron dewevrei` | Gilbertiodendron dewevrei | exact | 1.00 |  |
| `Aningeria altissima` | Aningeria altissima | exact | 1.00 | → Pouteria altissima |
| `Entandrophragma cylindricm` | Entandrophragma cylindricum | genus_constrained | 0.88 |  |
| `Sterculia sp.` | Sterculia | exact | 1.00 |  |
| `Annonaceae` | Annonaceae | exact | 1.00 |  |
| `Musanga sp.` | Musanga | exact | 1.00 |  |
| `Arbre indéterminé 12` | — | no_match |  |  |
| `Lophira alata Banks ex C.F.Gaertn.` | Lophira alata | exact | 1.00 |  |
| `Chlorophora excelsa` | Chlorophora excelsa | exact | 1.00 | → Milicia excelsa |
| `Milicia excelsa` | Milicia excelsa | exact | 1.00 |  |
| `TERMINALIA SUPERBA` | Terminalia superba | exact | 1.00 |  |
| `musanga cecropioides` | Musanga cecropioides | exact | 1.00 |  |
| `Celtis  mildbraedii` | Celtis mildbraedii | exact | 1.00 |  |
| `Pycnanthus_angolensis` | Pycnanthus angolensis | exact | 1.00 |  |
| `Baillonella toxisperma` | Baillonella toxisperma | exact | 1.00 |  |
| `Triplochyton scleroxilon` | — | no_match |  |  |
| `Picnatus angolens` | — | no_match |  |  |
| `Dacryodes cf. buettneri` | Dacryodes buettneri | exact | 1.00 |  |
| `Santiria aff. trimera` | Santiria trimera | exact | 1.00 |  |
| `Strombosia ? pustulata` | Strombosia pustulata | exact | 1.00 |  |
| `Diospyros spp.` | Diospyros | exact | 1.00 |  |
| `Cola` | Cola | exact | 1.00 |  |
| `Azobé` | — | no_match |  |  |
| `Moabi` | — | no_match |  |  |
| `G. dewevrei` | — | no_match |  |  |
| `Gilbertiodendron inexistens` | — | no_match |  |  |
| `Celtis durandii var. ugandensis` | Celtis durandii var. ugandensis | exact | 1.00 | → Celtis gomphophylla |
| `Uapaca guineensis var. guineensis` | Uapaca guineensis | genus_constrained | 0.68 |  |
| `Staudtia kamerunensis Warb.` | Staudtia kamerunensis | exact | 1.00 |  |
| `Greenwayodendron suaveolens (Engl. & Diels) Verdc.` | Greenwayodendron suaveolens | exact | 1.00 |  |
| `Magnoliopsida` | Magnoliopsida | exact | 1.00 |  |
| `Fabaceae` | Fabaceae | exact | 1.00 | → Fabaceae |
| `Morpho sp. 4` | — | no_match |  |  |
| `Guibourtia tessmannii` | Guibourtia tessmannii | exact | 1.00 |  |
| `Polyalthia suaveolens` | Polyalthia suaveolens | exact | 1.00 | → Greenwayodendron suaveolens |
| `Dacryodes edulis` | Dacryodes edulis | exact | 1.00 |  |

## Les moments à exploiter en séance

**La paire qui gonfle la richesse.** `Chlorophora excelsa` et `Milicia excelsa` sont dans
le fichier comme deux lignes distinctes, avec deux arbres différents. Ce sont deux noms du
même taxon, l'iroko. C'est la démonstration littérale de la question d'ouverture : combien
de noms, combien d'espèces. Faites compter la salle avant de lancer l'appariement.

**La même chose, mais pas côte à côte.** `Polyalthia suaveolens` (ligne 37) et
`Greenwayodendron suaveolens (Engl. & Diels) Verdc.` (ligne 32) sont le même taxon, comme
l'iroko plus haut. La différence est délibérée : Chlorophora et Milicia se suivent, on
peut les repérer à l'œil ; ces deux-là sont séparés par cinq lignes et portent des genres
qui ne se ressemblent pas. C'est la version réaliste du problème — personne ne relit un
inventaire de huit cents lignes en cherchant les paires. Si la salle a trouvé la première
sans l'outil, poser la question une seconde fois avant de lancer l'appariement.

À noter au passage : l'épithète `suaveolens` est identique des deux côtés, le genre non.
Cela coupe court à l'idée, fréquente, qu'un tri alphabétique sur la colonne des noms
suffirait à repérer les doublons.

**Le cas à arbitrer soi-même, et son contre-exemple.** `Uapaca guineensis var. guineensis`
ressort à 0,68, juste au-dessus du seuil par défaut. Le point n'est pas que la variété
serait « perdue » : **ce nom n'existe pas.** L'autonyme n'a jamais été créé, faute d'une
seconde variété décrite sous cette espèce. Vérifié contre deux sources indépendantes le
25/09/2026 — le référentiel interne ne connaît que `Uapaca guineensis var.
bingervillensis` (idtax 238914), et WCVP que `var. sudanica`.

Ce que l'outil fait de ce nom inventé est instructif : il ne dit pas qu'il n'existe pas,
il l'apparie à l'espèce, lui attribue l'identifiant 3905, et passe à la suite. **Un rang
taxonomique fictif reçoit un identifiant sans que personne soit consulté.** Le comportement
est systématique : `Garcinia kola var. kola` sort à 0,61, `Milicia excelsa var. excelsa` à
0,65, `Terminalia superba var. superba` à 0,70.

Le contre-exemple est deux lignes plus haut dans le fichier. `Celtis durandii var.
ugandensis` est un **vrai** nom infraspécifique : il sort en **exact 1,00** et se résout
proprement sur *Celtis gomphophylla*. Les deux lignes se suivent, et **seul le score les
distingue**. C'est l'argument le plus économique qu'on puisse faire sur la lecture des
scores.

Ce cas **n'apparaît pas dans l'onglet de révision**, puisqu'il a reçu un identifiant. Le
geste à transmettre est donc le tri par `match_score` croissant, pas l'ouverture de
l'onglet. Montrer ensuite l'effet du curseur : remonté à 0,75, ce nom bascule en non
apparié et rejoint la file de révision.

C'est la formulation qui porte : **le seuil ne décide pas de la qualité de l'appariement,
il décide de ce sur quoi on vous consulte.** Au-dessus, la décision est prise à votre
place, en silence.

**Deux fautes, ça ne passe plus.** `Entandrophragma cylindricm` (une lettre manquante)
sort à 0,88. `Triplochyton scleroxilon` (deux erreurs, dont une sur le genre) ne sort pas
du tout, et `Picnatus angolens` non plus — le genre y est méconnaissable. La leçon est
utile : l'appariement approximatif rattrape la faute de frappe, pas la faute de mémoire.

Et il vaut mieux qu'il en soit ainsi. Descendu à 0,30, le seuil apparie `Picnatus
angolens` à *Searsia angolensis*, une **Anacardiaceae**, quand le nom visé était
*Pycnanthus angolensis*, une **Myristicaceae**. La ligne aurait l'air propre — un nom
accepté, une famille, un identifiant — et seul le score la trahirait. Baisser le seuil ne
trouve pas davantage, cela invente davantage.

**Les noms vernaculaires.** `Azobé` et `Moabi` désignent pourtant des espèces bien
identifiées — *Lophira alata* et *Baillonella toxisperma*, toutes deux présentes ailleurs
dans le fichier sous leur nom scientifique. Aucun outil ne peut deviner cela. C'est le
moment de dire que la standardisation ne remplace pas la connaissance de terrain, elle la
met en forme.

**Le genre abrégé.** `G. dewevrei` n'est pas apparié, alors que `Gilbertiodendron
dewevrei` l'est deux lignes plus haut. Utile pour faire comprendre que l'abréviation, qui
va de soi dans un texte, est une perte d'information dans un tableau.

**Ce qui est rattrapé sans qu'on le demande.** Majuscules, minuscules, double espace,
underscore, espace final, `cf.`, `aff.`, `?`, `sp.`, `spp.` : tous ces bruits sont
absorbés. À montrer vite, mais à montrer — c'est ce qui évite à chacun de nettoyer sa
colonne à la main avant de commencer.

**Les niveaux autres que l'espèce.** `Sterculia sp.` et `Cola` s'apparient au genre,
`Annonaceae` à la famille, `Magnoliopsida` à la classe. Un appariement partiel est un
résultat, pas un échec : la ligne reste utilisable pour tout ce qui ne demande pas
l'espèce.

## Deux référentiels, deux listes d'espèces : *Dacryodes* et *Pachylobus*

C'est le seul moment de la journée où l'on montre qu'il existe plusieurs référentiels, et
il tient en deux minutes parce qu'il repose sur deux lignes du fichier.

`Dacryodes cf. buettneri` (ligne 19) et `Dacryodes edulis` (ligne 38, le safoutier)
s'apparient tous les deux en exact, sans difficulté. Ils ne posent un problème ni l'un ni
l'autre. Puis on change le référentiel de sortie dans la barre latérale, « Noms dans la
sortie » : *interne* → *WCVP*. Les deux lignes changent de genre.

| Nom saisi | idtax | Sortie référentiel interne (APD) | Sortie WCVP |
|---|---|---|---|
| `Dacryodes cf. buettneri` | 5015 | Dacryodes buettneri (Engl.) H.J.Lam | Pachylobus buettneri (Engl.) Guillaumin |
| `Dacryodes edulis` | 4365 | Dacryodes edulis (G.Don) H.J.Lam | Pachylobus edulis G.Don |

Les deux référentiels sont exactement inversés l'un par rapport à l'autre, et sur les deux
noms à la fois :

- l'APD accepte `Dacryodes buettneri` et classe `Pachylobus buettneri` en synonyme ;
- WCVP accepte `Pachylobus buettneri` et classe `Dacryodes buettneri` en synonyme.

Ce n'est pas un cas isolé ni une entrée oubliée : WCVP range dans *Pachylobus* toutes les
espèces de *Dacryodes* d'Afrique centrale — *buettneri*, *edulis*, *igaganga*,
*klaineana*, *letestui*, *macrophylla*, *normandii*, *pubescens*, *heterotricha*,
*yangambiensis*. Un inventaire de Burseraceae du Gabon ou du Cameroun n'aura donc pas la
même liste d'espèces selon le référentiel choisi, sans qu'un seul arbre ait changé.

**Ce qu'il faut en dire, et ce qu'il ne faut pas en dire.** Aucun des deux n'a tort, et
aucun n'est en retard sur l'autre : les limites du genre *Dacryodes* font l'objet d'un
désaccord réel et actuel. L'outil ne tranche pas et n'a pas à trancher. Il rend le choix
explicite, ce qui est déjà beaucoup — la plupart des listes d'espèces circulent sans que
personne ne sache lequel des deux a servi. C'est la question 3 de la section 6 du déroulé,
posée en début de matinée, qui trouve ici sa réponse.

**Le point à ne pas rater : la colonne `idtax` ne bouge pas.** 5015 reste 5015, 4365 reste
4365, quel que soit le référentiel affiché. C'est la démonstration littérale de la
question 6 — à quoi sert un identifiant, qu'un nom ne sait pas faire. Deux collègues qui
échangent une liste de noms doivent se mettre d'accord sur un référentiel ; deux collègues
qui échangent des identifiants n'ont pas ce problème. Le faire remarquer à voix haute, la
colonne est facile à ignorer.

**Deux contraintes pratiques.**

- Le sélecteur de référentiel de sortie **n'existe pas en mode hors ligne** : il est
  construit à partir des référentiels déclarés dans la base et disparaît sans connexion.
  Cette démonstration se fait donc sur votre écran, en ligne, et pas par les participants.
  C'est aussi la raison pour laquelle elle est placée en fin de matinée, dans le créneau
  qui dépend déjà du réseau, et non pendant la démonstration guidée de 10h00.
- À vérifier en ligne avant samedi : que les idtax 5015 et 4365 sont bien liés à leurs
  enregistrements WCVP dans `taxa_backbone_link`. La correspondance taxonomique est
  vérifiée des deux côtés — contre l'export APD et contre `rWCVPdata` — mais l'affichage
  dépend du lien, et un lien manquant laisserait le nom interne à l'écran sans rien
  signaler.

## Une curiosité du référentiel, désormais corrigée

`Fabaceae` s'appariait bien, mais l'entrée trouvée (idtax 16016) pointait vers une autre
entrée acceptée (idtax 5769) portant le même nom. À l'écran, la colonne du nom accepté
affichait « Fabaceae → Fabaceae », ce qui avait tout l'air d'un bug et n'en était pas un :
il y avait cinq entrées de niveau famille nommées `Fabaceae` dans le référentiel, héritage
de la coexistence des noms Fabaceae et Leguminosae.

Ces doublons ont été fusionnés depuis, sur 5769. La ligne est donc devenue un appariement
exact ordinaire, sans nom accepté affiché — **à condition que le cache ait été reconstruit
après la fusion**. Un participant dont le cache date d'avant verra encore « Fabaceae →
Fabaceae ».

C'est un détail, mais il vaut d'être connu avant qu'on vous le signale depuis la salle, et
il illustre à peu de frais que le référentiel n'est pas un objet figé. Le cache de
l'animateur est à reconstruire avant samedi, ne serait-ce que pour que le corrigé
ci-dessus corresponde à ce qui sera à l'écran.

## Reproduire ou modifier le fichier

Le script de génération est dans le répertoire de travail de la session qui l'a produit et
n'est pas versionné. Le fichier `.xlsx` se régénère à l'identique à partir de la liste de
noms ci-dessus : c'est une liste en dur, des `plot_id` alternés, des `dbh` tirés avec
`set.seed(26)`.

Les deux noms ajoutés en septembre — `Polyalthia suaveolens` et `Dacryodes edulis` — sont
en queue de liste et tirent leurs `dbh` **après** ceux de la seconde feuille, de sorte que
les 36 lignes d'origine et la feuille `genre_espece` sont inchangées au caractère près. Si
vous ajoutez d'autres noms, garder cet ordre de tirage : sinon toutes les valeurs de
diamètre changent, et un participant qui a déjà téléchargé le fichier travaille sur autre
chose que son voisin.

Pour vérifier qu'une version modifiée se comporte encore comme prévu, faire tourner le
fichier dans la chaîne d'appariement et comparer la répartition des `match_method` au
tableau ci-dessus.
