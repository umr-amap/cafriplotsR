# Les applications CafriplotsR en un coup d'œil

## Introduction

CafriplotsR propose dix applications interactives. Elles couvrent tout
le cycle de vie d’un jeu de données d’inventaire — l’explorer, en
standardiser la taxonomie, l’importer, le corriger, le relier aux
spécimens d’herbier — sans écrire d’autre code R que la ligne qui lance
l’application.

Chaque application s’ouvre sur le même écran de connexion. Ce qui
change, c’est **qui peut l’utiliser** : trois applications s’ouvrent
sans aucun identifiant, les sept autres exigent votre propre compte car
elles écrivent dans la base.

## Quelle application choisir ?

| Application | Lancement | Accès |
|----|----|----|
| Standardisation des noms taxonomiques | [`launch_taxonomic_match_app()`](https://umr-amap.github.io/cafriplotsR/reference/launch_taxonomic_match_app.md) | public ou compte |
| Référentiel taxonomique | [`launch_taxo_backbone_app()`](https://umr-amap.github.io/cafriplotsR/reference/launch_taxo_backbone_app.md) | public pour consulter, compte pour modifier |
| Interrogation des parcelles | [`launch_query_plots_app()`](https://umr-amap.github.io/cafriplotsR/reference/launch_query_plots_app.md) | public ou compte |
| Import de données de parcelles | [`launch_import_wizard()`](https://umr-amap.github.io/cafriplotsR/reference/launch_import_wizard.md) | compte |
| Caractéristiques et recensements | [`launch_feature_wizard()`](https://umr-amap.github.io/cafriplotsR/reference/launch_feature_wizard.md) | compte |
| Corrections enregistrement par enregistrement | [`launch_data_update_app()`](https://umr-amap.github.io/cafriplotsR/reference/launch_data_update_app.md) | compte |
| Import de traits au niveau du taxon | [`launch_taxa_traits_import()`](https://umr-amap.github.io/cafriplotsR/reference/launch_taxa_traits_import.md) | compte |
| Import de spécimens d’herbier | [`launch_specimen_import_wizard()`](https://umr-amap.github.io/cafriplotsR/reference/launch_specimen_import_wizard.md) | compte |
| Identifications des spécimens | [`launch_specimen_identification_app()`](https://umr-amap.github.io/cafriplotsR/reference/launch_specimen_identification_app.md) | compte |
| Liaison individus ↔︎ spécimens | [`launch_individual_specimen_linking_app()`](https://umr-amap.github.io/cafriplotsR/reference/launch_individual_specimen_linking_app.md) | compte |

## Travailler sans compte

L’écran de connexion des trois applications en lecture seule propose un
bouton **Connect as public user**. Il vous connecte via un compte
partagé, en lecture seule, qui atteint la taxonomie et les traits au
niveau des espèces. Rien de ce que vous faites dans ce mode ne peut
modifier la base, et les commandes d’édition sont masquées plutôt que
désactivées.

Cela suffit pour :

- standardiser votre propre liste d’espèces sur le référentiel d’Afrique
  centrale,
- consulter le référentiel, sa synonymie et les traits associés à un
  taxon,
- explorer les inventaires que leurs propriétaires ont ouverts à tous.

Cela ne suffit **pas** pour importer des données, corriger des
enregistrements ou gérer des spécimens. Ces applications n’affichent pas
le bouton public, car un compte en lecture seule ne permet d’aller au
bout d’aucun de leurs enchaînements.

## Explorer et standardiser

### Standardisation des noms taxonomiques

``` r

launch_taxonomic_match_app()
```

Confronte votre propre liste de noms d’espèces au référentiel
taxonomique des plantes d’Afrique centrale : appariement automatique
avec recherche floue, révision manuelle de ce que l’appariement n’a pas
su résoudre, et export de la liste standardisée. Le résultat attribue à
chacun de vos noms un identifiant de taxon stable (`idtax_n`), ce qui
permet ensuite de greffer traits et inventaires.

Commencez ici si vous arrivez avec une liste d’espèces issue de votre
terrain.

### Référentiel taxonomique

``` r

launch_taxo_backbone_app()
```

Consulte et gère le référentiel taxonomique lui-même : recherche de
taxons, examen des relations de synonymie, et génération du code R
correspondant à la requête que vous venez de construire à la main. Avec
un compte, l’application ajoute aussi de nouveaux taxons, met à jour des
enregistrements existants et entretient la synonymie. En utilisateur
public, vous disposez de la partie consultation et génération de code ;
les commandes d’édition n’apparaissent pas.

### Interrogation des parcelles

``` r

launch_query_plots_app()
```

Une interface interactive à
[`query_plots()`](https://umr-amap.github.io/cafriplotsR/reference/query_plots.md)
: filtrer les parcelles par pays, méthode ou autres critères, les
visualiser sur une carte, descendre jusqu’aux arbres individuels et
exporter le résultat. Les parcelles visibles dépendent du compte utilisé
— ce sont les politiques de sécurité au niveau des lignes qui décident,
et un utilisateur public ne voit que les inventaires ouverts à tous par
leurs propriétaires.

## Importer et mettre à jour

Ces applications exigent toutes votre propre compte, et ne touchent
jamais que les parcelles auxquelles ce compte donne droit.

### Import de données de parcelles

``` r

launch_import_wizard()
```

L’enchaînement complet d’import des parcelles et des mesures
individuelles : charger un fichier, faire correspondre ses colonnes aux
champs de la base, valider, prévisualiser ce qui sera écrit, puis
exécuter. L’application s’appuie sur les fonctions d’import du package :
les contrôles que vous feriez à la main sont appliqués pour vous.

### Caractéristiques et recensements

``` r

launch_feature_wizard()
```

Ajoute des caractéristiques à des parcelles existantes — soit un nouveau
recensement, avec ses dates et les personnes impliquées, soit des
caractéristiques quelconques au niveau de la parcelle.

### Corrections enregistrement par enregistrement

``` r

launch_data_update_app()
```

Corrige les métadonnées de parcelles et les enregistrements individuels
un par un. C’est la contrepartie accessible de
[`update_records()`](https://umr-amap.github.io/cafriplotsR/reference/update_records.md),
plus puissante mais qui suppose de savoir déjà dans quelle table vit la
valeur à changer.

### Import de traits au niveau du taxon

``` r

launch_taxa_traits_import()
```

Importe des mesures de traits attachées aux taxons plutôt qu’aux tiges
individuelles : chargement, correspondance de vos colonnes avec la liste
des traits, prévisualisation, exécution.

## Spécimens d’herbier

### Import de spécimens

``` r

launch_specimen_import_wizard()
```

Importe de nouveaux spécimens d’herbier depuis des fichiers Excel ou
CSV.

### Identifications des spécimens

``` r

launch_specimen_identification_app()
```

Met à jour les identifications enregistrées pour les spécimens, en
s’appuyant sur
[`update_ident_specimens()`](https://umr-amap.github.io/cafriplotsR/reference/update_ident_specimens.md).

### Liaison individus ↔︎ spécimens

``` r

launch_individual_specimen_linking_app()
```

Crée les liens entre arbres individuels et spécimens d’herbier, à partir
des informations d’herbier présentes dans votre jeu de données
d’individus.

## Obtenir un compte

L’accès public couvre la taxonomie et les traits. Travailler sur des
données d’inventaire — les vôtres ou celles d’un collègue — nécessite un
compte, qui détermine aussi les parcelles que vous pouvez interroger et
mettre à jour. L’accès est accordé par utilisateur : contactez les
mainteneurs pour en obtenir un.
