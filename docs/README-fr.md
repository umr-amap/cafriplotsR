# CafriplotsR

> Package R pour la gestion et l’exploration de la base de données des
> parcelles forestières d’Afrique centrale [réseau
> cafriplot](https://cafriplot.net/)

## Aperçu

`CafriplotsR` fournit des outils pour interroger une base de données
PostgreSQL contenant des données d’inventaires forestiers d’Afrique
tropicale. Le package offre des fonctions et des applications Shiny pour
(1) gérer les mesures d’arbres individuels sur lesquelles des mesures
(ou observations) de traits *sensus largo* au niveau du taxon ou de la
tige peuvent être agrégées, (2) standardiser les informations
taxonomiques et les enrichir avec des traits au niveau du taxon.
L’avantage de ce package est de permettre la gestion des inventaires,
traits et observations sous le même référentiel taxonomique, facilitant
ainsi l’intégration des données, la reproductibilité dans l’analyse et
la manipulation des données, et la réutilisabilité des données.

**Fonctionnalités principales :** - Interroger les données de parcelles,
les mesures d’arbres individuels et les caractéristiques écologiques -
Accéder et agréger les traits *sensus largo* au niveau de l’espèce -
Application Shiny pour standardiser et corriger votre propre liste de
noms taxonomiques

## Installation

Copiez les trois étapes ci-dessous dans la console R, l’une après
l’autre.

``` r

# 1. Laisser plus de temps au téléchargement (utile sur connexion lente)
options(timeout = max(3000, getOption("timeout")))

# 2. Installer l'utilitaire 'remotes' - nécessaire seulement la première fois
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

# 3. Installer CafriplotsR depuis GitHub (les packages requis suivent automatiquement)
remotes::install_github("umr-amap/cafriplotsR", upgrade = "never")
```

Vérifiez ensuite que l’installation a fonctionné :

``` r

library(CafriplotsR)
```

Si cette dernière ligne n’affiche aucune erreur, le package est prêt à
être utilisé.

**En cas de problème :**

- *« there is no package called ‘remotes’ »* — relancez l’étape 2.
- *L’installation s’interrompt sur une connexion lente* — redémarrez R,
  puis relancez les trois étapes en commençant par l’étape 1.
- Si R demande
  `Do you want to install from sources the package which needs compilation?`,
  répondez **non** (tapez `n` puis Entrée).

**Note :** Aucun identifiant n’est nécessaire pour démarrer. Les
applications de standardisation taxonomique, de consultation du
référentiel taxonomique et d’interrogation des parcelles proposent un
bouton **Connect as public user**, qui ouvre un accès en lecture seule à
la taxonomie et aux traits d’espèces. Tout ce qui écrit dans la base —
import, mise à jour, gestion des spécimens — nécessite votre propre
compte, qui ne donne accès qu’aux seules parcelles qui vous sont
autorisées.

## Logique du Package et Contrôle d’Accès

Le package `CafriplotsR` offre des outils pour **manipuler, exporter,
visualiser, standardiser et enrichir** les données d’inventaire de
plantes d’Afrique centrale.

### Modèle d’Accès

Le package implémente un **système d’accès à deux niveaux** :

1.  **Inventaires de parcelles** (sécurité au niveau des lignes) :
    - Chaque utilisateur a accès à **ses propres parcelles**, contrôlé
      par des politiques de sécurité au niveau des lignes de la base de
      données
    - Les politiques définissent quelles parcelles spécifiques chaque
      utilisateur peut interroger et mettre à jour
    - Assure que les fournisseurs de données gardent le contrôle sur
      leurs inventaires contribués
    - Certains inventaires sont accessibles à tous les utilisateurs
2.  **Traits au niveau de l’espèce** (accès pour tous les utilisateurs)
    :
    - **Tous les utilisateurs** ont un accès en lecture à la base de
      données taxonomique
    - Ces données sont greffées et agrégées aux inventaires

Ce design assure la souveraineté des données pour les propriétaires de
parcelles tout en permettant à la communauté de recherche de bénéficier
des connaissances taxonomiques et de traits partagées.

### Développements Futurs

- **Données d’occurrence d’espèces** : Accès libre aux enregistrements
  d’occurrence à travers l’Afrique centrale (pas encore implémenté). La
  base de données RAINBIO (uniquement pour les arbustes et arbres) sera
  accessible et interopérable avec les inventaires.

## Architecture de la Base de Données

Le package se connecte à deux bases de données PostgreSQL :

1.  **Base de données principale** (`plots_transects`) : Données de
    parcelles, sous-parcelles et arbres individuels
2.  **Base de données taxonomique** (`rainbio`) : Informations
    taxonomiques et traits au niveau de l’espèce

### Démarrage Rapide

``` r

library(CafriplotsR)

# Se connecter aux deux bases avec une seule invite d'identifiants
cons <- connect_cafri()
mydb <- cons$main
mydb_taxa <- cons$taxa

# Interroger les parcelles
plots <- query_plots(id_plot = c(1, 2, 3))

# Interroger les parcelles
plots <- query_plots(country = "GABON")

# Visualiser la structure de la base de données
get_database_fk(mydb)
```

## Fonctions Principales

### Gestion des Connexions

- [`connect_cafri()`](https://umr-amap.github.io/cafriplotsR/reference/connect_cafri.md) -
  Se connecter aux deux bases en une seule étape (recommandé)
- [`call.mydb()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.md) -
  Se connecter à la base de données principale uniquement
- [`call.mydb.taxa()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.taxa.md) -
  Se connecter à la base de données taxonomique uniquement
- [`cleanup_connections()`](https://umr-amap.github.io/cafriplotsR/reference/cleanup_connections.md) -
  Fermer toutes les connexions
- [`db_diagnostic()`](https://umr-amap.github.io/cafriplotsR/reference/db_diagnostic.md) -
  Diagnostics de connexion à la base de données

### Interrogation des Données

- [`query_plots()`](https://umr-amap.github.io/cafriplotsR/reference/query_plots.md) -
  Interroger les métadonnées des parcelles ou les individus

## Documentation

- **Aide des fonctions** : Utilisez `?nom_fonction` pour une
  documentation détaillée
- **Journal des modifications** : Voir
  [NEWS.md](https://umr-amap.github.io/cafriplotsR/NEWS.md) pour
  l’historique des versions et les mises à jour

## Mises à Jour Récentes

Voir [NEWS.md](https://umr-amap.github.io/cafriplotsR/NEWS.md) pour les
derniers changements, incluant : - Changements majeurs et guides de
migration - Nouvelles fonctionnalités et améliorations - Corrections de
bugs et améliorations

## Métadonnées du Package

- **Auteurs :** Gilles Dauby, Hugo Leblanc, Pierre Ploton
- **Mainteneur :** Gilles Dauby (<gilles.dauby@ird.fr>)
- **Licence :** GPL-2
- **Version R minimum :** 4.0

## Contribuer

Ce package suit un workflow de branches git : - Toutes les modifications
de code sont faites sur des branches de fonctionnalité - Les changements
sont documentés dans NEWS.md - Les pull requests sont revues avant la
fusion dans master

## Support

Pour les problèmes, questions ou demandes de fonctionnalités, contactez
le mainteneur du package.

## Citation

Pour citer CafriplotsR dans vos publications, utilisez :

``` r

citation("CafriplotsR")
```

Ou manuellement :

> Dauby, G., Leblanc, H., & Ploton, P. (2024). CafriplotsR: Tools for
> Exploring, Managing and Standardizing Vegetation Inventories in
> Central Africa. R package version 1.8.0.
> <https://umr-amap.github.io/cafriplotsR/>

Entrée BibTeX :

``` bibtex
@Manual{cafriplotsr,
  title = {CafriplotsR: Tools for Exploring, Managing and Standardizing Vegetation Inventories in Central Africa},
  author = {Gilles Dauby and Hugo Leblanc and Pierre Ploton},
  year = {2024},
  note = {R package version 1.8.0},
  url = {https://umr-amap.github.io/cafriplotsR/}
}
```
