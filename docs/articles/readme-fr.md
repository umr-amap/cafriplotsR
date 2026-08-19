# CafriplotsR - Documentation en Français

## CafriplotsR

> Package R pour la gestion et l’exploration de la base de données des
> parcelles forestières d’Afrique centrale [réseau
> cafriplot](https://cafriplot.net/)

### Aperçu

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

**Fonctionnalités principales :**

- Interroger les données de parcelles, les mesures d’arbres individuels
  et les caractéristiques écologiques
- Accéder et agréger les traits *sensus largo* au niveau de l’espèce
- Application Shiny pour standardiser et corriger votre propre liste de
  noms taxonomiques

### Installation

``` r

# Installer depuis GitHub
install.packages(c("tidyverse", "dbplyr", "devtools"))
devtools::install_github("umr-amap/cafriplotsR", upgrade = "never")
```

En cas de connexion internet lente, l’installation depuis GitHub
ci-dessus peut échouer. Vous pouvez essayer de lancer d’abord cette
ligne de code dans la console, elle augmentera le délai d’attente pour
l’installation :

``` r

options(timeout = max(3000, getOption("timeout")))
```

**Note :** L’accès à la base de données est restreint et nécessite des
identifiants appropriés.

### Logique du Package et Contrôle d’Accès

Le package `CafriplotsR` offre des outils pour **manipuler, exporter,
visualiser, standardiser et enrichir** les données d’inventaire de
plantes d’Afrique centrale.

#### Modèle d’Accès

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

#### Développements Futurs

- **Données d’occurrence d’espèces** : Accès libre aux enregistrements
  d’occurrence à travers l’Afrique centrale (pas encore implémenté). La
  base de données RAINBIO (uniquement pour les arbustes et arbres) sera
  accessible et interopérable avec les inventaires.

### Architecture de la Base de Données

Le package se connecte à deux bases de données PostgreSQL :

1.  **Base de données principale** (`plots_transects`) : Données de
    parcelles, sous-parcelles et arbres individuels
2.  **Base de données taxonomique** (`rainbio`) : Informations
    taxonomiques et traits au niveau de l’espèce

#### Démarrage Rapide

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

### Liaison de Spécimens d’Herbier : Améliorer la Qualité des Données dans le Temps

**Une fonctionnalité unique pour améliorer la qualité des données dans
le temps**

Les identifications de terrain dans les inventaires forestiers, bien que
précieuses, souffrent souvent d’incertitude taxonomique. Les spécimens
botaniques collectés sur les mêmes individus et déposés dans les
herbiers font l’objet de révisions taxonomiques expertes au fil du
temps, aboutissant à des identifications plus précises. Cependant, ces
connaissances améliorées restent généralement isolées dans les bases de
données d’herbiers, déconnectées des données d’inventaires écologiques.

**La solution CafriplotsR : Liens formels spécimen-individu**

Ce package implémente un **système de liaison de spécimens** qui crée
des connexions formelles et persistantes entre :

- Les arbres individuels dans les inventaires forestiers (avec leurs
  mesures écologiques)
- Les spécimens d’herbier collectés sur ces mêmes individus (avec leur
  taxonomie révisée par des experts)

**Avantages clés :**

1.  **Mises à jour taxonomiques automatiques** : Lorsque
    l’identification d’un spécimen est révisée par des taxonomistes,
    l’individu d’inventaire lié hérite automatiquement de la taxonomie
    mise à jour. Aucune ré-identification manuelle nécessaire.

2.  **Amélioration de la qualité des données dans le temps** : Vos
    données d’inventaire deviennent progressivement plus précises au fur
    et à mesure que les identifications de spécimens sont affinées, sans
    nécessiter de revisites sur le terrain ou d’efforts supplémentaires.

3.  **Traçabilité** : Chaque enregistrement d’inventaire maintient un
    lien clair vers son spécimen de référence, fournissant une preuve
    scientifique et permettant la vérification.

4.  **Confiance taxonomique** : Distinguez entre les identifications de
    terrain (sujettes à incertitude) et les identifications basées sur
    des spécimens (vérifiées par des experts).

5.  **Longévité des données** : Les données d’inventaire restent
    connectées à l’évolution des connaissances taxonomiques,
    garantissant une valeur scientifique à long terme.

📖 **Pour des instructions détaillées sur comment lier des spécimens aux
individus**, consultez la vignette : [Liaison de spécimens d’herbier aux
individus
d’inventaire](https://umr-amap.github.io/cafriplotsR/articles/specimen_linking_workflow-fr.md)

### Fonctions Principales

#### Gestion des Connexions

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

#### Interrogation des Données

- [`query_plots()`](https://umr-amap.github.io/cafriplotsR/reference/query_plots.md) -
  Interroger les métadonnées des parcelles ou les individus

### Documentation

- **Aide des fonctions** : Utilisez `?nom_fonction` pour une
  documentation détaillée
- **Journal des modifications** : Voir
  [NEWS.md](https://github.com/umr-amap/cafriplotsR/blob/master/NEWS.md)
  pour l’historique des versions et les mises à jour

### Mises à Jour Récentes

Voir
[NEWS.md](https://github.com/umr-amap/cafriplotsR/blob/master/NEWS.md)
pour les derniers changements, incluant :

- Changements majeurs et guides de migration
- Nouvelles fonctionnalités et améliorations
- Corrections de bugs et améliorations

### Métadonnées du Package

- **Auteurs :** Gilles Dauby, Hugo Leblanc
- **Mainteneur :** Gilles Dauby (<gilles.dauby@ird.fr>)
- **Licence :** GPL-2
- **Version R minimum :** 4.0

### Contribuer

Ce package suit un workflow de branches git :

- Toutes les modifications de code sont faites sur des branches de
  fonctionnalité
- Les changements sont documentés dans NEWS.md
- Les pull requests sont revues avant la fusion dans master

### Support

Pour les problèmes, questions ou demandes de fonctionnalités, contactez
le mainteneur du package.
