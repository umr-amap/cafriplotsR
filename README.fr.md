# CafriplotsR <img src="man/figures/logo.png" align="right" height="139" />

> Package R pour gérer et explorer la base de données des parcelles forestières d'Afrique centrale [réseau cafriplot](https://cafriplot.net/)

## Vue d'ensemble

`CafriplotsR` fournit des outils pour interroger une base de données PostgreSQL contenant des données d'inventaires forestiers d'Afrique tropicale.
Le package offre des fonctions et des applications Shiny pour (1) gérer les mesures d'arbres individuels sur lesquels les traits _sensus largo_ au niveau des taxa ou des tiges peuvent être agrégés, (2) standardiser les informations taxonomiques et enrichir avec des traits au niveau des taxa.
L'avantage de ce package est de permettre la gestion des inventaires, des traits et des observations sous un même référentiel taxonomique, facilitant l'intégration des données, la reproductibilité dans l'analyse et la manipulation des données, ainsi que leur réutilisabilité.

**Fonctionnalités clés :**
- Interroger les données de parcelles, les mesures d'arbres individuels et les caractéristiques écologiques
- Accéder et agréger les traits _sensus largo_ au niveau des espèces
- Application Shiny pour standardiser et corriger votre propre liste de noms taxonomiques

---

**[English version available here / Version anglaise disponible ici](README.md)**

---

## Pourquoi CafriplotsR ?

CafriplotsR, package R en développement (que vous pouvez découvrir : https://umr-amap.github.io/cafriplotsR/), est une 'boîte à outils' dédiée à la gestion, standardisation et harmonisation de données d'inventaires des Forêts d'Afrique centrale (FAC). CafriplotsR est adossé à un « réseau de réseaux » de parcelles permanentes en Afrique centrale nommé [Cafriplot](https://cafriplot.net/).

### Le défi

Nous sommes nombreux à inventorier les peuplements de ligneux dans les FAC, en ciblant plusieurs caractéristiques : la dynamique (mortalité et croissance), les diversités floristique et fonctionnelle, l'évaluation et la gestion des ressources, les effets des perturbations à la fois anciennes et contemporaines, les études des interactions faune-flore, etc.

Ces initiatives, ainsi que les groupes de recherches qui les portent, souffrent cependant de **visibilité insuffisante** :

- **Au sein de la communauté régionale** : Cette visibilité insuffisante limite les possibilités de collaboration, les échanges d'expérience, l'harmonisation des protocoles et l'identification des complémentarités en données et compétences parmi les scientifiques et gestionnaires qui travaillent sur ces forêts.

- **À l'international** : Cela fait dire trop souvent « on ne connait presque rien des forêts du Bassin du Congo ». Certes, les indicateurs montrent qu'il y a des lacunes de connaissance et de compétences comparativement aux autres grands blocs forestiers tropicaux ; mais affirmer que notre connaissance de ces forêts ne repose que sur une poignée d'initiatives internationales (et donc visibles) est réducteur.

### Le problème d'accessibilité des données

Contrairement aux données d'occurrences d'espèces dont l'accessibilité s'est considérablement améliorée (par exemple GBIF), il reste difficile d'avoir une vue d'ensemble des données d'inventaires en Afrique centrale, qu'il s'agisse des inventaires récemment mis en place, mais aussi des inventaires 'historiques' datant parfois de plusieurs décennies. En effet, ces inventaires n'existent parfois plus que sous des formats papier (quand ils n'ont pas tout à fait disparu !) alors qu'ils documentent la biodiversité végétale de ces forêts dans des localités parfois devenues inaccessibles.

**Les causes principales :**
1. De mauvaises pratiques d'archivage des données
2. L'absence de ressources pour maintenir l'accessibilité au-delà des projets
3. Une volonté insuffisante de rendre accessible les données

### Le défi de l'intégration des données

Par ailleurs, il existe une difficulté majeure à **combiner différents types de données** (ex. statuts de conservation d'espèces, traits fonctionnels, etc.) avec les données inventaires, alors même que cette compilation est une étape obligatoire pour investiguer de nombreuses questions de recherche. Ces compilations sont régulièrement réalisées, mais les méthodes suivies manquent de reproductibilité. Si on demandait à 10 personnes de réaliser ce type de compilation indépendamment, on arriverait probablement à 10 résultats différents. Ces résultats sont par exemple fonction :

- Des données accessibles au moment de la compilation (alors même que l'accès aux données varie fortement avec le contexte dans lequel évolue chaque personne)
- De la manière dont la taxonomie est standardisée entre bases de données

### La solution CafriplotsR

À partir de ces constats, la première chose serait de s'accorder sur le fait que les insuffisances exposées ci-dessus sont en effet problématiques.

CafriplotsR a pour ambition de relever ces défis à travers la **mise en commun d'une infrastructure et une gestion de données d'inventaires**, tout en **garantissant la souveraineté des données** à chaque utilisateur ou groupe de recherche.

**Le package vise à :**
- Améliorer la visibilité des travaux de terrain des différentes équipes présentes en Afrique centrale
- Faciliter la gestion des données collectées (encodage, nettoyage, consolidation, requêtes, etc.)
- Faciliter et améliorer la documentation et la reproductibilité des traitements
- Booster les collaborations scientifiques dans la région au travers de partages choisis et maîtrisés des données

### Comment CafriplotsR se distingue des initiatives globales

On pourrait comparer CafriplotsR à d'autres initiatives de 'centralisation' de données qui reposent sur des approches globales (ForestPlots.net par exemple), mais CafriplotsR s'en distingue car :

1. **Approche régionale, non globale** : Il ne s'agit pas d'une initiative globale, mais d'une approche régionale (l'Afrique centrale) et donc à une échelle géographique et humaine qui permet des interactions entre les acteurs impliqués dans la collecte, la gestion et l'utilisation de ces données de référence

2. **Gestion transparente de différents types de données** : CafriplotsR permet de gérer de manière transparente différents types de données associées à des espèces de plantes ligneuses (les occurrences, les traits, les attributs pertinents que l'on peut associer à un taxon)

3. **Souveraineté des données plutôt que centralisation stricte** : CafriplotsR ne vise pas une centralisation des données au sens strict car chaque utilisateur reste souverain dans la gestion de ses données. CafriplotsR a pour ambition de fédérer les groupes de recherche impliqués dans les inventaires de ligneux en Afrique Centrale

### Prochaines étapes

Avec un objectif de rendre accessible l'import, la gestion et la standardisation de données à travers des applications interactives et ludiques, les prochaines étapes doivent permettre de développer cet outil dans une **démarche de co-construction** afin d'identifier et de répondre aux besoins concrets des utilisateurs potentiels en Afrique centrale.

## Installation

Copiez les trois étapes ci-dessous dans la console R, l'une après l'autre.

```r
# 1. Laisser plus de temps au téléchargement (utile sur connexion lente)
options(timeout = max(3000, getOption("timeout")))

# 2. Installer l'utilitaire 'remotes' - nécessaire seulement la première fois
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

# 3. Installer CafriplotsR depuis GitHub (les packages requis suivent automatiquement)
remotes::install_github("umr-amap/cafriplotsR", upgrade = "never")
```

Vérifiez ensuite que l'installation a fonctionné :

```r
library(CafriplotsR)
```

Si cette dernière ligne n'affiche aucune erreur, le package est prêt à être utilisé.

**En cas de problème :**

- *« there is no package called 'remotes' »* — relancez l'étape 2.
- *L'installation s'interrompt sur une connexion lente* — redémarrez R, puis
  relancez les trois étapes en commençant par l'étape 1.
- Si R demande `Do you want to install from sources the package which needs
  compilation?`, répondez **non** (tapez `n` puis Entrée).

**Note :** Aucun identifiant n'est nécessaire pour démarrer. Les applications de
standardisation taxonomique, de consultation du référentiel taxonomique et
d'interrogation des parcelles proposent un bouton **Connect as public user**,
qui ouvre un accès en lecture seule à la taxonomie et aux traits d'espèces. Tout
ce qui écrit dans la base — import, mise à jour, gestion des spécimens —
nécessite votre propre compte, qui ne donne accès qu'aux seules parcelles qui
vous sont autorisées.


## Logique du package et contrôle d'accès

Le package `CafriplotsR` offre des outils pour **manipuler, exporter, visualiser, standardiser et enrichir** les données d'inventaires de plantes d'Afrique centrale.

### Modèle d'accès

Le package implémente un **système d'accès à deux niveaux** :

1. **Inventaires de parcelles** (sécurité au niveau des lignes) :
   - Chaque utilisateur a accès à **ses propres parcelles**, contrôlé par des politiques de sécurité au niveau des lignes de la base de données
   - Les politiques définissent quelles parcelles spécifiques chaque utilisateur peut interroger et mettre à jour
   - Garantit que les fournisseurs de données maintiennent le contrôle sur leurs inventaires contribués
   - Certains inventaires sont accessibles à tous les utilisateurs

2. **Traits au niveau des espèces** (accès pour tous les utilisateurs) :
   - **Tous les utilisateurs** ont un accès en lecture à la base de données taxonomique
   - Ces données sont greffées et agrégées aux inventaires

Cette conception garantit la souveraineté des données pour les propriétaires de parcelles tout en permettant à la communauté de recherche de bénéficier des connaissances taxonomiques et de traits partagées.

### Développement futur

- **Données d'occurrence d'espèces** : Accès ouvert aux données d'occurrence en Afrique centrale (pas encore implémenté). La base de données RAINBIO (uniquement pour les arbustes et arbres) sera accessible et interopérable avec les inventaires.

## Architecture de la base de données

Le package se connecte à deux bases de données PostgreSQL :

1. **Base de données principale** (`plots_transects`) : Données de parcelles, sous-parcelles et arbres individuels
2. **Base de données taxonomique** (`rainbio`) : Informations taxonomiques et traits au niveau des espèces

### Démarrage rapide

```r
library(CafriplotsR)

# Se connecter aux deux bases avec une seule invite d'identifiants
cons <- connect_cafri()
mydb <- cons$main
mydb_taxa <- cons$taxa

# Interroger les parcelles
plots <- query_plots(id_plot = c(1, 2, 3))

# Interroger les parcelles par pays
plots <- query_plots(country = "GABON")

# Visualiser la structure de la base de données
get_database_fk(mydb)
```

## Fonctions principales

### Gestion des connexions
- `connect_cafri()` - Se connecter aux deux bases en une seule étape (recommandé)
- `call.mydb()` - Se connecter à la base de données principale uniquement
- `call.mydb.taxa()` - Se connecter à la base de données taxonomique uniquement
- `cleanup_connections()` - Fermer toutes les connexions
- `db_diagnostic()` - Diagnostic de connexion à la base de données

### Interrogation des données
- `query_plots()` - Interroger les métadonnées de parcelles ou les individus

## Documentation

- **Aide sur les fonctions** : Utilisez `?nom_fonction` pour une documentation détaillée
- **Journal des modifications** : Voir [NEWS.md](NEWS.md) pour l'historique des versions et les mises à jour

## Mises à jour récentes

Voir [NEWS.md](NEWS.md) pour les derniers changements, incluant :
- Changements majeurs et guides de migration
- Nouvelles fonctionnalités et améliorations
- Corrections de bugs et améliorations

## Métadonnées du package

- **Auteurs :** Gilles Dauby, Hugo Leblanc, Pierre Ploton
- **Mainteneur :** Gilles Dauby (gilles.dauby@ird.fr)
- **Licence :** GPL-2
- **Version R minimale :** 4.0

## Contribuer

Ce package suit un flux de travail git avec branches :
- Toutes les modifications de code sont effectuées sur des branches de fonctionnalités
- Les modifications sont documentées dans NEWS.md
- Les pull requests sont examinées avant fusion dans master

## Support

Pour les problèmes, questions ou demandes de fonctionnalités, contactez le mainteneur du package.

## Citation

Pour citer CafriplotsR dans vos publications, utilisez :

```r
citation("CafriplotsR")
```

Ou manuellement :

> Dauby, G., Leblanc, H., & Ploton, P. (2024). CafriplotsR: Tools for Exploring, Managing and Standardizing Vegetation Inventories in Central Africa. R package version 1.8.0. https://umr-amap.github.io/cafriplotsR/

Entrée BibTeX :

```bibtex
@Manual{cafriplotsr,
  title = {CafriplotsR: Tools for Exploring, Managing and Standardizing Vegetation Inventories in Central Africa},
  author = {Gilles Dauby and Hugo Leblanc and Pierre Ploton},
  year = {2024},
  note = {R package version 1.8.0},
  url = {https://umr-amap.github.io/cafriplotsR/}
}
```
