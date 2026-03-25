# CafriplotsR — Newsletter Mars 2026 / March 2026

---

Vous recevez ce message parce que vous utilisez déjà le package CafriplotsR ou parce que vous avez récemment exprimé de l'intérêt pour celui-ci. Si vous ne souhaitez plus recevoir ces actualités, faites-le moi simplement savoir.

*You are receiving this message because you already use the CafriplotsR package or because you have recently expressed interest in it. If you would like to be removed from this list, just let me know.*

Toutes les informations additionelles sont disponibles sur le site du package : https://umr-amap.github.io/cafriplotsR/

*All information is available on the package website: https://umr-amap.github.io/cafriplotsR/*

Tout retour ou signalement de bug est bien sûr le bienvenu.

*Any feedback or bug report is of course welcome*

Gilles Dauby

---

## Actualités CafriplotsR — Mars 2026

*English version below*

### Nouvelles données

**Intégration de la base de données de densité du bois de Tervuren (TWDD)**

Une grande base de données de densité du bois — la TWDD — a été ajoutée à la base de données. Elle contient **13 332 échantillons** couvrant **2 994 espèces**, 1 022 genres et 156 familles de plantes sur six continents, dont 72 % des enregistrements provenant d'Afrique.

La TWDD ajoute 1 164 espèces, 160 genres et 8 familles de plantes qui n'étaient pas documentés auparavant.

Avant l'intégration, la taxonomie de la TWDD a été standardisée selon le référentiel taxonomique de CafriplotsR afin d'assurer la cohérence avec le reste de la base de données. Au total, **12 970 valeurs** issues de la TWDD ont été ajoutées.

> **Obligation de citation :** Toute utilisation de ces données doit citer la publication originale :
>
> Verbiest W.W.M., Hicter P., Beeckman H. et al. (2026). The Tervuren xylarium Wood Density Database (TWDD). *Scientific Data*, 13, 243. https://doi.org/10.1038/s41597-026-06563-2

### Nouvelles fonctionnalités

**Suivi des citations pour les traits au niveau taxonomique**

Les mesures de traits au niveau des taxons stockées dans la base de données sont désormais liées à leurs sources d'origine — études publiées ou bases de données de traits d'où les valeurs ont été extraites. Lorsque vous enrichissez un jeu de données d'espèces ou explorez des traits via les applications interactives (`launch_taxo_backbone_app()` et `launch_taxonomic_match_app()`), un panneau **Sources de données** dédié liste les citations concernées et le nombre de mesures/observations provenant de chaque source.

Cela rend la citation appropriée directe et sans ambiguïté : le panneau liste la référence complète de chaque source de sorte que l'utilisateur sait ce qu'il faut citer lorsque ces données sont utilisées. Ce nouveau développement renforce un des objectifs de ce package qui est de faciliter l'intégration et la ré-utilisation de ces différents jeux de données. Pour cela, il est nécessaire d'être le plus transparent possible sur cette intégration et donc de respecter les exigences de partage des données des publications et bases de données sous-jacentes.

> **Pour les utilisateurs R :** La fonction `query_taxa_traits()` accepte désormais le paramètre `include_citation = TRUE` pour joindre les informations de citation directement au tableau retourné. Les formats de sortie large (*wide*) et long sont tous deux pris en charge.

**Mode accès public pour deux applications Shiny interactives**

Deux applications de la boîte à outils CafriplotsR peuvent désormais être lancées **sans identifiants de connexion** — utile pour les collaborateurs, les relecteurs, ou toute personne souhaitant explorer les données sans demander un compte personnel :

- **Navigateur du référentiel taxonomique** (`launch_taxo_backbone_app()`) : Parcourez le référentiel taxonomique complet, effectuez des recherches par nom ou identifiant, explorez la hiérarchie taxonomique, et extrayez les traits au niveau des taxons en format large ou long avec les citations associées.

- **Application d'harmonisation taxonomique et d'enrichissement en traits** (`launch_taxonomic_match_app()`) : Chargez une liste d'espèces, standardisez les noms par rapport au référentiel CafriplotsR, et enrichissez-les avec les traits de la base de données — le tout dans une interface guidée, étape par étape.

En mode public, les applications se connectent automatiquement avec des identifiants en lecture seule, donc aucune connexion n'est requise. Les deux applications affichent une bannière claire indiquant que vous opérez en mode public avec accès en lecture seule.

---

## CafriplotsR News — March 2026



### New data

**Tervuren xylarium Wood Density Database (TWDD) integrated**

A large wood density dataset — the TWDD — has been added to the database. It contains **13,332 samples** spanning **2,994 species**, 1,022 genera, and 156 plant families across six continents, with 72% of records from Africa.

The TWDD adds 1,164 species, 160 genera, and 8 plant families not previously documented.

Before integration, the taxonomy of the TWDD was standardized against the CafriplotsR taxonomic backbone to ensure consistency with the rest of the database. A total of **12,970 values** from the TWDD were added.

> **Citation requirement:** Any use of this dataset must cite the original publication:
>
> Verbiest W.W.M., Hicter P., Beeckman H. et al. (2026). The Tervuren xylarium Wood Density Database (TWDD). *Scientific Data*, 13, 243. https://doi.org/10.1038/s41597-026-06563-2

### New capabilities

**Citation tracking for taxa-level traits**

Taxa-level trait measurements stored in the database are now linked to their original sources — published studies or trait databases from which values were extracted. When you enrich a species dataset or explore traits through the interactive apps (`launch_taxo_backbone_app()` and `launch_taxonomic_match_app()`), a dedicated **Data Sources** panel lists the citations involved and the number of measurements/observations from each source.

This makes proper attribution straightforward and unambiguous: the panel lists the full reference for each source so that users know exactly what to cite when using these data. This new development reinforces one of the core goals of this package, which is to facilitate the integration and reuse of diverse datasets. Achieving that requires being as transparent as possible about this integration and therefore respecting the data-sharing requirements of the underlying publications and databases.

> **For R users:** The `query_taxa_traits()` function now accepts `include_citation = TRUE` to attach citation information directly to the returned data frame. Both wide and long output formats are supported.

**Public access mode for two interactive Shiny apps**

Two apps in the CafriplotsR toolkit can now be launched **without database credentials** — useful for collaborators, reviewers, or anyone who wants to explore the data without requesting a personal account:

- **Taxonomic backbone browser** (`launch_taxo_backbone_app()`): Browse the full taxonomic reference, search by name or identifier, explore the taxonomic hierarchy, and extract taxa-level traits as wide or long tables with associated citations.

- **Taxonomic harmonisation & trait enrichment app** (`launch_taxonomic_match_app()`): Upload a species list, standardise names against the CafriplotsR backbone, and enrich with traits from the database — all in a guided, step-by-step interface.

In public mode the apps connect automatically using read-only credentials, so no login is required. Both apps display a clear banner indicating that you are operating in public mode with read-only access.

---


