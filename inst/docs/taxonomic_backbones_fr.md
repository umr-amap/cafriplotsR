# Référentiels taxonomiques dans CafriplotsR

Ce document décrit les deux référentiels taxonomiques disponibles dans le package,
leur structure en base de données et la façon dont ils sont liés l'un à l'autre.

---

## Vue d'ensemble

Le package maintient un **référentiel interne** dérivé du référentiel taxonomique de la base de données RAINBIO, 
lui même hérité à l'époque du référentiel de l'Herbarium de Wageningen (base de données Brahms).
Depuis la version X.Y, il peut être enrichi optionnellement avec le **référentiel WCVP** (World Checklist
of Vascular Plants), importé depuis le package R `rWCVPdata` et stocké dans la
même base de données aux côtés d'une table de correspondance (faisant le lien entre les deux).

La fonction `query_taxa()` expose les deux référentiels via son argument `backbone` :

```r
# Par défaut — référentiel interne uniquement
query_taxa(species = "Dacryodes edulis")

# Enrichi WCVP — remplace les colonnes taxonomiques par les valeurs WCVP
# lorsqu'une correspondance existe
query_taxa(species = "Dacryodes edulis", backbone = "wcvp")
```

---

## 1. Référentiel interne — `table_taxa`

Chaque ligne représente un taxon à n'importe quel rang (classe → infraspécifique).
La synonymie et la hiérarchie sont encodées par deux clés étrangères auto-référentielles.

### Colonnes principales

| Colonne | Type | Description |
|---|---|---|
| `idtax_n` | integer PK | Identifiant interne unique du taxon |
| `idtax_good_n` | integer FK → `idtax_n` | `NULL` = taxon accepté ; non-NULL = synonyme pointant vers le nom accepté |
| `id_parent` | integer FK → `idtax_n` | Noeud parent dans l'arbre taxonomique (utilisé pour la traversée hiérarchique récursive) |
| `tax_level` | varchar | Rang : `"higher"` / `"class"`, `"order"`, `"family"`, `"genus"`, `"species"`, `"infraspecific"` |
| `tax_famclass` | varchar | Nom de la classe |
| `tax_order` | varchar | Nom de l'ordre |
| `tax_fam` | varchar | Nom de la famille |
| `tax_gen` | varchar | Nom du genre |
| `tax_esp` | varchar | Épithète spécifique |
| `tax_rank01` / `tax_nam01` | varchar | Premier rang infraspécifique et épithète (ex. : `"var."` / `"thollonii"`) |
| `tax_rank02` / `tax_nam02` | varchar | Deuxième rang infraspécifique et épithète |
| `author1` | varchar | Auteur de l'épithète spécifique (basionyme) |
| `author2` | varchar | Auteur du premier taxon infraspécifique |
| `author3` | varchar | Auteur du deuxième taxon infraspécifique |
| `morpho_species` | boolean | S'agit-il d'une morpho-espèce ? |
| `id_tax_famclass` | integer FK | → `table_tax_famclass` (table de référence classe/division) |
| `tax_source` | varchar | Provenance de la donnée / référence source |
| `introduced_status` | varchar | Statut d'introduction (indigène, introduit, etc.) |
| `citation` | varchar | Référence bibliographique |
| `year_description` | integer | Année de description taxonomique |
| `data_modif_d/m/y` | integer | Date de dernière modification (jour / mois / année) |

### Synonymie

```
idtax_good_n IS NULL  →  taxon accepté
idtax_good_n = 1234   →  synonyme ; le nom accepté a idtax_n = 1234
```

### Hiérarchie

Chaque entrée est reliée à son parent via `id_parent`. Des CTE récursives
permettent de remonter (ancêtres) ou de descendre (descendants) dans l'arbre :

```
classe  ←  ordre  ←  famille  ←  genre  ←  espèce  ←  infraspécifique
```

Fonctions associées : `get_taxon_children()`, `get_taxon_ancestors()`, `get_taxon_hierarchy()`.

### Construction du nom complet

La chaîne de caractères du nom est construite à la volée à partir des colonnes
atomiques :

```r
# Espèce
paste(tax_gen, tax_esp)
# → "Piptadeniastrum africanum"

# Infraspécifique (un niveau)
paste(tax_gen, tax_esp, tax_rank01, tax_nam01)
# → "Guibourtia tessmannii var. tessmannii"

# Avec auteurs
paste(tax_gen, tax_esp, author1, tax_rank01, tax_nam01, author2)
```

---

## 2. Référentiel WCVP — `wcvp_names`

**Base de données :** importé depuis `rWCVPdata::wcvp_names`

Le jeu de données WCVP complet (~350 000 enregistrements) est importé tel quel
dans la base taxa. Contrairement au référentiel interne, la chaîne du nom complet
est **pré-construite** dans la colonne `taxon_name` et le statut d'acceptation est
stocké explicitement dans `taxon_status`.

### Colonnes principales

| Colonne | Type | Description |
|---|---|---|
| `plant_name_id` | integer PK | Identifiant unique WCVP du nom |
| `ipni_id` | varchar | Identifiant IPNI (Index of Plant Names) |
| `accepted_plant_name_id` | integer FK → `plant_name_id` | Nom accepté pour les synonymes (analogue de `idtax_good_n`) ; mis à `NA` dans les résultats de requête quand le taxon est déjà accepté |
| `parent_plant_name_id` | integer | Entrée parente dans la hiérarchie WCVP |
| `taxon_status` | varchar | `"Accepted"`, `"Synonym"`, `"Illegitimate"`, `"Invalid"`, etc. |
| `taxon_name` | varchar | Nom complet pré-construit (sans auteurs) |
| `taxon_rank` | varchar | `"Species"`, `"Genus"`, `"Family"`, `"Variety"`, etc. |
| `family` | varchar | Nom de la famille |
| `genus` | varchar | Nom du genre |
| `species` | varchar | Épithète spécifique |
| `infraspecific_rank` | varchar | Abréviation du rang infraspécifique |
| `infraspecies` | varchar | Épithète infraspécifique |
| `taxon_authors` | text | Chaîne auteur complète (basionyme + combinaison) |
| `geographic_area` | text | Codes de distribution WGSRPD |
| `lifeform_description` | text | Forme biologique (Raunkiær) |
| `first_published` | varchar | Année (ou intervalle) de première publication valide |
| `wcvp_version` | varchar | Tag de version du jeu de données (suivi des imports) |

### Synonymie

```
taxon_status = "Accepted"        →  taxon accepté
taxon_status = "Synonym" (etc.) →  suivre accepted_plant_name_id
                                     pour atteindre l'enregistrement accepté
```

### Import et suivi de version

La table `wcvp_import_metadata` enregistre chaque import :

| Colonne | Description |
|---|---|
| `wcvp_version` | Chaîne de version issue de `rWCVPdata` |
| `import_date` | Horodatage de l'import |
| `imported_by` | Utilisateur système |
| `record_count` | Nombre d'enregistrements importés |
| `link_count` | Nombre de liens présents dans `wcvp_idtax_link` |
| `is_current` | Seul l'import le plus récent est à `TRUE` |

```r
# Vérifier l'import en cours
get_wcvp_status()

# Vérifier si une version plus récente est disponible
check_wcvp_update()

# Réimporter (si une mise à jour est disponible)
import_wcvp_names(con_taxa, force = TRUE)
```

---

## 3. La table de correspondance — `wcvp_idtax_link`


Cette table fait correspondre chaque taxon interne (`idtax_n`) à un ou plusieurs
noms WCVP (`plant_name_id`). Elle est alimentée par le workflow de correspondance
automatique et peut être complétée par des entrées vérifiées manuellement.

### Colonnes

| Colonne | Type | Description |
|---|---|---|
| `idtax_n` | integer FK → `table_taxa` | Identifiant du taxon interne |
| `plant_name_id` | integer FK → `wcvp_names` | Identifiant du nom WCVP apparié |
| `match_type` | varchar | `"exact"` — chaîne identique ; `"fuzzy"` — correspondance approchée |
| `match_score` | numeric(4,3) | Similarité de chaîne normalisée (0–1) |
| `matched_on` | timestamp | Date et heure du calcul de la correspondance |
| `matched_by` | varchar | Utilisateur ayant lancé l'appariement |
| `verified` | boolean | `TRUE` si la correspondance a été vérifiée manuellement |
| `notes` | text | Annotation en texte libre |
| PK | (idtax_n, plant_name_id) | Clé primaire composite — un taxon interne peut être lié à plusieurs noms WCVP (cas des homonymes) |

### Workflow d'appariement

```r
con_taxa <- call.mydb.taxa()

# 1. Lancer l'appariement automatique (exact d'abord, puis fuzzy pour les non-appariés)
matches <- match_taxa_to_wcvp(
  con_taxa,
  methods          = c("exact", "fuzzy"),
  fuzzy_threshold  = 0.9,
  author_match     = "fuzzy",   # "none" | "exact" | "fuzzy"
  author_threshold = 0.6,
  n_cores          = 4L         # parallèle sous Windows (PSOCK) ou Unix (fork)
)

# 2. Réviser le tibble de correspondances manuellement si nécessaire
# matches contient : idtax_n, taxon_name_internal, plant_name_id, wcvp_taxon_name,
#                    match_type, match_score

# 3. Enregistrer les correspondances validées
save_wcvp_links(matches, con_taxa)
```

---

## 4. Structure parallèle

| Concept | Référentiel interne | Référentiel WCVP |
|---|---|---|
| Identifiant principal | `idtax_n` | `plant_name_id` |
| Pointeur vers l'accepté | `idtax_good_n` (`NULL` si accepté) | `accepted_plant_name_id` (`NA` si déjà accepté) |
| Parent hiérarchique | `id_parent` | `parent_plant_name_id` |
| Indicateur d'acceptation | déduit de `idtax_good_n IS NULL` | explicite : `taxon_status = "Accepted"` |
| Chaîne du nom complet | construite à la volée depuis les colonnes atomiques | pré-construite dans la colonne `taxon_name` |
| Stockage des auteurs | réparti sur `author1`, `author2`, `author3` | champ texte unique `taxon_authors` |
| Provenance de la donnée | `tax_source` | `wcvp_version` (niveau import) |

---

## 5. Comment `query_taxa()` intègre les deux référentiels

```
┌─────────────────────────────────────────────────────────────────┐
│  query_taxa(..., backbone = "wcvp")                             │
│                                                                 │
│  1. Interroge table_taxa (référentiel interne)                  │
│     • résolution des synonymes via idtax_good_n                 │
│     • filtres hiérarchiques (only_genus, include_children, …)   │
│                                                                 │
│  2. get_wcvp_names(idtax_n)                                     │
│     JOIN wcvp_idtax_link  ON idtax_n                            │
│     JOIN wcvp_names       ON plant_name_id                      │
│     suit accepted_plant_name_id pour les synonymes (optionnel)  │
│                                                                 │
│  3. .apply_wcvp_backbone()                                      │
│     Écrase : tax_fam, tax_gen, tax_esp,                         │
│              tax_sp_level, tax_infra_level,                     │
│              tax_infra_level_auth                               │
│     Ajoute : wcvp_plant_name_id,                                │
│              wcvp_accepted_plant_name_id,                       │
│              name_source ("wcvp" | "internal"),                 │
│              alt_taxon_name (nom interne original)              │
│                                                                 │
│  Taxa sans lien WCVP → name_source = "internal",               │
│  colonnes internes inchangées.                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## 6. Fonctions principales

| Fonction | Rôle |
|---|---|
| `query_taxa()` | Requête principale ; `backbone = "wcvp"` déclenche l'enrichissement |
| `match_tax()` | Résolution des synonymes + agrégation des traits au niveau genre |
| `add_taxa_table_taxa()` | Récupération rapide des infos taxonomiques (sans traits) |
| `setup_wcvp_schema()` | Crée les tables WCVP dans la base taxa |
| `import_wcvp_names()` | Importe ou rafraîchit le jeu de données WCVP |
| `match_taxa_to_wcvp()` | Génère les correspondances automatiques entre noms |
| `save_wcvp_links()` | Enregistre les correspondances dans `wcvp_idtax_link` |
| `get_wcvp_names()` | Recherche les infos WCVP pour un vecteur d'`idtax_n` |
| `get_wcvp_status()` | Affiche la version et les compteurs de l'import en cours |
| `check_wcvp_update()` | Détecte si une version WCVP plus récente est disponible |
| `get_taxon_children()` | Chemin récursif des descendants via `id_parent` |
| `get_taxon_ancestors()` | Remonte la hiérarchie jusqu'à la racine |
| `get_taxon_hierarchy()` | Chemin complet classe → taxon sous forme de liste structurée |
