# CafriplotsR - Aide-Mémoire

## Installation et Chargement

```r
# Installer le package (une seule fois)
install.packages("remotes")
remotes::install_github("gdauby/CafriplotsR")

# Charger le package (à chaque session)
library(CafriplotsR)
```

---

## Connexion à la Base de Données

```r
# Se connecter (identifiants demandés)
con <- call.mydb()

# Vérifier la connexion
db_diagnostic()

# Voir le statut
print_connection_status()

# Fermer les connexions (fin de session)
cleanup_connections()
```

**Identifiants atelier** : `user_testX` / `[mot de passe fourni]`

---

## Fonctions Principales

### Lister les Références

```r
# Liste des pays
country_list()

# Liste des méthodes d'inventaire
method_list()

```

### Requêtes sur les Parcelles

```r
# Toutes les parcelles accessibles
query_plots()

# Filtrer par pays
query_plots(country = "Gabon")

# Filtrer par nom de parcelle
query_plots(plot_name = "Makokou001")

# Extraire les individus
query_plots(
  id_plot = 123,
  extract_individuals = TRUE
)

# Extraire avec traits
query_plots(
  id_plot = 123,
  extract_individuals = TRUE,
  extract_traits = TRUE
)
```

### Requêtes Taxonomiques

```r
# Rechercher un taxon par nom
query_taxa(species = "Aucoumea klaineana")

# Rechercher par famille
query_taxa(family = "Burseraceae")

# Rechercher par genre
query_taxa(genus = "Aucoumea")

# Rechercher par ID
query_taxa(ids = c(12345, 67890))
```

### Requêtes sur les Traits

```r
# Traits pour une liste de taxons
query_taxa_traits(
  idtax = c(12345, 67890),
  format = "wide",
  add_taxa_info = TRUE
)

# Tous les traits (format long)
query_taxa_traits(
  idtax = c(12345, 67890),
  format = "long"
)
```

---

## Applications Interactives (Shiny)

```r
# Standardisation taxonomique
launch_taxonomic_match_app(language = "fr")

# Exploration des parcelles
launch_query_plots_app(language = "fr")
```

---

## Lecture / Écriture de Fichiers

```r
# Lire un fichier Excel
library(readxl)
mes_donnees <- read_excel("mon_fichier.xlsx")

# Écrire vers Excel
library(writexl)
write_xlsx(mes_donnees, "resultat.xlsx")

```

---

## Syntaxe R de Base

| Syntaxe | Signification | Exemple |
|---------|---------------|---------|
| `<-` | Assigner une valeur | `x <- 5` |
| `$` | Accéder à une colonne | `donnees$colonne` |
| `c()` | Créer un vecteur | `c(1, 2, 3)` |
| `%>%` | Enchaîner des opérations | `donnees %>% filter(...)` |
| `head()` | Premières lignes | `head(donnees, 10)` |
| `View()` | Voir en tableau | `View(donnees)` |
| `nrow()` | Nombre de lignes | `nrow(donnees)` |
| `names()` | Noms des colonnes | `names(donnees)` |

---

## Erreurs Fréquentes et Solutions

| Message d'erreur | Cause probable | Solution |
|------------------|----------------|----------|
| `Error: could not find function` | Package non chargé | `library(CafriplotsR)` |
| `connection refused` | Problème réseau/serveur | Vérifier internet, réessayer |
| `SSL SYSCALL error: EOF` | Connexion interrompue | `cleanup_connections()` puis reconnecter |
| `object not found` | Variable non définie | Vérifier l'orthographe, exécuter le code dans l'ordre |
| `Password required` | Identifiants manquants | Entrer utilisateur et mot de passe |
| `permission denied` | Pas d'accès aux données | Contacter l'administrateur |
| `Error in match.arg` | Mauvaise valeur de paramètre | Vérifier les options valides |

---

## Workflow Type : Enrichir ses Données avec des Traits

```r
# 1. Charger le package
library(CafriplotsR)

# 2. Se connecter
con <- call.mydb()

# 3. Standardiser les noms (application interactive)
launch_taxonomic_match_app(language = "fr")
# -> Exporter le résultat avec les idtax_n

# 4. Lire le fichier standardisé
library(readxl)
mes_especes <- read_excel("resultat_standardise.xlsx")

# 5. Extraire les IDs uniques
ids_taxons <- unique(mes_especes$idtax_n)
ids_taxons <- ids_taxons[!is.na(ids_taxons)]

# 6. Récupérer les traits
traits <- query_taxa_traits(
  idtax = ids_taxons,
  format = "wide",
  add_taxa_info = TRUE
)

# 7. Voir les résultats
View(traits$traits_numeric)

# 8. Exporter
library(writexl)
write_xlsx(traits$traits_numeric, "mes_traits.xlsx")
```

---

## Raccourcis RStudio Utiles

| Raccourci | Action |
|-----------|--------|
| `Ctrl + Enter` | Exécuter la ligne courante |
| `Ctrl + Shift + Enter` | Exécuter tout le script |
| `Ctrl + S` | Sauvegarder |
| `Ctrl + Z` | Annuler |
| `Tab` | Auto-complétion |
| `F1` (sur une fonction) | Voir l'aide |

---

## Aide et Documentation

```r
# Aide sur une fonction
?query_plots
help(query_taxa_traits)

# Voir les arguments d'une fonction
args(query_plots)
```

---

*CafriplotsR - Boîte à outils du réseau Cafriplots*
*Standardisation | Reproductibilité | Partage*
