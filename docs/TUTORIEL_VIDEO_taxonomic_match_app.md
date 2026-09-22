# Tutoriel vidéo — `launch_taxonomic_match_app()`

Plan de tournage pour une capture d'écran (OBS Studio) présentant l'application
de standardisation des noms taxonomiques. Le tutoriel est en français, pour une
durée cible de **14 à 16 minutes**.

Ce plan est calé sur le comportement réel de l'application
(`R/shiny_app_taxonomic_match.R` et les modules `mod_data_input`,
`mod_column_select`, `mod_auto_matching`, `mod_name_review`,
`mod_results_export`, `mod_traits_enrichment`). Documentation de référence :
[Utiliser l'Application de Standardisation des Noms Taxonomiques](articles/taxonomic-app-fr.html).

---

## 0. Préparation avant d'appuyer sur REC

### Jeu de données de démonstration

C'est le point le plus important. Préparer un petit fichier `.xlsx`
(15–20 lignes, colonnes `plot_id`, `tree_number`, `species_name`, `dbh`)
contenant volontairement **un cas de chaque situation** — sinon la démonstration
ne montre que le cas facile :

| Nom d'entrée | Ce que ça démontre |
|---|---|
| `Gilbertiodendron dewevrei` | correspondance exacte espèce |
| `Entandrophragma cylindricm` | faute de frappe → `genus_constrained` |
| `Aningeria altissima` | synonyme → nom accepté différent |
| `Sterculia` | correspondance exacte au genre (`tax_level = genus`) |
| `Fabaceae` | correspondance exacte à la famille |
| `Musanga sp.` / `Arbre indéterminé 12` | non apparié → révision manuelle |
| une cellule vide (NA) | ligne sans nom → assignation manuelle |
| `Pycnanthus angolense` | approximatif limite, à arbitrer soi-même |

Prévoir aussi une **deuxième feuille** dans le même classeur, avec `genus` et
`species` en colonnes séparées, pour démontrer le mode multi-colonnes en
20 secondes.

### Réglages écran et OBS

- Résolution 1920×1080, zoom navigateur à **125 %** : le texte de la barre
  latérale est petit à 100 % et devient illisible après compression vidéo.
- Onglet de navigateur seul, sans barre de favoris, profil vierge.
- Activer la mise en évidence du curseur et des clics dans OBS — beaucoup de
  clics portent sur de petits boutons.
- Lancer R et l'application **avant** de commencer à enregistrer, puis couper et
  reprendre sur l'écran de connexion : le démarrage n'a pas d'intérêt à l'écran.

### ⚠️ Sécurité

Ne jamais filmer l'écran de connexion avec de vrais identifiants. Deux options
propres :

1. faire la démonstration avec le bouton **« Se connecter en utilisateur
   public »** (compte en lecture seule — c'est exactement le cas d'usage d'un
   nouvel utilisateur) ;
2. couper l'enregistrement pendant la saisie du mot de passe.

---

## Découpage proposé

### Séquence 1 — Le problème (45 s, avant toute image de l'application)

**Écran** : le fichier Excel ouvert, curseur sur la colonne des noms.

**À dire :**

- « Vous avez une liste de noms d'espèces issue du terrain ou d'un vieux
  fichier. Ils contiennent des fautes de frappe, des synonymes, des noms abrégés
  au genre. »
- « Tant que ces noms ne sont pas rattachés à un identifiant taxonomique, vous
  ne pouvez ni regrouper vos données, ni les croiser avec d'autres jeux, ni
  récupérer de traits. »
- « L'application fait deux choses, dans cet ordre : elle **standardise** la
  nomenclature, puis elle **enrichit** la liste standardisée avec les traits
  disponibles dans la base. » ← phrase-clé du tutoriel ; c'est le sous-titre même
  de l'application.

### Séquence 2 — Lancement et connexion (1 min)

**Écran** : console R, puis écran de connexion.

```r
library(CafriplotsR)
launch_taxonomic_match_app()
```

**À dire :**

- « Aucune connexion préalable n'est nécessaire dans R : la connexion se fait
  dans l'application. »
- Montrer les trois voies de connexion et préciser à qui chacune s'adresse :
  - **compte personnel** : accès complet ;
  - **utilisateur public** : lecture seule, suffit pour standardiser des noms ;
  - **mode hors ligne** : référentiel en cache, utile en cas de connexion
    lente — préciser que l'onglet Traits disparaît alors, puisqu'il exige la
    base.
- Mentionner le sélecteur de langue EN/FR en haut à droite, et rester en
  français.

### Séquence 3 — Lire l'écran d'accueil (1 min)

*Ne pas sauter cette étape.*

**Écran** : panneau « À propos de cette application », déplié par défaut.

**À dire :**

- Parcourir la liste : import → colonnes → correspondance automatique → seuil →
  révision → export → traits.
- « L'ordre de cette liste est exactement l'ordre dans lequel vous allez
  travailler : la barre latérale à gauche pour les entrées, les onglets à droite
  pour les étapes. »
- Signaler le lien vers la documentation complète en bas du panneau.

### Séquence 4 — Charger les données (1 min 30)

**Écran** : barre latérale, module « Data input ».

**À montrer :**

1. **Import fichier** (mode par défaut) : choisir le `.xlsx`, puis le
   **sélecteur de feuille** qui apparaît pour un classeur multi-feuilles.
2. Basculer sur **Saisie texte** et coller trois noms pour montrer
   l'alternative : « un nom par ligne, ou séparés par virgule, point-virgule ou
   tabulation », puis bouton **Charger les noms**.
3. Revenir à l'import fichier pour la suite.

**À dire :**

- « Vos colonnes d'origine sont conservées d'un bout à l'autre : vous
  récupérerez votre tableau complet, enrichi, pas seulement une liste de noms. »
- Mentionner au passage le départ depuis un data.frame déjà chargé :

  ```r
  launch_taxonomic_match_app(data = mon_df, name_column = "species_name")
  ```

### Séquence 5 — Choisir la ou les colonnes (1 min 30)

**À montrer :**

1. **Colonne unique** : sélection de `species_name`.
2. **Colonnes multiples** : basculer sur la deuxième feuille, assigner `genus` /
   épithète / famille. Préciser que l'application **reconstruit une colonne
   combinée** à partir de l'information disponible (genre + épithète, ou genre
   seul, ou famille seule).
3. Case **« Apparier avec les noms d'auteurs »** : « à cocher seulement si votre
   liste contient les auteurs — plus précis, mais plus lent. Laissez décoché dans
   le doute. »

### Séquence 6 — La correspondance automatique (3 min)

*Le cœur du tutoriel.*

**Écran** : onglet **Auto Match**.

**À montrer, dans l'ordre :**

1. Le curseur **Similarité minimale (%)**, expliqué **avant** de cliquer sur
   Démarrer : « plus haut = moins de faux appariements mais plus de noms à
   réviser à la main ; plus bas = plus d'automatique mais plus de risque
   d'erreur. Par défaut 0,7, et c'est un bon compromis. »
2. Clic sur **Démarrer la correspondance**.
3. La boîte de dialogue **cache du référentiel** : « utilisez la copie en cache
   au quotidien ; téléchargez une copie fraîche après l'ajout ou la révision de
   taxons dans la base. La boîte indique l'âge du cache. »
4. Pendant le traitement, expliquer la **stratégie en cascade** — la vraie valeur
   ajoutée de l'application, à dire lentement :
   - exact sur l'espèce → exact sur le genre → exact sur la famille → exact sur
     un rang supérieur ;
   - puis **approximatif contraint au genre** : « si le genre est reconnu mais
     pas l'épithète, la recherche approximative est limitée aux espèces *de ce
     genre*. C'est ce niveau qui rattrape la plupart des fautes de frappe, et il
     est bien plus sûr qu'une recherche sur tout le référentiel puisque tous les
     candidats sont déjà botaniquement plausibles » ;
   - puis **approximatif complet** en dernier recours.
   - « Chaque nom est traité indépendamment : une même liste peut renvoyer des
     résultats de tous les niveaux. »
5. Les **statistiques en direct** dans la barre latérale (exact / genre /
   approximatif / non apparié) et la barre de progression.
6. Le **tableau de résultats** : montrer une ligne exacte, la ligne avec faute de
   frappe (`genus_constrained` + score), et la ligne synonyme
   (`is_synonym = TRUE`, `accepted_name` renseigné).

**À dire également** (30 secondes, mais cela sauve des utilisateurs) :

- « La progression est sauvegardée automatiquement. Si le navigateur se ferme,
  relancer l'application sur le même jeu de données propose de **reprendre** là
  où vous en étiez — ou de repartir de zéro. » Si possible, **le filmer** :
  fermer l'onglet en cours de traitement, relancer, montrer la fenêtre
  « Correspondance interrompue trouvée ».
- Piège à signaler explicitement : « les quatre niveaux exacts écrivent tous
  `match_method = "exact"`. Le rang réellement apparié est dans la colonne
  `tax_level`. Ne cherchez pas de valeur `exact_genus`, elle n'existe pas. »

### Séquence 7 — La révision manuelle (3 min)

**Écran** : onglet **Review**.

**À montrer :**

1. Le bandeau de statut : total / révisés / restants.
2. Le nom courant en gros, avec `(3 sur 7)`.
3. Le panneau de **suggestions classées** : expliquer le code couleur de la
   qualité — **vert ≥ 90 %, bleu ≥ 70 %** — et rappeler que « quand le genre est
   reconnu, les suggestions sont restreintes aux espèces de ce genre ».
4. Sélection d'une suggestion, puis passage au nom suivant.
5. La **recherche libre dans le référentiel** : champ de nom, filtre de **niveau
   taxonomique** (tous / espèce / genre / famille / ordre / infraspécifique /
   supérieur) et bouton Rechercher. Cas typique : le nom d'entrée est un nom
   vernaculaire ou trop dégradé, on cherche donc autre chose.
6. Le cas **ligne sans nom (NA)** : encadré jaune, et la recherche permet quand
   même d'assigner un identifiant.
7. Le bouton **« Marquer comme non résolu »** : « c'est une décision, pas un
   échec — la ligne sortira avec `match_method = "unresolved"`, ce qui est une
   information exploitable. »
8. Navigation **Précédent / Passer / Suivant** : « Passer laisse le nom pour plus
   tard, Précédent permet de corriger un choix. »

**À dire :**

- « Tout ce que vous validez ici reçoit `match_method = "manual"` et un score
  de 1 : vous saurez toujours, dans le fichier final, ce qui vient de la machine
  et ce qui vient de vous. » ← argument de traçabilité, très parlant pour un
  public scientifique.
- Si tous les noms sont appariés, l'application affiche un bandeau vert et invite
  à passer directement à l'export.

### Séquence 8 — Export (2 min)

**Écran** : onglet **Export**.

**À montrer :**

1. Les formats : **Excel (.xlsx, recommandé)**, CSV, RDS.
2. Les cases **« Inclure les colonnes »** : données d'origine / identifiants
   appariés / noms corrigés / métadonnées de correspondance.
3. L'aperçu du tableau et **les descriptions de colonnes affichées à côté** :
   « elles sont dans l'application, vous n'avez pas besoin de la documentation
   pour les relire. »
4. Le téléchargement, puis **ouvrir le fichier téléchargé à l'écran** — c'est le
   moment où le spectateur comprend le résultat concret.

**À dire, en pointant les colonnes une par une :**

- `idtax_n` = taxon apparié ; `idtax_good_n` = taxon **accepté** — « ils diffèrent
  quand le nom apparié est un synonyme, et c'est presque toujours `idtax_good_n`
  que vous voulez utiliser pour vos analyses ».
- `matched_name` (ce qui a été trouvé) contre `corrected_name` (le nom
  standardisé final) ; puis `is_synonym` et `accepted_name`.
- `match_method` et `match_score` : « c'est votre piste d'audit. Filtrez sur
  `fuzzy` et score faible pour repérer ce qui mérite une seconde lecture. »
- Mentionner l'option **WCVP** de la barre latérale, si elle est visible :
  « cochée, le nom standardisé est remplacé par le nom accepté du World Checklist
  of Vascular Plants de Kew quand le taxon y figure ; une colonne `name_source`
  indique d'où vient chaque nom. »

### Séquence 9 — Enrichissement en traits (2 min)

**Écran** : onglet **Enrichir avec les traits**.

**À dire :**

- « Deuxième mission de l'application. Elle n'a de sens qu'une fois les noms
  standardisés — c'est pour cela que l'onglet est en dernier. »
- Montrer le bandeau « N taxons uniques appariés », puis **Récupérer les traits
  depuis la base**.
- Expliquer l'agrégation : « chaque trait numérique sort en trois colonnes :
  `_mean`, `_sd`, `_n`. Par exemple `wood_density_mean`, `wood_density_sd` et
  `wood_density_n` — le `_n` vous dit sur combien de mesures repose la valeur, ne
  l'ignorez pas. »
- Traits catégoriels : choix entre **valeur la plus fréquente (mode)** et
  **toutes les valeurs concaténées**.
- Point important : « les mesures rattachées aux **synonymes** sont
  automatiquement consolidées sous le taxon accepté — vous ne perdez pas les
  données publiées sous l'ancien nom. »
- Montrer le panneau des sources et citations, puis le téléchargement du tableau
  enrichi.

### Séquence 10 — Clôture (45 s)

**À dire :**

- Récapitulatif en trois phrases : charger → apparier et réviser → exporter (et
  éventuellement enrichir).
- « Fermez l'onglet du navigateur quand vous avez terminé : l'application ferme
  proprement les connexions à la base. »
- Renvoyer vers la documentation : `?launch_taxonomic_match_app` et l'article
  « Utiliser l'Application de Standardisation des Noms Taxonomiques » du site
  pkgdown.
- Mentionner l'alternative programmatique en une phrase :
  `match_taxonomic_names()` et `standardize_taxonomic_batch()` pour les listes
  volumineuses ou les chaînes de traitement reproductibles — « l'application est
  faite pour les listes qui demandent un arbitrage humain ».

---

## Trois conseils de tournage

1. **Ne pas commenter en attendant.** Les phases de correspondance et
   d'enrichissement peuvent durer ; les enregistrer, puis couper au montage et
   poser la voix off sur un accéléré. L'explication de la cascade de
   correspondance mérite mieux qu'un « bon… ça tourne ».
2. **Une prise par séquence.** Les dix séquences ci-dessus sont indépendantes :
   si la séquence 7 rate, seule la 7 est à refaire. Nommer les fichiers OBS en
   conséquence.
3. **Les erreurs sont le meilleur contenu.** Un tutoriel où tout s'apparie du
   premier coup n'apprend rien. Ce sont la faute de frappe rattrapée par la
   contrainte au genre, le synonyme résolu et le nom impossible marqué « non
   résolu » qui font comprendre à quoi sert l'application.
