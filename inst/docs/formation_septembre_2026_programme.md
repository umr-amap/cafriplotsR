# Formation CafriplotsR — samedi 26 septembre 2026 (en ligne)

*Standardiser ses noms d'espèces, récupérer des traits, extraire des données
d'inventaire — sans écrire de code.*

---

## 1. Message d'invitation (à envoyer)

> **Objet : Formation en ligne CafriplotsR — samedi 26 septembre — confirmation d'intérêt**
>
> Bonjour,
>
> Je vous contacte car votre sujet de thèse / postdoc touche de près ou de loin aux
> données d'inventaires, aux listes d'espèces ou aux traits fonctionnels des forêts
> d'Afrique centrale.
>
> J'organise le **samedi 26 septembre**, en ligne et en français, une journée de
> formation à l'utilisation de **CafriplotsR**, un package R développé dans le cadre du
> réseau **Cafriplots** (réseau de réseaux de parcelles d'Afrique centrale).
>
> La journée n'est **pas** une formation à la programmation. Elle est construite autour
> de deux applications à interface graphique (clic, pas de code), sur **vos propres
> données** :
>
> - **Matin — standardiser une liste d'espèces** et l'enrichir automatiquement en traits
>   (densité du bois, forme de croissance et traits foliaires…), avec les
>   références bibliographiques associées. Utilisable ponctuellement, sans compte et sans
>   déposer la moindre donnée.
> - **Après-midi — interroger et extraire des données d'inventaire** : filtrer,
>   cartographier et exporter dans le format adapté à votre question (analyse de croissance,
>   séries multi-recensements, export spatial, format long…). Nous travaillerons sur un
>   **ensemble de parcelles en libre accès**, ouvertes à tous dans la base : rien à importer,
>   rien à préparer de votre côté.
>
> **Public** : doctorant·es et postdoctorant·es. Aucun prérequis en R au-delà de savoir
> installer un package. Groupe volontairement restreint (≈ 10 personnes) pour que chacun·e
> travaille sur ses propres données le matin.
>
> **Si vous avez déjà suivi la formation l'an dernier à Libreville, vous êtes tout aussi
> bienvenu·e.** Le package a beaucoup évolué depuis, et la journée ne sera pas une
> redite : parmi les changements, l'application de standardisation fonctionne désormais
> **sans compte** et même **hors ligne** sur un référentiel mis en cache, les identifiants
> **WCVP** peuvent être ajoutés en sortie à côté des identifiants internes, et chaque
> manipulation faite dans les applications génère le **code R équivalent**, copiable dans
> un script. Revenir permet aussi de repartir avec une liste d'espèces à jour, la
> taxonomie ayant continué d'évoluer de son côté.
>
> **Une seule condition pour participer : avoir installé le package avant la formation**
> (3 lignes à copier dans R, voir plus bas). Lors de la session précédente, la matinée
> entière y est passée. Une aide à l'installation sera proposée dans les jours qui
> précèdent — profitez-en, ne vous y prenez pas la veille.
>
> Le programme détaillé est ci-dessous. **Merci de me répondre avant le [DATE LIMITE]** en
> indiquant :
> 1. si vous souhaitez participer à la **journée entière**, à la **matinée seulement**,
>    ou pas cette fois ;
> 2. si vous disposez d'une **liste d'espèces** (fichier Excel ou CSV) sur laquelle
>    travailler pendant la session ;
> 3. le type de données que vous manipulez, en une ligne — cela m'aidera à adapter les
>    exemples ;
> 4. que vous vous engagez à **installer le package avant la formation**.
>
> Pour vous faire une idée avant de répondre :
>
> - Le réseau Cafriplots : <https://www.cafriplot.net/>
> - Présentation du package, en français : <https://umr-amap.github.io/cafriplotsR/articles/readme-fr.html>
>
> Le lien de connexion et les consignes d'installation seront envoyés quelques jours avant.
>
> Bien cordialement,
> Gilles Dauby — gilles.dauby@ird.fr

---

## 2. Pourquoi cette formation peut vous servir

La formation part de problèmes très concrets, que la plupart d'entre vous rencontrent
déjà :

| Situation courante | Ce que la journée apporte |
|---|---|
| Vous avez compilé une liste d'espèces à partir de plusieurs sources (terrain, herbier, littérature, fichiers de collègues) et les noms ne sont pas écrits pareil | Standardisation automatique + révision assistée des cas douteux |
| Vos richesses spécifiques sont gonflées par des **synonymes** comptés deux fois | Résolution automatique de la synonymie, identifiants de taxons stables |
| Vous devez estimer une **biomasse** et il vous manque la densité du bois pour une partie des espèces | Extraction des traits taxon, avec repli au niveau du genre quand l'espèce est absente |
| Une revue vous demande des noms suivant un **autre référentiel** que celui utilisé jusque-là, ou vous devez confronter votre liste à un jeu de données construit sur un référentiel différent | Passage d'un référentiel à l'autre sans tout refaire : les identifiants internes ne sont jamais écrasés, les identifiants **WCVP** s'ajoutent en colonnes séparées, et une colonne indique quel référentiel a servi pour chaque nom |
| Vous refaites le même nettoyage à chaque nouveau jeu de données | Chaque manipulation faite à la souris génère le **code R équivalent**, copiable dans un script |

### Selon votre thématique

- **Écologie forestière / diversité fonctionnelle** — obtenir une liste d'espèces propre et
  un tableau de traits prêt à l'emploi est en général la première étape (et souvent la plus
  chronophage) d'une analyse de diversité fonctionnelle ou d'assemblages.
- **Carbone et biomasse** — la densité du bois par espèce est un intrant direct des
  allométries de biomasse aérienne ; la couverture d'espèces est ici le facteur limitant, et
  c'est exactement ce que la base cherche à améliorer.
- **Télédétection** — extraction de placettes de terrain avec leurs coordonnées, exportables
  en **shapefile**, pour croiser vérité terrain et produits satellite ou drone ; les
  métadonnées de parcelles (méthode, date, surface) permettent de trier ce qui est
  comparable de ce qui ne l'est pas.
- **Agroforesterie / agronomie** — la standardisation taxonomique et l'accès aux traits
  fonctionnent sur n'importe quelle liste d'espèces ligneuses, pas seulement sur les
  inventaires forestiers.

---

## 3. Programme de la journée

Horaires indicatifs (heure de [FUSEAU HORAIRE À PRÉCISER]), à ajuster selon les participants.

### Matin — Standardisation taxonomique et enrichissement en traits (09h00 – 12h30)

| Horaire | Séquence | Contenu |
|---|---|---|
| 09h00 – 09h15 | Accueil | Tour de table rapide : qui fait quoi, avec quelles données |
| 09h15 – 09h45 | Pourquoi Cafriplots | Le problème de l'accessibilité et de la compilation des données d'inventaire en Afrique centrale ; ce qu'est un « réseau de réseaux » ; ce que contient la base aujourd'hui (> 1 300 inventaires, > 300 000 occurrences, traits taxon et individus) |
| 09h45 – 10h00 | Prise en main | Vérification rapide que le package tourne chez tout le monde, lancement de l'application, connexion en **mode public** (sans compte) ou en **mode hors ligne** |
| 10h00 – 10h30 | Démonstration guidée | Chargement d'une liste d'exemple, appariement automatique, lecture des indicateurs de qualité (`match_method`, `match_score`, synonymie) |
| 10h30 – 10h45 | *Pause* | |
| 10h45 – 11h45 | **Atelier sur vos données** | Chacun·e charge sa propre liste ; appariement, puis révision manuelle des noms douteux (suggestions floues, recherche manuelle, filtres par rang taxonomique) |
| 11h45 – 12h15 | Enrichissement en traits | Ajout des traits au niveau du taxon, formats large et long, panneau **Sources des données** et citations à reprendre en méthodes |
| 12h15 – 12h30 | Export et bilan | Export Excel / CSV / RDS, choix des colonnes, questions |

**À la fin de la matinée**, chaque participant·e repart avec sa liste d'espèces
standardisée, ses identifiants taxonomiques, et — si les taxons sont couverts — un tableau
de traits associé et ses références.

### Après-midi — Interroger et extraire des données de parcelles (14h00 – 17h00)

*Séance optionnelle, en fonction des intérêts exprimés.*

Cette séance se déroule sur un **ensemble de parcelles en libre accès**, ouvertes à tous
dans la base. Aucun import n'est nécessaire : vous manipulez des inventaires réels dès la
première minute, et il n'y aurait de toute façon pas le temps de traiter l'import des
données de chacun·e — cela fera l'objet d'une session dédiée si la demande existe.

> **Un point à comprendre d'emblée :** ce libre accès est un choix, pas la règle générale.
> Cafriplots n'est pas un entrepôt de données ouvert. Chaque utilisateur — ou groupe
> d'utilisateurs — ne voit que ses propres données, plus ce que d'autres ont explicitement
> ouvert. **Si vous déposez vos inventaires, vous en restez maître** : personne d'autre n'y
> accède sans votre accord. Les parcelles utilisées cet après-midi sont précisément celles
> qui ont été ouvertes pour permettre de découvrir l'outil.

| Horaire | Séquence | Contenu |
|---|---|---|
| 14h00 – 14h15 | Reprise | Ce qui distingue l'usage « ponctuel » du matin de l'usage « infrastructure » |
| 14h15 – 15h00 | Construire une requête | Filtres (pays, méthode, surface, période, taxons présents), carte interactive, tableau de métadonnées, sélection des parcelles |
| 15h00 – 15h15 | *Pause* | |
| 15h15 – 16h00 | **Choisir le bon format de sortie** | Le point central de l'après-midi : quel format pour quelle question — format large (une ligne par individu), format long (une ligne par mesure), séries **multi-recensements** (`dbh_census_1`, `dbh_census_2`…), **paires de recensements** pour les taux de croissance, style transect, style complet. Traits taxon et caractéristiques individuelles associés à la demande |
| 16h00 – 16h30 | Export et reproductibilité | Excel multi-feuilles, CSV zippés, RDS, **shapefile** ; onglet **documentation des colonnes** ; récupération du **code R équivalent** pour rejouer la requête dans un script |
| 16h30 – 17h00 | Discussion ouverte | Vos besoins, ce qui manque, les suites possibles : compte personnel, dépôt de vos propres inventaires, ateliers ultérieurs |

---

## 4. Ce que la journée ne couvre pas

Pour éviter tout malentendu :

- Ce n'est pas une formation à R ni aux statistiques.
- L'**import** de vos propres inventaires dans l'infrastructure (assistant d'import, gestion
  des recensements, liaison aux spécimens d'herbier) ne sera pas traité : à dix personnes,
  il n'y a pas le temps de traiter les données de chacun·e. C'est pour cela que
  l'après-midi s'appuie sur des parcelles en libre accès. Une session dédiée à l'import
  pourra être organisée ensuite si la demande existe.
- Aucune donnée personnelle ou non publiée ne vous sera demandée. Les données que vous
  chargez dans l'application de standardisation ne sont **pas** déposées dans la base.

---

## 5. Prérequis techniques

> ### ⚠️ La seule chose réellement exigée : installer le package **avant** la formation
>
> Ce n'est pas une formalité. Lors de la formation précédente, la quasi-totalité des
> participants ne l'avait pas fait, et **la matinée entière est passée en installation** —
> versions de R trop anciennes, absence de Rtools sous Windows, connexions très lentes. Ce
> sont des problèmes qui se règlent bien à l'avance et très mal à dix personnes en direct.
>
> Faites-le **au moins une semaine avant**, et signalez-moi tout blocage : une aide à
> l'installation sera proposée dans les jours qui précèdent.

À faire **avant** le 26 septembre :

```r
# 1. Laisser plus de temps au téléchargement (utile sur connexion lente)
options(timeout = max(3000, getOption("timeout")))

# 2. Installer l'assistant 'remotes' - seulement la première fois
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

# 3. Installer CafriplotsR depuis GitHub
remotes::install_github("umr-amap/cafriplotsR", upgrade = "never")
```

Puis vérifier que tout fonctionne :

```r
library(CafriplotsR)
launch_taxonomic_match_app()
```

Si l'application s'ouvre dans votre navigateur, vous êtes prêt·e. **Faites-le-moi savoir**,
et signalez-moi tout message d'erreur.

Ces mêmes instructions, avec une présentation générale du package en français, sont sur
<https://umr-amap.github.io/cafriplotsR/articles/readme-fr.html>.

**Les trois causes d'échec les plus fréquentes :**

- **Version de R trop ancienne** — il faut **R ≥ 4.1**. Vérifiez avec `R.version.string` ;
  si besoin, réinstallez R depuis <https://cran.r-project.org> *avant* d'installer le
  package. RStudio est recommandé, mais RStudio ne remplace pas R : ce sont deux
  installations distinctes.
- **Rtools manquant (Windows)** — si R propose d'installer des paquets « depuis les sources
  qui nécessitent compilation », répondez **non** (`n`) : cela évite d'avoir besoin de
  Rtools. Si l'installation échoue quand même, installez Rtools correspondant à votre
  version de R depuis <https://cran.r-project.org/bin/windows/Rtools/>.
- **Connexion lente** — l'étape 1 (`options(timeout = ...)`) sert précisément à cela. Si le
  téléchargement s'interrompt, relancez R et reprenez les trois étapes depuis le début.

**Autres points :**

- Une **connexion internet** pendant la session.
- **Aucun identifiant n'est nécessaire** : les deux applications proposent un bouton
  *Se connecter en utilisateur public* (lecture seule), qui donne accès à la taxonomie, aux
  traits et aux parcelles en libre accès utilisées l'après-midi.
- Apporter si possible **votre propre liste d'espèces** (`.xlsx` ou `.csv`), même partielle
  ou imparfaite — c'est le meilleur matériel de travail.

Les interfaces sont **bilingues français / anglais**, basculables à tout moment.

---

## 6. Informations pratiques

| | |
|---|---|
| **Date** | Samedi 26 septembre 2026 |
| **Format** | En ligne — [PLATEFORME ET LIEN À PRÉCISER] |
| **Langue** | Français |
| **Durée** | Journée : matin 09h00–12h30, après-midi 14h00–17h00 (l'après-midi est optionnelle) |
| **Effectif** | ≈ 10 participant·es |
| **Public** | Doctorant·es et postdoctorant·es — y compris celles et ceux ayant suivi la formation de Libreville l'an dernier |
| **Coût** | Gratuit |
| **Prérequis** | Package installé **avant** la séance (voir section 5) |
| **Contact** | Gilles Dauby — gilles.dauby@ird.fr |
| **Réseau Cafriplots** | <https://www.cafriplot.net/> |
| **Présentation du package (FR)** | <https://umr-amap.github.io/cafriplotsR/articles/readme-fr.html> |
| **Code source** | <https://github.com/umr-amap/cafriplotsR> |

**Réponse attendue avant le [DATE LIMITE]** : journée entière / matinée seulement /
pas disponible cette fois, + disposez-vous d'une liste d'espèces ? + une ligne sur vos
données + engagement à installer le package avant la séance.
