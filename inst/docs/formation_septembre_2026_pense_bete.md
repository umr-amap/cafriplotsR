---
title: "Samedi 26 septembre — pense-bête"
subtitle: "L'essentiel dans l'ordre de la journée. Le raisonnement est dans le déroulé animateur ; ici, seulement ce qu'il ne faut pas oublier."
lang: fr
toc: false
format:
  html:
    embed-resources: true
---

## Avant

**Vendredi soir**

- [ ] Zoom : partage d'écran autorisé aux participants, vidéo coupée à l'entrée, **ni code secret ni salle d'attente** — tester le lien depuis une fenêtre privée
- [ ] **`user_test2` repassé en lecture seule** — il peut actuellement *modifier et supprimer* la fiche de 8 parcelles, dont 1188. Deux lignes dans `formation_2026_acces_apres_midi.R`, étape 2a. **À faire avant de distribuer les identifiants**
- [ ] Vérifier que la parcelle **403**, visible elle aussi par ce compte, est bien à vous
- [ ] Les **trois formats testés** dans l'application en étant connecté comme `user_test2` (456 / ~892 / 436 lignes). Si les sorties ne diffèrent pas, la séquence de l'après-midi tombe
- [ ] Les identifiants `user_test2` notés quelque part de lisible **pour la salle** — ils ne sont pas dans le programme envoyé
- [ ] Cache de référentiel **reconstruit** — sinon votre écran ne correspond plus au corrigé sur la ligne `Fabaceae`
- [ ] Vérifier que les idtax **5015** et **4365** sont liés à WCVP dans `taxa_backbone_link` — sans le lien, la démo de 11h45 n'affiche rien et ne le dit pas
- [ ] Listes d'espèces reçues : en faire tourner **trois ou quatre**, noter pour chacun le **nombre de synonymes résolus**
- [ ] Clé USB avec les deux `.rds`, construits avec **la version que les participants ont installée**
- [ ] Antisèche A4 envoyée au contact, à tirer en dix exemplaires
- [ ] Numéro de téléphone de secours, **des deux côtés**
- [ ] Message séparé aux deux habitués : circuler, relayer les blocages, trois minutes de témoignage vers 11h00

**08h40 — vous êtes connecté**

- [ ] **Une seule machine porte le son**, celle du projecteur. Toutes les autres : micro *et* haut-parleurs coupés. C'est la panne qui tue la séance en trente secondes
- [ ] Notes de présentateur ouvertes **dans une fenêtre séparée**, pas dans celle que vous partagez
- [ ] Partager la **fenêtre du navigateur des slides**, pas l'écran entier

---

## Matin

### 09h00 – 09h15 · Accueil

- Une seule question à chacun : **quel est ton dernier fichier de noms d'espèces, et d'où vient-il ?**
- **Noter les réponses** — c'est le matériau de toute la matinée
- Demander aux deux habitués de **ne pas répondre les premiers**

### 09h15 – 09h40 · Raison d'être du package

- **Ne jamais appuyer sur `p`** — en écran partagé, les notes s'afficheraient devant la salle. Flèches uniquement
- Les trois questions dans les **sept premières minutes** :
  - **Q1** combien de noms différents, combien d'espèces ? → deux colonnes au paperboard, on y revient à 10h45
  - **Q2** *Aningeria altissima* / *Pouteria altissima* : **une espèce, ou deux ?** (espèces, pas arbres)
  - **Q3** qui sait **quel référentiel** a servi à établir sa liste ? Très peu de mains — c'est le but
- Slide **7, « Compiler n'est pas le seul problème »**, annonce les trois problématiques. Sa ligne du bas est l'architecture de la matinée : **« le premier, un package peut le résoudre ; les deux autres demandent un réseau »**
- Si vous êtes en retard, **garder la 7 et sauter les 8 et 9** : leur contenu y est annoncé, la salle ne sent pas le trou. L'inverse — sauter la 7 et garder les deux autres — recrée la coupure qu'elle corrige. Ne pas sacrifier le bloc C
- Finir sur « on ouvre l'application »

### 09h40 – 10h00 · Mise en route

- Utilisateur public **ou** hors ligne
- **Point de contrôle ferme à 10h00** : le dépannage s'arrête, les non-résolus passent en binôme avec un voisin
- Clé USB pour ceux qui n'ont pas de cache
- La phrase à glisser, au moment où ils tapent la seule ligne de R :

  > « C'est la seule ligne de R que vous taperez aujourd'hui, et c'est voulu — l'outil est fait pour ça. Pour ceux qui veulent du R, on y revient en fin de matinée, et vous repartirez avec un script qui tourne sur votre fichier. »

### 10h00 – 10h30 · Démonstration guidée

Fichier d'exemple **1**, feuille `inventaire` : 38 lignes, **36 noms distincts** (un doublon
exact, une cellule vide). Vous pilotez, ils regardent. Corrigé complet dans
`formation_2026_liste_especes_exemple_corrige.md`.

**Attendu à l'écran, au seuil par défaut de l'application (0,60) :** 27 `exact`,
2 `genus_constrained`, 8 `no_match`, 1 cellule vide — et **4 synonymes résolus**
(*Aningeria* → *Pouteria*, *Chlorophora* → *Milicia*, *Celtis durandii var. ugandensis* →
*Celtis gomphophylla*, *Polyalthia* → *Greenwayodendron*). Si vous voyez 5 synonymes, le
cache n'a pas été reconstruit : le cinquième était `Fabaceae → Fabaceae`.

**① 10h00 — Avant de lancer quoi que ce soit (3 min).** Afficher le fichier brut et
**faire compter la salle** : combien de lignes, combien d'espèces ?

- `Chlorophora excelsa` et `Milicia excelsa` sont deux lignes, deux arbres, **un seul
  taxon** — l'iroko. Elles se suivent, la salle peut les trouver
- Puis reposer la question : y en a-t-il d'autres ? **`Polyalthia suaveolens` (ligne 37) et
  `Greenwayodendron suaveolens` (ligne 32)** sont le même taxon. Cinq lignes d'écart, deux
  genres qui ne se ressemblent pas — personne ne les trouve
- Le point à faire entendre : **l'épithète est identique, le genre non.** Un tri
  alphabétique sur la colonne des noms ne les rapproche pas
- C'est la question 1 de 09h15 qui se rembourse ici, sur un fichier réel

**② 10h05 — Lancer, puis lire les quatre colonnes (5 min).** `match_method`,
`match_score`, le nom accepté, l'identifiant. Ne pas commenter les 26 exacts.

**③ 10h10 — Ce qui est rattrapé sans qu'on l'ait demandé (3 min).** Vite, mais le montrer :
c'est ce qui évite à chacun de nettoyer sa colonne à la main lundi.

- `TERMINALIA SUPERBA`, `musanga cecropioides` — casse
- `Celtis  mildbraedii` — double espace · `Pycnanthus_angolensis` — underscore
- `Dacryodes cf. buettneri`, `Santiria aff. trimera`, `Strombosia ? pustulata` — les marques de doute
- `Lophira alata Banks ex C.F.Gaertn.` — les auteurs

**④ 10h13 — Les niveaux autres que l'espèce (3 min).** `Sterculia sp.` et `Cola` → genre,
`Annonaceae` → famille, `Magnoliopsida` → classe.

> **Q4** — « Sterculia sp. » dans votre tableau : qu'est-ce que l'outil devrait en faire ?

Un appariement partiel **est un résultat**, pas un échec : la ligne reste utilisable pour
tout ce qui ne demande pas l'espèce.

**⑤ 10h16 — Ce que l'application accepte sans vous demander votre avis (5 min).** Le plus
important des trente minutes, parce qu'il prépare l'atelier.

- L'onglet **Révision** ne liste que les noms **sans identifiant** — ici **8**. Tout ce qui
  a été apparié, même mal, n'y apparaît jamais
- Or deux lignes ont été acceptées **sous 0,95, en silence** :
  - `Entandrophragma cylindricm` → **0,88**, et c'est juste
  - **`Uapaca guineensis var. guineensis` → 0,68 — et ce nom n'existe pas.** L'autonyme n'a jamais été créé : le référentiel interne ne connaît que `var. bingervillensis`, WCVP que `var. sudanica`. L'outil ne dit pas « ce nom n'existe pas », il l'apparie à l'espèce, lui donne un identifiant, et passe à la suite
- **Le contre-exemple est deux lignes plus haut** : `Celtis durandii var. ugandensis` est un **vrai** nom infraspécifique. Il sort en **exact 1,00** et se résout sur *Celtis gomphophylla*. Les deux cas sont côte à côte dans le fichier — c'est le score, et lui seul, qui les distingue
- C'est systématique, si quelqu'un demande : tout autonyme inventé retombe sur l'espèce à un score médiocre. `Garcinia kola var. kola` → 0,61, `Milicia excelsa var. excelsa` → 0,65
- **Le geste à leur transmettre : trier par `match_score` croissant et regarder le bas du
  tableau.** C'est là que sont les décisions prises à votre place — pas dans l'onglet
  Révision. C'est l'habitude la plus utile de toute la matinée pour leurs propres fichiers
- **Puis monter le curseur à 0,75 et relancer** : `Uapaca` disparaît des appariés et
  **apparaît dans la file de révision**, qui passe de 8 à 9

> Le seuil ne décide pas de la qualité de l'appariement. Il décide de **ce sur quoi on vous
> consulte**. Au-dessus, la décision est prise pour vous, sans le dire.

**⑥ 10h21 — Ce que l'appariement flou rattrape, et ce qu'il ne rattrape pas (3 min).**

- `Entandrophragma cylindricm` — une lettre manquante → **0,88**, rattrapé
- `Triplochyton scleroxilon` — deux erreurs dont une sur le genre → **rien**
- `Picnatus angolens` — le genre est méconnaissable (*Pycnanthus*) → **rien** non plus
- La formule : **l'appariement approximatif rattrape la faute de frappe, pas la faute de
  mémoire**
- Et le bon comportement : **il rejette plutôt que d'inventer.** À 0,30, ce même
  `Picnatus angolens` serait apparié à *Searsia angolensis*, une Anacardiaceae, là où on
  visait une Myristicaceae. Le seuil par défaut vous en protège

**⑦ 10h24 — Ce qu'aucun outil ne peut deviner (4 min).**

- **`Azobé`** et **`Moabi`** ne s'apparient pas — et pourtant ce sont *Lophira alata* et
  *Baillonella toxisperma*, **toutes deux présentes ailleurs dans le fichier** sous leur nom
  scientifique. La standardisation ne remplace pas la connaissance de terrain, elle la met
  en forme
- **`G. dewevrei`** échoue alors que `Gilbertiodendron dewevrei` passe deux lignes plus
  haut : l'abréviation va de soi dans un texte, c'est une perte d'information dans un
  tableau
- `Arbre indéterminé 12`, `Morpho sp. 4`, `Gilbertiodendron inexistens` — les non-noms, et
  l'outil a raison de ne rien forcer
- **Les 8 de la file de révision, en entier** : `Arbre indéterminé 12`, `Triplochyton
  scleroxilon`, `Picnatus angolens`, `Azobé`, `Moabi`, `G. dewevrei`, `Gilbertiodendron
  inexistens`, `Morpho sp. 4`

**⑧ 10h28 — La bascule vers l'atelier (2 min).**

> **Q5** — Il vous manque la densité du bois pour 30 % de vos espèces. Qu'est-ce que vous
> faites aujourd'hui ?

Les laisser décrire leur pratique réelle. On y revient à 11h45.

**Si vous êtes en retard**, sacrifier dans cet ordre : ③ le bruit rattrapé, puis ⑥ les deux
fautes, puis ⑦ les vernaculaires. **Ne jamais sacrifier ① ni ⑤** — le comptage ouvre la
matinée, la révision manuelle est ce qu'ils vont faire pendant l'heure suivante.

### 10h30 – 10h45 · Pause

### 10h45 – 11h45 · Atelier, sur leurs données

- **Le cœur de la matinée.** Les relais circulent
- Annoncer à chacun **son** nombre de synonymes résolus
- Vers 11h00 : les **deux témoignages** de trois minutes
- Les trois cas à absorber :
  - **sans fichier** → `formation_2026_liste_especes_exemple2.xlsx`, 33 lignes, colonne `Nom_terrain`. Attendu à 0,60 : **30 `exact`, 1 `genus_constrained`, 2 `no_match`, 1 synonyme**. Trois moments : **lignes 16 et 33**, `Greenwayodendron suaveolens` et `Polyalthra suaveolens`, même espèce (idtax 219) ; **`Polyalthra suaveolens` accepté à 0,73 en silence**, comme l'*Uapaca* de 10h16 — le même réflexe de tri par score s'applique ; **ligne 1 `Bois mort debout`**, `no_match` légitime, l'outil ne force rien
  - pour aller plus loin avec lui : **baisser le curseur à 0,30**. `Picnatus angolens`, honnêtement rejeté à 0,60, devient alors *Searsia angolensis* — **Anacardiaceae**, quand le nom visé est *Pycnanthus angolensis*, **Myristicaceae**. **Baisser le seuil ne trouve pas plus, ça invente plus**
  - **fichier 100 % accepté** → c'est un résultat : « noms = espèces, vous le savez maintenant, et vous avez les identifiants ». Puis l'envoyer sur les traits
  - **gros fichier** → au-delà de deux cents noms, travailler sur une tranche
- La réponse à « mon fichier est déjà propre », en trois étages :
  1. **Les fautes de frappe** — corrigeables soi-même, long mais faisable. L'outil fait gagner des heures
  2. **La synonymie** — pas corrigeable : quel que soit le temps qu'on y passe, un nom périmé reste parfaitement orthographié. L'outil fait ce qu'on ne peut pas faire
  3. **La jointure entre deux sources** — le plus coûteux. Dès qu'on rapproche deux jeux de données, la taxonomie **cesse d'être un libellé et devient une clé**. À la main c'est faisable, mais **les erreurs ne se voient pas** : un nom non apparié sort silencieusement du tableau, deux noms rapprochés à tort fusionnent deux espèces. Le résultat **a l'air fini**
- La formule : **une coquille se voit ; une jointure ratée produit un tableau propre et faux**
- Si quelqu'un dans la salle a déjà compilé des données, **faire raconter son histoire** plutôt que la vôtre

### 11h45 – 12h10 · Traits, référentiels et export

- Traits au niveau du taxon. Le panneau produit **trois onglets côte à côte** : *Wide Format (Aggregated)*, *Long Format (Detailed)*, *Data Sources* — chacun avec son bouton de téléchargement
- **Q6** à quoi sert un identifiant de taxon, qu'un nom ne sait pas faire ? → **un nom sert à désigner, un identifiant sert à joindre.** La question paraît byzantine tant qu'on n'a pas eu à croiser deux jeux de données ; dès qu'on en a eu besoin, c'est la seule qui compte
- **Q8 — les deux onglets ouverts côte à côte :** dans le format large, la densité du bois de cette espèce est **une seule valeur. D'où sort-elle ?**
  - Ne **pas** demander « que comprenez-vous par format long ? » : une définition demandée à froid ne produit que du silence. Celle-ci a une réponse qu'on atteint par le raisonnement
  - **La réponse** : c'est une **moyenne**. Le format large est une *agrégation* — `mean`, `sd`, `n` par trait. Les *n* mesures moyennées sont dans l'onglet long, une par ligne. **Le nombre du format large n'est pas une mesure, c'est un résumé de mesures**
  - Relance 1 : *et si ces n mesures viennent de trois sources qui ne sont pas d'accord ?* → l'onglet **Sources**, et la raison de son existence
  - Relance 2 : *lequel ouvrez-vous dans Excel, lequel donnez-vous à R ?* → le large se lit, le long se calcule, aucun n'est « le bon »
  - **Le principe à poser ici**, parce qu'il resservira à 14h : **un format large n'a nulle part où ranger ce qui est attaché à chaque mesure.** Ce matin il perd les *n* et leur source ; cet après-midi il perdra la hauteur de mesure — et l'arbre qui « rétrécit » de 31 cm
- **Q7** vous envoyez ce tableau à un collègue — qu'est-ce qui doit y figurer pour qu'il le refasse sans vous ?
- *Dacryodes* / *Pachylobus*, deux minutes, **sur votre écran seulement** — le sélecteur de référentiel n'existe pas hors ligne
- Le **panneau de code R**, une fois, sur un résultat réel :
  - il utilise `load_backbone_cache()`, donc **il tourne sans connexion**
  - **lire à voix haute** l'encadré : la révision manuelle n'a pas d'équivalent programmatique, les noms non résolus restent `idtax_n = NA`
  - l'exercice à donner : **relancer ce script lundi sur un second fichier**

### 12h10 – 12h30 · Bilan

- Import, gestion de ses données, `query_plots` : **dix minutes de mots, aucune démonstration**
- Qui reste l'après-midi — et dire à ceux qui voulaient du R que la permanence est faite pour ça

---

## Après-midi · 14h00 – 16h30

- **Une permanence, pas un cours.** Ils apportent leur question
- **Donner les identifiants `user_test2` au début** — contrairement au matin, ce n'est plus le bouton « utilisateur public » : il faut taper un login. Le prévoir avant que dix personnes cherchent où cliquer. Le compte voit **huit parcelles**, pas une
- `query_plots` **exige une connexion** — pas de mode hors ligne. Si le réseau est mort : démonstration sur votre écran, ou discussion
- La séquence préparée, sur la parcelle **1188** : le même jeu en **large** (456 lignes), **long** (~892), **paires de recensements** (436)
- **Rappeler Q8 de 11h45** : c'est le même principe, sur un autre objet. Le matin, le format large perdait les *n* mesures et leur source ; ici il perd la hauteur de mesure. Seconde occurrence, pas sujet neuf
- Le cas à sortir : **tag 344, −31,6 cm en trois ans**. Pas une erreur — mesuré à **1,30 m en 2021 et 3,37 m en 2024**

  > « L'information qui vous sauve est attachée à la mesure, pas à l'arbre — et c'est exactement ce que le format large ne peut pas vous montrer. »

- Dire aussi que la parcelle est **saine** : 353 arbres sur 436 entre 0 et 3 cm
- **Ne pas prévoir 17h00**

---

## À ne pas faire

- **Appuyer sur `p`** pendant la présentation
- **Promettre la souveraineté sans nuance.** Aujourd'hui la fiche de parcelle est protégée, pas les lignes d'arbres en dessous. Si la question vient : « l'accès est géré parcelle par parcelle ; aujourd'hui c'est la fiche qui est protégée, pas encore les arbres — c'est un chantier en cours »
- **Démontrer l'assistant d'import**, les subtilités multi-référentiels au-delà du seul exemple, ou quoi que ce soit qui suppose un compte

---

## Si ça tourne mal

| Ce qui casse | Ce que vous faites |
|---|---|
| Connexion de la salle morte | La matinée tient **hors ligne**. C'est tout l'intérêt du cache |
| Larsen | Couper micro *et* son sur toutes les machines sauf celle du projecteur |
| Votre flux tombe | Téléphone. Un atelier où ce sont eux qui manipulent tient à l'audio seul |
| Quelqu'un sans cache | Clé USB, puis `get_backbone_cache_path()` pour savoir où déposer les deux `.rds` |
| Plus de la moitié bloquée à 10h00 | On avance quand même, en binômes. Le dépannage ne mange pas l'atelier |
| `query_plots` inaccessible à 14h00 | Votre écran partagé, ou discussion. C'est la partie optionnelle, le risque est au bon endroit |

---

## Dimanche 27

- [ ] **Retirer 1188 de `user_test2`** — `formation_2026_acces_apres_midi.R`, étape 4
- [ ] Puis `deactivate_user(con, "user_test2")`. Ce compte fait partie des neuf que **P0.7** du plan de sécurité prévoit de supprimer : le réutiliser a reporté cette suppression, ne pas la perdre de vue
