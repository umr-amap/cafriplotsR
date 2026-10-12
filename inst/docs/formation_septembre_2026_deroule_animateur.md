---
title: "Formation CafriplotsR — déroulé animateur"
subtitle: "Samedi 26 septembre 2026 — à distance, participants réunis en salle à Douala"
lang: fr
toc: true
format:
  html:
    embed-resources: true
---

Document de travail interne, pour l'animation. Deux documents vont aux participants :
`formation_septembre_2026_programme.md` (l'invitation d'août, qui a servi aux
inscriptions) et `formation_septembre_2026_programme_jour.md`, le programme de la journée
envoyé le vendredi 25 avec le lien Zoom et les consignes de salle.

## 1. Les contraintes qui ont dicté ce plan

Elles sont notées ici pour que les choix qui suivent restent relisables dans six mois.

- Formation à distance, mais les participants sont tous réunis dans une même salle à
  Douala. Connexion de la salle potentiellement mauvaise.
- La matinée se fait avec tout le monde ; une partie du groupe ne sera plus là
  l'après-midi.
- Niveau en R très variable, du quasi-débutant à l'utilisateur confirmé.
- Deux participants connaissent déjà le package et l'utilisent ponctuellement, sans être
  allés loin.
- Temps limité, et il ne s'agit pas a priori de personnes qui alimenteront Cafriplots avec
  leurs données à court terme.

Conséquence principale : la journée vise un outil que chacun puisse réutiliser seul dès la
semaine suivante, pas une présentation de l'infrastructure.

## 2. Le dispositif technique

### Zoom, un lien, cinq connexions — et le projecteur en plus

Une connexion par binôme, soit cinq au maximum. Caméras coupées de part et d'autre. Par
défaut vous partagez votre écran ; quand quelqu'un est bloqué, il partage le sien et vous
le guidez.

Les cinq connexions ne sont pas négociables, et c'est le point à tenir si la salle propose
de tout regrouper sur le poste du projecteur « pour faire plus simple ». Elles suppriment
le point aveugle de l'animateur à distance : vous voyez l'écran de celui qui a le
problème, au lieu de le deviner par description. Un poste unique relié au vidéoprojecteur
vous rendrait aveugle à la salle pendant l'heure d'atelier, qui est la seule chose dont
les participants se souviendront lundi. Elles ont aussi un avantage de lisibilité — chaque
binôme voit votre démonstration sur son propre écran, à cinquante centimètres, plutôt que
sur une projection à l'autre bout de la salle.

Cela dit, la salle a un projecteur, et s'en priver serait dommage. Les deux dispositifs ne
s'opposent pas, ils ne servent pas aux mêmes moments :

- **Le projecteur, pour les séquences en plénière** — la raison d'être du package
  (09h15–09h40), la démonstration guidée (10h00–10h30), le bilan (12h10–12h30). Ce sont
  les moments où tout le monde regarde la même chose et où personne ne manipule : une
  image commune au mur tient la salle ensemble mieux que cinq écrans qui regardent chacun
  dans leur coin.
- **Les écrans individuels, pour l'atelier** (10h45–11h45) et pour tout ce qui suppose
  qu'un participant vous montre son problème. Là, le mur ne sert plus à rien et le partage
  d'écran est tout.

Concrètement : un des deux relais apporte sa machine et la branche au projecteur, cette
machine rejoint le Zoom comme les autres, et ce qu'elle affiche est simplement votre
partage d'écran agrandi. Rien de particulier à configurer. Vérifier seulement la sortie
vidéo — HDMI, VGA, ou adaptateur à prévoir si la machine n'a que de l'USB-C.

Ce poste est aussi le candidat naturel pour porter l'audio, voir ci-dessous.

### Le point à ne pas rater : l'audio

Cinq machines connectées au même Zoom dans la même pièce produisent un larsen permanent.
C'est la panne la plus banale de ce type de configuration, et elle rend la séance
impraticable en trente secondes.

Deux solutions, à trancher avant samedi :

- **Un seul poste porte l'audio.** Une machine désignée garde micro et haut-parleurs
  actifs ; les quatre autres rejoignent le Zoom avec micro coupé *et* son coupé (dans Zoom :
  rejoindre sans audio). Elles ne servent qu'à l'image. Simple, gratuit, mais toute la
  salle parle dans un seul micro. **Désigner la machine branchée au projecteur** : elle est
  déjà identifiée, déjà placée devant, et c'est une consigne de moins à expliquer.
- **Chacun son casque.** Les cinq postes gardent l'audio, chaque binôme avec un casque
  filaire. Plus confortable pour le dialogue, mais il faut cinq casques dans la salle et
  que personne ne les retire en cours de route.

À défaut de certitude, retenir la première et l'annoncer dans le mail : c'est celle qui ne
dépend pas du matériel apporté.

### Réglages Zoom à vérifier la veille

Le lien : <https://umontpellier-fr.zoom.us/j/97678683045> (ID 976 7868 3045). Il sert le
matin et l'après-midi, et il est dans le programme du jour.

- Autoriser le partage d'écran par les participants (par défaut, Zoom le réserve à
  l'animateur).
- Désactiver la vidéo à l'entrée.
- **Vérifier qu'il n'y a ni code secret ni salle d'attente.** Le lien n'embarque pas de
  `?pwd=` : s'il en faut un, le lien seul ne suffira pas et il faut l'annoncer. Une salle
  d'attente active oblige à admettre cinq connexions une par une pendant que la séance
  démarre.
- Ouvrir la réunion à 08h40, pas à 09h00.
- Récupérer le lien et l'envoyer avec le programme, pas le matin même.
- Prévoir un numéro de téléphone de secours des deux côtés : si la visio tombe, l'audio
  par téléphone suffit à faire tenir un atelier où ce sont eux qui manipulent.

### L'antisèche papier

Une page A4 imprimée, posée sur chaque table : la ligne de lancement de l'application, le
bouton « se connecter en utilisateur public », le bouton hors ligne, les quatre colonnes à
regarder en sortie, à qui écrire en cas de blocage.

Le papier est immunisé contre la connexion et contre le fait d'avoir décroché trois
minutes. C'est aussi ce qui permet à un débutant de rattraper sans interrompre le groupe.
Envoyer le PDF au contact sur place pour qu'il le tire en dix exemplaires.

## 3. Le filet de sécurité

### La matinée doit pouvoir tourner sans internet

C'est possible, et c'est le point le plus important du dispositif. L'application de
standardisation dispose d'un mode hors ligne, adossé à un référentiel mis en cache
localement : deux fichiers dans le dossier que renvoie `get_backbone_cache_path()`.

```
backbone_cache.rds       ~12,7 Mo
backbone_metadata.rds    ~1 Ko
```

Le cache s'écrit automatiquement la première fois qu'un appariement tourne en ligne. D'où
la formulation du devoir à donner aux participants : non pas « installez le package »,
mais « installez-le et faites tourner un appariement une fois ». L'essai à blanc *est* le
téléchargement du cache. Une fois le cache présent, le bouton « utiliser hors ligne »
apparaît sur l'écran de connexion.

Deux réserves :

- La clé USB règle le cache, pas l'installation. Installer exige une connexion et doit
  être fait avant samedi. Ne pas promettre le contraire.
- Le cache porte un numéro de version (`.backbone_cache_version`, aujourd'hui `1.1`). Une
  version du package qui le fait passer à `1.2` invalide tous les caches construits avant,
  qui redemanderont alors le réseau. Ne plus toucher à cette constante une fois que les
  participants ont commencé à construire leur cache.

### La clé USB

Une clé dans la salle avec les deux `.rds`, pour ceux qui arriveront sans cache. Les
fichiers sont portables : rien n'y est propre à une machine. La commande
`get_backbone_cache_path()` indique à chacun où les déposer.

Condition : la clé doit contenir un cache construit avec la même version du package que
celle installée par les participants, sinon il sera rejeté à la lecture.

### `query_plots` n'a pas de mode hors ligne

L'application de l'après-midi dispose du bouton utilisateur public mais pas du mode hors
ligne. C'est donc la seule partie de la journée qui dépende réellement du réseau — et
c'est aussi la partie optionnelle, celle dont l'échec coûte le moins. Le risque est placé
au bon endroit.

Si la connexion est morte à 14h00 : démonstration sur votre seul écran partagé, ou
discussion.

## 4. Matinée — 09h00 à 12h30 (heure de Douala)

Le 26 septembre, la France est à UTC+2 et le Cameroun à UTC+1 : 09h00 à Douala,
c'est 10h00 en France. Annoncer les horaires en heure de Douala, c'est là qu'est le
public.

| Horaire | Séquence | Ce qui s'y joue |
|---|---|---|
| 09h00 – 09h15 | Tour de table | Une seule question à chacun : quel est ton dernier fichier de noms d'espèces, et d'où vient-il ? Noter les réponses : c'est le matériau qui rendra concret tout le reste de la journée. |
| 09h15 – 09h40 | Raison d'être du package | La présentation ressac resserrée, mais **dans l'ordre inverse** : on part de leur fichier, le réseau arrive en fin de parcours. Questions 1 à 3 dans les sept premières minutes. Déroulé minute par minute dans `formation_septembre_2026_presentation_raison_detre.md`. |
| 09h40 – 10h00 | Mise en route | Tout le monde ouvre l'application, en utilisateur public ou hors ligne. Point de contrôle ferme : à 10h00 le dépannage s'arrête, les non-résolus passent en binôme avec un voisin. Une phrase en passant sur la seule ligne de R de la matinée, pour ceux qui sont venus apprendre R (voir plus bas). |
| 10h00 – 10h30 | Démonstration guidée | Sur le fichier d'exemple volontairement sale. Vous pilotez, ils regardent. Questions 4 et 5. |
| 10h30 – 10h45 | Pause | |
| 10h45 – 11h45 | Atelier sur leurs propres données | Le cœur de la matinée. Les relais circulent dans la salle. Les deux témoignages de trois minutes se placent ici, quand les premiers résultats tombent. Repli et cas particuliers ci-dessous. |
| 11h45 – 12h10 | Traits, référentiels et export | Enrichissement au niveau du taxon, formats de sortie, panneau des sources et citations. Questions 6 et 7. Deux minutes sur le cas *Dacryodes* / *Pachylobus* : même fichier, deux référentiels, deux listes d'espèces, un identifiant qui ne bouge pas (voir le corrigé). Sur votre écran seulement — le sélecteur de référentiel n'existe pas hors ligne. Puis le panneau de code R, montré une fois, sur un résultat réel : c'est la réponse à ceux qui sont venus apprendre R (voir plus bas). |
| 12h10 – 12h30 | Bilan et suite | Ce qui existe au-delà (import, gestion de ses données, `query_plots`) en dix minutes de mots, sans démonstration. Et qui reste l'après-midi. |

### L'atelier se fait sur leurs données, pas sur le fichier d'exemple

La question s'est posée de faire travailler tout le monde sur le fichier sale, pour une
heure prévisible. Elle est tranchée : **on garde leurs propres fichiers.**

**L'objection était qu'un participant puisse arriver avec un fichier déjà propre, sur
lequel il ne se passerait rien.** Ce cas n'existe quasiment pas, et c'est vérifié plutôt
que supposé. Une liste de quarante espèces d'Afrique centrale, toutes correctement
orthographiées, telles qu'elles circulent dans la littérature forestière, passée dans la
chaîne d'appariement : quarante appariements exacts, et **quinze synonymes**, soit 38 %.
*Combretodendron macrocarpum* devient *Petersianthus macrocarpus*, *Enantia chlorantha*
devient *Annickia chlorantha*, *Fagara heitzii* devient *Zanthoxylum heitzii*, *Guarea
cedrata* devient *Leplaea cedrata*, *Oxystigma oxyphyllum* devient *Prioria oxyphylla*.

Le chiffre n'est pas un taux mesuré — quelques cas célèbres avaient été glissés dans la
liste à dessein. Mais environ la moitié des résultats n'avait pas été anticipée, ce qui est
exactement la situation du participant.

La leçon tient en deux phrases : **les fautes de frappe, on peut les corriger soi-même —
c'est long et fastidieux, mais faisable. La synonymie, non : quel que soit le temps qu'on
y passe, un nom périmé reste parfaitement orthographié et rien ne le signale.**

Les deux ne sont donc pas du même ordre, et l'outil ne rend pas le même service. Sur les
coquilles, il fait gagner des heures sur un travail qu'on aurait pu faire. Sur la
synonymie, il fait ce qu'on ne peut pas faire du tout. C'est la seconde moitié qui justifie
l'outil ; la première, qui le rend agréable.

Quand quelqu'un dit « mon fichier est propre », il veut dire qu'il a payé le prix de la
première moitié. Il n'a pas touché à la seconde, et il ne le sait pas. Et la question 3,
posée vingt minutes plus tôt, aura établi que presque personne ne sait quel référentiel a
servi à établir sa liste. C'est précisément pour cela que la synonymie y est encore.

**Et il y a un troisième étage, qui est le plus coûteux de tous.** Tant qu'on reste dans un
seul fichier, la taxonomie n'est qu'un libellé : un nom mal écrit est laid, un nom périmé
est faux, mais le tableau tient debout. Dès qu'on en rapproche deux — son inventaire et une
base de traits, deux campagnes de terrain, sa liste et celle d'un collègue — **la taxonomie
cesse d'être un libellé et devient une clé de jointure.** Et une clé qui n'est pas
standardisée ne joint pas.

À la main, c'est encore faisable. Mais le coût ne croît pas avec le nombre de noms, il
croît avec le nombre de sources — et surtout, **les erreurs de cette étape-là ne se voient
pas** :

- un nom qui ne s'apparie pas **sort silencieusement** du tableau croisé ; personne ne
  compte les lignes perdues ;
- deux noms rapprochés à tort **fusionnent deux espèces** en une, et la richesse baisse
  sans qu'on sache pourquoi ;
- un arbitrage fait un mardi et un autre fait un jeudi ne suivent pas forcément la même
  règle, et rien n'en garde trace.

Dans les trois cas, le résultat **a l'air fini**. C'est ce qui rend cette erreur-là plus
grave qu'une faute de frappe : une coquille se voit, une jointure ratée produit un tableau
propre et faux.

C'est aussi la réponse à la question 6 — *à quoi sert un identifiant de taxon, qu'un nom ne
sait pas faire ?* Un nom sert à désigner ; un identifiant sert à **joindre**. Tant qu'on
n'a pas eu besoin de croiser deux jeux de données, la question paraît byzantine. Dès qu'on
en a eu besoin, elle est la seule qui compte. Si quelqu'un dans la salle a déjà compilé des
données, c'est son histoire qu'il faut faire raconter ici plutôt que la vôtre.

**Ce que coûterait le fichier commun.** D'abord une redite : la démonstration guidée de
10h00 vient de faire tourner ce fichier pendant trente minutes. Ensuite, et c'est le point
décisif, la disparition de la friction — leurs noms de colonnes, leur encodage, leurs
cellules fusionnées, leur arbitrage sur un appariement à 0,68. Cette friction *est* le
transfert : c'est elle qui rend quelqu'un capable de recommencer seul le lundi. Le fichier
d'exemple, lui, a été construit pour que tout se passe bien. Enfin le paperboard de 9h15
porte leurs deux chiffres à eux, et l'atelier est ce qui remplit la seconde colonne.

**Les trois cas à absorber.**

1. **Celui qui vient sans fichier.** Il y en aura, et il a désormais son propre jeu :
   `formation_2026_liste_especes_exemple2.xlsx`, envoyé avec le programme. 33 lignes,
   colonne `Nom_terrain` — c'est elle seule qu'on standardise, `Genre` et `Espece` ne sont
   là que comme colonnes accompagnantes, ce qui est déjà un enseignement : un fichier de
   terrain porte autre chose que des noms, et on le retrouve intact en sortie.

   Une liste de terrain réaliste, mélangeant miombo (*Brachystegia*, *Julbernardia
   paniculata*, *Isoberlinia*, *Pterocarpus angolensis*) et forêt dense (*Scorodophloeus*,
   *Coelocaryon*, *Anonidium*). Résultat vérifié le 25/09 **au seuil de l'application
   (0,60)** : **30 `exact`, 1 `genus_constrained`, 2 `no_match`, 1 synonyme résolu.**

   > Attention au seuil quand on rejoue ces chiffres en console :
   > `standardize_taxonomic_batch()` prend `min_similarity = 0.3` par défaut, l'application
   > `0.6` (`mod_auto_matching.R:26`). Les deux ne donnent pas le même tableau.

   **Trois moments à exploiter, dans cet ordre.**

   - **Lignes 16 et 33 — la même espèce deux fois.** `Greenwayodendron suaveolens` en 16,
     `Polyalthra suaveolens` en 33 : faute de frappe *et* synonyme, qui se résout sur
     idtax 48718 → **219**, soit exactement la ligne 16. Deux noms, une espèce, et
     l'identifiant le prouve. C'est la question 1 du matin qui se referme, sur leur écran
     et non sur un exemple. Les deux lignes sont assez éloignées pour qu'on ne les voie
     pas côte à côte.
   - **`Polyalthra suaveolens` est accepté à 0,73, sans qu'on demande rien.** Il ne passe
     donc pas par l'onglet Révision, qui ne contient que les noms sans identifiant. Même
     réflexe qu'à 10h16 sur l'*Uapaca* : **trier par `match_score` croissant**. C'est le
     geste que cette personne emportera chez elle.
   - **Ligne 1, `Bois mort debout`** — n'est pas un taxon, et ressort en `no_match`. Le
     bon comportement : l'outil ne force rien plutôt que d'inventer.

   **Et si on veut aller plus loin avec elle**, le cas de la ligne 29. `Picnatus angolens`
   est honnêtement rejeté au seuil par défaut. Descendu à **0,30**, il devient *Searsia
   angolensis*, une **Anacardiaceae**, quand le nom visé était *Pycnanthus angolensis*,
   l'ilomba, une **Myristicaceae** (idtax 11580, bien présent dans le référentiel). La
   ligne aurait l'air propre — un nom accepté, une famille, un identifiant — et seul le
   score la trahirait. **Baisser le seuil ne trouve pas davantage, cela invente
   davantage.**

   S'y ajoutent deux lignes de genre seul (`Erythrophleum sp`, `Polyalthia sp`, appariées
   au rang du genre) et `Piliostigma Thonningii`, épithète capitalisée et pourtant exacte —
   de quoi montrer ce que la normalisation absorbe sans qu'on le demande.
2. **Le fichier qui ressort à 100 % accepté.** C'est un résultat, pas un échec, et il faut
   le dire ainsi : « noms = espèces, vous le savez maintenant, et vous avez les
   identifiants ». Puis l'envoyer sur les traits — densité du bois sur sa liste propre —
   qui est le créneau de 11h45 de toute façon. Cette personne devient au passage une
   troisième paire de mains dans la salle.
3. **Le gros fichier.** Ce n'est pas un risque, c'est le meilleur cas. Le seul point à
   surveiller est la longueur de la file de révision manuelle : au-delà de deux cents noms,
   travailler sur une tranche. À annoncer dans le mail plutôt qu'à découvrir en séance.

**La mesure qui change le plus l'heure** est en amont : demander les fichiers à l'avance
(voir section 8). Trois ou quatre fichiers ouverts le vendredi soir, et l'atelier cesse
d'être imprévisible.

### Deux vitesses assumées

Le niveau en R ne doit pas être un facteur le matin : tout se fait à la souris, et la
seule ligne de R tapée par un débutant est celle qui lance l'application — sur l'antisèche
et sur un paperboard dans la salle.

Le panneau de code R généré sert de seconde vitesse. Montré une fois à 11h45, il donne aux
plus à l'aise de quoi s'occuper pendant que les autres terminent. La dispersion des niveaux
cesse d'être un problème et devient deux rythmes.

### Ceux qui sont venus pour « mieux apprendre R »

Plusieurs participants se sont inscrits avec cette attente. Ce n'est pas l'objet de la
journée, et il faut le dire — mais pas comme un refus, parce que ce serait à la fois
désagréable et faux.

**Pourquoi ce n'est pas l'objet.** D'abord le temps : on ne rend personne autonome en R en
une matinée partagée avec autre chose. Ensuite et surtout, les outils présentés sont
*conçus* pour ne pas exiger R. Ce n'est pas une limite de la formation, c'est l'intention
du développement — et le dire ainsi respecte l'autre moitié de la salle, celle qui ne
connaît pas R et n'a pas à se sentir en retard.

**Mais le pont existe, et il va dans le bon sens.** Chaque manipulation faite à la souris
dans l'application produit le code R équivalent, copiable. Ce n'est pas un lot de
consolation : c'est probablement ce qu'il y a de plus utile pour quelqu'un qui veut passer
au programmatique. La phrase à retenir pour le dire en séance —

> On n'apprend pas R en regardant du R. On l'apprend en ayant du code qui marche, sur un
> problème qu'on vient de résoudre soi-même à la main.

À 11h45, c'est exactement leur situation : ils ont fait le travail, ils comprennent ce
qu'ils ont fait, et le panneau leur donne le script qui le refait. Avec leurs noms de
colonnes, leur seuil de similarité, leur fichier. Aucun tutoriel ne part d'un aussi bon
endroit.

**Deux placements, pas un seul.**

- **09h40, à la mise en route**, une phrase en passant, au moment où ils tapent la seule
  ligne de R de la matinée : *« c'est la seule ligne de R que vous taperez aujourd'hui, et
  c'est voulu — l'outil est fait pour ça. Pour ceux qui veulent du R, on y revient en fin
  de matinée, et vous repartirez avec un script qui tourne sur votre fichier. »* Sans ça,
  quelqu'un peut passer trois heures à attendre la partie R au lieu de participer. La
  gestion de l'attente doit être précoce ; la démonstration, non.
- **11h45**, le panneau lui-même. Une fois, sur un résultat réel.

**Ce qu'il faut montrer, et dans cet ordre.** Le bouton n'apparaît qu'une fois un
appariement effectué. Le panneau donne le chargement des données, la combinaison des
colonnes en mode multi-colonnes, l'appariement automatique avec
`standardize_taxonomic_batch()`, le référentiel de sortie s'il a été changé, puis un
**script complet** avec un bouton « copier ». Deux points à souligner :

- le script utilise `load_backbone_cache()`, donc il tourne **sans connexion** sur le cache
  qu'ils ont déjà — ils peuvent le relancer chez eux, hors ligne ;
- un encadré du panneau dit que **la révision manuelle n'a pas d'équivalent
  programmatique** : les noms non résolus restent `idtax_n = NA`. C'est écrit dans
  l'application plutôt que passé sous silence, et il faut le lire à voix haute. C'est
  précisément ce qui distingue cet outil d'une promesse de reproductibilité intégrale.

**L'exercice à leur donner en partant** : relancer ce script lundi sur un second fichier.
C'est la seule chose qui transforme une démonstration en compétence, et elle ne coûte rien
à annoncer.

**Et s'ils en veulent plus**, c'est l'après-midi : la permanence est faite pour ça, et la
section 5 le prévoit déjà. Leur dire à 12h10, quand on annonce qui reste.

## 5. Après-midi — 14h00 à 16h30 : une clinique, pas un second cours

Le groupe sera incomplet et l'effectif imprévisible. Une permanence résiste à cela ; un
cours structuré non, et personne ne peut « rater » une permanence.

Principe : ceux qui restent apportent leur question. `query_plots` est ouvert sur les
parcelles en libre accès pour répondre à celles qui s'y prêtent. Les formats de sortie sont
montrés sur un cas réel apporté par quelqu'un, plutôt que sur un cas fabriqué. Ceux qui
veulent voir le code R généré le voient.

### Le libre accès ne suffit pas pour montrer long / large

C'est le point à régler vendredi soir, et il tient à un fait vérifié plutôt que supposé :
**les 83 parcelles ouvertes au login public ont toutes été relevées une seule fois.** Zéro
entrée de type `census` dans `data_liste_sub_plots` pour ces 83 identifiants. Ce sont les
transects Tombi / Mbandza / Ekagna au Congo (65, relevés en 1996) et les placettes Doudou
40 × 40 au Gabon (18, relevées en 2000), environ 8 000 arbres en tout.

Sur un inventaire à un seul passage, la distinction entre les trois formats **ne se voit
pas** : le format large a une seule colonne de dbh, le format long a autant de lignes que
d'arbres, et les paires de recensements ne renvoient rien. On peut décrire la différence,
on ne peut pas la montrer — et c'est une distinction qu'on ne comprend qu'en la voyant.

D'où l'ouverture d'une parcelle permanente pour la journée. La procédure, le profil et la
fermeture du dimanche sont dans `formation_2026_acces_apres_midi.R`. Parcelle retenue :
**1188** — 1 ha, 456 arbres tous identifiés, 82 taxons, deux recensements en 2021 et 2024,
**436 arbres mesurés aux deux dates**. Ce qui donne 456 lignes en large (deux colonnes de
dbh), ~892 en long, 436 en paires de recensements. Deux passages suffisent, et c'est même
plus lisible que cinq : le tableau large reste projetable.

**Le détail à exploiter, et il vaut à lui seul la séquence.** L'écart 2021–2024 va de
−31,6 cm à +42 cm. Ce ne sont pas des erreurs : les deux plus fortes « décroissances »
sont des arbres dont la **hauteur de mesure a changé** entre les deux passages — le tag
344 passe de 1,30 m à 3,37 m et perd 31 cm de diamètre, le tag 357 de 1,45 m à 3,44 m.
Des arbres à contreforts, devenus inmesurables à hauteur de poitrine.

La hauteur de mesure est un attribut **de la mesure**, pas de l'arbre. Le format large n'a
nulle part où le mettre : une ligne par arbre, une colonne par recensement. Le format long
le transporte avec chaque mesure. D'où la phrase à dire :

> *« Cet arbre a perdu 31 centimètres en trois ans. Ce n'est pas une faute de saisie : on
> l'a mesuré à 1,30 m en 2021 et à 3,37 m en 2024. L'information qui vous sauve est
> attachée à la mesure, pas à l'arbre — et c'est exactement ce que le format large ne peut
> pas vous montrer. »*

C'est un meilleur argument que « choisissez le bon format » parce qu'il montre un coût
concret : en large, on conclut que l'arbre a rétréci.

Le reste de la parcelle est sain et il faut le dire, sinon on donne l'impression d'un jeu
de données douteux : 353 arbres sur 436 entre 0 et 3 cm de croissance, 65 décroissances de
moins d'un centimètre — bruit de mesure ordinaire — et seulement 4 écarts au-delà de 6 cm.
Le script `formation_2026_exemple_croissance_1188.R` fait tourner tout cela, y compris le
tri entre les arbres dont la hauteur de mesure a bougé et les autres.

Deux conditions avant d'ouvrir : vérifier **qui a créé la parcelle**, et faire tourner les
trois formats dans l'application la veille. Si les trois sorties ne diffèrent pas, la
séquence n'a pas lieu d'être.

> **Piège de schéma, vérifié à mes dépens.** Le millésime d'un recensement n'est pas dans
> `data_traits_measures.year` — NULL pour l'essentiel des mesures d'inventaire — mais sur
> la sous-parcelle : `data_traits_measures.id_sub_plots` → `data_liste_sub_plots.year`.
> Compter par `year` donne zéro parcelle multi-recensements dans toute la base.

### Ce que la RLS ne couvre pas, et ce qu'on en dit

Vérifié le 25 septembre avec le seul login public, qui est publié sur internet : il lit
**446 156 individus sur 2 081 parcelles**, alors que 83 parcelles seulement lui sont
ouvertes. Plus 476 701 mesures individuelles, 42 320 spécimens, 2 053 481 mesures de
traits. La RLS est active sur `data_liste_plots` et sur `taxa_traits_measures`, et sur
rien d'autre.

L'objection naturelle est que des arbres sans métadonnées ne servent à rien. Elle ne tient
pas, et c'est le point à retenir : **`data_traits_measures` est dénormalisée et porte ses
propres colonnes de métadonnées**, hors de toute RLS. Sur les parcelles que la RLS masque,
ce login lit `decimallatitude` / `decimallongitude` pour **868 d'entre elles** — quatre
décimales, soit une dizaine de mètres — le pays, `original_plot_name` pour 64, les dates
via les sous-parcelles, et le collecteur nommable avec son institution via `id_colnam` →
`table_colnam`. Coordonnées, date, auteur, et la liste complète des arbres avec tags,
taxons, diamètres et positions : c'est un jeu d'occurrences publiable, attribué et
géolocalisé, sur des parcelles appartenant à des collègues nommés.

Ce n'est pas une découverte : c'est le point **P4.2** de `PLAN_SECURITY_REMEDIATION.md`,
« Extend RLS to the child tables, or stop claiming it works », ouvert depuis août. Deux
choses sont nouvelles : on s'apprête à affirmer le contraire devant une salle, et la copie
dénormalisée des métadonnées dans `data_traits_measures` n'est pas dans le périmètre que
P4.2 décrit — l'y ajouter.

Conséquence pour le discours de la matinée : la phrase « vous êtes seul à voir vos
inventaires » est vraie de la **fiche** de parcelle et fausse de tout ce qui pend en
dessous. La formulation honnête, si la question vient, tient en une phrase : *« l'accès est
géré parcelle par parcelle ; aujourd'hui c'est la fiche de la parcelle qui est protégée,
pas encore les lignes d'arbres en dessous — c'est un chantier en cours. »* Ne pas promettre
mieux que ce qui existe : ce sont des gens qui déposeront peut-être des données un jour, et
une promesse démentie coûte plus cher que la réserve.

Ne pas prévoir 17h00 : après une matinée dense et un déjeuner, la dernière demi-heure ne
produit rien.

## 6. Les questions à poser

Séquence à retravailler — la formulation de plusieurs de ces questions n'est pas encore
assez claire, et certaines demandent un support visuel préparé. À reprendre avant samedi.

**Les questions 1 à 3 sont faites** : formulation aboutie, temps de parole, ce qu'on fait
des réponses et à quel moment de la matinée chacune est remboursée, dans
`formation_septembre_2026_presentation_raison_detre.md`, qui détaille tout le créneau de
09h15–09h40. Les versions ci-dessous en restent le résumé. Les questions 4 à 7 relèvent des
créneaux de 10h00 et 11h45 et n'ont pas encore été reprises.

Le principe, lui, est arrêté : vérifier les notions qui paraissent basiques en les
demandant, plutôt qu'en les expliquant.

Format : vous posez, ils en discutent trente secondes à deux, vous prenez deux ou trois
réponses. Pas d'outil de sondage en ligne — bande passante, comptes à créer, rupture de
rythme. Demander aux deux habitués de ne pas répondre les premiers.

1. Dans votre dernier jeu de données : combien de noms d'espèces différents, et combien
   d'espèces selon vous ? L'écart entre les deux chiffres est le sujet de la matinée.
2. *Aningeria altissima* et *Pouteria altissima* : un arbre ou deux ? L'hésitation de la
   salle vaut toutes les explications sur la synonymie.
3. Qui peut dire quel référentiel taxonomique a servi à établir sa liste d'espèces
   actuelle ? Très peu de mains, en général. Amène l'idée qu'un nom sans référentiel n'est
   pas une information complète.
4. Vous avez « Sterculia sp. » dans votre tableau. Qu'est-ce que l'outil devrait en
   faire ? Fait comprendre qu'un appariement au genre est un résultat utile, pas un échec.
5. Il vous manque la densité du bois pour 30 % de vos espèces. Qu'est-ce que vous faites
   aujourd'hui ? Les laisser décrire leur pratique réelle avant de montrer le repli au
   genre, et discuter de ce que ce repli coûte.
6. À quoi sert un identifiant de taxon, qu'un nom ne sait pas faire ? La question qui a
   l'air la plus bête et qui porte tout le package.
7. Avant l'export : vous envoyez ce tableau à un collègue. Qu'est-ce qui doit y figurer
   pour qu'il puisse le refaire sans vous ? Amène les colonnes de provenance, les citations
   et le code généré comme une réponse à leur propre exigence, pas comme une fonctionnalité
   de plus.
8. **Sur le tableau de traits, les deux onglets ouverts côte à côte :** dans le format
   large, la densité du bois de cette espèce est **une seule valeur**. D'où sort-elle ?

   Poser la question ainsi plutôt que « que comprenez-vous par format long ? ». Une
   définition demandée à froid ne produit que du silence ou de la récitation ; celle-ci a
   une réponse que n'importe qui peut atteindre par le raisonnement, et elle délivre le
   concept sans qu'on ait eu à le définir.

   **La réponse.** C'est une moyenne. Le format large est une **agrégation** —
   `mean`, `sd`, `n` par trait (`helpers_traits_common.R:126`) — et les *n* mesures qui ont
   été moyennées sont dans l'onglet long, une par ligne. Le nombre du format large n'est
   pas une mesure, c'est un résumé de mesures.

   **Les deux relances, dans cet ordre.** *Et si ces n mesures viennent de trois sources
   qui ne sont pas d'accord ?* → l'onglet Sources, et la raison pour laquelle il existe.
   Puis : *lequel des deux ouvrez-vous dans Excel pour regarder, lequel donnez-vous à R ?*
   → le large se lit, le long se calcule, et aucun des deux n'est « le bon ».

   **Pourquoi cette question vaut d'être posée le matin.** Elle installe, sur un objet
   simple et sans enjeu, le principe qui portera tout l'après-midi : **un format large n'a
   nulle part où ranger ce qui est attaché à chaque mesure.** Le matin, ce qu'il perd est
   le détail des *n* observations et leur source ; l'après-midi, ce sera la hauteur de
   mesure du diamètre — et l'arbre qui « rétrécit » de 31 cm. Deux occurrences du même
   principe valent mieux qu'une explication.

## 7. Les deux participants qui connaissent déjà le package

Leur proposer un rôle, par un message séparé et à l'avance :

- circuler dans la salle pendant l'atelier et débloquer les voisins ;
- servir de relais — c'est eux qui signalent « trois personnes sont bloquées », ce qu'aucun
  partage d'écran ne montrera ;
- prendre trois minutes chacun, vers 11h00, pour raconter ce qu'ils font concrètement du
  package.

Le témoignage d'un pair porte plus loin qu'une démonstration de l'auteur, et ne coûte rien
à préparer.

## 8. À faire d'ici samedi

**Jeudi 24**

- Mail aux participants : devoir reformulé (installer *et* faire tourner un appariement une
  fois), fichier d'exemple joint, lien Zoom, consigne audio. Et sur la liste d'espèces
  qu'ils apportent, trois précisions qui décident de la qualité de l'atelier de 10h45 :
  - **« envoyez-la moi d'ici vendredi soir si vous pouvez »** — c'est la mesure à plus fort
    rendement de toute la préparation. Trois ou quatre fichiers ouverts vendredi soir, et
    l'heure d'atelier cesse d'être imprévisible. Même deux suffisent à changer la donne.
  - **« même si elle vous paraît propre »** — sans cette phrase, ceux qui ont une liste
    soignée ne l'apporteront pas, en croyant bien faire. Or c'est sur elles que la
    synonymie se voit le mieux : 38 % sur une liste d'essai correctement orthographiée
    (voir section 4).
  - **« au-delà de deux cents noms, apportez-la quand même, on travaillera sur une
    tranche »** — évite de découvrir en séance une file de révision manuelle qui ne tient
    pas dans l'heure.
- Groupe WhatsApp avec les deux relais, Stéphane et Fernandez : poser le contexte sans rien
  leur demander d'autre — leur emploi du temps est chargé et la contrepartie viendra
  vendredi. Une seule question dès maintenant : lequel des deux branche son ordi au
  projecteur, et avec quelle sortie vidéo. Elle ne bloque pas le mail — la consigne audio
  tient sans le nom de la machine — mais une réponse avant l'envoi permet de désigner ce
  poste au lieu d'annoncer qu'il sera désigné sur place.
- Identifier le contact sur place : postes, casques ou poste audio unique, multiprise, clé
  USB, imprimante pour l'antisèche, câble du projecteur.
- Geler `.backbone_cache_version` : ne plus y toucher à partir du moment où les
  participants construisent leur cache.

**Vendredi 25**

- **Envoyer le programme du jour** (`formation_septembre_2026_programme_jour.md`) aux
  participants et dans le groupe WhatsApp : lien Zoom, horaires en heure de Douala,
  consigne audio, et le devoir reformulé — installer *et* faire tourner un appariement une
  fois. C'est la dernière occasion de récupérer un blocage d'installation à froid.
- **Ouvrir la parcelle permanente de l'après-midi** — `formation_2026_acces_apres_midi.R`.
  Vérifier d'abord qui l'a créée, puis faire tourner les trois formats dans
  l'application pour s'assurer qu'ils diffèrent visiblement. Refermer dimanche.
- Test Zoom avec le contact, depuis la salle si possible et à la même heure que la
  formation.
- Préparer la clé USB (les deux `.rds`, construits avec la version que les participants
  installent).
- Finaliser l'antisèche A4 et l'envoyer en PDF.
- **Ouvrir les listes d'espèces reçues** et en faire tourner trois ou quatre dans la chaîne
  d'appariement. C'est ce qui transforme l'atelier de 10h45 en heure préparée : vous savez
  à l'avance qui va tomber sur quoi, vous repérez le fichier qui ne s'ouvrira pas, et vous
  arrivez avec deux ou trois exemples tirés de leurs propres données plutôt que des
  vôtres. Noter pour chacun le nombre de synonymes résolus — c'est le chiffre à leur
  annoncer en séance.
- Reprendre la formulation des questions 4 à 7 de la section 6. Les questions 1 à 3 sont
  faites, dans `formation_septembre_2026_presentation_raison_detre.md`.
- Relire le fichier d'exemple : `formation_2026_liste_especes_exemple.xlsx` (38 lignes,
  deux feuilles). Il est construit pour rencontrer un cas de chaque situation, et son
  comportement a été vérifié contre le référentiel : 26 appariements exacts, 3 approximatifs,
  7 à réviser à la main, 5 synonymes résolus. Le corrigé animateur, avec le détail nom par
  nom et les moments à exploiter, est dans `formation_2026_liste_especes_exemple_corrige.md`.
- Reconstruire votre propre cache de référentiel. Les doublons `Fabaceae` ont été fusionnés
  depuis la première version du fichier : sur un cache d'avant la fusion, la ligne
  `Fabaceae` affiche encore « Fabaceae → Fabaceae ». Ce n'est plus une décision à prendre
  sur le fichier, mais l'écran de l'animateur doit correspondre au corrigé.
- Vérifier en ligne que les idtax 5015 (`Dacryodes buettneri`) et 4365 (`Dacryodes
  edulis`) sont liés à leurs enregistrements WCVP dans `taxa_backbone_link`. Sans le lien,
  la démonstration de fin de matinée n'affiche rien et ne le dit pas.

**Optionnel, si le temps le permet**

- Enregistrer la démonstration de quinze minutes. Le plan de tournage existe déjà
  (`docs/TUTORIEL_VIDEO_taxonomic_match_app.md`). Envoyée au contact sur place, elle sert
  de secours si votre flux tombe en pleine démonstration — la salle la lance en local et
  vous commentez au téléphone. Elle resservira après la formation. À deux jours de
  l'échéance, ce n'est pas la priorité : la démonstration peut être refaite en direct.

## 9. Hors champ, assumé

- L'assistant d'import et la gestion de ses données dans l'infrastructure : dix minutes de
  mots en fin de matinée, aucune démonstration.
- Les subtilités multi-référentiels et WCVP : un seul exemple concret de deux minutes, le
  cas *Dacryodes* / *Pachylobus* en fin de matinée, puis « ça existe, on en reparle quand
  vous en aurez besoin ». L'exemple est là parce qu'il fait atterrir la question 6 mieux
  qu'une explication ; ce qui reste hors champ, c'est tout le reste — arbitrage entre
  référentiels, liens manquants, ajout d'un référentiel.
- Toute séquence qui suppose que chacun dispose d'un compte.

Vu le profil du groupe, le retour sur investissement de ces trois blocs est nul à court
terme, et ils mangeraient l'heure d'atelier qui est la seule chose dont les participants se
souviendront lundi.
