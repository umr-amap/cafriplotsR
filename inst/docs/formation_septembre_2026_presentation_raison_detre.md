---
title: "Raison d'être du package — déroulé de la séquence"
subtitle: "Samedi 26 septembre 2026, 09h15 – 09h40 (heure de Douala)"
lang: fr
toc: true
format:
  html:
    embed-resources: true
---

Document de travail interne. Il détaille le créneau de 09h15–09h40 du déroulé animateur
(`formation_septembre_2026_deroule_animateur.md`) et propose une formulation aboutie des
questions 1 à 3 de sa section 6.

## Le parti pris, et pourquoi

La présentation ressac (`Présentation_gilles_ressac.pptx`, 14 slides) est un argumentaire
de consortium : elle part du problème régional d'accessibilité des données, arrive à
Cafriplots et se termine sur « nos arguments pour rejoindre Cafriplots ». C'est le bon
ordre devant un partenaire potentiel.

Ce n'est pas le bon ordre ici. Le déroulé note que ce groupe n'est *a priori* pas composé
de personnes qui alimenteront Cafriplots avec leurs données à court terme. Leur ouvrir la
journée par « voici pourquoi rejoindre le réseau » les place en position d'être démarchés
pendant vingt-cinq minutes, avant même d'avoir touché l'outil qu'ils sont venus chercher.

La séquence est donc **inversée** : elle part du fichier qu'ils ont ouvert ce matin, et le
réseau arrive en fin de parcours, comme réponse à une question qu'ils se seront posée
eux-mêmes — d'où viennent la taxonomie et les traits que l'outil leur donne. On ne
présente pas l'infrastructure, on explique d'où vient ce qu'ils vont utiliser dans
quarante-cinq minutes.

Conséquence pratique : on garde environ la moitié des slides de la présentation ressac, et
on en change l'ordre.

## La règle de temps

Vingt-cinq minutes, et le point de contrôle de 10h00 est ferme : la mise en route qui suit
a besoin de ses vingt minutes complètes, et le dépannage d'installation est ce qui a mangé
la matinée entière lors de la session précédente. **Déborder ici se paie deux fois** — une
fois sur la mise en route, une fois sur l'atelier.

**L'ordre de sacrifice, si vous êtes en retard.** D'abord la troisième slide du bloc D,
celle sur la décroissance dans le temps : les deux premières portent l'essentiel. Ensuite
la deuxième, l'accessibilité régionale — si vous ne gardez qu'une slide du bloc D, gardez
*Compiler*, c'est la seule qui parle d'eux. Ensuite seulement, les chiffres du bloc E.

Ce qui ne se coupe pas : les blocs A, C et F. A parce qu'il installe le problème, C parce
qu'il installe la seule notion que personne ne peut deviner, F parce qu'il répond à la
question que la salle se pose en silence. Un créneau réduit à ces trois blocs, en douze
minutes, tient debout ; un créneau qui garde D et E en sautant C ne tient pas.

## Vue d'ensemble

| | Durée | Bloc | Écran |
|---|---|---|---|
| 09h15 | 3 min | **A** — Question 1 : combien de noms, combien d'espèces | rien, ou paperboard |
| 09h18 | 4 min | **B** — Pourquoi l'écart existe, + question 2 | 1 slide |
| 09h22 | 6 min | **C** — Question 3 en deux temps, et ce qu'est un référentiel | paperboard, puis 1 slide |
| 09h28 | 5 min | **D** — Ce n'est pas que votre problème | 4 slides : *Compiler* (ressac **6**), la charnière *Compiler n'est pas le seul problème* (ajoutée), puis ressac **3, 4** |
| 09h33 | 3 min | **E** — Un réseau de réseaux, et ses principes | 3 slides (ressac 12, 8, 11) |
| 09h36 | 4 min | **F** — Deux usages, et celui de ce matin | 1 slide |

Les trois questions occupent la première moitié du créneau, pas les vingt-cinq minutes.
C'est voulu : elles servent à établir que la salle a un problème, avant qu'on parle de la
solution. Une question posée à 09h35 serait une respiration ; posée à 09h15, c'est le
sujet.

Le bloc C est le plus long des six, et c'est délibéré : c'est là que se construit la seule
notion de la matinée que personne ne peut deviner. Les deux minutes qu'il prend sont
reprises sur le bloc D, qui est le plus intéressant pour vous et le moins nécessaire pour
eux.

## Bloc A — 09h15, trois minutes

### Question 1

> *« Prenez votre dernier fichier de noms d'espèces, celui dont vous venez de me parler.
> Deux chiffres. Premier : combien de noms différents il y a dedans — ça, vous pouvez le
> compter. Deuxième : combien d'espèces, à votre avis. Discutez trente secondes avec votre
> voisin, et donnez-moi les deux chiffres. »*

La reformulation par rapport à la version de la section 6 tient à une chose : demander les
**deux chiffres explicitement**, et dans cet ordre. La version courte (« combien de noms et
combien d'espèces ») laisse la salle répondre un seul nombre, parce qu'elle ne voit pas
encore pourquoi il y en aurait deux. En les nommant « premier » et « deuxième » on force la
paire.

**Ce que vous faites de la réponse.** Prendre trois personnes, pas plus. Écrire les paires
au paperboard, en grand, et les laisser visibles toute la matinée :

```
        noms      espèces
  A      312        ?
  B       88       85
  C      140      "sais pas"
```

Le « ? » et le « sais pas » sont les meilleures réponses et il faut le dire à voix haute.
Quelqu'un qui écrit 85 sous 88 a déjà une idée du problème ; quelqu'un qui n'écrit rien a
compris qu'il ne peut pas savoir. Les deux sont utiles ; la réponse qui vaut le moins est
celle qui recopie le premier chiffre dans la seconde colonne.

**Enchaînement :** *« L'écart entre ces deux colonnes, c'est le sujet de la matinée. À
midi, vous aurez le deuxième chiffre pour votre propre fichier. »*

**Ce qu'il ne faut pas faire ici :** expliquer pourquoi l'écart existe. C'est le bloc B.
La tentation est forte et elle coûte deux minutes.

## Bloc B — 09h18, quatre minutes

### Le fond

Une seule idée, et elle est contre-intuitive pour qui n'y a jamais pensé : **la liste des
noms et la liste des espèces ne sont pas la même liste**, parce qu'un nom n'est pas une
espèce mais une étiquette qu'on lui a collée à un moment donné. Plusieurs étiquettes pour
une même espèce, ça arrive tout le temps — les botanistes en changent, les sources ne
suivent pas au même rythme, et un fichier compilé à partir de trois sources porte les trois
états.

Le dire avec un exemple du terrain plutôt qu'en abstrait. L'iroko en est un bon :
*Chlorophora excelsa* et *Milicia excelsa* sont la même espèce, et la plupart des gens dans
la salle ont vu les deux noms.

> **L'unité de tout ce bloc est l'espèce, jamais l'arbre.** Ne jamais dire « c'est le même
> arbre » : dans le fichier d'exemple, *Chlorophora excelsa* et *Milicia excelsa* sont
> portés par **deux arbres différents**, deux lignes avec deux numéros — la réponse à « un
> arbre ou deux ? » y est littéralement « deux », l'inverse de ce qu'on veut faire
> entendre. Il y a trois niveaux en jeu ce matin — le nom, l'espèce, l'individu — et la
> question 1 en a déjà installé deux. Glisser « arbre » ici en ajoute un troisième au
> moment précis où on essaie de les séparer, et donne à la salle une raison légitime
> d'être perdue.

### Question 2

> *« Aningeria altissima et Pouteria altissima. Une espèce, ou deux ? Mains levées : qui
> dit une ? qui dit deux ? qui ne se prononce pas ? »*

Trois mains comptées, quinze secondes. Pas de discussion à deux ici : on veut la répartition
brute, et surtout la troisième colonne.

**Ce que vous faites de la réponse.** La salle va se partager, et c'est tout l'intérêt —
l'hésitation collective démontre mieux que n'importe quelle explication que la synonymie
n'est pas une connaissance qu'on peut supposer partagée. C'est une seule espèce :
*Aningeria altissima* est un synonyme, le nom accepté est *Pouteria altissima*.

Puis la phrase qui compte : *« Vous êtes une salle de spécialistes de la forêt d'Afrique
centrale et vous n'êtes pas d'accord. Ce n'est pas un problème de compétence. C'est que
l'information n'est pas dans le nom. »*

**Le rappel à garder pour plus tard, pas maintenant :** ce nom est dans le fichier
d'exemple, ligne 2. Ils le verront se résoudre tout seul à 10h00. Ne pas l'annoncer — la
surprise vaut mieux.

### Écran

Une slide, deux lignes, rien d'autre :

```
Chlorophora excelsa   =   Milicia excelsa
Aningeria altissima   =   Pouteria altissima
```

## Bloc C — 09h22, six minutes

### Le piège de cette question

Posée telle quelle — *« votre liste suit quel référentiel taxonomique ? »* — la question
suppose acquis le mot qu'elle emploie. Or vos propres notes de préparation posent le doute :
*« est-ce que les utilisateurs comprennent le concept de référentiel taxonomique ? Sans même
bien comprendre le concept, peut-être ont-ils déjà été confrontés à des difficultés
résultant de la confrontation de données provenant de différents référentiels. »*

C'est exactement la situation : **ils ont l'expérience, ils n'ont pas forcément le mot.**

Et cela casse la question, pour une raison précise. Tout son intérêt est que le silence soit
lisible. Si la moitié de la salle n'a pas le concept, le silence devient ambigu — vous ne
pouvez plus distinguer « je ne sais pas lequel » de « je ne comprends pas ce que vous
demandez ». Un silence qu'on ne sait pas lire est pire que pas de question du tout.

Deux mauvaises solutions : définir le terme d'abord, ce qui transforme 09h22 en cours
magistral au pire moment ; ou le supposer connu, ce qui produit le silence illisible. La
troisième voie est de **poser une question qui fonctionne sans le mot, et de faire sortir la
définition de leurs réponses**. C'est le principe déjà retenu en section 6 du déroulé :
vérifier les notions qui paraissent basiques en les demandant, plutôt qu'en les expliquant.

### Question 3a — l'expérience avant le mot

> *« Vous avez un doute sur un nom d'espèce. Soit l'orthographe, soit : est-ce que c'est
> encore le nom actuel ? Vous allez vérifier où ? Je veux du concret — un site, un livre,
> une personne, un fichier. »*

Tout le monde peut répondre à celle-là, y compris quelqu'un qui n'a jamais entendu le mot
référentiel. Prendre six à huit réponses, vite, et les écrire au paperboard **sans les
commenter** :

```
  une flore (Flore du Gabon, Flore du Cameroun)
  POWO / Kew
  GBIF
  Tropicos
  l'herbier — ou le collègue qui sait
  le fichier du projet précédent
  Wikipédia
```

Wikipédia viendra, et il faut le prendre sans ironie. C'est une réponse honnête, c'est ce
que font la plupart des gens pressés, et ça servira dans trente secondes.

### La définition, construite à partir de ce qu'ils ont dit

*« Tout ce que vous venez de citer, ce sont des référentiels — ou bien ça s'appuie sur un
référentiel sans le dire. »*

Trois propriétés, pas une de plus :

1. **Une liste qui tranche.** Pour un taxon donné, elle dit : voici le nom accepté, et voici
   les autres noms qui désignent la même chose. Une liste qui ne tranche pas n'est pas un
   référentiel, c'est une liste de noms. *(C'est ici que Wikipédia sert : on y trouve des
   noms, mais rien ne garantit qu'ils soient arbitrés ni que l'arbitrage soit le même d'une
   page à l'autre.)*
2. **Un choix, pas une vérité.** Il est fait par des gens, à partir de la littérature, et
   deux équipes également compétentes peuvent trancher différemment.
3. **Une chose datée.** Le référentiel de 2019 et celui de 2026 ne disent pas la même chose,
   sans que personne se soit trompé entre les deux.

Puis l'exemple concret, qui est celui qu'ils utiliseront dans trente-cinq minutes : le
référentiel interne du package vient de la base RAINBIO, elle-même héritée du référentiel de
l'herbier de Wageningen. *« Ce n'est pas tombé du ciel. C'est le travail d'un herbier,
repris, corrigé et maintenu depuis. »* Un référentiel a une provenance, comme une donnée.

> **Ce qu'il ne faut pas ouvrir.** La distinction nomenclature / taxonomie — le Code, la
> priorité, les types. Quelqu'un dans la salle voudra peut-être y aller. Une phrase pour
> fermer poliment : *« le Code de nomenclature dit si un nom est valablement publié ; le
> référentiel dit lequel on retient. C'est la seconde question qui fait que deux listes
> diffèrent, et c'est celle-là qui nous occupe aujourd'hui. »* Puis enchaîner. Ce débat vaut
> dix minutes qu'on n'a pas.

### Question 3b — maintenant elle est lisible

> *« Alors : votre liste d'espèces actuelle, celle de vos analyses. Elle suit lequel de ceux
> qui sont au tableau ? »*

Laisser le silence s'installer. Il sera long, et c'est maintenant un silence qui veut dire
quelque chose, puisque la notion vient d'être posée avec leurs mots.

Ne pas sauver la salle en répondant à sa place. Relancer une fois, plus bas : *« Personne ?
Moi non plus je ne saurais pas le dire pour la moitié des fichiers que je reçois. »* Se
compter dedans change le statut de la question : ce n'est pas un contrôle de connaissances,
c'est un constat partagé.

Si quelqu'un répond « la Flore du Gabon » ou « POWO », c'est une très bonne réponse et il
faut la valoriser — puis demander : *« et le fichier que votre collègue vous a envoyé l'an
dernier, il suivait lequel ? »* La question se déplace d'elle-même du fichier qu'on maîtrise
vers ceux qu'on agrège.

### Les trois phrases qui ferment le bloc

1. Un nom d'espèce sans son référentiel est une information incomplète, comme une date sans
   fuseau horaire ou une mesure sans unité.
2. Ce n'est pas grave tant qu'on travaille seul sur ses propres données. Ça le devient dès
   qu'on compile — et compiler, c'est ce que tout le monde fait.
3. Il existe une information qui, elle, ne dépend pas du référentiel : l'identifiant du
   taxon. C'est ce que l'outil met dans vos fichiers, à côté des noms.

La troisième est plantée ici et **ne sera pas développée** — elle sera démontrée à 11h45 sur
le cas *Dacryodes* / *Pachylobus*. Une phrase suffit ; ils n'ont pas encore de quoi la
comprendre, et la propriété n°2 ci-dessus est ce qui la rendra audible à ce moment-là.

### Écran

Le paperboard porte maintenant deux choses qui doivent rester visibles jusqu'à midi : les
paires de chiffres du bloc A, et la liste des référentiels du bloc C. Prévoir la place, ou
deux feuilles.

Pour finir le bloc, la slide du bloc B avec une troisième ligne ajoutée :

```
                        référentiel A      référentiel B
Dacryodes buettneri     nom accepté        synonyme
idtax 5015              5015               5015
```

Montrer, ne pas commenter. Cinq secondes à l'écran, la salle la reverra en fin de matinée.

## Bloc D — 09h28, cinq minutes

**Slides ressac 6, 3 puis 4**, dans cet ordre — l'inverse de la présentation ressac. Ce
sont les trois problématiques qui sous-tendent le package et le réseau, et elles forment
une progression, pas une liste : *on compile* → *l'accès est inégal* → *et il se dégrade
avec le temps*.

C'est ici que se fait le passage du « vous » au « nous », et c'est le point de couture le
plus fragile de la séquence : on quitte leur fichier pour un constat régional. Une première
version enchaînait directement sur l'accessibilité des données en Afrique centrale, et le
saut était trop grand — la salle passait sans préavis d'un problème qu'elle venait de
reconnaître à une généralité qui ne la concernait pas encore.

**La transition est déjà écrite, il suffit de ne pas la gâcher.** La deuxième des trois
phrases qui ferment le bloc C se termine par *« ça le devient dès qu'on compile — et
compiler, c'est ce que tout le monde fait »*. La slide suivante s'intitule **Compiler, tout
le monde le fait**. Enchaîner sans rien ajouter : le pont est fait par la répétition, pas
par une phrase de liaison.

- **Slide 6 d'abord** — compiler traits, inventaires, occurrences et statuts se fait en
  permanence, projet par projet, article par article. Le résultat dépend de qui a eu accès
  à quoi, n'est pas reproductible, et n'est presque jamais réutilisé. Et à chaque fois,
  **les noms ne s'alignent pas**. On est encore dans leur pratique — tout le monde dans la
  salle compile.

  **Le troisième point de cette slide est un récapitulatif**, et il faut le dire comme
  tel : ce ne sont pas des informations nouvelles, ce sont les trois choses que la salle
  vient d'apprendre, et chacune sera revue dans la matinée.

  | Cause | Vu au bloc | Revu à |
  |---|---|---|
  | Orthographes divergentes — majuscules, espaces, `cf.`, `sp.`, fautes de frappe | — | 10h00, rattrapé automatiquement |
  | Taxonomie qui a bougé | B | 10h00, sur le fichier d'exemple |
  | Référentiels différents d'une source à l'autre | C | 11h45, *Dacryodes* / *Pachylobus* |

  **Ne pas réduire le problème à « la taxonomie bouge ».** C'est la cause la plus visible,
  pas la plus fréquente. Deux sources peuvent être parfaitement à jour et ne pas s'aligner,
  simplement parce qu'elles suivent deux référentiels — c'est tout l'objet du bloc C. Et
  une faute de frappe suffit à créer une espèce de plus dans un comptage, ce qui ramène à
  la question 1.
- **Slide 3 ensuite** — les occurrences sont aujourd'hui largement accessibles, grâce aux
  dépôts globaux et aux bonnes pratiques d'archivage. Les données d'inventaire, non. Où et
  quand les forêts d'Afrique centrale ont-elles été inventoriées ? Poser la question
  vraiment, marquer un silence : personne ne peut y répondre de manière consolidée.
- **Slide 4 enfin** — et le problème s'aggrave tout seul : l'accessibilité décroît avec le
  temps écoulé depuis la collecte (Tessarolo et al. 2017), pour trois raisons — archivage
  défaillant, absence de ressources au-delà de la durée des projets, réticence à rendre
  accessible. La troisième est la plus coûteuse : elle fait perdre définitivement des
  données durement acquises.

L'élargissement se fait ainsi par degrés — leur fichier, puis leur pratique, puis leur
domaine — au lieu de sauter directement du premier au troisième.

**Pourquoi la troisième slide n'est plus en réserve.** Les deux premières décrivent un
état ; celle-ci décrit une *dynamique*. C'est elle qui rend la suite inévitable : si rien
n'est fait, la situation se dégrade d'elle-même, et chaque année sans dispositif coûte des
données qui existaient encore l'année précédente. Le réseau arrive juste après comme la
réponse à cette pente, et non comme une proposition parmi d'autres.

**La phrase de bascule vers le bloc E** : *« la boucle se referme : on ne peut pas compiler
sans standardiser les noms, et on ne peut pas standardiser les noms si personne ne sait
quel référentiel a servi. »*

**Si vous ne gardez qu'une des trois slides**, gardez *Compiler* : c'est celle qui parle
d'eux.

**La slide 4 en réserve.** L'accessibilité décroît avec le temps écoulé depuis la collecte
(Tessarolo et al. 2017) : archivage défaillant, absence de ressources au-delà de la durée des
projets, réticence à rendre accessible — la troisième étant la plus coûteuse, puisqu'elle
fait perdre définitivement des données durement acquises.

Elle est sortie du plan de base parce qu'elle est la plus documentée et la plus parlante
*pour vous*, alors que pour eux la slide 6 porte tout. La ressortir dans deux cas : si le
bloc C a été plus rapide que prévu, ou si le tour de table a fait apparaître quelqu'un qui
travaille sur des données anciennes ou orphelines — auquel cas elle cesse d'être un argument
général et devient sa situation, ce qui vaut la minute qu'elle coûte.

## Bloc E — 09h33, trois minutes

**Slides ressac 12, 8 et 11.** C'est ici, et seulement ici, qu'on prononce le nom du
réseau.

- **Slide 12, en transition** — pourquoi un réseau régional alors que GBIF, TRY et
  forestplot.net existent. L'argument : les outils globaux organisent un partage
  unilatéral, avec des interactions faibles ou nulles entre collecteurs, curateurs et
  utilisateurs ; l'échelle du biome a un sens biogéographique et un sens de gestion.
  **« Ce n'est pas une plateforme de données, c'est un réseau de réseaux »** — la phrase à
  faire entendre, et la seule du bloc qui doit rester en mémoire. Trente secondes, à
  l'écran, sans parler par-dessus.
- **Slide 8 — les principes**, et c'est le cœur du bloc. Ils sont formulés comme des
  principes et non comme des avantages, parce que c'est ce qu'ils sont, et parce que c'est
  ce qui distingue un réseau d'une plateforme. Quatre :
  1. **Souveraineté** — chaque réseau, chaque groupe reste maître de ses données et de
     leur utilisation ; rien n'est mis en commun sans décision de celui qui l'a produit.
  2. **Gestion commune** — un référentiel taxonomique et des structures de données
     partagés, ce qui rend les analyses reproductibles d'un réseau à l'autre.
  3. **Partage d'outils** — scripts et applications reproductibles, pour la recherche
     comme pour l'enseignement.
  4. **Visibilité** — rendre visibles les réseaux d'inventaires, y compris les dispositifs
     historiques en voie de disparition, ce qui renvoie directement à la slide précédente.

  **Le premier est en tête et en inversé, délibérément.** Devant un public qui n'a pas
  encore de données dans le réseau, c'est le seul qui compte vraiment : tant qu'il n'est pas
  posé, les trois autres ressemblent à une demande. Le formuler positivement, pas comme une
  réserve juridique — et ne pas le survoler, c'est lui qui rend le bloc F crédible trois
  minutes plus tard.
- **Slide 11** — les chiffres, en vingt secondes : plus de 300 000 occurrences, plus de
  1 300 inventaires de 1991 à 2025, plus de 300 000 traits au niveau individu, environ
  17 000 au niveau taxon. Ne pas les commenter un par un. Ils servent à une seule chose :
  établir que ce dont on parle existe et a une taille.

> **À vérifier avant samedi** — ces chiffres viennent de la présentation ressac et doivent
> être confirmés ou mis à jour. Un chiffre faux énoncé devant une salle de doctorants est
> cher payé. Vérifier aussi l'adresse : la présentation affiche `cafriplot.org`, le mail
> d'invitation envoie vers <https://www.cafriplot.net/>. Les deux ne peuvent pas être sur la
> même slide.

## Bloc F — 09h36, quatre minutes

Le bloc le plus important pour ce groupe précis, et celui qui n'existe pas dans la
présentation ressac.

### L'idée

Il y a **deux manières d'utiliser tout ça**, et il faut les distinguer nettement :

| | Usage ponctuel | Gérer ses données dans l'infrastructure |
|---|---|---|
| Quand | **ce matin** | **cet après-midi**, en introduction, faute de temps |
| Ce que vous faites | vous arrivez avec un fichier, vous repartez avec le même fichier en propre | vos inventaires vivent dans la base et vous les mettez à jour |
| Qui les voit | personne, rien n'est déposé | **vous seul** — plus ceux à qui vous les avez explicitement ouverts |
| Compte nécessaire | non | oui |
| Connexion | pas même nécessaire, le référentiel peut être en cache | oui |

**Ce matin, c'est la colonne de gauche.** Le dire explicitement, en ces termes :

> *« Ce qu'on va faire ce matin ne suppose aucun compte, aucun dépôt de données, et peut
> tourner sans internet. Vous arrivez avec votre fichier, vous repartez avec votre fichier
> propre. Rien ne reste ici. »*

Puis annoncer la colonne de droite comme **un contenu de la journée**, pas comme un « un
jour peut-être » : elle sera introduite cet après-midi, brièvement. Ceux qui partent à midi
savent ainsi ce qu'ils ne verront pas ; ceux qui hésitent à rester savent pourquoi rester.

### La ligne « qui les voit » est la mise en œuvre du principe 1

C'est une correction de fond par rapport à une première version de cette slide, qui disait
« vous les mettez à jour, d'autres les voient ». **C'est l'inverse.** Dans l'infrastructure,
chacun ne voit que les inventaires qu'il a créés, plus ceux qui lui ont été explicitement
ouverts — l'accès est déterminé par `created_by` et par des autorisations accordées une par
une (`define_user_policy()`). Personne ne voit les vôtres par défaut.

La souveraineté annoncée au bloc E n'est donc pas un principe affiché : c'est la manière
dont la base est construite, et c'est exactement ce qu'il faut dire. La question se pose
dans toutes les têtes et ne sera posée par personne ; la réponse est meilleure que ce que
la salle imagine.

### Pourquoi ce bloc mérite ses quatre minutes

Elle lève l'objection que personne ne formulera à voix haute mais que la moitié de la salle
aura en tête depuis le bloc E : *est-ce qu'on est en train de me demander mes données ?*
Tant que cette question reste ouverte, une partie de l'attention est occupée à la surveiller.

C'est aussi ce qui rend la colonne de droite audible plus tard. Une porte qu'on montre
ouverte sans pousser personne dedans est plus engageante qu'une porte devant laquelle on
insiste.

### Transition vers la mise en route

*« On ouvre l'application. À 10h00 j'arrête le dépannage, donc si quelque chose coince,
dites-le maintenant et pas dans un quart d'heure. »*

## Les trois questions sont des dettes

Chacune des trois est payée plus tard dans la matinée. C'est ce qui fait qu'elles ne sont
pas un artifice d'animation, et il vaut la peine de le signaler en les remboursant : *« la
question de 9h15, voilà la réponse »*.

| Question | Posée | Remboursée | Par quoi |
|---|---|---|---|
| 1. Combien de noms, combien d'espèces | 09h15 | 10h00 | *Chlorophora* / *Milicia excelsa* et *Polyalthia* / *Greenwayodendron suaveolens* dans le fichier d'exemple, puis leur propre fichier à 10h45 |
| 2. Une espèce ou deux | 09h18 | 10h00 | `Aningeria altissima` ligne 2 du fichier, résolu en *Pouteria altissima* sans rien demander |
| 3a. Où allez-vous vérifier un nom | 09h22 | 09h24 | immédiatement, par la définition construite à partir de leurs propres réponses |
| 3b. Quel référentiel suit votre liste | 09h26 | 11h45 | *Dacryodes* / *Pachylobus* : même fichier, deux référentiels, deux listes d'espèces, un identifiant qui ne bouge pas |

La 3a est la seule qui se rembourse dans la minute, et c'est ce qui la rend supportable :
elle a l'air d'une question de cours, elle sert en réalité à collecter le matériau de la
définition. Personne ne doit avoir l'impression d'avoir été interrogé pour rien.

Si une seule des trois doit être remboursée à voix haute, c'est la troisième : c'est celle
dont la réponse est la moins intuitive et la plus utile.

## Ce qui reste à faire sur cette séquence

- **Le jeu de slides est fait** : `formation_septembre_2026_slides_raison_detre.Rmd`,
  treize slides dont une de réserve (l'accessibilité dans le temps, la slide ressac 4).
  Les notes d'animation de chaque bloc sont dans les notes de présentateur de la slide
  correspondante. Pour les afficher, ouvrir le `.html` et appuyer sur **`p`** ; `Échap`
  les cache. Ne pas se fier à `?presentme=true` depuis le disque : ce paramètre a besoin
  de `localStorage`, que Chrome désactive sur les URL `file://`, et ouvre une popup que le
  navigateur bloque — il ne marche qu'en `http://`.
- **Mais `p` n'est pas utilisable pendant la séance** : les notes s'affichent dans la
  fenêtre du deck, donc dans ce qui est partagé en Zoom. Le dispositif pour samedi est
  donc : partager **la fenêtre du navigateur** contenant le deck, pas l'écran entier, et
  garder ce document-ci ouvert dans une seconde fenêtre sur votre écran. C'est le même
  contenu que les notes, en plus complet. La vraie fenêtre présentateur d'ioslides existe
  (`servr::httd("inst/docs")` puis l'URL en `http://` avec `?presentme=true`, popup
  autorisée) mais ajoute un serveur, une popup et un blocage possible — trois points de
  défaillance un matin où la connexion est déjà le risque principal.
- Une différence avec le découpage ci-dessus, imposée par ioslides : le bloc E est sur deux
  slides au lieu d'une. La phrase « ce n'est pas une plateforme de données, c'est un réseau
  de réseaux » est un titre de niveau 1, donc une slide de transition sur fond sombre ; les
  trois points qui la soutiennent sont sur la slide suivante. Le découpage est meilleur
  ainsi — l'affirmation d'abord, ce qu'elle implique ensuite.
- Vérifier les chiffres de la slide `chiffres` — ils sont **entre crochets** dans le jeu
  tant qu'ils ne sont pas confirmés — et trancher entre `cafriplot.org` et `cafriplot.net`.
- Décider du support d'écriture : le paperboard doit porter **deux** listes jusqu'à midi,
  les paires de chiffres du bloc A et les référentiels du bloc C. Une seule feuille ne
  suffira pas.
- Relire `taxonomic_backbones_fr.md`, qui documente les deux référentiels du package et leur
  table de correspondance. Rien à en projeter — c'est trop technique pour ce créneau — mais
  c'est la source à citer si quelqu'un demande où lire la suite, et c'est de là que vient la
  filiation RAINBIO / herbier de Wageningen mentionnée au bloc C.
- Les questions 4 à 7 de la section 6 du déroulé restent à retravailler ; elles relèvent des
  créneaux de 10h00 et 11h45, pas de celui-ci.
