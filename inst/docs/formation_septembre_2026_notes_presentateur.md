---
title: "Raison d'être du package — notes de présentateur"
subtitle: "09h15–09h40. À garder ouvert à côté de l'écran partagé, jamais dedans."
lang: fr
toc: false
format:
  html:
    embed-resources: true
---

> Document généré depuis `formation_septembre_2026_slides_raison_detre.Rmd`
> par `formation_2026_extraire_notes_presentateur.R`. Ne pas l'éditer à la main :
> corriger les `<div class="notes">` du `.Rmd` et relancer le script.

**Ne pas appuyer sur `p`** pendant la présentation : en écran partagé, les notes
s'afficheraient devant la salle. Les flèches suffisent pour avancer.

## Repères

| # | Slide | Repère |
|---:|---|---|
| 1 | Votre dernier fichier de noms d'espèces | Bloc A — 09h15, trois minutes |
| 2 | Aningeria / Pouteria — Une espèce, ou deux ? | Bloc B — 09h18 |
| 3 | Une seule espèce, plusieurs noms | Bloc B, la réponse |
| 4 | Un référentiel taxonomique | Bloc C — 09h22, six minutes |
| 5 | Deux référentiels ne disent pas la même chose | Bloc C, fin |
| 6 | Compiler, tout le monde le fait | Bloc D — 09h28, quatre minutes |
| 7 | Compiler n'est pas le seul problème | Bloc D, charnière |
| 8 | Les occurrences sont accessibles. Pour ce qui est des autres données primaires, c'est moins le cas | Bloc D, suite |
| 9 | L'accessibilité décroît avec le temps écoulé depuis la collecte | Bloc D, fin |
| 10 | ▸ Cafriplot - Ce n'est pas une plateforme de données. C'est un réseau de réseaux. | Bloc E — 09h32, quatre minutes |
| 11 | Les principes du réseau | Bloc E, suite |
| 12 | Ce que le réseau réunit aujourd'hui | Bloc E, fin |
| 13 | Deux manières d'utiliser tout ça | Bloc F — 09h36, quatre minutes |
| 14 | On ouvre l'application | Transition vers la mise en route — 09h40 |

---

## 1. Votre dernier fichier de noms d'espèces

**Bloc A — 09h15, trois minutes. Question 1.**

« Prenez votre dernier fichier, celui dont vous venez de me parler. Deux chiffres. Premier :
combien de noms différents il y a dedans, ça vous pouvez le compter. Deuxième : combien
d'espèces, à votre avis. Discutez trente secondes avec votre voisin. »

Nommer « premier » et « deuxième » est délibéré : sans ça la salle répond un seul nombre,
parce qu'elle ne voit pas encore pourquoi il y en aurait deux.

Trois personnes maximum. Écrire les paires au paperboard, en grand, et les laisser visibles
jusqu'à midi. Dire à voix haute que « je ne sais pas » dans la deuxième colonne est la
meilleure réponse : celui qui recopie le premier chiffre est celui qui n'a pas encore vu le
problème.

Ne PAS expliquer ici pourquoi l'écart existe — c'est le bloc B, et la tentation coûte deux
minutes.

## 2. Aningeria / Pouteria — Une espèce, ou deux ?

**Bloc B — 09h18. Question 2.**

« Mains levées : qui dit un ? qui dit deux ? qui ne se prononce pas ? » Trois mains comptées,
quinze secondes, pas de discussion à deux. On veut la répartition brute, et surtout la
troisième colonne.

La salle va se partager — c'est tout l'intérêt. L'hésitation collective démontre mieux que
n'importe quelle explication que la synonymie n'est pas une connaissance qu'on peut supposer
partagée.

Puis la phrase qui compte : « Vous êtes une salle de spécialistes de la forêt d'Afrique
centrale et vous n'êtes pas d'accord. Ce n'est pas un problème de compétence. C'est que
l'information n'est pas dans le nom. »

Ne pas annoncer que ce nom est dans le fichier d'exemple (ligne 2). Ils le verront se
résoudre tout seul à 10h00 ; la surprise vaut mieux.

## 3. Une seule espèce, plusieurs noms

**Bloc B, la réponse. 09h19.**

L'iroko d'abord — le dire à voix haute, l'étiquette n'est pas sur la slide : la plupart des
gens dans la salle ont vu les deux noms et personne ne se trompe dessus. Il rend la seconde
ligne évidente.

Pour *Aningeria* : c'est une seule espèce, dont le nom accepté est *Pouteria altissima*.

**L'unité est l'espèce, jamais l'arbre.** Ne pas dire « c'est le même arbre » : dans le
fichier d'exemple, *Chlorophora excelsa* et *Milicia excelsa* sont bien deux arbres
différents, deux lignes avec deux numéros. Une espèce, deux noms, deux individus — les
trois niveaux sont distincts et c'est exactement ce que la matinée sépare. Dire « le même
arbre » les recolle au moment où on essaie de les défaire.

Enchaîner directement sur le bloc C sans commenter la ligne du bas : elle sera relue toute
seule.

## 4. Un référentiel taxonomique

**Bloc C — 09h22, six minutes. Le bloc le plus long, et il ne se coupe jamais.**

D'abord la question 3a, SANS le mot, **avant** d'afficher cette slide : « Vous avez un doute
sur un nom d'espèce. L'orthographe, ou bien : est-ce que c'est encore le nom actuel ? Vous
allez vérifier où ? Je veux du concret — un site, un livre, une personne, un fichier. »

Six à huit réponses au paperboard, sans les commenter. Il viendra : une flore, POWO, GBIF,
Tropicos, l'herbier, le collègue qui sait, le fichier du projet précédent, Wikipédia. Prendre
Wikipédia sans ironie : c'est une réponse honnête, et elle sert pour la première propriété —
on y trouve des noms, rien ne garantit qu'ils soient arbitrés.

PUIS afficher cette slide : « tout ce que vous venez de citer, ce sont des référentiels. »

La ligne du bas : un référentiel a une provenance, comme une donnée.

**À ne pas ouvrir** : la distinction nomenclature / taxonomie. Si quelqu'un lance le Code et
la priorité, une phrase pour fermer — « le Code dit si un nom est valablement publié, le
référentiel dit lequel on retient ; c'est la seconde question qui fait que deux listes
diffèrent » — puis enchaîner. Ce débat vaut dix minutes qu'on n'a pas.

## 5. Deux référentiels ne disent pas la même chose

**Bloc C, fin. 09h27.** Cinq secondes à l'écran, sans commentaire long : la salle la reverra
à 11h45 en vrai, sur son propre fichier.

Avant de l'afficher, la question 3b, qui est maintenant lisible parce que la notion vient
d'être posée : « votre liste d'espèces actuelle, celle de vos analyses, elle suit lequel de
ceux qui sont au tableau ? »

Laisser le silence. Ne pas sauver la salle. Relancer une fois, plus bas : « Personne ? Moi
non plus je ne saurais pas le dire pour la moitié des fichiers que je reçois. » Se compter
dedans change le statut de la question.

Si quelqu'un répond « la Flore du Gabon » ou « POWO » : valoriser, puis déplacer — « et le
fichier que votre collègue vous a envoyé l'an dernier, il suivait lequel ? »

Les trois phrases qui ferment le bloc : (1) un nom sans référentiel est incomplet ; (2) ce
n'est pas grave seul, ça le devient dès qu'on compile, et compiler c'est ce que tout le monde
fait ; (3) l'identifiant, lui, ne dépend pas du référentiel. La troisième est plantée, PAS
développée.

## 6. Compiler, tout le monde le fait

**Bloc D — 09h28, quatre minutes.** Slide ressac 6.

**C'est la charnière de la présentation, et elle se joue à l'oral.** La dernière phrase du
bloc C se termine par « ça le devient dès qu'on compile — et compiler, c'est ce que tout le
monde fait ». Le titre de cette slide est cette phrase. Enchaîner sans rien ajouter : la
transition est faite par la répétition, il n'y a pas de pont à construire.

L'élargissement se fait ici, et par degrés : on part de leur pratique à eux — tout le monde
dans la salle compile — et non d'un constat régional qui leur tomberait dessus sans
préavis.

**Le point 3 est un récapitulatif, pas une information nouvelle** — et c'est à ce titre
qu'il faut le dire. Les trois causes sont les trois choses que la salle vient d'apprendre,
et chacune sera revue dans la matinée :

- **orthographes divergentes** — le bruit banal, majuscules, espaces doubles, `cf.`, `sp.`,
  fautes de frappe. Rattrapé automatiquement, ils le verront à 10h00 ;
- **taxonomie qui a bougé** — le bloc B, *Chlorophora* / *Milicia*, *Aningeria* /
  *Pouteria* ;
- **référentiels différents** — le bloc C, et le cas *Dacryodes* / *Pachylobus* à 11h45.

Ne pas réduire le problème à « la taxonomie bouge » : c'est la cause la plus visible mais
pas la plus fréquente. Deux sources peuvent être parfaitement à jour et ne pas s'aligner,
simplement parce qu'elles suivent deux référentiels. Et une faute de frappe suffit à créer
une espèce de plus dans un comptage.

La boucle à faire entendre : on ne peut pas compiler sans standardiser les noms, et on ne
peut pas standardiser les noms si personne ne sait quel référentiel a servi.

La ligne du bas est la bascule vers la slide suivante — de leur fichier vers la région.

## 7. Compiler n'est pas le seul problème

**Bloc D, charnière. 09h30.** Slide ajoutée, sans équivalent ressac.

Elle existe pour une seule raison : le passage de *Compiler* au constat régional était
brutal. On quittait leur fichier pour l'Afrique centrale sans prévenir. Ici on annonce le
plan, et les deux slides suivantes deviennent le développement des points 2 et 3 au lieu de
deux constats qui s'enchaînent sans raison apparente.

**Le titre reprend celui de la slide précédente**, comme *Compiler* reprenait la dernière
phrase du bloc C. C'est le même procédé : la transition est faite par la répétition, il n'y
a pas de pont à construire.

**Le point 1 est grisé et se dit vite** — c'est un récapitulatif, ils viennent de le vivre.
Ne pas le relire, le désigner.

**La ligne du bas est ce qu'il faut réussir.** C'est l'architecture de toute la matinée en
une phrase, et elle distingue les deux objets qu'on a tendance à confondre : *CafriplotsR*
est un outil, il résout le point 1 — c'est ce qu'ils feront à 10h45. *Cafriplots* est un
réseau, et les points 2 et 3 ne se résolvent pas autrement que par des gens qui décident de
mettre en commun. Dit ici, cela pré-justifie le bloc E au lieu de l'annoncer comme un
changement de sujet.

**Bénéfice opératoire, si vous débordez :** cette slide rend les deux suivantes
sacrifiables. Leur contenu est annoncé ; vous pouvez passer directement au bloc E sans que
la salle sente un trou. À l'inverse, sauter celle-ci et garder les deux autres reproduit
exactement le problème qu'elle corrige.

## 8. Les occurrences sont accessibles. Pour ce qui est des autres données primaires, c'est moins le cas

**Bloc D, suite. 09h30.** Slide ressac 3.

Maintenant seulement on passe du « vous » au « nous » : le problème est structurel et
régional, pas une négligence individuelle. La salle y arrive préparée, parce qu'elle vient
de reconnaître sa propre pratique sur la slide précédente.

La question au milieu — où et quand les forêts d'Afrique centrale ont-elles été
inventoriées ? — est à poser vraiment, pas à lire. Un temps de silence avant la ligne du
bas.

**Si on vous demande ce qu'est une « donnée primaire ».** Le terme est sur la slide sans y
être défini. Réponse en trente secondes :

> Une observation d'organismes qu'on ne peut pas recalculer : un taxon, un lieu, une date,
> un protocole. Tout le reste — les cartes, les indices, les statuts de conservation — s'en
> déduit.

Le critère n'est pas que la donnée soit « brute », c'est qu'elle soit **irrécupérable**.
Une richesse spécifique se recalcule à partir des observations ; aucune analyse ne
reconstitue l'arbre qu'on a mesuré. C'est ce qui décide de ce qu'une infrastructure doit
conserver, et c'est un meilleur argument que le partage.

Deux prolongements, si la discussion s'ouvre :

- **« Primaire » ne veut pas dire « objectif ».** Quelqu'un a décidé que cet individu était
  *Milicia excelsa* : c'est une détermination, donc révisable. Le nom ne se pose pas sur la
  donnée, il en fait partie — et c'est la partie qui bouge. Ce qui ramène à la matinée.
- **Le terme couvre des objets très inégaux**, et c'est le sujet de cette slide. Une
  occurrence dit « ce taxon était là ». Un relevé avec protocole ajoute la surface et
  l'effort, donc l'absence interprétable et l'abondance. Un dispositif permanent répété
  ajoute le temps, donc la croissance, la mortalité et le recrutement. **Un inventaire
  porte son propre dénominateur** — c'est ce qui le sépare de données d'occurrences.

D'où la formulation à garder si on veut résumer la slide en une phrase : **le partage
s'arrête exactement là où la donnée devient coûteuse à produire et interprétable.**

Si le temps manque, c'est ce bloc entier qui saute — passer directement de C à E. C'est le
seul dont l'absence ne se remarque pas, parce qu'il justifie l'existence du réseau devant des
gens qui ne sont pas venus pour ça. Mais si vous ne gardez qu'une des deux slides, gardez
*Compiler* : c'est elle qui parle d'eux.

## 9. L'accessibilité décroît avec le temps écoulé depuis la collecte

**Bloc D, fin. 09h31.** Slide ressac 4. Troisième et dernier degré de l'élargissement.

Les deux slides précédentes décrivaient un état ; celle-ci décrit une **dynamique**, et
c'est ce qui la rend nécessaire plutôt que décorative. Le problème ne stagne pas, il
s'aggrave avec le temps écoulé depuis la collecte. Chaque année sans dispositif coûte des
données qui existaient encore l'année d'avant.

C'est aussi la slide qui rend la suite inévitable : si rien n'est fait, la situation se
dégrade d'elle-même. Le réseau arrive juste après comme la réponse à cette pente.

Si le tour de table a fait apparaître quelqu'un qui travaille sur des données anciennes ou
orphelines, s'appuyer sur son cas : l'argument cesse d'être général et devient sa
situation.

Si vous êtes en retard, c'est ici qu'on coupe en premier — les deux slides précédentes
portent l'essentiel.

## 10. ▸ Cafriplot - Ce n'est pas une plateforme de données. C'est un réseau de réseaux.

**Bloc E — 09h32, quatre minutes.** Slide ressac 12.

C'est ici, et seulement ici, qu'on prononce le nom du réseau. Tout ce qui précède a servi à le
rendre nécessaire.

C'est la seule phrase de ce bloc qui doit rester en mémoire — la laisser à l'écran quelques
secondes sans parler par-dessus.

L'argument derrière : les outils globaux organisent un partage unilatéral, avec des
interactions faibles ou nulles entre collecteurs, curateurs et utilisateurs. Apports réels
pour les grandes échelles, peu d'intérêt pour les approches régionales. L'échelle du biome a
un sens biogéographique et un sens de gestion.

## 11. Les principes du réseau

**Bloc E, suite. 09h33.** Slide ressac 8, formulée comme des principes plutôt que comme des
avantages — c'est ce qu'ils sont, et c'est ce qui distingue un réseau d'une plateforme.

**La souveraineté est en premier et en inversé, délibérément.** Devant un public qui n'a
pas encore de données dans le réseau, c'est le seul principe qui compte vraiment : tant
qu'il n'est pas posé, les trois autres ressemblent à une demande. Ne pas le survoler — et
le formuler positivement, pas comme une réserve juridique.

C'est aussi ce qui rend le bloc F crédible trois minutes plus tard, et ce qui explique la
règle de visibilité qu'on y montre : dans l'infrastructure, chacun ne voit que ses propres
inventaires. La souveraineté n'est pas un principe affiché, c'est la manière dont la base
est construite.

Le quatrième point vaut d'être dit lentement s'il y a dans la salle quelqu'un qui travaille
sur un dispositif ancien : les inventaires historiques sont exactement ce que le réseau
cherche à ne pas perdre, et ça renvoie à la slide précédente.

## 12. Ce que le réseau réunit aujourd'hui

**Bloc E, fin. 09h34.** Slide ressac 11. Vingt secondes, sans commenter les chiffres un par un.

Ils ne servent qu'à une chose : établir que ce dont on parle existe et a une taille.

**À FAIRE AVANT SAMEDI** — les quatre chiffres sont entre crochets parce qu'ils viennent de la
présentation ressac et n'ont pas été revérifiés. Un chiffre faux énoncé devant une salle de
doctorants coûte cher. Les confirmer ou les mettre à jour, et retirer les crochets.

Vérifier aussi l'adresse : la présentation ressac affiche cafriplot.org, le mail d'invitation
envoie vers cafriplot.net. Les deux ne peuvent pas coexister.

## 13. Deux manières d'utiliser tout ça

**Bloc F — 09h36, quatre minutes.** Le bloc qui n'existe pas dans la présentation ressac, et
le plus important pour ce groupe précis.

Le dire dans ces termes : « Ce qu'on va faire ce matin ne suppose aucun compte, aucun dépôt de
données, et peut tourner sans internet. Vous arrivez avec votre fichier, vous repartez avec
votre fichier propre. Rien ne reste ici. »

Puis la colonne de droite, annoncée comme un contenu de la journée et non comme un « un
jour peut-être » : on l'introduira cet après-midi, brièvement, faute de temps. Ceux qui
partent à midi savent ainsi ce qu'ils ne verront pas, et ceux qui restent savent pourquoi
rester.

**Le point de visibilité est la mise en œuvre du principe 1.** Dans l'infrastructure, vous
ne voyez que les inventaires que vous avez créés, plus ceux qu'on vous a explicitement
ouverts. Personne ne voit les vôtres par défaut. Le dire explicitement : c'est la question
que les gens se posent et qu'ils ne poseront pas, et la réponse est meilleure que ce qu'ils
imaginent.

Pourquoi ce bloc vaut quatre minutes : il lève l'objection que personne ne formulera à voix
haute mais que la moitié de la salle surveille depuis le bloc E — est-ce qu'on est en train
de me demander mes données ? Tant que la question reste ouverte, une partie de l'attention
est occupée à la surveiller.

Et c'est ce qui rend la colonne de droite audible. Une porte qu'on montre ouverte sans
pousser personne dedans est plus engageante qu'une porte devant laquelle on insiste.

## 14. On ouvre l'application

**Transition vers la mise en route — 09h40.** Slide d'attente pendant tout le créneau
09h40–10h00, pour que la ligne de lancement reste à l'écran.

Le point de contrôle de 10h00 est ferme et doit être annoncé ici, pas subi à 10h00. C'est le
dépannage d'installation qui a mangé la matinée entière lors de la session précédente.

Les deux relais dans la salle circulent à partir de maintenant. C'est eux qui signalent
« trois personnes sont bloquées au fond », ce qu'aucun partage d'écran ne montrera.

Rappel : la même ligne est sur l'antisèche A4 posée sur chaque table.

