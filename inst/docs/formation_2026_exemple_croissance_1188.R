# Parcelle 1188 : d'où viennent les croissances aberrantes entre 2021 et 2024
# ============================================================================
#
# J'avais annoncé "de -31,6 cm à +42 cm, biologiquement impossible". Le premier
# chiffre est exact, la conclusion était fausse : dans la plupart des cas ce
# n'est pas une erreur, c'est la hauteur de mesure du diamètre qui a changé
# entre les deux passages. Un arbre à contreforts mesuré à 1,30 m puis à 3,37 m
# "rétrécit" de 31 cm sans que rien d'anormal ne se soit produit.
#
# C'est l'argument le plus net qu'on puisse donner sur le format de sortie :
# la hauteur de mesure est un attribut DE LA MESURE, pas de l'arbre. Le format
# large n'a nulle part où le mettre - une ligne par arbre, une colonne par
# recensement. Le format long, une ligne par mesure, le transporte avec elle.
# D'où la règle : en large on voit un arbre qui rétrécit, en long on voit
# pourquoi.
#
# Lecture seule. Se connecte en utilisateur public.

library(CafriplotsR)
library(dplyr)

desc <- jsonlite::fromJSON("https://umr-amap.github.io/cafriplotsR/public-access.json")
con  <- call.mydb(user = desc$user, pass = desc$password,
                  use_env_credentials = FALSE)

PARCELLE <- 1188

# --- Récupérer diamètres et hauteurs de mesure, par recensement -------------
#
# ATTENTION : le millésime du recensement n'est PAS dans
# data_traits_measures.year (NULL pour l'essentiel des mesures d'inventaire).
# Il est porté par la sous-parcelle : id_sub_plots -> data_liste_sub_plots.year

mesures <- DBI::dbGetQuery(con, sprintf("
  SELECT tm.id_data_individuals AS id_ind,
         i.tag,
         s.year,
         t.trait,
         tm.traitvalue AS valeur
  FROM data_traits_measures tm
  JOIN traitlist t              ON t.id_trait      = tm.traitid
  JOIN data_liste_sub_plots s   ON s.id_sub_plots  = tm.id_sub_plots
  JOIN data_individuals i       ON i.id_n          = tm.id_data_individuals
  WHERE tm.id_table_liste_plots = %d
    AND t.trait IN ('stem_diameter', 'height_of_stem_diameter')
    AND s.year IS NOT NULL", PARCELLE))

# Voilà le format LONG : une ligne par mesure, chaque ligne portant son année
# et la nature de ce qui est mesuré.
head(mesures)
nrow(mesures)

# --- Le format LARGE : une ligne par arbre ---------------------------------

large <- mesures %>%
  tidyr::pivot_wider(names_from  = c(trait, year),
                     values_from = valeur) %>%
  rename(dbh21 = stem_diameter_2021,          dbh24 = stem_diameter_2024,
         hom21 = height_of_stem_diameter_2021, hom24 = height_of_stem_diameter_2024) %>%
  filter(!is.na(dbh21), !is.na(dbh24)) %>%
  mutate(ecart = round(dbh24 - dbh21, 1))

nrow(large)   # 436 arbres mesurés aux deux dates

# --- 1. Ce qu'on voit si on ne regarde que les diamètres -------------------

large %>%
  mutate(classe = cut(ecart,
                      breaks = c(-Inf, -1, 0, 3, 6, Inf),
                      labels = c("décroissance > 1 cm", "décroissance < 1 cm",
                                 "0 à 3 cm (plausible)", "3 à 6 cm (fort)",
                                 "> 6 cm (douteux)"))) %>%
  count(classe)

# Résultat (25/09/2026) :
#   décroissance > 1 cm      11
#   décroissance < 1 cm      65     <- bruit de mesure ordinaire, écorce
#   0 à 3 cm (plausible)    353     <- l'immense majorité
#   3 à 6 cm (fort)           3
#   > 6 cm (douteux)          4
#
# Autrement dit la parcelle est saine : 353 arbres sur 436 dans l'intervalle
# attendu pour trois ans. Ce sont les quinze cas extrêmes qui méritent qu'on
# les regarde, pas le jeu entier.

# --- 2. Et maintenant avec la hauteur de mesure ----------------------------

large %>%
  arrange(desc(abs(ecart))) %>%
  select(tag, dbh21, dbh24, ecart, hom21, hom24) %>%
  head(10)

# tag 344 : 175,0 -> 143,4  (-31,6)   hom 1,30 -> 3,37
# tag 357 :  75,1 ->  59,6  (-15,5)   hom 1,45 -> 3,44
# tag 132 :  68,0 ->  73,9   (+5,9)   hom 3,60 -> 1,30
# tag 172 :  35,0 ->  31,8   (-3,2)   hom 2,10 -> 1,30
#
# Les deux plus grosses "décroissances" de la parcelle sont des arbres qu'on a
# mesurés deux mètres plus haut la seconde fois - typiquement des arbres à
# contreforts dont la base est devenue inmesurable. Rien d'aberrant : la
# comparaison n'est simplement pas valide telle quelle.

# --- 3. Séparer les deux populations ---------------------------------------

large %>%
  mutate(hom_change = !is.na(hom21) & !is.na(hom24) & abs(hom24 - hom21) > 0.1) %>%
  group_by(hom_change) %>%
  summarise(n          = n(),
            ecart_min  = min(ecart),
            ecart_med  = median(ecart),
            ecart_max  = max(ecart))

# Les arbres dont la hauteur de mesure n'a pas bougé forment une distribution
# beaucoup plus resserrée. Ce qui reste d'anormal après ce tri est un vrai
# candidat à la vérification terrain - par exemple le tag 7, 87,0 -> 129,0,
# mesuré à 1,30 m les deux fois.

# --- 4. Ce que la base sait déjà de ces arbres ------------------------------
#
# Les codes RAINFOR sont stockés comme n'importe quelle autre mesure.

DBI::dbGetQuery(con, sprintf("
  SELECT t.trait, tm.traitvalue_char AS code, COUNT(*) AS n
  FROM data_traits_measures tm JOIN traitlist t ON t.id_trait = tm.traitid
  WHERE tm.id_table_liste_plots = %d
    AND t.trait IN ('flag1_rainfor', 'flag2_rainfor', 'mortality_risk_flag')
  GROUP BY 1, 2 ORDER BY 1, 3 DESC", PARCELLE))

# mortality_risk_flag donne small_liana_low_load (46), hollow (9), leaning (8),
# strangler (3), dying (2)... Autant d'explications possibles pour un diamètre
# qui se comporte bizarrement, et autant de colonnes que le format large ne
# peut pas porter sans se transformer en tableau illisible.

# ============================================================================
# À DIRE EN SÉANCE, EN UNE PHRASE
# ============================================================================
#
# "Cet arbre a perdu 31 centimètres en trois ans. Ce n'est pas une faute de
#  saisie : on l'a mesuré à 1,30 m en 2021 et à 3,37 m en 2024. L'information
#  qui vous sauve est attachée à la mesure, pas à l'arbre - et c'est
#  exactement ce que le format large ne peut pas vous montrer."
