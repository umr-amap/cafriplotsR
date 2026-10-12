# Ouvrir la parcelle 1188 pour l'après-midi de la formation du 26/09/2026
# ============================================================================
#
# Pourquoi. Les 83 inventaires déjà ouverts au login public (transects Tombi /
# Mbandza / Ekagna au Congo, placettes Doudou 40x40 au Gabon) ont tous été
# relevés UNE SEULE FOIS - vérifié le 25/09/2026 : zéro entrée de type "census"
# dans data_liste_sub_plots pour ces 83 parcelles. Sur un inventaire à un seul
# passage, la distinction large / long / paires de recensements ne se voit pas :
# le format large a une seule colonne de dbh, le format long a autant de lignes
# que d'arbres, et les paires de recensements ne renvoient rien. Il faut une
# parcelle à plusieurs recensements pour que la démonstration ait lieu.
#
# Ce script ouvre cette parcelle au login public existant plutôt que de créer un
# compte : les participants cliquent le même bouton "utilisateur public" que le
# matin, il n'y a pas de mot de passe à distribuer ni à récupérer, et la
# fermeture est d'une ligne.
#
# À exécuter à la main, étape par étape. Rien ici n'est automatique.

library(CafriplotsR)
con <- call.mydb()   # votre compte, pas le public

parcelle_formation <- 1188

# --- PROFIL DE 1188, vérifié le 25/09/2026 ---------------------------------
#
#   456 individus, tous identifiés, 82 taxons
#   1 ha (coins de sous-parcelles sur une grille 0-100 x 0-100 m)
#   deux recensements : 2021 et 2024
#   445 diamètres en 2021, 447 en 2024, et 436 arbres mesurés AUX DEUX dates
#   croissance moyenne sur l'intervalle : +0,32 cm en 3 ans
#
# Ce que cela donne dans les trois formats :
#   large  -> 456 lignes, deux colonnes de dbh (2021, 2024)
#   long   -> ~892 lignes, une par mesure
#   paires -> 436 lignes, un intervalle de 3 ans par arbre, avec dbh0, dbh1
#
# Deux recensements suffisent, et c'est même plus lisible que cinq : le tableau
# large reste projetable.
#
# BONUS pour la séquence : l'écart 2021-2024 va de -31,6 cm à +42 cm, et ce ne
# sont PAS des erreurs. Les deux plus fortes décroissances sont des arbres dont
# la hauteur de mesure a changé (tag 344 : 1,30 m -> 3,37 m). La hauteur de
# mesure est un attribut de la mesure, pas de l'arbre : le format large n'a
# nulle part où la mettre, le format long la transporte. C'est l'argument le
# plus concret qu'on puisse donner sur le choix du format.
# Détail et script : formation_2026_exemple_croissance_1188.R

# --- ÉTAPE 1 : vérifier que la parcelle est bien à vous ---------------------
#
# Ouvrir la parcelle de quelqu'un d'autre sans son accord contredirait très
# exactement le principe de souveraineté présenté le matin même.

DBI::dbGetQuery(con, "
  SELECT p.id_liste_plots, p.plot_name, m.method, p.area_plot, p.country,
         p.locality_name, p.created_by, p.id_citation
  FROM data_liste_plots p
  LEFT JOIN methodslist m ON m.id_method = p.id_method
  WHERE p.id_liste_plots = 1188")

# --- ÉTAPE 2 : ouvrir ------------------------------------------------------
#
# FAIT LE 25/09/2026, mais autrement : l'accès est passé par le compte
# `user_test2` plutôt que par le login public. Ce qui suit corrige ce compte et
# le referme. La variante « login public » est conservée plus bas pour mémoire.

# ---- 2a. CORRECTIF À APPLIQUER AVANT SAMEDI -------------------------------
#
# Audit du 25/09 : user_test2 n'est PAS en lecture seule. Il porte trois
# politiques - _select, _update, _delete - sur huit parcelles :
#
#   403, 641, 677, 685, 687, 726, 727, 1188
#
# Autrement dit, quiconque a ces identifiants peut MODIFIER ou SUPPRIMER la
# fiche de ces huit parcelles, dont celle de la démonstration. Les arbres, eux,
# sont bien en lecture seule (INSERT/UPDATE/DELETE = FALSE sur
# data_individuals, data_traits_measures, data_ind_measures_feat).
#
# À dix personnes dans une salle, ce n'est pas un risque théorique : il suffit
# d'un clic dans une application d'import ou d'une ligne tapée dans R.

parcelles_user_test2 <- c(403, 641, 677, 685, 687, 726, 727, 1188)

# mode = "replace" (le défaut) supprime les politiques existantes et n'en
# recrée qu'une, en SELECT.
define_read_only_policy(con, "user_test2", parcelles_user_test2)

# La politique ne fait pas tout : define_user_policy() exécute de toute façon
# GRANT SELECT, INSERT, UPDATE, DELETE au niveau de la table
# (connections_db.R, dans le bloc grant_table_privileges). La RLS bloque ces
# écritures tant qu'aucune politique ne les autorise, mais autant retirer le
# privilège lui-même :
DBI::dbExecute(con, 'REVOKE INSERT, UPDATE, DELETE ON data_liste_plots FROM "user_test2"')

# Vérifier que c'est bien retombé :
DBI::dbGetQuery(con, "
  SELECT policyname, cmd FROM pg_policies
  WHERE tablename='data_liste_plots' AND roles::text LIKE '%user_test2%'")   # SELECT seul
DBI::dbGetQuery(con, "
  SELECT has_table_privilege('user_test2','data_liste_plots','DELETE') AS del,
         has_table_privilege('user_test2','data_liste_plots','UPDATE') AS upd")  # FALSE, FALSE

# ---- 2b. CE QUE LES PARTICIPANTS VERRONT ----------------------------------
#
# Huit parcelles, pas une. Six (641, 677, 685, 687, 726, 727) sont déjà
# ouvertes au login public, donc sans exposition supplémentaire. Deux ne le
# sont pas : 403 et 1188. Vérifier que 403 est bien à vous avant samedi.

DBI::dbGetQuery(con, "
  SELECT id_liste_plots, plot_name, created_by FROM data_liste_plots
  WHERE id_liste_plots IN (403, 1188)")

# ---- 2c. VARIANTE NON RETENUE : le login public ---------------------------
#
# define_read_only_policy(con, "CafriP_public", 1188, mode = "add")
#
# Évitait la distribution d'identifiants, mais exposait la fiche de 1188 à
# tout internet pendant la durée de l'ouverture. Le compte dédié est plus
# propre sur ce point.

# --- ÉTAPE 3 : vérifier, en vrai ------------------------------------------
#
# Se connecter réellement AVEC user_test2 et regarder ce qu'il voit. Une
# politique qui s'applique sans erreur n'est pas une politique qui marche.
#
#   con_test <- call.mydb(user = "user_test2", pass = "<le mot de passe>",
#                         use_env_credentials = FALSE, reset = TRUE)
#   DBI::dbGetQuery(con_test, "SELECT id_liste_plots, plot_name FROM data_liste_plots")
#
# Attendu : les huit parcelles, et une tentative d'écriture qui ne passe pas :
#
#   DBI::dbExecute(con_test, "UPDATE data_liste_plots SET notes='x' WHERE id_liste_plots=1188")
#   # doit renvoyer 0 ligne affectée, ou une erreur de permission
#
# Puis le test qui compte vraiment, dans l'application, en cliquant :
#
#   launch_query_plots_app()
#   -> se connecter avec user_test2
#   -> filtrer sur 1188, extraire en format large, puis long, puis en paires
#
# Les trois sorties doivent visiblement différer (456 / ~892 / 436 lignes).
# Si elles ne diffèrent pas, la séquence de l'après-midi n'a pas lieu d'être.

# --- ÉTAPE 4 : refermer, dimanche 27 ---------------------------------------
#
# define_read_only_policy(con, "user_test2", setdiff(parcelles_user_test2, 1188))
#
# Puis, pour de bon - `user_test2` fait partie des neuf comptes de test que
# PLAN_SECURITY_REMEDIATION.md (P0.7) prévoit de supprimer de la production.
# Le réutiliser pour la formation reporte cette suppression ; ne pas la perdre
# de vue :
#
#   deactivate_user(con, "user_test2")
#
# et noter dans le plan que P0.7 reste à faire pour user_test2 … user_test10.


# ============================================================================
# NOTE DE SCHÉMA - à lire avant de requêter des recensements
# ============================================================================
#
# Le millésime d'un recensement N'EST PAS dans data_traits_measures.year, qui
# est NULL pour l'essentiel des mesures d'inventaire. Il est porté par la
# sous-parcelle de type "census" :
#
#   data_traits_measures.id_sub_plots -> data_liste_sub_plots.year
#
# Compter les recensements via data_traits_measures.year donne zéro parcelle
# multi-recensements dans toute la base, ce qui est faux. Sur 1188, le bon
# chemin donne 445 diamètres en 2021 et 447 en 2024 ; le mauvais en donne 444
# en 2021, 1 en 2016, et 447 sans année.


# ============================================================================
# CE QUE L'OUVERTURE EXPOSE - et l'état réel de la RLS
# ============================================================================
#
# Le login public est publié sur internet (public-access.json sur gh-pages).
# Ouvrir 1188 rend sa fiche - nom, coordonnées, méthode, localité, auteur -
# lisible par n'importe qui pendant la durée de l'ouverture.
#
# Mais ses arbres le sont DÉJÀ, et ceux de toutes les autres parcelles avec :
# la RLS ne couvre que data_liste_plots et taxa_traits_measures. Vérifié le
# 25/09/2026 avec le seul login public : 446 156 individus sur 2 081 parcelles,
# 476 701 mesures individuelles, 42 320 spécimens, 2 053 481 mesures de traits.
#
# Et contrairement à ce qu'on pourrait croire, ces données ne sont pas
# anonymes : data_traits_measures est dénormalisée et porte ses propres
# colonnes de métadonnées, hors de toute RLS. Sur les parcelles que la RLS
# masque, ce login lit :
#
#   - decimallatitude / decimallongitude pour 868 parcelles masquées
#     (4 décimales, soit ~11 m), plus country
#   - original_plot_name pour 64 d'entre elles
#   - le collecteur, nommable et rattaché à son institution, via
#     id_colnam -> table_colnam (Texier/MBG, Libalah/Yaoundé I, Barbier/AMAP,
#     Sonké, Boupoya/Herbier National du Gabon...)
#   - les dates, via data_liste_sub_plots
#
# Autrement dit : coordonnées à ~10 m, date, auteur, et la liste complète des
# arbres avec tags, taxons, diamètres et positions. C'est un jeu d'occurrences
# publiable, attribué et géolocalisé, sur des parcelles qui appartiennent à des
# collègues nommés.
#
# C'est le point P4.2 de PLAN_SECURITY_REMEDIATION.md, "Extend RLS to the child
# tables, or stop claiming it works", ouvert depuis août. Le protéger suppose
# d'étendre la RLS aux tables filles ET de traiter la copie dénormalisée des
# métadonnées dans data_traits_measures - que P4.2 ne mentionne pas.
