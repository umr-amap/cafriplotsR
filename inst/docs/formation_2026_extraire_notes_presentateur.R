# Extraire les notes de présentateur des slides vers un document à lire à côté
# ============================================================================
#
# Les notes d'animation vivent dans les <div class="notes"> du .Rmd, et ioslides
# ne les montre qu'en appuyant sur 'p' - ce qui les affiche dans la fenêtre
# partagée, donc devant la salle. Inutilisable en écran partagé.
#
# Ce script les sort dans un document séparé, à garder ouvert sur un second
# écran ou à imprimer. Source unique : le .Rmd. À relancer après chaque
# modification des slides.
#
#   Rscript inst/docs/formation_2026_extraire_notes_presentateur.R
#
# Puis, pour le HTML et le PDF :
#   quarto render inst/docs/formation_septembre_2026_notes_presentateur.md
#   quarto render inst/docs/formation_septembre_2026_notes_presentateur.md --to typst

rmd  <- "D:/MonDossierR/database.transects/plotsdatabase/inst/docs/formation_septembre_2026_slides_raison_detre.Rmd"
dest <- "D:/MonDossierR/database.transects/plotsdatabase/inst/docs/formation_septembre_2026_notes_presentateur.md"

lines <- readLines(rmd, encoding = "UTF-8", warn = FALSE)

# --- Repérer les slides et leurs notes -------------------------------------
#
# Un titre de slide est un `##` (ou `#` pour une slide de rupture) hors bloc
# <style> et hors bloc de code. Les notes sont le <div class="notes"> qui suit.

in_style <- FALSE
in_fence <- FALSE
slides   <- list()
cur      <- NULL
in_notes <- FALSE

for (i in seq_along(lines)) {
  l <- lines[i]

  if (grepl("^<style", l))   in_style <- TRUE
  if (grepl("^</style>", l)) { in_style <- FALSE; next }
  if (in_style) next
  if (grepl("^```", l))      { in_fence <- !in_fence; next }
  if (in_fence) next

  # Nouveau titre de slide
  if (grepl("^#{1,2}[[:space:]]", l)) {
    if (!is.null(cur)) slides[[length(slides) + 1]] <- cur
    titre <- sub("^#{1,2}[[:space:]]+", "", l)
    titre <- trimws(gsub("\\{[^}]*\\}", "", titre))     # retirer {.flexbox .vcenter}
    segue <- grepl("^#[[:space:]]", l)                  # slide de rupture
    cur <- list(titre = titre, segue = segue, notes = character(0))
    in_notes <- FALSE
    next
  }

  if (is.null(cur)) next

  if (grepl('^<div class="notes">', l)) { in_notes <- TRUE;  next }
  if (in_notes && grepl("^</div>", l))  { in_notes <- FALSE; next }
  if (in_notes) cur$notes <- c(cur$notes, l)
}
if (!is.null(cur)) slides[[length(slides) + 1]] <- cur

cat(sprintf("%d slides trouvées\n", length(slides)))

# Titres de repli pour les slides sans titre visible
repli <- c("2" = "Aningeria / Pouteria — Une espèce, ou deux ?")
for (i in seq_along(slides)) {
  if (!nzchar(slides[[i]]$titre)) {
    slides[[i]]$titre <- if (as.character(i) %in% names(repli))
      repli[[as.character(i)]] else "(slide sans titre)"
  }
}

# --- Première ligne des notes : le repère horaire --------------------------

reperes <- vapply(slides, function(s) {
  if (length(s$notes) == 0) return("")
  first <- s$notes[which(nzchar(trimws(s$notes)))[1]]
  if (is.na(first)) return("")
  first <- gsub("\\*\\*|\\*|_", "", first)
  # garder ce qui précède le premier point, càd "Bloc D — 09h28, quatre minutes"
  sub("\\..*$", "", trimws(first))
}, character(1))

# --- Écrire le document ---------------------------------------------------

out <- c(
  "---",
  'title: "Raison d\'être du package — notes de présentateur"',
  'subtitle: "09h15–09h40. À garder ouvert à côté de l\'écran partagé, jamais dedans."',
  "lang: fr",
  "toc: false",
  "format:",
  "  html:",
  "    embed-resources: true",
  "---",
  "",
  "> Document généré depuis `formation_septembre_2026_slides_raison_detre.Rmd`",
  "> par `formation_2026_extraire_notes_presentateur.R`. Ne pas l'éditer à la main :",
  "> corriger les `<div class=\"notes\">` du `.Rmd` et relancer le script.",
  "",
  "**Ne pas appuyer sur `p`** pendant la présentation : en écran partagé, les notes",
  "s'afficheraient devant la salle. Les flèches suffisent pour avancer.",
  "",
  "## Repères",
  "",
  "| # | Slide | Repère |",
  "|---:|---|---|"
)

for (i in seq_along(slides)) {
  out <- c(out, sprintf("| %d | %s%s | %s |", i,
                        if (slides[[i]]$segue) "▸ " else "",
                        slides[[i]]$titre, reperes[i]))
}

out <- c(out, "", "---", "")

for (i in seq_along(slides)) {
  s <- slides[[i]]
  out <- c(out,
           sprintf("## %d. %s%s", i, if (s$segue) "▸ " else "", s$titre),
           "")
  notes <- s$notes
  # retirer les lignes vides en tête et en queue
  nz <- which(nzchar(trimws(notes)))
  if (length(nz) > 0) notes <- notes[min(nz):max(nz)] else notes <- "*(pas de notes)*"
  out <- c(out, notes, "")
}

con <- file(dest, open = "w", encoding = "UTF-8")
writeLines(out, con)
close(con)

cat(sprintf("écrit : %s (%d lignes)\n", basename(dest), length(out)))
