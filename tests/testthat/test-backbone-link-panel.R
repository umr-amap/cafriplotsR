# The taxon panel of launch_taxo_backbone_app() shows every backbone the
# database registers, not just WCVP, and says of each link whether it is the
# one supplying the taxon's name. These tests exercise the rendering alone: no
# database, no Shiny session.

# Minimal stand-in for a shiny.i18n translator
fake_i18n <- function() list(t = function(x) x)

bb_row <- function(code, name, is_name_source = TRUE) {
  dplyr::tibble(code = code, name = name, is_name_source = is_name_source)
}

link_row <- function(..., defaults = list(
  external_id = "1", is_preferred = TRUE, match_type = "exact",
  match_score = NA_real_, verified = FALSE, in_view = TRUE,
  taxon_name = "Diospyros iturensis", authors = "(Gurke) Letouzey & F.White",
  status = "accepted", status_raw = "Accepted",
  accepted_external_id = NA_character_, url = NA_character_)) {
  do.call(dplyr::tibble, utils::modifyList(defaults, list(...)))
}

test_that("a backbone with no link says so rather than being left out", {
  html <- as.character(.render_backbone_link_block(
    bb_row("apd", "African Plant Database"), NULL, fake_i18n()
  ))
  expect_match(html, "African Plant Database", fixed = TRUE)
  expect_match(html, "Not linked", fixed = TRUE)
})

test_that("a backbone not yet offered is flagged as such", {
  html <- as.character(.render_backbone_link_block(
    bb_row("apd", "African Plant Database", is_name_source = FALSE),
    NULL, fake_i18n()
  ))
  expect_match(html, "not yet offered as a source of names", fixed = TRUE)
})

test_that("a preferred link is shown as the one supplying the name", {
  html <- as.character(.render_backbone_link_block(
    bb_row("apd", "African Plant Database"), link_row(), fake_i18n()
  ))
  expect_match(html, "used for names", fixed = TRUE)
  expect_false(grepl("awaiting review", html, fixed = TRUE))
  expect_match(html, "Diospyros iturensis", fixed = TRUE)
})

test_that("an unreviewed fuzzy link says it is awaiting review", {
  html <- as.character(.render_backbone_link_block(
    bb_row("wcvp", "World Checklist of Vascular Plants"),
    link_row(is_preferred = FALSE, match_type = "fuzzy",
             match_score = 0.92, verified = FALSE),
    fake_i18n()
  ))
  expect_match(html, "awaiting review", fixed = TRUE)
  expect_match(html, "0.92", fixed = TRUE)
})

test_that("a verified link that is not preferred is not called unreviewed", {
  html <- as.character(.render_backbone_link_block(
    bb_row("wcvp", "World Checklist of Vascular Plants"),
    link_row(is_preferred = FALSE, match_type = "fuzzy", verified = TRUE),
    fake_i18n()
  ))
  expect_match(html, "not used for names", fixed = TRUE)
  expect_false(grepl("awaiting review", html, fixed = TRUE))
})

test_that("a link to an identifier absent from the import is called out", {
  html <- as.character(.render_backbone_link_block(
    bb_row("apd", "African Plant Database"),
    link_row(in_view = FALSE, taxon_name = NA_character_,
             authors = NA_character_, status = NA_character_),
    fake_i18n()
  ))
  expect_match(html, "ID absent from the current import", fixed = TRUE)
  expect_match(html, "name not found", fixed = TRUE)
})

test_that("an identifier is a link when the backbone gives a url template", {
  html <- as.character(.render_backbone_link_block(
    bb_row("apd", "African Plant Database"),
    link_row(external_id = "42", url = "http://africanplantdatabase.ch/42"),
    fake_i18n()
  ))
  expect_match(html, "href=\"http://africanplantdatabase.ch/42\"", fixed = TRUE)
})

test_that("every link of a backbone is listed, preferred or not", {
  rows <- rbind(
    link_row(external_id = "1"),
    link_row(external_id = "2", is_preferred = FALSE, match_type = "author_mismatch")
  )
  html <- as.character(.render_backbone_link_block(
    bb_row("apd", "African Plant Database"), rows, fake_i18n()
  ))
  expect_match(html, "author_mismatch", fixed = TRUE)
  expect_match(html, "used for names", fixed = TRUE)
})
