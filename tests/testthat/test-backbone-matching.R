# Matching internal taxa to a backbone and choosing the links that supply
# names (R/backbone_matching.R). Only the parts that need no database are
# tested here; the SQL is exercised against a database.


# ---------------------------------------------------------------------------
# .author_score()
# ---------------------------------------------------------------------------

test_that(".author_score() compares authors, NA when a side is missing", {
  s <- .author_score(c("(L.) Hepper", "Blume", NA, "L.", ""),
                     c("(L.) Hepper", "A.Chev.", "Mill.", NA, "L."))
  expect_equal(s[1], 1)
  expect_lt(s[2], 0.6)
  expect_true(all(is.na(s[3:5])))
})

test_that(".author_score() ignores basionym authors, 'ex' authors, spaces and dots", {
  s <- .author_score(c("(Klatt) B.L.Rob.", "Wedd. ex Blume", "D.Dietr.", "Hook. f.", "(L.) A.DC."),
                     c("B.L.Rob.", "Blume", "(Bojer ex Hook.) D.Dietr.", "Hook.f.", "Juss."))
  expect_equal(s[1:4], c(1, 1, 1, 1))
  expect_lt(s[5], 0.6)
  expect_equal(.author_score("L.", "(L.) Sw.", method = "exact"), 0)
  expect_true(is.na(.author_score("(L.)", "Sw.")))
})

test_that(".is_misapplied_taxon() finds auct. in any author column", {
  expect_equal(.is_misapplied_taxon(c("ZZ auct.", "L.", NA, "L."),
                                    c(NA, "auct. non Hook.", NA, NA),
                                    c(NA, NA, NA, "Sw.")),
               c(TRUE, TRUE, FALSE, FALSE))
})

test_that(".author_score() treats auct. on one side only as a conflict", {
  s <- .author_score(c("ZZ auct.", "(L.) Medik.", NA, "ZZ auct.", "auct. non L."),
                     c("auct.", "auct.", "auct.", "L.", "auct."))
  expect_equal(s, c(1, 0, 0, 0, 1))
})

test_that(".author_score(method = 'exact') requires identical strings", {
  s <- .author_score(c("L.", "L.", NA), c("L.", "(L.) Sw.", "L."), method = "exact")
  expect_equal(s, c(1, 0, NA))
})


# ---------------------------------------------------------------------------
# .match_exact_names()
# ---------------------------------------------------------------------------

.bb_names <- function() {
  data.frame(
    external_id = c("201", "202", "301", "401", "501", "502", "601"),
    taxon_name  = c("Vigna mungo", "Vigna mungo", "Diospyros macrophylla",
                    "Abelmoschus manihot", "Ficus exasperata", "Ficus exasperata",
                    "Abelmoschus manihot"),
    authors     = c("(L.) Hepper", "auct.", "A.Chev.", "(L.) Medik.", "Vahl", NA,
                    "auct."),
    status_raw  = c("Accepted", "Misapplied", "Synonym", "Accepted", "Accepted",
                    "Synonym", "Misapplied"),
    stringsAsFactors = FALSE
  )
}

test_that(".match_exact_names() without authors keeps every homonym", {
  names <- data.frame(.match_id = 1:2, name = c("Vigna mungo", "Nonexistent name"))
  res <- .match_exact_names(names, .bb_names())
  expect_equal(sort(res$external_id), c("201", "202"))
  expect_true(all(res$match_type == "exact"))
  expect_true(all(is.na(res$author_score)))
})

test_that(".match_exact_names() gives each author its own homonym", {
  names <- data.frame(.match_id = 1:2, name = "Vigna mungo",
                      author = c("(L.) Hepper", "ZZ auct."))
  res <- .match_exact_names(names, .bb_names(), author_match = "fuzzy")
  expect_equal(res$external_id[res$.match_id == 1], "201")
  expect_equal(res$external_id[res$.match_id == 2], "202")
  expect_true(all(res$match_type == "exact"))
})

test_that(".match_exact_names() marks names whose every candidate conflicts on authors", {
  names <- data.frame(.match_id = 1L, name = "Diospyros macrophylla", author = "Blume")
  res <- .match_exact_names(names, .bb_names(), author_match = "fuzzy")
  expect_equal(res$external_id, "301")
  expect_equal(res$match_type, "author_mismatch")
  expect_lt(res$author_score, 0.6)
})

test_that(".match_exact_names() without internal authors never picks an auct. record", {
  names <- data.frame(.match_id = 1L, name = "Abelmoschus manihot", author = NA_character_)
  res <- .match_exact_names(names, .bb_names(), author_match = "fuzzy")
  expect_equal(res$external_id, "401")
  expect_equal(res$match_type, "exact")
})

test_that(".match_exact_names() prefers comparable authors over unknown ones", {
  names <- data.frame(.match_id = 1L, name = "Ficus exasperata", author = "Vahl")
  res <- .match_exact_names(names, .bb_names(), author_match = "fuzzy")
  expect_equal(res$external_id, "501")
})

.bb_homonyms <- function() {
  data.frame(
    external_id = c("11", "12", "21", "22", "23", "31", "32", "41", "42"),
    taxon_name  = c("Phoenix", "Phoenix", "Renealmia", "Renealmia", "Renealmia",
                    "Cyperus tenuiculmis", "Cyperus tenuiculmis", "Aus bus", "Aus bus"),
    authors     = c("L.", "Haller", "L.f.", "Houtt.", "R.Br.", "Boeckeler", "Boeckeler",
                    "Mill.", "L."),
    status_raw  = c("Accepted", "Illegitimate", "Accepted", "Illegitimate", "Accepted",
                    "Illegitimate", "Accepted", "Synonym", "Invalid"),
    status      = c("accepted", "other", "accepted", "other", "accepted",
                    "other", "accepted", "synonym", "other"),
    stringsAsFactors = FALSE
  )
}

test_that(".match_exact_names() keeps only the accepted name among several candidates", {
  names <- data.frame(.match_id = 1:2, name = c("Phoenix", "Cyperus tenuiculmis"),
                      author = c(NA, "Boeckeler"))
  res <- .match_exact_names(names, .bb_homonyms(), author_match = "fuzzy")
  expect_equal(res$external_id[res$.match_id == 1], "11")
  expect_equal(res$external_id[res$.match_id == 2], "32")

  # without authors too
  res_none <- .match_exact_names(names[, 1:2], .bb_homonyms())
  expect_equal(sort(res_none$external_id), c("11", "32"))
})

test_that(".match_exact_names() keeps every candidate when accepted names are not exactly one", {
  names <- data.frame(.match_id = 1:2, name = c("Renealmia", "Aus bus"))
  res <- .match_exact_names(names, .bb_homonyms())
  expect_equal(sort(res$external_id[res$.match_id == 1]), c("21", "22", "23"))
  # no accepted name, one synonym beside an invalid one: the synonym
  expect_equal(res$external_id[res$.match_id == 2], "41")
})

test_that(".match_exact_names() keeps the only synonym beside illegitimate or invalid names", {
  bb <- data.frame(
    external_id = c("51", "52", "53", "61", "62", "71", "72", "81", "82"),
    taxon_name  = c(rep("Bus cus", 3), rep("Dus eus", 2), rep("Fus gus", 2), rep("Hus ius", 2)),
    authors     = c("A.", "B.", "C.", "A.", "B.", "A.", "B.", "A.", "B."),
    status_raw  = c("Synonym", "Illegitimate", "invalid name (nom. nud.)",
                    "Synonym", "Synonym", "Synonym", "Unplaced",
                    "Synonyme", "illeg. name"),
    status      = c("synonym", "other", "other", "synonym", "synonym", "synonym", "other",
                    "synonym", "other"),
    stringsAsFactors = FALSE
  )
  names <- data.frame(.match_id = 1:4, name = c("Bus cus", "Dus eus", "Fus gus", "Hus ius"))
  res <- .match_exact_names(names, bb)
  expect_equal(res$external_id[res$.match_id == 1], "51")
  expect_equal(sort(res$external_id[res$.match_id == 2]), c("61", "62"))  # two synonyms
  expect_equal(sort(res$external_id[res$.match_id == 3]), c("71", "72"))  # unplaced beside it
  expect_equal(res$external_id[res$.match_id == 4], "81")                 # APD wording
})

test_that(".match_exact_names() does not reduce author-mismatch candidates", {
  bb <- .bb_homonyms()
  names <- data.frame(.match_id = 1L, name = "Phoenix", author = "Roxb.")
  res <- .match_exact_names(names, bb, author_match = "fuzzy")
  expect_equal(sort(res$external_id), c("11", "12"))
  expect_true(all(res$match_type == "author_mismatch"))
})

test_that(".match_exact_names() returns an empty frame when nothing matches", {
  names <- data.frame(.match_id = 1L, name = "Nothing here", author = "L.")
  res <- .match_exact_names(names, .bb_names(), author_match = "fuzzy")
  expect_equal(nrow(res), 0)
  expect_true(all(c(".match_id", "external_id", "match_type", "author_score") %in% names(res)))
})


# ---------------------------------------------------------------------------
# .choose_preferred_links()
# ---------------------------------------------------------------------------

test_that(".choose_preferred_links() applies the rule", {
  links <- data.frame(
    idtax_n    = c(1L, 2L, 3L, 4L, 4L, 5L, 5L, 6L, 6L, 7L, 8L),
    match_type = c("exact", "fuzzy", "author_mismatch", "exact", "exact",
                   "fuzzy", "exact", "exact", "exact", "manual", "fuzzy"),
    verified   = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE,
                   FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  pref <- .choose_preferred_links(links$idtax_n, links$match_type, links$verified)
  expect_equal(pref, c(
    TRUE,         # sole exact
    FALSE,        # sole fuzzy, not verified
    FALSE,        # sole author mismatch, not verified
    FALSE, FALSE, # homonyms, none verified
    TRUE, FALSE,  # the verified one wins over an exact one
    FALSE, FALSE, # two verified: undecidable
    TRUE,         # sole manual
    TRUE          # sole fuzzy, verified
  ))
  expect_equal(.choose_preferred_links(integer(0), character(0), logical(0)), logical(0))
})


# ---------------------------------------------------------------------------
# .prepare_link_data()
# ---------------------------------------------------------------------------

test_that(".prepare_link_data() drops rejected rows and keeps verified duplicates", {
  m <- data.frame(
    idtax_n     = c(1, 1, 2, 3),
    external_id = c("10", "10", "20", "30"),
    match_type  = c("fuzzy", "fuzzy", "exact", "exact"),
    match_score = c(0.91234, 0.91234, 1, 1),
    verified    = c(FALSE, TRUE, NA, FALSE),
    decision    = c(NA, "accepted", NA, "rejected"),
    stringsAsFactors = FALSE
  )
  res <- .prepare_link_data(m)
  expect_equal(nrow(res), 2)
  expect_equal(res$verified[res$idtax_n == 1], TRUE)
  expect_equal(res$verified[res$idtax_n == 2], FALSE)
  expect_equal(res$match_score[res$idtax_n == 1], 0.912)
  expect_false(3L %in% res$idtax_n)
})

test_that(".prepare_link_data() refuses incomplete rows", {
  expect_error(.prepare_link_data(data.frame(idtax_n = 1, external_id = "1")), "match_type")
  expect_error(.prepare_link_data(data.frame(idtax_n = NA, external_id = "1", match_type = "exact")),
               "must not be missing")
  expect_error(.prepare_link_data(data.frame(idtax_n = 1, external_id = "1",
                                             match_type = "a_very_long_match_type_name")),
               "longer than 20")
})
