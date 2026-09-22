# African Plant Database importer (R/apd_integration.R).
#
# Only the parts that need no database are tested here: reading an export,
# deriving the canonical fields and the version. The import itself is
# exercised against a database.


# One record per case the derivation rules distinguish
.apd_raw <- function() {
  data.frame(
    ID           = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
    idtax_good_n = c(NA, "1", NA, NA, NA, NA, NA, NA, "99"),
    id_PARENT    = c("6", "6", "1", "1", "1", "7", "8", NA, "6"),
    taxon_name   = c("Cyathula lanceolata", "Cyathula deserti",
                     "Cyathula lanceolata var. pedicellata",
                     "Cyathula lanceolata var. lanceolata",
                     "Cyathula lanceolata f. minor", "Cyathula",
                     "AMARANTHACEAE", "cla. Magnoliopsida", "Cyathula sp. 1"),
    nom_standard = c("Cyathula lanceolata Schinz", "Cyathula deserti (N.E.Br.) Suess.",
                     "Cyathula lanceolata var. pedicellata Cavaco",
                     "Cyathula lanceolata Schinz var. lanceolata",
                     "Cyathula lanceolata f. minor Hauman", "Cyathula Blume",
                     "AMARANTHACEAE Juss.", "cla. Magnoliopsida", "Cyathula sp. 1"),
    tax_level    = c("species", "species", "varietas", "varietas", "forma",
                     "genus", "familia", "classis", "species"),
    taxrank      = c(NA, NA, "var. pedicellata", "var. ", "f. minor", NA, NA, NA, NA),
    tax_famclass = c(rep("Magnoliopsida", 6), NA, NA, "Magnoliopsida"),
    fk_famille   = c(rep("AMARANTHACEAE", 6), NA, NA, "AMARANTHACEAE"),
    tax_gen      = c(rep("Cyathula", 6), NA, NA, "Cyathula"),
    tax_esp      = c("lanceolata", "deserti", "lanceolata", "lanceolata",
                     "lanceolata f. minor", NA, NA, NA, "sp. 1"),
    author1      = c("Schinz", "(N.E.Br.) Suess.", "Schinz", "Schinz", "Schinz",
                     NA, NA, NA, "  "),
    author2      = c(NA, NA, "Cavaco", NA, "Hauman", NA, NA, NA, NA),
    taxon_status = c("Accepted", "Synonyme", "Accepted", "Accepted", "Accepted",
                     "Accepted", "Accepted", "Accepted", "Accepted"),
    citation     = NA_character_,
    year_description  = c("1896", "1934", NA, NA, NA, NA, NA, NA, "n.d."),
    date_modification = c("28.7.2026 16:31:25", "1.12.2025 15:49", "30.3.2007",
                          NA, NA, NA, NA, NA, "yesterday"),
    stringsAsFactors = FALSE
  )
}

.apd_prepared <- function() {
  res <- .prepare_apd_names(.apd_raw(), "2026-07-29")
  rownames(res) <- res$apd_id
  res
}


# ---------------------------------------------------------------------------
# .prepare_apd_names()
# ---------------------------------------------------------------------------

test_that(".prepare_apd_names() returns the columns of apd_names, as text", {
  res <- .apd_prepared()
  expect_equal(names(res), c(
    "apd_id", "apd_accepted_id", "apd_parent_id", "taxon_name", "nom_standard",
    "tax_level", "taxrank", "tax_famclass", "fk_famille", "tax_gen", "tax_esp",
    "author1", "author2", "taxon_status", "citation", "year_description",
    "date_modification", "family", "species", "infra_rank", "infra_epithet",
    "authors", "apd_version"
  ))
  expect_true(all(vapply(res, is.character, logical(1))))
  expect_equal(nrow(res), 9L)
  expect_equal(unique(res$apd_version), "2026-07-29")
})

test_that(".prepare_apd_names() keeps pointers as they are, dangling or not", {
  res <- .apd_prepared()
  expect_equal(res$apd_accepted_id, .apd_raw()$idtax_good_n)
  expect_equal(res$apd_parent_id, .apd_raw()$id_PARENT)
})

test_that(".prepare_apd_names() capitalises families and drops rank prefixes", {
  res <- .apd_prepared()
  expect_equal(res["7", "taxon_name"], "Amaranthaceae")
  expect_equal(res["7", "family"], "Amaranthaceae")
  expect_equal(res["1", "family"], "Amaranthaceae")
  expect_equal(res["8", "taxon_name"], "Magnoliopsida")
  expect_true(is.na(res["8", "family"]))
  expect_equal(res["7", "fk_famille"], NA_character_)
  expect_equal(res["1", "fk_famille"], "AMARANTHACEAE")
})

test_that(".prepare_apd_names() leaves a bare abbreviation alone", {
  raw <- .apd_raw()[6, ]
  raw$taxon_name <- "sp."
  raw$nom_standard <- "sp."
  expect_equal(.prepare_apd_names(raw, "2026-07-29")$taxon_name, "sp.")
})

test_that(".prepare_apd_names() splits infraspecific ranks, autonyms included", {
  res <- .apd_prepared()
  expect_equal(res["3", "infra_rank"], "var.")
  expect_equal(res["3", "infra_epithet"], "pedicellata")
  expect_equal(res["4", "infra_rank"], "var.")
  expect_equal(res["4", "infra_epithet"], "lanceolata")
  expect_equal(res["5", "infra_rank"], "f.")
  expect_equal(res["5", "infra_epithet"], "minor")
  expect_true(all(is.na(res[c("1", "6", "7", "9"), "infra_rank"])))
})

test_that(".prepare_apd_names() cuts species to the epithet only when a rank follows", {
  res <- .apd_prepared()
  expect_equal(res["5", "species"], "lanceolata")
  expect_equal(res["9", "species"], "sp. 1")
  expect_equal(res["1", "species"], "lanceolata")
  expect_true(is.na(res["6", "species"]))
})

test_that(".prepare_apd_names() takes authors from the column of the rank", {
  res <- .apd_prepared()
  expect_equal(res["1", "authors"], "Schinz")
  expect_equal(res["3", "authors"], "Cavaco")
  expect_true(is.na(res["4", "authors"]))       # autonym
  expect_equal(res["6", "authors"], "Blume")    # genus: from nom_standard
  expect_equal(res["7", "authors"], "Juss.")    # family: from nom_standard
  expect_true(is.na(res["8", "authors"]))       # nothing after the name
  expect_true(is.na(res["9", "authors"]))       # blank author1
})

test_that(".prepare_apd_names() reads dates and years, NULL when unreadable", {
  res <- .apd_prepared()
  expect_equal(res["1", "date_modification"], "2026-07-28 16:31:25")
  expect_equal(res["2", "date_modification"], "2025-12-01 15:49:00")
  expect_equal(res["3", "date_modification"], "2007-03-30 00:00:00")
  expect_true(is.na(res["9", "date_modification"]))
  expect_equal(res["1", "year_description"], "1896")
  expect_true(is.na(res["9", "year_description"]))
})

test_that(".prepare_apd_names() refuses records it cannot store", {
  raw <- .apd_raw()
  raw$ID[2] <- "1"
  expect_error(.prepare_apd_names(raw, "2026-07-29"), "more than once")

  raw <- .apd_raw()
  raw$idtax_good_n[2] <- "1a"
  expect_error(.prepare_apd_names(raw, "2026-07-29"), "idtax_good_n")

  raw <- .apd_raw()
  raw$ID[3] <- NA
  expect_error(.prepare_apd_names(raw, "2026-07-29"), "without")

  raw <- .apd_raw()
  raw$taxon_name[4] <- " "
  expect_error(.prepare_apd_names(raw, "2026-07-29"), "taxon_name")
})


# ---------------------------------------------------------------------------
# .apd_export_version()
# ---------------------------------------------------------------------------

test_that(".apd_export_version() accepts only a real YYYY-MM-DD date", {
  f <- withr::local_tempfile(lines = "x")
  expect_equal(.apd_export_version(f, "2026-07-29"), "2026-07-29")
  expect_error(.apd_export_version(f, "2026-13-01"), "YYYY-MM-DD")
  expect_error(.apd_export_version(f, "29/07/2026"), "YYYY-MM-DD")
  expect_error(.apd_export_version(f, c("2026-07-29", "2026-07-30")), "YYYY-MM-DD")
})

test_that(".apd_export_version() defaults to the last-modified date", {
  f <- withr::local_tempfile(lines = "x")
  Sys.setFileTime(f, as.POSIXct("2026-07-29 08:30:57"))
  expect_equal(.apd_export_version(f), "2026-07-29")
})


# ---------------------------------------------------------------------------
# .read_apd_export()
# ---------------------------------------------------------------------------

# Writes an export with an accented author, in the given encoding
.write_apd_export <- function(path, encoding, drop = character(0)) {
  cols <- c(setdiff(.apd_export_columns, drop), "STATUT_SYN")
  row <- setNames(rep("", length(cols)), cols)
  row[["ID"]] <- "10"
  row[["taxon_name"]] <- "Cyathula zurichensis"
  row[["author1"]] <- paste0("Z", intToUtf8(252), "rich")
  row[["STATUT_SYN"]] <- "A"
  lines <- c(paste(cols, collapse = "\t"),
             paste0("\"", row, "\"", collapse = "\t"))
  text <- paste0(paste(lines, collapse = "\n"), "\n")
  bytes <- if (encoding == "latin1") {
    iconv(text, "UTF-8", "latin1", toRaw = TRUE)[[1]]
  } else {
    charToRaw(enc2utf8(text))
  }
  writeBin(bytes, path)
  path
}

test_that(".read_apd_export() reads Latin-1 into UTF-8 and drops STATUT_SYN", {
  f <- .write_apd_export(withr::local_tempfile(fileext = ".txt"), "latin1")
  res <- .read_apd_export(f)
  expect_equal(names(res), .apd_export_columns)
  expect_equal(res$author1, paste0("Z", intToUtf8(252), "rich"))
  expect_true(validUTF8(res$author1))
  # a quoted empty field is read as "", and stored as NULL
  expect_true(is.na(.prepare_apd_names(res, "2026-07-29")$author2))
})

test_that(".read_apd_export() reads a UTF-8 export when told", {
  f <- .write_apd_export(withr::local_tempfile(fileext = ".txt"), "utf8")
  res <- .read_apd_export(f, encoding = "UTF-8")
  expect_equal(res$author1, paste0("Z", intToUtf8(252), "rich"))
})

test_that(".read_apd_export() refuses a file read with the wrong encoding", {
  utf8 <- .write_apd_export(withr::local_tempfile(fileext = ".txt"), "utf8")
  expect_error(.read_apd_export(utf8, encoding = "Latin-1"), "looks like UTF-8")

  latin1 <- .write_apd_export(withr::local_tempfile(fileext = ".txt"), "latin1")
  expect_error(.read_apd_export(latin1, encoding = "UTF-8"), "not valid UTF-8")
})

test_that(".read_apd_export() refuses a file that is not an APD export", {
  f <- .write_apd_export(withr::local_tempfile(fileext = ".txt"), "latin1",
                         drop = c("idtax_good_n", "nom_standard"))
  expect_error(.read_apd_export(f), "not an APD export")
  expect_error(.read_apd_export(file.path(tempdir(), "no_such_export.txt")), "not found")
})
