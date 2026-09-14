# Tests for placing a new taxon in the table_taxa tree
#
# The hierarchy view and get_taxon_children() walk only id_parent. A taxon
# added from launch_taxo_backbone_app() used to be written without it, and
# without tax_level, so it sat outside the tree. Database calls are mocked.

mock_con <- function() structure(list(), class = "mock_connection")

test_that(".taxon_level names the rank from the most precise column filled", {
  expect_equal(
    .taxon_level(tax_famclass = "Magnoliopsida", tax_order = "Magnoliales",
                 tax_fam = "Annonaceae", tax_gen = "Uvariopsis",
                 tax_esp = "dicaprio"),
    "species")
  expect_equal(.taxon_level(tax_fam = "Fabaceae", tax_gen = "Dialium",
                            tax_esp = "bambidiense sp. nov."), "species")
  expect_equal(.taxon_level(tax_fam = "Malvaceae", tax_gen = "Cola",
                            tax_esp = "lateritia", tax_nam01 = "maclaudi"),
               "infraspecific")
  expect_equal(.taxon_level(tax_fam = "Malvaceae", tax_gen = "Cola"), "genus")
  expect_equal(.taxon_level(tax_order = "Malvales", tax_fam = "Malvaceae"), "family")
  expect_equal(.taxon_level(tax_famclass = "Magnoliopsida", tax_order = "Malvales"),
               "order")
  expect_equal(.taxon_level(tax_famclass = "Magnoliopsida"), "higher")
  expect_true(is.na(.taxon_level()))
})

test_that(".taxon_level ignores blank strings and works on whole columns", {
  expect_equal(
    .taxon_level(tax_fam = c("Malvaceae", "Malvaceae"), tax_gen = c("Cola", "Cola"),
                 tax_esp = c("  ", "nitida")),
    c("genus", "species"))
})

test_that("each rank hangs from the one above it", {
  expect_equal(
    .parent_level(c("infraspecific", "species", "genus", "family", "order")),
    c("species", "genus", "family", "order", "higher"))
  expect_true(all(is.na(.parent_level(c("higher", "class", NA)))))
})

test_that("a parent is named by the flat columns of its child", {
  expect_equal(
    .parent_keys("species", tax_fam = "Malvaceae", tax_gen = "Cola", tax_esp = "nitida"),
    list(tax_fam = "Malvaceae", tax_gen = "Cola"))
  expect_equal(
    .parent_keys("infraspecific", tax_fam = "Malvaceae", tax_gen = "Cola",
                 tax_esp = "lateritia"),
    list(tax_fam = "Malvaceae", tax_gen = "Cola", tax_esp = "lateritia"))
  expect_equal(.parent_keys("family", tax_order = "Malvales", tax_fam = "Malvaceae"),
               list(tax_order = "Malvales"))
})

test_that("no parent is sought when the child cannot name it", {
  expect_null(.parent_keys("species", tax_fam = NA, tax_gen = "Cola"))
  expect_null(.parent_keys("family", tax_fam = "Malvaceae"))
  expect_null(.parent_keys("genus", tax_fam = NULL))
  expect_null(.parent_keys("genus", tax_fam = " "))
  expect_null(.parent_keys("higher", tax_famclass = "Magnoliopsida"))
  expect_null(.parent_keys(NA_character_))
})

test_that("the parent lookup prefers an accepted entry and reads both class spellings", {
  sql <- .parent_lookup_sql("genus", c("tax_fam", "tax_gen"))

  expect_match(sql, "tax_level = 'genus'", fixed = TRUE)
  expect_match(sql, "tax_fam = $1 AND tax_gen = $2", fixed = TRUE)
  expect_match(sql, "ORDER BY (idtax_good_n IS NOT NULL), idtax_n", fixed = TRUE)
  expect_match(.parent_lookup_sql("higher", "tax_famclass"),
               "tax_level IN ('class', 'higher')", fixed = TRUE)
})

test_that("the parent lookup only accepts known ranks and columns", {
  expect_error(.parent_lookup_sql("genus'; DROP TABLE table_taxa; --", "tax_fam"))
  expect_error(.parent_lookup_sql("genus", "idtax_n"))
})

test_that("a missing genus is created under its family, already linked", {
  created <- list()
  local_mocked_bindings(
    # the family exists, the genus does not
    .find_parent_entry = function(con, tax_gen, tax_fam, tax_order, tax_famclass,
                                  tax_esp, level) {
      if (level == "genus") 501L else NULL
    },
    .create_hierarchy_entry_for_parent = function(con, ...) {
      created[[length(created) + 1]] <<- list(...)
      900L
    }
  )

  parent <- .find_or_create_parent_entry(
    mock_con(), tax_gen = "Uvariopsis", tax_fam = "Annonaceae",
    tax_order = "Magnoliales", tax_famclass = "Magnoliopsida",
    tax_esp = "dicaprio", level = "species")

  expect_equal(parent, 900L)
  expect_length(created, 1)
  expect_equal(created[[1]]$tax_level, "genus")
  expect_equal(created[[1]]$id_parent, 501L)
  expect_equal(created[[1]]$tax_gen, "Uvariopsis")
  expect_equal(created[[1]]$tax_fam, "Annonaceae")
  expect_true(is.na(created[[1]]$tax_esp))
})

test_that("an existing parent is reused and nothing is created", {
  local_mocked_bindings(
    .find_parent_entry = function(...) 42L,
    .create_hierarchy_entry_for_parent = function(...) stop("must not create")
  )

  expect_equal(
    .find_or_create_parent_entry(mock_con(), tax_gen = "Cola", tax_fam = "Malvaceae",
                                 tax_esp = "nitida", level = "species"),
    42L)
})

test_that("nothing is looked up or created when the child cannot name its parent", {
  local_mocked_bindings(
    .find_parent_entry = function(...) stop("must not look up"),
    .create_hierarchy_entry_for_parent = function(...) stop("must not create")
  )

  # a family recorded without its order
  expect_null(.find_or_create_parent_entry(mock_con(), tax_fam = "Fabaceae",
                                           level = "family"))
})

test_that("a missing class is created with the 'higher' spelling and no parent", {
  created <- list()
  local_mocked_bindings(
    .find_parent_entry = function(...) NULL,
    .create_hierarchy_entry_for_parent = function(con, ...) {
      created[[length(created) + 1]] <<- list(...)
      10L + length(created)
    }
  )

  .find_or_create_parent_entry(mock_con(), tax_order = "Newales",
                               tax_famclass = "Newopsida", level = "family")

  # class first, then the order under it
  expect_equal(vapply(created, function(x) x$tax_level, character(1)),
               c("higher", "order"))
  expect_true(is.na(created[[1]]$id_parent))
  expect_equal(created[[2]]$id_parent, 11L)
})

local_add_taxa_mocks <- function(parent_id, env = parent.frame()) {
  written <- new.env()
  local_mocked_bindings(
    try_open_postgres_table = function(table, con) {
      if (table == "table_tax_famclass") {
        return(tibble::tibble(tax_famclass = "Magnoliopsida", id_tax_famclass = 7L))
      }
      tibble::tibble(idtax_n = integer(), tax_famclass = character(),
                     tax_order = character(), tax_fam = character(),
                     tax_gen = character(), tax_esp = character(),
                     tax_rank01 = character(), tax_nam01 = character())
    },
    .find_or_create_parent_entry = function(con, tax_gen, tax_fam, tax_order,
                                            tax_famclass, tax_esp, level) {
      written$level_asked <- level
      parent_id
    },
    .append_taxa_row = function(con, row) {
      written$row <- row
      367192L
    },
    .env = env
  )
  local_mocked_bindings(
    dbWithTransaction = function(conn, code, ...) code,
    .package = "DBI",
    .env = env
  )
  written
}

test_that(".add_taxa_noninteractive writes the rank and the parent", {
  written <- local_add_taxa_mocks(parent_id = 42L)

  new_id <- .add_taxa_noninteractive(
    tax_gen = "Uvariopsis", tax_esp = "dicaprio", tax_fam = "Annonaceae",
    tax_order = "Magnoliales", tax_famclass = "Magnoliopsida", con = mock_con())

  expect_equal(new_id, 367192L)
  expect_equal(written$level_asked, "species")
  expect_equal(written$row$tax_level, "species")
  expect_equal(written$row$id_parent, 42L)
  expect_equal(written$row$tax_source, "NEW")
})

test_that(".add_taxa_noninteractive still inserts a taxon whose parent cannot be named", {
  written <- local_add_taxa_mocks(parent_id = NULL)

  new_id <- .add_taxa_noninteractive(
    tax_fam = "Newaceae", tax_famclass = "Magnoliopsida", con = mock_con())

  expect_equal(new_id, 367192L)
  expect_equal(written$row$tax_level, "family")
  expect_true(is.na(written$row$id_parent))
})

test_that(".append_taxa_row refuses a pool", {
  expect_error(.append_taxa_row(structure(list(), class = "Pool"), data.frame(x = 1)),
               "pool")
})

test_that(".append_taxa_row reads the id back from the column's own sequence", {
  queries <- character()
  local_mocked_bindings(
    dbWriteTable = function(conn, name, value, ...) TRUE,
    dbGetQuery = function(conn, statement, ...) {
      queries <<- c(queries, statement)
      data.frame(idtax_n = if (grepl("currval", statement)) 367192 else 1)
    },
    .package = "DBI"
  )

  expect_identical(.append_taxa_row(mock_con(), data.frame(tax_gen = "Cola")), 367192L)
  expect_length(queries, 1)
})

test_that(".append_taxa_row falls back to MAX without an owned sequence", {
  local_mocked_bindings(
    dbWriteTable = function(conn, name, value, ...) TRUE,
    dbGetQuery = function(conn, statement, ...) {
      data.frame(idtax_n = if (grepl("currval", statement)) NA else 367192)
    },
    .package = "DBI"
  )

  expect_identical(.append_taxa_row(mock_con(), data.frame(tax_gen = "Cola")), 367192L)
})
