# Tests for the policy readers that seed plot_access.
#
# These are the parts a wrong answer is dangerous in: the seed uses them to
# decide which plots each of the 36 accounts may see. Every shape PostgreSQL
# can deparse a grant into is covered here, and so is every shape that must be
# refused rather than guessed at.

test_that(".parse_policy_plot_ids() reads the usual ARRAY form", {
  out <- .parse_policy_plot_ids("(id_liste_plots = ANY (ARRAY[179, 180, 181]))")
  expect_equal(out$kind, "ids")
  expect_equal(out$ids, c(179L, 180L, 181L))
})

test_that(".parse_policy_plot_ids() reads an ARRAY carrying an explicit cast", {
  out <- .parse_policy_plot_ids("(id_liste_plots = ANY (ARRAY[3, 4]::integer[]))")
  expect_equal(out$kind, "ids")
  expect_equal(out$ids, c(3L, 4L))
})

test_that(".parse_policy_plot_ids() reads the array-literal form", {
  out <- .parse_policy_plot_ids("(id_liste_plots = ANY ('{7,8,9}'::integer[]))")
  expect_equal(out$kind, "ids")
  expect_equal(out$ids, c(7L, 8L, 9L))
})

test_that(".parse_policy_plot_ids() reads a single-ID grant", {
  out <- .parse_policy_plot_ids("(id_liste_plots = 1188)")
  expect_equal(out$kind, "ids")
  expect_equal(out$ids, 1188L)
})

test_that(".parse_policy_plot_ids() normalises whitespace and ordering", {
  out <- .parse_policy_plot_ids("(id_liste_plots = ANY (ARRAY[ 5,\n  2 ,  5 ]))")
  expect_equal(out$kind, "ids")
  expect_equal(out$ids, c(2L, 5L))
})

test_that(".parse_policy_plot_ids() recognises the creator policies", {
  expect_equal(.parse_policy_plot_ids("(created_by = (CURRENT_USER)::text)")$kind,
               "creator")
  expect_equal(.parse_policy_plot_ids("(created_by = CURRENT_USER)")$kind,
               "creator")
  expect_length(.parse_policy_plot_ids("(created_by = CURRENT_USER)")$ids, 0L)
})

test_that(".parse_policy_plot_ids() treats a missing qual as no grant", {
  expect_equal(.parse_policy_plot_ids(NA_character_)$kind, "none")
  expect_equal(.parse_policy_plot_ids("")$kind, "none")
  expect_equal(.parse_policy_plot_ids("   ")$kind, "none")
})

test_that(".parse_policy_plot_ids() refuses a compound expression", {
  # Two grants ORed together is a shape define_user_policy() never writes. The
  # ID list may well be complete, but the semantics are not what the seed
  # assumes, so it must not be read.
  out <- .parse_policy_plot_ids(
    "((id_liste_plots = ANY (ARRAY[1, 2])) OR (created_by = CURRENT_USER))")
  expect_equal(out$kind, "unparseable")
  expect_length(out$ids, 0L)
})

test_that(".parse_policy_plot_ids() refuses a predicate on another column", {
  # This is the case a digit-scraping parser gets catastrophically wrong: it
  # would return plot 0 or plot 100 from a latitude filter.
  expect_equal(.parse_policy_plot_ids("(ddlat > (0)::double precision)")$kind,
               "unparseable")
  expect_equal(.parse_policy_plot_ids("(id_liste_plots <> 100)")$kind,
               "unparseable")
  expect_equal(.parse_policy_plot_ids("(country = 100)")$kind, "unparseable")
})

test_that(".parse_policy_plot_ids() refuses a subquery", {
  expect_equal(
    .parse_policy_plot_ids(
      "(id_liste_plots IN ( SELECT plot_access.id_liste_plots FROM plot_access))")$kind,
    "unparseable")
})

test_that(".parse_policy_plot_ids() refuses non-integer payloads", {
  expect_equal(.parse_policy_plot_ids("(id_liste_plots = ANY (ARRAY[1.5, 2]))")$kind,
               "unparseable")
  expect_equal(.parse_policy_plot_ids("(id_liste_plots = ANY (ARRAY[NULL]))")$kind,
               "unparseable")
  expect_equal(.parse_policy_plot_ids("(id_liste_plots = ANY (ARRAY[]::integer[]))")$kind,
               "unparseable")
})

test_that(".parse_policy_plot_ids() rejects a zero or negative ID", {
  expect_equal(.parse_policy_plot_ids("(id_liste_plots = 0)")$kind, "unparseable")
})

test_that(".parse_policy_plot_ids() takes one qual at a time", {
  expect_error(.parse_policy_plot_ids(c("(id_liste_plots = 1)", "(id_liste_plots = 2)")),
               "one qual at a time")
})

test_that(".policy_cmd_capability() maps every command PostgreSQL reports", {
  expect_equal(.policy_cmd_capability("SELECT"), "read")
  expect_equal(.policy_cmd_capability("UPDATE"), "write")
  expect_equal(.policy_cmd_capability("ALL"), "all")
  expect_true(is.na(.policy_cmd_capability("INSERT")))
})

test_that(".policy_cmd_capability() keeps DELETE separate from UPDATE", {
  # Folding the two into one can_write flag is what made 13,913 grants carry
  # DELETE. They are different rights and have to stay distinguishable.
  expect_equal(.policy_cmd_capability("DELETE"), "delete")
  expect_false(identical(.policy_cmd_capability("DELETE"),
                         .policy_cmd_capability("UPDATE")))
})

test_that(".policy_cmd_capability() is case- and whitespace-tolerant", {
  expect_equal(.policy_cmd_capability(" all "), "all")
  expect_equal(.policy_cmd_capability("select"), "read")
})

test_that(".policy_cmd_capability() errors rather than guessing", {
  expect_error(.policy_cmd_capability("TRUNCATE"), "Unknown policy command")
  expect_error(.policy_cmd_capability(NA_character_), "one command at a time")
  expect_error(.policy_cmd_capability(c("SELECT", "ALL")), "one command at a time")
})
