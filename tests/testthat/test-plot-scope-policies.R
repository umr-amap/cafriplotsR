# Tests for the step 5 plot-scope policy predicates.
#
# These run offline. The point is that the SQL a migration is about to install
# on six tables holding ~3.2M rows can be read and checked before anything is
# enabled, because enabling row-level security on a table with a wrong policy
# does not fail loudly - it either hides rows or blocks writes, and the owner
# never sees either.

test_that(".plot_scope_child_tables() is the six children, not data_liste_plots", {

  tabs <- .plot_scope_child_tables()

  expect_length(tabs, 6L)
  expect_false("data_liste_plots" %in% tabs)
  expect_true(all(tabs %in% .plot_scope_tables()))
  expect_setequal(setdiff(.plot_scope_tables(), tabs), "data_liste_plots")
})


test_that(".plot_grant_list_sql() adds can_write only for write mode", {

  read  <- .plot_grant_list_sql("read")
  write <- .plot_grant_list_sql("write")

  expect_true(grepl("FROM public.plot_access a", read, fixed = TRUE))
  expect_true(grepl("a.db_user = current_user", read, fixed = TRUE))
  expect_false(grepl("can_write", read, fixed = TRUE))

  expect_true(grepl("AND a.can_write", write, fixed = TRUE))

  # No can_delete mode: child DELETE follows can_write by design.
  expect_error(.plot_grant_list_sql("delete"))

  # Never the function, which cannot be inlined.
  expect_false(grepl("accessible_plots", read, fixed = TRUE))
  expect_false(grepl("accessible_plots", write, fixed = TRUE))
})


test_that("tables with their own plot key compare that column directly", {

  expect_equal(
    .plot_scope_predicate("data_individuals", "read"),
    paste0("data_individuals.id_table_liste_plots_n = ANY (",
           .plot_grant_list_sql("read"), ")"))

  expect_equal(
    .plot_scope_predicate("data_liste_sub_plots", "write"),
    paste0("data_liste_sub_plots.id_table_liste_plots = ANY (",
           .plot_grant_list_sql("write"), ")"))

  # 0 hops since denormalise_ind_measures_plot.R, so no join at all.
  feat <- .plot_scope_predicate("data_ind_measures_feat", "read")
  expect_true(grepl("data_ind_measures_feat.id_table_liste_plots = ANY",
                    feat, fixed = TRUE))
  expect_false(grepl("EXISTS", feat, fixed = TRUE))
  expect_false(grepl("data_traits_measures", feat, fixed = TRUE))
})


test_that("data_traits_measures routes through the individual, never its own key", {

  p <- .plot_scope_predicate("data_traits_measures", "read")

  expect_true(grepl("FROM public.data_individuals i", p, fixed = TRUE))
  expect_true(grepl("i.id_n = data_traits_measures.id_data_individuals",
                    p, fixed = TRUE))
  expect_true(grepl("i.id_table_liste_plots_n = ANY", p, fixed = TRUE))

  # The denormalised column is wrong on 8,575 rows and NULL on 256,179.
  expect_false(grepl("data_traits_measures.id_table_liste_plots",
                     p, fixed = TRUE))
})


test_that("data_subplot_feat reaches its plot through the subplot", {

  p <- .plot_scope_predicate("data_subplot_feat", "read")

  expect_true(grepl("FROM public.data_liste_sub_plots s", p, fixed = TRUE))
  expect_true(grepl("s.id_sub_plots = data_subplot_feat.id_sub_plots",
                    p, fixed = TRUE))
  expect_true(grepl("s.id_table_liste_plots = ANY", p, fixed = TRUE))
})


test_that("data_link_specimens keeps the second branch for the 277 rows", {

  p <- .plot_scope_predicate("data_link_specimens", "read")

  # Branch one: through the individual, 160,903 rows.
  expect_true(grepl("i.id_n = data_link_specimens.id_n", p, fixed = TRUE))
  # Branch two: its own key, the 277 rows nothing else reaches.
  expect_true(grepl("data_link_specimens.id_liste_plots = ANY",
                    p, fixed = TRUE))
  expect_true(grepl(" OR ", p, fixed = TRUE))
  # Parenthesised as a whole, or the OR would escape the policy's AND context.
  expect_true(startsWith(p, "("))
  expect_true(endsWith(p, ")"))

  # It is the only table with a second branch.
  others <- setdiff(.plot_scope_child_tables(), "data_link_specimens")
  for (tb in others) {
    expect_false(grepl(" OR ", .plot_scope_predicate(tb, "read"), fixed = TRUE),
                 info = tb)
  }
})


test_that("every child table gets all four commands covered", {

  for (tb in .plot_scope_child_tables()) {

    st <- .plot_scope_policy_statements(tb)

    expect_length(st, 5L)
    expect_true(grepl("ENABLE ROW LEVEL SECURITY", st[1], fixed = TRUE),
                info = tb)

    # Row-level security denies what it has no policy for, so a missing command
    # blocks it rather than leaving it open.
    for (cmd in c("FOR SELECT", "FOR INSERT", "FOR UPDATE", "FOR DELETE")) {
      expect_true(any(grepl(cmd, st, fixed = TRUE)), info = paste(tb, cmd))
    }
  }
})


test_that("SELECT reads the full grant list and writes read the write list", {

  for (tb in .plot_scope_child_tables()) {

    st  <- .plot_scope_policy_statements(tb)
    sel <- st[grepl("FOR SELECT", st, fixed = TRUE)]
    ins <- st[grepl("FOR INSERT", st, fixed = TRUE)]
    upd <- st[grepl("FOR UPDATE", st, fixed = TRUE)]
    del <- st[grepl("FOR DELETE", st, fixed = TRUE)]

    expect_false(grepl("can_write", sel, fixed = TRUE), info = tb)
    expect_true(grepl("can_write", ins, fixed = TRUE), info = tb)
    expect_true(grepl("can_write", upd, fixed = TRUE), info = tb)
    expect_true(grepl("can_write", del, fixed = TRUE), info = tb)

    # can_delete governs data_liste_plots only. Keying child DELETE on it would
    # break safe_delete_individual_features() for 13,913 write-only grants.
    expect_false(grepl("can_delete", paste(st, collapse = " "), fixed = TRUE),
                 info = tb)
  }
})


test_that("UPDATE carries WITH CHECK as well as USING", {

  # USING alone lets an account update a row it can see and re-point it at a
  # plot it cannot. mod_update_record.R:376 moves individuals between plots.
  for (tb in .plot_scope_child_tables()) {

    upd <- .plot_scope_policy_statements(tb)
    upd <- upd[grepl("FOR UPDATE", upd, fixed = TRUE)]

    expect_true(grepl("USING (", upd, fixed = TRUE), info = tb)
    expect_true(grepl("WITH CHECK (", upd, fixed = TRUE), info = tb)
  }
})


test_that("rollback disables row-level security before dropping anything", {

  rb <- .plot_scope_rollback_statements("data_individuals")

  expect_true(grepl("DISABLE ROW LEVEL SECURITY", rb[1], fixed = TRUE))
  expect_equal(sum(grepl("DROP POLICY IF EXISTS", rb, fixed = TRUE)), 4L)
})


test_that("policy names cannot collide with the ones already on the database", {

  nm <- .plot_scope_policy_names("data_individuals")

  expect_length(unique(nm), 4L)
  # define_user_policy() writes policy_<account>_<command>; add_created_by.R
  # writes creator_access_*; created_by_server_asserted.R writes insert_own.
  expect_false(any(grepl("^policy_", nm)))
  expect_false(any(grepl("^creator_access", nm)))
  expect_false(any(nm == "insert_own"))
})


test_that(".plot_scope_route() refuses a table it has no route for", {

  expect_error(.plot_scope_route("data_liste_plots"), "No plot-scope route")
  expect_error(.plot_scope_route("table_colnam"),     "No plot-scope route")
})


test_that("referenced columns are listed for validation against the catalog", {

  cols <- .plot_scope_referenced_columns()

  expect_s3_class(cols, "data.frame")
  expect_named(cols, c("table_name", "column_name"))
  expect_equal(nrow(cols), nrow(unique(cols)))

  # The join targets must be listed too, not just the policies' own tables.
  expect_true(any(cols$table_name == "data_individuals" &
                  cols$column_name == "id_n"))
  expect_true(any(cols$table_name == "data_link_specimens" &
                  cols$column_name == "id_liste_plots"))
  expect_true(any(cols$table_name == "data_traits_measures" &
                  cols$column_name == "id_data_individuals"))

  # Nothing names the column that is wrong on 8,575 rows.
  expect_false(any(cols$table_name == "data_traits_measures" &
                   cols$column_name == "id_table_liste_plots"))
})


test_that("no predicate mentions a table outside the plot scope", {

  allowed <- c(.plot_scope_tables(), "plot_access")

  for (tb in .plot_scope_child_tables()) {
    p <- .plot_scope_predicate(tb, "write")
    hits <- regmatches(p, gregexpr("public[.][a-z_]+", p))[[1]]
    hits <- sub("^public[.]", "", hits)
    expect_true(all(hits %in% allowed), info = paste(tb, paste(hits, collapse = ",")))
  }
})
