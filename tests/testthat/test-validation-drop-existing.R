# Dropping measurements the database already holds
#
# Validation reports how many rows repeat a measurement already recorded for
# the same individual, feature and census. Whether that is a mistake depends
# on the campaign: a second measurement of the same stem in the same census is
# sometimes deliberate. So the rows are offered for removal, unticked, and
# nothing is dropped unless the user asks.
#
# The filter is exercised through the module's own reactives rather than a
# live validation run, which would need a database.

validated <- function(n = 6, existing = integer(0)) {
  data <- data.frame(
    stringsAsFactors = FALSE,
    plot_name  = "PLOT1",
    tag        = seq_len(n),
    trait_name = "stem_diameter",
    traitvalue = seq_len(n) + 10
  )
  list(
    valid    = TRUE,
    data     = data,
    errors   = data.frame(row = integer(), column = character(),
                          issue = character(), stringsAsFactors = FALSE),
    warnings = data.frame(row = integer(), column = character(),
                          warning = character(), stringsAsFactors = FALSE),
    summary  = list(total_rows = n, errors = 0L, warnings = 0L),
    existing_rows = existing
  )
}

with_validation <- function(res, expr) {
  expr <- substitute(expr)
  shiny::testServer(
    mod_feat_step5_validation_server,
    args = list(
      matched_data   = shiny::reactive(NULL),
      feature_config = shiny::reactive(NULL),
      selected_plots = shiny::reactive(NULL),
      operation_mode = shiny::reactive("add_measurements"),
      con            = shiny::reactive(NULL),
      i18n           = shiny::reactive(list(t = function(x) x))
    ),
    {
      validation_result(res)
      eval(expr)
    }
  )
}

test_that("nothing is removed until the box is ticked", {
  with_validation(validated(6, existing = c(2L, 4L)), {
    expect_equal(nrow(effective_result()$data), 6)
    expect_null(effective_result()$dropped_existing)
  })
})

test_that("ticking the box removes exactly the rows the database already has", {
  with_validation(validated(6, existing = c(2L, 4L)), {
    session$setInputs(drop_existing = TRUE)

    kept <- effective_result()$data
    expect_equal(nrow(kept), 4)
    expect_equal(kept$tag, c(1, 3, 5, 6))
    expect_equal(effective_result()$dropped_existing, 2)
    expect_equal(effective_result()$summary$total_rows, 4)
  })
})

test_that("unticking the box puts them back", {
  with_validation(validated(6, existing = c(2L, 4L)), {
    session$setInputs(drop_existing = TRUE)
    expect_equal(nrow(effective_result()$data), 4)

    session$setInputs(drop_existing = FALSE)
    expect_equal(nrow(effective_result()$data), 6)
    expect_equal(effective_result()$data$tag, 1:6)
  })
})

test_that("removing every row fails validation instead of importing nothing", {
  # An import of zero rows would otherwise run and report success.
  with_validation(validated(3, existing = 1:3), {
    session$setInputs(drop_existing = TRUE)

    res <- effective_result()
    expect_equal(nrow(res$data), 0)
    expect_false(res$valid)
    expect_equal(nrow(res$errors), 1)
    expect_equal(res$summary$errors, 1)
  })
})

test_that("the box has no effect when the database holds none of the rows", {
  with_validation(validated(4, existing = integer(0)), {
    session$setInputs(drop_existing = TRUE)
    expect_equal(nrow(effective_result()$data), 4)
    expect_null(effective_result()$dropped_existing)
  })
})

test_that("the offer appears only when there is something to drop", {
  with_validation(validated(4, existing = integer(0)), {
    expect_null(output$existing_filter_ui)
  })

  with_validation(validated(6, existing = c(2L, 4L)), {
    html <- paste(as.character(output$existing_filter_ui$html), collapse = " ")
    expect_true(grepl("drop_existing", html, fixed = TRUE))
    expect_true(grepl("Remove the 2 measurement", html, fixed = TRUE))
    # Unticked by default: the input must not carry a checked attribute
    expect_false(grepl("checked", html, fixed = TRUE))
  })
})

test_that("the offer stays on screen, and reports its effect, once ticked", {
  with_validation(validated(6, existing = c(2L, 4L)), {
    session$setInputs(drop_existing = TRUE)

    expect_false(is.null(output$existing_filter_ui))
    effect <- paste(as.character(output$existing_filter_effect$html), collapse = " ")
    expect_true(grepl("2 row(s) removed. 4 row(s) will be imported.",
                      effect, fixed = TRUE))
  })
})

test_that("what the module hands to the import step is the filtered data", {
  # The wizard copies this into rv$matched_data, which step 6 writes from, so
  # the removal has to survive the module boundary.
  with_validation(validated(6, existing = c(2L, 4L)), {
    returned <- session$getReturned()

    expect_equal(nrow(returned()$data), 6)
    session$setInputs(drop_existing = TRUE)
    expect_equal(nrow(returned()$data), 4)
    expect_equal(returned()$data$tag, c(1, 3, 5, 6))
  })
})
