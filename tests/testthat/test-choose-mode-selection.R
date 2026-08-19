# Tests for R/mod_feat_step2_choose_mode.R
#
# Eight operation cards fill more than one screen, so what tells the user the
# click registered — and that Next is what comes next — is the whole point of
# this step.

test_that(".mode_selection_js marks the chosen card and fades the others", {
  js <- .mode_selection_js(
    c("step2-card_census", "step2-card_features", "step2-card_measurements"),
    "step2-card_features"
  )

  # every card is reset first, so a second choice cannot leave two marked
  expect_equal(lengths(regmatches(js, gregexpr("classList.remove('selected')", js, fixed = TRUE))), 3L)
  expect_match(js, "getElementById('step2-card_features')", fixed = TRUE)
  expect_match(js, "chosen.classList.add('selected')", fixed = TRUE)
  expect_match(js, "chosen.classList.remove('dimmed')", fixed = TRUE)
})

test_that(".mode_selection_js reveals the buttons only when they are off screen", {
  js <- .mode_selection_js("step2-card_census", "step2-card_census")

  expect_match(js, "getBoundingClientRect", fixed = TRUE)
  expect_match(js, "box.top < 0 || box.bottom > h", fixed = TRUE)
  expect_match(js, "scrollIntoView", fixed = TRUE)
})

test_that(".mode_selection_js is valid JavaScript", {
  skip_if(Sys.which("node") == "", "node is not available")

  js_file <- withr::local_tempfile(fileext = ".js")
  writeLines(
    .mode_selection_js(c("step2-card_census", "step2-card_features"),
                       "step2-card_census"),
    js_file
  )

  expect_equal(system2("node", c("--check", shQuote(js_file)),
                       stdout = FALSE, stderr = FALSE), 0L)
})

test_that("clicking a card records the mode and confirms it in words", {
  skip_if_not_installed("shiny")

  shiny::testServer(
    mod_feat_step2_choose_mode_server,
    args = list(i18n = shiny::reactive(list(t = function(x) x))),
    {
      expect_null(session$getReturned()())

      session$setInputs(mode_selected = "import_census")

      expect_equal(session$getReturned()(), "import_census")
      html <- as.character(output$mode_indicator$html)
      expect_match(html, "Selected: Import a Full Census", fixed = TRUE)
      expect_match(html, "Click Next to continue.", fixed = TRUE)
    }
  )
})
