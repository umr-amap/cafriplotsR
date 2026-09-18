# The Tropicos API key is a personal credential. No key ships with the package,
# so get_tropicos_key() has to find the user's own - from its argument, the
# session cache, or the environment - and say so plainly when there is none.

# Every test runs against a clean cache and a clean environment variable, and
# leaves both as it found them.
with_clean_key <- function(code) {
  had_cache <- exists("tropicos_key", envir = credentials, inherits = FALSE)
  old_cache <- if (had_cache) get("tropicos_key", envir = credentials) else NULL
  old_env <- Sys.getenv("TROPICOS_API_KEY", unset = NA_character_)

  if (had_cache) rm("tropicos_key", envir = credentials, inherits = FALSE)
  Sys.unsetenv("TROPICOS_API_KEY")

  on.exit({
    if (exists("tropicos_key", envir = credentials, inherits = FALSE)) {
      rm("tropicos_key", envir = credentials, inherits = FALSE)
    }
    if (had_cache) assign("tropicos_key", old_cache, envir = credentials)
    if (is.na(old_env)) Sys.unsetenv("TROPICOS_API_KEY") else Sys.setenv(TROPICOS_API_KEY = old_env)
  }, add = TRUE)

  force(code)
}

test_that("no key anywhere returns NULL rather than a shipped default", {
  with_clean_key({
    expect_null(get_tropicos_key(prompt = FALSE))
  })
})

test_that("a key given explicitly is returned and cached", {
  with_clean_key({
    expect_equal(get_tropicos_key("abc-123", prompt = FALSE), "abc-123")
    # Cached: a later call finds it without being given anything
    expect_equal(get_tropicos_key(prompt = FALSE), "abc-123")
  })
})

test_that("the environment variable is used and cached", {
  with_clean_key({
    Sys.setenv(TROPICOS_API_KEY = "from-renviron")
    expect_equal(get_tropicos_key(prompt = FALSE), "from-renviron")

    # Once cached, unsetting the variable does not lose the key
    Sys.unsetenv("TROPICOS_API_KEY")
    expect_equal(get_tropicos_key(prompt = FALSE), "from-renviron")
  })
})

test_that("an explicit key wins over the environment", {
  with_clean_key({
    Sys.setenv(TROPICOS_API_KEY = "from-renviron")
    expect_equal(get_tropicos_key("explicit", prompt = FALSE), "explicit")
  })
})

test_that("blank and missing keys are treated as no key at all", {
  with_clean_key({
    expect_null(get_tropicos_key("", prompt = FALSE))
    expect_null(get_tropicos_key("   ", prompt = FALSE))
    expect_null(get_tropicos_key(NA_character_, prompt = FALSE))
    expect_null(get_tropicos_key(character(0), prompt = FALSE))
    # None of those should have polluted the cache
    expect_false(exists("tropicos_key", envir = credentials, inherits = FALSE))
  })
})

test_that("an empty environment variable is not mistaken for a key", {
  with_clean_key({
    Sys.setenv(TROPICOS_API_KEY = "")
    expect_null(get_tropicos_key(prompt = FALSE))
  })
})

test_that("surrounding whitespace is trimmed off a key", {
  with_clean_key({
    expect_equal(get_tropicos_key("  key-with-spaces  ", prompt = FALSE),
                 "key-with-spaces")
  })
})

test_that("prompt = FALSE never blocks when there is no key", {
  with_clean_key({
    # A prompt in a non-interactive session would error; this must not.
    expect_silent(res <- get_tropicos_key(prompt = FALSE))
    expect_null(res)
  })
})

test_that("setup_tropicos_key() refuses an empty key without touching .Renviron", {
  with_clean_key({
    expect_false(suppressMessages(setup_tropicos_key("   ")))
    expect_null(get_tropicos_key(prompt = FALSE))
  })
})

test_that("no Tropicos key is hard-coded in the package sources", {
  skip_if_not(dir.exists(testthat::test_path("..", "..", "R")))
  r_files <- list.files(testthat::test_path("..", "..", "R"),
                        pattern = "\\.R$", full.names = TRUE)
  # A Tropicos key is a UUID; none should appear in the sources.
  uuid <- "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
  offenders <- Filter(
    function(f) any(grepl(uuid, readLines(f, warn = FALSE), perl = TRUE)),
    r_files
  )
  expect_equal(basename(offenders), character(0))
})
