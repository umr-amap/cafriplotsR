# No test may reach the real database or see a real login.
#
# A developer's .Renviron usually holds MYDB_USER / MYDB_PASS, and an
# interactive session often has a login cached and connections open. Any code
# path that falls back to call.mydb() would then quietly use them, and a failed
# expectation on a credential would print the real password. So for the whole
# run the environment variables are blanked, and the session's cached
# credentials and connections are set aside. Both are put back afterwards, so
# running the tests does not log the developer out.

withr::local_envvar(
  c(MYDB_USER = NA, MYDB_PASS = NA,
    CAFRI_PUBLIC_USER = NA, CAFRI_PUBLIC_PASS = NA),
  .local_envir = testthat::teardown_env()
)

local({
  saved_credentials <- as.list(credentials)
  rm(list = ls(envir = credentials), envir = credentials)

  db_slots <- c("mydb", "mydb_taxa", "pool_main", "pool_taxa")
  saved_db <- mget(db_slots, envir = .db_env, ifnotfound = list(NULL))
  for (slot in db_slots) .db_env[[slot]] <- NULL

  withr::defer({
    rm(list = ls(envir = credentials), envir = credentials)
    list2env(saved_credentials, envir = credentials)
    for (slot in db_slots) .db_env[[slot]] <- saved_db[[slot]]
  }, envir = testthat::teardown_env())
})
