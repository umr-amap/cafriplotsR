# Tests for R/utils_translations.R

test_that('get_translations returns named dictionaries for English and French', {
  en <- get_translations('en')
  fr <- get_translations('fr')

  expect_type(en, 'list')
  expect_type(fr, 'list')
  expect_true(length(en) > 0)
  expect_true(length(fr) > 0)
  expect_true(all(c('app_title', 'btn_start', 'msg_error') %in% names(en)))
  expect_true(all(c('app_title', 'btn_start', 'msg_error') %in% names(fr)))
  expect_false(identical(en$app_title, fr$app_title))
})

test_that('get_translations rejects unsupported languages', {
  err <- tryCatch(
    {
      get_translations('es')
      NULL
    },
    error = function(e) e
  )

  expect_s3_class(err, 'error')
  expect_match(conditionMessage(err), 'one of')
})

test_that('t_ returns translations for known keys and key fallback for unknown ones', {
  expect_equal(t_('btn_start', 'en'), 'Start')
  expect_equal(t_('btn_start', 'fr'), 'Démarrer')
  expect_equal(t_('missing_key_xyz', 'en'), 'missing_key_xyz')
})

