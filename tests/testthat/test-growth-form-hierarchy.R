# Tests for R/growth_form_hierarchy.R
#
# Since the seven branch traits were collapsed into growth_form_level_2 and
# growth_form_level_3, this table is the only record of which growth form sits
# under which. Losing a parent means a value is never offered.

test_that("every level 2 value hangs from a level 1 value", {
  hierarchy <- .growth_form_hierarchy()
  level_2 <- hierarchy[hierarchy$level == 2, ]

  expect_true(all(level_2$parent %in%
                    c("terrestrial_self_supporting", "not_self_supporting")))
})

test_that("every level 3 value hangs from a level 2 value", {
  hierarchy <- .growth_form_hierarchy()

  expect_true(all(hierarchy$parent[hierarchy$level == 3] %in%
                    hierarchy$value[hierarchy$level == 2]))
})

test_that("no value is placed twice", {
  hierarchy <- .growth_form_hierarchy()

  expect_false(anyDuplicated(hierarchy$value) > 0)
})

test_that("level 2 offers only the children of the level 1 choice", {
  expect_equal(.growth_form_children("terrestrial_self_supporting", 2),
               c("herbaceous", "semi_woody", "woody"))
  expect_equal(
    .growth_form_children("not_self_supporting", 2),
    c("epiphyte", "lithophyte", "climber", "hydrophyte", "saprophyte_parasite"))
})

test_that("climbers are refined at level 3", {
  expect_equal(.growth_form_children("climber", 3),
               c("herbaceous_vine", "woody_vine_liana", "scrambler", "strangler"))
  expect_length(.growth_form_children("climber", 2), 0)
})

test_that("a value with no finer form has no children", {
  expect_length(.growth_form_children("epiphyte", 3), 0)
  expect_length(.growth_form_children("tree", 3), 0)
})

test_that("no choice yet means nothing to offer", {
  expect_length(.growth_form_children("", 2), 0)
  expect_length(.growth_form_children(NA_character_, 2), 0)
  expect_length(.growth_form_children(NULL, 2), 0)
})

test_that("only values traitlist still accepts are offered", {
  expect_equal(
    .growth_form_children("woody", 3,
                          factorlevels = c("tree", "shrub", "not_a_form")),
    c("shrub", "tree"))
})

test_that("accepted values the hierarchy cannot place are named", {
  expect_equal(.growth_form_unplaced(2, c("woody", "moss")), "moss")
  expect_length(.growth_form_unplaced(3, c("tree", "strangler")), 0)
})

test_that(".parse_factorlevels splits with or without a space", {
  expect_equal(.parse_factorlevels("woody, herbaceous"), c("woody", "herbaceous"))
  expect_equal(.parse_factorlevels("woody,herbaceous ,"), c("woody", "herbaceous"))
  expect_length(.parse_factorlevels(NA_character_), 0)
  expect_length(.parse_factorlevels(""), 0)
})
