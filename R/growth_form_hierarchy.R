# Which growth form sits under which
#
# Growth forms are recorded on three traits, `growth_form_level_1` (id 41),
# `growth_form_level_2` (id 120) and `growth_form_level_3` (id 121). Each level
# refines the one above it: `tree` only makes sense under `woody`, which only
# makes sense under `terrestrial_self_supporting`.
#
# Until 1.9.8 that nesting was written into `traitlist` itself. There were seven
# traits, one per branch, and the description of each ended with the parent it
# belonged to ("... growth form if herbaceous."). Collapsing them into three flat
# traits removed the only place the database held the nesting: the level 2 and
# level 3 descriptions now cover every branch at once. The selector and
# choose_growth_form() kept parsing "if <parent>" out of the description, found
# nothing, and offered level 1 alone.
#
# The nesting therefore lives here. `factorlevels` in `traitlist` still decides
# which values are accepted; this table only decides where each one is offered.


#' Parent of every level 2 and level 3 growth form
#'
#' Transcribed from the seven branch traits (ids 42 to 47) that
#' `growth_form_level_2` and `growth_form_level_3` replaced. `climber` is a
#' level 2 value, so its four kinds are level 3, even though the old trait
#' holding them (id 47) called itself a "second hierarchical level".
#'
#' @return Data frame with columns `level` (2 or 3), `value` and `parent`, in
#'   the order values are offered.
#' @keywords internal
#' @noRd
.growth_form_hierarchy <- function() {
  branch <- function(level, parent, values) {
    data.frame(level = level, value = values, parent = parent,
               stringsAsFactors = FALSE)
  }

  rbind(
    branch(2, "terrestrial_self_supporting",
           c("herbaceous", "semi_woody", "woody")),
    branch(2, "not_self_supporting",
           c("epiphyte", "lithophyte", "climber", "hydrophyte",
             "saprophyte_parasite")),
    branch(3, "herbaceous",
           c("rosette", "elongated_leaf_bearing_rhizomatous", "cushion",
             "extensive_stemmmed", "tussock")),
    branch(3, "semi_woody",
           c("palmoid", "bambusoid", "succulent_stem")),
    branch(3, "woody",
           c("prostrate_shrub", "dwarf_shrub", "shrub", "tree", "dwarf_tree")),
    branch(3, "climber",
           c("herbaceous_vine", "woody_vine_liana", "scrambler", "strangler"))
  )
}


#' Growth forms that can be chosen under a parent
#'
#' @param parent Character scalar, the value chosen one level up.
#' @param level Integer, 2 or 3: the level being offered.
#' @param factorlevels Optional character vector, the values `traitlist`
#'   accepts for that level. A value of the hierarchy missing from it is not
#'   offered, so a value retired from the database cannot be written back.
#'
#' @return Character vector, possibly empty: a value with no finer form (e.g.
#'   `epiphyte`) has no children.
#' @keywords internal
#' @noRd
.growth_form_children <- function(parent, level, factorlevels = NULL) {
  if (length(parent) != 1 || is.na(parent) || !nzchar(parent)) {
    return(character(0))
  }

  hierarchy <- .growth_form_hierarchy()
  children <- hierarchy$value[hierarchy$level == level &
                                hierarchy$parent == parent]

  if (!is.null(factorlevels)) {
    children <- children[children %in% factorlevels]
  }
  children
}


#' Accepted growth forms the hierarchy cannot place
#'
#' A value added to `factorlevels` without being added to
#' [.growth_form_hierarchy()] has no parent, so it is never offered. This names
#' them, so they can be reported rather than silently hidden.
#'
#' @inheritParams .growth_form_children
#'
#' @return Character vector of the values in `factorlevels` with no parent.
#' @keywords internal
#' @noRd
.growth_form_unplaced <- function(level, factorlevels) {
  hierarchy <- .growth_form_hierarchy()
  setdiff(factorlevels, hierarchy$value[hierarchy$level == level])
}


#' Split a `traitlist.factorlevels` string into values
#'
#' Tolerates a comma with or without a following space.
#'
#' @param x Character scalar, e.g. `"woody, herbaceous"`.
#'
#' @return Character vector; empty for `NA` or an empty string.
#' @keywords internal
#' @noRd
.parse_factorlevels <- function(x) {
  if (length(x) != 1 || is.na(x) || !nzchar(trimws(x))) {
    return(character(0))
  }
  values <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  values[nzchar(values)]
}
