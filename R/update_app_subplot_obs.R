# Data Update App - observation features of a plot feature
#
# A subplot record (a census, most often) can itself be described by features:
# the people of a census, for instance. Those are rows of data_subplot_feat,
# keyed on id_sub_plots. Unlike the features of section 4 they are added and
# removed, not edited in place: changing a team member is removing one person
# and adding another. Both are staged in the app and written by section 5, in
# the same transaction as the other edits of the plot.

#' Feature types offered for subplot observation features
#'
#' The people types (`valuetype == "table_colnam"`) come first, then the other
#' types already used in `data_subplot_feat`. With `all_types = TRUE` every
#' remaining type of `subplotype_list` is offered as well.
#'
#' @param con A DBI connection.
#' @param all_types Logical. Offer every feature type?
#' @return A tibble with `id_subplotype`, `type`, `valuetype` and `group`
#'   (`"people"`, `"used"` or `"other"`), in display order.
#' @keywords internal
.upd_obs_feature_types <- function(con, all_types = FALSE) {
  types <- dplyr::as_tibble(DBI::dbGetQuery(
    con, "SELECT id_subplotype, type, valuetype FROM subplotype_list"
  ))
  used <- tryCatch(
    DBI::dbGetQuery(
      con, "SELECT DISTINCT id_type_sub_plot FROM data_subplot_feat"
    )$id_type_sub_plot,
    error = function(e) integer(0)
  )
  .upd_group_obs_feature_types(types, used, all_types)
}

#' Group and order feature types for the observation-feature picker
#'
#' Pure part of [.upd_obs_feature_types()].
#'
#' @param types Data frame with `id_subplotype`, `type`, `valuetype`.
#' @param used Integer vector of `id_subplotype` already used in
#'   `data_subplot_feat`.
#' @param all_types Logical. Keep the types of neither group?
#' @return A tibble, see [.upd_obs_feature_types()].
#' @keywords internal
.upd_group_obs_feature_types <- function(types, used, all_types = FALSE) {
  types <- dplyr::as_tibble(types)
  types$group <- ifelse(
    !is.na(types$valuetype) & types$valuetype == "table_colnam", "people",
    ifelse(types$id_subplotype %in% used, "used", "other")
  )
  if (!all_types) types <- types[types$group != "other", , drop = FALSE]
  types[order(match(types$group, c("people", "used", "other")), types$type), ,
        drop = FALSE]
}

#' Build the data_subplot_feat rows for new observation features
#'
#' One row per value. A people feature takes several person ids at once; every
#' other type takes one value. Values that are blank, or already recorded for
#' the same subplot and type, are dropped.
#'
#' The value goes where the feature wizard and `add_subplot_features()` put it:
#' `typevalue` for `numeric`, `integer` and `table_colnam` types (the person id
#' is a number), `typevalue_char` for the others.
#'
#' @param id_sub_plots Integer(1). The subplot record being described.
#' @param id_type Integer(1). `subplotype_list.id_subplotype` of the feature.
#' @param valuetype Character(1). `subplotype_list.valuetype` of the feature.
#' @param values Vector of values (person ids for a people feature).
#' @param existing Data frame of the observation features already recorded,
#'   with `id_sub_plots`, `id_type_sub_plot`, `typevalue`, `typevalue_char`.
#' @return A data frame ready for `data_subplot_feat`, zero rows if nothing is
#'   left to add. Stops on a value a numeric type cannot hold.
#' @keywords internal
.upd_obs_feature_rows <- function(id_sub_plots, id_type, valuetype, values,
                                  existing = NULL) {
  values <- as.character(values)
  values <- unique(trimws(values[!is.na(values)]))
  values <- values[nzchar(values)]

  numeric_store <- !is.na(valuetype) &&
    valuetype %in% c("numeric", "integer", "table_colnam")

  if (numeric_store) {
    num <- suppressWarnings(as.numeric(values))
    if (anyNA(num)) {
      stop("Not a number: ", paste(values[is.na(num)], collapse = ", "),
           call. = FALSE)
    }
    keep <- !duplicated(num)
    num <- num[keep]
    values <- values[keep]
  }

  if (!is.null(existing) && nrow(existing) > 0) {
    same <- existing[existing$id_sub_plots == id_sub_plots &
                       existing$id_type_sub_plot == id_type, , drop = FALSE]
    if (numeric_store) {
      new <- !num %in% same$typevalue
      num <- num[new]
      values <- values[new]
    } else {
      values <- values[!values %in% trimws(same$typevalue_char)]
    }
  }

  n <- length(values)
  if (n == 0) {
    return(data.frame(
      id_sub_plots = integer(0), id_type_sub_plot = integer(0),
      typevalue = numeric(0), typevalue_char = character(0),
      date_modif_d = integer(0), date_modif_m = integer(0),
      date_modif_y = integer(0)
    ))
  }

  today <- Sys.Date()
  data.frame(
    id_sub_plots     = rep(as.integer(id_sub_plots), n),
    id_type_sub_plot = rep(as.integer(id_type), n),
    typevalue        = if (numeric_store) num else rep(NA_real_, n),
    typevalue_char   = if (numeric_store) rep(NA_character_, n) else values,
    date_modif_d     = rep(as.integer(format(today, "%d")), n),
    date_modif_m     = rep(as.integer(format(today, "%m")), n),
    date_modif_y     = rep(as.integer(format(today, "%Y")), n),
    stringsAsFactors = FALSE
  )
}


# Stored rows can be NULL (not loaded) or a column-less tibble (no subplot):
# both mean "nothing stored".
.upd_obs_has_rows <- function(x) {
  !is.null(x) && nrow(x) > 0 && "id_subplot_feat" %in% names(x)
}

#' Rows a new value is checked against: stored and already staged
#'
#' @param stored Stored `data_subplot_feat` rows, as
#'   [fetch_subplot_obs_features_long()] returns them, or `NULL`.
#' @param staged Staged additions, or `NULL`.
#' @return A data frame with `id_sub_plots`, `id_type_sub_plot`, `typevalue`,
#'   `typevalue_char`.
#' @keywords internal
.upd_obs_existing <- function(stored, staged) {
  cols <- c("id_sub_plots", "id_type_sub_plot", "typevalue", "typevalue_char")
  parts <- list()
  if (.upd_obs_has_rows(stored)) {
    parts[[1]] <- as.data.frame(stored)[, cols, drop = FALSE]
  }
  if (!is.null(staged) && nrow(staged) > 0) {
    parts[[length(parts) + 1]] <- as.data.frame(staged)[, cols, drop = FALSE]
  }
  if (length(parts) == 0) return(NULL)
  do.call(rbind, parts)
}

#' Label staged additions for display
#'
#' @param rows Data frame from [.upd_obs_feature_rows()].
#' @param type Character(1). The feature type name.
#' @param people Named vector (name -> id) for a people feature, else `NULL`.
#' @param staged Additions already staged, to continue their `.key` sequence.
#' @return `rows` with `.type`, `.label` and a unique `.key`.
#' @keywords internal
.upd_obs_label_rows <- function(rows, type, people = NULL, staged = NULL) {
  rows$.type <- rep(type, nrow(rows))
  rows$.label <- if (!is.null(people)) {
    vapply(rows$typevalue, function(id) {
      hit <- names(people)[people == as.character(id)]
      if (length(hit) == 1) hit else as.character(id)
    }, character(1))
  } else if (all(is.na(rows$typevalue))) {
    rows$typevalue_char
  } else {
    as.character(rows$typevalue)
  }
  last <- if (is.null(staged) || nrow(staged) == 0) 0L else max(staged$.key)
  rows$.key <- last + seq_len(nrow(rows))
  rows
}

#' Observation features of one subplot record, with their pending status
#'
#' @param stored Stored rows (see [.upd_obs_existing()]).
#' @param staged Staged additions, or `NULL`.
#' @param drop Integer vector of `id_subplot_feat` staged for removal.
#' @param id_sub_plots Integer(1). The subplot record shown.
#' @return A data frame with `id_subplot_feat` (`NA` for an addition), `type`,
#'   `label`, `status` (`"stored"`, `"remove"` or `"add"`) and `key` (the
#'   addition's `.key`, `NA` for stored rows).
#' @keywords internal
.upd_obs_view <- function(stored, staged, drop, id_sub_plots) {
  out <- data.frame(id_subplot_feat = integer(0), type = character(0),
                    label = character(0), status = character(0),
                    key = integer(0), stringsAsFactors = FALSE)

  if (.upd_obs_has_rows(stored)) {
    s <- stored[stored$id_sub_plots == id_sub_plots, , drop = FALSE]
    if (nrow(s) > 0) {
      label <- ifelse(!is.na(s$colnam), s$colnam,
                      ifelse(!is.na(s$typevalue), as.character(s$typevalue),
                             s$typevalue_char))
      out <- rbind(out, data.frame(
        id_subplot_feat = as.integer(s$id_subplot_feat), type = s$type,
        label = label,
        status = ifelse(s$id_subplot_feat %in% drop, "remove", "stored"),
        key = NA_integer_, stringsAsFactors = FALSE
      ))
    }
  }

  if (!is.null(staged) && nrow(staged) > 0) {
    a <- staged[staged$id_sub_plots == id_sub_plots, , drop = FALSE]
    if (nrow(a) > 0) {
      out <- rbind(out, data.frame(
        id_subplot_feat = NA_integer_, type = a$.type, label = a$.label,
        status = "add", key = as.integer(a$.key), stringsAsFactors = FALSE
      ))
    }
  }
  out
}

#' Write staged observation feature changes
#'
#' Runs inside the caller's transaction ([.upd_apply_all()]): removals first,
#' then additions, so replacing a person by the same person is harmless.
#'
#' @param changes `NULL`, or a list with `add` (staged rows, display columns
#'   included) and `remove` (integer `id_subplot_feat`).
#' @param con A DBI connection (not a pool).
#' @return A list with `n_added` and `n_removed`.
#' @keywords internal
.upd_apply_obs_features <- function(changes, con) {
  out <- list(n_added = 0L, n_removed = 0L)
  if (is.null(changes)) return(out)

  ids <- as.integer(changes$remove)
  ids <- ids[!is.na(ids)]
  if (length(ids) > 0) {
    out$n_removed <- DBI::dbExecute(con, glue::glue_sql(
      "DELETE FROM data_subplot_feat WHERE id_subplot_feat IN ({ids*})",
      .con = con
    ))
  }

  add <- changes$add
  if (!is.null(add) && nrow(add) > 0) {
    add <- as.data.frame(add)[, !startsWith(names(add), "."), drop = FALSE]
    out$n_added <- as.integer(.db_append_table(con, "data_subplot_feat", add))
  }
  out
}
