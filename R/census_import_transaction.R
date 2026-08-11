# Full-census import
#
# Executes a whole census in one transaction: census record, recruits, stem
# grouping, then measurements for every stem. The order matters — recruits
# have to exist before their own measurements can point at them — and doing
# it in separate passes is what leaves a half-imported census behind when
# one step fails.


#' Build a SQL `VALUES` clause from a data frame
#'
#' Column by column rather than row by row: `apply()` over a data frame goes
#' through `as.matrix()`, which formats numbers and can pad them with spaces.
#'
#' @param df Data frame to render.
#' @return Single string, `"(...), (...)"`.
#' @keywords internal
#' @export
.sql_values_rows <- function(df) {

  rendered <- lapply(names(df), function(nm) {
    v <- df[[nm]]
    if (is.numeric(v)) {
      out <- format(v, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
    } else {
      out <- sprintf("'%s'", gsub("'", "''", as.character(v)))
    }
    blank <- is.na(v) | (!is.numeric(v) & !nzchar(trimws(as.character(v))))
    out[blank] <- "NULL"
    out
  })

  paste0("(", do.call(paste, c(rendered, sep = ", ")), ")", collapse = ", ")
}


#' Resolve the `census` subplot type id
#'
#' @param con Database connection.
#' @return Integer id from `subplotype_list`.
#' @keywords internal
#' @export
.census_subplot_type_id <- function(con) {
  res <- DBI::dbGetQuery(
    con, "SELECT id_subplotype FROM subplotype_list WHERE type = 'census'"
  )
  if (nrow(res) == 0) {
    stop("No 'census' entry in subplotype_list — cannot create a census record.",
         call. = FALSE)
  }
  as.integer(res$id_subplotype[1])
}


#' Census records already recorded for a set of plots
#'
#' @param plot_ids Integer vector of `data_liste_plots.id_liste_plots`.
#' @param con Database connection.
#' @return Data frame with `id_sub_plots`, `id_table_liste_plots`,
#'   `census_num`, `year`, `month`, `day`.
#' @keywords internal
#' @export
.fetch_census_subplots <- function(plot_ids, con) {

  plot_ids <- unique(as.integer(plot_ids[!is.na(plot_ids)]))
  empty <- data.frame(
    id_sub_plots = integer(0), id_table_liste_plots = integer(0),
    census_num = numeric(0), year = integer(0), month = integer(0),
    day = integer(0), stringsAsFactors = FALSE
  )
  if (length(plot_ids) == 0) return(empty)

  DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT sp.id_sub_plots,
            sp.id_table_liste_plots,
            sp.typevalue AS census_num,
            sp.year, sp.month, sp.day
       FROM data_liste_sub_plots sp
       JOIN subplotype_list spt ON sp.id_type_sub_plot = spt.id_subplotype
      WHERE spt.type = 'census'
        AND sp.id_table_liste_plots IN ({plot_ids*})
      ORDER BY sp.id_table_liste_plots, sp.typevalue",
    plot_ids = plot_ids, .con = con
  ))
}


#' Create one census record per plot
#'
#' Refuses to create a census number a plot already carries — a duplicate
#' census record silently splits a campaign's measurements in two.
#'
#' @param plot_ids Integer vector of plot ids.
#' @param census_number Census number to create.
#' @param year,month,day Census date parts; `month` and `day` may be `NA`.
#' @param con Database connection.
#' @return Data frame with `id_table_liste_plots` and `id_sub_plots`.
#' @keywords internal
#' @export
.create_census_subplots <- function(plot_ids, census_number, year,
                                    month = NA, day = NA, con) {

  plot_ids <- unique(as.integer(plot_ids[!is.na(plot_ids)]))
  if (length(plot_ids) == 0) {
    stop("No plots to create a census for.", call. = FALSE)
  }
  if (is.na(census_number)) {
    stop("A census number is required.", call. = FALSE)
  }

  clash <- .fetch_census_subplots(plot_ids, con)
  clash <- clash[!is.na(clash$census_num) &
                   as.numeric(clash$census_num) == as.numeric(census_number), ,
                 drop = FALSE]
  if (nrow(clash) > 0) {
    stop(sprintf(
      "Census %s already exists for %d of the selected plot(s). Select it instead of creating it again.",
      census_number, nrow(clash)
    ), call. = FALSE)
  }

  type_id <- .census_subplot_type_id(con)
  today   <- Sys.Date()

  records <- data.frame(
    id_table_liste_plots = plot_ids,
    id_type_sub_plot     = type_id,
    typevalue            = as.numeric(census_number),
    year                 = if (is.na(year)) NA_integer_ else as.integer(year),
    month                = if (is.na(month)) NA_integer_ else as.integer(month),
    day                  = if (is.na(day)) NA_integer_ else as.integer(day),
    date_modif_d         = as.integer(format(today, "%d")),
    date_modif_m         = as.integer(format(today, "%m")),
    date_modif_y         = as.integer(format(today, "%Y")),
    stringsAsFactors     = FALSE
  )

  DBI::dbGetQuery(con, sprintf(
    "INSERT INTO data_liste_sub_plots (%s) VALUES %s
     RETURNING id_sub_plots, id_table_liste_plots",
    paste(names(records), collapse = ", "),
    .sql_values_rows(records)
  ))
}


#' Insert recruits into `data_individuals`
#'
#' @param recruits Data frame with `id_table_liste_plots_n`, `tag` and any of
#'   the optional individual columns.
#' @param con Database connection.
#' @return Data frame with `id_individuals`, `tag` and `plot_name`.
#' @keywords internal
#' @export
.insert_census_recruits <- function(recruits, con) {

  if (is.null(recruits) || nrow(recruits) == 0) {
    return(data.frame(id_individuals = integer(0), tag = character(0),
                      plot_name = character(0), stringsAsFactors = FALSE))
  }

  # Same column set the Import Wizard writes, so both paths stay in step
  expected <- c("id_table_liste_plots_n", "tag", "idtax_n", "original_tax_name",
                "herbarium_nbe_type", "herbarium_nbe_char", "multi_tiges_id")
  cols <- intersect(expected, names(recruits))
  if (!all(c("id_table_liste_plots_n", "tag") %in% cols)) {
    stop("Recruits need `id_table_liste_plots_n` and `tag`.", call. = FALSE)
  }

  payload <- recruits[, cols, drop = FALSE]

  inserted <- DBI::dbGetQuery(con, sprintf(
    "INSERT INTO data_individuals (%s) VALUES %s
     RETURNING id_n AS id_individuals, tag",
    paste(cols, collapse = ", "),
    .sql_values_rows(payload)
  ))

  # RETURNING preserves the order rows were supplied in, so the plot names of
  # the source frame can be carried across
  inserted$plot_name <- as.character(recruits$plot_name)
  inserted
}


#' Attach an individual id to every measurement row
#'
#' Remeasures resolve against the database, recruits against the ids just
#' returned by the insert. Both are keyed on plot name plus tag.
#'
#' @param data Measurement rows with `plot_name` and `tag`.
#' @param plot_ids Integer vector of plot ids in play.
#' @param new_individuals Result of [.insert_census_recruits()].
#' @param con Database connection.
#' @return `data` with an `id_data_individuals` column.
#' @keywords internal
#' @export
.resolve_census_individuals <- function(data, plot_ids, new_individuals, con) {

  key <- paste(as.character(data$plot_name), .normalize_tag(data$tag), sep = "\r")

  existing <- .fetch_plot_individuals(
    plot_names  = unique(as.character(data$plot_name)),
    con         = con,
    with_status = FALSE
  )
  lookup <- data.frame(
    key  = paste(existing$plot_name, .normalize_tag(existing$tag), sep = "\r"),
    id_n = as.integer(existing$id_n),
    stringsAsFactors = FALSE
  )

  if (!is.null(new_individuals) && nrow(new_individuals) > 0) {
    lookup <- rbind(lookup, data.frame(
      key  = paste(as.character(new_individuals$plot_name),
                   .normalize_tag(new_individuals$tag), sep = "\r"),
      id_n = as.integer(new_individuals$id_individuals),
      stringsAsFactors = FALSE
    ))
  }

  data$id_data_individuals <- lookup$id_n[match(key, lookup$key)]
  data
}


#' Validate a prepared full-census payload
#'
#' The step 3 split already decided which rows are recruits, so a tag absent
#' from the database is expected here rather than an error. What is checked is
#' what the split deliberately left to a human: the census identity, the
#' recruits' taxonomy, and the multi-stem grouping.
#'
#' @param data Long measurement rows from the step 3 module.
#' @param config Configuration list from the step 3 module.
#' @param con Database connection or pool.
#' @return List with `errors` and `warnings`, both lists of strings.
#' @keywords internal
#' @export
.validate_census_import <- function(data, config, con) {

  errors <- list()
  warnings <- list()

  if (is.null(config)) {
    return(list(errors = list("No census configuration — go back to step 3."),
                warnings = list()))
  }

  # ---- measurements -----------------------------------------------------
  if (is.null(data) || nrow(data) == 0) {
    errors <- c(errors, list("No measurement rows prepared."))
  } else {
    if (!"traitid" %in% names(data) || any(is.na(data$traitid))) {
      errors <- c(errors, list(
        "Some measurement rows have no trait id — check the column-to-trait mapping."
      ))
    }
    has_num  <- "traitvalue" %in% names(data) && any(!is.na(data$traitvalue))
    has_char <- "traitvalue_char" %in% names(data) && any(!is.na(data$traitvalue_char))
    if (!has_num && !has_char) {
      errors <- c(errors, list("No measurement values found."))
    }
  }

  # ---- census identity --------------------------------------------------
  plot_ids <- unique(as.integer(data$id_liste_plots[!is.na(data$id_liste_plots)]))

  if (identical(config$census_mode, "create")) {
    if (is.null(config$census_number) || is.na(config$census_number) ||
        config$census_number < 1) {
      errors <- c(errors, list("A census number is required."))
    }
    if (is.null(config$census_year) || is.na(config$census_year)) {
      errors <- c(errors, list("A census year is required."))
    }
    clash <- tryCatch({
      existing <- .fetch_census_subplots(plot_ids, con)
      existing[!is.na(existing$census_num) &
                 as.numeric(existing$census_num) == as.numeric(config$census_number), ,
               drop = FALSE]
    }, error = function(e) NULL)
    if (!is.null(clash) && nrow(clash) > 0) {
      errors <- c(errors, list(sprintf(
        "Census %s already exists for %d of the selected plot(s). Select it instead of creating it again.",
        config$census_number, nrow(clash)
      )))
    }
  } else {
    if (is.null(config$census_map) || nrow(config$census_map) == 0) {
      errors <- c(errors, list("Select the census these measurements belong to."))
    } else {
      uncovered <- setdiff(plot_ids, as.integer(config$census_map$id_table_liste_plots))
      if (length(uncovered) > 0) {
        errors <- c(errors, list(sprintf(
          "%d plot(s) in the file have no census selected — measurements would not be linked to any census.",
          length(uncovered)
        )))
      }
    }
  }

  # ---- recruits ---------------------------------------------------------
  recruits <- config$recruits
  if (!is.null(recruits) && nrow(recruits) > 0) {

    if (any(is.na(recruits$tag))) {
      errors <- c(errors, list(sprintf(
        "%d recruit(s) have no tag.", sum(is.na(recruits$tag))
      )))
    }
    dup <- duplicated(paste(recruits$plot_name, recruits$tag))
    if (any(dup)) {
      errors <- c(errors, list(sprintf(
        "%d recruit(s) repeat a plot + tag already in this import.", sum(dup)
      )))
    }
    n_unidentified <- .null_default(config$n_unidentified_recruits, 0L)
    if (n_unidentified > 0) {
      warnings <- c(warnings, list(sprintf(
        "%d of %d recruit(s) have no idtax_n and will be recorded as unidentified (Magnoliopsida, 351190). Run launch_taxonomic_match_app() on your file first to identify them.",
        n_unidentified, nrow(recruits)
      )))
    }

    if ("multi_tiges_id" %in% names(recruits)) {
      stem_check <- tryCatch(
        .validate_multi_stem_grouping(recruits, con),
        error = function(e) list(
          errors = list(),
          warnings = list(paste("Could not check multi-stem grouping:", e$message))
        )
      )
      errors   <- c(errors, stem_check$errors)
      warnings <- c(warnings, stem_check$warnings)
    }
  }

  # ---- what the split flagged -------------------------------------------
  split <- config$split
  if (!is.null(split)) {
    if (nrow(split$duplicates) > 0) {
      errors <- c(errors, list(sprintf(
        "%d plot + tag combination(s) appear more than once in the file — each stem must have one row.",
        nrow(split$duplicates)
      )))
    }
    if (nrow(split$review) > 0 && identical(config$n_review_included, 0L)) {
      warnings <- c(warnings, list(sprintf(
        "%d row(s) held for review are excluded from this import. Correct their tags and re-upload, or confirm them in step 3.",
        nrow(split$review)
      )))
    }
    if (nrow(split$invalid) > 0) {
      warnings <- c(warnings, list(sprintf(
        "%d row(s) without a usable plot name or tag are excluded.", nrow(split$invalid)
      )))
    }
    if (nrow(split$missing_stems) > 0) {
      warnings <- c(warnings, list(sprintf(
        "%d stem(s) recorded in these plots have no row in the file. Run Compute Stem Status after the import.",
        nrow(split$missing_stems)
      )))
    }
  }

  list(errors = errors, warnings = warnings)
}


#' Import a full census in one transaction
#'
#' @description
#' Creates or reuses the census record, inserts the recruits, resolves their
#' multi-stem grouping, then writes the measurements for every stem — all
#' inside a single transaction, so a failure at any point leaves the database
#' as it was rather than half-populated.
#'
#' @param data Long measurement rows carrying `plot_name`, `tag`,
#'   `id_liste_plots`, `traitid` and the value columns.
#' @param config Configuration list from the step 3 module. Uses `recruits`,
#'   `census_mode`, `census_map`, `census_number`, `census_year`,
#'   `census_month`, `census_day`, `features_field` and
#'   `features_field_mappings`.
#' @param con Database connection or pool.
#' @param dry_run Report what would happen without writing.
#' @param i18n Optional translator.
#'
#' @return List with `success`, `dry_run`, counts (`n_census_records`,
#'   `n_recruits`, `n_stem_grouping`, `n_measurements`) and a `message`.
#' @keywords internal
#' @export
.execute_census_import <- function(data, config, con, dry_run = TRUE, i18n = NULL) {

  t <- function(x) if (!is.null(i18n)) i18n$t(x) else x

  recruits <- config$recruits
  n_recruits <- if (is.null(recruits)) 0L else nrow(recruits)
  plot_ids <- unique(as.integer(data$id_liste_plots[!is.na(data$id_liste_plots)]))

  n_census <- if (identical(config$census_mode, "create")) length(plot_ids) else 0L

  if (dry_run) {
    preview <- data[, intersect(
      c("plot_name", "tag", "row_role", "trait_name", "traitvalue", "traitvalue_char"),
      names(data)
    ), drop = FALSE]

    return(list(
      dry_run = TRUE, success = TRUE,
      n_census_records = n_census,
      n_recruits       = n_recruits,
      n_stem_grouping  = 0L,
      n_measurements   = nrow(data),
      n_subplot_records = nrow(data),
      n_people_records  = 0L,
      preview = preview,
      message = sprintf(
        t("Dry run: %d census record(s), %d recruit(s) and %d measurement(s) would be written."),
        n_census, n_recruits, nrow(data)
      )
    ))
  }

  is_pool    <- inherits(con, "Pool")
  actual_con <- if (is_pool) pool::poolCheckout(con) else con
  on.exit(if (is_pool) pool::poolReturn(actual_con), add = TRUE)

  today <- Sys.Date()
  n_grouped <- 0L

  tryCatch({
    DBI::dbBegin(actual_con)

    # ---- 1. census record -------------------------------------------------
    if (identical(config$census_mode, "create")) {
      cli::cli_alert_info("Creating census {config$census_number} for {length(plot_ids)} plot(s)...")
      census_map <- .create_census_subplots(
        plot_ids      = plot_ids,
        census_number = config$census_number,
        year          = config$census_year,
        month         = config$census_month,
        day           = config$census_day,
        con           = actual_con
      )
    } else {
      census_map <- config$census_map
    }

    # ---- 2. recruits ------------------------------------------------------
    new_individuals <- NULL
    if (n_recruits > 0) {
      cli::cli_alert_info("Inserting {n_recruits} recruit{?s}...")
      new_individuals <- .insert_census_recruits(recruits, actual_con)
      cli::cli_alert_success("Inserted {nrow(new_individuals)} individual{?s}")

      # ---- 3. multi-stem grouping ----------------------------------------
      n_grouped <- .apply_stem_grouping(recruits, new_individuals, actual_con)
      if (n_grouped > 0) {
        cli::cli_alert_success("Grouped {n_grouped} secondary stem{?s}")
      }
    }

    # ---- 4. measurements --------------------------------------------------
    data <- .resolve_census_individuals(data, plot_ids, new_individuals, actual_con)

    unresolved <- sum(is.na(data$id_data_individuals))
    matched <- data[!is.na(data$id_data_individuals), , drop = FALSE]
    if (nrow(matched) == 0) {
      stop("No measurement row could be attached to an individual.", call. = FALSE)
    }

    # The census subplot is assigned here rather than in step 3, so a census
    # created moments ago in this same transaction is picked up
    if (!is.null(census_map) && nrow(census_map) > 0) {
      matched$id_sub_plots <- census_map$id_sub_plots[
        match(as.integer(matched$id_liste_plots), as.integer(census_map$id_table_liste_plots))
      ]
    } else {
      matched$id_sub_plots <- NA_integer_
    }

    records <- data.frame(
      id_table_liste_plots = as.integer(matched$id_liste_plots),
      id_data_individuals  = as.integer(matched$id_data_individuals),
      id_sub_plots         = as.integer(matched$id_sub_plots),
      traitid              = as.integer(matched$traitid),
      traitvalue           = if ("traitvalue" %in% names(matched))
        suppressWarnings(as.numeric(matched$traitvalue)) else NA_real_,
      traitvalue_char      = if ("traitvalue_char" %in% names(matched))
        as.character(matched$traitvalue_char) else NA_character_,
      original_plot_name   = as.character(matched$plot_name),
      date_modif_d         = as.integer(format(today, "%d")),
      date_modif_m         = as.integer(format(today, "%m")),
      date_modif_y         = as.integer(format(today, "%Y")),
      stringsAsFactors     = FALSE
    )
    if ("issue" %in% names(matched)) records$issue <- as.character(matched$issue)

    cli::cli_alert_info("Inserting {nrow(records)} measurement{?s}...")
    inserted_ids <- .execute_trait_insert_with_returning(records, actual_con)

    DBI::dbCommit(actual_con)

    msg <- sprintf(
      t("Imported %d recruit(s) and %d measurement(s)."),
      nrow(if (is.null(new_individuals)) data.frame() else new_individuals),
      nrow(inserted_ids)
    )
    if (n_census > 0) {
      msg <- paste0(msg, sprintf(t(" Census %s created for %d plot(s)."),
                                 config$census_number, nrow(census_map)))
    }
    if (n_grouped > 0) {
      msg <- paste0(msg, sprintf(t(" %d stem(s) grouped as multi-stems."), n_grouped))
    }
    if (unresolved > 0) {
      msg <- paste0(msg, sprintf(t(" %d row(s) skipped (no matching individual)."),
                                 unresolved))
    }

    list(
      dry_run = FALSE, success = TRUE,
      n_census_records  = n_census,
      n_recruits        = if (is.null(new_individuals)) 0L else nrow(new_individuals),
      n_stem_grouping   = n_grouped,
      n_measurements    = nrow(inserted_ids),
      n_subplot_records = nrow(inserted_ids),
      n_people_records  = 0L,
      message = msg
    )

  }, error = function(e) {
    tryCatch(DBI::dbRollback(actual_con), error = function(e2) NULL)
    cli::cli_alert_danger("Census import rolled back: {e$message}")
    list(
      dry_run = FALSE, success = FALSE,
      n_census_records = 0L, n_recruits = 0L, n_stem_grouping = 0L,
      n_measurements = 0L, n_subplot_records = 0L, n_people_records = 0L,
      message = sprintf(t("Import failed and was rolled back: %s"), e$message)
    )
  })
}
