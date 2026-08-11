# Taxon revisions carried by a census table
#
# A census import matches stems on plot + tag and writes measurements; it does
# not touch data_individuals.idtax_n. When a field team corrects an
# identification between two censuses — normal, not exceptional — that
# correction used to be reported and then discarded.
#
# Whether a correction should be accepted depends on what evidence stands
# behind the identification it would overwrite, so the decision is surfaced
# rather than taken automatically. The database part and the deciding part are
# kept separate so the deciding part is testable without a connection.


#' Rank a taxonomic level so two determinations can be compared
#'
#' A file that names a genus where the database names a species is almost
#' always a data entry regression rather than a revision, and has to be told
#' apart from a genuine correction. Morphospecies (`Baphia sp1`) carry no
#' `tax_level` but do carry a genus and an epithet, so they rank with species.
#'
#' @param tax_level Character vector from `table_taxa.tax_level`.
#' @param morpho Logical vector from `table_taxa.morpho_species`.
#' @return Integer vector, higher means more precise; `NA` when unknown.
#' @keywords internal
#' @export
.taxon_precision <- function(tax_level, morpho = NULL) {
  ladder <- c(higher = 1L, class = 2L, order = 3L, family = 4L,
              genus = 5L, species = 6L, infraspecific = 7L)

  out <- unname(ladder[tolower(trimws(as.character(tax_level)))])
  if (!is.null(morpho)) {
    is_morpho <- !is.na(morpho) & morpho
    out[is_morpho & is.na(out)] <- ladder[["species"]]
  }
  as.integer(out)
}


#' Fetch names and precision for a set of taxon ids
#'
#' Reads the **taxa** database, which is a separate connection from the one
#' holding the plots. Showing bare `idtax_n` integers in the review step would
#' make it unusable, so this is not optional decoration.
#'
#' @param idtax Integer vector of `idtax_n` values.
#' @param con_taxa Connection or pool for the taxa database.
#' @return Data frame with `idtax_n`, `taxon_name`, `tax_level`, `precision`.
#'   An empty frame if nothing can be fetched — the caller degrades to ids.
#' @keywords internal
#' @export
.fetch_taxon_names <- function(idtax, con_taxa) {

  empty <- data.frame(
    idtax_n = integer(0), taxon_name = character(0),
    tax_level = character(0), precision = integer(0),
    stringsAsFactors = FALSE
  )

  idtax <- unique(suppressWarnings(as.integer(idtax)))
  idtax <- idtax[!is.na(idtax)]
  if (length(idtax) == 0 || is.null(con_taxa)) return(empty)

  tryCatch({
    is_pool <- inherits(con_taxa, "Pool")
    actual <- if (is_pool) pool::poolCheckout(con_taxa) else con_taxa
    on.exit(if (is_pool) pool::poolReturn(actual), add = TRUE)

    res <- DBI::dbGetQuery(actual, glue::glue_sql(
      "SELECT idtax_n, tax_fam, tax_gen, tax_esp, tax_rank01, tax_nam01,
              tax_level, morpho_species
         FROM table_taxa
        WHERE idtax_n IN ({idtax*})",
      idtax = idtax, .con = actual
    ))
    if (nrow(res) == 0) return(empty)

    data.frame(
      idtax_n    = as.integer(res$idtax_n),
      taxon_name = .assemble_taxon_name(res),
      tax_level  = as.character(res$tax_level),
      precision  = .taxon_precision(res$tax_level, res$morpho_species),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    message("Note: could not fetch taxon names (", conditionMessage(e),
            "). The review step will show ids.")
    empty
  })
}


#' Build a readable name from the taxa table's parts
#'
#' @param x Data frame with tax_fam, tax_gen, tax_esp, tax_rank01, tax_nam01.
#' @return Character vector.
#' @keywords internal
#' @export
.assemble_taxon_name <- function(x) {
  blank <- function(v) {
    v <- trimws(as.character(v))
    v[is.na(v) | !nzchar(v)] <- ""
    v
  }
  gen <- blank(x$tax_gen); esp <- blank(x$tax_esp)
  rk  <- blank(x$tax_rank01); nam <- blank(x$tax_nam01)
  fam <- blank(x$tax_fam)

  name <- trimws(paste(gen, esp, rk, nam))
  # Nothing below family: fall back to the family, then to "unidentified",
  # which is what idtax 351190 amounts to
  name[!nzchar(name)] <- fam[!nzchar(name)]
  name[!nzchar(name)] <- "unidentified"
  name
}


#' Fetch the herbarium evidence behind a set of individuals
#'
#' Individuals link to herbarium material through `data_link_specimens`, with
#' `linktype` distinguishing a specimen collected **from this tree**
#' (`type_individual`) from one used as a **comparison** when determining it
#' (`referenced_individual`). The two carry very different weight against a
#' proposed revision, so both are counted.
#'
#' @param id_n Integer vector of individual ids.
#' @param con Connection or pool for the main database.
#' @return Data frame with `id_n`, `n_voucher`, `n_reference`. Empty on failure
#'   — evidence is context, never a reason to block the step.
#' @keywords internal
#' @export
.fetch_specimen_evidence <- function(id_n, con) {

  empty <- data.frame(id_n = integer(0), n_voucher = integer(0),
                      n_reference = integer(0), stringsAsFactors = FALSE)

  id_n <- unique(suppressWarnings(as.integer(id_n)))
  id_n <- id_n[!is.na(id_n)]
  if (length(id_n) == 0 || is.null(con)) return(empty)

  tryCatch({
    is_pool <- inherits(con, "Pool")
    actual <- if (is_pool) pool::poolCheckout(con) else con
    on.exit(if (is_pool) pool::poolReturn(actual), add = TRUE)

    res <- DBI::dbGetQuery(actual, glue::glue_sql(
      "SELECT l.id_n,
              count(*) FILTER (WHERE lt.linktype = 'type_individual')       AS n_voucher,
              count(*) FILTER (WHERE lt.linktype = 'referenced_individual') AS n_reference
         FROM data_link_specimens l
         LEFT JOIN linktypelist lt ON lt.id_linktype = l.id_linktype
        WHERE l.id_n IN ({ids*})
        GROUP BY l.id_n",
      ids = id_n, .con = actual
    ))
    if (nrow(res) == 0) return(empty)

    data.frame(
      id_n        = as.integer(res$id_n),
      n_voucher   = as.integer(res$n_voucher),
      n_reference = as.integer(res$n_reference),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    message("Note: could not fetch specimen links (", conditionMessage(e),
            "). Revisions will be shown without herbarium evidence.")
    empty
  })
}


#' Classify the identification revisions carried by a census table
#'
#' @description
#' Takes the `taxon_drift` a [split_census_table()] found and works out, for
#' each revised stem, what evidence stands behind the identification it would
#' overwrite — and therefore whether accepting it should be the default.
#'
#' @details
#' Four evidence states, in decreasing weight:
#'
#' \describe{
#'   \item{`voucher`}{a specimen was collected **from this tree**. The revision
#'     contradicts physical material, so the default is to keep the database
#'     value and make the user override it deliberately.}
#'   \item{`collected_this_census`}{the uploaded row carries a herbarium number
#'     but no link exists yet — the specimen is in a press, not in the
#'     database. Detectable only from the file, which is why this belongs in
#'     the wizard and not in a later taxonomic pass.}
#'   \item{`reference`}{the determination rested on comparison with another
#'     specimen. Revising it is ordinary.}
#'   \item{`field_only`}{no herbarium material at all.}
#' }
#'
#' Two cases override the evidence entirely, because they are not really
#' revisions:
#'
#' \itemize{
#'   \item a stem recorded as unidentified that now has a determination is pure
#'     gain — accepted by default whatever the evidence, and reported as
#'     `identification_gained`;
#'   \item a file naming a *less* precise taxon than the database — a genus
#'     where there was a species — is almost always a data entry regression.
#'     It defaults to keeping the database value even with no evidence at all,
#'     and is reported as `precision_lost`.
#' }
#'
#' @param drift The `taxon_drift` frame from a `census_split`.
#' @param data The classified census table (`split$data`), used to find a
#'   herbarium number recorded against a revised stem in this census.
#' @param evidence Output of [.fetch_specimen_evidence()], or `NULL`.
#' @param taxa Output of [.fetch_taxon_names()], or `NULL`.
#' @param unidentified_idtax Taxon id standing for "not identified".
#' @return Data frame, one row per revision, with `row_id`, `id_n`,
#'   `plot_name`, `tag`, `idtax_db`, `idtax_file`, the resolved names and
#'   precisions, `n_voucher`, `n_reference`, `herbarium_nbe_char`, `evidence`,
#'   `category` and `decision` (`"keep_db"` or `"accept_file"`).
#' @seealso [split_census_table()], which produces the drift this consumes.
#' @export
.classify_taxon_revisions <- function(drift,
                                      data = NULL,
                                      evidence = NULL,
                                      taxa = NULL,
                                      unidentified_idtax = 351190L) {

  empty <- data.frame(
    row_id = integer(0), id_n = integer(0), plot_name = character(0),
    tag = character(0), idtax_db = integer(0), idtax_file = integer(0),
    name_db = character(0), name_file = character(0),
    precision_db = integer(0), precision_file = integer(0),
    n_voucher = integer(0), n_reference = integer(0),
    herbarium_nbe_char = character(0), evidence = character(0),
    category = character(0), decision = character(0),
    stringsAsFactors = FALSE
  )
  if (is.null(drift) || nrow(drift) == 0) return(empty)

  out <- data.frame(
    row_id     = as.integer(drift$row_id),
    plot_name  = as.character(drift$plot_name),
    tag        = as.character(drift$tag),
    idtax_db   = suppressWarnings(as.integer(drift$idtax_db)),
    idtax_file = suppressWarnings(as.integer(drift$idtax_file)),
    stringsAsFactors = FALSE
  )

  # id_n and any herbarium number come from the classified table, matched on
  # row_id — the one key that survives every reshaping of the census table
  out$id_n <- NA_integer_
  out$herbarium_nbe_char <- NA_character_
  if (!is.null(data) && "row_id" %in% names(data)) {
    hit <- match(out$row_id, data$row_id)
    if ("id_n" %in% names(data)) {
      out$id_n <- suppressWarnings(as.integer(data$id_n[hit]))
    }
    if ("herbarium_nbe_char" %in% names(data)) {
      v <- trimws(as.character(data$herbarium_nbe_char[hit]))
      v[is.na(v) | !nzchar(v)] <- NA_character_
      out$herbarium_nbe_char <- v
    }
  }

  # ---- names and precision ------------------------------------------------
  look <- function(ids, field, default) {
    if (is.null(taxa) || nrow(taxa) == 0) return(rep(default, length(ids)))
    taxa[[field]][match(ids, taxa$idtax_n)]
  }
  out$name_db        <- look(out$idtax_db,   "taxon_name", NA_character_)
  out$name_file      <- look(out$idtax_file, "taxon_name", NA_character_)
  out$precision_db   <- as.integer(look(out$idtax_db,   "precision", NA_integer_))
  out$precision_file <- as.integer(look(out$idtax_file, "precision", NA_integer_))

  # Fall back to the id so a failed taxa lookup degrades rather than blanks
  out$name_db[is.na(out$name_db)]     <- paste0("idtax ", out$idtax_db[is.na(out$name_db)])
  out$name_file[is.na(out$name_file)] <- paste0("idtax ", out$idtax_file[is.na(out$name_file)])

  # ---- herbarium evidence -------------------------------------------------
  out$n_voucher   <- 0L
  out$n_reference <- 0L
  if (!is.null(evidence) && nrow(evidence) > 0) {
    hit <- match(out$id_n, evidence$id_n)
    out$n_voucher[!is.na(hit)]   <- as.integer(evidence$n_voucher[hit[!is.na(hit)]])
    out$n_reference[!is.na(hit)] <- as.integer(evidence$n_reference[hit[!is.na(hit)]])
  }

  out$evidence <- ifelse(
    out$n_voucher > 0, "voucher",
    ifelse(!is.na(out$herbarium_nbe_char), "collected_this_census",
           ifelse(out$n_reference > 0, "reference", "field_only"))
  )

  # ---- category, which can override the evidence --------------------------
  gained <- !is.na(out$idtax_db) & out$idtax_db == unidentified_idtax &
    !is.na(out$idtax_file) & out$idtax_file != unidentified_idtax
  lost <- !is.na(out$precision_db) & !is.na(out$precision_file) &
    out$precision_file < out$precision_db & !gained

  out$category <- ifelse(gained, "identification_gained",
                         ifelse(lost, "precision_lost", "revision"))

  # ---- default decision ---------------------------------------------------
  # Accept unless something argues against it: a specimen of this very tree,
  # or a determination that has gone backwards.
  out$decision <- ifelse(
    gained, "accept_file",
    ifelse(lost | out$evidence == "voucher", "keep_db", "accept_file")
  )

  out[, names(empty), drop = FALSE]
}


#' Apply accepted identification revisions
#'
#' Writes the revisions the user accepted, one `UPDATE` per stem plus one
#' audit row each in `followup_updates_individuals`.
#'
#' @details
#' The audit row follows that table's snapshot convention — a copy of the
#' individual's identifying fields — with `modif_type = 'idtax_n'`, the value
#' already in use there. `idtax_n` holds the determination being replaced and
#' `idtax_n_new` the one replacing it; both columns come from
#' [migrate_followup_idtax()]. Without them the trail records that a
#' determination moved but not where to, which is the state its existing 4,440
#' identification rows are in — so this refuses to run rather than adding to
#' the pile.
#'
#' `original_tax_name` is deliberately untouched: it exists to preserve what
#' was written in the field the first time.
#'
#' Runs inside the caller's open transaction — it does not begin one.
#'
#' @param revisions Frame from [.classify_taxon_revisions()], already filtered
#'   to `decision == "accept_file"`, or `NULL`.
#' @param con Database connection inside the open transaction.
#' @return Number of individuals updated.
#' @export
.apply_taxon_revisions <- function(revisions, con) {

  if (is.null(revisions) || nrow(revisions) == 0) return(0L)

  revisions <- revisions[!is.na(revisions$id_n) &
                           !is.na(revisions$idtax_file), , drop = FALSE]
  if (nrow(revisions) == 0) return(0L)

  if (!"idtax_n_new" %in% DBI::dbListFields(con, "followup_updates_individuals")) {
    stop("followup_updates_individuals cannot record which taxa a revision moved between. Run migrate_followup_idtax() first.",
         call. = FALSE)
  }

  today <- Sys.Date()
  n <- 0L

  for (i in seq_len(nrow(revisions))) {
    id_n <- as.integer(revisions$id_n[i])
    to   <- as.integer(revisions$idtax_file[i])

    # Read the row back rather than trusting the uploaded copy: the snapshot
    # has to describe what is in the database, and the taxon being replaced
    # has to be the one actually being overwritten now
    before <- DBI::dbGetQuery(con, glue::glue_sql(
      "SELECT idtax_n, tag, sous_plot_name, id_table_liste_plots_n,
              herbarium_nbe_char, multi_tiges_id
         FROM data_individuals WHERE id_n = {id_n}",
      id_n = id_n, .con = con
    ))
    if (nrow(before) != 1) next

    from <- suppressWarnings(as.integer(before$idtax_n[1]))
    if (!is.na(from) && identical(from, to)) next

    DBI::dbExecute(con, glue::glue_sql(
      "UPDATE data_individuals SET idtax_n = {to} WHERE id_n = {id_n}",
      to = to, id_n = id_n, .con = con
    ))

    DBI::dbExecute(con, glue::glue_sql(
      "INSERT INTO followup_updates_individuals
         (id_n, idtax_n, idtax_n_new, tag, sous_plot_name,
          id_table_liste_plots_n, herbarium_nbe_char, multi_tiges_id,
          modif_type, date_modified, data_modif_y, data_modif_m, data_modif_d)
       VALUES ({id_n}, {from}, {to}, {tag}, {sub}, {plot_id}, {herb}, {multi},
               'idtax_n', {stamp}, {yy}, {mm}, {dd})",
      id_n = id_n, from = from, to = to,
      tag     = before$tag[1],
      sub     = before$sous_plot_name[1],
      plot_id = before$id_table_liste_plots_n[1],
      herb    = before$herbarium_nbe_char[1],
      multi   = before$multi_tiges_id[1],
      stamp   = format(today, "%Y-%m-%d"),
      yy = as.integer(format(today, "%Y")),
      mm = as.integer(format(today, "%m")),
      dd = as.integer(format(today, "%d")),
      .con = con
    ))
    n <- n + 1L
  }

  n
}


#' Collect the identification revisions a census import should ask about
#'
#' Thin wrapper tying the two database reads to the pure classifier, so a
#' caller with connections gets the finished table in one call.
#'
#' @param split A `census_split` from [split_census_table()].
#' @param con Main database connection or pool.
#' @param con_taxa Taxa database connection or pool.
#' @param unidentified_idtax Taxon id standing for "not identified".
#' @return The frame described in [.classify_taxon_revisions()].
#' @export
collect_taxon_revisions <- function(split, con = NULL, con_taxa = NULL,
                                    unidentified_idtax = 351190L) {

  if (!inherits(split, "census_split")) {
    stop("`split` must be a census_split from split_census_table().", call. = FALSE)
  }
  drift <- split$taxon_drift
  if (is.null(drift) || nrow(drift) == 0) {
    return(.classify_taxon_revisions(NULL))
  }

  hit <- match(drift$row_id, split$data$row_id)
  id_n <- suppressWarnings(as.integer(split$data$id_n[hit]))

  .classify_taxon_revisions(
    drift    = drift,
    data     = split$data,
    evidence = .fetch_specimen_evidence(id_n, con),
    taxa     = .fetch_taxon_names(c(drift$idtax_db, drift$idtax_file), con_taxa),
    unidentified_idtax = unidentified_idtax
  )
}
