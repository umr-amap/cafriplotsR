# =============================================================================
# Taxonomic backbones - matching internal taxa and saving links
#
# match_taxa_to_backbone() proposes links; review_backbone_matches()
# (R/backbone_review.R) lets a person accept or reject the uncertain ones;
# save_backbone_links() adds links; replace_backbone_links() rebuilds a
# backbone's links after comparing them with the current ones.
#
# Which link supplies names (is_preferred) follows one rule, written twice:
# in SQL (.preferred_links_sql, for save_backbone_links, which merges with
# links already stored) and in R (.choose_preferred_links, for
# replace_backbone_links, which writes every link of a taxon at once):
#   - a taxon with verified links: its verified link, if it has only one;
#   - otherwise: its only link, if that link is an exact or manual match.
# Fuzzy and author-mismatch links therefore supply names only once verified.
# =============================================================================


# ---- Authors -----------------------------------------------------------------------

#' Does an author string mark a misapplied name?
#' @noRd
.is_auct <- function(x) {
  !is.na(x) & grepl("(^|\\s)auct\\.", x)
}


#' Author string reduced to what identifies a name's author
#'
#' Drops basionym authors in brackets and what precedes "ex" (the author who
#' published the name is the one after it), then spaces and dots, and lowers
#' the case: "(Bojer ex Hook.) D.Dietr." and "D.Dietr." both give "ddietr".
#' @noRd
.normalise_authors <- function(x) {
  x <- gsub("\\([^)]*\\)", " ", x)
  x <- sub("^.*\\bex\\s+", "", x, perl = TRUE)
  x <- tolower(gsub("[[:space:].]", "", x))
  x[!is.na(x) & !nzchar(x)] <- NA_character_
  x
}


#' Is an internal taxon a misapplied name?
#'
#' Internal misapplications carry `auct.` in an author column
#' (`"ZZ auct."`). They are not matched to any backbone name.
#' @noRd
.is_misapplied_taxon <- function(author1, author2, author3) {
  .is_auct(author1) | .is_auct(author2) | .is_auct(author3)
}


#' Agreement between two author strings
#'
#' Authors are compared once normalised (see `.normalise_authors()`), so a
#' basionym author in brackets, an "ex" author or spacing present on one side
#' only makes no difference. `"fuzzy"`: Jaro-Winkler similarity. `"exact"`: 1
#' when identical, 0 otherwise. `NA` when either side is missing.
#'
#' `auct.` on one side only is a conflict (0), even when the other side is
#' missing: a misapplication and the name in the sense of its author are
#' different taxa. `auct.` on both sides is agreement (1).
#'
#' @param a,b Character vectors of the same length.
#' @param method `"fuzzy"` or `"exact"`.
#' @return Numeric vector.
#' @noRd
.author_score <- function(a, b, method = c("fuzzy", "exact")) {
  method <- match.arg(method)
  blank_to_na <- function(x) {
    x <- as.character(x)
    x[!is.na(x) & !nzchar(trimws(x))] <- NA_character_
    x
  }
  a <- blank_to_na(a)
  b <- blank_to_na(b)
  na <- .normalise_authors(a)
  nb <- .normalise_authors(b)

  score <- rep(NA_real_, length(a))
  both <- !is.na(na) & !is.na(nb)
  if (any(both)) {
    score[both] <- if (method == "fuzzy") {
      stringdist::stringsim(na[both], nb[both], method = "jw")
    } else {
      as.numeric(na[both] == nb[both])
    }
  }

  auct_a <- .is_auct(a)
  auct_b <- .is_auct(b)
  score[auct_a & auct_b] <- 1
  score[xor(auct_a, auct_b)] <- 0
  score
}


# ---- Matching ----------------------------------------------------------------------

#' Exact name matches, settled by authors
#'
#' Every backbone name identical to an input name is a candidate. Without
#' authors, all candidates are kept (several means homonyms, to review). With
#' authors, per input name:
#' - candidates whose author score reaches the threshold: the best ones;
#' - otherwise, candidates whose authors cannot be compared (missing on a
#'   side): all of them;
#' - otherwise every candidate conflicts on authors: all kept as
#'   `"author_mismatch"`, for review.
#'
#' Then, when several exact candidates remain and exactly one has the
#' canonical status `"accepted"`, only that one is kept. When none is
#' accepted, exactly one is a synonym and all the others are illegitimate or
#' invalid (`status_raw` starting with "illeg" or "invalid"), only the synonym
#' is kept.
#'
#' @param names Data frame with `.match_id`, `name` and, when authors are used,
#'   `author`.
#' @param backbone_names Data frame with `external_id`, `taxon_name`,
#'   `authors`, `status_raw` and optionally `status` (canonical).
#' @param author_match `"none"`, `"exact"` or `"fuzzy"`.
#' @param author_threshold Minimum fuzzy author score.
#' @return Data frame with `.match_id`, `external_id`, `backbone_taxon_name`,
#'   `backbone_authors`, `backbone_status`, `match_type`, `match_score`,
#'   `author_score`.
#' @noRd
.match_exact_names <- function(names, backbone_names, author_match = "none",
                               author_threshold = 0.6) {
  empty <- data.frame(
    .match_id = integer(0), external_id = character(0),
    backbone_taxon_name = character(0), backbone_authors = character(0),
    backbone_status = character(0), match_type = character(0),
    match_score = numeric(0), author_score = numeric(0),
    stringsAsFactors = FALSE
  )

  cols <- intersect(c("external_id", "taxon_name", "authors", "status_raw", "status"),
                    names(backbone_names))
  cand <- merge(names, backbone_names[, cols, drop = FALSE],
                by.x = "name", by.y = "taxon_name")
  if (nrow(cand) == 0) return(empty)

  cand$match_type <- "exact"
  cand$author_score <- NA_real_

  if (author_match != "none" && "author" %in% names(cand)) {
    cand$author_score <- .author_score(cand$author, cand$authors, method = author_match)
    threshold <- if (author_match == "exact") 1 else author_threshold

    s <- cand$author_score
    group <- cand$.match_id
    good <- !is.na(s) & s >= threshold
    unknown <- is.na(s)
    n_good <- stats::ave(as.integer(good), group, FUN = sum)
    n_unknown <- stats::ave(as.integer(unknown), group, FUN = sum)
    best <- stats::ave(ifelse(good, s, -Inf), group, FUN = max)

    keep_best <- n_good > 0 & good & s == best
    keep_best[is.na(keep_best)] <- FALSE
    keep_unknown <- n_good == 0 & unknown
    mismatch <- n_good == 0 & n_unknown == 0

    cand$match_type[mismatch] <- "author_mismatch"
    cand <- cand[keep_best | keep_unknown | mismatch, , drop = FALSE]
  }

  # Several identical names still standing: when exactly one is accepted, the
  # others (illegitimate, invalid, later homonyms...) are dropped
  if ("status" %in% names(cand) && nrow(cand) > 0) {
    exact_row <- cand$match_type == "exact"
    group <- cand$.match_id
    is_accepted <- exact_row & cand$status %in% "accepted"
    n_exact <- stats::ave(as.integer(exact_row), group, FUN = sum)
    n_accepted <- stats::ave(as.integer(is_accepted), group, FUN = sum)
    cand <- cand[!(exact_row & n_exact > 1 & n_accepted == 1 & !is_accepted), , drop = FALSE]

    # No accepted name, but exactly one synonym and only illegitimate or
    # invalid names beside it: the synonym, which leads to an accepted name
    exact_row <- cand$match_type == "exact"
    group <- cand$.match_id
    is_accepted <- exact_row & cand$status %in% "accepted"
    is_synonym <- exact_row & cand$status %in% "synonym"
    is_unusable <- exact_row & !is_accepted & !is_synonym &
      grepl("^(illeg|invalid)", cand$status_raw, ignore.case = TRUE)
    n_exact <- stats::ave(as.integer(exact_row), group, FUN = sum)
    n_accepted <- stats::ave(as.integer(is_accepted), group, FUN = sum)
    n_synonym <- stats::ave(as.integer(is_synonym), group, FUN = sum)
    n_unusable <- stats::ave(as.integer(is_unusable), group, FUN = sum)
    only_one_synonym <- n_exact > 1 & n_accepted == 0 & n_synonym == 1 &
      n_synonym + n_unusable == n_exact
    cand <- cand[!(exact_row & only_one_synonym & !is_synonym), , drop = FALSE]
  }

  data.frame(
    .match_id           = cand$.match_id,
    external_id         = as.character(cand$external_id),
    backbone_taxon_name = cand$name,
    backbone_authors    = cand$authors,
    backbone_status     = cand$status_raw,
    match_type          = cand$match_type,
    match_score         = 1,
    author_score        = cand$author_score,
    stringsAsFactors    = FALSE
  )
}


#' Match internal taxa to a backbone's names
#'
#' @description
#' Matches taxa from the internal \code{table_taxa} to the names of a backbone
#' already imported into the taxa database. Returns a tibble for review;
#' nothing is written. Review the uncertain rows with
#' [review_backbone_matches()], then save with [save_backbone_links()] or
#' rebuild all links with [replace_backbone_links()].
#'
#' Each row is a candidate link, of one of three types:
#' \itemize{
#'   \item \code{"exact"}: identical name, and authors agree or cannot be
#'     compared. A taxon with a single exact candidate gets it as its preferred
#'     link when saved. When several remain and exactly one is an accepted
#'     name, only the accepted one is returned; when none is accepted and
#'     exactly one is a synonym beside illegitimate or invalid names, only the
#'     synonym. Otherwise the candidates (homonyms) wait for review.
#'   \item \code{"author_mismatch"}: identical name, but every candidate's
#'     authors disagree with the taxon's (only with \code{author_match} other
#'     than \code{"none"}). Often a homonym or a misapplication, sometimes the
#'     same author written differently. Never preferred until verified, and not
#'     passed to fuzzy matching.
#'   \item \code{"fuzzy"}: a close name, for taxa without any identical name.
#'     Never preferred until verified.
#' }
#'
#' Matching with authors compares each taxon's own authors: two internal taxa
#' with the same name and different authors can be matched to different
#' backbone names. Authors are compared without basionym authors in brackets,
#' without what precedes "ex", and ignoring spaces and dots, so "(Klatt)
#' B.L.Rob." agrees with "B.L.Rob.". A backbone name marked \code{auct.} (a
#' misapplication) never matches.
#'
#' Internal taxa marked \code{auct.} in an author column (\code{"ZZ auct."})
#' are misapplications and are not matched at all; with
#' [replace_backbone_links()] and \code{taxa = "all"} they lose any link they
#' had.
#'
#' @param backbone Character. Backbone code, e.g. \code{"wcvp"}. A backbone not
#'   yet offered to users can be matched.
#' @param con_taxa Connection to the taxa database. If \code{NULL}, calls
#'   \code{call.mydb.taxa()}.
#' @param tax_ids Optional integer vector of \code{idtax_n} to match. If
#'   \code{NULL}, matches all taxa except morphospecies, mosses, lichens and
#'   fungi.
#' @param methods Character vector of matching methods. Default
#'   \code{c("exact", "fuzzy")}; fuzzy matching only runs on names without an
#'   identical backbone name.
#' @param fuzzy_threshold Numeric (0-1). Minimum name similarity for fuzzy
#'   matches. Default 0.9.
#' @param author_match Character. How authors settle exact matches:
#'   \code{"none"} (default) ignores them, \code{"exact"} requires identical
#'   strings, \code{"fuzzy"} compares them by Jaro-Winkler similarity. Authors
#'   are taken from \code{author1}/\code{author2}/\code{author3} of
#'   \code{table_taxa}, for the deepest rank present.
#' @param author_threshold Numeric (0-1). Minimum author similarity when
#'   \code{author_match = "fuzzy"}. Default 0.6.
#' @param n_cores Integer. Parallel workers for fuzzy matching. Default 1.
#' @param verbose Logical. Show progress. Default \code{TRUE}.
#'
#' @return A tibble with columns \code{idtax_n}, \code{taxon_name_internal},
#'   \code{authors_internal}, \code{external_id}, \code{backbone_taxon_name},
#'   \code{backbone_authors}, \code{backbone_status}, \code{match_type},
#'   \code{match_score} (name similarity) and \code{author_score} (author
#'   similarity, \code{NA} when it cannot be computed).
#'
#' @examples
#' \dontrun{
#' con_taxa <- call.mydb.taxa()
#' matches <- match_taxa_to_backbone("apd", con_taxa, author_match = "fuzzy")
#' matches <- review_backbone_matches(matches, review_file = "apd_review.rds")
#' replace_backbone_links(matches, "apd", con_taxa)
#' }
#'
#' @export
match_taxa_to_backbone <- function(backbone,
                                   con_taxa = NULL,
                                   tax_ids = NULL,
                                   methods = c("exact", "fuzzy"),
                                   fuzzy_threshold = 0.9,
                                   author_match = c("none", "exact", "fuzzy"),
                                   author_threshold = 0.6,
                                   n_cores = 1L,
                                   verbose = TRUE) {

  author_match <- match.arg(author_match)
  methods <- match.arg(methods, c("exact", "fuzzy"), several.ok = TRUE)
  if (is.null(con_taxa)) con_taxa <- call.mydb.taxa()

  info <- .backbone_info(backbone, con_taxa)
  view <- .quote_backbone_view(info$names_view)

  actual_con <- if (inherits(con_taxa, "Pool")) {
    pool::poolCheckout(con_taxa)
  } else {
    con_taxa
  }
  on.exit({
    if (inherits(con_taxa, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  empty <- dplyr::tibble(
    idtax_n = integer(), taxon_name_internal = character(),
    authors_internal = character(), external_id = character(),
    backbone_taxon_name = character(), backbone_authors = character(),
    backbone_status = character(), match_type = character(),
    match_score = numeric(), author_score = numeric()
  )

  if (verbose) cli::cli_alert_info("Fetching internal taxa...")

  taxa_query <- dplyr::tbl(actual_con, "table_taxa") %>%
    dplyr::filter(morpho_species == "false",
                  !grepl("Musci-", tax_fam),
                  !grepl("Lichenes", tax_fam),
                  tax_fam != "Fungi")

  if (!is.null(tax_ids)) {
    taxa_query <- taxa_query %>% dplyr::filter(idtax_n %in% !!tax_ids)
  }

  internal_taxa <- taxa_query %>%
    dplyr::select(idtax_n, tax_gen, tax_esp, tax_rank01, tax_nam01,
                  tax_rank02, tax_nam02, author1, author2, author3) %>%
    dplyr::collect()

  if (nrow(internal_taxa) == 0) {
    cli::cli_alert_warning("No taxa to match")
    return(empty)
  }

  filled <- function(x) !is.na(x) & nzchar(x)

  internal_taxa <- internal_taxa %>%
    dplyr::mutate(
      taxon_name_internal = dplyr::case_when(
        filled(tax_nam02) & filled(tax_rank02) ~
          paste(tax_gen, tax_esp, tax_rank01, tax_nam01, tax_rank02, tax_nam02),
        filled(tax_nam01) & filled(tax_rank01) ~
          paste(tax_gen, tax_esp, tax_rank01, tax_nam01),
        filled(tax_esp) ~ paste(tax_gen, tax_esp),
        TRUE ~ tax_gen
      ),
      # authors of the deepest rank present
      authors_internal = dplyr::case_when(
        filled(tax_nam02) & filled(author3) ~ author3,
        filled(tax_nam02) & filled(author2) ~ author2,
        filled(tax_nam01) & filled(author2) ~ author2,
        filled(tax_esp)   & filled(author1) ~ author1,
        # genus and above keep their author in author1 too
        !filled(tax_esp) & !filled(tax_nam01) & filled(author1) ~ author1,
        TRUE ~ NA_character_
      )
    )

  misapplied <- .is_misapplied_taxon(internal_taxa$author1, internal_taxa$author2,
                                     internal_taxa$author3)
  if (any(misapplied) && verbose) {
    cli::cli_alert_info("Left out, misapplied names (auct.): {sum(misapplied)} taxa")
  }
  internal_taxa <- internal_taxa[!misapplied, , drop = FALSE] %>%
    dplyr::select(idtax_n, taxon_name_internal, authors_internal)

  if (verbose) cli::cli_alert_info("Fetching {info$name} names from the database...")

  backbone_names <- DBI::dbGetQuery(actual_con, paste0(
    "SELECT external_id, taxon_name, authors, status, status_raw, rank,
            accepted_external_id, genus
       FROM ", view,
    " WHERE taxon_name IS NOT NULL"
  ))

  if (nrow(backbone_names) == 0) {
    cli::cli_abort("No names in backbone {.val {backbone}}. Import them first.")
  }

  # misapplications: internal ones are not matched, so no taxon can be one
  backbone_names <- backbone_names[!.is_auct(backbone_names$authors), , drop = FALSE]

  if (verbose) cli::cli_alert_info("Matching {nrow(internal_taxa)} taxa against {info$name}...")

  # One matching unit per distinct name, or name and authors: two taxa with the
  # same name and different authors are matched separately
  use_authors <- author_match != "none"
  keys <- if (use_authors) c("taxon_name_internal", "authors_internal") else "taxon_name_internal"
  unique_names <- internal_taxa %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::mutate(.match_id = dplyr::row_number())
  internal_taxa <- internal_taxa %>%
    dplyr::left_join(unique_names, by = keys)

  all_matches <- empty
  exact <- .match_exact_names(data.frame(.match_id = integer(0), name = character(0)),
                              backbone_names)

  if ("exact" %in% methods) {
    if (verbose) {
      author_note <- switch(author_match,
        exact = " (exact authors)",
        fuzzy = glue::glue(" (fuzzy authors, threshold {author_threshold})"),
        ""
      )
      cli::cli_alert_info("Exact name matching on {nrow(unique_names)} unique names{author_note}...")
    }

    names_df <- data.frame(
      .match_id = unique_names$.match_id,
      name      = unique_names$taxon_name_internal,
      stringsAsFactors = FALSE
    )
    if (use_authors) names_df$author <- unique_names$authors_internal

    exact <- .match_exact_names(names_df, backbone_names, author_match, author_threshold)

    matched <- internal_taxa %>%
      dplyr::inner_join(exact, by = ".match_id", relationship = "many-to-many") %>%
      dplyr::select(-.match_id)
    all_matches <- dplyr::bind_rows(all_matches, matched)
  }

  if ("fuzzy" %in% methods) {
    # names with an identical backbone name are settled above, even when their
    # authors disagree: a close name is no better evidence than the same name
    unmatched_taxa <- internal_taxa %>%
      dplyr::filter(!.match_id %in% exact$.match_id)
    unmatched_names <- unique(unmatched_taxa$taxon_name_internal)
    unmatched_names <- unmatched_names[!is.na(unmatched_names)]

    if (length(unmatched_names) > 0) {
      if (verbose) cli::cli_alert_info("Fuzzy matching on {length(unmatched_names)} unique names without an identical name...")

      # the fuzzy helper reads WCVP column names
      candidates <- data.frame(
        plant_name_id          = backbone_names$external_id,
        taxon_name             = backbone_names$taxon_name,
        taxon_authors          = backbone_names$authors,
        taxon_rank             = backbone_names$rank,
        taxon_status           = backbone_names$status_raw,
        accepted_plant_name_id = backbone_names$accepted_external_id,
        genus                  = backbone_names$genus,
        ipni_id                = NA_character_,
        stringsAsFactors       = FALSE
      )

      # A failure stops the run: matches without their fuzzy part would make
      # replace_backbone_links() drop every fuzzy link
      fuzzy_result <- tryCatch(
        .wcvp_match_fuzzy_fast(
          names_df        = data.frame(name = unmatched_names, stringsAsFactors = FALSE),
          wcvp_names      = candidates,
          name_col        = "name",
          fuzzy_threshold = fuzzy_threshold,
          n_cores         = n_cores,
          verbose         = verbose
        ),
        error = function(e) {
          msg <- conditionMessage(e)
          cli::cli_abort(c(
            "Fuzzy matching failed: {msg}",
            "i" = "Retry with fewer {.arg n_cores}, or {.code methods = \"exact\"} to skip fuzzy matching knowingly."
          ))
        }
      )

      if (!is.null(fuzzy_result) && nrow(fuzzy_result) > 0) {
        fuzzy_unique <- fuzzy_result %>%
          dplyr::filter(!is.na(wcvp_id), match_similarity >= fuzzy_threshold) %>%
          dplyr::transmute(
            taxon_name_internal = name,
            external_id         = as.character(wcvp_id),
            backbone_taxon_name = wcvp_name,
            backbone_authors    = wcvp_authors,
            backbone_status     = wcvp_status,
            match_type          = "fuzzy",
            match_score         = as.numeric(match_similarity)
          ) %>%
          dplyr::distinct(taxon_name_internal, external_id, .keep_all = TRUE)

        fuzzy_matched <- unmatched_taxa %>%
          dplyr::select(-.match_id) %>%
          dplyr::inner_join(fuzzy_unique, by = "taxon_name_internal",
                            relationship = "many-to-many") %>%
          dplyr::mutate(author_score = .author_score(authors_internal, backbone_authors))

        all_matches <- dplyr::bind_rows(all_matches, fuzzy_matched)
      }
    }
  }

  all_matches <- all_matches %>%
    dplyr::select(dplyr::all_of(names(empty))) %>%
    dplyr::arrange(idtax_n, external_id)

  if (verbose) .report_matches(all_matches, nrow(internal_taxa))

  all_matches
}


#' Print what a matching run found, per taxon
#' @noRd
.report_matches <- function(matches, n_taxa) {
  n_cand <- table(matches$idtax_n)
  first_type <- matches$match_type[!duplicated(matches$idtax_n)]
  ids <- matches$idtax_n[!duplicated(matches$idtax_n)]
  several <- as.integer(n_cand[as.character(ids)]) > 1

  n_exact_one <- sum(first_type == "exact" & !several)
  n_exact_several <- sum(first_type == "exact" & several)
  n_mismatch <- sum(first_type == "author_mismatch")
  n_fuzzy <- sum(first_type == "fuzzy")
  n_matched <- length(ids)

  cli::cli_h2("Matches")
  cli::cli_alert_success("Exact, one candidate: {n_exact_one} taxa")
  cli::cli_alert_info("Exact, several candidates: {n_exact_several} taxa (to review)")
  cli::cli_alert_info("Same name, authors differ: {n_mismatch} taxa (to review)")
  cli::cli_alert_info("Fuzzy: {n_fuzzy} taxa (to review)")
  cli::cli_alert_info("Unmatched: {n_taxa - n_matched} of {n_taxa} taxa")
  invisible(NULL)
}


# ---- Links: shared ------------------------------------------------------------------

#' Link rows ready to write, from matches
#'
#' Drops rows whose `decision` is `"rejected"`. `verified` is taken from the
#' matches when present. A pair given twice keeps its verified row.
#' @noRd
.prepare_link_data <- function(matches) {
  if ("decision" %in% names(matches)) {
    matches <- matches[is.na(matches$decision) | matches$decision != "rejected", , drop = FALSE]
  }

  missing_cols <- setdiff(c("idtax_n", "external_id", "match_type"), names(matches))
  if (length(missing_cols) > 0) {
    cli::cli_abort("{.arg matches} lacks column{?s} {.field {missing_cols}}.")
  }

  n <- nrow(matches)
  verified <- if ("verified" %in% names(matches)) as.logical(matches$verified) else rep(FALSE, n)
  verified[is.na(verified)] <- FALSE

  link_data <- data.frame(
    idtax_n     = as.integer(matches$idtax_n),
    external_id = as.character(matches$external_id),
    match_type  = as.character(matches$match_type),
    match_score = if ("match_score" %in% names(matches)) round(as.numeric(matches$match_score), 3) else rep(NA_real_, n),
    verified    = verified,
    matched_by  = rep(unname(Sys.info()["user"]), n),
    stringsAsFactors = FALSE
  )
  if (anyNA(link_data$idtax_n) || anyNA(link_data$external_id) || anyNA(link_data$match_type)) {
    cli::cli_abort("{.field idtax_n}, {.field external_id} and {.field match_type} must not be missing.")
  }
  too_long <- unique(link_data$match_type[nchar(link_data$match_type) > 20])
  if (length(too_long) > 0) {
    cli::cli_abort("{.field match_type} longer than 20 characters: {.val {too_long}}.")
  }

  link_data <- link_data[order(!link_data$verified), , drop = FALSE]
  link_data <- link_data[!duplicated(link_data[, c("idtax_n", "external_id")]), , drop = FALSE]
  rownames(link_data) <- NULL
  link_data
}


#' Which links supply names (R version of the rule)
#'
#' @param idtax_n,match_type,verified Vectors, one element per link; all links
#'   of each taxon must be present.
#' @return Logical vector.
#' @noRd
.choose_preferred_links <- function(idtax_n, match_type, verified) {
  if (length(idtax_n) == 0) return(logical(0))
  n_links <- stats::ave(rep(1L, length(idtax_n)), idtax_n, FUN = length)
  n_verified <- stats::ave(as.integer(verified), idtax_n, FUN = sum)
  ifelse(n_verified > 0,
         verified & n_verified == 1,
         match_type %in% c("exact", "manual") & n_links == 1)
}


#' Which links supply names (SQL version of the rule)
#'
#' Marks links preferred among the taxa `$2` of backbone `$1`, for taxa that
#' have no preferred link yet.
#' @noRd
.preferred_links_sql <- "
  UPDATE taxa_backbone_link t
     SET is_preferred = true
   WHERE t.id_backbone = $1
     AND t.idtax_n = ANY($2::int[])
     AND NOT t.is_preferred
     AND NOT EXISTS (SELECT 1 FROM taxa_backbone_link p
                      WHERE p.id_backbone = t.id_backbone
                        AND p.idtax_n = t.idtax_n
                        AND p.is_preferred)
     AND CASE
           WHEN EXISTS (SELECT 1 FROM taxa_backbone_link v
                         WHERE v.id_backbone = t.id_backbone
                           AND v.idtax_n = t.idtax_n
                           AND v.verified)
           THEN t.verified
                AND (SELECT count(*) FROM taxa_backbone_link v
                      WHERE v.id_backbone = t.id_backbone
                        AND v.idtax_n = t.idtax_n
                        AND v.verified) = 1
           ELSE t.match_type IN ('exact', 'manual')
                AND (SELECT count(*) FROM taxa_backbone_link u
                      WHERE u.id_backbone = t.id_backbone
                        AND u.idtax_n = t.idtax_n) = 1
         END"


# ---- Links: save -----------------------------------------------------------------------

#' Save links between internal taxa and a backbone
#'
#' @description
#' Writes matches to \code{taxa_backbone_link}. An existing link (same taxon,
#' backbone and external ID) has its match details updated; it stays verified
#' if it was.
#'
#' Rows whose \code{decision} is \code{"rejected"} (see
#' [review_backbone_matches()]) are not saved.
#'
#' A taxon without a preferred link gets one when a single link can supply its
#' names: its only verified link, or, when none is verified, its only link if
#' that link is an exact or manual match. Fuzzy and author-mismatch links
#' supply names only once verified.
#'
#' To rebuild all of a backbone's links after a new matching run, use
#' [replace_backbone_links()], which compares first.
#'
#' @param matches Data frame with \code{idtax_n}, \code{external_id},
#'   \code{match_type} and optionally \code{match_score}, \code{verified} and
#'   \code{decision}, e.g. from [match_taxa_to_backbone()] or
#'   [review_backbone_matches()].
#' @param backbone Character. Backbone code.
#' @param con_taxa Connection or pool to the taxa database, with write access.
#' @param replace Logical. If \code{TRUE} (default), the backbone's existing
#'   links of the taxa in \code{matches} are deleted first.
#' @param verbose Logical. Show progress. Default \code{TRUE}.
#'
#' @return Invisible integer: number of links written.
#'
#' @examples
#' \dontrun{
#' matches <- match_taxa_to_backbone("wcvp", con_taxa, tax_ids = c(101, 102))
#' save_backbone_links(matches, "wcvp", con_taxa)
#' }
#'
#' @export
save_backbone_links <- function(matches,
                                backbone,
                                con_taxa = NULL,
                                replace = TRUE,
                                verbose = TRUE) {

  if (nrow(matches) == 0) {
    if (verbose) cli::cli_alert_info("No matches to save")
    return(invisible(0L))
  }

  link_data <- .prepare_link_data(matches)
  if (nrow(link_data) == 0) {
    if (verbose) cli::cli_alert_info("No matches to save")
    return(invisible(0L))
  }

  if (is.null(con_taxa)) con_taxa <- call.mydb.taxa()
  info <- .backbone_info(backbone, con_taxa)

  actual_con <- if (inherits(con_taxa, "Pool")) {
    pool::poolCheckout(con_taxa)
  } else {
    con_taxa
  }
  on.exit({
    if (inherits(con_taxa, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  ids_literal <- .pg_array_literal(unique(link_data$idtax_n))

  DBI::dbBegin(actual_con)
  tryCatch({
    if (replace) {
      DBI::dbExecute(
        actual_con,
        "DELETE FROM taxa_backbone_link WHERE id_backbone = $1 AND idtax_n = ANY($2::int[])",
        params = list(info$id_backbone, ids_literal)
      )
    }

    DBI::dbWriteTable(actual_con, "tmp_backbone_links", link_data,
                      temporary = TRUE, overwrite = TRUE)

    DBI::dbExecute(actual_con, sprintf(
      "INSERT INTO taxa_backbone_link
              (idtax_n, id_backbone, external_id, match_type, match_score,
               matched_by, verified)
       SELECT idtax_n, %d, external_id, match_type, match_score, matched_by,
              verified
         FROM tmp_backbone_links
       ON CONFLICT (idtax_n, id_backbone, external_id) DO UPDATE
          SET match_type  = EXCLUDED.match_type,
              match_score = EXCLUDED.match_score,
              matched_by  = EXCLUDED.matched_by,
              matched_on  = CURRENT_TIMESTAMP,
              verified    = taxa_backbone_link.verified OR EXCLUDED.verified",
      info$id_backbone
    ))

    DBI::dbExecute(actual_con, .preferred_links_sql,
                   params = list(info$id_backbone, ids_literal))

    DBI::dbExecute(actual_con, "DROP TABLE tmp_backbone_links")
    DBI::dbCommit(actual_con)
  }, error = function(e) {
    DBI::dbRollback(actual_con)
    cli::cli_abort("Failed to save {backbone} links: {conditionMessage(e)}")
  })

  if (verbose) {
    n_saved <- nrow(link_data)
    cli::cli_alert_success("Saved {n_saved} link{?s} to {backbone} in taxa_backbone_link")
  }
  invisible(nrow(link_data))
}


# ---- Links: replace ------------------------------------------------------------------------

#' Rebuild a backbone's links from a new matching run
#'
#' @description
#' Compares new matches with the links stored for a backbone, taxon by taxon,
#' then (with \code{dry_run = FALSE}) replaces the stored links in one
#' transaction.
#'
#' Taxa with a verified link are left untouched: their stored links are kept
#' and the new matches for them are ignored.
#'
#' The comparison is on the link that supplies names (see
#' [save_backbone_links()] for the rule):
#' \itemize{
#'   \item \code{unchanged}: same preferred backbone name;
#'   \item \code{changed}: another preferred backbone name
#'     (\code{same_name = TRUE} when only the ID differs);
#'   \item \code{gained}: a preferred name where there was none;
#'   \item \code{lost}: no preferred name any more, e.g. a fuzzy match that
#'     now waits for review;
#'   \item \code{none}: no preferred name before or after.
#' }
#'
#' @param matches Data frame from [match_taxa_to_backbone()], optionally
#'   reviewed with [review_backbone_matches()]. Rows whose \code{decision} is
#'   \code{"rejected"} are dropped.
#' @param backbone Character. Backbone code.
#' @param con_taxa Connection or pool to the taxa database, with write access
#'   when \code{dry_run = FALSE}.
#' @param taxa Character. Which stored links are replaced: \code{"matched"}
#'   (default) only those of the taxa present in \code{matches};
#'   \code{"all"} every link of the backbone, so taxa that no longer match lose
#'   theirs. Use \code{"all"} after matching every taxon.
#' @param dry_run Logical. If \code{TRUE} (default), compare and report only.
#' @param verbose Logical. Show the comparison. Default \code{TRUE}.
#'
#' @return Invisibly, a tibble with one row per taxon: \code{idtax_n},
#'   \code{taxon_name_internal}, \code{old_external_id}, \code{old_name},
#'   \code{new_external_id}, \code{new_name}, \code{old_links},
#'   \code{new_links}, \code{change}, \code{same_name}.
#'
#' @examples
#' \dontrun{
#' matches <- match_taxa_to_backbone("wcvp", con_taxa, author_match = "fuzzy")
#' cmp <- replace_backbone_links(matches, "wcvp", con_taxa, taxa = "all")
#' replace_backbone_links(matches, "wcvp", con_taxa, taxa = "all", dry_run = FALSE)
#' }
#'
#' @export
replace_backbone_links <- function(matches,
                                   backbone,
                                   con_taxa = NULL,
                                   taxa = c("matched", "all"),
                                   dry_run = TRUE,
                                   verbose = TRUE) {

  taxa <- match.arg(taxa)
  if (is.null(con_taxa)) con_taxa <- call.mydb.taxa()
  info <- .backbone_info(backbone, con_taxa)
  view <- .quote_backbone_view(info$names_view)

  link_data <- .prepare_link_data(matches)

  current <- .backbone_query(con_taxa, paste0(
    "SELECT l.idtax_n, l.external_id, l.is_preferred, l.verified,
            v.taxon_name
       FROM taxa_backbone_link l
       LEFT JOIN ", view, " v ON v.external_id = l.external_id
      WHERE l.id_backbone = $1"),
    params = list(info$id_backbone))

  protected <- unique(current$idtax_n[current$verified])
  current <- current[!current$idtax_n %in% protected, , drop = FALSE]
  if (taxa == "matched") {
    current <- current[current$idtax_n %in% link_data$idtax_n, , drop = FALSE]
  }
  new <- link_data[!link_data$idtax_n %in% protected, , drop = FALSE]
  new$is_preferred <- .choose_preferred_links(new$idtax_n, new$match_type, new$verified)

  name_map <- if ("backbone_taxon_name" %in% names(matches)) {
    stats::setNames(as.character(matches$backbone_taxon_name), as.character(matches$external_id))
  } else {
    character(0)
  }
  internal_map <- if ("taxon_name_internal" %in% names(matches)) {
    stats::setNames(as.character(matches$taxon_name_internal), as.character(matches$idtax_n))
  } else {
    character(0)
  }

  ids <- sort(unique(c(current$idtax_n, new$idtax_n)))
  old_pref <- current[current$is_preferred, , drop = FALSE]
  new_pref <- new[new$is_preferred, , drop = FALSE]

  cmp <- dplyr::tibble(
    idtax_n             = ids,
    taxon_name_internal = unname(internal_map[as.character(ids)]),
    old_external_id     = old_pref$external_id[match(ids, old_pref$idtax_n)],
    old_name            = old_pref$taxon_name[match(ids, old_pref$idtax_n)],
    new_external_id     = new_pref$external_id[match(ids, new_pref$idtax_n)],
    old_links           = as.integer(table(factor(current$idtax_n, levels = ids))),
    new_links           = as.integer(table(factor(new$idtax_n, levels = ids)))
  )
  cmp$new_name <- unname(name_map[cmp$new_external_id])
  cmp$change <- dplyr::case_when(
    is.na(cmp$old_external_id) & is.na(cmp$new_external_id) ~ "none",
    is.na(cmp$old_external_id) ~ "gained",
    is.na(cmp$new_external_id) ~ "lost",
    cmp$old_external_id == cmp$new_external_id ~ "unchanged",
    TRUE ~ "changed"
  )
  cmp$same_name <- cmp$change == "changed" & !is.na(cmp$old_name) &
    !is.na(cmp$new_name) & cmp$old_name == cmp$new_name
  cmp <- cmp[, c("idtax_n", "taxon_name_internal", "old_external_id", "old_name",
                 "new_external_id", "new_name", "old_links", "new_links",
                 "change", "same_name")]

  if (verbose) {
    cli::cli_h2("{info$name}: stored links vs new matches")
    cli::cli_alert_info("Taxa with a verified link, left untouched: {length(protected)}")
    counts <- table(factor(cmp$change, levels = c("unchanged", "changed", "gained", "lost", "none")))
    for (k in names(counts)) {
      extra <- if (k == "changed") paste0(" (", sum(cmp$same_name), " with the same name, another ID)") else ""
      cli::cli_alert_info("{k}: {counts[[k]]} taxa{extra}")
    }
    cli::cli_alert_info("Links: {nrow(current)} stored, {nrow(new)} new")
    shown <- cmp[cmp$change %in% c("changed", "lost") & !cmp$same_name, , drop = FALSE]
    if (nrow(shown) > 0) {
      cli::cli_h3("Sample of changed and lost names")
      print(as.data.frame(utils::head(shown[, c("idtax_n", "taxon_name_internal", "old_name", "new_name", "change")], 15)),
            row.names = FALSE)
    }
  }

  if (dry_run) {
    if (verbose) cli::cli_alert_info("Dry run - nothing was changed. Re-run with {.code dry_run = FALSE}.")
    return(invisible(cmp))
  }

  actual_con <- if (inherits(con_taxa, "Pool")) {
    pool::poolCheckout(con_taxa)
  } else {
    con_taxa
  }
  on.exit({
    if (inherits(con_taxa, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  unverified_taxon <- "
       NOT EXISTS (SELECT 1 FROM taxa_backbone_link v
                    WHERE v.id_backbone = $1
                      AND v.idtax_n = %s
                      AND v.verified)"

  DBI::dbBegin(actual_con)
  n <- tryCatch({
    delete_sql <- paste0(
      "DELETE FROM taxa_backbone_link l
        WHERE l.id_backbone = $1
          AND ", sprintf(unverified_taxon, "l.idtax_n"),
      if (taxa == "matched") " AND l.idtax_n = ANY($2::int[])")
    params <- if (taxa == "matched") {
      list(info$id_backbone, .pg_array_literal(unique(new$idtax_n)))
    } else {
      list(info$id_backbone)
    }
    n_deleted <- DBI::dbExecute(actual_con, delete_sql, params = params)

    DBI::dbWriteTable(actual_con, "tmp_backbone_links", new,
                      temporary = TRUE, overwrite = TRUE)
    n_inserted <- DBI::dbExecute(actual_con, paste0(
      "INSERT INTO taxa_backbone_link
              (idtax_n, id_backbone, external_id, is_preferred, match_type,
               match_score, matched_by, verified)
       SELECT n.idtax_n, $1, n.external_id, n.is_preferred, n.match_type,
              n.match_score, n.matched_by, n.verified
         FROM tmp_backbone_links n
        WHERE ", sprintf(unverified_taxon, "n.idtax_n"), "
       ON CONFLICT (idtax_n, id_backbone, external_id) DO NOTHING"),
      params = list(info$id_backbone))
    DBI::dbExecute(actual_con, "DROP TABLE tmp_backbone_links")
    DBI::dbCommit(actual_con)
    c(deleted = n_deleted, inserted = n_inserted)
  }, error = function(e) {
    DBI::dbRollback(actual_con)
    cli::cli_abort("Failed to replace {backbone} links, nothing was changed: {conditionMessage(e)}")
  })

  if (verbose) {
    cli::cli_alert_success("Replaced {backbone} links: {n[['deleted']]} deleted, {n[['inserted']]} inserted")
    check_backbone_links(backbone, con_taxa = actual_con)
  }
  invisible(cmp)
}
