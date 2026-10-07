# =============================================================================
# Draw the structure of the two CafriplotsR databases
#
# Replaces get_database_fk(), formerly exported from R/database_structure.R,
# which drew man/figures/mydb_structure.png with dm::dm_draw() from a
# hard-coded list of ten main-database tables and their DECLARED keys only.
# Both databases have changed a lot since, so this script reads the structure
# from the catalogue each time it is run instead of naming tables:
#
#   * every table, materialised view and foreign table in the schema
#     (plots_transects and rainbio), each drawn in its own cluster
#   * primary keys and DECLARED foreign keys, from pg_constraint
#   * UNDECLARED links the package relies on but the schema does not enforce
#     (dashed grey). Many joins in R/ use columns that carry no FOREIGN KEY
#     constraint, so a diagram of declared keys alone shows most tables floating
#     unconnected. These come from two sources:
#       - LINK_HINTS below: joins written in the package code (kept only when
#         both columns still exist in the database)
#       - name matching: a column whose name is the single-column primary key of
#         exactly one other table (e.g. id_method -> methodslist.id_method)
#   * CROSS-DATABASE links (dashed red): any idtax_n / idtax_good_n column on the
#     main database points at rainbio's table_taxa. No database can enforce
#     these.
#
# Tables are coloured by domain (plots, individuals, traits, taxonomy, lookups,
# access control) using name patterns in DOMAIN_RULES.
#
# Read-only: it queries pg_catalog only, so it works for any user who can
# connect, whatever their row-level security policies.
#
# Usage:
#
#   source(system.file("scripts", "draw_database_structure.R",
#                      package = "CafriplotsR"))
#   res <- draw_database_structure()                 # prompts for credentials
#   res$graph                                        # view in RStudio
#
#   # the synthetic view get_database_fk() used to give: core tables only
#   draw_database_structure(include = CORE_TABLES)
#
#   # save the overview
#   draw_database_structure(include = CORE_TABLES,
#                           out_file = "db_structure.png")
#
#   # one database, every column with its type
#   draw_database_structure(con_taxa = FALSE, columns = "all",
#                           out_file = "plots_transects_full.svg")
#
#   res$links    # the edge list, with how each link was found
#   res$isolated # tables that were dropped because nothing links to them
#
# Requires DiagrammeR (and DiagrammeRsvg to save; rsvg to save as PNG/PDF).
# =============================================================================


# ---- Configuration ----------------------------------------------------------

# Tables left out of the drawing. Audit trails, staging and scratch tables add
# dozens of nodes and no structure.
#
# table_taxa on the MAIN database is a stale copy: the live taxonomy is
# rainbio.table_taxa, which is where the cross-database links point.
DEFAULT_EXCLUDE <- list(
  main = c("^followup_", "^tmp_", "^temp_", "_temp$", "_tmp$", "_old$",
           "_bak$", "backup", "^table_taxa$"),
  taxa = c("^followup_", "^tmp_", "^temp_", "_temp$", "_tmp$", "_old$",
           "_bak$", "backup")
)

# The tables most users meet, for a readable overview. get_database_fk()'s ten
# tables, plus what has become central since: the methods lookup, specimen
# links, taxa-level traits (migrated to the main database) and rainbio's
# taxonomy. Names absent from a database are simply not drawn.
CORE_TABLES <- c(
  # plots and their features
  "data_liste_plots", "data_liste_sub_plots", "data_subplot_feat",
  "subplotype_list", "table_countries", "methodslist", "table_colnam",
  # individuals, measurements, specimens
  "data_individuals", "data_traits_measures", "data_ind_measures_feat",
  "traitlist", "data_link_specimens", "specimens",
  # taxa-level traits and taxonomy (rainbio)
  "taxa_traits_measures", "table_taxa", "table_tax_famclass"
)

# Joins the package code performs on columns that have no FOREIGN KEY
# constraint. Each row is kept only if both columns exist in the introspected
# schema, so stale entries are harmless. Add to this list when a new join
# appears in R/.
LINK_HINTS <- data.frame(
  db = c(
    "main", "main", "main", "main", "main",
    "main", "main", "main", "main", "main",
    "main", "main", "main",
    "taxa", "taxa", "taxa"
  ),
  table_name = c(
    "data_individuals", "data_liste_sub_plots", "data_liste_sub_plots",
    "data_liste_sub_plots", "data_subplot_feat",
    "data_traits_measures", "data_traits_measures", "data_traits_measures",
    "data_traits_measures", "data_liste_plots",
    "data_link_specimens", "data_link_specimens", "data_ind_measures_feat",
    "table_taxa", "table_taxa", "table_taxa"
  ),
  column_name = c(
    "id_table_liste_plots_n", "id_table_liste_plots", "id_type_sub_plot",
    "id_colnam", "id_sub_plots",
    "id_sub_plots", "id_data_individuals", "id_table_liste_plots",
    "traitid", "id_parent_plot",
    "id_n", "id_specimen", "id_ind_measures",
    "idtax_good_n", "id_parent", "id_tax_famclass"
  ),
  ref_table = c(
    "data_liste_plots", "data_liste_plots", "subplotype_list",
    "table_colnam", "data_liste_sub_plots",
    "data_liste_sub_plots", "data_individuals", "data_liste_plots",
    "traitlist", "data_liste_plots",
    "data_individuals", "specimens", "data_traits_measures",
    "table_taxa", "table_taxa", "table_tax_famclass"
  ),
  ref_column = c(
    "id_liste_plots", "id_liste_plots", "id_subplotype",
    "id_table_colnam", "id_sub_plots",
    "id_sub_plots", "id_n", "id_liste_plots",
    "id_trait", "id_liste_plots",
    "id_n", "id_specimen", "id_trait_measures",
    "idtax_n", "idtax_n", "id_tax_famclass"
  ),
  stringsAsFactors = FALSE
)

# Columns on the main database that hold a rainbio taxon id.
CROSS_DB_TAXON_COLUMNS <- c("idtax_n", "idtax_good_n", "idtax", "id_tax")

# First matching rule wins, so order matters.
DOMAIN_RULES <- list(
  list(domain = "Access control",
       pattern = "access|user_|registry|polic|role",  fill = "#E5E5E5"),
  list(domain = "Plots & subplot features",
       pattern = "liste_plots|sub_plot|subplot",      fill = "#CFE2F3"),
  list(domain = "Traits",
       pattern = "trait",                             fill = "#FCE5CD"),
  list(domain = "Individuals & specimens",
       pattern = "individual|ind_|specimen|records",  fill = "#D9EAD3"),
  list(domain = "Taxonomy",
       pattern = "tax|backbone|dico|famclass|wcvp",   fill = "#EAD1DC"),
  list(domain = "Lookups & references",
       pattern = "colnam|countr|method|citation|list|type", fill = "#FFF2CC")
)
DOMAIN_OTHER_FILL <- "#FFFFFF"

DB_LABELS <- c(main = "plots_transects (main)", taxa = "rainbio (taxonomy)")


# ---- Main entry point -------------------------------------------------------

#' Draw both databases' tables and the links between them
#'
#' @param con_main,con_taxa Connections (DBI or pool). `NULL` opens one with
#'   `CafriplotsR::call.mydb()` / `call.mydb.taxa()`; `FALSE` leaves that
#'   database out.
#' @param columns `"keys"` shows primary and linking columns only, `"all"`
#'   every column with its type, `"none"` table names only.
#' @param infer_links Draw undeclared links (hints and name matching).
#' @param drop_isolated Leave out tables with no link at all.
#' @param exclude List with `main` and `taxa` regex vectors of tables to leave
#'   out.
#' @param include Optional: keep only these tables. Either a character vector
#'   of exact table names (e.g. `CORE_TABLES`) or a single regex. Links to
#'   tables left out disappear.
#' @param rankdir Graphviz layout direction (`"LR"`, `"TB"`, ...).
#' @param out_file Optional `.svg`, `.png` or `.pdf` path.
#' @param width Pixel width for PNG output.
#'
#' @return Invisibly, a list with `graph` (htmlwidget), `dot` (source),
#'   `tables`, `links` and `isolated`.
draw_database_structure <- function(con_main = NULL,
                                    con_taxa = NULL,
                                    columns = c("keys", "all", "none"),
                                    infer_links = TRUE,
                                    drop_isolated = TRUE,
                                    exclude = DEFAULT_EXCLUDE,
                                    include = NULL,
                                    rankdir = "LR",
                                    schema = "public",
                                    out_file = NULL,
                                    width = 3000) {
  columns <- match.arg(columns)
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("Package 'DiagrammeR' is required: install.packages('DiagrammeR')",
         call. = FALSE)
  }

  if (is.null(con_main)) con_main <- CafriplotsR::call.mydb()
  if (is.null(con_taxa)) con_taxa <- CafriplotsR::call.mydb.taxa()

  dbs <- list()
  if (!isFALSE(con_main)) dbs$main <- .introspect_schema(con_main, schema)
  if (!isFALSE(con_taxa)) dbs$taxa <- .introspect_schema(con_taxa, schema)
  if (length(dbs) == 0) stop("Both databases were left out.", call. = FALSE)

  # Tables and their columns, after exclusion
  cols <- do.call(rbind, lapply(names(dbs), function(db) {
    x <- dbs[[db]]$columns
    x$db <- rep(db, nrow(x))
    pats <- exclude[[db]]
    if (length(pats)) {
      drop <- Reduce(`|`, lapply(pats, grepl, x = x$table_name))
      x <- x[!drop, , drop = FALSE]
    }
    if (length(include) > 1) {
      x <- x[x$table_name %in% include, , drop = FALSE]
    } else if (length(include) == 1) {
      x <- x[grepl(include, x$table_name), , drop = FALSE]
    }
    x
  }))
  tables <- unique(cols[, c("db", "table_name", "kind", "n_rows")])

  keys <- do.call(rbind, lapply(names(dbs), function(db) {
    x <- dbs[[db]]$keys
    x$db <- rep(db, nrow(x))
    x
  }))
  pk <- keys[keys$contype == "p", c("db", "table_name", "column_name")]
  pk <- .semi(pk, cols, c("db", "table_name", "column_name"))

  links <- .collect_links(keys, pk, cols, infer_links, has_taxa = "taxa" %in% names(dbs))

  linked <- unique(rbind(
    data.frame(db = links$db, table_name = links$table_name),
    data.frame(db = links$ref_db, table_name = links$ref_table)
  ))
  isolated <- .anti(tables, linked, c("db", "table_name"))
  if (drop_isolated && nrow(isolated)) {
    message("Left out ", nrow(isolated), " unlinked table(s): ",
            paste0(isolated$db, ".", isolated$table_name, collapse = ", "))
    tables <- .semi(tables, linked, c("db", "table_name"))
  }

  dot <- .build_dot(tables, cols, pk, links, columns, rankdir)
  graph <- DiagrammeR::grViz(dot)

  if (!is.null(out_file)) .save_graph(graph, out_file, width)

  message(nrow(tables), " tables, ", nrow(links), " links (",
          paste(names(table(links$source)), table(links$source),
                sep = ": ", collapse = ", "), ")")

  if (interactive()) print(graph)
  invisible(list(graph = graph, dot = dot, tables = tables, links = links,
                 isolated = isolated))
}


# ---- Introspection ----------------------------------------------------------

.introspect_schema <- function(con, schema) {
  actual_con <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
  on.exit(if (inherits(con, "Pool")) pool::poolReturn(actual_con), add = TRUE)

  columns <- DBI::dbGetQuery(actual_con, "
    SELECT c.relname                              AS table_name,
           CASE c.relkind WHEN 'm' THEN 'matview'
                          WHEN 'f' THEN 'foreign'
                          ELSE 'table' END        AS kind,
           GREATEST(c.reltuples, -1)::bigint      AS n_rows,
           a.attname                              AS column_name,
           format_type(a.atttypid, a.atttypmod)   AS data_type,
           a.attnum                               AS position
    FROM pg_class c
    JOIN pg_namespace n ON n.oid = c.relnamespace
    JOIN pg_attribute a ON a.attrelid = c.oid
                       AND a.attnum > 0 AND NOT a.attisdropped
    WHERE n.nspname = $1
      AND c.relkind IN ('r', 'p', 'm', 'f')
    ORDER BY c.relname, a.attnum", params = list(schema))

  keys <- DBI::dbGetQuery(actual_con, "
    SELECT con.contype::text  AS contype,
           c.relname          AS table_name,
           a.attname          AS column_name,
           rc.relname         AS ref_table,
           ra.attname         AS ref_column
    FROM pg_constraint con
    JOIN pg_class c      ON c.oid = con.conrelid
    JOIN pg_namespace n  ON n.oid = c.relnamespace
    CROSS JOIN LATERAL unnest(con.conkey) WITH ORDINALITY AS k(attnum, ord)
    JOIN pg_attribute a  ON a.attrelid = con.conrelid AND a.attnum = k.attnum
    LEFT JOIN pg_class rc     ON rc.oid = con.confrelid
    LEFT JOIN pg_attribute ra ON ra.attrelid = con.confrelid
                             AND ra.attnum = con.confkey[k.ord]
    WHERE n.nspname = $1
      AND con.contype IN ('p', 'f')", params = list(schema))

  list(columns = columns, keys = keys)
}


# ---- Links ------------------------------------------------------------------

.collect_links <- function(keys, pk, cols, infer_links, has_taxa) {
  link_cols <- c("db", "table_name", "column_name",
                 "ref_db", "ref_table", "ref_column")

  declared <- keys[keys$contype == "f", ]
  declared$ref_db <- declared$db
  declared$source <- rep("declared", nrow(declared))
  out <- declared[, c(link_cols, "source")]

  if (infer_links) {
    hints <- LINK_HINTS
    hints$ref_db <- hints$db
    hints$source <- rep("code join", nrow(hints))

    # A column named like exactly one other table's single-column primary key
    pk_single <- pk[!duplicated(pk[, c("db", "table_name")]) &
                      !duplicated(pk[, c("db", "table_name")], fromLast = TRUE), ]
    pk_single <- pk_single[!pk_single$column_name %in% c("id", "gid", "fid"), ]
    pk_unique <- pk_single[!duplicated(pk_single[, c("db", "column_name")]) &
                             !duplicated(pk_single[, c("db", "column_name")],
                                         fromLast = TRUE), ]
    by_name <- merge(cols[, c("db", "table_name", "column_name")],
                     setNames(pk_unique, c("db", "ref_table", "column_name")),
                     by = c("db", "column_name"))
    by_name <- by_name[by_name$table_name != by_name$ref_table, ]
    by_name$ref_column <- by_name$column_name
    by_name$ref_db <- by_name$db
    by_name$source <- rep("name match", nrow(by_name))

    out <- rbind(out, hints[, c(link_cols, "source")],
                 by_name[, c(link_cols, "source")])
  }

  if (has_taxa) {
    cross <- cols[cols$db == "main" &
                    cols$column_name %in% CROSS_DB_TAXON_COLUMNS,
                  c("db", "table_name", "column_name")]
    cross$ref_db <- rep("taxa", nrow(cross))
    cross$ref_table <- rep("table_taxa", nrow(cross))
    cross$ref_column <- rep("idtax_n", nrow(cross))
    cross$source <- rep("cross-database", nrow(cross))
    out <- rbind(out, cross[, c(link_cols, "source")])
  }

  # Both ends must exist after exclusion
  out <- .semi(out, cols, c("db", "table_name", "column_name"))
  ref_cols <- setNames(cols[, c("db", "table_name", "column_name")],
                       c("ref_db", "ref_table", "ref_column"))
  out <- .semi(out, ref_cols, c("ref_db", "ref_table", "ref_column"))

  # One edge per column pair; the first source listed wins (declared first)
  out <- out[!duplicated(out[, link_cols]), ]
  # A child column pointing at the same table twice (e.g. hint + name match on
  # different parent columns) keeps the higher-priority one only
  out <- out[!duplicated(out[, c("db", "table_name", "column_name",
                                 "ref_db", "ref_table")]), ]
  rownames(out) <- NULL
  out
}


# ---- DOT generation ---------------------------------------------------------

.domain_of <- function(table_name) {
  for (rule in DOMAIN_RULES) {
    if (grepl(rule$pattern, table_name)) return(rule)
  }
  list(domain = "Other", fill = DOMAIN_OTHER_FILL)
}

.node_id <- function(db, table_name) paste0(db, "__", table_name)

.html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

.node_label <- function(db, table_name, kind, n_rows, cols, pk, links, columns) {
  rule <- .domain_of(table_name)
  rows <- if (n_rows < 0) "rows: ?" else
    paste("~", format(n_rows, big.mark = ",", scientific = FALSE), "rows")
  if (kind != "table") rows <- paste0(rows, " | ", kind)

  header <- sprintf(
    '<TR><TD BGCOLOR="%s"><FONT POINT-SIZE="12">%s</FONT><BR/><FONT POINT-SIZE="9">%s</FONT></TD></TR>',
    rule$fill, .html_escape(table_name), rows)

  tcols <- cols[cols$db == db & cols$table_name == table_name, ]
  is_pk <- tcols$column_name %in%
    pk$column_name[pk$db == db & pk$table_name == table_name]
  is_fk <- tcols$column_name %in%
    links$column_name[links$db == db & links$table_name == table_name]
  is_ref <- tcols$column_name %in%
    links$ref_column[links$ref_db == db & links$ref_table == table_name]

  show <- switch(columns,
                 keys = is_pk | is_fk | is_ref,
                 all = rep(TRUE, nrow(tcols)),
                 none = rep(FALSE, nrow(tcols)))
  # Port on every linked column so edges can attach even when hidden
  body <- vapply(which(show), function(i) {
    tag <- paste(c(if (is_pk[i]) "PK", if (is_fk[i]) "FK"), collapse = " ")
    # Graphviz rejects an empty <FONT></FONT>
    if (nzchar(tag)) tag <- sprintf('<FONT POINT-SIZE="8" COLOR="#999999">%s</FONT> ', tag)
    type <- if (columns == "all") {
      sprintf(' <FONT POINT-SIZE="8" COLOR="#777777">%s</FONT>',
              .html_escape(tcols$data_type[i]))
    } else ""
    name <- .html_escape(tcols$column_name[i])
    if (is_pk[i]) name <- paste0("<U>", name, "</U>")
    sprintf(
      '<TR><TD PORT="%s" ALIGN="LEFT">%s%s%s</TD></TR>',
      .html_escape(tcols$column_name[i]), tag, name, type)
  }, character(1))

  paste0('<<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="3">',
         header, paste(body, collapse = ""), "</TABLE>>")
}

.build_dot <- function(tables, cols, pk, links, columns, rankdir) {
  clusters <- vapply(unique(tables$db), function(db) {
    tb <- tables[tables$db == db, ]
    nodes <- vapply(seq_len(nrow(tb)), function(i) {
      sprintf('    "%s" [label = %s];',
              .node_id(db, tb$table_name[i]),
              .node_label(db, tb$table_name[i], tb$kind[i], tb$n_rows[i],
                          cols, pk, links, columns))
    }, character(1))
    sprintf(paste0('  subgraph "cluster_%s" {\n',
                   '    label = "%s"; fontsize = 18; fontname = "Helvetica-Bold";\n',
                   '    style = "rounded,dashed"; color = "#888888";\n%s\n  }'),
            db, DB_LABELS[[db]], paste(nodes, collapse = "\n"))
  }, character(1))

  edge_style <- c(
    "declared"       = 'color = "#333333"',
    "code join"      = 'color = "#888888", style = dashed',
    "name match"     = 'color = "#888888", style = dashed',
    "cross-database" = 'color = "#C0392B", style = dashed, penwidth = 1.5'
  )
  # In "none" mode columns have no rows, so edges attach to the node itself
  port <- function(col) if (columns == "none") "" else sprintf(':"%s"', col)
  edges <- vapply(seq_len(nrow(links)), function(i) {
    l <- links[i, ]
    sprintf('  "%s"%s -> "%s"%s [%s];',
            .node_id(l$db, l$table_name), port(l$column_name),
            .node_id(l$ref_db, l$ref_table), port(l$ref_column),
            edge_style[[l$source]])
  }, character(1))

  # Hidden columns in "keys" mode never hide a linked column, so every port
  # referenced above exists.
  legend <- .legend_dot()

  paste0(
    "digraph db_structure {\n",
    sprintf('  graph [rankdir = %s, splines = true, nodesep = 0.3, ranksep = 1.2, fontname = "Helvetica", compound = true];\n', rankdir),
    '  node [shape = plaintext, fontname = "Helvetica", fontsize = 10];\n',
    '  edge [arrowhead = normal, arrowsize = 0.6];\n',
    paste(clusters, collapse = "\n"), "\n",
    paste(edges, collapse = "\n"), "\n",
    legend, "\n}\n")
}

.legend_dot <- function() {
  domains <- c(vapply(DOMAIN_RULES, function(r) {
    sprintf('<TR><TD BGCOLOR="%s" ALIGN="LEFT">%s</TD></TR>', r$fill, .html_escape(r$domain))
  }, character(1)),
  sprintf('<TR><TD BGCOLOR="%s" ALIGN="LEFT">Other</TD></TR>', DOMAIN_OTHER_FILL))
  lines <- paste0(
    '<TR><TD ALIGN="LEFT">Links (child &#8594; parent)</TD></TR>',
    '<TR><TD ALIGN="LEFT">solid: declared foreign key</TD></TR>',
    '<TR><TD ALIGN="LEFT"><FONT COLOR="#888888">dashed grey: undeclared, from code or column name</FONT></TD></TR>',
    '<TR><TD ALIGN="LEFT"><FONT COLOR="#C0392B">dashed red: across databases</FONT></TD></TR>',
    '<TR><TD ALIGN="LEFT"><U>underlined</U> = primary key</TD></TR>')
  sprintf(
    '  legend [label = <<TABLE BORDER="1" CELLBORDER="0" CELLSPACING="0" CELLPADDING="3"><TR><TD>LEGEND (drawn %s)</TD></TR>%s%s</TABLE>>];',
    format(Sys.Date()), paste(domains, collapse = ""), lines)
}


# ---- Output -----------------------------------------------------------------

.save_graph <- function(graph, out_file, width) {
  if (!requireNamespace("DiagrammeRsvg", quietly = TRUE)) {
    stop("Package 'DiagrammeRsvg' is required to save the figure.",
         call. = FALSE)
  }
  svg <- DiagrammeRsvg::export_svg(graph)
  ext <- tolower(tools::file_ext(out_file))
  if (ext == "svg") {
    writeLines(svg, out_file, useBytes = TRUE)
  } else if (ext %in% c("png", "pdf")) {
    if (!requireNamespace("rsvg", quietly = TRUE)) {
      stop("Package 'rsvg' is required for PNG/PDF output; ",
           "use an .svg file name or install.packages('rsvg').", call. = FALSE)
    }
    if (ext == "png") {
      rsvg::rsvg_png(charToRaw(svg), out_file, width = width)
    } else {
      rsvg::rsvg_pdf(charToRaw(svg), out_file)
    }
  } else {
    stop("out_file must end in .svg, .png or .pdf", call. = FALSE)
  }
  message("Saved ", normalizePath(out_file, mustWork = FALSE))
  invisible(out_file)
}


# ---- Small data.frame helpers (no dplyr needed) ----------------------------

.key_string <- function(df, by) do.call(paste, c(df[by], sep = "\r"))

.semi <- function(x, y, by) {
  x[.key_string(x, by) %in% .key_string(setNames(y[by], by), by), , drop = FALSE]
}

.anti <- function(x, y, by) {
  x[!.key_string(x, by) %in% .key_string(setNames(y[by], by), by), , drop = FALSE]
}
