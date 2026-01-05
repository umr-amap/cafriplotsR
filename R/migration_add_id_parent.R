# Migration: Add id_parent Column to table_taxa
#
# This migration adds hierarchical parent-child relationships to the taxonomic
# backbone by adding an id_parent column that links each taxon to its
# immediate parent level:
#   - Species → Genus
#   - Genus → Family
#   - Family → Order
#   - Order → Class
#   - Class → NULL (root)
#
# Run this migration once to:
# 1. Add the id_parent column
# 2. Create missing intermediate level entries (genus, family, order, class)
# 3. Populate id_parent for all existing entries
#
# Dependencies: DBI, dplyr, cli


#' Add id_parent Column to table_taxa
#'
#' Adds the id_parent column with foreign key constraint and index.
#' This is Phase 1 of the hierarchy migration.
#'
#' @param con Database connection to taxa database
#' @param dry_run If TRUE, only print SQL without executing
#'
#' @return TRUE if successful
#' @export
migration_add_id_parent_column <- function(con = NULL, dry_run = FALSE) {
  if (is.null(con)) {
    con <- call.mydb.taxa()
  }

  # Handle pool connections
  actual_con <- if (inherits(con, "Pool")) {
    pool::poolCheckout(con)
  } else {
    con
  }

  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  cli::cli_h1("Migration: Add id_parent column to table_taxa")

  # Check if column already exists
  existing_cols <- DBI::dbListFields(actual_con, "table_taxa")
  if ("id_parent" %in% existing_cols) {
    cli::cli_alert_warning("Column 'id_parent' already exists. Skipping column creation.")
    return(TRUE)
  }

  # SQL statements
  sql_add_column <- "ALTER TABLE table_taxa ADD COLUMN id_parent INTEGER;"

  sql_add_fk <- "
    ALTER TABLE table_taxa
    ADD CONSTRAINT fk_table_taxa_id_parent
    FOREIGN KEY (id_parent) REFERENCES table_taxa(idtax_n)
    ON DELETE SET NULL;
  "

  sql_add_index <- "CREATE INDEX idx_table_taxa_id_parent ON table_taxa(id_parent);"

  if (dry_run) {
    cli::cli_h2("Dry run - SQL statements that would be executed:")
    cli::cli_code(sql_add_column)
    cli::cli_code(sql_add_fk)
    cli::cli_code(sql_add_index)
    return(TRUE)
  }

  tryCatch({
    cli::cli_alert_info("Adding id_parent column...")
    DBI::dbExecute(actual_con, sql_add_column)
    cli::cli_alert_success("Column added")

    cli::cli_alert_info("Adding foreign key constraint...")
    DBI::dbExecute(actual_con, sql_add_fk)
    cli::cli_alert_success("Foreign key constraint added")

    cli::cli_alert_info("Creating index...")
    DBI::dbExecute(actual_con, sql_add_index)
    cli::cli_alert_success("Index created")

    cli::cli_alert_success("Migration Phase 1 complete: id_parent column added")
    return(TRUE)

  }, error = function(e) {
    cli::cli_alert_danger("Migration failed: {e$message}")
    stop(e)
  })
}


#' Create Missing Hierarchy Entries
#'
#' Creates entries for genus, family, order, and class levels where they
#' don't already exist. This is Phase 2 of the hierarchy migration.
#'
#' Uses the `tax_level` column to identify existing entries at each level.
#' Valid tax_level values: "class", "higher", "order", "family", "genus", "species", "infraspecific"
#'
#' NOTE: Classes are currently stored in a separate table_tax_famclass table
#' and linked via id_tax_famclass. This migration creates class-level entries
#' in table_taxa for each class, which will serve as the root of the hierarchy.
#'
#' @param con Database connection to taxa database
#' @param dry_run If TRUE, only count what would be created
#' @param verbose If TRUE, show progress
#'
#' @return Data frame with counts of created entries
#' @export
migration_create_hierarchy_entries <- function(con = NULL, dry_run = FALSE, verbose = TRUE) {
  if (is.null(con)) {
    con <- call.mydb.taxa()
  }

  # Handle pool connections
  actual_con <- if (inherits(con, "Pool")) {
    pool::poolCheckout(con)
  } else {
    con
  }

  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  cli::cli_h1("Migration: Create missing hierarchy entries")

  created_counts <- list(
    class = 0,
    order = 0,
    family = 0,
    genus = 0
  )

  # Get all existing taxa with tax_level column
  all_taxa <- dplyr::tbl(actual_con, "table_taxa") %>%
    dplyr::select(idtax_n, id_tax_famclass, tax_famclass, tax_order, tax_fam, tax_gen, tax_esp,
                  tax_rank01, tax_nam01, tax_level) %>%
    dplyr::collect()

  if (verbose) cli::cli_alert_info("Loaded {nrow(all_taxa)} existing taxa")

  # Show distribution by tax_level
  level_counts <- all_taxa %>%
    dplyr::count(tax_level) %>%
    dplyr::arrange(tax_level)
  if (verbose) {
    cli::cli_alert_info("Existing taxa by level:")
    for (i in seq_len(nrow(level_counts))) {
      cli::cli_li("{level_counts$tax_level[i]}: {level_counts$n[i]}")
    }
  }

  # Get all classes from the dedicated table_tax_famclass lookup table
  # This is where classes are currently stored (linked via id_tax_famclass)
  all_classes <- dplyr::tbl(actual_con, "table_tax_famclass") %>%
    dplyr::select(id_tax_famclass, tax_famclass) %>%
    dplyr::collect()

  if (verbose) cli::cli_alert_info("Found {nrow(all_classes)} classes in table_tax_famclass")

  # 1. Create CLASS entries in table_taxa from table_tax_famclass
  # These entries will have: tax_famclass set, all other taxonomic fields NULL, id_parent = NULL
  cli::cli_h2("Step 1: Class-level entries (from table_tax_famclass)")

  # Check which classes already have dedicated entries in table_taxa
  # Use tax_level "higher" or "class" (higher may represent class in existing data)
  existing_class_entries <- all_taxa %>%
    dplyr::filter(tax_level %in% c("class", "higher") |
                  (is.na(tax_order) & is.na(tax_fam) & is.na(tax_gen) & is.na(tax_esp) & !is.na(tax_famclass))) %>%
    dplyr::distinct(tax_famclass) %>%
    dplyr::pull(tax_famclass)

  missing_classes <- all_classes %>%
    dplyr::filter(!tax_famclass %in% existing_class_entries)

  if (nrow(missing_classes) > 0) {
    if (verbose) cli::cli_alert_info("Found {nrow(missing_classes)} classes without dedicated entries in table_taxa")

    if (!dry_run) {
      for (i in seq_len(nrow(missing_classes))) {
        class_row <- missing_classes[i, ]
        new_entry <- .create_hierarchy_entry(
          actual_con,
          tax_famclass = class_row$tax_famclass,
          id_tax_famclass = class_row$id_tax_famclass,
          tax_level = "class"
        )
        created_counts$class <- created_counts$class + 1
        if (verbose) cli::cli_alert_success("Created class entry: {class_row$tax_famclass} (ID: {new_entry})")
      }
    } else {
      if (verbose) cli::cli_alert_info("Would create {nrow(missing_classes)} class entries")
      created_counts$class <- nrow(missing_classes)
    }
  } else {
    if (verbose) cli::cli_alert_success("All classes already have entries in table_taxa")
  }

  # Refresh taxa list after creating class entries
  if (!dry_run && created_counts$class > 0) {
    all_taxa <- dplyr::tbl(actual_con, "table_taxa") %>%
      dplyr::select(idtax_n, id_tax_famclass, tax_famclass, tax_order, tax_fam, tax_gen, tax_esp,
                    tax_rank01, tax_nam01, tax_level) %>%
      dplyr::collect()
  }

  # 2. Create missing ORDER entries
  cli::cli_h2("Step 2: Order-level entries")
  unique_orders <- all_taxa %>%
    dplyr::filter(!is.na(tax_order)) %>%
    dplyr::distinct(tax_order, tax_famclass) %>%
    dplyr::collect()

  # Use tax_level = "order" to identify existing order entries
  existing_order_entries <- all_taxa %>%
    dplyr::filter(tax_level == "order") %>%
    dplyr::distinct(tax_order) %>%
    dplyr::pull(tax_order)

  missing_orders <- unique_orders %>%
    dplyr::filter(!tax_order %in% existing_order_entries)

  if (nrow(missing_orders) > 0) {
    if (verbose) cli::cli_alert_info("Found {nrow(missing_orders)} orders without dedicated entries")

    if (!dry_run) {
      for (i in seq_len(nrow(missing_orders))) {
        new_entry <- .create_hierarchy_entry(
          actual_con,
          tax_order = missing_orders$tax_order[i],
          tax_famclass = missing_orders$tax_famclass[i],
          tax_level = "order"
        )
        created_counts$order <- created_counts$order + 1
      }
      if (verbose) cli::cli_alert_success("Created {created_counts$order} order entries")
    } else {
      created_counts$order <- nrow(missing_orders)
    }
  } else {
    if (verbose) cli::cli_alert_success("All orders already have entries")
  }

  # 3. Create missing FAMILY entries
  cli::cli_h2("Step 3: Family-level entries")
  unique_families <- all_taxa %>%
    dplyr::filter(!is.na(tax_fam)) %>%
    dplyr::distinct(tax_fam, tax_order, tax_famclass) %>%
    dplyr::collect()

  # Use tax_level = "family" to identify existing family entries
  existing_family_entries <- all_taxa %>%
    dplyr::filter(tax_level == "family") %>%
    dplyr::distinct(tax_fam) %>%
    dplyr::pull(tax_fam)

  missing_families <- unique_families %>%
    dplyr::filter(!tax_fam %in% existing_family_entries)

  if (nrow(missing_families) > 0) {
    if (verbose) cli::cli_alert_info("Found {nrow(missing_families)} families without dedicated entries")

    if (!dry_run) {
      for (i in seq_len(nrow(missing_families))) {
        new_entry <- .create_hierarchy_entry(
          actual_con,
          tax_fam = missing_families$tax_fam[i],
          tax_order = missing_families$tax_order[i],
          tax_famclass = missing_families$tax_famclass[i],
          tax_level = "family"
        )
        created_counts$family <- created_counts$family + 1
      }
      if (verbose) cli::cli_alert_success("Created {created_counts$family} family entries")
    } else {
      created_counts$family <- nrow(missing_families)
    }
  } else {
    if (verbose) cli::cli_alert_success("All families already have entries")
  }

  # 4. Create missing GENUS entries
  cli::cli_h2("Step 4: Genus-level entries")
  unique_genera <- all_taxa %>%
    dplyr::filter(!is.na(tax_gen)) %>%
    dplyr::distinct(tax_gen, tax_fam, tax_order, tax_famclass) %>%
    dplyr::collect()

  # Use tax_level = "genus" to identify existing genus entries
  existing_genus_entries <- all_taxa %>%
    dplyr::filter(tax_level == "genus") %>%
    dplyr::distinct(tax_gen, tax_fam) %>%
    dplyr::mutate(key = paste(tax_gen, tax_fam, sep = "|"))

  missing_genera <- unique_genera %>%
    dplyr::mutate(key = paste(tax_gen, tax_fam, sep = "|")) %>%
    dplyr::filter(!key %in% existing_genus_entries$key)

  if (nrow(missing_genera) > 0) {
    if (verbose) cli::cli_alert_info("Found {nrow(missing_genera)} genera without dedicated entries")

    if (!dry_run) {
      for (i in seq_len(nrow(missing_genera))) {
        new_entry <- .create_hierarchy_entry(
          actual_con,
          tax_gen = missing_genera$tax_gen[i],
          tax_fam = missing_genera$tax_fam[i],
          tax_order = missing_genera$tax_order[i],
          tax_famclass = missing_genera$tax_famclass[i],
          tax_level = "genus"
        )
        created_counts$genus <- created_counts$genus + 1
      }
      if (verbose) cli::cli_alert_success("Created {created_counts$genus} genus entries")
    } else {
      created_counts$genus <- nrow(missing_genera)
    }
  } else {
    if (verbose) cli::cli_alert_success("All genera already have entries")
  }

  cli::cli_h2("Summary")
  total_created <- sum(unlist(created_counts))
  action <- if (dry_run) "Would create" else "Created"
  cli::cli_alert_info("{action} {total_created} hierarchy entries:")
  cli::cli_ul(c(
    "Classes: {created_counts$class}",
    "Orders: {created_counts$order}",
    "Families: {created_counts$family}",
    "Genera: {created_counts$genus}"
  ))

  return(as.data.frame(created_counts))
}


#' Create a hierarchy entry (internal helper)
#'
#' Creates a new entry in table_taxa for a given taxonomic level.
#' Uses tax_level column to indicate the taxonomic rank.
#'
#' Note: id_tax_famclass is deprecated in the new hierarchy system.
#' Classes are now stored directly in table_taxa with id_parent = NULL.
#'
#' @param con Database connection
#' @param tax_gen Genus name
#' @param tax_fam Family name
#' @param tax_order Order name
#' @param tax_famclass Class name
#' @param tax_level Taxonomic level: "class", "order", "family", "genus"
#' @param id_tax_famclass (deprecated) ID in table_tax_famclass for backward compatibility
#'
#' @keywords internal
.create_hierarchy_entry <- function(con, tax_gen = NA, tax_fam = NA,
                                    tax_order = NA, tax_famclass = NA,
                                    tax_level = NA, id_tax_famclass = NA) {
  # NOTE: id_tax_famclass is deprecated in the new hierarchy system
  # We keep it for backward compatibility during migration, but new entries
  # will rely on id_parent for hierarchy traversal instead.

  # If not provided, try to look it up for backward compatibility
  if (is.na(id_tax_famclass) && !is.na(tax_famclass)) {
    class_row <- tryCatch({
      DBI::dbGetQuery(
        con,
        "SELECT id_tax_famclass FROM table_tax_famclass WHERE tax_famclass = $1",
        params = list(tax_famclass)
      )
    }, error = function(e) data.frame())

    if (nrow(class_row) > 0) {
      id_tax_famclass <- class_row$id_tax_famclass[1]
    }
  }

  new_entry <- tibble::tibble(
    tax_gen = if (is.na(tax_gen)) NA_character_ else tax_gen,
    tax_esp = NA_character_,
    tax_fam = if (is.na(tax_fam)) NA_character_ else tax_fam,
    tax_order = if (is.na(tax_order)) NA_character_ else tax_order,
    tax_famclass = if (is.na(tax_famclass)) NA_character_ else tax_famclass,
    tax_rank01 = NA_character_,
    tax_nam01 = NA_character_,
    tax_rank02 = NA_character_,
    tax_nam02 = NA_character_,
    tax_source = "H_MIG",
    tax_level = if (is.na(tax_level)) NA_character_ else tax_level,
    idtax_good_n = NA_integer_,
    id_tax_famclass = if (is.na(id_tax_famclass)) NA_integer_ else as.integer(id_tax_famclass),
    morpho_species = FALSE,
    id_parent = NA_integer_  # Will be set in Phase 3 (link_hierarchy)
  )

  # Add modification fields
  new_entry <- .add_modif_field(new_entry)
  new_entry <- new_entry %>%
    dplyr::rename(
      data_modif_m = date_modif_m,
      data_modif_y = date_modif_y,
      data_modif_d = date_modif_d
    )

  DBI::dbWriteTable(con, "table_taxa", new_entry, append = TRUE, row.names = FALSE)

  # Get the new ID
  rs <- DBI::dbSendQuery(con, "SELECT MAX(idtax_n) FROM table_taxa")
  lastval <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)

  return(lastval$max)
}


#' Populate id_parent for Existing Entries
#'
#' Links each taxon to its parent level using tax_level column.
#' This is Phase 3 of the hierarchy migration.
#'
#' @param con Database connection to taxa database
#' @param dry_run If TRUE, only count what would be updated
#' @param batch_size Number of records to update per batch
#' @param verbose If TRUE, show progress
#'
#' @return Data frame with counts of linked entries
#' @export
migration_link_hierarchy <- function(con = NULL, dry_run = FALSE, batch_size = 1000, verbose = TRUE) {
  if (is.null(con)) {
    con <- call.mydb.taxa()
  }

  # Handle pool connections
  actual_con <- if (inherits(con, "Pool")) {
    pool::poolCheckout(con)
  } else {
    con
  }

  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  cli::cli_h1("Migration: Link hierarchy via id_parent")

  linked_counts <- list(
    order_to_class = 0,
    family_to_order = 0,
    genus_to_family = 0,
    species_to_genus = 0,
    infra_to_species = 0
  )

  # 1. Link orders to classes
  # Orders link to class entries (tax_level IN ('class', 'higher'))
  cli::cli_h2("Step 1: Link orders to classes")
  sql_order_to_class <- "
    UPDATE table_taxa child
    SET id_parent = (
      SELECT parent.idtax_n FROM table_taxa parent
      WHERE parent.tax_famclass = child.tax_famclass
        AND parent.tax_level IN ('class', 'higher')
      LIMIT 1
    )
    WHERE child.tax_level = 'order'
      AND child.id_parent IS NULL;
  "

  if (!dry_run) {
    result <- DBI::dbExecute(actual_con, sql_order_to_class)
    linked_counts$order_to_class <- result
    if (verbose) cli::cli_alert_success("Linked {result} orders to classes")
  } else {
    count_sql <- "SELECT COUNT(*) FROM table_taxa WHERE tax_level = 'order' AND id_parent IS NULL;"
    count <- DBI::dbGetQuery(actual_con, count_sql)$count[1]
    linked_counts$order_to_class <- count
    if (verbose) cli::cli_alert_info("Would link {count} orders to classes")
  }

  # 2. Link families to orders
  cli::cli_h2("Step 2: Link families to orders")
  sql_family_to_order <- "
    UPDATE table_taxa child
    SET id_parent = (
      SELECT parent.idtax_n FROM table_taxa parent
      WHERE parent.tax_order = child.tax_order
        AND parent.tax_level = 'order'
      LIMIT 1
    )
    WHERE child.tax_level = 'family'
      AND child.id_parent IS NULL;
  "

  if (!dry_run) {
    result <- DBI::dbExecute(actual_con, sql_family_to_order)
    linked_counts$family_to_order <- result
    if (verbose) cli::cli_alert_success("Linked {result} families to orders")
  } else {
    count_sql <- "SELECT COUNT(*) FROM table_taxa WHERE tax_level = 'family' AND id_parent IS NULL;"
    count <- DBI::dbGetQuery(actual_con, count_sql)$count[1]
    linked_counts$family_to_order <- count
    if (verbose) cli::cli_alert_info("Would link {count} families to orders")
  }

  # 3. Link genera to families
  cli::cli_h2("Step 3: Link genera to families")
  sql_genus_to_family <- "
    UPDATE table_taxa child
    SET id_parent = (
      SELECT parent.idtax_n FROM table_taxa parent
      WHERE parent.tax_fam = child.tax_fam
        AND parent.tax_level = 'family'
      LIMIT 1
    )
    WHERE child.tax_level = 'genus'
      AND child.id_parent IS NULL;
  "

  if (!dry_run) {
    result <- DBI::dbExecute(actual_con, sql_genus_to_family)
    linked_counts$genus_to_family <- result
    if (verbose) cli::cli_alert_success("Linked {result} genera to families")
  } else {
    count_sql <- "SELECT COUNT(*) FROM table_taxa WHERE tax_level = 'genus' AND id_parent IS NULL;"
    count <- DBI::dbGetQuery(actual_con, count_sql)$count[1]
    linked_counts$genus_to_family <- count
    if (verbose) cli::cli_alert_info("Would link {count} genera to families")
  }

  # 4. Link species to genera
  cli::cli_h2("Step 4: Link species to genera")
  sql_species_to_genus <- "
    UPDATE table_taxa child
    SET id_parent = (
      SELECT parent.idtax_n FROM table_taxa parent
      WHERE parent.tax_gen = child.tax_gen
        AND parent.tax_fam = child.tax_fam
        AND parent.tax_level = 'genus'
      LIMIT 1
    )
    WHERE child.tax_level = 'species'
      AND child.id_parent IS NULL;
  "

  if (!dry_run) {
    result <- DBI::dbExecute(actual_con, sql_species_to_genus)
    linked_counts$species_to_genus <- result
    if (verbose) cli::cli_alert_success("Linked {result} species to genera")
  } else {
    count_sql <- "SELECT COUNT(*) FROM table_taxa WHERE tax_level = 'species' AND id_parent IS NULL;"
    count <- DBI::dbGetQuery(actual_con, count_sql)$count[1]
    linked_counts$species_to_genus <- count
    if (verbose) cli::cli_alert_info("Would link {count} species to genera")
  }

  # 5. Link infraspecific to species
  cli::cli_h2("Step 5: Link infraspecific to species")
  sql_infra_to_species <- "
    UPDATE table_taxa child
    SET id_parent = (
      SELECT parent.idtax_n FROM table_taxa parent
      WHERE parent.tax_gen = child.tax_gen
        AND parent.tax_fam = child.tax_fam
        AND parent.tax_esp = child.tax_esp
        AND parent.tax_level = 'species'
      LIMIT 1
    )
    WHERE child.tax_level = 'infraspecific'
      AND child.id_parent IS NULL;
  "

  if (!dry_run) {
    result <- DBI::dbExecute(actual_con, sql_infra_to_species)
    linked_counts$infra_to_species <- result
    if (verbose) cli::cli_alert_success("Linked {result} infraspecific taxa to species")
  } else {
    count_sql <- "SELECT COUNT(*) FROM table_taxa WHERE tax_level = 'infraspecific' AND id_parent IS NULL;"
    count <- DBI::dbGetQuery(actual_con, count_sql)$count[1]
    linked_counts$infra_to_species <- count
    if (verbose) cli::cli_alert_info("Would link {count} infraspecific taxa to species")
  }

  cli::cli_h2("Summary")
  total_linked <- sum(unlist(linked_counts))
  action <- if (dry_run) "Would link" else "Linked"
  cli::cli_alert_info("{action} {total_linked} entries total:")
  cli::cli_ul(c(
    "Orders → Classes: {linked_counts$order_to_class}",
    "Families → Orders: {linked_counts$family_to_order}",
    "Genera → Families: {linked_counts$genus_to_family}",
    "Species → Genera: {linked_counts$species_to_genus}",
    "Infraspecific → Species: {linked_counts$infra_to_species}"
  ))

  return(as.data.frame(linked_counts))
}


#' Run Full Hierarchy Migration
#'
#' Runs all three phases of the hierarchy migration in order:
#' 1. Add id_parent column
#' 2. Create missing hierarchy entries
#' 3. Link existing entries
#'
#' @param con Database connection to taxa database
#' @param dry_run If TRUE, only show what would happen
#' @param create_backup If TRUE, create backup table first
#'
#' @return List with results from each phase
#' @export
run_hierarchy_migration <- function(con = NULL, dry_run = TRUE, create_backup = TRUE) {
  if (is.null(con)) {
    con <- call.mydb.taxa()
  }

  # Handle pool connections
  actual_con <- if (inherits(con, "Pool")) {
    pool::poolCheckout(con)
  } else {
    con
  }

  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  cli::cli_h1("Full Hierarchy Migration")

  if (dry_run) {
    cli::cli_alert_warning("DRY RUN MODE - No changes will be made")
  }

  results <- list()

  # Create backup if requested
  if (create_backup && !dry_run) {
    cli::cli_h2("Creating backup...")
    backup_name <- paste0("table_taxa_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    sql_backup <- glue::glue_sql("CREATE TABLE {`backup_name`} AS SELECT * FROM table_taxa", .con = actual_con)
    DBI::dbExecute(actual_con, sql_backup)
    cli::cli_alert_success("Backup created: {backup_name}")
  }

  # Phase 1: Add column
  cli::cli_h2("Phase 1: Add id_parent column")
  results$phase1 <- migration_add_id_parent_column(actual_con, dry_run = dry_run)

  # Phase 2: Create missing entries
  cli::cli_h2("Phase 2: Create missing hierarchy entries")
  results$phase2 <- migration_create_hierarchy_entries(actual_con, dry_run = dry_run)

  # Phase 3: Link entries
  cli::cli_h2("Phase 3: Link hierarchy")
  results$phase3 <- migration_link_hierarchy(actual_con, dry_run = dry_run)

  cli::cli_h1("Migration Complete")

  return(results)
}


#' Verify Hierarchy Integrity
#'
#' Checks the integrity of the id_parent hierarchy after migration.
#' Uses tax_level column to identify taxonomic levels.
#'
#' @param con Database connection to taxa database
#'
#' @return Data frame with verification results
#' @export
verify_hierarchy_integrity <- function(con = NULL) {
  if (is.null(con)) {
    con <- call.mydb.taxa()
  }

  # Handle pool connections
  actual_con <- if (inherits(con, "Pool")) {
    pool::poolCheckout(con)
  } else {
    con
  }

  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  cli::cli_h1("Verifying hierarchy integrity")

  checks <- list()

  # Check 1: Orphaned species (no genus parent)
  sql_orphan_species <- "
    SELECT COUNT(*) as n FROM table_taxa
    WHERE tax_level = 'species'
      AND id_parent IS NULL;
  "
  checks$orphan_species <- DBI::dbGetQuery(actual_con, sql_orphan_species)$n[1]

  # Check 2: Orphaned genera (no family parent)
  sql_orphan_genera <- "
    SELECT COUNT(*) as n FROM table_taxa
    WHERE tax_level = 'genus'
      AND id_parent IS NULL;
  "
  checks$orphan_genera <- DBI::dbGetQuery(actual_con, sql_orphan_genera)$n[1]

  # Check 3: Orphaned families (no order parent)
  sql_orphan_families <- "
    SELECT COUNT(*) as n FROM table_taxa
    WHERE tax_level = 'family'
      AND id_parent IS NULL;
  "
  checks$orphan_families <- DBI::dbGetQuery(actual_con, sql_orphan_families)$n[1]

  # Check 4: Orphaned orders (no class parent)
  sql_orphan_orders <- "
    SELECT COUNT(*) as n FROM table_taxa
    WHERE tax_level = 'order'
      AND id_parent IS NULL;
  "
  checks$orphan_orders <- DBI::dbGetQuery(actual_con, sql_orphan_orders)$n[1]

  # Check 5: Circular references
  sql_circular <- "
    SELECT COUNT(*) as n FROM table_taxa
    WHERE id_parent = idtax_n;
  "
  checks$circular_refs <- DBI::dbGetQuery(actual_con, sql_circular)$n[1]

  # Check 6: Invalid parent references
  sql_invalid_parent <- "
    SELECT COUNT(*) as n FROM table_taxa t1
    WHERE t1.id_parent IS NOT NULL
      AND NOT EXISTS (SELECT 1 FROM table_taxa t2 WHERE t2.idtax_n = t1.id_parent);
  "
  checks$invalid_parents <- DBI::dbGetQuery(actual_con, sql_invalid_parent)$n[1]

  # Check 7: Total with id_parent set
  sql_linked <- "SELECT COUNT(*) as n FROM table_taxa WHERE id_parent IS NOT NULL;"
  checks$total_linked <- DBI::dbGetQuery(actual_con, sql_linked)$n[1]

  # Check 8: Total taxa
  sql_total <- "SELECT COUNT(*) as n FROM table_taxa;"
  checks$total_taxa <- DBI::dbGetQuery(actual_con, sql_total)$n[1]

  # Check 9: Distribution by tax_level
  sql_by_level <- "SELECT tax_level, COUNT(*) as n FROM table_taxa GROUP BY tax_level ORDER BY tax_level;"
  level_dist <- DBI::dbGetQuery(actual_con, sql_by_level)

  cli::cli_h2("Verification Results")
  cli::cli_ul(c(
    "Total taxa: {checks$total_taxa}",
    "Taxa with id_parent: {checks$total_linked}",
    "Orphaned species: {checks$orphan_species}",
    "Orphaned genera: {checks$orphan_genera}",
    "Orphaned families: {checks$orphan_families}",
    "Orphaned orders: {checks$orphan_orders}",
    "Circular references: {checks$circular_refs}",
    "Invalid parent refs: {checks$invalid_parents}"
  ))

  cli::cli_h3("Distribution by tax_level")
  for (i in seq_len(nrow(level_dist))) {
    cli::cli_li("{level_dist$tax_level[i]}: {level_dist$n[i]}")
  }

  if (checks$circular_refs == 0 && checks$invalid_parents == 0) {
    cli::cli_alert_success("Hierarchy integrity verified - no critical issues found")
  } else {
    cli::cli_alert_warning("Critical issues detected - review results")
  }

  return(as.data.frame(checks))
}
