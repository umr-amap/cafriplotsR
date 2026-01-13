# Hierarchy Consistency Functions
#
# These functions ensure consistency between the flat taxonomic columns
# (tax_gen, tax_fam, tax_order, tax_famclass) and the hierarchical id_parent
# structure in table_taxa.
#
# The system uses a HYBRID approach:
# - Flat columns: Used for fast querying and backward compatibility
# - id_parent: Used for hierarchical navigation and tree structures
#
# These must stay in sync to maintain data integrity.


#' Check Hierarchy Consistency
#'
#' Validates that flat taxonomic columns match the hierarchy defined by id_parent.
#' Returns taxa where the flat columns don't match their parent entries.
#'
#' @param con Database connection to taxa database
#' @param fix Logical, if TRUE attempts to fix inconsistencies (default FALSE)
#' @param limit Integer, max number of inconsistencies to return (default 100)
#'
#' @return Data frame with inconsistent taxa, or NULL if all consistent
#'
#' @examples
#' \dontrun{
#' con <- call.mydb.taxa()
#'
#' # Check for inconsistencies
#' issues <- check_hierarchy_consistency(con)
#'
#' # Fix inconsistencies automatically
#' check_hierarchy_consistency(con, fix = TRUE)
#' }
#'
#' @export
check_hierarchy_consistency <- function(con = NULL, fix = FALSE, limit = 100) {

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

  cli::cli_h1("Checking hierarchy consistency")

  # SQL to find inconsistencies
  # Check each level: species->genus, genus->family, family->order, order->class

  inconsistencies <- list()

  # 1. Check species: tax_gen should match parent genus entry
  cli::cli_h2("Checking species → genus consistency")
  species_issues <- DBI::dbGetQuery(actual_con, sprintf("
    SELECT
      child.idtax_n,
      child.tax_gen as flat_genus,
      parent.tax_gen as parent_genus,
      child.tax_esp,
      child.tax_fam,
      'species_genus_mismatch' as issue_type
    FROM table_taxa child
    LEFT JOIN table_taxa parent ON child.id_parent = parent.idtax_n
    WHERE child.tax_level = 'species'
      AND child.id_parent IS NOT NULL
      AND child.tax_gen != parent.tax_gen
    LIMIT %d
  ", limit))

  if (nrow(species_issues) > 0) {
    cli::cli_alert_warning("Found {nrow(species_issues)} species with genus mismatch")
    inconsistencies$species_genus <- species_issues
  } else {
    cli::cli_alert_success("All species have consistent genus")
  }

  # 2. Check genus: tax_fam should match parent family entry
  cli::cli_h2("Checking genus → family consistency")
  genus_issues <- DBI::dbGetQuery(actual_con, sprintf("
    SELECT
      child.idtax_n,
      child.tax_gen,
      child.tax_fam as flat_family,
      parent.tax_fam as parent_family,
      'genus_family_mismatch' as issue_type
    FROM table_taxa child
    LEFT JOIN table_taxa parent ON child.id_parent = parent.idtax_n
    WHERE child.tax_level = 'genus'
      AND child.id_parent IS NOT NULL
      AND child.tax_fam != parent.tax_fam
    LIMIT %d
  ", limit))

  if (nrow(genus_issues) > 0) {
    cli::cli_alert_warning("Found {nrow(genus_issues)} genera with family mismatch")
    inconsistencies$genus_family <- genus_issues
  } else {
    cli::cli_alert_success("All genera have consistent family")
  }

  # 3. Check family: tax_order should match parent order entry
  cli::cli_h2("Checking family → order consistency")
  family_issues <- DBI::dbGetQuery(actual_con, sprintf("
    SELECT
      child.idtax_n,
      child.tax_fam,
      child.tax_order as flat_order,
      parent.tax_order as parent_order,
      'family_order_mismatch' as issue_type
    FROM table_taxa child
    LEFT JOIN table_taxa parent ON child.id_parent = parent.idtax_n
    WHERE child.tax_level = 'family'
      AND child.id_parent IS NOT NULL
      AND child.tax_order != parent.tax_order
    LIMIT %d
  ", limit))

  if (nrow(family_issues) > 0) {
    cli::cli_alert_warning("Found {nrow(family_issues)} families with order mismatch")
    inconsistencies$family_order <- family_issues
  } else {
    cli::cli_alert_success("All families have consistent order")
  }

  # 4. Check order: tax_famclass should match parent class entry
  cli::cli_h2("Checking order → class consistency")
  order_issues <- DBI::dbGetQuery(actual_con, sprintf("
    SELECT
      child.idtax_n,
      child.tax_order,
      child.tax_famclass as flat_class,
      parent.tax_famclass as parent_class,
      'order_class_mismatch' as issue_type
    FROM table_taxa child
    LEFT JOIN table_taxa parent ON child.id_parent = parent.idtax_n
    WHERE child.tax_level = 'order'
      AND child.id_parent IS NOT NULL
      AND child.tax_famclass != parent.tax_famclass
    LIMIT %d
  ", limit))

  if (nrow(order_issues) > 0) {
    cli::cli_alert_warning("Found {nrow(order_issues)} orders with class mismatch")
    inconsistencies$order_class <- order_issues
  } else {
    cli::cli_alert_success("All orders have consistent class")
  }

  # 5. Check infraspecific: tax_gen, tax_esp should match parent species
  cli::cli_h2("Checking infraspecific → species consistency")
  infra_issues <- DBI::dbGetQuery(actual_con, sprintf("
    SELECT
      child.idtax_n,
      child.tax_gen as flat_genus,
      child.tax_esp as flat_species,
      parent.tax_gen as parent_genus,
      parent.tax_esp as parent_species,
      child.tax_nam01,
      'infraspecific_species_mismatch' as issue_type
    FROM table_taxa child
    LEFT JOIN table_taxa parent ON child.id_parent = parent.idtax_n
    WHERE child.tax_level = 'infraspecific'
      AND child.id_parent IS NOT NULL
      AND (child.tax_gen != parent.tax_gen OR child.tax_esp != parent.tax_esp)
    LIMIT %d
  ", limit))

  if (nrow(infra_issues) > 0) {
    cli::cli_alert_warning("Found {nrow(infra_issues)} infraspecific taxa with species mismatch")
    inconsistencies$infraspecific_species <- infra_issues
  } else {
    cli::cli_alert_success("All infraspecific taxa have consistent species")
  }

  # Summary
  if (length(inconsistencies) == 0) {
    total_issues <- 0
  } else {
    total_issues <- sum(sapply(inconsistencies, function(x) if (is.data.frame(x)) nrow(x) else 0))
  }

  cli::cli_h2("Summary")
  if (total_issues == 0) {
    cli::cli_alert_success("No inconsistencies found! Hierarchy is consistent.")
    return(NULL)
  } else {
    cli::cli_alert_warning("Found {total_issues} total inconsistencies")

    if (fix) {
      cli::cli_h2("Fixing inconsistencies")
      fix_result <- fix_hierarchy_inconsistencies(actual_con, inconsistencies)
      return(fix_result)
    } else {
      cli::cli_alert_info("Run with fix = TRUE to automatically correct these")
      return(inconsistencies)
    }
  }
}


#' Fix Hierarchy Inconsistencies
#'
#' Updates flat columns to match the hierarchy defined by id_parent.
#'
#' @param con Database connection
#' @param inconsistencies List of inconsistencies from check_hierarchy_consistency
#'
#' @return Summary of fixes applied
#'
#' @keywords internal
fix_hierarchy_inconsistencies <- function(con, inconsistencies) {

  fixed_counts <- list()

  # Fix each type of inconsistency
  for (issue_type in names(inconsistencies)) {
    issues <- inconsistencies[[issue_type]]

    if (nrow(issues) == 0) next

    cli::cli_alert_info("Fixing {nrow(issues)} {issue_type} issues...")

    fixed_count <- 0

    tryCatch({
      DBI::dbExecute(con, "BEGIN;")

      if (issue_type == "species_genus") {
        # Update species: tax_gen to match parent genus
        sql <- sprintf("
          UPDATE table_taxa child
          SET tax_gen = parent.tax_gen
          FROM table_taxa parent
          WHERE child.id_parent = parent.idtax_n
            AND child.tax_level = 'species'
            AND child.tax_gen != parent.tax_gen
        ")
        fixed_count <- DBI::dbExecute(con, sql)

      } else if (issue_type == "genus_family") {
        # Update genus: tax_fam to match parent family
        sql <- sprintf("
          UPDATE table_taxa child
          SET tax_fam = parent.tax_fam
          FROM table_taxa parent
          WHERE child.id_parent = parent.idtax_n
            AND child.tax_level = 'genus'
            AND child.tax_fam != parent.tax_fam
        ")
        fixed_count <- DBI::dbExecute(con, sql)

      } else if (issue_type == "family_order") {
        # Update family: tax_order to match parent order
        sql <- sprintf("
          UPDATE table_taxa child
          SET tax_order = parent.tax_order
          FROM table_taxa parent
          WHERE child.id_parent = parent.idtax_n
            AND child.tax_level = 'family'
            AND child.tax_order != parent.tax_order
        ")
        fixed_count <- DBI::dbExecute(con, sql)

      } else if (issue_type == "order_class") {
        # Update order: tax_famclass to match parent class
        sql <- sprintf("
          UPDATE table_taxa child
          SET tax_famclass = parent.tax_famclass
          FROM table_taxa parent
          WHERE child.id_parent = parent.idtax_n
            AND child.tax_level = 'order'
            AND child.tax_famclass != parent.tax_famclass
        ")
        fixed_count <- DBI::dbExecute(con, sql)

      } else if (issue_type == "infraspecific_species") {
        # Update infraspecific: tax_gen, tax_esp to match parent species
        sql <- sprintf("
          UPDATE table_taxa child
          SET tax_gen = parent.tax_gen,
              tax_esp = parent.tax_esp
          FROM table_taxa parent
          WHERE child.id_parent = parent.idtax_n
            AND child.tax_level = 'infraspecific'
            AND (child.tax_gen != parent.tax_gen OR child.tax_esp != parent.tax_esp)
        ")
        fixed_count <- DBI::dbExecute(con, sql)
      }

      DBI::dbExecute(con, "COMMIT;")

      cli::cli_alert_success("Fixed {fixed_count} {issue_type} issues")
      fixed_counts[[issue_type]] <- fixed_count

    }, error = function(e) {
      tryCatch(DBI::dbExecute(con, "ROLLBACK;"), error = function(e2) {})
      cli::cli_alert_danger("Failed to fix {issue_type}: {e$message}")
      fixed_counts[[issue_type]] <- 0
    })
  }

  total_fixed <- sum(unlist(fixed_counts))
  cli::cli_alert_success("Fixed {total_fixed} total inconsistencies")

  return(fixed_counts)
}


#' Update Taxon Parent (with consistency check)
#'
#' Safely updates a taxon's parent (id_parent) while maintaining consistency
#' with flat taxonomic columns. This is the SAFE way to modify hierarchy.
#'
#' @param idtax_n Taxon ID to update
#' @param new_parent_id New parent taxon ID
#' @param con Database connection to taxa database
#' @param update_flat_columns Logical, if TRUE updates flat columns to match new parent (default TRUE)
#'
#' @return Logical, TRUE if successful
#'
#' @examples
#' \dontrun{
#' con <- call.mydb.taxa()
#'
#' # Move species to different genus (updates both id_parent and tax_gen)
#' update_taxon_parent(
#'   idtax_n = 12345,
#'   new_parent_id = 67890,
#'   con = con
#' )
#' }
#'
#' @export
update_taxon_parent <- function(idtax_n, new_parent_id, con = NULL,
                                update_flat_columns = TRUE) {

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

  # Get child taxon info
  child <- DBI::dbGetQuery(actual_con, sprintf(
    "SELECT * FROM table_taxa WHERE idtax_n = %d", idtax_n
  ))

  if (nrow(child) == 0) {
    stop("Taxon with idtax_n = ", idtax_n, " not found")
  }

  # Get new parent info
  parent <- DBI::dbGetQuery(actual_con, sprintf(
    "SELECT * FROM table_taxa WHERE idtax_n = %d", new_parent_id
  ))

  if (nrow(parent) == 0) {
    stop("Parent taxon with idtax_n = ", new_parent_id, " not found")
  }

  # Validate parent-child relationship
  child_level <- child$tax_level[1]
  parent_level <- parent$tax_level[1]

  valid_relationships <- list(
    infraspecific = "species",
    species = "genus",
    genus = "family",
    family = "order",
    order = c("class", "higher")
  )

  if (!is.null(valid_relationships[[child_level]])) {
    if (!parent_level %in% valid_relationships[[child_level]]) {
      stop(sprintf("Invalid parent-child relationship: %s cannot be child of %s",
                   child_level, parent_level))
    }
  }

  tryCatch({
    DBI::dbExecute(actual_con, "BEGIN;")

    if (update_flat_columns) {
      # Update both id_parent AND flat columns to maintain consistency

      if (child_level == "species") {
        # Update genus
        sql <- sprintf("
          UPDATE table_taxa
          SET id_parent = %d, tax_gen = '%s'
          WHERE idtax_n = %d
        ", new_parent_id, parent$tax_gen[1], idtax_n)

      } else if (child_level == "genus") {
        # Update family
        sql <- sprintf("
          UPDATE table_taxa
          SET id_parent = %d, tax_fam = '%s'
          WHERE idtax_n = %d
        ", new_parent_id, parent$tax_fam[1], idtax_n)

      } else if (child_level == "family") {
        # Update order
        sql <- sprintf("
          UPDATE table_taxa
          SET id_parent = %d, tax_order = '%s'
          WHERE idtax_n = %d
        ", new_parent_id, parent$tax_order[1], idtax_n)

      } else if (child_level == "order") {
        # Update class
        sql <- sprintf("
          UPDATE table_taxa
          SET id_parent = %d, tax_famclass = '%s'
          WHERE idtax_n = %d
        ", new_parent_id, parent$tax_famclass[1], idtax_n)

      } else if (child_level == "infraspecific") {
        # Update genus and species
        sql <- sprintf("
          UPDATE table_taxa
          SET id_parent = %d, tax_gen = '%s', tax_esp = '%s'
          WHERE idtax_n = %d
        ", new_parent_id, parent$tax_gen[1], parent$tax_esp[1], idtax_n)

      } else {
        # Just update id_parent
        sql <- sprintf("
          UPDATE table_taxa
          SET id_parent = %d
          WHERE idtax_n = %d
        ", new_parent_id, idtax_n)
      }

    } else {
      # Only update id_parent (not recommended - may cause inconsistency)
      cli::cli_alert_warning("Updating id_parent without flat columns - may cause inconsistency")
      sql <- sprintf("
        UPDATE table_taxa
        SET id_parent = %d
        WHERE idtax_n = %d
      ", new_parent_id, idtax_n)
    }

    result <- DBI::dbExecute(actual_con, sql)
    DBI::dbExecute(actual_con, "COMMIT;")

    cli::cli_alert_success("Updated taxon {idtax_n} parent to {new_parent_id}")
    return(TRUE)

  }, error = function(e) {
    tryCatch(DBI::dbExecute(actual_con, "ROLLBACK;"), error = function(e2) {})
    cli::cli_alert_danger("Failed to update parent: {e$message}")
    return(FALSE)
  })
}
