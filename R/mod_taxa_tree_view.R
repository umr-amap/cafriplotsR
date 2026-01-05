#' Taxa Tree View Module - UI
#'
#' UI component for displaying taxonomic hierarchy as a tree view
#'
#' @param id Module namespace ID
#'
#' @return A shiny tagList
#' @keywords internal
#' @export
mod_taxa_tree_view_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # CSS for tree styling
    shiny::tags$style(shiny::HTML("
      .taxonomy-tree {
        font-family: 'Consolas', 'Monaco', monospace;
        font-size: 14px;
        padding: 15px;
        background-color: #f8f9fa;
        border-radius: 8px;
        border: 1px solid #dee2e6;
      }
      .taxonomy-tree ul {
        list-style: none;
        padding-left: 25px;
        margin: 0;
      }
      .taxonomy-tree > ul {
        padding-left: 0;
      }
      .tree-node {
        padding: 4px 8px;
        margin: 2px 0;
        border-radius: 4px;
        display: flex;
        align-items: center;
        gap: 8px;
      }
      .tree-node:hover {
        background-color: #e9ecef;
      }
      .tree-node.current {
        background-color: #d4edda;
        border: 2px solid #28a745;
        font-weight: bold;
      }
      .tree-node .level-badge {
        font-size: 10px;
        padding: 2px 6px;
        border-radius: 10px;
        color: white;
        font-weight: 500;
        text-transform: uppercase;
      }
      .level-badge.class { background-color: #6f42c1; }
      .level-badge.order { background-color: #fd7e14; }
      .level-badge.family { background-color: #20c997; }
      .level-badge.genus { background-color: #17a2b8; }
      .level-badge.species { background-color: #28a745; }
      .level-badge.infraspecific { background-color: #6c757d; }
      .tree-connector {
        color: #6c757d;
        margin-right: 5px;
      }
      .taxon-name {
        flex-grow: 1;
      }
      .taxon-id {
        font-size: 11px;
        color: #6c757d;
        background-color: #e9ecef;
        padding: 1px 6px;
        border-radius: 3px;
      }
      .children-count {
        font-size: 11px;
        color: #6c757d;
        margin-left: 10px;
      }
      .hierarchy-path {
        margin-bottom: 15px;
        padding: 10px;
        background-color: #e9ecef;
        border-radius: 4px;
      }
      .hierarchy-path .path-item {
        display: inline-block;
        padding: 2px 8px;
        margin: 2px;
        border-radius: 3px;
        background-color: white;
        border: 1px solid #dee2e6;
      }
      .hierarchy-path .path-separator {
        color: #6c757d;
        margin: 0 2px;
      }
    ")),
    shiny::uiOutput(ns("tree_view_ui"))
  )
}


#' Taxa Tree View Module - Server
#'
#' Server logic for displaying taxonomic hierarchy
#'
#' @param id Module namespace ID
#' @param pool Reactive returning taxa database connection pool
#' @param selected_taxon Reactive returning selected taxon data from search module
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return NULL
#'
#' @keywords internal
#' @export
mod_taxa_tree_view_server <- function(id, pool, selected_taxon, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive to build hierarchy from selected taxon
    hierarchy <- shiny::reactive({
      shiny::req(selected_taxon())
      shiny::req(pool())

      taxon <- selected_taxon()
      if (is.null(taxon) || nrow(taxon) == 0 || is.null(taxon$idtax_n)) {
        return(NULL)
      }

      tryCatch({
        get_taxon_hierarchy(taxon$idtax_n, con = pool())
      }, error = function(e) {
        cli::cli_alert_warning("Could not load hierarchy: {e$message}")
        NULL
      })
    })

    # Reactive to count children
    children_counts <- shiny::reactive({
      shiny::req(selected_taxon())
      shiny::req(pool())

      taxon <- selected_taxon()
      if (is.null(taxon) || nrow(taxon) == 0 || is.null(taxon$idtax_n)) {
        return(NULL)
      }

      tryCatch({
        count_taxon_children(taxon$idtax_n, con = pool())
      }, error = function(e) {
        NULL
      })
    })

    # Main UI
    output$tree_view_ui <- shiny::renderUI({
      if (is.null(selected_taxon()) || nrow(selected_taxon()) == 0) {
        return(
          shiny::div(
            class = "alert alert-info",
            shiny::icon("info-circle"),
            " ",
            i18n()$t("Please select a taxon from the Browse & Search tab first")
          )
        )
      }

      h <- hierarchy()
      if (is.null(h)) {
        return(
          shiny::div(
            class = "alert alert-warning",
            shiny::icon("exclamation-triangle"),
            " ",
            i18n()$t("Could not load hierarchy for this taxon. The id_parent column may not be populated yet.")
          )
        )
      }

      taxon <- selected_taxon()
      counts <- children_counts()

      shiny::tagList(
        shiny::h4(
          shiny::icon("sitemap"),
          " ",
          i18n()$t("Taxonomic Hierarchy")
        ),

        # Breadcrumb path
        shiny::div(
          class = "hierarchy-path",
          shiny::strong(i18n()$t("Path: ")),
          build_breadcrumb_path(h, i18n)
        ),

        # Main tree view
        shiny::div(
          class = "taxonomy-tree",
          build_hierarchy_tree_html(h, taxon$idtax_n, i18n)
        ),

        # Children summary
        if (!is.null(counts) && counts["total"] > 0) {
          shiny::wellPanel(
            style = "margin-top: 15px; background-color: #d4edda;",
            shiny::h5(
              shiny::icon("chart-bar"),
              " ",
              i18n()$t("Children Summary")
            ),
            shiny::fluidRow(
              shiny::column(4, shiny::strong(i18n()$t("Total children:")), " ", counts["total"]),
              shiny::column(4, shiny::strong(i18n()$t("Genera:")), " ", counts["genera"]),
              shiny::column(4, shiny::strong(i18n()$t("Species:")), " ", counts["species"])
            ),
            if (counts["infraspecific"] > 0) {
              shiny::fluidRow(
                shiny::column(4, shiny::strong(i18n()$t("Infraspecific:")), " ", counts["infraspecific"]),
                shiny::column(4, shiny::strong(i18n()$t("Families:")), " ", counts["families"]),
                shiny::column(4, shiny::strong(i18n()$t("Orders:")), " ", counts["orders"])
              )
            }
          )
        }
      )
    })

    return(NULL)
  })
}


#' Build Breadcrumb Path HTML
#'
#' Creates a breadcrumb-style path showing the hierarchy.
#'
#' @param hierarchy Hierarchy list from get_taxon_hierarchy
#' @param i18n i18n translator
#'
#' @return Shiny tagList
#' @keywords internal
build_breadcrumb_path <- function(hierarchy, i18n) {
  levels <- c("class", "order", "family", "genus", "species", "infraspecific")
  path_items <- list()

  for (level in levels) {
    if (!is.null(hierarchy[[level]])) {
      entry <- hierarchy[[level]]
      is_current <- !is.null(hierarchy$current_level) && hierarchy$current_level == level

      path_items[[length(path_items) + 1]] <- shiny::span(
        class = paste("path-item", if (is_current) "current" else ""),
        style = if (is_current) "font-weight: bold; background-color: #d4edda; border-color: #28a745;" else "",
        entry$name
      )
    }
  }

  # Add separators between items
  if (length(path_items) > 0) {
    result <- list(path_items[[1]])
    if (length(path_items) > 1) {
      for (i in 2:length(path_items)) {
        result[[length(result) + 1]] <- shiny::span(class = "path-separator", "\u2192")
        result[[length(result) + 1]] <- path_items[[i]]
      }
    }
    return(shiny::tagList(result))
  }

  return(NULL)
}


#' Build Hierarchy Tree HTML
#'
#' Creates the nested tree view HTML structure.
#'
#' @param hierarchy Hierarchy list from get_taxon_hierarchy
#' @param current_id The ID of the currently selected taxon
#' @param i18n i18n translator
#'
#' @return Shiny tagList
#' @keywords internal
build_hierarchy_tree_html <- function(hierarchy, current_id, i18n) {
  levels <- c("class", "order", "family", "genus", "species", "infraspecific")
  level_labels <- c(
    class = "Class",
    order = "Order",
    family = "Family",
    genus = "Genus",
    species = "Species",
    infraspecific = "Infra"
  )

  # Build tree recursively
  build_node <- function(level_index) {
    if (level_index > length(levels)) return(NULL)

    level <- levels[level_index]
    entry <- hierarchy[[level]]

    if (is.null(entry)) {
      return(build_node(level_index + 1))
    }

    is_current <- entry$idtax_n == current_id

    # Build child content
    child_content <- build_node(level_index + 1)

    shiny::tags$ul(
      shiny::tags$li(
        shiny::div(
          class = paste("tree-node", if (is_current) "current" else ""),
          shiny::span(class = "tree-connector", if (level_index > 1) "\u251C\u2500\u2500" else ""),
          shiny::span(class = paste("level-badge", level), level_labels[level]),
          shiny::span(class = "taxon-name", entry$name),
          shiny::span(class = "taxon-id", paste("ID:", entry$idtax_n))
        ),
        child_content
      )
    )
  }

  build_node(1)
}
