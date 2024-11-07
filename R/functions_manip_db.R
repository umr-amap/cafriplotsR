

#' Launch shiny app for taxonomic standardization
#'
#' Tool for standardizing taxonomy according to the taxonomic backbone (step needed before importing data to databases)
#'
#' @return Launch in web browser the app
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @import shiny
#' @importFrom shinybusy use_busy_spinner show_spinner hide_spinner
#' @export
launch_stand_tax_app <- function() {

  app <- list(ui = ui <- fluidPage(
    titlePanel("Standardisation de noms d'especes vegetales pour l'Afrique tropicale - correction des erreurs orthographiques et homogeneisation de la synonymie"),

    # shinybusy::use_busy_bar(color = "#01DF01", height = "15px"),
    shinybusy::use_busy_spinner(spin = "fading-circle"),

    sidebarPanel(
      fileInput(inputId = "data1",
                label = "Ajoutez votre fichier de donnees - format excel (xlsx"),

      uiOutput("Box_match"),

      uiOutput("Box4"),

      uiOutput("Box5"),

      h4("   "),

      # radioButtons("dico.choix", "Utiliser le dictionnaire par defaut",
      #              c("oui","choisir un autre dictionnaire"),
      #              selected = "oui"),
      # textInput("champ.nom", "Quel est le nom du champs contenant les noms a standardiser?",
      #              value = "Code"),
      uiOutput("Dico")
      # fileInput(inputId = "data2", label = "Ajoutez votre dictionnaire - format csv")
    ),

    mainPanel(
      h4("Avancement et nombre de noms sans correspondance"),
      htmlOutput("summary"),

      verbatimTextOutput("test"),

      h4("Observations"),
      verbatimTextOutput("view"),

      h4("Propostion de correspondance par ressemblance orthographique"),
      verbatimTextOutput("list.match"),

      uiOutput("Box7"),

      uiOutput("Box9"),

      # uiOutput("BoxChoixNom"),

      uiOutput("Box1"),

      uiOutput("Box2"),

      uiOutput("Box3"),

      uiOutput("Box10"),

      uiOutput("Box6"),

      uiOutput("Box8"),

      h4("Nom choisi"),

      tableOutput("list.sp.m"),

      reactable::reactableOutput(outputId = "concern_rows"),

      tableOutput("table_chosen_sp"),

      uiOutput("Action1"),

      h4("   "),
      uiOutput("Action2"),

      # uiOutput("Action3"),

      h4("   "),
      uiOutput("go.back.one"),

      # Button
      h4("   "),
      downloadButton("downloadData", "Exporter une table incorporant les resultats")

    )
  ) ,

  server = function(input, output, session) {

    options(shiny.maxRequestSize=30*1024^2)

    # stop the serveur in the end of the session
    session$onSessionEnded(function() {
      stopApp()
    })

    # options(shiny.maxRequestSize=30*1024^2)

    # if(!exists("mydb")) call.mydb()

    val.nom.chosen <- reactiveValues(n = 1) ### Pointeur pour le nom a standardiser
    data.to.standardize.reac <- reactiveValues(df = NULL) ### reactive value destine a stocker la table des individus qui sera mise a jour
    original.data <- reactiveValues(df = NULL) ### reactive value destine a stocker la table des individus originale (pas mise a jour)
    original.list.name <- reactiveValues(df = NULL) ### reactive value destine a stocker un vecteur contenant la liste des noms original
    stand.list.name <- reactiveValues(df = NULL) ### vecteur des noms devant etre standardise
    list.match.reac <- reactiveValues(df = NULL)

    test_dat <- reactiveValues(df = NULL)

    DicoNames <- reactiveValues(df = NULL) ### reactive value destine a stocker la table des individus


    observeEvent(input$data1, {

      test_dat$df <- input$data1$datapath

      # file.rename(input$data1$datapath,
      #             paste(input$data1$datapath, ".xlsx", sep=""))

      # DATA <- readxl::read_xlsx("D:/MonDossierR/database.transects/individuals_to_be_added/", sheet =  1)

      DATA <- suppressWarnings(readxl::read_xlsx(input$data1$datapath, sheet =  1))


      # DATA <- readxl::read_excel(paste(input$data1$datapath, ".xlsx", sep=""), 1)

      # DATA <- readxl::read_excel(input$data1$datapath, 1)


      if(!any(colnames(DATA) == "id_data")) {
        DATA <-
          DATA %>%
          tibble::add_column(id_data = seq(1, nrow(DATA), 1))
      }

      ### Save table des individus original
      original.data$df <- DATA

      shinybusy::show_spinner()

      data(table_taxa_tb)
      DicoNames1 <-
        table_taxa_tb %>%
        mutate(tax_sp_level = ifelse(!is.na(tax_esp), paste(tax_gen, tax_esp), NA)) %>%
        mutate(tax_infra_level = ifelse(!is.na(tax_esp),
                                        paste0(tax_gen,
                                               " ",
                                               tax_esp,
                                               ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
                                               ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
                                               ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
                                               ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), "")),
                                        NA)) %>%
        mutate(tax_infra_level_auth = ifelse(!is.na(tax_esp),
                                             paste0(tax_gen,
                                                    " ",
                                                    tax_esp,
                                                    ifelse(!is.na(author1), paste0(" ", author1), ""),
                                                    ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
                                                    ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
                                                    ifelse(!is.na(author2), paste0(" ", author2), ""),
                                                    ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
                                                    ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), ""),
                                                    ifelse(!is.na(author3), paste0(" ", author3), "")),
                                             NA))

      # DicoNames1 <-
      #   dplyr::tbl(mydb, "table_taxa") %>%
      #   mutate(tax_sp_level = ifelse(!is.na(tax_esp), paste(tax_gen, tax_esp), NA)) %>%
      #   mutate(tax_infra_level = ifelse(!is.na(tax_esp),
      #                                   paste0(tax_gen,
      #                                          " ",
      #                                          tax_esp,
      #                                          ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
      #                                          ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
      #                                          ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
      #                                          ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), "")),
      #                                   NA)) %>%
      #   mutate(tax_infra_level_auth = ifelse(!is.na(tax_esp),
      #                                        paste0(tax_gen,
      #                                               " ",
      #                                               tax_esp,
      #                                               ifelse(!is.na(author1), paste0(" ", author1), ""),
      #                                               ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
      #                                               ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
      #                                               ifelse(!is.na(author2), paste0(" ", author2), ""),
      #                                               ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
      #                                               ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), ""),
      #                                               ifelse(!is.na(author3), paste0(" ", author3), "")),
      #                                        NA)) %>%
      #   dplyr::collect()

      DicoNames$df <- DicoNames1

      # shinybusy::update_busy_bar(100)
      shinybusy::hide_spinner()


    })

    ### Choix de la colonne de la table originale a standardiser
    output$Box_match = renderUI({
      req(input$data1)

      checkboxInput("authors", "Matching with authors name ?", FALSE)

    })

    ### Choix de la colonne de la table originale a standardiser
    output$Box4 = renderUI({
      req(input$data1)

      DATA.col <- original.data$df
      colnames(DATA.col) <- seq(1, ncol(original.data$df), 1)

      selectInput("champ.nom",
                  "Selectionner le nom de la colonne a standardiser",
                  c(colnames(original.data$df[, as.numeric(colnames(dplyr::select_if(DATA.col, is.character)))])))


    })



    observeEvent(input$champ.nom, { #  input$data1

      DATA <- original.data$df

      champ.nom <-
        input$champ.nom

      original.list.name$df <-
        DATA %>%
        dplyr::distinct(!!rlang::sym(champ.nom))


      ### Ajout de colonnes destinees a la mise a jour
      if (!any(colnames(DATA) == "found.name")) {
        ## Ajout de found.name si pas deja present

        DATA <- DATA %>% dplyr::mutate(found.name = "")

      }

      if(!any(colnames(DATA)=="ID.dico.name")) { ## Ajout de ID.dico.name si pas deja present

        DATA <- DATA %>% dplyr::mutate(ID.dico.name = 0)

      }else{

        DATA <- DATA %>%
          dplyr::mutate(ID.dico.name = as.numeric(ID.dico.name))

      }

      if(!any(colnames(DATA)=="ID.dico.name.good")) { ## Ajout de ID.dico.name.good si pas deja present

        DATA <- DATA %>% dplyr::mutate(ID.dico.name.good = 0)

      } else{

        DATA <- DATA %>%
          dplyr::mutate(ID.dico.name.good = as.numeric(ID.dico.name.good))

      }

      if(!any(colnames(DATA) == "corrected.name")) { ## Ajout de corrected.name si pas deja present

        DATA <- DATA %>% dplyr::mutate(corrected.name = "")

      }

      if (any(colnames(DATA) == "found.name")) {
        if (length(which(is.na(DATA$found.name))) > 0)
          DATA$found.name[which(is.na(DATA$found.name))] <-
            "" ### Si des observations de found.name sont en NA, les mettre en ""
      }

      ### Procedure de matching des noms --> noms correspondant a des noms du referentiel

      # select code and id_data
      DATA.selected <-
        DATA %>%
        dplyr::select(!!rlang::sym(champ.nom), id_data) %>%
        dplyr::rename(Code =!!rlang::sym(champ.nom)) %>%
        dplyr::filter(!is.na(Code))

      # matching based on full_name_no_auth

      if(!input$authors)
        join.table <-
        DATA.selected %>%
        dplyr::left_join(DicoNames$df %>%
                           dplyr::select(idtax_n, tax_infra_level),
                         by=c("Code" = "tax_infra_level"))

      if(input$authors)
        join.table <-
        dplyr::left_join(DATA.selected,
                         dplyr::select(DicoNames$df, idtax_n, tax_infra_level_auth),
                         by=c("Code" = "tax_infra_level_auth"))


      ### Matching with genus for those missing
      join.table_not_match <-
        join.table %>%
        dplyr::filter(is.na(idtax_n)) %>%
        dplyr::select(-idtax_n)

      join.table_genus <-
        join.table_not_match %>%
        left_join(DicoNames$df %>%
                    dplyr::filter(is.na(tax_esp), !is.na(tax_gen)) %>%
                    dplyr::select(idtax_n,
                                  idtax_good_n,
                                  tax_gen),
                  by = c("Code" = "tax_gen")) %>%
        filter(!is.na(idtax_n))

      ## Matching with family for those missing
      join.table_not_match <-
        join.table %>%
        dplyr::filter(is.na(idtax_n)) %>%
        dplyr::select(-idtax_n)

      join.table_family <-
        join.table_not_match %>%
        left_join(DicoNames$df %>%
                    dplyr::filter(is.na(tax_esp), is.na(tax_gen), !is.na(tax_fam)) %>%
                    dplyr::select(idtax_n,
                                  idtax_good_n,
                                  tax_fam),
                  by = c("Code" = "tax_fam")) %>%
        filter(!is.na(idtax_n))



      join.table <-
        join.table %>%
        left_join(join.table_genus %>%
                    dplyr::select(idtax_n, id_data) %>%
                    dplyr::rename(idtax_n_genus = idtax_n),
                  by = c("id_data" = "id_data")) %>%
        dplyr::mutate(idtax_n = ifelse(is.na(idtax_n), idtax_n_genus, idtax_n)) %>%
        dplyr::select(-idtax_n_genus)

      join.table <-
        join.table %>%
        left_join(join.table_family %>%
                    dplyr::select(idtax_n, id_data) %>%
                    dplyr::rename(idtax_n_fam = idtax_n),
                  by = c("id_data" = "id_data")) %>%
        dplyr::mutate(idtax_n = ifelse(is.na(idtax_n), idtax_n_fam, idtax_n)) %>%
        dplyr::select(-idtax_n_fam)

      join.table <-
        join.table %>%
        left_join(DicoNames$df %>%
                    dplyr::select(idtax_n, idtax_good_n, tax_gen, tax_infra_level, tax_infra_level_auth),
                  by = c("idtax_n" = "idtax_n"))

      # print(join.table)

      # Check how many exact match
      count_table <-
        join.table %>%
        dplyr::group_by(id_data) %>%
        dplyr::count() %>%
        dplyr::ungroup()

      # print(count_table)

      join.table <-
        join.table %>%
        left_join(count_table) %>%
        mutate(idtax_n = case_when(
          n > 1 ~ NA,
          n == 1 ~ idtax_n
          ))

      join.table %>%
        select(idtax_n, Code) %>%
        filter(is.na(idtax_n)) %>%
        print()

      # print(join.table)
      # if more than one matching by name, take the first
      join.table <-
        join.table %>%
        dplyr::group_by(id_data) %>%
        dplyr::summarise(idtax_n = dplyr::first(idtax_n),
                         idtax_good_n = dplyr::first(idtax_good_n),
                         Code = dplyr::first(Code))

      # excluding names with no matching
      join.table_sel <-
        join.table %>%
        dplyr::filter(!is.na(idtax_n)) %>%
        dplyr::select(Code, id_data, idtax_n, idtax_good_n)

      # combining matched names with DATA, adding found.name
      DATA <-
        DATA %>%
        dplyr::left_join(dplyr::select(join.table_sel, Code, id_data),
                         by=c("id_data"="id_data")) %>%
        dplyr::mutate(found.name = Code) %>%
        dplyr::select(-Code)

      # adding ID.dico.name
      DATA <-
        DATA %>%
        dplyr::left_join(dplyr::select(join.table_sel, idtax_n, id_data),
                         by = c("id_data" = "id_data")) %>%
        dplyr::mutate(ID.dico.name = ifelse(is.na(idtax_n), ID.dico.name, idtax_n)) %>%
        dplyr::select(-idtax_n)

      # adding ID.dico.name.good
      DATA <-
        DATA %>%
        dplyr::left_join(dplyr::select(join.table_sel, idtax_good_n, id_data),
                         by = c("id_data" = "id_data")) %>%
        dplyr::mutate(ID.dico.name.good = ifelse(is.na(idtax_good_n), ID.dico.name.good, idtax_good_n)) %>%
        dplyr::select(-idtax_good_n)

      # finding good names
      join.corrected.names <-
        dplyr::left_join(
          DATA %>%
            dplyr::select(ID.dico.name.good),
          DicoNames$df %>% dplyr::select(idtax_n, tax_infra_level),
          by = c("ID.dico.name.good" = "idtax_n")
        )

      # adding good names
      DATA$corrected.name <-
        join.corrected.names$tax_infra_level

      # adding detvalue
      # if (!any(colnames(DATA) == "detvalue")) {
      #   DATA <-
      #     DATA %>%
      #     tibble::add_column(detvalue = rep(0, nrow(.)))
      # }

      # %>%
      #   dplyr::mutate(detvalue=as.integer(detvalue))

      # DATA %>% select(ID.dico.name, original_tax_name, found.name) %>%
      #   dplyr::filter(ID.dico.name == 0) %>% print()

      test <-
        DATA %>%
        dplyr::filter(ID.dico.name == 0) %>%
        dplyr::distinct(!!rlang::sym(champ.nom), found.name) %>%
        dplyr::filter(!is.na(!!rlang::sym(champ.nom))) %>%
        dplyr::arrange(!!rlang::sym(champ.nom))

      if (nrow(test) > 0) {

        test <- test %>%
          dplyr::mutate(id_tax_search = seq(1, nrow(.), 1)) %>%
          dplyr::mutate(id_tax_search = as.character(id_tax_search))

      } else {

        test <- test %>%
          dplyr::mutate(id_tax_search = "")
      }
#
#       test <- DATA %>%
#         dplyr::filter(ID.dico.name == 0) %>%
#         dplyr::distinct(`Code -T`, found.name) %>%
#         dplyr::filter(!is.na(`Code -T`)) %>%
#         dplyr::arrange(`Code -T`)

      DATA <-
        DATA %>%
        dplyr::left_join(test %>%
                           dplyr::select(!!rlang::sym(champ.nom), id_tax_search))

      # DATA <-
      #   DATA %>%
      #   dplyr::left_join(test %>%
      #                      dplyr::select(`Code -T`, id_tax_search))

      test <-
        test %>%
        dplyr::rename(Code = 1)



      stand.list.name$df <-
        test %>%
        dplyr::select(Code, id_tax_search) %>%
        dplyr::filter(!is.na(Code))

      data.to.standardize.reac$df <-
        DATA

    })

    ### Une fois une colonne choisi, proposer de commencer la standardisation
    observeEvent(input$champ.nom, {

      output$Box5 = renderUI({
        req(input$champ.nom)
        req(input$data1)

        actionButton("do", "Commencer la standardisation")
      })
    }
    )

    output$summary <- renderUI({

      req(input$champ.nom)

      TXT2 <- paste("Nombre total de noms : ", nrow(unique(original.data$df[,input$champ.nom])))

      TXT <- paste("Nombre total de noms a standardiser : ", nrow(stand.list.name$df))

      TXT3 <- paste("NOMBRE DE NOMS RESTANT : ", nrow(stand.list.name$df)-(val.nom.chosen$n-1))

      HTML(paste(TXT2, TXT, TXT3, sep = '<br/>'))
    })


    ### Une fois la confirmation du debut de standardisation
    observeEvent(input$do, {

      output$Box7 = renderUI( {
        if (is.null(input$sector) || input$sector == "Selectionner un nom a standardiser") {
          return()
        }else{

          radioButtons("nbe_choice", "Nombre de noms similaires propose",
                       c("Top 10", "Top 20", "Top 30"),
                       selected = "Top 10")

        }
      })

      output$Box9 = renderUI( {
        if (is.null(input$sector) || input$sector == "Selectionner un nom a standardiser") {
          return()
        }else{

          radioButtons("sort", "Trier les propositions",
                       c("ressemblance","alphabetique"),
                       selected = "ressemblance")

        }
      })

      output$Box1 = renderUI({
        req(input$data1)
        # if(input$dico.choix!="oui")
          # req(input$data2)

        ### 'truc' pour eviter de mal afficher les noms avec caracteres speciaux

        id.names <- as.list(stand.list.name$df$id_tax_search)
        names(id.names) <- enc2utf8(dplyr::pull(stand.list.name$df, Code))

        # Encoding(names(id.names)) <-  "UTF-8"

        selectInput("sector","Selectionner un nom a standardiser",
                    choices=id.names, selected=val.nom.chosen$n)

      })
      #

      output$Box3 = renderUI( {
        if (is.null(input$sector) || input$sector == "Selectionner un nom a standardiser") {
          return()

        }else{

          radioButtons("choice.kind", "Chercher une correspondance dans",
                       c("Noms sans auteurs", "Noms avec auteurs"),
                       selected = ifelse(input$authors, "Noms avec auteurs", "Noms sans auteurs") )
        }
      })

      output$Box10 = renderUI( {
        if (is.null(input$sector) || input$sector == "Selectionner un nom a standardiser") {
          return()

        }else{

          radioButtons("choice.kind3", "Afficher les taxas infra-specifiques",
                       c("oui", "non"),
                       selected = "oui")

        }
      })



      output$Box6 = renderUI( {
        if (is.null(input$sector) || input$sector == "Choisir") {
          return()
        }else{
          radioButtons("choice.kind2", "Chercher une correspondance dans",
                       c("les taxa", "les genres", "les familles", "les classes"), #
                       selected = "les taxa")

        }
      })


      output$Box8 = renderUI( {
        if (is.null(input$sector) || input$sector == "Choisir") {
          return()
        }else{
          radioButtons("cf", "Identification certaine ou pas (cf)",
                       c("ok","cf"),
                       selected = "ok")

        }
      })


      output$Box2 = renderUI( {

        if (is.null(input$sector) || input$sector == "Selectionner un nom a standardiser") {
          return()
        }else{

          req(input$data1)

          Name1 <-
            stand.list.name$df %>%
            dplyr::filter(id_tax_search == as.numeric(input$sector)) %>%
            dplyr::pull(Code)


          if(input$choice.kind == "Noms sans auteurs" & input$choice.kind2 == "les taxa") {

            if(input$choice.kind3 == "oui") {

              dist. <-
                RecordLinkage::levenshteinSim(tolower(Name1), tolower(DicoNames$df$tax_infra_level))

            }

            if(input$choice.kind3 == "non") {

              subset_dico <-
                DicoNames$df %>%
                dplyr::filter(is.na(tax_rank01) & is.na(tax_nam01))

              dist. <-
                RecordLinkage::levenshteinSim(tolower(Name1), tolower(subset_dico$tax_infra_level))
            }
          }


          if(input$choice.kind == "Noms avec auteurs" & input$choice.kind2 == "les taxa") {

            if(input$choice.kind3 == "oui") {

              dist. <-
                RecordLinkage::levenshteinSim(tolower(Name1), tolower(DicoNames$df$tax_infra_level_auth))

            }

            if(input$choice.kind3 == "non") {

              subset_dico <-
                DicoNames$df %>%
                dplyr::filter(is.na(tax_rank01) & is.na(tax_nam01))

              dist. <-
                RecordLinkage::levenshteinSim(tolower(Name1), tolower(subset_dico$tax_infra_level_auth))
            }


          }

          if(input$choice.kind2 == "les genres") {

            dico_genus <-
              DicoNames$df %>%
              dplyr::filter(is.na(tax_esp))

            dist. <-
              RecordLinkage::levenshteinSim(tolower(Name1), tolower(dico_genus$tax_gen))

          }

          if(input$choice.kind2 == "les familles") {

            dico_family <-
              DicoNames$df %>%
              dplyr::filter(is.na(tax_esp), is.na(tax_gen), !is.na(tax_fam))

            dist. <-
              RecordLinkage::levenshteinSim(tolower(Name1), tolower(dico_family$tax_fam))

          }

          if(input$choice.kind2 == "les classes") {

            dico_classes <-
              DicoNames$df %>%
              dplyr::filter(is.na(tax_esp), is.na(tax_fam), is.na(tax_order)) %>%
              dplyr::filter(!is.na(tax_famclass))

            dist. <-
              RecordLinkage::levenshteinSim(tolower(Name1), tolower(dico_classes$tax_famclass))

          }

          if (input$nbe_choice == "Top 10")
            nbe.match <-
            10 ### Nombre de proposition pour la correspondance a afficher
          if (input$nbe_choice == "Top 20")
            nbe.match <-
            20 ### Nombre de proposition pour la correspondance a afficher
          if (input$nbe_choice == "Top 30")
            nbe.match <-
            30 ### Nombre de proposition pour la correspondance a afficher
          # if (input$nbe_choice == "Tous")
          #   nbe.match <-
          #   length(dist.[!is.na(dist.)]) ### Nombre de proposition pour la correspondance a afficher

          # test_dat$df <- dico_genus[order(dist., decreasing = T),]

          if (input$choice.kind2 == "les genres") {

            matches. <-
              dico_genus[order(dist., decreasing = T)[1:nbe.match],]

            # if(input$sort=="alphabetique")

          } else {

            if (input$choice.kind2 == "les familles") {

              matches. <-
                dico_family[order(dist., decreasing = T)[1:nbe.match],]

            } else {
              if (input$choice.kind2 == "les classes") {

              if(nrow(dico_classes) < nbe.match) {
                nbe.match_class <- nrow(dico_classes)

              } else {
                nbe.match_class <- nbe.match
              }

              matches. <-
                dico_classes[order(dist., decreasing = T)[1:nbe.match_class],]

            } else {

              if (input$choice.kind3 == "oui") {

                matches. <-
                  DicoNames$df[order(dist., decreasing = T)[1:nbe.match],]

              }

              if(input$choice.kind3 == "non") {

                subset_dico <-
                  DicoNames$df %>%
                  dplyr::filter(is.na(tax_rank01) & is.na(tax_nam01))

                matches. <-
                  subset_dico[order(dist., decreasing = T)[1:nbe.match],]

              }
            }
            }

          }

          matches. <-
            dplyr::select(matches., tax_infra_level,
                          tax_infra_level_auth,
                          idtax_n, idtax_good_n, tax_fam, tax_gen, tax_esp, tax_famclass)

          if (input$choice.kind == "Noms sans auteurs" &
              input$choice.kind2 == "les taxa") {

            selected.field <- "tax_infra_level"
            list.match <-
              matches. %>%
              dplyr::select(!!rlang::sym(selected.field), idtax_n)

          }

          if (input$choice.kind == "Noms avec auteurs" &
              input$choice.kind2 == "les taxa") {
            selected.field <- "tax_infra_level_auth"
            list.match <-
              matches. %>%
              dplyr::select(!!rlang::sym(selected.field), idtax_n)
            # list.match <- c(matches.[,c(selected.field)])
          }


          if(input$choice.kind2 == "les genres") {
            selected.field <- "tax_gen"
            list.match <-
              matches. %>%
              dplyr::select(!!rlang::sym(selected.field), idtax_n, tax_esp)
            # list.match <- c(unique(matches.[,c(selected.field)]))
          }

          if(input$choice.kind2 == "les familles") {
            selected.field <- "tax_fam"
            list.match <-
              matches. %>%
              dplyr::select(!!rlang::sym(selected.field), idtax_n, tax_esp)
            # list.match <- c(unique(matches.[,c(selected.field)]))
          }

          if(input$choice.kind2 == "les classes") {
            selected.field <- "tax_famclass"
            list.match <-
              matches. %>%
              dplyr::select(!!rlang::sym(selected.field), idtax_n, tax_esp)
            # list.match <- c(unique(matches.[,c(selected.field)]))
          }

          if(input$sort == "alphabetique")
            list.match <-
            list.match %>%
            arrange(!!rlang::sym(selected.field))

          list.match.reac$df <- list.match

          id.names <- as.list(seq(1, nrow(list.match.reac$df), 1))
          names(id.names) <- enc2utf8(dplyr::pull(list.match.reac$df, 1))


          selectInput("stock",
                      "Choisir le nom correct si present",
                      choices = id.names,
                      selected = 1)



        }
      })


      output$Action1 = renderUI({
        if (is.null(input$stock) ||
            input$stock == "Choisir le nom correct si present") {
          return()

        } else {
          Name2 <-
            list.match.reac$df %>%
            dplyr::slice(as.numeric(input$stock)) %>%
            dplyr::pull(1)

          actionButton(inputId = "confirm.name", label = paste("Choisir", Name2))

        }
      })


      output$Action2 = renderUI({
        if (is.null(input$stock) || input$stock == "Choisir le nom correct si present") {
          return()
        }else{
          actionButton(inputId="no.match", label="Pas de correspondance, passer au nom suivant")
        }
      })

      output$list.sp.m <- renderTable({ # renderTable

        req(input$data1)
        # if(input$dico.choix!="oui") req(input$data2)

        if (is.null(input$stock) || input$stock == " ") {
          return()
        }else{

          Name1 <-
            stand.list.name$df %>%
            dplyr::filter(id_tax_search == as.numeric(input$sector)) %>%
            dplyr::pull(Code)

          id_Name2 <-
            list.match.reac$df %>%
            dplyr::slice(as.numeric(input$stock)) %>%
            dplyr::pull(2)

          Name2 <-
            list.match.reac$df %>%
            dplyr::slice(as.numeric(input$stock)) %>%
            dplyr::pull(1)


          if (input$choice.kind == "Noms sans auteurs" &
              input$choice.kind2 == "les taxa")
            selected.field <- "tax_infra_level"
          if (input$choice.kind == "Noms avec auteurs" &
              input$choice.kind2 == "les taxa")
            selected.field <- "tax_infra_level_auth"
          if (input$choice.kind2 == "les genres")
            selected.field <- "tax_gen"
          if (input$choice.kind2 == "les familles")
            selected.field <- "tax_fam"
          if (input$choice.kind2 == "les classes")
            selected.field <- "tax_famclass"

          test_syn <-
            DicoNames$df %>%
            dplyr::filter(idtax_n == id_Name2) %>%
            dplyr::select(idtax_n, idtax_good_n)

          if(!is.na(test_syn$idtax_good_n)) if(test_syn$idtax_n != test_syn$idtax_good_n)
            syn_check <- TRUE
          if(!is.na(test_syn$idtax_good_n)) if(test_syn$idtax_n == test_syn$idtax_good_n)
            syn_check <- FALSE
          if(is.na(test_syn$idtax_good_n)) syn_check <- FALSE

          if(syn_check) {

            id_good <-
              DicoNames$df %>%
              # filter(!!rlang::sym(selected.field)==Name2) %>%
              dplyr::filter(idtax_n == !!id_Name2) %>%
              dplyr::select(idtax_good_n) %>%
              dplyr::pull()

            good_name <-
              DicoNames$df %>%
              dplyr::filter(idtax_n == !!id_good) %>%
              dplyr::select(tax_infra_level_auth) %>%
              dplyr::pull()

            data_to_print <-
              dplyr::tibble('Nom cherche' = Name1,
                            "Nom propose" = Name2,
                            "Est considere synonyme de" = good_name
              )


            # test_dat$df <-
            #   good_name

            data_to_print

            # print("Considere comme synonyme de")
            # print(DicoNames$df$full_name_no_auth[which(DicoNames$df$id_n==DicoNames$df$id_good_n[which(DicoNames$df$full_name_no_auth %in% Name2)])])
          }else{

            dplyr::tibble('Nom cherche'=Name1, "Nom propose" = Name2)

          }



        }
      })

      output$concern_rows <- reactable::renderReactable({

        Name1 <-
          stand.list.name$df %>%
          dplyr::filter(id_tax_search == as.numeric(input$sector)) %>%
          dplyr::pull(id_tax_search)


        reactable::reactable(
          data = data.to.standardize.reac$df %>%
            filter(id_tax_search == Name1),
          filterable = TRUE,
          highlight = TRUE,
          searchable = TRUE,
          pagination = FALSE,
          height = 300
        )

      })


      output$table_chosen_sp <- renderTable({ # renderTable

        req(input$data1)
        # if(input$dico.choix!="oui") req(input$data2)

        if (is.null(input$stock) || input$stock == " ") {
          return()
        }else{

          Name1 <-
            stand.list.name$df %>%
            dplyr::filter(id_tax_search == as.numeric(input$sector)) %>%
            dplyr::pull(Code)

          id_Name2 <-
            list.match.reac$df %>%
            dplyr::slice(as.numeric(input$stock)) %>%
            dplyr::pull(2)

          Name2 <-
            list.match.reac$df %>%
            dplyr::slice(as.numeric(input$stock)) %>%
            dplyr::pull(1)


          # if(input$choice.kind=="Noms sans auteurs" & input$choice.kind2=="les taxa")
          #   selected.field <- "tax_infra_level"
          # if(input$choice.kind=="Noms avec auteurs" & input$choice.kind2=="les taxa")
          #   selected.field <- "tax_infra_level_auth"
          # if(input$choice.kind2=="les genres")  selected.field <- "tax_gen"

          test_syn <-
            DicoNames$df %>%
            dplyr::filter(idtax_n == !!id_Name2) %>%
            dplyr::select(idtax_n, idtax_good_n)

          if(!is.na(test_syn$idtax_good_n)) if(test_syn$idtax_n != test_syn$idtax_good_n)
            syn_check <- TRUE
          if(!is.na(test_syn$idtax_good_n)) if(test_syn$idtax_n == test_syn$idtax_good_n)
            syn_check <- FALSE
          if(is.na(test_syn$idtax_good_n)) syn_check <- FALSE

          if(syn_check) {

            id_good <-
              DicoNames$df %>%
              dplyr::filter(idtax_n == !!id_Name2) %>%
              dplyr::select(idtax_good_n) %>%
              dplyr::pull()

            DicoNames$df %>%
              dplyr::filter(idtax_n == !!id_good) %>%
              # mutate(tax_infra_level_auth = ifelse(!is.na(tax_esp),
              #                                      paste0(tax_gen,
              #                                             " ",
              #                                             tax_esp,
              #                                             ifelse(!is.na(author1), paste0(" ", author1), ""),
              #                                             ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
              #                                             ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
              #                                             ifelse(!is.na(author2), paste0(" ", author2), ""),
              #                                             ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
              #                                             ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), ""),
              #                                             ifelse(!is.na(author3), paste0(" ", author3), "")),
              #                                      NA)) %>%
              dplyr::select(tax_fam,
                            tax_gen,
                            tax_esp,
                            tax_rank01,
                            tax_nam01,
                            tax_rank02,
                            author1,
                            author2)

          }else{

            DicoNames$df %>%
              dplyr::filter(idtax_n == id_Name2) %>%
              dplyr::select(tax_fam,
                            tax_gen,
                            tax_esp,
                            tax_rank01,
                            tax_nam01,
                            tax_rank02,
                            author1,
                            author2)

          }
        }
      })

      observeEvent(input$confirm.name, {

        req(input$data1)
        # if(input$dico.choix!="oui") req(input$data2)

        Name1 <-
          stand.list.name$df %>%
          dplyr::filter(id_tax_search == as.numeric(input$sector)) %>%
          dplyr::pull(id_tax_search)

        id_Name2 <-
          list.match.reac$df %>%
          dplyr::slice(as.numeric(input$stock)) %>%
          dplyr::pull(2)


        if (input$choice.kind == "Noms sans auteurs" &
            input$choice.kind2 == "les taxa")
          selected.field <- "tax_infra_level"
        if (input$choice.kind == "Noms avec auteurs" &
            input$choice.kind2 == "les taxa")
          selected.field <- "tax_infra_level_auth"
        if (input$choice.kind2 == "les genres")
          selected.field <- "tax_gen"
        if (input$choice.kind2 == "les familles")
          selected.field <- "tax_fam"
        if (input$choice.kind2 == "les classes")
          selected.field <- "tax_famclass"


        Name2 <-
          DicoNames$df %>%
          dplyr::filter(idtax_n == id_Name2) %>%
          dplyr::select(!!rlang::sym(selected.field)) %>%
          dplyr::pull()

        # complete found.name based on found_taxa
        data.to.standardize.reac$df <-
          data.to.standardize.reac$df %>%
          dplyr::mutate(found.name = replace(found.name, id_tax_search == Name1, Name2))

        Encoding(Name2) <- "UTF-8"

        found_taxa <-
          DicoNames$df %>%
          dplyr::filter(idtax_n == !!id_Name2)

        ### in case of duplicates in diconames
        if(nrow(found_taxa)>1) {
          found_taxa <-
            found_taxa %>%
            dplyr::group_by(!!rlang::sym(selected.field)) %>%
            dplyr::summarise(
              idtax_good_n = dplyr::first(idtax_good_n),
              tax_infra_level = dplyr::first(tax_infra_level),
              idtax_n = dplyr::first(idtax_n)
            )
        }


        # complete ID.dico.name based on found_taxa
        data.to.standardize.reac$df <-
          data.to.standardize.reac$df %>%
          dplyr::mutate(ID.dico.name = replace(ID.dico.name, id_tax_search == Name1, found_taxa$idtax_n))

        # complete ID.dico.name.good based on found_taxa
        data.to.standardize.reac$df <-
          data.to.standardize.reac$df %>%
          dplyr::mutate(ID.dico.name.good = replace(ID.dico.name.good, id_tax_search == Name1, found_taxa$idtax_good_n))


        # complete detvalue based on buttons cf
        # if(input$cf=="cf") {
        #   data.to.standardize.reac$df <-
        #     data.to.standardize.reac$df %>%
        #     dplyr::mutate(detvalue=replace(detvalue, id_tax_search==Name1, 3))
        # }


        ### checking for synonymies
        if(!is.na(found_taxa$idtax_good_n)) {
          if(found_taxa$idtax_n != found_taxa$idtax_good_n) {
            found_correc_taxa <-
              DicoNames$df %>%
              dplyr::filter(idtax_n == found_taxa$idtax_good_n)

            data.to.standardize.reac$df <-
              data.to.standardize.reac$df %>%
              dplyr::mutate(corrected.name = replace(corrected.name, id_tax_search == Name1, found_correc_taxa$tax_infra_level))
          }else{
            data.to.standardize.reac$df <-
              data.to.standardize.reac$df %>%
              dplyr::mutate(corrected.name = replace(corrected.name, id_tax_search == Name1, found_taxa$tax_infra_level))
          }
        }else{
          data.to.standardize.reac$df <-
            data.to.standardize.reac$df %>%
            dplyr::mutate(corrected.name = replace(corrected.name, id_tax_search == Name1, found_taxa$tax_infra_level))
        }

      })

      observeEvent(input$confirm.name, {
        Name1 <-
          stand.list.name$df %>%
          dplyr::filter(id_tax_search == as.numeric(input$sector)) %>%
          dplyr::pull(id_tax_search)
        # Encoding(Name1) <- "UTF-8"

        id.orig <- which(stand.list.name$df$id_tax_search == Name1)
        if(length(id.orig)>0) val.nom.chosen$n <- id.orig + 1

      }
      )

      # observeEvent(input$confirm.name, {
      #
      #   # values$n <- values$n + 1
      #   # if(values$n>nrow(stand.list.name$df))   values$n <- nrow(stand.list.name$df)
      # })

      observeEvent(input$no.match, {
        req(input$data1)
        # if(input$dico.choix!="oui") req(input$data2)

        Name1 <-
          stand.list.name$df %>%
          dplyr::filter(id_tax_search == as.numeric(input$sector)) %>%
          dplyr::pull(id_tax_search)
        # Encoding(Name1) <- "UTF-8"

        id.orig <- which(stand.list.name$df$id_tax_search == Name1)
        if(length(id.orig)>0) val.nom.chosen$n <- id.orig + 1

        data.to.standardize.reac$df <-
          data.to.standardize.reac$df %>%
          dplyr::mutate(ID.dico.name = replace(ID.dico.name, id_tax_search == Name1, 0))

        data.to.standardize.reac$df <-
          data.to.standardize.reac$df %>%
          dplyr::mutate(found.name = replace(found.name, id_tax_search == Name1, ""))

        data.to.standardize.reac$df <-
          data.to.standardize.reac$df %>%
          dplyr::mutate(ID.dico.name.good = replace(ID.dico.name.good, id_tax_search == Name1, 0))

        data.to.standardize.reac$df <-
          data.to.standardize.reac$df %>%
          dplyr::mutate(corrected.name = replace(corrected.name, id_tax_search == Name1, ""))

        # data.to.standardize.reac$df[which(data.to.standardize.reac$df[,input$champ.nom][[1]]==Name1),"found.name"] <- ""
        # data.to.standardize.reac$df[which(data.to.standardize.reac$df[,input$champ.nom][[1]]==Name1),"ID.dico.name"] <- 0
        # data.to.standardize.reac$df[which(data.to.standardize.reac$df[,input$champ.nom][[1]]==Name1),"ID.dico.name.good"] <- 0
        # data.to.standardize.reac$df[which(data.to.standardize.reac$df[,input$champ.nom][[1]]==Name1),"corrected.name"] <- ""
      })

      # Downloadable csv of selected dataset ----
      output$downloadData <- downloadHandler(


        filename = function() {

          paste("standardized.taxo",".csv", sep="")

        },
        content = function(file) {

          readr::write_excel_csv(data.to.standardize.reac$df %>%
                                   dplyr::select(-id_tax_search,
                                                 -id_data),
                                 file, na = "")

        }
      )

      output$go.back.one = renderUI({
        if (val.nom.chosen$n>1) {
          actionButton(inputId="go.back", label="Revenir au nom precedent")
        }
      })

      observeEvent(input$go.back, {
        # values$n <- values$n - 1
        Name1 <-
          stand.list.name$df %>%
          dplyr::filter(id_tax_search == as.numeric(input$sector)) %>%
          dplyr::pull(id_tax_search)

        id.orig <- which(stand.list.name$df[,"id_tax_search"]==Name1)
        #
        if(length(id.orig)==0) val.nom.chosen$n <- val.nom.chosen$n - 1
        if(length(id.orig)>0) val.nom.chosen$n <- id.orig - 1

      })
    }
    )


    # output$view <- renderPrint({
    #
    #   print(input$authors)
    #
    #
    # })



  }
  )

  shiny::runApp(app, launch.browser = TRUE)

}




#' Load the database
#'
#' Load the database and ask for password
#'
#' @param pass string
#' @param user string
#'
#' @importFrom rstudioapi askForPassword
#' @importFrom RPostgres Postgres
#' @return The database is loaded
#' #'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @export
call.mydb <- function(pass=NULL, user=NULL, offline = FALSE) {

  if(!exists("mydb")) {

    if(!offline) {

      if(is.null(pass))
        pass <- rstudioapi::askForPassword("Please enter your password")

      if(is.null(user))
        user <- rstudioapi::askForPassword("Please enter your user name")


      # mydb <- DBI::dbConnect(RPostgres::Postgres(),dbname = 'plots_transects',
      #                   host = 'localhost',
      #                   port = 5432, # or any other port specified by your DBA
      #                   user = 'postgres',
      #                   password = pass)

      mydb <- DBI::dbConnect(RPostgres::Postgres(),
                             dbname = 'plots_transects',
                             host = 'dg474899-001.dbaas.ovh.net',
                             port = 35699, # or any other port specified by your DBA
                             user = user,
                             password = pass)

    } else {

      # mydb <-
      #   list(data_liste_plots = dplyr::tbl(mydb, "data_liste_plots"),
      #        data_liste_sub_plots = dplyr::tbl(mydb, "data_liste_sub_plots"),
      #        table_colnam = dplyr::tbl(mydb, "table_colnam"),
      #        subplotype_list = dplyr::tbl(mydb, "subplotype_list"),
      #        specimens = dplyr::tbl(mydb, "specimens"),
      #        diconame = dplyr::tbl(mydb, "diconame"),
      #        data_individuals = dplyr::tbl(mydb, "data_individuals"),
      #        data_link_specimens = dplyr::tbl(mydb, "data_link_specimens"),
      #        subplotype_list = dplyr::tbl(mydb, "subplotype_list"),
      #        traitlist = dplyr::tbl(mydb, "traitlist"),
      #        data_traits_measures = dplyr::tbl(mydb, "data_traits_measures"))

      mydb <-
        list(data_liste_plots = data_liste_plots,
             data_liste_sub_plots = data_liste_sub_plots,
             table_colnam = table_colnam,
             subplotype_list = subplotype_list,
             specimens = specimens,
             diconame = diconame,
             data_individuals = data_individuals,
             data_link_specimens = data_link_specimens,
             subplotype_list = subplotype_list,
             traitlist = traitlist,
             data_traits_measures = data_traits_measures)


    }

    assign("mydb", mydb, envir = .GlobalEnv)

    # return(mydb)
  }
}


#' Load the taxonomic database
#'
#' Load the database and ask for password
#'
#' @param pass string
#' @param user string
#'
#' @return The database is loaded
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @export
call.mydb.taxa <- function(pass=NULL, user=NULL) {

  if(!exists("mydb_taxa")) {

    if(is.null(pass))
      pass <- rstudioapi::askForPassword("Please enter your password")

    if(is.null(user))
      user <- rstudioapi::askForPassword("Please enter your user name")

    mydb_taxa <- DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = 'rainbio',
      host = 'dg474899-001.dbaas.ovh.net',
      port = 35699,
      # or any other port specified by your DBA
      user = user,
      password = pass
    )

    assign("mydb_taxa", mydb_taxa, envir = .GlobalEnv)

  }
}



#' List of countries
#'
#' Provide list of countries where plots occur
#'
#' @return A tibble of all countries
#' @import dplyr
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @export
country_list <- function() {

  if(!exists("mydb")) call.mydb() # mydb <-

  nn <-
    dplyr::tbl(mydb, "data_liste_plots")
  nn <-
    dplyr::group_by(nn, country) %>%
    summarise(n = n())
  nn <-
    nn %>%
    collect() %>%
    dplyr::mutate(n = as.integer(n)) %>%
    arrange(country)

  # %>%
  #   dplyr::group_by(country) %>%
  #   dplyr::count() %>%
  #   dplyr::mutate(n=as.integer(n)) %>%
  #   dplyr::collect()

  # dbDisconnect(mydb)

  return(nn)
}



#' List of province
#'
#' Provide list of province where plots occur
#' @param country string
#'
#' @return A tibble of all province
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @importFrom rlang enquo
#' @export
province_list <- function(country=NULL) {
  if(!exists("mydb")) call.mydb()

  nn <-
    dplyr::tbl(mydb, "data_liste_plots")

  if(!is.null(country)) {

    var <- rlang::enquo(country)

    nn <-
      nn %>%
      dplyr::filter(country == !!var)
  }

  nn <-
    nn %>%
    dplyr::group_by(province) %>%
    dplyr::collect() %>%
    dplyr::count() %>%
    dplyr::mutate(n = as.integer(n))

  # dbDisconnect(mydb)

  return(nn)
}



#' List of method
#'
#' Provide list of method where plots occur
#'
#' @return A tibble of all method
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @importFrom stringr str_squish
#'
#' @export
method_list <- function() {

  if(!exists("mydb")) call.mydb()

  nn <-
    dplyr::tbl(mydb, "methodslist") %>%
    collect() 
  
  # dbDisconnect(mydb)
  return(nn)
}




#' Add a method in method list
#'
#' Add method and associated descriptors in method list table
#'
#' @return nothing
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_method string value with new method descriptors, avoid space
#' @param new_description_method
#'
#'
#' @export
add_method <- function(new_method = NULL,
                       new_description_method = NULL) {

  if(is.null(new_method)) stop("define new method")

  if(!exists("mydb")) call.mydb()

  new_data_renamed <- tibble(
    method = new_method,
    description_method = ifelse(is.null(new_description_method), NA, new_description_method)
  )

  print(new_data_renamed)

  Q <- utils::askYesNo("confirm adding this metho?")

  if(Q) DBI::dbWriteTable(mydb, "methodslist", new_data_renamed, append = TRUE, row.names = FALSE)

}





#' List of trait and features potentially liked to individual
#'
#' Provide list of traits and features available
#'
#' @return A tibble of all traits and features that can be linked to individual
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @export
traits_list <- function(id_trait = NULL) {

  if (!exists("mydb")) call.mydb()

  all_colnames_ind <-
    try_open_postgres_table(table = "traitlist", con = mydb) %>%
    dplyr::select(trait,
                  id_trait,
                  traitdescription,
                  maxallowedvalue,
                  minallowedvalue,
                  expectedunit,
                  valuetype)

  if (is.null(id_trait)) {

    all_colnames_ind <- all_colnames_ind %>%
      dplyr::collect()

  } else {

    all_colnames_ind <- all_colnames_ind %>%
      filter(id_trait == !!id_trait) %>%
      dplyr::collect()

  }

  return(all_colnames_ind)
}



#' List, extract map selected plots
#'
#' Provide and map list of selected plots, including associated traits for individuals
#'
#' @return A tibble of all plots or individuals
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param team_lead string fuzzy person name to look for
#' @param plot_name string fuzzy plot name to look for
#' @param tag numeric exact tag number of the plot
#' @param country string fuzzy country name to look for
#' @param province string fuzzy province name to look for
#' @param locality_name string fuzzy locality_name name to look for
#' @param method stringfuzzy method name to look for
#' @param date_y integer year of collect
#' @param extract_individuals logical whether individuals instead of plot list as output
#' @param map logical whether map whould be produced
#' @param zoom integer positive values indicating zoom level for ggplot style map
#' @param label logical for ggplot map, whether adding label or not
#' @param try_optimal_label logical if map is ggplot
#' @param map_type string mapview or ggplot, mapview by default
#' @param id_individual numeric id of individual to be extracted
#' @param id_plot numeric id of plot to be extracted
#' @param id_specimen numeric id of specimen linked to individuals
#' @param show_multiple_census logical whether multiple census should be shown, by default FALSE
#' @param remove_ids logical remove all ids columns, by default TRUE
#' @param collapse_multiple_val logical whether multiple traits measures should be collapsed (resulting values as character, separated by dash)
#' @param extract_traits whether species level traits should be extracted as well
#' @param extract_individual_features whether individual level trait or features should be extracted
#' @param traits_to_genera if species-level traits should be extrapolated to genus level, by default is FALSE
#' @param wd_fam_level logical, if wood density should be given at family level or not NOT YET FULLY AVAILABLE
#' @param include_liana logical, if liana should be included, by default FALSE
#'
#' @importFrom DBI dbSendQuery dbFetch dbClearResult dbWriteTable
#' @importFrom stringr str_flatten str_trim str_extract
#' @importFrom date as.date
#' @importFrom tidyselect vars_select_helpers
#' @importFrom BIOMASS correctCoordGPS
#' @importFrom glue glue_sql
#'
#' @return A tibble of plots or individuals if extract_individuals is TRUE
#'
#' @export
query_plots <- function(team_lead = NULL,
                        plot_name = NULL,
                        tag = NULL,
                        country = NULL,
                        province = NULL,
                        locality_name = NULL,
                        method = NULL,
                        date_y = NULL,
                        extract_individuals = FALSE,
                        map = FALSE,
                        zoom = 0,
                        label = T,
                        try_optimal_label = F,
                        map_type = "mapview",
                        id_individual = NULL,
                        id_plot = NULL,
                        id_tax = NULL,
                        id_specimen = NULL,
                        show_multiple_census = FALSE,
                        show_all_coordinates = FALSE,
                        remove_ids = TRUE,
                        collapse_multiple_val = FALSE,
                        extract_traits = TRUE,
                        extract_individual_features = TRUE,
                        traits_to_genera = FALSE,
                        wd_fam_level = FALSE,
                        include_liana = FALSE,
                        extract_subplot_features = TRUE) {

  if (!exists("mydb")) call.mydb()

  if (!is.null(id_individual) | !is.null(id_specimen))
  {

    if (!is.null(id_specimen)) {

      cli::cli_rule(left = "Extracting individuals linked to specimens")

      tbl <- "data_link_specimens"
      sql <- glue::glue_sql("SELECT * FROM {`tbl`} WHERE id_specimen IN ({vals*})",
                            vals = id_specimen, .con = mydb)

      res <- func_try_fetch(con = mydb, sql = sql)

      if (nrow(res) == 0) {
        stop("No individuals in the database linked to this specimen")
      }

      if (!is.null(id_individual)) cli::cli_alert_info("id_individual provided is not considered and individuals linked to id_specimen is used instead")

      id_individual <- res$id_n

    }

    cli::cli_rule(left = "Extracting from queried individuals - id_individual")
    extract_individuals <- TRUE

    if(!is.null(id_plot))
      cli::cli_alert_warning("id_plot not null replaced by id_plot of the id_individuals")

    tbl <- "data_individuals"
    sql <- glue::glue_sql("SELECT * FROM {`tbl`} WHERE id_n IN ({vals*})",
                         vals = id_individual, .con = mydb)

    res <- func_try_fetch(con = mydb, sql = sql)

    id_plot <-
      res %>%
      dplyr::select(id_table_liste_plots_n) %>%
      pull()

  }

  if (!is.null(id_tax))
  {

    cli::cli_rule(left = "Extracting from queried taxa - idtax_n")
    extract_individuals <- TRUE

    if(!is.null(id_plot))
      cli::cli_alert_warning("id_plot not null replaced by id_plot where idtax_n are found")

    id_plot <-
      merge_individuals_taxa(id_tax = id_tax) %>%
      pull(id_table_liste_plots_n)

    # id_plot <-
    #   tbl(mydb, "data_individuals") %>%
    #   query_tax_all(id_search = idtax, extract_individuals = T, verbose = FALSE, simple_ind_extract = T) %>%
    #   pull(id_table_liste_plots_n)

  }

  if (is.null(id_plot)) {

    query <- 'SELECT * FROM data_liste_plots WHERE MMM'

      # query <- gsub(pattern = "MMM", replacement = paste0(" team_leader ILIKE '%", team_lead, "%' AND MMM"), x=query)

    # query <- "SELECT * FROM data_liste_plots WHERE  team_leader ILIKE '%Dauby%' AND country IN ('Gabon', 'Cameroun')"

    if (!is.null(country)) {

      id_liste_plots_match <- vector('list', length(country))
      for (i in 1:length(country)) {

        query_country <-
          paste0("SELECT * FROM table_countries WHERE country ILIKE '%", country[i], "%'")

        rs_country <- func_try_fetch(con = mydb, sql = query_country)
        rs_country <- dplyr::as_tibble(rs_country)

        id_liste_plots_match[[i]] <- rs_country$id_country
      }

      query <-
        gsub(
          pattern = "MMM",
          replacement = paste0("id_country IN ('", paste(unlist(id_liste_plots_match), collapse = "', '"), "') AND MMM"),
          x = query
        )
    }

    if (!is.null(locality_name)) {
      if (length(locality_name) == 1) {
        query <-
          gsub(
            pattern = "MMM",
            replacement = paste0(" locality_name ILIKE '%", locality_name, "%' AND MMM"),
            x = query
          )
      } else {

        id_liste_plots_match <- vector('list', length(locality_name))
        for (i in 1:length(locality_name)) {

          query_loc <-
            paste0("SELECT * FROM data_liste_plots WHERE locality_name ILIKE '%", locality_name[i], "%'")

          res_loc <- func_try_fetch(con = mydb, sql = query_loc)

          # rs_loc <- DBI::dbSendQuery(mydb, query_loc)
          # res_loc <- DBI::dbFetch(rs_loc)
          # DBI::dbClearResult(rs_loc)
          # res_loc <- dplyr::as_tibble(res_loc)
          id_liste_plots_match[[i]] <- res_loc$id_liste_plots
        }

        query <-
          gsub(
          pattern = "MMM",
          replacement = paste0(
            "id_liste_plots IN ('",
            paste(unlist(id_liste_plots_match), collapse = "', '"),
            "') AND MMM"
          ),
          x = query
        )
      }
    }

    if (!is.null(province)) {
      if (length(province) == 1) {
        query <-
          gsub(
            pattern = "MMM",
            replacement = paste0(" province ILIKE '%", province, "%' AND MMM"),
            x = query
          )
      } else{
        query <-
          gsub(
            pattern = "MMM",
            replacement = paste0(
              "province IN ('",
              paste(province, collapse = "', '"),
              "') AND MMM"
            ),
            x = query
          )
      }
    }

    if (!is.null(method)) {
      query_method <- 'SELECT * FROM methodslist WHERE MMM'
      if (length(method) == 1) {
        query_method <-
          gsub(
            pattern = "MMM",
            replacement = paste0(" method ILIKE '%", method, "%' AND MMM"),
            x = query_method
          )

      } else {
        query_method <-
          gsub(
            pattern = "MMM",
            replacement = paste0("method IN ('", paste(method, collapse = "', '"), "') AND MMM"),
            x = query_method
          )
      }

      query_method <-
        gsub(pattern = "AND MMM", replacement = "", query_method)

      res_meth <- func_try_fetch(con = mydb, sql = query_method)

      # rs_meth <- DBI::dbSendQuery(mydb, query_method)
      # res_meth <- DBI::dbFetch(rs_meth)
      # DBI::dbClearResult(rs_meth)
      # res_meth <- dplyr::as_tibble(res_meth)

      if (nrow(res_meth) == 0) {
        warning("no method selected!")

      } else {
        cli::cli_alert_info("method(s) selected {res_meth}")

        query <-
          gsub(
            pattern = "MMM",
            replacement = paste0(
              "id_method IN ('",
              paste(res_meth$id_method, collapse = "', '"),
              "') AND MMM"
            ),
            x = query
          )
      }
    }

    if (!is.null(plot_name)) {
      if (length(plot_name) == 1) {
        query <-
          gsub(
            pattern = "MMM",
            replacement = paste0(" plot_name ILIKE '%", plot_name, "%' AND MMM"),
            x = query
          )
      } else {
        query <-
          gsub(
            pattern = "MMM",
            replacement = paste0(
              "plot_name IN ('",
              paste(plot_name, collapse = "', '"),
              "') AND MMM"
            ),
            x = query
          )
      }
    }

    query <- gsub(pattern = "AND MMM", replacement = "", query)

    if(grepl("WHERE MMM", query)) query <- gsub(pattern = " WHERE MMM", replacement = "", query)

    res <- func_try_fetch(con = mydb, sql = query)

    res <-
      res %>%
      dplyr::select(-id_old)


  } else {

    cli::cli_rule(left = "Extracting from queried plot - id_plot")

    res <-
      dplyr::tbl(mydb, "data_liste_plots") %>%
      dplyr::filter(id_liste_plots %in% id_plot) %>%
      dplyr::collect() %>%
      dplyr::select(-id_old)

  }

  ## link method
  res <-
    res %>%
    dplyr::select(-method) %>% # remove old method
    dplyr::left_join(dplyr::tbl(mydb, "methodslist") %>%
                       dplyr::collect(),
                     by = c("id_method" = "id_method")) %>%
    dplyr::relocate(method, .after = data_provider)

  ## link country
  res <-
    res %>%
    dplyr::select(-country) %>% # remove old method
    dplyr::left_join(dplyr::tbl(mydb, "table_countries") %>%
                       dplyr::collect(),
                     by = c("id_country" = "id_country")) %>%
    # dplyr::rename(country = colnam) %>%
    dplyr::relocate(country, .after = additional_people)

  if (extract_subplot_features & nrow(res) > 0) {

    all_subplots <- query_subplots(ids_plots =  res$id_liste_plots,
                                   verbose = FALSE)

    id_plots_filtered <- c()
    if (!is.null(team_lead)) {

      id_liste_plots_match <-
        .link_colnam(
        data_stand = tibble(colnam = team_lead),
        column_searched = "colnam",
        column_name = "colnam",
        id_field = "id_colnam",
        id_table_name = "id_table_colnam",
        db_connection = mydb,
        table_name = "table_colnam"
      )

      if (!exists("id_liste_plots_match")) {
        cli::cli_alert_warning("no team_leader found based on team_lead provided")
      } else {

        ids_team_lead <-
          id_liste_plots_match$id_colnam

        id_plots_filtered <-
          all_subplots$all_subplots %>%
          filter(type == "team_leader") %>%
          filter(typevalue %in% ids_team_lead) %>%
          pull(id_table_liste_plots)

        if (nrow(id_plots_filtered) == 0)
          cli::cli_alert_danger("No plot found for this team_leader")

        all_subplots$all_subplot_pivot <-
          all_subplots$all_subplot_pivot %>%
          filter(id_table_liste_plots %in% id_plots_filtered)

        all_subplots$all_subplots <-
          all_subplots$all_subplots %>%
          filter(id_table_liste_plots %in% id_plots_filtered)
      }
    }

    if (any(!is.na(all_subplots$census_features))) {
      census_features <- all_subplots$census_features

      census_features <-
        census_features %>%
        dplyr::left_join(res %>% dplyr::select(plot_name, id_liste_plots),
                         by = c("id_table_liste_plots" = "id_liste_plots")) %>%
        dplyr::relocate(plot_name, .before = year)
    }

    res <- rm_field(res, field = c("additional_people", "team_leader"))

    if (any(!is.na(all_subplots$all_subplot_pivot)))
      res <-
      res %>%
      dplyr::left_join(all_subplots$all_subplot_pivot,
                       by = c("id_liste_plots" = "id_table_liste_plots"))

    if (any(names(res) == "data_manager"))
      res <- res %>%
      dplyr::relocate(data_manager, .after = plot_name)

    if (any(names(res) == "principal_investigator"))
    res <- res %>%
      dplyr::relocate(principal_investigator, .after = plot_name)

    if (any(names(res) == "additional_people"))
      res <- res %>%
      dplyr::relocate(additional_people, .after = plot_name)

    if (any(names(res) == "team_leader"))
      res <- res %>%
      dplyr::relocate(team_leader, .after = plot_name)

    if(show_all_coordinates) {

      if (any(!is.na(all_subplots$all_subplots))) {
        all_ids_subplot_coordinates <-
          all_subplots$all_subplots %>%
          filter(grepl("ddlon", type) | grepl("ddlat", type))
      } else {
        all_ids_subplot_coordinates <- tibble()
      }

      if(nrow(all_ids_subplot_coordinates) > 0) {

        cli::cli_alert_info('Extracting coordinates')

        all_coordinates_subplots_rf <-
          all_ids_subplot_coordinates  %>%
          mutate(
            coord2 = unlist(lapply(strsplit(type, "_"), function(x)
              x[length(x)])),
            coord1 = unlist(lapply(strsplit(type, "_"), function(x)
              x[length(x) - 1])),
            coord3 = unlist(lapply(strsplit(type, "_"), function(x)
              x[1])),
            coord4 = unlist(lapply(strsplit(type, "_"), function(x)
              x[2]))
          ) %>%
          dplyr::select(coord1,
                        coord2,
                        coord3,
                        coord4,
                        type,
                        typevalue,
                        id_sub_plots,
                        id_table_liste_plots) %>%
          arrange(coord2) %>%
          left_join(res %>% select(id_liste_plots, method),
                    by = c("id_table_liste_plots" = "id_liste_plots"))

        all_coordinates_subplots_rf <-
          all_coordinates_subplots_rf %>%
          filter(coord4 == "plot")

        if (nrow(all_coordinates_subplots_rf) > 0) {
          all_plots_coord <- unique(all_coordinates_subplots_rf$id_table_liste_plots)

          coordinates_subplots_plot <- vector('list', length(all_plots_coord))
          coordinates_subplots_plot_sf <- vector('list', length(all_plots_coord))
          for (j in 1:length(all_plots_coord)) {

            grouped_coordinates <-
              all_coordinates_subplots_rf %>%
              dplyr::select(-type) %>%
              dplyr::filter(id_table_liste_plots == all_plots_coord[j]) %>%
              group_by(coord1, coord2, coord3, id_table_liste_plots) %>%
              summarise(typevalue = mean(typevalue),
                        id_sub_plots = stringr::str_c(id_sub_plots,
                                                      collapse = ", ")) %>%
              ungroup()

            coordinates_subplots <-
              tidyr::pivot_wider(grouped_coordinates,
                                 names_from = coord3,
                                 values_from = c(typevalue, id_sub_plots))

            if (nrow(coordinates_subplots) > 0) {

              coordinates_subplots <-
                coordinates_subplots %>%
                mutate(coord1 = as.numeric(coord1),
                       coord2 = as.numeric(coord2))

              coordinates_subplots <-
                coordinates_subplots %>%
                mutate(Xrel = coord1 - min(coordinates_subplots$coord1),
                       Yrel = coord2 - min(coordinates_subplots$coord2))

                cor_coord <-
                  suppressWarnings(BIOMASS::correctCoordGPS(
                    longlat = coordinates_subplots[, c("typevalue_ddlon", "typevalue_ddlat")],
                    rangeX = c(0, dist(range(as.numeric(coordinates_subplots$coord1)))),
                    rangeY = c(0, dist(range(as.numeric(coordinates_subplots$coord2)))),
                    coordRel = coordinates_subplots %>%
                      dplyr::select(Xrel, Yrel),
                    drawPlot = F,
                    rmOutliers = T
                  ))

                poly_plot <- sf::st_as_sf(cor_coord$polygon)
                sf::st_crs(poly_plot) <- cor_coord$codeUTM

                poly_plot <- sf::st_transform(poly_plot, 4326)

                poly_plot <-
                  poly_plot %>%
                  dplyr::mutate(id_liste_plots = all_plots_coord[j])
                # %>%
                #   left_join(res %>%
                #               dplyr::select(plot_name, id_liste_plots),
                #             by = c("plot_name" = "plot_name"))

                coordinates_subplots_plot_sf[[j]] <-
                  poly_plot


            } else {

              cli::cli_alert_danger("No coordinates for {all_plots_coord[j]} available")


            }

            coordinates_subplots_plot[[j]] <-
              coordinates_subplots

          }

          coordinates_subplots <-
            bind_rows(coordinates_subplots_plot)

          coordinates_subplots_plot_sf <-
            do.call('rbind', coordinates_subplots_plot_sf)

          coordinates_subplots_plot_sf <-
            coordinates_subplots_plot_sf %>%
            left_join(res %>% dplyr::select(id_liste_plots, plot_name),
                      by = c("id_liste_plots" = "id_liste_plots"))

          coordinates_subplots <-
            coordinates_subplots %>%
            left_join(res %>% dplyr::select(id_liste_plots, plot_name),
                      by = c("id_table_liste_plots" = "id_liste_plots"))

        }



      } else {

        show_all_coordinates <- FALSE
        cli::cli_alert_danger("No coordinates for quadrat available")

      }

    }

    if (length(id_plots_filtered) > 0)
      res <-
      res %>% filter(id_liste_plots %in% id_plots_filtered)

  }

  if (nrow(res) == 0)
    stop("No plot are found based on inputs")

  res <- res %>% dplyr::arrange(plot_name)

  if(map) {

    cli::cli_rule(left = "Mapping")

    if (requireNamespace("spData", quietly = TRUE)) {
      library(spData)
      data(world)
    }

    if(any(is.na(res$ddlat)) | any(is.na(res$ddlon))) {
      not_georef_plot <-
        dplyr::filter(res, is.na(ddlat), is.na(ddlon)) %>%
        dplyr::pull(plot_name)

      cli::cli_alert_warning("removing following plots because missing coordinates: {not_georef_plot}")

    }

    res <-
      res %>%
      dplyr::filter(!is.na(ddlat), !is.na(ddlon)) %>%
      dplyr::select(-id_senterre_db)

    data_sf <- sf::st_as_sf(res, coords = c("ddlon", "ddlat"), crs = 4326)
    bbox_data <- sf::st_bbox(data_sf)

    if(map_type == "ggplot") {

      outputmap <-
        ggplot2::ggplot() +
        ggplot2::geom_sf(data = world, alpha=0.8)

      if(label) {
        if(try_optimal_label) {

          if (!any(rownames(utils::installed.packages()) == "ggrepel"))
            stop("ggrepel package needed, please install it")

          outputmap <-
            outputmap +
            ggrepel::geom_text_repel(ggplot2::aes(x= res$ddlon, y= res$ddlat, label= data_sf$plot_name), hjust=0, vjust=0)

        }else{
          outputmap <-
            outputmap +
            ggplot2::geom_text(ggplot2::aes(x= res$ddlon, y= res$ddlat, label= data_sf$plot_name), hjust=0, vjust=0)
        }
      }

      outputmap <- outputmap +
        ggplot2::geom_sf(data = data_sf)

      outputmap <-
        outputmap +
        ggplot2::coord_sf(xlim = c(bbox_data[1]-zoom, bbox_data[3]+zoom), ylim = c(bbox_data[2]-zoom, bbox_data[4]+zoom))


    }

    if(map_type == "mapview") {
      map_types <- c("OpenStreetMap.DE",
                     "Esri.WorldImagery",
                     "Esri.WorldPhysical")

      print(map_type)
      outputmap <-  mapview::mapview(data_sf, map.types = map_types)

      if(show_all_coordinates) {
        if(!is.null(unlist(coordinates_subplots_plot_sf)))
          outputmap <- outputmap +
            mapview::mapview(coordinates_subplots_plot_sf, map.types = map_types)
      }

    }

    print(outputmap)

  }

  if(extract_individuals) {

    ## getting all metadata
    selec_plot_tables <-
      res %>%
      dplyr::select(plot_name,  locality_name, #team_leader,
                    id_liste_plots,
                    dplyr::contains("date_census"),
                    dplyr::contains("team_leader"),
                    dplyr::contains("principal_investigator"))

    res_individuals_full <-
      merge_individuals_taxa(id_individual = id_individual,
                             id_plot = selec_plot_tables$id_liste_plots,
                             id_tax = id_tax)

    res_individuals_full <- rm_field(data = res_individuals_full,
             field = c("photo_tranche",
                       "liane",
                       "dbh",
                       "dbh_height",
                       "tree_height",
                       "branch_height",
                       "branchlet_height",
                       "crown_spread",
                       "observations",
                       "observations_census_2",
                       "id_census2",
                       "dbh_census2",
                       "id_specimen_old",
                       "tax_tax",
                       "id_korup_ctfs",
                       "tag_korup_ctfs",
                       "id_table_data_senterre",
                       "id_diconame",
                       "code_individu",
                       "author1",
                       "author2",
                       "author3",
                       "tax_source",
                       "citation",
                       "id_tropicos",
                       "id_brlu",
                       "fktax",
                       "multi_tiges_id",
                       "multi_tiges_id_good",
                       "multi_stem_id_good",
                       "id_table_liste_plots",
                       "strate_cat",
                       "position_transect",
                       "position_x",
                       "position_y"))

    if(!is.null(tag)) {

      res_individuals_full <-
        res_individuals_full %>%
        dplyr::filter(ind_num_sous_plot %in% tag)

    }

    if (!include_liana) {

      res_individuals_full <-
        res_individuals_full %>%
        dplyr::filter(liana == FALSE)

      res_individuals_full <- rm_field(data = res_individuals_full,
                                       field = c("liana"))

    }


    res_individuals_full <-
      res_individuals_full %>%
      # dplyr::collect() %>%
      dplyr::mutate(original_tax_name = stringr::str_trim(original_tax_name))

    #adding metada information
    res_individuals_full <-
      res_individuals_full %>%
      dplyr::left_join(selec_plot_tables,
                       by = c("id_table_liste_plots_n" = "id_liste_plots"))
    
    if (extract_individual_features) {
      #### Add traits at individual levels
      all_traits <- traits_list()
      all_traits_list <-
        .get_trait_individuals_values(
          traits =  all_traits$id_trait, #
          src_individuals = res_individuals_full,
          # id_individuals = res_individuals_full$id_n,
          show_multiple_census = show_multiple_census
        )
      
      all_traits_list <-
        all_traits_list[unlist(lapply(all_traits_list, is.data.frame))]
      
      if (length(all_traits_list) > 0)
        res_individuals_full <-
        res_individuals_full %>%
        left_join(purrr::reduce(all_traits_list,
                                dplyr::full_join,
                                by = 'id_data_individuals'),
                  by = c('id_n' = 'id_data_individuals'))
      
    }
    
    # if (length(all_traits_list) > 0) {
    #   for (i in 1:length(all_traits_list)) {
    #     res_individuals_full <-
    #       res_individuals_full %>%
    #       dplyr::left_join(all_traits_list[[i]] %>%
    #                          dplyr::select(-id_old),
    #                        by = c("id_n" = "id_n"))
    #
    #   }
    # }

    if (extract_traits) {

      cli::cli_alert_info("Extracting taxa-level traits")

      queried_traits_tax <-
        query_traits_measures(idtax = unique(res_individuals_full$idtax_individual_f), )

      if (!is.null(nrow(queried_traits_tax$traits_idtax_num)))
        queried_traits_tax$traits_idtax_num <-
        queried_traits_tax$traits_idtax_num %>%
        dplyr::select(-starts_with("basisofrecord"))

      if (!is.null(nrow(queried_traits_tax$traits_idtax_char)))
        queried_traits_tax$traits_idtax_char <-
        queried_traits_tax$traits_idtax_char %>%
        dplyr::select(-starts_with("basisofrecord"))

      if (!is.null(nrow(queried_traits_tax$traits_found))) {

        if (any(class(queried_traits_tax$traits_idtax_num) == "data.frame"))
          res_individuals_full <-
            res_individuals_full %>%
            left_join(queried_traits_tax$traits_idtax_num,
                      by = c("idtax_individual_f" = "idtax"))

        if (any(class(queried_traits_tax$traits_idtax_char) == "data.frame"))
          res_individuals_full <-
            res_individuals_full %>%
            left_join(queried_traits_tax$traits_idtax_char,
                      by = c("idtax_individual_f" = "idtax"))

      } else {

          cli::cli_alert_info("No taxa-level traits found for extracted taxa")

      }

      if (traits_to_genera) {

        cli::cli_alert_info("Taxa-level traits aggregated to genus if no values are available")
        cli::cli_alert_info("Source of data added to columns names starting with source_")
        
        ### complete traits at genus level
        
        
        # list_genus <- res_individuals_full %>%
        #   # dplyr::filter(is.na(tax_sp_level)) %>%
        #   dplyr::select(id_n, tax_gen)
        
        
        
        
        res_traits_to_genera <- 
          .traits_to_genera_aggreg(dataset = res_individuals_full, 
                                   wd_fam_level = wd_fam_level)
        
        if (length(res_traits_to_genera$dataset_pivot_wider_char) > 1) {
          col_names_char <- 
            res_traits_to_genera$dataset_pivot_wider_char %>% select(-id_n, -tax_gen) %>% names()
          col_names_dataset <- 
            names(res_individuals_full)
          
          res_individuals_full <- res_individuals_full %>% 
            select(-all_of(col_names_dataset[which(col_names_dataset %in% col_names_char)])) %>% 
            left_join(res_traits_to_genera$dataset_pivot_wider_char %>% 
                        select(-tax_gen),
                      by = c("id_n" = "id_n"))
          
        }
        
        if (length(res_traits_to_genera$dataset_pivot_wider_num) > 1) {
          col_names_num <- 
            res_traits_to_genera$dataset_pivot_wider_num %>% select(-id_n, -tax_gen) %>% names()
          col_names_dataset <- 
            names(res_individuals_full)
          
          res_individuals_full <- res_individuals_full %>% 
            select(-all_of(col_names_dataset[which(col_names_dataset %in% col_names_num)])) %>% 
            left_join(res_traits_to_genera$dataset_pivot_wider_num %>% 
                        select(-tax_gen),
                      by = c("id_n" = "id_n"))
          
        }
        
      }


    }

    res <-
      res_individuals_full %>%
      dplyr::arrange(id_n) %>%
      dplyr::relocate(plot_name, .before = id_old) %>%
      dplyr::relocate(locality_name, .before = id_old) %>%
      dplyr::relocate(sous_plot_name, .before = id_old) %>%
      dplyr::relocate(ind_num_sous_plot, .before = id_old) %>%
      dplyr::relocate(tax_sp_level, .before = id_old) %>%
      dplyr::relocate(tax_infra_level, .before = id_old) %>%
      dplyr::relocate(tax_gen, .before = id_old) %>%
      dplyr::relocate(tax_fam, .before = id_old) %>%
      dplyr::relocate(colnam, .before = id_old) %>%
      dplyr::relocate(colnbr, .before = id_old) %>%
      dplyr::relocate(suffix, .before = id_old)

    if (any(names(res_individuals_full) == "stem_diameter"))
      res <-
      res %>%
      dplyr::relocate(stem_diameter, .before = ind_num_sous_plot)

    if (any(names(res_individuals_full) == "tree_height"))
      res <-
      res %>%
      dplyr::relocate(tree_height, .before = ind_num_sous_plot)

  }

  if(remove_ids & extract_individuals) {

    cli::cli_alert_warning("ids removed - remove_ids = {remove_ids} ")

    # res <-
    #   res %>%
    #   dplyr::select_at(which(!grepl("id_", colnames(res))))

    res <-
      res %>%
      dplyr::rename(idDB = id_n) %>%
      dplyr::select(-tidyselect::starts_with("id_")) %>%
      dplyr::rename(id_n = idDB)

  }

  if(remove_ids & !extract_individuals) {

    cli::cli_alert_warning("Identifiers are removed because the parameter 'remove_ids' = {remove_ids} ")

    # res <-
    #   res %>%
    #   dplyr::select_at(which(!grepl("id_", colnames(res))))

    res <-
      res %>%
      dplyr::rename(idDB = id_liste_plots) %>%
      dplyr::select(-tidyselect::starts_with("id_")) %>%
      dplyr::rename(id_liste_plots = idDB)

  }

  # dbDisconnect(mydb)

  res_list <-
    list(
      extract = NA,
      census_features = NA,
      coordinates = NA,
      coordinates_sf = NA
    )

  res_list$extract <- res

  if (nrow(res) < 100)
    print_table(res_print = res)

  if(show_multiple_census) {
    res_list$census_features <- census_features

    print_table(census_features)
  }

  if (show_all_coordinates)
    res_list$coordinates <- coordinates_subplots

  if (show_all_coordinates)
    res_list$coordinates_sf <- coordinates_subplots_plot_sf

  res_list <- res_list[!is.na(res_list)]

  if (length(res_list) == 1)
    res_list <- res_list[[1]]

  return(res_list)

}



.traits_to_genera_aggreg <- function(dataset, wd_fam_level_add = wd_fam_level) {
  
  list_genus <- dataset %>%
    # dplyr::filter(is.na(tax_sp_level)) %>%
    dplyr::select(id_n, tax_gen)
  
  all_sp_genera <- query_taxa(
    genus = list_genus %>%
      dplyr::filter(!is.na(tax_gen)) %>%
      dplyr::distinct(tax_gen) %>%
      dplyr::pull(tax_gen),
    class = NULL,
    extract_traits = FALSE,
    verbose = FALSE,
    exact_match = TRUE
  )
  
  all_sp_genera <-
    all_sp_genera %>%
    filter(tax_gen %in% unique(list_genera$tax_gen),
           !is.na(tax_infra_level))
  
  all_val_sp <- query_traits_measures(idtax = all_sp_genera %>%
                                        filter(!is.na(tax_esp)) %>%
                                        pull(idtax_n),
                                      idtax_good = all_sp_genera %>%
                                        filter(!is.na(tax_esp)) %>%
                                        pull(idtax_good_n),
                                      add_taxa_info = T)
  
  if (any(class(all_val_sp$traits_idtax_char) == "data.frame")) {
    
    traits_idtax_char <-
      all_val_sp$traits_found %>%
      dplyr::filter(valuetype == "categorical") %>%
      dplyr::select(idtax,
                    trait,
                    traitvalue_char,
                    basisofrecord,
                    id_trait_measures) %>%
      dplyr::mutate(rn = data.table::rowid(trait)) %>%
      tidyr::pivot_wider(
        names_from = trait,
        values_from = c(traitvalue_char, basisofrecord, id_trait_measures), 
        names_prefix = "taxa_level_"
      ) %>%
      dplyr::select(-rn) %>%
      left_join(all_val_sp$traits_idtax_char %>%
                  dplyr::select(idtax, tax_gen),
                by = c("idtax" = "idtax"))
    
    names(traits_idtax_char) <- gsub("traitvalue_char_", "", names(traits_idtax_char))
    
    traits_idtax_concat <-
      traits_idtax_char %>%
      dplyr::select(tax_gen, starts_with("id_trait_")) %>%
      dplyr::mutate(across(starts_with("id_trait_"), as.character)) %>%
      dplyr::group_by(tax_gen) %>%
      dplyr::mutate(dplyr::across(where(is.character),
                                  ~ stringr::str_c(.[!is.na(.)],
                                                   collapse = ", "))) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()
    
    cli::cli_alert_info("Extracting most frequent value for categorical traits at genus level")
    
    traits_idtax_char <-
      traits_idtax_char %>%
      dplyr::select(-starts_with("id_trait_")) %>%
      group_by(tax_gen, across(where(is.character))) %>%
      count() %>%
      arrange(tax_gen, desc(n)) %>%
      ungroup() %>%
      group_by(tax_gen) %>%
      dplyr::summarise_if(is.character, ~ first(.x[!is.na(.x)]))
    
    traits_idtax_char <-
      left_join(traits_idtax_char,
                traits_idtax_concat, by = c("tax_gen" = "tax_gen"))
    
    colnames_traits <- names(traits_idtax_char %>%
                               dplyr::select(
                                 -tax_gen,
                                 -starts_with("id_trait_"),
                                 -starts_with("basisofrecord_")
                               ))
    
    colnames_data <- names(dataset)
    
    dataset_subset <- 
      dataset %>% 
      select(id_n, 
             tax_gen,
             all_of(colnames_traits[which(colnames_traits %in% colnames_data)]))
    
    dataset_pivot <- 
      dataset_subset %>% 
      pivot_longer(cols = starts_with("taxa_level"),
                   names_to = "trait") %>% 
      arrange(tax_gen, trait)
    
    dataset_traits_pivot <- 
      traits_idtax_char %>% 
      select(tax_gen,
             all_of(colnames_traits)) %>% 
      pivot_longer(cols = starts_with("taxa_level"),
                   names_to = "trait") %>% 
      arrange(tax_gen, trait)
    
    dataset_genus_level <- 
      dataset_pivot %>% 
      filter(is.na(value)) %>% 
      select(-value) %>% 
      left_join(dataset_traits_pivot,
                by = c("tax_gen" = "tax_gen",
                       "trait" = "trait"))
    
    dataset_sp_level <- 
      dataset_pivot %>% 
      filter(!is.na(value)) %>% 
      select(-value) %>% 
      left_join(dataset_traits_pivot,
                by = c("tax_gen" = "tax_gen",
                       "trait" = "trait")) %>% 
      mutate(source = "species")
    
    
    dataset_genus_level_filled <- 
      dataset_genus_level  %>% 
      filter(!is.na(value)) %>% 
      mutate(source = "genus")
    
    
    dataset_genus_level_unfilled <- 
      dataset_genus_level  %>% 
      filter(is.na(value)) %>% 
      mutate(source = NA_character_)
    
    
    dataset_pivot_wider_char <- 
      bind_rows(dataset_sp_level, dataset_genus_level_filled, dataset_genus_level_unfilled) %>% 
      pivot_wider(names_from = trait, 
                  values_from = c(value, source))
    
    names(dataset_pivot_wider_char) <- 
      gsub("value_", "", names(dataset_pivot_wider_char))
    
    
    # for (j in 1:length(colnames_traits)) {
    #   
    #   if (colnames_traits[j] %in% names(res_individuals_full)) {
    #     
    #     var1 <- paste0(colnames_traits[j], ".y")
    #     var2 <- paste0(colnames_traits[j], ".x")
    #     
    #     res_individuals_full <-
    #       res_individuals_full %>%
    #       left_join(
    #         traits_idtax_char %>%
    #           dplyr::select(tax_gen, colnames_traits[j]),
    #         by = c("tax_gen" = "tax_gen")
    #       ) %>%
    #       # dplyr::select(
    #       #   tax_sp_level,
    #       #   id_n,
    #       #   tax_gen,
    #       #   paste0(colnames_traits[j], ".x"),
    #       #   paste0(colnames_traits[j], ".y")
    #       # ) %>%
    #       mutate("{colnames_traits[j]}" :=
    #                ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var2)))),
    #                       ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
    #                              NA,
    #                              !!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
    #                       !!rlang::parse_expr(quo_name(rlang::enquo(var2))))) %>%
    #       mutate("source_{colnames_traits[j]}" :=
    #                ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var2)))),
    #                       ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
    #                              NA,
    #                              "genus"),
    #                       "species")) %>%
    #       dplyr::select(-paste0(colnames_traits[j], ".x"),
    #                     -paste0(colnames_traits[j], ".y"))
    #     
    #   } else {
    #     
    #     var1 <- colnames_traits[j]
    #     
    #     res_individuals_full <-
    #       res_individuals_full %>%
    #       left_join(
    #         traits_idtax_char %>%
    #           dplyr::select(tax_gen, colnames_traits[j]),
    #         by = c("tax_gen" = "tax_gen")
    #       ) %>%
    #       mutate("source_{colnames_traits[j]}" :=
    #                ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
    #                       NA,
    #                       "genus"))
    #     
    #   }
    # }
  } else {
    dataset_pivot_wider_char <- NA
  }
  
  if (any(class(all_val_sp$traits_idtax_num) == "data.frame")) {
    
    traits_idtax_num <-
      all_val_sp$traits_found %>%
      dplyr::filter(valuetype == "numeric") %>%
      dplyr::select(idtax,
                    trait,
                    traitvalue,
                    basisofrecord,
                    id_trait_measures) %>%
      dplyr::mutate(rn = data.table::rowid(trait)) %>%
      tidyr::pivot_wider(
        names_from = trait,
        values_from = c(traitvalue, basisofrecord, id_trait_measures), 
        names_prefix = "taxa_level_"
      ) %>%
      dplyr::select(-rn) %>%
      dplyr::left_join(all_val_sp$traits_idtax_num %>%
                         dplyr::select(idtax, tax_gen),
                       by = c("idtax" = "idtax"))
    
    names(traits_idtax_num) <- gsub("traitvalue_", "", names(traits_idtax_num))
    
    traits_idtax_concat <-
      traits_idtax_num %>%
      dplyr::select(tax_gen, starts_with("id_trait_")) %>%
      dplyr::mutate(across(starts_with("id_trait_"), as.character)) %>%
      dplyr::group_by(tax_gen) %>%
      dplyr::mutate(dplyr::across(where(is.character),
                                  ~ stringr::str_c(.[!is.na(.)],
                                                   collapse = ", "))) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()
    
    traits_idtax_num <-
      traits_idtax_num %>%
      dplyr::select(-starts_with("id_trait_"), -idtax) %>%
      dplyr::group_by(tax_gen) %>%
      dplyr::summarise(dplyr::across(where(is.numeric),
                                     .fns= list(mean = mean,
                                                sd = sd,
                                                n = length),
                                     .names = "{.col}_{.fn}"))
    
    
    colnames_traits <- names(traits_idtax_num %>%
                               dplyr::select(
                                 -tax_gen,
                                 -starts_with("id_trait_"),
                                 -starts_with("basisofrecord_")
                               ))
    
    dataset_subset <- 
      dataset %>% 
      select(id_n, 
             tax_gen,
             tax_fam,
             plot_name,
             all_of(colnames_traits[which(colnames_traits %in% colnames_data)]))
    
    dataset_pivot <- 
      dataset_subset %>% 
      pivot_longer(cols = starts_with("taxa_level"),
                   names_to = "trait") %>% 
      arrange(tax_fam, tax_gen, trait)
    
    dataset_traits_pivot <- 
      traits_idtax_num %>% 
      select(tax_gen,
             all_of(colnames_traits)) %>% 
      pivot_longer(cols = starts_with("taxa_level"),
                   names_to = "trait") %>% 
      arrange(tax_gen, trait) %>% 
      filter(!is.na(value))
    
    dataset_genus_level <- 
      dataset_pivot %>% 
      filter(is.na(value)) %>% 
      select(-value) %>% 
      left_join(dataset_traits_pivot,
                by = c("tax_gen" = "tax_gen",
                       "trait" = "trait"))
    
    dataset_sp_level <- 
      dataset_pivot %>% 
      filter(!is.na(value)) %>% 
      # select(-value) %>% 
      # left_join(dataset_traits_pivot,
      #           by = c("tax_gen" = "tax_gen",
      #                  "trait" = "trait")) %>% 
      mutate(source = "species")
    
    
    dataset_genus_level_filled <- 
      dataset_genus_level  %>% 
      filter(!is.na(value)) %>% 
      mutate(source = "genus")
    
    
    dataset_genus_level_unfilled <- 
      dataset_genus_level  %>% 
      filter(is.na(value)) %>% 
      mutate(source = NA_character_)
    
    
    dataset_pivot_wider_num <- 
      bind_rows(dataset_sp_level, dataset_genus_level_filled, dataset_genus_level_unfilled) %>% 
      pivot_wider(names_from = trait, 
                  values_from = c(value, source))
    
    names(dataset_pivot_wider_num) <- 
      gsub("value_", "", names(dataset_pivot_wider_num))
    
    
    # for (j in 1:length(colnames_traits)) {
    #   
    #   if (colnames_traits[j] %in% names(res_individuals_full)) {
    #     
    #     var1 <- paste0(colnames_traits[j], ".y")
    #     var2 <- paste0(colnames_traits[j], ".x")
    #     
    #     res_individuals_full <-
    #       res_individuals_full %>%
    #       left_join(
    #         traits_idtax_num %>%
    #           dplyr::select(tax_gen, colnames_traits[j]),
    #         by = c("tax_gen" = "tax_gen")
    #       ) %>%
    #       # dplyr::select(
    #       #   tax_sp_level,
    #       #   id_n,
    #       #   tax_gen,
    #       #   paste0(colnames_traits[j], ".x"),
    #       #   paste0(colnames_traits[j], ".y")
    #       # ) %>%
    #       mutate("{colnames_traits[j]}" :=
    #                ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var2)))),
    #                       ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
    #                              NA,
    #                              !!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
    #                       !!rlang::parse_expr(quo_name(rlang::enquo(var2))))) %>%
    #       mutate("source_{colnames_traits[j]}" :=
    #                ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var2)))),
    #                       ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
    #                              NA,
    #                              "genus"),
    #                       "species")) %>%
    #       dplyr::select(-paste0(colnames_traits[j], ".x"),
    #                     -paste0(colnames_traits[j], ".y"))
    #     
    #     
    #   } else {
    #     
    #     var1 <- colnames_traits[j]
    #     
    #     res_individuals_full <-
    #       res_individuals_full %>%
    #       left_join(
    #         traits_idtax_num %>%
    #           dplyr::select(tax_gen, colnames_traits[j]),
    #         by = c("tax_gen" = "tax_gen")
    #       ) %>%
    #       mutate("source_{colnames_traits[j]}" :=
    #                ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
    #                       NA,
    #                       "genus"))
    #     
    #   }
    # }
    
    if (any(colnames_traits == "taxa_level_wood_density_mean")) {
      
      cli::cli_alert_info("Setting wood density SD to averaged species and genus level according to BIOMASS dataset")
      
      sd_10 <- BIOMASS::sd_10
      
      
      ### replacing wd sd to species and genus level sd from biomass
      dataset_pivot_wider_num <-
        dataset_pivot_wider_num %>%
        mutate(taxa_level_wood_density_sd = replace(taxa_level_wood_density_sd,
                                                    source_taxa_level_wood_density_mean == "species",
                                                    sd_10$sd[1])) %>%
        mutate(taxa_level_wood_density_sd = replace(taxa_level_wood_density_sd,
                                                    source_taxa_level_wood_density_mean == "genus",
                                                    sd_10$sd[2]))
      
      if (wd_fam_level_add) {
        
        dataset_pivot_wider_num <-
          dataset_pivot_wider_num %>%
          mutate(taxa_level_wood_density_sd = replace(
            taxa_level_wood_density_sd,
            is.na(tax_gen) & !is.na(tax_fam),
            sd_10$sd[3]
          ))
        
      }
      
      # %>%
      #   filter(!is.na(tax_gen), is.na(tax_sp_level))
    }
    
    ### averaged wd for plots
    wd_plot_level <- 
      dataset_pivot_wider_num %>%
      dplyr::group_by(plot_name) %>%
      dplyr::summarise(taxa_level_wood_density_mean_plot_level = mean(taxa_level_wood_density_mean, na.rm = T),
                       taxa_level_wood_density_sd_plot_level = mean(taxa_level_wood_density_sd, na.rm = T))
    
    dataset_pivot_wider_num <- 
      dataset_pivot_wider_num %>%
      dplyr::left_join(wd_plot_level,
                       by = c("plot_name" = "plot_name")) %>%
      dplyr::mutate(taxa_level_wood_density_mean = 
                      ifelse(is.na(taxa_level_wood_density_mean),
                             taxa_level_wood_density_mean_plot_level,
                             taxa_level_wood_density_mean),
                    taxa_level_wood_density_sd = 
                      ifelse(is.na(taxa_level_wood_density_sd),
                             taxa_level_wood_density_sd_plot_level,
                             taxa_level_wood_density_sd),
                    source_taxa_level_wood_density_sd = 
                      ifelse(is.na(source_taxa_level_wood_density_sd),
                             "plot_mean",
                             source_taxa_level_wood_density_sd),
                    source_taxa_level_wood_density_mean = 
                      ifelse(is.na(source_taxa_level_wood_density_mean),
                             "plot_mean",
                             source_taxa_level_wood_density_mean)) %>%
      dplyr::select(-taxa_level_wood_density_mean_plot_level, taxa_level_wood_density_sd)
    
    
    #                 ifelse(is.na(taxa_level_wood_density_mean),
    #                                          wood_density_mean_plot_level,
    #                                          wood_density_mean)) %>%
    # dplyr::mutate(wood_density_sd = ifelse(is.na(wood_density_sd),
    #                                        wood_density_sd_plot_level,
    #                                        wood_density_sd))
    
  } else {
    dataset_pivot_wider_num <- NA
  }
  
  return(list(dataset_pivot_wider_char = dataset_pivot_wider_char,
              dataset_pivot_wider_num = dataset_pivot_wider_num))
  
}





#' Explore allometric relation
#'
#' Provide allometric data and graph dbh-height of selected taxa
#'
#' @return A tibble
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param genus_searched string
#' @param tax_esp_searched string
#' @param tax_fam_searched string
#' @param id_search integer
#'
#' @return A tibble of taxa or individuals if extract_individuals is TRUE
#' @export
explore_allometric_taxa <- function(genus_searched = NULL,
                                    tax_esp_searched = NULL,
                                    tax_fam_searched = NULL,
                                    id_search = NULL) {

  if(!exists("mydb")) call.mydb()
  if(!exists("mydb_taxa")) call.mydb.taxa()

  res_taxa <- query_taxa(
    genus = genus_searched,
    species = tax_esp_searched,
    ids =  id_search, verbose = F)

  tax_data <-
    query_plots(id_tax = res_taxa$idtax_n)

  if(nrow(tax_data)>0) {
    # cat(paste0("\n ", nrow(tax_data), " taxa selected"))
    # print(tax_data$full_name_no_auth)
    #
    # specimens_id_diconame <-
    #   dplyr::tbl(mydb, "specimens") %>%
    #   dplyr::select(id_specimen, id_diconame_n)
    #
    # diconames_id <-
    #   dplyr::tbl(mydb, "diconame") %>%
    #   dplyr::select(id_n, id_good_n)
    #
    # specimens_linked <-
    #   specimens_id_diconame %>%
    #   dplyr::left_join(diconames_id, by=c("id_diconame_n"="id_n")) %>%
    #   dplyr::rename(id_dico_name_specimen=id_good_n) %>%
    #   dplyr::select(id_specimen, id_dico_name_specimen)
    #
    # selec_plot_tables <-
    #   dplyr::tbl(mydb, "data_liste_plots") %>%
    #   dplyr::select(plot_name, team_leader, country, locality_name, id_liste_plots)
    #
    # all_individuals_data <-
    #   dplyr::tbl(mydb, "data_individuals") %>%
    #   dplyr::left_join(specimens_linked, by=c("id_specimen"="id_specimen")) %>%
    #   dplyr::left_join(selec_plot_tables, by=c("id_table_liste_plots_n"="id_liste_plots")) %>%
    #   dplyr::mutate(id_diconame_final=ifelse(!is.na(id_dico_name_specimen), id_dico_name_specimen, id_diconame_n)) %>%
    #   dplyr::left_join(dplyr::tbl(mydb, "diconame"), by=c("id_diconame_final"="id_n")) %>%
    #   dplyr::filter(id_diconame_final %in% tax_data$id_n) %>%
    #   dplyr::select(id_n, multi_tiges_id_good, plot_name,team_leader,country,locality_name, sous_plot_name, ind_num_sous_plot, code_individu, dbh, full_name_no_auth, full_name_used, tax_fam, tax_gen, tax_esp, morphocat,
    #                 id_diconame_final, dbh_height, tree_height, branch_height, branchlet_height, crown_spread, liane, strate_cat,
    #                 herbarium_code_char, id_specimen) %>%
    #   dplyr::arrange(id_n)

    data_allo1 <-
      tax_data %>%
      # dplyr::select(tree_height, stem_diameter) %>%
      dplyr::filter(!is.na(tree_height), tree_height>0, stem_diameter>0) %>%
      dplyr::collect()

    cat(paste0("\n The number of individuals with both tree height and stem_diameter values is ", nrow(data_allo1)))

    if(nrow(data_allo1)>1) {
      gg_plot1 <-
        ggplot2::ggplot() +
        ggplot2::geom_point(data = data_allo1,
                            mapping = ggplot2::aes(x = stem_diameter, y = tree_height)) +
        ggplot2::xlab("Stem diameter (cm)") +
        ggplot2::ylab("Tree height (m)")

    }else{
      gg_plot1 <- NA
    }

    data_allo2 <-
      tax_data %>%
      # dplyr::select(tree_height, dbh, crown_spread, id_n, plot_name, country, full_name_no_auth) %>%
      dplyr::filter(!is.na(crown_width), crown_width>0, stem_diameter>0) %>%
      dplyr::collect()

    cat(paste0("\n The number of individuals with both crown_width and stem_diameter values is ", nrow(data_allo2)))
    cat("\n")

    if(nrow(data_allo2)>1) {
      gg_plot2 <-
        ggplot2::ggplot() +
        ggplot2::geom_point(data = data_allo2, mapping =
                              ggplot2::aes(x = stem_diameter, y = crown_width)) +
        ggplot2::xlab("Stem diameter (cm)") +
        ggplot2::ylab("Crown width (m)")
    }else{
      gg_plot2 <- NA
    }
  }else{
    cli::cli_alert_danger("No taxa found. Select at least one taxa")
    # cat(paste0("\n You currently selected ", nrow(tax_data), "taxa"))
    print(tax_data)
  }

  # data_allo3 <-
  #   all_individuals_data %>%
  #   dplyr::select(tree_height, dbh, crown_spread, id_n, plot_name, country, locality_name) %>%
  #   filter(!is.na(crown_spread), !is.na(tree_height)) %>%
  #   collect()
  #
  # gg_plot3 <-
  #   ggplot() +
  #   geom_point(data = data_allo3, mapping = aes(x = tree_height, y = crown_spread))

  if (nrow(tax_data) > 0)
    return(
      list(
        data_height_dbh = data_allo1,
        data_crow_dbh = data_allo2,
        taxa_data = tax_data,
        plot_height_dbh = gg_plot1,
        plot_crown_dbh = gg_plot2
      )
    )
}


#' Herbarium labels
#'
#' Produce rtf file with herbarium specimens labels
#'
#' @return A tibble
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param dat data.frame
#' @param theme string
#' @param outfile string
#' @param font string
#' @param font_size integer
herbarium_label <-
  function (dat = NULL,
            theme = c("GILLES"),
            outfile = "herblabel.rtf",
            font = c("Roman", "Arial"),
            font_size = 1
  )
  {

    if (is.null(dat)) {
      stop("'dat' should be specified")
    }

    theme <- match.arg(theme)
    font <- match.arg(font)
    herbdat000 <- dat
    herbdat000[herbdat000 == ""] <- NA
    dat$LAT_FLAG <- toupper(dat$LAT_FLAG)
    dat$LON_FLAG <- toupper(dat$LON_FLAG)

    if (any(is.na(herbdat000$HERBARIUM))) {
      stop(paste(
        "\"HERBARIUM\" not provided for row: ",
        paste(which(is.na(
          herbdat000$HERBARIUM
        )) +
          1, collapse = ", ")
      ))
    }

    if (any(is.na(herbdat000$COLLECTOR))) {
      stop(paste(
        "\"COLLECTOR\" not provided for row: ",
        paste(which(is.na(
          herbdat000$COLLECTOR
        )) +
          1, collapse = ", ")
      ))
    }
    if (any(is.na(herbdat000$COLLECTOR_NUMBER))) {
      stop(paste(
        "\"COLLECTOR_NUMBER\" not provided for row: ",
        paste(which(is.na(
          herbdat000$COLLECTOR_NUMBER
        )) +
          1, collapse = ", ")
      ))
    }
    if (any(is.na(herbdat000$DATE_COLLECTED))) {
      stop(paste(
        "\"DATE_COLLECTED\" not provided for row: ",
        paste(which(is.na(
          herbdat000$DATE_COLLECTED
        )) +
          1, collapse = ", ")
      ))
    }
    if (any(is.na(herbdat000$FAMILY))) {
      warning(paste(
        "\"FAMILY\" not provided for row: ",
        paste(which(is.na(
          herbdat000$FAMILY
        )) +
          1, collapse = ", ")
      ))
    }
    # if (any(is.na(herbdat000$GENUS))) {
    #   warning(paste("\"GENUS\" must be provided for row: ",
    #                 paste(which(is.na(herbdat000$GENUS)) + 1, collapse = ", ")))
    # }
    if (any(is.na(herbdat000$COUNTRY))) {
      stop(paste(
        "\"COUNTRY\" not provided for row: ",
        paste(which(is.na(
          herbdat000$COUNTRY
        )) +
          1, collapse = ", ")
      ))
    }
    # if (any(is.na(herbdat000$STATE_PROVINCE))) {
    #   warning(paste("\"STATE_PROVINCE\" not provided for row: ",
    #                 paste(which(is.na(herbdat000$STATE_PROVINCE)) +
    #                         1, collapse = ", ")))
    # }
    # if (any(is.na(herbdat000$COUNTY))) {
    #   warning(paste("\"COUNTY\" not provided for row: ", paste(which(is.na(herbdat000$COUNTY)) +
    #                                                              1, collapse = ", ")))
    #   herbdat000$COUNTY[is.na(herbdat000$COUNTY)] <- " "
    # }
    if (any(is.na(herbdat000$LOCALITY))) {
      warning(paste(
        "\"LOCALITY\" not provided for row: ",
        paste(which(is.na(
          herbdat000$LOCALITY
        )) + 1, collapse = ", ")
      ))
    }
    if (any(is.na(herbdat000$IDENTIFIED_BY))) {
      warning(paste(
        "\"IDENTIFIED_BY\" not provided for row: ",
        paste(which(is.na(
          herbdat000$IDENTIFIED_BY
        )) + 1, collapse = ", ")
      ))
    }
    if (any(is.na(herbdat000$DATE_IDENTIFIED))) {
      warning(paste(
        "\"DATE_IDENTIFIED\" not provided for row: ",
        paste(which(is.na(
          herbdat000$DATE_IDENTIFIED
        )) +
          1, collapse = ", ")
      ))
    }
    print(paste(nrow(herbdat000), "herbarium specimen labels to create:"))
    pgenus <- herblabel::pgenus
    formatdate <- function(x) {
      if (!is.na(suppressWarnings(as.integer(x)))) {
        if (!grepl("^darwin", R.version$os)) {
          x <- as.Date(as.integer(x), origin = "1899-12-30")
        }
        else {
          x <- as.Date(as.integer(x), origin = "1904-01-01")
        }
      }
      res <- format(as.Date(x), "%d %B %Y")
      return(res)
    }

    formatdate2 <- function(x) {
      if (!is.na(suppressWarnings(as.integer(x)))) {
        if (!grepl("^darwin", R.version$os)) {
          x <- as.Date(as.integer(x), origin = "1899-12-30")
        }
        else {
          x <- as.Date(as.integer(x), origin = "1904-01-01")
        }
      }
      res <- format(as.Date(x))
      return(res)
    }


    Cap <- function(x) {
      paste(toupper(substring(x, 1, 1)), tolower(substring(x,
                                                           2)), sep = "")
    }

    Cap2 <- function(x) {
      paste(toupper(substring(x, 1, 1)), substring(x, 2),
            sep = "")
    }
    replace_space <- function(x) {
      gsub("^[[:space:]]+|[[:space:]]+$", "", x)
    }

    REPLACE <- function(x) {
      if (length(x) > 1) {
        stop("only one string is allowed")
      }
      bbb <- gsub(" +", " ", gsub(",+", ", ", gsub(", +",
                                                   ",", x)))
      bbb <- gsub("^[[:space:]]+|[[:space:]]+$", "", bbb)
      endchar <- substr(bbb, nchar(bbb), nchar(bbb))
      if (endchar == ",") {
        yyy <- gregexpr(pattern = ",", bbb)
        res <- substr(bbb, start = 1, stop = ifelse(unlist(lapply(yyy,
                                                                  function(x) {
                                                                    max(x) - 1
                                                                  })) > 1, unlist(lapply(yyy, function(x) {
                                                                    max(x) - 1
                                                                  })), nchar(bbb)))
      }
      else {
        res <- bbb
      }
      res <- gsub("^[[:space:]]+|[[:space:]]+$", "", res)
      return(res)
    }



    latin_source <- herblabel::latin_source
    genera_names <- as.character(pgenus$GENUS)
    italic_latin <- function(x) {
      res.split <- unlist(strsplit(x, split = " "))
      res.split2 <- tolower(gsub(",|\\.", "", res.split))
      res.split3 <- gsub(",|\\.", "", res.split)
      found <- res.split2 %in% latin_source
      genus_found <- Cap(res.split3) %in% genera_names
      res.split[genus_found] <- res.split[genus_found]
      res.split[found] <- paste("\\cf3 \\i ", res.split[found],
                                "\\i0\\cf0 ", sep = "")
      paste(res.split, collapse = " ", sep = "")
    }

    # same_families <- c("Palmae", "Arecaceae", "Gramineae", "Poaceae",
    #                    "Leguminosae", "Fabaceae", "Guttiferae", "Clusiaceae",
    #                    "Cruciferae", "Brassicaceae", "Labiatae", "Lamiaceae",
    #                    "Compositae", "Asteraceae", "Umbelliferae", "Apiaceae")


    herbdat000$FAMILY <- toupper(herbdat000$FAMILY)
    herbdat000$GLOBAL_UNIQUE_IDENTIFIER <- replace_space(herbdat000$GLOBAL_UNIQUE_IDENTIFIER)
    herbdat000$HERBARIUM <- replace_space(herbdat000$HERBARIUM)
    herbdat000$TITLE <- replace_space(herbdat000$TITLE)
    herbdat000$COLLECTOR <- replace_space(herbdat000$COLLECTOR)
    herbdat000$ADDITIONAL_COLLECTOR <- replace_space(herbdat000$ADDITIONAL_COLLECTOR)
    herbdat000$COLLECTOR_NUMBER <- replace_space(herbdat000$COLLECTOR_NUMBER)
    herbdat000$DATE_COLLECTED <- replace_space(herbdat000$DATE_COLLECTED)
    herbdat000$LOCAL_NAME <- replace_space(herbdat000$LOCAL_NAME)
    herbdat000$FAMILY <- replace_space(herbdat000$FAMILY)
    herbdat000$GENUS <- replace_space(herbdat000$GENUS)
    herbdat000$SPECIES <- replace_space(herbdat000$SPECIES)
    herbdat000$AUTHOR_OF_SPECIES <- replace_space(herbdat000$AUTHOR_OF_SPECIES)
    herbdat000$INFRASPECIFIC_RANK <- replace_space(herbdat000$INFRASPECIFIC_RANK)
    herbdat000$INFRASPECIFIC_EPITHET <- replace_space(herbdat000$INFRASPECIFIC_EPITHET)
    herbdat000$AUTHOR_OF_INFRASPECIFIC_RANK <- replace_space(herbdat000$AUTHOR_OF_INFRASPECIFIC_RANK)
    herbdat000$COUNTRY <- replace_space(herbdat000$COUNTRY)
    herbdat000$STATE_PROVINCE <- replace_space(herbdat000$STATE_PROVINCE)
    herbdat000$COUNTY <- replace_space(herbdat000$COUNTY)
    herbdat000$LOCALITY <- replace_space(herbdat000$LOCALITY)
    herbdat000$IMAGE_URL <- replace_space(herbdat000$IMAGE_URL)
    herbdat000$RELATED_INFORMATION <- replace_space(herbdat000$RELATED_INFORMATION)
    herbdat000$LAT_DEGREE <- replace_space(herbdat000$LAT_DEGREE)
    herbdat000$LAT_MINUTE <- replace_space(herbdat000$LAT_MINUTE)
    herbdat000$LAT_SECOND <- replace_space(herbdat000$LAT_SECOND)
    herbdat000$LAT_FLAG <- replace_space(herbdat000$LAT_FLAG)
    herbdat000$LON_DEGREE <- replace_space(herbdat000$LON_DEGREE)
    herbdat000$LON_MINUTE <- replace_space(herbdat000$LON_MINUTE)
    herbdat000$LON_SECOND <- replace_space(herbdat000$LON_SECOND)
    herbdat000$LON_FLAG <- replace_space(herbdat000$LON_FLAG)
    herbdat000$ELEVATION <- replace_space(herbdat000$ELEVATION)
    herbdat000$ATTRIBUTES <- replace_space(herbdat000$ATTRIBUTES)
    herbdat000$REMARKS <- replace_space(herbdat000$REMARKS)
    herbdat000$GEOREFERENCE_SOURCES <- replace_space(herbdat000$GEOREFERENCE_SOURCES)
    herbdat000$PROJECT <- replace_space(herbdat000$PROJECT)
    herbdat000$IDENTIFIED_BY <- replace_space(herbdat000$IDENTIFIED_BY)
    herbdat000$DATE_IDENTIFIED <- replace_space(herbdat000$DATE_IDENTIFIED)
    herbdat000$TYPE_STATUS <- replace_space(herbdat000$TYPE_STATUS)
    herbdat000$PROCESSED_BY <- replace_space(herbdat000$PROCESSED_BY)
    herbdat000$DATE_LASTMODIFIED <- replace_space(herbdat000$DATE_LASTMODIFIED)
    herbdat000$LAT_DEGREE <- as.character(as.integer(herbdat000$LAT_DEGREE))
    herbdat000$LAT_MINUTE <- as.character(as.integer(herbdat000$LAT_MINUTE))
    herbdat000$LAT_SECOND <- as.character(round(as.numeric(herbdat000$LAT_SECOND),
                                                digits = 2))
    herbdat000$LON_DEGREE <- as.character(as.integer(herbdat000$LON_DEGREE))
    herbdat000$LON_MINUTE <- as.character(as.integer(herbdat000$LON_MINUTE))
    herbdat000$LON_SECOND <- as.character(round(as.numeric(herbdat000$LON_SECOND),
                                                digits = 2))
    comment_genus <- rep("", length(herbdat000$GENUS))
    comment_family <- rep("", length(herbdat000$FAMILY))

    lat_check_ind_1 <- rep(FALSE, nrow(herbdat000))
    lat_check_ind_2 <- rep(FALSE, nrow(herbdat000))
    lat_check_ind_3 <- rep(FALSE, nrow(herbdat000))
    lon_check_ind_1 <- rep(FALSE, nrow(herbdat000))
    lon_check_ind_2 <- rep(FALSE, nrow(herbdat000))
    lon_check_ind_3 <- rep(FALSE, nrow(herbdat000))
    herbdat000$LAT_FLAG <- toupper(herbdat000$LAT_FLAG)
    herbdat000$LON_FLAG <- toupper(herbdat000$LON_FLAG)
    for (i in 1:nrow(herbdat000)) {
      herbdat_temp <- herbdat000[i,]
      if (any(
        !is.na(herbdat_temp$LAT_DEGREE),
        !is.na(herbdat_temp$LAT_MINUTE),!is.na(herbdat_temp$LAT_SECOND)
      ) & is.na(herbdat_temp$LAT_FLAG)) {
        lat_check_ind_1[i] <- TRUE
      }
      if (all(
        !is.na(herbdat_temp$LAT_DEGREE),
        !is.na(herbdat_temp$LAT_MINUTE),!is.na(herbdat_temp$LAT_SECOND)
      ) & is.na(herbdat_temp$LAT_FLAG)) {
        lat_check_ind_2[i] <- TRUE
      }
      if (!herbdat_temp$LAT_FLAG %in% c("N", "S", NA)) {
        lat_check_ind_3[i] <- TRUE
      }
      if (any(
        !is.na(herbdat_temp$LON_DEGREE),
        !is.na(herbdat_temp$LON_MINUTE),!is.na(herbdat_temp$LON_SECOND)
      ) & is.na(herbdat_temp$LON_FLAG)) {
        lon_check_ind_1[i] <- TRUE
      }
      if (all(
        !is.na(herbdat_temp$LON_DEGREE),
        !is.na(herbdat_temp$LON_MINUTE),!is.na(herbdat_temp$LON_SECOND)
      ) & is.na(herbdat_temp$LON_FLAG)) {
        lon_check_ind_2[i] <- TRUE
      }
      if (!herbdat_temp$LON_FLAG %in% c("E", "W", NA)) {
        lon_check_ind_3[i] <- TRUE
      }
    }

    if (any(lat_check_ind_1)) {
      lat_check_ind_1_msg <-
        paste(
          "Degree, Minutes and Seconds for Latitude not completed in row:",
          paste(which(lat_check_ind_1), collapse = ", ")
        )
      stop(lat_check_ind_1_msg)
    }
    if (any(lat_check_ind_2)) {
      lat_check_ind_2_msg <- paste("LAT_FLAG not specified in row:",
                                   paste(which(lat_check_ind_2), collapse = ", "))
      stop(lat_check_ind_2_msg)
    }
    if (any(lat_check_ind_3)) {
      lat_check_ind_3_msg <-
        paste("Only N or S is allowed for the LAT_FLAG in row:",
              paste(which(lat_check_ind_3), collapse = ", "))
      stop(lat_check_ind_3_msg)
    }
    if (any(lon_check_ind_1)) {
      lon_check_ind_1_msg <-
        paste(
          "Degree, Minutes and Seconds for Longitude not completed in row:",
          paste(which(lon_check_ind_1), collapse = ", ")
        )
      stop(lon_check_ind_1_msg)
    }
    if (any(lon_check_ind_2)) {
      lon_check_ind_2_msg <- paste("LON_FLAG must be specified in row:",
                                   paste(which(lon_check_ind_2), collapse = ", "))
      stop(lon_check_ind_2_msg)
    }
    if (any(lon_check_ind_3)) {
      lon_check_ind_3_msg <-
        paste("Only N or S is allowed for the LON_FLAG in row:",
              paste(which(lon_check_ind_3), collapse = ", "))
      stop(lon_check_ind_3_msg)
    }

    if (font == "Roman") {
      fonttab <-
        "{\\fonttbl{\\f0\\froman\\fcharset134 SimSun;}{\\f1\\froman\\fcharset134 Times New Roman;}}"
    }

    if (font == "Arial") {
      fonttab <-
        "{\\fonttbl{\\f0\\fswiss\\fcharset134 SimSun;}{\\f1\\fswiss\\fcharset134 Arial;}}"
    }

    temp1 <-
      paste(
        "{\\rtf1\\ansi\\ansicpg936\\deflangfe2052\\fcharset134\\deff1",
        fonttab,
        "{\\stylesheet{\\*\\cs3 Default Paragraph Font;}}{\\colortbl\\red0\\green0\\blue0;\\red255\\green0\\blue0;\\red0\\green255\\blue0;\\red0\\green0\\blue255;}\\paperw12240\\paperh15840\\margl1800\\margr1800\\margt1440\\margb1440\\gutter0\\ftnbj\\aenddoc\\jcompress1\\viewkind4\\viewscale100\\asianbrkrule\\allowfieldendsel\\snaptogridincell\\viewkind4\\sectd\\sbkpage\\pgwsxn11906\\pghsxn16838\\marglsxn600\\margrsxn600\\margtsxn720\\margbsxn10\\guttersxn0\\headery720\\footery720\\pgbrdropt0\\sectdefaultcl\\cols2\\colsx1080\\linebetcol1\\endnhere",
        sep = ""

      )

    temp2 <- c()
    temp_count <- seq(0, nrow(herbdat000), by = 5)
    temp_count[1] <- 1
    NEW_DATE_COLLECTED <- as.character(c())
    NEW_DATE_IDENTIFIED <- as.character(c())
    herbdat_row1 <- herbdat000[1, ]
    for (i in 1:nrow(herbdat000)) {

      herbdat <- herbdat000[i, ]

      if (nrow(herbdat000) > 5) {
        if (i %in% temp_count) {
          print(paste("Making label for row: ", i))
        }
      }

      if (theme == "GILLES") {
        res <-
          c(
            paste(
              "{\\pard\\keep\\keepn\\fi0\\li0\\brsp20\\qc\\sb350\\sa80\\fs",
              trunc(font_size * 20),
              "",
              herbdat$HERBARIUM,
              "\\b0\\par }",
              sep = ""
            ),
            ifelse(
              is.na(herbdat$TITLE),
              "",
              paste(
                "{\\pard\\keep\\keepn\\fi0\\li0\\fs",
                trunc(font_size * 18),
                "\\qc\\sb10\\sa100\\b ",
                herbdat$TITLE,
                "\\b0 \\par }",
                sep = ""
              )
            ),
            ifelse(
              is.na(herbdat$FAMILY),
              paste(
                "{\\pard\\keep\\keepn\\fi0\\li0\\qc\\sb10\\sa100\\fs",
                trunc(font_size * 18),
                "\\b ",
                "\\b0\\qc0 \\par }",
                sep = ""
              ),
              paste(
                "{\\pard\\keep\\keepn\\fi0\\li0\\qc\\sb10\\sa100\\fs",
                trunc(font_size * 18),
                "\\b ",
                herbdat$FAMILY,
                "\\b0\\qc0 \\par }",
                sep = ""
              )
            ),
            ifelse(
              is.na(herbdat$FULL_NAME),
              "",
              paste(
                "{\\pard\\keep\\keepn\\fi-288\\li288\\sb100\\sa200\\fs",
                trunc(font_size * 20),
                "\\b\\i ",
                herbdat$FULL_NAME,
                "\\b0\\par}",
                sep = ""
              )
            ),
            paste(
              "{\\pard\\keep\\keepn\\fi0\\li0\\sb120\\sa20\\fs",
              trunc(font_size * 18),
              " ",
              REPLACE(
                paste(
                  toupper(ifelse(
                    is.na(herbdat$COUNTRY), "", herbdat$COUNTRY
                  )),
                  ", ",
                  ifelse(is.na(herbdat$STATE_PROVINCE), "", herbdat$STATE_PROVINCE),
                  ", ",
                  ifelse(is.na(herbdat$COUNTY), "", herbdat$COUNTY),
                  ", ",
                  ifelse(
                    is.na(herbdat$LOCALITY),
                    "",
                    as.character(herbdat$LOCALITY)
                  ),
                  sep = ""
                )
              ),
              "\\par}",
              sep = ""
            ),
            REPLACE(ifelse(
              is.na(herbdat$LAT_DEGREE),
              "",
              paste(
                "{\\pard\\keep\\keepn\\fi0\\li0\\sb20\\sa150\\fs",
                trunc(font_size * 18),
                "\\qj ",
                herbdat$LAT_DEGREE,
                "\\u176;",
                herbdat$LAT_MINUTE,
                "\\u39;",
                herbdat$LAT_SECOND,
                "\\u34;",
                herbdat$LAT_FLAG,
                ", ",
                herbdat$LON_DEGREE,
                "\\u176;",
                herbdat$LON_MINUTE,
                "\\u39;",
                herbdat$LON_SECOND,
                "\\u34;",
                herbdat$LON_FLAG,
                ifelse(
                  is.na(herbdat$ELEVATION),
                  "",
                  paste("; ", herbdat$ELEVATION, "m", sep = "")
                ),
                "\\par }",
                sep = ""
              )
            )),
            ifelse((is.na(
              herbdat$ATTRIBUTES
            )) &
              (is.na(herbdat$REMARKS)), "", italic_latin(gsub(
                "\\.  ", "\\. ", gsub(" \\.", "\\.", gsub(
                  "\\. \\.", "\\. ", gsub("\\. +", "\\. ",
                                          REPLACE(
                                            paste(
                                              "{\\pard\\keep\\keepn\\fi0\\li0",
                                              paste("\\fs", trunc(font_size * 18), sep = ""),
                                              "\\sb60",
                                              ifelse(is.na(herbdat$ATTRIBUTES), "", Cap2(as.character(
                                                herbdat$ATTRIBUTES
                                              ))),
                                              ifelse(is.na(herbdat$ATTRIBUTES), "", ". "),
                                              ifelse(is.na(herbdat$REMARKS), "", Cap2(as.character(herbdat$REMARKS))),
                                              "\\sa80\\par}",
                                              sep = " "
                                            )
                                          ))
                ))
              ))),
            ifelse(
              is.na(herbdat$ADDITIONAL_COLLECTOR),
              paste(
                "{\\pard\\keep\\keepn\\fi0\\sb200\\sa100\\fs",
                trunc(font_size * 18),
                "\\tqr\\tx4850\\b ",
                herbdat$COLLECTOR,
                ", #",
                herbdat$COLLECTOR_NUMBER,
                "\\b0",
                "  ",
                ifelse(nchar(
                  paste(
                    herbdat$COLLECTOR,
                    herbdat$ADDITIONAL_COLLECTOR,
                    ", #",
                    herbdat$COLLECTOR_NUMBER
                  )
                ) > 40, "\\line", "  "),
                " \\tab ",
                tryCatch(
                  formatdate(herbdat$DATE_COLLECTED),
                  error = function(e) {
                    cat(" ")
                    herbdat$DATE_COLLECTED
                  }
                ),
                "\\par}",
                sep = ""
              ),
              paste(
                "{\\pard\\keep\\keepn\\fi0\\sb200\\sa100",
                paste("\\fs", trunc(font_size * 18), sep = ""),
                "\\tqr\\tx4850\\b ",
                herbdat$COLLECTOR,
                ", ",
                herbdat$ADDITIONAL_COLLECTOR,
                "  #",
                herbdat$COLLECTOR_NUMBER,
                "\\b0",
                "  ",
                ifelse(nchar(
                  paste(
                    herbdat$COLLECTOR,
                    herbdat$ADDITIONAL_COLLECTOR,
                    ", #",
                    herbdat$COLLECTOR_NUMBER
                  )
                ) > 40, "\\line", "  "),
                " \\tab ",
                tryCatch(
                  formatdate(herbdat$DATE_COLLECTED),
                  error = function(e) {
                    cat(" ")
                    herbdat$DATE_COLLECTED
                  }
                ),
                "\\par}",
                sep = ""
              )
            ),
            ifelse(
              is.na(herbdat$PROJECT),
              "",
              paste(
                "{\\pard\\keep\\keepn\\fi0\\li0\\sa160\\fs",
                trunc(font_size * 18),
                "\\ql\\b ",
                as.character(herbdat$PROJECT),
                "\\ql0\\b0\\par }",
                sep = ""
              )
            ),
            ifelse(
              is.na(herbdat$GLOBAL_UNIQUE_IDENTIFIER) &
                is.na(herbdat$TYPE_STATUS) &
                is.na(herbdat$IDENTIFIED_BY) & is.na(herbdat$DATE_IDENTIFIED),
              "",
              paste(
                "{\\pard\\keep\\sa40\\keepn\\fi0\\li0\\fs",
                trunc(font_size * 18),
                "\\tqr\\tx4850 ",
                # gsub("_","", ifelse(is.na(herbdat$GLOBAL_UNIQUE_IDENTIFIER), "", as.character(herbdat$GLOBAL_UNIQUE_IDENTIFIER))),
                " \\tab ",
                ifelse(is.na(herbdat$TYPE_STATUS), "", herbdat$TYPE_STATUS),
                ifelse(
                  is.na(herbdat$IDENTIFIED_BY),
                  "",
                  paste(" Det.: ", herbdat$IDENTIFIED_BY)
                ),
                ifelse(is.na(herbdat$DATE_IDENTIFIED), "", ", "),
                ifelse(is.na(herbdat$DATE_IDENTIFIED), "",
                       tryCatch(
                         formatdate(herbdat$DATE_IDENTIFIED),
                         error = function(e) {
                           cat(" ")
                           herbdat$DATE_IDENTIFIED
                         }
                       )),
                "\\par}",
                sep = ""
              )
            ),
            paste(
              "{\\pard\\keep\\keepn\\sa100\\fs",
              trunc(font_size * 18),
              sep = ""
            ),
            " \\par }",
            paste("{\\pard\\keep\\qc\\fs", trunc(font_size * 18), sep = ""),
            "  .                  .                   .\\par}"
          )
      }

      NEW_DATE_COLLECTED[i] <- tryCatch(formatdate2(herbdat$DATE_COLLECTED),
                                        error = function(e) {
                                          herbdat$DATE_COLLECTED
                                        })
      NEW_DATE_IDENTIFIED[i] <- tryCatch(formatdate2(herbdat$DATE_IDENTIFIED),
                                         error = function(e) {
                                           herbdat$DATE_IDENTIFIED
                                         })
      temp2 <- c(temp2, res)
      herbdat_row1 <- rbind(herbdat_row1, herbdat)
    }

    template <- c(temp1, temp2, "}")

    res <- template[!template %in% ""]

    res <- res[!res %in% " "]
    res <- replace_space(res)
    res <- iconv(x = res, from = "UTF-8", to = "GB18030")
    writeLines(res, outfile)
    cat("Herbarium Labels have been saved to:\n", file.path(getwd(),
                                                            outfile), "\n", sep = "")
    modified_dat <- herbdat_row1[-1, ]
    modified_dat$GENUS <- comment_genus
    modified_dat$FAMILY <- comment_family
    dat$DATE_COLLECTED <- NEW_DATE_COLLECTED
    dat$DATE_IDENTIFIED <- NEW_DATE_IDENTIFIED
    return(invisible(list(dat = dat, modified_dat = modified_dat)))
  }




#' Add new plot metadata
#'
#' Add metadata for new plots
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data tibble
#' @param col_names_select string a vector of string indicating columns names of new_data
#' @param col_names_corresp string a vector of string indicating to which columns selected columns of new_data corresponds
#'
#' @importFrom methods new
#' @importFrom stats dist sd
#' @importFrom utils askYesNo data
#' @importFrom kableExtra cell_spec kable_styling
#'
#' @return No return value, new plots are added
#' @export
add_plots <- function(new_data,
                      col_names_select,
                      col_names_corresp) {

  if(!exists("mydb")) call.mydb()

  new_data_renamed <-
    new_data

  for (i in 1:length(col_names_select)) {
    if(any(colnames(new_data_renamed)==col_names_select[i])){
      new_data_renamed <-
        new_data_renamed %>%
        dplyr::rename_at(dplyr::vars(col_names_select[i]), ~ col_names_corresp[i])
    }else{
      stop(paste("Column name provided not found in provided new dataset", col_names_corresp[i]))

    }
  }

  ## Checking dates
  if (any(colnames(new_data_renamed) == "date_y"))
    if (any(new_data_renamed$date_y[!is.na(new_data_renamed$date_y)] > lubridate::year(Sys.Date())) |
        any(new_data_renamed$date_y[!is.na(new_data_renamed$date_y)] < 1900))
      stop("ERREUR dans date_y, year provided impossible")
  if (any(colnames(new_data_renamed) == "date_m"))
    if (any(new_data_renamed$date_m[!is.na(new_data_renamed$date_m)] > 12) |
        any(new_data_renamed$date_m[!is.na(new_data_renamed$date_m)] < 1))
      stop("ERREUR dans date_m, month provided impossible")
  if (any(colnames(new_data_renamed) == "data_d"))
    if (any(new_data_renamed$data_d[!is.na(new_data_renamed$data_d)] > 31) |
        any(new_data_renamed$data_d[!is.na(new_data_renamed$data_d)] < 1))
      stop("ERREUR dans data_d, day provided impossible")
  if (any(colnames(new_data_renamed) == "ddlon"))
    if (any(new_data_renamed$ddlon > 180) |
        any(new_data_renamed$ddlon < -180))
      stop("ERREUR dans ddlon, longitude provided impossible")
  if (any(colnames(new_data_renamed) == "ddlat"))
    if (any(new_data_renamed$ddlat > 90) |
        any(new_data_renamed$ddlon < -90))
      stop("ERREUR dans ddlat, latitude provided impossible")

  ## Checking if names plot are already in the database
  if(any(colnames(new_data_renamed) == "plot_name")) {

    found_plot <-
      try_open_postgres_table(table = "data_liste_plots", con = mydb) %>%
      dplyr::filter(plot_name %in% !!new_data_renamed$plot_name) %>%
      dplyr::collect()

    if (nrow(found_plot) > 0) {
      print(found_plot)
      stop("Some plot_name in new data already in the plot list table. No duplicate allowed.")
    }
  }

  ## Checking method
  if(!any(names(new_data_renamed) == "method")) {

    stop("missing method information")

  } else {

    # new_data_renamed <-
    #   new_data_renamed %>%
    #   mutate(id_method = .link_method(method = unique(new_data_renamed$method)))

    new_data_renamed <-
      .link_table(
      data_stand = new_data_renamed,
      column_searched = "method",
      column_name = "method",
      id_field = "id_method",
      id_table_name = "id_method",
      db_connection = mydb,
      table_name = "methodslist"
    )

    # new_data_renamed <-
    #   new_data_renamed %>%
    #  dplyr::select(-method)

    col_names_corresp[which(col_names_corresp == "method")] <-
      "id_method"

  }

  ## Checking country
  if(!any(names(new_data_renamed) == "country")) {

    stop("missing country information")

  } else {

    # new_data_renamed <-
    #   .link_country(data_stand = new_data_renamed, country_field = "country")

    new_data_renamed <-
      .link_table(
      data_stand = new_data_renamed,
      column_searched = "country",
      column_name = "country",
      id_field = "id_country",
      id_table_name = "id_country",
      db_connection = mydb,
      table_name = "table_countries"
    )

      # new_data_renamed %>%
      # mutate(id_country = .link_country(country_field = unique(new_data_renamed$country)))

    # new_data_renamed <-
    #   new_data_renamed %>%
    #   dplyr::select(-country_name)

    col_names_corresp[which(col_names_corresp == "country")] <-
      "id_country"

  }

  ## Checking team_leader
  if(!any(names(new_data_renamed) == "team_leader")) {

    cli::cli_alert_danger("missing team_leader column")

    chose_pi <- askYesNo(msg = "Choose one team_leader for all plot ?")

    if (chose_pi) {
      # id_team_leader <-
      #   .link_colnam(
      #     data_stand = tibble(team_leader = " "),
      #     collector_field = "team_leader",
      #     id_colnam = "id_team_leader"
      #   )

      id_team_leader <- .link_colnam(
        data_stand = tibble(team_leader = " "),
        column_searched = "team_leader",
        column_name = "colnam",
        id_field = "id_team_leader",
        id_table_name = "id_table_colnam",
        db_connection = mydb,
        table_name = "table_colnam"
      )

      id_team_leader <-
        tibble(plot_name = new_data_renamed$plot_name,
               team_leader = id_team_leader$id_team_leader)

    }

  } else {

    cli::cli_alert_info("Identifying team_leader")

    team_leader_sep <-
      new_data_renamed %>%
      dplyr::select(plot_name, team_leader) %>%
      tidyr::separate_rows(team_leader, sep = ",") %>%
      mutate(team_leader = stringr::str_squish(team_leader))

    # id_team_leader <-
    #   .link_colnam(data_stand = team_leader_sep,
    #                collector_field = "team_leader",
    #                id_colnam = "team_leader")

    id_team_leader <-
      .link_colnam(
      data_stand = team_leader_sep,
      column_searched = "team_leader",
      column_name = "colnam",
      id_field = "team_leader",
      id_table_name = "id_table_colnam",
      db_connection = mydb,
      table_name = "table_colnam"
    )

    # col_names_corresp[which(col_names_corresp == "team_leader")] <-
    #   "id_colnam"

  }

  ## Checking team_leader
  if(!any(names(new_data_renamed) == "PI")) {

    cli::cli_alert_danger("missing PI column")

    chose_pi <- askYesNo(msg = "Choose one PI for all plot ?")

    if (chose_pi) {
      # id_pi <- .link_colnam(data_stand = tibble(PI = " "),
      #                       collector_field = "PI", id_colnam = "id_pi")


      id_pi <-
        .link_colnam(
        data_stand = tibble(PI = " "),
        column_searched = "PI",
        column_name = "colnam",
        id_field = "id_pi",
        id_table_name = "id_table_colnam",
        db_connection = mydb,
        table_name = "table_colnam"
      )

      id_pi <-
        tibble(plot_name = new_data_renamed$plot_name,
               PI = id_pi$id_pi)

    }

  } else {

    cli::cli_alert_info("Identifying PI")

    pi_sep <-
      new_data_renamed %>%
      dplyr::select(plot_name, PI) %>%
      tidyr::separate_rows(PI, sep = ",") %>%
      mutate(PI = stringr::str_squish(PI))

    # id_pi <-
    #   .link_colnam(data_stand = pi_sep,
    #                collector_field = "PI", id_colnam = "PI")

    id_pi <-
      .link_colnam(
      data_stand = pi_sep,
      column_searched = "PI",
      column_name = "colnam",
      id_field = "PI",
      id_table_name = "id_table_colnam",
      db_connection = mydb,
      table_name = "table_colnam"
    )

  }


  ## Checking data manager
  if(!any(names(new_data_renamed) == "data_manager")) {

    cli::cli_alert_danger("missing data_manager column")

    chose_data_manager <- askYesNo(msg = "Choose one data_manager for all plot ?")

    if (chose_data_manager) {
      # data_manager <- .link_colnam(data_stand = tibble(data_manager = " "),
      #                       collector_field = "data_manager",
      #                       id_colnam = "id_data_manager")

      data_manager <- .link_colnam(
        data_stand = tibble(data_manager = " "),
        column_searched = "data_manager",
        column_name = "colnam",
        id_field = "id_data_manager",
        id_table_name = "id_table_colnam",
        db_connection = mydb,
        table_name = "table_colnam"
      )

      id_data_manager <-
        tibble(plot_name = new_data_renamed$plot_name,
               data_manager = data_manager$id_data_manager)

    }

  } else {

    cli::cli_alert_info("Identifying data manager")

    data_manager_sep <-
      new_data_renamed %>%
      dplyr::select(plot_name, data_manager) %>%
      tidyr::separate_rows(data_manager, sep = ",") %>%
      mutate(data_manager = stringr::str_squish(data_manager))

    # data_manager_sep <-
    #   .link_colnam(data_stand = data_manager_sep,
    #                collector_field = "data_manager", id_colnam = "data_manager")


    data_manager_sep <-
      .link_colnam(
      data_stand = data_manager_sep,
      column_searched = "data_manager",
      column_name = "colnam",
      id_field = "data_manager",
      id_table_name = "id_table_colnam",
      db_connection = mydb,
      table_name = "table_colnam"
    )

  }

  if(!any(names(new_data_renamed) == "additional_people")) {

    cli::cli_alert_danger("missing additional_people information")

  } else {

    cli::cli_alert_info("Identifying additional people list for the plot/transect")

    add_col_sep <-
      new_data_renamed %>%
      dplyr::select(plot_name, additional_people) %>%
      tidyr::separate_rows(additional_people, sep = ",") %>%
      mutate(additional_people = stringr::str_squish(additional_people))

    # add_col_sep <-
    #   .link_colnam(data_stand = add_col_sep,
    #                collector_field = "additional_people", id_colnam = "additional_people")


    add_col_sep <- .link_colnam(
      data_stand = add_col_sep,
      column_searched = "additional_people",
      column_name = "colnam",
      id_field = "additional_people",
      id_table_name = "id_table_colnam",
      db_connection = mydb,
      table_name = "table_colnam"
    )

    # add_col_sep <-
    #   add_col_sep %>%
    #   dplyr::select(-col_name)

  }

  new_data_renamed <-
    rm_field(new_data_renamed,
             field = c("team_leader", "PI", "additional_people", "data_manager"))

  col_names_corresp <-
    col_names_corresp[which(!col_names_corresp %in% c("team_leader", "PI", "additional_people", "data_manager"))]

  ## Checking coordinates
  if (any(names(new_data_renamed) == "ddlat"))
    if (any(new_data_renamed$ddlat > 90) | any(new_data_renamed$ddlat < -90)) stop("ddlat impossible")

  if (any(names(new_data_renamed) == "ddlon"))
    if (any(new_data_renamed$ddlon > 180) | any(new_data_renamed$ddlon < -180)) stop("ddlon impossible")

  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(all_of(col_names_corresp))

  new_data_renamed <-
    new_data_renamed %>%
    mutate(
      data_modif_d = lubridate::day(Sys.Date()),
      data_modif_m = lubridate::month(Sys.Date()),
      data_modif_y = lubridate::year(Sys.Date())
    )

  add <- utils::askYesNo(msg = "Add these data to the table of plot data?")

  if(add) {
    DBI::dbWriteTable(mydb, "data_liste_plots", new_data_renamed, append = TRUE, row.names = FALSE)
    cli::cli_alert_success("{nrow(new_data_renamed)} plot imported in data_liste_plots")

    ids_list_plot <-
      try_open_postgres_table(table = "data_liste_plots", con = mydb) %>%
      filter(plot_name %in% !!new_data_renamed$plot_name) %>%
      collect() %>%
      dplyr::select(id_liste_plots, plot_name)

    if (exists("id_team_leader")) {

      id_team_leader <-
        id_team_leader %>%
        left_join(ids_list_plot, by = c("plot_name" = "plot_name"))

      add_subplot_features(new_data = id_team_leader,
                           id_plot_name = "id_liste_plots",
                           subplottype_field = c("team_leader"),
                           add_data = T,
                           ask_before_update = F)

    }

    if (exists("id_pi")) {

      id_pi <-
        id_pi %>%
        left_join(ids_list_plot, by = c("plot_name" = "plot_name")) %>%
        rename(principal_investigator = PI)

      add_subplot_features(new_data = id_pi,
                           id_plot_name = "id_liste_plots",
                           subplottype_field = c("principal_investigator"),
                           add_data = T,
                           ask_before_update = F)

    }

    if (exists("add_col_sep")) {

      add_col_sep <-
        add_col_sep %>%
        left_join(ids_list_plot, by = c("plot_name" = "plot_name"))

      add_subplot_features(new_data = add_col_sep,
                           id_plot_name = "id_liste_plots",
                           subplottype_field = c("additional_people"),
                           add_data = T,
                           ask_before_update = F)

    }

    if (exists("data_manager_sep")) {

      data_manager_sep <-
        data_manager_sep %>%
        left_join(ids_list_plot, by = c("plot_name" = "plot_name"))

      add_subplot_features(new_data = data_manager_sep,
                           id_plot_name = "id_liste_plots",
                           subplottype_field = c("data_manager"),
                           add_data = T,
                           ask_before_update = F)

    }

  }

  if(!add)
     message("no data added")

  return(new_data_renamed)

}








#' Internal function
#'
#' Compare two rows and provide cols that differ and html with coloring different values
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param vec_1 tibble one row and same col numbers of vec_2
#' @param vec_2 tibble one row and same col numbers of vec_1
#'
#' @importFrom kableExtra cell_spec kable_styling
#'
#' @return A list with one tibble of logical and a html table
#' @export
.comp_print_vec <- function(vec_1, vec_2) {

  # vec_2 <- vec_2 %>%
  #   replace(., is.na(.), -9999)

  vars_2_num <- names(vec_2)[unlist(lapply(vec_2, is.numeric))]
  vars_2_char <- names(vec_2)[unlist(lapply(vec_2, is.character))]

  vec_2 <- vec_2 %>%
    mutate_at(vars(all_of(c(
      vars_2_num
    ))),
    ~ tidyr::replace_na(. , -9999))

  vec_2 <- vec_2 %>%
    mutate_at(vars(all_of(c(
      vars_2_char
    ))),
    ~ tidyr::replace_na(. , "-9999"))

  vars_1_num <- names(vec_2)[unlist(lapply(vec_1, is.numeric))]
  vars_1_char <- names(vec_2)[unlist(lapply(vec_1, is.character))]

  vec_1 <- vec_1 %>%
    mutate_at(vars(all_of(c(
      vars_1_num
    ))),
    ~ tidyr::replace_na(. , -9999))
  vec_1 <- vec_1 %>%
    mutate_at(vars(all_of(c(
      vars_1_char
    ))),
    ~ tidyr::replace_na(. , "-9999"))

  vec_2_miss <- vec_2
  vec_2_miss <- vec_2_miss %>%
    mutate_if(is.character, list(~ if_else(. == "-9999", TRUE, FALSE)))
  vec_2_miss <- vec_2_miss %>%
    mutate_if(is.numeric, list(~ if_else(. == -9999, TRUE, FALSE)))

  vec_1_miss <- vec_1
  vec_1_miss <- vec_1_miss %>%
    mutate_if(is.character, list(~ if_else(. == "-9999", TRUE, FALSE)))
  vec_1_miss <- vec_1_miss %>%
    mutate_if(is.numeric, list(~ if_else(. == -9999, TRUE, FALSE)))

  comp_val <-
    vec_1 != vec_2

  comp_val <-
    as_tibble(comp_val)

  comp_val <-
    comp_val %>%
    dplyr::select_if(~isTRUE(.))

  if (ncol(comp_val) > 0) {

    if (any(colnames(vec_1) == "idtax_n")) {
      old_tax <-
        query_taxa(ids = vec_2$idtax_n, check_synonymy = F, class = NULL, extract_traits = F)

      new_tax <-
        query_taxa(ids = vec_1$idtax_n, check_synonymy = F, class = NULL, extract_traits = F)

      vec_1 <-
        vec_1 %>%
        dplyr::left_join(
          new_tax %>%
            dplyr::select(tax_fam, tax_gen, tax_esp, idtax_n),
          by = c("idtax_n" = "idtax_n")
        )

      vec_2 <-
        vec_2 %>%
        dplyr::left_join(
          old_tax %>%
            dplyr::select(tax_fam, tax_gen, tax_esp, idtax_n),
          by = c("idtax_n" = "idtax_n")
        )

    }

    comp_tb <-
      tibble(
        cols = colnames(vec_1),
        current = unlist(vec_1),
        new = unlist(vec_2)
      )

    comp_tb_html <- comp_tb

    comp_tb_html <-
      comp_tb_html %>%
      replace(., is.na(.),"-9999") %>%
      mutate(new :=
               kableExtra::cell_spec(new, "html",
                                     color = ifelse(new != current, "red", "blue"))) %>%
      replace(., . == "-9999", NA)

    comp_tb_html <-
      comp_tb_html %>%
      kableExtra::kable(format = "html", escape = F) %>%
      kableExtra::kable_styling("striped", full_width = F)

    return(list(comp_tb = comp_val, comp_html = comp_tb_html))

  } else{

    return(list(comp_tb = FALSE, comp_html = NA))

  }

  # print(comp_tb_html)

}


# update_dico_name <- function(genus_searched = NULL,
#                              tax_esp_searched = NULL,
#                              tax_fam_searched = NULL,
#                              # tax_order_searched = NULL,
#                              id_searched = NULL,
#                              new_tax_gen = NULL,
#                              new_tax_esp = NULL,
#                              new_tax_fam = NULL,
#                              # new_tax_order = NULL,
#                              new_tax_rank1 = NULL,
#                              new_tax_name1 = NULL,
#                              # new_introduced_status = NULL,
#                              ask_before_update = TRUE,
#                              add_backup = TRUE,
#                              show_results = TRUE,
#                              cancel_synonymy= FALSE,
#                              synonym_of = NULL,
#                              exact_match = FALSE) {
#
#   if(!exists("mydb")) call.mydb()
#
#   if(all(is.null(c(genus_searched, tax_esp_searched,
#                    tax_fam_searched, synonym_of,
#                    id_searched)) & !cancel_synonymy))
#     stop("\n Provide the species to be updated or precise new synonymy")
#
#   ### checking if at least one modification is asked
#   new_vals <- c(new_tax_gen = new_tax_gen,
#                 new_tax_esp = new_tax_esp,
#                 new_tax_fam = new_tax_fam)
#   if (!any(!is.null(new_vals)) &
#       is.null(synonym_of) &
#       !cancel_synonymy)
#     stop("\n No new values to be updated.")
#
#   ### querying for entries to be modified
#   if(is.null(id_searched)) {
#     cat(paste("\n", genus_searched, " - ", tax_esp_searched, "-", tax_fam_searched))
#     query_tax <-
#       query_tax_all(genus_searched = genus_searched, tax_esp_searched = tax_esp_searched,
#                     tax_fam_searched = tax_fam_searched)
#     #    , check_synonymy = FALSE,
#     # exact_match = exact_match
#   }else{
#     query_tax <-
#       query_tax_all(id_search = id_searched)
#     # , check_synonymy = FALSE
#   }
#
#   if(is.null(query_tax)) query_tax <- tibble()
#
#   if(nrow(query_tax)>0) {
#     message(paste("\n ", nrow(query_tax), "taxa selected"))
#     print(query_tax %>% as.data.frame())
#     nrow_query=TRUE
#   }else{
#     nrow_query=FALSE
#   }
#
#   if(nrow_query)
#     modif_types <-
#     vector(mode = "character", length = nrow(query_tax))
#
#   ## if the modification does not concern synonymies, check if provided values are different for those existing
#   if(nrow_query & !cancel_synonymy & is.null(synonym_of)) {
#
#     query_tax_n <- query_tax
#     col_new <- c()
#     for (i in c("tax_esp", "tax_fam",
#                 "tax_gen", "tax_rank01", "tax_nam01")) {
#       if(any(grepl(pattern = i, x = names(new_vals)))) {
#         col_new <- c(col_new, i)
#         var <- enquo(i)
#         query_tax_n <-
#           query_tax_n %>%
#           dplyr::mutate(!!var := new_vals[grep(i, names(new_vals))])
#       }
#     }
#
#     query_tax_n <-
#       query_tax_n %>%
#       dplyr::select(col_new)
#
#     # new_vals <-
#     #   dplyr::tibble(
#     #     tax_order = ifelse(!is.null(new_tax_order), new_tax_order, query_tax$tax_order),
#     #     tax_fam = ifelse(!is.null(new_tax_fam), as.character(new_tax_fam), query_tax$tax_fam),
#     #     tax_gen = ifelse(!is.null(new_tax_gen), as.character(new_tax_gen), query_tax$tax_gen),
#     #     tax_esp = ifelse(!is.null(new_tax_esp), as.character(new_tax_esp), query_tax$tax_esp),
#     #     tax_rank1 = ifelse(!is.null(new_tax_rank1), new_tax_rank1, query_tax$tax_rank01),
#     #     tax_name1 = ifelse(!is.null(new_tax_name1), new_tax_name1, query_tax$tax_nam01),
#     #     introduced_status = ifelse(!is.null(new_introduced_status), new_introduced_status, query_tax$introduced_status)
#     #                 )
#
#     query_tax_n <-
#       query_tax_n %>%
#       replace(., is.na(.), -9999)
#
#     sel_query_tax <-
#       dplyr::bind_rows(query_tax_n, query_tax %>%
#                          dplyr::select(col_new))
#
#     sel_query_tax <-
#       sel_query_tax %>%
#       replace(., is.na(.), -9999)
#
#     print(sel_query_tax)
#
#     comp_vals <-
#       apply(
#         sel_query_tax,
#         MARGIN = 2,
#         FUN = function(x)
#           unique(x[nrow(query_tax_n)]) != x[(nrow(query_tax_n) + 1):length(x)]
#       )
#
#     # comp_vals <-
#     #   apply(sel_query_tax, MARGIN = 2, FUN = function(x) x[1]!=x[2:length(x)])
#
#     if(!is.null(nrow(comp_vals))) {
#       query_tax <-
#         query_tax[apply(comp_vals, MARGIN = 1, FUN = function(x) any(x)),]
#       modif_types <- modif_types[apply(comp_vals, MARGIN = 1, FUN = function(x) any(x))]
#       comp_vals <-
#         apply(comp_vals, MARGIN = 2, FUN = function(x) any(x))
#     }else{
#       query_tax <- query_tax
#     }
#
#     if(any(is.na(comp_vals)))
#       comp_vals <-
#       comp_vals[!is.na(comp_vals)]
#
#     modif_types[1:length(modif_types)] <-
#       paste0(modif_types, rep(paste(names(comp_vals)[comp_vals], sep=", "), length(modif_types)), collapse ="__")
#
#   }else{
#     comp_vals <- TRUE
#   }
#
#   new_id_diconame_good <- NULL
#   if(nrow_query & cancel_synonymy) {
#     if(query_tax$id_good_n == query_tax$id_n) {
#
#       cat("\n This taxa is not considered as synonym. No modification is thus done on its synonymy")
#       comp_vals <- FALSE
#
#     }else{
#
#       new_id_diconame_good <- NA
#
#       modif_types[1:length(modif_types)] <-
#         paste(modif_types, "cancel_synonymy", sep="__")
#
#     }
#
#   }
#
#
#   Q.syn2 <- FALSE
#   if(nrow_query & !is.null(synonym_of)) {
#     Q.syn <- TRUE
#
#     ## checking if taxa selected is already a synonym of another taxa
#     if(query_tax$id_good_n != query_tax$id_n) {
#
#       if(query_tax$id_good_n != query_tax$id_n) {
#
#         query_tax_all(id_search = query_tax$id_good_n)
#
#         Q.syn <-
#           utils::askYesNo("Taxa selected is already a synonym of this taxa. Are you sure you want to modify this?", default = FALSE)
#
#       }
#     }
#
#     if(Q.syn) {
#
#       ## checking if others names are pointing to the selected taxa as synonyms
#       syn_of_new_syn <-
#         tbl(mydb, "diconame") %>%
#         filter(id_good_n == !!query_tax$id_n) %>%
#         collect()
#
#       if(nrow(syn_of_new_syn) > 0) {
#
#         message("\n Some names are considered synonyms of the selected taxa:")
#
#         print(
#           syn_of_new_syn %>%
#             dplyr::select(
#               tax_fam,
#               tax_gen,
#               tax_esp,
#               tax_rank1,
#               tax_name1,
#               id_n,
#               id_good_n
#             )
#         )
#
#         Q.syn2 <-
#           utils::askYesNo("Do you confirm to also modify the synonymies of these selected names?",
#                           default = FALSE)
#
#         if(Q.syn2)
#           ids_others_names_synonyms <-
#           syn_of_new_syn$id_n
#
#       }
#
#       # if(Q.syn2) {
#
#       if (!any(names(synonym_of) == "genus"))
#         synonym_of$genus <- NULL
#       if (!any(names(synonym_of) == "species"))
#         synonym_of$species <- NULL
#       if (!any(names(synonym_of) == "id"))
#         synonym_of$id <- NULL
#
#       new_syn <-
#         query_tax_all(genus_searched = synonym_of$genus,
#                       tax_esp_searched = synonym_of$species,
#                       id_search = synonym_of$id,
#                       show_synonymies = FALSE)
#
#       if(nrow(new_syn) == 0) {
#
#         cat("\n No taxa found for new synonymy. Select one.")
#         Q.syn <- FALSE
#
#       }
#
#       if(nrow(new_syn) > 1) {
#
#         cat("\n More than one taxa found for new synonymy. Select only one.")
#         Q.syn <- FALSE
#
#       }
#
#       if(nrow(new_syn) == 1) {
#
#         # message("\n synonym of:")
#         # print(new_syn %>% as.data.frame())
#
#         new_id_diconame_good <- new_syn$id_n
#
#         modif_types[1:length(modif_types)] <-
#           paste(modif_types, "new_synonymy", sep="__")
#
#       }
#
#       # }
#     }
#
#   }else{
#     Q.syn <- TRUE
#   }
#
#   # if(!any(comp_vals)) stop("No update performed because no values are different.")
#
#   if(any(comp_vals) & Q.syn & nrow_query) {
#
#     cli::cli_alert_info(" Number of rows selected to be updated {nrow(query_tax)} ")
#
#     if(ask_before_update) {
#       Q <-
#         utils::askYesNo(msg = "Do you confirm you want to update these rows for selected fields?", default = FALSE)
#     }else{
#       Q <- TRUE
#     }
#
#     if(Q) {
#
#       if(add_backup) {
#
#         query_tax <-
#           query_tax %>%
#           tibble::add_column(date_modified = Sys.Date()) %>%
#           tibble::add_column(modif_type = modif_types)
#
#         DBI::dbWriteTable(mydb, "followup_updates_diconames",
#                           query_tax, append = TRUE, row.names = FALSE)
#
#         if(Q.syn2) {
#           syn_of_new_syn <-
#             syn_of_new_syn %>%
#             tibble::add_column(date_modified = Sys.Date()) %>%
#             tibble::add_column(modif_type = modif_types)
#
#           DBI::dbWriteTable(mydb, "followup_updates_diconames",
#                             syn_of_new_syn, append = TRUE, row.names = FALSE)
#
#
#         }
#       }
#
#       rs <-
#         DBI::dbSendQuery(
#           mydb,
#           statement = "UPDATE diconame SET tax_fam=$2, tax_gen=$3, tax_esp=$4, id_good_n=$5, tax_rank1=$6, tax_name1=$7 WHERE id_n = $1",
#           params = list(
#             query_tax$id_n,
#             # $1
#             rep(
#               ifelse(!is.null(new_tax_fam), new_tax_fam, query_tax$tax_fam),
#               nrow(query_tax)
#             ),
#             # $2
#             rep(
#               ifelse(!is.null(new_tax_gen), new_tax_gen, query_tax$tax_gen),
#               nrow(query_tax)
#             ),
#             # $3
#             rep(
#               ifelse(!is.null(new_tax_esp), new_tax_esp, query_tax$tax_esp),
#               nrow(query_tax)
#             ),
#             # $4
#             # rep(ifelse(!is.null(new_tax_order), new_tax_order, query_tax$tax_order), nrow(query_tax)), # $5
#             rep(
#               ifelse(
#                 !is.null(new_id_diconame_good),
#                 new_id_diconame_good,
#                 query_tax$id_good_n
#               ),
#               nrow(query_tax)
#             ),
#             # $5
#             rep(
#               ifelse(!is.null(new_tax_rank1), new_tax_rank1, query_tax$tax_rank1),
#               nrow(query_tax)
#             ),
#             # $6
#             rep(
#               ifelse(!is.null(new_tax_name1), new_tax_name1, query_tax$tax_name1),
#               nrow(query_tax)
#             )
#           )
#         ) # $7
#
#       DBI::dbClearResult(rs)
#
#       rs <-
#         DBI::dbSendQuery(mydb, statement="SELECT *FROM diconame WHERE id_n = $1",
#                          params=list(query_tax$id_n))
#       if(show_results) print(DBI::dbFetch(rs))
#       DBI::dbClearResult(rs)
#
#       if(Q.syn2) {
#         cli::cli_alert_info("Updating synonymies for others taxa")
#         rs <-
#           DBI::dbSendQuery(mydb, statement="UPDATE diconame SET id_good_n=$2 WHERE id_n = $1",
#                            params= list(ids_others_names_synonyms, # $1
#                                         rep(ifelse(!is.null(new_id_diconame_good),
#                                                    new_id_diconame_good, syn_of_new_syn$id_good_n),
#                                             nrow(syn_of_new_syn)) # $2
#                            ))
#
#         DBI::dbClearResult(rs)
#
#         rs <-
#           DBI::dbSendQuery(mydb, statement="SELECT *FROM diconame WHERE id_n = $1",
#                            params=list(ids_others_names_synonyms))
#         if(show_results) print(DBI::dbFetch(rs))
#         DBI::dbClearResult(rs)
#
#
#       }
#
#     }
#   }else{
#     if(nrow(query_tax)==0) print("No update because no taxa found.")
#
#     if(!any(comp_vals)) print("No update performed because no values are different.")
#
#     if(!Q.syn) print("No update because new synonymy not correctly defined.")
#
#     if(!nrow_query) print("No updates because none taxa were found based on query parameters (genus/species/family/id)")
#
#   }
# }


# update_dico_name <- function(genus_searched = NULL,
#                              tax_esp_searched = NULL,
#                              tax_fam_searched = NULL,
#                              new_tax_gen = NULL,
#                              new_tax_esp = NULL,
#                              new_full_name_auth = NULL,
#                              new_tax_fam = NULL,
#                              new_tax_rank1 = NULL,
#                              new_tax_name1 = NULL,
#                              new_taxook = NULL,
#                              new_morphocat = NULL,
#                              new_detvalue = NULL,
#                              new_full_name_no_auth = NULL,
#                              new_full_name_used = NULL,
#                              new_full_name_used2 = NULL,
#                              new_id_diconame_good = NULL,
#                              id_search = NULL,
#                              ask_before_update = TRUE,
#                              add_backup = TRUE,
#                              show_results = TRUE,
#                              no_synonym_modif = FALSE,
#                              synonym_of = NULL
# ) {
#
#   if(!exists("mydb")) call.mydb()
#
#   if(all(is.null(c(genus_searched, tax_esp_searched, tax_fam_searched, synonym_of, id_search, new_full_name_no_auth, new_full_name_used, new_full_name_used2, new_morphocat))) & !no_synonym_modif) stop("\n Provide the species to be updated or precise new synonymy")
#
#   ### checking if at least one modification is asked
#   new_vals <- c(new_tax_gen, new_tax_esp, new_full_name_auth, new_tax_fam, new_taxook,new_detvalue, new_id_diconame_good, no_synonym_modif, new_full_name_no_auth, new_full_name_used, new_full_name_used2, new_morphocat)
#   if(!any(!is.null(new_vals)) & is.null(synonym_of)) stop("\n No new values to be updated.")
#
#   ## if the modif is a change in synonymy, show synonyms
#   if(no_synonym_modif | !is.null(synonym_of)) {
#     show_synonymies <- TRUE
#   }else{
#     show_synonymies <- FALSE
#   }
#
#   ### querying for entries to be modified
#   if(is.null(id_search)) {
#     query_tax <-
#       query_tax_all(genus_searched = genus_searched, tax_esp_searched = tax_esp_searched, tax_fam_searched = tax_fam_searched, show_synonymies=show_synonymies)
#   }else{
#     query_tax <-
#       query_tax_all(id_search = id_search, show_synonymies = FALSE)
#   }
#
#   if(nrow(query_tax)>0) {
#     nrow_query=TRUE
#   }else{
#     nrow_query=FALSE
#   }
#
#   if(nrow_query)
#     modif_types <-
#     vector(mode = "character", length = nrow(query_tax))
#
#   ## if the modification does not concern synonymies, check if provided values are different for those existing
#   if(nrow_query & !no_synonym_modif & is.null(synonym_of)) {
#     new_vals <-
#       dplyr::tibble(tax_fam=ifelse(!is.null(new_tax_fam), as.character(new_tax_fam), query_tax$tax_fam),
#                     tax_gen=ifelse(!is.null(new_tax_gen), as.character(new_tax_gen), query_tax$tax_gen),
#                     tax_esp=ifelse(!is.null(new_tax_esp), as.character(new_tax_esp), query_tax$tax_esp),
#                     tax_rank=ifelse(!is.null(new_tax_rank1), new_tax_rank1, query_tax$tax_rank1),
#                     new_tax_name1=ifelse(!is.null(new_tax_name1), new_tax_name1, query_tax$tax_name1), # $4
#                     taxook=ifelse(!is.null(new_taxook), new_taxook, query_tax$taxook), # $5
#                     full_name=ifelse(!is.null(new_full_name_auth), as.character(new_full_name_auth), query_tax$full_name), # $6
#                     detvalue=ifelse(!is.null(new_detvalue), new_detvalue, query_tax$detvalue), # $7
#                     id_good_n=ifelse(!is.null(new_id_diconame_good), new_id_diconame_good, query_tax$id_good_n),
#                     full_name_no_auth=ifelse(!is.null(new_full_name_no_auth), as.character(new_full_name_no_auth), query_tax$full_name_no_auth),
#                     full_name_used=ifelse(!is.null(new_full_name_used), as.character(new_full_name_used), query_tax$full_name_used),
#                     full_name_used2=ifelse(!is.null(new_full_name_used2), as.character(new_full_name_used2), query_tax$full_name_used2),
#                     morphocat=ifelse(!is.null(new_morphocat), new_morphocat, query_tax$morphocat))
#
#     # comp_vals <-
#     #   query_tax %>% dplyr::select(tax_fam, tax_gen, tax_esp, taxook, full_name, detvalue, id_good_n) != new_vals
#
#     new_vals <-
#       new_vals %>%
#       replace(., is.na(.), -9999)
#
#     sel_query_tax <-
#       dplyr::bind_rows(new_vals, query_tax %>% dplyr::select(tax_fam, tax_gen, tax_esp, tax_rank1, tax_name1, taxook, full_name, detvalue, id_good_n, full_name_no_auth, full_name_used, full_name_used2, morphocat))
#
#     sel_query_tax <-
#       sel_query_tax %>%
#       replace(., is.na(.), -9999)
#
#     comp_vals <-
#       apply(sel_query_tax, MARGIN = 2, FUN = function(x) x[1]!=x[2:length(x)])
#
#     if(!is.null(nrow(comp_vals))) {
#       query_tax <-
#         query_tax[apply(comp_vals, MARGIN = 1, FUN = function(x) any(x)),]
#       comp_vals <-
#         apply(comp_vals, MARGIN = 2, FUN = function(x) any(x))
#     }else{
#       query_tax <- query_tax
#     }
#
#     if(any(is.na(comp_vals))) comp_vals <- comp_vals[!is.na(comp_vals)]
#
#     modif_types[1:length(modif_types)] <-
#       paste(modif_types, rep(paste(names(comp_vals)[comp_vals], sep=", "), length(modif_types)), collapse ="__")
#
#   }else{
#     comp_vals <- TRUE
#   }
#
#   if(nrow_query & no_synonym_modif) {
#     if(query_tax$id_good_n==query_tax$id_n) {
#       cat("\n This taxa is not considered as synonym. No modification is thus done on its synonymy")
#       comp_vals <- FALSE
#     }else{
#       new_id_diconame_good <- query_tax$id_n
#
#       modif_types[1:length(modif_types)] <-
#         paste(modif_types, "cancel_synonymy", sep="__")
#
#     }
#   }
#
#   ## checking if taxa selected is already a synonym of another taxa
#   if(nrow_query & !is.null(synonym_of)) {
#     Q.syn <- TRUE
#     if(query_tax$id_good_n != query_tax$id_n) {
#       query_tax_all(id_search = query_tax$id_good_n, show_synonymies = FALSE)
#       Q.syn <-
#         utils::askYesNo("Taxa selected is already a synonym of this taxa. Are you sure you want to modify this?", default = FALSE)
#     }
#
#     if(Q.syn) {
#
#       ## checking if others names are pointing to the selected taxa as synonyms
#       syn_of_new_syn <-
#         tbl(mydb, "diconame") %>%
#         filter(id_good_n == !!query_tax$id_n) %>%
#         collect()
#
#       if(nrow(syn_of_new_syn)>0) {
#
#         message("\n Some names are considered synonyms of the selected taxa:")
#
#         print(
#           syn_of_new_syn %>%
#             dplyr::select(
#               full_name_no_auth,
#               tax_fam,
#               tax_gen,
#               tax_esp,
#               tax_rank1,
#               tax_name1,
#               id_n,
#               id_good_n
#             )
#         )
#
#         Q.syn2 <-
#           utils::askYesNo("Do you confirm to also modify the synonymies of these selected names?",
#                           default = FALSE)
#
#         if(Q.syn2)
#
#           ids_others_names_synonyms <-
#           syn_of_new_syn$id_n
#
#       }
#
#
#
#       if(!any(names(synonym_of)=="genus")) synonym_of$genus <- NULL
#       if(!any(names(synonym_of)=="species")) synonym_of$species <- NULL
#       if(!any(names(synonym_of)=="id")) synonym_of$id <- NULL
#
#       new_syn <-
#         query_tax_all(genus_searched = synonym_of$genus, tax_esp_searched = synonym_of$species, id_search = synonym_of$id, show_synonymies = FALSE)
#
#
#       if(nrow(new_syn)==0) {
#         cat("\n No taxa found for new synonymy. Select one.")
#         Q.syn <- FALSE
#       }
#
#       if(nrow(new_syn)>1) {
#         cat("\n More than one taxa found for new synonymy. Select only one.")
#         Q.syn <- FALSE
#       }
#
#       if(nrow(new_syn) == 1) {
#
#
#         new_id_diconame_good <- new_syn$id_n
#
#         modif_types[1:length(modif_types)] <-
#           paste(modif_types, "new_synonymy", sep="__")
#
#       }
#     }
#   }else{
#     Q.syn <- TRUE
#   }
#
#   # if(!any(comp_vals)) stop("No update performed because no values are different.")
#
#   if(any(comp_vals) & Q.syn & nrow_query) {
#
#     cat(paste("\n Number of rows selected to be updated :", nrow(query_tax), "\n"))
#
#     if(ask_before_update) {
#       Q <-
#         utils::askYesNo(msg = "Do you confirm you want to update these rows for selected fields?", default = FALSE)
#     }else{
#       Q <- TRUE
#     }
#
#     if(Q) {
#
#       if(add_backup) {
#
#         query_tax <-
#           query_tax %>%
#           tibble::add_column(date_modified = Sys.Date()) %>%
#           tibble::add_column(modif_type = modif_types)
#
#         DBI::dbWriteTable(
#           mydb,
#           "followup_updates_diconames",
#           query_tax,
#           append = TRUE,
#           row.names = FALSE
#         )
#
#         # query_p <-
#         #     paste0("INSERT INTO followup_updates_diconames VALUES(", paste0(rep("$", ncol(query_tax)), seq(1, ncol(query_tax), 1), collapse = ", "), ")",
#         #            collapse = "")
#
#         # rs <-
#         #     dbSendQuery(mydb, statement=query_p, params=as.list(query_tax %>% slice(1) %>%  unlist(., use.names=FALSE)))
#         #
#         # dbClearResult(rs)
#
#       }
#
#       # tbl(mydb, "followup_updates_diconames")
#
#       rs <-
#         DBI::dbSendQuery(mydb, statement="UPDATE diconame SET tax_fam=$2, tax_gen=$3, tax_esp=$4, taxook=$5, full_name=$6, detvalue=$7, id_good_n=$8, full_name_no_auth=$9, full_name_used=$10, full_name_used2=$11, morphocat=$12, tax_rank1=$13, tax_name1=$14 WHERE id_n = $1",
#                          params=list(query_tax$id_n, # $1
#                                      rep(ifelse(!is.null(new_tax_fam), new_tax_fam, query_tax$tax_fam), nrow(query_tax)), # $2
#                                      rep(ifelse(!is.null(new_tax_gen), new_tax_gen, query_tax$tax_gen), nrow(query_tax)), # $3
#                                      rep(ifelse(!is.null(new_tax_esp), new_tax_esp, query_tax$tax_esp), nrow(query_tax)), # $4
#                                      rep(ifelse(!is.null(new_taxook), new_taxook, query_tax$taxook), nrow(query_tax)), # $5
#                                      rep(ifelse(!is.null(new_full_name_auth), new_full_name_auth, query_tax$full_name), nrow(query_tax)), # $6
#                                      rep(ifelse(!is.null(new_detvalue), new_detvalue, query_tax$detvalue), nrow(query_tax)), # $7
#                                      rep(ifelse(!is.null(new_id_diconame_good), new_id_diconame_good, query_tax$id_good_n), nrow(query_tax)), # $8
#                                      rep(ifelse(!is.null(new_full_name_no_auth), new_full_name_no_auth, query_tax$full_name_no_auth), nrow(query_tax)), # $9
#                                      rep(ifelse(!is.null(new_full_name_used), new_full_name_used, query_tax$full_name_used), nrow(query_tax)), # $10
#                                      rep(ifelse(!is.null(new_full_name_used2), new_full_name_used2, query_tax$full_name_used2), nrow(query_tax)), # $11
#                                      rep(ifelse(!is.null(new_morphocat), new_morphocat, query_tax$morphocat), nrow(query_tax)), # $12
#                                      rep(ifelse(!is.null(new_tax_rank1), new_tax_rank1, query_tax$tax_rank1), nrow(query_tax)), # $13
#                                      rep(ifelse(!is.null(new_tax_name1), new_tax_name1, query_tax$tax_name1), nrow(query_tax)))) # $14
#
#       DBI::dbClearResult(rs)
#
#       rs <-
#         DBI::dbSendQuery(mydb, statement="SELECT *FROM diconame WHERE id_n = $1",
#                          params=list(query_tax$id_n))
#       if(show_results) print(DBI::dbFetch(rs))
#       DBI::dbClearResult(rs)
#
#       if(Q.syn2) {
#
#         message("\n updating synonymies for others taxa")
#
#         rs <-
#           DBI::dbSendQuery(mydb, statement="UPDATE diconame SET id_good_n=$2 WHERE id_n = $1",
#                            params = list(ids_others_names_synonyms, # $1
#                                         rep(ifelse(!is.null(new_id_diconame_good),
#                                                    new_id_diconame_good, syn_of_new_syn$id_good_n),
#                                             nrow(syn_of_new_syn)) # $2
#                            ))
#
#         DBI::dbClearResult(rs)
#
#         rs <-
#           DBI::dbSendQuery(mydb, statement="SELECT *FROM diconame WHERE id_n = $1",
#                            params=list(ids_others_names_synonyms))
#         if(show_results) print(DBI::dbFetch(rs))
#         DBI::dbClearResult(rs)
#
#
#       }
#     }
#
#   }else{
#     if(nrow(query_tax)==0) print("No update because no taxa found.")
#
#     if(!any(comp_vals)) print("No update performed because no values are different.")
#
#     if(!Q.syn) print("No update because new synonymy not correctly defined.")
#
#     if(!nrow_query) print("No updates because none taxa were found based on query parameters (genus/species/family/id)")
#
#   }
#
#   # dbDisconnect(mydb)
#
# }












#' Add new individuals data
#'
#' Add new individuals data
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data tibble new data to be import
#' @param col_names_select string
#' @param col_names_corresp string
#' @param id_col integer indicate which name of col_names_select is the id for matching plot in metadata
#' @param launch_adding_data logical FALSE whether adding should be done or not
#'
#' @return No return value individuals updated
#' @export
add_individuals <- function(new_data ,
                            col_names_select,
                            col_names_corresp,
                            id_col,
                            launch_adding_data = FALSE) {

  logs <-
    dplyr::tibble(
      column = as.character(),
      note = as.character()
    )

  if(!exists("mydb")) call.mydb()

  if(length(col_names_select) != length(col_names_corresp))
    stop("Provide same numbers of corresponding and selected colnames")

  # new_data_renamed <-
  #   new_data %>%
  #   dplyr::rename_at(dplyr::vars(col_names_select[id_col]), ~ col_names_corresp[id_col])

  new_data_renamed <-
    .rename_data(dataset = new_data,
                 col_old = col_names_select,
                 col_new = col_names_corresp)

  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(all_of(col_names_corresp))

  # new_data_renamed <-
  #   new_data %>%
  #   dplyr::rename(plot_name= !!vars(col_names_select[id_col]))

  ### Linking plot names

  # new_data_renamed <-
  #   .link_plot_name(data_stand = new_data_renamed, plot_name_field = "plot_name")

  new_data_renamed <-
    .link_table(data_stand = new_data_renamed,
              column_searched = "plot_name",
              column_name = "plot_name",
              id_field = "id_liste_plots",
              id_table_name = "id_liste_plots",
              db_connection = mydb,
              table_name = "data_liste_plots",
              keep_columns = "plot_name")


  ids_plot <-
    new_data_renamed %>%
    dplyr::select(plot_name, id_liste_plots) %>%
    dplyr::distinct(plot_name, id_liste_plots)

  if(any(is.na(ids_plot$id_liste_plots))) {
    warning("some plot are not found in metadata")
    print(ids_plot %>%
            dplyr::filter(is.na(id_liste_plots)))
    ids_plot <-
      ids_plot %>%
      dplyr::filter(is.na(id_liste_plots))

    logs <-
      dplyr::bind_rows(logs,
                     dplyr::tibble(
                       column = "id_liste_plots",
                       note = paste(nrow(ids_plot %>%
                                            dplyr::filter(is.na(id_liste_plots))),
                                     "some plot are not found in metadata")
                     ))
  }

  plots_already_in_db <-
    dplyr::tbl(mydb, "data_individuals") %>%
    dplyr::filter(id_table_liste_plots_n %in% !!ids_plot$id_liste_plots) %>%
    dplyr::distinct(id_table_liste_plots_n) %>%
    dplyr::collect()

  if (nrow(plots_already_in_db) > 0) {
    print(
      plots_already_in_db %>%
        dplyr::left_join(
          dplyr::tbl(mydb, "data_liste_plots") %>%
            dplyr::select(plot_name, id_liste_plots) %>%
            dplyr::collect(),
          by = c("id_table_liste_plots_n" = "id_liste_plots")
        ) %>%
        dplyr::pull(plot_name)
    )
    warning("data for some plots already in database")

  }


  new_data_renamed <-
    new_data_renamed %>%
    dplyr::left_join(ids_plot) %>%
    dplyr::rename(id_table_liste_plots_n = id_liste_plots)


  col_names_select <-
    col_names_select[-id_col]
  col_names_corresp <-
    col_names_corresp[-id_col]



  col_names_corresp <-
    c(col_names_corresp, "id_table_liste_plots_n")

  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(all_of(col_names_corresp))

  ### CHECKS
  method <-
    ids_plot %>%
    dplyr::left_join(
      dplyr::tbl(mydb, "data_liste_plots") %>%
        dplyr::select(plot_name, id_liste_plots, id_method) %>%
        dplyr::left_join(dplyr::tbl(mydb, "methodslist")) %>%
        dplyr::collect(),
      by = c("id_liste_plots" = "id_liste_plots")
    ) %>%
    dplyr::distinct(method)

  if (nrow(method) > 1) {
    print(method)
    stop("More than one method selected, import plot of one method at a time")
  }

  if (!any(colnames(new_data_renamed) == "idtax_n"))
    stop("idtax_n column missing")

  if (any(new_data_renamed$idtax_n == 0))
    stop(paste(
      "idtax_n is NULL for",
      sum(new_data_renamed$idtax_n == 0),
      "individuals"
    ))

  if (any(is.na(new_data_renamed$idtax_n)))
    stop(paste(
      "idtax_n is missing for",
      sum(new_data_renamed$idtax_n == 0),
      "individuals"
    ))

  unmatch_id_diconame <-
    new_data_renamed %>%
    dplyr::select(idtax_n) %>%
    dplyr::left_join(
      try_open_postgres_table(table = "table_taxa", con = mydb_taxa) %>%
      # dplyr::tbl(mydb_taxa, "table_taxa") %>%
        dplyr::select(idtax_n, id_tax_famclass) %>%
        filter(idtax_n %in% !!new_data_renamed$idtax_n) %>%
        dplyr::collect(),
      by = c("idtax_n" = "idtax_n")
    ) %>%
    dplyr::filter(is.na(id_tax_famclass)) %>%
    dplyr::pull(idtax_n)

  if (length(unmatch_id_diconame) > 0)
    stop(paste("idtax_n not found in diconame", unmatch_id_diconame))

  ## checking DBH
  # if(!any(colnames(new_data_renamed)=="dbh")) stop("dbh column missing")
  if(any(is.na(names(new_data_renamed) == "dbh"))) {

    message("\n dbh and others traits measure should be added independantly using add_traits_measures function")

  }



  ## checking column given method
  if(dplyr::pull(method) == "Large") {

    if (!any(colnames(new_data_renamed) == "sous_plot_name"))
      stop("sous_plot_name column missing")
    if (!any(colnames(new_data_renamed) == "ind_num_sous_plot"))
      stop("ind_num_sous_plot column missing")
    # if (!any(colnames(new_data_renamed) == "dbh"))
    #   stop("dbh column missing")
    # if (!any(colnames(new_data_renamed) == "position_transect"))
    #   stop("position_transect column missing")
    # if (!any(colnames(new_data_renamed) == "strate_cat"))
    #   stop("strate_cat column missing")

    ## checking strate info
    # miss_strate <-
    #   new_data_renamed %>%
    #   dplyr::filter(!strate_cat %in% c("Ad", "Ado"))

    # if (nrow(miss_strate) > 0) {
    #   warning(paste(
    #     "strate_cat missing or not equal to Ad or Ado for",
    #     nrow(miss_strate),
    #     "individuals"
    #   ))
    #   print(miss_strate)
    # }

    ## checking sous_plot_name
    type_sousplot <-
      new_data_renamed %>%
      dplyr::distinct(sous_plot_name) %>%
      dplyr::pull()

    if (!any(type_sousplot == c("A", "B", "C", "D")))
      warning("sous_plot_name should include A B C and D")

    # check ind_num_sous_plot
    for (i in unique(new_data_renamed$id_table_liste_plots_n)) {
      for (j in c("A", "B", "C", "D")) {
        duplicates_ind_plot <-
          new_data_renamed %>%
          dplyr::filter(id_table_liste_plots_n == i, sous_plot_name == j) %>%
          dplyr::group_by(ind_num_sous_plot) %>%
          dplyr::count() %>%
          dplyr::filter(n > 1)

      if(nrow(duplicates_ind_plot)>0) {
        plot_name <-
          dplyr::tbl(mydb, "data_liste_plots") %>%
          dplyr::select(plot_name, id_liste_plots) %>%
          dplyr::filter(id_liste_plots == i) %>%
          dplyr::pull(plot_name)

        warning(paste(nrow(duplicates_ind_plot),
                      "duplicate in ind_num_sous_plot for ", plot_name, j))

        logs <-
          dplyr::bind_rows(logs,
                           dplyr::tibble(
                             column = "ind_num_sous_plot",
                             note = paste(
                               nrow(duplicates_ind_plot),
                               "duplicate in ind_num_sous_plot for ",
                               plot_name,
                               j
                             )
                           ))
      }

      }

    }

  }

  if (dplyr::pull(method) == "1ha-IRD" | dplyr::pull(method) == " ") {
    if (!any(colnames(new_data_renamed) == "ind_num_sous_plot"))
      stop("ind_num_sous_plot column missing - Tag individual")


    ### checking duplicated tags within plots
    duplicated_tags <-
      new_data_renamed %>%
      group_by(id_table_liste_plots_n, ind_num_sous_plot) %>%
      count() %>%
      filter(n > 1)

    duplicated_tags <-
      new_data_renamed %>%
      dplyr::left_join(
        duplicated_tags ,
        by = c(
          "id_table_liste_plots_n" = "id_table_liste_plots_n",
          "ind_num_sous_plot" = "ind_num_sous_plot"
        )
      ) %>%
      dplyr::filter(!is.na(n)) %>%
      dplyr::left_join(
        tbl(mydb, "data_liste_plots") %>%
          dplyr::select(id_liste_plots, plot_name) %>%
          dplyr::collect(),
        by = c("id_table_liste_plots_n" = "id_liste_plots")
      )

    if(nrow(duplicated_tags) > 0) {
      warning("\n Duplicated tags in some plots")
      print(duplicated_tags)

      readr::write_excel_csv(duplicated_tags, "duplicated_tags.csv")
    }

    if(any(names(new_data_renamed) == "multi_tiges_id")) {
      cli::cli_alert_info("Checking multi tiges")

      # all_multi_tiges <- new_data_renamed %>%
      #   filter(!is.na(multi_tiges_id))

      # all_multi_tiges %>%
      #   dplyr::select(idtax_n, id_table_liste_plots_n) %>%
      #   left_join(new_data_renamed %>%
      #               dplyr::select(idtax_n, id_table_liste_plots_n))
      #
      # for (j in 1:length(unique(all_multi_tiges$id_table_liste_plots_n))) {
      #
      #   new_data_subset <-
      #     new_data_renamed %>%
      #     filter(id_table_liste_plots_n == unique(all_multi_tiges$id_table_liste_plots_n)[j])
      #
      #   all_multi_tiges_subset <-
      #     all_multi_tiges %>%
      #     filter(id_table_liste_plots_n == unique(all_multi_tiges$id_table_liste_plots_n)[j])
      #
      #   for (i in 1:nrow(all_multi_tiges_subset)) {
      #
      #     new_data_subset %>%
      #       dplyr::select(ind_num_sous_plot, idtax_n) %>%
      #       filter()
      #
      #
      #
      #   }
      #
      #
      # }


    }

  }

  ## checking ind_num_sous_plot

  if(!is.numeric(new_data_renamed$ind_num_sous_plot)) {

    new_data_renamed <-
      new_data_renamed %>%
      dplyr::mutate(ind_num_sous_plot = as.numeric(ind_num_sous_plot))

    if(any(is.na(new_data_renamed$ind_num_sous_plot)))
      new_data_renamed %>%
      filter(is.na(ind_num_sous_plot)) %>%
      print()
      stop("ind_num_sous_plot missing after converting to numeric")
  }

  # check herbarium specimen coherence

  if (!any(colnames(new_data_renamed) == "herbarium_nbe_type"))
    cli::cli_alert_danger("herbarium_nbe_type column missing")
  if (!any(colnames(new_data_renamed) == "herbarium_nbe_char"))
    cli::cli_alert_danger("herbarium_nbe_char column missing")

  if (any(colnames(new_data_renamed) == "herbarium_nbe_char")) {
    all_herb_ref <-
      new_data_renamed %>%
      dplyr::distinct(herbarium_nbe_char) %>%
      dplyr::filter(!is.na(herbarium_nbe_char))
  }


  if (any(colnames(new_data_renamed) == "herbarium_nbe_type")) {

    all_herb_type <-
      new_data_renamed %>%
      dplyr::distinct(herbarium_nbe_type) %>%
      dplyr::filter(!is.na(herbarium_nbe_type))

    if (nrow(all_herb_type) != nrow(all_herb_ref)) {

      print(all_herb_type)
      print(all_herb_ref)
      cli::cli_alert_warning("Number of herbarium specimen type and reference are not identical")

      missing_herb_ref <-
        all_herb_type %>%
        filter(!herbarium_nbe_type %in% all_herb_ref$herbarium_nbe_char)

      if(nrow(missing_herb_ref) > 0) {
        print(missing_herb_ref)
        stop("Specimen in type not found in reference specimens")
      }

      missing_herb_type <- all_herb_ref %>%
        filter(!herbarium_nbe_char %in% all_herb_type$herbarium_nbe_type)


      if(nrow(missing_herb_type) > 0) {

        cli::cli_alert_danger("Some specimens type not represented in specimens links")

        print(missing_herb_type)

        complete_type_specimen <-
          askYesNo(msg = "Complete automatically type specimen by taking the first individual?")

        if(complete_type_specimen) {

          new_data_renamed <-
            new_data_renamed %>%
            mutate(id_temp = 1:nrow(.))

          for (i in 1:nrow(missing_herb_type)) {

            id_selected <-
              new_data_renamed %>%
              filter(herbarium_nbe_char == missing_herb_type$herbarium_nbe_char[i]) %>%
              arrange(ind_num_sous_plot, id_table_liste_plots_n) %>%
              dplyr::slice(1) %>%
              dplyr::select(id_temp)

            new_data_renamed <-
              new_data_renamed %>%
              mutate(herbarium_nbe_type = replace(herbarium_nbe_type,
                                                  id_temp == id_selected$id_temp,
                                                  missing_herb_type$herbarium_nbe_char[i]))
            # %>%
            #   filter(herbarium_nbe_char == missing_herb_type$herbarium_nbe_char[i]) %>%
            #   dplyr::select(herbarium_nbe_type)




          }

          new_data_renamed <-
            new_data_renamed %>%
            dplyr::select(-id_temp)

        }
      }
    }

    herb_type_dups <-
      new_data_renamed %>%
      dplyr::group_by(herbarium_nbe_type) %>%
      dplyr::count() %>%
      dplyr::filter(n > 1,!is.na(herbarium_nbe_type))

    # herb_type_dups <-
    #   new_data_renamed %>%
    #   distinct(herbarium_nbe_type, id_diconame_n) %>%
    #   filter(!is.na(herbarium_nbe_type), !is.na(id_diconame_n)) %>%
    #   group_by(herbarium_nbe_type) %>%
    #   count() %>%
    #   filter(n>1)

    if (nrow(herb_type_dups) > 0) {

      warning(paste(
        "herbarium_nbe_type is duplicated for",
        nrow(herb_type_dups),
        "specimen"
      ))

      new_data_renamed %>%
        dplyr::filter(herbarium_nbe_type %in% dplyr::pull(herb_type_dups, herbarium_nbe_type))

      logs <-
        dplyr::bind_rows(logs,
                         dplyr::tibble(
                           column = "herbarium_nbe_type",
                           note = paste(
                             "herbarium_nbe_type is duplicated for",
                             paste(
                               dplyr::pull(herb_type_dups, herbarium_nbe_type),
                               collapse = ";"
                             ),
                             "specimen"
                           )
                         ))
    }
  }

  ## check herbarium specimen reference coherence
  if(any(colnames(new_data_renamed)=="herbarium_nbe_char")) {
    herb_ref_multiple_taxa <-
      new_data_renamed %>%
      dplyr::distinct(herbarium_nbe_char, idtax_n) %>%
      dplyr::filter(!is.na(herbarium_nbe_char)) %>%
      dplyr::group_by(herbarium_nbe_char) %>%
      dplyr::count() %>%
      dplyr::filter(n>1)

    herb_ref_multiple_taxa <-
      new_data_renamed %>%
      dplyr::filter(herbarium_nbe_char %in% dplyr::pull(herb_ref_multiple_taxa, herbarium_nbe_char)) %>%
      dplyr::select(herbarium_nbe_char, original_tax_name, idtax_n) %>%
      dplyr::distinct()

    if(nrow(herb_ref_multiple_taxa) > 0) {
      logs <-
        dplyr::bind_rows(logs,
                         dplyr::tibble(
                           column = "herbarium_nbe_char",
                           note = paste("herbarium_nbe_char carry different identification for",
                                        paste(herb_ref_multiple_taxa %>%
                                                dplyr::distinct(herbarium_nbe_char) %>%
                                                dplyr::pull(),
                                              collapse = "; "),
                                        paste(herb_ref_multiple_taxa %>%
                                                dplyr::distinct(original_tax_name) %>%
                                                dplyr::pull(),
                                              collapse = ", "))
                         ))
    }

  }

  new_data_renamed <-
    new_data_renamed %>%
    tibble::add_column(
      data_modif_d = lubridate::day(Sys.Date()),
      data_modif_m = lubridate::month(Sys.Date()),
      data_modif_y = lubridate::year(Sys.Date())
    )

  if(launch_adding_data) {

    print(list(new_data_renamed, logs))

    confirmed <- utils::askYesNo("Confirm adding?")

    if(confirmed) {

      DBI::dbWriteTable(mydb, "data_individuals", new_data_renamed, append = TRUE, row.names = FALSE)
      cli::cli_alert_success("Added individuals : {nrow(new_data_renamed)} rows to individuals table")
    }
  }

  return(list(new_data_renamed, logs))

}








#' Get backups of modified taxonomic data
#'
#' List taxonomic data that has been modified
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id look backups for a specific id (of taxonomic table)
#' @param last_months look backups performed this last month
#' @param last_10_entry look the last 10 backups performed
#' @param last look the last backup
#'
#' @return A tibble
#' @export
get_updates_diconame <- function(id = NULL,
                                 last_months = NULL,
                                 last_10_entry = TRUE,
                                 last = NULL) {


  if(!exists("mydb")) call.mydb()

  tb <-
    dplyr::tbl(mydb, "followup_updates_diconames")

  if (!is.null(id) & !last_10_entry) {
    var <- rlang::enquo(id)
    entry <-
      tb %>%
      dplyr::filter(id_n == !!var) %>%
      dplyr::collect()
  }

  if(!is.null(last_months) & !last_10_entry) {

    one_month_earlier <-
      lubridate::month(Sys.Date() %m-% months(last_months))
    if(one_month_earlier<10) one_month_earlier <- paste0("0", one_month_earlier)
    one_month_earlier <-
      paste(lubridate::year(Sys.Date()), one_month_earlier, sep="-")

    this_month <-
      lubridate::month(Sys.Date())
    if(this_month<10) this_month <- paste0("0", this_month)
    this_month <-
      paste(lubridate::year(Sys.Date()), this_month, sep="-")

    query <-
      paste0("SELECT * FROM followup_updates_diconames WHERE date_modified ILIKE '%",one_month_earlier,"%' OR date_modified ILIKE '%", this_month, "%'")

    rs <- DBI::dbSendQuery(mydb, query)
    entry <- DBI::dbFetch(rs)
    DBI::dbClearResult(rs)
    entry <- dplyr::as_tibble(entry)
  }

  if(last_10_entry) {
    max_id <-
      tb %>%
      dplyr::arrange(dplyr::desc(id_fol_up_diconame)) %>%
      dplyr::select(id_fol_up_diconame) %>%
      dplyr::slice_head() %>%
      dplyr::collect()

    if(is.null(last)) {
      last_10 <- (dplyr::pull(max_id)-10)
    }else{
      last_10 <- (dplyr::pull(max_id)-last)
    }

    entry <-
      dplyr::tbl %>%
      dplyr::filter(id_fol_up_diconame > last_10) %>%
      dplyr::collect()

  }

  return(entry)
}



# add_entry_dico_name <- function(tax_gen = NULL,
#                                 tax_esp = NULL,
#                                 tax_fam = NULL,
#                                 full_name = NULL,
#                                 tax_rank1 = NULL,
#                                 tax_name1 = NULL,
#                                 synonym_of = NULL,
#                                 detvalue = NULL,
#                                 morphocat = NULL) {
#
#   if(!exists("mydb")) call.mydb()
#
#   if(is.null(full_name) & !is.null(tax_esp)) stop("Provide full name with authors")
#
#   if(is.null(tax_gen) & is.null(tax_esp) & is.null(tax_fam)) stop("Provide at least one genus/family/ new name to enter")
#
#   if(!is.null(tax_fam) & is.null(full_name) & is.null(tax_gen)) {
#     full_name <- tax_fam
#   }
#
#   if(is.null(detvalue)) {
#     if(is.null(tax_esp) & !is.null(tax_gen)) detvalue <- 6
#     if(is.null(tax_esp) & is.null(tax_gen)) detvalue <- 7
#
#     if(!is.null(tax_esp) & !is.null(tax_gen)) detvalue <- 1
#
#     cat(paste("\n No detvalue provided, by default, the following value is given:", detvalue))
#   }
#
#   if(is.null(morphocat)) {
#     if(is.null(tax_esp) & !is.null(tax_gen)) morphocat <- 3
#     if(is.null(tax_esp) & is.null(tax_gen)) morphocat <- 3
#
#     if(!is.null(tax_esp) & !is.null(tax_gen)) morphocat <- 1
#
#     cat(paste("\n No morphocat provided, by default, the following value is given:", morphocat))
#   }
#
#   check_taxo <- TRUE
#
#   if(is.null(tax_fam) & !is.null(tax_gen)) {
#     tax_fam <-
#       query_tax_all(genus_searched = tax_gen)  %>%
#       dplyr::distinct(tax_fam) %>%
#       dplyr::pull()
#     tax_fam <- tax_fam[which(!is.na(tax_fam))]
#     if(length(tax_fam)>1) cat(paste("\n No tax_fam provided, and two different family names for this genus", paste0(tax_fam, sep=", ")))
#     if(length(tax_fam)>1) check_taxo <- FALSE
#     if(length(tax_fam)==1) cat(paste("\n No tax_fam provided, based on genus, the following family is chosen:", tax_fam))
#   }
#
#   # if(is.null(tax_order) & !is.null(tax_fam)) {
#   #   tax_order <-
#   #     query_tax_all(tax_fam_searched = tax_fam) %>%
#   #   # , verbose = F, exact_match = T,
#   #   #              class = NULL, check_synonymy = FALSE)
#   #     dplyr::distinct(tax_order) %>%
#   #     dplyr::pull()
#   #   tax_order <- tax_order[which(!is.na(tax_order))]
#   #   if(length(tax_order)>1)
#   #     cat(paste("\n No tax_order provided, and two different order names for this family", paste0(tax_order, sep=", ")))
#   #   if(length(tax_order)>1) check_taxo <- FALSE
#   #   if(length(tax_order)==1)
#   #     cat(paste("\n No tax_order provided, based on family, the following order is chosen:", tax_order))
#   # }
#
#   # if(is.null(tax_famclass) & !is.null(tax_order)) {
#   #   tax_famclass <-
#   #     query_taxa(order = tax_order, verbose = F, exact_match = T,
#   #                class = NULL, check_synonymy = FALSE) %>%
#   #     dplyr::distinct(tax_famclass) %>%
#   #     dplyr::pull()
#   #   tax_famclass <- tax_famclass[which(!is.na(tax_famclass))]
#   #   if(length(tax_famclass)>1)
#   #     cat(paste("\n No tax_famclass provided, and two different class names for this order",
#   #               paste0(tax_famclass, sep=", ")))
#   #   if(length(tax_famclass)>1) check_taxo <- FALSE
#   #   if(length(tax_famclass)==1)
#   #     cat(paste("\n No tax_famclass provided, based on order, the following class is chosen:", tax_famclass))
#   # }
#
#   tax_fam_new <- TRUE
#   if(!is.null(tax_fam) & check_taxo) {
#     searched_tax_fam <-
#       dplyr::tbl(mydb, "diconame") %>%
#       dplyr::distinct(tax_fam) %>%
#       dplyr::filter(tax_fam == !!tax_fam) %>%
#       dplyr::collect()
#     if(nrow(searched_tax_fam)==0) {
#       tax_fam_new <-
#         utils::askYesNo(msg = "The provided family name is currently not present in the dictionnary. Are you sure it is correctly spelled?", default = FALSE)
#     }
#   }
#
#
#   if(!is.null(full_name) & !is.null(tax_gen)) {
#     if(!grepl(tax_gen, full_name)) stop("\n Genus and full_name are provided, but genus is not found within full name, there must be an ERROR")
#   }
#
#   if(!is.null(full_name) & !is.null(tax_esp)) {
#     if(!grepl(tax_esp, full_name)) stop("\n Species and full_name are provided, but tax_esp is not found within full_name, there must be an ERROR")
#   }
#
#   if(is.null(tax_gen) & !is.null(tax_esp)) {
#     stop("\n species epithet provided but no genus (provide tax_gen)")
#   }
#
#   if(!is.null(tax_gen)) {
#
#     family_check <-
#       query_tax_all(tax_fam_searched = tax_fam)
#     # exact_match = T, verbose = F, class = NULL, check_synonymy = FALSE)
#
#     genus_check <-
#       query_tax_all(genus_searched = tax_gen)
#     # exact_match = T,
#     # verbose = F,
#     # class = NULL,
#     # check_synonymy = FALSE)
#
#     if(!is.null(genus_check)) {
#       if(nrow(genus_check) > 0 & !any(family_check$tax_gen %in% tax_gen)) {
#         cat(paste("\n The provided genus is present in the taxonomic backbone, but with different family name:", genus_check$tax_fam[1]))
#         check_taxo <- FALSE
#       }
#     }
#   }
#
#   # tbl(mydb, "diconame") %>% collect() %>% slice(n())
#
#   if(check_taxo & tax_fam_new) {
#
#     if(!is.null(tax_gen) & !is.null(tax_esp)) paste_taxa <- paste(tax_gen, tax_esp)
#     if(!is.null(tax_gen) & is.null(tax_esp)) paste_taxa <- tax_gen
#     if(!is.null(tax_fam) & is.null(tax_gen)) paste_taxa <- tax_fam
#     if(is.null(full_name) & !is.null(tax_gen) & is.null(tax_esp)) full_name <- tax_gen
#
#     if(is.null(tax_esp)) tax_esp <- NA
#     if(is.null(tax_gen)) tax_gen <- NA
#     if(is.null(tax_fam)) tax_fam <- NA
#     if(is.null(tax_rank1)) tax_rank1 <- NA
#     if(is.null(tax_name1)) tax_name1 <- NA
#     # if(is.null(tax_rank2)) tax_rank2 <- NA
#     # if(is.null(tax_name2)) tax_name2 <- NA
#
#     # tax_rank <- NA
#     # if(!is.na(tax_esp) & is.na(tax_name1) & is.na(tax_name1))
#     #   tax_rank <- "ESP"
#     # if(is.na(tax_esp) & is.na(tax_name1) & is.na(tax_name2))
#     #   tax_rank <- NA
#     # if(!is.na(tax_esp) & !is.na(tax_rank1)) {
#     #   if(tax_rank1=="subsp.") tax_rank <- "SUBSP"
#     #   if(tax_rank1=="var.") tax_rank <- "VAR"
#     #   if(tax_rank1=="f.") tax_rank <- "F"
#     # }
#     #
#     # if(!is.na(tax_rank)) {
#     #   if(tax_rank=="VAR") tax_rankinf <- "VAR"
#     #   if(tax_rank=="SUBSP") tax_rankinf <- "SUBSP"
#     # }
#     # if(!is.na(tax_fam) & is.na(tax_gen) & is.na(tax_esp)) tax_rankinf <- "FAM"
#     # if(!is.na(tax_fam) & !is.na(tax_gen) & is.na(tax_esp)) tax_rankinf <- "GEN"
#     # if(!is.na(tax_fam) & !is.na(tax_gen) & !is.na(tax_esp) & is.na(tax_rank))
#     #   tax_rankinf <- "ESP"
#     # if(!is.na(tax_fam) & !is.na(tax_gen) & !is.na(tax_esp) & tax_rank=="ESP")
#     #   tax_rankinf <- "ESP"
#     #
#     # if(!is.na(tax_order) & is.na(tax_fam) & is.na(tax_gen) & is.na(tax_esp)) tax_rankesp <- "ORDER"
#     # if(!is.null(tax_famclass) & is.na(tax_order) & is.na(tax_fam) & is.na(tax_gen) & is.na(tax_esp)) tax_rankesp <- "CLASS"
#     # if(!is.na(tax_fam) & is.na(tax_gen) & is.na(tax_esp)) tax_rankesp <- "FAM"
#     # if(!is.na(tax_fam) & !is.na(tax_gen) & is.na(tax_esp)) tax_rankesp <- "GEN"
#     # if(!is.na(tax_fam) & !is.na(tax_gen) & !is.na(tax_esp))
#     #   tax_rankesp <- "ESP"
#
#     if (!is.na(tax_gen) &
#         !is.na(tax_esp))
#       paste_taxa <- paste(tax_gen, tax_esp)
#     if (!is.na(tax_gen) &
#         is.na(tax_esp))
#       paste_taxa <- tax_gen
#     if (!is.na(tax_fam) &
#         is.na(tax_gen))
#       paste_taxa <- tax_fam
#
#     new_rec <-
#       dplyr::tibble(
#         tax_fam = tax_fam,
#         tax_gen = tax_gen,
#         tax_esp = tax_esp,
#         tax_rank1 = tax_rank1,
#         tax_name1 = tax_name1,
#         detvalue = detvalue,
#         morphocat = morphocat,
#         full_name_no_auth = paste_taxa,
#         full_name_used = paste_taxa,
#         full_name_used2 = paste_taxa,
#         # tax_rank02 = tax_rank2,
#         # tax_nam02 = tax_name2,
#         full_name = full_name,
#         id_good = NA,
#         id = NA
#       ) %>%
#       dplyr::mutate(id_good = as.numeric(id_good))
#
#     seek_dup <-
#       tbl(mydb, "diconame")
#
#
#     # if(!is.na(new_rec$tax_order)) {
#     #   seek_dup <- seek_dup %>%
#     #     filter(tax_order == !!new_rec$tax_order)
#     # }else{
#     #   seek_dup <- seek_dup %>%
#     #     filter(is.na(tax_order))
#     # }
#
#     if(!is.na(new_rec$tax_fam)) {
#       seek_dup <- seek_dup %>%
#         filter(tax_fam == !!new_rec$tax_fam)
#     }else{
#       seek_dup <- seek_dup %>%
#         filter(is.na(tax_fam))
#     }
#
#     if(!is.na(new_rec$tax_gen)) {
#       seek_dup <- seek_dup %>%
#         filter(tax_gen == !!new_rec$tax_gen)
#     }else{
#       seek_dup <- seek_dup %>%
#         filter(is.na(tax_gen))
#     }
#
#     if(!is.na(new_rec$tax_esp)) {
#       seek_dup <- seek_dup %>%
#         filter(tax_esp == !!new_rec$tax_esp)
#     }else{
#       seek_dup <- seek_dup %>%
#         filter(is.na(tax_esp))
#     }
#
#     if(!is.na(new_rec$tax_rank1)) {
#       seek_dup <- seek_dup %>%
#         filter(tax_rank1 == !!new_rec$tax_rank1)
#     }else{
#       seek_dup <- seek_dup %>%
#         filter(is.na(tax_rank1))
#     }
#
#     if(!is.na(new_rec$tax_name1)) {
#       seek_dup <- seek_dup %>%
#         filter(tax_name1 == !!new_rec$tax_name1)
#     }else{
#       seek_dup <- seek_dup %>%
#         filter(is.na(tax_name1))
#     }
#
#     # if(!is.na(new_rec$tax_nam02)) {
#     #   seek_dup <- seek_dup %>%
#     #     filter(tax_nam02 == !!new_rec$tax_nam02)
#     # }else{
#     #   seek_dup <- seek_dup %>%
#     #     filter(is.na(tax_nam02))
#     # }
#
#     seek_dup <-
#       seek_dup %>%
#       collect()
#
#     launch_adding_data <- TRUE
#
#     if (nrow(seek_dup) > 0) {
#
#       cli::cli_alert_info("New entry fit to one entry already in table_taxa")
#       print(as.data.frame(seek_dup))
#       launch_adding_data <- FALSE
#
#     }
#
#     if(launch_adding_data) {
#
#       new_rec <-
#         new_rec %>%
#         tibble::add_column(data_modif_d = lubridate::day(Sys.Date()),
#                            data_modif_m = lubridate::month(Sys.Date()),
#                            data_modif_y = lubridate::year(Sys.Date()))
#
#       cli::cli_alert_info("Adding new entry")
#       DBI::dbWriteTable(mydb, "diconame", new_rec, append = TRUE, row.names = FALSE)
#
#       new_entry <-
#         dplyr::tbl(mydb, "diconame") %>%
#         dplyr::filter(full_name == !!new_rec$full_name,
#                       data_modif_d == !!lubridate::day(Sys.Date()),
#                       data_modif_m == !!lubridate::month(Sys.Date()),
#                       data_modif_y == !!lubridate::year(Sys.Date())) %>%
#         dplyr::collect()
#
#       if(!is.null(synonym_of)) {
#
#         if (!is.list(synonym_of)) {
#           stop(
#             "synonym_of should be a list with the first element \nbeing the genus and the second element the species epiteth of the taxa of the correct name \nOR the idtax_n"
#           )
#         }
#
#         if (!any(names(synonym_of) == "genus") &
#             !any(names(synonym_of) == "species") &
#             !any(names(synonym_of) == "id"))
#           stop("synonym_of should have at least of the thre following element : genus, species or idtax_n")
#
#         if (!any(names(synonym_of) == "genus"))
#           synonym_of$genus <- NULL
#         if (!any(names(synonym_of) == "species"))
#           synonym_of$species <- NULL
#         if (!any(names(synonym_of) == "id"))
#           synonym_of$id <- NULL
#
#         syn_searched <-
#           query_tax_all(genus_searched = synonym_of$genus,
#                         tax_esp_searched = synonym_of$species,
#                         id_search = synonym_of$id)
#
#         print(syn_searched)
#         if (nrow(syn_searched) > 1)
#           stop("More than 1 taxa as synonym. Select only one.")
#         if (nrow(syn_searched) == 0)
#           stop("No taxa found in the dictionnary. Select one.")
#
#
#         update_dico_name(new_id_diconame_good = syn_searched$id_good_n,
#                          id_search = new_entry$id_n,
#                          ask_before_update = FALSE,
#                          add_backup = FALSE)
#         # update_dico_name(
#         #   new_id_diconame_good = syn_searched$idtax_good_n,
#         #   id_search = new_entry$idtax_n,
#         #   ask_before_update = FALSE,
#         #   add_backup = FALSE,
#         #   show_results = FALSE
#         # )
#
#       }else{
#
#         rs <-
#           DBI::dbSendQuery(mydb, statement="UPDATE diconame SET id_good_n=$2 WHERE id_n = $1",
#                            params=list(new_entry$id_n, new_entry$id_n)) # $10
#
#         DBI::dbClearResult(rs)
#
#       }
#
#       # print(dplyr::tbl(mydb, "table_taxa") %>%
#       #         dplyr::collect() %>%
#       #         dplyr::filter(idtax_n == max(idtax_n)))
#
#       res_selected <- dplyr::tbl(mydb, "diconame") %>%
#         dplyr::filter(id_n == !!new_entry$id_n) %>%
#         collect()
#
#       res_selected <- as_tibble(cbind(
#         columns = names(res_selected),
#         record = t(res_selected)
#       )) %>%
#         kableExtra::kable(format = "html", escape = F) %>%
#         kableExtra::kable_styling("striped", full_width = F)
#
#       print(res_selected)
#
#     }
#
#   }else{
#
#     cat("\n NO ADDED ENTRY")
#
#   }
# }





#' Delete an entry in taxonomic table
#'
#' Delete an entry in taxonomic table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
#' @export
.delete_taxa <- function(id) {

  if(!exists("mydb_taxa")) call.mydb()

  # DBI::dbExecute(mydb,
  #                "DELETE FROM table_taxa WHERE idtax_n=$1", params=list(id)
  # )

  query <- "DELETE FROM table_taxa WHERE MMM"
  query <-
    gsub(
      pattern = "MMM",
      replacement = paste0("idtax_n IN ('",
                           paste(unique(id), collapse = "', '"), "')"),
      x = query
    )

  rs <- DBI::dbSendQuery(mydb_taxa, query)
  DBI::dbClearResult(rs)
}

#' Delete an entry in subplotype_list table
#'
#' Delete an entry in subplotype_list table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
.delete_subplottype <- function(id) {

  if(!exists("mydb")) call.mydb()

  askYesNo(msg = "You are about to delete entries in the table that contain subplot features types. Do you confirm ?")

  query <- "DELETE FROM subplotype_list WHERE MMM"
  query <-
    gsub(
      pattern = "MMM",
      replacement = paste0("id_subplotype IN ('",
                           paste(unique(id), collapse = "', '"), "')"),
      x = query
    )

  rs <- DBI::dbSendQuery(mydb, query)
  DBI::dbClearResult(rs)

}

#' Delete an entry in subplotype_list table
#'
#' Delete an entry in subplotype_list table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
.delete_subplotfeature <- function(id) {

  if(!exists("mydb")) call.mydb()

  query <- "DELETE FROM data_liste_sub_plots WHERE MMM"
  query <-
    gsub(
      pattern = "MMM",
      replacement = paste0("id_sub_plots IN ('",
                           paste(unique(id), collapse = "', '"), "')"),
      x = query
    )

  rs <- DBI::dbSendQuery(mydb, query)
  DBI::dbClearResult(rs)

  # DBI::dbExecute(mydb,
  #                "DELETE FROM data_liste_sub_plots WHERE id_sub_plots=$1", params=list(id)
  # )
}

#' Delete an entry in trait measurement table
#'
#' Delete an entry in trait measurement table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
#' @export
.delete_entry_trait_measure <- function(id) {

  if(!exists("mydb")) call.mydb()

  # DBI::dbExecute(mydb,
  #                "DELETE FROM data_traits_measures WHERE id_trait_measures=$1", params=list(id)
  # )

  feats <- query_traits_measures_features(id_trait_measures = id)

  if (!is.null(dim(feats$all_feat_pivot))) {

    feats$all_feat_pivot <-
      test <-
      feats$all_feat_pivot %>%
      mutate(id_ind_meas_feat_n = str_extract_all(id_ind_meas_feat, "[[:digit:]]+"))


    print(feats)

    rm_feats <- askYesNo(msg = "Remove associated features")

    if (rm_feats)
      .delete_entry_trait_measure_features(id = as.numeric(unlist(feats$all_feat_pivot$id_ind_meas_feat_n)))

  }

  query <- "DELETE FROM data_traits_measures WHERE MMM"
  query <-
    gsub(
      pattern = "MMM",
      replacement = paste0("id_trait_measures IN ('",
                           paste(unique(id), collapse = "', '"), "')"),
      x = query
    )

  rs <- DBI::dbSendQuery(mydb, query)
  DBI::dbClearResult(rs)
}


#' Delete an entry in trait measurement features table
#'
#' Delete an entry in trait measurement features table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
#' @export
.delete_entry_trait_measure_features <- function(id) {

  if(!exists("mydb")) call.mydb()

  query <- "DELETE FROM data_ind_measures_feat WHERE MMM"
  query <-
    gsub(
      pattern = "MMM",
      replacement = paste0("id_ind_meas_feat IN ('",
                           paste(unique(id), collapse = "', '"), "')"),
      x = query
    )

  rs <- DBI::dbSendQuery(mydb, query)
  DBI::dbClearResult(rs)
}

# .delete_entry_diconame <- function(id) {
#
#   if(!exists("mydb")) call.mydb()
#
#   DBI::dbExecute(mydb,
#                  "DELETE FROM diconame WHERE id_n=$1", params=list(id)
#   )
# }


#' Delete an entry in trait measurement table
#'
#' Delete an entry in trait measurement table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id_ind integer
#' @param id_specimen integer
#'
#' @return No values
#' @export
.delete_link_individual_specimen <- function(id_ind = NULL,
                                             id_specimen = NULL,
                                             id_link = NULL) {

  if(!exists("mydb")) call.mydb()

  if(!is.null(id_ind)) {
    selected_link <-
      dplyr::tbl(mydb, "data_link_specimens") %>%
      dplyr::filter(id_n %in% !!id_ind) %>%
      dplyr::collect() %>%
      as.data.frame()

    print(selected_link)
    confirm <-
      utils::askYesNo(msg = "Confirm removing these links?")

    if(confirm)
      for (i in 1:nrow(selected_link))
        DBI::dbExecute(mydb,
                     "DELETE FROM data_link_specimens WHERE id_n=$1",
                     params=list(selected_link$id_n[i]))
  }

  if(!is.null(id_specimen)) {
    selected_link <-
      dplyr::tbl(mydb, "data_link_specimens") %>%
      dplyr::filter(id_specimen %in% !!id_specimen) %>%
      dplyr::collect() %>%
      as.data.frame()

    print(selected_link)

    confirm <-
      utils::askYesNo(msg = "Confirm removing these links?")

    if(confirm) {
      query <- "DELETE FROM data_link_specimens WHERE MMM"
      query <-
        gsub(
          pattern = "MMM",
          replacement = paste0("id_specimen IN ('",
                               paste(unique(selected_link$id_specimen), collapse = "', '"), "')"),
          x = query
        )

      rs <- DBI::dbSendQuery(mydb, query)
      DBI::dbClearResult(rs)
    }


  }

  if (!is.null(id_link)) {

    query <- "DELETE FROM data_link_specimens WHERE MMM"
    query <-
      gsub(
        pattern = "MMM",
        replacement = paste0("id_link_specimens IN ('",
                             paste(unique(id_link), collapse = "', '"), "')"),
        x = query
      )

    rs <- DBI::dbSendQuery(mydb, query)
    DBI::dbClearResult(rs)

  }


}

#' Delete an entry in individuals table
#'
#' Delete an entry in individuals table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
#' @export
.delete_individuals <- function(id) {

  if(!exists("mydb")) call.mydb()

  # DBI::dbExecute(mydb,
  #                "DELETE FROM data_individuals WHERE id_n=$1", params=list(id)
  # )

  query <- "DELETE FROM data_individuals WHERE MMM"
  query <-
    gsub(
      pattern = "MMM",
      replacement = paste0("id_n IN ('",
                           paste(unique(id), collapse = "', '"), "')"),
      x = query
    )

  rs <- DBI::dbSendQuery(mydb, query)
  DBI::dbClearResult(rs)

}


#' Delete an entry in specimen table
#'
#' Delete an entry in specimen table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
#' @export
.delete_specimens <- function(id) {

  if(!exists("mydb")) call.mydb()

  query <- "DELETE FROM specimens WHERE MMM"
  query <-
    gsub(
      pattern = "MMM",
      replacement = paste0("id_specimen IN ('",
                           paste(unique(id), collapse = "', '"), "')"),
      x = query
    )

  rs <- DBI::dbSendQuery(mydb, query)
  DBI::dbClearResult(rs)

}


#' Delete an entry in plot meta-data
#'
#' Delete an entry in plot meta-data table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
#' @export
.delete_plot <- function(id) {

  if(!exists("mydb")) call.mydb()

  DBI::dbExecute(mydb,
                 "DELETE FROM data_liste_plots WHERE id_liste_plots=$1", params=list(id)
  )
}


#' Delete an entry in colnam table
#'
#' Delete an entry in colnam table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
.delete_colnam <- function(id) {

  if(!exists("mydb")) call.mydb()



  DBI::dbExecute(mydb,
                 "DELETE FROM table_colnam WHERE id_table_colnam=$1", params=list(id)
  )
}

#' Exploring specimens data
#'
#' Exploring specimens data and if necessary export labels
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param collector string collector name
#' @param id_colnam integer id of collector
#' @param number integer specimen number
#' @param number_min integer minimum specimen number
#' @param number_max integer maximum specimen number
#' @param genus_searched string specimens of genus searched
#' @param tax_esp_searched string specimens of species searched
#' @param tax_fam_searched string specimens of family searched
#' @param id_search integer id searched
#' @param subset_columns logical if only a subset of columns should be provided
#' @param show_previous_modif logical if you want to see previous modification of the entry - useful to see previous identification for example
#' @param generate_labels logical if labels should be produced
#' @param project_title string if labels are produced title of the label
#' @param file_labels string if labels are produced name of the rtf file
#' @param extract_linked_individuals logical extract individuals linked to selected specimens
#'
#' @return A tibble
#' @export
query_specimens <- function(collector = NULL,
                            id_colnam = NULL,
                            number = NULL,
                            number_min = NULL,
                            number_max = NULL,
                            genus_searched = NULL,
                            tax_esp_searched = NULL,
                            tax_fam_searched = NULL,
                            id_search = NULL,
                            subset_columns = TRUE,
                            show_previous_modif = TRUE,
                            generate_labels = FALSE,
                            project_title = "Reference specimens collected in trees inventory",
                            file_labels = "labels",
                            extract_linked_individuals = FALSE) {

  if(!exists("mydb")) call.mydb()

  diconames_id <-
    try_open_postgres_table(table = "table_idtax", con = mydb) %>%
    dplyr::select(idtax_n, idtax_good_n) %>%
    dplyr::mutate(idtax_f = ifelse(is.na(idtax_good_n), idtax_n, idtax_good_n))

  query_speci <-
    try_open_postgres_table(table = "specimens", con = mydb) %>%
    dplyr::left_join(
      diconames_id %>%
        dplyr::select(-idtax_good_n),
      by = c("idtax_n" = "idtax_n")
    ) %>%
    # left_join(add_taxa_table_taxa(),
    #           by = c("idtax_f" = "idtax_n")) %>%
    dplyr::left_join(dplyr::tbl(mydb, "table_colnam"),
                     by = c("id_colnam" = "id_table_colnam"))


  # %>%
  #   dplyr::select(-id_specimen_old, -id_diconame, -photo_tranche, -id_colnam, -id_good, -id, -id_good_n)

  if (subset_columns & !generate_labels)
    query_speci <-
    query_speci %>%
    dplyr::select(
      colnam,
      colnbr,
      suffix,
      ddlat,
      ddlon,
      country,
      locality,
      detby,
      detd,
      detm,
      dety,
      add_col,
      cold,
      colm,
      coly,
      detvalue,
      description,
      id_specimen,
      idtax_f,
      id_tropicos,
      id_colnam
    )

  ## filter by collector or id_colnam (id of people table)
  if ((!is.null(collector) |
       !is.null(id_colnam)) & is.null(id_search)) {

    if (is.null(id_colnam)) {

      id_colnam <- .link_colnam(data_stand = tibble(colnam = collector),column_searched = "colnam")$id_colnam

    }

    query_speci <-
      query_speci %>%
      dplyr::filter(id_colnam == !!id_colnam)

  }

  if(!is.null(number) & is.null(id_search)) {

    var <- rlang::enquo(number)

    query_speci <-
      query_speci %>%
      dplyr::filter(colnbr %in% var)


  }

  if(!is.null(id_colnam) & is.null(id_search)) {

    var <- rlang::enquo(id_colnam)

    query_speci <-
      query_speci %>%
      dplyr::filter(id_colnam %in% var)
  }



  if(!is.null(number_min) & is.null(id_search)) {

    var <- rlang::enquo(number_min)

    query_speci <-
      query_speci %>%
      dplyr::filter(colnbr >= var)
  }

  if(!is.null(number_max) & is.null(id_search)) {

    var <- rlang::enquo(number_max)

    query_speci <-
      query_speci %>%
      dplyr::filter(colnbr<=var)
  }

  if(!is.null(genus_searched) & is.null(id_search)) {

    var <- rlang::enquo(genus_searched)

    query_speci <-
      query_speci %>%
      dplyr::filter(grepl(!!var, tax_gen))
  }

  if(!is.null(tax_fam_searched) & is.null(id_search)) {

    var <- rlang::enquo(tax_fam_searched)

    query_speci <-
      query_speci %>%
      dplyr::filter(grepl(!!var, tax_fam))
  }

  if(!is.null(tax_esp_searched) & is.null(id_search)) {

    var <- rlang::enquo(tax_esp_searched)

    query_speci <-
      query_speci %>%
      dplyr::filter(grepl(!!var, tax_esp))
  }

  if(!is.null(id_search)) {

    var <- rlang::enquo(id_search)

    query_speci <-
      query_speci %>%
      dplyr::filter(id_specimen %in% var)
  }
  query <-
    query_speci %>%
    dplyr::collect()


  query_tax <- add_taxa_table_taxa(ids = unique(query$idtax_f))
  query_tax <- query_tax %>% collect()

  query <- left_join(
    query,
    query_tax %>%
      dplyr::select(-data_modif_d,-data_modif_m,-data_modif_y),
    by = c("idtax_f" = "idtax_n")
  )

  # print(query)

  if(extract_linked_individuals) {

    linked_ind <-
      dplyr::tbl(mydb, "data_link_specimens") %>%
      dplyr::filter(id_specimen == !!query$id_specimen) %>%
      dplyr::select(id_n, id_specimen) %>%
      dplyr::collect()

    linked_ind <-
      query_plots(
        id_individual = linked_ind$id_n,
        extract_individuals = TRUE,
        remove_ids = FALSE
      )

    cli::cli_alert_info(
      "This specimen is linked to {nrow(linked_ind)} individuals from {length(unique(linked_ind$plot_name))} plot(s)"
    )

  }

  if (nrow(query) == 1 & show_previous_modif) {

    ## get previous modifications of queried entries
    # modif_backups <-
    #   dplyr::tbl(mydb, "followup_updates_specimens") %>%
    #   dplyr::filter(id_specimen == !!query$id_specimen) %>%
    #   dplyr::filter(grepl("idtax_n", modif_type)) %>%
    #   dplyr::left_join(
    #     dplyr::tbl(mydb, "table_taxa") %>%
    #       dplyr::select(-data_modif_d,-data_modif_m,-data_modif_y),
    #     by = c("id_diconame_n" = "idtax_n")
    #   ) %>%
    #   dplyr::left_join(dplyr::tbl(mydb, "table_colnam"),
    #                    by = c("id_colnam" = "id_table_colnam")) %>%
    #   dplyr::collect()

    # if(nrow(modif_backups) > 0) {
    #
    #         # cat("\n Previous modification in identification")
    #   #
    #   # print(modif_backups)
    #
    #   modif_backups <-
    #     modif_backups %>%
    #     dplyr::select(all_of(names(query)))
    #
    # }else{
    #
    #   cli::cli_alert_info("No identification change in backups")
    #
    # }
  }


  nrow_query <-
    nrow(query)

  if(nrow(query) < 50)
  {
    res_html <-
      tibble(columns = names(query), data.frame(t(query),
                                                fix.empty.names = T)) %>%
      mutate_all(~ tidyr::replace_na(., ""))

    # if(nrow(query) == 1 & show_previous_modif) {
    #   if(nrow(modif_backups) > 0) {
    #     for (i in ((nrow_query  + 2):(nrow(query)  + 1))) {
    #
    #       col_ <- colnames(res_html)[i]
    #       var_new <-
    #         rlang::parse_expr(rlang::quo_name(rlang::enquo(col_)))
    #
    #       res_html <-
    #         res_html %>%
    #         mutate(!!var_new :=
    #                  kableExtra::cell_spec(!!var_new,
    #                                        "html",
    #                                        background = "grey",
    #                                        color = "white", italic = T))
    #     }
    #   }
    # }

    res_html %>%
      kableExtra::kable(format = "html", escape = F) %>%
      kableExtra::kable_styling("striped", full_width = F) %>%
      print()
  }

  if(!extract_linked_individuals) return(query)

  if(extract_linked_individuals) return(list(linked_ind = linked_ind,
                                             query = query))

}








#' Add an observation in trait measurement table
#'
#' Add a trait measure in trait measurement table
#'
#' @return list of tibbles that should be/have been added
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data tibble
#' @param col_names_select string vector
#' @param col_names_corresp string vector
#' @param collector_field string column name which contain the collector name
#' @param plot_name_field string column name which contain the plot_name for linking
#' @param individual_plot_field string column name which contain the individual tag for linking
#' @param id_plot_name string column name which contain the ID of plot_name
#' @param id_tag_plot string column name which contain the ID of individuals table
#' @param id_specimen string column name which contain the ID of specimen
#' @param traits_field string vector listing trait columns names in new_data
#' @param features_field string vector listing features (column names) to link to measurementsin new_data
#' @param add_data logical whether or not data should be added - by default FALSE
#' @param allow_multiple_value if multiple values linked to one individual can be uploaded at once
#'
#' @export
add_traits_measures <- function(new_data,
                                col_names_select = NULL,
                                col_names_corresp = NULL,
                                collector_field = NULL,
                                plot_name_field = NULL,
                                individual_plot_field = NULL,
                                id_plot_name = NULL,
                                id_tag_plot = NULL,
                                id_specimen = NULL,
                                traits_field,
                                features_field = NULL,
                                allow_multiple_value = FALSE,
                                add_data = FALSE) {

  for (i in 1:length(traits_field))
    if (!any(colnames(new_data) == traits_field[i]))
      stop(paste("traits_field provide not found in new_data", traits_field[i]))

  if (!is.null(features_field)) for (i in 1:length(features_field))
    if (!any(colnames(new_data) == features_field[i]))
      stop(paste("features_field provide not found in new_data", features_field[i]))



  if (!is.null(col_names_select) & !is.null(col_names_corresp)) {
    new_data_renamed <-
      .rename_data(dataset = new_data,
                   col_old = col_names_select,
                   col_new = col_names_corresp)
  } else{

    new_data_renamed <- new_data

  }

  ## removing entries with NA values for traits
  new_data_renamed <-
    new_data_renamed %>%
    dplyr::filter_at(dplyr::vars(!!traits_field), dplyr::any_vars(!is.na(.)))

  if (nrow(new_data_renamed) == 0)
    stop("no values for selected trait(s)")

  if (!any(col_names_corresp == "day")) {
    cli::cli_alert_info("no information collection day provided")
    new_data_renamed <-
      new_data_renamed %>%
      mutate(day = NA) %>%
      mutate(day = as.numeric(day))

    if (is.null(plot_name_field) & is.null(individual_plot_field) &
        is.null(id_specimen) & is.null(id_plot_name) &
        is.null(id_tag_plot))
      stop("no links provided (either plot, specimen or tag), thus date is mandatory")
  }

  if (!any(col_names_corresp == "year")) {
    cli::cli_alert_info("no information collection year provided")
    new_data_renamed <-
      new_data_renamed %>%
      mutate(year = NA) %>%
      mutate(year = as.numeric(year))

    if (is.null(plot_name_field) & is.null(individual_plot_field) &
        is.null(id_specimen) & is.null(id_plot_name) &
        is.null(id_tag_plot))
      stop("no links provided (either plot, specimen or tag), thus date is mandatory")
  }

  if (!any(col_names_corresp == "month")) {
    cli::cli_alert_info("no information collection month provided")
    new_data_renamed <-
      new_data_renamed %>%
      mutate(month = NA) %>%
      mutate(month = as.numeric(month))

    if (is.null(plot_name_field) & is.null(individual_plot_field) &
        is.null(id_specimen) & is.null(id_plot_name) &
        is.null(id_tag_plot))
      stop("no links provided (either plot, specimen or tag), thus date is mandatory")
  }

  if(!any(col_names_corresp == "country")) {
    cli::cli_alert_info("no country provided")
    new_data_renamed <-
      new_data_renamed %>%
      mutate(country = NA) %>%
      mutate(country = as.character(country))

    if(is.null(plot_name_field) & is.null(individual_plot_field) &
       is.null(id_specimen) & is.null(id_plot_name) &
       is.null(id_tag_plot)) stop("no links provided (either plot, specimen or tag), thus country is mandatory")

  }

  if (!any(col_names_corresp == "decimallatitude")) {
    cli::cli_alert_info("no decimallatitude provided")
    new_data_renamed <-
      new_data_renamed %>%
      dplyr::mutate(decimallatitude = NA) %>%
      dplyr::mutate(decimallatitude = as.double(decimallatitude))

    if (is.null(plot_name_field) & is.null(individual_plot_field) &
        is.null(id_specimen) & is.null(id_plot_name) &
        is.null(id_tag_plot))
      stop("no links provided (either plot, specimen or tag), thus decimallatitude is mandatory")
  }

  if (!any(col_names_corresp == "decimallongitude")) {
    cli::cli_alert_info("no decimallongitude provided")
    new_data_renamed <-
      new_data_renamed %>%
      dplyr::mutate(decimallongitude = NA) %>%
      dplyr::mutate(decimallongitude = as.double(decimallongitude))

    if (is.null(plot_name_field) & is.null(individual_plot_field) &
        is.null(id_specimen) & is.null(id_plot_name) &
        is.null(id_tag_plot))
      stop("no links provided (either plot, specimen or tag), thus decimallongitude is mandatory")
  }

  new_data_renamed <-
    new_data_renamed %>%
    mutate(id_new_data = 1:nrow(.))

  ### Linking collectors names
  if(!is.null(collector_field)) {
    if(!any(colnames(new_data_renamed) == collector_field))
      stop("no collector_field found in new dataset")
    # new_data_renamed <-
    #   .link_colnam(data_stand = new_data_renamed, collector_field = collector_field)

    new_data_renamed <-
      .link_colnam(
      data_stand = new_data_renamed,
      column_searched = collector_field,
      column_name = "colnam",
      id_field = "id_colnam",
      id_table_name = "id_table_colnam",
      db_connection = mydb,
      table_name = "table_colnam"
    )

  } else{
    new_data_renamed <-
      new_data_renamed %>%
      mutate(id_colnam = NA) %>%
      mutate(id_colnam = as.numeric(id_colnam))

    if (is.null(plot_name_field) & is.null(individual_plot_field) &
        is.null(id_specimen) & is.null(id_plot_name) &
        is.null(id_tag_plot))
      stop("no links provided (either plot, specimen or tag), thus collector_field is mandatory")
  }

  ### Linking plot names
  if(!is.null(plot_name_field)) {
    if (!any(colnames(new_data_renamed) == plot_name_field))
      stop("plot_name_field not found in colnames")

    # new_data_renamed <-
    #   .link_plot_name(data_stand = new_data_renamed, plot_name_field = plot_name_field)

    new_data_renamed <-
      .link_table(data_stand = new_data_renamed,
                column_searched = plot_name_field,
                column_name = "plot_name",
                id_field = "id_liste_plots",
                id_table_name = "id_liste_plots",
                db_connection = mydb,
                table_name = "data_liste_plots")

  }

  if (!is.null(id_plot_name)) {
    id_plot_name <- "id_table_liste_plots_n"

    new_data_renamed <-
      new_data_renamed %>%
      dplyr::rename_at(dplyr::vars(id_plot_name), ~ id_plot_name)

    if (any(colnames(new_data_renamed) == "plot_name"))
      new_data_renamed <-
      new_data_renamed %>%
      dplyr::select(-plot_name)

    link_plot <-
      new_data_renamed %>%
      dplyr::left_join(
        dplyr::tbl(mydb, "data_liste_plots") %>%
          dplyr::select(plot_name, id_liste_plots) %>% dplyr::collect(),
        by = c("id_table_liste_plots_n" = "id_liste_plots")
      )

    if (dplyr::filter(link_plot, is.na(plot_name)) %>%
        nrow() > 0) {
      print(dplyr::filter(link_plot, is.na(plot_name)))
      cli::cli_alert_danger("provided id plot not found in plot metadata")
    }

    new_data_renamed <-
      new_data_renamed %>%
      dplyr::rename(id_liste_plots = id_table_liste_plots_n)
  }

  ### linking individuals by id
  if(!is.null(id_tag_plot) & is.null(individual_plot_field)) {

    id_tag <-
      "id_n"

    new_data_renamed <-
      new_data_renamed %>%
      dplyr::rename_at(dplyr::vars(all_of(id_tag_plot)), ~ id_tag)


    link_individuals <-
      new_data_renamed %>%
      dplyr::left_join(
        dplyr::tbl(mydb, "data_individuals") %>%
          dplyr::select(idtax_n, id_n, sous_plot_name) %>%
          dplyr::filter(id_n %in% !!unique(new_data_renamed$id_n)) %>%
          dplyr::collect() %>%
          dplyr::mutate(rrr = 1),
        by = c("id_n" = "id_n")
      )

    if (dplyr::filter(link_individuals, is.na(rrr)) %>%
        nrow() > 0) {
      print(dplyr::filter(link_individuals, is.na(sous_plot_name)))
      stop("provided id individuals not found in data_individuals")
    }

    new_data_renamed <-
      new_data_renamed %>%
      dplyr::rename(id_data_individuals = id_n)
  } else{

    new_data_renamed <-
      new_data_renamed %>%
      tibble::add_column(id_data_individuals = NA) %>%
      dplyr::mutate(id_data_individuals = as.integer(id_data_individuals))

  }


  if (is.null(id_plot_name) & is.null(plot_name_field)) {

    if (!is.null(id_tag_plot) & is.null(individual_plot_field)) {

      queried_individuals <-
        query_plots(id_individual = new_data_renamed$id_data_individuals, remove_ids = F)

      new_data_renamed <-
        new_data_renamed %>%
        left_join(queried_individuals %>%
                    dplyr::select(id_n, id_table_liste_plots_n),
                  by = c("id_data_individuals" = "id_n")) %>%
        rename(id_liste_plots = id_table_liste_plots_n)

    } else {

      new_data_renamed <-
        new_data_renamed %>%
        dplyr::mutate(id_liste_plots = NA) %>%
        dplyr::mutate(id_liste_plots = as.integer(id_liste_plots))

    }
  }

  ### check for different census for concerned plots
  multiple_census <- FALSE
  census_check <- utils::askYesNo(msg = "Link trait measures to census (only for permanent plots) ?")

  if(census_check) {
    unique_ids_plots <- unique(new_data_renamed$id_liste_plots)
    censuses <-
      try_open_postgres_table(table = "data_liste_sub_plots", con = mydb) %>%
      dplyr::filter(id_table_liste_plots %in% unique_ids_plots, id_type_sub_plot==27) %>%
      dplyr::left_join(dplyr::tbl(mydb, "data_liste_plots") %>%
                         dplyr::select(plot_name, id_liste_plots), by=c("id_table_liste_plots"="id_liste_plots")) %>%
      dplyr::left_join(dplyr::tbl(mydb, "subplotype_list") %>%
                         dplyr::select(type, id_subplotype), by=c("id_type_sub_plot"="id_subplotype")) %>%
      dplyr::left_join(dplyr::tbl(mydb, "table_colnam") %>%
                         dplyr::select(id_table_colnam, colnam), by=c("id_colnam"="id_table_colnam")) %>%
      dplyr::collect()

    if(nrow(censuses) > 0) { # & length(unique(censuses$typevalue))>1

      cli::cli_alert_info("Multiple census for concerned plots")
      censuses %>%
        dplyr::select(plot_name, id_table_liste_plots, year, month, day, typevalue, type, colnam, additional_people) %>%
        as.data.frame() %>%
        print()
      census_chosen <- readline(prompt="Choose census ")

      chosen_ids_subplots <-
        censuses %>%
        dplyr::filter(typevalue == as.numeric(census_chosen)) %>%
        dplyr::select(id_table_liste_plots, id_sub_plots)

      if(nrow(chosen_ids_subplots) == 0) stop("chosen census not available")

      missing_census <-
        new_data_renamed %>%
        dplyr::distinct(id_liste_plots) %>%
        dplyr::filter(!id_liste_plots %in% chosen_ids_subplots$id_table_liste_plots) %>%
        dplyr::filter(!is.na(id_liste_plots))

      if(nrow(missing_census)) {
        print(missing_census %>%
                dplyr::left_join(dplyr::tbl(mydb, "data_liste_plots") %>%
                                   dplyr::select(id_liste_plots, plot_name) %>%
                                   dplyr::collect(),
                                 by=c("id_liste_plots"="id_liste_plots")) %>%
                as.data.frame())
        warning(paste("Missing census for", nrow(missing_census),"plots, census chosen :", census_chosen))
      }

      new_data_renamed <-
        new_data_renamed %>%
        dplyr::left_join(chosen_ids_subplots,
                         by = c("id_liste_plots" = "id_table_liste_plots"))
      # %>%
      #   filter(id_liste_plots==824) %>%
      #   select(id_sub_plots)

      if (as.numeric(census_chosen) > 1)
        multiple_census <- TRUE

    } else {

      new_data_renamed <-
        new_data_renamed %>%
        tibble::add_column(id_sub_plots = NA) %>%
        dplyr::mutate(id_sub_plots = as.integer(id_sub_plots))
      multiple_census <- FALSE
    }
  }else{

    new_data_renamed <-
      new_data_renamed %>%
      tibble::add_column(id_sub_plots = NA) %>%
      dplyr::mutate(id_sub_plots = as.integer(id_sub_plots))

  }

  ### Linking specimens
  if(!is.null(id_specimen)) {

    id_tag <-
      "id_specimen"

    new_data_renamed <-
      new_data_renamed %>%
      dplyr::rename_at(dplyr::vars(id_specimen), ~ id_tag)

    link_specimen <-
      new_data_renamed %>%
      dplyr::filter(!is.na(id_specimen)) %>%
      dplyr::left_join(
        dplyr::tbl(mydb, "specimens") %>%
          dplyr::select(id_diconame_n, id_specimen) %>% dplyr::collect(),
        by = c("id_specimen" = "id_specimen")
      )

    if(dplyr::filter(link_specimen, is.na(id_diconame_n)) %>%
       nrow()>0) {
      print(dplyr::filter(link_specimen, is.na(id_diconame_n)))
      stop("provided id specimens not found in specimens table")
    }
  }else{

    if (!any(colnames(new_data_renamed) == "id_specimen")) {

      new_data_renamed <-
        new_data_renamed %>%
        mutate(id_specimen = NA) %>%
        dplyr::mutate(id_specimen = as.integer(id_specimen))

    } else{

      warning("id_specimen column already in new_data, check if content is correct")

    }

  }

  ### preparing dataset to add for each trait
  list_add_data <- vector('list', length(traits_field))
  for (i in 1:length(traits_field)) {

    trait <- traits_field[i]
    if(!any(colnames(new_data_renamed) == trait))
      stop(paste("trait field not found", trait))

    data_trait <-
      new_data_renamed


    data_trait <-
      data_trait %>%
      dplyr::filter(!is.na(!!sym(trait)))


    if(nrow(data_trait) > 0) {
      ### adding trait id and adding potential issues based on trait
      data_trait <-
        .link_trait(data_stand = data_trait, trait = trait)

      if (any(data_trait$trait == 0)) {

        add_0 <- utils::askYesNo("Some value are equal to 0. Do you want to add these values anyway ??")

        if(!add_0)
          data_trait <-
            data_trait %>%
            dplyr::filter(trait != 0)

      }

      ## see what type of value numeric of character
      valuetype <-
        data_trait %>%
        dplyr::distinct(id_trait) %>%
        dplyr::left_join(
          dplyr::tbl(mydb, "traitlist") %>%
            dplyr::select(valuetype, id_trait) %>%
            dplyr::collect(),
          by = c("id_trait" = "id_trait")
        )

      ### Linking individuals
      if (!is.null(individual_plot_field)) {

        individual_plot <-
          "ind_num_sous_plot"

        data_trait <-
          data_trait %>%
          dplyr::rename_at(dplyr::vars(all_of(individual_plot_field)), ~ individual_plot)


        ## not numeric or missing individuals tag
        nbe_not_numeric <-
          suppressWarnings(which(is.na(as.numeric(data_trait$ind_num_sous_plot))))

        data_trait <-
          data_trait %>%
          dplyr::mutate(ind_num_sous_plot = as.numeric(ind_num_sous_plot))

        if(length(nbe_not_numeric) > 0) {
          cli::cli_alert_warning(
            "Number of non numeric (or missing) value in column indicating invividual number in plot : {length(nbe_not_numeric)}"
          )
          print(nbe_not_numeric)

          data_trait <-
            data_trait %>%
            filter(!is.na(ind_num_sous_plot))

          cli::cli_alert_warning("Number of non numeric (or missing) value REMOVED")
        }

        ## vector of id of all plots
        ids_plots_represented <-
          data_trait %>%
          dplyr::distinct(id_liste_plots) %>%
          dplyr::filter(!is.na(id_liste_plots)) %>%
          dplyr::pull()

        ## query of all individuals of these plots
        all_individual_selected_plot <-
          dplyr::tbl(mydb, "data_individuals") %>%
          dplyr::select(ind_num_sous_plot, id_table_liste_plots_n,
                        id_n, id_diconame_n, id_specimen) %>%
          dplyr::filter(id_table_liste_plots_n %in% ids_plots_represented) %>%
          dplyr::collect()


        cli::cli_h3("Checking plot by plot if individuals already linked to selected trait")
        cli::cli_alert_info("Expected for some individuals if added traits measures are new census")

        linked_individuals_list <- vector('list', length(ids_plots_represented))
        linked_individuals_likely_dup <- vector('list', length(ids_plots_represented))
        for (j in 1:length(ids_plots_represented)) {

          ### getting all individuals of selected plot
          all_individual_selected_plot_subset <-
            all_individual_selected_plot %>%
            dplyr::filter(id_table_liste_plots_n == ids_plots_represented[j])

          new_data_renamed_subset <-
            data_trait %>%
            dplyr::filter(id_liste_plots == ids_plots_represented[j])

          ## individuals in new observations linked to data_individuals
          linked_individuals <-
            dplyr::left_join(new_data_renamed_subset,
                             all_individual_selected_plot_subset,
                             by=c("ind_num_sous_plot" = "ind_num_sous_plot"))

          ## getting individuals that have already observations traits_measures table
          individuals_already_traits <-
            dplyr::tbl(mydb, "data_traits_measures") %>%
            dplyr::filter(id_data_individuals %in% !!linked_individuals$id_n) %>%
            dplyr::collect()

          if(nrow(individuals_already_traits) > 0 &
             any(unique(data_trait$id_trait) %in%
                 unique(individuals_already_traits$traitid))) {

            cli::cli_alert_warning("Individuals of plot id {ids_plots_represented[j]} already linked to this trait - consistency should be checked")

            linked_individuals %>%
              dplyr::select(id_new_data,
                            id_trait,
                            id_table_liste_plots_n,
                            id_sub_plots,
                            ind_num_sous_plot,
                            id_n,
                            trait)

            ## traits measures linked to same individuals from same subplot and same trait
            possible_duplicates_measures <-
              individuals_already_traits %>%
              filter(
                traitid == unique(data_trait$id_trait),
                id_sub_plots %in% unique(data_trait$id_sub_plots)
              ) %>%
              dplyr::select(
                            id_data_individuals,
                            id_trait_measures,
                            traitvalue) %>%
              dplyr::rename(traitvalue_exist = traitvalue)


            linked_individuals_already_db <-
              linked_individuals %>%
              dplyr::left_join(possible_duplicates_measures,
                               by = c("id_n" = "id_data_individuals")) %>%
              filter(!is.na(id_trait_measures)) %>%
              dplyr::select(id_new_data, trait, traitvalue_exist)

            linked_individuals_likely_dup[[j]] <-
              linked_individuals_already_db

            # linked_problems_individuals_list[[j]] <-
            #   individuals_already_traits

            # if(any(colnames(new_data_renamed)=="id_diconame_n") &
            #    any(traits_field=="id_diconame_n"))

            ### Which individuals show different information between what is provided and what is in the db
            # problems_individuals <-
            #   linked_individuals %>%
            #   dplyr::filter(id_diconame!=id_diconame_n | dbh.x!=dbh.y)
            #
            # if(nrow(problems_individuals)>0) {
            #   cat(paste(nrow(problems_individuals), "individuals with problematic matches\n",
            #             nrow(linked_individuals), "individuals in total"))
            #
            #   selected_tax <-
            #     dplyr::tbl(mydb,"diconame") %>%
            #     dplyr::filter(id_n %in% problems_individuals$id_diconame_n) %>%
            #     dplyr::collect()
            #
            #   problems_individuals <-
            #     problems_individuals %>%
            #     dplyr::left_join(selected_tax %>%
            #                        dplyr::select(id_n, full_name_no_auth),
            #                      by=c("id_diconame_n"="id_n"))
            #
            #   for (j in 1:nrow(problems_individuals)) {
            #     problems_individuals_selected <-
            #       problems_individuals %>%
            #       dplyr::slice(j)
            #
            #     print(problems_individuals_selected %>%
            #             dplyr::select(plot_name,
            #                           ind_num_sous_plot, dbh.x, dbh.y,
            #                           corrected.name, full_name_no_auth))
            #     response <-
            #       askYesNo("Do you want to still link these individuals?")
            #
            #     if(!response) {
            #       linked_individuals <-
            #         linked_individuals %>%
            #         dplyr::filter(!id_new_data %in% problems_individuals_selected$id_new_data)
            #
            #

            #     }
            #   }
            # }
          }

          linked_individuals_list[[j]] <-
            linked_individuals %>%
            dplyr::select(id_new_data, id_n, id_specimen)

        }

        linked_individuals_list <-
          dplyr::bind_rows(linked_individuals_list)

        linked_individuals_likely_dup <-
          dplyr::bind_rows(linked_individuals_likely_dup)

        if(nrow(linked_individuals_likely_dup) > 0) {

          cli::cli_alert_info("Found {nrow(linked_individuals_likely_dup)} measures likely already in db")

          remove_dup <- askYesNo(msg = "Remove these measures?")
          if(remove_dup)
            data_trait <-
            data_trait %>%
            filter(!id_new_data %in% linked_individuals_likely_dup$id_new_data)

        }



        # linked_problems_individuals_list <-
        #   dplyr::bind_rows(linked_problems_individuals_list)

        ## Adding link to individuals in plots
        data_trait <-
          data_trait %>%
          dplyr::left_join(linked_individuals_list)

        if (!any(colnames(data_trait) == "id_data_individuals")) {

          data_trait <-
            data_trait %>%
            dplyr::rename(id_data_individuals = id_n)

        } else {

          data_trait <-
            data_trait %>%
            dplyr::mutate(id_data_individuals = id_n)

        }

        not_linked_ind <-
          data_trait %>%
          dplyr::filter(is.na(id_data_individuals))

        if (nrow(not_linked_ind) > 0) {
          message("Measures not linked to individuals")
          print(paste(nrow(not_linked_ind), "measures"))
          print(not_linked_ind %>%
                  as.data.frame())
          remove_not_link <-
            utils::askYesNo(msg = "Remove these measures ?")

          unlinked_individuals <-
            not_linked_ind

          if (remove_not_link)
            data_trait <-
            data_trait %>%
            dplyr::filter(!is.na(id_data_individuals))


        }else{
          unlinked_individuals <- NA
        }

        ## identify duplicated individuals i.e. observations linked to same individual
        ids_dup <-
          data_trait %>%
          dplyr::group_by(id_data_individuals) %>%
          dplyr::count() %>%
          dplyr::filter(n > 1, !is.na(id_data_individuals))

        if (nrow(ids_dup) > 0) {
          cli::cli_alert_warning("More than one observation of selected trait for a given individual - {nrow(ids_dup)} individuals concerned - adding issue")

          obs_dup <-
            data_trait %>%
            dplyr::filter(id_data_individuals %in% dplyr::pull(ids_dup, id_data_individuals)) %>%
            dplyr::select(trait, plot_name, ind_num_sous_plot, id_data_individuals, id_new_data)

          issue_2 <- vector(mode = "character", length = nrow(data_trait))
          for (k in 1:nrow(ids_dup)) {
            obs_dup_sel <- obs_dup %>%
              dplyr::filter(id_data_individuals %in% ids_dup$id_data_individuals[k])
            if(length(unique(obs_dup_sel$trait))>1) {
              issue_2[data_trait$id_new_data %in% obs_dup_sel$id_new_data] <-
                rep("more than one observation for a single individual carrying different value", nrow(obs_dup_sel))
            }else{
              issue_2[data_trait$id_new_data %in% obs_dup_sel$id_new_data] <-
                rep("more than one observation for a single individual carrying identical value", nrow(obs_dup_sel))
            }
          }

          issue_2[issue_2 == ""] <- NA

          ## merging issue
          data_trait <-
            data_trait %>%
            tibble::add_column(issue_2 = issue_2) %>%
            dplyr::mutate(issue = paste(ifelse(is.na(issue), "", issue), ifelse(is.na(issue_2), "", issue_2), sep = ", ")) %>%
            dplyr::mutate(issue = ifelse(issue ==", ", NA, issue)) %>%
            dplyr::select(-issue_2)
        }
      } # end Linking individuals

      ## adding id_diconame_n ONLY if no individuals or specimen linked
      # otherwise, identification retrieved from individual or specimen
      if (!any(colnames(data_trait) == "id_diconame")) {

        data_no_specimen_no_individual <-
          data_trait

        if (any(colnames(data_trait) == "id_data_individuals")) {
          data_no_specimen_no_individual <-
            data_no_specimen_no_individual %>%
            dplyr::filter(is.na(id_data_individuals))
        }

        if (any(colnames(data_trait) == "id_specimen")) {
          data_no_specimen_no_individual <-
            data_no_specimen_no_individual %>%
            dplyr::filter(is.na(id_specimen))
        }

        data_trait <-
          data_trait %>%
          dplyr::mutate(id_diconame = NA) %>%
          dplyr::mutate(id_diconame = as.integer(id_diconame))

      } else {

        data_no_specimen_no_individual <-
          data_trait %>%
          dplyr::filter(is.na(id_data_individuals) & is.na(id_specimen) & is.na(id_diconame))

        ids_ind <- data_trait$id_data_individuals

        ## retrieving taxonomic information for linked individuals
        founded_ind <-
          query_plots(extract_individuals = T, id_individual = ids_ind, remove_ids = FALSE)

        ids_diconames <- data_trait$id_diconame

        data_trait_compa_taxo <-
          data_trait %>%
          dplyr::left_join(dplyr::tbl(mydb, "diconame") %>%
                      dplyr::filter(id_n %in% ids_diconames) %>%
                      dplyr::select(tax_fam, tax_gen, full_name_no_auth, id_n) %>%
                      dplyr::collect(),
                    by=c("id_diconame"="id_n"))

        data_trait_compa_taxo <-
          data_trait_compa_taxo %>%
          dplyr::left_join(founded_ind %>%
                      dplyr::select(id_n, tax_fam, tax_gen, full_name_no_auth) %>%
                      dplyr::rename(tax_fam_linked = tax_fam, tax_gen_linked = tax_gen, full_name_no_auth_linked = full_name_no_auth),
                    by=c("id_data_individuals"="id_n")) %>%
          dplyr::select(id_new_data, tax_fam, tax_fam_linked, tax_gen,
                        tax_gen_linked, full_name_no_auth, full_name_no_auth_linked)

        diff_fam <-
          data_trait_compa_taxo %>%
          dplyr::filter(tax_fam != tax_fam_linked)
        if (nrow(diff_fam) > 0) {
          message("Some measures linked to individuals carry different family")
          print(diff_fam)
          diff_fam <-
            diff_fam %>%
            dplyr::mutate(
              issue = paste(
                "ident. when measured and in DB)",
                full_name_no_auth,
                full_name_no_auth_linked
              )
            )
          ## merging issue
          data_trait <-
            data_trait %>%
            dplyr::left_join(
              diff_fam %>%
                dplyr::select(id_new_data, issue) %>%
                dplyr::rename(issue_tax = issue),
              by = c("id_new_data" = "id_new_data")
            )

          data_trait <-
            data_trait %>%
            dplyr::mutate(issue = paste(ifelse(is.na(issue), "", issue),
                                        ifelse(is.na(issue_tax), "", issue_tax), sep = ", ")) %>%
            dplyr::mutate(issue = ifelse(issue == ", ", NA, issue)) %>%
            dplyr::select(-issue_tax)
        }

        diff_gen <-
          data_trait_compa_taxo %>%
          dplyr::filter(tax_gen != tax_gen_linked, !id_new_data %in% diff_fam$id_new_data)

        if(nrow(diff_gen)>0) {
          message("Some measures linked to individuals carry different genus")
          print(diff_gen)
          diff_gen <-
            diff_gen %>%
            dplyr::mutate(issue = paste("ident. when measured and in DB)",
                                 full_name_no_auth, full_name_no_auth_linked))

          ## merging issue
          data_trait <-
            data_trait %>%
            dplyr::left_join(diff_gen %>%
                        dplyr::select(id_new_data, issue) %>%
                        dplyr::rename(issue_tax = issue),
                      by=c("id_new_data"="id_new_data"))

          data_trait <-
            data_trait %>%
            dplyr::mutate(issue = paste(ifelse(is.na(issue), "", issue),
                                        ifelse(is.na(issue_tax), "", issue_tax), sep = ", ")) %>%
            dplyr::mutate(issue = ifelse(issue ==", ", NA, issue)) %>%
            dplyr::select(-issue_tax)
        }

      }

      no_linked_measures <- FALSE
      if (nrow(data_no_specimen_no_individual) > 0) {
        print(data_no_specimen_no_individual)
        cli::cli_alert_danger(
          "no taxa identification, no link to specimen, no link to individuals for measures/observations"
        )
        no_linked_measures <- TRUE
      }

      ### choosing kind of measures
      cli::cli_h3("basis")
      if (!any(colnames(data_trait) == "basisofrecord")) {
        choices <-
          dplyr::tibble(
            basis =
              c(
                'LivingSpecimen',
                'PreservedSpecimen',
                'FossilSpecimen',
                'literatureData',
                'traitDatabase',
                'expertKnowledge'
              )
          )

        print(choices)
        selected_basisofrecord <-
          readline(prompt = "Choose basisofrecord : ")

        data_trait <-
          data_trait %>%
          dplyr::mutate(basisofrecord = rep(choices$basis[as.numeric(selected_basisofrecord)], nrow(.)))
      }


      ### comparing measures from previous census
      if(multiple_census &
         valuetype$valuetype == "numeric") {
        cli::cli_alert_info("Comparing measures from previous censuses")

        comparisons <-
          data_trait %>%
          dplyr::select(id_data_individuals, trait) %>%
          dplyr::left_join(dplyr::tbl(mydb, "data_traits_measures") %>%
                             dplyr::filter(traitid == !!unique(data_trait$id_trait)) %>%
                             dplyr::select(id_data_individuals, traitvalue) %>%
                             dplyr::collect(),
                           by=c("id_data_individuals"="id_data_individuals"),
                           relationship = "many-to-many") %>%
          filter(!is.na(traitvalue)) %>%
          dplyr::group_by(id_data_individuals) %>%
          dplyr::summarise(traitvalue = max(traitvalue, na.rm = TRUE),
                           trait = dplyr::first(trait)) %>%
          dplyr::mutate(traitvalue = replace(traitvalue, traitvalue == -Inf, NA))

        ## comparison with previous census if new values is lower than previous --> issue annotated
        if (any(!is.na(comparisons$traitvalue))) {
          # message("\n multiple data")
          finding_incoherent_values <-
            comparisons %>%
            dplyr::mutate(diff = trait - traitvalue) %>%
            dplyr::filter(diff < 0)

          if(any( finding_incoherent_values$diff < 0)) {
            cli::cli_alert_danger("Incoherent new values compared to previous censuses")
            finding_incoherent_values <-
              finding_incoherent_values %>%
              dplyr::mutate(issue_new =
                              ifelse(diff < 0, "value lower than previous census", NA))

            ### merging issues
            data_trait <-
              data_trait %>%
              dplyr::left_join(finding_incoherent_values %>%
                                 dplyr::select(id_data_individuals, issue_new),  by = c("id_data_individuals"="id_data_individuals")) %>%
              dplyr::mutate(issue = ifelse(!is.na(issue), paste(issue, issue_new, sep="|"), issue_new)) %>%
              dplyr::select(-issue_new)

          }
        }
      }


      ### identify if measures are already within DB
      cli::cli_alert_info("Identifying if imported values are already in DB")
      trait_id <- unique(data_trait$id_trait)
      selected_data_traits <-
        data_trait %>%
        dplyr::select(id_data_individuals,
                      id_trait,
                      id_liste_plots,
                      id_sub_plots,
                      trait,
                      issue)

      #### identify if duplicate values in the dataset to upload

      duplicated_rows <- selected_data_traits %>%
        group_by(id_data_individuals,
                 id_trait,
                 id_liste_plots,
                 id_sub_plots) %>%
        count() %>%
        filter(n > 1)

      if (nrow(duplicated_rows) > 0) {
        print(duplicated_rows)
        cli::cli_alert_warning("Duplicated values for dataset to upload")
        if (!askYesNo(msg = "Are you sure you want to continue ?")) stop("check duplicated value")
      }

      all_vals <-
        dplyr::tbl(mydb, "data_traits_measures") %>%
        dplyr::select(id_data_individuals, traitid, id_table_liste_plots, id_sub_plots,
                      traitvalue, traitvalue_char, issue) %>%
        dplyr::filter(traitid == trait_id, id_data_individuals %in% !!selected_data_traits$id_data_individuals) %>% #, !is.na(id_sub_plots)
        dplyr::collect()

      if (valuetype$valuetype == "numeric")
        all_vals <-
        all_vals %>%
        dplyr::rename(id_trait = traitid,
                      id_liste_plots = id_table_liste_plots,
                      trait = traitvalue) %>%
        dplyr::select(-traitvalue_char)

      if (valuetype$valuetype == "character")
        all_vals <- all_vals %>%
        dplyr::rename(id_trait = traitid,
                      id_liste_plots = id_table_liste_plots,
                      trait = traitvalue_char) %>%
        dplyr::select(-traitvalue) %>%
        dplyr::mutate(trait = stringr::str_trim(trait))

      if (valuetype$valuetype == "ordinal")
        all_vals <- all_vals %>%
        dplyr::rename(id_trait = traitid,
                      id_liste_plots = id_table_liste_plots,
                      trait = traitvalue_char) %>%
        dplyr::select(-traitvalue) %>%
        dplyr::mutate(trait = stringr::str_trim(trait))

      if (nrow(all_vals) > 0) {
        duplicated_rows <-
          dplyr::bind_rows(selected_data_traits,
                           all_vals) %>%
          dplyr::filter(is.na(issue)) %>%
          dplyr::group_by(id_data_individuals,
                          id_trait,
                          id_liste_plots,
                          id_sub_plots,
                          issue) %>%
          dplyr::count() %>%
          dplyr::filter(n > 1) %>%
          filter(id_data_individuals %in% selected_data_traits$id_data_individuals)
        # %>% # , id_data_individuals==73764
        # dplyr::filter(!grepl("more than one observation", issue))

        duplicated_rows_with_issue_no_double <-
          dplyr::bind_rows(selected_data_traits,
                           all_vals) %>%
          dplyr::filter(!is.na(issue),!grepl("more than one observation", issue)) %>%
          dplyr::select(-issue) %>%
          dplyr::group_by(id_data_individuals, id_trait, id_liste_plots, id_sub_plots) %>%
          dplyr::count() %>%
          dplyr::filter(n > 1)

        duplicated_rows_with_issue_double <-
          dplyr::bind_rows(selected_data_traits,
                           all_vals) %>%
          dplyr::filter(!is.na(issue), grepl("more than one observation", issue)) %>%
          dplyr::select(-issue) %>%
          dplyr::group_by(id_data_individuals, id_trait, id_liste_plots, id_sub_plots) %>%
          dplyr::count() %>%
          dplyr::filter(n > 2)

        # %>% #
        #   dplyr::filter(!grepl("more than one observation", issue))
        duplicated_rows <-
          dplyr::bind_rows(duplicated_rows,
                           duplicated_rows_with_issue_no_double,
                           duplicated_rows_with_issue_double)

        if (nrow(duplicated_rows) > 1) {
          cli::cli_alert_danger("Some values are already in DB or some values are duplicated in the dataset to upload")

          print(duplicated_rows %>%
                  dplyr::ungroup() %>%
                  dplyr::select(id_data_individuals, id_liste_plots, id_sub_plots))

          rm_val <- askYesNo(msg = "Exclude these values ?")

          if (rm_val) {

            data_trait <-
              data_trait %>%
              dplyr::filter(!id_data_individuals %in% duplicated_rows$id_data_individuals)

            cli::cli_alert_warning("{nrow(duplicated_rows)} values excluded values because already in DB")
          }

          if (!allow_multiple_value) if (nrow(data_trait) < 1) stop("no new values anymore to import after excluding duplicates")
        }
      }

      cli::cli_h3(".add_modif_field")
      data_trait <-
        .add_modif_field(dataset = data_trait)


      if (valuetype$valuetype == "ordinal" |
          valuetype$valuetype == "character")
        val_type <- "character"

      if (valuetype$valuetype == "numeric")
        val_type <- "numeric"

      if (valuetype$valuetype == "integer")
        val_type <- "numeric"

      cli::cli_h3("data_to_add")
      data_to_add <-
        dplyr::tibble(
          id_table_liste_plots = data_trait$id_liste_plots,
          id_data_individuals = data_trait$id_data_individuals,
          id_specimen = data_trait$id_specimen,
          id_diconame = data_trait$id_diconame,
          id_colnam = data_trait$id_colnam,
          id_sub_plots = data_trait$id_sub_plots,
          country = data_trait$country,
          decimallatitude = data_trait$decimallatitude,
          decimallongitude = data_trait$decimallongitude,
          elevation = ifelse(rep(
            any(colnames(data_trait) == "elevation"), nrow(data_trait)
          ), data_trait$elevation, NA),
          verbatimlocality = ifelse(rep(
            any(colnames(data_trait) == "verbatimlocality"), nrow(data_trait)
          ), data_trait$verbatimlocality, NA),
          basisofrecord = data_trait$basisofrecord,
          references = ifelse(rep(
            any(colnames(data_trait) == "reference"), nrow(data_trait)
          ), data_trait$reference, NA),
          year = ifelse(rep(
            any(colnames(data_trait) == "year"), nrow(data_trait)
          ), data_trait$year, NA),
          month = ifelse(rep(
            any(colnames(data_trait) == "month"), nrow(data_trait)
          ), data_trait$month, NA),
          day = ifelse(rep(any(
            colnames(data_trait) == "day"
          ), nrow(data_trait)), data_trait$day, NA),
          measurementremarks = ifelse(rep(
            any(colnames(data_trait) == "measurementremarks"),
            nrow(data_trait)
          ), data_trait$measurementremarks, NA),
          measurementmethod = ifelse(rep(
            any(colnames(data_trait) == "measurementmethod"), nrow(data_trait)
          ), data_trait$measurementmethod, NA),
          traitid = data_trait$id_trait,
          traitvalue = ifelse(
            rep(val_type == "numeric", nrow(data_trait)),
            data_trait$trait,
            NA
          ),
          traitvalue_char = ifelse(
            rep(val_type == "character", nrow(data_trait)),
            as.character(data_trait$trait),
            NA
          ),
          original_tax_name = ifelse(rep(
            any(colnames(data_trait) == "original_tax_name"), nrow(data_trait)
          ), data_trait$original_tax_name, NA),
          original_plot_name = ifelse(rep(
            any(colnames(data_trait) == "original_plot_name"), nrow(data_trait)
          ), data_trait$original_plot_name, NA),
          original_specimen = ifelse(rep(
            any(colnames(data_trait) == "original_specimen"), nrow(data_trait)
          ), data_trait$original_specimen, NA),
          issue = data_trait$issue,
          date_modif_d = data_trait$date_modif_d,
          date_modif_m = data_trait$date_modif_m,
          date_modif_y = data_trait$date_modif_y
        )

      if(no_linked_measures)
        list_add_data[[i]] <-
        data_no_specimen_no_individual

      list_add_data[[i]] <-
        data_to_add

      print(data_to_add)

      # print(data_to_add %>%
      #         dplyr::left_join(tbl(mydb, "data_liste_sub_plots") %>%
      #                            select(typevalue, id_type_sub_plot, id_sub_plots) %>%
      #                            collect(), by=c("id_sub_plots"="id_sub_plots"))) %>%
      #   dplyr::left_join(tbl(mydb, "subplotype_list") %>%
      #                      select(id_subplotype, type ) %>%
      #                      collect(), by=c("id_type_sub_plot"="id_subplotype")) %>%
      #   View()

      if (data_to_add %>% dplyr::distinct() %>% nrow() != nrow(data_to_add)) {

        duplicates_lg <- duplicated(data_to_add)

        cli::cli_alert_warning("Duplicates in new data for {trait} concerning {length(duplicates_lg[duplicates_lg])} id(s)")

        cf_merge <-
          askYesNo(msg = "confirm merging duplicates?")

        if (cf_merge) {

          issues_dup <- data_to_add %>%
            filter(id_data_individuals %in% data_to_add[duplicates_lg, "id_data_individuals"]) %>%
            dplyr::select(issue, id_data_individuals)

          ## resetting issue
          if(any(grepl("identical value", issues_dup$issue))) {

            issues_dup_modif_issue <-
              issues_dup[grepl("identical value", issues_dup$issue),]

            data_to_add <-
              data_to_add %>%
              mutate(issue = replace(issue, id_data_individuals %in% issues_dup_modif_issue$id_data_individuals, NA))

          }

          data_to_add <- data_to_add %>% dplyr::distinct()
        } else{
          if (!allow_multiple_value) stop()
        }

      }

      response <-
        utils::askYesNo("Confirm add these data to data_traits_measures table?")

      if(add_data & response) {

        DBI::dbWriteTable(mydb, "data_traits_measures",
                          data_to_add,
                          append = TRUE,
                          row.names = FALSE)

        cli::cli_alert_success("Adding data : {nrow(data_to_add)} values added")


        if (!is.null(features_field)) {

          imported_data <- tbl(mydb, "data_traits_measures") %>%
            filter(date_modif_d == !!data_to_add$date_modif_d[1],
                   date_modif_m == !!data_to_add$date_modif_m[1],
                   date_modif_y == !!data_to_add$date_modif_y[1]) %>%
            select(id_trait_measures, id_data_individuals) %>%
            collect() %>%
            arrange(id_trait_measures)

          ids <- imported_data %>% slice((nrow(imported_data)-nrow(data_to_add)+1):nrow(imported_data))

          data_feats <-
            data_trait %>% select(all_of(features_field), id_data_individuals) %>%
            mutate(id_trait_measures = ids$id_trait_measures,
                   id_data_individuals = ids$id_data_individuals)

          add_traits_measures_features(
            new_data = data_feats,
            id_trait_measures = "id_trait_measures",
            features = features_field , #
            add_data = T
          )

        }



      }

    } else{

      cli::cli_alert_info("no added data for {trait} - no values different of 0")

    }
  }

  # linked_problems_individuals_list <-
  #   linked_problems_individuals_list %>%
  #   dplyr::select(plot_name,
  #                 ind_num_sous_plot,
  #                 country,
  #                 leaf_area,
  #                 specific_leaf_area,
  #                 dbh.x,
  #                 dbh.y,
  #                 original_tax_name,
  #                 corrected.name,
  #                 full_name_no_auth,
  #                 id_table_liste_plots_n,
  #                 ddlon,
  #                 ddlat) %>%
  #   left_join(tbl(mydb, "data_liste_plots") %>%
  #               dplyr::select(plot_name, id_liste_plots) %>%
  #               collect(), by=c("id_table_liste_plots_n"="id_liste_plots")) %>%
  #   rename(dbh_provided = dbh.x,
  #          dbh_database = dbh.y,
  #          name_provided = original_tax_name,
  #          name_provided_corrected = corrected.name,
  #          name_database = full_name_no_auth,
  #          plot_name_provided = plot_name.x,
  #          plot_name_corrected = plot_name.y)


  if(exists('unlinked_individuals'))
    return(list(list_traits_add = list_add_data, unlinked_individuals = unlinked_individuals))

  if(!exists('unlinked_individuals'))
    return(list(list_traits_add = list_add_data))

}



#' Add new specimens data
#'
#' Add new specimens data
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data tibble new data to be imported
#' @param col_names_select string plot name of the selected plots
#' @param col_names_corresp string country of the selected plots
#' @param plot_name_field integer indicate which name of col_names_select is the id for matching liste plots table
#' @param collector_field integer indicate which name of col_names_select is the id for matching collector
#'
#' @param launch_adding_data logical FALSE whether adding should be done or not
#'
#' @return No return value individuals updated
#' @export
add_specimens <- function(new_data ,
                          col_names_select,
                          col_names_corresp,
                          # id_col,
                          plot_name_field = NULL,
                          collector_field = NULL,
                          launch_adding_data = FALSE) {

  # logs <-
  #   dplyr::tibble(
  #     column = as.character(),
  #     note = as.character()
  #   )

  if(!exists("mydb")) call.mydb()

  if(length(col_names_select)!=length(col_names_corresp))
    stop("Provide same numbers of corresponding and selected colnames")

  new_data_renamed <-
    new_data %>%
    mutate(id_new_data=1:nrow(.))

  for (i in 1:length(col_names_select)) {
    if (any(colnames(new_data_renamed) == col_names_select[i])) {
      new_data_renamed <-
        new_data_renamed %>%
        dplyr::rename(!!col_names_corresp[i] := !!col_names_select[i])
        # dplyr::rename_at(dplyr::vars(col_names_select[i]), ~ col_names_corresp[i])
    } else{
      stop(paste(
        "Column name provided not found in provided new dataset",
        col_names_select[i]
      ))
    }
  }

  col_names_corresp <-
    c(col_names_corresp, "id_new_data")

  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(all_of(col_names_corresp))

  ### check diconame id
  if(!any(colnames(new_data_renamed)=="idtax_n")) stop("idtax_n column missing")

  if (any(new_data_renamed$idtax_n == 0))
    stop(paste(
      "idtax_n is NULL for",
      sum(new_data_renamed$idtax_n == 0),
      "individuals"
    ))

  if (any(is.na(new_data_renamed$idtax_n)))
    stop(paste(
      "idtax_n is missing for",
      sum(new_data_renamed$idtax_n == 0),
      "individuals"
    ))

  unmatch_id_diconame <-
    new_data_renamed %>%
    dplyr::select(idtax_n) %>%
    dplyr::left_join(try_open_postgres_table_mem(table = "table_taxa", con = mydb_taxa) %>%
                       dplyr::select(idtax_n, idtax_good_n) %>%
                       dplyr::filter(idtax_n %in% !!new_data_renamed$idtax_n) %>%
                       dplyr::collect() %>%
                       dplyr::mutate(tag = 1), by=c("idtax_n" = "idtax_n")) %>%
    dplyr::filter(is.na(tag)) %>%
    dplyr::pull(idtax_n)

  if (length(unmatch_id_diconame) > 0)
    stop(paste("idtax_n not found in table_taxa", unmatch_id_diconame))


  ### check locality and adding it if link to plots
  if(!any(colnames(new_data_renamed) == "locality"))
    warning("locality column missing"
    )

  ### Linking collectors names
  if (!is.null(collector_field)) {


    new_data_renamed <-
      .link_colnam(data_stand = new_data_renamed,
                   column_searched = collector_field)

    new_data_renamed <-
      new_data_renamed %>%
      dplyr::select(-original_colnam)

  } else{
    if (!any(colnames(new_data_renamed) == "id_colnam"))
      stop("indicate the field if of collector name for standardizing")
  }

  ### check determination data
  if (any(colnames(new_data_renamed) == "detd")) {
    new_data_renamed <-
      new_data_renamed %>%
      mutate(detd = as.numeric(detd))
  }

  if(any(colnames(new_data_renamed) == "detm")) {
    new_data_renamed <-
      new_data_renamed %>%
      mutate(detm = as.numeric(detm))
  }

  if (any(colnames(new_data_renamed) == "dety")) {
    new_data_renamed <-
      new_data_renamed %>%
      mutate(dety = as.numeric(dety))
  }

  if (!any(names(new_data_renamed) == "suffix")) {

    new_data_renamed <-
      new_data_renamed %>%
      dplyr::mutate(suffix = NA) %>%
      dplyr::mutate(suffix = as.character(suffix))

  }

  ## check if not duplicates in new specimens
  dup_imported_datasets <-
    new_data_renamed %>%
    dplyr::select(colnbr, id_colnam, suffix, id_new_data) %>%
    group_by(colnbr, id_colnam, suffix) %>%
    count() %>%
    filter(n > 1)

  if (nrow(dup_imported_datasets) > 0) {
    print(dup_imported_datasets)
    stop("Duplicates in imported dataset")
  }


  ## check if specimens are not already in database
  matched_specimens <-
    dplyr::tbl(mydb, "specimens") %>%
    dplyr::select(colnbr, id_colnam, id_specimen) %>%
    dplyr::filter(!is.na(id_colnam)) %>%
    dplyr::collect() %>%
    dplyr::left_join(
      new_data_renamed %>%
        dplyr::select(colnbr, id_colnam, id_new_data),
      by = c("colnbr" = "colnbr", "id_colnam" = "id_colnam")
    ) %>%
    dplyr::filter(!is.na(id_new_data))

    # dplyr::mutate(combined = paste(colnbr, id_colnam, sep="-")) %>%
    # dplyr::left_join(new_data_renamed %>%
    #                    dplyr::select(colnbr, id_colnam, id_new_data) %>%
    #                    dplyr::mutate(combined = paste(colnbr, id_colnam, sep="-")) %>%
    #                    dplyr::select(combined, id_new_data)) %>%
    # dplyr::filter(!is.na(id_new_data))

  if (nrow(matched_specimens) > 0) {
    warning(paste("New specimens already in database", nrow(matched_specimens)))
    print(matched_specimens)
  }

  new_data_renamed <-
    new_data_renamed %>%
    tibble::add_column(data_modif_d=lubridate::day(Sys.Date()),
                       data_modif_m=lubridate::month(Sys.Date()),
                       data_modif_y=lubridate::year(Sys.Date()))

  if (any(colnames(new_data_renamed) == "col_name"))
    new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(-col_name)

  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(-id_new_data)

  if(launch_adding_data) {

    print(list(new_data_renamed))

    confirmed <- utils::askYesNo("Confirm adding?")

    if(confirmed) {

      DBI::dbWriteTable(mydb, "specimens", new_data_renamed, append = TRUE, row.names = FALSE)

      message(paste0(nrow(new_data_renamed), " records added to specimens table"))
    }

  }

  return(list(new_data_renamed))

}


#' Internal function
#'
#' Replace NA by 9999
#'
#' @return tibble
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param vec vector
#'
#' @export
replace_NA <- function(vec, inv = FALSE) {

  vec_num <- names(vec)[unlist(lapply(vec, is.numeric))]
  vec_char <- names(vec)[unlist(lapply(vec, is.character))]

  if(!inv) {
    vec <- vec %>%
      dplyr::mutate_at(dplyr::vars(dplyr::all_of(c(
        vec_num
      ))),
      ~ tidyr::replace_na(. , -9999))

    vec <- vec %>%
      dplyr::mutate_at(dplyr::vars(dplyr::all_of(c(
        vec_char
      ))),
      ~ tidyr::replace_na(. , "-9999"))

  }

  if(inv) {

    vec <- vec %>%
      dplyr::mutate_at(dplyr::vars(dplyr::all_of(c(
        vec_char
      ))),
      ~ dplyr::na_if(. , "-9999"))

    vec <- vec %>%
      dplyr::mutate_at(dplyr::vars(dplyr::all_of(c(
        vec_num
      ))),
      ~ dplyr::na_if(. , -9999))

  }

  return(vec)

}


#' Internal function
#'
#' Compare values between of given columns and identify different values based on id matches
#'
#' @return tibble
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param dataset tibble contain values to compare and id for matching
#' @param col_new string vector containing column names of dataset
#' @param id_col_nbr string vector
#' @param type_data string indicate which table of database is targetted. e.g. 'individuals'
#'
#' @export
.find_ids <- function(dataset, col_new, id_col_nbr, type_data) {

  ids_new_data <-
    dataset %>%
    dplyr::select(!!col_new[id_col_nbr]) %>%
    dplyr::pull()

  if(type_data=="taxa")
    corresponding_data <-
      dplyr::tbl(mydb_taxa, "table_taxa")

  if(type_data=="individuals")
    corresponding_data <-
      dplyr::tbl(mydb, "data_individuals")

  if(type_data == "trait")
    corresponding_data <-
      dplyr::tbl(mydb_taxa, "table_traits")

  if(type_data == "sp_trait_measures")
    corresponding_data <-
      dplyr::tbl(mydb, "table_traits_measures")

  if(type_data == "plot_data")
    corresponding_data <-
      dplyr::tbl(mydb, "data_liste_plots")

  if(type_data == "data_liste_sub_plots")
    corresponding_data <-
      dplyr::tbl(mydb, "data_liste_sub_plots")

  if(type_data == "specimens")
    corresponding_data <-
      dplyr::tbl(mydb, "specimens")

  if(type_data == "trait_measures") {
    # all_data <-
    #   dplyr::tbl(mydb, "data_traits_measures")

    if(col_new[id_col_nbr] %in% c("id_n", "id_old")) {

      corresponding_data <-
        .get_trait_individuals_values(traits = col_new[-id_col_nbr])

      corresponding_data <- corresponding_data[[1]]
    }

    if(col_new[id_col_nbr] %in% c("id_trait_measures")) {

      # new_name <- col_new[-id_col_nbr]

      corresponding_data <-
        dplyr::tbl(mydb, "data_traits_measures")
      # %>%
      #   dplyr::select(id_trait_measures, traitvalue, issue, measurementremarks)
      # %>%
        # dplyr::collect() %>%
        # rename(!!new_name := traitvalue)

    }
  }

  id <- "id"

  corresponding_data_full <-
    corresponding_data %>%
    dplyr::rename_at(dplyr::vars(col_new[id_col_nbr]), ~ id) %>%
    dplyr::filter(id %in% ids_new_data)

  corresponding_data <-
    corresponding_data %>%
    dplyr::select(dplyr::all_of(col_new)) %>%
    dplyr::rename_at(dplyr::vars(col_new[id_col_nbr]), ~ id) %>%
    dplyr::filter(id %in% ids_new_data) %>%
    dplyr::collect()

  all_tb_update <- vector('list', length(col_new[-id_col_nbr]))
  for (i in 1:length(col_new[-id_col_nbr])) {
    cat(" ", col_new[-id_col_nbr][i])
    # var <- enquo(col_names_corresp[-id_col][i])

    var_ <- col_new[-id_col_nbr][i]

    var <- rlang::enquo(var_)

    var_new <- paste0(col_new[-id_col_nbr][i], "_new")
    var_old <- paste0(col_new[-id_col_nbr][i], "_old")
    id <- col_new[id_col_nbr]
    var_id <- rlang::enquo(id)

    quo_var <- rlang::quo_name(rlang::enquo(id))

    select_col_new <-
      dplyr::select(dataset, !!var_id, !!var) %>%
      dplyr::rename(!!var_new := !!var)

    id <- "id"
    select_col_new <-
      select_col_new %>%
      dplyr::rename_at(dplyr::vars(col_new[id_col_nbr]), ~ id)

    select_col_old <-
      dplyr::select(corresponding_data, "id", !!var) %>%
      dplyr::rename(!!var_old := !!var)

    matches <-
      dplyr::left_join(select_col_new, select_col_old, by = c("id"="id"))

    matches <- replace_NA(vec = matches)


    # matches[,2] <-
    #   replace_NA(vec = matches[,2])
    # matches[,3] <-
    #   replace_NA(vec = matches[,3])

    # matches[is.na(matches[,2]), 2] <- -9999
    # matches[is.na(matches[,3]), 3] <- -9999

    quo_var <- rlang::parse_expr(rlang::quo_name(rlang::enquo(var_new)))
    quo_var_old <- rlang::parse_expr(rlang::quo_name(rlang::enquo(var_old)))

    matches <-
      matches %>%
      dplyr::filter(!!quo_var != !!quo_var_old)

    if (nrow(matches) > 0) {

      matches[, 2] <- replace_NA(vec = matches[, 2], inv = T)
      matches[, 3] <- replace_NA(vec = matches[, 3], inv = T)

    }

    all_tb_update[[i]] <- matches
    names(all_tb_update)[i] <- col_new[-id_col_nbr][i]
  }

  return(list(corresponding_data_full, all_tb_update))
}







#' Internal function
#'
#' rename columns based on new and old columns names
#'
#' @return tibble
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param dataset tibble
#' @param col_old string vector
#' @param col_new string vector
#'
#' @export
.rename_data <- function(dataset, col_old, col_new) {

  if (length(col_old) != length(col_new))
    stop("number of new columns names different of number of selected column names")

  for (i in 1:length(col_old)) {
    if (any(colnames(dataset) == col_old[i])) {
      dataset <-
        dataset %>%
        dplyr::rename_at(dplyr::vars(col_old[i]), ~ col_new[i])
    } else{
      stop(paste(
        "Column name provided not found in provided new dataset",
        col_old[i]
      ))
    }
  }
  return(dataset)
}




#' Internal function
#'
#' Add modification day month and year column before adding/updating
#'
#' @return vector
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param dataset string tibble to add dates fields
#'
#' @export
.add_modif_field <- function(dataset) {
  dataset <-
    dataset %>%
    tibble::add_column(date_modif_d = lubridate::day(Sys.Date()),
                       date_modif_m = lubridate::month(Sys.Date()),
                       date_modif_y = lubridate::year(Sys.Date()))
  return(dataset)
}


#' Internal function
#'
#' Compute
#'
#' @return vector
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @export
.query_unmatched_specimens <- function() {

  all_herbarium_individuals <-
    dplyr::tbl(mydb, "data_individuals") %>%
    dplyr::select(herbarium_nbe_char, herbarium_code_char, herbarium_nbe_type, id_diconame_n, id_specimen, id_n) %>%
    dplyr::filter(!is.na(herbarium_nbe_char) | !is.na(herbarium_code_char) | !is.na(herbarium_nbe_type)) %>%
    dplyr::collect()

  ### all specimens with more than one id_diconame in table individuals
  all_herbarium_individuals_not_linked_diff_tax <-
    all_herbarium_individuals %>%
    dplyr::filter(is.na(id_specimen), !is.na(herbarium_nbe_char)) %>%
    dplyr::distinct(herbarium_nbe_char, id_diconame_n) %>%
    dplyr::group_by(herbarium_nbe_char) %>%
    dplyr::count() %>%
    dplyr::filter(n>1)

  #### all specimens with more than one id_diconame in table individuals with names
  all_herbarium_individuals_not_linked_diff_tax <-
    all_herbarium_individuals %>%
    dplyr::filter(is.na(id_specimen), !is.na(herbarium_nbe_char)) %>%
    dplyr::distinct(herbarium_nbe_char, id_diconame_n) %>%
    dplyr::left_join(dplyr::tbl(mydb, "diconame") %>%
                dplyr::select(id_n, full_name_no_auth, tax_gen) %>%
                  dplyr::collect(),
              by=c("id_diconame_n"="id_n")) %>%
    dplyr::filter(herbarium_nbe_char %in% dplyr::pull(all_herbarium_individuals_not_linked_diff_tax,
                                        herbarium_nbe_char)) %>%
    dplyr::arrange(herbarium_nbe_char)

  ### all specimens with more than one genus in table individuals
  herb_specimen_diff_gen <-
    all_herbarium_individuals_not_linked_diff_tax %>%
    dplyr::distinct(herbarium_nbe_char, tax_gen) %>%
    dplyr::group_by(herbarium_nbe_char) %>%
    dplyr::count() %>%
    dplyr::filter(n>1)

  ### all individuals concerned by specimens with more than one genus in table individuals
  data_individuals_concerned <-
    dplyr::tbl(mydb, "data_individuals") %>%
    dplyr::filter(herbarium_nbe_char %in% !!herb_specimen_diff_gen$herbarium_nbe_char) %>%
    dplyr::collect() %>%
    dplyr::select(dbh, code_individu, sous_plot_name, ind_num_sous_plot, herbarium_nbe_char,
                  herbarium_code_char, herbarium_nbe_type, id_diconame_n) %>%
    dplyr::left_join(dplyr::tbl(mydb, "diconame") %>%
                       dplyr::select(id_n, full_name_no_auth, tax_gen, tax_esp, tax_fam) %>%
                       dplyr::collect(),
                     by=c("id_diconame_n"="id_n")) %>%
    dplyr::arrange(herbarium_nbe_char) %>%
    dplyr::collect()

  ### extraction of all specimens not linked to specimens table excluding problematic specimens
  all_herbarium_individuals_not_linked <-
    all_herbarium_individuals %>%
    dplyr::filter(is.na(id_specimen), !is.na(herbarium_nbe_char)) %>%
    dplyr::distinct(herbarium_nbe_char, id_diconame_n) %>%
    dplyr::filter(!herbarium_nbe_char %in%
                    unique(dplyr::pull(all_herbarium_individuals_not_linked_diff_tax,
                                herbarium_nbe_char)))

  regexp <- "[[:digit:]]+"
  num_extracted <-
    stringr::str_extract(all_herbarium_individuals_not_linked$herbarium_nbe_char, regexp)

  df <-
    dplyr::tibble(full = all_herbarium_individuals_not_linked$herbarium_nbe_char,
           num = num_extracted)

  coll_extracted <-
    apply(df, MARGIN = 1, FUN = function(x) gsub(x[2], "", x[1]))
  coll_extracted <-
    trimws(coll_extracted)

  all_herbarium_individuals_not_linked <-
    all_herbarium_individuals_not_linked %>%
    tibble::add_column(col_name = coll_extracted) %>%
    tibble::add_column(colnbr = num_extracted)

  # all_herbarium_individuals_not_linked <-
  #   .link_colnam(data_stand = all_herbarium_individuals_not_linked,
  #                collector_field = 3)

  all_herbarium_individuals_not_linked <-
    .link_colnam(
    data_stand = all_herbarium_individuals_not_linked,
    column_searched = "col_name",
    column_name = "colnam",
    id_field = "id_colnam",
    id_table_name = "id_table_colnam",
    db_connection = mydb,
    table_name = "table_colnam"
  )

  return(list(all_herbarium_individuals_not_linked_diff_tax = all_herbarium_individuals_not_linked_diff_tax,
              data_individuals_not_linked_diff_tax_concerned = data_individuals_concerned,
              all_herbarium_individuals_not_linked = all_herbarium_individuals_not_linked))
}








#' Split plot data into census
#'
#' split plot data into a list where each element is a census
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param meta tibble output of query_plot with no export individuals
#' @param dataset tibble output of query_plot with export individuals
#'
#' @importFrom rlang parse_expr
#'
#' @return A list with as many tibble as census
#' @export
.split_censuses <- function(meta, dataset) {

  split_census <- list()
  for (i in 1:nrow(meta)) {
    select_census <-
      dataset %>%
      dplyr::select(dplyr::contains(paste0("census_", i)),
                    dplyr::contains(paste0("date_census_", i)),
                    dplyr::contains(paste0("date_census_julian_", i)),
             id_n)

    stem_census <- paste0("stem_diameter_census_", i)
    stem_census_enquo <-
      rlang::parse_expr(rlang::quo_name(rlang::enquo(stem_census)))

    select_census <-
      select_census %>%
      dplyr::filter(!is.na(!!stem_census_enquo),
             !!stem_census_enquo>0)

    split_census[[i]] <- select_census
    names(split_census)[[i]] <- paste0("census_", meta$typevalue[i])
  }

  return(split_census)

}



#' Add time difference in number of days for two census
#'
#' Add
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param census1 tibble with first census
#' @param census2 tibble with second census
#'
#' @return tibble joining both census and with a added column indicating time intervall
#' @export
.time_diff <- function(census1, census2) {

  ## renaming first and second census to 1 and 2
  select_census_1 <-
    census1 %>%
    dplyr::select(dplyr::contains("date_census_julian")) %>%
    colnames() %>%
    strsplit(split = "_") %>%
    unlist()
  select_census_1 <- select_census_1[length(select_census_1)]

  census1 <-
    census1 %>%
    dplyr::rename_at(dplyr::vars(dplyr::ends_with(paste0("_", select_census_1))),
                          funs(stringr::str_replace(., paste0("_", select_census_1), "_1")))

  select_census_2 <-
    census2 %>%
    dplyr::select(dplyr::contains("date_census_julian")) %>%
    colnames() %>%
    strsplit(split = "_") %>%
    unlist()
  select_census_2 <- select_census_2[length(select_census_2)]

  census2 <-
    census2 %>%
    dplyr::rename_at(dplyr::vars(dplyr::ends_with(paste0("_", select_census_2))),
                          funs(stringr::str_replace(., paste0("_", select_census_2), "_2")))

  ## excluding NA stem diameter (new recruits and dead stems)
  joined_census <-
    dplyr::left_join(census1, census2, by=c("id_n"="id_n")) %>%
    dplyr::filter(!is.na(stem_diameter_census_1), !is.na(stem_diameter_census_2))

  joined_census <-
    joined_census %>%
    dplyr::mutate(time_diff =  (date_census_julian_2 - date_census_julian_1 )/365.25)

  joined_census
}



#' Identify potential errors for estimating growth
#'
#' Add a column of individuals to be excluded because of potential errors
#' Adapted from http://ctfs.si.edu/Public/CTFSRPackage/
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param censuses tibble with first census
#' @param slope numeric see http://ctfs.si.edu/Public/CTFSRPackage/index.php/web/topics/growth~slash~growth.r/trim.growth
#' @param intercept numeric see http://ctfs.si.edu/Public/CTFSRPackage/index.php/web/topics/growth~slash~growth.r/trim.growth
#' @param err.limit integer any measure of second diameter higher than err.limit standard deviation below the first measure will be excluded
#' @param maxgrow numeric any growth in mm/year higher than maxgrow will be excluded
#' @param mindbh numeric minimum diameter in mm for excluding measures
#'
#' @return tibble joining both census and with a added column indicating logical whether inidividual should be excluded
#' @export
.trim.growth <- function(censuses,
                         slope = 0.006214,
                         intercept = 0.9036,
                         err.limit = 4,
                         maxgrow = 75,
                         # pomcut = 0.05,
                         mindbh = 100
                         # dbhunit = "cm",
) {

  # if(dbhunit=='cm') intercept=intercept/10

  censuses <-
    censuses %>%
    dplyr::mutate(dbh_mm_census1 = stem_diameter_census_1*10) %>%
    dplyr::mutate(dbh_mm_census2 = stem_diameter_census_2*10)


  ## get the standard deviation of linear model
  stdev.dbh1 <- slope * censuses$dbh_mm_census1 + intercept
  growth <- (censuses$dbh_mm_census2- censuses$dbh_mm_census1) / censuses$time_diff
  bad.neggrow <- which(censuses$dbh_mm_census2 <= (censuses$dbh_mm_census1 - err.limit *
                                                     stdev.dbh1))

  bad.posgrow <- which(growth > maxgrow)
  # homdiff <- abs(as.numeric(cens2$hom) - as.numeric(cens1$hom)) / as.numeric(cens1$hom)

  accept <- rep(TRUE, nrow(censuses))
  # accept[homdiff > pomcut] <- FALSE
  accept[bad.neggrow] <- FALSE
  accept[bad.posgrow] <- FALSE
  accept[is.na(growth)] <- FALSE
  # if (exclude.stem.change) {
  #   accept[cens1$stemID != cens2$stemID] <- FALSE
  # }
  accept[censuses$dbh_mm_census1 < mindbh] <- FALSE

  accept[is.na(censuses$dbh_mm_census1) | is.na(censuses$dbh_mm_census2) | censuses$dbh_mm_census2 <= 0] <- FALSE

  censuses <-
    censuses %>%
    mutate(accepted_growth = accept)
  return(censuses)
}



#' Growth computing for multiple census
#'
#' Growth computing for multiple census
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param dataset tibble, ouput of query_plots
#' @param metadata tibble, ouput of query_plots
#' @param mindbh numeric see http://ctfs.si.edu/Public/CTFSRPackage/index.php/web/topics/growth~slash~growth.r/trim.growth
#' @param err.limit integer any measure of second diameter higher than err.limit standard deviation below the first measure will be excluded
#' @param maxgrow numeric any growth in mm/year higher than maxgrow will be excluded
#' @param method string either 'I' or 'E'
#' @param export_ind_growth whether growth per individuals should be exported
#'
#' @importFrom date as.date date.ddmmmyy
#'
#' @return tibble
#'
#' @details
#' growthrate is in
#'
#'
#' @export
growth_computing <- function(dataset,
                             metadata,
                             mindbh = NULL,
                             err.limit = 4, # any measure of second diameter higher than err.limit standard deviation below the first measure will be excluded
                             maxgrow = 75, # any growth (mm/year) higher than maxgrow will be excluded
                             method = "I",
                             export_ind_growth = TRUE) {

  if (!any(rownames(utils::installed.packages()) == "date")) {
    stop("date package needed, please install it")
  }


  if (!is.list(metadata)) {
    stop("metadata should be a list obtained with the function query_plot with show_multiple_census = T")
  }
  
  if (!is.list(dataset)) {
    stop("dataset should be a list obtained with the function query_plot with show_multiple_census = T and extract_individuals = T")
  }
  

  if (!any(metadata[[2]]$typevalue > 1))
    stop("Only one census recorded for all selected plots")

  if (!any(names(dataset$extract) == "id_table_liste_plots_n"))
    stop("id_table_liste_plots_n column missing in dataset, make sure you obtain dataset with remove_ids = F")

  all_ids_plot <-
    metadata[[2]] %>%
    dplyr::filter(typevalue >1) %>%
    dplyr::distinct(id_table_liste_plots) %>%
    dplyr::pull()

  plot_names <-
    metadata[[2]] %>%
    dplyr::filter(typevalue >1) %>%
    dplyr::distinct(id_table_liste_plots, plot_name) %>%
    dplyr::pull(plot_name)

  cli::cli_alert_info(paste("Multiple census recorded for", length(all_ids_plot), "plots"))

  full_results <-
    full_results_ind <-
    full_results_mortality <-
    vector('list', length = length(all_ids_plot)*10)

  for (plot in 1:length(all_ids_plot)) {

    cli::cli_alert_info(plot_names[plot])

    selected_dataset <-
      dataset$extract %>%
      dplyr::filter(id_table_liste_plots_n == all_ids_plot[plot])

    selected_metadata_census <-
      metadata[[2]] %>%
      dplyr::filter(id_table_liste_plots == all_ids_plot[plot])

    skipped_census_missing_dates <-
      selected_metadata_census %>%
      dplyr::filter(is.na(year) | is.na(month))

    not_run <- FALSE
    if (nrow(skipped_census_missing_dates) > 0) {
      message(paste("Census excluded because missing year and/or month"))
      print(skipped_census_missing_dates)
      not_run <- TRUE

    }

    if (length(unique(selected_metadata_census$year)) == 1 &
        length(unique(selected_metadata_census$month)) == 1 &
        length(unique(selected_metadata_census$day)) == 1) {

      cli::cli_alert_danger("Dates do not differ between censuses")
      not_run <- TRUE

    }

    selected_metadata_census <-
      selected_metadata_census %>%
      dplyr::arrange(typevalue) %>%
      dplyr::mutate(date =
                      paste(ifelse(!is.na(month),
                                   month, 1), # if day is missing, by default 1
                            ifelse(!is.na(day),
                                   day, 1), # if month is missing, by default 1
                            ifelse(!is.na(year),
                                   year, ""),
                            sep = "/")) %>%
      dplyr::mutate(date_julian = date::as.date(date))

    arranged_sub_plots <-
      selected_metadata_census %>%
      dplyr::arrange(typevalue) %>%
      dplyr::select(date_julian, id_sub_plots) %>%
      dplyr::arrange(date_julian) %>%
      dplyr::pull(id_sub_plots)

    if (!paste(arranged_sub_plots, collapse = "_") ==
        paste(selected_metadata_census$id_sub_plots, collapse = "_")) {
      message(paste("Dates are not in chronological order"))
      not_run <- TRUE
    }

    selected_metadata_census <-
      selected_metadata_census %>%
      dplyr::filter(!is.na(year) & !is.na(month))

    if (!not_run) {
      for (i in 1:(nrow(selected_metadata_census) - 1)) {

        splitted_census <-
          .split_censuses(meta = selected_metadata_census,
                          dataset = selected_dataset)


        ## renaming first and second census to 1 and 2
        select_census_1 <-
          splitted_census[[i]] %>%
          dplyr::select(dplyr::contains('stem_diameter_census')) %>%
          colnames() %>%
          strsplit(split = "_") %>%
          unlist()

        select_census_1 <-
          select_census_1[length(select_census_1)]

        # splitted_census[[i]] <-
        #   splitted_census[[i]] %>%
        #   dplyr::rename_at(dplyr::vars(dplyr::ends_with(paste0("_", select_census_1))),
        #                    funs(stringr::str_replace(., paste0("_", select_census_1), "_1")))

        splitted_census[[i]] <-
          splitted_census[[i]] %>%
          dplyr::rename_at(dplyr::vars(dplyr::ends_with(paste0("_", select_census_1))),
                           list(~ stringr::str_replace(., paste0("_", select_census_1), "_1")))


        select_census_2 <-
          splitted_census[[i+1]] %>%
          dplyr::select(dplyr::contains('stem_diameter_census')) %>%
          colnames() %>%
          strsplit(split = "_") %>%
          unlist()

        select_census_2 <- select_census_2[length(select_census_2)]

        # splitted_census[[i+1]] <-
        #   splitted_census[[i+1]] %>%
        #   dplyr::rename_at(dplyr::vars(dplyr::ends_with(paste0("_", select_census_2))),
        #                    funs(stringr::str_replace(., paste0("_", select_census_2), "_2")))

        splitted_census[[i+1]] <-
          splitted_census[[i+1]] %>%
          dplyr::rename_at(dplyr::vars(dplyr::ends_with(paste0("_", select_census_2))),
                           list(~ stringr::str_replace(., paste0("_", select_census_2), "_2")))


        ### detecting dead stems by filtering either 0 stem diameter or NA
        deads <-
          splitted_census[[i]] %>%
          dplyr::select(id_n, stem_diameter_census_1) %>%
          dplyr::left_join(
            splitted_census[[i + 1]] %>%
              dplyr::select(id_n, stem_diameter_census_2),
            by = c("id_n" = "id_n")
          ) %>%
          dplyr::filter(is.na(stem_diameter_census_2) |
                          stem_diameter_census_2 == 0)

        recruits <-
          splitted_census[[i + 1]] %>%
          dplyr::select(id_n, stem_diameter_census_2) %>%
          dplyr::left_join(
            splitted_census[[i]] %>%
              dplyr::select(id_n, stem_diameter_census_1),
            by = c("id_n" = "id_n")
          ) %>%
          dplyr::filter(is.na(stem_diameter_census_1) |
                          stem_diameter_census_1 == 0)

        censuses <- .time_diff(census1 = splitted_census[[i]], census2 = splitted_census[[i+1]])

        censuses <-
          .trim.growth(
            censuses = censuses,
            err.limit = err.limit,
            maxgrow = maxgrow,
            mindbh = mindbh
          )

        size1 <- censuses$dbh_mm_census1
        size2 <- censuses$dbh_mm_census2

        if (method == "I") {

          growthrate <- (size2 - size1) / censuses$time_diff

        } else if (method == "E") {

          growthrate <- (log(size2) - log(size1)) / censuses$time_diff

        }

        # setting NA to values considered to be problematic
        growthrate[!censuses$accepted_growth] <- NA

        censuses <-
          censuses %>%
          mutate(growthrate = growthrate)

        if (export_ind_growth) {

          ind_growth <-
            censuses %>%
            left_join(
              selected_dataset %>%
                dplyr::select(
                  id_n,
                  idtax_individual_f,
                  tax_sp_level,
                  tax_fam,
                  tax_gen,
                  tax_esp,
                  plot_name,
                  sous_plot_name,
                  ind_num_sous_plot,
                  id_table_liste_plots_n
                ),
              by = c("id_n" = "id_n")
            ) %>%
            dplyr::relocate(plot_name,
                            sous_plot_name,
                            ind_num_sous_plot,
                            tax_sp_level,
                            tax_fam,
                            tax_gen,
                            tax_esp,
                            .before = date_census_1)

          full_results_ind[[length(full_results_ind[unlist(lapply(full_results_ind, function(x) !is.null(x)))]) + 1]] <-
            ind_growth

        }


        ### growth computation

        ## number of stems at the outset (first census) excluding "exclude" stems
        N_outset <-
          splitted_census[[i]] %>%
          dplyr::left_join(censuses %>%
                      dplyr::select(id_n, growthrate) %>%
                      dplyr::filter(is.na(growthrate)) %>%
                      dplyr::mutate(growthrate = stringr::str_replace_na(growthrate, "no")) %>%
                      dplyr::rename(exclude = growthrate) %>%
                        distinct(),
                    by = c("id_n" = "id_n")) %>%
          dplyr::filter(is.na(exclude)) %>%
          nrow()

        N_survivor <-
          N_outset - nrow(deads)

        averaged_time_diff <-
          mean(censuses$time_diff)

        mortality_rate <-
          (log(N_outset)-log(N_survivor))/averaged_time_diff



        result <-
          list(
          plot_name = selected_metadata_census$plot_name[1],
          censuses = paste(names(splitted_census[i]), names(splitted_census[i+1]), collapse = "_", sep = "_"),
          growthrate = mean(growthrate, na.rm = TRUE),
          nbe_dead = nrow(deads),
          dbhmean_deads = ifelse(nrow(deads)>0, mean(deads$stem_diameter_census_1), NA),
          mortality_rate = mortality_rate,
          nbe_recruits = nrow(recruits),
          dbhmean_recruits = ifelse(nrow(recruits) > 0, mean(recruits$stem_diameter_census_2), NA),
          dbhmax_recruits = ifelse(nrow(recruits) > 0, max(recruits$stem_diameter_census_2), NA),
          N_survivor = N_survivor,
          N_outset = N_outset,
          N_excluded = length(growthrate[is.na(growthrate)]),
          sd_growthrate = sd(growthrate, na.rm = TRUE),
          dbhmean1 = mean(censuses$dbh_mm_census1[censuses$accepted_growth], na.rm = TRUE),
          dbhmean2 = mean(censuses$dbh_mm_census2[censuses$accepted_growth], na.rm = TRUE),
          nbe_days_intervall = mean(censuses$time_diff[censuses$accepted_growth], na.rm = TRUE)*365.25,
          date1 = date::date.ddmmmyy(mean(censuses$date_census_julian_1[censuses$accepted_growth],
                                          na.rm = TRUE)),
          date2 = date::date.ddmmmyy(mean(censuses$date_census_julian_2[censuses$accepted_growth],
                                          na.rm = TRUE))
        )

        full_results[[length(full_results[unlist(lapply(full_results, function(x) !is.null(x)))]) + 1]] <-
          result

        deads <-  deads %>%
          left_join(
            selected_dataset %>%
              dplyr::select(
                id_n,
                idtax_individual_f,
                tax_sp_level,
                tax_fam,
                tax_gen,
                tax_esp,
                plot_name,
                sous_plot_name,
                ind_num_sous_plot,
                id_table_liste_plots_n
              ),
            by = c("id_n" = "id_n")
          )


        full_results_mortality[[length(full_results_mortality[unlist(lapply(full_results_mortality, function(x) !is.null(x)))]) + 1]] <-
          deads

      }
    }else{

      cli::cli_alert_warning(paste(
        "No growth analysis for",
        selected_metadata_census$plot_name[1],
        "because dates are not conform"
      ))
    }
  }

  full_results <-
    full_results[unlist(lapply(full_results, function(x) !is.null(x)))]

  full_results_ind <-
    full_results_ind[unlist(lapply(full_results_ind, function(x) !is.null(x)))]

  full_results_mortality <-
    full_results_mortality[unlist(lapply(full_results_mortality, function(x) !is.null(x)))]

  if(!export_ind_growth)
    return(plot_results = bind_rows(lapply(full_results,
                                           FUN = function(x) bind_rows(x))),
           mortality = full_results_mortality)

  if(export_ind_growth)
    return(list(plot_results = bind_rows(lapply(full_results,
                                                FUN = function(x) bind_rows(x))),
                ind_results = full_results_ind,
                mortality = full_results_mortality))

}






#' Query exact match
#'
#' Extract from a sql database an exact match on a given field
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param tbl tibble with one field listing names to be searched
#' @param field string column name to be search
#' @param values_q string names to be searched
#' @param con PqConnection connection to RPostgres database
#'
#'
#' @return A list of two elements, one with the extract if any, two with the names with id not NA when matched
#' @export
query_exact_match <- function(tbl, field, values_q, con) {

  if (length(field) == 1) {

    field_col <- dplyr::sym(field)

    query_tb <- tibble(!!field_col := tolower(values_q))

  } else {

    query_tb <- tibble(species := tolower(values_q))

  }

  if (length(field) == 1) sql <- glue::glue_sql("SELECT * FROM {`tbl`} WHERE lower({`field`}) IN ({vals*})",
                                               vals = tolower(values_q), .con = con)
  if (length(field) > 1) sql <- glue::glue_sql("SELECT * FROM {`tbl`} WHERE lower(concat({`field[1]`},' ',{`field[2]`})) IN ({vals*})",
                                              vals = tolower(values_q), .con = con)

  res_q <- func_try_fetch(con = con, sql = sql)

  if (length(field) == 1) query_tb <- query_tb %>%
    left_join(res_q %>% dplyr::select(!!field_col) %>% mutate(!!field_col := tolower(!!field_col)) %>% distinct() %>%
                mutate(id = seq_len(nrow(.))))

  if (length(field) > 1) query_tb <- query_tb %>%
    left_join(res_q %>% dplyr::select(dplyr::all_of(field)) %>%
                mutate(species = paste(!!dplyr::sym(field[1]), !!dplyr::sym(field[2]), sep = " ")) %>%
                mutate(species = tolower(species)) %>%
                distinct() %>%
                mutate(id = seq_len(nrow(.))))

  return(list(res_q = res_q,
              query_tb = query_tb))

}

#' Query fuzzy match
#'
#' Extract from a sql database an exact match on a given field
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param tbl tibble with one field listing names to be searched
#' @param field string column name to be search
#' @param values_q string names to be searched
#' @param con PqConnection connection to RPostgres database
#'
#'
#' @return A list of two elements, one with the extract if any, two with the names with id not NA when matched
#' @export
query_fuzzy_match <- function(tbl, field, values_q, con) {

  # if (length(field) == 0) sql <-glue::glue_sql("SELECT * FROM {`tbl`} WHERE SIMILARITY (lower({`field`}), {values_q}) > {sim_thres} ;",
  #                      .con = con)

  if (length(field) == 1) sql <- glue::glue_sql("SELECT * FROM {`tbl`} ORDER BY SIMILARITY (lower({`field`}), {values_q}) DESC LIMIT 1;",
                                               .con = con)

  # if (length(field) > 0) sql <-glue::glue_sql("SELECT * FROM {`tbl`} WHERE SIMILARITY (lower(concat({`field[1]`},' ',{`field[2]`})), {values_q}) > {sim_thres} ;",
  #                                              .con = con)

  if (length(field) > 1)  sql <- glue::glue_sql("SELECT * FROM {`tbl`} ORDER BY SIMILARITY (lower(concat({`field[1]`},' ',{`field[2]`})), {values_q}) DESC LIMIT 5;",
                                                .con = con)

  res_q <- func_try_fetch(con = con, sql = sql)

  # rs <- DBI::dbSendQuery(con, sql)
  # res_q <-DBI::dbFetch(rs) %>% as_tibble
  # DBI::dbClearResult(rs)

  if (nrow(res_q) == 0) {

    cli::cli_alert_warning("Failed fuzzy match for {values_q[i]} in {field} field in {tbl}")

  }

  return(res_q)

}


#' List, extract taxa
#'
#' Provide list of selected taxa
#'
#' @return A tibble of all taxa
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param class character string of class
#' @param family string
#' @param genus string
#' @param order string
#' @param species string genus followed by species name separated by one space
#' @param tax_nam01 string
#' @param tax_nam02 string
#' @param only_genus logical
#' @param only_family logical
#' @param only_class logical
#' @param ids integer id of searched taxa
#' @param verbose logical
#' @param exact_match logical
#' @param check_synonymy logical
#' @param extract_traits logical
#'
#'
#' @return A tibble of plots or individuals if extract_individuals is TRUE
#' @export
query_taxa <-
  function(
    class = NULL, # c("Magnoliopsida", "Pinopsida", "Lycopsida", "Pteropsida")
    family = NULL,
    genus = NULL,
    order = NULL,
    species = NULL,
    only_genus = FALSE,
    only_family = FALSE,
    only_class = FALSE,
    ids = NULL,
    verbose = TRUE,
    exact_match = FALSE,
    check_synonymy =TRUE,
    extract_traits = TRUE
  ) {

    # if(!exists("mydb")) call.mydb()

    if(!exists("mydb_taxa")) call.mydb.taxa(pass = "Anyuser2022", user = "common")

    if(!is.null(class)) {

      res_q <- query_exact_match(
        tbl = "table_tax_famclass",
        field = "tax_famclass",
        values_q = class,
        con = mydb_taxa
      )

      res_class <-
        tbl(mydb_taxa, "table_taxa") %>%
        filter(id_tax_famclass %in% !!res_q$res_q$id_tax_famclass) %>%
        dplyr::select(idtax_n, idtax_good_n) %>%
        collect()

    }

    if(is.null(ids)) {

      if(!is.null(order)) {

        q_res <- query_exact_match(
          tbl = "table_taxa",
          field = "tax_order",
          values_q = order,
          con = mydb_taxa
        )

        if (!exact_match & any(is.na(q_res$query_tb$id))) {

          if (verbose) cli::cli_alert_info("Fuzzy search for order for {sum(is.na(q_res$query_tb$id))} name(s)")

          query_tb_miss <-
            q_res$query_tb %>%
            filter(is.na(id))

          fuz_list <- vector('list', nrow(query_tb_miss))
          for (i in 1:nrow(query_tb_miss)) {

            fuz_list[[i]] <- query_fuzzy_match(
              tbl = "table_taxa",
              field = "tax_order",
              values_q = query_tb_miss$tax_order[i],
              con = mydb_taxa
            )

          }

          fuz_res <- bind_rows(fuz_list) %>% distinct()

        }

        if (!exact_match & any(is.na(q_res$query_tb$id))) {
          res_order <- bind_rows(fuz_res, q_res$res_q)
        } else {
          res_order <- q_res$res_q
        }

      }

      if(!is.null(family)) {

        q_res <- query_exact_match(
          tbl = "table_taxa",
          field = "tax_fam",
          values_q = family,
          con = mydb_taxa
        )

        if (!exact_match & any(is.na(q_res$query_tb$id))) {

          if (verbose) cli::cli_alert_info("Fuzzy search for family for {sum(is.na(q_res$query_tb$id))} name(s)")

          query_tb_miss <-
            q_res$query_tb %>%
            filter(is.na(id))

          fuz_list <- vector('list', nrow(query_tb_miss))
          for (i in 1:nrow(query_tb_miss)) {

            fuz_list[[i]] <- query_fuzzy_match(
              tbl = "table_taxa",
              field = "tax_fam",
              values_q = query_tb_miss$tax_fam[i],
              con = mydb_taxa
            )

          }

          fuz_res <- bind_rows(fuz_list) %>% distinct()

        }

        if (!exact_match & any(is.na(q_res$query_tb$id))) {
          res_family <- bind_rows(fuz_res, q_res$res_q)
        } else {
          res_family <- q_res$res_q
        }

      }

      if(!is.null(genus)) {

        q_res <- query_exact_match(
          tbl = "table_taxa",
          field = "tax_gen",
          values_q = genus,
          con = mydb_taxa
        )

        if (!exact_match & any(is.na(q_res$query_tb$id))) {

          if (verbose) cli::cli_alert_info("Fuzzy search for genus for {sum(is.na(q_res$query_tb$id))} name(s)")

          query_tb_miss <-
            q_res$query_tb %>%
            filter(is.na(id))

          fuz_list <- vector('list', nrow(query_tb_miss))
          for (i in 1:nrow(query_tb_miss)) {

            fuz_list[[i]] <- query_fuzzy_match(
              tbl = "table_taxa",
              field = "tax_gen",
              values_q = query_tb_miss$tax_gen[i],
              con = mydb_taxa
            )

          }

          fuz_res <- bind_rows(fuz_list) %>% distinct()

        }

        if (!exact_match & any(is.na(q_res$query_tb$id))) {
          res_genus <- bind_rows(fuz_res, q_res$res_q)
        } else {
          res_genus <- q_res$res_q
        }

      }

      if(!is.null(species)) {

        q_res <- query_exact_match(
          tbl = "table_taxa",
          field = c("tax_gen", "tax_esp"),
          values_q = species,
          con = mydb_taxa
        )

        if (!exact_match & any(is.na(q_res$query_tb$id))) {

          if (verbose) cli::cli_alert_info("Fuzzy search for species for {sum(is.na(q_res$query_tb$id))} name(s)")

          query_tb_miss <-
            q_res$query_tb %>%
            filter(is.na(id))

          fuz_list <- vector('list', nrow(query_tb_miss))
          for (i in 1:nrow(query_tb_miss)) {

            fuz_list[[i]] <- query_fuzzy_match(
              tbl = "table_taxa",
              field = c("tax_gen", "tax_esp"),
              values_q = query_tb_miss$species[i],
              con = mydb_taxa
            )

          }

          fuz_res <- bind_rows(fuz_list) %>% distinct()

        }

        if (!exact_match & any(is.na(q_res$query_tb$id))) {
          res_species <- bind_rows(fuz_res, q_res$res_q)
        } else {
          res_species <- q_res$res_q
        }

      }

      no_match <- FALSE
      res <-
        tbl(mydb_taxa, "table_taxa")

      if(!is.null(class))
        res <- res %>%
        filter(idtax_n %in% !!res_class$idtax_n)

      if (!is.null(order))
        if (nrow(res_order) > 0) {
          res <-
            res %>%
            filter(idtax_n %in% !!res_order$idtax_n)

        } else {

          if (verbose) cli::cli_alert_danger("no match for order")
          no_match <- TRUE
        }

      if(!is.null(family))
        if(nrow(res_family)>0) {
          res <-
            res %>%
            filter(idtax_n %in% !!res_family$idtax_n)
        }else{
          if (verbose) cli::cli_alert_danger("no match for family")
          no_match <- TRUE
        }

      if(!is.null(genus))
        if(nrow(res_genus)>0) {
          res <-
            res %>%
            filter(idtax_n %in% !!res_genus$idtax_n)
        }else{
          if (verbose) cli::cli_alert_danger("no match for genus")
          no_match <- TRUE
        }

      if (!is.null(species))
        if (nrow(res_species) > 0) {
          res <-
            res %>%
            filter(idtax_n %in% !!res_species$idtax_n)
        } else{
          if (verbose) cli::cli_alert_danger("no match for species")
          no_match <- TRUE
        }

      if(!no_match) {
        res <- res %>% collect()
      } else {
        res <- NULL
        if (verbose) cli::cli_alert_danger("no matching names")
      }

    } else {

      if(!is.null(class)) {

        ids <-
          ids[ids %in% res_class$idtax_n]

        if (length(ids) == 0) {

          stop("id provided not found in the class queried")

        }

      }

      tbl <- "table_taxa"
      sql <-glue::glue_sql("SELECT * FROM {`tbl`} WHERE idtax_n IN ({vals*})",
                           vals = ids, .con = mydb_taxa)

      res <- func_try_fetch(con = mydb_taxa, sql = sql)

      # rs <- DBI::dbSendQuery(mydb_taxa, sql)
      # res <- DBI::dbFetch(rs)
      # DBI::dbClearResult(rs)
      # res <- dplyr::as_tibble(res)

    }

    if (only_genus)
      res <-
        res %>%
        dplyr::filter(is.na(tax_esp))

    if (only_family)
      res <-
        res %>%
        dplyr::filter(is.na(tax_esp),
                      is.na(tax_gen))

    if (only_class)
      res <-
        res %>%
        dplyr::filter(is.na(tax_esp),
                      is.na(tax_gen),
                      is.na(tax_order),
                      is.na(tax_fam))

    ## checking synonymies
    if(!is.null(res) & check_synonymy) {

      ## if selected taxa are synonyms
      if(any(!is.na(res$idtax_good_n))) {

        if (any(res$idtax_good_n > 1)) {

          if (verbose) {

            cli::cli_alert_info("{sum(res$idtax_good_n > 1, na.rm = TRUE)} taxa selected is/are synonym(s)")
            cli::cli_alert_info("{nrow(res)} taxa selected before checking synonymies")

          }

          ## retrieving good idtax_n if selected ones are considered synonyms
          idtax_accepted <-
            res %>%
            dplyr::select(idtax_n, idtax_good_n) %>%
            dplyr::mutate(idtax_f = ifelse(!is.na(idtax_good_n),
                                           idtax_good_n, idtax_n)) %>%
            dplyr::distinct(idtax_f) %>%
            dplyr::rename(idtax_n = idtax_f)

          idtax_already_extracted <-
            res %>%
            filter(idtax_n %in% idtax_accepted$idtax_n)

          idtax_missing <- idtax_accepted %>%
            filter(!idtax_n %in% idtax_already_extracted$idtax_n)

          res <-
            tbl(mydb_taxa, "table_taxa") %>%
            dplyr::filter(idtax_n %in% !!idtax_missing$idtax_n) %>%
            collect() %>%
            bind_rows(idtax_already_extracted)

          if (verbose) cli::cli_alert_info("{nrow(res)} selected taxa after checking synonymies")

        }
      }

      ## retrieving all synonyms from selected taxa
      id_synonyms <-
        tbl(mydb_taxa, "table_taxa") %>%
        filter(idtax_good_n %in% !!res$idtax_n) %>% ## all taxa synonyms of selected taxa
        # filter(idtax_n %in% !!res$idtax_n) %>% ## excluding taxa already in extract
        dplyr::select(idtax_n, idtax_good_n) %>%
        collect()

      if(nrow(id_synonyms) > 0) {

        if(verbose) {
          cli::cli_alert_info("{sum(id_synonyms$idtax_good_n > 0, na.rm = TRUE)} taxa selected have synonym(s)")
          cli::cli_alert_info("{nrow(res)} taxa selected before checking synonymies")
        }

        synonyms <- query_taxa(ids = id_synonyms$idtax_n,
                               check_synonymy = FALSE,
                               verbose = FALSE,
                               class = NULL,
                               extract_traits = FALSE)

        res <-
          res %>%
          bind_rows(synonyms)

      }
    }

    if(!is.null(res)) {
      res <-
        res %>%
        mutate(tax_sp_level = ifelse(!is.na(tax_esp), paste(tax_gen, tax_esp), NA)) %>%
        mutate(tax_infra_level = ifelse(!is.na(tax_esp),
                                        paste0(tax_gen,
                                               " ",
                                               tax_esp,
                                               ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
                                               ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
                                               ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
                                               ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), "")),
                                        NA)) %>%
        mutate(tax_infra_level_auth = ifelse(!is.na(tax_esp),
                                             paste0(tax_gen,
                                                    " ",
                                                    tax_esp,
                                                    ifelse(!is.na(author1), paste0(" ", author1), ""),
                                                    ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
                                                    ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
                                                    ifelse(!is.na(author2), paste0(" ", author2), ""),
                                                    ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
                                                    ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), ""),
                                                    ifelse(!is.na(author3), paste0(" ", author3), "")),
                                             NA)) %>%
        dplyr::mutate(introduced_status = stringr::str_trim(introduced_status)) %>%
        dplyr::mutate(tax_sp_level = as.character(tax_sp_level),
                      tax_infra_level = as.character(tax_infra_level),
                      tax_infra_level_auth = as.character(tax_infra_level_auth)) %>%
        dplyr::select(-tax_famclass) %>%
        left_join(dplyr::tbl(mydb_taxa, "table_tax_famclass") %>%
                    dplyr::collect(),
                  by = c("id_tax_famclass" = "id_tax_famclass")) %>%
        dplyr::relocate(tax_famclass, .after = tax_order) %>%
        dplyr::relocate(year_description, .after = citation) %>%
        dplyr::relocate(data_modif_d, .after = morpho_species) %>%
        dplyr::relocate(data_modif_m, .after = morpho_species) %>%
        dplyr::relocate(data_modif_y, .after = morpho_species) %>%
        dplyr::relocate(tax_sp_level, .before = idtax_n) %>%
        dplyr::relocate(id_tax_famclass, .after = morpho_species)

      if(extract_traits) {

        traitsqueried <-
          query_traits_measures(idtax = res$idtax_n, idtax_good = res$idtax_good_n)

        if (length(traitsqueried$traits_idtax_num) > 1)
          res <-
            res %>%
            left_join(traitsqueried$traits_idtax_num,
                      by = c("idtax_n" = "idtax"))

        if (length(traitsqueried$traits_idtax_char) > 1)
          res <-
            res %>%
            left_join(traitsqueried$traits_idtax_char,
                      by = c("idtax_n" = "idtax"))

        # if (any(class(traitsqueried$traits_idtax_num) == "data.frame"))
        #   res <-
        #     res %>%
        #     left_join(traitsqueried$traits_idtax_num,
        #               by = c("idtax_n" = "idtax"))
        #
        # if (any(class(traitsqueried$traits_idtax_char) == "data.frame"))
        #   res <-
        #     res %>%
        #     left_join(traitsqueried$traits_idtax_char,
        #               by = c("idtax_n" = "idtax"))

      }



    }

    if (!is.null(res)) {
      if (any(names(res) == "a_habit"))
        res <-
          res %>%
          dplyr::select(-a_habit,-a_habit_secondary)

      if (any(names(res) == "fktax"))
        res <-
          res %>%
          dplyr::select(-fktax)

      if (any(names(res) == "id_good"))
        res <-
          res %>%
          dplyr::select(-id_good)

      if (any(names(res) == "tax_tax"))
        res <-
          res %>%
          dplyr::select(-tax_tax)

    }

    if(!is.null(res)) {
      if (verbose & nrow(res) < 50) {

        res_print <-
          res %>%
          dplyr::relocate(tax_infra_level_auth, .before = tax_order) %>%
          dplyr::relocate(idtax_n, .before = tax_order) %>%
          dplyr::relocate(idtax_good_n, .before = tax_order)

        print_table(res_print)

        # res_print <-
        #   res_print %>%
        #   mutate(across(where(is.character), ~ tidyr::replace_na(., "")))
        #
        # res_print <- suppressMessages(as_tibble(cbind(columns = names(res_print), record = t(res_print)),
        #                                         .name_repair = "universal"))
        #
        # res_print %>%
        #   kableExtra::kable(format = "html", escape = F) %>%
        #   kableExtra::kable_styling("striped", full_width = F) %>%
        #   print()

      }

      if(verbose & nrow(res) >= 20)
        cli::cli_alert_info("Not showing html table because too many taxa")
    }

    if(!is.null(res)) return(res)
  }



# query_taxa <-
#   function(
#     class = c("Magnoliopsida", "Pinopsida", "Lycopsida", "Pteropsida"),
#     family = NULL,
#     genus = NULL,
#     order = NULL,
#     species = NULL,
#     tax_nam01 = NULL,
#     tax_nam02 = NULL,
#     only_genus = FALSE,
#     only_family = FALSE,
#     only_class = FALSE,
#     ids = NULL,
#     verbose = TRUE,
#     exact_match = FALSE,
#     check_synonymy =TRUE,
#     extract_traits = TRUE
#   ) {
#
#     if(!exists("mydb")) call.mydb()
#
#     if(!is.null(class)) {
#
#       # if(!exact_match) {
#       #   all_res <- vector('list', length(class))
#       #   for (i in 1:length(class)) {
#       #
#       #     rs <- DBI::dbSendQuery(mydb, paste0("SELECT * FROM table_tax_famclass WHERE tax_famclass ILIKE '%",
#       #                                         class[i], "%'"))
#       #     res <- DBI::dbFetch(rs)
#       #     DBI::dbClearResult(rs)
#       #     all_res[[i]] <-
#
#       #     res_class <- bind_rows(all_res)
#       #   }
#       # }
#       # if(exact_match)
#         # rs <- DBI::dbSendQuery(mydb, paste0("SELECT * FROM table_tax_famclass WHERE tax_famclass ='",
#         #                                         class[i], "'"))
#
#           tbl <- "table_tax_famclass"
#
#           sql <-glue::glue_sql("SELECT * FROM {`tbl`} WHERE tax_famclass IN ({vals*})",
#                    vals = class, .con = mydb)
#           rs <- DBI::dbSendQuery(mydb, sql)
#           res_class <-DBI::dbFetch(rs)
#           DBI::dbClearResult(rs)
#
#           res_class <-
#             tbl(mydb, "table_taxa") %>%
#             filter(id_tax_famclass %in% !!res_class$id_tax_famclass) %>%
#             dplyr::select(idtax_n, idtax_good_n) %>%
#             collect()
#
#           # test <- tbl(mydb, "table_taxa") %>%
#           #   dplyr::select(idtax_n, idtax_good_n, id_tax_famclass) %>%
#           #   collect()
#
#         }
#
#     if(is.null(ids)) {
#
#       if(!is.null(order)) {
#
#
#         query_tb <- tibble(tax_order = order)
#
#         tbl <- "table_taxa"
#         field <- "tax_order"
#         values_que <- order
#         con_mydb <- mydb
#
#         sql <-glue::glue_sql("SELECT * FROM {`tbl`} WHERE lower({`field`}) IN ({vals*})",
#                              vals = tolower(values_que), .con = con_mydb)
#         rs <- DBI::dbSendQuery(con_mydb, sql)
#         res_order <-DBI::dbFetch(rs) %>% as_tibble
#         DBI::dbClearResult(rs)
#
#         query_tb <- query_tb %>%
#           left_join(res_order %>% dplyr::select(tax_order) %>% distinct() %>%
#                       mutate(id = seq_len(nrow(.))))
#
#         if (!exact_match & any(is.na(query_tb$id))) {
#           query_tb_miss <-
#             query_tb %>%
#             filter(is.na(id))
#
#           for (i in 1:nrow(query_tb_miss)) {
#
#             tbl <- "table_taxa"
#             field <- "tax_order"
#             values_que <- query_tb_miss$tax_order[i]
#             con_mydb <- mydb
#
#             sql <-glue::glue_sql("SELECT * FROM {`tbl`} WHERE SIMILARITY (lower({`field`}), {values_que}) > 0.5 ;", .con = con_mydb)
#             rs <- DBI::dbSendQuery(con_mydb, sql)
#             res_order <-DBI::dbFetch(rs) %>% as_tibble
#             DBI::dbClearResult(rs)
#
#
#
#           }
#
#
#         }
#
#         all_res <- list()
#         for (i in 1:length(order)) {
#           if(!exact_match)
#             rs <- DBI::dbSendQuery(mydb, paste0("SELECT * FROM table_taxa WHERE tax_order ILIKE '%",
#                                                 order[i], "%'"))
#           if(exact_match)
#             rs <- DBI::dbSendQuery(mydb, paste0("SELECT * FROM table_taxa WHERE tax_order ='",
#                                                 order[i], "'"))
#
#           res <- DBI::dbFetch(rs)
#           DBI::dbClearResult(rs)
#           all_res[[i]] <- dplyr::as_tibble(res) %>%
#             dplyr::select(idtax_n, id_good)
#         }
#         res_order <- bind_rows(all_res)
#       }
#
#       if(!is.null(family)) {
#         all_res <- list()
#         for (i in 1:length(family)) {
#           if(!exact_match)
#             rs <- DBI::dbSendQuery(mydb, paste0("SELECT * FROM table_taxa WHERE tax_fam ILIKE '%",
#                                                 family[i], "%'"))
#           if(exact_match)
#             rs <- DBI::dbSendQuery(mydb, paste0("SELECT * FROM table_taxa WHERE tax_fam ='",
#                                                 family[i], "'"))
#
#           res <- DBI::dbFetch(rs)
#           DBI::dbClearResult(rs)
#           all_res[[i]] <- dplyr::as_tibble(res) %>%
#             dplyr::select(idtax_n, id_good)
#         }
#         res_family <- bind_rows(all_res)
#       }
#
#       if(!is.null(genus)) {
#
#         if (!exact_match) {
#
#           all_res <- list()
#           for (i in 1:length(genus)) {
#
#             rs <- DBI::dbSendQuery(mydb, paste0("SELECT * FROM table_taxa WHERE tax_gen ILIKE '%",
#                                                 genus[i], "%'"))
#             res <- DBI::dbFetch(rs)
#             DBI::dbClearResult(rs)
#             all_res[[i]]  <- dplyr::as_tibble(res) %>%
#               dplyr::select(idtax_n, id_good)
#
#           }
#           res_genus <- bind_rows(all_res)
#
#         }
#
#         if (exact_match) {
#           query <- "SELECT * FROM table_taxa WHERE MMM"
#           query <-
#             gsub(
#               pattern = "MMM",
#               replacement = paste0("tax_gen IN ('",
#                                    paste(unique(genus), collapse = "', '"), "')"),
#               x = query
#             )
#
#           rs <- DBI::dbSendQuery(mydb, query)
#           res_genus <- DBI::dbFetch(rs)
#           DBI::dbClearResult(rs)
#           res_genus <- dplyr::as_tibble(res_genus)
#
#         }
#             # rs <- DBI::dbSendQuery(mydb, paste0("SELECT * FROM table_taxa WHERE tax_gen ='",
#             #                                     genus[i], "'"))
#
#
#       }
#
#       if(!is.null(species)) {
#         all_res <- list()
#         for (i in 1:length(species)) {
#           if(!exact_match)
#             rs <- DBI::dbSendQuery(mydb, paste0("SELECT * FROM table_taxa WHERE tax_esp ILIKE '%",
#                                                 species[i], "%'"))
#           if(exact_match)
#             rs <- DBI::dbSendQuery(mydb, paste0("SELECT * FROM table_taxa WHERE tax_esp ='",
#                                                 species[i], "'"))
#
#           res <- DBI::dbFetch(rs)
#           DBI::dbClearResult(rs)
#           all_res[[i]]  <- dplyr::as_tibble(res) %>%
#             dplyr::select(idtax_n, id_good)
#         }
#         res_species <- bind_rows(all_res)
#       }
#
#       # if(!is.null(habit)) {
#       #   all_res <- list()
#       #   for (i in 1:length(habit)) {
#       #     rs <- DBI::dbSendQuery(mydb, paste0("SELECT * FROM table_taxa WHERE a_habit ILIKE '%",
#       #                                         habit[i], "%'"))
#       #     res <- DBI::dbFetch(rs)
#       #     DBI::dbClearResult(rs)
#       #     all_res[[i]]  <- dplyr::as_tibble(res) %>%
#       #       dplyr::select(idtax_n, id_good)
#       #   }
#       #   res_habit <- bind_rows(all_res)
#       # }
#
#       no_match <- FALSE
#       res <-
#         tbl(mydb, "table_taxa")
#
#       if(!is.null(class))
#         res <- res %>%
#         filter(idtax_n %in% !!res_class$idtax_n)
#
#       if (!is.null(order))
#         if (nrow(res_order) > 0) {
#           res <-
#             res %>%
#             filter(idtax_n %in% !!res_order$idtax_n)
#         } else{
#           message("\n no match for order")
#           no_match <- TRUE
#         }
#
#       if(!is.null(family))
#         if(nrow(res_family)>0) {
#           res <-
#             res %>%
#             filter(idtax_n %in% !!res_family$idtax_n)
#         }else{
#           message("\n no match for family")
#           no_match <- TRUE
#         }
#
#       if(!is.null(genus))
#         if(nrow(res_genus)>0) {
#           res <-
#             res %>%
#             filter(idtax_n %in% !!res_genus$idtax_n)
#         }else{
#           message("\n no match for genus")
#           no_match <- TRUE
#         }
#
#       if (!is.null(species))
#         if (nrow(res_species) > 0) {
#           res <-
#             res %>%
#             filter(idtax_n %in% !!res_species$idtax_n)
#         } else{
#           message("\n no match for species")
#           no_match <- TRUE
#         }
#
#       # if (!is.null(habit))
#       #   if (nrow(res_habit) > 0) {
#       #     res <-
#       #       res %>%
#       #       filter(idtax_n %in% !!res_habit$idtax_n)
#       #   } else{
#       #     message("\n no match for habit")
#       #     no_match <- TRUE
#       #   }
#
#       if(!no_match) {
#         res <- res %>% collect()
#         # query <- gsub(pattern = "AND MMM", replacement = "", query)
#         # rs <- DBI::dbSendQuery(mydb, query)
#         # res <- DBI::dbFetch(rs)
#         # DBI::dbClearResult(rs)
#         # res <- dplyr::as_tibble(res)
#       } else {
#         res <- NULL
#         message("no matching names")
#       }
#
#     } else {
#
#       if(!is.null(class)) {
#
#         # message("\n Filtering by class.")
#
#         ids <-
#           ids[ids %in% res_class$idtax_n]
#
#         if (length(ids) == 0) {
#
#           stop("id provided not found in the class queried")
#
#         }
#
#       }
#
#       query <- "SELECT * FROM table_taxa WHERE MMM"
#       query <-
#         gsub(
#           pattern = "MMM",
#           replacement = paste0("idtax_n IN ('",
#                                paste(unique(ids), collapse = "', '"), "')"),
#           x = query
#         )
#
#       rs <- DBI::dbSendQuery(mydb, query)
#       res <- DBI::dbFetch(rs)
#       DBI::dbClearResult(rs)
#       res <- dplyr::as_tibble(res)
#
#     }
#
#     if(only_genus) {
#
#       res <-
#         res %>%
#         dplyr::filter(is.na(tax_esp))
#
#     }
#
#     if(only_family) {
#
#       res <-
#         res %>%
#         dplyr::filter(is.na(tax_esp),
#                       is.na(tax_gen))
#
#     }
#
#     if(only_class) {
#
#       res <-
#         res %>%
#         dplyr::filter(is.na(tax_esp),
#                       is.na(tax_gen),
#                       is.na(tax_order),
#                       is.na(tax_fam))
#
#     }
#
#     ## checking synonymies
#     if(!is.null(res) & check_synonymy) {
#
#       ## if selected taxa are synonyms
#       if(any(!is.na(res$idtax_good_n))) {
#
#         if (any(res$idtax_good_n > 1)) {
#
#           if (verbose) {
#
#             cli::cli_alert_info("{sum(res$idtax_good_n > 1, na.rm = TRUE)} taxa selected is/are synonym(s)")
#
#             cli::cli_alert_info("{nrow(res)} taxa selected before checking synonymies")
#
#           }
#
#           ## retrieving good idtax_n if selected ones are considered synonyms
#           idtax_accepted <-
#             res %>%
#             dplyr::select(idtax_n, idtax_good_n) %>%
#             dplyr::mutate(idtax_f = ifelse(!is.na(idtax_good_n),
#                                            idtax_good_n, idtax_n)) %>%
#             dplyr::distinct(idtax_f) %>%
#             dplyr::rename(idtax_n = idtax_f)
#
#           res <-
#             tbl(mydb, "table_taxa") %>%
#             dplyr::filter(idtax_n %in% !!idtax_accepted$idtax_n) %>%
#             collect()
#
#           # %>%
#           #   dplyr::select(tax_gen, idtax_n)
#
#           if (verbose) cli::cli_alert_info("{nrow(res)} taxa selected after checking synonymies")
#
#         }
#       }
#
#
#       ## retrieving all synonyms from selected taxa
#       id_synonyms <-
#         tbl(mydb, "table_taxa") %>%
#         filter(idtax_good_n %in% !!res$idtax_n) %>% ## all taxa synonyms of selected taxa
#         # filter(idtax_n %in% !!res$idtax_n) %>% ## excluding taxa already in extract
#         dplyr::select(idtax_n, idtax_good_n) %>%
#         collect()
#
#       if(nrow(id_synonyms) > 0) {
#
#         if(verbose) {
#           cli::cli_alert_info("{sum(id_synonyms$idtax_good_n > 0, na.rm = TRUE)} taxa selected have synonym(s)")
#           cli::cli_alert_info("{nrow(res)} taxa selected before checking synonymies")
#         }
#
#         synonyms <- query_taxa(ids = id_synonyms$idtax_n,
#                                check_synonymy = FALSE,
#                                verbose = FALSE,
#                                class = NULL,
#                                extract_traits = FALSE)
#
#         res <-
#           res %>%
#           bind_rows(synonyms)
#
#       }
#     }
#
#     if(!is.null(res)) {
#
#       res <-
#         res %>%
#         mutate(tax_sp_level = ifelse(!is.na(tax_esp), paste(tax_gen, tax_esp), NA)) %>%
#         mutate(tax_infra_level = ifelse(!is.na(tax_esp),
#                                         paste0(tax_gen,
#                                                " ",
#                                                tax_esp,
#                                                ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
#                                                ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
#                                                ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
#                                                ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), "")),
#                                         NA)) %>%
#         mutate(tax_infra_level_auth = ifelse(!is.na(tax_esp),
#                                              paste0(tax_gen,
#                                                     " ",
#                                                     tax_esp,
#                                                     ifelse(!is.na(author1), paste0(" ", author1), ""),
#                                                     ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
#                                                     ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
#                                                     ifelse(!is.na(author2), paste0(" ", author2), ""),
#                                                     ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
#                                                     ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), ""),
#                                                     ifelse(!is.na(author3), paste0(" ", author3), "")),
#                                              NA)) %>%
#         dplyr::mutate(introduced_status = stringr::str_trim(introduced_status)) %>%
#         dplyr::mutate(tax_sp_level = as.character(tax_sp_level),
#                       tax_infra_level = as.character(tax_infra_level),
#                       tax_infra_level_auth = as.character(tax_infra_level_auth)) %>%
#         dplyr::select(-tax_famclass) %>%
#         left_join(dplyr::tbl(mydb, "table_tax_famclass") %>%
#                     dplyr::collect(),
#                   by = c("id_tax_famclass" = "id_tax_famclass")) %>%
#         dplyr::relocate(tax_famclass, .after = tax_order)
#
#
#       if (extract_traits) {
#
#         traitsqueried <-
#           query_traits_measures(idtax = res$idtax_n, idtax_good = res$idtax_good_n)
#
#         if (any(class(traitsqueried$traits_idtax_num) == "data.frame"))
#           res <-
#             res %>%
#             left_join(traitsqueried$traits_idtax_num,
#                       by = c("idtax_n" = "idtax"))
#
#         if (any(class(traitsqueried$traits_idtax_char) == "data.frame"))
#           res <-
#             res %>%
#             left_join(traitsqueried$traits_idtax_char,
#                       by = c("idtax_n" = "idtax"))
#
#
#
#
#
#       }
#
#     }
#
#     if (!is.null(res)) {
#       if (any(names(res) == "a_habit"))
#         res <-
#           res %>%
#           dplyr::select(-a_habit,-a_habit_secondary)
#
#       if (any(names(res) == "fktax"))
#         res <-
#           res %>%
#           dplyr::select(-fktax)
#
#       if (any(names(res) == "id_good"))
#         res <-
#           res %>%
#           dplyr::select(-id_good)
#
#       if (any(names(res) == "tax_tax"))
#         res <-
#           res %>%
#           dplyr::select(-tax_tax)
#
#
#     }
#
#
#     if(!is.null(res)) {
#       if (verbose & nrow(res) < 50) {
#
#         res_print <-
#           res %>%
#           # dplyr::select(-fktax,-id_good,-tax_tax) %>%
#           dplyr::relocate(tax_infra_level_auth, .before = tax_order) %>%
#           dplyr::relocate(idtax_n, .before = tax_order) %>%
#           dplyr::relocate(idtax_good_n, .before = tax_order)
#
#         res_print <-
#           res_print %>%
#           mutate_all(~ as.character(.)) %>%
#           mutate_all(~ tidyr::replace_na(., ""))
#
#         as_tibble(cbind(columns = names(res_print), record = t(res_print))) %>%
#           kableExtra::kable(format = "html", escape = F) %>%
#           kableExtra::kable_styling("striped", full_width = F) %>%
#           print()
#
#       }
#
#
#       if(verbose & nrow(res) >= 50)
#         message("\n Not showing html table because too many taxa")
#     }
#
#     if(!is.null(res)) return(res)
#   }

#' Merge individuals and taxonomic backbone
#'
#' Merge idividuals and taxonomic backbone
#'#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id_individual numeric id of individuals to be extracted if any
#' @param id_plot numeric id of plots to be extracted if any
#'
#'
#' @return A src object with merged taxa
#' @export
merge_individuals_taxa <- function(id_individual = NULL,
                                   id_plot = NULL,
                                   id_tax = NULL) {

  specimens <- try_open_postgres_table(table = "specimens", con = mydb)

  # getting taxo identification from specimens
  specimens_id_diconame <-
    specimens %>%
    dplyr::select(id_specimen,
                  idtax_n,
                  id_colnam,
                  colnbr,
                  suffix,
                  id_tropicos,
                  id_brlu)

  # getting all ids from diconames

  diconames_id <-
    dplyr::tbl(mydb, "table_idtax") %>%
    dplyr::select(idtax_n, idtax_good_n) %>%
    dplyr::mutate(idtax_f = ifelse(is.na(idtax_good_n), idtax_n, idtax_good_n))


  # getting correct id_diconame considering synonymies
  specimens_linked <-
    specimens_id_diconame %>%
    dplyr::left_join(diconames_id, by = c("idtax_n" = "idtax_n")) %>%
    dplyr::rename(idtax_specimen_f = idtax_f) %>%
    dplyr::left_join(tbl(mydb, "table_colnam") %>%
                       dplyr::select(id_table_colnam, colnam),
                     by = c("id_colnam" = "id_table_colnam"))

  if (!is.null(id_individual)) {

    res_individuals_full <-
      dplyr::tbl(mydb, "data_individuals") %>%
      dplyr::filter(id_n %in% id_individual)

  } else {

    res_individuals_full <-
      dplyr::tbl(mydb, "data_individuals")

  }

  ### getting links to specimens -
  ## TO BE e_dED POTENTIALLY IF MORE THAN ONE SPECIMEN IS LINKED TO ONE INDIVIDUAL
  ## CODE TO EXTRACT THE "BEST" IDENTIFICATION IF DIFFERENT TO BE IMPLEMENETED
  links_specimens <-
    dplyr::tbl(mydb, "data_link_specimens") %>%
    dplyr::select(id_n, id_specimen) %>%
    dplyr::group_by(id_n) %>%
    dplyr::summarise(id_specimen = max(id_specimen, na.rm = T))

  res_individuals_full <-
    res_individuals_full %>%
    dplyr::select(-id_specimen) %>%
    dplyr::left_join(links_specimens,
                     by=c("id_n"="id_n"))

  if (!is.null(id_plot))
    res_individuals_full <-
    res_individuals_full %>%
    dplyr::filter(id_table_liste_plots_n %in% !!id_plot)   #filtering for selected plots

  res_individuals_full <-
    res_individuals_full %>%
    dplyr::left_join(diconames_id %>%
                       dplyr::select(idtax_n, idtax_f),
                     # dplyr::rename(id_diconame_good_n = idtax_good_n),
                     by = c("idtax_n" = "idtax_n")) %>%
    dplyr::left_join(
      specimens_linked %>%
        dplyr::select(
          id_specimen,
          idtax_specimen_f,
          colnam,
          colnbr,
          suffix
        ),
      by = c("id_specimen" = "id_specimen")
    ) %>% # adding id_diconame for specimens
    dplyr::mutate(idtax_individual_f = ifelse(!is.na(idtax_specimen_f),
                                              idtax_specimen_f,
                                              idtax_f))

  taxa_extract <- add_taxa_table_taxa(ids = res_individuals_full %>% pull(idtax_individual_f))
  taxa_extract <- taxa_extract %>% collect()

  res_individuals_full <- res_individuals_full %>% collect()

  res_individuals_full <-
    res_individuals_full %>% #### selecting id_dico_name from specimens if any
    dplyr::left_join(
      taxa_extract %>%
        dplyr::select(-data_modif_d,-data_modif_m,-data_modif_y),
      by = c("idtax_individual_f" = "idtax_n")
    )

  if (!is.null(id_tax))
    res_individuals_full <-
    res_individuals_full %>%
    filter(idtax_individual_f %in% id_tax)

  return(res_individuals_full)

}

#' Query for traits
#'
#' Query for traits for given taxa, include synonyms or not
#'
#' @return vector
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param idtax vector of idtax_n
#' @param idtax_good vector of idtax_good; NULL if no synonym
#' @param add_taxa_info logical
#' @param id_trait integer indicating the id of the trait to look for
#' @param trait_cat_mode vector string if "most_frequent" then the most frequent value for categorical trait is given, if "all_unique" then all unique value separated by comma
#' @param verbose logical
#'
#' @export
query_traits_measures <- function(idtax = NULL,
                                  idtax_good = NULL,
                                  add_taxa_info = FALSE,
                                  id_trait = NULL,
                                  trait_cat_mode = "most_frequent",
                                  verbose = TRUE,
                                  pivot_table = TRUE,
                                  include_remarks = FALSE) {

  tbl <- "table_traits_measures"
  tbl2 <- "table_traits"

  if (!is.null(idtax)) {

    if (!is.null(idtax_good)) {

      idtax_tb <- dplyr::tibble(idtax = idtax, idtax_good = idtax_good)

    } else {

      table_taxa <- try_open_postgres_table(table = "table_taxa", con = mydb_taxa)

      # table_taxa <- try_open_postgres_table(table = "table_idtax", con = mydb)

      ids_syn <- table_taxa %>%
        dplyr::select(idtax_n, idtax_good_n) %>%
        dplyr::filter(idtax_good_n %in% !!idtax) %>%
        dplyr::collect()

      idtax <- unique(c(idtax, ids_syn$idtax_n))

      idtax_tb <- table_taxa %>%
        # dplyr::select(idtax_n, idtax_good_n) %>%
        dplyr::filter(idtax_n %in% !!idtax) %>%
        dplyr::collect() %>%
        dplyr::rename(idtax = idtax_n,
                      idtax_good = idtax_good_n)

    }


    if (!is.null(id_trait)) {
      sql <- glue::glue_sql("SELECT * FROM {`tbl`} LEFT JOIN {`tbl2`} ON {`tbl`}.fk_id_trait = {`tbl2`}.id_trait  WHERE id_trait IN ({vals2*}) AND idtax IN ({vals*})",
                            vals = idtax, .con = mydb_taxa, vals2 = id_trait)
    } else {
      sql <- glue::glue_sql("SELECT * FROM {`tbl`} LEFT JOIN {`tbl2`} ON {`tbl`}.fk_id_trait = {`tbl2`}.id_trait  WHERE idtax IN ({vals*})",
                            vals = idtax, .con = mydb_taxa)
    }

  } else {

    sql <- glue::glue_sql("SELECT * FROM {`tbl`} LEFT JOIN {`tbl2`} ON {`tbl`}.fk_id_trait = {`tbl2`}.id_trait  WHERE id_trait IN ({vals2*})",
                          vals = idtax, .con = mydb_taxa, vals2 = id_trait)
    
    idtax_tb <- 
      table_taxa %>%
      dplyr::rename(idtax = idtax_n,
                    idtax_good = idtax_good_n)

  }

  traits_found <-
    func_try_fetch(con = mydb_taxa, sql = sql)
  
  if (is.null(idtax)) {
    
    idtax <- unique(traits_found$idtax)
    
    idtax_tb <- 
      idtax_tb %>%
      dplyr::filter(idtax %in% !!idtax) %>%
      dplyr::collect()
  }
    

  # sql <-glue::glue_sql("SELECT * FROM {`tbl`} WHERE idtax IN ({vals*}) LEFT OUTER JOIN table_traits ON table_traits_measures.id_trait = table_traits.id_trait",
  #                      vals = idtax, .con = mydb_taxa)


  traits_found <-
    traits_found %>%
    dplyr::left_join(idtax_tb %>%
                       dplyr::select(idtax, idtax_good),
                     by = c("idtax" = "idtax")) %>%
    dplyr::mutate(idtax = ifelse(is.na(idtax_good), idtax, idtax_good)) %>%
    dplyr::select(-idtax_good)

  if (add_taxa_info) {

    taxa_infos <-
      add_taxa_table_taxa(ids = traits_found$idtax) %>%
      collect() %>%
      dplyr::select(idtax_n,
                    idtax_good_n,
                    tax_fam,
                    tax_gen,
                    tax_esp,
                    tax_infra_level)

    traits_found <-
      traits_found %>%
      left_join(taxa_infos,
                by = c("idtax" = "idtax_n"))

  }

  if(nrow(traits_found) > 0) {

    if (any(traits_found$valuetype == "categorical")) {

      # max_unique_val <- traits_found %>%
      #   dplyr::filter(valuetype == "categorical") %>%
      #   dplyr::filter(!is.na(traitvalue_char)) %>%
      #   dplyr::select(idtax,
      #                 trait,
      #                 traitvalue_char,
      #                 basisofrecord,
      #                 id_trait_measures) %>%
      #   group_by(idtax, trait) %>%
      #   count() %>%
      #   ungroup() %>%
      #   group_by(trait) %>%
      #   summarise(max_n = max(n))

      traits_idtax_char <-
        traits_found %>%
        dplyr::filter(valuetype == "categorical") %>%
        dplyr::select(idtax,
                      trait,
                      traitvalue_char,
                      basisofrecord,
                      measurementremarks,
                      id_trait_measures)

      if (!include_remarks)
        traits_idtax_char <-
          traits_idtax_char %>%
          dplyr::select(-measurementremarks)

      if (pivot_table) {
        traits_idtax_char <-
          traits_idtax_char %>%
          mutate(rn = 1:nrow(.)) %>%
          # dplyr::mutate(rn = data.table::rowid(trait)) %>%
          tidyr::pivot_wider(
            names_from = trait,
            values_from = c(traitvalue_char, basisofrecord, id_trait_measures),
            names_prefix = "taxa_level_"
          ) %>%
          dplyr::select(-rn)

        names(traits_idtax_char) <- gsub("traitvalue_char_", "", names(traits_idtax_char))

        traits_idtax_concat <-
          traits_idtax_char %>%
          dplyr::select(idtax, starts_with("id_trait_")) %>%
          dplyr::mutate(across(starts_with("id_trait_"), as.character)) %>%
          dplyr::group_by(idtax) %>%
          dplyr::mutate(dplyr::across(where(is.character),
                                      ~ stringr::str_c(.[!is.na(.)],
                                                       collapse = ", "))) %>%
          dplyr::ungroup() %>%
          dplyr::distinct()


        if (trait_cat_mode == "all_unique") {

          if (verbose) cli::cli_alert_info("Extracting all unique values for categorical traits")

          ### concatenate all distinct values
          traits_idtax_char <-
            traits_idtax_char %>%
            dplyr::select(-starts_with("id_trait_")) %>%
            dplyr::group_by(idtax) %>%
            dplyr::mutate(dplyr::across(where(is.character),
                                        ~ stringr::str_c(.[!is.na(.)],
                                                         collapse = ", "))) %>%
            dplyr::distinct() %>%
            dplyr::ungroup()

        }

        if (trait_cat_mode == "most_frequent") {

          if (verbose) cli::cli_alert_info("Extracting most frequent value for categorical traits")

          traits_idtax_char <-
            traits_idtax_char %>%
            dplyr::select(-starts_with("id_trait_")) %>%
            group_by(idtax, across(where(is.character))) %>%
            count() %>%
            arrange(idtax, desc(n)) %>%
            ungroup() %>%
            group_by(idtax) %>%
            dplyr::summarise_if(is.character, ~ first(.x[!is.na(.x)]))


          # trait_multiple_val <- max_unique_val %>% filter(max_n > 1) %>% pull(trait)
          #
          # traits_idtax_char_multiple <-
          #   traits_idtax_char %>%
          #   dplyr::select(-starts_with("id_trait_")) %>%
          #   group_by(idtax, across({{trait_multiple_val}})) %>%
          #   count() %>%
          #   arrange(idtax, desc(n)) %>%
          #   ungroup() %>%
          #   group_by(idtax) %>%
          #   dplyr::summarise_if(is.character, ~ first(.x[!is.na(.x)]))
          #
          # trait_unique_val <- max_unique_val %>% filter(max_n == 1) %>% pull(trait)
          #
          # traits_idtax_char_multiple <-
          #   traits_idtax_char %>%
          #   # dplyr::select(-starts_with("id_trait_"),
          #   #               idtax,
          #   #               all_of(trait_unique_val),
          #   #               -all_of(trait_multiple_val)) %>%
          #   group_by(idtax, across({{trait_unique_val}})) %>%
          #   count() %>%
          #   ungroup() %>%
          #   group_by(idtax) %>%
          #   dplyr::summarise_if(is.character, ~ .x[!is.na(.x)])


        }

        traits_idtax_char <-
          left_join(traits_idtax_char,
                    traits_idtax_concat, by = c("idtax" = "idtax"))

      }



      if (add_taxa_info) {

        traits_idtax_char <-
          traits_idtax_char %>%
          left_join(taxa_infos,
                    by = c("idtax" = "idtax_n"))
      }


    } else {

      traits_idtax_char <- NA

    }

    if (any(traits_found$valuetype == "numeric")) {

      traits_idtax_num <-
        traits_found %>%
        filter(valuetype == "numeric") %>%
        dplyr::select(idtax,
                      trait,
                      traitvalue,
                      basisofrecord,
                      id_trait_measures) %>%
        dplyr::mutate(rn = data.table::rowid(trait)) %>%
        tidyr::pivot_wider(
          names_from = trait,
          values_from = c(traitvalue, basisofrecord, id_trait_measures),
          names_prefix = "taxa_level_"
        ) %>%
        dplyr::select(-rn) %>%
        dplyr::mutate(across(starts_with("id_trait_"), as.character))


      # if (!is.null(idtax_good))
        traits_idtax_num <-
          traits_idtax_num %>%
          left_join(idtax_tb %>%
                      dplyr::select(idtax, idtax_good),
                    by = c("idtax" = "idtax")) %>%
          # dplyr::select(idtax, idtax_good) %>%
          dplyr::mutate(idtax = ifelse(is.na(idtax_good), idtax, idtax_good)) %>%
          dplyr::select(-idtax_good)

      names(traits_idtax_num) <- gsub("traitvalue_", "", names(traits_idtax_num))

      traits_idtax_concat <-
        traits_idtax_num %>%
        dplyr::select(idtax, starts_with("id_trait_")) %>%
        dplyr::mutate(dplyr::across(starts_with("id_trait_"), as.character)) %>%
        dplyr::group_by(idtax) %>%
        dplyr::mutate(dplyr::across(where(is.character),
                                    ~ stringr::str_c(.[!is.na(.)],
                                                     collapse = ", "))) %>%
        dplyr::ungroup() %>%
        dplyr::distinct()

      ### concatenate all distinct values
      traits_idtax_num_summarized <-
        traits_idtax_num %>%
        dplyr::select(-starts_with("id_trait_")) %>%
        dplyr::group_by(idtax) %>%
        dplyr::summarise(dplyr::across(where(is.numeric),
                                       .fns= list(mean = mean,
                                                  sd = sd,
                                                  n = length),
                                       .names = "{.col}_{.fn}"))

      traits_idtax_num <-
        traits_idtax_num_summarized %>%
        left_join(traits_idtax_concat, by = c("idtax" = "idtax"))
      # %>%
      #   left_join(traits_idtax_num %>%
      #               dplyr::select(idtax,
      #                             starts_with("basisofrecord_")) %>%
      #               dplyr::distinct(),
      #             by = c("idtax" = "idtax"))


      if (add_taxa_info) {

        traits_idtax_num <-
          traits_idtax_num %>%
          left_join(taxa_infos,
                    by = c("idtax" = "idtax_n"))
      }


    } else {

      traits_idtax_num <- NA

    }

  } else {

    traits_found <- NA
    traits_idtax_num <- NA
    traits_idtax_char <- NA

  }

  return(list(traits_found = traits_found,
              traits_idtax_num = traits_idtax_num,
              traits_idtax_char = traits_idtax_char))
}


#' List, extract taxa
#'
#' Provide list of selected taxa
#'
#' @return A tibble of all taxa
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param ids idtax to filter the table
#'
#'
#' @return A src object with merged taxa
#' @export
add_taxa_table_taxa <- function(ids = NULL) {

  if(!exists("mydb_taxa")) call.mydb.taxa(pass = "Anyuser2022", user = "common")

  table_taxa <- try_open_postgres_table(table = "table_taxa", con = mydb_taxa)

  table_taxa <-
    table_taxa %>%
    mutate(tax_sp_level = ifelse(!is.na(tax_esp), paste(tax_gen, tax_esp), NA)) %>%
    mutate(tax_infra_level = ifelse(!is.na(tax_esp),
                                    paste0(tax_gen,
                                           " ",
                                           tax_esp,
                                           ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
                                           ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
                                           ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
                                           ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), "")),
                                    NA)) %>%
    mutate(tax_infra_level_auth = ifelse(!is.na(tax_esp),
                                         paste0(tax_gen,
                                                " ",
                                                tax_esp,
                                                ifelse(!is.na(author1), paste0(" ", author1), ""),
                                                ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
                                                ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
                                                ifelse(!is.na(author2), paste0(" ", author2), ""),
                                                ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
                                                ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), ""),
                                                ifelse(!is.na(author3), paste0(" ", author3), "")),
                                         NA)) %>%
    dplyr::select(-year_description)

  if (!is.null(ids)) {

    table_taxa <-
      table_taxa %>%
      filter(idtax_n %in% ids)

  }



  return(table_taxa)
}




#' Get species-plot data frame
#'
#' Convert the extract of database into a species-plot data frame
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#'
#' @return A data frame with taxa as row, plot as columns and values as number of individuals
#' @export
species_plot_matrix <- function(data_tb, tax_col = "tax_sp_level", plot_col = "plot_name") {

  tax_col_enquo <-
    rlang::parse_expr(rlang::quo_name(rlang::enquo(tax_col)))
  plot_col_enquo <-
    rlang::parse_expr(rlang::quo_name(rlang::enquo(plot_col)))

  nbe_row_identified <-
    data_tb %>%
    filter(!is.na(!!tax_col_enquo)) %>%
    nrow()

  if (nbe_row_identified != nrow(data_tb)) {

      cli::cli_alert_info("Removing {nrow(data_tb) - nbe_row_identified} unidentified individuals")

    data_tb <-
      data_tb %>%
      filter(!is.na(!!tax_col_enquo))

  }

  data_tb_grouped <-
    data_tb %>%
    dplyr::mutate(ab = 1) %>%
    dplyr::group_by(!!plot_col_enquo, !!tax_col_enquo) %>%
    summarise(ab = sum(ab)) %>%
    ungroup()

  data_mat_tb <-
    tidyr::pivot_wider(data = data_tb_grouped,
                       names_from = !!plot_col_enquo,
                       values_from = ab,
                       values_fill = list(ab = 0))

  ## format the tibble into a matrix
  data_mat <- data_mat_tb %>% dplyr::select(-!!tax_col_enquo) %>%  as.data.frame()
  row.names(data_mat) <-
    data_mat_tb %>%
    dplyr::select(!!tax_col_enquo) %>%
    dplyr::pull()

  return(data_mat)
}




#' Add link between specimen and individual
#'
#' Generate link between individual and specimens
#'
#' @return A tibble of all subplots
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param new_data tibble fuzzy person name to look for
#' @param col_names_select a vector of string that select columns of new_data to consider, it must be 3 columns
#' @param col_names_corresp a vector of string of same length of col_names_select, should not be changed
#' @param launch_adding_data logical, if TRUE links are added, by default it is FALSE for security
#'
#' @export
.add_link_specimens <- function(new_data,
                                col_names_select = NULL,
                                col_names_corresp = c("id_specimen", "id_n", "type"),
                                launch_adding_data = FALSE) {

  if (is.null(col_names_select))
    col_names_select <- names(new_data)

  new_data_renamed <-
    .rename_data(dataset = new_data,
                 col_old = col_names_select,
                 col_new = col_names_corresp)

  message(paste0("Number of new links: ", nrow(new_data_renamed)))

  message(paste0("adding link for: ", nrow(distinct(new_data_renamed, id_specimen)), " different specimens"))

  cli::cli_alert_info("Prepare to add links for: {nrow(distinct(new_data_renamed, id_specimen))} different specimens")


  check_dup <-
    tbl(mydb, "data_link_specimens") %>%
    dplyr::select(id_n, id_specimen) %>%
    collect() %>%
    bind_rows(new_data_renamed %>%
                dplyr::select(id_n, id_specimen)) %>%
    group_by(id_n, id_specimen) %>%
    count() %>%
    filter(n > 1) %>%
    filter(id_n %in% unique(new_data_renamed$id_n)) %>%
    ungroup()

  if(nrow(check_dup) > 0) {

    print(check_dup)
    message("some link to be added are already in the database")
    message("Excluding existing link from the new data")

    new_data_renamed <-
      new_data_renamed %>%
      filter(!id_n %in% check_dup$id_n)

    message(paste0("New links to be added are now: ", nrow(new_data_renamed)))

  }

  data_to_add <-
    new_data_renamed %>%
    dplyr::select(all_of(col_names_corresp))

  print(data_to_add)

  if(launch_adding_data) {

    cli::cli_alert_success("Added links : {nrow(data_to_add)} rows to link table")

    DBI::dbWriteTable(mydb, "data_link_specimens",
                      data_to_add, append = TRUE, row.names = FALSE)
  }

}



















#' Query and standardize taxonomy
#'
#' Query and standardize taxonomy for synonymies and add traits information at species and genus levels
#'
#' @return tibble
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param idtax vector of idtax_n to be search
#' @param queried_tax tibble, output of query_taxa
#' @param verbose logical whether results should be shown in viewer
#'
#' @examples
#' match_tax(idtax = c(3095, 219))
#'
#' @export
match_tax <- function(idtax, queried_tax = NULL, verbose = TRUE) {

  if (is.null(queried_tax)) {

    queried_tax <- query_taxa(ids = idtax,
                               class = NULL,
                               verbose = FALSE)

  } else {

    idtax <- unique(queried_tax$idtax_n)

  }

  queried_taxa_updated <- queried_tax

  list_genera <-
    queried_tax %>%
    dplyr::filter(!is.na(tax_gen), is.na(idtax_good_n)) %>%
    dplyr::distinct(tax_gen) %>%
    dplyr::pull(tax_gen)

  all_sp_genera <- query_taxa(
    genus = list_genera,
    class = NULL,
    extract_traits = FALSE,
    verbose = FALSE,
    exact_match = TRUE
  )

  all_sp_genera <- all_sp_genera %>%
    filter(!is.na(idtax_good_n)) %>%
    left_join(all_sp_genera %>%
                filter(is.na(idtax_good_n)) %>%
                dplyr::select(idtax_n, tax_gen) %>%
                dplyr::rename(tax_gen_good = tax_gen),
              by = c("idtax_good_n" = "idtax_n")) %>%
    mutate(tax_gen = ifelse(!is.na(tax_gen_good), tax_gen_good, tax_gen))

  all_sp_genera <-
    all_sp_genera %>%
    filter(tax_gen %in% list_genera,
           !is.na(tax_infra_level))

  all_val_sp <- query_traits_measures(idtax = all_sp_genera %>%
                                        filter(!is.na(tax_esp)) %>%
                                        pull(idtax_n),
                                      idtax_good = all_sp_genera %>%
                                        filter(!is.na(tax_esp)) %>%
                                        pull(idtax_good_n),
                                      add_taxa_info = T)

  # level_trait <- rep("species", nrow(res))

  if (any(class(all_val_sp$traits_idtax_char) == "data.frame")) {

    traits_idtax_char <-
      all_val_sp$traits_found %>%
      dplyr::filter(valuetype == "categorical") %>%
      dplyr::select(idtax,
                    trait,
                    traitvalue_char,
                    basisofrecord,
                    id_trait_measures) %>%
      dplyr::mutate(rn = data.table::rowid(trait)) %>%
      tidyr::pivot_wider(
        names_from = trait,
        values_from = c(traitvalue_char, basisofrecord, id_trait_measures)
      ) %>%
      dplyr::select(-rn) %>%
      left_join(all_val_sp$traits_idtax_char %>%
                  dplyr::select(idtax, tax_gen),
                by = c("idtax" = "idtax"))

    names(traits_idtax_char) <- gsub("traitvalue_char_", "", names(traits_idtax_char))

    traits_idtax_concat <-
      traits_idtax_char %>%
      dplyr::select(tax_gen, starts_with("id_trait_")) %>%
      dplyr::mutate(across(starts_with("id_trait_"), as.character)) %>%
      dplyr::group_by(tax_gen) %>%
      dplyr::mutate(dplyr::across(where(is.character),
                                  ~ stringr::str_c(.[!is.na(.)],
                                                   collapse = ", "))) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()

    if (verbose) cli::cli_alert_info("Extracting most frequent value for categorical traits at genus level")

    traits_idtax_char <-
      traits_idtax_char %>%
      dplyr::select(-starts_with("id_trait_")) %>%
      group_by(tax_gen, across(where(is.character))) %>%
      count() %>%
      arrange(tax_gen, desc(n)) %>%
      ungroup() %>%
      group_by(tax_gen) %>%
      dplyr::summarise_if(is.character, ~ first(.x[!is.na(.x)]))

    traits_idtax_char <-
      left_join(traits_idtax_char,
                traits_idtax_concat, by = c("tax_gen" = "tax_gen"))

    colnames_traits <- names(traits_idtax_char %>%
                               dplyr::select(
                                 -tax_gen,
                                 -starts_with("id_trait_"),
                                 -starts_with("basisofrecord_")
                               ))

    for (j in 1:length(colnames_traits)) {

      if (colnames_traits[j] %in% names(queried_taxa_updated)) {

        var1 <- paste0(colnames_traits[j], ".y")
        var2 <- paste0(colnames_traits[j], ".x")

        queried_taxa_updated <-
          queried_taxa_updated %>%
          left_join(
            traits_idtax_char %>%
              dplyr::select(tax_gen, colnames_traits[j]),
            by = c("tax_gen" = "tax_gen")
          ) %>%
          mutate("{colnames_traits[j]}" :=
                   ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var2)))),
                          ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
                                 NA,
                                 !!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
                          !!rlang::parse_expr(quo_name(rlang::enquo(var2))))) %>%
          mutate("source_{colnames_traits[j]}" :=
                   ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var2)))),
                          ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
                                 NA,
                                 "genus"),
                          "species")) %>%
          dplyr::select(-paste0(colnames_traits[j], ".x"),
                        -paste0(colnames_traits[j], ".y"))

      } else {

        var1 <- colnames_traits[j]

        queried_taxa_updated <-
          queried_taxa_updated %>%
          left_join(
            traits_idtax_char %>%
              dplyr::select(tax_gen, colnames_traits[j]),
            by = c("tax_gen" = "tax_gen")
          ) %>%
          mutate("source_{colnames_traits[j]}" :=
                   ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
                          NA,
                          "genus"))

      }
    }


  }

  if (any(class(all_val_sp$traits_idtax_num) == "data.frame")) {

    traits_idtax_num <-
      all_val_sp$traits_found %>%
      dplyr::filter(valuetype == "numeric") %>%
      dplyr::select(idtax,
                    trait,
                    traitvalue,
                    basisofrecord,
                    id_trait_measures) %>%
      dplyr::mutate(rn = data.table::rowid(trait)) %>%
      tidyr::pivot_wider(
        names_from = trait,
        values_from = c(traitvalue, basisofrecord, id_trait_measures)
      ) %>%
      dplyr::select(-rn) %>%
      dplyr::left_join(all_val_sp$traits_idtax_num %>%
                         dplyr::select(idtax, tax_gen),
                       by = c("idtax" = "idtax"))

    names(traits_idtax_num) <- gsub("traitvalue_", "", names(traits_idtax_num))

    traits_idtax_concat <-
      traits_idtax_num %>%
      dplyr::select(tax_gen, starts_with("id_trait_")) %>%
      dplyr::mutate(across(starts_with("id_trait_"), as.character)) %>%
      dplyr::group_by(tax_gen) %>%
      dplyr::mutate(dplyr::across(where(is.character),
                                  ~ stringr::str_c(.[!is.na(.)],
                                                   collapse = ", "))) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()

    traits_idtax_num <-
      traits_idtax_num %>%
      dplyr::select(-starts_with("id_trait_"), -idtax) %>%
      dplyr::group_by(tax_gen) %>%
      dplyr::summarise(dplyr::across(where(is.numeric),
                                     .fns= list(mean = mean,
                                                sd = sd,
                                                n = length),
                                     .names = "{.col}_{.fn}"))


    colnames_traits <- names(traits_idtax_num %>%
                               dplyr::select(
                                 -tax_gen,
                                 -starts_with("id_trait_"),
                                 -starts_with("basisofrecord_")
                               ))

    for (j in 1:length(colnames_traits)) {

      if (colnames_traits[j] %in% names(queried_taxa_updated)) {

        var1 <- paste0(colnames_traits[j], ".y")
        var2 <- paste0(colnames_traits[j], ".x")

        queried_taxa_updated <-
          queried_taxa_updated %>%
          left_join(
            traits_idtax_num %>%
              dplyr::select(tax_gen, colnames_traits[j]),
            by = c("tax_gen" = "tax_gen")
          ) %>%
          mutate("{colnames_traits[j]}" :=
                   ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var2)))),
                          ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
                                 NA,
                                 !!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
                          !!rlang::parse_expr(quo_name(rlang::enquo(var2))))) %>%
          mutate("source_{colnames_traits[j]}" :=
                   ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var2)))),
                          ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
                                 NA,
                                 "genus"),
                          "species")) %>%
          dplyr::select(-paste0(colnames_traits[j], ".x"),
                        -paste0(colnames_traits[j], ".y"))


      } else {

        var1 <- colnames_traits[j]

        queried_taxa_updated <-
          queried_taxa_updated %>%
          left_join(
            traits_idtax_num %>%
              dplyr::select(tax_gen, colnames_traits[j]),
            by = c("tax_gen" = "tax_gen")
          ) %>%
          mutate("source_{colnames_traits[j]}" :=
                   ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
                          NA,
                          "genus"))

      }
    }

  }


  queried_taxa_syn_sub <-
    queried_taxa_updated %>%
    filter(!is.na(idtax_good_n)) %>%
    dplyr::select(idtax_n, idtax_good_n, tax_infra_level) %>%
    filter(idtax_n %in% idtax) %>%
    rename(taxa_submitted = tax_infra_level) %>%
    left_join(queried_taxa_updated, by = c("idtax_good_n" = "idtax_n")) %>%
    relocate(tax_infra_level, .after = "taxa_submitted")

  queried_taxa_not_syn_sub <-
    queried_taxa_updated %>%
    filter(is.na(idtax_good_n)) %>%
    dplyr::select(idtax_n, tax_infra_level) %>%
    filter(idtax_n %in% idtax) %>%
    rename(taxa_submitted = tax_infra_level) %>%
    left_join(queried_taxa_updated, by = c("idtax_n" = "idtax_n")) %>%
    relocate(tax_infra_level, .after = "taxa_submitted")

  results <- bind_rows(queried_taxa_syn_sub, queried_taxa_not_syn_sub) %>%
    arrange(tax_fam, tax_gen, tax_esp)


  if (verbose & nrow(results) < 100) {

    res_print <-
      results %>%
      # dplyr::select(-fktax,-id_good,-tax_tax) %>%
      dplyr::relocate(tax_infra_level_auth, .before = tax_order) %>%
      dplyr::relocate(idtax_n, .before = tax_order) %>%
      dplyr::relocate(idtax_good_n, .before = tax_order)

    print_table(res_print)

    # res_print <-
    #   res_print %>%
    #   mutate_all(~ as.character(.)) %>%
    #   mutate_all(~ tidyr::replace_na(., ""))
    #
    # as_tibble(cbind(columns = names(res_print), record = t(res_print))) %>%
    #   kableExtra::kable(format = "html", escape = F) %>%
    #   kableExtra::kable_styling("striped", full_width = F) %>%
    #   print()

  }


  if(verbose & nrow(results) >= 100)
    message("\n Not showing html table because too many taxa")

  return(results)

}







#' Find unlinked individual
#'
#' Extract individuals for which no links exist with herbarium specimens
#'
#' @return list
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param ids vector of id_n of individuals for which we want to explore links
#'
#'
#' @export
get_ref_specimen_ind <- function(collector = NULL, ids = NULL) {

  if (!is.null(collector)) {

    # collector <-

    collector <-
      .link_colnam(
      data_stand = tibble(colnam = collector),
      column_searched = "colnam",
      column_name = "colnam",
      id_field = "id_colnam",
      id_table_name = "id_table_colnam",
      db_connection = mydb,
      table_name = "table_colnam"
    )

  }

  all_id_individuals_links <-
    tbl(mydb, "data_link_specimens") %>%
    dplyr::select(id_n) %>%
    distinct(id_n) %>%
    dplyr::collect()

  if (!is.null(ids))
    all_herb_individuals <-
    tbl(mydb, "data_individuals") %>%
    dplyr::select(id_n, id_specimen, herbarium_nbe_char,
                  herbarium_code_char, herbarium_nbe_type) %>%
    dplyr::filter(!id_n %in% !!all_id_individuals_links$id_n) %>%
    dplyr::filter(id_n %in% !!ids) %>%
    dplyr::collect()

  if (is.null(ids))
    all_herb_individuals <-
    tbl(mydb, "data_individuals") %>%
    dplyr::select(id_n, id_specimen, herbarium_nbe_char,
                  herbarium_code_char, herbarium_nbe_type) %>%
    dplyr::filter(!id_n %in% !!all_id_individuals_links$id_n) %>%
    collect()

  ## all individual with a link to specimen not linked to specimen table
  all_herb_not_linked <-
    all_herb_individuals %>%
    filter(!is.na(herbarium_nbe_char) | !is.na(herbarium_code_char))

  ## getting number of specimen
  all_herb_not_linked <-
    all_herb_not_linked %>%
    filter(!is.na(herbarium_nbe_char)) %>%
    mutate(herbarium_nbe_char = stringr::str_replace(string = herbarium_nbe_char,
                                                     pattern = "-",
                                                     replacement = " ")) %>%
    mutate(herbarium_nbe_char = stringr::str_replace_all(string = herbarium_nbe_char,
                                                     pattern = "[.]",
                                                     replacement = " ")) %>%
    mutate(nbrs = readr::parse_number(herbarium_nbe_char)) %>%
    mutate(nbrs = ifelse(nbrs < 1, nbrs*-1, nbrs)) %>%
    arrange(desc(nbrs))

  ## getting collector of specimen
  all_herb_not_linked <-
    all_herb_not_linked %>%
    # filter(grepl("IDU", herbarium_nbe_char)) %>%
    mutate(coll = stringr::str_replace(string = herbarium_nbe_char,
                                       pattern = as.character(nbrs),
                                       replacement = "")) %>%
    mutate(coll = str_trim(coll))


  ## linking collector to collector table
  # all_herb_not_linked <-
  #   .link_colnam(data_stand = all_herb_not_linked, collector_field = "coll")

  all_herb_not_linked <- .link_colnam(
    data_stand = all_herb_not_linked,
    column_searched = "coll",
    column_name = "colnam",
    id_field = "id_colnam",
    id_table_name = "id_table_colnam",
    db_connection = mydb,
    table_name = "table_colnam"
  )

  # if (!is.null(collector)) {
  #   cli::cli_alert_info("filtering on collector : {collector$col_name}")
  #
  #   all_herb_not_linked <-
  #     all_herb_not_linked %>%
  #     filter(id_colnam == collector$id_colnam)
  #
  # }

  ## getting information from data_liste_plot
  all_herb_not_linked <-
    all_herb_not_linked %>%
    left_join(tbl(mydb, "data_individuals") %>%
                select(id_n, id_table_liste_plots_n, idtax_n) %>%
                collect(),
              by=c("id_n"="id_n")) %>%
    left_join(tbl(mydb, "data_liste_plots") %>%
                select(id_liste_plots, plot_name, team_leader) %>%
                collect(),
              by=c("id_table_liste_plots_n"="id_liste_plots"))

  all_linked_individuals <-
    tbl(mydb, "data_link_specimens") %>%
    distinct(id_n) %>%
    collect()

  ### selection of all individuals with specimens linked but not included in link table
  # all_herb_missing_link <-
  #   all_herb_not_linked %>%
  #   filter(!id_n %in% all_linked_individuals$id_n)
  #
  # all_herb_missing_link_unique <-
  #   all_herb_missing_link %>%
  #   group_by(id_colnam, nbrs) %>%
  #   count() %>%
  #   ungroup()
  #
  # cli::cli_alert_info("Missing link for {nrow(all_herb_missing_link_unique)} specimens")
  # print(all_herb_missing_link_unique %>%
  #         dplyr::select(-n) %>%
  #         group_by(id_colnam) %>%
  #         count())

  return(list(all_herb_not_linked = all_herb_not_linked,
              all_linked_individuals = all_linked_individuals))


  # all_herb_missing_link = all_herb_missing_link,

}



#' Update id table for taxa
#'
#' Update and rewrite idtaxa to database, require administrator rights
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @export
update_taxa_link_table <- function() {

  # if (exists("mydb_taxa")) rm(mydb_taxa)
  call.mydb.taxa()

  call.mydb()

  id_taxa_table <- try_open_postgres_table(table = "table_taxa", con = mydb_taxa) %>%
    # dplyr::tbl(mydb_taxa, "table_taxa") %>%
    dplyr::select(idtax_n, idtax_good_n) %>%
    dplyr::collect()

  dbWriteTable(mydb,
               name = "table_idtax",
               value = id_taxa_table,
               append = FALSE,
               overwrite = TRUE)

  cli::cli_alert_success("table_idtax updated")

  # dplyr::tbl(mydb, "table_idtax")


}


#' Send query to postgresql database
#'
#' Access to postgresql database with repeating if no successful
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @export
func_try_fetch <- function(con, sql) {
  rep <- TRUE
  rep_try <- 1
  while(rep) {

    res_q <- try({rs <- DBI::dbSendQuery(con, sql);
    DBI::dbFetch(rs)}, silent = T)

    if (any(grepl("Lost connection to database", res_q[1])))
      stop("Lost connection to database")

    if (any(grepl("Failed to prepare query", res_q[1]))) {
      rep <- TRUE
      cli::cli_alert_warning("failed to query, trying again")
      rep_try <- rep_try + 1
    } else {
      rep <- FALSE
    }

    if (rep_try == 10)
      stop("Failed to connect to database")
  }
  res_q <- res_q %>% as_tibble(.name_repair = "universal")
  DBI::dbClearResult(rs)

  return(res_q)
}

#' Open postgresql database table
#'
#' Access to postgresql database table with repeating if no successful
#'
#' @param table string
#' @param con connection to database
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @export
try_open_postgres_table <- function(table, con) {

  rep <- TRUE
  rep_try <- 1
  while(rep) {

    res_q <- try({table_postgre <- dplyr::tbl(con, table)}, silent = TRUE)

    if (any(grepl("Lost connection to database", res_q[1])))
      stop("Lost connection to database")

    if (any(grepl("Failed to prepare query", res_q[1]))) {
      rep <- TRUE
      cli::cli_alert_warning("failed to query, trying again")
      rep_try <- rep_try + 1
    } else {
      rep <- FALSE
    }

    if (rep_try == 10)
      stop("Failed to connect to database")
  }

  return(table_postgre)
}

#' Memoised try_open_postgres_table function
#'
#' Memoised 'try_open_postgres_table' function
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @importFrom memoise memoise
#'
#' @export
try_open_postgres_table_mem <- memoise::memoise(try_open_postgres_table)



#' Add new entry to taxonomic table
#'
#' Add new entry to taxonomic table
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param tax_gen string genus name
#' @param tax_esp string species name
#' @param tax_fam string family name
#' @param tax_rank1 string tax_rank1 name
#' @param tax_name1 string tax_name1 name
#' @param detvalue integer detvalue code
#' @param morphocat integer morphocat code
#' @param full_name string full name : genus + species + authors
#' @param synonym_of list if the new entry should be put in synonymy with an existing taxa, add in a list at least one values to identify to which taxa it will be put in synonymy: genus, species or id
#'
#'
#' @return A tibble
#' @export
add_entry_taxa <- function(search_name_tps = NULL,
                           tax_gen = NULL,
                           tax_esp = NULL,
                           tax_fam = NULL,
                           tax_order = NULL,
                           tax_famclass = NULL,
                           tax_rank1 = NULL,
                           tax_name1 = NULL,
                           tax_rank2 = NULL,
                           tax_name2 = NULL,
                           # a_habit = NULL,
                           author1 = NULL,
                           author2 = NULL,
                           author3 = NULL,
                           year_description = NULL,
                           synonym_of = NULL,
                           morpho_species = FALSE,
                           TPS_KEY = "15ad0b4c-f0d3-46ab-b649-178f2c75724f",
                           tax_tax = NULL)
{

  # if (!exists("mydb")) call.mydb()
  # if (exists("mydb_taxa")) rm(mydb_taxa)
  if (!exists("mydb_taxa")) call.mydb.taxa()

  if (is.null(search_name_tps) &
      is.null(tax_gen) & is.null(tax_esp) & is.null(tax_fam))
    search_name_tps <-
      readline(prompt = "Enter name to search on Tropicos  : ")

  if (!is.null(search_name_tps)) {

    if (search_name_tps != "") {
      res_tps <- taxize::tp_search(sci = search_name_tps, key = TPS_KEY)

      if (ncol(res_tps) == 1) {

        cli::cli_alert_info("Not found on Tropicos")


        # tax_tax <- readline(prompt = "Enter tax_tax  : ")
        # tax_tax <- stringr::str_squish(tax_tax)
        # if (tax_tax == '')
        #   tax_tax <- NULL

        tax_gen <- readline(prompt = "Enter tax_gen  : ")
        tax_gen <- stringr::str_squish(tax_gen)
        if (tax_gen == '')
          tax_gen <- NULL

        tax_esp <- readline(prompt = "Enter tax_esp  : ")
        tax_esp <- stringr::str_squish(tax_esp)
        if (tax_esp == '')
          tax_esp <- NULL

        author1 <- readline(prompt = "Enter author1  : ")
        author1 <- stringr::str_squish(author1)
        if (author1 == '')
          author1 <- NULL

        tax_rank1 <- readline(prompt = "Enter tax_rank1 (one of 'var.' or 'subsp.')  : ")
        tax_rank1 <- stringr::str_squish(tax_rank1)
        if (tax_rank1 == '')
          tax_rank1 <- NULL

        if (!is.null(tax_rank1)) {
          if (!tax_rank1 %in% c('var.', 'subsp.')) stop("tax_rank1 must be 'var.' or 'subsp.'")
        }

        tax_name1 <- readline(prompt = "Enter tax_name1 (var. or subsp. name)  : ")
        tax_name1 <- stringr::str_squish(tax_name1)
        if (tax_name1 == '')
          tax_name1 <- NULL

        tax_order <- readline(prompt = "Enter tax_order  : ")
        if (tax_order == '')
          tax_order <- NULL

        morpho_species <-
          askYesNo(msg = "This is NOT a morphotaxa, confirm or set NO if it is a morphotaxa", default = FALSE)
        morpho_species <- !morpho_species

      } else {

        cli::cli_alert_success("Match on Tropicos")

        res_tps_view <- tibble(feat = names(res_tps))

        for (h in 1:nrow(res_tps))
          res_tps_view <- res_tps_view %>%
          bind_cols(unlist(res_tps[h,]))

        names(res_tps_view) <- c("feat", seq(1, nrow(res_tps), 1))

        res_tps_view %>%
          kableExtra::kable(format = "html", escape = F) %>%
          kableExtra::kable_styling("striped", full_width = F) %>%
          print()

        tax_sel <- readline(prompt = "Choose the taxa  (0 if none) : ")

        if (tax_sel != "0") {

          res_tps_selected <- res_tps %>% slice(as.numeric(tax_sel))

          tax_tax <- res_tps_selected$scientificnamewithauthors
          cat(cli::bg_red(paste(tax_tax)))
          cf_ <- readline(prompt = paste("Confirm tax_tax ? press enter"))
          if (cf_ != '') {
            tax_tax <- readline(prompt = "Enter tax_tax  : ")
            if (tax_tax == '')
              tax_tax <- NULL
          }

          tax_gen <- unlist(strsplit(res_tps_selected$scientificname, " "))[1]
          cat(cli::bg_red(paste(tax_gen)))
          cf_ <- readline(prompt = paste("Confirm tax_gen ? press enter"))
          if (cf_ != '') {
            tax_gen <- readline(prompt = "Enter tax_gen  : ")
            if (tax_gen == '')
              tax_gen <- NULL
          }

          tax_esp <- unlist(strsplit(res_tps_selected$scientificname, " "))[2]
          cat(cli::bg_red(paste(tax_esp)))
          cf_ <- readline(prompt = paste("Confirm tax_esp ? press enter"))
          if (cf_ != '') {
            tax_esp <- readline(prompt = "Enter tax_esp  : ")
            if (tax_esp == '')
              tax_esp <- NULL
          }

          tax_rank1 <- res_tps_selected$rankabbreviation
          if (tax_rank1 != "sp.") {
            cat(cli::bg_red(paste(tax_rank1)))
            cf_ <- readline(prompt = paste("Confirm tax_rank1 ? press enter"))
            if (cf_ != '') {
              tax_rank1 <- readline(prompt = "Enter tax_rank1  : ")
              if (tax_rank1 == '')
                tax_rank1 <- NULL
            }
          } else {
            tax_rank1 <- NULL
          }

          tax_name1 <- unlist(strsplit(res_tps_selected$scientificname, " "))[4]
          if (!is.na(tax_name1)) {
            cat(cli::bg_red(paste(tax_name1)))
            cf_ <- readline(prompt = paste("Confirm tax_name1 ? press enter"))
            if (cf_ != '') {
              tax_name1 <- readline(prompt = "Enter tax_name1  : ")
              if (tax_name1 == '')
                tax_name1 <- NULL
            }
          } else {
            tax_name1 <- NULL
          }

          tax_fam <- res_tps_selected$family
          cat(cli::bg_red(paste(tax_fam)))
          cf_ <- readline(prompt = paste("Confirm tax_fam ? press enter"))
          if (cf_ != '') {
            tax_fam <- readline(prompt = "Enter tax_fam  : ")
            if (tax_fam == '')
              tax_fam <- NULL
          }

          author1 <- res_tps_selected$author
          cat(cli::bg_red(paste(author1)))
          cf_ <- readline(prompt = paste("Confirm author1 ? press enter"))
          if (cf_ != '') {
            author1 <- readline(prompt = "Enter author1  : ")
            if (author1 == '')
              author1 <- NULL
          }

          if (!is.null(tax_name1)) {
            author2 <- readline(prompt = "Enter author2  : ")
            if (author2 == '')
              author2 <- NULL
          } else {
            author2 <- NULL
          }

          year_description <- as.numeric(res_tps_selected$displaydate)

        }

      }

    }

  }

  all_growth_form <- choose_growth_form()

  if (is.null(tax_tax) & !is.null(tax_esp) & is.null(author1))
    stop("Provide full name with authors")

  if (is.null(tax_gen) & is.null(tax_esp) & is.null(tax_fam) & is.null(tax_order) & is.null(tax_famclass))
    stop("Provide at least one genus/family/order/class new name to enter")

  if (!is.null(tax_fam) & is.null(tax_tax) & is.null(tax_gen)) {
    tax_tax <- tax_fam
  }

  if(!is.null(tax_order) & is.null(tax_tax) & is.null(tax_gen)) {
    tax_tax <- tax_order
  }

  if(!is.null(tax_famclass) & is.null(tax_tax) & is.null(tax_gen)) {
    tax_tax <- tax_famclass
  }

  check_taxo <- TRUE

  if (is.null(tax_fam) & !is.null(tax_gen)) {

    tax_fam <-
      query_taxa(
        genus = tax_gen,
        verbose = F,
        exact_match = T,
        class = NULL,
        check_synonymy = FALSE,
        extract_traits = FALSE
      )

    if (is.null(nrow(tax_fam)))
      stop("genus not in database")

    tax_fam <- tax_fam %>%
      dplyr::distinct(tax_fam) %>%
      dplyr::pull()
    tax_fam <- tax_fam[which(!is.na(tax_fam))]

    if (length(tax_fam) > 1)
      cli::cli_alert_warning("No tax_fam provided, and two different family names for this genus {tax_fam}.")
    if (length(tax_fam) > 1)
      check_taxo <- FALSE
    if (length(tax_fam) == 1)
      cli::cli_alert_info("No tax_fam provided, based on genus, the following family is chosen {tax_fam}.")
  }

  if(is.null(tax_order) & !is.null(tax_fam)) {
    tax_order <-
      query_taxa(
        family = tax_fam,
        verbose = F,
        exact_match = T,
        class = NULL,
        check_synonymy = FALSE,
        extract_traits = FALSE
      )

    if (is.null(tax_order))
      stop("Family not existing, define order")

    tax_order <-
      tax_order %>%
      dplyr::distinct(tax_order) %>%
      dplyr::pull()

    tax_order <- tax_order[which(!is.na(tax_order))]
    if (length(tax_order) > 1)
      cli::cli_alert_warning("No tax_order provided, and two different order names for this family: {tax_order}")
    if (length(tax_order) > 1)
      check_taxo <- FALSE
    if (length(tax_order) == 1)
      cli::cli_alert_info("No tax_order provided, based on family, the following order is chosen: {tax_order}")
  }

  if(is.null(tax_famclass) & !is.null(tax_order)) {
    tax_famclass <-
      query_taxa(
        order = tax_order,
        verbose = F,
        exact_match = T,
        class = NULL,
        check_synonymy = FALSE,
        extract_traits = FALSE
      ) %>%
      dplyr::distinct(tax_famclass) %>%
      dplyr::pull()
    tax_famclass <- tax_famclass[which(!is.na(tax_famclass))]
    if (length(tax_famclass) > 1)
      cli::cli_alert_warning("No tax_famclass provided, and two different class names for this order: {tax_famclass}")
    if (length(tax_famclass) > 1)
      check_taxo <- FALSE
    if (length(tax_famclass) == 1)
      cli::cli_alert_info("No tax_famclass provided, based on order, the following class is chosen: {tax_famclass}")
  }

  tax_fam_new <- TRUE
  if(!is.null(tax_fam) & check_taxo) {
    searched_tax_fam <-
      dplyr::tbl(mydb_taxa, "table_taxa") %>%
      dplyr::distinct(tax_fam) %>%
      dplyr::filter(tax_fam == !!tax_fam) %>%
      dplyr::collect()
    if(nrow(searched_tax_fam)==0) {
      tax_fam_new <-
        utils::askYesNo(msg = "The provided family name is currently not present in the dictionnary. Are you sure it is correctly spelled?", default = FALSE)
    }
  }

  if(!is.null(tax_tax) & !is.null(tax_gen)) {
    if(!grepl(tax_gen, tax_tax)) stop("\n Genus and tax_tax are provided, but genus is not found within full name, there must be an ERROR")
  }

  if(!is.null(tax_tax) & !is.null(tax_esp)) {
    if(!grepl(tax_esp, tax_tax)) stop("\n Species and tax_tax are provided, but tax_esp is not found within tax_tax, there must be an ERROR")
  }

  if(is.null(tax_gen) & !is.null(tax_esp)) {
    stop("\n species epithet provided but no genus (provide tax_gen)")
  }

  if(!is.null(tax_gen)) {

    family_check <-
      query_taxa(family = tax_fam, exact_match = T, verbose = F, class = NULL, check_synonymy = FALSE, extract_traits = F)

    genus_check <-
      query_taxa(genus = tax_gen,
                                exact_match = T,
                                verbose = F,
                                class = NULL,
                                check_synonymy = FALSE, extract_traits = F)

    family_check %>%
      filter(tax_gen == tax_gen)

    if (!is.null(genus_check)) {
      if (nrow(genus_check) > 0 & !any(family_check$tax_gen[!is.na(family_check$tax_gen)] == tax_gen)) {
        cat(
          paste(
            "\n The provided genus is present in the taxonomic backbone, but with different family name:",
            genus_check$tax_fam[1]
          )
        )
        check_taxo <- FALSE
      }
    }
  }

  # tbl(mydb, "diconame") %>% collect() %>% slice(n())

  if (check_taxo & tax_fam_new) {

    if (!is.null(tax_gen) &
        !is.null(tax_esp))
      paste_taxa <- paste(tax_gen, tax_esp)
    if (!is.null(tax_gen) & is.null(tax_esp))
      paste_taxa <- tax_gen
    if (!is.null(tax_fam) & is.null(tax_gen))
      paste_taxa <- tax_fam
    if (is.null(tax_tax) &
        !is.null(tax_gen) & is.null(tax_esp))
      tax_tax <- tax_gen

    if (is.null(tax_esp))
      tax_esp <- NA
    if (is.null(tax_gen))
      tax_gen <- NA
    if (is.null(tax_fam))
      tax_fam <- NA
    if (is.null(tax_order))
      tax_order <- NA
    if (is.null(tax_rank1))
      tax_rank1 <- NA
    if (is.null(tax_name1))
      tax_name1 <- NA
    if (is.null(tax_rank2))
      tax_rank2 <- NA
    if (is.null(tax_name2))
      tax_name2 <- NA
    # if(is.null(a_habit)) a_habit <- NA
    if (is.null(author1))
      author1 <- NA
    if (is.null(author2))
      author2 <- NA
    if (is.null(author3))
      author3 <- NA

    tax_rank <- NA
    if(!is.na(tax_esp) & is.na(tax_name1) & is.na(tax_name1))
      tax_rank <- "ESP"
    if(is.na(tax_esp) & is.na(tax_name1) & is.na(tax_name2))
      tax_rank <- NA
    if(!is.na(tax_esp) & !is.na(tax_rank1)) {
      if(tax_rank1=="subsp.") tax_rank <- "SUBSP"
      if(tax_rank1=="var.") tax_rank <- "VAR"
      if(tax_rank1=="f.") tax_rank <- "F"
    }

    if(!is.na(tax_rank)) {
      if(tax_rank=="VAR") tax_rankinf <- "VAR"
      if(tax_rank=="SUBSP") tax_rankinf <- "SUBSP"
    }

    if (!is.na(tax_fam) &
        is.na(tax_gen) & is.na(tax_esp))
      tax_rankinf <- "FAM"

    if (!is.na(tax_fam) &
        !is.na(tax_gen) & is.na(tax_esp))
      tax_rankinf <- "GEN"

    if (!is.na(tax_fam) &
        !is.na(tax_gen) & !is.na(tax_esp) & is.na(tax_rank))
      tax_rankinf <- "ESP"

    if (!is.na(tax_fam) &
        !is.na(tax_gen) & !is.na(tax_esp) & tax_rank == "ESP")
      tax_rankinf <- "ESP"

    if (!is.na(tax_order) &
        is.na(tax_fam) &
        is.na(tax_gen) & is.na(tax_esp) & is.na(tax_rank))
      tax_rankinf <- "ORDER"

    if (!is.null(tax_famclass) &
        is.na(tax_order) &
        is.na(tax_fam) &
        is.na(tax_gen) & is.na(tax_esp) & is.na(tax_rank))
      tax_rankinf <- "CLASS"

    if(!is.na(tax_order) & is.na(tax_fam) & is.na(tax_gen) & is.na(tax_esp)) tax_rankesp <- "ORDER"
    if(!is.null(tax_famclass) & is.na(tax_order) & is.na(tax_fam) & is.na(tax_gen) & is.na(tax_esp)) tax_rankesp <- "CLASS"
    if(!is.na(tax_fam) & is.na(tax_gen) & is.na(tax_esp)) tax_rankesp <- "FAM"
    if(!is.na(tax_fam) & !is.na(tax_gen) & is.na(tax_esp)) tax_rankesp <- "GEN"
    if(!is.na(tax_fam) & !is.na(tax_gen) & !is.na(tax_esp))
      tax_rankesp <- "ESP"

    ## get id of class
    id_tax_fam_class <-
      try_open_postgres_table(table = "table_tax_famclass", con = mydb_taxa) %>%
      # tbl(mydb_taxa, "table_tax_famclass") %>%
      filter(tax_famclass == !!tax_famclass) %>%
      collect()

    new_rec <-
      dplyr::tibble(
        tax_order = tax_order,
        tax_famclass = tax_famclass,
        tax_fam = tax_fam,
        tax_gen = tax_gen,
        tax_esp = tax_esp,
        tax_rank01 = tax_rank1,
        tax_nam01 = tax_name1,
        tax_rank02 = tax_rank2,
        tax_nam02 = tax_name2,
        tax_source = "NEW",
        tax_rank = tax_rank,
        tax_rankinf = tax_rankinf,
        tax_rankesp = tax_rankesp,
        fktax = NA,
        author1 = author1,
        author2 = author2,
        author3 = author3,
        citation = NA,
        year_description = ifelse(!is.null(year_description), year_description, NA),
        idtax_good_n = NA,
        id_tax_famclass = id_tax_fam_class$id_tax_famclass,
        morpho_species = morpho_species
      )

    seek_dup <-
      try_open_postgres_table(table = "table_taxa", con = mydb_taxa)


    if(!is.na(new_rec$tax_famclass))
      seek_dup <- seek_dup %>%
      filter(tax_famclass == !!new_rec$tax_famclass)
    if(!is.na(new_rec$tax_order)) {
      seek_dup <- seek_dup %>%
        filter(tax_order == !!new_rec$tax_order)
    }else{
      seek_dup <- seek_dup %>%
        filter(is.na(tax_order))
    }

    if(!is.na(new_rec$tax_fam)) {
      seek_dup <- seek_dup %>%
        filter(tax_fam == !!new_rec$tax_fam)
    }else{
      seek_dup <- seek_dup %>%
        filter(is.na(tax_fam))
    }

    if(!is.na(new_rec$tax_gen)) {
      seek_dup <- seek_dup %>%
        filter(tax_gen == !!new_rec$tax_gen)
    }else{
      seek_dup <- seek_dup %>%
        filter(is.na(tax_gen))
    }

    if(!is.na(new_rec$tax_esp)) {
      seek_dup <- seek_dup %>%
        filter(tax_esp == !!new_rec$tax_esp)
    }else{
      seek_dup <- seek_dup %>%
        filter(is.na(tax_esp))
    }

    if(!is.na(new_rec$tax_rank01)) {
      seek_dup <- seek_dup %>%
        filter(tax_rank01 == !!new_rec$tax_rank01)
    }else{
      seek_dup <- seek_dup %>%
        filter(is.na(tax_rank01))
    }

    if(!is.na(new_rec$tax_nam01)) {
      seek_dup <- seek_dup %>%
        filter(tax_nam01 == !!new_rec$tax_nam01)
    }else{
      seek_dup <- seek_dup %>%
        filter(is.na(tax_nam01))
    }

    if(!is.na(new_rec$tax_nam02)) {
      seek_dup <- seek_dup %>%
        filter(tax_nam02 == !!new_rec$tax_nam02)
    }else{
      seek_dup <- seek_dup %>%
        filter(is.na(tax_nam02))
    }

    seek_dup <-
      seek_dup %>%
      collect()

    launch_adding_data <- TRUE

    if (nrow(seek_dup) > 0) {
      cli::cli_h3(cli::bg_red("new entry fit to one entry already in table_taxa"))
      print(as.data.frame(seek_dup))
      launch_adding_data <- FALSE
    }

    if(launch_adding_data) {

      new_rec <-
        .add_modif_field(new_rec)

      new_rec <-
        new_rec %>%
        rename(data_modif_m = date_modif_m,
               data_modif_y = date_modif_y,
               data_modif_d = date_modif_d)

      cli::cli_alert_success(cli::col_yellow("Adding new entry"))
      DBI::dbWriteTable(mydb_taxa, "table_taxa", new_rec, append = TRUE, row.names = FALSE)

      rs <- DBI::dbSendQuery(mydb_taxa, statement = "SELECT MAX(idtax_n) FROM table_taxa")
      lastval <- DBI::dbFetch(rs)
      DBI::dbClearResult(rs)

      new_entry <-
        dplyr::tbl(mydb_taxa, "table_taxa") %>%
        dplyr::filter(idtax_n == !!lastval$max) %>%
        dplyr::collect()

      ### add growth form data
      if(nrow(all_growth_form) > 0) {
        cli::cli_alert_info("add growth form data")

        all_growth_form <- all_growth_form %>%
          dplyr::mutate(idtax = new_entry$idtax_n)

        all_growth_form_pivot <-
          all_growth_form %>%
          tidyr::pivot_wider(names_from = trait,
                             values_from = value)

        add_sp_traits_measures(new_data = all_growth_form_pivot,
                            traits_field = names(all_growth_form_pivot)[2:ncol(all_growth_form_pivot)],
                            idtax = "idtax",
                            add_data = T,
                            ask_before_update = FALSE)

      }

      if(!is.null(synonym_of)) {

        if(!is.list(synonym_of)) {
          stop("synonym_of should be a list with the first element \nbeing the genus and the second element the species epiteth of the taxa of the correct name \nOR the idtax_n")
        }

        if(!any(names(synonym_of)=="genus") & !any(names(synonym_of)=="species") & !any(names(synonym_of)=="id"))
          stop("synonym_of should have at least of the thre following element : genus, species or idtax_n")

        if(!any(names(synonym_of)=="genus")) synonym_of$genus <- NULL
        if(!any(names(synonym_of)=="species")) synonym_of$species <- NULL
        if(!any(names(synonym_of)=="id")) synonym_of$id <- NULL

        syn_searched <-
          query_taxa(genus = synonym_of$genus,
                                    species =synonym_of$species,
                                    ids = synonym_of$id, extract_traits = F)

        print(syn_searched)
        if(nrow(syn_searched)>1) stop("More than 1 taxa as synonym. Select only one.")
        if(nrow(syn_searched)==0) stop("No taxa found in the dictionnary. Select one.")

        update_dico_name(new_id_diconame_good = syn_searched$idtax_good_n, id_search = new_entry$idtax_n,
                         ask_before_update = FALSE, add_backup = FALSE, show_results = FALSE)

      } else {
        # update_dico_name(new_id_diconame_good = new_entry$id_n, id_search = new_entry$id_n,
        #                  ask_before_update = FALSE, add_backup = FALSE, show_results = FALSE)
      }

      # print(dplyr::tbl(mydb, "table_taxa") %>%
      #         dplyr::collect() %>%
      #         dplyr::filter(idtax_n == max(idtax_n)))

      print(dplyr::tbl(mydb_taxa, "table_taxa") %>%
              dplyr::filter(idtax_n == !!new_entry$idtax_n) %>%
              collect() %>%
              as.data.frame())
    }

  } else {

    cli::cli_alert_warning("NO ADDED ENTRY")
  }
}


#' Choose growth forms
#'
#' Return a tibble of growth form chosen by hierarchy
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#'
#' @return A tibble
#' @export
choose_growth_form <- function() {

  growth_form_cat <- query_trait(pattern = "growth")

  condition_hierarchical <- sapply(strsplit(growth_form_cat$traitdescription, 'if '), `[`, 2)
  condition_hierarchical <- sapply(strsplit(unlist(condition_hierarchical), '[.]'), `[`, 1)

  growth_form_cat <-
    growth_form_cat %>%
    mutate(condition_hierarchical = condition_hierarchical)

  all_growth_form <- vector('list', 10)

  first_level <- choice_trait_cat(id_trait =  growth_form_cat %>%
                                    filter(trait == "growth_form_level_1") %>%
                                    pull(id_trait))

  if (!any(is.na(first_level))) {

    all_growth_form[[1]] <- first_level

    second_level <- choice_trait_cat(id_trait = growth_form_cat %>%
                                       filter(condition_hierarchical == first_level$value) %>%
                                       pull(id_trait))

    if (!all(is.na(second_level))) if(!is.na(second_level$value)) all_growth_form[[2]] <- second_level

    if (!any(is.na(second_level))) {

      id_t <- growth_form_cat %>%
        filter(condition_hierarchical == second_level$value) %>%
        pull(id_trait)

      if (length(id_t) > 0) {

        third_level <- choice_trait_cat(id_trait = id_t)

      } else {

        third_level <- NA

      }


      if (!any(is.na(third_level))) {

        all_growth_form[[3]] <- third_level

        filtered_growth_form <-
          growth_form_cat %>%
          filter(condition_hierarchical == third_level$value)

        if (nrow(filtered_growth_form)  > 0) {

          fourth_level <- choice_trait_cat(id_trait =  filtered_growth_form %>%
                                             pull(id_trait))

          all_growth_form[[4]] <- fourth_level

        } else {

          fourth_level <- NA

        }

        if (!any(is.na(fourth_level))) {

          filtered_growth_form <-
            growth_form_cat %>%
            filter(condition_hierarchical == fourth_level$value)

          if (nrow(filtered_growth_form)  > 0) {

            fith_level <- choice_trait_cat(id_trait =  filtered_growth_form %>%
                                             pull(id_trait))

            all_growth_form[[5]] <- fith_level

          } else {

            fith_level <- NA

          }
        }
      }
    }
  }

  all_growth_form <-
    bind_rows(all_growth_form[unlist(lapply(all_growth_form, function(x) !is.null(x)))])

  return(all_growth_form)

}



choice_trait_cat <- function(id_trait) {

  trait_selected <-
    query_trait(id_trait = id_trait)

  print(tibble(description = unlist(stringr::str_split(trait_selected$traitdescription, pattern = "[.]"))) %>%
          kableExtra::kable(format = "html", escape = F) %>%
          kableExtra::kable_styling("striped", full_width = F) %>%
          print())

  print(trait_selected$list_factors[[1]])

  cli::cli_alert_info("Choose any {trait_selected$trait}")
  first_level_choice <-
    readline(prompt = "")

  if (first_level_choice != "") {

    suppressWarnings(if(is.na(as.numeric(first_level_choice)))
      stop(paste("Choose a number for selecting", trait_selected$trait)))

    selected_value <-
      trait_selected$list_factors[[1]] %>%
      slice(as.numeric(first_level_choice)) %>%
      mutate(trait = trait_selected$trait)

  } else {

    selected_value <- NA

  }

  return(selected_value)

}



#' Query in taxa trait table
#'
#' Query in taxa trait table by id or pattern
#'
#' @return tibble with query results
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param id_trait integer id of trait to select
#' @param pattern string vector trait to look for in the table
#'
#' @export
query_trait <- function(id_trait = NULL, pattern = NULL) {

  if(!exists("mydb_taxa")) call.mydb.taxa(pass = "Anyuser2022", user = "common")

  if (!is.null(id_trait)) {
    cli::cli_alert_info("query trait by id")

    table_traits <- try_open_postgres_table(table = "table_traits", con = mydb_taxa)

    valuetype <-
      table_traits %>%
      dplyr::filter(id_trait == !!id_trait) %>%
      dplyr::collect()
  }

  if (is.null(id_trait) & !is.null(pattern)) {

    cli::cli_alert_info("query trait by string pattern")

    sql <- glue::glue_sql(paste0("SELECT * FROM table_traits WHERE trait ILIKE '%", pattern, "%'"))

    valuetype <- func_try_fetch(con = mydb_taxa, sql = sql)

    # rs <- DBI::dbSendQuery(mydb_taxa, )
    # res <- DBI::dbFetch(rs)
    # DBI::dbClearResult(rs)
    # valuetype <- dplyr::as_tibble(res)

  }

  valuetype <-
    valuetype %>%
    dplyr::mutate(list_factors = purrr::pmap(
      .l = .,
      .f = function(factorlevels,
                    ...) {

        as_tibble(unlist(stringr::str_split(factorlevels, ", ")))

      }
    ))

  return(valuetype)

}



#' Query in colnam table
#'
#' Query in colnam table by id or pattern
#'
#' @return tibble with query results
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param id_trait integer id of trait to select
#' @param pattern string vector trait to look for in the table
#'
#' @export
query_colnam <- function(id_colnam = NULL, pattern = NULL) {

  if(!exists("mydb")) call.mydb()

  if (!is.null(id_colnam)) {
    cli::cli_alert_info("query colnam by id")

    table_colnam <- try_open_postgres_table(table = "table_colnam", con = mydb)

    valuetype <-
      table_colnam %>%
      dplyr::filter(id_table_colnam %in% !!id_colnam) %>%
      dplyr::collect()
  }

  if (is.null(id_colnam) & !is.null(pattern)) {

    cli::cli_alert_info("query colnam by string pattern")

    sql <- glue::glue_sql(paste0("SELECT * FROM table_colnam WHERE colnam ILIKE '%", pattern, "%'"))

    valuetype <- func_try_fetch(con = mydb, sql = sql)
  }

  return(valuetype)

}


#' Query link between specimens and individuals
#'
#' Query links between specimens and individuals
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id_ind integer
#' @param id_specimen integer
#'
#' @return No values
#' @export
query_link_individual_specimen <- function(id_ind = NULL,
                                             id_specimen = NULL) {

  if(!exists("mydb")) call.mydb()


  if(!is.null(id_ind)) {

    selected_link <-
      dplyr::tbl(mydb, "data_link_specimens") %>%
      dplyr::filter(id_n %in% !!id_ind) %>%
      dplyr::collect() %>%
      as.data.frame()

  }

  if(!is.null(id_specimen)) {

    selected_link <-
      dplyr::tbl(mydb, "data_link_specimens") %>%
      dplyr::filter(id_specimen %in% !!id_specimen) %>%
      dplyr::collect() %>%
      as.data.frame()

  }

  return(selected_link)

}






.delete_taxa <- function(id) {

  if (exists("mydb_taxa")) rm(mydb_taxa)
  if (!exists("mydb_taxa")) call.mydb.taxa()

  # DBI::dbExecute(mydb,
  #                "DELETE FROM table_taxa WHERE idtax_n=$1", params=list(id)
  # )

  query <- "DELETE FROM table_taxa WHERE MMM"
  query <-
    gsub(
      pattern = "MMM",
      replacement = paste0("idtax_n IN ('",
                           paste(unique(id), collapse = "', '"), "')"),
      x = query
    )

  rs <- DBI::dbSendQuery(mydb_taxa, query)
  DBI::dbClearResult(rs)
}





#' Delete an entry in traits measurements table
#'
#' Delete an entry in traits measurements table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
#' @export
.delete_entry_sp_trait_measure <- function(id) {

  if (!exists("mydb_taxa")) call.mydb.taxa()

  tbl <- "table_traits_measures"
  sql <- glue::glue_sql("SELECT * FROM {`tbl`} WHERE id_trait_measures IN ({vals*})",
                       vals = id, .con = mydb_taxa)

  valuetype <- func_try_fetch(con = mydb_taxa, sql = sql)

}




#' Divide 1 ha square plots into 25 squares subplots of 400m² following a regular 5*5 grid
#'
#' @param coordinates_sf a spatial object representing the plot geometries, either a \code{SpatialPolygonsDataFrame} or \code{sf} object. Each line should correspond to a single plot.
#' @param plot_name the name as character of the column where the plot name are stored. Default value is 'plot_name'.
#' @return A \code{sf} object with the 25 subplots geometries with 2 fields : sous_plot_name and plot_name for each plot.
#' @details The function takes either a \code{SpatialPolygonsDataFrame} or \code{sf} object containing the plot geometries and the plot names. For each plot, it first
#' identifies the 4 corners, then creates the 25 square subplots following a regular 5*5 grid. The subplots are named using the xy coordinates inside the plot, starting
#' from 0_0 for the southeasterly corner to 80_80 for the northwesternly.
#' @examples
#' ## Test 1
#'
#' # Define the coordinates of the 4 corners
#' x1 <- c(0, 1, 1, 0, 0)
#' y1 <- c(0, 0, 1, 1, 0)
#' x2 <- c(2, 2, 3, 3, 2)
#' y2 <- c(1, 2, 2, 1, 1)
#'
#' # Combine x and y coordinates into matrix
#' coords1 <- cbind(x1, y1)
#' coords2 <- cbind(x2, y2)
#'
#' # Rotate by 20 degrees the matrix coord1
#' angle <- pi/9  # angle in radians
#' rotation_mat <- matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)), nrow = 2)
#' coords1 <- coords1 %*% rotation_mat
#'
#' # Create SF object
#' poly1 <- st_sfc(st_polygon(list(coords1)))
#' poly2 <- st_sfc(st_polygon(list(coords2)))
#'
#' coordinates_sf <-
#'   st_as_sf(data.frame(
#'     plot_name = c('Plot_001', 'Plot_002'),
#'     geometry = c(poly1, poly2)
#'   ))
#'
#' #Plot
#' plot(coordinates_sf$geometry)
#'
#' # Divide the plot into smaller squares
#' sub_plot <- divid_plot(coordinates_sf = coordinates_sf, plot_name = 'plot_name')
#'
#' # Plot the plots and the result subplots
#' par(mfrow = c(1, 2))
#' plot(coordinates_sf$geometry, main = "Plots")
#' plot(sub_plot$geometry, main = "Subplots")
#'
#' # Plot the plots and the result subplots
#' library(ggplot2)
#' ggplot(sub_plot)  +
#'   geom_sf() +
#'   scale_fill_continuous(type = 'viridis')+
#'   geom_sf_text(aes(label = as.character(sous_plot_name)))
#'
#' # Extract datas
#' x <- query_plots(locality_name = "Mbalmayo", extract_individuals = F, show_all_coordinates = TRUE)
#'
#' coordinates_sf <- x$coordinates_sf
#' sub_plot <- divid_plot(coordinates_sf,'plot_name')
#'
#' par(mfrow = c(1, 1))
#' for(i in 1:length(unique(sub_plot$plot_name))) {
#'
#'   print(ggplot(sub_plot %>% filter(plot_name == unique(plot_name)[i]))  +
#'           geom_sf() +
#'           scale_fill_continuous(type = 'viridis')+
#'           geom_sf_label(aes(label = as.character(sous_plot_name)))  +
#'           ggtitle(paste(unique(unique(sub_plot$plot_name)[i]))) )
#' }
#'
#' @importFrom forcats fct_recode
#' @importFrom sf st_polygon st_sf
#'
#' @export
divid_plot <- function (coordinates_sf, plot_name = 'plot_name') {

  # Get plot data by name
  names <- coordinates_sf[[plot_name]]
  n <- 5

  for (i in 1:length(names)){

    plot <- coordinates_sf[i,]

    #####################################################################
    ##### STEP 1 : EXTRACT THE 4 CORNERS OF THE PLOT i
    #####################################################################

    coord <- plot %>%
      st_coordinates() %>%
      as.data.frame() %>%
      distinct(X, Y, .keep_all = TRUE) %>%
      mutate ( corner = case_when(
        X == min(X[which(Y %in% sort(Y)[1:2])]) ~ 1, #'bottom left',
        X == max(X[which(Y %in% sort(Y)[1:2])]) ~ 4, #'bottom right',
        X == min(X[which(Y %in% sort(Y)[3:4])]) ~ 2, #'top left',
        X == max(X[which(Y %in% sort(Y)[3:4])]) ~ 3 )) %>% #'top right'
      arrange(corner) %>%
      select(-c(corner,L1,L2)) %>%
      as.matrix()


    #####################################################################
    ##### STEP 2 : CREATE THE 25 SUBPLOT SQUARES
    #####################################################################

    y_length <- (coord[2,2]-coord[1,2]) / n
    x_length <- (coord[4,1]-coord[1,1]) / n
    x_shift <- (coord[2,1]-coord[1,1]) / n
    y_shift <- (coord[4,2]-coord[1,2]) / n

    for (y in 1:n){

      for (x in 1:n){

        tmp  <-   sf::st_polygon(
          list(
            rbind(
              c(coord[1,1]+(x-1)*x_length+(y-1)*x_shift,coord[1,2]+(y-1)*y_length+(x-1)*y_shift),
              c(coord[1,1]+(x-1)*x_length+(y)*x_shift,coord[1,2]+y*y_length+(x-1)*y_shift),
              c(coord[1,1]+x*x_length+(y)*x_shift,coord[1,2]+y*y_length+(x)*y_shift),
              c(coord[1,1]+x*x_length+(y-1)*x_shift,coord[1,2]+(y-1)*y_length+(x)*y_shift),
              c(coord[1,1]+(x-1)*x_length+(y-1)*x_shift,coord[1,2]+(y-1)*y_length+(x-1)*y_shift)
            )
          )
        )

        assign (paste('smaller_square',x,y, sep = "_"), tmp)

      }

    }

    #####################################################################
    #### STEP 3 : ASSIGN plot_name AND subplot_name TO SUBPLOTS
    #####################################################################

    nrows <- 25

    sub_plot <- st_sf(crs = st_crs(plot),
                      sous_plot_name = 1:nrows,
                      geometry = st_sfc(lapply(1:nrows,
                                               function(x) st_geometrycollection())
                      )
    ) # Create a fake multipolygons of 25 object with the good crs

    # Add the right 25 polygon geometry
    for (j in 1:25) {sub_plot$geometry[j] <- mget(ls(pattern = "smaller_square"))[[j]]}

    sub_plot <- sub_plot %>% mutate(plot_name = names[i]) # Add the plot name

    sub_plot$sous_plot_name <- as.character(sub_plot$sous_plot_name) # Renamme the subplot id

    sub_plot$sous_plot_name <- fct_recode(sub_plot$sous_plot_name,
                                          "0_0" = '1',
                                          "0_20" = '2',
                                          "0_40" = '3',
                                          "0_60" = '4',
                                          "0_80" = '5',
                                          "20_0" = '6',
                                          "20_20" = '7',
                                          "20_40" = '8',
                                          "20_60" = '9',
                                          "20_80" = '10',
                                          "40_0" = '11',
                                          "40_20" = '12',
                                          "40_40" = '13',
                                          "40_60" = '14',
                                          "40_80" = '15',
                                          "60_0" = '16',
                                          "60_20" = '17',
                                          "60_40" = '18',
                                          "60_60" = '19',
                                          "60_80" = '20',
                                          "80_0" = '21',
                                          "80_20" = '22',
                                          "80_40" = '23',
                                          "80_60" = '24',
                                          "80_80" = '25'
    )

    sub_plot$sous_plot_name <- factor(sub_plot$sous_plot_name,
                                      levels = c("0_0","0_20","0_40","0_60","0_80",
                                                 "20_0","20_20","20_40","20_60","20_80",
                                                 "40_0","40_20","40_40","40_60","40_80",
                                                 "60_0","60_20","60_40","60_60","60_80",
                                                 "80_0","80_20","80_40","80_60","80_80"))





    assign(paste('subplot',names[i],sep = '_'),sub_plot)

  }

  sub_plot <- do.call(rbind,mget(ls(pattern = "subplot"))) # combine all multipolygons in the same sf
  print(plot(sub_plot$geometry, col = sub_plot$sous_plot_name))
  return (sub_plot)
}








#' Check the order of subplots in a given data frame
#'
#' This function checks the order of subplots in a given data frame against a predefined order.
#' It also checks if there are any missing or too much subplots. If there is any issue, it plots the mean of
#' the indicator variable 'ind_num_sous_plot' for each subplot and displays it in a spatial plot to see where the
#' errors from thanks to an indicator 'check.
#'
#' @param ind.extract A data frame containing the indicator variable 'ind_num_sous_plot' and the names of the plots and subplots in the columns 'plot_name' and 'sous_plot_name' respectively.
#' @param sub_plot A \code{sf} object containing the names of the plots and subplots in the columns 'plot_name' and 'sous_plot_name' respectively.
#' @return The function returns a message indicating if the order of subplots in the data frame is correct or if there is a problem with the order. If there is any issue, it plots the errors.
#'
#' @author Hugo Leblanc
#'
#' @examples
#' ## Test 1
#' # Define the data for 2 plots
#' df <- data.frame(plot_name = c(rep("plot1", each = 250),rep("plot2", each = 250)),
#'                  sous_plot_name = rep(c("0_0","20_0","40_0","60_0","80_0",
#'                                         "80_20","60_20", "40_20","20_20","0_20",
#'                                         "0_40","20_40","40_40","60_40","80_40",
#'                                         "80_60","60_60","40_60","20_60","0_60",
#'                                         "0_80","20_80","40_80","60_80","80_80"),
#'                                       each = 10),
#'                  ind_num_sous_plot = c(1:250, 1:50, 76:100, 51:75, 101:250))
#'
#' # Define the 2 plots geometry
#' square1 <- st_polygon(list(rbind(c(0,0), c(0,1), c(1,1), c(1,0), c(0,0))))
#' square2 <- st_polygon(list(rbind(c(2,2), c(2,3), c(3,3), c(3,2), c(2,2))))
#'
#' # Define the size of the subplot sides
#' n <- 5
#' side_length <- 1/n
#'
#' # Define the subplot geometries
#' for (j in c(0,2)){
#'
#'   for (x in 1:n){
#'
#'     for (i in 1:n){
#'
#'       tmp  <-   st_polygon(
#'         list(
#'           rbind(
#'             c(j+(i-1)*side_length,j+(x-1)*side_length),
#'             c(j+(i-1)*side_length,j+x*side_length),
#'             c(j+i*side_length,j+x*side_length),
#'             c(j+i*side_length,j+(x-1)*side_length),
#'             c(j+(i-1)*side_length,j+(x-1)*side_length)
#'           )
#'         )
#'       )
#'
#'       if(j == 0){assign (paste('smaller_square1',x,i, sep = "_"), tmp)}
#'       else{assign (paste('smaller_square2',x,i, sep = "_"), tmp)}
#'
#'     }
#'
#'   }
#' }
#'
#'
#' # Define the predefinite order of subplots
#' order <- c("0_0",
#'            "20_0",
#'            "40_0",
#'            "60_0",
#'            "80_0",
#'            "0_20",
#'            "20_20",
#'            "40_20",
#'            "60_20",
#'            "80_20",
#'            "0_40",
#'            "20_40",
#'            "40_40",
#'            "60_40",
#'            "80_40",
#'            "0_60",
#'            "20_60",
#'            "40_60",
#'            "60_60",
#'            "80_60",
#'            "0_80",
#'            "20_80",
#'            "40_80",
#'            "60_80",
#'            "80_80")
#'
#' # Assign names to geometries
#' sub_plot <- st_sf(
#'   plot_name = rep(c("plot1","plot2"),each = 25),
#'   sous_plot_name = rep(order,2),
#'   geometry = st_sfc(lapply(1:25,function(x) st_geometrycollection()))
#' )
#'
#' for (j in 1:25) {sub_plot$geometry[j] <- mget(ls(pattern = "smaller_square1"))[[j]]}
#' for (j in 1:25) {sub_plot$geometry[j+25] <- mget(ls(pattern = "smaller_square2"))[[j]]}
#'
#' rm(list=ls(pattern = "smaller_square1"))
#' rm(list=ls(pattern = "smaller_square2"))
#'
#' # Plot
#' ggplot(sub_plot)  +
#'   #geom_sf() +
#'   scale_fill_continuous(type = 'viridis')+
#'   geom_sf_label(aes(label = as.character(sous_plot_name)))  +
#'   ggtitle(paste(unique(tmp$plot_name)))
#'
#' # Check the order of subplots
#' test.order.subplot(df, sub_plot)
#'
#' ## Test 2
#'
#' library(plotsdatabase)
#'
#' # Extract datas
#' x <- query_plots(locality_name = "Mbalmayo", extract_individuals = TRUE, show_all_coordinates = TRUE)
#'
#' coordinates_sf <- x$coordinates_sf
#' ind.extract <- x$extract
#'
#' sub_plot <- divid_plot(coordinates_sf,'plot_name')
#' test.order.subplot(ind.extract, sub_plot)
#'
#' @import ggplot2
#' @import dplyr
#' @import sf
#'
#' @export
#'
test.order.subplot <- function(ind.extract, sub_plot){

  order <- c("0_0",
             "20_0",
             "40_0",
             "60_0",
             "80_0",
             "80_20",
             "60_20",
             "40_20",
             "20_20",
             "0_20",
             "0_40",
             "20_40",
             "40_40",
             "60_40",
             "80_40",
             "80_60",
             "60_60",
             "40_60",
             "20_60",
             "0_60",
             "0_80",
             "20_80",
             "40_80",
             "60_80",
             "80_80")

  for (i in 1:length(unique(ind.extract$plot_name))){

    tmp <- ind.extract %>%
      filter (plot_name == unique(ind.extract$plot_name)[i]) %>%
      group_by(plot_name, sous_plot_name) %>%
      summarise(mean_id = mean(ind_num_sous_plot),
                plot_name = unique(plot_name)) %>%
      ungroup()

    tmp <- merge(tmp, sub_plot, by=c('plot_name','sous_plot_name'), all.x = TRUE) %>%
      arrange(mean_id)


    if(length(tmp$sous_plot_name) != length(order) ) {

      print(paste(unique(ind.extract$plot_name)[i], '  : MISSING OR TOO MUCH SUBPLOTS'))

      if (length(tmp$sous_plot_name) > length(order)){
        nmin <- length(order)
        nmax <- length(tmp$sous_plot_name)
        tmp_order <- order[nmin+1:nmax]
        tmp_order[nmin+1:nmax] <- NA
      }else{
        tmp_order <- order[1:length(tmp$sous_plot_name)]
      }


      tmp <- st_as_sf(tmp) %>%
        mutate(check = case_when(
          sous_plot_name == tmp_order ~ 'IT SEEMS GOOD',
          T ~ 'IT SEEMS BAD'

        ))

      print(ggplot(tmp) +
              geom_sf(aes(fill = mean_id)) +
              scale_fill_continuous(type = 'viridis')+
              geom_sf_label(aes(label = as.character(mean_id), col = check))  +
              ggtitle(paste(unique(tmp$plot_name))))

    }else{

      if (FALSE %in% unique(tmp$sous_plot_name == order) ){

        print(paste(unique(ind.extract$plot_name)[i], '  : ORDER SUBPLOTS PROBLEM'))


        tmp <- st_as_sf(tmp) %>%
          mutate(check = case_when(
            sous_plot_name == order ~ 'IT SEEMS GOOD',
            T ~ 'IT SEEMS BAD'

          ))

        print(ggplot(tmp) +
                geom_sf(aes(fill = mean_id)) +
                scale_fill_continuous(type = 'viridis')+
                geom_sf_label(aes(label = as.character(mean_id), col = check))  +
                ggtitle(paste(unique(tmp$plot_name))))

      } else {

        print(paste(unique(ind.extract$plot_name)[i], '  : OK'))

      }
    }
  }
}


#' print table as html in viewer
#'
#' print table as html in viewer reordered
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param res_print tibble
#'
#'
#' @return print html in viewer
print_table <- function(res_print) {

  res_print <-
    res_print %>%
    mutate(across(where(is.character), ~ tidyr::replace_na(., "")))

  res_print <- suppressMessages(as_tibble(cbind(columns = names(res_print), record = t(res_print)),
                                          .name_repair = "universal"))

  res_print %>%
    kableExtra::kable(format = "html", escape = F) %>%
    kableExtra::kable_styling("striped", full_width = F) %>%
    print()

}









rm_field <- function(data, field) {

  if (any(names(data) %in% field)) {

    field <-
      field[which(field %in% names(data))]

    data <-
      data %>%
      dplyr::select(-all_of(field))

  }

  return(data)

}



#' Add 1ha IRd plot coordinates
#'
#' print table as html in viewer reordered
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param dataset tibble
#' @param ddlat column name of dataset containing latitude in decimal degrees
#' @param ddlon column name of dataset containing longitude in decimal degrees
#' @param launch_add_data whether addd data or not
#' @param X_theo column that contain the X quadrat name
#' @param Y_theo column that contain the Y quadrat name
#' @param check_existing_data check if data already exists
#' @param add_cols string character vectors with columns names of dataset of additonal information
#' @param cor_cols string character vectors with colums names corresponding to add_cols
#' @param collector_field string vector of size one with column name containing the name of the person collecting data
#'
#' @return print html in viewer
#' @export
add_plot_coordinates <-
  function(dataset,
           ddlat = "Latitude",
           ddlon = "Longitude",
           launch_add_data = FALSE,
           X_theo = "X_theo",
           Y_theo = "Y_theo",
           check_existing_data = TRUE,
           add_cols = NULL,
           cor_cols = NULL,
           collector_field = NULL) {
    
    X_theo_p <- dplyr::sym(X_theo)
    Y_theo_p <- dplyr::sym(Y_theo)
    
    dataset <- 
      dataset %>% 
      mutate(quadrat = paste(!!X_theo_p, !!Y_theo_p, sep = "_"))
    
    all_q <- dataset %>%
      distinct(quadrat) %>% pull()
    
    all_cols <- c(ddlat, ddlon)
    
    res_l <- vector('list', length(all_cols))
    for (i in 1:length(all_cols)) {
      col_s <- dplyr::sym(all_cols[i])
      
      if (!any(names(dataset) == col_s))
        stop(glue::glue("{col_s} column not found"))
      
      if (i == 1)
        names_pref <- "ddlat_plot_X_Y_"
      if (i == 2)
        names_pref <- "ddlon_plot_X_Y_"
      
      dataset <-
        dataset %>%
        mutate(!!col_s := as.numeric(!!col_s))
      
      res_l[[i]] <-
        tidyr::pivot_wider(
          data = dataset,
          names_from = quadrat,
          values_from = !!col_s,
          names_prefix = names_pref
        ) %>%
        group_by(plot_name) %>%
        summarise(across(starts_with(names_pref), ~ mean(.x, na.rm = TRUE)),
                  across(all_of(add_cols), ~ first(.x)),
                  across(all_of(collector_field), ~ first(.x)))
      
      print(res_l[[i]])
      
      if (launch_add_data) {
        
        add_subplot_features(new_data = res_l[[i]], 
                             col_names_select = add_cols, 
                             col_names_corresp = cor_cols, 
                             plot_name_field = "plot_name", 
                             collector_field = collector_field,
                             subplottype_field = res_l[[i]] %>% 
                               dplyr::select(starts_with("ddl")) %>% names(), 
                             add_data = TRUE,
                             ask_before_update = FALSE,
                             check_existing_data = check_existing_data)
        
      } else {
        cli::cli_alert_danger("No data added because launch_add_data is FALSE")
      }
    }
    
    return(res_l)
    
  }
