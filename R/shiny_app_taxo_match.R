
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
          mutate(id_data = seq(1, nrow(DATA), 1))
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

