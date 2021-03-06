library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)

ui <- fluidPage(
    theme = shinytheme("cerulean"),
    titlePanel("Therapeutically targeting aging slows age-related bone loss in mice."),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "query_dataset",
                label = "Select dataset",
                choices = c("Femur", "Kyphosis", "Metabolic", "Survival", "Weight")
            ),
            selectInput(
                inputId = "query_result_type",
                label = "Select result type",
                choices = c("Aging trajectories", "Sample Size calculations")
            ),
            conditionalPanel(
                condition = "input.query_result_type == 'Aging trajectories'",
                selectInput(inputId = 'query_intervention', 
                            label = "Select intervention", 
                            choices = c("Controls", "Beta Sito-sterol", "Clioquinol", "HBX", "Lithium"))
            ),
            conditionalPanel(
                condition = "input.query_result_type == 'Sample Size calculations' & (input.query_dataset == 'Femur' | input.query_dataset == 'Metabolic')",
                selectizeInput(inputId = 'query_trait', 
                            label = "Select trait", 
                            multiple = FALSE,
                            choices = NULL)
            )
        ),
        mainPanel(
            tabsetPanel(id = "myTabset",
                tabPanel(title = "Aging trajectories", 
                         value = "Aging trajectories", 
                         DT::dataTableOutput("table_age")),
                tabPanel(title = "Sample size calculations", 
                         value = "Sample Size calculations", 
                         DT::dataTableOutput("table_ss"),
                         div("Click a sample size estimate in the table for text describing the sample size calculation.", style = "color:blue; font-size:large"),
                         textOutput("power_text")),
                tabPanel("Results and Code", includeMarkdown("data/README_code.md"))
            )
        )
    )
)


server <- function(input, output, session) {
    
    observeEvent(req((input$query_dataset == "Kyphosis" | input$query_dataset == "Survival" | input$query_dataset == "Weight") & input$query_result_type == "Sample Size calculations"), {
        showModal(modalDialog(
            title = "Usage notification",
            "Sample size estimates not available for kyphosis, survival, or weight.",
            footer = modalButton("OK")
            )
        )
    })
    
    observeEvent(input$query_dataset, {
        query_dataset_cleaned <- switch(input$query_dataset,
                                        "Femur" = "femur",
                                        "Metabolic" = "metabolic")
        if(input$query_dataset == "Femur" | input$query_dataset == "Metabolic"){
            trait_list <- read_rds(paste0("data/tables_power/", query_dataset_cleaned, "/tables_ss.rds"))
            trait_list <- bind_rows(trait_list)
            trait_vector <- sort(unique(trait_list$trait))
            updateSelectizeInput(session, 
                                 inputId = "query_trait",
                                 choices = trait_vector,
                                 server = TRUE,
                                 options = list(
                                     maxOptions = 100,
                                     maxItems = 1
                                 ))
        } else {
            trait_vector <- "None"
            updateSelectizeInput(session, 
                                 inputId = "query_trait",
                                 choices = trait_vector,
                                 server = TRUE,
                                 options = list(
                                     maxOptions = 100,
                                     maxItems = 1
                                 ))
        }
        
    })
    
    observeEvent(input$query_result_type, {
        updateTabsetPanel(session,
            inputId = "myTabset",
            selected = input$query_result_type
        )
    })
    
    output$table_age <- DT::renderDataTable({
        query_dataset_cleaned <- switch(input$query_dataset,
                                        "Femur" = "femur", 
                                        "Kyphosis" = "tortuosity",
                                        "Metabolic" = "metabolic",
                                        "Survival" = "survival",
                                        "Weight" = "weight")
        query_intervention_cleaned <- switch(input$query_intervention,
                                             "Controls" = "controls",
                                             "Beta Sito-sterol" = "BS",
                                             "Clioquinol" = "CQ",
                                             "HBX" = "HBX",
                                             "Lithium" = "L")
        tables_age <- read_rds(paste0("data/tables_age/", query_dataset_cleaned, "/tables_age.rds"))
        if(query_dataset_cleaned == "survival"){
            tables_age %>%
                filter(intervention == query_intervention_cleaned)
        } else{
            tables_age <- as_tibble(tables_age[[paste(query_dataset_cleaned, query_intervention_cleaned, sep = "_")]]) 
            tables_age %>%
                arrange(trait)
        }
    },
    options = list(lengthMenu = c(40, 100, 200), pageLength = 40, dom = 'lfrtipB', 
                   buttons = c('copy', 'csv', 'excel')), 
    escape = FALSE, 
    extensions = 'Buttons',
    rownames = FALSE
    )
    
    table_ss_rv <- reactive({
        query_dataset_cleaned <- switch(input$query_dataset,
                                        "Femur" = "femur",
                                        "Metabolic" = "metabolic")
        if(input$query_dataset == "Femur" | input$query_dataset == "Metabolic"){
            tables_ss <- read_rds(paste0("data/tables_power/", query_dataset_cleaned, "/tables_ss.rds"))
            tables_ss <- bind_rows(tables_ss)
            tables_ss %>%
                remove_rownames() %>%
                filter(trait == input$query_trait) %>%
                select(effect_size, obs_3 = N_3times, obs_4 = N_4times, 
                       obs_5 = N_5times, obs_6 = N_6times, obs_7 = N_7times, 
                       obs_8 = N_8times, obs_9 = N_9times)
        } else {
            tables_ss <- NULL
        }
        
    })
    
    output$table_ss <- DT::renderDataTable({
        table_ss_rv()
    },
    options = list(lengthMenu = c(10, 50), pageLength = 10, dom = 'lfrtipB', 
                   buttons = c('copy', 'csv', 'excel')), 
    escape = FALSE, 
    extensions = 'Buttons',
    rownames = FALSE,
    selection = list(target = "cell", mode = "single")
    )
    
    output$power_text <- renderText({
        req(nrow(input$table_ss_cells_selected) > 0)
        if(input$table_ss_cells_selected[1,2] == 0) {
            #If the first column is selected, print this
            text1 <- "Select a sample size estimate, not the effect size, for sample size calculation text."
            text1
        } else {
            text1 <- paste0("To achieve 80% power to detect a ", table_ss_rv()[input$table_ss_cells_selected[1,1],1]*100, 
                            "% difference in age-associated changes in ", input$query_trait, " between two groups with ", input$table_ss_cells_selected[1,2] + 2, 
                            " repeated observations, ",
                            round(table_ss_rv()[input$table_ss_cells_selected[1,1],input$table_ss_cells_selected[1,2] + 1]), 
                            " mice are needed in each group.")
            text1
        } 
        })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
