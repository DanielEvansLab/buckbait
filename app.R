library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)

ui <- fluidPage(
    theme = shinytheme("cerulean"),
    titlePanel("Buck Institute BAIT"),
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
                condition = "input.query_result_type == 'Sample Size calculations'",
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
                         DT::dataTableOutput("table_ss"))
            )
        )
    )
)


server <- function(input, output, session) {
    
    observeEvent(input$query_dataset, {
        query_dataset_cleaned <- switch(input$query_dataset,
                                        "Femur" = "femur")
        trait_list <- read_rds(paste0("data/tables_power/", query_dataset_cleaned, "/tables_ss.rds"))
        trait_list <- do.call(rbind, trait_list)
        trait_vector <- sort(unique(trait_list$trait))
        updateSelectizeInput(session, 
                             inputId = "query_trait",
                             choices = trait_vector,
                             server = TRUE,
                             options = list(
                                 maxOptions = 100,
                                 maxItems = 1
                             ))
    })
    
    observeEvent(input$query_result_type, {
        updateTabsetPanel(session,
            inputId = "myTabset",
            selected = input$query_result_type
        )
    })
    
    output$table_age <- DT::renderDataTable({
        query_dataset_cleaned <- switch(input$query_dataset,
                                        "Femur" = "femur")
        query_intervention_cleaned <- switch(input$query_intervention,
                                             "Controls" = "controls",
                                             "Beta Sito-sterol" = "BS",
                                             "Clioquinol" = "CQ",
                                             "Lithium" = "L")
        tables_age <- read_rds(paste0("data/tables_age/", query_dataset_cleaned, "/tables_age.rds"))
        tables_age <- as_tibble(tables_age[[paste(query_dataset_cleaned, query_intervention_cleaned, sep = "_")]]) 
        tables_age %>%
            arrange(trait)
    },
    options = list(lengthMenu = c(40, 100, 200), pageLength = 40, dom = 'lfrtipB', 
                   buttons = c('copy', 'csv', 'excel')), 
    escape = FALSE, 
    extensions = 'Buttons',
    rownames = FALSE
    )
    
    output$table_ss <- DT::renderDataTable({
        query_dataset_cleaned <- switch(input$query_dataset,
                                        "Femur" = "femur")
        tables_ss <- read_rds(paste0("data/tables_power/", query_dataset_cleaned, "/tables_ss.rds"))
        tables_ss <- do.call(rbind, tables_ss)
        tables_ss <- remove_rownames(tables_ss)
        tables_ss %>%
            filter(trait == input$query_trait) %>%
            select(last_col(), everything())
    },
    options = list(lengthMenu = c(10, 50), pageLength = 10, dom = 'lfrtipB', 
                   buttons = c('copy', 'csv', 'excel')), 
    escape = FALSE, 
    extensions = 'Buttons',
    rownames = FALSE
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
