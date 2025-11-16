# module_signature.R

# UI do módulo
mod_signature_ui <- function(id, signature_name) {
  ns <- NS(id)  # Namespace to prevent ID conflicts
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h4("Step-by-Step Filtering"),
        uiOutput(ns("omicLayerFilterUI")),  # Omic Layer é fixo, mas dinâmico
        selectInput(ns("cancerFilter"), "2. Select Cancer Type:", choices = NULL),
        selectInput(ns("MetabolismFilter"), "3. Select Metabolism:", choices = NULL),
        selectInput(ns("PathwayFilter"), "4. Select Pathway:", choices = NULL),
        selectInput(ns("MetabolicCellDeathFilter"), "5. Select Metabolic Cell Death type:", choices = NULL),
        selectizeInput(ns("signatureFilter"), "6. Select Signature:", choices = NULL, multiple = FALSE),
        hr(),
        actionButton(ns("resetFilters"), "Reset Filters", icon = icon("redo")),
        br(), br(),
        downloadButton(ns("downloadData"), "Download Summary Results", icon = icon("download"))
      ),
      mainPanel(
        h3("Integrative Summary of Signature Analyses"),
        uiOutput(ns("summaryText")),  
        h3("Table results"),
        tableOutput(ns("selectedRowTable"))  
      )
    )
  )
}

# Server do módulo atualizado
mod_signature_server <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Use namespace to avoid ID collisions in Shiny modules
    
    # Helper function to update selectInput choices with fallback message
    update_filter <- function(input_id, choices) {
      if (length(choices) == 0) {
        choices <- "No results available"
      }
      updateSelectInput(session, input_id, choices = choices, selected = NULL)
    }
    
    # Render fixed but dynamic Omic Layer input (based on dataset content)
    output$omicLayerFilterUI <- renderUI({
      req(dataset())
      data <- dataset()
      omic_layer_value <- unique(na.omit(data$Omic_layer))[1]
      selectInput(
        ns("omicLayerFilter"),
        "1. Select Omic Layer:",
        choices = omic_layer_value,
        selected = omic_layer_value
      )
    })
    
    # Initialize Cancer Types based on the full dataset
    observe({
      req(dataset())
      data <- dataset()
      update_filter("cancerFilter", unique(na.omit(data$Cancer_types)))
    })
    
    # Upon selecting Cancer Type, update Metabolism options and reset downstream filters
    observeEvent(input$cancerFilter, {
      req(dataset(), input$cancerFilter)
      data <- dataset()
      filtered <- data[data$Cancer_types == input$cancerFilter, ]
      
      update_filter("MetabolismFilter", unique(na.omit(filtered$Metabolism)))
      update_filter("PathwayFilter", NULL)
      update_filter("MetabolicCellDeathFilter", NULL)
      update_filter("signatureFilter", NULL)
    })
    
    # After selecting Metabolism, update Pathway options and reset downstream filters
    observeEvent(input$MetabolismFilter, {
      req(dataset(), input$cancerFilter, input$MetabolismFilter)
      data <- dataset()
      filtered <- data[
        data$Cancer_types == input$cancerFilter &
          data$Metabolism == input$MetabolismFilter, ]
      
      update_filter("PathwayFilter", unique(na.omit(filtered$Pathway)))
      update_filter("MetabolicCellDeathFilter", NULL)
      update_filter("signatureFilter", NULL)
    })
    
    # After selecting Pathway, update Metabolic Cell Death options
    observeEvent(input$PathwayFilter, {
      req(dataset(), input$cancerFilter, input$MetabolismFilter, input$PathwayFilter)
      data <- dataset()
      filtered <- data[
        data$Cancer_types == input$cancerFilter &
          data$Metabolism == input$MetabolismFilter &
          data$Pathway == input$PathwayFilter, ]
      
      update_filter("MetabolicCellDeathFilter", unique(na.omit(filtered$Metabolic_cell_death)))
      update_filter("signatureFilter", NULL)
    })
    
    # After selecting Metabolic Cell Death, update Signature options
    observeEvent(input$MetabolicCellDeathFilter, {
      req(dataset(), input$cancerFilter, input$MetabolismFilter,
          input$PathwayFilter, input$MetabolicCellDeathFilter)
      data <- dataset()
      filtered <- data[
        data$Cancer_types == input$cancerFilter &
          data$Metabolism == input$MetabolismFilter &
          data$Pathway == input$PathwayFilter &
          data$Metabolic_cell_death == input$MetabolicCellDeathFilter, ]
      
      updateSelectizeInput(session, "signatureFilter",
                           choices = unique(na.omit(filtered$Multiomics_Signature)),
                           server = TRUE)  # Enable server-side loading
    })
    
    # Reactive object holding the filtered row (based on all filters selected)
    filteredData <- reactive({
      req(dataset(), input$cancerFilter, input$MetabolismFilter,
          input$PathwayFilter, input$MetabolicCellDeathFilter, input$signatureFilter)
      data <- dataset()
      filtered <- data[
        data$Cancer_types == input$cancerFilter &
          data$Metabolism == input$MetabolismFilter &
          data$Pathway == input$PathwayFilter &
          data$Metabolic_cell_death == input$MetabolicCellDeathFilter &
          data$Multiomics_Signature == input$signatureFilter, ]
      
      if (nrow(filtered) > 0) {
        return(filtered[1, , drop = FALSE])  # Return only the first matching row
      } else {
        return(NULL)
      }
    })

    
    
    # Display Summary without Bold Formatting
    output$summaryText <- renderUI({
      req(filteredData())  # Ensure data is available
      row <- filteredData()
      
      # Ensure row is not empty
      if (nrow(row) == 0) {
        return(HTML("<p><i>No data available for the selected signature.</i></p>"))
      }
      
      # Helper function to format statistical results safely, ensuring "NS" is removed
      format_stat <- function(type, pval) {
        if (!is.null(type) && !is.na(type) && type != "NS") { 
          return(paste0(type, " (p = ", sprintf("%.3g", as.numeric(pval)), ")"))
        }
        return(NULL)  # Exclude NS values
      }
      
      # Lookup cancer full name from tcga_types
      cancer_abbr <- row[["Cancer_types"]]  # This is the abbreviation (e.g., "UVM")
      cancer_full <- tcga_types$Cancer_names[match(cancer_abbr, tcga_types$Cancer_abbreviation)]
      
      # If no match found, keep the abbreviation
      if (is.na(cancer_full) || cancer_full == "") {
        cancer_full <- cancer_abbr
      }
      
      # Determine Correlation Direction
      correlation_direction <- ifelse(!is.na(row[["SCS"]]) && row[["SCS"]] == "P", "positive", "negative")
      
      # Tumor_vs_normal Status
      Tumor_vs_normal_text <- ifelse(!is.na(row[["Tumor_vs_normal"]]) && row[["Tumor_vs_normal"]] == "No_data", ".",
                                paste0(" and its element(s) show ", row[["Tumor_vs_normal"]], 
                                       " in tumor tissue compared to non-tumor tissue."))
      
      # Cox Regression Summary (Filter out NS values)
      cox_results <- Filter(Negate(is.null), c(
        format_stat(row[["Cox_OS_type"]], row[["Cox_OS_p.value"]]),
        format_stat(row[["Cox_DSS_type"]], row[["Cox_DSS_p.value"]]),
        format_stat(row[["Cox_DFI_type"]], row[["Cox_DFI_p.value"]]),
        format_stat(row[["Cox_PFI_type"]], row[["Cox_PFI_p.value"]])
      ))
      cox_summary <- if (length(cox_results) > 0) paste(cox_results, collapse = ", ") 
      else "not significant in protection or risk."
      
      # Survival Analysis Summary (Filter out NS values)
      survival_results <- Filter(Negate(is.null), c(
        format_stat(row[["OS_worst_prognosis_group"]], row[["OS_p.value"]]),
        format_stat(row[["OS_worst_prognosis_group"]], row[["DSS_p.value"]]),
        format_stat(row[["OS_worst_prognosis_group"]], row[["DFI_p.value"]]),
        format_stat(row[["OS_worst_prognosis_group"]], row[["PFI_p.value"]])
      ))
      survival_summary <- if (length(survival_results) > 0) paste(survival_results, collapse = ", ")
      else "not significant results in prognostic metrics."
      
      # Microenvironment and Immune Classification (Filter out NS values)
      microenv_text <- if (!is.na(row[["Microenvironment_classification"]]) && row[["Microenvironment_classification"]] != "NS") {
        paste0("This signature is associated with a <b>", row[["Microenvironment_classification"]], "</b> profile")
      } else { "" }
      
      immune_text <- if (!is.na(row[["Immune_classification"]]) && row[["Immune_classification"]] != "NS") {
        paste0(" and an immune phenotype classified as <b>", row[["Immune_classification"]], "</b>.")
      } else { "" }
      
      # Constructing the Summary Text Dynamically
      HTML(paste0(
        "<p>The identifier <b>", row[["Nomenclature"]], " ", "</b> represents a <b>", row[["Omic feature"]], 
        "</b> signature in <b>", cancer_full, "</b>. This signature has a <b>", correlation_direction, "</b> correlation with TMB", 
        Tumor_vs_normal_text, "</p>",
        
        "<p>Cox regression analysis suggests this signature is <b>", cox_summary, "</b>.</p>",
        
        "<p>Survival analysis shows <b>", survival_summary, "</b>.</p>",
        
        "<p>", microenv_text, immune_text, "</p>",
        
        "<p><i>For detailed statistical results, click 'Download'. To visualize the results, copy the signature name, open the 'Analysis and Plotting' dropdown menu, and select the desired analysis.</i></p>"
      ))
    })
    
    
    

    
    # Display filtered data in table
    output$selectedRowTable <- renderTable({
      req(filteredData())
      filteredData()
    })
    
    # Download handler
    output$downloadData <- downloadHandler(
      filename = function() { paste0("selected_summary_", Sys.Date(), ".tsv") },
      content = function(file) {
        data <- filteredData()
        data <- data %>% 
          mutate(Summary = HTML(paste0("<p>Signature ", data$Nomenclature, " summary...</p>")))
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    # **Reset filters properly**
    observeEvent(input$resetFilters, {
      updateSelectInput(session, "cancerFilter", choices = unique(na.omit(dataset()$Cancer_types)), selected = NULL)
      updateSelectInput(session, "MetabolismFilter", choices = NULL, selected = NULL)
      updateSelectInput(session, "PathwayFilter", choices = NULL, selected = NULL)
      updateSelectInput(session, "MetabolicCellDeathFilter", choices = NULL, selected = NULL)
      updateSelectInput(session, "signatureFilter", choices = NULL, selected = NULL)
    })
  })
}
