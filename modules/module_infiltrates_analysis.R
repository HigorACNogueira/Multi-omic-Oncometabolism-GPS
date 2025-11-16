# mod_infiltrates_analysis.R

# UI Component
mod_infiltrates_analysis_ui <- function(id) {
  ns <- NS(id)  # Namespace to avoid ID conflicts
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        h3("Tumor Immune Infiltration Analysis"),
        selectizeInput(ns("target"), "Select Signature:", choices = NULL, multiple = FALSE, options = list(maxOptions = 5)),
        actionButton(ns("plot_button"), "Generate Plot", icon = icon("chart-line")),
        br(), br(),
        downloadButton(ns("download_plot"), "Download Plot", icon = icon("download"))
      ),
      mainPanel(
        # Envolver o plotOutput() com withSpinner() para mostrar carregamento
        withSpinner(plotOutput(ns("plot"), width = "1200px", height = "800px"), 
                    type = 3, color = "#2c3e50", color.background = "#FFFFFF")  # Tipo do spinner e cor
      )
    )
  )
}

# Server Component
mod_infiltrates_analysis_server <- function(id, Target) {  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Namespace
    
    # Mapping function for Omic_Layer
    map_omic_layer <- function(omic_layer) {
      mapping <- list(
        "Gene expression" = "mRNA",
        "Transcript expression" = "transcript",
        "Protein expression" = "protein",
        "Mutation" = "mutation",
        "CNV" = "cnv",
        "Methylation" = "methylation",
        "miRNA expression" = "miRNA"
      )
      return(mapping[[omic_layer]])  # Return mapped value
    }
    
    # Populate the Signature dropdown with available options
    observe({
      req(Target())  
      data <- Target()
      
      choices_named <- setNames(data$Signatures, data$Nomenclature)
      
      updateSelectizeInput(session, "target", choices = choices_named, server = TRUE)
    })
      
    # Reactive expression to store user-selected parameters
    selected_inputs <- eventReactive(input$plot_button, {
      req(input$target)  # Ensure inputs are selected
      
      data <- Target()
      selected_signature <- input$target
      
      # Automatically retrieve the corresponding Omic_layer
      omic_layer <- data %>%
        filter(Signatures == selected_signature) %>%
        pull(Omic_layer) %>%
        unique() %>%
        .[1]  # Get the first unique value
      
      list(
        Gene = selected_signature, 
        data_type = map_omic_layer(omic_layer)
      )
    })
    
    # Reactive function to generate plot
    generate_plot <- reactive({
      req(selected_inputs())
      
      vis_gene_TIL_cor(
        Gene = selected_inputs()$Gene,
        cor_method = "spearman",
        data_type = selected_inputs()$data_type,
        sig = c("B cell memory_CIBERSORT",
                "B cell naive_CIBERSORT",
                "B cell plasma_CIBERSORT",
                "Cancer associated fibroblast_XCELL",
                "Class-switched memory B cell_XCELL",
                "Common lymphoid progenitor_XCELL",
                "Endothelial cell_XCELL",
                "Eosinophil_CIBERSORT",
                "Granulocyte-monocyte progenitor_XCELL",
                "Hematopoietic stem cell_XCELL",
                "Macrophage M0_CIBERSORT",
                "Macrophage M1_CIBERSORT",
                "Macrophage M2_CIBERSORT",
                "Mast cell activated_CIBERSORT",
                "Monocyte_CIBERSORT",
                "Myeloid dendritic cell activated_CIBERSORT",
                "Myeloid dendritic cell resting_CIBERSORT",
                "Neutrophil_CIBERSORT",
                "NK cell activated_CIBERSORT",
                "NK cell resting_CIBERSORT",
                "T cell CD4+ memory activated_CIBERSORT",
                "T cell CD4+ memory resting_CIBERSORT",
                "T cell CD4+ naive_CIBERSORT",
                "T cell CD4+ Th1_XCELL",
                "T cell CD4+ Th2_XCELL",
                "T cell CD8+_CIBERSORT",
                "T cell follicular helper_CIBERSORT",
                "T cell gamma delta_CIBERSORT",
                "T cell regulatory (Tregs)_CIBERSORT"),
        Plot = TRUE  # Fixed "TRUE" -> TRUE (logical value)
      )
    })
    
    # Render the plot
    output$plot <- renderPlot({
      generate_plot()
    }, width = 1200, height = 800)  # Ensure correct rendering size
    
    # Download functionality for the generated plot
    output$download_plot <- downloadHandler(
      filename = function() {
        req(input$target, input$omic_layer)  # Ensure values are available
        paste0(
          "infiltrates_plot_", 
          input$target, "_", 
          input$omic_layer, "_", 
          Sys.Date(), ".png"
        ) %>% gsub(" ", "_", .)  # Replace spaces with underscores for safety
      },
      content = function(file) {
        png(file, width = 12, height = 8, units = "in", res = 300)  # High-resolution output
        print(generate_plot())  # Save the plot
        dev.off()
      }
    )
  })
}