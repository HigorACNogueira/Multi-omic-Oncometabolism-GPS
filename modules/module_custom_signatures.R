# =========================================================
# Module: Custom Signature Builder
# File: modules/module_custom_signatures.R
# =========================================================

mod_custom_signatures_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    fluidRow(
      column(
        width = 12,
        div(
          style = "border-bottom: 1px solid #ddd; margin-bottom: 10px;",
          h3("Custom Signature Builder", style = "margin-top: 0;"),
          p(
            "Build multi-molecule signatures by grouping rows that share the same cancer type, metabolism, ",
            "and additional metadata. Correlation is used only to distinguish positive vs. negative blocks, ",
            "as in the original aggregate_Target() function."
          )
        )
      )
    ),
    
    fluidRow(
      # Sidebar
      column(
        width = 3,
        wellPanel(
          tags$h5("Grouping configuration"),
          
          div(
            style = "background:#f7f7f7; border:1px solid #ddd; border-radius:6px; padding:8px; margin-bottom:10px;",
            tags$b("Fixed grouping columns"),
            tags$ul(
              tags$li("Cancer_types"),
              tags$li("Metabolism")
            ),
            p(
              style = "font-size:11px; color:#555; margin-bottom:0;",
              "All signatures are formed within these axes. Additional grouping columns can be added below."
            )
          ),
          
          uiOutput(ns("optional_cols_ui")),
          
          tags$hr(),
          actionButton(ns("build"), "Build signatures", class = "btn btn-primary btn-block"),
          br(), br(),
          downloadButton(ns("download_csv"), "Download signatures (.csv)", class = "btn btn-success btn-block")
        )
      ),
      
      # Main panel
      column(
        width = 9,
        wellPanel(
          fluidRow(
            column(
              width = 6,
              tags$h5("Summary"),
              verbatimTextOutput(ns("summary_text"), placeholder = TRUE)
            ),
            column(
              width = 6,
              div(
                style = "background:#f7f7f7; border:1px solid #ddd; border-radius:6px; padding:8px; margin-top:5px;",
                tags$b("Logic"),
                tags$ul(
                  tags$li("Data are grouped by Cancer_types and Metabolism"),
                  tags$li("You may add any subset of the original metadata columns"),
                  tags$li("Positive and negative correlations are aggregated separately")
                ),
                p(
                  style = "font-size:11px; color:#555; margin-bottom:0;",
                  "Within each group, Target entries form a signature and Interactions are summarized to common elements."
                )
              )
            )
          ),
          tags$hr(),
          tags$h5("Generated signatures"),
          DT::DTOutput(ns("signatures_table")) %>%
            shinycssloaders::withSpinner(type = 4)
        )
      )
    )
  )
}


mod_custom_signatures_server <- function(id, dataset_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---------------------- Helpers (from original code) ----------------------
    
    # Function to format the Target signature according to requirements
    format_gene_signature <- function(Target) {
      formatted_Target <- sapply(Target, function(gene) {
        if (grepl("-", gene)) {
          paste0("", gene, "")
        } else {
          gene
        }
      })
      signature <- paste(formatted_Target, collapse = " + ")
      paste0("(", signature, ")")
    }
    
    # Function to extract common interactions between group elements
    find_common_interactions <- function(Interactions) {
      interaction_list <- strsplit(Interactions, " / ")
      
      # Single-row group: keep original string
      if (length(interaction_list) == 1) {
        return(Interactions[1])
      }
      
      # All interactions identical?
      if (all(sapply(interaction_list, function(x) identical(x, interaction_list[[1]])))) {
        return(Interactions[1])
      }
      
      # Intersection
      common <- Reduce(intersect, interaction_list)
      
      if (length(common) > 0) {
        paste(common, collapse = " / ")
      } else {
        "No common interactions"
      }
    }
    
    # ---------------------- UI: optional grouping columns ----------------------
    
    base_cols <- c("Cancer_types", "Metabolism")
    
    # Colunas da função original que podem ser usadas como agrupamento adicional
    original_grouping_pool <- c(
      "Molecular_class", "Metabolism", "Pathways", "Metabolic_cell_death",
      "Cancer_types", "Tumor_vs_normal", "Omic_layer", "Phenotypic_layer",
      "Cox_OS_type", "Cox_DSS_type", "Cox_DFI_type", "Cox_PFI_type",
      "OS_worst_prognosis_group", "DSS_worst_prognosis_group",
      "DFI_worst_prognosis_group", "PFI_worst_prognosis_group",
      "Immune_classification"
    )
    
    observe({
      df <- dataset_reactive()
      req(df, nrow(df) > 0)
      
      # Só disponibilizar as colunas que existem no dataset e que pertencem ao pool original,
      # excluindo as fixas (Cancer_types, Metabolism) para não duplicar.
      available <- intersect(colnames(df), original_grouping_pool)
      available <- setdiff(available, base_cols)
      available <- sort(available)
      
      # Sugestão inicial: usar exatamente as colunas que você usou no aggregate_Target()
      suggested <- intersect(
        c(
          "Molecular_class", "Pathways", "Metabolic_cell_death", "Tumor_vs_normal",
          "Omic_layer", "Phenotypic_layer", "Cox_OS_type", "Cox_DSS_type",
          "Cox_DFI_type", "Cox_PFI_type", "OS_worst_prognosis_group",
          "DSS_worst_prognosis_group", "DFI_worst_prognosis_group",
          "PFI_worst_prognosis_group", "Immune_classification"
        ),
        available
      )
      
      output$optional_cols_ui <- renderUI({
        selectizeInput(
          ns("optional_cols"),
          label = "Optional grouping columns (subset of the original function):",
          choices = available,
          selected = NULL,          # ← Agora SEM colunas pré-selecionadas
          multiple = TRUE,
          options = list(
            plugins    = list("remove_button", "drag_drop"),
            placeholder = "Choose additional grouping columns…"
          )
        )
      })
    })
    
    # ---------------------- Core aggregation (module version of aggregate_Target) ----
    
    aggregate_Target_module <- function(df001, opt_cols) {
      # Agrupamento real: fixos + opcionais
      grouping_cols <- unique(c(
        base_cols,
        opt_cols
      ))
      
      # Função interna: mesma lógica de agrupamento da função original,
      # mas usando grouping_cols ao invés de um vetor fixo.
      aggregate_and_format <- function(df) {
        req(all(c("Target", "Interactions") %in% colnames(df)))
        
        df %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) %>%
          dplyr::summarise(
            Target       = format_gene_signature(Target),
            Interactions = find_common_interactions(Interactions),
            .groups      = "drop"
          )
      }
      
      # Separar correlações positivas e negativas (como no código original)
      positive <- df001 %>% dplyr::filter(.data$Correlation_rho > 0)
      negative <- df001 %>% dplyr::filter(.data$Correlation_rho < 0)
      
      positive_aggregated <- if (nrow(positive)) aggregate_and_format(positive) else NULL
      negative_aggregated <- if (nrow(negative)) aggregate_and_format(negative) else NULL
      
      if (!is.null(positive_aggregated)) {
        positive_aggregated <- positive_aggregated %>% dplyr::mutate(Correlation_type = "positive")
      }
      if (!is.null(negative_aggregated)) {
        negative_aggregated <- negative_aggregated %>% dplyr::mutate(Correlation_type = "negative")
      }
      
      aggregated_data <- dplyr::bind_rows(positive_aggregated, negative_aggregated)
      
      aggregated_data
    }
    
    # ---------------------- Reactive: build signatures ----------------------
    
    make_signatures <- eventReactive(input$build, {
      df <- dataset_reactive()
      req(df, nrow(df) > 0)
      
      # Checagens mínimas
      validate(need(all(base_cols %in% colnames(df)),
                    "Dataset must contain 'Cancer_types' and 'Metabolism'."))
      validate(need("Target"       %in% colnames(df), "'Target' column is required."))
      validate(need("Interactions" %in% colnames(df), "'Interactions' column is required."))
      validate(need("Correlation_rho" %in% colnames(df), "'Correlation_rho' column is required."))
      
      opt_cols <- input$optional_cols %||% character(0)
      
      aggregated <- aggregate_Target_module(df, opt_cols)
      validate(need(!is.null(aggregated) && nrow(aggregated) > 0,
                    "No signatures could be formed with the current configuration."))
      
      # Contar número de membros na assinatura
      members_count <- function(sig) {
        core <- gsub("[()]", "", sig)
        if (nzchar(core)) length(strsplit(core, " \\+ ", fixed = FALSE)[[1]]) else 0
      }
      
      aggregated <- aggregated %>%
        dplyr::mutate(
          Members = vapply(.data$Target, members_count, integer(1), USE.NAMES = FALSE)
        )
      
      # Ordenar por câncer, metabolismo e número de membros
      if (all(base_cols %in% colnames(aggregated))) {
        aggregated <- aggregated %>%
          dplyr::arrange(.data$Cancer_types, .data$Metabolism, dplyr::desc(.data$Members))
      } else {
        aggregated <- aggregated %>%
          dplyr::arrange(dplyr::desc(.data$Members))
      }
      
      # Renomear para bater com o seu df002_signatures
      aggregated <- aggregated %>%
        dplyr::rename(
          Multiomics_Signature = .data$Target,
          Common_interaction   = .data$Interactions
        )
      
      # Ordem das colunas: metadata chave na frente
      front_cols <- c(
        "Members",
        "Multiomics_Signature",
        "Common_interaction",
        base_cols,
        opt_cols,
        "Correlation_type"
      )
      front_cols <- front_cols[front_cols %in% colnames(aggregated)]
      other_cols <- setdiff(colnames(aggregated), front_cols)
      aggregated <- aggregated[, c(front_cols, other_cols), drop = FALSE]
      
      aggregated
    }, ignoreInit = TRUE)
    
    # ---------------------- Outputs ----------------------
    
    output$summary_text <- renderText({
      req(make_signatures())
      x   <- make_signatures()
      opt <- input$optional_cols %||% character(0)
      paste0(
        "Signatures: ", nrow(x),
        " | Columns: ", ncol(x),
        "\nGrouping: ",
        paste(
          c("Cancer_types", "Metabolism", opt),
          collapse = " + "
        )
      )
    })
    
    output$signatures_table <- DT::renderDT({
      req(make_signatures())
      DT::datatable(
        make_signatures(),
        rownames   = FALSE,
        filter     = "top",
        extensions = c("Buttons"),
        options    = list(
          pageLength = 25,
          scrollX    = TRUE,
          dom        = "Bfrtip",
          buttons    = c("copy", "csv", "excel")
        )
      )
    })
    
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("custom_signatures_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
      },
      content = function(file) {
        req(make_signatures())
        readr::write_csv(make_signatures(), file)
      }
    )
  })
}
