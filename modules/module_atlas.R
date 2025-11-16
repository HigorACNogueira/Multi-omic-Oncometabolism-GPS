# =========================================================
# Module: Atlas (Target-centric exploration)
# File: modules/module_atlas.R
# =========================================================

mod_atlas_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Atlas — Exploration of individual molecular targets"),
    fluidRow(
      column(
        width = 3,
        wellPanel(
          selectizeInput(
            ns("target_input"),
            "Target (gene / miRNA / lncRNA / transcript):",
            choices = NULL,
            multiple = FALSE,
            options = list(placeholder = "Type to search…")
          ),
          actionButton(ns("run"), "Search", class = "btn-primary"),
          tags$hr(),
          uiOutput(ns("ctab_picker_ui")),
          uiOutput(ns("omic_picker_ui")),
          uiOutput(ns("phen_picker_ui")),
          uiOutput(ns("meta_picker_ui")),
          uiOutput(ns("path_picker_ui")),
          uiOutput(ns("mcd_picker_ui")),
          actionButton(ns("apply_filters"), "Apply filters", class = "btn-info btn-sm"),
          br(), br(),
          helpText("Refine filters until a single row remains; then review the summary and export.")
        )
      ),
      column(
        width = 9,
        wellPanel(
          fluidRow(
            column(2, uiOutput(ns("box_metabolism"))),
            column(2, uiOutput(ns("box_pathways"))),
            column(2, uiOutput(ns("box_mcd"))),
            column(2, uiOutput(ns("box_cancers"))),
            column(2, uiOutput(ns("box_omics"))),
            column(2, uiOutput(ns("box_phenos")))
          )
        )
      )
    ),
    
    # --- SUMMARY ABOVE THE TABLE ---
    fluidRow(
      column(
        width = 12,
        wellPanel(
          tags$details(
            open = NA,
            tags$summary(tags$strong("Clinical / Molecular narrative")),
            uiOutput(ns("summaryText")),
            div(style = "text-align:right; margin-top:10px;",
                downloadButton(ns("dl_summary_txt"), "Download summary (.txt)"))
          )
        )
      )
    ),
    
    # --- FILTER RESULT (expect 1 row) ---
    fluidRow(
      column(
        width = 12,
        wellPanel(
          tags$h4("Filtered record"),
          uiOutput(ns("row_status")),
          DT::DTOutput(ns("row_single")),
          div(style = "text-align:right; margin-top:10px;",
              downloadButton(ns("dl_row_single"), "Download row (.csv)"))
        )
      )
    )
  )
}


# =========================================================
# SERVER
# =========================================================

mod_atlas_server <- function(id, atlas_reactive, tcga_types = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    `%||%` <- function(a, b) if (!is.null(a)) a else b
    
    # Resolve a column by preferred names (first that exists)
    pick_col <- function(df, candidates) {
      nm <- candidates[candidates %in% names(df)]
      if (length(nm)) nm[1] else NA_character_
    }
    
    # Populate Target choices
    observe({
      df <- atlas_reactive()
      req(df, nrow(df) > 0)
      validate(need("Target" %in% names(df), "Atlas dataset must contain a 'Target' column."))
      ch <- sort(unique(trimws(df$Target)))
      ch <- ch[nzchar(ch)]
      updateSelectizeInput(session, "target_input", choices = ch, server = TRUE)
    })
    
    # Filter by Target (exact case-insensitive; fallback to contains)
    filtered_all <- eventReactive(input$run, {
      df <- atlas_reactive()
      req(df, nrow(df) > 0, input$target_input)
      target <- trimws(input$target_input)
      validate(need(nzchar(target), "Please select a Target."))
      
      exact_idx <- which(toupper(df$Target) == toupper(target))
      if (length(exact_idx) > 0) {
        out <- df[exact_idx, , drop = FALSE]
        attr(out, "match_mode") <- "exact"
        return(out)
      }
      idx <- grep(target, df$Target, ignore.case = TRUE, perl = TRUE)
      validate(need(length(idx) > 0, sprintf("No rows for Target = '%s'.", target)))
      out <- df[idx, , drop = FALSE]
      attr(out, "match_mode") <- "contains"
      out
    }, ignoreInit = TRUE)
    
    # Column map
    col_map <- reactive({
      df <- atlas_reactive(); req(df)
      list(
        metabolism   = pick_col(df, c("Metabolism")),
        pathways     = pick_col(df, c("Pathways")),
        mcd          = pick_col(df, c("Metabolic_cell_death")),
        ctab         = pick_col(df, c("Cancer_types")),
        omic         = pick_col(df, c("Omic_layer")),
        phen         = pick_col(df, c("Phenotypic_layer")),
        rho          = pick_col(df, c("Correlation_rho","rho_signature")),
        p_adj        = pick_col(df, c("Correlation_p.adj","p.adj","p_adj")),
        tnt          = pick_col(df, c("Tumor_vs_normal","Tumor_vs_non_tumor","Tumor_nonTumor")),
        phenotype    = pick_col(df, c("Phenotypic_layer","Phenotypic_layers","Phenotypic_layer_signature")),
        omic_one     = pick_col(df, c("Omic_layer","Omic_layers","Omic_layer_signature")),
        microenv     = pick_col(df, c("Microenvironment_classification")),
        immune       = pick_col(df, c("Immune_classification","Immune_classification_signature")),
        molecular    = pick_col(df, c("Molecular_class")),
        interaction  = pick_col(df, c("Interactions")),
        cox_os_t     = pick_col(df, c("Cox_OS_type","Cox_OS_type_signature")),
        cox_dss_t    = pick_col(df, c("Cox_DSS_type","Cox_DSS_type_signature")),
        cox_dfi_t    = pick_col(df, c("Cox_DFI_type","Cox_DFI_type_signature")),
        cox_pfi_t    = pick_col(df, c("Cox_PFI_type","Cox_PFI_type_signature")),
        cox_os_p     = pick_col(df, c("Cox_OS_p.value","Cox_OS_p_value","Cox_OS_p")),
        cox_dss_p    = pick_col(df, c("Cox_DSS_p.value","Cox_DSS_p_value","Cox_DSS_p")),
        cox_dfi_p    = pick_col(df, c("Cox_DFI_p.value","Cox_DFI_p_value","Cox_DFI_p")),
        cox_pfi_p    = pick_col(df, c("Cox_PFI_p.value","Cox_PFI_p_value","Cox_PFI_p")),
        surv_os_t    = pick_col(df, c("OS_worst_prognosis_group")),
        surv_dss_t   = pick_col(df, c("DSS_worst_prognosis_group")),
        surv_dfi_t   = pick_col(df, c("DFI_worst_prognosis_group")),
        surv_pfi_t   = pick_col(df, c("PFI_worst_prognosis_group")),
        surv_os_p    = pick_col(df, c("OS_p.value","OS_p_value","OS_p")),
        surv_dss_p   = pick_col(df, c("DSS_p.value","DSS_p_value","DSS_p")),
        surv_dfi_p   = pick_col(df, c("DFI_p.value","DFI_p_value","DFI_p")),
        surv_pfi_p   = pick_col(df, c("PFI_p.value","PFI_p_value","PFI_p"))
      )
    })
    
    # ---------------- CTAB picker ----------------
    output$ctab_picker_ui <- renderUI({
      df <- filtered_all()
      if (is.null(df) || nrow(df) == 0) return(tags$em("Search a target to enable CTAB filter."))
      cols <- col_map(); ctab <- cols$ctab
      validate(need(!is.na(ctab) && ctab %in% names(df), "No cancer type column (Cancer_types/CTAB)."))
      cands <- sort(unique(trimws(as.character(df[[ctab]]))))
      selectInput(ns("ctab_pick"), "Cancer type:", choices = cands, selected = cands[1])
    })
    
    # --------------- Count boxes (global counts for chosen Target) ---------------
    make_box <- function(title, value) {
      htmltools::tags$div(
        style = "background:#f7f7f7;border:1px solid #ddd;border-radius:8px;padding:10px;text-align:center;",
        htmltools::tags$div(style = "font-size:11px;color:#666;", title),
        htmltools::tags$div(style = "font-size:22px;font-weight:bold;", value)
      )
    }
    
    render_count_box <- function(col_key, output_id) {
      output[[output_id]] <- renderUI({
        df <- filtered_all(); req(df)
        cols <- col_map(); ck <- cols[[col_key]]
        if (is.na(ck) || !ck %in% names(df)) return(make_box("—", 0))
        v <- df[[ck]]
        if (col_key == "mcd") v <- v[!is.na(v) & tolower(trimws(v)) != "unrelated"]
        n <- length(unique(na.omit(trimws(as.character(v)))))
        make_box(
          switch(col_key,
                 metabolism = "Metabolic processes",
                 pathways   = "Pathways",
                 mcd        = "Metabolic cell death",
                 ctab       = "Cancer types",
                 omic       = "Omic layers",
                 phen       = "Phenotypic layers", "—"),
          n
        )
      })
    }
    
    render_count_box("metabolism", "box_metabolism")
    render_count_box("pathways",   "box_pathways")
    render_count_box("mcd",        "box_mcd")
    render_count_box("ctab",       "box_cancers")
    render_count_box("omic",       "box_omics")
    render_count_box("phen",       "box_phenos")
    
    # ============== Subset by selected CTAB ==============
    rows_ctab <- reactive({
      df <- filtered_all(); req(df)
      cols <- col_map(); ctab <- cols$ctab; req(!is.na(ctab))
      pick <- input$ctab_pick; req(pick)
      df[trimws(as.character(df[[ctab]])) == trimws(pick), , drop = FALSE]
    })
    
    make_picker <- function(id, label, values) {
      if (length(values) == 0) {
        return(tags$div(
          style = "color:#999; font-size:12px; margin:6px 0 0 2px;",
          "No values available for this step."
        ))
      }
      selectInput(id, label, choices = values, selected = values[1])
    }
    
    # ------------------------- STEP 1: Omic layer -------------------------
    output$omic_picker_ui <- renderUI({
      df <- rows_ctab(); req(df)
      cols <- col_map(); k <- cols$omic
      if (is.na(k) || !(k %in% names(df))) {
        return(tags$div(
          style = "color:#999; font-size:12px; margin:6px 0 0 2px;",
          "No omic layer column found for this target."
        ))
      }
      vals <- sort(unique(na.omit(trimws(as.character(df[[k]])))))
      if (!length(vals)) {
        return(tags$div(
          style = "color:#999; font-size:12px; margin:6px 0 0 2px;",
          "No omic layers available for the selected cancer type."
        ))
      }
      make_picker(ns("omic_pick"), "Step 2 – Omic layer:", vals)
    })
    
    # ------------------- STEP 2: Phenotypic layer ------------------------
    output$phen_picker_ui <- renderUI({
      df <- rows_ctab(); req(df)
      cols <- col_map()
      k_omic <- cols$omic
      k_phen <- cols$phen
      
      if (is.null(input$omic_pick)) {
        return(tags$div(
          style = "color:#999; font-size:12px; margin:6px 0 0 2px;",
          "Select an omic layer to enable the phenotypic filter."
        ))
      }
      req(!is.na(k_omic), !is.na(k_phen))
      
      df2 <- df[trimws(as.character(df[[k_omic]])) == trimws(input$omic_pick), , drop = FALSE]
      vals <- sort(unique(na.omit(trimws(as.character(df2[[k_phen]])))))
      if (!length(vals)) {
        return(tags$div(
          style = "color:#999; font-size:12px; margin:6px 0 0 2px;",
          "No phenotypic layers available for this omic layer."
        ))
      }
      make_picker(ns("phen_pick"), "Step 3 – Phenotypic layer:", vals)
    })
    
    # --------------------- STEP 3: Metabolic process ----------------------
    output$meta_picker_ui <- renderUI({
      df <- rows_ctab(); req(df)
      cols <- col_map()
      k_omic <- cols$omic
      k_phen <- cols$phen
      k_meta <- cols$metabolism
      
      if (is.null(input$omic_pick)) {
        return(tags$div(
          style = "color:#999; font-size:12px; margin:6px 0 0 2px;",
          "Select an omic layer first."
        ))
      }
      if (is.null(input$phen_pick)) {
        return(tags$div(
          style = "color:#999; font-size:12px; margin:6px 0 0 2px;",
          "Select a phenotypic layer to enable the metabolic process filter."
        ))
      }
      req(!is.na(k_omic), !is.na(k_phen), !is.na(k_meta))
      
      df2 <- df[
        trimws(as.character(df[[k_omic]])) == trimws(input$omic_pick) &
          trimws(as.character(df[[k_phen]])) == trimws(input$phen_pick),
        , drop = FALSE
      ]
      vals <- sort(unique(na.omit(trimws(as.character(df2[[k_meta]])))))
      if (!length(vals)) {
        return(tags$div(
          style = "color:#999; font-size:12px; margin:6px 0 0 2px;",
          "No metabolic processes available for this combination."
        ))
      }
      make_picker(ns("meta_pick"), "Step 4 – Metabolic process:", vals)
    })
    
    # ------------------------- STEP 4: Pathway ----------------------------
    output$path_picker_ui <- renderUI({
      df <- rows_ctab(); req(df)
      cols <- col_map()
      k_omic <- cols$omic
      k_phen <- cols$phen
      k_meta <- cols$metabolism
      k_path <- cols$pathways
      
      if (is.null(input$omic_pick) || is.null(input$phen_pick) || is.null(input$meta_pick)) {
        return(tags$div(
          style = "color:#999; font-size:12px; margin:6px 0 0 2px;",
          "Complete the previous steps to enable pathway selection."
        ))
      }
      req(!is.na(k_omic), !is.na(k_phen), !is.na(k_meta), !is.na(k_path))
      
      df2 <- df[
        trimws(as.character(df[[k_omic]]))  == trimws(input$omic_pick)  &
          trimws(as.character(df[[k_phen]])) == trimws(input$phen_pick) &
          trimws(as.character(df[[k_meta]])) == trimws(input$meta_pick),
        , drop = FALSE
      ]
      vals <- sort(unique(na.omit(trimws(as.character(df2[[k_path]])))))
      if (!length(vals)) {
        return(tags$div(
          style = "color:#999; font-size:12px; margin:6px 0 0 2px;",
          "No pathways available for this metabolic process."
        ))
      }
      make_picker(ns("path_pick"), "Step 5 – Pathway:", vals)
    })
    
    # ----------- STEP 5 (opcional): Metabolic cell death ------------------
    output$mcd_picker_ui <- renderUI({
      df <- rows_ctab(); req(df)
      cols <- col_map()
      k_omic <- cols$omic
      k_phen <- cols$phen
      k_meta <- cols$metabolism
      k_path <- cols$pathways
      k_mcd  <- cols$mcd
      
      if (is.na(k_mcd) || !(k_mcd %in% names(df))) return(NULL)
      
      if (is.null(input$omic_pick) || is.null(input$phen_pick) ||
          is.null(input$meta_pick) || is.null(input$path_pick)) {
        return(tags$div(
          style = "color:#999; font-size:12px; margin:6px 0 0 2px;",
          "Select a pathway to enable the metabolic cell death filter (optional)."
        ))
      }
      
      df2 <- df[
        trimws(as.character(df[[k_omic]]))  == trimws(input$omic_pick)  &
          trimws(as.character(df[[k_phen]])) == trimws(input$phen_pick) &
          trimws(as.character(df[[k_meta]])) == trimws(input$meta_pick) &
          trimws(as.character(df[[k_path]])) == trimws(input$path_pick),
        , drop = FALSE
      ]
      
      vals <- trimws(as.character(df2[[k_mcd]]))
      vals <- vals[!is.na(vals)]
      vals <- vals[tolower(vals) != "unrelated"]
      vals <- sort(unique(vals))
      
      if (!length(vals)) return(NULL)
      
      make_picker(ns("mcd_pick"), "Step 6 – Metabolic cell death (optional):", c("— any —", vals))
    })
    
    # ============== APPLY FILTERS AND OBTAIN A UNIQUE ROW ============================
    rows_filtered <- eventReactive(input$apply_filters, {
      df <- rows_ctab(); req(df)
      cols <- col_map()
      
      # mandatory filters
      must <- list(
        list(col = cols$omic,  val = input$omic_pick),
        list(col = cols$phen,  val = input$phen_pick),
        list(col = cols$metabolism, val = input$meta_pick),
        list(col = cols$pathways,   val = input$path_pick)
      )
      
      for (m in must) {
        req(!is.null(m$val), !is.na(m$val))
        df <- df[trimws(as.character(df[[m$col]])) == trimws(m$val), , drop = FALSE]
      }
      
      # optional MCD
      if (!is.null(input$mcd_pick) && !identical(input$mcd_pick, "— any —")) {
        k <- cols$mcd
        if (!is.na(k) && k %in% names(df)) {
          df <- df[trimws(as.character(df[[k]])) == trimws(input$mcd_pick), , drop = FALSE]
        }
      }
      
      df
    }, ignoreInit = TRUE)
    
    # ------------------ Status + one-row table ------------------
    output$row_status <- renderUI({
      df <- rows_filtered()
      if (is.null(df)) return(HTML("<p><i>Click 'Apply filters' to fetch the record.</i></p>"))
      n <- nrow(df)
      if (n == 0) HTML("<p style='color:#b71c1c'><b>No rows match these filters.</b> Please adjust the selections.</p>")
      else if (n > 1) HTML(paste0("<p style='color:#e65100'><b>", n,
                                  " rows match these filters.</b> Refine selections until a single row remains.</p>"))
      else HTML("<p style='color:#1b5e20'><b>1 row selected.</b></p>")
    })
    
    output$row_single <- DT::renderDT({
      df <- rows_filtered(); req(df, nrow(df) >= 1)
      DT::datatable(
        df,
        rownames = FALSE,
        selection = "none",
        options = list(pageLength = 5, scrollX = TRUE, dom = "Bfrtip", buttons = c("copy","csv","excel")),
        extensions = c("Buttons")
      )
    })
    
    output$dl_row_single <- downloadHandler(
      filename = function() {
        tgt <- input$target_input %||% "target"
        ctb <- input$ctab_pick %||% "cancer"
        paste0("atlas_row_", gsub("[^A-Za-z0-9]+","_", tgt), "_", gsub("[^A-Za-z0-9]+","_", ctb), ".csv")
      },
      content = function(file) {
        df <- rows_filtered(); req(df, nrow(df) >= 1)
        readr::write_csv(df, file)
      }
    )
    
    # ===================== SUMMARY (HTML reactive) =====================
    pick_from_row <- function(row, nm) if (!is.na(nm) && nm %in% names(row)) row[[nm]][1] else NA
    fmt_stat <- function(type, pval) {
      if (!is.null(type) && !is.na(type) && type != "NS") {
        pv <- suppressWarnings(as.numeric(pval))
        pv_txt <- if (!is.na(pv)) sprintf("%.3g", pv) else as.character(pval)
        return(paste0(type, " (p = ", pv_txt, ")"))
      }
      NULL
    }
    
    summary_html <- reactive({
      df <- rows_filtered(); req(df, nrow(df) >= 1)
      row  <- df[1, , drop = FALSE]
      cols <- col_map()
      
      # Cancer full name
      ctab_val <- pick_from_row(row, cols$ctab)
      cancer_full <- ctab_val
      if (!is.null(tcga_types) && all(c("Cancer_abbreviation","Cancer_names") %in% names(tcga_types))) {
        hit <- tcga_types$Cancer_names[match(ctab_val, tcga_types$Cancer_abbreviation)]
        if (!is.na(hit) && nzchar(hit)) cancer_full <- hit
      }
      
      # Fields
      target_sym <- as.character(row[["Target"]])
      molecular  <- as.character(pick_from_row(row, cols$molecular))
      metabolism <- as.character(pick_from_row(row, cols$metabolism))
      pathways   <- as.character(pick_from_row(row, cols$pathways))
      mcd        <- as.character(pick_from_row(row, cols$mcd))
      interaction <- trimws(as.character(pick_from_row(row, cols$interaction)))
      
      rho        <- suppressWarnings(as.numeric(pick_from_row(row, cols$rho)))
      p_adj      <- suppressWarnings(as.numeric(pick_from_row(row, cols$p_adj)))
      phenotype  <- as.character(pick_from_row(row, cols$phenotype))
      omic_layer <- as.character(pick_from_row(row, cols$omic_one))
      tnt        <- trimws(as.character(pick_from_row(row, cols$tnt)))
      
      formatted_rho <- if (!is.na(rho)) sprintf("%.3f", rho) else NA
      formatted_p   <- if (!is.na(p_adj)) formatC(p_adj, format = "e", digits = 2) else NA
      
      # Intro (sem nomenclature; antes das assinaturas)
      intro <- paste0(
        "<p>The target <b>", target_sym, "</b> in <b>", cancer_full,
        "</b> is annotated as <b>", molecular, "</b> and is involved in <b>",
        metabolism, "</b> and <b>", pathways, "</b>.</p>"
      )
      
      # MCD sentence
      mcd_text <- if (!is.na(mcd) && mcd != "" && tolower(mcd) != "unrelated") {
        paste0("Also, this target is associated with <b>", mcd, "</b>.")
      } else ""
      
      # Interactions (campo 'Interactions' se existir)
      interactions_sentence <- if (!is.na(interaction) && nzchar(interaction) && interaction != "No meaningful interaction") {
        paste0("<p>Reported molecular interactions: <b>", interaction, "</b>.</p>")
      } else ""
      
      # Correlation
      corr_text <- if (!is.na(tnt) && tnt != "" && tnt != "No data") {
        paste0(
          "<p>This target shows a correlation between <b>", omic_layer, "</b> and <b>", phenotype,
          "</b> (ρ = ", formatted_rho, ", p.adj = ", formatted_p,
          "), and its expression remains <b>", tnt,
          "</b> in tumor tissue compared to non-tumor tissue.</p>"
        )
      } else {
        paste0(
          "<p>This record shows a correlation between <b>", omic_layer, "</b> and <b>", phenotype,
          "</b> (ρ = ", formatted_rho, ", p.adj = ", formatted_p, ").</p>"
        )
      }
      
      # Cox & Survival
      cox_parts <- Filter(Negate(is.null), c(
        fmt_stat(pick_from_row(row, cols$cox_os_t),  pick_from_row(row, cols$cox_os_p))  %>% { if (!is.null(.)) paste0("OS: ", .)  else NULL },
        fmt_stat(pick_from_row(row, cols$cox_dss_t), pick_from_row(row, cols$cox_dss_p)) %>% { if (!is.null(.)) paste0("DSS: ", .) else NULL },
        fmt_stat(pick_from_row(row, cols$cox_dfi_t), pick_from_row(row, cols$cox_dfi_p)) %>% { if (!is.null(.)) paste0("DFI: ", .) else NULL },
        fmt_stat(pick_from_row(row, cols$cox_pfi_t), pick_from_row(row, cols$cox_pfi_p)) %>% { if (!is.null(.)) paste0("PFI: ", .) else NULL }
      ))
      cox_summary <- if (length(cox_parts) > 0) paste(cox_parts, collapse = ", ") else "not significant in protection or risk."
      
      surv_parts <- Filter(Negate(is.null), c(
        fmt_stat(pick_from_row(row, cols$surv_os_t),  pick_from_row(row, cols$surv_os_p))  %>% { if (!is.null(.)) paste0("OS: ", .)  else NULL },
        fmt_stat(pick_from_row(row, cols$surv_dss_t), pick_from_row(row, cols$surv_dss_p)) %>% { if (!is.null(.)) paste0("DSS: ", .) else NULL },
        fmt_stat(pick_from_row(row, cols$surv_dfi_t), pick_from_row(row, cols$surv_dfi_p)) %>% { if (!is.null(.)) paste0("DFI: ", .) else NULL },
        fmt_stat(pick_from_row(row, cols$surv_pfi_t), pick_from_row(row, cols$surv_pfi_p)) %>% { if (!is.null(.)) paste0("PFI: ", .) else NULL }
      ))
      surv_summary <- if (length(surv_parts) > 0) paste(surv_parts, collapse = ", ") else "not significant results in prognostic metrics."
      
      # Microenvironment / Immune
      microenv <- trimws(as.character(pick_from_row(row, cols$microenv)))
      immune   <- trimws(as.character(pick_from_row(row, cols$immune)))
      micro_valid <- (!is.na(microenv) && microenv != "NS" && nzchar(microenv))
      immune_valid <- (!is.na(immune) && immune != "NS" && nzchar(immune))
      microimmune_text <- if (micro_valid && immune_valid) {
        paste0("This target is associated with a <b>", microenv,
               "</b> profile and an immune phenotype classified as <b>", immune, "</b>.")
      } else if (micro_valid) {
        paste0("This target is associated with a <b>", microenv, "</b> profile.")
      } else if (immune_valid) {
        paste0("This taget exhibits an immune phenotype classified as <b>", immune, "</b>.")
      } else ""
      
      HTML(paste0(
        intro,
        interactions_sentence,
        corr_text,
        "<p>", mcd_text, "</p>",
        "<p>Cox regression analysis suggests association with: <b>", cox_summary, "</b>.</p>",
        "<p>Survival analysis shows <b>", surv_summary, "</b>.</p>",
        if (nzchar(microimmune_text)) paste0("<p>", microimmune_text, "</p>") else "",
        "<p><i>Export the row below for full details.</i></p>"
      ))
    })
    
    output$summaryText <- renderUI(summary_html())
    
    output$dl_summary_txt <- downloadHandler(
      filename = function() {
        tgt <- input$target_input %||% "target"
        ctb <- input$ctab_pick %||% "cancer"
        paste0("atlas_summary_", gsub("[^A-Za-z0-9]+","_", tgt), "_", gsub("[^A-Za-z0-9]+","_", ctb), ".txt")
      },
      content = function(file) {
        # usa o reactive summary_html() e remove tags
        html <- as.character(isolate(summary_html()))
        txt  <- gsub("<[^>]+>", "", html)
        writeLines(txt, file, useBytes = TRUE)
      }
    )
  })
}
