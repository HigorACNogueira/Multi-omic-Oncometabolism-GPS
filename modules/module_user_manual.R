# --- User Manual UI ---
mod_user_manual_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
      .manual-wrapper {
        padding: 10px 0;
      }
      .manual-title {
        font-family: 'Segoe UI', 'Roboto', sans-serif;
        font-weight: 600;
        color: #1F3B4D;
        margin-bottom: 8px;
        display: flex;
        align-items: center;
        gap: 8px;
      }
      .manual-subtitle {
        font-family: 'Segoe UI', 'Roboto', sans-serif;
        font-size: 13px;
        color: #607D8B;
        margin-bottom: 15px;
      }
      .manual-text {
        font-family: 'Segoe UI', 'Roboto', sans-serif;
        font-size: 14px;
        line-height: 1.7;
        color: #333333;
        padding: 20px 24px;
        background-color: #ffffff;
        border: 1px solid #E0E0E0;
        border-radius: 12px;
        box-shadow: 0 3px 8px rgba(0,0,0,0.05);
        max-height: 750px;
        overflow-y: auto;
      }
      .manual-section-title {
        font-weight: 600;
        font-size: 15px;
        margin-top: 18px;
        margin-bottom: 6px;
        color: #1F3B4D;
      }
      .manual-section-subtitle {
        font-weight: 600;
        font-size: 14px;
        margin-top: 12px;
        margin-bottom: 4px;
        color: #37474F;
      }
      .manual-list {
        margin-top: 2px;
        margin-bottom: 6px;
        padding-left: 18px;
      }
      .manual-list li {
        margin-bottom: 3px;
      }
      .manual-faq details {
        margin-top: 6px;
        margin-bottom: 6px;
        border-left: 3px solid #CFD8DC;
        padding-left: 8px;
      }
      .manual-faq summary {
        font-weight: 600;
        cursor: pointer;
      }
      .manual-highlight {
        background-color: #E3F2FD;
        border-radius: 6px;
        padding: 8px 10px;
        font-size: 13px;
        margin-top: 8px;
        margin-bottom: 8px;
      }
    ")),
    
    div(class = "manual-wrapper",
        div(
          class = "manual-title",
          span("📘 User Manual – OncoMetabolismGPS")
        ),
        # div(
        #   class = "manual-subtitle",
        #   "How to navigate, query genes, explore signatures and their meaningful interactions, and interpret results in OncoMetabolismGPS."
        # ),
        
        div(
          class = "manual-text",
        
          
          # ------------------------------------------------------------------
          # 0. App Overview
          # ------------------------------------------------------------------
          div(class = "manual-section-title", "App Overview"),
          p("Multi-omic OncoMetabolismGPS is an interactive Shiny application developed as part of the research associated with the pre-print 
          A Multi-Omic Atlas of Convergent and Divergent Metabolic Regulatory Circuitries in Cancer across 33 cancer types. 
            The application allows:"),
          tags$ul(class = "manual-list",
                  tags$li("Simple gene and miRNA queries;"),
                  tags$li("Exploration of integrative multi-omic metabolic signatures across cancer types;"),
                  tags$li("enabling dynamic visualization of meaningful interactions, phenotypic associations, survival metrics, and regulatory circuitries")
          ),
          
          # ------------------------------------------------------------------
          # 1. Home
          # ------------------------------------------------------------------
          div(class = "manual-section-title", "1. Home"),
          div(class = "manual-section-subtitle", "Access"),
          tags$ul(class = "manual-list",
                  tags$li("The app opens on the ", tags$strong("Home"), " tab by default.")
          ),
          div(class = "manual-section-subtitle", "What you see"),
          tags$ul(class = "manual-list",
                  tags$li("Welcome message: “Welcome to OncoMetabolismGPS”;"),
                  tags$li("Brief description of the platform’s purpose;"),
                  tags$li("Illustrative image summarizing the workflow of the app.")
          ),
          
          # ------------------------------------------------------------------
          # 2. Search Your Gene
          # ------------------------------------------------------------------
          div(class = "manual-section-title", "2. Search Your Gene"),
          div(class = "manual-section-subtitle", "Access"),
          tags$ul(class = "manual-list",
                  tags$li("Click on the ", tags$strong("Search Your Gene"), " tab.")
          ),
          div(class = "manual-section-subtitle", "Main features"),
          tags$ul(class = "manual-list",
                  
                  tags$li("What this module does and how to use it"),
                  
                  tags$li("This module is designed to help you quickly find whether a gene of interest appears in our 
                  database of signatures. The idea is simple: you type in a gene symbol, 
                  and the module tells you whether that gene is part of any identified signature. 
                  If it is, the module shows you the signature, its standardized nomenclature, 
                  and a full interpretation of what that signature represents biologically and clinically."),
                  
                  
                  tags$li("Search for any gene, transcript isoforms, or mature miRNA of interest;"),
                  tags$li("Check whether it is present in our signatures;"),
                  tags$li("Visualize associated nomenclatures;"),
                  tags$li("Obtain automatic interpretation;"),
                  tags$li("Download raw data and interpretation.")
          ),
          
          div(class = "manual-section-subtitle", "Step 1: Data entry"),
          tags$ul(class = "manual-list",
                  tags$li(tags$strong("Field – Enter Gene:"), " type the gene symbol (official HGNC symbol is recommended, e.g., ", tags$code("RRM2"), ")."),
                  tags$li(tags$strong("Button – Search Gene:"), " click to submit the query."),
                  tags$li(tags$strong("Status messages:"),
                          tags$ul(class = "manual-list",
                                  tags$li(tags$em("Gene found:"), " the gene is present in at least one signature;"),
                                  tags$li(tags$em("Gene not found:"), " the gene is not present in the database.")
                          )
                  )
          ),
          
          div(class = "manual-section-subtitle", "Step 2: Viewing results"),
          p(tags$strong("Layout: three main columns")),
          
          tags$ul(class = "manual-list",
                  tags$li(tags$strong("Left column – Main results"),
                          tags$ul(class = "manual-list",
                                  tags$li("Corresponding signature: multi-omic signature and standardized signature ID;"),
                                  tags$li("Button ", tags$strong("Download Data"), ": exports a ", tags$code(".csv"), " file with the full data.")
                          )
                  ),
                  tags$li(tags$strong("Central column – Corresponding nomenclatures"),
                          tags$ul(class = "manual-list",
                                  tags$li("Lists all nomenclatures that contain the queried gene;"),
                                  tags$li("Each nomenclature code summarizes omic, clinical, and biological features;"),
                                  tags$li("Clicking a nomenclature updates the detailed interpretation.")
                          )
                  ),
                  tags$li(tags$strong("Right column – Signature interpretation"),
                          tags$ul(class = "manual-list",
                                  tags$li("Cancer type (abbreviation and full name);"),
                                  tags$li("Unique signature ID;"),
                                  tags$li("Associated metabolism and metabolic pathway;"),
                                  tags$li(" Regulatory circuitries - meaningful interactions;"),
                                  tags$li("Type of metabolic cell death;"),
                                  tags$li("Omic layers and phenotypic traits;"),
                                  tags$li("Tumor vs. non-tumor classification;"),
                                  tags$li("Prognostic association (hazard ratios);"),
                                  tags$li("Kaplan–Meier prognostic classifications;"),
                                  tags$li("Tumor microenvironment category;"),
                                  tags$li("Immune profile."),
                                  tags$li("Button ", tags$strong("Download Interpretation"), ": exports a ", tags$code(".txt"), " report.")
                          )
                  )
          ),
          
          div(class = "manual-section-subtitle", "Download options"),
          tags$ul(class = "manual-list",
                  tags$li("Raw data: ", tags$code(".csv"), ";"),
                  tags$li("Signature interpretation: ", tags$code(".txt"), ".")
          ),
          
          div(class = "manual-section-subtitle", "FAQs – Search Your Gene"),
          div(class = "manual-faq",
              tags$details(
                tags$summary("Do I need the signature code to search?"),
                p("No. Only the gene (or miRNA) name is required.")
              ),
              tags$details(
                tags$summary("Can I click multiple nomenclatures?"),
                p("Yes. Each click updates the interpretation panel for the selected nomenclature.")
              ),
              tags$details(
                tags$summary("Can I download the data?"),
                p("Yes. Both raw data (", tags$code(".csv"), ") and interpretation (", tags$code(".txt"), ") are available.")
              ),
              tags$details(
                tags$summary("What does “No signatures contain this gene” mean?"),
                p("The gene exists in the reference databases but is not included in any current multi-omic signature.")
              )
          ),
          
          # ------------------------------------------------------------------
          # 3. Omic Layer– and Metabolic Pathway–specific Signatures
          # ------------------------------------------------------------------
          div(class = "manual-section-title", "3. Filter Omic Layer– and Metabolic Pathway–specific Signatures"),
          div(class = "manual-section-subtitle", "Access"),
          tags$ul(class = "manual-list",
                  tags$li("Hover over the top menu ", tags$strong("Filter Omic Layer– and Metabolic Pathway–specific Signatures"), " to open the dropdown.")
          ),
          div(class = "manual-section-subtitle", "Sub-tabs"),
          tags$ul(class = "manual-list",
                  tags$li("CNV-Specific Signatures – copy number alterations;"),
                  tags$li("mRNA-Specific Signatures – protein-coding gene expression;"),
                  tags$li("Methylation-Specific Signatures – DNA methylation;"),
                  tags$li("Mutation-Specific Signatures – somatic mutations;"),
                  tags$li("miRNA-Specific Signatures – microRNA expression;"),
                  tags$li("Protein-Specific Signatures – protein expression;"),
                  tags$li("Transcript-Specific Signatures – transcript isoforms.")
          ),
          
          div(class = "manual-section-subtitle", "Filtering workflow"),
          p("Filtering is sequential and constrains subsequent options:"),
          tags$ol(class = "manual-list",
                  tags$li(tags$strong("Filter 1 – Omic layer:"),
                          " automatically defined by the dataset and reflects the selected tab."),
                  tags$li(tags$strong("Filter 2 – Cancer type:"),
                          " available tumor types (CTAB); selection restricts metabolism options."),
                  tags$li(tags$strong("Filter 3 – Metabolism:"),
                          " metabolic process categories (e.g., carbohydrates); activated after cancer type."),
                  tags$li(tags$strong("Filter 4 – Pathway:"),
                          " individual metabolic pathways (e.g., glycolysis); activated after metabolism."),
                  tags$li(tags$strong("Filter 5 – Metabolic cell death:"),
                          " filters signatures by associated regulated cell death type (e.g., ferroptosis)."),
                  tags$li(tags$strong("Filter 6 – Signature:"),
                          " lists all signatures matching the previous filters; selecting a signature triggers the integrative summary.")
          ),
          
          div(class = "manual-section-subtitle", "Reset and download"),
          tags$ul(class = "manual-list",
                  tags$li(tags$strong("Reset Filters:"), " clears all selections and returns to the initial state."),
                  tags$li(tags$strong("Download Summary Results:"), " exports a ", tags$code(".tsv"), " file containing a summarized view of the selected signature.")
          ),
          
          div(class = "manual-section-subtitle", "Results after filtering"),
          tags$ul(class = "manual-list",
                  tags$li(tags$strong("Integrative summary:"),
                          " signature nomenclature, cancer type, molecular class, metabolism and pathway."),
                  tags$li(tags$strong("Molecular interaction:"),
                          " highlights clinically relevant molecular interactions."),
                  tags$li(tags$strong("Molecular correlation:"),
                          " Spearman correlation (ρ), p-values, and tumor vs. non-tumor differential expression."),
                  tags$li(tags$strong("Survival analysis:"),
                          " Cox regression (OS, DSS, DFI, PFI) and Kaplan–Meier stratification."),
                  tags$li(tags$strong("Microenvironment and immunophenotype:"),
                          " tumor microenvironment profile (e.g., “Hot”, “Cold”) and immune classifications."),
                  tags$li(tags$strong("Metabolic cell death association:"),
                          " indicates whether the signature is linked to metabolic cell death programs."),
                  tags$li(tags$strong("Complete data table:"),
                          " tabular visualization of all variables and metrics.")
          ),
          
          div(class = "manual-section-subtitle", "FAQs – Omic Layer– and Pathway–specific Signatures"),
          div(class = "manual-faq",
              tags$details(
                tags$summary("Can I apply filters in any order?"),
                p("No. Filters are sequential to ensure biological and analytical consistency.")
              ),
              tags$details(
                tags$summary("Do I need to apply all filters?"),
                p("Yes. All filters must be selected to unambiguously define a single signature.")
              ),
              tags$details(
                tags$summary("What does “No data available…” mean?"),
                p("No signatures satisfy the selected combination of omic layer, cancer type, metabolism, pathway, and metabolic cell death.")
              ),
              tags$details(
                tags$summary("What happens when I click “Reset Filters”?"),
                p("All filters return to their initial state and the view is cleared.")
              )
          ),
          
          # ------------------------------------------------------------------
          # 4. Regulatory Circuitry
          # ------------------------------------------------------------------
          div(class = "manual-section-title", "4. Regulatory Circuitry"),
          
          div(class = "manual-section-subtitle", "Module overview"),
          p(
            "This module allows you to visualize the regulatory circuitry underlying a selected signature. ",
            "A regulatory circuitry represents how the signature, its regulators, metabolic pathways, tumor phenotypes, ",
            "immune microenvironment, and clinical prognosis are connected and interpretively aligned. ",
            "This visualization helps you understand how and why a signature behaves in a specific biological or clinical context."
          ),
          
          tags$ul(class = "manual-list",
                  tags$li("Signature – the multi-omic biomarker of interest;"),
                  tags$li("Regulators – molecules that modulate or define the signature;"),
                  tags$li("Metabolic pathways – processes in which the signature is embedded;"),
                  tags$li("Tumor phenotypes – features such as TMB, MSI, or stemness;"),
                  tags$li("Immune microenvironment – immune context, including “Hot”, “Cold”, or “Variable” profiles;"),
                  tags$li("Clinical prognosis – direction and strength of association with patient outcome.")
          ),
          
          div(class = "manual-section-subtitle", "How to use this module"),
          
          tags$ol(class = "manual-list",
                  tags$li(
                    tags$strong("Select a signature"),
                    tags$ul(class = "manual-list",
                            tags$li("In the input box, type or paste the ", tags$strong("Nomenclature"), " of the signature to visualize."),
                            tags$li("Click ", tags$strong("Plot circuitry"), " to generate the graph.")
                    )
                  ),
                  tags$li(
                    tags$strong("Customize the information displayed"),
                    p("Use the checkboxes to choose which biological aspects are shown in the diagram:"),
                    tags$ul(class = "manual-list",
                            tags$li(tags$strong("Pathway Concordance"), " – relationships between the signature and metabolic pathways;"),
                            tags$li(tags$strong("Phenotypic Association"), " – whether the signature increases or decreases phenotypes such as TMB or stemness;"),
                            tags$li(tags$strong("Immune Classification"), " – alignment with immune profiles such as “Hot”, “Cold”, or “Variable”;"),
                            tags$li(tags$strong("Prognosis (Keywords)"), " – qualitative prognostic interpretation derived from descriptive annotations;"),
                            tags$li(tags$strong("Prognosis (Cox Data)"), " – prognostic interpretation derived from statistical survival modelling.")
                    )
                  ),
                  tags$li(
                    tags$strong("Adjust the layout"),
                    tags$ul(class = "manual-list",
                            tags$li("Select among alternative node layouts (e.g., ", tags$code("circle"), ", ", tags$code("stress"), 
                                    ", ", tags$code("kk"), ", ", tags$code("fr"), ") to improve readability.")
                    )
                  ),
                  tags$li(
                    tags$strong("Refine visual appearance"),
                    tags$ul(class = "manual-list",
                            tags$li("Adjust node size and label size to enhance clarity and emphasis in the graph.")
                    )
                  ),
                  tags$li(
                    tags$strong("Download the figure"),
                    tags$ul(class = "manual-list",
                            tags$li("Export the circuitry as ", tags$code("PDF"), " or ", tags$code("PNG"), " for use in presentations and manuscripts.")
                    )
                  )
          ),
          
          div(class = "manual-section-subtitle", "How to read the circuitry graph"),
          
          tags$strong("Nodes (circles)"),
          p("Each node corresponds to a biological component. Colors encode node type and interpretation:"),
          
          tags$table(
            class = "table table-sm",
            tags$thead(
              tags$tr(
                tags$th("Color"),
                tags$th("Node type"),
                tags$th("Interpretation")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("Green"),
                tags$td("Signature"),
                tags$td("Multi-omic biomarker of interest.")
              ),
              tags$tr(
                tags$td("Orange"),
                tags$td("Regulators"),
                tags$td("Molecules that regulate or define the signature.")
              ),
              tags$tr(
                tags$td("Blue"),
                tags$td("Metabolic pathways"),
                tags$td("Metabolic processes in which the signature participates.")
              ),
              tags$tr(
                tags$td("Purple"),
                tags$td("Tumor phenotypes"),
                tags$td("Phenotypic traits such as TMB, MSI, or stemness.")
              ),
              tags$tr(
                tags$td("Gold"),
                tags$td("Immune phenotypes"),
                tags$td("Immune context (e.g., “Hot”, “Cold”, or “Variable” microenvironment).")
              ),
              tags$tr(
                tags$td("Red"),
                tags$td("Prognosis"),
                tags$td("Direction and pattern of clinical outcome associated with the circuitry.")
              )
            )
          ),
          
          tags$br(),
          tags$strong("Edges (arrows)"),
          p("Edges represent directionality and qualitative interpretation of relationships:"),
          
          tags$table(
            class = "table table-sm",
            tags$thead(
              tags$tr(
                tags$th("Edge appearance"),
                tags$th("Meaning")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("Dashed orange arrows"),
                tags$td("Regulatory relationships (regulator → signature).")
              ),
              tags$tr(
                tags$td("Solid green arrows"),
                tags$td("Convergent effects – components act in the same biological direction.")
              ),
              tags$tr(
                tags$td("Solid red arrows"),
                tags$td("Divergent effects – components act in opposite or counterbalancing directions.")
              )
            )
          ),
          
          div(class = "manual-section-subtitle", "When to use this module"),
          p("Use the Regulatory Circuitry module when you want to:"),
          tags$ul(class = "manual-list",
                  tags$li("Clarify the biological meaning of a multi-omic signature in its metabolic, phenotypic, immune, and clinical context;"),
                  tags$li("Formulate mechanistic hypotheses suitable for experimental validation;"),
                  tags$li("Communicate the structure of a signature’s regulatory network in figures and supplementary material.")
          ),
          
          # ------------------------------------------------------------------
          # 5. Custom Signature Builder
          # ------------------------------------------------------------------
          div(class = "manual-section-title", "5. Custom Signature Builder"),
          
          div(class = "manual-section-subtitle", "Module overview"),
          p(
            "The Custom Signature Builder allows you to construct user-defined signatures by grouping molecular entities ",
            "that share similar biological characteristics. Starting from a dataset of genes, proteins, miRNAs, lncRNAs and ",
            "other entities, together with their metabolic, phenotypic, and clinical annotations, the module lets you specify ",
            "which variables define the grouping structure. It then automatically builds and summarizes custom multi-omic signatures."
          ),
          
          div(class = "manual-section-subtitle", "Core workflow"),
          tags$ol(class = "manual-list",
                  tags$li(
                    tags$strong("Filter molecular components by correlation threshold"),
                    tags$ul(class = "manual-list",
                            tags$li("Molecular entities are first filtered according to a minimum correlation threshold defined in the module.")
                    )
                  ),
                  tags$li(
                    tags$strong("Group variables into signature units"),
                    tags$ul(class = "manual-list",
                            tags$li("Selected grouping variables are used to aggregate molecular entities into coherent signature units.")
                    )
                  ),
                  tags$li(
                    tags$strong("Identify common molecular interactions"),
                    tags$ul(class = "manual-list",
                            tags$li("Within each group, the module identifies molecular interactions shared by all members.")
                    )
                  ),
                  tags$li(
                    tags$strong("Generate formatted signatures"),
                    tags$ul(class = "manual-list",
                            tags$li("Signatures are represented in a standardized format, for example "),
                            tags$li(tags$code("Component A + Component B + Component C"), ".")
                    )
                  ),
                  tags$li(
                    tags$strong("Quantify signature membership"),
                    tags$ul(class = "manual-list",
                            tags$li("The number of molecular components per signature is computed and reported.")
                    )
                  ),
                  tags$li(
                    tags$strong("Separate signatures by correlation sign (optional)"),
                    tags$ul(class = "manual-list",
                            tags$li("When enabled, signatures are split into positive and negative correlation groups.")
                    )
                  )
          ),
          
          p("All resulting signatures can be inspected interactively in the app and exported for downstream analyses."),
          
          # ------------------------------------------------------------------
          # Fixed grouping variables
          # ------------------------------------------------------------------
          div(class = "manual-section-subtitle", "Fixed grouping variables"),
          
          p(
            "Two variables are always included in the grouping structure and cannot be removed. ",
            "They define the core biological context for each signature:"
          ),
          
          tags$table(
            class = "table table-sm",
            tags$thead(
              tags$tr(
                tags$th("Variable"),
                tags$th("Meaning")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("Cancer_types"),
                tags$td("Tumor type with which the molecular entities are associated.")
              ),
              tags$tr(
                tags$td("Metabolism"),
                tags$td("Metabolic class or metabolic process involved.")
              )
            )
          ),
          
          p(
            "Every generated signature is, therefore, tumor-type-specific and metabolism-aware by construction."
          ),
          
          # ------------------------------------------------------------------
          # User-configurable grouping variables
          # ------------------------------------------------------------------
          div(class = "manual-section-subtitle", "User-configurable grouping variables"),
          
          p(
            "In addition to the fixed variables, you may optionally select any combination of additional variables to refine ",
            "how signatures are grouped. Adding more grouping variables produces more specific and smaller signatures; ",
            "using fewer grouping variables results in broader and larger signatures."
          ),
          
          tags$table(
            class = "table table-sm",
            tags$thead(
              tags$tr(
                tags$th("Optional variable (examples)"),
                tags$th("Interpretation")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("Molecular class"),
                tags$td("Type of molecule (e.g., gene, miRNA, lncRNA, protein).")
              ),
              tags$tr(
                tags$td("Metabolic cell death"),
                tags$td("Associated regulated cell-death pathway.")
              ),
              tags$tr(
                tags$td("Tumor vs normal"),
                tags$td("Direction of expression change in tumor versus control samples.")
              ),
              tags$tr(
                tags$td("Omic layer"),
                tags$td("Data modality, such as CNV, mRNA expression, protein, or methylation.")
              ),
              tags$tr(
                tags$td("Phenotypic layer"),
                tags$td("Clinical or phenotypic axis used in the model (e.g., TMB, MSI, stemness).")
              ),
              tags$tr(
                tags$td("Cox clinical endpoint"),
                tags$td("Hazard-type classification derived from Cox regression models.")
              ),
              tags$tr(
                tags$td("Worst prognosis Kaplan–Meier group"),
                tags$td("Patient subgroup with the worst survival outcome based on Kaplan–Meier analysis.")
              ),
              tags$tr(
                tags$td("Immune classification"),
                tags$td("Immune microenvironment phenotype (e.g., Hot, Cold, Intermediate/Variable).")
              )
            )
          ),
          
          div(class = "manual-highlight",
              p(tags$strong("Tip – Specificity versus generality")),
              p(
                "Selecting more grouping variables results in highly specific signatures with fewer members. ",
                "Selecting fewer grouping variables yields broader signatures that aggregate larger sets of molecules."
              )
          ),
          
          # ------------------------------------------------------------------
          # Split by correlation sign
          # ------------------------------------------------------------------
          div(class = "manual-section-subtitle", "Split by correlation sign"),
          
          p(
            "The Custom Signature Builder can optionally separate signatures according to the sign of correlation. ",
            "This setting controls whether positive and negative correlations are handled jointly or independently."
          ),
          
          tags$table(
            class = "table table-sm",
            tags$thead(
              tags$tr(
                tags$th("Setting"),
                tags$th("Interpretation")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("Enabled"),
                tags$td("Generates two distinct sets of signatures: one based on positive correlations and one based on negative correlations. The output includes a ", tags$code("Correlation_type"), " column (", tags$em("positive"), " or ", tags$em("negative"), ").")
              ),
              tags$tr(
                tags$td("Disabled"),
                tags$td("Combines all correlated components into a single set of signatures, regardless of correlation direction.")
              )
            )
          ),
          
          # ------------------------------------------------------------------
          # Output structure
          # ------------------------------------------------------------------
          div(class = "manual-section-subtitle", "Output structure"),
          
          p(
            "The resulting table summarizes each custom signature together with the metadata used during grouping. ",
            "Columns typically include:"
          ),
          
          tags$table(
            class = "table table-sm",
            tags$thead(
              tags$tr(
                tags$th("Column"),
                tags$th("Meaning")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("Members"),
                tags$td("Number of molecular components in the signature.")
              ),
              tags$tr(
                tags$td("Multiomics_Signature"),
                tags$td("Formatted representation of the signature (e.g., ", tags$code("Component A + Component B + Component C"), ").")
              ),
              tags$tr(
                tags$td("Common_interaction"),
                tags$td("Molecular interaction(s) conserved across all members within the signature.")
              ),
              tags$tr(
                tags$td("Grouping variables"),
                tags$td("Variables used to define how molecules were aggregated into signatures (e.g., cancer type, metabolism, omic layer).")
              ),
              tags$tr(
                tags$td("Correlation_type"),
                tags$td("Direction of the correlation sign (", tags$em("positive"), " or ", tags$em("negative"), ") when splitting by sign is enabled.")
              )
            )
          ),
          
          p(
            "By default, signatures are sorted by ", tags$strong("Cancer_types"), " and by ", tags$strong("Members"), 
            " in descending order, prioritizing tumor-specific signatures with larger membership."
          ),
          
          # ------------------------------------------------------------------
          # Downloading results
          # ------------------------------------------------------------------
          div(class = "manual-section-subtitle", "Downloading results"),
          
          tags$ul(class = "manual-list",
                  tags$li("Users can export the full set of custom signatures in ", tags$code(".csv"), " format for downstream analysis or documentation."),
                  tags$li("Downloaded tables preserve all grouping variables and correlation annotations for reproducible analyses.")
          ),
        
          
          
          # ------------------------------------------------------------------
          # 7. Analysis and Plotting
          # ------------------------------------------------------------------
          div(class = "manual-section-title", "7. Analysis and Plotting"),
          div(class = "manual-section-subtitle", "Access"),
          tags$ul(class = "manual-list",
                  tags$li("Hover over the ", tags$strong("Analysis and Plotting"), " menu to open the analysis submenu.")
          ),
          div(class = "manual-section-subtitle", "Sub-tabs"),
          tags$ul(class = "manual-list",
                  tags$li("Correlation Analysis – associations between omic layers and phenotypes;"),
                  tags$li("Tumor vs. Normal Analysis – comparative expression analysis;"),
                  tags$li("Cox Analysis – survival associations using proportional hazards models;"),
                  tags$li("Survival Analysis – Kaplan–Meier curves and prognostic stratification;"),
                  tags$li("Immune Infiltrates Analysis – detailed immune context evaluation.")
          ),
          div(class = "manual-section-subtitle", "General workflow"),
          tags$ul(class = "manual-list",
                  tags$li("Select the analysis tab;"),
                  tags$li("Specify the signature or gene/miRNA of interest;"),
                  tags$li("Inspect generated plots and tables;"),
                  tags$li("Export figures and results when required.")
          ),
          
          # ------------------------------------------------------------------
          # 8. About Us
          # ------------------------------------------------------------------
          div(class = "manual-section-title", "8. About Us"),
          div(class = "manual-section-subtitle", "Access"),
          tags$ul(class = "manual-list",
                  tags$li("Click the ", tags$strong("About Us"), " tab.")
          ),
          div(class = "manual-section-subtitle", "Content"),
          tags$ul(class = "manual-list",
                  tags$li("Information on platform developers and contributors;"),
                  tags$li("Institutional affiliations and funding acknowledgments;"),
                  tags$li("Contact information and links to associated publications.")
          ),
          
          # ------------------------------------------------------------------
          # 9. Citation and Attribution
          # ------------------------------------------------------------------
          div(class = "manual-section-title", "9. Citation and Attribution"),
          div(
            class = "manual-highlight",
            p(tags$strong("If you use OncoMetabolismGPS, its datasets, analytical pipelines, or conceptual framework in your work, please cite:")),
            p(
              tags$em("Nogueira HAC, Rodrigues ES, Lopes VS, Medina-Acosta E. "),
              tags$em("A Multi-Omic Atlas of Convergent and Divergent Metabolic Regulatory Circuitries in Cancer."),
              " 2025."
            ),
            p("Proper citation supports reproducibility, transparency, and the continued development of this resource.")
          )
        )
    )
  )
}
