# ===================== Developers Module (Refined UI) =======================
# - Clean, professional layout
# - Consistent photo size (not too big, not too small)
# - Responsive card grid
# - Clear sections for team, logo credit, and issue reporting
# - Accessible labels and keyboard-friendly focus states
# ============================================================================

mod_developers_ui <- function(id) {
  ns <- NS(id)
  
  # ---- Inline CSS ----
  styles <- tags$style(HTML(sprintf("
    /* Section spacing */
    .%s-wrap { max-width: 1100px; margin: 0 auto; padding: 16px 16px 64px; }
    .%s-header { text-align: center; margin-bottom: 12px; }
    .%s-subtitle { text-align: center; color: #555; max-width: 900px; margin: 0 auto 28px; }

    /* Card grid */
    .%s-grid { display: grid; grid-template-columns: repeat(12, 1fr); gap: 16px; }
    @media (max-width: 575.98px) { .%s-grid { grid-template-columns: repeat(1, 1fr); } }
    @media (min-width: 576px) and (max-width: 991.98px) { .%s-grid { grid-template-columns: repeat(2, 1fr); } }
    @media (min-width: 992px) { .%s-grid { grid-template-columns: repeat(4, 1fr); } }  /* <-- changed from 3 to 4 */

    /* Person card */
    .%s-card {
      background: #fff; border: 1px solid #eee; border-radius: 14px;
      padding: 18px; box-shadow: 0 6px 14px rgba(0,0,0,0.06); transition: transform .15s ease, box-shadow .15s ease;
    }
    .%s-card:focus-within, .%s-card:hover {
      transform: translateY(-2px);
      box-shadow: 0 10px 22px rgba(0,0,0,0.10);
    }

    /* Avatar (consistent size) */
    .%s-avatar {
      width: 140px; height: 140px; border-radius: 100%%; object-fit: cover; object-position: center;
      border: 3px solid #f3f4f6; box-shadow: 0 1px 0 rgba(0,0,0,0.04) inset; margin-bottom: 12px;
    }

    .%s-name { font-size: 1.05rem; font-weight: 700; margin: 4px 0 0; }
    .%s-role { color: #6b7280; margin: 0 0 12px; }

    /* Link buttons (pill) */
    .%s-links { display: flex; gap: 8px; flex-wrap: wrap; justify-content: center; }
    .%s-btn {
      display: inline-flex; align-items: center; gap: 6px; padding: 6px 12px; border-radius: 9999px;
      background: #f3f4f6; color: #111827; text-decoration: none; border: 1px solid #e5e7eb; font-size: 0.9rem;
      transition: background .15s, color .15s, border-color .15s;
    }
    .%s-btn:hover, .%s-btn:focus { background: #e5e7eb; color: #111827; border-color: #d1d5db; outline: none; }

    /* Icon alignment */
    .%s-icon { width: 16px; height: 16px; display: inline-block; }

    /* Logo card */
    .%s-logo-card {
      background: #fff; border: 1px solid #eee; border-radius: 14px; padding: 18px;
      box-shadow: 0 6px 14px rgba(0,0,0,0.06); text-align: center;
    }
    .%s-logo {
      width: 140px; height: 140px; object-fit: cover; border-radius: 100%%;
      border: 3px solid #f3f4f6; margin-bottom: 10px; transition: transform .15s ease;
    }
    .%s-logo:hover { transform: scale(1.04); }

    /* Issue card */
    .%s-issue {
      background: #fff; border: 1px solid #eee; border-radius: 14px; padding: 18px;
      box-shadow: 0 6px 14px rgba(0,0,0,0.06); max-width: 900px; margin: 0 auto;
    }
  ",
                                    "dev", "dev", "dev",
                                    "dev", "dev", "dev", "dev",
                                    "dev", "dev", "dev",
                                    "dev", "dev", "dev",
                                    "dev", "dev",
                                    "dev", "dev", "dev", "dev",
                                    "dev", "dev", "dev", "dev"
  )))
  
  # ---- Data (you can move this list to global.R if preferred) ----
  people <- list(
    list(
      name = "Higor Almeida Cordeiro Nogueira",
      role = "Researcher",
      img  = "HACN_Photo.jpg",
      rg   = "https://www.researchgate.net/profile/Higor-Cordeiro-Nogueira",
      gh   = "https://github.com/HigorACNogueira",
      li   = "https://linkedin.com/in/higor-almeida-950082255"
    ),
    list(
      name = "Emanuell Rodrigues de Souza",
      role = "Researcher",
      img  = "ESR_Photo.jpg",
      rg   = "https://www.researchgate.net/profile/Emanuell-Rodrigues-De-Souza",
      gh   = "https://github.com/Emanuell-Souza",
      li   = "http://www.linkedin.com/in/emanuell-rodrigues-de-souza-35b40a300"
    ),
    list(
      name = "Victor dos Santos Lopes",
      role = "Researcher",
      img  = "VSL_Photo.png",
      # rg   = "https://www.researchgate.net/profile/Emanuell-Rodrigues-De-Souza",
      # gh   = "https://github.com/Emanuell-Souza",
      li   = "https://www.linkedin.com/in/victor-lopes-880604377?utm_source=share&utm_campaign=share_via&utm_content=profile&utm_medium=ios_app"
    ),
    list(
      name = "Enrique Medina-Acosta",
      role = "Team Head",
      img  = "EMA_Photo.jpg",
      rg   = "https://www.researchgate.net/profile/Enrique-Medina-Acosta",
      gh   = "https://github.com/quiquemedina",
      loop   = "https://loop.frontiersin.org/people/51475/overview"
    )
  )
  
  # ---- Helper to render a profile card ------------------------------------
  person_card <- function(p) {
    # fallback alt text and safe image path (served from /www)
    img_src <- p$img
    alt_txt <- sprintf("Photo of %s", p$name)
    
    links <- tagList()
    if (!is.null(p$rg)) links <- tagAppendChild(links,
                                                tags$a(class = sprintf("%s-btn", "dev"), href = p$rg, target = "_blank", `aria-label` = "ResearchGate",
                                                       tags$span(class = sprintf("%s-icon", "dev"), icon("graduation-cap")), "ResearchGate"))
    if (!is.null(p$gh)) links <- tagAppendChild(links,
                                                tags$a(class = sprintf("%s-btn", "dev"), href = p$gh, target = "_blank", `aria-label` = "GitHub",
                                                       tags$span(class = sprintf("%s-icon", "dev"), icon("github")), "GitHub"))
    if (!is.null(p$li)) links <- tagAppendChild(links,
                                                tags$a(class = sprintf("%s-btn", "dev"), href = p$li, target = "_blank", `aria-label` = "LinkedIn",
                                                       tags$span(class = sprintf("%s-icon", "dev"), icon("linkedin")), "LinkedIn"))
    
    if (!is.null(p$loop)) links <- tagAppendChild(links,
        tags$a(class = sprintf("%s-btn", "dev"), href = p$loop, target = "_blank", `aria-label` = "Frontiers Loop",
          tags$span(
            class = sprintf("%s-icon", "dev"),
            icon("user-circle")   # No official Loop icon exists
          ),
          "Loop"
        )
      )
    
    
    tags$div(
      class = sprintf("%s-card", "dev"),
      tags$div(style = "display:flex; flex-direction:column; align-items:center;"),
      tags$img(src = img_src, alt = alt_txt, class = sprintf("%s-avatar", "dev"), loading = "lazy"),
      tags$div(class = sprintf("%s-name", "dev"), p$name),
      tags$div(class = sprintf("%s-role", "dev"), p$role),
      tags$div(class = sprintf("%s-links", "dev"), links)
    )
  }
  
  # ---- Layout --------------------------------------------------------------
  tagList(
    styles,
    
    tags$div(class = sprintf("%s-wrap", "dev"),
             # Header
             tags$div(class = sprintf("%s-header", "dev"),
                      tags$h3("Research Group"),
             ),
             tags$p(
               "We work at the intersection of bioinformatics, machine learning, and multi-omics analytics ",
               "to enable precision oncology and translational discoveries.",
               class = sprintf("%s-subtitle", "dev")
             ),
             
             # Team grid
             tags$div(class = sprintf("%s-grid", "dev"),
                      lapply(people, function(p) tags$div(person_card(p)))
             ),
             
             tags$br(), tags$br(),
             
             # Logo credit
             fluidRow(
               column(
                 width = 12,
                 tags$div(class = sprintf("%s-logo-card", "dev"),
                          tags$img(
                            src = "Cancer_metabolism_GPS.png",
                            alt = "OncoMetabolism GPS logo",
                            class = sprintf("%s-logo", "dev"),
                            loading = "lazy"
                          ),
                          tags$p(
                            em("Logo concept by Higor Almeida Cordeiro Nogueira using ChatGPT."),
                            style = "margin: 6px 0 0;"
                          )
                 )
               )
             ),
             
             tags$br(), tags$br(),
             
             # Issue reporting
             tags$div(class = sprintf("%s-issue", "dev"),
                      tags$p(
                        tags$strong("Issue reporting"), br(),
                        "If you encounter any issues while using OncoMetabolismGPS, please report them including:",
                        tags$ul(
                          tags$li("Exact location"),
                          tags$li("What you expected vs. what you observed"),
                          tags$li("Screenshots and console/log messages if possible")
                        ),
                        "Send your report to ",
                        tags$a("higoralmeida1995@gmail.com", href = "mailto:higoralmeida1995@gmail.com"),
                        "."
                      )
             )
    )
  )
}

# Server remains unchanged
mod_developers_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No dynamic logic
  })
}
