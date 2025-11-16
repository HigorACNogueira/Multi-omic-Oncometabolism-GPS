mod_article_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "max-width: 1000px; margin: 0 auto; text-align: center;",
        
        # Clickable text
        a(href = "https://www.medrxiv.org/content/10.1101/2025.03.06.25323523v1", target = "_blank", 
          h3("Read our preprint by clicking here", 
             style = "margin-bottom: 20px; text-decoration: none; color: inherit;")
        ),
        
        fluidRow(
          column(12, align = "center",
                 # Clickable image
                 a(href = "https://www.medrxiv.org/content/10.1101/2025.03.06.25323523v1", target = "_blank",
                   div(style = "margin-top: 10px;"),  # Additional CSS-based spacing
                   img(src = "Pre_Print_MedRxiv.png", style = "max-width: 80%; height: auto;")
                 )
                 
          )
        )
    )
  )
}

# Server do módulo (neste caso, vazio, pois não há lógica dinâmica)
mod_article_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Sem lógica de servidor necessária
  })
}
