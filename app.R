library(shiny)
library(DT)

# Interface
ui <- fluidPage(
  titlePanel("Lançamento de Frequência"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("nome", "???? Nome Completo"),
      textInput("matricula", "???? Número de Matrícula"),
      selectInput("polo", "???? Polo de Atendimento",
                  choices = c("Itabira", "Belo Horizonte", "Montes Claros", "Outros")),
      dateInput("data", "???? Data da Frequência"),
      selectInput("presenca", "??? Status de Presença",
                  choices = c("Presente", "Ausente", "Justificado")),
      textAreaInput("obs", "??????? Observações (opcional)", ""),
      actionButton("adicionar", "Adicionar Frequência")
    ),
    
    mainPanel(
      DTOutput("tabela_frequencia")
    )
  )
)

# Servidor
server <- function(input, output, session) {
  # Armazena os dados
  dados <- reactiveVal(data.frame(
    Nome = character(),
    Matrícula = character(),
    Polo = character(),
    Data = as.Date(character()),
    Presença = character(),
    Observações = character(),
    stringsAsFactors = FALSE
  ))
  
  # Adiciona nova linha
  observeEvent(input$adicionar, {
    nova_linha <- data.frame(
      Nome = input$nome,
      Matrícula = input$matricula,
      Polo = input$polo,
      Data = input$data,
      Presença = input$presenca,
      Observações = input$obs,
      stringsAsFactors = FALSE
    )
    dados(rbind(dados(), nova_linha))
  })
  
  # Exibe tabela
  output$tabela_frequencia <- renderDT({
    datatable(dados(), options = list(pageLength = 10))
  })
}

# Executa o app
shinyApp(ui, server)