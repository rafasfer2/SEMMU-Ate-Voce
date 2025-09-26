library(shiny)
library(shinyjs)

usuarios <- data.frame(
  usuario = c("rafael", "admin", "profissional1"),
  senha = c("semmu2025", "1234", "abcd"),
  nivel = c("gestor", "admin", "tecnico"),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  useShinyjs(),  # Ativa shinyjs
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    tags$style(HTML("
      body { margin: 0; padding: 0; }

      .top-bar {
        background-color: white;
        height: 120px;
        width: 100%;
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 0 40px;
        box-sizing: border-box;
        position: relative;
        z-index: 2;
        border-bottom: 1px solid rgba(0,0,0,0.2);
      }

      .top-bar .logo img {
        background-color: #f2e6f2;
        padding: 8px;
        border-radius: 8px;
        height: 100px;
      }

      .top-bar .divider {
        height: 75%;
        width: 2px;
        background-color: rgba(0,0,0,0.3);
        margin-left: 8px;
        margin-right: 15px;

      }

      .top-bar .title {
        font-size: 20px;
        font-weight: bold;
        text-align: left;
      }

      .top-bar .user-info {
        font-size: 16px;
        color: #4B0082;
        text-align: right;
      }

      .background-purple {
        background-color: #C8A2C8;
        height: 40vh;
        width: 100%;
        position: fixed;
        top: 0;
        left: 0;
        z-index: 0;
      }
      
      .background-light {
        background-color: #f2e6f2;
        height: 60vh;
        width: 100%;
        position: fixed;
        bottom: 0;
        left: 0;
        z-index: 0;
      }

      .login-box {
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        background-color: white;
        padding: 50px;
        border-radius: 12px;
        box-shadow: 0 0 15px rgba(0,0,0,0.2);
        width: 450px;
        z-index: 3;
        text-align: center;
      }

      .login-box h3 {
        font-size: 22px;
        font-weight: bold;
        margin-bottom: 10px;
      }

      .login-box hr {
        margin: 5px auto 25px auto;
        width: 95%;
        border: none;
        border-top: 3px solid #C8A2C8;
      }

      .login-box label {
        font-size: 16px;
        font-weight: bold;
      }

      .login-box input {
        display: block;
        margin-left: auto;
        margin-right: auto;
        width: 90%;
        max-width: 450px;
        padding: 18px;
        font-size: 18px;
        text-align: center;
        box-sizing: border-box;
        margin-bottom: 20px;
      }

      .login-box .login-btn {
        background-color: #C8A2C8;
        color: white;
        border: none;
        padding: 15px 30px;
        border-radius: 20px;
        font-size: 20px;
        font-weight: bold;
        cursor: pointer;
      }

      .login-links {
        margin-top: 15px;
        font-size: 14px;
      }

      footer {
        position: fixed;
        bottom: 0;
        width: 100%;
        background-color: #f9f9f9;
        text-align: center;
        padding: 10px;
        font-size: 14px;
        color: #666;
        z-index: 4;
      }

      .btn-quadrado {
        display: inline-block;
        width: 240px;
        height: 240px;
        font-size: 18px;
        text-align: center;
        vertical-align: middle;
        padding: 30px;
        margin: 20px;
        border-radius: 16px;
        background-color: #4E005C;
        color: white;
        border: none;
        cursor: pointer;
        line-height: 1.3;
        white-space: normal;
      }

      .btn-quadrado i {
        font-size: 40px;
        margin-bottom: 12px;
        display: block;
      }
      .painel-principal {
        position: relative;
        z-index: 3;
      }
    "))
  ),
  
  div(class = "top-bar",
      div(class = "logo", img(src = "SEMMU-PRETO.png", height = "100px")),
      div(class = "divider"),
      div(class = "title", "SEMMU - SIAM - Sistema Integrado de Atendimento à Mulher"),
      uiOutput("usuario_logado")
  ),
  
  div(class = "background-purple"),
  div(class = "background-light"),
  
  uiOutput("tela_login"),
  
  tags$footer("© SIAM | Sistema Integrado de Atendimento à Mulher"),
  uiOutput("painel_principal")
)

server <- function(input, output, session) {
  logado <- reactiveVal(FALSE)
  usuario_atual <- reactiveVal("")
  nivel_atual <- reactiveVal("")
  
  output$tela_login <- renderUI({
    if (!logado()) {
      div(class = "login-box",
          h3("AUTENTICAÇÃO INTEGRADA",
             style = "color: #4B0082; opacity: 0.85; padding: 10px; border-radius: 5px;"),
          tags$hr(),
          textInput("usuario", "Usuário", placeholder = "Digite seu login"),
          passwordInput("senha", "Senha", placeholder = "Digite sua senha"),
          tags$script(HTML("
            $(document).on('keypress', function(e) {
              if (e.which == 13) {
                $('#login').click();
              }
            });
          ")),
          actionButton("login", "Entrar →", class = "login-btn"),
          div(class = "login-links",
              tags$p(HTML("🔑 <a href='#' style='color:#4B0082;'>Cadastre-se</a>")),
              tags$p(HTML("❓ <a href='#' style='color:#4B0082;'>Recuperar senha</a>"))
          )
      )
    }
  })
  
  output$painel_principal <- renderUI({
    if (logado()) {
      tagList(
        br(),
        fluidRow(
          column(6, actionButton("planejamento", label = HTML("<i class='fas fa-calendar-alt'></i><br>Planejamento<br>de Visitas"), class = "btn-quadrado")),
          column(6, actionButton("frequencia", label = HTML("<i class='fas fa-users'></i><br>Lançamento<br>de Frequência"), class = "btn-quadrado"))
        ),
        br(),
        fluidRow(
          column(6, actionButton("palestras", label = HTML("<i class='fas fa-chalkboard-teacher'></i><br>Registro<br>de Palestras"), class = "btn-quadrado")),
          column(6, actionButton("dashboard", label = HTML("<i class='fas fa-chart-bar'></i><br>Indicadores"), class = "btn-quadrado"))
        )
      )
    }
  })
  
  output$usuario_logado <- renderUI({
    if (logado()) {
      div(class = "user-info",
          HTML(paste0("Usuário: <strong>", usuario_atual(), "</strong><br>Nível: <strong>", toupper(nivel_atual()), "</strong><br>",
                      "<a href='#' style='color:#4B0082;'>Alterar cadastro</a>"))
      )
    }
  })
  
  observeEvent(input$login, {
    req(input$usuario, input$senha)  # Garante que os dois inputs existem
    
    if (input$usuario != "" && input$senha != "") {
      credenciais_validas <- usuarios$usuario == input$usuario & usuarios$senha == input$senha
      if (any(credenciais_validas)) {
        usuario_atual(input$usuario)
        nivel_atual(usuarios$nivel[credenciais_validas])
        logado(TRUE)
      } else {
        showModal(modalDialog(
          title = "Erro de autenticação",
          "Usuário ou senha incorretos. Tente novamente.",
          easyClose = TRUE
        ))
      }
    }
  })
  
}

shinyApp(ui, server)
