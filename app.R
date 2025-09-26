library(shiny)
library(shinyjs)
library(RSQLite)
library(DBI)
library(bcrypt)

ui <- fluidPage(
  useShinyjs(),  # Ativa shinyjs
  
  tags$head(
    # Font Awesome para ícones
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    
    # Estilos personalizados
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

    .left-group {
      display: flex;
      align-items: center;
      position: relative;
      z-index: 3;
    }

    .left-group .logo {
      font-weight: bold;
      font-size: 22px;
      margin-right: 20px;
    }

    .left-group .divider {
      height: 60%;
      width: 1px;
      background-color: rgba(0,0,0,0.3);
      margin-right: 20px;
      position: relative;
      z-index: 3;

    }

    .top-bar .title {
      font-size: 28px;
      font-weight: bold;
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
        font-size: 20px;
        font-weight: bold;
      }

      .login-box input {
        display: block;
        margin-left: auto;
        margin-right: auto;
        width: 100%;
        max-width: 450px;
        padding: 18px;
        font-size: 18px;
        text-align: left;
        box-sizing: border-box;
        margin-bottom: 20px;
      }

      .login-box .login-btn {
        background-color: #C8A2C8;
        color: white;
        border: none;
        padding: 15px 30px;
        border-radius: 20px;
        font-size: 24px;
        font-weight: bold;
        cursor: pointer;
      }

      .login-links {
        margin-top: 15px;
        font-size: 16px;
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
        font-size: 22px;
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
    ")),
    tags$script(HTML("
      $(document).on('keypress', function(e) {
        if (e.which == 13) {
          $('#login').click();
        }
      });
        $(document).on('click', '.liberar-btn', function() {
          var usuario = $(this).data('usuario');
          Shiny.setInputValue('liberar_usuario_js', usuario, {priority: 'event'});
        });
    "))
    
  ),
  
  div(class = "top-bar",
      div(class = "left-group",
          div(class = "logo", img(src = "SEMMU-PRETO.png", height = "100px")),
          div(class = "divider"),
          ),
      div(class = "title", "SEMMU | SIAM - Sistema Integrado de Atendimento à Mulher"),
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
  nome_completo <- reactiveVal("")
  
  # Conexão com o banco SQLite
  con <- dbConnect(SQLite(), "usuarios.db")
  
  # Criação da tabela (executado uma única vez)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS usuarios (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      nome_completo TEXT,
      cpf TEXT,
      email TEXT,
      telefone TEXT,
      usuario TEXT UNIQUE,
      senha TEXT,
      nivel_acesso TEXT,
      ativo BOOLEAN,
      data_cadastro DATE,
      data_ultima_entrada DATETIME,
      responsavel_cadastro TEXT
    )
  ")

  # Inserção de usuário de teste (executado uma única vez)
  if (nrow(dbGetQuery(con, "SELECT * FROM usuarios WHERE usuario = 'rafael'")) == 0) {
    senha_hash <- bcrypt::hashpw("semmu2025", bcrypt::gensalt())
    
    dbExecute(con, "
    INSERT INTO usuarios (
      nome_completo, cpf, email, telefone, usuario, senha, nivel_acesso, ativo, data_cadastro, responsavel_cadastro
    ) VALUES (?, ?, ?, ?, ?, ?, ?, 1, DATE('now'), ?)",
              params = list(
                "Rafael Fernandes",
                "057.424.596-02",
                "rafasfer2@gmail.com",
                "(31) 9 8752-1679",
                "rafael",
                senha_hash,
                "Desenvolvedor",
                "admin"
              )
    )
  }
  
  observeEvent(input$login, {
    req(input$usuario, input$senha)
    
    query <- "SELECT * FROM usuarios WHERE usuario = ? AND ativo = 1"
    resultado <- dbGetQuery(con, query, params = list(input$usuario))
    
    if (nrow(resultado) == 1 && bcrypt::checkpw(input$senha, resultado$senha[1])) {
      usuario_atual(resultado$usuario[1])
      nivel_atual(resultado$nivel_acesso[1])
      nome_completo(resultado$nome_completo[1])
      logado(TRUE)
      
      dbExecute(con, "UPDATE usuarios SET data_ultima_entrada = DATETIME('now') WHERE id = ?", params = list(resultado$id[1]))
      return()
    }
    
    showModal(modalDialog(
      title = "Erro de autenticação",
      "Usuário ou senha incorretos. Tente novamente.",
      easyClose = TRUE
    ))
  })
  
  output$usuario_logado <- renderUI({
    req(logado())
    div(class = "user-info",
        HTML(paste0(
          "Usuária: <strong>", nome_completo(), "</strong><br>",
          "Nível: <strong>", tools::toTitleCase(nivel_atual()), "</strong><br>",
          "<a href='#' style='color:#4B0082;'>Alterar cadastro</a>"
        ))
    )
  })
  
  output$tela_login <- renderUI({
    if (!logado()) {
      div(class = "login-box",
          
          tags$style(HTML("
            .login-box input[type='text'], 
            .login-box input[type='password'] {
                width: 115%; 
                padding: 12px 10px; 
                display: block;
                margin: 0 auto 15px auto; 
                border: 1px solid #ccc;
                border-radius: 4px;
            }
        ")),
          h3("AUTENTICAÇÃO INTEGRADA", style = "color: #4E005C; opacity: 0.85; padding: 10px; border-radius: 5px;"),
          tags$hr(),
          textInput("usuario", "Usuário", placeholder = "Digite seu login"),
          passwordInput("senha", "Senha", placeholder = "Digite sua senha"),
          actionButton("login", "Entrar →", class = "login-btn"),
          div(class = "login-links",
              tags$p(actionLink("abrir_cadastro", "🔑 Clique aqui para se cadastrar", style = "color:#4B0082;")),
              tags$p(HTML("❓ <a href='#' style='color:#4B0082;'>Recuperar senha</a>"))
          )
      )
    }
  })
  
  # Verifica se o usuário tem acesso ao painel administrativo
  tem_acesso_admin <- reactive({
    nivel <- tolower(nivel_atual())
    nivel %in% c("Gestor", "Desenvolvedor")
  })
  
  # Botão de administração com controle visual
  output$painel_principal <- renderUI({
    req(logado())
    tagList(
      br(),
      fluidRow(
        column(6, actionButton("planejamento", label = HTML("<i class='fas fa-calendar-alt'></i><br>Planejamento<br>de Visitas"), class = "btn-quadrado")),
        column(6, actionButton("frequencia", label = HTML("<i class='fas fa-users'></i><br>Lançamento<br>de Frequência"), class = "btn-quadrado")),
        column(6, actionButton("palestras", label = HTML("<i class='fas fa-chalkboard-teacher'></i><br>Registro<br>de Palestras"), class = "btn-quadrado")),
        column(6, actionButton("dashboard", label = HTML("<i class='fas fa-chart-bar'></i><br>Indicadores"), class = "btn-quadrado")),
      ),
      br(),
      fluidRow(
        column(6, actionButton("painel_liberacao", label = HTML("<i class='fas fa-user-check'></i><br>Liberar<br>Acessos"), class = "btn-quadrado")),
        column(6,
               if (tem_acesso_admin()) {
                 actionButton("admin_panel", label = HTML("<i class='fas fa-user-cog'></i><br>Administração<br>de Usuárias"), class = "btn-quadrado")
               } else {
                 tags$button(
                   HTML("<i class='fas fa-user-cog'></i><br>Administração<br>de Usuárias"),
                   class = "btn-quadrado",
                   style = "background-color: #ccc; color: #666; cursor: not-allowed;",
                   disabled = NA
                 )
               }
        )
      )
    )
  })
  
  # Modal de cadastro de nova usuária
  observeEvent(input$abrir_cadastro, {
    showModal(modalDialog(
      title = "Cadastro de Nova Usuária",
      size = "l",
      fluidPage(
        textInput("cad_nome", "Nome completo"),
        textInput("cad_cpf", "CPF"),
        textInput("cad_email", "E-mail"),
        textInput("cad_telefone", "Telefone"),
        textInput("cad_usuario", "Nome de usuário"),
        passwordInput("cad_senha", "Senha"),
        actionButton("confirmar_cadastro", "Cadastrar")
      ),
      easyClose = TRUE
    ))
  })
  
  # Salva nova usuária com acesso pendente
  observeEvent(input$confirmar_cadastro, {
    req(input$cad_usuario, input$cad_senha)
    
    # Verifica se o nome de usuário já existe
    existe <- dbGetQuery(con, "SELECT COUNT(*) AS total FROM usuarios WHERE usuario = ?", params = list(input$cad_usuario))
    if (existe$total[1] > 0) {
      showModal(modalDialog("Este nome de usuário já está cadastrado. Escolha outro.", easyClose = TRUE))
      return()
    }
    
    # Validação de senha mínima
    if (nchar(input$cad_senha) < 6) {
      showModal(modalDialog("A senha deve ter pelo menos 6 caracteres.", easyClose = TRUE))
      return()
    }
    
    # Gera hash seguro da senha
    senha_hash <- bcrypt::hashpw(input$cad_senha)
    
    # Insere nova usuária com acesso pendente
    dbExecute(con, "
    INSERT INTO usuarios (
      nome_completo, cpf, email, telefone, usuario, senha, nivel_acesso, ativo, data_cadastro, responsavel_cadastro
    ) VALUES (?, ?, ?, ?, ?, ?, ?, 0, DATE('now'), ?)",
              params = list(
                input$cad_nome,
                input$cad_cpf,
                input$cad_email,
                input$cad_telefone,
                input$cad_usuario,
                senha_hash,
                "pendente",
                usuario_atual()
              )
    )
    
    showModal(modalDialog(
      title = "Cadastro enviado!",
      "Seu cadastro foi registrado e está aguardando liberação da administração.",
      easyClose = TRUE
    ))
  })
  
  # Painel administrativo completo
  observeEvent(input$admin_panel, {
    if (!tem_acesso_admin()) return(NULL)
    
    showModal(modalDialog(
      title = "Painel de Administração",
      size = "l",
      fluidPage(
        DT::dataTableOutput("tabela_usuarios"),
        br(),
        textInput("usuario_liberar", "Usuário para liberar acesso"),
        selectInput("nivel_liberar", "Definir nível de acesso", choices = c("Psicossocial", "Coordenador", "Gestor", "Admin")),
        actionButton("liberar_acesso", "Liberar acesso"),
        br(), hr(),
        textInput("editar_usuario", "Usuário para editar"),
        textInput("novo_nome", "Novo nome completo"),
        textInput("novo_email", "Novo e-mail"),
        passwordInput("nova_senha", "Nova senha"),
        checkboxInput("ativo_status", "Usuária está ativa?", value = TRUE),
        actionButton("salvar_edicao", "Salvar alterações"),
        textInput("usuario_redefinir", "Usuário para redefinir senha"),
        actionButton("redefinir_senha", "Gerar nova senha")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$redefinir_senha, {
    req(input$usuario_redefinir)
    
    resultado <- dbGetQuery(con, "SELECT * FROM usuarios WHERE usuario = ?", params = list(input$usuario_redefinir))
    
    if (nrow(resultado) == 1) {
      nova_senha <- paste0(sample(LETTERS, 4), sample(0:9, 4), collapse = "")
      senha_hash <- bcrypt::hashpw(nova_senha, bcrypt::gensalt())
      
      dbExecute(con, "UPDATE usuarios SET senha = ? WHERE usuario = ?", params = list(senha_hash, input$usuario_redefinir))
      
      showModal(modalDialog(
        title = "Senha redefinida!",
        paste("Nova senha temporária para", input$usuario_redefinir, "é:", nova_senha),
        easyClose = TRUE
      ))
    } else {
      showModal(modalDialog("Usuário não encontrado.", easyClose = TRUE))
    }
  })
  
  # Exibe tabela completa da base de usuárias
  output$tabela_usuarios <- DT::renderDataTable({
    dbGetQuery(con, "SELECT * FROM usuarios")
  })
  
  # Libera acesso e define nível
  observeEvent(input$liberar_acesso, {
    req(input$usuario_liberar, input$nivel_liberar)
    
    dbExecute(con, "
    UPDATE usuarios SET ativo = 1, nivel_acesso = ? WHERE usuario = ?",
              params = list(input$nivel_liberar, input$usuario_liberar)
    )
    
    showModal(modalDialog(
      title = "Acesso liberado!",
      paste("Usuária", input$usuario_liberar, "foi ativada com nível", input$nivel_liberar),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$confirmar_liberacao, {
    usuario <- input$liberar_usuario_js
    nivel <- input$nivel_liberar_modal
    
    dbExecute(con, "UPDATE usuarios SET ativo = 1, nivel_acesso = ? WHERE usuario = ?", params = list(nivel, usuario))
    
    removeModal()
    showModal(modalDialog("Usuária liberada com sucesso!", easyClose = TRUE))
  })
  
  # Edita dados da usuária
  observeEvent(input$salvar_edicao, {
    req(input$editar_usuario)
    dbExecute(con, "
    UPDATE usuarios SET 
      nome_completo = ?, 
      email = ?, 
      senha = ?, 
      ativo = ? 
    WHERE usuario = ?",
              params = list(
                input$novo_nome,
                input$novo_email,
                input$nova_senha,
                as.integer(input$ativo_status),
                input$editar_usuario
              )
    )
    showModal(modalDialog("Usuária atualizada com sucesso!", easyClose = TRUE))
  })
  
  observeEvent(input$painel_liberacao, {
    if (!logado() || !tem_acesso_admin()) return(NULL) # Impede que o modal apareça antes do login
    
    showModal(modalDialog(
      title = "Liberação de Acesso",
      size = "l",
      fluidPage(
        DT::dataTableOutput("tabela_pendentes"),
        br(),
        textInput("usuario_liberar", "Usuária para liberar"),
        selectInput("nivel_liberar", "Definir nível de acesso", choices = c("Psicossocial", "Coordenador", "Gestor", "Admin")),
        actionButton("liberar_acesso", "Liberar acesso")
      ),
      easyClose = TRUE
    ))
  })
  
  output$tabela_pendentes <- DT::renderDataTable({
    dados <- dbGetQuery(con, "SELECT nome_completo, usuario, email, nivel_acesso FROM usuarios WHERE ativo = 0")
    dados$Ação <- paste0(
      "<button class='btn btn-success btn-sm liberar-btn' data-usuario='", dados$usuario, "'>Liberar</button>"
    )
    DT::datatable(dados, escape = FALSE, rownames = FALSE, options = list(pageLength = 10))
  })
  
  observe({
    input$liberar_usuario_js
    isolate({
      usuario <- input$liberar_usuario_js
      showModal(modalDialog(
        title = paste("Liberar acesso para", usuario),
        selectInput("nivel_liberar_modal", "Definir nível de acesso", choices = c("Psicossocial", "Coordenador", "Gestor", "Admin")),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("confirmar_liberacao", "Confirmar")
        )
      ))
    })
  })
  
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
  
}

shinyApp(ui, server)

#  dbExecute(con, "UPDATE usuarios SET senha = 1234 WHERE usuario = rafael", params = list("novaSenha", "rafael"))