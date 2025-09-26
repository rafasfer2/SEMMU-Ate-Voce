library(shiny)
library(shinythemes)
library(shinyjs)
library(DT)
library(RSQLite)


# Função auxiliar para rótulo com asterisco vermelho
labelObrigatorio <- function(texto) {
  tagList(
    tags$label(
      tags$span(strong(texto)),
      tags$span("*", style = "color:red; margin-left:5px;")
    )
  )
}


con_usuarios <- dbConnect(SQLite(), "usuarios.db")

# Verifica se o usuário já existe
existe <- dbGetQuery(con_usuarios, "
  SELECT COUNT(*) as total FROM usuarios WHERE usuario = 'rafasfer2'
")

if (existe$total == 0) {
  # Inserir novo administrador
  dbExecute(con_usuarios, "
    INSERT INTO usuarios (nome, cpf, email, usuario, senha, nivel, aprovado)
    VALUES ('Rafael da Silva Fernandes', '057.424.596-02', 'rafasfer2@gmail.com', 'rafasfer2', 'senha123', 'admin', 1)
  ")
} else {
  # Atualizar dados do administrador
  dbExecute(con_usuarios, "
    UPDATE usuarios
    SET senha = 'senha123', nivel = 'admin', aprovado = 1
    WHERE usuario = 'rafasfer2'
  ")
}

# Verificar resultado
print(dbGetQuery(con_usuarios, "SELECT * FROM usuarios WHERE usuario = 'rafasfer2'"))

dbDisconnect(con_usuarios)


ui <- fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
  
  # Máscaras e estilos
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jquery.mask/1.14.16/jquery.mask.min.js"),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('applyMasks', function(message) {
        $('#part_cpf').mask('000.000.000-00');
        $('#part_tel').mask('(00) 00000-0000');
      });
    ")),
    tags$style(HTML("
      html, body {
        height: 100%;
        margin: 0;
        padding: 0;
      }
      .main-container {
        min-height: calc(100vh - 100px);
        padding-bottom: 20px;
      }
      .rodape-global {
        background-color: #f0f0f0;
        color: #555;
        text-align: center;
        padding: 15px;
        font-size: 14px;
        border-top: 1px solid #ccc;
      }
      .erro input, .erro select, .erro textarea {
        border-color: red !important;
        box-shadow: 0 0 5px red !important;
      }
    ")),
    tags$script(HTML("
      $(document).on('click', '.editar', function() {
        var linha = $(this).data('linha');
        Shiny.setInputValue('editar_linha', linha, {priority: 'event'});
      });
      $(document).on('click', '.remover', function() {
        var linha = $(this).data('linha');
        Shiny.setInputValue('remover_linha', linha, {priority: 'event'});
      });
    ")),
    tags$script(HTML("
      $(document).on('click', '.editar', function() {
        var linha = $(this).data('linha');
        Shiny.setInputValue('editar_linha', linha, {priority: 'event'});
      });
      $(document).on('click', '.remover', function() {
        var linha = $(this).data('linha');
        Shiny.setInputValue('remover_linha', linha, {priority: 'event'});
      });
    ")),
    tags$script(HTML("
      $(document).on('click', '.aprovar', function() {
        var id = $(this).data('id');
        Shiny.setInputValue('aprovar_usuario', id, {priority: 'event'});
      });
      $(document).on('click', '.rejeitar', function() {
        var id = $(this).data('id');
        Shiny.setInputValue('rejeitar_usuario', id, {priority: 'event'});
      });
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateProfissional', function(value) {
        $('#profissional_movel').val(value);
      });
    ")),
    tags$style(HTML("
  #login_btn {
    width: 100%;
    font-size: 18px;
  }
"))
  ),
  
  titlePanel("Formulário de Frequência por Polo"),
  
  tabsetPanel(id = "abas",
              
              # Aba 1 – Início
              tabPanel("Início",
                       fluidRow(
                         column(4, offset = 4,
                                h3("🔐 Acesso ao Sistema"),
                                textInput("login_id", "CPF, Usuário ou E-mail"),
                                passwordInput("login_senha", "Senha"),
                                actionButton("login_btn", "Entrar", class = "btn btn-success btn-lg"),
                                br(),
                                conditionalPanel("false",  # invisível por padrão
                                                 actionButton("restaurar_admin", "🔧 Restaurar Acesso Admin")
                                ),
                                br(), br(),
                                actionLink("novo_cadastro", "🆕 Novo Cadastro"),
                                br(),
                                actionLink("recuperar_senha", "❓ Esqueci minha senha" )
                         )
                       )
              ),
              
              # Aba 2 – Profissional e Polo
              tabPanel("Profissional e Polo",
                       sidebarLayout(
                         sidebarPanel(
                           labelObrigatorio("Profissional Responsável"),
                           tags$div(
                             class = "form-group shiny-input-container",
                             tags$label("Profissional Responsável"),
                             tags$input(id = "profissional_movel", type = "text", class = "form-control", value = "", readonly = NA)
                           ),
                           
                           labelObrigatorio("Polo Visitado"),
                           selectInput("polo_visitado", NULL,
                                       choices = c("Selecione", "Polo 01 - Cedere 1", "Polo 02 - Palmares 2",
                                                   "Polo 03 - Valentim Serra", "Polo 04 - Paulo Fonteles",
                                                   "Polo 05 - Vila Carimã", "Polo 06 - Vila Brasil",
                                                   "Polo 07 - Vila Alto Bonito", "Polo 08 - Vila Sansão", "Outros")),
                           
                           conditionalPanel(
                             condition = "input.polo_visitado == 'Outros'",
                             textInput("polo_outros", "✏️ Informe o nome do polo visitado")
                           ),
                           
                           labelObrigatorio("Data da Visita"),
                           dateInput("data_visita", NULL),
                           
                           actionButton("voltar2", "⬅️ Voltar"),
                           actionButton("avancar2", "Avançar ➡️")
                         ),
                         mainPanel()
                       )
              ),
              
              # Aba 3 – Participantes
              tabPanel("Participantes",
                       sidebarLayout(
                         sidebarPanel(
                           labelObrigatorio("Nome Completo"),
                           textInput("part_nome", NULL),
                           
                           labelObrigatorio("Sexo"),
                           selectInput("part_sexo", NULL,
                                       choices = c("Selecione", "Feminino", "Masculino", "Outro")),
                           
                           labelObrigatorio("Telefone"),
                           textInput("part_tel", NULL),
                           
                           labelObrigatorio("CPF"),
                           textInput("part_cpf", NULL),
                           
                           labelObrigatorio("Endereço Residencial"),
                           textInput("part_end", NULL),
                           
                           actionButton("add_part", "➕ Adicionar Participante"),
                           br(), br(),
                           actionButton("voltar3", "⬅️ Voltar"),
                           actionButton("avancar3", "Avançar ➡️"),
                           
                           actionButton("confirmar_envio", "✅ Confirmar e Enviar"),
                           br(), br(),
                           conditionalPanel(
                             condition = "output.envio_finalizado == true",
                             actionButton("ir_consulta", "🔍 Consultar Registros"),
                             actionButton("novo_lancamento", "🆕 Novo Lançamento")
                           )
                         ),
                         mainPanel(
                           h4("📋 Participantes Adicionados"),
                           DTOutput("tabela_participantes"),
                           br(),
                           h4("📋 Tabela Completa"),
                           DTOutput("tabela_resumo_participantes")
                         )
                       )
              ),
              
              # Aba 4 – Consulta e Exportação
              tabPanel("Consulta e Exportação",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("filtro_prof", "👤 Filtrar por Profissional:",
                                       choices = c("Todos", "Elisangela Moreira", "Eleusa",
                                                   "Josélia Viana", "Sandra Araújo", "Keylla Alves da Silva")),
                           selectInput("filtro_polo", "📍 Filtrar por Polo:",
                                       choices = c("Todos", "Polo 01 - Cedere 1", "Polo 02 - Palmares 2",
                                                   "Polo 03 - Valentim Serra", "Polo 04 - Paulo Fonteles",
                                                   "Polo 05 - Vila Carimã", "Polo 06 - Vila Brasil",
                                                   "Polo 07 - Vila Alto Bonito", "Polo 08 - Vila Sansão", "Outros")),
                           dateRangeInput("filtro_data", "📅 Intervalo de Datas:",
                                          start = Sys.Date() - 30, end = Sys.Date()),
                           actionButton("filtrar_dados", "🔍 Aplicar Filtros"),
                           br(), br(),
                           downloadButton("exportar_filtrado", "📥 Exportar Dados Filtrados"),
                           hr(),
                           textInput("busca_participante", "🔍 Buscar histórico por nome ou CPF"),
                           actionButton("buscar_historico", "Ver Histórico")
                         ),
                         mainPanel(
                           h4("📊 Resultados Filtrados"),
                           DTOutput("tabela_filtrada"),
                           br(),
                           h4("📜 Histórico da Participante"),
                           DTOutput("tabela_historico")
                         )
                       )
              ),
              tabPanel("Administração",
                       fluidRow(
                         column(12,
                                h3("👥 Gerenciamento de Usuários"),
                                DTOutput("tabela_usuarios"),
                                br(),
                                actionButton("atualizar_usuarios", "🔄 Atualizar Lista")
                         )
                       )
              )
  )
)

server <- function(input, output, session) {
  # Pacotes e variáveis
  library(RSQLite)
  library(dotenv)
  library(emayili)
  library(digest)
  load_dot_env(".env")
  
  # Conexões com bancos
  con <- dbConnect(SQLite(), "frequencia.db")
  con_usuarios <- dbConnect(SQLite(), "usuarios.db")
  
  # Criar tabela de registros
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS registros (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      Nome TEXT, Sexo TEXT, Telefone TEXT, CPF TEXT, Endereco TEXT,
      Profissional TEXT, Polo TEXT, Data TEXT, Protocolo TEXT, Usuario TEXT
    )
  ")
  
  # Criar tabela de usuários
  dbExecute(con_usuarios, "
    CREATE TABLE IF NOT EXISTS usuarios (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      nome TEXT, cpf TEXT, email TEXT, usuario TEXT, senha TEXT,
      nivel TEXT, aprovado INTEGER DEFAULT 0
    )
  ")
  
  # Inserir administrador se não existir
  existe <- dbGetQuery(con_usuarios, "SELECT COUNT(*) as total FROM usuarios WHERE usuario = 'rafasfer2'")
  if (existe$total == 0) {
    dbExecute(con_usuarios, "
      INSERT INTO usuarios (nome, cpf, email, usuario, senha, nivel, aprovado)
      VALUES (?, ?, ?, ?, ?, ?, 1)
    ", params = c("Rafael da Silva Fernandes", "057.424.596-02", "rafasfer2@gmail.com", "rafasfer2", "senha123", "admin"))
  }
  
  # Sessão de login
  usuario_logado <- reactiveVal(NULL)
  
  # Verifica se é administrador
  is_admin <- reactive({
    req(usuario_logado())
    usuario_logado()$nivel[1] == "admin"
  })
  
  # Ação ao clicar em "Entrar"
  observeEvent(input$login_btn, {
    req(input$login_id, input$login_senha)
    
    resultado <- dbGetQuery(con_usuarios, "
    SELECT * FROM usuarios
    WHERE (cpf = ? OR email = ? OR usuario = ?)
    AND senha = ?
    AND aprovado = 1
  ", params = c(input$login_id, input$login_id, input$login_id, input$login_senha))
    
    if (nrow(resultado) == 1) {
      usuario_logado(resultado)
      
      # Atualiza campo de profissional
      session$sendCustomMessage(type = "updateProfissional", message = resultado$nome[1])
      
      # Direciona para aba correta
      if (resultado$nivel[1] == "admin") {
        updateTabsetPanel(session, "abas", selected = "Administração")
      } else {
        updateTabsetPanel(session, "abas", selected = "Profissional e Polo")
      }
      
      showNotification(paste("Bem-vindo,", resultado$nome[1]), type = "message")
    } else {
      showNotification("Credenciais inválidas ou usuário não aprovado.", type = "error")
    }
  })
  
  
  # Renderizar tabela de usuários pendentes
  output$tabela_usuarios <- renderDT({
    req(is_admin())
    dbGetQuery(con_usuarios, "SELECT * FROM usuarios WHERE aprovado = 0")
  })
  
  # Aprovar usuário
  observeEvent(input$aprovar_usuario, {
    req(is_admin())
    dbExecute(con_usuarios, "UPDATE usuarios SET aprovado = 1 WHERE id = ?", params = input$aprovar_usuario)
    showNotification("Usuário aprovado com sucesso!", type = "message")
  })
  
  
  # Rejeitar usuário
  observeEvent(input$rejeitar_usuario, {
    req(is_admin())
    dbExecute(con_usuarios, "DELETE FROM usuarios WHERE id = ?", params = input$rejeitar_usuario)
    showNotification("Usuário rejeitado e removido.", type = "warning")
  })
  
  # Encerrar conexões ao fechar app
  onStop(function() {
    dbDisconnect(con)
    dbDisconnect(con_usuarios)
  })
  
  # Lista reativa de participantes
  participantes <- reactiveVal(data.frame(
    Nome = character(),
    Sexo = character(),
    Telefone = character(),
    CPF = character(),
    Endereço = character(),
    stringsAsFactors = FALSE
  ))
  
  participantesTemp <- reactiveVal(data.frame(
    Nome = character(),
    Sexo = character(),
    Telefone = character(),
    CPF = character(),
    Endereço = character(),
    Profissional = character(),
    Polo = character(),
    Data = as.Date(character()),
    stringsAsFactors = FALSE
  ))
  
  participantesFinal <- reactiveVal(data.frame(
    Nome = character(),
    Sexo = character(),
    Telefone = character(),
    CPF = character(),
    Endereço = character(),
    Profissional = character(),
    Polo = character(),
    Data = as.Date(character()),
    stringsAsFactors = FALSE
  ))
  
  visita <- reactiveVal(NULL)
  
  # Adiciona participante com validação mínima
  observeEvent(input$add_part, {
    if (input$part_nome == "" || input$part_sexo == "Selecione") {
      showNotification("Preencha o nome completo e selecione o sexo.", type = "error")
    } else {
      v <- visita()
      novo <- data.frame(
        Nome = input$part_nome,
        Sexo = input$part_sexo,
        Telefone = input$part_tel,
        CPF = input$part_cpf,
        Endereço = input$part_end,
        Profissional = v$profissional,
        Polo = v$polo,
        Data = v$data,
        stringsAsFactors = FALSE
      )
      participantesTemp(rbind(participantesTemp(), novo))
      session$sendCustomMessage("applyMasks", "ok")
    }
  })
  
  # Tabela com botões de ação
  output$tabela_participantes <- renderDT({
    dados <- participantes()
    if (nrow(dados) == 0) return(NULL)
    
    dados$Ações <- paste0(
      '<button class="editar btn btn-sm btn-warning" data-linha="', seq_len(nrow(dados)), '">✏️ Alterar</button> ',
      '<button class="remover btn btn-sm btn-danger" data-linha="', seq_len(nrow(dados)), '">❌ Remover</button>'
    )
    
    datatable(
      dados,
      escape = FALSE,
      selection = "none",
      rownames = FALSE,
      options = list(
        pageLength = 5,
        dom = 't',
        columnDefs = list(list(targets = ncol(dados), orderable = FALSE))
      )
    )
  })
  
  # Tabela completa (sem botões)
  output$tabela_resumo_participantes <- renderDT({
    dados <- participantes()
    if (nrow(dados) == 0) return(NULL)
    datatable(dados[, 1:5], options = list(pageLength = 5))
  })
  
  # Editar participante
  observeEvent(input$editar_linha, {
    linha <- as.numeric(input$editar_linha)
    dados <- participantes()
    if (linha >= 1 && linha <= nrow(dados)) {
      updateTextInput(session, "part_nome", value = dados$Nome[linha])
      updateSelectInput(session, "part_sexo", selected = dados$Sexo[linha])
      updateTextInput(session, "part_tel", value = dados$Telefone[linha])
      updateTextInput(session, "part_cpf", value = dados$CPF[linha])
      updateTextInput(session, "part_end", value = dados$Endereço[linha])
      participantes(dados[-linha, ])
      session$sendCustomMessage("applyMasks", "ok")
    }
  })
  
  # Remover participante
  observeEvent(input$remover_linha, {
    linha <- as.numeric(input$remover_linha)
    dados <- participantes()
    if (linha >= 1 && linha <= nrow(dados)) {
      participantes(dados[-linha, ])
    }
  })
  
  # Navegação entre abas
  observeEvent(input$avancar1, { updateTabsetPanel(session, "abas", selected = "Profissional e Polo") })
  observeEvent(input$voltar2, { updateTabsetPanel(session, "abas", selected = "Início") })
  observeEvent(input$avancar2, {
    polo_final <- if (input$polo_visitado == "Outros") input$polo_outros else input$polo_visitado
    visita(list(
      profissional = input$profissional_movel,
      polo = polo_final,
      data = input$data_visita
    ))
    updateTabsetPanel(session, "abas", selected = "Participantes")
  })
  observeEvent(input$voltar3, { updateTabsetPanel(session, "abas", selected = "Profissional e Polo") })
  observeEvent(input$avancar3, { updateTabsetPanel(session, "abas", selected = "Consulta e Exportação") })
  
  # Filtros e exportação
  observeEvent(input$filtrar_dados, {
    query <- "SELECT * FROM registros WHERE 1=1"
    params <- list()
    
    if (input$filtro_prof != "Todos") {
      query <- paste(query, "AND Profissional = ?")
      params <- c(params, input$filtro_prof)
    }
    if (input$filtro_polo != "Todos") {
      query <- paste(query, "AND Polo = ?")
      params <- c(params, input$filtro_polo)
    }
    if (!is.null(input$filtro_data)) {
      query <- paste(query, "AND date(Data) BETWEEN ? AND ?")
      params <- c(params, as.character(input$filtro_data[1]), as.character(input$filtro_data[2]))
    }
    
    dados <- dbGetQuery(con, query, params = params)
    
    output$tabela_filtrada <- renderDT({
      datatable(dados, options = list(pageLength = 10))
    })
    
    output$exportar_filtrado <- downloadHandler(
      filename = function() { paste("dados_filtrados_", Sys.Date(), ".csv", sep = "") },
      content = function(file) { write.csv(dados, file, row.names = FALSE) }
    )
  })
  
  # Histórico por participante
  observeEvent(input$buscar_historico, {
    termo <- input$busca_participante
    dados <- participantes()
    historico <- subset(dados, grepl(termo, Nome, ignore.case = TRUE) | grepl(termo, CPF))
    output$tabela_historico <- renderDT({
      datatable(historico, options = list(pageLength = 10))
    })
  })
  
  output$envio_finalizado <- reactive({
    envio_finalizado()
  })
  outputOptions(output, "envio_finalizado", suspendWhenHidden = FALSE)
  
  envio_finalizado <- reactiveVal(FALSE)
  
  observeEvent(input$confirmar_envio, {
    dados <- participantesTemp()
    if (nrow(dados) == 0) {
      showNotification("Adicione pelo menos um participante antes de enviar.", type = "error")
    } else {
      protocolo <- paste0("PRT-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", sample(1000:9999, 1))
      profissional <- input$profissional_movel
      usuario <- usuario_logado()$usuario[1]
      
      for (i in seq_len(nrow(dados))) {
        dbExecute(con, "INSERT INTO registros (Nome, Sexo, Telefone, CPF, Endereco, Profissional, Polo, Data, Protocolo, Usuario)
                      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
                  params = c(as.list(dados[i, 1:5]), profissional, dados$Polo[i], dados$Data[i], protocolo, usuario))
      }
      
      envio_finalizado(TRUE)
      participantesTemp(dados[0, ])
      showNotification(paste("Registro salvo com sucesso! Protocolo:", protocolo), type = "message")
    }
  })
  
  observeEvent(input$ir_consulta, {
    updateTabsetPanel(session, "abas", selected = "Consulta e Exportação")
  })
  
  observeEvent(input$novo_lancamento, {
    # Limpa os campos e dados
    updateTextInput(session, "part_nome", value = "")
    updateSelectInput(session, "part_sexo", selected = "Selecione")
    updateTextInput(session, "part_tel", value = "")
    updateTextInput(session, "part_cpf", value = "")
    updateTextInput(session, "part_end", value = "")
    participantes(data.frame(
      Nome = character(),
      Sexo = character(),
      Telefone = character(),
      CPF = character(),
      Endereço = character(),
      Profissional = character(),
      Polo = character(),
      Data = as.Date(character()),
      stringsAsFactors = FALSE
    ))
    envio_finalizado(FALSE)
    updateTabsetPanel(session, "abas", selected = "Profissional e Polo")
  })
  
  observeEvent(input$editar_linha, {
    linha <- as.numeric(input$editar_linha)
    dados <- participantesTemp()
    if (linha >= 1 && linha <= nrow(dados)) {
      updateTextInput(session, "part_nome", value = dados$Nome[linha])
      updateSelectInput(session, "part_sexo", selected = dados$Sexo[linha])
      updateTextInput(session, "part_tel", value = dados$Telefone[linha])
      updateTextInput(session, "part_cpf", value = dados$CPF[linha])
      updateTextInput(session, "part_end", value = dados$Endereço[linha])
      participantesTemp(dados[-linha, ])
      session$sendCustomMessage("applyMasks", "ok")
    }
  })
  
  observeEvent(input$remover_linha, {
    linha <- as.numeric(input$remover_linha)
    dados <- participantesTemp()
    if (linha >= 1 && linha <= nrow(dados)) {
      participantesTemp(dados[-linha, ])
    }
  })
  
  observeEvent(input$buscar_historico, {
    termo <- input$busca_participante
    dados <- dbGetQuery(con, "SELECT * FROM registros WHERE Nome LIKE ? OR CPF LIKE ?",
                        params = c(paste0("%", termo, "%"), paste0("%", termo, "%")))
    output$tabela_historico <- renderDT({
      datatable(dados, options = list(pageLength = 10))
    })
  })
  
  observeEvent(input$novo_cadastro, {
    showModal(modalDialog(
      title = "Novo Cadastro",
      textInput("cad_nome", "Nome Completo"),
      textInput("cad_cpf", "CPF"),
      textInput("cad_email", "E-mail"),
      textInput("cad_usuario", "Nome de Usuário"),
      passwordInput("cad_senha", "Senha"),
      actionButton("confirmar_cadastro", "Enviar Cadastro"),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$confirmar_cadastro, {
    dbExecute(con_usuarios, "
    INSERT INTO usuarios (nome, cpf, email, usuario, senha, aprovado)
    VALUES (?, ?, ?, ?, ?, 0)
  ", params = c(input$cad_nome, input$cad_cpf, input$cad_email, input$cad_usuario, input$cad_senha))
    
    removeModal()
    showNotification("Cadastro enviado! Aguarde aprovação do administrador.", type = "message")
  })
  
  observeEvent(input$recuperar_senha, {
    showModal(modalDialog(
      title = "Recuperar Acesso",
      textInput("rec_email", "Informe seu e-mail cadastrado"),
      actionButton("enviar_recuperacao", "Enviar Instruções"),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$enviar_recuperacao, {
    showNotification("Instruções de recuperação enviadas (simulado).", type = "message")
    removeModal()
  })
  
  output$tabela_usuarios <- renderDT({
    req(is_admin())
    dados <- dbGetQuery(con_usuarios, "SELECT * FROM usuarios WHERE aprovado = 0")
    if (nrow(dados) == 0) return(NULL)
    
    dados$Ações <- paste0(
      '<button class="aprovar btn btn-sm btn-success" data-id="', dados$id, '">✅ Aprovar</button> ',
      '<button class="rejeitar btn btn-sm btn-danger" data-id="', dados$id, '">❌ Rejeitar</button>'
    )
    
    datatable(dados, escape = FALSE, selection = "none", rownames = FALSE)
  })
  
  observeEvent(input$aprovar_usuario, {
    dbExecute(con_usuarios, "UPDATE usuarios SET aprovado = 1 WHERE id = ?", params = input$aprovar_usuario)
    showNotification("Usuário aprovado com sucesso!", type = "message")
  })
  
  observeEvent(input$rejeitar_usuario, {
    dbExecute(con_usuarios, "DELETE FROM usuarios WHERE id = ?", params = input$rejeitar_usuario)
    showNotification("Usuário rejeitado e removido.", type = "warning")
  })
  
  observeEvent(input$atualizar_usuarios, {
    output$tabela_usuarios <- renderDT({
      dados <- dbGetQuery(con_usuarios, "SELECT id, nome, cpf, email, usuario, aprovado FROM usuarios")
      dados$Ações <- paste0(
        '<button class="aprovar btn btn-sm btn-success" data-id="', dados$id, '">✅ Aprovar</button> ',
        '<button class="rejeitar btn btn-sm btn-danger" data-id="', dados$id, '">❌ Rejeitar</button>'
      )
      datatable(
        dados,
        escape = FALSE,
        selection = "none",
        rownames = FALSE,
        options = list(pageLength = 10, columnDefs = list(list(targets = ncol(dados), orderable = FALSE)))
      )
    })
  })
  

  criar_email_aprovacao <- function(nome, usuario) {
    compose_email(
      body = md(glue::glue("
      ## Cadastro aprovado!  
      Olá **{nome}**,  

      Seu cadastro foi aprovado com sucesso.  
      Você já pode acessar o sistema com seu login: `{usuario}`  

      ---
      _Este é um e-mail automático. Não responda._
    "))
    )
  }
  
  enviar_email <- function(destinatario, email_objeto) {
    smtp_send(
      email = email_objeto,
      from = "seuemail@gmail.com",
      to = destinatario,
      subject = "Cadastro aprovado!",
      credentials = creds(
        host = "smtp.gmail.com",
        port = 587,
        user = "seuemail@gmail.com",
        password = Sys.getenv("EMAIL_SENHA"),
        use_ssl = TRUE
      )
    )
  }
  
  # Envio de e-mail ao confirmar
  observeEvent(input$confirmar_envio, {
    req(usuario_logado())
    
    nome <- input$part_nome
    cpf <- input$part_cpf
    telefone <- input$part_tel
    
    html_corpo <- sprintf('
      <table style="width:100%%; font-family:Arial, sans-serif; background-color:#f0f0f0; padding:20px;">
        <tr>
          <td style="text-align:center;">
            <h2 style="color:#2c3e50;">Cadastro aprovado com sucesso!</h2>
            <p style="font-size:16px; color:#34495e;">
              Olá %s,<br><br>
              Seu cadastro foi aprovado no sistema SEMMU.<br>
              CPF: %s<br>
              Telefone: %s<br><br>
              Você já pode acessar a plataforma com seu login.<br><br>
              Em caso de dúvidas, entre em contato com nossa equipe.
            </p>
            <hr style="margin:30px 0;">
            <p style="font-size:12px; color:#7f8c8d;">
              Este e-mail foi gerado automaticamente. Não responda.<br>
              © 2025 SEMMU - Secretaria Municipal da Mulher
            </p>
          </td>
        </tr>
      </table>
    ', nome, cpf, telefone)
    
    email <- envelope() |>
      from(Sys.getenv("EMAIL_USUARIO")) |>
      to("rafasfer2.reserve@gmail.com") |>
      subject("Cadastro aprovado - SEMMU") |>
      html(html_corpo)
    
    smtp <- emayili::server(
      host = "smtp.gmail.com",
      port = 587,
      username = Sys.getenv("EMAIL_USUARIO"),
      password = Sys.getenv("EMAIL_SENHA")
    )
    
    tryCatch({
      smtp(email)
      showNotification("✅ E-mail enviado com sucesso!", type = "message")
      output$envio_finalizado <- reactive({ TRUE })
    }, error = function(e) {
      showNotification(paste("❌ Erro ao enviar:", e$message), type = "error")
      output$envio_finalizado <- reactive({ FALSE })
    })
  })
  
  # Exportação e filtros (exemplo básico)
  output$tabela_filtrada <- renderDT({
    datatable(participantes(), options = list(pageLength = 10))
  })
  
  output$tabela_historico <- renderDT({
    filtro <- input$busca_participante
    dados <- participantes()
    resultado <- subset(dados, grepl(filtro, Nome) | grepl(filtro, CPF))
    datatable(resultado, options = list(pageLength = 5))
  })
  
  output$exportar_filtrado <- downloadHandler(
    filename = function() {
      paste("dados_filtrados_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(participantes(), file, row.names = FALSE)
    }
  )
  
  # Envio de e-mail (exemplo)
  observeEvent(input$confirmar_envio, {
    nome <- input$part_nome
    cpf <- input$part_cpf
    telefone <- input$part_tel
    
    html_corpo <- sprintf("...")  # corpo HTML formatado
    
    email <- envelope() |>
      from(Sys.getenv("EMAIL_USUARIO")) |>
      to("rafasfer2.reserve@gmail.com") |>
      subject("Cadastro aprovado - SEMMU") |>
      html(html_corpo)
    
    smtp <- emayili::server(
      host = "smtp.gmail.com",
      port = 587,
      username = Sys.getenv("EMAIL_USUARIO"),
      password = Sys.getenv("EMAIL_SENHA")
    )
    
    tryCatch({
      smtp(email)
      envio_finalizado(TRUE)
      showNotification("✅ E-mail enviado com sucesso!", type = "message")
    }, error = function(e) {
      envio_finalizado(FALSE)
      showNotification(paste("❌ Erro ao enviar:", e$message), type = "error")
    })
  })
  
  # Atualizações de UI e tabelas
  outputOptions(output, "envio_finalizado", suspendWhenHidden = FALSE)
  
}

shinyApp(ui, server)
