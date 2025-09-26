library(emayili)
library(dotenv)

# Carregar variáveis do .env
load_dot_env(".env")

# Criar corpo do e-mail em HTML
html_corpo <- '
<table style="width:100%; font-family:Arial, sans-serif; background-color:#f0f0f0; padding:20px;">
  <tr>
    <td style="text-align:center;">
      <h2 style="color:#2c3e50;">Teste de envio de e-mail</h2>
      <p style="font-size:16px; color:#34495e;">
        Olá Rafael,<br><br>
        Este é um teste automático para validar o envio de e-mails via <strong>emayili</strong>.<br>
        Se você recebeu esta mensagem, está tudo funcionando perfeitamente!<br><br>
        Abraços,<br>
        <em>Sistema SEMMU</em>
      </p>
      <hr style="margin:30px 0;">
      <p style="font-size:12px; color:#7f8c8d;">
        Este e-mail foi gerado automaticamente. Não responda.
      </p>
    </td>
  </tr>
</table>
'

# Criar envelope
email <- envelope() |>
  from(Sys.getenv("EMAIL_USUARIO")) |>
  to("rafasfer2.reserve@gmail.com") |>
  subject("🔧 Teste de envio - SEMMU") |>
  html(html_corpo)

# Configurar servidor SMTP
smtp <- server(
  host = "smtp.gmail.com",
  port = 587,
  username = Sys.getenv("EMAIL_USUARIO"),
  password = Sys.getenv("EMAIL_SENHA")
)

# Enviar e-mail
smtp(email)