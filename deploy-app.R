library(rsconnect)
source("auth/shiny_auth_gregcull.R")
rsconnect::setAccountInfo(name = shiny_account,
                          token = shiny_token,
                          secret = shiny_secret)

# Define name
name = "inhe_multi"

deployApp(appDir = ".", appName = name)
