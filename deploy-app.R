library(rsconnect)
source("auth/shiny_auth_frolle.R")
rsconnect::setAccountInfo(name = shiny_account,
                          token = shiny_token,
                          secret = shiny_secret)

# Define name
name = "inhe_prev"
deployApp(appDir = ".", appName = name)
