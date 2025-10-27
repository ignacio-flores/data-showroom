library(rsconnect)
source("auth/shiny_auth_hubquin.R")
rsconnect::setAccountInfo(name=shiny_account,
                          token=shiny_token,
                          secret=shiny_secret)

# Define name
#name = "ineq-country-view"
#name = "ineq-country-comp"
#name = "ineq-prev"
#name = "topo-country-view"
#name = "topo-source-comp"
#name = "topo-country-comp"
#name = "topo-prev"
#name = "topo-aba1"
#name = "topo-aba2"
name = "topo-ffba3"
#name = "topo-ffba2"
#name = "topo-ffba3"
#name = "eigt-ft1"
#name = "eigt-ft2"
#name = "eigt-kf3"
#name = "eigt-wm2"

deployApp(appDir = ".", appName = name)
