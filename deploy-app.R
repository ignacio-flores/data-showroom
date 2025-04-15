library(rsconnect)
source("auth/shiny_auth.R")
rsconnect::setAccountInfo(name='gcwealth',
                          token=shiny_token,
                          secret=shiny_secret)

# Define the paths

#deployApp(appDir = "~/Documents/GitHub/data-showroom", appName = "ineq-country-view")
deployApp(appDir = "~/Documents/GitHub/data-showroom", appName = "ineq-country-comp")
#deployApp(appDir = "~/Documents/GitHub/data-showroom", appName = "ineq-prev")
