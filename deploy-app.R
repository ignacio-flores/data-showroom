library(rsconnect)
source("auth/shiny_auth.R")
rsconnect::setAccountInfo(name='gcwealth',
                          token=shiny_token,
                          secret=shiny_secret)

# Define the paths
deployApp(appDir = "~/Documents/GitHub/data-showroom", appName = "ineq-country-view")
#deployApp(appDir = "~/Documents/GitHub/data-showroom", appName = "ineq-country-comp")
#deployApp(appDir = "~/Documents/GitHub/data-showroom", appName = "ineq-prev")
#deployApp(appDir = "~/Documents/GitHub/data-showroom", appName = "topo-country-comp")
#deployApp(appDir = "~/Documents/GitHub/data-showroom", appName = "topo-source-comp")
#deployApp(appDir = "~/Documents/GitHub/data-showroom", appName = "topo-aba1")
#deployApp(appDir = "~/Documents/GitHub/data-showroom", appName = "topo-country-view")
#deployApp(appDir = "~/Documents/GitHub/data-showroom", appName = "topo-prev")


rsconnect::appDependencies()