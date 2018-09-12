install.packages('rsconnect')

rsconnect::setAccountInfo(name='xuz074',
                          token='1FD5005251D0CA1C80CEAED2B3E8AA8A',
                          secret='DSPe6UpW8swDs9P3vohE9hpLI+L016G5c9BvtdkT')

library(rsconnect)
install.packages('shinyapps')
library(shinyapps)

shinyapps::setAccountInfo(name='xuz074',
                          token='1FD5005251D0CA1C80CEAED2B3E8AA8A',
                          secret='DSPe6UpW8swDs9P3vohE9hpLI+L016G5c9BvtdkT')
setwd("app")
runApp()

deployApp(appName = rchapp)

library(shinyAce)
