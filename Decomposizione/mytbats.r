library(forecast)
library(zoo)
library(imputeTS)
library(lubridate)


decompose = function(series, seasons) {
  x = msts(series, seasonal.period=seasons)
  
  fit = tbats(
    x,
    use.box.cox = FALSE, #mettendo null, prova true / false e sceglie in base all'aic
    use.trend = NULL,
    use.damped.trend = FALSE,
    use.arma.errors = TRUE,
    use.parallel = TRUE,
    num.cores = NULL,
    bc.lower = 0,
    bc.upper = 1,
    biasadj = FALSE
  )
  
  plot(fit)
  
  plot(residuals(fit))
  resid = residuals(fit)
  
  dfcomp = tbats.components(fit)
  dfcomp = cbind(dfcomp, data.frame(resid))
  dfcomp = dfcomp[, c("dfcomp.observed", "dfcomp.level", "dfcomp.season1", "dfcomp.season2", "data.frame(resid)")]

  colnames(dfcomp) = c("observed", "trend", "season_daily", "season_weekly", "resid")

  print(fit$AIC)
  return(dfcomp)
}




setwd("c:\\Users\\leona\\Google Drive\\Università\\4\\Data Science Lab\\progetto\\")

cutz = c("2018-01-01", "2018-03-01", "2018-09-01", "2019-03-01",
         "2019-09-01", "2020-03-01", "2020-09-01")


for (cut in cutz) {

  filename = paste(cut, "00_00_00.csv")
  semester_df = read.csv(filename, sep=",")
  
  y = semester_df$consumo_attiva
  
  index = semester_df$dataora

  seasons = c(24, 24*7)
  dfcomp = decompose(y, seasons)
  
}



