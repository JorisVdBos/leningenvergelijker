leningGrafiek <- function(aflosTabel,
                          startDate,
                          kolommen, 
                          inflatie = FALSE, 
                          inflatiePerc = 0,
                          cumulatief = FALSE){
  if(is.null(kolommen))
    return(NULL)
  if(inflatie){
    inflatieMaand <- (1+inflatiePerc/100)^(1/12)
    
    for(kolom in kolommen){
      aflosTabel[, inflatie := get(kolom)/inflatieMaand^maand]
      
      aflosTabel[[paste0(kolom, "_inflatie")]] <- round(aflosTabel$inflatie, 2)
    }
    kolommen <- paste0(kolommen, "_inflatie")
  }
  
  if(cumulatief){
    for(kolom in kolommen){
      aflosTabel[, cumul := cumsum(get(kolom))]
      
      aflosTabel[[paste0("cumulatief_", kolom)]] <- aflosTabel$cumul
    }
    kolommen <- paste0("cumulatief_", kolommen)
  }
  
  # aflosTabel$maand <- as.Date(paste0(startDate, "-01"), format = "%Y-%m-%d") +
  #   months(aflosTabel$maand - 1)
  # melt <- reshape2::melt(aflosTabel[,c("maand", kolommen), with = FALSE],
  #                        id = "maand")
  # plot <- nPlot(value ~ maand, group = 'variable', data = melt, type = 'lineChart')
  # plot$xAxis( tickFormat="#!function(d) {return d3.time.format('%b %Y')(new Date( d * 86400000 ));}!#" )
  
  
  aflosTabel$maand <- format(as.Date(paste0(startDate, "-01"), format = "%Y-%m-%d") +
                               months(aflosTabel$maand - 1), "%Y-%m")

  plot <- mPlot(x = "maand", y = kolommen, type = "Line", data = aflosTabel)
  plot$set(pointSize = 0, lineWidth = 1)
  plot$set(dom = 'graph')
  #plot$set(ymin = 0)
  
  return(plot)
}