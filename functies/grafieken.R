leningGrafiek <- function(aflosTabel,
                          startDate,
                          kolommen, 
                          inflatie = FALSE, 
                          inflatiePerc = 0){
  if(inflatie){
    inflatieMaand <- (1+inflatiePerc/100)^(1/12)
    
    for(kolom in kolommen){
      aflosTabel[, inflatie := get(kolom)/inflatieMaand^maand]
      aflosTabel
      
      aflosTabel[[paste0(kolom, "_inflatie")]] <- round(aflosTabel$inflatie, 2)
    }
    aflosTabel$inflatie <- NULL
    kolommen <- paste0(kolommen, "_inflatie")
  }
  
  aflosTabel$maand <- format(as.Date(paste0(startDate, "-01"), format = "%Y-%m-%d") + 
                               months(aflosTabel$maand - 1), "%Y-%m")
  plot <- mPlot(x = "maand", y = kolommen, type = "Line", data = aflosTabel)
  plot$set(pointSize = 0, lineWidth = 1)
  plot$set(dom = 'graph')
  #plot$set(ymin = 0)
  
  plot
}