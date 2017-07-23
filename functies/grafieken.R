# For testing: load("testVergelijkLeningenShiny.RData")

leningGrafiek <- function(aflosTabel,
                          startDate,
                          kolommen, 
                          inflatie = FALSE, 
                          inflatiePerc = 0,
                          cumulatief = FALSE){
  
  if(!cumulatief && "vermogen" %in% kolommen){
    kolommen[which(kolommen == "vermogen")] <- "vermogenVerschil"
  }
  
  if(cumulatief){
    for(kolom in kolommen){
      aflosTabel[, cumul := cumsum(get(kolom))]
      
      aflosTabel[[paste0("cumulatief_", kolom)]] <- aflosTabel$cumul
    }
    kolommen <- paste0("cumulatief_", kolommen)
    
    if("cumulatief_vermogen" %in% kolommen){
      kolommen[which(kolommen == "cumulatief_vermogen")] <- "vermogen"
    }
  }
  
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
  
  # rCharts code (werkt niet op shinyapps.io)
  #  
  # aflosTabel$maand <- format(as.Date(paste0(startDate, "-01"), format = "%Y-%m-%d") +
  #                              months(aflosTabel$maand - 1), "%Y-%m")
  # plot <- mPlot(x = "maand", y = kolommen, type = "Line", data = aflosTabel)
  # plot$set(pointSize = 0, lineWidth = 1)
  # plot$set(dom = 'graph')
  
  
  
  # Ggplot2
  aflosTabel$maand <- as.Date(paste0(startDate, "-01"), format = "%Y-%m-%d") +
                               months(aflosTabel$maand - 1)
  
  aflosTabel <- aflosTabel[, c("maand", kolommen), with = FALSE]
  aflosTabelMelt <- melt(aflosTabel, id.vars = "maand")
  names(aflosTabelMelt)[which(names(aflosTabelMelt) == "value")] <- "Euro"
  names(aflosTabelMelt)[which(names(aflosTabelMelt) == "variable")] <- "Variabele"
  
  plot <- ggplot(aflosTabelMelt) + 
    geom_line(aes(maand, Euro, col = Variabele))
  
  return(list(plot = plot,
              tabel = aflosTabel))
}

# For testing: load("testVergLeningGrafiek.RData")
vergLeningGrafiek <- function(opgeslagenAflosTabellen, 
                              kolom){
  if(is.null(opgeslagenAflosTabellen))
    return(NULL)
  
  leningen <- names(opgeslagenAflosTabellen)
  
  data <- data.table()
  nietMeegerekendeLeningen <- c()
  for(lening in leningen){
    leningData <- data.table(lening = lening, 
                             maand = opgeslagenAflosTabellen[[lening]]$maand, 
                             metriek = opgeslagenAflosTabellen[[lening]][[kolom]])
    if(length(unique(leningData$metriek)) > 2){
      data <- rbind(data, leningData)
    } else {
      nietMeegerekendeLeningen <- c(nietMeegerekendeLeningen, lening)
    }
  }
  
  dataCast <- dcast(data, maand ~ lening, value.var = "metriek")
  
  # Plot
  colnames(data)[which(colnames(data) == "metriek")] <- kolom
  plot <- ggplot(data) + 
    geom_line(aes_string("maand", kolom, col = "lening")) +
    scale_y_continuous(name ="Euro", labels = comma)
  
  return(list(plot = plot,
              tabel = data))
}