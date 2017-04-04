# Functies ----
simuleerLening <- function(leningTotaalEuro,
                           percentJaar, 
                           jaar,
                           type, 
                           afkoopjaar = 0){
  
  variabelen <- list(leningTotaalEuro = leningTotaalEuro,
                     percentJaar = percentJaar,
                     jaar = jaar,
                     type = type,
                     afkoopjaar = afkoopjaar)
  
  if(type == "vast")
    return(list(aflostabel = simuleerLeningVast(leningTotaalEuro,
                              percentJaar, 
                              jaar,
                              afkoopjaar),
                 kenmerken = variabelen))
  else if(grep("variabel", type) == 1){
    verdubbeljaar <- as.numeric(gsub("variabel", "", type))
    return(list(aflostabel = simuleerLeningVariabel(leningTotaalEuro,
                                                    percentJaar, 
                                                    jaar,
                                                    afkoopjaar,
                                                    verdubbeljaar),
                kenmerken = variabelen))
  }
}
# Simuleer lening vast ----
simuleerLeningVast <- function(leningTotaalEuro,
                           percentJaar, 
                           jaar, afkoopjaar = 0){
  
  if(length(leningTotaalEuro) > 1){
    return(meerLeningenVast(leningTotaalEuro,
                     percentJaar, 
                     jaar))
  }
  
  file <- "data/maandelijkseAflossingen.RData"
  if(file.exists(file)){
    load(file)
    a <- leningTotaalEuro
    b <- percentJaar
    c <- jaar
    previouslyCalculated <- maandelijkseAflossing[leningTotaalEuro == a & 
                                                    percentJaar == b & 
                                                    jaar == c]
  } else {
    maandelijkseAflossing <- NULL
    previouslyCalculated <- data.table()
  }
  
  if(dim(previouslyCalculated)[1] == 1){
    percentMaand <- (1+percentJaar)^(1/12) - 1
    aflostabelMid <- simuleerAflossingTabelVast(leningTotaalEuro,
                                                percentMaand, 
                                                jaar,
                                                previouslyCalculated$aflossingMaand)
  } else {
    
    
    # Maandelijkse aflossing zoeken
    min <- leningTotaalEuro/(jaar * 12)
    max <- leningTotaalEuro*(1+percentJaar)^jaar/(jaar*12) * 1.5
    mid <- (min + max)/2
    
    percentMaand <- (1+percentJaar)^(1/12) - 1
    
    repeat{
      # Aflossingen mid
      aflostabelMid <- simuleerAflossingTabelVast(leningTotaalEuro,
                                                  percentMaand, 
                                                  jaar,
                                                  mid)
      
      # Check open
      open <- aflostabelMid[jaar*12+1]$open
      if(open < 0.01 && open > 0)
        break()
      
      dum <- mid
      if(open < 0){
        if(mid == (min+mid)/2)
          break()
        mid <- (min+mid)/2
        max <- dum
      } else {
        if(mid ==  (max+mid)/2)
          break()
        mid <- (max+mid)/2
        min <- dum
      }
      
    }
    
    maandelijkseAflossing <- rbind(maandelijkseAflossing, data.table(
      leningTotaalEuro = leningTotaalEuro,
      percentJaar = percentJaar,
      jaar = jaar,
      aflossingMaand = mid))
    
    save(maandelijkseAflossing, file = file)
  }
  
  aflostabelMid <- afkopen(aflostabelMid, percentJaar, afkoopjaar)
  
  return(aflostabelMid)
}

# Simuleer lening Variabel ----
simuleerLeningVariabel <- function(leningTotaalEuro,
                                   percentJaar, 
                                   jaar, afkoopjaar = 0, 
                                   verdubbelJaar = 3){
  
  if(is.na(afkoopjaar))
    afkoopjaar = 0
  if(is.na(verdubbelJaar))
    verdubbelJaar = 3
  
  vast1 <- simuleerLeningVast(leningTotaalEuro,
                              percentJaar, 
                              jaar)
  # Na 3 jaar herziening
  if(jaar <= verdubbelJaar || afkoopjaar <= verdubbelJaar){
    totaalVar <- afkopen(vast1, percentJaar, afkoopjaar)
    return(vast1)
  }
  
  vast2 <- simuleerLeningVast(vast1[maand == verdubbelJaar*12+1]$open,
                              percentJaar*2, 
                              jaar-verdubbelJaar)
  
  totaalVar <- rbind(vast1[maand <= verdubbelJaar*12], vast2)
  totaalVar$maand <- 1:(dim(totaalVar)[1])
  
  totaalVar <- afkopen(totaalVar, percentJaar*2, afkoopjaar)
  
  return(totaalVar)
}

# Aflossingstabel lening vast ----
simuleerAflossingTabelVast <- function(leningTotaalEuro,
                              percentMaand, 
                              jaar,
                              aflossingMaand,
                              type = "vast"){
  aflossing <- data.table(maand = 0, 
                          aflossing = aflossingMaand,
                          open = leningTotaalEuro, 
                          interest = leningTotaalEuro*percentMaand)
  for(mnd in 1:(jaar*12)){
    vorigeMaand <- aflossing[maand == mnd - 1]
    open <- vorigeMaand$open + vorigeMaand$interest - vorigeMaand$aflossing
    if(!type == "vast"){
      if(mnd == 3*12){
        percentMaand <- ((1+percentMaand)^12 + 0.01)^(1/12)-1
      } else
      if(mnd == 6*12)
        percentMaand <- (1+((1+percentMaand)^12-1)*2)^(1/12)-1
    }
    aflossing <- rbind(aflossing, data.table(
      maand = mnd, 
      aflossing = aflossingMaand,
      open = open,
      interest = open*percentMaand))
  }
  
  # Laatste maand geen aflossing meer
  aflossing[maand == jaar*12, aflossing := 0]
  
  return(aflossing)
}

# Composiete leningen ----
composieteLening <- function(leningTotaalEuro,
                             percentJaar, 
                             jaar, 
                             type = "vast",
                             afkoopjaar = 0){
  leningen <- paste0("lening", 1:length(leningTotaalEuro))
  for(lening in 1:length(leningTotaalEuro)){
    assign(leningen[lening],
           value = simuleerLening(leningTotaalEuro[lening],
                                  percentJaar[lening], 
                                  jaar[lening],
                                  type[lening], 
                                  afkoopjaar[lening])$aflostabel)
  }
  
  leningenTotaal <- data.table()
  for(lening in 1:length(leningen)){
    leningenTotaal <- rbind(leningenTotaal,
                            get(leningen[lening]))
  }
  
  leningenTotaal <- leningenTotaal[, lapply(.SD, sum), by = maand]
  
  return(list(aflostabel = leningenTotaal,
              kenmerken = list(leningTotaalEuro = leningTotaalEuro,
                                 percentJaar = percentJaar,
                                 jaar = jaar,
                                 type = type,
                                 afkoopjaar = afkoopjaar)))
}

# Lening afkopen ----
afkopen <- function(lening, percentJaar, afkoopjaar){
  if(afkoopjaar == 0 || afkoopjaar >= dim(lening)[1]/12)
    return(lening)
  
  administratieveBoetePerc <- ((1+percentJaar)^(1/12) - 1)*3
  open <- lening[maand == afkoopjaar*12-1]$open
  
  lening[maand == afkoopjaar*12, c("open", "interest", "aflossing") := list(0,0,open + open*administratieveBoetePerc)]
  lening[maand > afkoopjaar*12, c("open", "interest", "aflossing") := list(0,0,0)]
  
  return(lening)
}
# Vergelijk Leningen ----
vergelijkLeningen <- function(leningen){
  for(lening in leningen){
    leningComp <- length(lening$kenmerken$leningTotaalEuro)
    if(leningComp == 1){
      descr <- paste0(lening$kenmerken$leningTotaalEuro, " euro ", 
                      lening$kenmerken$type, " op ",
                      lening$kenmerken$jaar, " jaar met rentevoet ",
                      lening$kenmerken$percentJaar)
      if(lening$kenmerken$afkoopjaar > 0)
        descr <- paste0(descr, " afgekocht na ", lening$kenmerken$afkoopjaar, " jaar")
      print(descr)
    } else {
      print(paste0("Lening bestaande uit ", leningComp, " delen"))
      for(i in 1:leningComp){
        descr <- paste0(lening$kenmerken$leningTotaalEuro[i], " euro ",
                        lening$kenmerken$type[i]," op ",
                        lening$kenmerken$jaar[i], " jaar met rentevoet ",
                        lening$kenmerken$percentJaar[i])
        if(lening$kenmerken$afkoopjaar[i] > 0)
          descr <- paste0(descr, " afgekocht na ", lening$kenmerken$afkoopjaar[i], " jaar")
        print(descr)
      }
    }
      
    print("    Aflossen:")
    for(aflossingen in unique(lening$aflostabel$aflossing)){
      if(aflossingen > 0)
        print(paste0("      ", sum(aflossingen == lening$aflostabel$aflossing), " maanden: ", aflossingen, " euro"))
    }
    print(paste0("    Lening totale kost: ", sum(lening$aflostabel$aflossing) - 150000))
    
    maandInflatie <- (1+0.02)^(1/12) - 1
    inflatieProduct <- (1/(1+maandInflatie)^lening$aflostabel$maand)
    
    print(paste0("    Lening totale kost met inflatie 2%/jaar: ", sum(lening$aflostabel$aflossing*inflatieProduct) - 150000))
    print("")
  }
}

# Investeringen ----
investeringen <- function(aflossingsTabel, 
                          vermogenVoorAankoop,
                          totaalKosten,
                          investeringsPercentage,
                          gemiddeldeOpbrengstJaar = 0.02,
                          maandelijksSparen){
  
  maandOpbrengst <- (1+gemiddeldeOpbrengstJaar)^(1/12) - 1
  maandInflatie <- (1+0.02)^(1/12) - 1
  
  startKapitaal <- vermogenVoorAankoop + aflossingsTabel$open[1] - totaalKosten
  
  totaalVermogen <- data.table(maand = 0,
                               vermogen = startKapitaal,
                               maandelijksSparen = maandelijksSparen,
                               beleggingenWinst = startKapitaal*investeringsPercentage*maandOpbrengst)
  for(mnd in 1:max(aflossingsTabel$maand)){
    nieuwVermogen <- totaalVermogen[maand == mnd-1]$vermogen + 
      totaalVermogen[maand == mnd-1]$maandelijksSparen + 
      totaalVermogen[maand == mnd-1]$beleggingenWinst -
      aflossingsTabel[maand == mnd-1]$aflossing
    totaalVermogen <- rbind(totaalVermogen, data.table(
      maand = mnd,
      vermogen = nieuwVermogen,
      maandelijksSparen = totaalVermogen[maand == mnd-1]$maandelijksSparen*(1+maandInflatie),
      beleggingenWinst = nieuwVermogen*investeringsPercentage*maandOpbrengst
    ))
  }
  
  setkey(totaalVermogen, maand)
  setkey(aflossingsTabel, maand)
  
  totaalVermogen <- aflossingsTabel[totaalVermogen]
  
  totaalVermogen$jaar <- ceiling(totaalVermogen$maand/12)
  
  return(totaalVermogen)
}
