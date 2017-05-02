# simuleerLening ----
simuleerLening <- function(leningTotaalEuro,
                           percentJaar, 
                           jaar,
                           type, 
                           variabelType,
                           afkoopjaar = 0){

  if(length(leningTotaalEuro) > 1){
    return(composieteLening(leningTotaalEuro,
                            percentJaar,
                            jaar,
                            type, 
                            variabelType,
                            afkoopjaar = afkoopjaar))
  }
  
  variabelen <- list(leningTotaalEuro = leningTotaalEuro,
                     percentJaar = percentJaar,
                     jaar = jaar,
                     type = type,
                     variabelType = variabelType,
                     afkoopjaar = afkoopjaar)
  
  if(type == "Vast")
    return(list(aflostabel = simuleerLeningVast(leningTotaalEuro,
                              percentJaar, 
                              jaar,
                              afkoopjaar),
                 kenmerken = variabelen))
  else if(type == "Variabel"){
    return(list(aflostabel = simuleerLeningVariabel(leningTotaalEuro,
                                                    percentJaar, 
                                                    jaar,
                                                    afkoopjaar,
                                                    verdubbelJaar = variabelType),
                kenmerken = variabelen))
  }
}


# Simuleer lening vast ----
simuleerLeningVast <- function(leningTotaalEuro,
                               percentJaar, 
                               jaar, 
                               afkoopjaar = 0){
  
  percentMaand <- (1+percentJaar)^(1/12) - 1
  
  # Maandelijkse aflossing zoeken
  min <- leningTotaalEuro/(jaar * 12)
  max <- leningTotaalEuro*(1+percentJaar)^jaar/(jaar*12) * 1.5
  
  
  # Aflossingen mid
  aflosrestMin <- simuleerAflossingTabelVast(leningTotaalEuro,
                                             percentMaand, 
                                             jaar,
                                             min)[jaar*12]$lening_open
  
  aflosrestMax <- simuleerAflossingTabelVast(leningTotaalEuro,
                                             percentMaand, 
                                             jaar,
                                             max)[jaar*12]$lening_open
  
  slope <- (aflosrestMax - aflosrestMin) / (max - min)
  solution <- -(aflosrestMin/slope - min)
  
  # Afronden op 1 cent naar boven en aflossingstabel opnieuw berekenen
  solution <- round(solution,2)
  aflostabelMid <- simuleerAflossingTabelVast(leningTotaalEuro,
                                              percentMaand, 
                                              jaar,
                                              solution, opkuis = TRUE)
  
  # Eventuele afkopen
  aflostabelMid <- afkopen(aflostabelMid, percentJaar, afkoopjaar)
  
  return(aflostabelMid)
}

# Simuleer lening Variabel ----
simuleerLeningVariabel <- function(leningTotaalEuro,
                                   percentJaar, 
                                   jaar, 
                                   afkoopjaar = 0, 
                                   verdubbelJaar = 3){
  
  if(is.na(afkoopjaar))
    afkoopjaar = 0
  if(is.na(verdubbelJaar))
    verdubbelJaar = 3
  
  vast1 <- simuleerLeningVast(leningTotaalEuro,
                              percentJaar, 
                              jaar)
  # Na "verdubbelJaar" jaar herziening
  if(jaar <= verdubbelJaar || (afkoopjaar > 0 && afkoopjaar <= verdubbelJaar)){
    vast1 <- afkopen(vast1, percentJaar, afkoopjaar)
    return(vast1)
  }
  
  vast2 <- simuleerLeningVast(vast1[maand == verdubbelJaar*12-1]$lening_open,
                              percentJaar*2, 
                              jaar-verdubbelJaar)
  
  totaalVar <- rbind(vast1[maand < verdubbelJaar*12-1], vast2)
  totaalVar$maand <- 1:(dim(totaalVar)[1])
  
  totaalVar <- afkopen(totaalVar, percentJaar*2, afkoopjaar)
  
  return(totaalVar)
}

# Aflossingstabel lening vast ----
simuleerAflossingTabelVast <- function(leningTotaalEuro,
                              percentMaand, 
                              jaar,
                              aflossingMaand,
                              type = "vast",
                              opkuis = FALSE){
  aflossing <- data.table(maand = 1, 
                          aflossing = aflossingMaand,
                          lening_open = leningTotaalEuro-aflossingMaand+leningTotaalEuro*percentMaand, 
                          lening_kapitaal = aflossingMaand-leningTotaalEuro*percentMaand,
                          lening_interest = leningTotaalEuro*percentMaand)
  for(mnd in 2:(jaar*12)){
    vorigeMaand <- aflossing[maand == mnd - 1]
    if(!type == "vast"){
      if(mnd == 3*12){
        percentMaand <- ((1+percentMaand)^12 + 0.01)^(1/12)-1
      } else
      if(mnd == 6*12)
        percentMaand <- (1+((1+percentMaand)^12-1)*2)^(1/12)-1
    }
    lening_open <- vorigeMaand$lening_open + vorigeMaand$lening_open*percentMaand - aflossingMaand
    aflossing <- rbind(aflossing, data.table(
      maand = mnd, 
      aflossing = aflossingMaand,
      lening_open = lening_open,
      lening_kapitaal = aflossingMaand-vorigeMaand$lening_open*percentMaand,
      lening_interest = vorigeMaand$lening_open*percentMaand))
  }
  
  
  if(opkuis){
    # Laatste maand aflossing zal de totale lening aflossen
    aflossing[maand == jaar*12, aflossing := aflossing+lening_open]
    aflossing[maand == jaar*12, lening_open := 0]
    aflossing[maand == jaar*12, lening_kapitaal := aflossing-lening_interest]
    for(col in colnames(aflossing)[-1]){
      aflossing[[col]] <- round(aflossing[[col]]*100)/100
    }
  }
  
  return(aflossing)
}

# Composiete leningen ----
composieteLening <- function(leningTotaalEuro,
                             percentJaar, 
                             jaar, 
                             type,
                             variabelType,
                             afkoopjaar = 0){
  
  if(length(afkoopjaar) < length(leningTotaalEuro) && afkoopjaar == 0){
    afkoopjaar <- rep(0, length(leningTotaalEuro))
  }
  
  leningen <- paste0("lening", 1:length(leningTotaalEuro))
  for(lening in 1:length(leningTotaalEuro)){
    assign(leningen[lening],
           value = simuleerLening(leningTotaalEuro = leningTotaalEuro[lening],
                                  percentJaar = percentJaar[lening], 
                                  jaar = jaar[lening],
                                  type = type[lening], 
                                  variabelType = variabelType[lening],
                                  afkoopjaar = afkoopjaar[lening])$aflostabel)
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
                               variabelType = variabelType,
                               afkoopjaar = afkoopjaar)))
}

# Lening afkopen ----
afkopen <- function(lening, percentJaar, afkoopjaar){
  if(afkoopjaar == 0 || afkoopjaar >= dim(lening)[1]/12)
    return(lening)
  
  administratieveBoetePerc <- ((1+percentJaar)^(1/12) - 1)*3
  afkoopsom <- lening[maand == afkoopjaar*12]$lening_open
  
  lening[maand == afkoopjaar*12, c("lening_open", "lening_interest", "aflossing") := list(0,0,afkoopsom + afkoopsom*administratieveBoetePerc)]
  lening[maand > afkoopjaar*12, c("lening_open", "lening_interest", "aflossing") := list(0,0,0)]
  
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
        print(paste0("      ", 
                     sum(aflossingen == lening$aflostabel$aflossing), 
                     " maanden: ", 
                     round(aflossingen,2), " euro"))
    }
    print(paste0("    Lening som afbetalingen: ", 
                 round(sum(lening$aflostabel$aflossing) - 150000),2))
    
    maandInflatie <- (1+0.02)^(1/12) - 1
    inflatieProduct <- (1/(1+maandInflatie)^lening$aflostabel$maand)
    
    print(paste0("    Lening som afbetalingen met correctie voor inflatie 2%/jaar: ", 
                 round(sum(lening$aflostabel$aflossing*inflatieProduct) - 150000,2), 
                 " euro"))
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
  
  startKapitaal <- vermogenVoorAankoop + aflossingsTabel$lening_open[1] - totaalKosten
  
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
