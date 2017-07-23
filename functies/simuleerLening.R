
simuleerLeningShiny <- function(leningTotaalEuro,
                                percentJaar,
                                jaar,
                                type,
                                variabelType,
                                opties = list()){
  
  lening <- simuleerLening(leningTotaalEuro=leningTotaalEuro,
                           percentJaar=percentJaar, 
                           jaar=jaar, 
                           type=type,
                           variabelType=variabelType)
  
  # Bekekeningen
lening$aflostabel <- kostenVermogenToevoegen(lening$aflostabel, opties)
  
  totaalExtraKosten <- sum(lening$aflostabel$extraKosten)
  totaalKostenInfl <- sum(lening$aflostabel$extraKosten_inflatie)
  
  vermogenEindeLening <- lening$aflostabel$vermogen[max(jaar)*12]
  vermogenEindeLeningInf <- 
    vermogenEindeLening/(((1+opties$Inflatie_Percentage/100)^(1/12))^(max(jaar)*12))
  
  totaalOpbrengsten <- sum(lening$aflostabel$beleggen_interest)
  totaalOpbrengstenInfl <- sum(lening$aflostabel$beleggen_interest/
                                 (((1+opties$Inflatie_Percentage/100)^(1/12))^(lening$aflostabel$beleggen_interest)))
  
  # Berekeningen
  berekeningen <- data.table(
    Totaal_Afbetalingen = sum(lening$aflostabel$aflossing),
    Totaal_Interesten = round(sum(lening$aflostabel$aflossing_interest), 2),
    Totaal_Extra_Kosten = round(totaalExtraKosten, 2),
    Vermogen_EindeLening = vermogenEindeLening,
    Vermogen_EindeLening_Inflatie = round(vermogenEindeLeningInf, 2),
    Vermogen_Belegging_Opbrengsten = round(totaalOpbrengsten, 2),
    Vermogen_Belegging_Opbrengsten_Inflatie = round(totaalOpbrengstenInfl, 2)
  )
  
  return(list(aflostabel = lening$aflostabel,
              beschrijving = beschrijfLening(lening, opties, berekeningen),
              berekeningen = berekeningen,
              opties = opties))
}

numberToEuro <- function(int){
  str <- as.character(as.double(int))
  str <- gsub("\\.", ",", str)
  charsWithoutDot <- nchar(gsub(",.+", "", str))
  while(charsWithoutDot > 3){
    str <- paste0(substr(str, 1, charsWithoutDot - 3), 
                  ".", 
                  substr(str, charsWithoutDot - 3 + 1, nchar(str)))
    charsWithoutDot <- charsWithoutDot - 3
  }
  
  if(int %% 1 == 0){
    str <- paste0(str, ",00")
  } else {
    if(nchar(gsub(".+,", "", str)) == 1)
      str <- paste0(str, "0")
  }
  
  return(str)
}

beschrijfLening <- function(lening, opties, berekeningen){
  beschrijving <- "<h2>Lening simulatie</h2>"
  aantalLeningen <- 
    length(lening$kenmerken$leningTotaalEuro)
  for(i in 1:aantalLeningen){
    if(aantalLeningen > 1)
      beschrijving <- c(beschrijving, paste0(
        "<h3>Lening deel ",
        i,
        ":</h3>"))
    beschrijving <- c(beschrijving, paste0(
      "<li>Lening bedrag: ", 
      numberToEuro(lening$kenmerken$leningTotaalEuro[i]),
      " euro</li>"))
    if(lening$kenmerken$type[i] == "Vast"){
      beschrijving <- c(beschrijving, paste0(
        "<li>Lening type: ", 
        lening$kenmerken$type[i],
        "</li>"
      ))
    } else 
    if(lening$kenmerken$type[i] == "Variabel"){
      beschrijving <- c(beschrijving, paste0(
        "<li>Lening type: ", 
        lening$kenmerken$type[i],
        "<ul><li>Herberekening na ", 
        lening$kenmerken$variabelType[i],
        " jaar</ul></li>"
      ))
      
    }
    
    beschrijving <- c(beschrijving, paste0(      
      "<li>Lening rentevoet: ", 
      lening$kenmerken$percentJaar[i]*100,
      "%</li>",
      "<li>Lening jaar: ", 
      lening$kenmerken$jaar[i],
      " jaar</li>"
    ))
    
  }
  
  # Lening beoordelen
  beschrijving <- c(beschrijving,"<h2>Beoordeling</h2>")
  beschrijving <- c(beschrijving, paste0(      
    "<li>Totaal afbetalingen: ", 
    numberToEuro(berekeningen$Totaal_Afbetalingen),
    " euro</li>",
    "<li>Totaal afbetaald aan interesten: ", 
    numberToEuro(berekeningen$Totaal_Interesten),
    " euro</li>"
  ))
  if(opties$Kosten_Bijhouden){
    beschrijving <- c(beschrijving, paste0(      
      "<li>Totaal extra kosten: ", 
      numberToEuro(berekeningen$Totaal_Extra_Kosten),
      " euro</li>",  
      "<li>Totaal alle kosten: ", 
      numberToEuro(berekeningen$Totaal_Interesten + berekeningen$Totaal_Extra_Kosten),
      " euro</li>"
    ))
  }
  if(opties$Vermogen_Bijhouden){
    beschrijving <- c(beschrijving, paste0(      
      "<li>Vermogen na afbetaling lening: ", 
      numberToEuro(berekeningen$Vermogen_EindeLening),
      " euro</li>"
    ))
    if(opties$Inflatie_Inrekenen){
      beschrijving <- c(beschrijving, paste0(      
        "<li>Vermogen na afbetaling lening, waarde vandaag: ", 
        numberToEuro(berekeningen$Vermogen_EindeLening_Inflatie),
        " euro</li>"
      ))
    }
    if(opties$Vermogen_Beleggingspercentage > 0){
      beschrijving <- c(beschrijving, paste0(      
        "<li>Totaal interesten van beleggingen: ", 
        numberToEuro(berekeningen$Vermogen_Belegging_Opbrengsten),
        " euro</li>"
      ))
      if(opties$Inflatie_Inrekenen){
        beschrijving <- c(beschrijving, paste0(      
          "<li>Totaal interesten van beleggingen, waarde vandaag: ", 
          numberToEuro(berekeningen$Vermogen_Belegging_Opbrengsten_Inflatie),
          " euro</li>"
        ))
      }
    }
  }
  
  return(beschrijving)
}

kostenVermogenToevoegen <- function(aflostabel, opties){
  if(!opties$Kosten_Bijhouden){
    opties$Kosten_Eenmalig <- 0
    opties$Kosten_Maandelijks <- 0
    opties$Kosten_Jaarlijks <- 0
  }
  
  if(!opties$Inflatie_Inrekenen){
    opties$Inflatie_Percentage <- 0
  }
  
  if(!opties$Vermogen_Bijhouden){
    opties$Vermogen_Start <- 0
    opties$Vermogen_Maandelijsk_Sparen <- 0
    opties$Vermogen_Beleggingspercentage <- 0
    opties$Vermogen_Opbrengst <- 0
  }
  
  aflostabel$extraKosten = rep(opties$Kosten_Maandelijks, dim(aflostabel)[1])
  aflostabel[1,extraKosten := extraKosten + opties$Kosten_Eenmalig]
  aflostabel[(aflostabel$maand - 1) %% 12 == 0,
             extraKosten := extraKosten + opties$Kosten_Jaarlijks]
  
  inflatieMaand <- (1+opties$Inflatie_Percentage/100)^(1/12)
  aflostabel[,extraKosten_inflatie := round(extraKosten*inflatieMaand^maand, 2)]
  
  maxAflossing <- max(aflostabel$aflossing)
  belegOpbrengstMaand <- (1+opties$Vermogen_Opbrengst/100)^(1/12) -1
  vermogenVerschil <- 0
  vermogen <- opties$Vermogen_Start
  interest <- 0
  
  if(opties$Inflatie_Inrekenen){
    maandelijksSparen <- opties$Vermogen_Maandelijsk_Sparen*inflatieMaand^(0:(dim(aflostabel)[1]-1))
  } else {
    maandelijksSparen <- rep(opties$Vermogen_Maandelijsk_Sparen, dim(aflostabel)[1]) 
  }
  
  if(opties$Kosten_Bijhouden){
    if(opties$Inflatie_Inrekenen){
      extraKosten <- aflostabel$extraKosten_inflatie
    } else {
      extraKosten <- aflostabel$extraKosten
    }
  } else {
    extraKosten <- rep(0, dim(aflostabel)[1])
  }
  
  for(mnd in 1:dim(aflostabel)[1]){
    interest[mnd+1] <- vermogen[mnd]*opties$Vermogen_Beleggingspercentage/100*belegOpbrengstMaand
    # Nieuw vermogen volgende maand = vermogen vorige maand
    vermogenVerschil[mnd+1] <- 
      # Maandelijks sparen
      maandelijksSparen[mnd] + 
      # Interest van beleggingen
      ifelse(interest[mnd]>0, interest[mnd], 0) + 
      # Lagere aflossingen genereren meer vermogen
      (maxAflossing - aflostabel[maand == mnd]$aflossing) -
      # Extra kosten lening
      extraKosten[mnd]
    vermogen[mnd+1] <- vermogen[mnd] + vermogenVerschil[mnd+1]
  }
  aflostabel$vermogenVerschil <- round(vermogenVerschil[-1], 2)
  aflostabel$vermogen <- round(vermogen[-1], 2)
  aflostabel$beleggen_interest <- ifelse(round(interest[-1], 2)>0, round(interest[-1], 2), 0) 
  
  # if(opties$Inflatie_Inrekenen){
  #   aflostabel[, vermogen_inflatie := vermogen/inflatieMaand^maand]
  #   aflostabel[, beleggen_interest_inflatie := beleggen_interest/inflatieMaand^maand]
  # }
  
  return(aflostabel)
}
