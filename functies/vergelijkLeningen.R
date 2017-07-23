vergelijkLeningenShiny <- function(opgeslagenLeningen, opgeslagenAflosTabellen){
  
  # Splits input/output
  if(metrieken[1] %in% colnames(opgeslagenLeningen)){
    resultaten <- opgeslagenLeningen[, metrieken, with = FALSE]
    opgeslagenLeningen <- opgeslagenLeningen[, -metrieken, with = FALSE]
  } else {
    resultaten <- data.table("Totaal_Afbetalingen" = rep(NA, dim(opgeslagenLeningen)[1]))
    for(m in metrieken){
      resultaten[[m]] <- rep(NA, dim(opgeslagenLeningen)[1])
    }
  }
  
  
  # Berekenen resultaten
  for(geselecteerdeRij in 1:dim(opgeslagenLeningen)[1]){
    bank <- opgeslagenLeningen[geselecteerdeRij]$Bank
    if(is.na(resultaten[geselecteerdeRij]$Totaal_Afbetalingen) ||
       !bank %in% names(opgeslagenAflosTabellen)){
      opties <- as.list(opgeslagenLeningen[geselecteerdeRij])
      leningResulaat <- simuleerLeningShiny(
        leningTotaalEuro = as.numeric(
          strsplit(
            opgeslagenLeningen$Te_Lenen_Bedrag[geselecteerdeRij],split = "/"
          )[[1]]
        ),
        percentJaar = as.numeric(
          strsplit(
            opgeslagenLeningen$Rentevoet[geselecteerdeRij],split = "/"
          )[[1]]
        )/100, 
        jaar = as.numeric(
          strsplit(
            opgeslagenLeningen$Jaar[geselecteerdeRij],split = "/"
          )[[1]]
        ), 
        type = strsplit(
          opgeslagenLeningen$Vast_Of_Variabel[geselecteerdeRij],split = "/"
        )[[1]],
        variabelType = suppressWarnings(
          as.numeric(
            strsplit(
              opgeslagenLeningen$Variabel_Herziening[geselecteerdeRij],split = "/"
            )[[1]]
          )),
        opties = opties)
      
      # Opslaan metrieken
      for(m in metrieken){
        resultaten[[m]][geselecteerdeRij] <- leningResulaat$berekeningen[[m]]
      }
      
      # Opslaan aflostabel
      opgeslagenAflosTabellen[[bank]] <- leningResulaat$aflostabel
    }
  }
  
  return(list(opgeslagenLeningen = cbind(opgeslagenLeningen, resultaten),
              opgeslagenAflosTabellen = opgeslagenAflosTabellen,
              beschrijving = beschrijfResultaten(resultaten, leningNamen = opgeslagenLeningen$Bank)))
}

beschrijfResultaten <- function(resultaten, leningNamen){
  beschrijving <- "<h2>Resultaten</h2>"
  for(metriek in metrieken){
    metriekMax <- which(resultaten[[metriek]] == max(resultaten[[metriek]], na.rm = TRUE))
    metriekMin <- which(resultaten[[metriek]] == min(resultaten[[metriek]], na.rm = TRUE))
    
    if(length(metriekMax) + length(metriekMin) > 1)
      beschrijving <- c(beschrijving, paste0(
        "Op vlak van ", nameToText(mapNames(metriek)), ": \n",
        ifelse(sum(is.na(resultaten[[metriek]])) > 0,
               paste0(
                 "<li>Opgepast: Lening", ifelse(sum(is.na(resultaten[[metriek]])) > 1, "en ", " "), 
                 paste(leningNamen[which(is.na(resultaten[[metriek]]))], collapse = " en "),
                 " werd", ifelse(sum(is.na(resultaten[[metriek]])) > 1, "en ", " "),
                 "hierbij niet berekend</li>\n"
               ),
               ""),
        
        "<li>Meeste: Lening", 
        ifelse(length(metriekMax) > 1, "en ", " "), 
        paste(leningNamen[metriekMax], collapse = " en "), 
        " met ", numberToEuro(resultaten[[metriek]][metriekMax[1]]), " euro</li>\n",
        
        "<li>Minste: Lening", 
        ifelse(length(metriekMin) > 1, "en ", " "), 
        paste(leningNamen[metriekMin], collapse = " en "), 
        " met ", numberToEuro(resultaten[[metriek]][metriekMin[1]]), " euro</li>\n",
        
        "</ul><br>"
      ))
  }
  
  return(beschrijving)
  
}

nameToText <- function(name){
  name <- gsub("_", " ", name)
  name <- tolower(name)
  name <- gsub("inflatie", "rekening houdend met inflatie", name)
  name <- gsub(" \\).+", "", name)
  
  return(name)
}
