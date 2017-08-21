
namesMapping <- data.table(columnName = character(0), name = character(0))

for(map in  list(c("Bank","Lening naam"),
                 c("Te_Lenen_Bedrag","Te lenen bedrag (euro)"),
                 c("Vast_Of_Variabel","vast of variabel"),
                 c("Variabel_Herziening","Variabel herziening (jaar)"),
                 c("Rentevoet","Rentevoet (%)"),
                 c("Jaar","Lening duur (jaar)"),
                 c("Kosten_Bijhouden","Kosten bijhouden"),
                 c("Kosten_Eenmalig","Kosten eenmalig (euro)"),
                 c("Kosten_Maandelijks","Kosten maandelijks (euro)"),
                 c("Kosten_Jaarlijks","Kosten jaarlijks (euro)"),
                 c("Inflatie_Inrekenen","Inflatie inrekenen"),
                 c("Inflatie_Percentage","inflatie (%)"),
                 c("Vermogen_Bijhouden","Vermogen bijhouden"),
                 c("Vermogen_Start","Vermogen start (euro)"),
                 c("Vermogen_Maandelijsk_Sparen","Maandelijks inkomsten min de vaste kosten (euro)"),
                 c("Vermogen_Beleggingspercentage","beleggingspercentage (%)"),
                 c("Vermogen_Opbrengst","Beleggingen opbrengst (%)"),
                 c("Totaal_Afbetalingen","Totaal betaald (euro)"),
                 c("Totaal_Interesten","Totaal afbetaalde interesten (euro)"),
                 c("Totaal_Extra_Kosten","Totaal betaalde extra kosten (euro)"),
                 c("Vermogen_EindeLening","Vermogen bij einde lening (euro)"),
                 c("Vermogen_EindeLening_Inflatie","Vermogen bij einde lening, inflatie (euro)"),
                 c("Vermogen_Belegging_Opbrengsten","Totaal beleggingsopbrengsten (euro)"),
                 c("Vermogen_Belegging_Opbrengsten_Inflatie","Totaal beleggingsopbrengsten, inflatie (euro)"),
                 c("maand","Maand"),
                 c("aflossing","Aflossing (euro)"),
                 c("lening_open","Lening openstaand (euro)"),
                 c("aflossing_kapitaal","Aflossing: Kapitaal (euro)"),
                 c("aflossing_interest","Aflossing: Interest (euro)"),
                 c("extraKosten","Extra kosten (euro)"),
                 c("extraKosten_inflatie","Extra kosten, inflatie (euro)"),
                 c("vermogenVerschil","Vermogen verschil vorige maand (euro)"),
                 c("vermogen","Vermogen (euro)"),
                 c("beleggen_interest","Beleggingsopbrengsten (euro)")))
namesMapping <- rbind(namesMapping, data.table(columnName = map[1], name = map[2]))

mapNames <- function(names){
  if(length(names) > 1){
    result <- c()
      for(name in names)
        result <- c(result, mapNames(name))
      return(result)
  }
  
  if(length(which(namesMapping$columnName == names)) ==1)
    return(namesMapping[which(namesMapping$columnName == names)]$name)
  
  return(paste0(names, "?"))
}

unMapNames <- function(names){
  if(length(names) > 1){
    result <- c()
    for(name in names)
      result <- c(result, unMapNames(name))
    return(result)
  }
  
  if(length(which(namesMapping$name == names)) ==1)
    return(namesMapping[which(namesMapping$name == names)]$columnName)
  
  return(paste0(names, "?"))
}