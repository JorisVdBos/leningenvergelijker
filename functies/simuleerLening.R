
simuleerLening <- function(leningTotaalEuro,
                           percentJaar,
                           jaar,
                           type,
                           aflossingen = NULL){
  
  if(is.null(aflossingen)){
    # Zelf berekenen
    lening <- simuleerLening(leningTotaalEuro,
                             percentJaar, 
                             jaar,
                             type)
  } else {
    # aflostabel opstellen
  }
  
  return(list(beschrijving = beschrijving,
              aflosTabel = aflostTabel))
}
