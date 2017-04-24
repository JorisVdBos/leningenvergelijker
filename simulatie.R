rm(list = ls())
# Libraries
library(data.table)
library(ggplot2)
options(digits=12)
source("functies.R")

# Settings ----
vermogenVoorAankoop <- 135000

# Aankoop
aankoopKostEuro <- 219500
bijkomendeKosten <- 17850
totaalKosten <- aankoopKostEuro + bijkomendeKosten

# Lening
leningTotaalEuro <- 150000  
percentJaar <- 0.018
jaar <- 15

# Investeren
investeringsPercentage <- 0.5  # Percentage op totaal vermogen
maandInkomsten <- 2250 + 1750
maandelijksSparen <- maandInkomsten - 900

# Simations ----
# Lening Vaste
aflossingsTabelVastKbc <- simuleerLening(leningTotaalEuro = 150000, 
                                   percentJaar = 0.017, 
                                   jaar = 15, type = "vast")
# 
# aflossingsTabelVastArgenta <- simuleerLening(leningTotaalEuro = 150000, 
#                                              percentJaar = 0.018, 
#                                              jaar = 15, type = "vast")
# 
# aflossingsTabelVastIng <- simuleerLening(leningTotaalEuro = 150000, 
#                                              percentJaar = 0.0165, 
#                                              jaar = 15, type = "vast")


# Argenta 2 leningen
# aflossingsTabel2VV <- composieteLening(leningTotaalEuro = c(125000, 25000), 
#                                      percentJaar = c(0.018, 0.0145), 
#                                      jaar = c(15,5),
#                                      type = c("vast", "vast"), afkoopjaar = c(0,0))
# 
# aflossingsTabel2 <- composieteLening(leningTotaalEuro = c(125000, 25000), 
#                                      percentJaar = c(0.018, 0.0145), 
#                                      jaar = c(15,6),
#                                      type = c("vast", "variabel3"), afkoopjaar = c(0,0))
# 
# # Lening 3
# aflossingsTabel3 <- composieteLening(leningTotaalEuro = c(125000, 25000), 
#                                        percentJaar = c(0.018, 0.0145), 
#                                        jaar = c(15,9),
#                                        type = c("vast", "variabel3"), afkoopjaar = c(0,0))
# 
# 
# # Lening 4
# aflossingsTabel4 <- composieteLening(leningTotaalEuro = c(125000, 25000), 
#                                        percentJaar = c(0.018, 0.0145), 
#                                        jaar = c(15,9),
#                                        type = c("vast", "variabel3"), afkoopjaar = c(0,6))

# Lening 5
aflossingsTabel5Kbc <- composieteLening(leningTotaalEuro = c(125000, 25000),
                                        percentJaar = c(0.017, 0.0091),
                                        jaar = c(15,9),
                                        type = c("vast", "variabel3"), 
                                        afkoopjaar = c(0,0))

aflossingsTabel5Kbc2 <- composieteLening(leningTotaalEuro = c(125000, 25000),
                                        percentJaar = c(0.017, 0.0091),
                                        jaar = c(15,9),
                                        type = c("vast", "variabel3"), afkoopjaar = c(0,6))

vergelijkLeningen(list(
  # aflossingsTabelVastArgenta,
  aflossingsTabelVastKbc,
  aflossingsTabel5Kbc, 
  aflossingsTabel5Kbc2
  # aflossingsTabelVastIng
  #aflossingsTabel2, 
  #aflossingsTabel2VV
  #aflossingsTabel3, 
  #aflossingsTabel4
  ))

# met Investeringen:
vermogenTabel1 <- investeringen(aflossingsTabelVastKbc$aflostabel,
                                vermogenVoorAankoop,
                                totaalKosten,
                                investeringsPercentage,
                                gemiddeldeOpbrengstJaar = 0.03,
                                maandelijksSparen)
ggplot(data = vermogenTabel1, aes(maand/12, vermogen)) + 
  geom_point() + 
  labs(x = 'Jaar', y = "Totaal vermogen") + 
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})

# Studie verdelingen 2 

# Setup
variabelVastVerdeling <- function(vastPerc = 0.017, 
                                  investeringsPercentage = 0.5,
                                  gemiddeldeOpbrengstJaar = 0.03){
  leningen <- 10
  vastVariabel <- data.table()
  for(var_lening_jaar in c(9))
    for(afkoop in c(3,6,9))
      for(i in 0:leningen){
        deelVariabel <- i*15000
        deelVast <- 150000-deelVariabel
        vastVariabel <- rbind(vastVariabel, data.table(
          deel_vast = deelVast,
          deel_variabel = deelVariabel,
          var_lening_type = "variabel3",
          var_perc = 0.0091,
          var_lening_jaar = var_lening_jaar,
          afkopen_na_x_jaar = afkoop
        ))
      }
  
  # for(var_lening_jaar in c(15))
  #   for(afkoop in c(0,10))
  #     for(i in 0:leningen){
  #       deelVariabel <- i*15000
  #       deelVast <- 150000-deelVariabel
  #       vastVariabel <- rbind(vastVariabel, data.table(
  #         deel_vast = deelVast,
  #         deel_variabel = deelVariabel,
  #         var_lening_type = "variabel10",
  #         var_perc = 0.0155,
  #         var_lening_jaar = var_lening_jaar,
  #         afkopen_na_x_jaar = afkoop
  #       ))
  #     }
  # 
  # for(var_lening_jaar in c(8,11))
  #   for(afkoop in c(0,8,11,13))
  #     for(i in 0:leningen){
  #       deelVariabel <- i*15000
  #       deelVast <- 150000-deelVariabel
  #       vastVariabel <- rbind(vastVariabel, data.table(
  #         deel_vast = deelVast,
  #         deel_variabel = deelVariabel,
  #         var_lening_type = "variabel5",
  #         var_perc = 0.0135,
  #         var_lening_jaar = var_lening_jaar,
  #         afkopen_na_x_jaar = afkoop
  #       ))
  #     }
  
  # Filter:
  vastVariabel <- vastVariabel[deel_vast/(deel_vast + deel_variabel) > 0.6 |
                                 (deel_vast/(deel_vast + deel_variabel) <= 0.6 & afkopen_na_x_jaar == 0)]
  
  results <- data.table()
  remove <- c()
  pb <- txtProgressBar(style = 3)
  for(i in 1:dim(vastVariabel)[1]){
    setTxtProgressBar(pb, value = i/dim(vastVariabel)[1])
    
    lening <- composieteLening(leningTotaalEuro = c(vastVariabel$deel_vast[i], vastVariabel$deel_variabel[i]), 
                               percentJaar = c(0.017, vastVariabel$var_perc[i]), 
                               jaar = c(15,vastVariabel$var_lening_jaar[i]),
                               type = c("vast", vastVariabel$var_lening_type[i]), 
                               afkoopjaar = c(0,vastVariabel$afkopen_na_x_jaar[i]))
    
    totaalAfbetalingen <- sum(lening$aflostabel$aflossing)-150000
    
    maandInflatie <- (1+0.02)^(1/12) - 1
    inflatieProduct <- (1/(1+maandInflatie)^lening$aflostabel$maand)
    
    leningKostInflatie <- sum(lening$aflostabel$aflossing*inflatieProduct) - 150000
    
    # Investeren
    invest <- investeringen(lening$aflostabel,
                            vermogenVoorAankoop,
                            totaalKosten,
                            investeringsPercentage = investeringsPercentage,
                            gemiddeldeOpbrengstJaar = gemiddeldeOpbrengstJaar,
                            maandelijksSparen)
    
    aflossing_maand1 <- lening$aflostabel$aflossing[1]
    
    vermogenNaInvesterenJaar5 <- invest$vermogen[5*12]
    vermogenNaInvesterenJaar10 <- invest$vermogen[10*12]
    vermogenNaInvesterenJaar15 <- invest$vermogen[15*12]
    
    if(aflossing_maand1 > 1200){
      remove <- c(remove, i)
    } else {
      results <- rbind(results, data.table(
        aflossing_maand1 = aflossing_maand1,
        totaal_afbetalingen = totaalAfbetalingen,
        lening_kost_inflatie = leningKostInflatie,
        vermogen_na_investeren_jaar5 = vermogenNaInvesterenJaar5,
        vermogen_na_investeren_jaar10 = vermogenNaInvesterenJaar10,
        vermogen_na_investeren_jaar15 = vermogenNaInvesterenJaar15
      ))
    }
    
  }
  close(pb)
  if(length(remove) > 0)
    vastVariabel <- vastVariabel[-remove]
  vastVariabel <- cbind(vastVariabel, results)
  
  vastVariabel <- vastVariabel[order(lening_kost_inflatie)]
  View(vastVariabel[order(lening_kost_inflatie)])
  
  # vastVariabel$varLeningJaar_varLeningType_afkopenJaar <- paste(vastVariabel$var_lening_jaar,
  #                                  vastVariabel$var_lening_type, 
  #                                  vastVariabel$afkopen_na_x_jaar, sep = "_")
  
  bank <- "kbc"
  g <- ggplot(vastVariabel, aes(deel_vast/(deel_vast+deel_variabel), lening_kost_inflatie)) + 
    geom_point(aes(col = factor(var_lening_jaar), shape = factor(var_lening_type), size = afkopen_na_x_jaar))
  ggsave(g, filename = paste0(bank, "_totkost.png"))
  
  g2 <- ggplot(vastVariabel, aes(deel_vast/(deel_vast+deel_variabel), vermogen_na_investeren_jaar15)) + 
    geom_point(aes(col = factor(var_lening_jaar), shape = factor(var_lening_type), size = afkopen_na_x_jaar))
  ggsave(g2, filename = paste0(bank, "_totVerm.png"))
  
  return(list(plot1 = g,
              plot2 = g2,
              tabel = vastVariabel))
}

vastVariabel1 <- variabelVastVerdeling(vastPerc = 0.017, 
                                       investeringsPercentage = 0.5,
                                       gemiddeldeOpbrengstJaar = 0.03)
vastVariabel2 <- variabelVastVerdeling(vastPerc = 0.017, 
                                       investeringsPercentage = 0.8,
                                       gemiddeldeOpbrengstJaar = 0.05)
# vastVariabel3 <- variabelVastVerdeling(vastPerc = 0.017, gemiddeldeOpbrengstJaar = 0.1)
# vastVariabel4 <- variabelVastVerdeling(vastPerc = 0.017, gemiddeldeOpbrengstJaar = 0.2)