# Libraries
library(data.table)
library(ggplot2)
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
investeringsPercentage <- 0.8  # Percentage op totaal vermogen
maandInkomsten <- 2250 + 1750
maandelijksSparen <- maandInkomsten - 900

# Simations ----
# Lening 1
aflossingsTabel1 <- simuleerLening(leningTotaalEuro = 150000, 
                                   percentJaar = 0.017, 
                                   jaar = 15, type = "vast")

aflossingsTabel1Arg <- simuleerLening(leningTotaalEuro = 150000, 
                                   percentJaar = 0.018, 
                                   jaar = 15, type = "vast")


# Argenta 2 leningen
aflossingsTabel2VV <- composieteLening(leningTotaalEuro = c(125000, 25000), 
                                     percentJaar = c(0.018, 0.0145), 
                                     jaar = c(15,5),
                                     type = c("vast", "vast"), afkoopjaar = c(0,0))

aflossingsTabel2 <- composieteLening(leningTotaalEuro = c(125000, 25000), 
                                     percentJaar = c(0.018, 0.0145), 
                                     jaar = c(15,6),
                                     type = c("vast", "variabel3"), afkoopjaar = c(0,0))

# Lening 3
aflossingsTabel3 <- composieteLening(leningTotaalEuro = c(125000, 25000), 
                                       percentJaar = c(0.018, 0.0145), 
                                       jaar = c(15,9),
                                       type = c("vast", "variabel3"), afkoopjaar = c(0,0))


# Lening 4
aflossingsTabel4 <- composieteLening(leningTotaalEuro = c(125000, 25000), 
                                       percentJaar = c(0.018, 0.0145), 
                                       jaar = c(15,9),
                                       type = c("vast", "variabel3"), afkoopjaar = c(0,6))

vergelijkLeningen(list(
  aflossingsTabel1, 
  aflossingsTabel1Arg,
  #aflossingsTabel2, 
  aflossingsTabel2VV
  #aflossingsTabel3, 
  #aflossingsTabel4
  ))

# met Investeringen:
vermogenTabel1 <- investeringen(aflossingsTabel1$aflostabel,
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
leningen <- paste0("Lening", 1:10)
for(i in 1:length(leningen)){
  
}

