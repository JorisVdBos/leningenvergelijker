shinyServer(function(input, output, session) {
  # Lening simuleren ----
  # Lening input
  observeEvent(input$kostenCheck, {
    toggle(id = "lenKostendiv", condition = input$kostenCheck)
  })
  
  observeEvent(input$inflCheck, {
    toggle(id = "lenInfldiv", condition = input$inflCheck)
  })
  
  observeEvent(input$vermogenCheck, {
    toggle(id = "lenVermdiv", condition = input$vermogenCheck)
  })
  
  leningSamenstelling <- data.table(Te_Lenen_Bedrag = numeric(),
                                    Vast_Of_Variabel = character(),
                                    Rentevoet = numeric(),
                                    Jaar = numeric())
  observeEvent(input$lenVoegToe, {
    # Invoer checken:
    checks <- TRUE
    
    inputs <- c("lenBedr", "lenRV", "lenJaar")
    errorDivs <- c("lenBedrError", "lenRVError", "lenJaarError")
    
    for(inp in inputs){
      dum <- 
        tryCatch({
          as.numeric(
            gsub(",", "\\.", input[[inp]])
          )
        },
        warning = function(x){
          NA
        })
      
      if(is.na(dum)){
        toggle(id = errorDivs[which(inputs == inp)], 
               condition = TRUE)
        checks <- FALSE
      } else {
        toggle(id = errorDivs[which(inputs == inp)], 
               condition = FALSE)
        assign(x = inp, 
               value = dum)
      }
    }
    
    
    if(!checks)
      return(NULL)
    
    # Invoer ok:
    leningSamenstelling <<- 
      rbind(leningSamenstelling, 
            data.table(
              "Te_Lenen_Bedrag" = floor(lenBedr),
              "Vast_Of_Variabel" = input$lenVastOfVar,
              "Rentevoet" = lenRV,
              "Jaar" = floor(lenJaar)
            ))
    toonLeningSamenstelling()
  })
  
  toonLeningSamenstelling <- function(){
    output$lenInputDT <- renderDataTable(leningSamenstelling,
                                         options=list(autoWidth = TRUE,
                                                      scrollX=TRUE, 
                                                      paging = FALSE,
                                                      searching = FALSE,
                                                      pageLength = -1))
  }
  toonLeningSamenstelling()
  
  observeEvent(input$lenLaatsteWeg, {
    if(is.null(leningSamenstelling) || dim(leningSamenstelling)[1] == 0)
      return(NULL)
    
    leningSamenstelling <<- 
      leningSamenstelling[-dim(leningSamenstelling)[1]]
    toonLeningSamenstelling()
  })
  
  observeEvent(input$lenAllesWeg,{
    leningSamenstelling <<- leningSamenstelling[-(1:dim(leningSamenstelling)[1])]
    toonLeningSamenstelling()
  })
  
  # Leningen opslaan
  opgeslagenLeningen <- NULL
  if(file.exists("opgeslagenLeningenVoorbeelden.RData"))
    load(file = "opgeslagenLeningenVoorbeelden.RData")
  
  observeEvent(input$lenOpslaan, {
    # Invoer checken:
    checks <- TRUE
    
    if(is.null(leningSamenstelling) || dim(leningSamenstelling)[1] == 0){
      toggle(id = "lenSamError", 
             condition = TRUE)
      checks <- FALSE
    } else {
      toggle(id = "lenSamError", 
             condition = FALSE)
    }
    if(is.null(input$lenBank) || input$lenBank == ""){
      toggle(id = "lenBankError", 
             condition = TRUE)
      checks <- FALSE
    } else {
      toggle(id = "lenBankError", 
             condition = FALSE)
    }
    
    inputs <- c()
    errorDivs <- c()
    if(input$kostenCheck){
      inputs <- c(inputs, "lenKost1", "lenKostM", "lenKostJ")
      errorDivs <- c(errorDivs, "lenKost1Error", "lenKostMError", "lenKostJError")
    } else {
      lenKost1 <- NA
      lenKostM <- NA
      lenKostJ <- NA
    }
    if(input$inflCheck){
      inputs <- c(inputs, "lenInfl")
      errorDivs <- c(errorDivs, "lenInflError")
    } else {
      lenInfl <- NA
    }
    if(input$vermogenCheck){
      inputs <- c(inputs, "lenVermStart", "lenVermInk", "lenVermUit", 
                  "lenVermBelPerc", "lenVermBelOpbrPerc")
      errorDivs <- c(errorDivs, "lenVermStartError", "lenVermInkError", 
                     "lenVermUitError", "lenVermBelPercError", "lenVermBelOpbrPercError")
    } else {
      lenVermStart <- NA
      lenVermInk <- NA
      lenVermUit <- NA
      lenVermBelPerc <- NA
      lenVermBelOpbrPerc <- NA
    }
    
    for(inp in inputs){
      dum <- 
        tryCatch({
          as.numeric(
            gsub(",", "\\.", input[[inp]])
          )
        },
        warning = function(x){
          NA
        })
      
      if(is.na(dum)){
        toggle(id = errorDivs[which(inputs == inp)], 
               condition = TRUE)
        print(paste("Error: ", inp))
        checks <- FALSE
      } else {
        toggle(id = errorDivs[which(inputs == inp)], 
               condition = FALSE)
        assign(x = inp, 
               value = dum)
      }
    }
    
    if(!checks)
      return(NULL)
    # Opslaan:
    opgeslagenLeningen <<- rbind(opgeslagenLeningen, data.table(
      "Bank" = input$lenBank,
      "Te_Lenen_Bedrag" = paste(leningSamenstelling$Te_Lenen_Bedrag, collapse = "/"),
      "Vast_Of_Variabel" = paste(leningSamenstelling$Vast_Of_Variabel, collapse = "/"),
      "Rentevoet" = paste(leningSamenstelling$Rentevoet, collapse = "/"),
      "Jaar" = paste(leningSamenstelling$Jaar, collapse = "/"),
      "Kosten_Bijhouden" = input$kostenCheck,
      "Kosten_Eenmalig" = lenKost1,
      "Kosten_Maandelijks" = lenKostM,
      "Kosten_Jaarlijks" = lenKostJ,
      "Inflatie_Inrekenen" = input$inflCheck,
      "Inflatie_Percentage" = lenInfl,
      "Vermogen_Bijhouden" = input$vermogenCheck,
      "Vermogen_Start" = lenVermStart,
      "Vermogen_Maandelijske_Inkomsten" = lenVermInk,
      "Vermogen_Maandelijske_Uitgaven" = lenVermUit,
      "Vermogen_Beleggingspercentage" = lenVermBelPerc,
      "Vermogen_Opbrengst" = lenVermBelOpbrPerc
    ))
    
    toonOpgeslagenLeningen()
  })
  
  toonOpgeslagenLeningen <- function(){
    if(is.null(opgeslagenLeningen) || dim(opgeslagenLeningen)[1] == 0){
      output$leningenDT <- 
        renderDataTable(data.table(Lening ="Geen opgeslagen leningen gevonden!"))
    } else {
      output$leningenDT <- renderDataTable(opgeslagenLeningen,
                                           options=list(autoWidth = TRUE,
                                                        scrollX=TRUE, 
                                                        paging = FALSE,
                                                        searching = FALSE,
                                                        pageLength = -1))
    }
  }
  toonOpgeslagenLeningen()
  
  
  # Lening berekenen
  # lenBereken knop
  
  # Toggle off all errorDivs
  for(errorDiv in c("lenBedrError", "lenRVError", "lenJaarError",
                    "lenSamError", "lenBankError",
                    "lenKost1Error", "lenKostMError", "lenKostJError", "lenInflError",
                    "lenVermStartError", "lenVermInkError", "lenVermUitError", "lenVermBelPercError",
                    "lenVermBelOpbrPercError"))
    toggle(id = errorDiv, condition = FALSE)
})