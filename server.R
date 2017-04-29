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
    opgeslagenLeningen <- data.table(read_feather(path = "opgeslagenLeningenVoorbeelden.feather"))
  
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
                                           options=list(paging = FALSE,
                                                        deferRender = FALSE,
                                                        info = FALSE,
                                                        processing = TRUE,
                                                        searching = FALSE,
                                                        autoWidth = TRUE,
                                                        scrollX=TRUE, 
                                                        pageLength = -1),
                                           rownames = FALSE,
                                           selection = 'single')
    }
    # Testing:
    # write_feather(x = opgeslagenLeningen, path = "opgeslagenLeningenVoorbeelden.feather")
    toggle(id = "leningenVerwAlles2Div", condition = FALSE)
  }
  toonOpgeslagenLeningen()
  
  observeEvent(input$leningenVerw, {
    geselecteerdeRij <- input$leningenDT_rows_selected
    if(is.null(geselecteerdeRij) || length(geselecteerdeRij) == 0){
      return(NULL)
    }
    opgeslagenLeningen <<- opgeslagenLeningen[-geselecteerdeRij]
    toonOpgeslagenLeningen()
  })
  
  observeEvent(input$leningenVerwAlles, {
    toggle(id = "leningenVerwAllesDiv", condition = FALSE)
    toggle(id = "leningenVerwAlles2Div", condition = TRUE)
  })
  
  observeEvent(input$leningenVerwAlles2, {
    opgeslagenLeningen <<- NULL
    toonOpgeslagenLeningen()
    toggle(id = "leningenVerwAllesDiv", condition = TRUE)
    toggle(id = "leningenVerwAlles2Div", condition = FALSE)
    
  })
  
  # Import/Export
  observeEvent(input$leningenImp, {
    print(input$leningenImp$datapath)
    tryCatch({
      opgeslagenLeningen   <<- data.table(read_feather(input$leningenImp$datapath))
      toonOpgeslagenLeningen()
      toggle(id = "leningenImpError", condition = FALSE)
      print("Succes")
    },
    error = function(x){
      toggle(id = "leningenImpError", condition = TRUE)
      print("Fail")
    })
  })
  
  output$leningenExp <- downloadHandler(
    filename = function() {
      'LeningenVergelijker.feather'
    },
    content = function(file) {
      write_feather(x = opgeslagenLeningen, path = file)
    }
  )
  
  # Lening berekenen
  # lenBereken knop
  
  # Toggle off all errorDivs
  for(errorDiv in c("lenBedrError", "lenRVError", "lenJaarError",
                    "lenSamError", "lenBankError",
                    "lenKost1Error", "lenKostMError", "lenKostJError", "lenInflError",
                    "lenVermStartError", "lenVermInkError", "lenVermUitError", "lenVermBelPercError",
                    "lenVermBelOpbrPercError",
                    "leningenImpError"))
    toggle(id = errorDiv, condition = FALSE)
})