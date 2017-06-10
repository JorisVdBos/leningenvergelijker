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
  
  observeEvent(input$lenVastOfVar, {
    toggle(id = "lenVariabelOptie", condition = input$lenVastOfVar == "Variabel")
  })
  
  leningSamenstelling <- data.table(Te_Lenen_Bedrag = numeric(),
                                    Vast_Of_Variabel = character(),
                                    Variabel_Herziening = numeric(),
                                    Rentevoet = numeric(),
                                    Jaar = numeric())
  observeEvent(input$lenVoegToe, {
    # Invoer checken:
    checks <- TRUE
    
    inputs <- c("lenBedr", "lenRV", "lenJaar")
    errorDivs <- c("lenBedrError", "lenRVError", "lenJaarError")
    
    if(input$lenVastOfVar == "Variabel"){
      inputs <- c(inputs, "lenVarType")
      errorDivs <- c(errorDivs, "lenVarTypeError")
    } else {
      lenVarType <- "Vast"
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
    if(is.numeric(lenVarType))
      lenVarType <- floor(lenVarType)
    
    leningSamenstelling <<- 
      rbind(leningSamenstelling, 
            data.table(
              "Te_Lenen_Bedrag" = floor(lenBedr),
              "Vast_Of_Variabel" = input$lenVastOfVar,
              "Variabel_Herziening" = lenVarType, 
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
  if(file.exists("opgeslagenLeningenVoorbeelden.feather"))
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
      inputs <- c(inputs, "lenVermStart", "lenVermInk", 
                  "lenVermBelPerc", "lenVermBelOpbrPerc")
      errorDivs <- c(errorDivs, "lenVermStartError", "lenVermInkError", 
                     "lenVermBelPercError", "lenVermBelOpbrPercError")
    } else {
      lenVermStart <- NA
      lenVermInk <- NA
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
      "Variabel_Herziening" = paste(leningSamenstelling$Variabel_Herziening, collapse = "/"),
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
      "Vermogen_Maandelijsk_Sparen" = lenVermInk,
      "Vermogen_Beleggingspercentage" = lenVermBelPerc,
      "Vermogen_Opbrengst" = lenVermBelOpbrPerc
    ))
    
    toonOpgeslagenLeningen()
  })
  
  toonOpgeslagenLeningen <- function(){
    if(is.null(opgeslagenLeningen) || dim(opgeslagenLeningen)[1] == 0){
      opgeslagenLeningen <- 
        data.table(Lening ="Geen opgeslagen leningen gevonden!")
    }
    
    output$leningenDT <- renderDataTable(opgeslagenLeningen,
                                         options=list(paging = FALSE,
                                                      deferRender = FALSE,
                                                      info = FALSE,
                                                      searching = FALSE,
                                                      autoWidth = TRUE,
                                                      scrollX=TRUE, 
                                                      pageLength = -1),
                                         rownames = FALSE,
                                         selection = 'single')
    
    # Testing:
    # if(!is.null(opgeslagenLeningen) &&
    #    dim(opgeslagenLeningen)[1] > 0)
    #   write_feather(x = opgeslagenLeningen, path = "opgeslagenLeningenVoorbeelden.feather")
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
      if(is.null(opgeslagenLeningen) || dim(opgeslagenLeningen)[1] == 0)
        return(NULL)
      
      write_feather(x = opgeslagenLeningen, path = file)
    }
  )
  
  # Lening berekenen
  berekendeLening <- NULL
  
  observeEvent(input$lenBereken, {
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
      inputs <- c(inputs, "lenVermStart", "lenVermInk", 
                  "lenVermBelPerc", "lenVermBelOpbrPerc")
      errorDivs <- c(errorDivs, "lenVermStartError", "lenVermInkError", 
                     "lenVermBelPercError", "lenVermBelOpbrPercError")
    } else {
      lenVermStart <- NA
      lenVermInk <- NA
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
    
    # opties:
    leningInvoer <- data.table(
      "Te_Lenen_Bedrag" = paste(leningSamenstelling$Te_Lenen_Bedrag, collapse = "/"),
      "Vast_Of_Variabel" = paste(leningSamenstelling$Vast_Of_Variabel, collapse = "/"),
      "Variabel_Herziening" = paste(leningSamenstelling$Variabel_Herziening, collapse = "/"),
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
      "Vermogen_Maandelijsk_Sparen" = lenVermInk,
      "Vermogen_Beleggingspercentage" = lenVermBelPerc,
      "Vermogen_Opbrengst" = lenVermBelOpbrPerc
    )
    
    # Check ok
    toggle(id = "lenBereken2Error", condition = FALSE)
    toggle(id = "leningBerekenBds", condition = TRUE)
    toggle(id = "leningResultaat", condition = FALSE)
    
    # Bereken nieuwe lening
    opties <-  as.list(leningInvoer[1])
    berekendeLening <<- simuleerLeningShiny(
      leningTotaalEuro = as.numeric(
        strsplit(
          leningInvoer$Te_Lenen_Bedrag[1],split = "/"
        )[[1]]
      ),
      percentJaar = as.numeric(
        strsplit(
          leningInvoer$Rentevoet[1],split = "/"
        )[[1]]
      )/100, 
      jaar = as.numeric(
        strsplit(
          leningInvoer$Jaar[1],split = "/"
        )[[1]]
      ), 
      type = strsplit(
        leningInvoer$Vast_Of_Variabel[1],split = "/"
      )[[1]],
      variabelType = suppressWarnings(
        as.numeric(
          strsplit(
            leningInvoer$Variabel_Herziening[1],split = "/"
          )[[1]]
        )),
      opties = opties)
    
    # Tonen resultaat
    output$lenBeschrijving <- renderUI({
      HTML(paste(berekendeLening$beschrijving, collapse = "\n"))
    })
    
    # Beschrijving kosten
    nietweergeven <- c()
    if(!opties$Kosten_Bijhouden){
      nietweergeven <- c(nietweergeven, "extraKosten")
      nietweergeven <- c(nietweergeven, "extraKosten_inflatie")
    }
    if(!opties$Inflatie_Inrekenen){
      nietweergeven <- c(nietweergeven, "extraKosten_inflatie")
    }
    if(!opties$Vermogen_Bijhouden){
      nietweergeven <- c(nietweergeven, "vermogen", "beleggen_interest")
    }
    if(length(nietweergeven) > 0){
      berekendeLening$aflostabel <- berekendeLening$aflostabel[,-nietweergeven, with = FALSE]
    }
    
    # Leningsimulaties output:
    output$lenAflossingstabel <- renderDataTable({
      berekendeLening$aflostabel
    }, options = list(deferRender = FALSE,
                      info = FALSE,
                      searching = FALSE,
                      scrollX=TRUE, 
                      pageLength = 12),
    rownames = FALSE,
    selection = 'none')
    
    # Plotopties
    output$grafiekKolommenUI <- renderUI({
      opties <- colnames(berekendeLening$aflostabel)[colnames(berekendeLening$aflostabel) != "maand"]
      if(length(grep("inflatie", opties)) > 0)
        opties <- opties[-grep("inflatie", opties)]
      opties <- opties[!opties %in% c("lening_open", "vermogen")]
      selectInput("grafiekKolommen", "Plot volgende kolommen: ", 
                  choices = opties, multiple = TRUE, selected = opties)
    })
    
    output$grafiekStartDatumUI <- renderUI({
      opties <- format(
        as.Date(Sys.time())-days(as.integer(format(Sys.time(), "%d"))-1) + months(-12:13),
        format = "%Y-%m"
      )
      selectInput("grafiekDatum", "Maand van de eerste betaling:", 
                  choices = opties, selected = opties[13], multiple = FALSE)
    })
    
    
    observeEvent(c(input$grafiekDatum, input$grafiekKolommen, input$grafiekInflatie, 
                   input$grafiekInflatiePerc, input$grafiekCumulatief), {
                     plot1 <- leningGrafiek(aflosTabel = berekendeLening$aflostabel, 
                                            startDate = input$grafiekDatum, 
                                            kolommen = input$grafiekKolommen,
                                            inflatie = input$grafiekInflatie,
                                            inflatiePerc = input$grafiekInflatiePerc, 
                                            cumulatief = input$grafiekCumulatief)
                     output$grafiekPlot <- renderChart2({
                       plot1
                     })
                   })
    
    
    toggle(id = "leningBerekenBds", condition = FALSE)
    toggle(id = "leningResultaat", condition = TRUE)
  })
  
  observeEvent(input$lenBereken2, {
    # Check invoer
    geselecteerdeRij <- input$leningenDT_rows_selected
    if(is.null(geselecteerdeRij) || length(geselecteerdeRij) != 1){
      toggle(id = "lenBereken2Error", condition = TRUE)
      toggle(id = "leningBerekenBds", condition = FALSE)
      toggle(id = "leningResultaat", condition = FALSE)
      return(NULL)
    }
    
    # Check ok
    toggle(id = "lenBereken2Error", condition = FALSE)
    toggle(id = "leningBerekenBds", condition = TRUE)
    toggle(id = "leningResultaat", condition = FALSE)
    
    # Bereken nieuwe lening
    opties <- as.list(opgeslagenLeningen[geselecteerdeRij])
    berekendeLening <<- simuleerLeningShiny(
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
    
    # Tonen resultaat
    output$lenBeschrijving <- renderUI({
      HTML(paste(berekendeLening$beschrijving, collapse = "\n"))
    })
    
    # Beschrijving kosten
    nietweergeven <- c()
    if(!opties$Kosten_Bijhouden){
      nietweergeven <- c(nietweergeven, "extraKosten")
      nietweergeven <- c(nietweergeven, "extraKosten_inflatie")
    }
    if(!opties$Inflatie_Inrekenen){
      nietweergeven <- c(nietweergeven, "extraKosten_inflatie")
    }
    if(!opties$Vermogen_Bijhouden){
      nietweergeven <- c(nietweergeven, "vermogen", "beleggen_interest")
    }
    if(length(nietweergeven) > 0){
      berekendeLening$aflostabel <- berekendeLening$aflostabel[,-nietweergeven, with = FALSE]
    }
    
    # Leningsimulaties output:
    output$lenAflossingstabel <- renderDataTable({
      berekendeLening$aflostabel
    }, options = list(deferRender = FALSE,
                      info = FALSE,
                      searching = FALSE,
                      scrollX=TRUE, 
                      pageLength = 12),
    rownames = FALSE,
    selection = 'none')
    
    # Plotopties
    output$grafiekKolommenUI <- renderUI({
      opties <- colnames(berekendeLening$aflostabel)[colnames(berekendeLening$aflostabel) != "maand"]
      if(length(grep("inflatie", opties)) > 0)
        opties <- opties[-grep("inflatie", opties)]
      opties <- opties[!opties %in% c("lening_open", "vermogen")]
      selectInput("grafiekKolommen", "Plot volgende kolommen: ", 
                  choices = opties, multiple = TRUE, selected = opties)
    })
    
    output$grafiekStartDatumUI <- renderUI({
      opties <- format(
        as.Date(Sys.time())-days(as.integer(format(Sys.time(), "%d"))-1) + months(-12:13),
        format = "%Y-%m"
      )
      selectInput("grafiekDatum", "Maand van de eerste betaling:", 
                  choices = opties, selected = opties[13], multiple = FALSE)
    })
    
    
    observeEvent(c(input$grafiekDatum, input$grafiekKolommen, input$grafiekInflatie, 
                   input$grafiekInflatiePerc, input$grafiekCumulatief), {
      plot1 <- leningGrafiek(aflosTabel = berekendeLening$aflostabel, 
                             startDate = input$grafiekDatum, 
                             kolommen = input$grafiekKolommen,
                             inflatie = input$grafiekInflatie,
                             inflatiePerc = input$grafiekInflatiePerc, 
                             cumulatief = input$grafiekCumulatief)
      output$grafiekPlot <- renderChart2({
        plot1
      })
    })
    
    
    toggle(id = "leningBerekenBds", condition = FALSE)
    toggle(id = "leningResultaat", condition = TRUE)
  })
  
  
  # Toggle off all errorDivs
  for(errorDiv in c("lenBedrError", "lenRVError", "lenJaarError",
                    "lenVarTypeError",
                    "lenSamError", "lenBankError",
                    "lenKost1Error", "lenKostMError", "lenKostJError", "lenInflError",
                    "lenVermStartError", "lenVermInkError", "lenVermBelPercError",
                    "lenVermBelOpbrPercError",
                    "leningenImpError",
                    "leningBerekenBds", "leningResultaat",
                    "lenBereken2Error"))
    toggle(id = errorDiv, condition = FALSE)
})