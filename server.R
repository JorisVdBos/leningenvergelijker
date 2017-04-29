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
  
  leningSamenstelling <- NULL
  observeEvent(input$lenVoegToe, {
    # Invoer checken:
    checks <- TRUE
    
    inputs <- c("lenTot", "lenRV", "lenJaar")
    errorDivs <- c("lenTotError", "lenRVError", "lenJaarError")
    
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
              "Te_Lenen_Bedrag" = floor(lenTot),
              "Vast_Of_Variabel" = input$lenVastOfVar,
              "rentevoet" = lenRV,
              "Jaar" = floor(lenJaar)
            ))
    output$lenInputDT <- renderDataTable(leningSamenstelling,
                                         options=list(autoWidth = TRUE,
                                                      scrollX=TRUE, 
                                                      paging = FALSE,
                                                      searching = FALSE,
                                                      pageLength = -1))
  })
  
  observeEvent(input$lenLaatsteWeg, {
    if(is.null(leningSamenstelling) || dim(leningSamenstelling)[1] == 0)
      return(NULL)
    
    leningSamenstelling <<- 
      leningSamenstelling[-dim(leningSamenstelling)[1]]
    output$lenInputDT <- renderDataTable(leningSamenstelling,
                                         options=list(autoWidth = TRUE,
                                                      scrollX=TRUE, 
                                                      paging = FALSE,
                                                      searching = FALSE,
                                                      pageLength = -1))
  })
  
  # Leningen opslaan
  # lenOpslaan knop
  
  # Lening berekenen
  # lenBereken knop
  
  # Toggle off all errorDivs
  for(errorDiv in c("lenTotError", "lenRVError", "lenJaarError"))
    toggle(id = errorDiv, condition = FALSE)
})