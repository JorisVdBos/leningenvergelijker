dbHeader <- dashboardHeader(title = "Leningen vergelijker")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text     = "Simuleer lening", 
             tabName  = "simLen", 
             icon     = icon("calculator")),
    menuItem(text     = "Vergelijk leningen", 
             tabName  = "vergLen", 
             icon     = icon("line-chart")),
    menuItem(text     = "Meer info", 
             tabName  = "meerInfo", 
             icon     = icon("question"))
  )
)

body <- dashboardBody(
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "shinydashboard-0.5.1/shinydashboard.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "AdminLTE-2.0.6/AdminLTE.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "AdminLTE-2.0.6/_all-skins.min.css")
  ),
  tabItems(
    tabItem(
      tabName = "simLen",
      fluidPage(
        fluidRow(
          h1("Simuleer een lening"),
          p("Eest een beetje uitleg over de app."),
          p("Bladibla :)"))
        ),
        tabsetPanel(
          tabPanel(
            "Nieuwe lening",
            wellPanel(
              fluidRow(
                column(
                  width = 6,
                  textInput("lenBedr",
                            "Te lenen bedrag in euro:",
                            placeholder = "150000"),
                  div(id = "lenBedrError",
                      em(HTML("<font color='red'>Gelieve een juist getal in te geven.</font>")),
                      br()),
                  br(),
                  radioButtons("lenVastOfVar",
                               "Variabel of vaste rentevoet",
                               choices = c("Vast", "Variabel")),
                  textInput("lenRV",
                            "Rentevoet in %:",
                            placeholder = "2,5"),
                  div(id = "lenRVError",
                      p(em(HTML("<font color='red'>Gelieve een juist getal in te geven.</font>"))),
                      br()),
                  textInput("lenJaar",
                            "Jaar:",
                            placeholder = "25"),
                  div(id = "lenJaarError",
                      p(em(HTML("<font color='red'>Gelieve een juist getal in te geven.</font>")))),
                  br(),
                  actionButton(
                    "lenVoegToe",
                    "Voeg toe"),
                  actionButton(
                    "lenLaatsteWeg",
                    "Verwijder laatse invoer"),
                  actionButton(
                    "lenAllesWeg",
                    "Verwijder alles"),
                  br(),
                  br(),
                  br(),
                  br(),
                  dataTableOutput("lenInputDT"),
                  div(id = "lenSamError",
                      p(em(HTML("<font color='red'>Gelieve een leninghoeveelheid in te geven.</font>"))))
                ),
                column(
                  width = 6,
                  p(paste0("Optionele extra informatie waar de ",
                           "simulatie rekening mee kan houden:")),
                  checkboxInput(
                    "kostenCheck",
                    "Kosten"),
                  div(
                    id = "lenKostendiv",
                    textInput("lenKost1",
                              "Eenmalige kosten, zoals bijvoorbeeld dossierkosten: ", 
                              placeholder = "500"),
                    div(id = "lenKost1Error",
                        p(em(HTML("<font color='red'>Gelieve een juist getalin te geven.</font>")))),
                    textInput("lenKostM", 
                              paste0("Maandlijke kosten, zoals bijvoorbeeld ",
                                     "bankrekeningkosten of schuldsaldo verzekering: "), 
                              placeholder = "162,62"),
                    div(id = "lenKostMError",
                        p(em(HTML("<font color='red'>Gelieve een juist getal in te geven.</font>")))),
                    textInput("lenKostJ", 
                              paste0("Jaarlijkse kosten, zoals bijvoorbeeld ",
                                     "brandverzekerning: "), 
                              placeholder = "256,3")),
                  div(id = "lenKostJError",
                      p(em(HTML("<font color='red'>Gelieve een juist getal in te geven.</font>")))),
                  checkboxInput(
                    "inflCheck",
                    "Inflatie", 
                    value = FALSE),
                  div(
                    id = "lenInfldiv",
                    textInput("lenInfl", 
                              "Inflatie in percent per jaar: ", 
                              placeholder = "2,0")),
                  div(id = "lenInflError",
                      p(em(HTML("<font color='red'>Gelieve een juist getal in te geven.</font>")))),
                  checkboxInput(
                    "vermogenCheck",
                    "Vermogen bijhouden", 
                    value = FALSE),
                  div(
                    id = "lenVermdiv",
                    textInput("lenVermStart", 
                              "Vermogen bij start ingang lening:", 
                              placeholder = "20000"),
                    div(id = "lenVermStartError",
                        p(em(HTML("<font color='red'>Gelieve een juist getal in te geven.</font>")))),
                    textInput("lenVermInk", 
                              "Inkomsten per maand:", 
                              placeholder = "2400"),
                    div(id = "lenVermInkError",
                        p(em(HTML("<font color='red'>Gelieve een juist getal in te geven.</font>")))),
                    textInput("lenVermUit", 
                              "Uitgaven per maand (de lening niet meegeteld):", 
                              placeholder = "1200"),
                    div(id = "lenVermUitError",
                        p(em(HTML("<font color='red'>Gelieve een juist getal in te geven.</font>")))),
                    textInput("lenVermBelPerc", 
                              "Percentage van vermogen in beleggingen:", 
                              value = 0,
                              placeholder = "40"),
                    div(id = "lenVermBelPercError",
                        p(em(HTML("<font color='red'>Gelieve een juist getal in te geven.</font>")))),
                    textInput("lenVermBelOpbrPerc", 
                              "Opbrengstpercentage van belegd vermogen per jaar:", 
                              placeholder = "2.0"),
                    div(id = "lenVermBelOpbrPercError",
                        p(em(HTML("<font color='red'>Gelieve een juist getal in te geven.</font>"))))
                    )
                )
              )
            ),
            fluidRow(
              column(
                width = 8,
                align="center",
                br(),
                br(),
                actionButton(
                  "lenBereken",
                  label = "       Start simulatie",
                  icon("play"),
                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
              ),
              column(
                width = 4,
                "Deze lening opslaan:",
                textInput("lenBank",
                          "Naam van de bank:"),
                div(id = "lenBankError",
                    p(em(HTML("<font color='red'>Gelieve een naam in te voeren.</font>")))),
                actionButton(
                  "lenOpslaan",
                  "Opslaan")
              )
            ),
            "Simulatie output"
          ),
          tabPanel(
            "Opgeslagen leningen",
            wellPanel(
              dataTableOutput("leningenDT"),
              fluidRow(
                column(
                  width = 4
                ),
                column(
                  width = 4,
                  align = "right",
                  br(),
                  actionButton(
                    "leningenVerw",
                    "Verwijder geselecteerde lening"
                  ),
                  br(),
                  br(),
                  div(id = "leningenVerwAllesDiv",
                     actionButton(
                       "leningenVerwAlles",
                       "Verwijder alle opgeslagen leningen")
                  ),
                  div(id = "leningenVerwAlles2Div",
                     actionButton(
                       "leningenVerwAlles2",
                       "Ben je zeker?",
                       style = "color: #fff; background-color: #ff0000; border-color: #2e6da4")
                  )
                ),
                column(
                  width = 4,
                  align = "left",
                  fileInput(
                    "leningenImp",
                    "Importeer tabel",
                    multiple = FALSE, 
                    accept = "RData"
                  ),
                  div(id = "leningenImpError",
                      p(em(HTML("<font color='red'>Gelieve een naam in te voeren.</font>")))),
                  downloadButton(
                    "leningenExp",
                    "Exporteer tabel"
                  )
                )
              ),
              column(
                width = 8,
                align="center",
                br(),
                br(),
                actionButton(
                  "lenBereken2",
                  label = "       Start simulatie",
                  icon("play"),
                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
              ))
          )
        )
    ),
    tabItem(
      tabName = "vergLen",
      h1("Simuleer verschillende leningen")
    ),
    tabItem(
      tabName = "meerInfo",
      h1("Meer informatie over deze applicatie")
    )
  )
)


ui <- dashboardPage(skin    = "green",
                    header  = dbHeader,
                    sidebar = sidebar,
                    body    = body)
