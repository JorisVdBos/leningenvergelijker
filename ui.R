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
                  textInput("lenTot",
                            "Te lenen bedrag in euro:",
                            placeholder = "150000"),
                  div(id = "lenTotError",
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
                      p(em(HTML("<font color='red'> Gelieve een juist getal in te geven.</font>")))),
                  br(),
                  actionButton(
                    "lenVoegToe",
                    "Voeg toe"),
                  actionButton(
                    "lenLaatsteWeg",
                    "Verwijder laatse invoer"),
                  br(),
                  br(),
                  br(),
                  br(),
                  dataTableOutput("lenInputDT")
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
                    textInput("lenKostM", 
                              paste0("Maandlijke kosten, zoals bijvoorbeeld ",
                                     "bankrekeningkosten of schuldsaldo verzekering: "), 
                              placeholder = "162,62"),
                    textInput("lenKostJ", 
                              paste0("Jaarlijkse kosten, zoals bijvoorbeeld ",
                                     "brandverzekerning: "), 
                              placeholder = "256,3")),
                  checkboxInput(
                    "inflCheck",
                    "Inflatie", 
                    value = FALSE),
                  div(
                    id = "lenInfldiv",
                    textInput("lenInfl", 
                              "Inflatie in percent per jaar: ", 
                              placeholder = "2,0")),
                  checkboxInput(
                    "vermogenCheck",
                    "Vermogen bijhouden", 
                    value = FALSE),
                  div(
                    id = "lenVermdiv",
                    textInput("lenVermStart", 
                              "Vermogen bij start ingang lening:", 
                              placeholder = "20000"),
                    textInput("lenVermInk", 
                              "Inkomsten per maand:", 
                              placeholder = "2400"),
                    textInput("lenVermUit", 
                              "Uitgaven per maand (de lening niet meegeteld):", 
                              placeholder = "1200"),
                    textInput("lenVermBelPerc", 
                              "Percentage van vermogen in beleggingen:", 
                              placeholder = "40"),
                    textInput("lenVermBelOpbrPerc", 
                              "Opbrengstpercentage van belegd vermogen per jaar:", 
                              placeholder = "2.0")
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
              "2"
            )
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
