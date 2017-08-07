# Header ----
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

# Body ----
body <- dashboardBody(
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "shinydashboard-0.5.1/shinydashboard.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "AdminLTE-2.0.6/AdminLTE.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "AdminLTE-2.0.6/_all-skins.min.css")
  ),
  tabItems(
    # Lening simulatie ----
    tabItem(
      tabName = "simLen",
      h1("Simuleer een lening"),
      #p("Een huis of appartement gekocht? Proficiat! Maar hola, de zoektocht is nog niet afgelopen! Een goede lening vinden kan je duizenden euro's besparen, dus een nieuwe zoektocht gaat van start. Algouw ligt je keukentafel vol met papieren met letterlijk duizenden cijfertjes. Bank A geeft een betere rentevoet, maarja bank B heeft dan weer goedkopere verzekeringen! Economisch gezien moet je denken aan inflatie en zo weinig mogelijk lenen, maar fiscaal gezien moet je dan weer zo lang mogelijk lenen. Vriend 1 zegt dit en vriend 2 zegt dat, maar welke lening is nu de beste?"),
      p("Om leningen te vergelijken begonnen wij een excelbestand waar we alle leningvoorstellen verzamelden. Zelfs met enkele excel functies kregen we moeilijk vat op de waarde van de verschillende voorstellen en moesten we toch nog vaak teruggrijpen naar de aflostabellen van de banken. Daarom schreef ik voor mezelf en mijn partner een applicatie in de computertaal 'R' die leningen simuleerde. Zo kon ik ook vragen beantwoorden zoals wat met inflatie, bank kosten, beleggingen, ..."),
      p("Hieronder zie je een voorbeeld van een verzameling bankvoorstellen. Aflostabellen en grafieken bekom je door een lening aan te klikken en op de knop 'Start simulatie' te klikken. In de tab 'nieuwe lening' kan je zelf leningen aan deze tabel toevoegen. Zo kan je je verzameling bankvoorstellen hier aanmaken, exporteren en opnieuw inladen zoveel je wilt!"),
      # Invoer simulator ----
      tabsetPanel(
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
                     styleclass = "danger")
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
            )
          ),
          fluidRow(
            column(
              width = 8,
              align="center",
              br(),
              br(),
              actionButton(
                "lenBereken2",
                label = "Start simulatie",
                styleclass = "success"),
              br(),
              div(id = "lenBereken2Error",
                  p(em(HTML("<font color='red'>Gelieve een lening aan te duiden in bovenstaande tabel.</font>")))),
              br(),
              br(),
              br()
            )
          )
        ),
        # Nieuwe lening ----
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
                    em(HTML("<font color='red'>Gelieve een correct getal in te geven.</font>")),
                    br()),
                br(),
                radioButtons("lenVastOfVar",
                             "Variabel of vaste rentevoet",
                             choices = c("Vast", "Variabel")),
                div(id = "lenVariabelOptie",
                    textInput("lenVarType",
                              "Herziening jaren:",
                              placeholder = "3"),
                    div(id = "lenVarTypeError",
                        em(HTML("<font color='red'>Gelieve een correct getal in te geven.</font>")),
                        br()),
                    paste0("Opmerking: De simulatie gaat steeds van het slechste scenario uit: Dat bij de ",
                           "eerste herziening van de rentevoet, deze verdubbelt met een maximum van 2%.")),
                textInput("lenRV",
                          "Rentevoet in %:",
                          placeholder = "2,5"),
                div(id = "lenRVError",
                    p(em(HTML("<font color='red'>Gelieve een correct getal in te geven.</font>"))),
                    br()),
                textInput("lenJaar",
                          "Jaar:",
                          placeholder = "25"),
                div(id = "lenJaarError",
                    p(em(HTML("<font color='red'>Gelieve een correct getal in te geven.</font>")))),
                br(),
                actionButton(
                  "lenVoegToe",
                  "Voeg toe",styleclass = "warning"),
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
                      p(em(HTML("<font color='red'>Gelieve een correct getal in te geven.</font>")))),
                  textInput("lenKostM", 
                            paste0("Maandlijke kosten, zoals bijvoorbeeld ",
                                   "bankrekeningkosten of schuldsaldo verzekering: "), 
                            placeholder = "162,62"),
                  div(id = "lenKostMError",
                      p(em(HTML("<font color='red'>Gelieve een correct getal in te geven.</font>")))),
                  textInput("lenKostJ", 
                            paste0("Jaarlijkse kosten, zoals bijvoorbeeld ",
                                   "brandverzekerning: "), 
                            placeholder = "256,3")),
                div(id = "lenKostJError",
                    p(em(HTML("<font color='red'>Gelieve een correct getal in te geven.</font>")))),
                checkboxInput(
                  "inflCheck",
                  "Inflatie", 
                  value = FALSE),
                div(
                  id = "lenInfldiv",
                  paste0("Naast extra berekingen worden", 
                         "volgende waarden per maand aangepast aan de inflatie: ",
                         "Extra kosten van de lening, je maandelijks sparen. ", 
                         "Deze worden normaal door de jaren wel aangepast door ",
                         "de bank en jezelf."),
                  textInput("lenInfl", 
                            "Inflatie in percent per jaar: ", 
                            placeholder = "2,0")),
                div(id = "lenInflError",
                    p(em(HTML("<font color='red'>Gelieve een correct getal in te geven.</font>")))),
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
                      p(em(HTML("<font color='red'>Gelieve een correct getal in te geven.</font>")))),
                  textInput("lenVermInk", 
                            "Gespaard bedrag per maand:", 
                            placeholder = "500"),
                  div(id = "lenVermInkError",
                      p(em(HTML("<font color='red'>Gelieve een correct getal in te geven.</font>")))),
                  textInput("lenVermBelPerc", 
                            "Percentage van vermogen in beleggingen:", 
                            value = 0,
                            placeholder = "40"),
                  div(id = "lenVermBelPercError",
                      p(em(HTML("<font color='red'>Gelieve een correct getal in te geven.</font>")))),
                  textInput("lenVermBelOpbrPerc", 
                            "Opbrengstpercentage van belegd vermogen per jaar:", 
                            placeholder = "2.0"),
                  div(id = "lenVermBelOpbrPercError",
                      p(em(HTML("<font color='red'>Gelieve een correct getal in te geven.</font>"))))
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
              br(),
              actionButton(
                "lenBereken",
                label = "       Start simulatie",
                styleclass = "success"),
              br(),
              br()
            ),
            column(
              width = 4,
              "Deze lening opslaan:",
              textInput("lenBank",
                        "Naam van de bank:"),
              div(id = "lenBankError",
                  p(em(HTML("<font color='red'>Gelieve een naam in te voeren.</font>")))),
              div(id = "lenBankError2",
                  p(em(HTML("<font color='red'>Deze naam bestaat al!</font>")))),
              actionButton(
                "lenOpslaan",
                "Opslaan"),
              div(id = "lenBankSucces",
                  p(em(HTML("<font color='green'>Je lening werd aan de tabel toegevoegd!</font>")))),
              br(),
              br()
            )
          )
        )
      ),
      # Lening resultaat ----
      fluidPage(
        div(id = "leningBerekenBds",
            "Berekenen..."),
        div(
          id = "leningResultaat",
          wellPanel(
              uiOutput("lenBeschrijving")
          ),
          tabsetPanel(
            tabPanel(
              "Aflostabel",
              dataTableOutput("lenAflossingstabel"),
              downloadButton(
                "lenAflossingstabelExport",
                "Exporteer aflossingstabel (.csv)"
              )
            ),
            tabPanel(
              "Grafiek",
              wellPanel(
                uiOutput("grafiekKolommenUI"),
                uiOutput("grafiekStartDatumUI"), 
                checkboxInput("grafiekInflatie", "Inflatie inrekenen"),
                sliderInput("grafiekInflatiePerc", "Inflatie percentage:", 
                            min = -10, max = 10, value = 2, step = 0.1),
                checkboxInput("grafiekCumulatief", "Cumulatief")
              ),
              wellPanel(
                plotOutput("grafiekPlot")
              ),
              dataTableOutput("grafiekTabel"),
              downloadButton(
                "grafiekExport",
                "Exporteer grafiek data (.csv)"
              )
            )
          )
        )
      )
    ),
    # Vergelijk leningen ----
    tabItem(
      tabName = "vergLen",
      h1("Vergelijk leningen"),
      p("Op deze pagina worden al je ingegeven leningen naast elkaar gelegd en vergeleken."),
      wellPanel(
        dataTableOutput("vergLenInputDT"),
        br(),
        actionButton("vergLenButton", label = "Start vergelijking!", styleclass = "success")
      ),
      
      div(id = "lenBerekenBds",
          "Berekenen..."),
      div(
        id = "lenResultaat",
        wellPanel(
          uiOutput("vergLenBeschrijving")
        ),
        tabsetPanel(
          tabPanel(
            "Tabel",
            dataTableOutput("vergLenOutputDT"),
            downloadButton(
              "vergLenAflossingstabelExport",
              "Exporteer vergelijkingstabel (.csv)"
            )
          ),
          tabPanel(
            "Grafiek",
            wellPanel(
              uiOutput("vergGrafiekKolommenUI"),
              uiOutput("vergGrafiekStartDatumUI"), 
              checkboxInput("vergGrafiekInflatie", "Inflatie inrekenen"),
              sliderInput("vergGrafiekInflatiePerc", "Inflatie percentage:", 
                          min = -10, max = 10, value = 2, step = 0.1),
              checkboxInput("vergGrafiekCumulatief", "Cumulatief")
            ),
            wellPanel(
              plotOutput("vergGrafiekPlot")
            ),
            dataTableOutput("vergGrafiekTabel"),
            downloadButton(
              "vergGrafiekExport",
              "Exporteer grafiek data (.csv)"
            )
          )
        )
      )
    ),
    tabItem(
      tabName = "meerInfo",
      h1("Meer informatie over deze applicatie"),
      fluidRow(HTML(paste0(
        "<p>Deze applicatie werd gebouwd door mij, <a href=\"mailto:joris.bdbossche@gmail.com\">Joris Van den Bossche</a>, geïnspireerd door mijn eigen zoektocht naar een lening! Alle vragen of opmerkingen over deze applicatie zijn uiterst welkom.</p>",
        "<p>De code van deze applicatie kan je terugvinden op  <a href=\"https://github.com/JorisVdBos/leningenvergelijker\">mijn github account</a>.</p>",
        "<p>Mijn LinkedIn:</p>",
        "<script src=\"//platform.linkedin.com/in.js\" type=\"text/javascript\"></script>
<script type=\"IN/MemberProfile\" data-id=\"https://www.linkedin.com/in/joris-van-den-bossche-8a12b943\" data-format=\"inline\" data-related=\"false\"></script>",
        "<p>Volg mij op Twitter:<br>
          <a href=\"https://twitter.com/Joris_VdB_\" class=\"twitter-follow-button\" data-show-count=\"false\" data-size=\"large\">Follow @Joris_VdB_</a> <script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document, 'script', 'twitter-wjs');</script></p>
"
      )))
    )
  )
)


ui <- dashboardPage(skin    = "green",
                    header  = dbHeader,
                    sidebar = sidebar,
                    body    = body)
