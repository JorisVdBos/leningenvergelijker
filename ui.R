# Header ----
dbHeader <- dashboardHeader(title = "Leningen vergelijker")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text     = "Inleiding", 
             tabName  = "inleiding", 
             icon     = icon("home")),
    menuItem(text     = "Simuleer lening", 
             tabName  = "simLen", 
             icon     = icon("calculator")),
    menuItem(text     = "Vergelijk leningen", 
             tabName  = "vergLen", 
             icon     = icon("line-chart")),
    menuItem(text     = "Meer informatie", 
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
    tabItem(
      tabName = "inleiding",
      h1("Welkom!"),
      p("Welkom op mijn applicatie! Als je hier bent, wil dat zeggen dat je een lening wil aangaan bij de bank. Net als jij, wou ik de beste lening op de markt te pakken krijgen en samen met mijn partner hebben we verschillende banken afgeschuimd naar informatie. Om de leningen van de verschillende banken te vergelijken, begonnen wij een excelbestand waar we alle leningvoorstellen verzamelden. Zelfs met enkele excel truukjes kregen we moeilijk vat op de waarde van de verschillende voorstellen en moesten we toch nog vaak teruggrijpen naar de aflostabellen van de banken. Daarom schreef ik voor mezelf en mijn partner een applicatie in de computertaal 'R' die leningen simuleerde. Zo kon ik ook vragen beantwoorden in verband met inflatie, verzekeringskosten en beleggen."),
      p("Om te beginnen, ga naar het tabblad 'Simuleer lening' om leningen toe te voegen en aflostabellen te simuleren. Om de verschillende leningen naast elkaar te vergelijken, ga naar het tabblad 'Vergelijk leningen'. Om meer over deze applicatie te weten te komen, ga naar het tabblad 'Meer informatie'. Veel succes met de zoektocht naar je perfecte lening!")
    ),
    # Lening simulatie ----
    tabItem(
      tabName = "simLen",
      h1("Simuleer een lening"),
      #p("Een huis of appartement gekocht? Proficiat! Maar hola, de zoektocht is nog niet afgelopen! Een goede lening vinden kan je duizenden euro's besparen, dus een nieuwe zoektocht gaat van start. Algouw ligt je keukentafel vol met papieren met letterlijk duizenden cijfertjes. Bank A geeft een betere rentevoet, maarja bank B heeft dan weer goedkopere verzekeringen! Economisch gezien moet je denken aan inflatie en zo weinig mogelijk lenen, maar fiscaal gezien moet je dan weer zo lang mogelijk lenen. Vriend 1 zegt dit en vriend 2 zegt dat, maar welke lening is nu de beste?"),
      h2("Voorbeeld"),
      p("Onderaan zie je al drie leningen als voorbeeld ingevuld. Zij stemmen overeen met het volgende volledig fictieve voorbeeld:"),
      p("Tine heeft een appartement gekocht en wil een lening van 150.000 euro aangaan over 25 jaar. Ze verdient 1500 euro netto per maand en houdt 800 euro per maand over voor de lening en om te sparen. Na de aankoop van het huis heeft ze nog 3.000 euro aan spaargeld over, wat ze voor 65% in een beleggingsportefeille houdt. De laatste jaren brachten haar beleggingen haar een gemiddelde rente van 5% per jaar op."),
      p("In bank 1 wordt haar een lening op vaste rentevoet aangeboden aan 2,5%. In de bank 2 raden ze een lening aan 1,9% aan op variabele rentevoet, met herziening om de drie jaar. Hun verzekeringen zijn goedkoper dan bank 1. Bank 3 is duurder dan de andere twee banken, maar zij hebben lagere dossierkosten. Zij raden een gecombineerde lening aan, van 100.000 euro aan 2,5% vast over 25 jaar en 50.000 euro variabel aan 1,9%, herzien om de drie jaar over 15 jaar."),
      p("Selecteer een lening in de tabel en klik op de knop 'Start simulatie' om de aflostabel van de lening te bekijken. Bekijk ook zeker de grafieken onder het tabblad 'Grafiek'. Als je een vergelijking van de drie grafieken wil bekijken, ga dan naar 'vergelijk leningen'. Dit kan je terugvinden door op de drie streepjes te klikken bovenaan de pagina."),
      h2("Zelf aan de slag"),
      p("In de tab 'nieuwe lening' kan je zelf leningen aan deze tabel toevoegen. Alle informatie ingegeven in deze website wordt niet opgeslagen. Om je gegevens te bewaren, kan je je bankvoorstellen exporteren en later opnieuw inladen. (Opgepast: Als je de pagina ververst, worden al je gegevens gewist!)"),
      # Invoer simulator ----
      tabsetPanel(
        tabPanel(
          "Opgeslagen leningen",
          wellPanel(
            p("In deze tabel vind je alle informatie over de leningen terug. Van links naar rechts vind je:"), 
            HTML("<ul>
<li>Het te lenen bedrag</li>
<li>Het type lening: Vast of variabel</li>
<li>Indien variabel hoeveel jaar tot herziening</li>
<li>De rentevoet</li>
<li>looptijd van de lening</li>
<li>Eenmalige kosten van de lening, zoals de dossierkosten</li>
<li>Maandelijkse kosten zijn de kosten van de rekeningen, bankkaarten en ook maandelijkse verzekeringen zoals bijvoorbeeld de schuldsaldoverzekering</li>
<li>Jaarlijkse kosten zijn bijvoorbeeld de brandverzekering</li>
<li>De inflatie. In België was deze 1,97 % en 2,20 % respectievelijk in 2016 en 2017</li>
<li>Je vermogen bij de start van de lening (na de aankoop van je huis.)</li>
<li>Je maandelijks inkomsten min de vaste kosten is het bedrag dat je overhoudt na het aftrekken van je vaste kosten zoals eten en elektriciteit van je maandelijkse loon. Dit is het bedrag dat je zal gebruiken om je lening af te betalen en de extra kosten te bekostigen. Het overschot van dit bedrag wordt gespaard en eventueel belegd.</li>
<li>Hoeveel procent van je spaarpot je in beleggingen zal steken</li>
<li>Hoeveel deze beleggingen zullen opbrengen. Er wordt aangenomen dat geld op de spaarrekening niets opbrengt!</li></ul>"),
            br(),
            br(),
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
                    p(em(HTML("<font color='red'>Voeg een lening toe met de knop 'Voeg toe'. 
                              Op deze manier kan je je lening opdelen in verschillende delen!</font>"))))
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
                            placeholder = "5000"),
                  div(id = "lenVermStartError",
                      p(em(HTML("<font color='red'>Gelieve een correct getal in te geven.</font>")))),
                  textInput("lenVermInk", 
                            "Beschikbaar maandelijks bedrag na vaste kosten:", 
                            placeholder = "800"),
                  p(paste0("Voorbeeld: Je verdient 1500 euro netto. Na je vaste kosten zoals electriciteit, eten ",
                    "en andere diverse maandelijkse kosten, blijft er nog 800 euro over voor je lening en te sparen. ",
                    "Als de gesimuleerde lening een afbetaling van 600 euro uitkomt, zal het de overige 200 euro ",
                    "gerekend worden als spaargeld. Hieronder kan je nog specifieren of je dit bedrag belegt of niet.")),
                  div(id = "lenVermInkError",
                      p(em(HTML("<font color='red'>Gelieve een correct getal in te geven.</font>")))),
                  textInput("lenVermBelPerc", 
                            "Percentage van gespaard vermogen in beleggingen:", 
                            value = 0,
                            placeholder = "45"),
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
                checkboxInput("grafiekCumulatief", "Per maand")
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
      p("Op deze pagina worden al je ingegeven leningen naast elkaar gelegd en vergeleken. Wij vonden de grafiek die het vermogen weergeeft doorheen de jaren de meest doorslaggevende. Dit is namelijk het geld dat je in je handen overhoudt doorheen de jaren. Voor Tine lijkt in dat geval Bank 3 de beste keuze. Omdat ze in het begin meer afbetaalt, zal ze naar het einde van de 25 jaren veel kunnen sparen en uiteindelijk veel meer overhouden. Voor we deze colclusie kunnen trekken moeten we echter even stilstaan bij de aannames die bij de simulatie horen."),
      h2("Belangrijke opmerkingen"),
      p("De simulatie gaat er van uit dat het beschikbare bedrag er zal zijn doorheen de looptijd van de lening en dat het gespaarde geld nooit wordt aangesproken. We nemen zelfs aan dat je maandelijks bedrag meegroeit met de inflatie! Dit kan niet altijd het geval zijn, door ziekte, veranderen van werk, etc. kunnen maandelijkse inkomsten plots veranderen in goede of slechte zin. Ook kan het zijn dat het spaargeld wordt aangesproken voor een vakantie of dure aankoop. In onderstaande grafiek is het duidelijk dat Tine de eerste 15 jaar zeer weinig zal kunnen spenderen aan andere dingen dan aan de lening. Dit is een risico waar zeker aandacht aan gespendeerd moet worden. Om het risico te verkleinen zou ze nieuwe simulatie kunnen maken waarin het aandeel in de lening op 15 jaar kleiner is, wat het risico zou verkleinen."),
      p("Een tweede aanname is dat het beleggingsopbrengstpercentage en de inflatie hetzelfde zal blijven gedurende de looptijd van de lening. Een slechte belegging kan hierdoor roet in het eten gooien. Ook hier is het aan te raden om een appel voor de dorst achter te houden om het risico te verkleinen."),
      p("De variable rentevoeten worden aangenomen steeds naar het maximum te stijgen na de eerste herziening (banken noemen dit het 'worst-case scenario'). In de tijd van dit schrijven, in 2017, was dit de verwachting. In de vermogen grafiek van Tine zie je de aanpassing van de rentevoet in de 'knik' in de grafiek na drie jaar. (Deze is ook aanwezig in het voorstel van Bank 3, maar niet zo goed zichtbaar.) Het is onwaarschijnlijk, maar wel mogelijk dat de rentevoeten toch laag blijven, en Tine de volledige looptijd aan een lagere rentevoet terugbetaalt en dus beter uitkomt dan in de simulatie berekend werd."),
      p("Een laatste aanname die de simulatie maakt is dat er geen vervroegde leningsafbetalingen gebeuren tijdens de looptijd. Vergeet niet dat je op eender welk moment een deel van je lening vervroegd kan aflossen. Soms kan het voordelig zijn je lening af te betalen."),
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
          dataTableOutput("vergLenOutputDT"),
          downloadButton(
            "vergLenAflossingstabelExport",
            "Exporteer vergelijkingstabel (.csv)"
          )
        ),
        wellPanel(
          uiOutput("vergGrafiekKolommenUI"),
          uiOutput("vergGrafiekStartDatumUI"), 
          checkboxInput("vergGrafiekInflatie", "Inflatie inrekenen"),
          sliderInput("vergGrafiekInflatiePerc", "Inflatie percentage:", 
                      min = -10, max = 10, value = 2, step = 0.1),
          checkboxInput("vergGrafiekCumulatief", "Per maand")
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
    ),
    tabItem(
      tabName = "meerInfo",
      h1("Meer informatie over deze applicatie"),
      fluidPage(
        fluidRow(HTML(paste0(
          "<p>In de zomer van 2017 bouwde ik deze applicatie, geïnspireerd door mijn eigen zoektocht naar een lening. Alle vragen, aanbevelingen of opmerkingen over deze applicatie zijn uiterst welkom. Contacteer mij via <a href=\"mailto:joris.bdbossche@gmail.com\">mijn email</a> of via onderstaande kanalen.</p>",
          "<p>Alle code van deze applicatie is terug te vinden op <a href=\"https://github.com/JorisVdBos/leningenvergelijker\">mijn github account</a>.</p>",
          "<p>Mijn LinkedIn account:</p>",
          "<br>",
          "<script src=\"//platform.linkedin.com/in.js\" type=\"text/javascript\"></script>
          <script type=\"IN/MemberProfile\" data-id=\"https://www.linkedin.com/in/joris-van-den-bossche-8a12b943\" data-format=\"inline\" data-related=\"false\"></script>",
          "<br>",
          "<p>Volg mij op Twitter!<br>
          <a href=\"https://twitter.com/Joris_VdB_\" class=\"twitter-follow-button\" data-show-count=\"false\" data-size=\"large\">Follow @Joris_VdB_</a> <script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document, 'script', 'twitter-wjs');</script></p>
          "
        )))
      )
    )
  )
)


ui <- dashboardPage(skin    = "green",
                    header  = dbHeader,
                    sidebar = sidebar,
                    body    = body)
