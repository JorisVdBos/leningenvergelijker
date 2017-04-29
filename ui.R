dbHeader <- dashboardHeader(title = "Leningen vergelijker")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text     = "Simuleer lening", 
             tabName  = "simLen", 
             icon     = icon("play")),
    menuItem(text     = "Vergelijk leningen", 
             tabName  = "vergLen", 
             icon     = icon("calculator")),
    menuItem(text     = "Meer info", 
             tabName  = "meerInfo", 
             icon     = icon("question"))
  )
)

body <- dashboardBody(
  shinyjs::useShinyjs(),
  tabItems(
    tabItem(
      tabName = "simLen",
      h1("Simuleer een lening")
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


ui <- dashboardPage(skin    = "blue",
                    header  = dbHeader,
                    sidebar = sidebar,
                    body    = body)
