
# Define UI for application that draws a histogram
shinyUI(
    navbarPage(
    " ", 
    
    tabPanel(
        "Charts", 
        
        sidebarPanel(
            titlePanel("Main filters"), 
            
            selectInput(
                "market", "Choose a market:", 
                c("Santiago", 
                "Concepción", 
                "Valparaíso - Viña", 
                "Iquique - Alto Hospicio",
                "Coquimbo - La Serena",
                "Rancagua",
                "Puerto Montt",
                "Punta Arenas"),
                selected = "Coquimbo - La Serena"
            ), 
            
            checkboxGroupInput(
                "levels", "Choose some levels:", 
                choiceNames = list(
                    "PK", "K", "1B", "2B", "3B", "4B", "5B", "6B", "7B", "8B", 
                    "I M", "II M", "III M", "IV M"
                ), 
                choiceValues = list(
                    -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
                ), 
                selected = -1
            ), 
            
            actionButton("filterButton", "RUN FILTERS!")
        ), 
        
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "SIMCE",
                    
                    br(), 
                    
                    selectInput(
                        "simcePlot", "Which time series do you want?", 
                        choices = c("Both", "Difference"), 
                        selected = "Both"
                    ), 
                    
                    fluidRow(
                        
                        column(
                            4, 
                            selectInput(
                                "simceTest", "Choose a test:",
                                choices = c("Math", "Spanish"),
                                selected = "Math"
                                )
                            ), 
                        
                        column(
                            4,
                            selectInput(
                                "simceFun", "Choose a function:", 
                                choices = c("Mean", "Median"), 
                                selected = "Mean"
                            )
                        )
                    ),
                        
                    hr(), 
                    plotOutput("plotSimce")
                ), 
                
                tabPanel(
                    "Segregation", 
                    
                    br(), 
                    
                    selectInput(
                        "segregationIndex", "Choose a segregation Index:", 
                        choices = c("Duncan", "Hutchens"), 
                        selected = "Duncan"
                    ), 
                    
                    hr(), 
                    
                    plotOutput("plotSegregation")
                    
                )
            ), 
            fluidRow(
                tags$b("Using data from:"), 
                textOutput("counties")
            )
        )
    ), 
    

    
    tabPanel(
        "Technical notes", 
        
        titlePanel("Technical notes"), 
        
        tags$ul(
            tags$li("Our main data set is built using 4 sources of information:"), 
            tags$ol(
                tags$li(tags$em("Matricula por estudiante,"), "available at: ", 
                        tags$a("http://datos.mineduc.cl/dashboards/19776/descarga-bases-de-datos-de-matricula-por-estudiante/",
                               href = "http://datos.mineduc.cl/dashboards/19776/descarga-bases-de-datos-de-matricula-por-estudiante/")), 
                tags$li(tags$em("Prioritarios y preferentes,"), "available at: ", 
                        tags$a("http://datos.mineduc.cl/dashboards/19939/bases-de-datos-alumnos-prioritarios/",
                               href = "http://datos.mineduc.cl/dashboards/19939/bases-de-datos-alumnos-prioritarios/")),
                tags$li(tags$em("Oferta establecimientos SAE 2020,"), "available at: ", 
                        tags$a("http://datos.mineduc.cl/dashboards/20514/descarga-bases-de-datos-de-los-proceso-de-admision-escolar-anos-2016-y-2017/",
                               href = "http://datos.mineduc.cl/dashboards/20514/descarga-bases-de-datos-de-los-proceso-de-admision-escolar-anos-2016-y-2017/")),
                tags$li(tags$em("SIMCE 2016 Scores,"), "available at:" , 
                        tags$a("https://informacionestadistica.agenciaeducacion.cl/#/bases", 
                               href = "https://informacionestadistica.agenciaeducacion.cl/#/bases"))
                ), 
            
            br(), 
            tags$li("We only consider students enrolled at 'regular teaching' (the remainder of (cod_ense-10)/100 must be 0)."), 
            
            br(), 
            tags$li("We only consider schools that participated in SAE 2020 (can be found in at least one of SAE's files)."), 
            
            br(), 
            tags$li("For the SIMCE charts, we also restrict our set of schools to those having a valid SIMCE score during 2016 (noaplica == 0 or noaplica == 'Rbd y curso rinde Simce').")
            )
        )
    )
)
