# Dashbaard Header element
header <- dashboardHeader(
    title = shinyDashboardLogoDIY(
        boldText = "Yale SARS-CoV-2 Genomic Surveillance Initiative",
        mainText = "",
        textSize = 22,
        badgeText = " ",
        badgeTextColor = "white",
        badgeTextSize = 5,
        badgeBackColor = "# ",
        badgeBorderRadius = 6),
    disable = FALSE, 
    titleWidth  = 450
)

# Sidebar Panel ####
siderbar <- dashboardSidebar(
    width = 240,
    collapsed = FALSE, 
    
    sidebarMenu(
        
        tags$br(),
        
        # Project Management
        menuItem("Variant Frequencies", 
                 tabName = "painel_1", 
                 icon = icon("users"),
                 selected = TRUE),
        
        menuItem("Eff. Reproduction Number \n (Rt)", 
                 tabName = "painel_2", 
                 icon = icon("file-medical"),
                 selected = FALSE),
        
        menuItem("Infections Estimates per Variant", 
                 tabName = "painel_3", 
                 icon = icon("chart-line"),
                 selected = FALSE)
        
        # menuItem("Inserir paciente", 
        #          tabName = "painel_4", 
        #          icon = icon("user"),
        #          selected = FALSE)
    )
    
)

# Dashboard body #####
body <- dashboardBody(
    
    shinyDashboardThemes("grey_light")
    
    ,shinyjs::useShinyjs()
    
    # Fixing the top bar
    ,shinyjs::useShinyjs()
    ,shinyjs::inlineCSS("body > div > header > nav > a {visibility: hidden}"),
    
    # Parsing the pattern to the value box
    tags$style(
        type = 'text/css' 
        ,'.bg-green {background-color: #2D882D!important; }'
    ),
    
    tags$style(
        type = 'text/css' 
        ,'.bg-black {background-color: #1D1D1DB3!important; }'
    ),
    
    
    tags$style(
        type = 'text/css' 
        ,'.bg-blue {background-color: #0D4D4D!important; }'
    ),
    
    tags$style(
        type = 'text/css' 
        ,'.bg-red {background-color: #AA3939!important; }'
    ),
    
    #  Items generator
    tabItems(
        
        # Variants Frequencies ####
        tabItem(tabName = "painel_1"
                
                ,tags$br()
                
                ,h2(strong('Variant Frequencies') 
                    ,align = 'center')
                
                ,tags$br(), tags$br()
                
                # Cards ####
                ,fluidRow(
                    column(width = 4, infoBoxOutput('nsequences', width = 12))
                )
                
                ,tags$hr()
                
                ,box(width = 12
                     ,id = 'variantcounts'
                     ,height = '550px'
                     ,title = 'Variants Frequencies in CT, USA'
                     
                     ,tags$br()
                     
                     ,fluidRow(
                         column(width = 12
                                ,plotlyOutput("variant"
                                              ,height = "500px")
                                )
                     )
                )
                
                #,box(width = 7, id = 'map', height = '550px',
                #     title = 'Distribuição geográfica dos pacientes',
                #     
                #     tags$br(),
                #     
                #     fluidRow(
                #         column(width = 12,
                #                leafletOutput("mapa", height = "500px"))
                #     )
                # )
                
        ),
        
        # Acompanhamento clínico dos pacientes ####
        tabItem(tabName = "painel_2",
                
                tags$br(),
                
                h2(strong('Indicadores clínicos'), align = 'center'),
                
                tags$br(), tags$br(),
                
                fluidRow(
                    # IMC plot 
                    box(width = 6, id = 'imc', height = '500px',
                        title = 'Índice de Massa Corporal',
                        
                        fluidRow(
                            column(width = 12, plotlyOutput('imcplot', height = '500px'))
                        )),
                    
                    # fumantes e diabeticos
                    box(width = 6, id = 'fumantes', height = '500px',
                        title = 'Proporção de pacientes Fumantes e com Diabetes tipo 2',
                        
                        fluidRow(
                            column(width = 12, plotlyOutput('fumdiab', height = '500px'))
                        ))
                ),
                
                tags$hr(),
                
                fluidRow(
                    # Pressão alta 
                    box(width = 6, id = 'pressao', height = '500px',
                        title = 'Pressão sistólica: distribuição de valores e 
                    porcentagem de pacientes acima do limiar (140 mmHg)',
                    
                    fluidRow(
                        column(width = 8, plotlyOutput('viopresis', height = '450px')),
                        column(width = 4, plotlyOutput('freqpresis', height = '450px'))
                    )),
                    
                    # fumantes e diabeticos
                    box(width = 6, id = 'colesterol', height = '500px',
                        title = 'Colesterol total: distribuição de valores e 
                    porcentagem de pacientes acima do limiar (190 mg/dL)',
                    
                    fluidRow(
                        column(width = 8, plotlyOutput('viocolest', height = '450px')),
                        column(width = 4, plotlyOutput('freqcolest', height = '450px')),
                    ))
                )
        ),
        
        # Modelo preditivo ####
        tabItem(tabName = "painel_3",
                
                tags$br(),
                
                h2(strong('Algoritmo de predição de risco cardiovascular'), 
                   align = 'center'),
                
                tags$br(), tags$br(),
                
                fluidRow(
                    column(width = 5,
                           selectInput(inputId = "paciente",
                                       label = tags$p('Selecione o(a) paciente que deseja consultar',
                                                      style = 'font-size: 130%;'),
                                       choices =  unique(dados$paciente),
                                       width = '400px',
                                       selected = NULL,
                                       multiple = FALSE)),
                ),
                
                tags$hr(),
                
                # Cards ####
                fluidRow(
                    column(width = 3, infoBoxOutput('pac_sex', width = 12)),
                    column(width = 3, infoBoxOutput('pac_idade', width = 12)),
                    column(width = 3, infoBoxOutput('pac_fumante', width = 12)),
                    column(width = 3, infoBoxOutput('pac_diabetes', width = 12))
                ),
                
                fluidRow(
                    column(width = 3, infoBoxOutput('pac_imc', width = 12)),
                    column(width = 3, infoBoxOutput('pac_pressao', width = 12)),
                    column(width = 3, infoBoxOutput('pac_colesterol', width = 12))
                ),
                
                tags$hr(), tags$br(),
                
                fluidRow(
                    box(width = 12, id = 'pressao', height = '400px',
                        title = 'Previsão do algoritmo',
                        
                        fluidRow(
                            column(width = 4,
                                   tags$br(), tags$br(), tags$br(), tags$br(),
                                   tags$br(), tags$br(),
                                   
                                   h4('O algoritmo prevê a chance do paciente dessenvolver 
                                      uma doença cardiovascular nos próximos 6 meses.'
                                   ),
                                   tags$br(),
                                   
                                   h4('O objetivo desse algoritmo é auxiliar na priorização do 
                                      atendimento dos pacientes, portanto, é uma ferramenta 
                                      de apoio e não substitui o critério clínico do 
                                      médico especialista.')
                            ),
                            column(width = 7, offset = 1,
                                   h3('Probabilidade (%) de desenvolver doença cardiovascular em 6 meses',
                                      align = 'center'),
                                   
                                   plotlyOutput('paciente_pred', height = '350px'))
                        )
                        
                    )
                )
        ),
        
        # Modelo preditivo 2 Inserir paciente ####
        tabItem(tabName = "painel_4",
                
                tags$br(),
                
                h2(strong('Algoritmo de predição de risco cardiovascular: pacientes novos'), 
                   align = 'center'),
                
                h3('Insira os dados do paciente que deseja consultar',
                   align = 'center'),
                
                tags$br(), tags$br(),
                
                fluidRow(
                    column(width = 2,
                           selectInput(inputId = "sexo",
                                       label = 'Sexo biológico',
                                       choices =  c("Feminino", "Masculino"),
                                       width = NULL,
                                       selected = NULL,
                                       multiple = FALSE)
                    ),
                    column(width = 2,
                           selectInput(inputId = "fuma",
                                       label = ' É fumante?',
                                       choices =  c("Não", "Sim"),
                                       width = NULL,
                                       selected = NULL,
                                       multiple = FALSE)
                    ),
                    column(width = 2,
                           selectInput(inputId = "diab",
                                       label = 'Tem diabetes tipo II?',
                                       choices =  c("Não", "Sim"),
                                       width = NULL,
                                       selected = NULL,
                                       multiple = FALSE)
                    ),
                    column(width = 4,
                           sliderInput(inputId = "idade", 
                                       label = "Idade (anos)", 
                                       value = 0, 
                                       min = 0, 
                                       max = 100,
                                       width = '100%')
                    )
                ),
                
                tags$hr(),
                
                fluidRow(
                    column(width = 4,
                           sliderInput(inputId = "imc2", 
                                       label = "I.M.C.", 
                                       value = 24, 
                                       min = 10, 
                                       max = 40,
                                       width = '100%')
                    ),
                    column(width = 4,
                           sliderInput(inputId = "press", 
                                       label = "Pressão Arterial Sistólica (mmHg)", 
                                       value = 110, 
                                       min = 70, 
                                       max = 250,
                                       width = '100%')
                    ),
                    column(width = 4,
                           sliderInput(inputId = "col", 
                                       label = "Colesterol total (mg/dL)", 
                                       value = 110, 
                                       min = 70, 
                                       max = 250,
                                       width = '100%')
                    )
                ),
                
                tags$hr(), tags$br(),
                
                fluidRow(
                    box(width = 12, id = '', height = '400px',
                        title = 'Previsão do algoritmo',
                        
                        fluidRow(
                            column(width = 4,
                                   tags$br(), tags$br(), tags$br(), tags$br(),
                                   tags$br(), tags$br(),
                                   
                                   h4('O algoritmo prevê a chance do paciente dessenvolver 
                                      uma doença cardiovascular nos próximos 6 meses.'
                                   ),
                                   tags$br(),
                                   
                                   h4('O objetivo desse algoritmo é auxiliar na priorização do 
                                      atendimento dos pacientes, portanto, é uma ferramenta 
                                      de apoio e não substitui o critério clínico do 
                                      médico especialista.')
                            ),
                            column(width = 7, offset = 1,
                                   h3('Probabilidade (%) de desenvolver doença cardiovascular em 6 meses',
                                      align = 'center'),
                                   
                                   plotlyOutput('paciente_pred2', height = '350px'))
                        )
                        
                    )
                )
        )
    )
)    


### Integração ####
ui <-  dashboardPage(header, siderbar, body)
