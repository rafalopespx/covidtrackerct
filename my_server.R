# Theme to plot ####
theme_set(theme_bw())

server <- function(input, output) {
    
    # Project Management #####
    
    # Cards #
    
    output$nsequences <- renderInfoBox({
        
        infoBox(
            tags$p("Number of sequences"
                   ,style = 'font-size: 130%;')
            ,tags$p(nrow(gisaid_data)
                    ,style = 'font-size: 130%;')
            ,icon = icon("users")
            ,color = "black"
        )
    })
    
    # Variant Frequency time series #
    
    output$variantcounts <- renderPlotly({
        ggplotly(gisaid_data %>% pir_plot()) %>% 
            layout(legend = list(orientation = 'h',
                                 xanchor = "center",
                                 x = 0.5))
    })
    
    # mapa #
    output$mapa <- renderLeaflet({
        dados %>%
            mapa_pacientes()
    })

    # Monitoramento clínico #####
    
    # IMC ###
    output$imcplot <- renderPlotly({
        dados %>% 
            mutate(imc = case_when(bmi < 18.5 ~ 'Magreza',
                                   bmi >= 18.5 & bmi < 25 ~ 'Normal',
                                   bmi >= 25 & bmi < 29.9 ~ 'Sobrepeso',
                                   bmi >= 29.9 ~ 'Obesidade')) %>%
            group_by(imc) %>%
            summarise(count = n()) %>%
            pizzaplot(label = 'imc', n = 'count')
    })
    
    output$fumdiab <- renderPlotly({
        dados %>%
            rename(Fumante = smoking_Y,
                   `Diabetes tipo 2` = t2d_Y) %>%
            pivot_longer(Fumante:`Diabetes tipo 2`, names_to = 'carateristica') %>%
            group_by(carateristica) %>%
            summarize(n = sum(value == 1),
                      porcentagem = round(sum(value == 1)/n()*100,1)) %>%
            freq_plot2()
    })
    
    # pressão ###
    
    output$viopresis <- renderPlotly({
        
        dados %>%
            vioplot2(var = 'sbp', limiar = 140, 
                     etiqueta = 'Pressão sistólica (mmHg)')
    })
    
    output$freqpresis <- renderPlotly({
        dados %>% 
            freq_plot(var = 'sbp', limiar = 140)
    })
    
    # colesterol ###
    
    output$viocolest <- renderPlotly({
        
        dados %>%
            vioplot2(var = 'tchol', limiar = 190, 
                     etiqueta = 'Colesterol total (mg/dL)')
    })
    
    output$freqcolest <- renderPlotly({
        dados %>% 
            freq_plot(var = 'tchol', limiar = 190)
    })
    
    # Modelo preditivo ######
    
    # separar os dados do paciente #
    
    dados_pac <- reactive({
        dados %>%
            filter(paciente == input$paciente) %>%
            select(numAge:gender_M)
    })
    
    # Cards dados do paciente ##
    
    output$pac_sex <- renderInfoBox({
        
        infoBox(
            tags$p("Sexo", style = 'font-size: 110%;'),
            tags$p(ifelse(dados_pac()$gender_M == 1, 'Masculino', 'Feminino'), 
                   style = 'font-size: 120%;'),
            icon = icon("venus-mars"), color = "green"
        )
    })
    
    output$pac_idade <- renderInfoBox({
        
        infoBox(
            tags$p("Idade", style = 'font-size: 110%;'),
            tags$p(paste(dados_pac()$numAge, 'anos'), 
                   style = 'font-size: 120%;'),
            icon = icon("calendar"), color = "red"
        )
    })
    
    output$pac_fumante <- renderInfoBox({
        
        infoBox(
            tags$p("Fumante?", style = 'font-size: 110%;'),
            tags$p(ifelse(dados_pac()$smoking_Y == 1, 'Sim', 'Não'), 
                   style = 'font-size: 120%;'),
            icon = icon("smoking"), color = "green"
        )
    })
    
    
    output$pac_diabetes <- renderInfoBox({
        
        infoBox(
            tags$p("Diabetes tipo 2?", style = 'font-size: 110%;'),
            tags$p(ifelse(dados_pac()$t2d_Y == 1, 'Sim', 'Não'), 
                   style = 'font-size: 120%;'),
            icon = icon("vial"), color = "red"
        )
    })
    
    output$pac_imc <- renderInfoBox({
        
        infoBox(
            tags$p("IMC", style = 'font-size: 100%;'),
            tags$p(paste(dados_pac()$bmi, '|', 
                         
                             case_when(dados_pac()$bmi < 18.5 ~ 'Magreza',
                                       dados_pac()$bmi >= 18.5 & dados_pac()$bmi < 25 ~ 'Normal',
                                       dados_pac()$bmi >= 25 & dados_pac()$bmi < 29.9 ~ 'Sobrepeso',
                                       dados_pac()$bmi >= 29.9 ~ 'Obesidade')
                         ), 
                   style = 'font-size: 110%;'),
            icon = icon("cloudscale"), color = "green"
        )
    })
    
    output$pac_pressao <- renderInfoBox({
        
        infoBox(
            tags$p("Pressão sistólica", style = 'font-size: 100%;'),
            tags$p(paste(dados_pac()$sbp, "|",
                         ifelse(dados_pac()$sbp > 140, 
                                'Alta', 'Normal')), 
                   style = 'font-size: 110%;'),
            icon = icon("heart"), color = "red"
        )
    })
    
    output$pac_colesterol <- renderInfoBox({
        
        infoBox(
            tags$p("Colesterol total", style = 'font-size: 100%;'),
            tags$p(paste(dados_pac()$tchol, "|",
                         ifelse(dados_pac()$tchol > 190, 
                                'Alto', 'Normal')), 
                   style = 'font-size: 110%;'),
            icon = icon("syringe"), color = "green"
        )
    })
    
    output$paciente_pred <- renderPlotly({
        
        dt <- dados_pac() %>% 
            as.matrix() %>% 
            xgboost::xgb.DMatrix(data = .)
        
        pred <- round(predict(xgb_mod1, dt)*100,1)
        
        fig <- gauge_plot(pred, ' ')
        
        return(fig)
        
    })
    
    
    pac_novo <- reactive({
        data.frame(
            'numAge' = as.numeric(input$idade),
            'bmi' = as.numeric(input$imc2),
            'tchol' = as.numeric(input$col),
            'sbp' = as.numeric(input$press),
            'smoking_Y' = as.numeric(ifelse(input$fuma == 'Sim', 1, 0)),
            't2d_Y' = as.numeric(ifelse(input$diab == 'Sim', 1, 0)),
            'gender_M' = as.numeric(ifelse(input$sexo == 'Masculino', 1, 0))
        )
    })
    
    output$paciente_pred2 <- renderPlotly({
        
        dt <- pac_novo() %>% 
            as.matrix() %>% 
            xgboost::xgb.DMatrix(data = .)
        
        pred <- round(predict(xgb_mod1, dt)*100,1)
        
        fig <- gauge_plot(pred, ' ')
        
        return(fig)
        
    })
    
}