# Dados e modelo ####

load('data/xgboost.RData')
load('data/dados_fake.RData')
gisaid_data<-vroom::vroom("data/gisaid_hcov-19_2023_03_29_14.tsv")

# Cores 
paleta <- c('#0D4D4D', '#AA3939', '#2D882D', '#AA6C39', '#1D1D1D')
paleta2 <- c("#0D4D4DB3", "#AA3939B3", "#2D882DB3", "#AA6C39B3", "#1D1D1DB3")
paleta3 <- c("#0D4D4D66", "#AA393966", "#2D882D66", "#AA6C3966", "#1D1D1D66")

# função mapa de pacientes ####

mapa_pacientes <- function(df) {
    
    # query
    dt <- df %>%
        group_by(latitude, longitude,municipio) %>%
        summarise(n =n())
    
    # gráfico
    leaflet(dt) %>% 
        addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addCircleMarkers(
            radius = ~dt$n/2,
            fillOpacity = 0.7, stroke = F, 
            popup = paste0("<b>Município: </b>", dt$municipio,"<br>",
                           "<b>Pacientes: </b>", dt$n),
            label = ~ municipio,
            color = paleta[3]
        )
    
}


# Piramide pacientes faixa etária ####

pir_plot <- function(df) {
    
    df2 <- dados %>%
        mutate(faixa = factor(case_when(numAge < 21 ~ '< 21',
                                        numAge >= 21 & numAge <= 30 ~ '21 a 30',
                                        numAge >= 31 & numAge <= 40 ~ '31 a 40',
                                        numAge >= 41 & numAge <= 50 ~ '41 a 50',
                                        numAge >= 51 & numAge <= 60 ~ '51 a 60',
                                        numAge >= 61 & numAge <= 70 ~ '61 a 70',
                                        numAge >= 70 & numAge <= 80 ~ '71 a 80',
                                        numAge >= 81 ~ '>80'),
                              levels = c('>80','71 a 80', '61 a 70', '51 a 60',
                                         '41 a 50', '31 a 40', '21 a 30', '< 21')),
               
               sexo = ifelse(gender_M == 1, 'Masculino', 'Feminino')) %>%
        group_by(faixa, sexo) %>%
        count() %>%
        mutate(n = ifelse(sexo == "Feminino", n* -1, n))
    
    plot <- ggplot(df2, aes(x = faixa, y = n, fill = sexo)) +
        geom_bar(stat = "identity", alpha = 0.8, width = 0.95) +
        coord_flip() +
        scale_fill_manual(values = c('#AA3939', '#0D4D4D')) +
        theme_bw() +
        theme(panel.grid = element_blank(),
              axis.title.y = element_text(vjust = 2.5)) +
        scale_y_continuous(limits = function(x) {y = round(max(abs(x)) + (0.1*max(abs(x))))
        return(c(y*-1,y))}, labels = abs) +
        labs(x = 'Faixa etária', y = ' ',
             fill = " ")
    
    return(plot)
}


## gráfico de IMC ####

pizzaplot <- function(df, n, label) {
    
    df %>%
    plot_ly() %>% 
        add_pie(labels = ~ df[[label]], values = ~ df[[n]],
                hole = 0.5,
                marker = list(colors = paleta,
                              line = list(color = '#FFFFFF', width = 1)),
                domain = list(row = 0, column = 0)) %>% 
        layout(legend = list(orientation = 'h',
                             xanchor = "center",
                             x = 0.5))
    
}


## Violin plot customizado ####

vioplot2 <- function(df, var, limiar, etiqueta) {
    df %>% 
        ggplot(aes(y = df[[var]], x = '')) +
        geom_violin(trim = FALSE, fill = paleta2[1]) +
        geom_hline(yintercept = limiar, colour = 'darkred') +
        geom_text(aes(x = 1.5, y = limiar + (limiar/20)), label = 'Limiar') +
        theme(panel.grid = element_blank(),
              axis.title.y = element_text(vjust = 2.5),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank()) +
        labs(x = ' ', y = etiqueta)
}


## Barra de uma variável (frequência) ####

freq_plot <- function(df, var, limiar) {
    
    n = length(df[[var]][df[[var]]> limiar])
    porcentagem = round(n/nrow(df)*100, digits = 1)
    
    plot <- ggplot(data = NULL) +
        geom_col(aes(x = '', y = 100), fill = "lightgrey",
                 width = 2.8) +
        geom_col(aes(text = paste('pacientes:', n), x = '', y = porcentagem),
                 width = 2.8, fill = paleta[2]) +
        theme(panel.grid = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank())
    
    return(plot)
}


# barra de frequência com varias etiquetas #####
freq_plot2 <- function(df) {
    
    plot <- ggplot(df, aes(y = carateristica)) +
        geom_col(aes(x = 100), fill = "lightgrey",
                 width = 0.9) +
        geom_col(aes(text = paste('pacientes:', n), x = porcentagem),
                 width = 0.9, fill = paleta[1]) +
        theme(panel.grid = element_blank(),
              axis.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.title = element_blank())
    
    return(plot)
}


# gauge chart ####

gauge_plot <- function(value, title) {
    
    fig <- plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = value,
        title = list(text = title),
        type = "indicator",
        mode = "gauge+number",
        delta = list(reference = 50),
        gauge = list(
            axis = list(range = list(NULL, 100)),
            bar = list(color = paleta[2]),
            steps = list(list(range = c(0, 50), color = "lightgray"),
                         list(range = c(50, 100), color = "gray")
                         )
            )
        )
    fig <- fig %>%
        layout(margin = list(l=20,r=30))
    
    fig
    
}
