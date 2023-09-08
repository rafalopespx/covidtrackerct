#### Variant Report for SARS-CoV-2 ####
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#initial commit date: 06/25/22
#original author: Kien Pham
#email k.pham@yale.edu or duckien242@gmail.com
#to be updated Tuesday afternoons

########################################################

# I. PACKAGE INSTALL ####
packages = c("dplyr","tidyr","ggplot2","RColorBrewer","shiny",
             "shinythemes","plotly","DT","data.table",
             "stats","ggpubr","stringr", "MetBrewer")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

rm(packages, package.check)

library(gsheet)
library(purrr)
library(readr)
library(DescTools)
library(lubridate)
library(zoo)
library(EpiEstim)
library(ggpubr)
library(httr)
library(jsonlite)

set.seed(1234)

########################################################
##### Sourcing pre-processed files ####################
source("auxiliar_processing.R")

########################################################
##### SET RECENT DATES (CHANGE FOR WEEKLY BUILD) #######

# date_gisaid = as.Date("2023-08-22") # FOR FILTERING GISAID DOWNLOAD, REFERENCE THIS DATE
# date_3_months = as.Date(date_gisaid - 92) # CHANGE TO MOST RECENT 3 MONTHS FOR WHO VARIANT PLOT
# date_3_weeks = as.Date(date_gisaid - 22) # CHANGE TO MOST RECENT 3 WEEKS FOR LINEAGE PLOT
# date_covidestim<-"2023-05-04" # CHANGE TO MOST RECENT DATE, FIXED UNTIL COVIDESTIM IS UP AGAIN

################# DATA CHECK LIST ######################

# 1. full metadata file (in data folder, or download from gisaid & change to correct template in data folder, CODE TBA)
# 2. cumulative case data (download from John Hopkins github: https://raw.githubusercontent.com/CSSEGISandData/SARS-CoV-2/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv)
### NOTE: AFTER MAR 9, 2023, JHU STOPPED UPDATING THEIR COVID DATA. We are now using NYTimes' github, which combines JHU data with federal case count. https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv summary table: variant surveillance for Connecticut (template in data or backup folder)
# 4. lab metadata file (for Ct value graph)
# 5. variant name call file: to be updated weekly following pango releases: https://github.com/cov-lineages/pango-designation/

################# STEPS #########################
# 1. update GISAID date in server.R & ui.R
# 2. download latest week's Connecticut metadata from GISAID based on GISAID date, 
# search Location = Connecticut & Collection date = GISAID date, then download Patient metadata
# 3. download lineage designation from pangolin github, manually add latest (Omicron) lineages to variant_names.tsv file, with proper WHO variant designation
# 4. download latest GLab metadata sheet from Google Drive
# 5. change Table_new.csv to Table.csv, delete old Table.csv
# 6. Run App, check for any errors

########################################################
######### START SHINY APP SERVER #########
shinyServer(function(input,output){
  
  ######## X. PLOTTING ##############
  
  # 1. Daily lineage freq in last 3 months
  
  output$lin_freq <- renderPlotly({
    
    if (input$button == "3-day rolling average") {
      plot_ly(lineages_daily_sorted, 
              x = ~`Collection.date`, 
              y = ~`movavg`, 
              color = ~`pango_lineage`, 
              colors = lineages_sum$color,
              type = "scatter", 
              mode = "lines")|>  
        add_trace() |> 
        layout(title = list(title = "Lineage frequencies in Connecticut (last 3 months): 3-day rolling average"),
               yaxis = list(title = "Lineage frequency (%)"), 
               xaxis = list(title = "Time (days)"))
      
    } else {
      
      plot_ly(lineages_daily_sorted, 
              x = ~`Collection.date`, 
              y = ~freq, 
              color = ~`pango_lineage`, 
              colors = lineages_sum$color,
              type = "scatter", 
              mode = "lines")|>  
        add_trace() |> 
        layout(title = list(title = "Daily lineage frequencies in Connecticut (last 3 months)"),
               yaxis = list(title = "Lineage frequency (%)"), 
               xaxis = list(title = "Time (days)"))
    }
    
  })
  
  # 2. Weekly variant freq since 1-2021
  
  output$var_freq <- renderPlotly({
    
    if (input$button_who == "Line chart display") {
      who_weekly |> 
        filter(freq > 0.02,
               week >= "2023-01-01") |> 
        droplevels() |> 
        ungroup() |> 
        plot_ly(x = ~`week`, 
                y = ~ freq, 
                color = ~`Variant names`, 
                colors = c(MetBrewer::met.brewer("Signac", 
                                                 direction = 1,
                                                 length(unique(who_weekly$`Variant names`)), 
                                                 type = "continuous")),
                type = 'scatter' ,
                mode = 'lines')|>  
        layout(title = list(title = "Weekly variant frequencies in Connecticut (since January 2021)"),
               yaxis = list(title = "Lineage frequency (%)"), 
               xaxis = list(title = "Time (weeks)"))
    } else {
      who_weekly |> 
        filter(freq > 0.02,
               week >= "2023-01-01") |> 
        droplevels() |> 
        ungroup() |> 
        plot_ly(x = ~`week`, 
                y = ~freq, 
                color = ~`Variant names`, 
                colors = c(MetBrewer::met.brewer("Signac", 
                                                 direction = 1,
                                                 length(unique(who_weekly$`Variant names`)), 
                                                 type = "continuous")),
                type = "bar")|>  
        layout(title = list(title = "Weekly variant frequencies in Connecticut (since January 2021)"),
               yaxis = list(title = "Lineage frequency (%)"), 
               xaxis = list(title = "Time (weeks)"), 
               barmode = "stack")
    }
    
  })
  
  # 2.a. Rt values by variant
  
  
  who_colors_rt = c(
    "Delta" = "#11adf6",
    "Omicron (BA.1)" = "#2b931a",
    "Omicron (BA.2)" = "#fdb902",
    "Omicron (BA.4)" = "#f47110", 
    "Omicron (XBB)" = "#333333", 
    "Omicron (BA.5)" = "#a90505"
  )
  
  
  RtData <- eventReactive(input$Go,{
    
    #*******************************************************************************
    #RT FUNCTION ####
    #*#******************************************************************************
    
    #Rt Calculation
    
    source("functions/rt_fun.R")
    
    # make rt calculation into reactive one (for loading speed's sake)
    
    #run estimate_R function on
    rt <- lapply(var_list, 
                 safely(rt_fun))
    
    rt_comb <- lapply(1:2, 
                      function(ind) do.call(rbind, 
                                            lapply(rt, 
                                                   `[[`, 
                                                   ind)))
    
    rt_comb <- rt_comb[[1]] %>%
      mutate_all(~gsub("_infections", "", .)) 
    
    rt_comb <- rt_comb %>%
      mutate(days = as.Date(days)) %>%
      left_join(var_merge3[[2]], by = c("days","variant")) %>% 
      left_join(var_merge3[[3]], by = c("days","variant")) 
    
    # rt_comb$variant <- factor(rt_comb$variant, 
    #                           levels = c("Omicron (BA.5)",
    #                                      "Omicron (XBB)",
    #                                      "Omicron (BA.4)",
    #                                      "Omicron (BA.2)",
    #                                      "Omicron (BA.1)",
    #                                      "Omicron (BA.3)",
    #                                      "Delta",
    #                                      "Other"))
    write.csv(rt_comb, "outputs/SARS-CoV-2 effective reproduction number in Connecticut since November 2021.csv")
    
    return(list(rt_comb = rt_comb))
    
    
  })
  
  output$rt <- renderPlot({
    if (input$button_rt == "Effective reproduction number"){
      rt_plot <- ggplot(RtData()$rt_comb, aes(x = `days`, y = as.numeric(Rt), group = variant, color = variant)) +
        geom_line() + 
        geom_ribbon(aes(ymin = as.numeric(rtlowci), 
                        ymax = as.numeric(rtupci), fill = variant), 
                    alpha=0.1, 
                    linetype="blank",
                    color="grey") +
        geom_hline(yintercept = 1) +
        labs(
          title = "Variant effective reproduction number (Rt) in Connecticut (since Nov 2021)",
          y = "Rt value",
          x = "Time (weeks)"
        ) + 
        scale_fill_manual(values = c(who_colors_rt)) +
        scale_color_manual(values = c(who_colors_rt)) +
        scale_x_date(date_breaks="3 month", date_labels="%m-%Y") +
        theme_light()+
        theme(
          plot.title = element_text(size = 24,hjust = 0.5),
          axis.title.x = element_text(size = 17, hjust = 0.5),
          axis.text.x = element_text(size = 14, hjust = 0.5),
          axis.text.y = element_text(size = 14, hjust = 0.5),
          legend.text = element_text(size = 14, hjust = 0.5),
          legend.title = element_text(size = 18, hjust = 0.5),
          axis.title.y = element_text(size = 17, hjust = 0.5)
        )
      rt_plot
      
    }
    else if (input$button_rt == "Estimated cases by variant") {
      I_plot = ggplot(RtData()$rt_comb, aes(x = `days`, y = as.numeric(I), group = variant, color = variant)) +
        geom_line() + 
        labs(
          title = "Variant estimated infections in Connecticut (since Nov 2021)",
          y = "Estimated infections (cases)",
          x = "Time (weeks)"
        ) + 
        geom_ribbon(aes(ymin = as.numeric(I_025), 
                        ymax = as.numeric(I_975), fill = variant), 
                    alpha=0.1, 
                    linetype="blank",
                    color="grey") +
        scale_fill_manual(values = c(who_colors_rt)) +
        scale_color_manual(values = c(who_colors_rt)) +
        scale_x_date(date_breaks="3 month", date_labels="%m-%Y") +
        theme_light() +
        theme(
          plot.title = element_text(size = 22,hjust = 0.5)
        )
      
      I_plot
    }
    else {
      print("Click Go to load Effective reproduction number calculator")
    }
  })
  
  
  # 3. summary table
  
  output$table1 <- DT::renderDataTable(
    datatable(table1_new,
              filter = "top",
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: center;font-size:20px',
                
                'Summary table: SARS-CoV-2 variants in Connecticut.'),
              options = list(pageLength = recent_variants_count, 
                             dom = 'Bfrtip',
                             selection="multiple",
                             order = list(list(5,'desc'),list(4,'desc')),
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#504d49', 'color': '#fff'});",
                               "}"))
    )
  )
  
  
  # 4. Ct values by lineage
  
  output$CT <- renderPlot({
    
    intercept_char <- Ct_values %>% dplyr::filter(`Ctvals` == c("BA.2"))
    intercept <- as.numeric(intercept_char$`Yale.N1.FAM.`)
    intercept <- mean(intercept,na.rm = TRUE)
    rm(intercept_char)
    
    Ct_graph <- ggplot(data = Ct_values, aes(x= `Ctvals`, y = as.numeric(`Yale.N1.FAM.`)))+
      geom_hline(yintercept = intercept,colour = "lightgrey")+
      geom_boxplot(width=0.4,outlier.shape = NA,colour = "#666666")+
      geom_jitter(aes(col =`Ctvals`), alpha = 0.2,size = 0.2, stroke = 2,shape = 21, width = 0.15)+
      labs(title="PCR cycle threshold (CT) values by lineage",x=NULL, y = "Ct (N)")+
      scale_color_brewer(palette="Dark2")+
      coord_fixed(ratio = 0.06)+
      scale_y_reverse(breaks = seq(10, 40, by = 5))+
      theme_light()+
      theme(
        plot.title=element_text(size=20),
        legend.position="none",axis.text = element_text(size = 10, color = "black",face ="bold"), 
        axis.title=element_text(size=12, face ="bold"))
    Ct_graph
    
  })
  
  
  
  # 5. seq_prop
  
  seq_prop_short = seq_prop %>% # last 10 months for better viewing of recent trends
    filter(week > (as.Date(lubridate::today() - 92) - 300))
  
  seq_prop_plot = plot_ly(seq_prop) %>%
    add_bars(x = ~week, y = ~case_count, colors = "blue", 
             name = "weekly cases",data = seq_prop, showlegend=TRUE, inherit=FALSE) %>%
    add_lines(x = ~week, y = ~percent, colors ="red", yaxis = "y2", 
              name = "%seq",data = seq_prop, showlegend=TRUE, inherit=FALSE) %>%
    layout(title = "Proportion of SARS-CoV-2 cases sequenced in Connecticut",
           yaxis = ay1,
           yaxis2 = ay2)
  
  seq_prop_log = plot_ly(seq_prop) %>%
    add_trace(x = ~week, y = ~log_cases, colors = "blue", 
              name = "log cumulative cases",data = seq_prop, showlegend=TRUE, inherit=FALSE, mode = 'lines+markers') %>%
    add_trace(x = ~week, y = ~log_seq, colors ="red", 
              name = "log cumulative sequences",data = seq_prop, showlegend=TRUE, inherit=FALSE, mode = 'lines+markers') %>%
    layout(title = "Log cumulative SARS-CoV-2 cases and sequences in Connecticut",
           yaxis = ay1)
  
  seq_prop_plot_short = plot_ly(seq_prop_short) %>%
    add_bars(x = ~week, y = ~case_count, colors = "blue", 
             name = "weekly cases",data = seq_prop_short, showlegend=TRUE, inherit=FALSE) %>%
    add_lines(x = ~week, y = ~percent, colors ="red", yaxis = "y2", 
              name = "%seq",data = seq_prop_short, showlegend=TRUE, inherit=FALSE) %>%
    layout(title = "Proportion of SARS-CoV-2 cases sequenced in Connecticut",
           yaxis = ay1,
           yaxis2 = ay2)
  
  seq_prop_log_short = plot_ly(seq_prop_short) %>%
    add_trace(x = ~week, y = ~log_cases, colors = "blue", 
              name = "log cumulative cases",data = seq_prop_short, showlegend=TRUE, inherit=FALSE, mode = 'lines+markers') %>%
    add_trace(x = ~week, y = ~log_seq, colors ="red", 
              name = "log cumulative sequences",data = seq_prop_short, showlegend=TRUE, inherit=FALSE, mode = 'lines+markers') %>%
    layout(title = "Log cumulative SARS-CoV-2 cases and sequences in Connecticut",
           yaxis = ay1)
  
  output$seq_prop <- renderPlotly({
    
    if (input$button_shorten == TRUE & 
        input$button_seq_prop == "Proportion of cases sequenced") {
      ggplotly(seq_prop_plot)
    } else {
      if (input$button_shorten == FALSE & 
          input$button_seq_prop == "Proportion of cases sequenced") {
        ggplotly(seq_prop_plot_short)
      } else {
        if (input$button_shorten == TRUE & 
            input$button_seq_prop == "Log cumulative cases-sequences") {
          ggplotly(seq_prop_log)
        } else {
          ggplotly(seq_prop_log_short)
        }
      }
    }
    
  })
  
  
  # 6. Total sequences 
  
  output$total_seq <- renderValueBox({
    
    total_sequences = new_total$total[nrow(new_total)] 
    
    valueBox(
      value = HTML("<p style='font-size:40px', text-align: center>",
                   total_sequences,"</p>"),
      subtitle = HTML("<p text-align: center>",
                      "Yale SARS-CoV-2 Genomic Surveillance Initiative: <br>Genomes Published<br>"),
      color = "orange",
      width = 4
    )
  })
  
  
  # 7. Nextstrain 
  
  observe({ 
    
    test <<- paste0("https://nextstrain.org/groups/grubaughlab-public/CT-SARS-CoV-2/connecticut?c=clade_membership") 
  })
  
  output$nextstrain <- renderUI({
    input$Member
    my_test <- tags$iframe(src=test, height = 800, width = 700)
    print(my_test)
    my_test
  })
  
  output$nextstrain_caption <- renderInfoBox({
    
    nextstrain_txt = "We created a custom Nextstrain page to visualize the relatedness of sequenced SARS-CoV-2 cases in the state. Below shows a phylogenetic tree of historic sequences in Connecticut, colored by pango lineage assignments."
    
    infoBox(title = HTML("Phylogenetics:"),
            value = HTML("<p style='font-size:18px; align: center'>",
                         nextstrain_txt,"</p>"),
            icon = icon("creative-commons-sampling"),
            fill = TRUE,
            width = 4
    )
  })
  
  # 8. Twitter 
  
  output$twitter_caption <- renderInfoBox({
    
    twitter_txt = ""
    
    infoBox(title = HTML("Twitter:"),
            value = HTML("<p style='font-size:18px'>",
                         twitter_txt,"</p>"),
            color = "orange",
            icon = icon("twitter"),
            fill = TRUE,
            width = 4
    )
  })
  
  # 9. About
  output$about_tracker <- renderInfoBox({
    
    infoBox(title = HTML("CovidTrackerCT:"),
            value = HTML("<p style='font-size:18px'>",
                         "CovidTrackerCT was built and is maintained by members of the <a href = 'https://grubaughlab.com/'>Grubaugh Lab </a> at the Yale School of Public Health, New Haven, CT. 
                        We started this website in order to share our weekly reports on SARS-CoV-2 genomic epidemiology and communicate the results of our research in a clear, up-to-date way to the people for whom they matter most; residents of Connecticut. "),
            color = "orange",
            fill = TRUE,
            width = 4
    )
  })
  
  output$about_grubaughlab <- renderInfoBox({
    
    infoBox(title = HTML("Grubaugh Lab:"),
            value = HTML("<p style='font-size:18px'>",
                         "The Grubaugh lab at the Yale School of Public Health does research in viral sequencing, evolution, and transmission. As it became clear that SARS-CoV-2 is a threat, we focused all of our efforts on helping in any way we could with the response, and are now applying our experience and knowledge to understand its spread in Connecticut."),
            color = "orange",
            fill = TRUE,
            width = 4
    )
  })
  
  output$about_impact <- renderInfoBox({
    
    infoBox(title = HTML("Impact Yale:"),
            value = HTML("<p style='font-size:18px'>",
                         "Our effort is part of a larger, multidisciplinary consortium of Yale laboratories called 
                         IMPACT (<strong>I</strong>mplementing <strong>M</strong>edical and <strong>P</strong>ublic health <strong>A</strong>ction against Coronavirus in <strong>CT</strong>). 
                         Together, these laboratories conduct a number of studies, including the Yale SARS-CoV-2 Genomic Surveillance Initiative. Samples being sequenced in this project are compiled in a biorepository curated by IMPACT. Many of the protocols we use benefitted from the input of many researchers at Yale and around the world, for which we are grateful."),
            color = "orange",
            fill = TRUE,
            width = 4
    )
  })
  
  # 10. Download buttons
  output$downloadLineages <- downloadHandler(
    filename = function() {
      paste("SARS-CoV-2 lineage frequency in Connecticut since ",date_3_months,".csv",sep ='')
    },
    content = function(con) {
      write.csv(lineages_daily_sorted,con)
    }
  )
  
  output$downloadVariants <- downloadHandler(
    filename = function() {
      paste("SARS-CoV-2 variant frequency in Connecticut.csv",sep ='')
    },
    content = function(con) {
      write.csv(who_weekly,con)
    }
  )
  
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste("Summary table of SARS-CoV-2 variant frequency.csv",sep ='')
    },
    content = function(con) {
      write.csv(table1_new,con)
    }
  )
  
  output$downloadRt <- downloadHandler(
    filename = function() {
      paste("SARS-CoV-2 effective reproduction number in Connecticut since November 2021.csv",sep ='')
    },
    content = function(con) {
      write.csv(rt_comb,con)
    }
  )
  
  output$downloadseq_prop <- downloadHandler(
    filename = function() {
      paste("SARS-CoV-2 case count and sequenced proportion in Connecticut.csv",sep ='')
    },
    content = function(con) {
      write.csv(seq_prop,con)
    }
  )
  
  
})
