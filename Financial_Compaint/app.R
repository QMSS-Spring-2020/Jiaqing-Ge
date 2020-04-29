library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(wordcloud)
library(plotly)
library(ggthemes)
library(gapminder)
library(scales)
library(usmap)
library(tidycensus)
library(lubridate)
library(TTR)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(tidytext)
library(stringr)
library(tidyr)
library( geojsonio )

#import data
complaints_test <- readRDS('complaints_sub.rds')
complaints_narrative_corp <- readRDS('complaints_narrative_corp.rds')
complaints_test <- complaints_test %>%
    mutate(year = as.integer(substr(Date.received, start = 1, stop = 4))) %>%
    mutate(month = as.integer(substr(Date.received, start = 6, stop = 7))) %>%
    mutate(day = as.integer(substr(Date.received, start =9 , stop = 10)))
complaints_test$Date.received <- ymd(complaints_test$Date.received)
# create overall complaint level graphs
complaints_pattern <- function(x){
    if (x=="All"){
    p_ <- complaints_test %>% 
        mutate(date = as.POSIXct(paste(month , day , sep = "." )  , format = "%m.%d" )) %>%
        group_by(date) %>%
        summarise(number_of_complaints = n()) %>%
        ggplot(aes(x = date, y = number_of_complaints))+ geom_point()+ylab("Number of complaints")+
        theme_economist()+
        scale_x_datetime(labels=  date_format("%b-%d"),date_breaks = '1 month')+
        geom_smooth(lwd=1, se=FALSE,color = 'red')+
        geom_line(aes(x=date, y=SMA(number_of_complaints,10), color = 'red'))+ 
        theme(legend.position="none")+
        ggtitle(paste("Number of Complaints throughout a Year (Overall level):",simpleCap(tolower(x)), sep = ' '))
    } else{
    p_ <-  complaints_test %>% 
        filter(Company %in% x)  %>%
        mutate(date = as.POSIXct(paste(month , day , sep = "." )  , format = "%m.%d" )) %>%
        group_by(date) %>%
        summarise(number_of_complaints = n()) %>%
        ggplot(aes(x = date, y = number_of_complaints))+ geom_point()+ylab("Number of complaints")+
        theme_economist()+
        scale_x_datetime(labels=  date_format("%b-%d"),date_breaks = '1 month')+
        geom_smooth(lwd=1, se=FALSE,color = 'red')+
        theme(legend.position="none")+
        ggtitle(paste("Number of Complaints throughout a Year of selected companies"))+theme(plot.title = element_text(size=10))}
    ggplotly(p_)}
company_names <- complaints_test %>%
    select(Company)%>%
    distinct()
Top_10_comanies <- complaints_test %>%
    group_by(Company) %>%
    summarise(number_of_complaints = n())%>%
    arrange(desc(number_of_complaints))%>%
    head(10)%>%
    select(Company) # This line select the companies with most complaints
Top_10_comanies_plot <- complaints_test %>%
    filter(Company %in% Top_10_comanies$Company) %>%
    group_by(Company,Product)%>%
    summarise(number_of_complaints = n())%>%
    ungroup()%>%
    mutate(Company = factor(Company, levels=Top_10_comanies$Company))%>%
    ggplot(aes(fill=Product, y=number_of_complaints, x=Company)) + 
    geom_bar(position="stack", stat="identity")+ylab('Nunber of Complaints by Company')+coord_flip()+theme(legend.position="right")+guides(fill=guide_legend(nrow=15))
Top_10_comanies_plot <- ggplotly(Top_10_comanies_plot,width = 1500, height = 500, type = 'bar')
Top_10_product <- complaints_test %>%
    group_by(Product) %>%
    summarise(number_of_complaints = n())%>%
    arrange(desc(number_of_complaints))%>%
    head(10)%>%
    select(Product) # This line select the companies with most complaints
Top_10_product_plot <- complaints_test %>%
    filter(Product %in% Top_10_product$Product) %>%
    group_by(Product,Submitted.via)%>%
    summarise(number_of_complaints = n())%>%
    ungroup()%>%
    mutate(Product = factor(Product, levels=Top_10_product$Product))%>%
    ggplot(aes(fill=Submitted.via, y=number_of_complaints, x=Product)) + 
    geom_bar(position="stack", stat="identity")+ylab('Nunber of Complaints by Product')+coord_flip()+theme(legend.position="bottom")
Top_10_product_plot <- ggplotly(Top_10_product_plot,width = 1200, height = 500, type = 'bar')
share_of_products_ <- complaints_test %>%
    group_by(Product) %>%
    summarize(count = n()) %>%
    plot_ly(labels = ~Product, values = ~count,width = 1200, height = 500) %>%
    add_pie(hole = 0.6) %>%
    layout(title = "Share of Products",  showlegend = T,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),legend = list(x = 100, y = 0.5))

top_word_response <- complaints_narrative_corp %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>% 
    group_by(Company.response.to.consumer) %>% 
    top_n(10) %>% 
    ungroup() %>%
    ggplot(aes(word, tf_idf, fill = Company.response.to.consumer)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~Company.response.to.consumer, ncol = 2, scales = "free") +
    coord_flip()
top_word_response <- ggplotly(top_word_response,width = 1000, height = 1000, type = 'bar')
company_response <- complaints_test %>%
    select(Company.response.to.consumer)%>%
    distinct()
wordcloud_rep <- repeatable(wordcloud)
word_cloud <- function(max_word,freq,company_res){
    sub_set <- complaints_narrative_corp %>%
        filter(Company.response.to.consumer == company_res)
    print(wordcloud_rep(words = sub_set$word, freq = sub_set$n,
        min.freq = freq,max.words=max_word, random.order=FALSE, rot.per=0.35, 
        colors=brewer.pal(8, "Dark2")))                  
              }
states <- 
    geojson_read( 
        x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
        , what = "sp"
    )
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)

labels <- sprintf(
    "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
    states$name, states$density
) %>% lapply(htmltools::HTML)

leaflet(states) %>%
    setView(-96, 37.8, 4) %>%
    addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
    addPolygons(
        fillColor = ~pal(density),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) %>%
    addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
              position = "bottomright")
### SHINY UI ###
ui <- bootstrapPage(
    tags$head(includeHTML("gtag.html")),
    navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
               "Financial Complaints", id="nav",
              
               tabPanel("Overall Complaint level through out a year",
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                                pickerInput("level_select", "Level:",   
                                            choices = c("Overall","Company"), 
                                            selected = c("Company"),
                                            multiple = FALSE),
                                
                                pickerInput("company_select", "Company",   
                                            choices = as.character(company_names$Company), 
                                            options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                            selected = c('BANK OF AMERICA, NATIONAL ASSOCIATION'),multiple = TRUE)),
                            
                            mainPanel(
                                tabPanel('Complaint Throughout a Year',plotlyOutput("complaints_pattern_plot")))
                                    )
                                ),
               tabPanel("Shares of Complaints",
                            mainPanel(
                                tabsetPanel(
                                    tabPanel('Top 10 Companies of number of complaints',plotlyOutput("Top_10_comanies_plot")),
                                    tabPanel('Top 10 Products of highest number of complaints',plotlyOutput("Top_10_product_plot")),
                                    tabPanel('Share of Complaints by Product',plotlyOutput("share_of_products_")))
                        )
               ),
               tabPanel("Company response text mining",
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                                
                                sliderInput("max_word", step = 1,
                                            "Max number of words to show",
                                            min = 1,
                                            max = 500,
                                            value = 100),
                                pickerInput("response", "Company response to consumer",   
                                            choices = as.character(company_response$Company.response.to.consumer), 
                                            selected = c('Closed with monetary relief'),
                                            multiple = FALSE),
                                sliderInput("freq",
                                            "Minimum Frequency:",
                                            min = 1,  max = 1000, value = 100),
                            ),
                            
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Frequentest words by Company response", plotlyOutput("top_word_response")),
                                    tabPanel("Word cloud", plotOutput("word_cloud_plot"))
                                )
                            )
                        )
               )
    )
)



### SHINY SERVER ###

server = function(input, output, session) {

    # update company selections
    observeEvent(input$level_select, {
        if (input$level_select=="Overall") {
            updatePickerInput(session = session, inputId = "company_select", 
                              choices = "All", selected = "All")
        }

        if (input$level_select=="Company") {
            updatePickerInput(session = session, inputId = "company_select", 
                              choices = as.character(company_names$Company), 
                              selected = c('BANK OF AMERICA, NATIONAL ASSOCIATION'))
        }
    }, ignoreInit = TRUE)

    output$complaints_pattern_plot <- renderPlotly({
        complaints_pattern(input$company_select)
    })
    output$Top_10_comanies_plot <- renderPlotly({
        Top_10_comanies_plot
    })
    output$Top_10_product_plot <- renderPlotly({
        Top_10_product_plot
    })
    output$share_of_products_ <- renderPlotly({
        share_of_products_
    })
    output$top_word_response <- renderPlotly({
        top_word_response
    })
    output$word_cloud_plot <- renderPlot({
        word_cloud(input$max_word,input$freq ,input$response)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
