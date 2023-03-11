library(shiny)
library(dplyr)
library(tidyr)
library(plotly)
library(kableExtra)
library(DT)
library(stringr)
library(bslib)

top_docs_final <- read.csv("../data/40_tweets.csv") %>% filter(Topic != "Topic 0")
top_words<-read.csv("../data/top_words_upd.csv") %>% 
  rename("Topic" = "topic") %>% 
  filter(Topic != "Topic 0") %>%
  filter(!word %in% c('vaccine', 'vaccination', 'vaccinated',
                    'injection', 'shot', 'jab', 'jabbed'))

top_docs_final2 <- read.csv("../data/top_docs_upd.csv")


top_docs_ts <- top_docs_final2 %>% 
    mutate(Topic = stringr::str_replace(Topic, " ", "_")) %>%
    group_by(week_start, Topic) %>%
    summarise(n_tweets_per_topic_week = n()) %>%
    ungroup() %>%
    group_by(week_start) %>%
    mutate(n_tweets_per_week = sum(n_tweets_per_topic_week)) %>%
    ungroup() %>%
    mutate(prop_tweets_per_topic_week = n_tweets_per_topic_week / n_tweets_per_week) %>%
    ungroup() %>%
    pivot_wider(names_from = Topic, values_from = c(prop_tweets_per_topic_week, n_tweets_per_topic_week))


ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "lux"),
    titlePanel('Topic Modeling of COVID-19 Vaccine \nMisinformation Tweets'),
    helpText("Investigate tweets that have been identified as containing COVID-19 vaccine misinformation and clustered into topics."), 
    helpText("Select one or more misinformation topics from the drop down menu and learn about them in each tab."),
    sidebarLayout(
        sidebarPanel(
            selectInput('topic', 'Select Topic(s):', unique(top_docs_final$Topic), multiple = TRUE, selected = "Topic 1"),
            markdown("All tweets are classified as containing COVID-19 vaccine misinformation by [Hayawi et al., 2021](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8648668/)"),
            markdown("Tweets were clustered using [BERTopic approach](https://maartengr.github.io/BERTopic/api/bertopic.html), which included:"), 
            markdown("* creation of document embeddings  
                     * dimensionality reduction of document embeddings  
                     * development of class-based TF-IDF scores for each term in each topic")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(title = 'Term Importance',
                         p(" "),
                         p("Term importance of top ten words for each misinformation topic:"),
                         plotlyOutput("Barchart"), style='width: auto; height: 100px'),
                tabPanel(title = 'Trends Over Time', 
                         p(" "),
                         p("Time series of the number of misinformation tweets by topic over time."),
                         plotlyOutput("Lineplot", height = "300px"),
                         plotlyOutput("PropPlot", height = "300px")),
                tabPanel(title = 'Table of Example Tweets', 
                         p(" "),
                         p("Table shows the most representative tweets for each topic, with representativeness based on BertTopic modeling approach."),
                         DTOutput('Table'))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    n_inputs <- reactive({length(input$topic)})
    input_names <- reactive(str_replace({input$topic}, " ", "_"))
    output$Barchart <- renderPlotly({
      color_vec <- c("#f44336", "#9c27b0", "#3f51b5", "#03a9f4", "#009688",  "#8bc34a", "#ffeb3b", "#ff9800")
        p <- ggplotly(ggplot(top_words %>% filter(Topic %in% input$topic) %>% 
                        arrange(desc(value)) %>% group_by(Topic) %>% top_n(10) %>% ungroup(), 
                    aes(x = reorder(word, value), y = value, fill = Topic)) +
                      scale_fill_manual(values = color_vec) +
            geom_col(position = "dodge") + coord_flip() + ylab("Term Importance for Topic") +
            xlab("") + theme_bw() + 
              ggtitle("Representative Words for Each Topic") +
            theme(axis.text = element_text(face = "bold")))

    })
    
    
    output$Lineplot <- renderPlotly({
        color_vec <- c("#f44336", "#9c27b0", "#3f51b5", "#03a9f4", "#009688",  "#8bc34a", "#ffeb3b", "#ff9800")
      
         if(n_inputs() == 1) {

           fig2 <- 
               plot_ly(data = top_docs_ts,
                       x = ~week_start,
                       y = ~.data[[paste0("n_tweets_per_topic_week_", str_replace(input$topic, " ", "_"))]],
                       name = input$topic,
                       type = 'scatter',
                       mode = 'none',
                       stackgroup = 'one',
                       fillcolor = color_vec[1],
                       opacity = 0.8)
           fig2 <- fig2 %>% layout(title = 'Counts of Vaccine Misinformation Tweets per Topic',
                                   xaxis = list(title = "", showgrid = FALSE),
                                   yaxis = list(title = "Number of Misinfo Tweets", showgrid = FALSE))
           
           }
        
        else { if(n_inputs() > 1){
            fig2 <- 
                
                plot_ly(top_docs_ts,
                        x = ~week_start,
                        y = ~.data[[paste0("n_tweets_per_topic_week_", str_replace(input$topic[1], " ", "_"))]],
                        name = input$topic[1],
                        type = 'scatter',
                        mode = 'none',
                        stackgroup = 'one',
                        fillcolor = color_vec[1],
                        opacity = 0.8
                        ) 
                for(i in 2:length(input$topic)){
                    fig2 <- fig2 %>% 
                        add_trace(y = ~.data[[paste0("n_tweets_per_topic_week_", str_replace(input$topic[i], " ", "_"))]], 
                                  name = input$topic[i],
                                  fillcolor = color_vec[i],
                                  opacity = 0.8)
                    }
                
            
            fig2 <- fig2 %>% layout(title = 'Counts of Vaccine Misinformation Tweets per Topic',
                                    xaxis = list(title = "", showgrid = FALSE),
                                    yaxis = list(title = "Number of Misinformation Tweets", showgrid = FALSE))
            
            
            } 
            else{p("Please select at least one Topic")}
        }
        
    })  
    
    output$PropPlot <- renderPlotly({
      color_vec <- c("#f44336", "#9c27b0", "#3f51b5", "#03a9f4", "#009688",  "#8bc34a", "#ffeb3b", "#ff9800")
      
      if(n_inputs() == 1) {
        fig <- plot_ly(top_docs_ts, 
                     x = ~week_start, 
                     y = ~.data[[paste0("prop_tweets_per_topic_week_", str_replace(input$topic, " ", "_"))]],
                     name = input$topic,
                     type = 'scatter', 
                     mode = 'none', 
                     stackgroup = 'one',
                     groupnorm = 'percent',
                     fillcolor = color_vec[1])
                     # fillcolor = rgba(paste0(col2rgb(color_vec[1]))
        fig <- fig %>% layout(title = 'Relative Volume of Vaccine Misinformation Tweets per Topic',
                              
                              xaxis = list(title = "", showgrid = FALSE),
                              yaxis = list(title = "Percentage of Misinfo Tweets", showgrid = FALSE, ticksuffix = '%'))
      }
      else { if(n_inputs() > 1){
        fig <- plot_ly(top_docs_ts, 
                       x = ~week_start, 
                       y = ~.data[[paste0("prop_tweets_per_topic_week_", str_replace(input$topic[1], " ", "_"))]],
                       name = input$topic[1],
                       type = 'scatter', 
                       mode = 'none', 
                       stackgroup = 'one',
                       groupnorm = 'percent',
                       fillcolor = color_vec[1])
        for(i in 2:n_inputs()){
          fig <- fig %>% 
            add_trace(y = ~.data[[paste0("prop_tweets_per_topic_week_", str_replace(input$topic[i], " ", "_"))]], 
                      name = input$topic[i],
                      fillcolor = color_vec[i])
        }
        fig <- fig %>% layout(title = 'Relative Volume of Vaccine Misinformation Tweets per Topic',
                              
                              xaxis = list(title = "", showgrid = FALSE),
                              yaxis = list(title = "Percentage of Misinfo Tweets", showgrid = FALSE, ticksuffix = '%'))
        
        
        }
        }
        


      
      
      
      
    })
    
    output$Table <- renderDT(
        top_docs_final %>% filter(Topic %in% input$topic) %>%
                                               select(c("Topic", "Document")) %>%
                                               rename("Tweet Content" = "Document") %>%
                                               group_by(Topic) %>%
                                               slice(1:10),
        caption = "Most Representative Tweets per Topic"
                             )
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
