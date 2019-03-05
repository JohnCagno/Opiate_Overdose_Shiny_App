library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)

overdose <- read.csv("Overdose_Cleaned.csv")

overdose_scaled <- overdose %>% 
  mutate_each_(funs(scale(.) %>% as.vector), vars=c("Deaths.Per.100.000","Prescriptions.Per.Million"))

grouped <- overdose %>% 
  group_by(Year) %>% 
  summarise(Deaths.Per.100000 = sum(Deaths.Per.100.000), Prescriptions.Per.Million = sum(Prescriptions.Per.Million)) 

grouped2 <- overdose %>% 
  group_by(Year) %>% 
  summarise(Deaths = sum(Deaths.Per.100.000), Prescriptions = sum(Prescriptions.Per.Million)) 

grouped2 <- grouped2 %>% mutate_each_(funs(scale(.) %>% as.vector), 
                                      vars=c("Deaths","Prescriptions"))

# Define UI for application that draws a histogram
# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("State Level Opiate Overdose Data"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      conditionalPanel(
        condition="input.tabselected==4"
      ),
      
      conditionalPanel(
        condition="input.tabselected == 3 || input.tabselected == 2",
        selectInput("si", "State", unique(overdose$State)),
        radioButtons("gi","Select Barplot Metric",choices = c(colnames(overdose)[6], colnames(overdose)[7]))
      ),
      
      conditionalPanel(
        condition="input.tabselected==1",
        radioButtons("si2", "Year", unique(overdose$Year))
      ),
      
      # br() element to introduce extra vertical spacing ----
      br()
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        
        id = "tabselected",
        
        tabPanel("Read Me",
                 value = 4,
                 h3("Welcome to the State Level Opiate Overdose Data App!"),
                 h4("This app was create to look at opiate overdose deaths at a state level.
                    The dataset, courtesy of the CDC, provides information on deaths and the number of prescribed opiates by state by year.
                    Use the Plots and Data tab to view state level data, and the USA totals tab to view nation level data"),
                 h5("Created by John Cagno")
                 ),
        
        
        
        tabPanel("Plots",
                 value = 3,
                 h2("Opiate Prescriptions and Overdoses"),
                 plotlyOutput("barplot"),
                 plotlyOutput("linechart")
                 
        ),
        
        tabPanel("Data",
                 value = 2,
                 h2("Data"),
                 dataTableOutput(outputId = "table")
                 
        ),
        
        tabPanel("USA Totals",
                 value = 1,
                 h2("USA Totals"),
                 plotlyOutput(outputId = "usa2"),
                 plotlyOutput(outputId = "usa")
                 
        )
        )
      
      )
    )
)

# Define server logic required to draw a histogram

server <- function(input, output, session) {
  filtered_data <- reactive({filter(overdose, State == input$si)})
  filtered_data2 <- reactive({filtered_data %>%
      select(State, Year, input$gi)})
  filtered_data_scaled <- reactive({filter(overdose_scaled, State == input$si)})
  
  
  filtered_data_usa <- reactive({
    filter(overdose, Year == input$si2)%>%
      select(State, Deaths.Per.100.000)%>%
      top_n(10)%>%
      ungroup()%>%
      arrange(desc(Deaths.Per.100.000))
  })
  
  
  output$barplot <- renderPlotly(
    {
      p <- ggplot(filtered_data(), aes_string(as.character("Year"), input$gi, fill = input$gi)) + 
        geom_bar(stat="identity", position="dodge") + xlab("Year") + ylab(input$gi)+
        scale_fill_continuous(high = "#132B43", low = "#56B1F7")
      ggplotly(p) 
    }
  )
  
  output$linechart <- renderPlotly(
    {
      
      j <- ggplot(filtered_data_scaled(), aes(Year)) + 
        geom_line(aes(y = Deaths.Per.100.000, color = "Deaths.Per.100.000")) + 
        geom_line(aes(y = Prescriptions.Per.Million, color = "Prescriptions.Per.Million")) +
        ggtitle("Scaled Overdoses Deaths and Opiate Prescriptons") + xlab("Year") + ylab("Scaled Values") 
      j <- ggplotly(j)
    }
  )
  
  output$table <- renderDataTable(
    {
      filtered_data()
    }
  )
  
  output$usa <- renderPlotly(
    {
      z <- ggplot(grouped2, aes(Year)) + 
        geom_line(aes(y = Deaths, color = "Deaths")) + 
        geom_line(aes(y = Prescriptions, color = "Prescriptions")) +
        ggtitle("Scaled Overdoses Deaths and Opiate Prescriptons") + xlab("Year") + ylab("Scaled Values") 
      z <- ggplotly(z)
    }
    
  )
  
  output$usa2 <- renderPlotly(
    {
      filtered_data_usa
      p <- ggplot(filtered_data_usa(), aes(x = reorder(State, Deaths.Per.100.000), Deaths.Per.100.000, fill = Deaths.Per.100.000)) + 
        geom_bar(stat="identity", position="dodge") + xlab("Year") + ylab("Deaths Per 100,000") +
        scale_fill_continuous(high = "#132B43", low = "#56B1F7")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      ggplotly(p) 
    }
    
  )
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)






