library(dplyr)
library(survival)
library(ggplot2)
library(directlabels)
library(dygraphs)


#Add External Data
covid_cases <- read.csv("data/days_from_first.csv")
external <- read.csv("data/external.csv")
combined <- merge(external, covid_cases, "fips")

#model Generation
datalist<-list()
j<-1
for(j in 1:6)
{
  day<-20+(j*5)
  regression <- lm( combined[,day]~percent_seniors + percent_poverty + income + population_density, data = combined)
  datalist[[j]]<-regression
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(""),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("percent_seniors",
                  "Percent Seniors:",
                  min = 12,
                  max = 26,
                  value = 18,
                  step=1),
      
      sliderInput("percent_poverty",
                  "Percent Poverty:",
                  min = 7,
                  max = 25,
                  value = 14,
                  step=1),
      
      sliderInput("income",
                  "Median Income:",
                  min = 18870,
                  max = 39285,
                  value = 26583,
                  step=500),
      
      sliderInput("population_density",
                  "Population Density:",
                  min = 3,
                  max = 1131,
                  value = 55,
                  step=10)),
      
      # Show a plot of the generated distribution
      mainPanel(
        dygraphOutput("distPlot")
      )
    )
  )
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    
    output$distPlot <- renderDygraph({
      
      user_val <- as.data.frame(input$percent_seniors) 
      user_val$percent_seniors <-input$percent_seniors
      user_val$`input$percent_seniors`<-NULL
      user_val$percent_poverty <- input$percent_poverty
      user_val$income <- input$income
      user_val$population_density <- input$population_density
      user_val$Score_5 <- predict(datalist[[1]], user_val)
      user_val$Score_10 <- predict(datalist[[2]], user_val)
      user_val$Score_15 <- predict(datalist[[3]], user_val)
      user_val$Score_20 <- predict(datalist[[4]], user_val)
      user_val$Score_25 <- predict(datalist[[5]], user_val)
      user_val$Score_30 <- predict(datalist[[6]], user_val)
      
      cases<-0
      cases<-append(cases,sum(user_val[,5]))
      cases<-append(cases,sum(user_val[,6]))
      cases<-append(cases,sum(user_val[,7]))
      cases<-append(cases,sum(user_val[,8]))
      cases<-append(cases,sum(user_val[,9]))
      cases<-append(cases,sum(user_val[,10]))
      
      final<-as.data.frame(cases)
      final$days<-c(0,5,10,15,20,25,30)
      hospitalization <- final
      hospitalization$cases <- hospitalization$cases *0.15

      spline.x <- as.data.frame(spline(hospitalization$days, hospitalization$cases))
      spline.d <- as.data.frame(spline(final$days, final$cases))
      splines <- merge(spline.x, spline.d,"x")
      names(splines)<-c("Days", "Hospitalizations", "Cases")
      
      dygraph(splines) %>% dyOptions(colors=c("#ff5b5b","#4572ad"), fillGraph = T, fillAlpha = 0.65) %>%
        dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
        dyAxis("x", drawGrid = FALSE, label="Days since first confirmed case") %>% 
        dyAxis("y", label = "COVID-19 cases per 100,000")%>%
        dyLimit(10, label = "Average Ventilator count", color = "Dark Blue", labelLoc = "left")%>%
        dyLimit(30, label = "Average ICU Bed count", color = "Dark Blue", labelLoc = "left") %>%
        dyRangeSelector(height=20)
      
    })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
  
  