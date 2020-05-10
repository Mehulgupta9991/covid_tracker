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


#Add External Data_Deaths
covid_deaths <- read.csv("data/deaths.csv")
external <- read.csv("data/external.csv")
combined_d <- merge(external, covid_deaths, "fips")

#model Generation_Deaths
datalist_deaths<-list()
j<-1
for(j in 1:4)
{
  day<-20+(j*5)+10 #a 15 day lag time is added
  regression_d <- lm( combined_d[,day]~percent_seniors + percent_poverty + income + population_density, data = combined_d)
  datalist_deaths[[j]]<-regression_d
}



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Covid-19 Predictor"),
  
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
                  max = 500,
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
      
      user_val$Death_5 <- predict(datalist_deaths[[1]], user_val)
      user_val$Death_10 <- predict(datalist_deaths[[2]], user_val)
      user_val$Death_15 <- predict(datalist_deaths[[3]], user_val)
      user_val$Death_20 <- predict(datalist_deaths[[4]], user_val)

      cases<-0
      cases<-append(cases,sum(user_val[,5]))
      cases<-append(cases,sum(user_val[,6]))
      cases<-append(cases,sum(user_val[,7]))
      cases<-append(cases,sum(user_val[,8]))
      cases<-append(cases,sum(user_val[,9]))
      cases<-append(cases,sum(user_val[,10]))
      
      deaths<-0
      deaths<-append(deaths,sum(user_val[,11]))
      deaths<-append(deaths,sum(user_val[,12]))
      deaths<-append(deaths,sum(user_val[,13]))
      deaths<-append(deaths,sum(user_val[,14]))

      final<-as.data.frame(cases)
      final$days<-c(0,5,10,15,20,25,30)
      
      final_d<-as.data.frame(deaths)
      final_d$days<-c(0,15,20,25,30)
      
      spline.x <- as.data.frame(spline(final_d$days, final_d$deaths, n=25))
      spline.d <- as.data.frame(spline(final$days, final$cases, n=25))
      splines <- cbind(spline.x, spline.d)
      splines[,3] <-NULL
      
      names(splines)<-c("Days", "Deaths", "Cases")
      
      dygraph(splines) %>% dyOptions(colors=c("#ff5b5b","#4572ad"), fillGraph = T, fillAlpha = 0.65) %>%
        dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
        dyAxis("x", drawGrid = FALSE, label="Days since 1st confirmed case (drag to zoom)") %>% 
        dyAxis("y", label = "Cases per 100,000", valueRange=c(0,250))%>%
        dyLimit(10, label = "Ventilator count", color = "Dark Blue", labelLoc = "left")%>%
        dyLimit(30, label = "ICU bed count", color = "Dark Blue", labelLoc = "left") %>%
        dyRangeSelector(height=20)
      
    })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
  
  