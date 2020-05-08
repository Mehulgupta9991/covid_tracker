library(dplyr)
library(survival)
library(ggplot2)

defaultW <- getOption("warn")
options(warn = -1)
datalist<-list()
coxmodels<-list()

for(j in 1:8)
{
  
  #Creating Event Data
  covid_cases <- read.csv("data/covid_data.csv")
  days_min <-0
  days_max <-0
  days_total <-0
  censor<-0
  barrier <- j*15
  for (i in 2:nrow(covid_cases)) 
  {
    days_only<-covid_cases[i,12:length(covid_cases)]
    days_min[i]<-min(which(days_only>= 1))
    days_max[i]<-min(which(days_only>= barrier))
    
    censor[i]<-ifelse(days_max[i]=="Inf",0, 1)
    days_max[i]<-ifelse(days_max[i]=="Inf",105, days_max[i])
    days_total[i]<-days_max[i]-days_min[i]
  }
  
  covid_cases$days_to <-days_total
  covid_cases$status <- censor
  
  #Reading in other variables of interest
  external <- read.csv("data/external.csv")
  combined <- merge(external, covid_cases, "fips")
  
  #Spliting Datasets
  combined <-combined[!combined$days_to =="-Inf",]
  even_indexes<-seq(2,nrow(combined),2)
  odd_indexes<-seq(1,nrow(combined),2)
  training <- data.frame(combined[even_indexes,])
  validation <-data.frame(combined[odd_indexes,])
  
  #model Generation
  signature <- coxph(Surv(days_to, status) ~percent_seniors + percent_poverty + percent_uninsured + income + population_density, data = training)
  validation$signature <- predict(signature, validation)
  regression <- lm(days_to ~ signature, data = validation)
  coxmodels[[j]] <- signature
  regression <- broom::tidy(regression)
  keep<-regression[,2]
  datalist[[j]]<-keep
}

storage = do.call(cbind, datalist)

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
         
         sliderInput("percent_uninsured",
                     "Percent Uninsured:",
                     min = 4,
                     max = 18,
                     value = 9,
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
                     step=10),
        
        sliderInput("Hospital",
                    "Hospital Beds:",
                    min = 0,
                    max = 60,
                    value = 0,
                    step=1)),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
     output$distPlot <- renderPlot({
      
      user_val <- as.data.frame(input$percent_seniors) 
      user_val$percent_seniors <-input$percent_seniors
      user_val$`test$percent_seniors`<-NULL
      user_val$percent_poverty <- input$percent_poverty
      user_val$percent_uninsured <- input$percent_uninsured
      user_val$income <- input$income
      user_val$population_density <- input$population_density
      user_val$Score_10 <- predict(coxmodels[[1]], user_val)
      user_val$Score_20 <- predict(coxmodels[[2]], user_val)
      user_val$Score_30 <- predict(coxmodels[[3]], user_val)
      user_val$Score_40 <- predict(coxmodels[[4]], user_val)
      user_val$Score_50 <- predict(coxmodels[[5]], user_val)
      user_val$Score_60 <- predict(coxmodels[[6]], user_val)
      user_val$Score_70 <- predict(coxmodels[[7]], user_val)
      user_val$Score_80 <- predict(coxmodels[[8]], user_val)
      
      len<-nrow(storage)
      storage[2,9] <- storage[2,1] * user_val$Score_10
      storage[2,10] <- storage[2,2] * user_val$Score_20
      storage[2,11] <- storage[2,3] * user_val$Score_30
      storage[2,12] <- storage[2,4] * user_val$Score_40
      storage[2,13] <- storage[2,5] * user_val$Score_50
      storage[2,14] <- storage[2,6] * user_val$Score_60
      storage[2,15] <- storage[2,7] * user_val$Score_70
      storage[2,16] <- storage[2,8] * user_val$Score_80
      storage[1,9] <- storage[1,1]
      storage[1,10] <- storage[1,2]
      storage[1,11] <- storage[1,3]
      storage[1,12] <- storage[1,4]
      storage[1,13] <- storage[1,5]
      storage[1,14] <- storage[1,6]
      storage[1,15] <- storage[1,7]
      storage[1,16] <- storage[1,8]
      days<-0
      days<-append(days,sum(storage[,9]))
      days<-append(days,sum(storage[,10]))
      days<-append(days,sum(storage[,11]))
      days<-append(days,sum(storage[,12]))
      days<-append(days,sum(storage[,13]))
      days<-append(days,sum(storage[,14]))
      days<-append(days,sum(storage[,15]))
      days<-append(days,sum(storage[,16]))
      
      final<-as.data.frame(days)
      final$Cases<-c(0,10,20,30,40,50,60,70,80)
      
      hospitalization <- final
      hospitalization$Cases <- hospitalization$Cases *0.15
      
      spline.d <- as.data.frame(spline(final$days, final$Cases))
      spline.x <- as.data.frame(spline(hospitalization$days, hospitalization$Cases))
      ggplot(final) + coord_cartesian(xlim = c(0, 40), ylim = c(0, 130))+ theme_bw() + geom_hline(yintercept=input$Hospital, linetype="dashed", color = "dark blue", size=0.5) + xlab("Days From First Confirmed Case") + ylab("Total Number of Cases") + geom_line(data = spline.d, aes(x = x, y = y)) + geom_line(data = spline.x, aes(x = x, y = y, col="red"))
      

   })
}

# Run the application 
shinyApp(ui = ui, server = server)

