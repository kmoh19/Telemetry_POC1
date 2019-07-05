#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(DT)


ui <- fluidPage(

    # Application title
    titlePanel("Aternity POC"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(width = 4,
            dateInput("Sdate", "Start Date:",value = ymd('2019-06-24')),
            dateInput("Edate", "End Date:",value = ymd('2019-06-24')+days(7)),
            br(),
            h4("Activity Summary"),
            DT::dataTableOutput("table")
        ),

        # Show a plot of the generated distribution
        mainPanel(navbarPage(
            title = 'Application Profile',
            tabPanel(h4("User Baseline/Counts"),
                     h3("Average No. of Unique Users Per Application-WeekDay"),
                     
            plotOutput("distPlot"),
            h3("Distinct Users Per Application-Date"),
            plotOutput("distPlot1")
        ),
        tabPanel(h4("Server Profile"),
                 DT::dataTableOutput("table1")
        
        )))
    )
)

# Define server logic required to draw charts
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # get summaries
        z <-interval(input$Sdate,input$Edate)
        x <- fetchData %>% filter(date(TIMEFRAME) %within% z) %>%  group_by(tf=date(TIMEFRAME),APPLICATION_NAME) %>% summarise(n=n_distinct(USERNAME))%>%  group_by(d=wday(tf,label = TRUE),APPLICATION_NAME) %>% summarise(u=round(mean(n),2))
        ggplot(x,aes(x=d,y=u,group=1))+geom_line(color="Orange")+facet_wrap(.~APPLICATION_NAME)+geom_text(aes(label=u), position=position_dodge(width=0.9), vjust=-0.25)
 
    })
    
    output$distPlot1<-renderPlot({
        z <-interval(input$Sdate,input$Edate)
        x1 <- fetchData %>% filter(date(TIMEFRAME) %within% z) %>%  group_by(tf=date(TIMEFRAME),APPLICATION_NAME) %>% summarise(n=n_distinct(USERNAME))%>%  group_by(w=wday(tf,label = TRUE),APPLICATION_NAME) %>% summarise(u=round(mean(n),2))
        x2 <- fetchData %>% filter(date(TIMEFRAME) %within% z) %>%  group_by(d=date(TIMEFRAME),APPLICATION_NAME) %>% summarise(n=n_distinct(USERNAME))%>% mutate(w=wday(d,label = TRUE))
        x<-merge(x1,x2, by=c("APPLICATION_NAME","w"))
        ggplot(x,aes(x=d,y=n))+geom_bar(stat = "identity")+facet_wrap(.~APPLICATION_NAME)+geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25)+
            geom_errorbar(aes(ymin = u, ymax = u), color="Orange",lty=2)
        
    })
    
    output$table<-DT::renderDataTable(DT::datatable({fetchData %>% filter(date(TIMEFRAME) %within% interval(input$Sdate,input$Edate)) %>%  group_by(APP_NAME=APPLICATION_NAME,ACTIVITY=ACTIVITY_NAME) %>% summarise(Users_unq=n_distinct(USERNAME),Resp_Time=round(mean(ACTIVITY_RESPONSE_TIME),2))
        },options = list(searching = TRUE, paging= TRUE,scrollX = TRUE)))
    
    
    output$table1<-DT::renderDataTable(DT::datatable({fetchData %>% select(APPLICATION_NAME,SERVER_NAME,SERVER_HOSTNAME,SERVER_IP) %>% distinct(.keep_all = TRUE) %>% arrange(APPLICATION_NAME,SERVER_NAME)
        },options = list(searching = TRUE, paging= TRUE,scrollX = TRUE)))
    
    }
    
    
   

# Run the application 
shinyApp(ui = ui, server = server)
