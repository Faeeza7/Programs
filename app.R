library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(data.table)
library(dashboardthemes)
library(shinyalert)
library(ggplot2)
library(GGally)
library(corrplot)

Cerealdata=read.csv('C:/Users/Admin/Documents/R classes notes/R programs/excel files/1.cereal.csv',header = T)
Cerealdata1=c("name","mfr","type","shelf")
Cerealdata=Cerealdata %>% mutate(across(.cols=all_of(Cerealdata1),.fns = as.factor))
ns=Cerealdata %>% select("calories","protein","fat","sodium","fiber","carbo","sugars","potass","weight","cups","rating")
fs=Cerealdata %>% select("name","type","shelf")
Cerealdata <- Cerealdata[-c(43),]
DB=head(Cerealdata)
View(Cerealdata)
ui <- dashboardPage(
  dashboardHeader(title = "Cereal Data",
                  tags$li(class="dropdown",tags$a(href="https://faeeza7-cerealanalysis-cdata-nezt75.streamlit.app/",icon ("Webpage"),"About",target="_blank"))),
  dashboardSidebar(sidebarMenu(id="tab1",selected = "Cerl",
                               menuItem("Home",tabName = "Cerl",icon = icon("home")),
                               menuItem("Rating Info",tabName = "NI",icon=icon("chart-line")),
                               menuItem("Other Info", tabName ="OI",icon=icon("chart-line")))),
  
  dashboardBody(shinyDashboardThemes(
    theme = "blue_gradient"
  ),
    tabItems(
      tabItem("Cerl",
              tabsetPanel(type="tabs",selected="CI",id="intabset1",
                          tabPanel("Cereal Data", value="CI",
                                   fluidRow(column(12,h1("Cereal Data Analysis"))),
                                   fluidRow(column(12,h2("Description"))),
                                   fluidRow(column(12,h3("This dataset contains nutrition information for 74 breakfast cereals and includes 16 variables. The rating column is our target(QOI) as a rating of the cereals."))),
                                   img(src = "cer.png", height =500, width = 1000,
                                       style="float: block; margin-left: auto; margin-right: auto;"),),
                          tabPanel("Dataset",value="DB",
                                   fluidRow(column(12,h1("Description of Data"))),
                                   h3("name: Name of cereal

mfr: Manufacturer of cereal:

A: American Home Food Products;

G: General Mills;

K: Kelloggs;

N: Nabisco;

P: Post;

Q: Quaker Oats;

R: Ralston Purina;

type: cold or hot."),
                                   fluidRow(column(12,offset=0,dataTableOutput("db")))
                          ),
                          tabPanel("Findings",value="findings",
                                   h1("Some of The findings are"),
                                   h4("Healthier Cereal = High Rating"),
                                   h4("High Sugar & Calories = Low Rating"),
                                   h1("Questions Concerning Cereal data"),
                                   h4("Highest Rating Value?"),
                                   h4("Relation(Manufacturer and Rating)"),
                                   h4("Rating Compare to calories count"),
                                   h4("Relation(Sugar,Calories,carbs and Fat)"),
                                   h4("Nutrition for Good Rating"),
                                   h4("Manufacturer with highest Calories"),
                                   h4("Shelf and Rating"),
                                   h4("Type and Rating"))
              )),
      
      tabItem("NI",
              tabsetPanel(type="tabs",selected="rating",id="intabset1",
                          tabPanel("Rating", value="rating",
                                   h1("Rating Value"),
                                   box(plotlyOutput("Rh"),width=12),
                                   fluidRow(column(11,offset=1,fluidRow(radioButtons("radio3","Numeric Summary",
                                                                                     choices= c("Average","Minimum","Maximum","Variance","All"))))),
                                   fluidRow(column(12,offset=0,dataTableOutput("ra")))),
                          
                          tabPanel("MFR",Value="mfr-r",
                                   h1("Manufacturer with Rating"),
                                   box(plotlyOutput("rr"),width=12),
                                   
                                   
                                   fluidRow(column(11,offset=1,fluidRow(radioButtons("radio2","Numeric Summary",
                                                                                     choices= c("Average","Minimum","Maximum","Variance","All"))))),
                                   fluidRow(column(12,offset=0,dataTableOutput("ao")))),
                          tabPanel("correlation",Value="correlation",
                                   h1("Correlation"), 
                                   box(plotOutput("cr"), width = 12)),
                          
                          tabPanel("Nutri Info", value="nutri Info",
                                   h1("Nutrition Information VS Rating"),
                                   fluidRow(column(6),offset=1,fluidRow(selectizeInput(
                                     "select3", "Please select the Nutrition of your choice", multiple = FALSE,
                                     choices = c("calories","protein","fat","sodium","fiber","carbo","sugars","potass","vitamins")
                                   )
                                   )),
                                   box(plotlyOutput("NUTRI"), width = 12),
                                   fluidRow(column(11,offset=1,fluidRow(radioButtons("radio4","Numeric Summary",
                                                                                     choices= c("Average","Minimum","Maximum","Variance","All"))))),
                                   fluidRow(column(12,offset=0,dataTableOutput("nu")))
                                   
                          )
                          
                          
              )          
              
              
      ),
      tabItem("OI",
              tabsetPanel(type="tabs",selected="sh",id="intabset1",
                          tabPanel("shelf", value="sh",
                                   h1("Shelf vs Rating"),
                                   fluidRow(column(6),offset=1,fluidRow(selectizeInput(
                                     "select9", "Please select the Nutrition of your choice", multiple = FALSE,
                                     choices = c("name","shelf","type")
                                   )
                                   )),
                                   fluidRow(
                                     box(plotlyOutput("oi"), width = 12),
                                     fluidRow(column(11,offset=1,fluidRow(radioButtons("radio5","Numeric Summary",
                                                                                       choices= c("Average","Minimum","Maximum","Variance","All"))))),
                                     fluidRow(column(12,offset=0,dataTableOutput("sh")))
                                     
                                     
                                   )),
                          tabPanel("Conclusion",value="concl",
                                   h1("Conclusion"),
                                   h3("There is a positive correlation between calories and sugars in cereal."),
                                   h3("Most cereals do not have relatively high potassium values."),
                                   h3("Kellogg’s offers the most cereals out of any manufacturer that is above the median calorie count (110). "),
                                   h3("The more calories that a cereal has, the less likely it is to receive a high rating."),
                                   h3("Manufacturers that want to bring in high ratings should create cereals that are high in fiber, protein, and potassium and avoid creating cereals with high-calorie counts or lots of sugar or fat."),
                                   h3("Cereals with high ratings are more likely to be placed on the first or third shelf, because that is generally where the consumers’ eyes gravitate"))))
      
      
      
      
    )
    
    
    
  )
)

server <- function(input,output,session)
{
  output$db <- renderDataTable({
    df = as.data.frame(DB)
    
    datatable(df, options = list(searching = FALSE))
    
    
  })
  
  output$Rh <- renderPlotly({
    ggplot(Cerealdata, aes(x=rating)) +
      geom_histogram(color="black",fill="brown")+
      labs(title= "highest Rating Value",
           y="Rating")
  })
  
  output$rr <- renderPlotly({
    ggplot(Cerealdata, aes(x=mfr,y=rating)) +
      geom_boxplot(color="black",fill="brown")+
      labs(title= "Manufacturer vs Rating",
           x="Manufacturer", 
           y="Rating")
  })
  
  
  output$NUTRI <- renderPlotly({
    if (input$select3 == "calories"  ){
      ggplot(Cerealdata,aes(x=calories,y=rating,fill=mfr))+
        geom_point(size=5,alpha=0.5)
    }
    else if (input$select3 =="protein" ){
      ggplot(Cerealdata,aes(x=protein,y=rating,fill=mfr))+
        geom_point(size=5,alpha=0.5)
    }
    else if (input$select3 =="fat" ){
      ggplot(Cerealdata,aes(x=fat,y=rating,fill=mfr))+
        geom_point(size=5,alpha=0.5)
    }
    else if (input$select3 =="sodium" ){
      ggplot(Cerealdata,aes(x=sodium,y=rating,fill=mfr))+
        geom_point(size=5,alpha=0.5)
    }
    else if (input$select3 =="fiber" ){
      ggplot(Cerealdata,aes(x=fiber,y=rating,fill=mfr))+
        geom_point(size=5,alpha=0.5)
    }
    else if (input$select3 =="carbo" ){
      ggplot(Cerealdata,aes(x=carbo,y=rating,fill=mfr))+
        geom_point(size=5,alpha=0.5)
    }
    else if (input$select3 =="sugars" ){
      ggplot(Cerealdata,aes(x=sugars,y=rating,fill=mfr))+
        geom_point(size=5,alpha=0.5)
    }
    else if (input$select3 =="potass" ){
      ggplot(Cerealdata,aes(x=potass,y=rating,fill=mfr))+
        geom_point(size=5,alpha=0.5)
    }
    else if (input$select3 =="vitamins" ){
      ggplot(Cerealdata,aes(x=vitamins,y=rating,fill=mfr))+
        geom_point(size=5,alpha=0.5)
    }
  })
  
  output$cr <- renderPlot({
    ggcorr(ns[,c(1:11)],label = T,
           low="blue",mid="red",high="green")
  })
  
  output$oi <- renderPlotly({
    if (input$select9 == "name"  ){
      
      ggplot(Cerealdata,aes(x=mfr,fill=name))+
        geom_bar(size=5,alpha=0.5)+
        
        coord_flip()
    }
    else if (input$select9 =="shelf" ){
      ggplot(Cerealdata,aes(x=mfr,y=rating,color=shelf))+
        geom_point(size=5,alpha=0.5)+
        facet_wrap(~shelf)
    }
    else if (input$select9 =="type" ){
      ggplot(Cerealdata,aes(x=mfr,y=rating,color=type))+
        geom_point(size=5,alpha=0.5)+
        facet_wrap(~type)
    }
  })
  
  output$ra <- renderDataTable({
    if(input$radio3 == "Average"){
      df = as.data.frame(Cerealdata %>% 
                           summarise(t=n(),
                                     avg_rating=round(mean(rating),2),
                                     
                           ),
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
      
    }
    else if(input$radio3 == "Minimum"){
      df = as.data.frame(Cerealdata%>% 
                           
                           summarise(t=n(),
                                     min_rating=round(min(rating),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio3 == "Maximum"){
      df = as.data.frame(Cerealdata %>% 
                           
                           summarise(t=n(),
                                     max_rating=round(max(rating),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio3 == "Variance"){
      df = as.data.frame(Cerealdata %>% 
                           
                           summarise(t=n(),
                                     Variance_rating=round(var(rating),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio3 == "All"){
      df = as.data.frame(Cerealdata %>% 
                           
                           summarise(t=n(),
                                     avg_rating=round(mean(rating),2),
                                     
                                     max_rating=round(max(rating),2),
                                     
                                     min_rating=round(min(rating),2),
                                     
                                     Variance_rating=round(var(rating),2),  
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE,paging = FALSE,Search.col= TRUE))
    }
    
    
  })
  
  output$ao <- renderDataTable({
    if(input$radio2 == "Average"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     avg_rating=round(mean(rating),2),
                                     
                           ),
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
      
    }
    else if(input$radio2 == "Minimum"){
      df = as.data.frame(Cerealdata%>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     min_rating=round(min(rating),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio2 == "Maximum"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     max_rating=round(max(rating),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio2 == "Variance"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     Variance_rating=round(var(rating),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio2 == "All"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     avg_rating=round(mean(rating),2),
                                     
                                     max_rating=round(max(rating),2),
                                     
                                     min_rating=round(min(rating),2),
                                     
                                     Variance_rating=round(var(rating),2),  
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE,paging = FALSE,Search.col= TRUE))
    }
    
    
  })
  
  output$nu <- renderDataTable({
    if(input$radio4 == "Average" & input$select3 == "calories"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     avg_calories=round(mean(calories),2),
                                     
                                     
                           ),
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
      
    }
    else if(input$radio4 == "Average" & input$select3 == "protein"){
      df = as.data.frame(Cerealdata%>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     avg_protein=round(mean(protein),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio4 == "Average" & input$select3 == "fat"){
      df = as.data.frame(Cerealdata%>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     avg_fat=round(mean(fat),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio4 == "Average" & input$select3 == "fat"){
      df = as.data.frame(Cerealdata%>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     mean_sodium=round(mean(sodium),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio4 == "Average" & input$select3 == "sodium"){
      df = as.data.frame(Cerealdata%>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     avg_sodium=round(mean(sodium),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio4 == "Average" & input$select3 == "fiber"){
      df = as.data.frame(Cerealdata%>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     avg_fiber=round(mean(fiber),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio4 == "Average" & input$select3 == "carbo"){
      df = as.data.frame(Cerealdata%>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     avg_carbo=round(mean(carbo),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio4 == "Average" & input$select3 == "sugars"){
      df = as.data.frame(Cerealdata%>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     avg_sugars=round(mean(sugars),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio4 == "Average" & input$select3 == "potass"){
      df = as.data.frame(Cerealdata%>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     avg_potass=round(mean(potass),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio4 == "Average" & input$select3 == "vitamins"){
      df = as.data.frame(Cerealdata%>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     avg_vitamins=round(mean(vitamins),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio4 == "Minimum" & input$select3 == "calories"){
      df = as.data.frame(Cerealdata%>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     min_calories=round(min(calories),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio4 == "Minimum" & input$select3 == "protein"){
      df = as.data.frame(Cerealdata%>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     min_protein=round(min(protein),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }else if(input$radio4 == "Minimum" & input$select3 == "fat"){
      df = as.data.frame(Cerealdata%>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     min_fat=round(min(fat),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }else if(input$radio4 == "Minimum" & input$select3 == "sodium"){
      df = as.data.frame(Cerealdata%>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     min_sodium=round(min(sodium),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }else if(input$radio4 == "Minimum" & input$select3 == "fiber"){
      df = as.data.frame(Cerealdata%>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     min_fiber=round(min(fiber),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }else if(input$radio4 == "Minimum" & input$select3 == "carbo"){
      df = as.data.frame(Cerealdata%>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     min_carbo=round(min(carbo),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }else if(input$radio4 == "Minimum" & input$select3 == "sugars"){
      df = as.data.frame(Cerealdata%>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     min_sugars=round(min(sugars),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }else if(input$radio4 == "Minimum" & input$select3 == "potass"){
      df = as.data.frame(Cerealdata%>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     min_potass=round(min(potass),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }else if(input$radio4 == "Minimum" & input$select3 == "vitamins"){
      df = as.data.frame(Cerealdata%>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     min_vitamins=round(min(vitamins),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    
    else if(input$radio4 == "Maximum"& input$select3 == "calories"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     max_calories=round(max(calories),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio4 == "Maximum"& input$select3 == "protein"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     max_protein=round(max(protein),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio4 == "Maximum"& input$select3 == "fat"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     max_fat=round(max(fat),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio4 == "Maximum"& input$select3 == "sodium"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     max_sodium=round(max(sodium),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio4 == "Maximum"& input$select3 == "fiber"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     max_fiber=round(max(fiber),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio4 == "Maximum"& input$select3 == "carbo"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     max_carbo=round(max(carbo),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio4 == "Maximum"& input$select3 == "sugars"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     max_sugars=round(max(sugars),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio4 == "Maximum"& input$select3 == "potass"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     max_potass=round(max(potass),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio4 == "Maximum"& input$select3 == "vitamins"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     max_vitamins=round(max(vitamins),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio4 == "Variance" & input$select3 == "calories"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     Variance_calories=round(var(calories),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio4 == "Variance" & input$select3 == "protein"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     Variance_protein=round(var(protein),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }else if(input$radio4 == "Variance" & input$select3 == "fat"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     Variance_fat=round(var(fat),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }else if(input$radio4 == "Variance" & input$select3 == "sodium"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     Variance_sodium=round(var(sodium),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }else if(input$radio4 == "Variance" & input$select3 == "fiber"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     Variance_fiber=round(var(fiber),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }else if(input$radio4 == "Variance" & input$select3 == "carbo"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     Variance_carbo=round(var(carbo),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }else if(input$radio4 == "Variance" & input$select3 == "sugars"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     Variance_sugars=round(var(sugars),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }else if(input$radio4 == "Variance" & input$select3 == "potass"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     Variance_potass=round(var(potass),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }else if(input$radio4 == "Variance" & input$select3 == "vitamins"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     Variance_vitamins=round(var(vitamins),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio4 == "All" & input$select3 == "calories"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     avg_calories=round(mean(calories),2),
                                     
                                     max_calories=round(max(calories),2),
                                     
                                     min_calories=round(min(calories),2),
                                     
                                     Variance_calories=round(var(calories),2),  
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE,paging = FALSE,Search.col= TRUE))
    }
    else if(input$radio4 == "All" & input$select3 == "protein"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     avg_protein=round(mean(protein),2),
                                     
                                     max_protein=round(max(protein),2),
                                     
                                     min_protein=round(min(protein),2),
                                     
                                     Variance_protein=round(var(protein),2),  
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE,paging = FALSE,Search.col= TRUE))
    }
    else if(input$radio4 == "All" & input$select3 == "fat "){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     avg_fat =round(mean(fat ),2),
                                     
                                     max_fat =round(max(fat ),2),
                                     
                                     min_fat =round(min(fat ),2),
                                     
                                     Variance_fat =round(var(fat ),2),  
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE,paging = FALSE,Search.col= TRUE))
    }
    else if(input$radio4 == "All" & input$select3 == "sodium "){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     avg_sodium =round(mean(sodium ),2),
                                     
                                     max_sodium =round(max(sodium ),2),
                                     
                                     min_sodium =round(min(sodium ),2),
                                     
                                     Variance_sodium =round(var(sodium ),2),  
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE,paging = FALSE,Search.col= TRUE))
    }
    else if(input$radio4 == "All" & input$select3 == "fiber"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     avg_fiber=round(mean(fiber),2),
                                     
                                     max_fiber=round(max(fiber),2),
                                     
                                     min_fiber=round(min(fiber),2),
                                     
                                     Variance_fiber=round(var(fiber),2),  
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE,paging = FALSE,Search.col= TRUE))
    }
    else if(input$radio4 == "All" & input$select3 == "carbo"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     avg_carbo=round(mean(carbo),2),
                                     
                                     max_carbo=round(max(carbo),2),
                                     
                                     min_carbo=round(min(carbo),2),
                                     
                                     Variance_carbo=round(var(carbo),2),  
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE,paging = FALSE,Search.col= TRUE))
    }
    else if(input$radio4 == "All" & input$select3 == "sugars"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     avg_sugars=round(mean(sugars),2),
                                     
                                     max_sugars=round(max(sugars),2),
                                     
                                     min_sugars=round(min(sugars),2),
                                     
                                     Variance_sugars=round(var(sugars),2),  
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE,paging = FALSE,Search.col= TRUE))
    }
    else if(input$radio4 == "All" & input$select3 == "potass"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     avg_potass=round(mean(potass),2),
                                     
                                     max_potass=round(max(potass),2),
                                     
                                     min_potass=round(min(potass),2),
                                     
                                     Variance_potass=round(var(potass),2),  
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE,paging = FALSE,Search.col= TRUE))
    }
    else if(input$radio4 == "All" & input$select3 == "vitamins"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr) %>% 
                           summarise(t=n(),
                                     avg_vitamins=round(mean(vitamins),2),
                                     
                                     max_vitamins=round(max(vitamins),2),
                                     
                                     min_vitamins=round(min(vitamins),2),
                                     
                                     Variance_vitamins=round(var(vitamins),2),  
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE,paging = FALSE,Search.col= TRUE))
    }
    
    
  })
  
  output$sh <- renderDataTable({
    if(input$radio5 == "Average" & input$select9 == "name"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(name,mfr) %>% 
                           summarise(t=n(),
                                     avg_rating=round(mean(rating),2),
                                     
                           ),
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
      
    }
    else if(input$radio5 == "Average" & input$select9 == "shelf"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr,shelf) %>% 
                           summarise(t=n(),
                                     avg_rating=round(mean(rating),2),
                                     
                           ),
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
      
    }
    else if(input$radio5 == "Average" & input$select9 == "type"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr,type) %>% 
                           summarise(t=n(),
                                     avg_rating=round(mean(rating),2),
                                     
                           ),
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
      
    }
    else if(input$radio5 == "Minimum" & input$select9 == "name" ){
      df = as.data.frame(Cerealdata%>% 
                           group_by(mfr,name) %>% 
                           summarise(t=n(),
                                     min_rating=round(min(rating),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio5 == "Minimum" & input$select9 == "shelf" ){
      df = as.data.frame(Cerealdata%>% 
                           group_by(mfr,shelf) %>% 
                           summarise(t=n(),
                                     min_rating=round(min(rating),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio5 == "Minimum" & input$select9 == "type" ){
      df = as.data.frame(Cerealdata%>% 
                           group_by(mfr,type) %>% 
                           summarise(t=n(),
                                     min_rating=round(min(rating),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio5 == "Maximum" & input$select9 == "name"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr,name) %>% 
                           summarise(t=n(),
                                     max_rating=round(max(rating),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio5 == "Maximum" & input$select9 == "shelf"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr,shelf) %>% 
                           summarise(t=n(),
                                     max_rating=round(max(rating),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio5 == "Maximum" & input$select9 == "type"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr,type) %>% 
                           summarise(t=n(),
                                     max_rating=round(max(rating),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio5 == "Variance" & input$select9 == "name"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(name) %>% 
                           summarise(t=n(),
                                     Variance_rating=round(var(rating),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio5 == "Variance" & input$select9 == "shelf"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr,shelf) %>% 
                           summarise(t=n(),
                                     Variance_rating=round(var(rating),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio5 == "Variance" & input$select9 == "type"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr,type) %>% 
                           summarise(t=n(),
                                     Variance_rating=round(var(rating),2),
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio5 == "All" & input$select9 == "name"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(name,mfr) %>% 
                           summarise(t=n(),
                                     avg_rating=round(mean(rating),2),
                                     
                                     max_rating=round(max(rating),2),
                                     
                                     min_rating=round(min(rating),2),
                                     
                                     Variance_rating=round(var(rating),2),  
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE,paging = FALSE,Search.col= TRUE))
    }
    else if(input$radio5 == "All" & input$select9 == "shelf"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr,shelf) %>% 
                           summarise(t=n(),
                                     avg_rating=round(mean(rating),2),
                                     
                                     max_rating=round(max(rating),2),
                                     
                                     min_rating=round(min(rating),2),
                                     
                                     Variance_rating=round(var(rating),2),  
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE,paging = FALSE,Search.col= TRUE))
    }
    else if(input$radio5 == "All" & input$select9 == "type"){
      df = as.data.frame(Cerealdata %>% 
                           group_by(mfr,type) %>% 
                           summarise(t=n(),
                                     avg_rating=round(mean(rating),2),
                                     
                                     max_rating=round(max(rating),2),
                                     
                                     min_rating=round(min(rating),2),
                                     
                                     Variance_rating=round(var(rating),2),  
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE,paging = FALSE,Search.col= TRUE))
    }
    
    
  })
  
}

shinyApp(ui,server)