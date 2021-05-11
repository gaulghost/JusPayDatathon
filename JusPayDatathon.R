library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(rsconnect)
getwd()
# Reading the CSV File
data <- read.csv("TransactionsDatasheet.csv")
print(data)
is.data.frame(data)

# Conversion of numerical number to Date-Time Format
data$time <- ymd_h(data$time)
print(data)

# Grouping Columns on the basis provided and producing new dataframe
by_day <- group_by(data, mid, time, pg,pmt)
xx <- summarise(by_day, success1 = sum(success, na.rm = TRUE), t1 = sum(t, na.rm = TRUE))
print(xx)

# Making a Success Rate Column & storing it in new DataFrame
yy <- mutate(xx, successRate = success1*100/t1)
print(yy)

# Plotting the graph on basis of mid(Merchant - Column), pmt(Payment Type - Row) and pg(Payment Gateway)
a <- ggplot(data = yy) + 
  geom_line(mapping = aes(x = time, y = successRate, color = pg)) + 
  facet_grid(pmt ~ mid)
a + theme(axis.text.x = element_text(angle = 90, hjust = 1))
fGraph <- a + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plotting the graph on basis of mid(Merchant - Column), pmt(Payment Type) and pg(Payment Gateway - Row)
b <- ggplot(data = yy) + 
  geom_line(mapping = aes(x = time, y = successRate, color = pmt)) + 
  facet_grid(pg ~ mid)
b + theme(axis.text.x = element_text(angle = 90, hjust = 1))
fGraph1 <- b + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# some surplus plots using pipe
df1 <- data %>% 
  group_by(mid, sub_type, time) %>% 
  summarise(success1 = sum(success, na.rm = TRUE), t1 = sum(t, na.rm = TRUE)) %>%
  mutate(successRate = success1*100/t1)

plt1 <- ggplot(data = df1) + 
  geom_line(mapping = aes(x = time, y = successRate)) + 
  facet_grid( sub_type ~ mid) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

df2 <-  data %>% 
  group_by(mid, pmt, time) %>% 
  summarise(success1 = sum(success, na.rm = TRUE), t1 = sum(t, na.rm = TRUE)) %>%
  mutate(successRate = success1*100/t1)

plt2 <- ggplot(data = df2) + 
  geom_line(mapping = aes(x = time, y = successRate)) + 
  facet_grid( pmt ~ mid) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

df3 <-  data %>% 
  group_by(mid, pg, time) %>% 
  summarise(success1 = sum(success, na.rm = TRUE), t1 = sum(t, na.rm = TRUE)) %>%
  mutate(successRate = success1*100/t1)

plt3 <- ggplot(data = df3) + 
  geom_line(mapping = aes(x = time, y = successRate)) + 
  facet_grid( pg ~ mid) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# ROUGH
#k <- ggplot(data = yy) + 
#  geom_line(mapping = aes(x = time, y = successRate , color = pg)) + 
#  facet_wrap( ~ mid, nrow = 2)

#k + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#cd <- filter(yy, mid == 'countrydelight' & pmt == 'UPI')

#print(cd)

#j <- ggplot(data = cd) + 
#  geom_line(mapping = aes(x = time, y = successRate))

#j + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#-------------------------------DEVELOPMENT OF WEB APP------------------------------------------------

library(shiny)
library(shinydashboard)
library(DT)

# Options to customize fGraph obtained above
xFacetgrid <- c("bank", "pmt", "mid", "pg", "sub_type", "None")
yFacetgrid <- c("bank", "pmt", "mid", "pg", "sub_type", "None")
graphColor <- c("bank", "pmt", "mid", "pg", "sub_type", "None")
midChoices <- data$mid %>% unique()
midChoices

ui <- dashboardPage(
  
  #----------------------------------------header--------------------------------------------
  
  dashboardHeader(
    title = "JusPay Dashboard"
  ),
  
  #--------------------------------- Sidebar content-----------------------------------------
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Dataset", tabName = "dataset", icon = icon("table")),
      menuItem("Charts", tabName = "charts", icon = icon("bar-chart-o"),
               menuSubItem("Part 1", tabName = "Part1", icon = icon("bar-chart-o")),
               menuSubItem("Part 2", tabName = "Part2", icon = icon("bar-chart-o"))
      ),
      menuItem("Inference", tabName = "inference", icon = icon("list-alt"))
    )
  ),
  
  
  
  
  
  #----------------------------------------Body content----------------------------------------
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box( width = 12,
                     plotOutput("rGraph", height = "500px")
                )
              ),
              
              fluidRow(
                box( width = 12,
                     title = "Controlling Plotting Parameters",
                     div(class = "row",
                         div(class = "col-sm-6 col-md-4",selectInput("mid", "Merchant ID", midChoices))
                         #div(class = "col-sm-6 col-md-4",radioButtons("yFacet", "Facet Grid Y axis", yFacetgrid)),
                         #div(class = "col-sm-6 col-md-4",radioButtons("gColor", "Distinction by Class based on Color", graphColor))
                     )
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "inference",
              fluidRow(
                box( width = 12,
                     title = "Inference Obtained by Visualizing Graph",
                     p("Inference Obtained : We shall Inform the Merchant about the dip ( >20%/ 30% )  regarding the specific portal page and payment method if it persists for more than specified duration( 5/ 10 minutes), so that merchant can look into and rectify the problem. This is called Alerting Mechanism which also saves the trouble for Merchant searching for the problem."),
                     tags$table(border = 3, style = "width:100%;", 
                                tags$thead(
                                  tags$tr(
                                    tags$th(colspan = 4, align = "center", "Where Dips were Obtained")
                                  ),
                                  tags$tr(
                                    tags$th(align = "center", "Merchant ID(mid)"),
                                    tags$th(align = "center", "Payment Type(pmt)"),
                                    tags$th(align = "center", "Payment Gateway(pg)"),
                                    tags$th(align = "center", "sub_type")
                                  )
                                ), 
                                tags$tbody(
                                  tags$tr(
                                    tags$td(strong("countrydelight")),
                                    tags$td(align = "center", "UPI"),
                                    tags$td(align = "center", "NA"),
                                    tags$td(align = "center", "UPI_COLLECT")
                                  ),
                                  tags$tr(
                                    tags$td(strong("drivezy")),
                                    tags$td(align = "center", "UPI, NB, CONSUMER_FINANCE"),
                                    tags$td(align = "center", "NA, EPAYLATER, ZAAKPAY"),
                                    tags$td(align = "center", "UPI_COLLECT")
                                  ),
                                  tags$tr(
                                    tags$td(strong("fanfight")),
                                    tags$td(align = "center", "NB"),
                                    tags$td(align = "center", "RAZORPAY, ZAAKPAY"),
                                    tags$td(align = "center", "UPI_COLLECT, NA")
                                  ),
                                  tags$tr(
                                    tags$td(strong("medlife_prod")),
                                    tags$td(align = "center", "UPI"),
                                    tags$td(align = "center", "-"),
                                    tags$td(align = "center", "UPI_COLLECT")
                                  ),
                                  tags$tr(
                                    tags$td(strong("pharmeasytech")),
                                    tags$td(align = "center", "UPI"),
                                    tags$td(align = "center", "NA"),
                                    tags$td(align = "center", "UPI_COLLECT")
                                  ),
                                  tags$tr(
                                    tags$td(strong("purplle.com")),
                                    tags$td(align = "center", "NB"),
                                    tags$td(align = "center", "NA"),
                                    tags$td(align = "center", "ALL VARY")
                                  ),
                                  tags$tr(
                                    tags$td(strong("urbanclap")),
                                    tags$td(align = "center", "-"),
                                    tags$td(align = "center", "NA"),
                                    tags$td(align = "center", "REDIRECT_WALLET_DEBIT")
                                  ),
                                  tags$tr(
                                    tags$td(strong("zivame")),
                                    tags$td(align = "center", "WALLET"),
                                    tags$td(align = "center", "FREECHARGE, FSS_ATM_PIN_V2"),
                                    tags$td(align = "center", "DIRECT_WALLET_DEBIT, REDIRECT_WALLET_DEBIT")
                                  )
                                )
                     )
                )
              )
      ),
      
      # Third tab content
      tabItem(tabName = "Part1",
              h3(style = "align: centre;", "Graphs may take some time to load wait patiently (Best viewed on fullscreen)"),
              fluidRow(
                box( width = 12,
                     h4(style = "align: center;", "Plot Success Rate v/s Date, Dividing line on basis of Payment Gateway, Partioning Graphs based on Merchant ID and Payment Type"),
                     plotOutput("rGraph1", height = "100vh")
                )
              ),
              fluidRow(
                box( width = 12,
                     h4(style = "align: center;", "Plot Success Rate v/s Date, Dividing line on basis of Payment Type, Partioning Graphs based on Merchant ID and Payment Gateway"),
                     plotOutput("rGraph2", height = "100vh")
                )
              )
      ),
      
      # Fourth tab content
      tabItem(tabName = "Part2",
              h3("Graphs may take some time to load wait patiently (Best viewed on fullscreen)"),
              fluidRow(
                box( width = 12,
                     h4("Plot Success Rate v/s Date, Partioning Graphs based on Merchant ID(mid) and sub_type"),
                     plotOutput("rplt1", height = "100vh")
                )
              ),
              fluidRow(
                box( width = 12,
                     h4("Plot Success Rate v/s Date, Partioning Graphs based on Merchant ID(mid) and Payment Type(pmt)"),
                     plotOutput("rplt2", height = "100vh")
                )
              ),
              fluidRow(
                box( width = 12,
                     h4("Plot Success Rate v/s Date, Partioning Graphs based on Merchant ID(mid) and Payment Gateway(pg)"),
                     plotOutput("rplt3", height = "100vh")
                )
              )
      ),
      
      # Fifth tab content
      tabItem(tabName = "dataset",
              h2("Given Dataset with Transformation of hr(renamed to time) Column"),
              DT::dataTableOutput("givenDataset")
      )
      
    )
  )
)

#-------------------------------------------Server Side--------------------------------------

server <- function(input, output) {
  
  
  observeEvent(input$mid, {
    print(input$mid)
    by_day <- group_by(data, mid, time) %>% summarise(success1 = sum(success, na.rm = TRUE), t1 = sum(t, na.rm = TRUE)) %>%
      filter(mid == input$mid) %>% mutate(successRate = success1*100/t1)
    print(by_day)
    
    df7 <- ggplot(data = by_day) +
      geom_line(mapping = aes(x = time, y = successRate))
    
    output$rGraph <- renderPlot({
      df7
    })
    
  })
  
  #Dashboard Graph
  df5 <- reactive({
    req(input$gColor)
    req(input$xFacet)
    req(input$yFacet)
    df4 <-  data %>% 
      group_by(input$gColor, time, input$xFacet, input$yFacet) %>% 
      summarise(success1 = sum(success, na.rm = TRUE), t1 = sum(t, na.rm = TRUE)) %>%
      mutate(successRate = success1*100/t1)
  })
  #observe Event
  dgraph5 <- reactive({
    
    print(input$gColor)
    print(input$xFacet)
    print(input$yFacet)
    
    ggplot(data = df5()) + 
      geom_line(mapping = aes(x = time, y = successRate, color = input$gColor)) + 
      facet_grid( input$yFacet ~ input$xFacet) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  
  
  # Part1 Chart
  output$rGraph1 <- renderPlot({
    fGraph
  })
  output$rGraph2 <- renderPlot({
    fGraph1
  })
  
  #Part2 Chart
  output$rplt1 <- renderPlot({
    plt1
  })
  output$rplt2 <- renderPlot({
    plt2
  })
  output$rplt3 <- renderPlot({
    plt3
  })
  
  #Dataset
  output$givenDataset = DT::renderDataTable({
    data
  })
  
}

shinyApp(ui, server)
#-----------------------Calculating dip in runtime---------------------------
data_mid <- data %>% group_by(mid, time) %>% summarise(success1 = sum(success, na.rm = TRUE), t1 = sum(t, na.rm = TRUE)) %>% mutate(successRate = success1*100/t1)
#print(data_mid)
#ggplot(data = data_mid) + geom_line(mapping = aes(x = time , y = successRate)) + facet_grid(. ~ mid)

for (i in data$mid %>% unique()){
  data_mid_i <- filter(data_mid, mid == i)
  #print(data_mid_i)
  data_mid_i.sd = sd(data_mid_i$successRate)
  data_mid_i.mean = mean(data_mid_i$successRate)
  data_mid_i.median = median(data_mid_i$successRate)
  x_is_greater = TRUE;
  for( j in 1:nrow(data_mid_i)){
    if(data_mid_i[j,5] <= (data_mid_i.median - data_mid_i.sd)){
      cat(sprintf("\"%s\" \"%s\"\n", i, " is going down or has a dip at "))
      print(data_mid_i[j,2])
      x_is_greater = FALSE
    }
    else if (x_is_greater == FALSE && data_mid_i[j,5] > (data_mid_i.median - data_mid_i.sd)){
      cat(sprintf("\"%s\" \"%s\"\n", i, " has recovered at "))
      print(data_mid_i[j,2])
      x_is_greater = TRUE
    }
  }
  print('------------------------------------------------------------------------')
}


#-----------------------------------------------------------------------------------

data_mid <- data %>% group_by(mid, time,pg) %>% summarise(success1 = sum(success, na.rm = TRUE), t1 = sum(t, na.rm = TRUE)) %>% mutate(successRate = success1*100/t1)
print(data_mid)
#ggplot(data = data_mid) + geom_line(mapping = aes(x = time , y = successRate)) + facet_grid(pg ~ mid)

for (i in data$mid %>% unique()){
  data_mid_i <- filter(data_mid, mid == i)
  #print(data_mid_i)
  x_is_greater = TRUE;
  
  for(k in data_mid_i$pg %>% unique()){
    data_mid_i_pg <- filter(data_mid_i, pg == k)
    #print(data_mid_i_pg)
    data_mid_i_pg.sd = sd(data_mid_i_pg$successRate)
    data_mid_i_pg.mean = mean(data_mid_i_pg$successRate)
    data_mid_i_pg.median = median(data_mid_i_pg$successRate)
    x_is_greater = TRUE;
    
    for( j in 1:nrow(data_mid_i_pg)){
      if(data_mid_i_pg[j,6] < (data_mid_i_pg.median - data_mid_i_pg.sd)){
        cat(sprintf("\"%s\" \"%s\" \"%s\" \"%s\"\n", k, " of ", i, " is going down or has a dip at "))
        print(data_mid_i[j,2])
        x_is_greater = FALSE
      }
      else if (x_is_greater == FALSE && data_mid_i_pg[j,6] > (data_mid_i_pg.median - data_mid_i_pg.sd)){
        cat(sprintf("\"%s\" \"%s\" \"%s\" \"%s\"\n", k, " of ", i, " has recovered at "))
        print(data_mid_i[j,2])
        x_is_greater = TRUE
      }
    }
    print('------------------------------------------------------------------------')
  }
  print('**************************************************************************')
}

#-------------------------------DEBUG---------------------------------
data_mid <- data %>% group_by(mid, time,pg) %>% summarise(success1 = sum(success, na.rm = TRUE), t1 = sum(t, na.rm = TRUE)) %>% mutate(successRate = success1*100/t1)
#print(data_mid)
#ggplot(data = data_mid) + geom_line(mapping = aes(x = time , y = successRate)) + facet_grid(pg ~ mid)

for (i in data$mid %>% unique()){
  data_mid_i <- filter(data_mid, mid == i)
  #print(data_mid_i)
  x_is_greater = TRUE;
  for(k in data_mid_i$pg %>% unique()){
    if(k == ""){
      next
    }
    data_mid_i_pg <- filter(data_mid_i, pg == k)
    #print(data_mid_i_pg)
    data_mid_i_pg.sd = sd(data_mid_i_pg$successRate)
    data_mid_i_pg.mean = mean(data_mid_i_pg$successRate)
    data_mid_i_pg.median = median(data_mid_i_pg$successRate)
    x_is_greater = TRUE;
    if(is.na(data_mid_i_pg.sd) || is.na(data_mid_i_pg.mean) || is.na(data_mid_i_pg.median)){
      next
    }
    for( j in 1:nrow(data_mid_i_pg)){
      if(data_mid_i_pg[j,6] <= (data_mid_i_pg.median - data_mid_i_pg.sd)){
        cat(sprintf("\"%s\" \"%s\" \"%s\" \"%s\"\n", k, " of ", i, " is going down or has a dip at "))
        print(data_mid_i[j,2])
        x_is_greater = FALSE
      }
      else if (x_is_greater == FALSE && data_mid_i_pg[j,6] > (data_mid_i_pg.median - data_mid_i_pg.sd)){
        cat(sprintf("\"%s\" \"%s\" \"%s\" \"%s\"\n", k, " of ", i, " has recovered at "))
        print(data_mid_i[j,2])
        x_is_greater = TRUE
      }
    }
    print('------------------------------------------------------------------------')
  }
  print('**************************************************************************')
}

#---------------------------------DIP IN UPI (NOT CONSIDERING MERCHANTS)-----------------
data_upi <- data %>% group_by(time, pmt) %>% filter(pmt == "UPI") %>% summarise(success1 = sum(success, na.rm = TRUE), t1 = sum(t, na.rm = TRUE)) %>% mutate(successRate = success1*100/t1)
#print(data_upi)

data_upi.sd = sd(data_upi$successRate)
data_upi.median = median(data_upi$successRate)
x_is_greater = TRUE;

for( i in 1:nrow(data_upi)){
  if(data_upi[i,5] <= (data_upi.median - data_upi.sd) ){
    cat(sprintf("\"%s\"\n", "UPI is going down or has a dip at "))
    print(data_upi[i,1])
    x_is_greater = FALSE
  }
  else if (x_is_greater == FALSE && data_upi[i,5] > (data_upi.median - data_upi.sd)){
    cat(sprintf("\"%s\"\n", "UPI has recovered at "))
    print(data_upi[i,1])
    x_is_greater = TRUE
  }
}

nrow(data_upi)

#------------------------- DIP IN UPI (FOR EACH OF THE MERCHANTS) --------------------------
data_mid_upi <- data %>% group_by(mid, time, pmt) %>% filter(pmt == "UPI") %>% summarise(success1 = sum(success, na.rm = TRUE), t1 = sum(t, na.rm = TRUE)) %>% mutate(successRate = success1*100/t1)
print(data_mid_upi)
for(i in data_mid_upi$mid %>% unique()){
  data_mid_upi_i <- filter(data_mid_upi , mid == i)
  data_mid_upi_i.sd = sd(data_mid_upi_i$successRate)
  data_mid_upi_i.median = sd(data_mid_upi_i$successRate)
  x_is_greater = TRUE
  for(j in 1:nrow(data_mid_upi_i) ){
    if(data_mid_upi_i[j,6] <= (data_mid_upi_i.median - data_mid_upi_i.sd)){
      cat(sprintf("\"%s\" \"%s\" \"%s\"\n","UPI of ", i, " is going down or has a dip at "))
      print(data_mid_upi_i[j,2])
      x_is_greater = FALSE
    }
    else if(x_is_greater == FALSE &&  data_mid_upi_i[j,6] > (data_mid_upi_i.median - data_mid_upi_i.sd)){
      cat(sprintf("\"%s\" \"%s\" \"%s\"\n","UPI of ", i, " has recovered at "))
      print(data_mid_upi_i[j,2])
      x_is_greater = TRUE
    }
  }
  print("-------------------------------------------------------------------")
}