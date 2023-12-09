library(shiny)
library(shinydashboard)
library(ggplot2)
library(formattable)
library(dplyr)
library(gridExtra)
library(scales)
library(shiny)
library(DT)


ui <- (dashboardPage(
  dashboardHeader(title = "FIFA 19"),
  dashboardSidebar(sidebarMenu(
    menuItem("Dataset", tabName = "dataset", icon = icon("database")),
    menuItem("Best Ranks", tabName = "ranks", icon = icon("medal")),
    menuItem(
      "Distributions",
      tabName = "dist",
      icon = icon("bar-chart")
    ),
    menuItem(
      "Positions",
      tabName = "pos",
      icon = icon("square-poll-vertical")
    )
  )),
  dashboardBody(tabItems(
    # First tab content
    tabItem(
      tabName = "ranks",
      fluidRow(
        valueBoxOutput("box1"),
        valueBoxOutput("box2"),
        valueBoxOutput("box3"),
        valueBoxOutput("box4"),
        valueBoxOutput("box5"),
        valueBoxOutput("box6"),
        
        tags$img(src="fifa.jpg", style = "height: 70%; width: 100%")
        
      )
    ),
    
    
    tabItem(
      tabName = "dist",
      # Second tab content
      box(
        title = tagList("Distribution Based on Age"),
        status = "primary",
        width = NULL,
        solidHeader = TRUE,
        collapsible = FALSE,
        splitLayout(
          cellWidths = c("50%", "30%"),
          plotOutput("plot", height = 250, width = 500),
          sliderInput("bins", "Bins", 10, 35, step = 5, 30)
        )
      ),
      
      box(
        title = tagList("Top Nth Valuable Club"),
        status = "primary",
        width = NULL,
        solidHeader = TRUE,
        collapsible = FALSE,
        
        splitLayout(
          cellWidths = c("50%", "30%"),
          plotOutput("plot3", height = 250, width = 500),
          numericInput("n", "Enter N", value = 10)
        )
        
      ),
      box(
        title = tagList("Distribution Based on Nationality"),
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        
        plotOutput("plot1", height = 250, width = 500)
        
      ),
      box(
        title = tagList(
          "Distribution between Age and Speed of players Based on Value Bracket"
        ),
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        plotOutput("plot4", height = 250, width = 500)#,textInput("y",label="y-column", value= "SprintSpeed",placeholder ="Enter y-column" ))
      )
    ),
    
    tabItem(
      tabName = "pos",
      # third tab content
      box(
        title = tagList(icon("square-poll-vertical"), "Positions Based On values"),
        status = "primary",
        width = NULL,
        solidHeader = TRUE,
        collapsible = FALSE,
        
        numericInput("v", "Enter value", value = 50, width = 200),
        box(width = 12, title = "", plotOutput("plot5"))
        
      )
      
    ),
    
    tabItem(
      tabName = "dataset",
      box(
        title = tagList(icon("database"), "Dataset"),
        status = "primary",
        width = NULL,
        solidHeader = TRUE,
        collapsible = FALSE,
        DT::dataTableOutput("table")
      ),
      hr(),
      box(
        title = tagList(icon("database"), "Dataset Summary"),
        status = "primary",
        width = NULL,
        solidHeader = TRUE,
        collapsible = FALSE,
        DT::dataTableOutput("table_summary")
      )
      
    )
    
  ))
))



server <- function(input, output, session) {
  #Read data
  df = read.csv(file = "fifa19.csv")
  df = tbl_df(df)
  
  
  #Data Cleaning
  df$Dribbling = ifelse(is.na(df$Dribbling),ave(df$Dribbling, FUN = function(x) mean(x, na.rm = 'TRUE')),df$Dribbling)
  df$GKHandling = ifelse(is.na(df$GKHandling),ave(df$GKHandling, FUN = function(x) mean(x, na.rm = 'TRUE')),df$GKHandling)
  df$GKDiving = ifelse(is.na(df$GKDiving),ave(df$GKDiving, FUN = function(x) mean(x, na.rm = 'TRUE')),df$GKDiving)
  df$GKKicking = ifelse(is.na(df$GKKicking),ave(df$GKKicking, FUN = function(x) mean(x, na.rm = 'TRUE')),df$GKKicking)
  df$GKPositioning = ifelse(is.na(df$GKPositioning),ave(df$GKPositioning, FUN = function(x) mean(x, na.rm = 'TRUE')),df$GKPositioning)
  df$SprintSpeed = ifelse(is.na(df$SprintSpeed),ave(df$SprintSpeed, FUN = function(x) mean(x, na.rm = 'TRUE')),df$SprintSpeed)
  df$Potential = ifelse(is.na(df$Potential),ave(df$Potential, FUN = function(x) mean(x, na.rm = 'TRUE')),df$Potential)
  
  value_breaks <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, Inf)
  value_labels <- c("0-10M", "10-20M", "20-30M", "30-40M", "40-50M","50-60M", "60-70M", "70-80M", "80-90M","90-100M","100M+")
  value_brackets <- cut(x=df$Value, breaks=value_breaks, labels=value_labels, include.lowest = TRUE)
  df <-mutate(df, value_brackets)
  
  GK<-df$GKHandling + df$GKDiving + df$GKKicking + df$GKPositioning
  df <-mutate(df, GK)
  
  
  #Graphs
  output$box1 <- renderValueBox({
    t = 0
    x = max(df['Overall'])
    for (i in seq_len(nrow(df))) {
      players_over_80 <- df[i, "Overall"]
      if (players_over_80 >= 80) {
        t = t + 1
      }
    }
    Best_players_percentage = (t / nrow(df))
    valueBox(
      percent(Best_players_percentage),
      "Players over 80 Overall Percentage",
      icon = icon("futbol")
    )
  })
  
  output$box2 <- renderValueBox({
    f = 0
    x = max(df['SprintSpeed'])
    for (i in seq_len(nrow(df))) {
      speed <- df[i, "SprintSpeed"]
      if (speed >= 88) {
        f = f + 1
      }
    }
    Fastest_players_percentage = (f / nrow(df))
    valueBox(
      percent(Fastest_players_percentage),
      "Fastest Players Percentage",
      icon = icon("person-running")
    )
  })
  
  output$box3 <- renderValueBox({
    count = 0
    ma = max(df["Overall"])
    for (i in seq_len(nrow(df))) {
      Age <- df[i, "Age"]
      Potential <- df[i, "Potential"]
      if (Age < 20 && Potential > 69) {
        count = count + 1
      }
    }
    valueBox(count, "Number of Wonderkids", icon = icon("star"))
  })
  
  output$box4 <- renderValueBox({
    m = max(df["Overall"])
    for (i in seq_len(nrow(df))) {
      #N<-d[i,"Name"]
      N1 <- df[i, "Overall"]
      if (N1 == m) {
        name = df[i, "Name"]
        break
      }
    }
    valueBox(name, "Best Player", icon = icon("fire"))
  })
  
  output$box5 <- renderValueBox({
    m = max(df["Value"])
    for (i in seq_len(nrow(df))) {
      N1 <- df[i, "Value"]
      if (N1 == m) {
        name = df[i, "Name"]
      }
    }
    valueBox(name, "Most Valuble Player", icon = icon("euro"))
  })
  
  output$box6 <- renderValueBox({
    m = max(df["GK"])
    for (i in seq_len(nrow(df))) {
      N1 <- df[i, "GK"]
      
      if (N1 == m) {
        name = df[i, "Name"]
      }
    }
    valueBox(name, "Best GoalKeeper", icon = icon("hands"))
  })
  
  output$plot <- renderPlot({
    g_age <- ggplot(data = df, aes(Age))
    g_age +
      geom_histogram(col = "orange",
                     bins = as.numeric(input$bins),
                     aes(fill = ..count..))
    #+ ggtitle("Distribution based on Age")
  })
  
  output$plot1 <- renderPlot({
    countries_count <- count(df, Nationality)
    top_10_countries <- top_n(countries_count, 10, n)
    top_10_country_names <- top_10_countries$Nationality
    
    country <- filter(df, Nationality == top_10_country_names)
    ggplot(country, aes(x = Nationality)) +
      geom_bar(col = "orange", aes(fill = ..count..)) #+ ggtitle("Distribution based on Nationality of Players (Top 10 Countries)")
    
  })
  
  output$plot3 <- renderPlot({
    group_clubs <- group_by(df, Club)
    club_value <- summarise(group_clubs, total_val = sum(Value))
    top_10_valuable_clubs <- top_n(club_value, input$n)
    
    ggplot(top_10_valuable_clubs, aes(x = Club, y = total_val)) + geom_bar(stat = "identity", aes(fill = total_val)) + coord_flip()
    
  })
  
  output$plot4 <- renderPlot({
    g_age_overall <- ggplot(head(df, 1000000), aes(Age, SprintSpeed))
    g_age_overall  + geom_point(color ='blue') + geom_smooth(color = "darkblue")
    # +ggtitle("Distribution between Age and Speed of players")
  })
  
  output$plot5 <- renderPlot({
    gf1 <- filter(df, Value < input$v)
    g1 <-
      ggplot(gf1, aes(Position)) + geom_bar(aes(fill = value_brackets)) +
      ggtitle("(0-v)")
    gf2 <- filter(df, Value > input$v)
    g2 <-
      ggplot(gf2, aes(Position)) + geom_bar(aes(fill = value_brackets)) +
      ggtitle("(v+)")
    grid.arrange(g1, g2, ncol = 1)
    
  })
  
  output$table = DT::renderDataTable({
    datatable(df[, -1],
              class = "display nowrap compact",
              options = list(scrollX = TRUE, searching = TRUE))
  })
  
  output$table_summary= DT::renderDataTable({
    datatable(summary(df[, -1]),
              class = "display nowrap compact",
              options = list(scrollX = TRUE, searching = TRUE))
  })
  

  
}

shinyApp(ui, server)

