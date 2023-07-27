# Check each package and install if not installed
for (package_name in c("tidyverse", "shiny", "rvest", "polite")) {
  if (!require(package_name, character.only = TRUE)) {
    # If not installed, install it
    install.packages(package_name, dependencies = TRUE)
  }
}

# Initialize a data frame to store the results
results <- data.frame(year = integer(), pos = integer(), team = character())

# Loop through each year from 1980 to 2022
for (year in 1980:2022) {
  # Formulate the URL for the current year
  url <- paste0("https://finalsiren.com/AFLLadder.asp?AFLLadderTypeID=2&SeasonID=", year, "&Round=22-1")
  
  # Read the HTML code from the website
  webpage <- read_html(url)
  
  # Use CSS selectors to scrape the first table
  # The 'table' CSS selector matches any <table> tag
  table_data <- webpage %>% html_nodes("table") %>% .[[1]] %>% html_table(fill = TRUE)
  
  # Making first row as column headers
  col_names <- table_data[1,]
  table_data <- table_data[-1,]
  
  # Assign the column names
  names(table_data) <- col_names
  
  # Extract the "pos" value for all teams
  for (team in unique(table_data$Team)) {
    pos <- table_data[table_data$Team == team, "Pos"]
    
    # Append the result to the data frame
    results <- rbind(results, data.frame(year = year, pos = as.integer(pos), team = team))
  }
}

# UI
ui <- fluidPage(
  titlePanel("Ladder position per Year"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Choose a Team:", choices = unique(results$team))
    ),
    mainPanel(
      plotOutput("linePlot")
    )
  )
)

# Server
server <- function(input, output) {
  output$linePlot <- renderPlot({
    req(input$team)
    # Filter data based on user's choice
    filtered_data <- results %>% filter(team == input$team)
    
    # Plot the results as a time series
    ggplot(filtered_data, aes(x = year, y = pos)) +
      geom_line() +
      scale_y_reverse(breaks = seq(1, 18, 1)) +
      theme_classic() +
      labs(x = "Year", y = "Position", title = paste("Ladder position of", input$team, "per Year"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
