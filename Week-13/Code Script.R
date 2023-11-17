#load packages
library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)

# Function to read csv files
read_year_data <- function(year) {
  data <- read_csv(paste0("yob", year, ".txt"), col_names = c("Name", "Sex", "Count"))
}

# Initialise a list to store data frames for each year
data_list <- list()

# Loop through the years from 1882 to 2022
for (year in 1882:2022) {
  data <- read_year_data(year)
  data_list[[as.character(year)]] <- data
}

# Function to filter female names
filter_female_names <- function(data) {
  return(
    data %>%
      filter(Sex == "F")
  )
}

# Function to filter male names
filter_male_names <- function(data) {
  return(
    data %>%
      filter(Sex == "M")
  )
}

# Function to merge female and male names and filter the merged names that differ in the count of females (Count.x) and males (Count.y) by less than 300. The criteria of Count.x < 2*Count.y & Count.y < 2*Count.x also ensures a more equal distribution of the proportions between both sexes eg eliminates cases where Count.x=1 and Count.y=300.
merge_names <- function(data) {
  female_names <- filter_female_names(data)
  male_names <- filter_male_names(data)
  
  overlap_names <- merge(
    female_names,
    male_names,
    by = "Name"
  )
  gender_neutral_names <- overlap_names %>% 
    filter(Count.x < 2*Count.y & Count.y < 2*Count.x & abs(Count.x-Count.y)<300) 
  
  return(gender_neutral_names)
}

# Initialise a list to store gender-neutral names for each year
gender_neutral_names_list <- list()

# Process data for each year
for (year in 1882:2022) {
  data <- data_list[[as.character(year)]]
  gender_neutral_names <- merge_names(data) 
  gender_neutral_names_list[[as.character(year)]] <- gender_neutral_names
}

#For distPlot1
# Initialise a list to store proportion of gender-neutral names for each year
proportion_list <- list()

# Calculate proportions for each year
for (year in 1882:2022) {
  gender_neutral_data <- gender_neutral_names_list[[as.character(year)]]
  all_names_data <- data_list[[as.character(year)]]
  
  total_count_gender_neutral <- sum(gender_neutral_data$Count.x) + sum(gender_neutral_data$Count.y)
  total_count_all_names <- sum(all_names_data$Count)
  proportion <- total_count_gender_neutral / total_count_all_names
  
  proportion_list[[as.character(year)]] <- proportion
}

# Create a dataframe for the proportion data
proportion_df <- data.frame(
  year = 1882:2022,
  proportion = unlist(proportion_list)
)

#Initialise a list to store proportion of males with gender-neutral names for each year
proportion_m_list <- list()

# Calculate male proportions for each year
for (year in 1882:2022) {
  gender_neutral_data <- gender_neutral_names_list[[as.character(year)]]
  all_names_data <- data_list[[as.character(year)]]
  
  total_count_m <- sum(gender_neutral_data$Count.y) 
  total_count_all_names <- sum(all_names_data$Count)
  proportion <- total_count_m / total_count_all_names
  
  proportion_m_list[[as.character(year)]] <- proportion
}

#Create a dataframe for the male proportion data
proportion_m_df <- data.frame(
  year = 1882:2022,
  proportion = unlist(proportion_m_list)
)

#Initialise a list to store proportion of female with gender-neutral names for each year
proportion_f_list <- list()

# Calculate female proportions for each year
for (year in 1882:2022) {
  gender_neutral_data <- gender_neutral_names_list[[as.character(year)]]
  all_names_data <- data_list[[as.character(year)]]
  
  total_count_f <- sum(gender_neutral_data$Count.x) 
  total_count_all_names <- sum(all_names_data$Count)
  proportion <- total_count_f / total_count_all_names
  
  proportion_f_list[[as.character(year)]] <- proportion
}

# Create a dataframe for the female proportion data
proportion_f_df <- data.frame(
  year = 1882:2022,
  proportion = unlist(proportion_f_list)
)

#For distPlot2

#From the list of gender-neutral names in 2022, 
#convert count to integer and store it in a new column 
#create a new column Total Count with values of the total count of each gender-neutral name
#create a new column Proportion Overlap Between M & F which with values the proportion overlap in the counts of both sexes
#select relevant columns -- Name, `Count of Female Babies`, `Count of Male Babies`, `Total Count`, `Proportion Overlap Between M & F`
#arrange the data in descending order of the total count
#slice the first 10 rows
# Store the results in popular_gn variable
popular_gn <- gender_neutral_names_list[["2022"]] %>%
  mutate(`Count of Female Babies` = as.integer(Count.x),
         `Count of Male Babies` = as.integer(Count.y), 
         `Total Count` = `Count of Female Babies` + `Count of Male Babies`,
         `Proportion Overlap Between M & F` = round(
           as.double(((Count.x + Count.y) - abs(Count.x - Count.y)) / (Count.x + Count.y)),
           digits = 2
         )) %>%
  select(Name, `Count of Female Babies`, `Count of Male Babies`, `Total Count`, `Proportion Overlap Between M & F`) %>%
  arrange(desc(`Total Count`)) %>%
  slice(1:10)

#find which are the names in the top 10 most popular gender-neutral names of 2020 which has a proportion overlap of more than 90%, storing it in a new variable 
low_overlap_gn <- popular_gn %>% slice(1:10) %>% filter(`Proportion Overlap Between M & F`>=0.9) %>% pull(Name)

#same as above but select the entire Name column instead (to be used later in the selectInput function in distPlot2)
low_overlap <- popular_gn %>% slice(1:10) %>% filter(`Proportion Overlap Between M & F`>=0.9) %>% select(Name)

# Initialise an empty list to store the gender-neutral names with proportion overlap > 90% and their proportions each year
name_trend_list <- list()

# Loop through the gender-neutral names with proportion overlap > 90%
for (i in 1:5) {
  popular_name <- low_overlap_gn[i]
  
  name_trend <- data.frame()  
  
  for (year in 1882:2022) {
    
    all_names_data <- data_list[[as.character(year)]]
    
    # Filter the data for the specific name
    name_data <- all_names_data %>% filter(Name == popular_name)
    
    # Calculate the proportion for the specific name
    proportion <- name_data$Count / sum(all_names_data$Count)
    
    name_data <- mutate(name_data, Proportion = proportion, Year = year)
    
    name_trend <- bind_rows(name_trend, name_data)
  }
  
  # Append this name trend to the list
  name_trend_list[[i]] <- name_trend
}

# Combine the dataframes for each name for all the years
combined_name_trend_df <- do.call(rbind, name_trend_list) 

#define UI

ui <- fluidPage(
  
  #text in the shiny app
 
  h4(strong("Motivation for the Analysis")),
  
  p("Recently, there has been a notable surge in gender-neutral names in several countries, including the United States, the United Kingdom, and Australia. Various sources support and highlight factors contributing to this trend, such as a heightened awareness and acceptance of gender fluidity (New York Post, 2018; Metro, 2023) and a push against traditional gender stereotypes that aim to challenge institutionalized sexism and misogyny (Medium, 2023). These rationales reflect a notable shift in societal attitudes, advocating for a more inclusive and progressive mindset. Consequently, more parents are opting for gender-neutral names, intending to impart these progressive values to their children. This trend not only mirrors evolving social attitudes but also sparks essential considerations about our changing society. Recognising its significance, I felt compelled to explore this evolving trend firsthand."),
  
  h4(strong("Datasets Used")),
  
  p("For the purpose of this exploration, my focus will be on the U.S. I have compiled a list of names given to babies born in the U.S. spanning the past 140 years from 1882 to 2022. These datasets are sourced from the records of the Social Security Administration, a federal agency of the U.S. government."),
  
  h4(strong("What are Gender-neutral Names?")),
  
  p("To streamline the analysis, I will define gender-neutral names as those meeting the following criteria:"),
      
  p(strong("1. The difference in counts between both sexes is less than 300")),
  p(strong("2. The count for either sex is less than double the opposite sex")),
    
  p("These criteria ensure a balanced distribution of names between both sexes."),
  
  h4(strong("Are gender-neutral Names on the Rise?")),
  
  #create sidebar for distplot1 
    sidebarLayout(
    sidebarPanel(
      #sliders that change the number of bars
      sliderInput("number_of_years", "Number of years ago:", 
                  min = 1, max = 141, value = 141),
      #radio buttons that show the proportions of none/male/female/both sexes with gender-neutral names
      radioButtons("proportion_per_sex", "Proportion of _____ given gender-neutral names:",
                   choices = c("None", "Male", "Female", "Both"),
                   selected = "None")
    ),
    
    #show distplot1 in the main panel
    mainPanel(
      plotOutput("distPlot1")
    )
  ),
  #more text
  p("The plot above depicts a significant rise in the popularity of gender-neutral names over the last 140 years, spanning from 1882 to 2022. Interestingly, the proportion of gender-neutral names in the late 1800s was quite high, but the trend declined, reaching its lowest point around the 1960s. It wasn’t until the late 1900s that the trend experienced a resurgence, and has since increased exponentially, more than doubling since the 1800s. This provides testament for the shift in preference of gender-neutral names over the years, which is perhaps reflective of the increased acceptance of gender fluidity as proposed earlier, prompting parents to seek names that transcend traditional gender boundaries."),
  
  p("The plot also reveals a shift in the gender distribution of these names. Initially, during the late 1800s to early 1900s, gender-neutral names were more prevalent among male babies. However, the proportion has since become more spread out across both sexes, and often more common among female babies, especially during the mid-late 1900s. This shift suggests that parents are increasingly open to the idea of giving their daughters gender-neutral names, challenging the traditional stereotypes that dictate how names should align with gender norms. This shift perhaps reflects a broader societal advancement toward a more egalitarian culture that prioritises gender equality."),
  
  p("This raises an intriguing question about the historical gender associations of names that are currently considered gender-neutral. Have they always been gender-neutral, or were they once more skewed towards a particular sex, possibly more commonly associated among males?"),

  h4(strong("Historical Gender-associations of Gender-neutral Names")),
    
  p("To delve into this question, I will analyse the trends of gender-neutral names that are currently popular among both male and female babies, characterised by a large proportion overlap between the counts of both sexes (more than 90%)."),
  
  p("The table below shows the top 10 gender-neutral names in 2020. Click the button below to highlight instances where the proportion overlap is more than 90%."),
  
  #create sidebar with numericInput that allows users to change the number of rows shown in the table below
    sidebarLayout(
      sidebarPanel(
        numericInput(
          inputId = "observation",
          label = "Top _ Gender-Neutral Names of 2022:",
          value = 5,
          min = 1,
          max = 10
        )
      ),
      #in the main panel, there is a table and a clickable button above it
      mainPanel(
        actionButton("highlightButton", label = "Click to highlight", style = "background-color: black; color: white;"),
        DTOutput("table")
      )
    ),
  
  #more text
  
  p(""),
  
  p("Out of the 10 names, 5 of them have a proportion overlap of more than 90%. We will zoom into each of these names, investigating the trends for both sex separately, to shed light on their evolving gender-neutrality. Select a name from the dropdown menu to display its corresponding plot and click on the navigation buttons on the side of the text card to draw the plot. Drag your cursor across a specific area of the plot to zoom in, and hover over the plot lines to see the actual proportion for each year. The text at the side provides explanations for the trends in the plot. Click on the 'Combined Plots' tab to display the plots of all 5 names in a single view to better compare the trends."),
  
  # Create a sidebar for a dropdown list of gender-neutral names with a proportion overlap > 90% that users can select
  sidebarLayout(
    sidebarPanel(
      selectInput("Name", "Name:", low_overlap$Name),
      # Card container below the dropdown list that contains an explanation about the plot
      div(
        id = "card-container",
        style = "width: 100%; aspect-ratio: 100 / 71.4; background-color: white; border: 1px solid #ccc; padding: 10px; margin-top: 10px; box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2); transition: 0.3s; display: flex; align-items: center; justify-content: space-between;",
        div(
          style = "display: flex; align-items: center; width: 100%;",
          # Left and right navigation buttons
          actionButton("prevButton", "<", style = "margin-right: auto;"),
          div(
            style = "flex: 1; display: flex; justify-content: center; align-items: center; padding: 1px; width=100%",
            textOutput("textOutput")
          ),
          actionButton("nextButton", ">")
        )
      )
    ),
    # Two tabs in the main panel
    mainPanel(
      tabsetPanel(
        type = "tabs",
        # Left tab displays the plot for the name that the user chooses from the dropdown list
        tabPanel("Individual Plots", plotlyOutput("distPlot2")),
        # Right tab shows the plots for all 5 names (not interactive, just easier to compare all the plots at one glance)
        tabPanel("Combined Plots", plotOutput("distPlot3"))
      )
    )
  ),
  
  
  #more text
  
  p(""),
  
  p("Observing the plot, we can see that 3 out of the 5 names (Blake, Charlie, and Finley) were originally male-associated but have seen a recent surge in popularity among female babies. Blake and Charlie used to be highly popular among male babies, but this popularity has fallen drastically over the years. As of 2022, more female babies are named Charlie than male babies, and the same trend is also observed for Finley. This observation supports the hypothesis that names that are considered gender-neutral now were more commonly associated among males in the past."),
  
  p("As proposed at the beginning of the datastory, some parents advocate for gender-neutral names as a means to challenge institutionalised sexism and misogyny. Names lacking explicit femininity tend to have an advantage in scenarios such as job applications, a viewpoint supported by articles in the Guardian (Mahdawi, 2016) and the Wall Street Journal (Greathouse, 2016). This suggests that adopting a gender-neutral name can potentially create more opportunities by concealing one's female identity. In fact, gender-neutral names such as Armani and Finley convey the notion of strength, as they both mean warrior. This association of strength in the meanings of these names not only provides a neutral ground in obscuring gender but also carries connotations of power, echoing the notion that gender-neutral names may project a sense of fortitude and capability in individuals."), 
  
  p("Additonally, as gender-neutral names seek to establish a distinct identity for children regardless of their assigned gender, the desire for individuality could have steered parents away from conventional or popular names, and could be a possible reason for the decline in once-popular names such as Blake and Charlie among male babies."),
  
  h4(strong("Final Conclusions")),
  
  p("The rise in popularity of gender-neutral names aligns with a broader societal push for inclusivity and a fluid understanding of gender and identity. By choosing such names for their children, parents forge distinct identities in their children that challenge traditional gender associations. This contributes to the ongoing cultural discourse on gender, reflecting the journey towards a more inclusive and egalitarian society.")
)

#define server for UI
server <- function(input, output, session) {
  #render output for distPlot1
  output$distPlot1 <- renderPlot({
   
    # Filter the main proportion dataframe for the slider to work
    filtered_proportion_df <- proportion_df %>%
      filter(year %in% seq(max(proportion_df$year) - input$number_of_years + 1, max(proportion_df$year)))
    
    # Filter the male proportion dataframe for the slider to work
    filtered_proportion_m_df <- proportion_m_df %>%
      filter(year %in% seq(max(proportion_m_df$year) - input$number_of_years + 1, max(proportion_m_df$year)))
    
    # Filter the female proportion data frame for the slider to work
    filtered_proportion_f_df <- proportion_f_df %>%
      filter(year %in% seq(max(proportion_f_df$year) - input$number_of_years + 1, max(proportion_f_df$year)))
    
    #plot an empty plot
    distPlot1 <- ggplot() +
      labs(title = "Proportion of Gender-Neutral Names per Year", subtitle="1882 to 2022", x = "Year", y = "Proportion")
    
    # Define the dataset to be used based on the selected radio button
    selected_data <- switch(input$proportion_per_sex,
                            "Male" = filtered_proportion_m_df,
                            "Female" = filtered_proportion_f_df
    )
    
    # Overlay the plot based on the selected dataset
    if (input$proportion_per_sex == "Both"){
      distPlot1 <- distPlot1 +
        geom_col(data = filtered_proportion_df, aes(x = year, y = proportion, fill = "Gender-Neutral")) +
        geom_col(data = filtered_proportion_f_df, aes(x = year, y = proportion, fill = "Female")) +
        geom_col(data = filtered_proportion_m_df, aes(x = year, y = proportion, fill = "Male"), alpha = 0.7) +
        guides(fill = guide_legend(title = NULL)) 
    } else if
      (input$proportion_per_sex == "None"){
        distPlot1 <- distPlot1 +
          geom_col(data = filtered_proportion_df, aes(x = year, y = proportion, fill = "Gender-Neutral")) +
          guides(fill = guide_legend(title = NULL)) 
      } else {
      distPlot1 <- distPlot1 +
        geom_col(data = filtered_proportion_df, aes(x = year, y = proportion, fill = "Gender-Neutral")) +
        geom_col(data = selected_data, aes(x = year, y = proportion, fill = input$proportion_per_sex)) +
      guides(fill = guide_legend(title = NULL))  # Remove the legend title for the selected category
    }
    
    # Set colours for the fill categories
    distPlot1 + scale_fill_manual(values = c("Gender-Neutral" = "black", "Male" = "powderblue", "Female" = "palevioletred1"))
    
  })
  
  #render table output in shiny interface, number of rows shown depends on the number selected in the numbericInput function with id observation
  output$table <- renderDT({
    datatable(
      head(popular_gn, input$observation),
      escape = FALSE,
      options = list(
        paging = FALSE,     # Removes pagination
        searching = FALSE,  # Removes the search bar
        info = FALSE,       # Removes "Showing X of X entries"
        ordering = FALSE    # Disables column sorting
      )
    ) %>%
      formatStyle(
        names(popular_gn),  # Apply the style to all columns
        textAlign = "left"  # Align text to the left
      )
  })
  
  #when highlight button is clicked, instances where proportion overlap between male and female > 0.9 are highlighted
  observeEvent(input$highlightButton, {
    popular_gn$`Proportion Overlap Between M & F` <- ifelse(
      popular_gn$`Proportion Overlap Between M & F` >= 0.9,
      paste0("<span style='background-color: yellow;'>", popular_gn$`Proportion Overlap Between M & F`, "</span>"),
      as.character(popular_gn$`Proportion Overlap Between M & F`)
    )
    output$table <- renderDT({
      datatable(
        head(popular_gn, input$observation),
        escape = FALSE,
        options = list(
          paging = FALSE,     # Removes pagination
          searching = FALSE,  # Removes the search bar
          info = FALSE,       # Removes "Showing X of X entries"
          ordering = FALSE    # Disables column sorting
        )
      ) %>%
        formatStyle(
          names(popular_gn),  # Apply the style to all columns
          textAlign = "left"  # Align text to the left
        )
    })
  })
  
  #distPlot2
  # Initialise the selected name and interval
  selected_name <- reactiveVal()
  interval_start <- reactiveVal(1882)
  interval_end <- reactiveVal(1882)  # Set initial interval_end to 1882
  cumulative_start <- reactiveVal(1882)
  cumulative_end <- reactiveVal(1882)  # Set initial cumulative_end to 1882
  
  # Observe select input changes and update the card and plot
  observeEvent(input$Name, {
    # When the name is changed, reset the year intervals
    interval_start(1882)
    interval_end(1882)
    cumulative_start(1882)
    cumulative_end(1882)
    
    # Set the selected_name
    selected_name(input$Name)
    
    # Update the plot based on the new name and reset year intervals
    updatePlot(selected_name(), interval_start(), interval_end())
  })
  
  # Define a function to set a different y limit for the plot of each name
  calculateYAxisLimits <- function(name) {
    filtered_df <- combined_name_trend_df %>%
      filter(Name == name, Year >= 1882, Year <= 2022)
    min_value <- min(filtered_df$Proportion)
    max_value <- max(filtered_df$Proportion)
    return(c(min_value, max_value))
  }
  
  # Function to update the plot and explanation card
  updatePlot <- function(selected_name, interval_start, interval_end) {
    y_limits <- calculateYAxisLimits(selected_name)  # Calculate y-axis limits based on selected name
    filtered_df <- combined_name_trend_df %>%
      filter(Name == selected_name, Year >= interval_start, Year <= interval_end)
    
    #Set proportion for 1882 to 0 so that eg if the name is only established in, say, 1960, the plot only draws a line from 1960, and does not trace the line to the y-axis
  filtered_df <- filtered_df %>%
   bind_rows(
    data.frame(Name = selected_name, Year = 1882, Sex = "M", Proportion = 0),
   data.frame(Name = selected_name, Year = 1882, Sex = "F", Proportion = 0)
 )
    
  #line plot of the proportion of each name agaisnt year, grouped by sex
  gg <- ggplot(filtered_df) +
      aes(x = Year, y = Proportion, group = Sex, color = Sex, text = paste("Year:", Year, "<br>Proportion:", Proportion, "<br>Sex:", Sex)) +
      geom_line() +
      labs(x = "Year", y = "Proportion", title = "Trend of Gender-Neutral Names") +
      scale_x_continuous(breaks = seq(1882, 2022, by = 25), limits = c(1882, 2022), expand = c(0, 0)) +
      scale_y_continuous(limits = y_limits)
    
    #set tooltip text to 'text' defined in the aesthetic layer of ggplot, and render plotly output
    output$distPlot2 <- renderPlotly(
      ggplotly(
        gg, tooltip = "text"
        ) 
      )
  }
  
  # Handle next button clicks, when button is clicked, plot updates the interval by 50 years
  observeEvent(input$nextButton, {
    new_cumulative_end <- cumulative_end() + 50
    
    # if the new cumulative end exceeds the maximum year (2022), set it back to 2022
    if (new_cumulative_end > 2022) {
      new_cumulative_end <- 2022
    }
    
    cumulative_end(new_cumulative_end)
    interval_start(cumulative_start())
    interval_end(new_cumulative_end)
    updatePlot(selected_name(), interval_start(), interval_end())
  })
  
  # Handle previous button clicks, when button is clicked, plot updates the interval by -50 years
  observeEvent(input$prevButton, {
    # if cumulative end is 2022, when the previous button is clicked, set new_cumulative_end to 1982, since 2022 - 50 is 1972 which would mess up the consistency in the plot shown when the next and previous buttons are clicked
    new_cumulative_end <- if (cumulative_end() == 2022){
      new_cumulative_end <- 1982
      # if the new cumulative end exceeds the minimum year (1882), set it back to 1882
      } else if (cumulative_end() <= 1882) {
      new_cumulative_end <- 1882
      } else (new_cumulative_end <- cumulative_end() - 50)
    
    cumulative_end(new_cumulative_end)
    interval_start(cumulative_start())
    interval_end(new_cumulative_end)
    updatePlot(selected_name(), interval_start(), interval_end())
  })
  
  #function for the text output in the explanation text card, when a particular name is chosen, and interval_end is a particular year, print the text
  generateTextExplanation <- function(selected_name, interval_start, interval_end) {
    explanation <- ""
    
    if (selected_name() == "Finley") {
      if (interval_end() == 1882) {
        explanation <- "Meaning of Finley: Fair warrior"
      } else if (interval_end() == "1932") {
        explanation <- "1882-1932: 'Finley' was solely used as a male name, though it was not very popular"
      } else if (interval_end() == "1982") {
        explanation <- "1932-1982: 'Finley' continued being used exclusively among male babies, but was still relatively unpopular"
      } else if (interval_end() == "2022") {
        explanation <- "In the late 1900s, 'Finley' started to appear among female babies, and picked up in popularity for both sexes. In 2004, it became more popular among female babies than male babies, reaching its peak among female babies (0.0522%) in 2017."
      }
    } else if (selected_name() == "Blake") {
      if (interval_end() == 1882) {
        explanation <- "Meaning of Blake: Black or pale"
      } else if (interval_end() == "1932") {
        explanation <- "1882-1932: 'Blake' was solely used as a name for male babies. However, it was not very popular"
      } else if (interval_end() == "1982") {
        explanation <- "'Blake' started to rise in popularity among male babies in the 1940s, surging around 1979. On the other hand, 'Blake' was first introduced among female babies in 1951, but remained relatively unpopular"
      } else if (interval_end() == "2022") {
        explanation <- "'Blake' began to rise in popularity among female babies around 2007, and reached its peak (0.165%) among male babies in 2012. However, its popularity among male babies soon declined rapidly. In 2022, the proportions of female and male babies named 'Blake' are almost equal"
      }
    } else if (selected_name() == "Tatum") {
      if (interval_end() == "1882") {
        explanation <- "Meaning of Tatum: Derived from the Old English name 'Tata' of unknown origins and the Old English word 'ham' meaning homestead"
      } else if (interval_end() == "1932") {
        explanation <- "No occurrence of this name among either sex between 1882 to 1932"
      } else if (interval_end() == "1982") {
        explanation <- "'Tatum' was first introduced among female babies in 1966 and male babies in 1973. However, it was not very popular among either sex"
      } else if (interval_end() == "2022") {
        explanation <- "'Tatum' started to see an exponential increase in popularity among both sexes in the late 1900s"
      }
    } else if (selected_name() == "Charlie") {
      if (interval_end() == "1882") {
        explanation <- "Meaning of Charlie: Free man"
      } else if (interval_end() == "1932") {
        explanation <- "This name spans back to the 1880s and was highly popular among male babies, which, however, declined over the years. Conversely, though 'Charlie' was also seen in female babies, it was relatively unpopular."
      } else if (interval_end() == "1982") {
        explanation <- "1932-1982: 'Charlie' continued decreasing in popularity among male babies while remaining relatively unpopular among female babies"
      } else if (interval_end() == "2022") {
        explanation <- "'Charlie' started regaining popularity among both sexes around the late 1900s. Its popularity among female babies overtook male babies in 2016."
      }
    } else if (selected_name() == "Armani") {
      if (interval_end() == "1882") {
        explanation <- "Meaning of Armani: Soldier, warrior"
      } else if (interval_end() == "1932") {
        explanation <- "No occurrence of this name among either sex between 1882 to 1932"
      } else if (interval_end() == "1982") {
        explanation <- "No occurrence of this name among either sex between 1932 to 1982"
      } else if (interval_end() == "2022") {
        explanation <- "'Armani' first occurred in the mid 1980s, and has risen in popularity ever since."
      }
    }
    
    return(explanation)
  }
  
   output$textOutput <- renderText({
    if (!is.null(selected_name())) {
      generateTextExplanation(selected_name(), interval_start(), interval_end)
    }
  })
  
  
  # Create a plot to visualise the same trend using facet_wrap with free y-scales so that each plot is shown to its full height
  output$distPlot3 <- renderPlot({
    ggplot(combined_name_trend_df) +
      aes(x = Year, y = Proportion, group = Sex, color = Sex) +
      geom_line() +
      labs(x = "Year", y = "Proportion", title="Trend of Gender-Neutral Names") +
      facet_wrap(~Name, scales = "free_y") +
      scale_x_continuous(breaks = seq(1882, 2022, by = 25)) 
  })
  
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)
