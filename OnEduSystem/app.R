library(shiny)
library(randomForest)
library(rpart)
library(dplyr)
library(tidyr)
library(ggplot2)

# UI definition
ui <- navbarPage(
  "Improve online Education",
  
  # First Page
  tabPanel("Home",
          fluidPage(

  mainPanel(
    img(src = "homepage.png", height = "600", width = "1360"),
    # Output for model comparison bar plot
    plotOutput("modelComparisonPlot"),
    
  )
)
  
),
# Second Page
  tabPanel("Univariate Analysis",
    fluidPage(
      # Center-align h2 and add color
      tags$head(
        tags$style(HTML("
          h2 {
            text-align: center;
            color: Purple; /* Change to the desired color */
          }
        "))
      ),
      h2("Histograms"),
      plotOutput("ageHistogram"),
      plotOutput("studyTimeHistogram"),
      plotOutput("socialMediaTimeHistogram"),
      plotOutput("internetFacilityHistogram"),
      h2("Barplots"),
      plotOutput("economicStatusBarPlot"),
      plotOutput("homeLocationBarPlot"),
      plotOutput("devicesUsedBarPlot"),
      plotOutput("sportEngagementBarPlot")
    )
      
  ),
# Third Page
tabPanel("Multivariate Analysis",
         fluidPage(
           tags$head(
             tags$style(HTML("
          h2 {
            text-align: center;
            color: purple; /* Change to the desired color */
          }
        "))
           ),
           h2("Multivariate Analysis with Boxplots"),
           plotOutput("studyTimeBoxplot"),
           plotOutput("averageMarkBoxplot"),
           plotOutput("educationSatisfactionBoxplot"),
           plotOutput("genderGamingBoxplot"),
           plotOutput("groupedBarChart1"),
           plotOutput("groupedBarChart2")
         )
),

# FourthPage
tabPanel("Classification",
         fluidPage(
           tags$head(
             tags$style(HTML("
          h2 {
            text-align: center;
            color: purple; /* Change to the desired color */
          }
        "))
           ),
           h2("Classification Analysis"),
           plotOutput("comparisonPlot")
         )
),
# FifthPage
tabPanel("Regression",
         fluidPage(
           tags$head(
             tags$style(HTML("
          h2 {
            text-align: center;
            color: purple; /* Change to the desired color */
          }
        "))
           ),
           h2("Regression Analysis"),
           plotOutput("comparisonPlot")
         )
)
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {

    sys_data <- read.csv("/OnEduSystem/online_edu_sys.csv")
    
    # Plot histograms
    output$ageHistogram <- renderPlot({
      hist(sys_data$age_years, col = "skyblue", border = "black", main = "Histogram of Age Distribution", xlab = "Age", ylab = "Frequency")
    })
    
    output$studyTimeHistogram <- renderPlot({
      hist(sys_data$study_time, col = "skyblue", border = "black", main = "Study Time Distribution", xlab = "Study Time (Hours)", ylab = "Frequency")
    })
    
    output$socialMediaTimeHistogram <- renderPlot({
      hist(sys_data$social_media_time, col = "skyblue", border = "black", main = "Social Media Time Distribution", xlab = "Social Media Time (Hours)", ylab = "Frequency")
    })
    
    output$internetFacilityHistogram <- renderPlot({
      hist(sys_data$internet_facility, col = "skyblue", border = "black", main = "Internet Facility Distribution", xlab = "Internet facility", ylab = "Frequency")
    })
    
    # Plot bar plots
    output$economicStatusBarPlot <- renderPlot({
      barplot(table(sys_data$economic_status),
              main = "Economic Status Distribution",
              xlab = "Economic Status",
              ylab = "Frequency",
              col = "skyblue",
              border = "black"
      )
    })
    
    output$homeLocationBarPlot <- renderPlot({
      barplot(table(sys_data$home_location),
              main = "Home Location Distribution",
              xlab = "Home Location",
              ylab = "Frequency",
              col = "skyblue",
              border = "black"
      )
    })
    
    output$devicesUsedBarPlot <- renderPlot({
      barplot(table(sys_data$devices_used),
              main = "Used Device Distribution",
              xlab = "Devices Used",
              ylab = "Frequency",
              col = "skyblue",
              border = "black"
      )
    })
    
    output$sportEngagementBarPlot <- renderPlot({
      barplot(table(sys_data$sport_engagement),
              main = "Sports Engagement Distribution",
              xlab = "Sports Engagement",
              ylab = "Frequency",
              col = "skyblue",
              border = "black"
      )
    })
    
    # Plot boxplots
    output$studyTimeBoxplot <- renderPlot({
      ggplot(sys_data, aes(x = level_of_education, y = study_time, fill = level_of_education)) +
        geom_boxplot() +
        labs(title = "Study Time by Level of Education",
             x = "Level of Education",
             y = "Study Time") +
        theme_minimal()
    })
    
    output$averageMarkBoxplot <- renderPlot({
      ggplot(sys_data, aes(x = average_mark_score, y = social_media_time, fill = average_mark_score)) +
        geom_boxplot() +
        labs(title = "Average Mark Score by Social Media Time",
             x = "Average Mark Score",
             y = "Social Media Time") +
        theme_minimal()
    })
    
    # Plot boxplots
    output$educationSatisfactionBoxplot <- renderPlot({
      ggplot(sys_data, aes(x = level_of_education, y = study_time, fill = satisfaction_level)) +
        geom_boxplot() +
        labs(title = "Study Time by Level of Education and Satisfaction",
             x = "Level of Education",
             y = "Study Time",
             fill = "Satisfaction Level") +
        theme_minimal()
    })
    
    output$genderGamingBoxplot <- renderPlot({
      ggplot(sys_data, aes(x = gender, y = age_years, fill = gaming_interest)) +
        geom_boxplot() +
        labs(title = "Average Mark Score by Gender and Gaming Interest",
             x = "Gender",
             y = "Average Mark Score",
             fill = "Gaming Interest") +
        theme_minimal()
    })
    
    # Create a table for performance level, internet facility, and separate room study
    performance_internet_room <- table(sys_data$performance_level, sys_data$internet_facility, sys_data$separate_room_study)
    
    # Convert to data frame
    performance_internet_room_df <- as.data.frame.table(performance_internet_room)
    
    # Plot grouped bar chart
    output$groupedBarChart1 <- renderPlot({
      ggplot(performance_internet_room_df, aes(x = Var1, y = Freq, fill = Var2)) +
        geom_bar(stat = "identity", position = "dodge") +
        facet_wrap(~Var3, scales = "free_x") +
        labs(x = "Performance Level", y = "Frequency", fill = "Variables") +
        ggtitle("Performance Level by Internet Facility and Separate Room Study")
    })
    
    # Create a table for performance level, sport engagement, and elder supervising
    performance_sport_elder <- table(sys_data$performance_level, sys_data$sport_engagement, sys_data$elder_supervising)
    
    # Convert to data frame
    performance_sport_elder_df <- as.data.frame.table(performance_sport_elder)
    
    # Plot grouped bar chart
    output$groupedBarChart2 <- renderPlot({
      ggplot(performance_sport_elder_df, aes(x = Var1, y = Freq, fill = Var2)) +
        geom_bar(stat = "identity", position = "dodge") +
        facet_wrap(~Var3, scales = "free_x") +
        labs(x = "Performance Level", y = "Frequency", fill = "Variables") +
        ggtitle("Performance Level by Sport Engagement and Elder Supervising")
    })
    
    
    # Convert categorical variables to factors
    sys_data <- sys_data %>%
      mutate(across(where(is.character), as.factor))
    
    # Scale numeric variables
    numeric_cols <- c("age_years", "number_of_subjects", "family_size", "internet_facility",
                      "study_time", "sleep_time", "social_media_time", "online_mode_interaction",
                      "performance_level")
    
    sys_data[numeric_cols] <- lapply(sys_data[numeric_cols], scale_numeric)
    
    # Data Split
    set.seed(123)
    train_indices <- sample(1:nrow(sys_data), 0.7 * nrow(sys_data))
    train_data <- sys_data[train_indices, ]
    test_data <- sys_data[-train_indices, ]
    
    # Define target and predictors
    target_variable <- "satisfaction_level"
    predictors <- setdiff(names(train_data), target_variable)
    
    # Train the random forest model
    rf_model <- randomForest(as.formula(paste(target_variable, "~ .")), data = train_data, ntree = 100, mtry = 5)
    
    # Train Decision Tree
    dt_model <- rpart(as.formula(paste(target_variable, "~ .")), data = train_data, method = "class")
    
    # Evaluate models
    rf_predictions <- predict(rf_model, newdata = test_data)
    dt_predictions <- predict(dt_model, newdata = test_data, type = "class")
    
    # Calculate metrics
    confusion_rf <- table(rf_predictions, test_data$satisfaction_level)
    confusion_dt <- table(dt_predictions, test_data$satisfaction_level)
    
    accuracy_rf <- sum(diag(confusion_rf)) / sum(confusion_rf)
    precision_rf <- confusion_rf[2, 2] / sum(confusion_rf[, 2])
    recall_rf <- confusion_rf[2, 2] / sum(confusion_rf[2, ])
    f1_score_rf <- 2 * (precision_rf * recall_rf) / (precision_rf + recall_rf)
    
    accuracy_dt <- sum(diag(confusion_dt)) / sum(confusion_dt)
    precision_dt <- confusion_dt[2, 2] / sum(confusion_dt[, 2])
    recall_dt <- confusion_dt[2, 2] / sum(confusion_dt[2, ])
    f1_score_dt <- 2 * (precision_dt * recall_dt) / (precision_dt + recall_dt)
    
    # Create a dataframe for metrics comparison
    comparison_data <- data.frame(
      Model = c("Random Forest", "Decision Tree"),
      Accuracy = c(accuracy_rf, accuracy_dt),
      Precision = c(precision_rf, precision_dt),
      Recall = c(recall_rf, recall_dt),
      F1_Score = c(f1_score_rf, f1_score_dt)
    )
    
    # Reshape data using pivot_longer
    comparison_data_long <- comparison_data %>%
      pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")
    
    # Create a grouped bar plot for metrics comparison
    output$comparisonPlot <- renderPlot({
      ggplot(comparison_data_long, aes(x = Model, y = Value, fill = Metric)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Model Performance: RF vs. DT", y = "Metric Value", fill = "Metric") +
        theme_minimal() +
        facet_wrap(~Metric, scales = "free_y")
    })
    
   
}

# Run the application 
shinyApp(ui = ui, server = server)
