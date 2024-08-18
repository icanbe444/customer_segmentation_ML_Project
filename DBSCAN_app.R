# Load necessary libraries
library(shiny)
library(dplyr)
library(fastDummies)
library(dbscan)
library(ggplot2)
library(tidyr)
library(cluster)  # For silhouette score

# Defining the UI
ui <- fluidPage(
  
  # App title
  titlePanel("Customer Segmentation with DBSCAN Clustering"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = ".csv"),
      numericInput("eps_value", "EPS Value:", value = 0.5, min = 0.1, max = 5, step = 0.1),
      numericInput("min_samples", "Min Points:", value = 5, min = 1, max = 50),
      actionButton("process", "Process Data")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Clusters", tableOutput("clusterTable")),
                  tabPanel("PCA Plot", plotOutput("pcaPlot")),
                  tabPanel("Silhouette Plot", plotOutput("silhouettePlot"), verbatimTextOutput("avgSilhouette")),
                  tabPanel("Gender Visualization", plotOutput("genderBarPlot"), plotOutput("genderPie")),
                  tabPanel("Age Distribution", plotOutput("ageDistPlot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reading the dataset
  datasetInput <- reactive({
    req(input$file1)
    data <- read.csv(input$file1$datapath, header = TRUE)
    
    # Data processing
    data <- data[, 2:9]
    data[data == ""] <- NA
    data <- data[complete.cases(data),]
    
    return(data)
    
  })
  
  # Summary of the dataset
  output$summary <- renderPrint({
    req(datasetInput())
    summary(datasetInput())
  })
    
    # Processing and clustering the dataset after button click
    observeEvent(input$process, {
      data <- datasetInput()
      
      # Building the dataset
      dataset <- data %>% select_if(is.numeric)
      character <- data %>% select_if(is.character)
      
      # Transforming character into numeric
      character <- dummy_cols(character, remove_most_frequent_dummy = TRUE)
      
      # Removing characters in the data
      dataset <- cbind(dataset, character[,6:18])
      
      # Scaling the dataset
      dataset[, 1: 16] <- scale(dataset[, 1: 16])
    
    
    
    
    
    # Applying DBSCAN clustering
    dbscan_model <- dbscan(dataset, eps = input$eps_value, minPts = input$min_samples)
    
    # Display the number of points in each cluster
    output$clusterTable <- renderTable({
      table(dbscan_model$cluster)
    })
    
    # Adding cluster information to the dataset
    dataset <- cbind(dataset, cluster = dbscan_model$cluster)
    
    # PCA and plotting the clusters
    output$pcaPlot <- renderPlot({
      pca <- prcomp(dataset[, -ncol(dataset)], center = TRUE, scale. = TRUE)
      pca_data <- data.frame(pca$x, cluster = as.factor(dbscan_model$cluster))
      
      # Visualizing the clusters using PCA
      ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
        geom_point() +
        labs(x = "Principal Component 1", y = "Principal Component 2", color = "Cluster") +
        ggtitle("PCA Plot with DBSCAN Clusters")
    })
    
    # Calculating and visualize the silhouette score for DBSCAN
    output$silhouettePlot <- renderPlot({
      # Exclude noise points (cluster = 0) for silhouette score calculation
      valid_clusters <- dbscan_model$cluster[dbscan_model$cluster != 0]
      valid_data <- dataset[dbscan_model$cluster != 0, -ncol(dataset)]  # Exclude noise points and cluster column
      
      # Check if there are valid clusters to compute the silhouette score
      if(length(unique(valid_clusters)) > 1) {
        silhouette_dbscan <- silhouette(valid_clusters, dist(valid_data))
        
        # Visualize the silhouette plot
        fviz_silhouette(silhouette_dbscan) +
          ggtitle(paste("Average Silhouette Score for DBSCAN: ", round(avg_silhouette_dbscan, 3))) +
          theme(legend.position = "bottom", legend.text = element_text(size = 8)) +
          guides(color = guide_legend(nrow = 1, override.aes = list(size = 3)))
        
      } else {
        plot.new()
        text(0.5, 0.5, "Not enough clusters to compute silhouette score", cex = 1.5)
      }
    })
    
    # Displaying the average silhouette score
    output$avgSilhouette <- renderPrint({
      # Exclude noise points (cluster = 0) for silhouette score calculation
      valid_clusters <- dbscan_model$cluster[dbscan_model$cluster != 0]
      valid_data <- dataset[dbscan_model$cluster != 0, -ncol(dataset)]  # Exclude noise points and cluster column
      
      # Checking if there are valid clusters to compute the silhouette score
      if(length(unique(valid_clusters)) > 1) {
        silhouette_dbscan <- silhouette(valid_clusters, dist(valid_data))
        avg_silhouette_dbscan <- mean(silhouette_dbscan[, 3])
        paste("Average Silhouette Score for DBSCAN: ", round(avg_silhouette_dbscan, 3))
      } else {
        "Not enough clusters to compute silhouette score"
      }
    })
    
    # Gender visualization
    output$genderBarPlot <- renderPlot({
      a <- table(data$Gender)
      barplot(a, main = "Using BarPlot to display Gender Comparison",
              ylab = "Count", xlab = "Gender",
              col = rainbow(length(a)),
              legend = rownames(a))
    })
    
    output$genderPie <- renderPlot({
      a <- table(data$Gender)
      pct <- round(a / sum(a) * 100)
      lbs <- paste(c("Female", "Male"), "", pct, "%", sep = " ")
      pie3D(a, labels = lbs, main = "Pie Chart Depicting Ratio of Female and Male")
    })
    
    # Age distribution
    output$ageDistPlot <- renderPlot({
      age_classes <- c("0-20", "21-40", "41-60", "61-80", "81-100")
      age_colors <- c("blue", "green", "orange", "red", "purple")
      
      hist(data$Age,
           breaks = c(0, 20, 40, 60, 80, 100),
           col = age_colors,
           main = "Histogram to Show Count of Age Class",
           xlab = "Age Class",
           ylab = "Frequency",
           labels = TRUE)
      
      legend("topright",
             legend = age_classes,
             fill = age_colors,
             title = "Age Classes")
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
