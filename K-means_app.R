# Load necessary libraries
library(shiny)
library(dplyr)
library(fastDummies)
library(factoextra)
library(ggpubr)
library(tidyr)
library(ggplot2)
library(plotrix)
library(cluster)  # For silhouette score


# Define the UI
ui <- fluidPage(
  
  # App title
  titlePanel("Customer Segmentation with K-Means Clustering"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = ".csv"),
      numericInput("clusters_num", "Number of Clusters (K):", value = 6, min = 1, max = 20),
      actionButton("process", "Process Data")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Optimal Clusters", plotOutput("elbowPlot")),
                  tabPanel("Clusters", tableOutput("clusterCenters")),
                  tabPanel("PCA Plot", plotOutput("pcaPlot")),
                  tabPanel("Silhouette Plot", plotOutput("silhouettePlot"), verbatimTextOutput("avgSilhouette")),
                  tabPanel("Gender Visualization", plotOutput("genderBarPlot"), plotOutput("genderPie")),
                  tabPanel("Age Distribution", plotOutput("ageDistPlot"))
      )
    )
  )
)

# Defining server logic
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
  
  # Processing and clustering the dataset after clicking the button
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
    
    # Determine and visualize the optimal number of clusters using the elbow method
    output$elbowPlot <- renderPlot({
      fviz_nbclust(dataset, kmeans, method = "wss") + labs(subtitle = "Elbow Method")
    })
    
    # Applying K-Means clustering
    kmeans_model <- kmeans(dataset, centers = input$clusters_num, iter.max = 10)
    
    # Displaying cluster centers
    output$clusterCenters <- renderTable({
      kmeans_model$centers
    })
    
    # Adding cluster information to the dataset
    dataset <- cbind(dataset, cluster = kmeans_model$cluster)
    
    # PCA and plotting the clusters
    output$pcaPlot <- renderPlot({
      pca <- prcomp(dataset[, -ncol(dataset)], center = TRUE, scale. = TRUE)
      pca_data <- data.frame(pca$x, cluster = as.factor(kmeans_model$cluster))
      
      # Visualizing the clusters using PCA
      ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
        geom_point() +
        labs(x = "Principal Component 1", y = "Principal Component 2", color = "Cluster") +
        ggtitle("PCA Plot with K-Means Clusters")
    })
    
    # Calculating and visualize the silhouette score
    output$silhouettePlot <- renderPlot({
      # Compute the silhouette score for K-Means clustering
      silhouette_kmeans <- silhouette(kmeans_model$cluster, dist(dataset[, -ncol(dataset)]))
      
      # Visualizing the silhouette plot
      fviz_silhouette(silhouette_kmeans) +
        ggtitle(paste("Silhouette Plot for K-Means (K =", input$clusters_num, ")"))
    })
    
    # Displaying the average silhouette score
    output$avgSilhouette <- renderPrint({
      silhouette_kmeans <- silhouette(kmeans_model$cluster, dist(dataset[, -ncol(dataset)]))
      avg_silhouette_kmeans <- mean(silhouette_kmeans[, 3])
      
      paste("Average Silhouette Score for K =", input$clusters_num, ": ", round(avg_silhouette_kmeans, 3))
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
