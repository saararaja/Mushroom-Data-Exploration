library(shiny)
library(tidyverse)
library(readxl)
library(ggplot2)
library(gridExtra)
library(caret)
library(tree)
library(class)
library(cluster)
library(ggplot2)
library(reshape2)
library(dendextend)

################ Static Code ######################

#Read in the raw data
newData <- readxl::read_excel("./mushrooms.xlsx") %>%
  rename('cap_shape' = `cap-shape`,
         'cap_surface' = `cap-surface`,
         'cap_color' = `cap-color`,
         'gill_attachment' = `gill-attachment`,
         'gill_spacing' = `gill-spacing`,
         'gill_size' = `gill-size`,
         'gill_color' = `gill-color`,
         'stalk_shape' = `stalk-shape`,
         'stalk_root' = `stalk-root`,
         'stalk_surface_above_ring' = `stalk-surface-above-ring`,
         'stalk_surface_below_ring' = `stalk-surface-below-ring`,
         'stalk_color_above_ring' = `stalk-color-above-ring`,
         'stalk_color_below_ring' = `stalk-color-below-ring`,
         'veil_type' = `veil-type`,
         'veil_color' = `veil-color`,
         'ring_number' = `ring-number`,
         'ring_type' = `ring-type`,
         'spore_print_color' = `spore-print-color`)

#Make all the columns factors
col_names <- names(newData)
newData[,col_names] <- lapply(newData[,col_names], factor)

#Set the seed
set.seed(5)

#80% of data in train, 20% in test
train_index <- sample(1:nrow(newData), size = nrow(newData)*0.8)
test_index <- setdiff(1:nrow(newData), train_index)

#Final Train and Test Sets
Train <- newData[train_index, ]
Test <- newData[test_index, ]

#Remove veil-type because only one level
Train <- Train %>% select(-veil_type)
Test <- Test %>% select(-veil_type)

## Modeling
#Fit KNN on Train Set with Cross Validation Results
trctrl <- trainControl(method = "repeatedcv", number = 10)

knnFit <- train(class ~., data = Train, method = "knn",
                trControl = trctrl,
                tuneLength = 10
)

#Fit Classification Tree on Train Set with Cross Validation Results
tree.fit <- tree(class ~., data = Train)
prune_tree <- cv.tree(tree.fit, FUN=prune.misclass)

## Clustering
#Create dissimilarity matrix using method = gower since categorical data
gower.dist <- daisy(newData, metric = c("gower"))

#Agglomerative clustering
agg.clust <- hclust(gower.dist, method = "complete")

#Create dendrogram
dendro <- as.dendrogram(agg.clust)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  
  ################# Raw Data Tab Elements ##################
  
  #Dynamic Title for Data Tab
  output$datatitle <- renderUI({
    text <- paste0(input$subset, ":")
    h2(text)
  })
  
  #Subset Reactive Context
  
  getSubset <- reactive({
    
    if(input$subset=="Edible Mushrooms Data"){
      subData <- newData %>% filter(class == "e")
    } else if (input$subset=="Poisonous Mushrooms Data"){
      subData <- newData %>% filter(class == "p")
    } else {subData <- newData}
    
    subData
  })
  
  
  #Data Table for Data Tab
  output$rawdata <- renderDataTable({
    getSubset()
  })
  
  #Download the data
  output$downloadData <- downloadHandler(filename = function(){
    paste0(input$subset, ".csv")
  },
  content = function(file){
    write.csv(getSubset(), file)
  })
  
  
  
  ########################## Data Exploration Elements ############################
  
  #Plot Reactive Context
  plotInput <- reactive({
    
    #Variable Conversions
    onevar <- sym(input$one)
    twovar_first <- sym(toString(input$two[1]))
    twovar_second <- sym(toString(input$two[2]))
    
    #One Variable Graph
    if(input$number=="One Variable Summary"){
      if(input$split){
        g <- ggplot(newData, aes(x = !! onevar, fill=class))
        g + geom_bar(position = "dodge") + ggtitle(paste0("Frequency of ", input$one, " by Mushroom Classification")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
      } else {
        g <- ggplot(newData, aes(x = !! onevar))
        g + geom_bar(fill = 'violetred') + ggtitle(paste0("Frequency of ", input$one)) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
      }
    }
    #Two Variable Graph
    else {
      if(is.null(input$two[1])){
        return()
      } else if (is.na(input$two[2])){
        return()
      } else {
        g <- ggplot(newData, aes(x = !! twovar_first, fill = !! twovar_second))
        g + geom_bar(position = "dodge") + ggtitle(paste0("Frequency of ", input$two[1], " by ", input$two[2])) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
      }
    }
  })
  
  
  #Create Plot
  output$bars <- renderPlot({
    plotInput()
  })
  
  #Save the Plot
  output$saveplot <- downloadHandler(filename = function(){
    paste0("Mushroom Bar Plot.png")
  },
  content = function(file){
    ggsave(file, plotInput())
  })
  
  #Subset Reactive Context
  TableSubset <- reactive({
    
    if(input$number=="One Variable Summary"){
      if(input$split){
        Value <- newData %>% select(class, !!input$one)
        
      } else {
        Value <- newData %>% select(!!input$one)
      }
    }
    #Two Variable Table
    else {
      if(is.null(input$two[1])){
        return()
      } else if (is.na(input$two[2])){
        return()
      } else {
        Value <- newData %>% select(!!input$two[1], !!input$two[2])
      }
    }
    #Return
    Value
  })
  
  #Frequency Reactive Context
  Freq <- reactive({
    Value <- TableSubset()
    table(Value)
  })
  
  
  
  #Frequency Table
  output$freq <- renderTable({
    Freq()
  })
  
  
  #Download the data
  output$downloadData2 <- downloadHandler(filename = function(){
    paste0("Mushroom Data Subset.csv")
  },
  content = function(file){
    write.csv(TableSubset(), file)
  })
  
  
  #Chi Squared Content 1
  output$chi_results <- renderText({
    
    #Get the Data Subset we are currently looking at
    Data <- Freq()
    
    #Make chi squared with one var and class if checkbox is ticked
    if(input$chi){
      results <- chisq.test(Data)
      text <- paste0("The chi squared statistic between mushroom class and ", input$one, " is ", round(results$statistic, 2), " and the p-value is ", format.pval(results$p.value, eps = .001, digits = 2), ".")
      if(results$p.value<.05){
        decision <- paste0("We reject the null hypothesis that mushroom class is independent of ", input$one, " at the alpha=0.05 significance level.")
        text_final <- paste(text, decision, sep = "\n")
      } else {
        decision <- paste0("We fail to reject the null hypothesis that mushroom class is independent of ", input$one, " at the alpha=0.05 significance level.")
        text_final <- paste(text, decision, sep = "\n")
      }
      #Return
      text_final
    }
    
  })
  
  #Chi Squared Content 2
  output$chi_results2 <- renderText({
    
    #Get the Data Subset we are currently looking at
    Data <- Freq()
    
    if(input$chi2){
      results <- chisq.test(Data)
      text <- paste0("The chi squared statistic between ", input$two[1], " and ", input$two[2], " is ", round(results$statistic, 2), " and the p-value is ", format.pval(results$p.value, eps = .001, digits = 2), ".")
      if(results$p.value<.05){
        decision <- paste0("We reject the null hypothesis that ", input$two[1], " is independent of ", input$two[2], " at the alpha=0.05 significance level.")
        text_final <- paste(text, decision, sep = "\n")
      } else {
        decision <- paste0("We fail to reject the null hypothesis that ", input$two[1], " is independent of ", input$two[2], " at the alpha=0.05 significance level.")
        text_final <- paste(text, decision, sep = "\n")
      }
      #Return
      text_final
    }
  })
  
  
  #Update to uncheck checkboxes if input$number is selected to One Variable
  observe({
    if (input$number == "One Variable Summary"){
      #uncheck chi2
      updateCheckboxInput(session, inputId = "chi2", value = FALSE)
    }
  })
  
  #Update to uncheck checkboxes if input$number is selected to Two Variables
  observe({
    if (input$number == "Two Variable Summary"){
      #uncheck chi
      updateCheckboxInput(session, inputId = "chi", value = FALSE)
      #uncheck split
      updateCheckboxInput(session, inputId = "split", value = FALSE)
    }
  })
  
  #Add another update that if you change the variables for the two var selection it also unchecks the checkbox
  
  ############################## Clustering Page #################################
  
  #Create reactive color pallete
  colorPalette <- reactive({
    if (input$color =="Neutral"){
      col_list <- c("darkslategray", "darkslategray4", "darkslategray3", "gold3", "darkcyan", "cyan3",
                    "darkolivegreen4", "darkolivegreen3", "darkseagreen4", "darkseagreen3")
    } else {
      col_list <- c("mediumorchid", "palevioletred3", "mediumaquamarine", "cornflowerblue", "yellowgreen",
                    "seagreen3", "indianred2", "darkturquoise", "chartreuse3", "lightcoral")
    }
  })

  #Create Dendrogram
  output$Dendro <- renderPlot({
    #Get Color Palette
    colors <- colorPalette()

    #Create Specifications
    dendro.col <- dendro %>%
      set("branches_k_color", k = input$clust, value = colors[1:input$clust]) %>%
      set("branches_lwd", 0.6) %>%
      set("labels_colors", value = c("darkslategray")) %>%
      set("labels_cex", 0.5)

    #Make the dendrogram
    ggd1 <- as.ggdend(dendro.col)

    #Create the plot
    ggplot(ggd1, theme = theme_minimal()) +
      labs(x = "Num. observations", y = "Height", title = paste0("Dendrogram, k = ", input$clust)) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 18))

  })

  ################################ Modeling Page #################################
  
  
  


  #CV Plots
  output$cv <- renderPlot({

    #KNN Selection
    if(input$model=="KNN"){
      knn_plot <- plot(knnFit$results[,1], knnFit$results[,2], xlab = "K value",
                       ylab = "Classification Accuracy",
                       main = "Training Accuracy Based on Number of Neighbors",
                       type = "o", col = "blue",
                       panel.first = grid(lty = 1))
    }
    #Tree Selection
    else {
      tree_plot <- plot(prune_tree$size, prune_tree$dev, xlab = "Number of Nodes",
                        ylab = "Classification Deviance",
                        main = "Training Misclassification Based on Number of Nodes",
                        type = "o", col = "blue",
                        panel.first = grid(lty = 1))
    }
  })


  #Reactive KNN Fit
  KNN_Fit <- reactive({
    knnctrl <- trainControl(method = "none")
    knnFit <- train(class ~., data = Train, method = "knn",
                    trControl = knnctrl,
                    tuneGrid=data.frame(k=input$K)
    )
    knnFit
  })

  #Reactive Tree Fit
  Tree_Fit <- reactive({
    tree.fit2 <- prune.misclass(tree.fit, best = input$nodes)
  })

  #Reactive KNN Prediction
  KNN_Pred <- reactive({
    knnFit <- KNN_Fit()
    #Predict on the test data set based on model fit
    knnPred <- predict(knnFit, newdata = Test)
  })

  #Reactive Tree Prediction
  Tree_Pred <- reactive({
    treeFit <- Tree_Fit()
    #Predict on the test data set based on model fit
    treePred <- predict(treeFit, newdata = Test, type="class")
  })

  #Confusion Matrix
  output$confusion <- renderPrint({
    #KNN
    if(input$model=="KNN"){
      knnPred <- KNN_Pred()
      #Create confusion matrix
      table(knnPred, Test$class)
    }
    #Tree
    else{
      treePred <- Tree_Pred()
      #Create confusion matrix
      table(treePred, Test$class)
    }
  })

  #Text
  output$acc <- renderText({
    #KNN
    if(input$model=="KNN"){
      knnPred <- KNN_Pred()
      paste0("With K = ", input$K, ", the Misclassification Error rate is: ", round(mean(knnPred != Test$class),5), ".")
    }
    #Tree
    else {
      treePred <- Tree_Pred()
      paste0("With # Nodes = ", input$nodes, ", the Misclassification Error rate is: ", round(mean(treePred != Test$class),5), ".")
    }
  })

  #Make Prediction
  output$outcome <- renderText({
    if(input$new){
      #Make the new data frame
      Temp <- data.frame(
        cap_shape = input$cap_shape,
        cap_surface = input$cap_surface,
        cap_color = input$cap_color,
        bruises = input$bruises,
        odor = input$odor,
        gill_attachment = input$gill_attachment,
        gill_spacing = input$gill_spacing,
        gill_size = input$gill_size,
        gill_color = input$gill_color,
        stalk_shape = input$stalk_shape,
        stalk_root = input$stalk_root,
        stalk_surface_above_ring = input$stalk_surface_above_ring,
        stalk_surface_below_ring = input$stalk_surface_below_ring,
        stalk_color_above_ring = input$stalk_color_above_ring,
        stalk_color_below_ring = input$stalk_color_below_ring,
        veil_color = input$veil_color,
        ring_number = input$ring_number,
        ring_type = input$ring_type,
        spore_print_color = input$spore_print_color,
        population = input$population,
        habitat = input$habitat
      )
      #KNN
      if(input$model=="KNN"){
        #Bring in the KNN fit
        knnFit <- KNN_Fit()
        #Predict on the user defined data set based on model fit
        knnPred <- predict(knnFit, newdata = Temp)
        #Text
        if (knnPred[1]=='e'){
          text <- paste0("Prediction with K = ", input$K, ": This mushroom is edible")
        } else {
          text <- paste0("Prediction with K = ", input$K, ": This mushroom is poisonous")
        }
      }
      #Tree
      else {
        #Bring in the Tree fit
        treeFit <- Tree_Fit()
        # Make some adjustmusts to Temp column types
        col_names2 <- names(Temp)
        Temp[,col_names2] <- lapply(Temp[,col_names2], factor)
        #Predict on the user defined data set based on model fit
        treePred <- predict(treeFit, newdata = Temp, type="class")
        #Text
        if (treePred[1]=='p'){
          text <- paste0("Prediction with # of Nodes = ", input$nodes, ": This mushroom is poisonous")
        } else {
          text <- paste0("Prediction with # of Nodes = ", input$nodes, ": This mushroom is edible")
        }
      }
    }
  })




  
  
  
  
  
})
