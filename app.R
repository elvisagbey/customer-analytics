################################################################
#########   Kmeans Customer Segmentation              ##########
#########                                             ##########
################################################################

# Packages

suppressPackageStartupMessages(library(shiny)) 
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(factoextra))
suppressPackageStartupMessages(library(cluster))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(googleVis))
suppressPackageStartupMessages(library(psych))


# User Interface

ui <- fluidPage( theme = NULL,

    ##Application title
    
    titlePanel("Customer Segmentation.io"
       ),

    ## Sidebar with a inputs
    
    sidebarLayout(
        sidebarPanel(
            
            ### file upload 
            
            fileInput(
                inputId = "userDataset",
                label = ">1. Upload csv file of your customer data",
                placeholder = "No file selected",
                accept = c(
                    "text/csv",
                    "text/coma-separated-values, text/plain",
                    ".csv"
                )
            ),
            
            ### input for selecting clustering algorithm
            
           selectInput(
               inputId = "algorithm",
               label = ">2. Choose clustering algorithm",
               choices = list("Kmeans (Hartigan-Wong)" = "Kmeans")
           ),
           
           ### input for selecting desired number of clusters
           
           numericInput(
               inputId = "noSegments",
               label = ">3. Indicate desired no of segments",
               value = 0,
               min = 2,
               max = 12,
               step = 1
            ),
           
           ### conditional input for displaying next button
           
           conditionalPanel(
               condition = "input.noSegments > 1",
               actionButton(
                   inputId = "nextButton",
                   label = ">4. Next"
               )
           ),
           
           ### conditional input for selecting customer attributes and execute button
           
           conditionalPanel(
               condition = "input.nextButton > 0",
              
               #### input for selecting customer attributes
               
               uiOutput(
                   outputId = "userVariables"
               ),
               
               #### input for selecting data labels
               
               uiOutput(
                   outputId = "dataLabel"
               ),
               
               #### actino button to run segmentation
               
               actionButton(
                   inputId = "executeButton",
                   label = strong(">7. Run/re-run segmentation")
               )
           ),
           
           ### Contitional Panel to execute bootstrap
           
           # conditionalPanel(
           #     condition = "input.executeButton > 0",
           #     sliderInput(
           #         inputId = "boostrap",
           #         label = ">8. Use slider to adjust number of simulations.
           #         NOTE: Higher values may give better result but slow down the visualization",
           #         min = 5,
           #         max = 100,
           #         step = 5,
           #         value = 0
           #     ),
           #     
           #     #### Bootstrap action button
           #     actionButton(
           #         inputId = "startBoostrap",
           #         label = ">9. Adjust"
           #     )
           # ),
           
           ### Sample Dataset download link
           
           h3(
               a("Try it out! Click here to download a sample dataset", 
                 href = "https://drive.google.com/uc?export=download&id=1QOMM144tkdIyjfm2E46P80fgLMzosQBh")
           )


        ),
        
        
        ### Defining outputs for mainpanel

        mainPanel(
           tabsetPanel(
               
               #### First tab - Snapshot of dataset
               
               tabPanel(
                   title = "Snapshot of your customer data",
                   h4(strong("Quickview of top 5 rows of your dataset:")),
                   tableOutput("dataset"),
                   h4(strong("Quickview of last 5 rows of your dataset:")),
                   tableOutput("datasetDown"),
                   h4(strong("Some descriptive statistics:")),
                   verbatimTextOutput("descriptiveStats")
               ),
               
               #### Second tab - Customer segmentation results
               
               tabPanel(
                   title = "Customer Segmentation Results",
                   h4(strong("Customer segment and corresponding size:")),
                   tableOutput("segmentSize"),
                   h4(strong("Bar plot of features")),
                   htmlOutput("barPlot"),
                   h4(strong("Summary of Cluster Characteristics")),
                   verbatimTextOutput("segmentSummaryStats"),
                   h4(strong("Quick view of segmented dataset:")),
                   dataTableOutput("segmentation")
               ),
               
               #### Third tab - Statistical Results
               
               tabPanel(
                   title ="Statistical Results",
                   h4(strong("Attributes used as bases for segmentation:")),
                   tableOutput("attributes"),
                   h4(strong("K means model output as displayed in R studio:")),
                   verbatimTextOutput("modeloutput")
               ),
               
               #### Fourth tap - Visualization
               
               tabPanel(
                   title = "Visualization",
                   plotOutput(outputId = "clusterPlot"),
                   h4(strong("Optimal number of clusters using elbow method")),
                   plotOutput(outputId = "elbow"),
                 #  plotOutput(outputId = "optimalK"),
                   h4(strong("Sihouette Plot")),
                   plotOutput(outputId = "silPlot")
               )
           )
        )
    )
)



# Random number generator

set.seed(123)


# Server Logic

server <- function(input, output, session) {

    ## create reactive data object of the uploaded data
    
    userDataset <- reactive({
        
        req(input$userDataset)
        userDataset <- read.csv(input$userDataset$datapath) %>% 
            na.omit()
    })
    
   
    ## create a reactive UI input for the user to select 
    ## customer attributes which form basis for segmentation
    
    output$userVariables <- renderUI({
        variableNames <- names(userDataset())
        checkboxGroupInput(
            inputId = "variables",
            label = ">5. Select customer attributes to use as basis for 
            segmentation. (Do not include data labels like customer ID or name)",
            choices = variableNames,
            
        )
    })
    
    
    ## create a reactive UI input for the user to select dataset 
    ## labels like customer id etc which form basis for segmentation
    
    output$dataLabel <- renderUI({
        variableNames <- names(userDataset())
        checkboxGroupInput(
            inputId = "dataLabels",
            label = ">6. Select dataset label if any. e.g customer ID, 
            customer first name or customer last name)",
            choices = variableNames
            
        )
    })
    
    ## Make Matrix object of attributes to be used in clustering
    
    selectedAttributeDataset <- reactive({
        
        selectedAttributeDataset <- select(userDataset(),
                                    input$variables) %>% 
            as.list() %>% 
            unlist() %>% 
            matrix(nrow = nrow(userDataset()),
                   ncol = length(input$variables),
                   byrow = FALSE) %>% 
            `colnames<-`(input$variables)
    
    })
    
    
    
    ## Display a snapshot of the uploaded dataset
    
    ### top view
    
    output$dataset <- renderTable({
        
        input$executeButton
        isolate(head(userDataset(),5))
    })
    
    ### bottom view
    
    output$datasetDown <- renderTable({
        
        input$executeButton
        isolate(tail(userDataset(),5))
    })
    
    ### descriptive Stats
    
    output$descriptiveStats <- renderPrint({
        input$executeButton
        isolate(
            summary(userDataset())
        )
    })
    
    ## Summary tab
    
    output$attributes <- renderTable({
        input$executeButton
       isolate(input$variables)
    }, striped = T, hover = T, bordered = T, colnames = F)
    
    
    ### K-means clustering
    
    kmeanUser <- reactive({
        
        input$executeButton
        isolate(
            kmeanUser <- kmeans(
                x = scale(selectedAttributeDataset()),
                centers = input$noSegments,
                nstart = 50,
                algorithm = "Hartigan-Wong"
            )
        )

    })
    
    
    ### model output
    
    output$modeloutput <- renderPrint({
        input$executeButton
        isolate(print(kmeanUser()))
    })
    
    ## customer segmentation tab
    
    ### segment size
    
    output$segmentSize <- renderTable({
        
        input$executeButton
        
        isolate(
            
            data.frame(
            "Customer_segment" = 1:input$noSegments,
            "Segment_size" = kmeanUser()$size) %>% 
                mutate("Percent %" = (Segment_size/sum(Segment_size))*100)
           
        
        )
    }, striped = T, bordered = T, hover = T)
    
    
    
    
    ### Bar plot of features
    
    
    gvisData <- reactive({
        kmeanUser()$centers %>% as.data.frame() %>%  
            mutate("Clusters" = 1:length(kmeanUser()$size))
    })
    
    output$barPlot <- renderGvis({
        input$executeButton
        
        isolate(
            
            kmeanUser()$centers %>% 
                as.data.frame() %>%  
                mutate("Clusters" = 1:length(kmeanUser()$size)) %>%
                gvisColumnChart(
                xvar = "Clusters",
                yvar = colnames(kmeanUser()$centers)
            )
        )
    })

    
    
    
    
#    output$barPlot <- renderPlot({
        
#        input$executeButton
#        isolate(
            
#            barplot(t(kmeanUser()$centers), 
#                    xlab = "clusters", 
#                    ylab = "value", 
#                    beside = T,
#                    col = brewer.pal(n = length(colnames(kmeanUser()$centers)), 
#                                     name = "Spectral"))
#            )
        
#        legend("topright", ncol = 2, 
#               legend = colnames(kmeanUser()$centers), 
#               fill = brewer.pal(n = length(colnames(kmeanUser()$centers)), 
#                                 name = "Spectral"))
#    })
    
    
    ### segmentation
    
    output$segmentation <- renderDataTable({
        
        input$executeButton
        isolate(
            
            if (class(input$dataLabels) == "character"){
                cbind(select(userDataset(),input$dataLabels),
                      "segment" = kmeanUser()$cluster)
            }else{
                cbind(userDataset(), 
                      "segment" = kmeanUser()$cluster)
            }
            
        )
    })
    
    
    ### Summary Stats of Segment
    
    segmentedDataset <- reactive({
         mutate(userDataset(), "Cluster" = kmeanUser()$cluster)
    })
    
    
        
    clusterSummary <- function(){
        
        mylist  <- list()
        
        for (i in seq_along(kmeanUser()$size)) {
            
            mylist[[i]] <- segmentedDataset() %>%
                filter(Cluster == i) %>% 
                select(input$variables) %>% 
                summary()
        }
       return(mylist)
    }
    
    
    output$segmentSummaryStats <- renderPrint({
        
        input$executeButton
        
        isolate(

          clusterSummary()
        )
    })
    

    
    
    #summary stats of segments
    
    
    
    ## visualization
    
    ## cluster plot
    
    output$clusterPlot <- renderPlot({
        input$executeButton
        isolate(
            fviz_cluster(
                object = kmeanUser(),
                data = selectedAttributeDataset(),
                ellipse.type = "norm",
                xlab = FALSE,
                ylab = FALSE
            )
        )
    })
    
    #optimal number of clusters
    


    
    output$elbow <- renderPlot({
        
        
        input$executeButton
        
        withProgress(message = "Making plot", detail = "This may take a while...", value = 0, {
            incProgress(1/10)})
        isolate(
        fviz_nbclust(
            x = scale(selectedAttributeDataset()),
            FUNcluster = kmeans,
            method = "wss"
        )
        )
    })
    
    
    
    # clusGapStat <- reactive({
    #     
    #     clusGap(
    #         x= scale(selectedAttributeDataset()),
    #         FUNcluster = kmeans,
    #         K.max = 10,
    #         B = input$boostrap,
    #         nstart = 50
    #     )
    # })
    # 
    # output$optimalK <- renderPlot({
    # 
    #     input$startBoostrap
    #     isolate(
    #        fviz_gap_stat(
    #            
    #            clusGapStat()
    # 
    #         )
    #     )
    # })
    
    
    ### Silhouette Plot
    
    sil <- reactive({
        silhouette(kmeanUser()$cluster, dist(selectedAttributeDataset()))
    })
    
    
    output$silPlot <- renderPlot({
        
        input$executeButton
       isolate(
           plot(
            sil(), 
            col = 1:length(kmeanUser()$size)
        )
       )
    })
    
    
    
   
}

# Run the application 
shinyApp(ui = ui, server = server)
