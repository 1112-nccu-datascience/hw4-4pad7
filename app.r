library(shiny)
library(ggbiplot)
library(ggplot2)
library(ca)

data(iris)

ui <- navbarPage("　🌺 data science HW4",
  sidebarLayout(
    sidebarPanel(
      #name/department/student number
      tags$img(src = "iris.png", width = "70%", height = "auto", style = "display: block; margin-left: auto; margin-right: auto;"),
      h4("department: 資碩計一", style = "font-family: 'times'; font-si20pt",  align = "center"),
      h4("student number: 111753214", style = "font-family: 'times'; font-si20pt",  align = "center"),
      h4("name: 思沛淇", style = "font-family: 'times'; font-si20pt",  align = "center")
    ),
    mainPanel(
      tabsetPanel(
        #CA
        tabPanel("CA plot",
          tabsetPanel(
            fluidRow(
              column(
                3,
                selectInput("x_var", "Select X Variable:", choices = colnames(iris)),
                selectInput("y_var", "Select Y Variable:", choices = colnames(iris))
              ),
              column(
                9,
                plotOutput("CA_plot")
              )
            )
          )
        ),
        
        #PCA
        tabPanel("PCA",
          tabsetPanel(
            #PCA1
            tabPanel("PCA plot",
              column(
                3,
                selectInput("x_axis", "X軸", choices = c("PC1", "PC2", "PC3", "PC4")),
                selectInput("y_axis", "Y軸", choices = c("PC1", "PC2", "PC3", "PC4"))
              ),
              column(
                9,
                plotOutput("pca_plot")
              )
            ),
            
            #PCA2
            tabPanel("input data",
              dataTableOutput("pca_input_data_table")
            ),
            
            #PCA3
            tabPanel("output data",
              dataTableOutput("pca_result_table")
            )
          )
        ),
        
        #histogram
        tabPanel("histogram",
          plotOutput("histogram_plot")
        ),
        
        #heat_map
        tabPanel("heatmap",
          plotOutput("heatmap_plot")
        ),
        
        #!!!!!
        tabPanel("🥚🥚🥚",
          fluidRow(
            column(
              3,
              h4("作業好難寫(ˊOˋ)...",  align = "center"),
              actionButton("change_image_btn", "Change Image"),
              tags$style(type='text/css', "#change_image_btn { vertical-align: middle; height: 40px; width: 100%; font-size: 20px;}")
            ),
            column(
              9,
              br(),
              uiOutput("random_image_output")
            )
          )          
        )
        
      )
    )
  )
)

# sever
server <- function(input, output) {
  
  #CA
  output$CA_plot <- renderPlot({
    mydata <- iris[, c(input$x_var, input$y_var)]
    mytable <- with(mydata, table(mydata[[1]], mydata[[2]]))
    
    prop_table_row <- prop.table(mytable, 1)
    prop_table_col <- prop.table(mytable, 2)
    
    fit <- ca(mytable)
    
    plot(fit, mass = TRUE, contrib = "absolute", map = "rowgreen", arrows = c(FALSE, TRUE))
  })
   
  #PCA
  log.ir <- log(iris[, 1:4])
  ir.species <- iris[, 5]
  ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
  
  output$pca_plot <- renderPlot({
    x_var <- switch(input$x_axis,
                    "PC1" = ir.pca$x[, 1],
                    "PC2" = ir.pca$x[, 2],
                    "PC3" = ir.pca$x[, 3],
                    "PC4" = ir.pca$x[, 4])
    
    y_choices <- c("PC1", "PC2", "PC3", "PC4")
    y_choices <- y_choices[y_choices != input$x_axis]
    
    y_var <- switch(input$y_axis,
                    "PC1" = ir.pca$x[, 1],
                    "PC2" = ir.pca$x[, 2],
                    "PC3" = ir.pca$x[, 3],
                    "PC4" = ir.pca$x[, 4])
    
    g <- ggbiplot(pcobj = ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species)
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
    g <- g + geom_point(aes_string(x = x_var, y = y_var))
    
    print(g)
  })
  
  #INPUT
  output$pca_input_data_table <- renderDataTable({
    iris  
  }, options = list(pageLength = 20))
  
  #OUTPUT
  pca <- reactive({
    prcomp(iris[, 1:4], center = TRUE, scale. = TRUE)
  })
  
  pca_result <- reactive({
    result <- pca()
    data.frame(PC1 = result$x[, 1], PC2 = result$x[, 2], PC3 = result$x[, 3], PC4 = result$x[, 4])
  })

  output$pca_result_table <- renderDataTable({
    pca_result()
  }, options = list(pageLength = 20))
  
  #random PIC
  generateRandomImage <- function() {
    image_list <- list.files(path = "www", pattern = "*.jpg$")
    random_image <- sample(image_list, 1)
    return(random_image)
  }
  random_image <- reactiveVal(generateRandomImage())
  
  observeEvent(input$change_image_btn, {
    random_image(generateRandomImage())
  })
  
  output$random_image_output <- renderUI({
    tags$img(src = random_image(), width = "50%", height = "50%")
  })
  
  #histogram_plot
  output$histogram_plot <- renderPlot({
    ggplot(iris, aes(x = Petal.Width)) +
      geom_histogram(binwidth = 0.1, fill = "lightblue", color = "black") +
      labs(x = "Petal Width", y = "Frequency") +
      theme_minimal()
  })
  
  #heat_map_plot
  output$heatmap_plot <- renderPlot({
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, fill = Petal.Length)) +
      geom_tile() +
      labs(x = "Sepal Length", y = "Sepal Width", fill = "Petal Length") +
      scale_fill_gradient(low = "white", high = "blue") +
      theme_minimal()
  })
  
}

shinyApp(ui = ui, server = server)