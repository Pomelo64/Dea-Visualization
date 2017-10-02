library(shiny)
library(lpSolve)
library(Benchmarking)
library(smacof)
library(plotly)












# Define UI for DEA Viz application
shinyUI(
        navbarPage(
                title = 'DEA Visualization',
                
                
                tabPanel('Data upload', 
                         fluidPage(
                                 
                                 titlePanel("Upload Facotrs Data"),
                                 
                                 sidebarLayout(
                                         sidebarPanel(
                                                 helpText("Note:Please upload the inputs&outputs dataset in the",
                                                          "CSV format such that the inputs are the left-most columns",
                                                          "and the outputs are at the right-most columns of the dataset.",
                                                          "In the right side panel it is possible to check the format ",
                                                          "of the dataset."), 
                                                 tags$hr(),
                                                 
                                                 fileInput('factors_datafile', 'Upload the I&O Dataset',
                                                           accept=c('text/csv', 
                                                                    'text/comma-separated-values,text/plain', 
                                                                    '.csv')),
                                                 
                                                 numericInput("num_of_inputs", "Number of Input factors", 1), 
                                                 
                                                 checkboxInput('header', 'Header', TRUE),
                                                 checkboxInput('dmu_labels', 'DMU Labels', FALSE),
                                                 
                                                 radioButtons('sep', 'Separator',
                                                              c(Comma=',',
                                                                Semicolon=';',
                                                                Tab='\t'),
                                                              ','),
                                                 
                                                 radioButtons('dec', 'Decimal Symbol',
                                                              c(Comma=',',
                                                                Dot='.'),
                                                              '.'),
                                                 
                                                 radioButtons('quote', 'Quote',
                                                              c(None='',
                                                                'Double Quote'='"',
                                                                'Single Quote'="'"),
                                                              '"'),
                                                 tags$hr(),
                                                 
                                                 
                                                 
                                                 helpText("Note: When the data shown in the right panel is what ",
                                                          "it is supposed to be, each variable in one column and",
                                                          "inputs are separated correctly from outputs,",
                                                          "then press the submit button."),
                                                 
                                                 actionButton("submit_button","Submit")
                                         ),
                                         
                                         
                                         mainPanel(
                                                 h4("Dataset Evaluation"),
                                                 verbatimTextOutput("dataset_evaluation_message"),
                                                 
                                                 h4("Dataset Description"),
                                                 verbatimTextOutput("factors_info"), 
                                                 
                                                 h4("Inputs Factors"),
                                                 tableOutput("inputs_table"),
                                                 
                                                 h4("Outputs Factors"),
                                                 tableOutput("outputs_table")
                                                 
                                                 
                                         )
                                 )
                         )),
                # MDU
                #####        
                navbarMenu('Plots',
                           tabPanel("CEM MDU",
                                    fluidPage(
                                            
                                            # Application title
                                            titlePanel("Cross-Efficiency Unfolding"),
                                            
                                            sidebarLayout(
                                                    
                                                    sidebarPanel(
                                                            #action button 
                                                            #size 
                                                            #transparency
                                                            #labels 
                                                            
                                                            helpText("Note: By pressing the 'plot' button, the uploaded data",
                                                                     "would be visualized on the right panel using cross-efficiency",
                                                                     "unfolding. Afterwards, the below widgets could help to have",
                                                                     "a better final map."), 
                                                            tags$hr(),
                                                            radioButtons("cem_approach",label = "CEM Approach", choices = list("Benevolent", "Aggressive"),selected = "Benevolent"),
                                                            actionButton("cem_mdu_button","Plot"), 
                                                            tags$hr(),
                                                            
                                                            sliderInput("cem_row_point_size", label = "Row Objects Point Size", min = 1 , max = 4 , value = 1),
                                                            sliderInput("cem_col_point_size", label = "Col Objects Point Size", min = 1 , max = 4 , value = 1), 
                                                            sliderInput("cem_row_transparency", label = "Row Objects Transparency", min = 0.1 , max = 1 , value = 0.5),
                                                            sliderInput("cem_col_transparency", label = "Col Objects Transparency", min = 0.1 , max = 1 , value = 0.5),
                                                            
                                                            checkboxInput('cem_unfolding_labels', 'Labels', TRUE)
                                                            
                                                            
                                                            
                                                    ),
                                                    
                                                    
                                                    # Show the caption, a summary of the dataset and an HTML 
                                                    # table with the requested number of observations
                                                    mainPanel(
                                                            
                                                            #plotlyOutput("cem_mdu_plot"),
                                                            #plotlyOutput("cem_mdu_plot"),
                                                            plotOutput("cem_mdu_plot",
                                                                       dblclick = "cem_dblclick",
                                                                       brush = brushOpts(
                                                                               id = "cem_brush",
                                                                               resetOnNew = TRUE
                                                                       )
                                                                       
                                                            ),
                                                            verbatimTextOutput("cem_mdu_info"),
                                                            #tableOutput("cem_mdu_info")
                                                            downloadButton('download_cem_unfolding', 'Download the Plot')
                                                            
                                                            
                                                            
                                                    )
                                            )
                                    ) 
                           ),
                           #####
                           # Coplot
                           #####
                           
                           tabPanel("Co-Plot",
                                    fluidPage(
                                            
                                            # Application title
                                            titlePanel("Co-Plot"),
                                            
                                            sidebarLayout(
                                                    
                                                    sidebarPanel(
                                                            
                                                            helpText("Note: By pressing the 'plot' button, the uploaded data",
                                                                     "would be visualized on the right panel using co-plot ",
                                                                     "method.(Adler & Raveh, 2008) Further , the below",
                                                                     "widgets could help to have a better final map."), 
                                                            tags$hr(),
                                                            actionButton("coplot_button","Plot"), 
                                                            tags$hr(),
                                                            
                                                            sliderInput("coplot_point_size", label = "Point Size", min = 1 , max = 4 , value = 1),
                                                            sliderInput("coplot_point_transparency", label = "Point Transparency", min = 0.1 , max = 1 , value = 0.5),
                                                            sliderInput("coplot_vector_transparency", label = "Vector Transparency", min = 0.1 , max = 1 , value = 0.5),
                                                            sliderInput("coplot_vector_treshold", label = "Vector Treshold", min = 0.1 , max = 0.95 , value = 0.5),
                                                            
                                                            checkboxInput('coplot_labels', 'Labels', TRUE)
                                                            
                                                    ),
                                                    
                                                    
                                                    # Show the caption, a summary of the dataset and an HTML 
                                                    # table with the requested number of observations
                                                    mainPanel(
                                                            
                                                            plotlyOutput("coplot_plot"),
                                                            #plotlyOutput("cem_mdu_plot"),
                                                            tableOutput("coplot_info")
                                                            #tableOutput("cem_mdu_info")
                                                            #plotOutput("coplot_verify"),
                                                            #downloadButton('download_coplot', 'Download the Plot')
                                                            
                                                            
                                                            
                                                    )
                                            )
                                    ) 
                           ),
                           #####
                           # Porembski 
                           #####
                           tabPanel("Porembski Graph",
                                    fluidPage(
                                            
                                            # Application title
                                            titlePanel("Porembski Graph"),
                                            
                                            sidebarLayout(
                                                    
                                                    sidebarPanel(
                                                            
                                                            helpText("Note: By pressing the 'plot' button, the uploaded data",
                                                                     "would be visualized on the right panel using Porembski ",
                                                                     "method.(Porembski et al., 2005) Further , the below",
                                                                     "widgets could help to have a better final map."), 
                                                            tags$hr(),
                                                            radioButtons("Porembski_dea_model",label = "DEA Model", choices = list("CRS", "VRS"),selected = "CRS"),
                                                            
                                                            actionButton("Porembski_button","Plot"), 
                                                            
                                                            tags$hr(),
                                                            
                                                            sliderInput("Porembski_point_size", label = "Point Size", min = 1 , max = 4 , value = 1),
                                                            sliderInput("Porembski_point_transparency", label = "Point Transparency", min = 0.1 , max = 1 , value = 0.5),
                                                            sliderInput("Porembski_edge_transparency", label = "Edge Transparency", min = 1 , max = 5 , value = 1),
                                                            sliderInput("Porembski_edge_treshold", label = "Edge Treshold", min = 0 , max = 0.95 , value = 0),
                                                            
                                                            checkboxInput('Porembski_labels', 'Labels', TRUE)
                                                            
                                                    ),
                                                    
                                                    
                                                    # Show the caption, a summary of the dataset and an HTML 
                                                    # table with the requested number of observations
                                                    mainPanel(
                                                            
                                                            #plotOutput("Porembski_plot"),
                                                            plotOutput("Porembski_plot",
                                                                       dblclick = "Porembski_dblclick",
                                                                       brush = brushOpts(
                                                                               id = "Porembski_brush",
                                                                               resetOnNew = TRUE
                                                                       )
                                                            ),
                                                            #plotlyOutput("cem_mdu_plot"),
                                                            tableOutput("Porembski_info"),
                                                            
                                                            downloadButton('download_Porembski', 'Download the Plot')
                                                            
                                                            
                                                            
                                                    )
                                            )
                                    ) 
                           ),
                           tabPanel("PCA Biplot",
                                    fluidPage(
                                            
                                            # Application title
                                            titlePanel("PCA Biplot"),
                                            
                                            sidebarLayout(
                                                    
                                                    sidebarPanel(
                                                            
                                                            helpText("Note: By pressing the 'plot' button, the uploaded data",
                                                                     "would be visualized on the right panel using PCA Biplot ",
                                                                     "method. Further , the below",
                                                                     "widgets could help to have a better final map."), 
                                                            tags$hr(),
                                                            
                                                            actionButton("biplot_button","Plot"), 
                                                            
                                                            tags$hr(),
                                                            
                                                            radioButtons("biplot_dea_model",label = "DEA Model for Coloring", choices = list("CRS", "VRS"),selected = "CRS"),
                                                            
                                                            sliderInput("biplot_point_size", label = "Point Size", min = 1 , max = 4 , value = 1),
                                                            sliderInput("biplot_point_transparency", label = "Point Transparency", min = 0.1 , max = 1 , value = 0.5),
                                                            sliderInput("biplot_vector_transparency", label = "Vector Transparency", min = 0.1 , max = 1 , value = 0.5),
                                                            sliderInput("biplot_vector_size", label = "Vector Size", min = 1 , max = 5 , value = 1),
                                                            #sliderInput("biplot_vector_treshold", label = "Vector Treshold", min = 0 , max = 0.95 , value = 0),
                                                            sliderInput("biplot_vector_text_size", label = "Vector Text Size", min = 1 , max = 10 , value =2 ),
                                                            checkboxInput('biplot_labels', 'Labels', TRUE)
                                                            
                                                    ),
                                                    
                                                    
                                                    # Show the caption, a summary of the dataset and an HTML 
                                                    # table with the requested number of observations
                                                    mainPanel(
                                                            
                                                            #plotOutput("biplot_plot"),
                                                            plotOutput("biplot_plot",
                                                                       dblclick = "biplot_dblclick",
                                                                       brush = brushOpts(
                                                                               id = "biplot_brush",
                                                                               resetOnNew = TRUE
                                                                       )
                                                            ),
                                                            downloadButton('download_biplot', 'Download the Plot'),
                                                            tableOutput("biplot_info")
                                                            
                                                    )
                                            )
                                    ) 
                           ), 
                           tabPanel("Self-Organizing Map",
                                    fluidPage(
                                            
                                            # Application title
                                            titlePanel("Self-Organizing Map"),
                                            
                                            sidebarLayout(
                                                    
                                                    sidebarPanel(
                                                            
                                                            helpText("Note: By pressing the 'plot' button, the uploaded data",
                                                                     "would be visualized on the right panel using Self- ",
                                                                     "Organizing Map(SOM). Further , the below",
                                                                     "widgets could help to have a better final map."), 
                                                            tags$hr(),
                                                            
                                                            actionButton("som_button","Plot"), 
                                                            
                                                            tags$hr(),
                                                            
                                                            radioButtons("som_dea_model",label = "DEA Model for Coloring", choices = list("CRS", "VRS"),selected = "CRS"),
                                                            #radioButtons("biplot_dea_model",label = "DEA Model for Coloring", choices = list("CRS", "VRS"),selected = "CRS"),
                                                            sliderInput("som_h_size", label = "Horizontal Grid Size", min = 2 , max = 20 , value = 5),
                                                            sliderInput("som_v_size", label = "Vertical Grid Size", min = 2 , max = 20 , value = 5),
                                                            #sliderInput("biplot_point_size", label = "Point Size", min = 1 , max = 4 , value = 1),
                                                            #sliderInput("biplot_point_transparency", label = "Point Transparency", min = 0.1 , max = 1 , value = 0.5),
                                                            #sliderInput("biplot_vector_transparency", label = "Vector Transparency", min = 0.1 , max = 1 , value = 0.5),
                                                            #sliderInput("biplot_vector_size", label = "Vector Size", min = 1 , max = 5 , value = 1),
                                                            #sliderInput("biplot_vector_treshold", label = "Vector Treshold", min = 0 , max = 0.95 , value = 0),
                                                            radioButtons("som_labels",label = "DMU Labels", choices = list("Yes", "No"),selected = "Yes")
                                                            #checkboxInput('som_labels', 'Labels', TRUE)
                                                            
                                                    ),
                                                    
                                                    
                                                    # Show the caption, a summary of the dataset and an HTML 
                                                    # table with the requested number of observations
                                                    mainPanel(
                                                            
                                                            #plotOutput("biplot_plot"),
                                                            plotOutput("som_plot",
                                                                       dblclick = "som_dblclick",
                                                                       brush = brushOpts(
                                                                               id = "som_brush",
                                                                               resetOnNew = TRUE
                                                                       )
                                                            ),
                                                            downloadButton('download_som_main', 'Download the Plot'),
                                                            plotOutput('som_var_plots'),
                                                            tableOutput("som_info"),
                                                            downloadButton('download_som_properties', 'Download the Plot')
                                                            
                                                    )
                                            )
                                    ) 
                           ),
                           tabPanel("Costa Frontier",
                                    fluidPage(
                                            
                                            # Application title
                                            titlePanel("Costa Frontier"),
                                            
                                            sidebarLayout(
                                                    
                                                    sidebarPanel(
                                                            
                                                            helpText("Note: By pressing the 'plot' button, the uploaded data",
                                                                     "would be visualized on the right panel using Costa et al.(2016) ",
                                                                     "method. Further , the below",
                                                                     "widgets could help to have a better final map."), 
                                                            tags$hr(),
                                                            
                                                            actionButton("Costa_button","Plot"), 
                                                            
                                                            tags$hr(),
                                                            
                                                            radioButtons("Costa_dea_model",label = "DEA Model for Coloring", 
                                                                         choices = list("CRS-Input Oriented","CRS-Output Oriented", "VRS-Input Oriented","VRS-Output Oriented"),
                                                                         selected = "CRS-Input Oriented"),
                                                            
                                                            sliderInput("Costa_point_size", label = "Point Size", min = 1 , max = 4 , value = 1),
                                                            sliderInput("Costa_point_transparency", label = "Point Transparency", min = 0.1 , max = 1 , value = 0.5),
                                                            #sliderInput("biplot_vector_transparency", label = "Vector Transparency", min = 0.1 , max = 1 , value = 0.5),
                                                            #sliderInput("biplot_vector_size", label = "Vector Size", min = 1 , max = 5 , value = 1),
                                                            #sliderInput("biplot_vector_treshold", label = "Vector Treshold", min = 0 , max = 0.95 , value = 0),
                                                            #sliderInput("biplot_vector_text_size", label = "Vector Text Size", min = 1 , max = 10 , value =2 ),
                                                            checkboxInput('biplot_labels', 'Labels', TRUE)
                                                            
                                                    ),
                                                    
                                                    
                                                    # Show the caption, a summary of the dataset and an HTML 
                                                    # table with the requested number of observations
                                                    mainPanel(
                                                            
                                                            #plotOutput("biplot_plot"),
                                                            plotOutput("Costa_plot",
                                                                       dblclick = "Costa_dblclick",
                                                                       brush = brushOpts(
                                                                               id = "Costa_brush",
                                                                               resetOnNew = TRUE
                                                                       )
                                                            ),
                                                            downloadButton('download_Costa', 'Download the Plot'),
                                                            tableOutput("Costa_info")
                                                            
                                                    )
                                            )
                                    ) 
                           )
                           #Endo of Costa Frintier                                
                ),
                
                tabPanel('About')
        )
)