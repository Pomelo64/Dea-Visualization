library(shiny)
library(lpSolve)
library(Benchmarking)
library(smacof)



# Define UI for DEA Viz application
shinyUI(
        # Data load page
        #####
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
                # Plots Menu
                #####       
                navbarMenu('Plots',
                           # Simple variable histograms 
                           ##### 
                           tabPanel("Variable Histograms",
                                    fluidPage(
                                            
                                            # Application title
                                            titlePanel("Variable Histograms"),
                                            
                                            sidebarLayout(
                                                    
                                                    sidebarPanel(
                                                            #action button 
                                                            #size 
                                                            #transparency
                                                            #labels 
                                                            
                                                            helpText("Note: By pressing the plot button, the chosen variables would be visualized in histogram dot-plots. Each dot is representative of a DMU. Furthermore, one single DMU can be highlighted in the plots."), 
                                                            tags$hr(),
                                                            # dynamic UI of checkbox inputs
                                                            uiOutput("dotplot_var_selection_ui"), 
                                                            uiOutput("dotplot_dmu_selection_ui"),
                                                            actionButton("dotplot_button","Plot") 
                                                            
                                                            #tags$hr(),
                                                            #helpText("Note: You can highlight the position of a single DMU on the plots"),
                                                            #uiOutput("dotplot_dmu_selection_ui")
                                                            
                                                            
                                                            
                                                            
                                                    ),
                                                    
                                                    
                                                    # Show the caption, a summary of the dataset and an HTML 
                                                    # table with the requested number of observations
                                                    mainPanel(
                                                            
                                                            
                                                            plotOutput("dotplot_plot",width = "800px",height = "600px"),
                                                            
                                                            #textOutput("dotplot_info")
                                                            #DT::dataTableOutput("dotplot_brush_info")
                                                            downloadButton('download_dotplot', 'Download the Plot')
                                                            
                                                            
                                                    )
                                            )
                                    ) 
                           ),
                           # CEM MDU
                           #####  
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
                                                            uiOutput("cem_dotplot_dmu_selection_ui"),
                                                            checkboxInput('row_unfolding_labels', 'Row Object Labels', FALSE),
                                                            checkboxInput('col_unfolding_labels', 'Col Object Labels', FALSE),
                                                            sliderInput("cem_row_point_size", label = "Row Objects Point Size", min = 1 , max = 10 , value = 4),
                                                            sliderInput("cem_col_point_size", label = "Col Objects Point Size", min = 1 , max = 10 , value = 4), 
                                                            sliderInput("cem_row_transparency", label = "Row Objects Transparency", min = 0.1 , max = 1 , value = 0.5),
                                                            sliderInput("cem_col_transparency", label = "Col Objects Transparency", min = 0.1 , max = 1 , value = 0.5),
                                                            tags$hr(),
                                                            helpText("For Optimum Weights' Biplot:"),
                                                            radioButtons("cem_weight_plot_method",label = "Data Reduction Method", choices = list("PCA", "MDS"),selected = "PCA"),
                                                            checkboxInput('cem_biplot_point_labels', 'Biplot Point Labels', FALSE),
                                                            uiOutput("cem_mds_var_selection_output"),
                                                            sliderInput("cem_biplot_point_size", label = "Biplot Point Size", min = 1 , max = 10 , value = 4),
                                                            sliderInput("cem_biplot_vector_size", label = "Biplot Vector Size", min = 1 , max = 10 , value = 2),
                                                            sliderInput("cem_biplot_vector_text_size", label = "Biplot Vector Text Size", min = 1 , max = 10 , value = 4),
                                                            sliderInput("cem_biplot_point_transparency", label = "Biplot Point Transparency", min = 0.1 , max = 1 , value = 0.5),
                                                            sliderInput("cem_biplot_vector_transparency", label = "Biplot Vector Transparency",min = 0.1 , max = 1 , value = 0.5)
                                                            
                                                            
                                                            
                                                            
                                                            
                                                    ),
                                                    
                                                    
                                                    # Show the caption, a summary of the dataset and an HTML 
                                                    # table with the requested number of observations
                                                    mainPanel(
                                                            tabsetPanel(
                                                                    id = 'CEM_tabset_panel',
                                                                    tabPanel("CEM Unfolding Visualization",
                                                                             
                                                                             plotOutput("cem_mdu_plot",width = "800px",height = "600px",
                                                                                        dblclick = "cem_dblclick",
                                                                                        brush = brushOpts(
                                                                                                id = "cem_brush",
                                                                                                resetOnNew = TRUE
                                                                                        )
                                                                                        
                                                                             ),
                                                                             DT::dataTableOutput("cem_brush_info"),
                                                                             DT::dataTableOutput("all_data"),
                                                                             verbatimTextOutput("cem_mdu_info"),
                                                                             #tableOutput("cem_mdu_info")
                                                                             downloadButton('download_cem_unfolding', 'Download the Plot')
                                                                             
                                                                    ),
                                                                    tabPanel("Optimum Weights Dotplots",
                                                                             
                                                                             helpText("Standardized Weights of Inputs"), 
                                                                             plotOutput("input_weights_dotplots",width = "800px",height = "600px"),
                                                                             tags$hr(),
                                                                             helpText("Standardized Weights of Outputs"),
                                                                             plotOutput("output_weights_dotplots",width = "800px",height = "600px"),
                                                                             
                                                                             DT::dataTableOutput("cem_weights_info"),
                                                                             DT::dataTableOutput("cem_weights_std_info")
                                                                             
                                                                             # now the dotplots using grid.arrange() similar to the simple dotplots
                                                                             
                                                                    ),
                                                                    
                                                                    tabPanel("Optimum Weights Plot",
                                                                             
                                                                             helpText("MDS of opt_weights"), 
                                                                             plotOutput("opt_weights_plot",width = "800px",height = "600px"),
                                                                             downloadButton('download_cem_weight_plot', 'Download the Plot')
                                                                             #tags$hr(),
                                                                             #helpText("Standardized Weights of Outputs"),
                                                                             #plotOutput("output_weights_dotplots",width = "800px",height = "600px"),
                                                                             
                                                                             #DT::dataTableOutput("cem_weights_info"),
                                                                             #DT::dataTableOutput("cem_weights_std_info")
                                                                             
                                                                             # now the dotplots using grid.arrange() similar to the simple dotplots
                                                                             
                                                                    )
                                                                    
                                                            )
                                                            
                                                            
                                                    )
                                            )
                                    ) 
                           ),
                           #####
                           # Coplot
                           # Removed Due to Code bugs. Replaced by MDS plots
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
                                                            checkboxInput('Porembski_labels', 'Labels', FALSE),
                                                            sliderInput("Porembski_point_size", label = "Point Size", min = 1 , max = 10 , value = 4),
                                                            sliderInput("Porembski_point_transparency", label = "Point Transparency", min = 0.1 , max = 1 , value = 0.5),
                                                            sliderInput("Porembski_edge_transparency", label = "Edge Transparency", min = 1 , max = 10 , value = 4),
                                                            sliderInput("Porembski_edge_treshold", label = "Edge Treshold", min = 0 , max = 0.95 , value = 0)
                                                            
                                                            
                                                            
                                                    ),
                                                    
                                                    
                                                    # Show the caption, a summary of the dataset and an HTML 
                                                    # table with the requested number of observations
                                                    mainPanel(
                                                            
                                                            #plotOutput("Porembski_plot"),
                                                            plotOutput("Porembski_plot",width = "800px",height = "600px",
                                                                       dblclick = "Porembski_dblclick",
                                                                       brush = brushOpts(
                                                                               id = "Porembski_brush",
                                                                               resetOnNew = TRUE
                                                                       )
                                                            ),
                                                            downloadButton('download_Porembski_graph', 'Download the Plot'),
                                                            DT::dataTableOutput("Proembski_brush_info")
                                                            #tableOutput("Porembski_info"),
                                                            
                                                            
                                                            
                                                            
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
                                                            checkboxInput('biplot_labels', 'Labels', FALSE),
                                                            sliderInput("biplot_point_size", label = "Point Size", min = 1 , max = 10 , value = 4),
                                                            sliderInput("biplot_point_transparency", label = "Point Transparency", min = 0.1 , max = 1 , value = 0.5),
                                                            sliderInput("biplot_vector_transparency", label = "Vector Transparency", min = 0.1 , max = 1 , value = 0.5),
                                                            sliderInput("biplot_vector_size", label = "Vector Size", min = 1 , max = 10 , value = 4),
                                                            #sliderInput("biplot_vector_treshold", label = "Vector Treshold", min = 0 , max = 0.95 , value = 0),
                                                            sliderInput("biplot_vector_text_size", label = "Vector Text Size", min = 1 , max = 10 , value =2 )
                                                            #sliderInput("biplot_vector_text_size", label = "Vector Text Size", min = 1 , max = 10 , value =2 ),
                                                            
                                                            
                                                    ),
                                                    
                                                    
                                                    # Show the caption, a summary of the dataset and an HTML 
                                                    # table with the requested number of observations
                                                    mainPanel(
                                                            
                                                            #plotOutput("biplot_plot"),
                                                            plotOutput("biplot_plot",width = "800px",height = "600px",
                                                                       dblclick = "biplot_dblclick",
                                                                       brush = brushOpts(
                                                                               id = "biplot_brush",
                                                                               resetOnNew = TRUE
                                                                       )
                                                            ),
                                                            downloadButton('download_biplot', 'Download the Plot'),
                                                            #tableOutput("biplot_info")
                                                            DT::dataTableOutput("biplot_brush_info")
                                                            
                                                            
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
                                                            
                                                            radioButtons("Costa_dea_model",label = "DEA Model for Coloring", 
                                                                         choices = list("CRS-Input Oriented","CRS-Output Oriented", "VRS-Input Oriented","VRS-Output Oriented"),
                                                                         selected = "CRS-Input Oriented"),
                                                            actionButton("Costa_button","Plot"), 
                                                            
                                                            tags$hr(),
                                                            
                                                            
                                                            checkboxInput('Costa_labels', 'Labels', FALSE),
                                                            sliderInput("Costa_point_size", label = "Point Size", min = 1 , max = 10 , value = 4),
                                                            sliderInput("Costa_point_transparency", label = "Point Transparency", min = 0.1 , max = 1 , value = 0.5)
                                                            #sliderInput("biplot_vector_transparency", label = "Vector Transparency", min = 0.1 , max = 1 , value = 0.5),
                                                            #sliderInput("biplot_vector_size", label = "Vector Size", min = 1 , max = 5 , value = 1),
                                                            #sliderInput("biplot_vector_treshold", label = "Vector Treshold", min = 0 , max = 0.95 , value = 0),
                                                            #sliderInput("biplot_vector_text_size", label = "Vector Text Size", min = 1 , max = 10 , value =2 ),
                                                            
                                                            
                                                    ),
                                                    
                                                    
                                                    # Show the caption, a summary of the dataset and an HTML 
                                                    # table with the requested number of observations
                                                    mainPanel(
                                                            
                                                            #plotOutput("biplot_plot"),
                                                            plotOutput("Costa_plot",width = "800px",height = "600px",
                                                                       dblclick = "Costa_dblclick",
                                                                       brush = brushOpts(
                                                                               id = "Costa_brush",
                                                                               resetOnNew = TRUE
                                                                       )
                                                            ),
                                                            downloadButton('download_Costa', 'Download the Plot'),
                                                            #tableOutput("Costa_info")
                                                            DT::dataTableOutput("Costa_brush_info")
                                                            
                                                    )
                                            )
                                    )
                           ),# End of Costa Frontier 
                           tabPanel("MDS Color Plots",
                                    fluidPage(
                                            # Application title
                                            titlePanel("MDS Color Plots"),
                                            
                                            sidebarLayout(
                                                    
                                                    sidebarPanel(
                                                            
                                                            helpText("Note: By pressing the 'plot' button, the chosen data type",
                                                                     "would be visualized on the right panel using Multidimensional Scaling ",
                                                                     "method. Further , the below",
                                                                     "widgets could help to have a better final map."), 
                                                            tags$hr(),
                                                            radioButtons("mds_dataset",label = "Dataset of MDS", 
                                                                         choices = list("Original Variables","Ratio Variables"),
                                                                         selected = "Original Variables"),
                                                            radioButtons("mds_model",label = "MDS Type", 
                                                                         choices = list("Ratio","Interval","Ordinal"),
                                                                         selected = "Ratio"),
                                                            radioButtons("mds_distance",label = "Distance Function", 
                                                                         choices = list("Euclidean","Manhattan"),
                                                                         selected = "Euclidean"),
                                                            uiOutput("mds_var_selection_ui"),
                                                            actionButton("mds_button","Plot"), 
                                                            
                                                            tags$hr(),
                                                            
                                                            
                                                            checkboxInput('mds_labels', 'Labels', TRUE),
                                                            sliderInput("mds_point_size", label = "Point Size", min = 1 , max = 10 , value = 4),
                                                            sliderInput("mds_point_transparency", label = "Point Transparency", min = 0.1 , max = 1 , value = 0.5)
                                                            #sliderInput("biplot_vector_transparency", label = "Vector Transparency", min = 0.1 , max = 1 , value = 0.5),
                                                            #sliderInput("biplot_vector_size", label = "Vector Size", min = 1 , max = 5 , value = 1),
                                                            #sliderInput("biplot_vector_treshold", label = "Vector Treshold", min = 0 , max = 0.95 , value = 0),
                                                            #sliderInput("biplot_vector_text_size", label = "Vector Text Size", min = 1 , max = 10 , value =2 ),
                                                            
                                                            
                                                    ),
                                                    
                                                    
                                                    # Show the caption, a summary of the dataset and an HTML 
                                                    # table with the requested number of observations
                                                    mainPanel(
                                                            
                                                            #plotOutput("biplot_plot"),
                                                            plotOutput("mds_plot",width = "800px",height = "600px",
                                                                       dblclick = "mds_dblclick",
                                                                       brush = brushOpts(
                                                                               id = "mds_brush",
                                                                               resetOnNew = TRUE
                                                                       )
                                                            ),
                                                            downloadButton('download_mds', 'Download the Plot'),
                                                            #tableOutput("mds_info")
                                                            DT::dataTableOutput("mds_brush_info")
                                                            
                                                    )
                                            )
                                    )
                           )# MDS Multiplots
                           
                ), # End of Plotting Methods -  Overall
                
                
                
                
                tabPanel("Read Me!",
                         fluidPage(
                                 # Application title
                                 titlePanel("About the App and its Developer"),
                                 
                                 sidebarLayout(
                                         
                                         sidebarPanel(
                                                 tags$div(
                                                         "This app is created and currently under development by Shahin Ashkiani. It was made public in 16th Aug 2017. ",
                                                         tags$br(),
                                                         "The current version needs several improvements and amendments, however this boat even at this ",
                                                         "level can sail. If not seas, but it can survive lakes!",
                                                         tags$br(),
                                                         "I couldn't develop this applet without help of these packages: Shiny, MASS, readr, lpSolve, Benchmarking, smacof, devtools, ggplot2, kohonen, ggrepel, DT.",
                                                         tags$br(),
                                                         "Salute to all the great developers of all these great packages!",
                                                         tags$br(),
                                                         tags$br(),
                                                         "To contact me you can use the following address:", 
                                                         tags$br(),
                                                         " <Contact@shahin-ashkiani.com>.", 
                                                         tags$br(),
                                                         "You can also visit my website at here:",
                                                         tags$a(href="www.Shahin-Ashkiani.com", "www.Shahin-Ashkiani.com"), 
                                                         tags$br(),
                                                         tags$br(),
                                                         tags$br(),
                                                         "© Shahin Ashkiani 2017
                                                         [August 2017]
                                                         Except as permitted by the European copyright law applicable to you, you may not reproduce any of the parts on this website. The files downloadable from this website is reproducible with proper citation of the applet and its author.
                                                         I may change these terms of use from time to time."
                                                 )
                                                 
                                                 #tags$a(href="www.Shahin-Ashkiani.com", "www.Shahin-Ashkiani.com") 
                                                 
                                                 
                                                 ),
                                         
                                         
                                         # Show the caption, a summary of the dataset and an HTML 
                                         # table with the requested number of observations
                                         mainPanel(
                                                 
                                                 #"kos",
                                                 #tags$img(src = "https://ibb.co/g3cjzv", width = "100px", height = "100px")
                                                 img(src='Shahin-Shiny-Cartoon.png', width = "700px", height = "500px") 
                                         )
                                         )
                                 )
                )
                
                
        ))
