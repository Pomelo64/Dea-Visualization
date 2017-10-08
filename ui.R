library(shiny)
library(lpSolve)
library(Benchmarking)
library(smacof)
library(shinyBS)



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
                                                 
                                                 #bsButton("upload_data_help_button", label = "", icon = icon("question"),
                                                 #         style = "info", size = "extra-small"),
                                                 
                                                 
                                                 
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
                                                 
                                                 tabsetPanel(
                                                         tabPanel("Data Upload",
                                                                  h4("Dataset Evaluation"),
                                                                  verbatimTextOutput("dataset_evaluation_message"),
                                                                  
                                                                  h4("Dataset Description"),
                                                                  verbatimTextOutput("factors_info"), 
                                                                  
                                                                  h4("Inputs Factors"),
                                                                  tableOutput("inputs_table"),
                                                                  
                                                                  h4("Outputs Factors"),
                                                                  tableOutput("outputs_table")
                                                         ),
                                                         tabPanel("Help",
                                                                  tags$iframe(src = "Uploadhelp.html", style="height:600px; width:100%")
                                                                  
                                                         )
                                                 )
                                                 
                                                 
                                         )
                                 )
                         )),
                # Plots Menu
                #####       
                navbarMenu('Plots',
                           # Simple variable histograms 
                           ##### 
                           tabPanel("Distributions",
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
                                                            #bsButton("histogram_help_button", label = "", icon = icon("question"),
                                                            #         style = "info", size = "extra-small"),
                                                            tags$hr(),
                                                            # dynamic UI of checkbox inputs
                                                            uiOutput("dotplot_var_selection_ui"), 
                                                            uiOutput("dotplot_dmu_selection_ui"),
                                                            actionButton("dotplot_button","Plot"),
                                                            tags$hr(), 
                                                            helpText("Select Efficiency distribution(s) to be visualized"), 
                                                            #checkboxGroupInput("eff_vars", "Select Efficiency distribution(s) for Visualization", label =  c("crs","fdh","vrs","irs","add","fdh+"), selected = "crs")
                                                            checkboxGroupInput(inputId = "eff_vars_checkbox",label = "Select Efficiency(s)",choiceNames = as.list(c("crs","vrs","fdh","drs","irs","add")), choiceValues = as.list(c(1,2,3,4,5,6)),selected = NULL ),
                                                            actionButton("dotplot_efficiency_button","Plot")
                                                            
                                                            
                                                            
                                                            
                                                    ),
                                                    
                                                    
                                                    # Show the caption, a summary of the dataset and an HTML 
                                                    # table with the requested number of observations
                                                    mainPanel(
                                                            id = 'Histrograms_tabset',
                                                            tabsetPanel(
                                                                    tabPanel( "Variable Distributions",
                                                                              plotOutput("dotplot_plot",width = "800px",height = "600px"),
                                                                              #bsModal("histogram_modal_example", "Variable Dotplots: Help", "histogram_help_button", size = "large",
                                                                              #        htmlOutput("histogram_help")),
                                                                              
                                                                              #textOutput("dotplot_info")
                                                                              #DT::dataTableOutput("dotplot_brush_info")
                                                                              downloadButton('download_dotplot', 'Download the Plot')
                                                                    ),
                                                                    tabPanel("Efficiency Distributions",
                                                                             plotOutput("dotplot_efficiency_plot",width = "800px",height = "600px"),
                                                                             #bsModal("histogram_modal_example", "Variable Dotplots: Help", "histogram_help_button", size = "large",
                                                                             #        htmlOutput("histogram_help")),
                                                                             
                                                                             #textOutput("dotplot_info")
                                                                             #DT::dataTableOutput("dotplot_brush_info")
                                                                             downloadButton('download_dotplot_efficiency', 'Download the Plot')
                                                                             
                                                                    ), 
                                                                    tabPanel("Help",
                                                                             
                                                                             #tags$img(src = "/Users/Shaahin/Downloads/inputsoutputs.jpg", width = "100px")
                                                                             #tags$iframe(src = "/Users/Shaahin/Dropbox/Visualization/Shiny/DEA\ first\ try/Uploadhelp.html", seamless=NA)
                                                                             #htmlOutput("distribution_help")
                                                                             tags$iframe(src = "DistributionHelp.html", style="height:600px; width:100%")
                                                                             #uiOutput("distribution_help")
                                                                    )
                                                            )
                                                            
                                                            
                                                            
                                                            
                                                    )
                                            )
                                    ) 
                           ),
                           tabPanel("Correlations",
                                    fluidPage(
                                            # Application title
                                            titlePanel("Correlation Plots"),
                                            
                                            sidebarLayout(
                                                    
                                                    sidebarPanel(
                                                            
                                                            helpText("Note: Choose the set of variables and the visualization",
                                                                     "approach to correlation matrix, then press button to see",
                                                                     "the correlation plot."), 
                                                            #bsButton("correlation_help_button", label = "", icon = icon("question"),
                                                            #         style = "info", size = "extra-small"),
                                                            tags$hr(),
                                                            
                                                            radioButtons("correlation_dataset",label = "Dataset of Correlations", 
                                                                         choices = list("Variables without Efficiency Scores",
                                                                                        "Variables with CRS Efficiency",
                                                                                        "Variables with VRS Efficiency",
                                                                                        "Variables with FDH Efficiency",
                                                                                        "Variables with DRS Efficiency",
                                                                                        "Variables with IRS Efficiency",
                                                                                        "Variables with ADD Efficiency"
                                                                         )),
                                                            radioButtons("correlation_package",label = "Correlation Package", choices = list("CorrPlot","HeatMap","Performance Analytics")),
                                                            
                                                            actionButton("correlation_button","Plot")
                                                            
                                                            
                                                            
                                                    ),
                                                    
                                                    
                                                    # Show the caption, a summary of the dataset and an HTML 
                                                    # table with the requested number of observations
                                                    mainPanel(
                                                            
                                                            #plotOutput("biplot_plot"),
                                                            plotOutput("correlation_plot",width = "800px",height = "600px"),
                                                            #bsModal("correlation_modal", "Correlation Matrix: Help", "correlation_help_button", size = "large",
                                                            #        textOutput("correlation_help")),
                                                            
                                                            downloadButton('download_correlation', 'Download the Plot')
                                                            #tableOutput("mds_info")
                                                            #DT::dataTableOutput("mds_brush_info")
                                                            
                                                    )
                                            )
                                    )
                           ), # Correlations
                           
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
                                                                             downloadButton('download_cem_input_dotplot', 'Download the Plot'),
                                                                             tags$hr(),
                                                                             helpText("Standardized Weights of Outputs"),
                                                                             plotOutput("output_weights_dotplots",width = "800px",height = "600px"),
                                                                             downloadButton('download_cem_output_dotplot', 'Download the Plot'),
                                                                             
                                                                             #DT::dataTableOutput("cem_weights_info"),
                                                                             DT::dataTableOutput("cem_weights_std_info")
                                                                             
                                                                             # now the dotplots using grid.arrange() similar to the simple dotplots
                                                                             
                                                                    ),
                                                                    
                                                                    tabPanel("Optimum Weights Plot",
                                                                             
                                                                             
                                                                             #plotOutput("opt_weights_plot",width = "800px",height = "600px"),
                                                                             plotOutput("opt_weights_plot",width = "800px",height = "600px",
                                                                                        dblclick = "opt_weights_dblclick",
                                                                                        brush = brushOpts(
                                                                                                id = "opt_weights_brush",
                                                                                                resetOnNew = TRUE
                                                                                        )
                                                                             ),
                                                                             downloadButton('download_cem_weight_plot', 'Download the Plot'),
                                                                             DT::dataTableOutput("cem_opt_weights_brush_info")
                                                                             
                                                                             
                                                                    ), 
                                                                    
                                                                    tabPanel("Help",
                                                                             tags$iframe(src = "cemHelp.html", style="height:600px; width:100%")
                                                                             
                                                                    )
                                                                    
                                                            )
                                                            
                                                            
                                                    )
                                            )
                                    ) 
                           ),
                           #####
                           # Coplot
                           # Removed Due to Code bugs. Also the method is a nonesense and not defendable. Replaced by MDS plots
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
                                                            sliderInput("Porembski_edge_transparency", label = "Edge Transparency", min = 1 , max = 15 , value = 4),
                                                            sliderInput("Porembski_edge_treshold", label = "Edge Treshold", min = 0 , max = 0.95 , value = 0)
                                                            
                                                            
                                                            
                                                    ),
                                                    
                                                    
                                                    # Show the caption, a summary of the dataset and an HTML 
                                                    # table with the requested number of observations
                                                    mainPanel(id = "porembski",
                                                              
                                                              tabsetPanel(
                                                                      tabPanel("Porembski Graph",
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
                                                                               
                                                                      ),
                                                                      tabPanel("Help",
                                                                               tags$iframe(src = "porembskiHelp.html", style="height:600px; width:100%")
                                                                               
                                                                      )
                                                              )
                                                              
                                                              
                                                              
                                                              
                                                              
                                                              
                                                    )
                                            )
                                    ) 
                           ),
                           ##### 
                           tabPanel("Variable Profile PCA Bi-plot",
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
                           #####
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
                           #####
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
                           #####
                           tabPanel("Variable Profile MDS Color-Plots",
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
                                                         [September 2017]
                                                         Currently the applet is under aGPLv3.0 lisence.
                                                         Hopefuly it is compatible with the lisence of the libraries. 
                                                         I may change the lisence later."
                                                 )
                                                 
                                                 #tags$a(href="www.Shahin-Ashkiani.com", "www.Shahin-Ashkiani.com") 
                                                 
                                                 
                                                 ),
                                         
                                         
                                         # Show the caption, a summary of the dataset and an HTML 
                                         # table with the requested number of observations
                                         mainPanel(
                                                 
                                                 #"kos",
                                                 #tags$img(src = "https://ibb.co/g3cjzv", width = "100px", height = "100px")
                                                 img(src='Shahin-Shiny-Cartoon.png', width = "700px", height = "500px"),
                                                 
                                                 tags$h2("References:Papers"),
                                                 tags$ol(
                                                         tags$li("Adler, N., & Raveh, A. (2008). Presenting DEA graphically. Omega, 36(5), 715-729."), 
                                                         tags$li("Ashkiani,S., Mar Molinero, C. (2017) Visualization of Cross-Efficiency Matrices Using Multidimensional Unfolding, Working Paper "), 
                                                         tags$li("Carboni, O. A., & Russu, P. (2015). Assessing regional wellbeing in Italy: An application of Malmquist–DEA and self-organizing map neural clustering. Social indicators research, 122(3), 677-700."),
                                                         tags$li("e Costa, C. A. B., de Mello, J. C. C. S., & Meza, L. A. (2016). A new approach to the bi-dimensional representation of the DEA efficient frontier with multiple inputs and outputs. European Journal of Operational Research, 255(1), 175-186."),
                                                         tags$li("Greenacre, M. J. (2010). Biplots in practice. Fundacion BBVA."),
                                                         tags$li("Kohonen, T. (1998). The self-organizing map. Neurocomputing, 21(1), 1-6."),
                                                         tags$li("Porembski, M., Breitenstein, K., & Alpar, P. (2005). Visualizing efficiency and reference relations in data envelopment analysis with an application to the branches of a German bank. Journal of Productivity Analysis, 23(2), 203-221.")
                                                         
                                                 ),
                                                 
                                                 
                                                 tags$br(),
                                                 tags$br(),
                                                 
                                                 tags$h2("References:Libraries"),
                                                 tags$ol(
                                                         tags$li("Michel Berkelaar and others (2015). lpSolve: Interface to 'Lp_solve' v.
                                                                 5.5 to Solve Linear/Integer Programs. R package version 5.6.13.
                                                                 https://CRAN.R-project.org/package=lpSolve"),
                                                         tags$li("P. Bogetoft and L. Otto (2015), Benchmarking with DEA and SFA, R package
                                                                 version 0.26."),
                                                         tags$li("Brian G. Peterson and Peter Carl (2014). PerformanceAnalytics: Econometric
                                                                 tools for performance and risk analysis. R package version 1.4.3541.
                                                                 https://CRAN.R-project.org/package=PerformanceAnalytics
                                                                 "),
                                                         tags$li( "Baptiste Auguie (2016). gridExtra: Miscellaneous Functions for Grid
                                                                  Graphics. R package version 2.2.1.
                                                                  https://CRAN.R-project.org/package=gridExtra"),
                                                         tags$li("Jan de Leeuw, Patrick Mair (2009). Multidimensional Scaling Using
                                                                 Majorization: SMACOF in R. Journal of Statistical Software, 31(3), 1-30.
                                                                 URL http://www.jstatsoft.org/v31/i03/.
                                                                 "),
                                                         
                                                         tags$li("Kamil Slowikowski (2016). ggrepel: Repulsive Text and Label Geoms for
                                                                 'ggplot2'. R package version 0.6.5.
                                                                 https://CRAN.R-project.org/package=ggrepel
                                                                 "),
                                                         tags$li("Taiyun Wei and Viliam Simko (2016). corrplot: Visualization of a
                                                                 Correlation Matrix. R package version 0.77.
                                                                 https://CRAN.R-project.org/package=corrplot
                                                                 "),
                                                         tags$li("Erich Neuwirth (2014). RColorBrewer: ColorBrewer Palettes. R package
                                                                 version 1.1-2. https://CRAN.R-project.org/package=RColorBrewer
                                                                 "),
                                                         tags$li("Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth Edition. Springer, New York. ISBN 0-387-95457-0"),
                                                         tags$li("Hadley Wickham, Jim Hester and Romain Francois (2017). readr: ReadRectangular Text Data. R package version 1.1.1.https://CRAN.R-project.org/package=readr"),
                                                         tags$li("H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag
                                                                 New York, 2009."), 
                                                         
                                                         tags$li("R. Wehrens and L.M.C. Buydens, Self- and Super-organising Maps in R: the
                                                                 kohonen package J. Stat. Softw., 21(5), 2007
                                                                 "),
                                                         
                                                         tags$li("Yihui Xie (2016). DT: A Wrapper of the JavaScript Library 'DataTables'. R
                                                                 package version 0.2. https://CRAN.R-project.org/package=DT
                                                                 ")
                                                         
                                                         )
                                                 
                                                         )      
                                                         )
                                                         )
                                                         )
                
                
                                 ))
