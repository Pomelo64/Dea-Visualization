library(shiny)
library(MASS)
library(readr)
library(lpSolve)
library(Benchmarking)
library(smacof)
library(devtools)
library(ggplot2)
library(kohonen)
library(ggrepel)
library(DT)
library(gridExtra)
library(RColorBrewer)
#library(shinyjs)
options(expressions = 500000)

# 17 August 10am ---> rectifying xlim ylim. Now all the points are seen clearly
# 17 August  ------> experiment on Costa frontier to figure out the best combination of shape and color, also to add legend 
# 17 August ----> ggrepel added to rectify the problem of overlapping texts 

# 18 August ---> Legends added
# 18 August ----> Size of the plots is fixed, so it would remain square and does not change by the size of browser
# 18 August ----> Color difference between efficient and inefficient unit added

# 19 August ----> Tooltip and tooltip table is added

# 29 August ---->  dotplots of variables
# 30 August ---->  download button for dotplots


# 31 August ----> Benevolent weights standardization and their dotplots added. 
# 31 August -----> zero weights generated for Spanish Airports imputed standardized. Probably because of dea_4cem() . Use dea() 

# 1 September ----> Bug resolving : Important! 
# 2 September ----> PCA and MDS std weight plots are added 
# 3 September ----> brushing, zooming ,and download button for the PCA/MDS weight plots 


set.seed(7)
#####
##CEM MDU Functions

all_OFs = function(input_mat, output_mat) {
        #this function receives the input and output levels and generates a matrix in which each 
        #row is the objective function coefficients of the corresponding unit. 
        #in other words, all_OFs[i,] is the OF coefficients of unit i
        #ready to be used in lp()
        
        # I assume that the row size of the input_mat and output_mat are equal
        # Although, it is possible to put a check here 
        input_mat = as.matrix(input_mat)
        output_mat = as.matrix(output_mat)
        
        number_of_units = nrow(input_mat)
        number_of_inputs = ncol(input_mat)
        number_of_outputs = ncol(output_mat)
        
        #creating the final matrix 
        all_OFs = matrix(nrow = number_of_units, ncol =  number_of_outputs+number_of_inputs  )
        
        #filling the final matrix
        for (unit in 1:number_of_units) {
                all_OFs[unit,] = c(output_mat[unit,],rep(0,number_of_inputs))
                
        }
        
        all_OFs
        #now by picking every row
        #it is possible to add input and output names to this matrix. Maybe in next versions
        
}

constraints_directions = function(input_mat) {
        #the = and <= directions of A matrix 
        # assuming that nrow(input_mat)==nrow(output_mat)
        number_of_units = nrow(input_mat)
        
        t = c("==",rep("<=",number_of_units-1))
        
        t
        
} #--- End of Constraints_directions()

rhs = function(input_mat){
        #returns the rhs values
        #assuming nrow(input_mat)==ncol(input_mat)
        
        number_of_units = nrow(input_mat)
        
        t=c(1,rep(0,number_of_units-1))
        
        t
}


simple_efficiency = function(input_mat,output_mat,epsilon_value = 0.001){
        #this function is supposed to return the simple efficiency scores of all units. 
        #assuming that nrow(input_mat)==nrow(output_mat)
        number_of_units = nrow(input_mat)
        OF_coefficients = all_OFs(input_mat, output_mat)
        
        number_of_inputs = ncol(input_mat)
        number_of_outputs = ncol(output_mat)
        d = number_of_inputs + number_of_outputs
        
        #A_lower = epsilons_mat(input_mat=input_mat,output_mat=output_mat,epsilon_value = 0.00001)
        A_lower = diag(d)
        
        C_upper =constraints_directions(input_mat = input_mat )
        C = c(C_upper,rep(">=",d))
        
        rhs_upper=rhs(input_mat = input_mat)
        rhs = c(rhs_upper, rep(epsilon_value,d))
        
        simple_eff = vector(length = number_of_units)
        
        for (i in 1:number_of_units){
                
                A_upper=A_mat(unit=i, input_mat=input_mat, output_map=output_mat)
                A = rbind(A_upper,A_lower)
                
                
                
                
                DEA_LP = lp(direction = "max",objective.in = OF_coefficients[i,],const.mat = A, const.dir = C, const.rhs = rhs  )
                #model$solution & $objval
                
                simple_eff[i] = DEA_LP$objval 
                
        }
        
        simple_eff  
        
        
} #---- End of Simple_efficiency()



crs_eff <- function(dataset, num_of_inputs, orientation = "in" ){
        inputs <- dataset[,1:num_of_inputs]
        outputs <- dataset[,(num_of_inputs+1):ncol(dataset)]
        dea(X = inputs, Y = outputs, RTS = "crs" , DUAL = TRUE, ORIENTATION = orientation )
} 

vrs_eff <- function(dataset, num_of_inputs, orientation = "in"){
        inputs <- dataset[,1:num_of_inputs]
        outputs <- dataset[,(num_of_inputs+1):ncol(dataset)]
        dea(X = inputs, Y = outputs, RTS = "vrs" , DUAL = TRUE , ORIENTATION = orientation)
} 

# Co-plot Functions
#####


#####
# Multiplot from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/



# Shiny Main Body
#####
shinyServer(function(input, output) {
        
        ##### 
        ##First reactive expression of the uploaded file 
        #to make the uploaded file a reactive expression
        datafile <- reactive({
                inFile <- input$factors_datafile
                
                if (is.null(inFile))
                        return(NULL)
                
                read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                         quote=input$quote, stringsAsFactors = FALSE, dec = input$dec)
                
                #read_delim(file = inFile$datapath, col_names = input$header , quote = input$quote,delim = input$sep  )
        })
        
        #####
        ## for the upload panel 
        
        # for generating the description of the dataset
        output$factors_info <- renderText({
                t<- datafile()
                
                paste("The dataset of",nrow(t),"DMUs, composed of",as.numeric(input$num_of_inputs),"inputs, and",ncol(t)-as.numeric(input$num_of_inputs),"outputs.")
        })
        
        # for rendering reactive table of inputs
        output$inputs_table <-renderTable({
                if (is.null(datafile())) { 
                        return(NULL) 
                } else {
                        datafile()[1:6,1:as.numeric(input$num_of_inputs)]     
                }
                
                # inFile <- input$factors_datafile
                #if (is.null(inFile))
                #        return(NULL)
                #read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                #         quote=input$quote)[1:6,1:as.numeric(input$num_of_inputs)]
                
                
                
                
        })
        
        #for rendering reactive table of outputs    
        output$outputs_table <- renderTable({
                if (is.null(datafile())) { 
                        return(NULL) 
                } else {
                        datafile()[1:6,(as.numeric(input$num_of_inputs)+1):ncol(datafile())]   
                }
                #inFile <- input$factors_datafile
                #if (is.null(inFile))
                #        return(NULL)
                #t <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                #         quote=input$quote)[1:6,]
                
                
                
        })
        
        ##### 
        ## Evaluation of the uploaded data and passing data to computation part 
        
        #returns two messages based on whether the dataset meets all the requirements or not
        dataset_evaluation <- eventReactive(input$submit_button, {
                
                
                #t <- datafile()
                t <- data.frame(sapply(datafile(),parse_number))
                dataset_evaluation_result <- vector(length = 3)
                names(dataset_evaluation_result) <- c("Numerical Factors","Non-negative Factors","No Missing Value")
                
                
                dataset_evaluation_result[1] <- ( sum(sapply(t,is.numeric)) == ncol(t) ) 
                dataset_evaluation_result[2] <- ( !(FALSE %in% (t>=0)) ) 
                dataset_evaluation_result[3] <- ( !(TRUE %in% is.na(t)) )
                
                ifelse(test = (FALSE %in% dataset_evaluation_result) , 
                       yes = "Error! There is something wrong with the dataset. It maybe either having non-numeric data, negative values, or missing values." ,
                       no = "Great! The dataset meets the requirements." )  
                
        })
        
        #sends the evaluation result as a message to UI
        output$dataset_evaluation_message <- renderText({
                
                dataset_evaluation()
                #t <- datafile()
                #t <- data.frame(sapply(t,parse_number))
                #sapply(t,class)
        })
        
        # send the verified dataset into a reactive expressing, so it can be used later on 
        final_dataset_reactive <- reactive({
                data.frame(sapply(datafile(),parse_number))
                #ifelse(test = dataset_evaluation() == "Great! The dataset meets the requirements.",yes = data.frame(sapply(datafile(),parse_number)) , no = NULL ) 
        })
        
        efficiencies_crs <- reactive({
                dataset <- final_dataset_reactive()
                
                num_of_inputs <- as.numeric(input$num_of_inputs)
                inputs <- dataset[,1:num_of_inputs]
                outputs <- dataset[,(num_of_inputs+1):ncol(dataset)]
                
                dea(X = inputs, Y = outputs, RTS = "crs" , DUAL = TRUE)
        })
        
        efficiencies_vrs <- reactive({
                dataset <- final_dataset_reactive()
                
                num_of_inputs <- as.numeric(input$num_of_inputs)
                inputs <- dataset[,1:num_of_inputs]
                outputs <- dataset[,(num_of_inputs+1):ncol(dataset)]
                
                dea(X = inputs, Y = outputs, RTS = "vrs", DUAL = TRUE )
        })
        
        
        # CEM MDU        
        #####
        #----- CEM computation functions
        A_mat = function(unit, dataset, number_of_inputs) {
                #this function returns A matrix - matrix of constraints - for DMUunit formulation. 
                # unit can be a number from 1 to nrow(input_mat)
                # This function can be developed in a way that it returns all A matrices for
                # all units in a data structure such as list. It would be faster but 
                # pre-mature optimization is the root of all devils. 
                
                number_of_units = nrow(dataset)
                number_of_inputs = number_of_inputs
                number_of_outputs = ncol(dataset)- number_of_inputs
                number_of_variables = number_of_inputs+number_of_outputs
                
                
                A = matrix(nrow = number_of_units, ncol= number_of_variables)
                
                
                
                #first constraint is always the numerator constraint
                A[1,]= unlist(c(rep(0,number_of_outputs),dataset[unit,1:number_of_inputs]))
                ##print(dim(A))
                
                
                input_mat_without_unit <- as.matrix(dataset[-unit,1:number_of_inputs])
                output_mat_without_unit <- as.matrix(dataset[-unit,(number_of_inputs+1):number_of_variables])
                
                
                d = number_of_units-1
                
                ##print(d)
                for (i in 1:d) { 
                        ##print(i)
                        A[(i+1), ]=c(output_mat_without_unit[i,],-1*input_mat_without_unit[i,])
                        
                } 
                
                
                A
                
        } 
        
        dea_4cem = function(dataset, number_of_inputs, epsilon = 0.000001) {
                # returns a list , the first element is the efficiency of the DMUs (CRS)
                # and the second element is the matrix of the optimum weights 
                
                # because of using lp() without lpSolveAPI, the lower bound is ignored in some cases
                # so better to round the results in 4 digits or so, then there would be no negative value 
                
                # this function is supposed to replace dea() of Benchmarking
                # This function supposed to give back the optimum [ or one optimum set of] weights
                
                # since dea() is giving bulshit results for Colombian hospital case
                # indeed, the summation of the VIs is not equal to one! 
                # so it means the formulation that dea() is using is not the one that I need
                
                number_of_units = nrow(dataset)
                number_of_inputs = number_of_inputs
                number_of_outputs = ncol(dataset)- number_of_inputs
                number_of_variables = number_of_inputs+number_of_outputs
                
                
                eff_weight_mat = matrix(nrow = number_of_units, ncol = (number_of_variables+1))
                
                lprec_eff_scores <- vector(length = number_of_units)
                lprec_opt_weights <- matrix(nrow = number_of_units, ncol = number_of_variables)
                
                for (unit in 1:number_of_units){
                        
                        OF = unlist(c(as.vector(dataset[unit,(number_of_inputs+1):number_of_variables]),
                                      rep(0,number_of_inputs)))
                        
                        A_upper = unlist(c(rep(0,number_of_outputs),dataset[unit,1:number_of_inputs]))
                        A_middle = cbind(dataset[-unit,(number_of_inputs+1):number_of_variables],-dataset[-unit,1:number_of_inputs])
                        A_bottom = OF 
                        A_upper = unname(A_upper)
                        A_middle = unname(A_middle)
                        A_bottom = unname(A_bottom)
                        
                        A = as.matrix(rbind(A_upper, A_middle, A_bottom))
                        
                        const_directions = c("==",rep("<=",(number_of_units)))
                        
                        RHS_values = c(1,rep(0,(number_of_units-1)),1)
                        
                        lp_model=lp(direction = "max", 
                                    objective.in = OF ,
                                    const.mat = A ,
                                    const.dir = const_directions,
                                    const.rhs = RHS_values)
                        
                        eff_weight_mat[unit,] = c(lp_model$objval,lp_model$solution)
                        
                        #----- new implementation
                        
                        #OF
                        OF <- dataset[unit,]
                        
                        
                        lprec <- make.lp(nrow = 0 , ncol = number_of_variables )
                        set.objfn(lprec, OF)
                        lprec_const_directions <- gsub(x = const_directions, pattern = "==", replacement = "=")
                        for (constraint_no in 1:nrow(A)) {
                                add.constraint(lprec, A[constraint_no,], lprec_const_directions[constraint_no], RHS_values[constraint_no])
                        }
                        set.bounds(lprec, lower = rep(x = epsilon, number_of_variables))
                        ColNames <- c(colnames(dataset)[(number_of_inputs+1):number_of_variables],colnames(dataset)[1:number_of_inputs])
                        dimnames(lprec) <- list(1:nrow(A), ColNames)
                        
                        #lprec
                        lp.control(lprec,sense='max')
                        solve(lprec)
                        lprec_eff_scores[unit] <- get.objective(lprec)
                        lprec_opt_weights[unit, ] <- get.variables(lprec)               
                        
                        
                }
                eff_scores <-   eff_weight_mat[,1]
                weight_mat <- eff_weight_mat[,-1]
                
                Input_weights_colnames <- paste0(colnames(dataset)[1:number_of_inputs]," Weight")
                Output_weights_colnames <- 
                        paste0(colnames(dataset)[(number_of_inputs+1):number_of_variables]," Weight")
                
                colnames(weight_mat) <- c(Output_weights_colnames,Input_weights_colnames)
                
                #eff_weight_list = list(eff_scores, weight_mat)
                eff_weight_list = list(eff_scores, weight_mat, lprec_eff_scores, lprec_opt_weights)
                
                return(eff_weight_list)
                
                
                
        }
        
        #returns the benevolent optimum weights of a given unit      
        CEM_unit = function(dataset , unit , epsilon = 0.000001, number_of_inputs ){
                #this function must return the the benevolent optimum weights for the given unit 
                # because of using lp() without lpSolveAPI, the lower bound is ignored in some cases
                # so better to round the results in 4 digits or so, then there would be no negative value 
                
                set.seed(7)
                
                number_of_units = nrow(dataset)
                number_of_inputs = number_of_inputs
                number_of_outputs = ncol(dataset)- number_of_inputs
                
                number_of_variables = ncol(dataset)
                
                
                eff_weight_list <- dea_4cem(dataset = dataset, number_of_inputs = number_of_inputs)
                #4th or 2nd component of dea_4cem? 
                eff_weight_mat <- eff_weight_list[[2]]
                simple_eff <- eff_weight_list[[1]]
                
                
                #simple_eff = eff_weight_mat[,1]
                unit_simple_eff = simple_eff[unit]
                
                
                
                output_mat_refined<- as.matrix(dataset[-unit,(number_of_inputs+1):number_of_variables])
                #output_mat_refined = output_mat[-unit,]
                
                
                OF = apply(output_mat_refined,2,sum)
                OF = c(OF,rep(0,number_of_inputs))
                
                
                #preparing the A matrix
                A_middle = diag(number_of_variables)
                
                
                #---- needs debugging A_mat()
                A_upper=A_mat(unit=unit, dataset=dataset, number_of_inputs = number_of_inputs)
                
                A_last = unlist(c(dataset[unit,(number_of_inputs+1):number_of_variables],
                                  -1*unit_simple_eff*dataset[unit,1:number_of_inputs]))
                
                
                A = rbind(A_upper,A_middle,A_last)
                
                #two new lines
                
                #preparing the constraint directions
                C_upper = c("==",rep("<=",number_of_units-1))
                C_middle = rep(">=",number_of_variables)
                C_last = "=="
                C = c(C_upper,C_middle,C_last)
                
                #print(C)
                
                #preparing the RHS values
                rhs_upper = c(1,rep(0,number_of_units-1))
                rhs_middle = rep(0,number_of_variables)
                rhs_last = 0
                rhs_total = c(rhs_upper, rhs_middle,rhs_last)
                
                #print(rhs_total)
                
                t = lp(direction = "max", objective.in = OF , const.mat = A , const.dir = C , const.rhs = rhs_total)
                
                #print(t$solution>=0)
                #print("CEM_unit output, ben opt weights")
                #print(t$solution)
                
                if (sum(t$solution) == 0 ) {
                        cem_weight <- eff_weight_mat[unit,]
                } else {
                        cem_weight <- t$solution 
                }
                
                #return(t$solution)
                return(cem_weight)
        }
        
        
        # benevolent cross-efficiency matrix generator function 
        CEM = function(dataset, number_of_inputs) {
                #this function returns the benevolent CEM 
                
                
                number_of_units = nrow(dataset)
                number_of_inputs = number_of_inputs
                number_of_outputs = ncol(dataset)- number_of_inputs
                number_of_variables = number_of_inputs+number_of_outputs
                
                eff_weight_list <- dea_4cem(dataset = dataset, number_of_inputs = number_of_inputs)
                eff_weight_mat <- eff_weight_list[[4]]
                
                CEM_opt_weights = matrix(nrow = number_of_units, ncol = number_of_variables )
                
                
                for (unit in 1:number_of_units) {
                        
                        CEM_opt_weights[unit,] = CEM_unit(dataset , unit , epsilon = 0.00001, number_of_inputs = number_of_inputs)
                        
                        # can be moved to cem_unit()
                        #if ( sum(CEM_opt_weights[unit,]) == 0 ) { 
                        #        
                        #        CEM_opt_weights[unit,] <- eff_weight_mat[unit,]
                        #        #print(eff_weight_mat[unit,])
                        #}
                }
                
                
                
                CEM = matrix (nrow = number_of_units, ncol = number_of_units)
                
                # matrix multiplication solution 
                w_outputs = CEM_opt_weights[,1:number_of_outputs]
                w_inputs = CEM_opt_weights[,(number_of_outputs+1):number_of_variables]
                inputs <- dataset[,1:number_of_inputs]
                outputs <- dataset[,(number_of_inputs+1):number_of_variables]
                
                cem_output <- w_outputs %*% t(outputs)
                cem_input <- w_inputs %*% t(inputs)
                
                CEM <- cem_output/ cem_input 
                
                #cem_matrix_multiplication <- cem_output/cem_input 
                
                #print("-----")
                #print("dimensions of the new cem mat")
                #print(dim(cem_matrix_multiplication))
                
                # can be integrated in the above loop!  
                #for (row in 1:number_of_units){
                #        w_outputs = CEM_opt_weights[row,1:number_of_outputs]
                #        w_inputs = CEM_opt_weights[row,(number_of_outputs+1):number_of_variables]
                #        
                #        
                #        
                #        ## This can be heavily optimized with matrix operations rather than scalar
                #        CEM[row,] = apply(X = t(t(dataset[,(number_of_inputs+1):number_of_variables]) * CEM_opt_weights[row,1:number_of_outputs]), MARGIN = 1 , FUN = sum ) / apply(X = t(t(dataset[,1:number_of_inputs]) * CEM_opt_weights[row,(number_of_outputs+1):number_of_variables]), MARGIN = 1 , FUN = sum )
                #
                #}
                
                #print("-----")
                #print("Do the two approaches give identical output?")
                #print(identical(round(CEM,4),round(cem_matrix_multiplication,4)))
                #print(sum(round(CEM,4)==round(cem_matrix_multiplication,4))) 
                #print("-----")
                
                return(CEM)
                
        }
        
        ben_weights <- function(dataset, number_of_inputs) {
                
                #this function is supposed to return the benevolent optimum weights 
                #first the outputs, then the inputs         
                number_of_units = nrow(dataset)
                
                number_of_outputs = ncol(dataset)-number_of_inputs
                number_of_factors = ncol(dataset)
                
                eff_weight_list <- dea_4cem(dataset = dataset, number_of_inputs = number_of_inputs)
                eff_weight_mat <- eff_weight_list[[4]]
                
                CEM_opt_weights = matrix(nrow = number_of_units, ncol = number_of_factors )
                
                
                # max normalization for getting reasonable weights scale 
                # it is possible to use other normalizations 
                dataset <- apply(X = dataset, MARGIN = 2 , FUN = function(x) x/max(x))
                
                for (unit in 1:number_of_units) {
                        CEM_opt_weights[unit,] = CEM_unit(dataset , unit , epsilon = 0.00001, number_of_inputs = number_of_inputs)
                        
                        if (sum(CEM_opt_weights[unit,]) == 0 ) {
                                CEM_opt_weights[unit,] <- eff_weight_mat[unit,]
                                print("warning! zero weight vector detected")
                                print("replaced with:")
                                print(CEM_opt_weights[unit,])
                        }
                }
                
                
                factor_labels = vector(length = number_of_factors)
                factor_labels = c(colnames(dataset[,(number_of_inputs+1):number_of_factors]),
                                  colnames(dataset[,1:number_of_inputs]))
                
                factor_labels <- paste(factor_labels,"Weight",sep = " ")
                
                row_labels <- paste0("DMU",1:number_of_units)
                
                CEM_opt_weights = as.data.frame(CEM_opt_weights)
                colnames(CEM_opt_weights) = factor_labels
                row.names(CEM_opt_weights) = row_labels 
                
                
                
                
                
                return(round(CEM_opt_weights,5))  
        }
        
        weight_standardization <- function(weight_dataset , number_of_inputs){
                
                number_of_variables <- ncol(weight_dataset)
                number_of_outputs <- number_of_variables - number_of_inputs
                
                #output weights standardization
                if (number_of_outputs != 1 ) {
                        output_standardization_factor <-
                                apply(X = weight_dataset[,1:number_of_outputs] , MARGIN = 1 , FUN = sum )
                        standardized_outputs <- weight_dataset[,1:number_of_outputs]/output_standardization_factor
                        
                        
                } else {
                        
                        standardized_outputs <- weight_dataset[,1:number_of_outputs]/weight_dataset[,1:number_of_outputs]
                        standardized_outputs <- as.matrix(standardized_outputs)
                        colnames(standardized_outputs) <- colnames(weight_dataset)[,1:number_of_outputs]
                }
                
                #input weights standardization
                if (number_of_inputs != 1){
                        input_standardization_factor <-
                                apply(X = weight_dataset[,(number_of_outputs+1):number_of_variables] 
                                      , MARGIN = 1 , FUN = sum )
                        standardized_inputs <-
                                weight_dataset[,(number_of_outputs+1):number_of_variables]/input_standardization_factor
                } else {
                        standardized_inputs <-
                                weight_dataset[,(number_of_outputs+1):number_of_variables]/
                                weight_dataset[,(number_of_outputs+1):number_of_variables]
                        
                        standardized_inputs <- as.matrix(standardized_inputs)
                        colnames(standardized_inputs) <- colnames(weight_dataset)[,(number_of_outputs+1):number_of_variables]
                }
                
                standardized_weights_list <- list(round(standardized_inputs,5),round(standardized_outputs,5))
                
                return(standardized_weights_list)
        }
        
        #----- End of CEM requisit functions 
        
        #generating benevolent CEM by pressing the 'cem_mdu_button' 
        cem_reactive <- eventReactive(input$cem_mdu_button, {
                
                number_of_inputs <- as.numeric(input$num_of_inputs)
                t<- final_dataset_reactive()
                
                switch(EXPR = input$cem_approach ,
                       "Benevolent" = CEM(dataset = t, number_of_inputs = input$num_of_inputs) , 
                       #"Aggressive" = CEM_agg(input_mat = as.matrix(t[,1:number_of_inputs]) ,output_mat = as.matrix(t[,(number_of_inputs+1):ncol(t)] )) )
                       "Aggressive" = NULL )
                #CEM(input_mat = as.matrix(t[,1:number_of_inputs]) ,output_mat = as.matrix(t[,(number_of_inputs+1):ncol(t)] )) 
                
                
        })
        
        #CEM MDU plot
        cem_unfolding <- reactive({
                t <- cem_reactive()
                t <- round(t,4)
                unfolding(delta = round((1-t),2),ndim = 2)
        })
        
        cem_ranges <- reactiveValues(x = NULL, y = NULL)
        
        cem_unfolding_plot <- reactive({
                
                t <- cem_unfolding()
                row_df <- data.frame(t$conf.row, "DMU" = c(1:nrow(t$conf.row)), Type = "Rating", Shape = 19) 
                col_df <- data.frame(t$conf.col, "DMU" = c(1:nrow(t$conf.col)), Type = "Rated", Shape = 24) 
                
                
                unfolded_df  <- rbind(row_df,col_df)
                unfolded_df$alpha = NA
                unfolded_df$point_size = NA
                
                
                unfolded_df$alpha[unfolded_df$Type == "Rating"] <-  input$cem_row_transparency
                unfolded_df$alpha[unfolded_df$Type == "Rated"] <-  input$cem_col_transparency
                
                unfolded_df$point_size[unfolded_df$Type == "Rating"] <- input$cem_row_point_size
                unfolded_df$point_size[unfolded_df$Type == "Rated"] <- input$cem_col_point_size
                
                
                total_min <-  min(unfolded_df$D1,unfolded_df$D2)
                total_max <- max( unfolded_df$D1,unfolded_df$D2)
                
                
                g<- ggplot(data = unfolded_df) + 
                        geom_point(aes(x = D1 , y = D2 , 
                                       color = Type, 
                                       shape = factor(unfolded_df$Shape) ,
                                       alpha = unfolded_df$alpha ,
                                       size = unfolded_df$point_size)
                        )+ 
                        scale_alpha(guide = FALSE) +
                        scale_size(guide = FALSE) + 
                        scale_size_identity() +
                        scale_alpha_identity() + 
                        scale_color_manual(name = "Object Type", values = c("Rating"="blue","Rated"="red"), labels = c("Rating","Rated"))+
                        scale_shape_manual(name = "Object Type", values = c(19,24), labels = c("Rating","Rated"))+
                        coord_fixed(ratio = 1,  expand = TRUE) +
                        ggtitle("Cross-Efficiency Unfolding")
                
                #g <- ggplot() + geom_point(data = row_df, aes(x = D1 , y = D2, color = "blue"),
                #                           size = input$cem_row_point_size,
                #                           shape = 19,
                #                           alpha = input$cem_row_transparency) + 
                #        geom_point(data = col_df, aes(x = D1 , y = D2, color = "red"),
                #                   size = input$cem_col_point_size,
                #                   shape = 24,
                #                   alpha = input$cem_col_transparency) +
                #        coord_fixed(ratio = 1,  expand = TRUE)
                
                
                g <- g  + theme_linedraw() + annotate("text", x = Inf, y = -Inf, label = "© DEA-Viz",
                                                      hjust=1.1, vjust=-1.1, col="blue", cex=6,
                                                      fontface = "bold", alpha = 0.4) + 
                        xlim(total_min , total_max) +
                        ylim(total_min , total_max ) 
                
                g <- g  + 
                        coord_cartesian(xlim = cem_ranges$x, ylim = cem_ranges$y, expand = TRUE)
                
                #g
                g<- switch(EXPR = as.character(input$row_unfolding_labels), 
                           "TRUE" = (g + geom_text_repel(data = row_df, aes(x = D1, y = D2 , label = DMU), color = "skyblue") ),
                           "FALSE" = g 
                )
                
                g<- switch(EXPR = as.character(input$col_unfolding_labels), 
                           "TRUE" = (g + geom_text_repel(data = col_df, aes(x = D1, y = D2 , label = DMU), color = "orange") ),
                           "FALSE" = g 
                )
                
                g
        })
        
        output$cem_mdu_plot <- renderPlot({
                cem_unfolding_plot()
        })
        
        #for zooming by double click
        observeEvent(input$cem_dblclick, {
                brush <- input$cem_brush
                if (!is.null(brush)) {
                        cem_ranges$x <- c(brush$xmin, brush$xmax)
                        cem_ranges$y <- c(brush$ymin, brush$ymax)
                        
                } else {
                        cem_ranges$x <- NULL
                        cem_ranges$y <- NULL
                }
        })
        
        #Info of CEM unfolding
        output$cem_mdu_info <- renderText({
                t <- cem_reactive()
                #t <- ifelse(test = input$cem_approach == "Benevolent" , benevolent_cem_reactive() , aggressive_cem_reactive() ) 
                t<- unfolding(delta = round((1-t),1),ndim = 2)
                t$stress
        })
        
        #download the graph
        output$download_cem_unfolding <- downloadHandler(
                filename = "CEM_MDU.png",
                content = function(file) {
                        #png(file)
                        #print(cem_unfolding_plot())
                        #dev.off()
                        ggsave(file, plot = cem_unfolding_plot(), device = "png", dpi = 450)
                }
        ) 
        
        output$cem_brush_info <- DT::renderDataTable({
                
                #cem_for_brush <- CEM(dataset = final_dataset_reactive())  
                cem_for_brush <- cem_reactive()
                avg_cross_eff <- round(apply(cem_for_brush,MARGIN = 2, FUN = mean),3)
                simple_efficiency <- round(diag(cem_for_brush),3)
                
                t <- cem_unfolding()
                row_df <- data.frame(t$conf.row, "DMU" = c(1:nrow(t$conf.row)), Type = "Rating", `Average Cross-Efficiency` = avg_cross_eff , `Simple Efficiency` = simple_efficiency  ) 
                col_df <- data.frame(t$conf.col, "DMU" = c(1:nrow(t$conf.col)), Type = "Rated", `Average Cross-Efficiency` = avg_cross_eff , `Simple Efficiency` = simple_efficiency ) 
                unfolded_df  <- rbind(row_df,col_df)
                
                res <- brushedPoints(unfolded_df, input$cem_brush)
                datatable(res)
                
        })
        
        #------ Weight dotplots         
        
        input_weights_dotplot_dataset <- eventReactive(input$cem_mdu_button,{
                dataset <- final_dataset_reactive()
                number_of_inputs <- input$num_of_inputs
                
                t<- ben_weights(dataset, number_of_inputs)
                t<- weight_standardization(weight_dataset = t , number_of_inputs = number_of_inputs)[[1]]
                
                return(t)
        })
        
        output_weights_dotplot_dataset <- eventReactive(input$cem_mdu_button,{
                dataset <- final_dataset_reactive()
                number_of_inputs <- input$num_of_inputs
                
                t<- ben_weights(dataset, number_of_inputs)
                t<- weight_standardization(weight_dataset = t , number_of_inputs = number_of_inputs)[[2]]
                
                return(t)
        })
        
        output$cem_dotplot_dmu_selection_ui <- renderUI({
                dataset <- final_dataset_reactive()
                
                selectInput(inputId = "cem_dotplot_dmu_selection",
                            label = "Highlight a DMU in Dotplot Tab",
                            choices = c("None",1:nrow(dataset))
                )
        })
        
        
        
        input_weight_plot_list_func <- reactive({
                
                dataset <- input_weights_dotplot_dataset()
                
                color <- rep("Others",nrow(dataset))
                if (input$cem_dotplot_dmu_selection != "None") {color[as.numeric(input$cem_dotplot_dmu_selection)] <- "Selected"}
                dataset$color <- as.factor(color)
                
                # for removing legend when there is no selected DMU
                if (input$cem_dotplot_dmu_selection == "None" ) {
                        all_plots <- lapply(X = 1:(ncol(dataset)-1), function(x) ggplot()+
                                                    geom_dotplot(data = dataset, aes(x = dataset[,x],fill = color) , alpha = 0.6)+ 
                                                    theme_linedraw()+
                                                    xlab(colnames(dataset)[x]) +  
                                                    scale_fill_manual(name = "DMUs", values =  c("Others"="blue" , "Selected"="orange")) +
                                                    guides(fill = FALSE )
                                            
                        )
                        
                } else {
                        all_plots <- lapply(X = 1:(ncol(dataset)-1), function(x) ggplot()+
                                                    geom_dotplot(data = dataset, aes(x = dataset[,x],fill = color) , alpha = 0.6)+ 
                                                    theme_linedraw()+
                                                    xlab(colnames(dataset)[x]) +  
                                                    scale_fill_manual(name = "DMUs", values =  c("Others"="blue" , "Selected"="orange")) 
                                            
                        )
                        
                }
                
                if (length(all_plots)==0) return(NULL)
                
                return(all_plots)
                #grid.arrange(grobs=ptlist,ncol=2)
                
        })
        
        input_weights_plot_grid_func <- reactive({
                ptlist <- input_weight_plot_list_func()
                grid.arrange(grobs=ptlist,ncol=2)
        })
        
        output$download_cem_input_dotplot <- downloadHandler(
                filename = "CEM_weight_input_dotplot.png",
                content = function(file) {
                        ptlist <- input_weight_plot_list_func()
                        ggsave(file, arrangeGrob(grobs = ptlist , ncol = 3),device = "png", dpi = 450)
                        #ggsave(file, plot = cem_unfolding_plot(), device = "png", dpi = 450)
                }
        ) 
        
        
        output$input_weights_dotplots <-  renderPlot({
                #dotplot_plot_func()
                input_weights_plot_grid_func()
        })
        
        
        output_weight_plot_list_func <- reactive({
                
                dataset <- output_weights_dotplot_dataset()
                
                color <- rep("Others",nrow(dataset))
                if (input$cem_dotplot_dmu_selection != "None") {color[as.numeric(input$cem_dotplot_dmu_selection)] <- "Selected"}
                dataset$color <- as.factor(color)
                
                # for removing legend when there is no selected DMU
                if (input$cem_dotplot_dmu_selection == "None" ) {
                        all_plots <- lapply(X = 1:(ncol(dataset)-1), function(x) ggplot()+
                                                    geom_dotplot(data = dataset, aes(x = dataset[,x],fill = color) , alpha = 0.6)+ 
                                                    theme_linedraw()+
                                                    xlab(colnames(dataset)[x]) +  
                                                    scale_fill_manual(name = "DMUs", values =  c("Others"="blue" , "Selected"="orange")) +
                                                    guides(fill = FALSE )
                                            
                        )
                        
                } else {
                        all_plots <- lapply(X = 1:(ncol(dataset)-1), function(x) ggplot()+
                                                    geom_dotplot(data = dataset, aes(x = dataset[,x],fill = color) , alpha = 0.6)+ 
                                                    theme_linedraw()+
                                                    xlab(colnames(dataset)[x]) +  
                                                    scale_fill_manual(name = "DMUs", values =  c("Others"="blue" , "Selected"="orange")) 
                                            
                        )
                        
                }
                
                
                if (length(all_plots)==0) return(NULL)
                
                return(all_plots)
                
        })
        
        output_weights_plot_grid_func <- reactive({
                ptlist <- output_weight_plot_list_func()
                grid.arrange(grobs=ptlist,ncol=2)
        })
        
        output$download_cem_input_dotplot <- downloadHandler(
                filename = "CEM_weight_output_dotplot.png",
                content = function(file) {
                        ptlist <- output_weight_plot_list_func()
                        ggsave(file, arrangeGrob(grobs = ptlist , ncol = 3),device = "png", dpi = 450)
                        #ggsave(file, plot = cem_unfolding_plot(), device = "png", dpi = 450)
                }
        ) 
        
        output$output_weights_dotplots <-  renderPlot({
                output_weights_plot_grid_func()
        })
        
        
        
        
        
        
        
        output$cem_weights_info <- DT::renderDataTable({
                
                dataset <- final_dataset_reactive()
                number_of_inputs <- input$num_of_inputs
                t<- ben_weights(dataset, number_of_inputs)
                #head(dataset)
                datatable(t)
                
        })
        
        output$cem_weights_std_info <- DT::renderDataTable({
                dataset <- final_dataset_reactive()
                number_of_inputs <- input$num_of_inputs
                
                t<- ben_weights(dataset, number_of_inputs)
                t<- weight_standardization(weight_dataset = t , number_of_inputs = number_of_inputs) 
                
                datatable(cbind(t[[1]],t[[2]]))
        })       
        
        output$all_data <- DT::renderDataTable({
                
                dataset <- final_dataset_reactive()
                #number_of_inputs <- input$num_of_inputs
                #t<- ben_weights(dataset, number_of_inputs)
                #head(dataset)
                datatable(dataset)
                
        })
        
        #---- weight biplot tabset of CEM  
        
        opt_weight_mds_data <- eventReactive(input$cem_mdu_button,{
                # --- Mds data
                dataset <- final_dataset_reactive()
                weights <- ben_weights(dataset = dataset , number_of_inputs = input$num_of_inputs)
                std_weights_list <- weight_standardization(weight_dataset = weights, number_of_inputs = 3)
                std_weights_mat <-cbind(std_weights_list[[1]],std_weights_list[[2]])
                
                #print(paste0("std_weights_mat dim:",dim(std_weights_mat)))
                
                colnames(std_weights_mat) <- paste(colnames(dataset),"weight",sep = " ")
                
                #print(paste0("colnames of std_weights_mat:", colnames(std_weights_mat)))
                
                std_weights_dist <- dist(x = std_weights_mat, method = "euclidean")
                mds_model <- smacofSym(delta = std_weights_dist, ndim = 2 , type = "ratio"  )
                
                #print(paste0("dim of std_weights_dist", dim(matrix(std_weights_dist))) )
                #print(paste0("mds_model conf dim:", dim(mds_model$conf)))
                #print(head(mds_model$conf))
                
                t <- list(weights = data.frame(std_weights_mat), mds_coo = data.frame(DMU = 1:nrow(dataset) , mds_model$conf) ) 
                #print(head(t[[1]]))
                #print(head(t[[2]]))
                
                return(t)
                
        })
        
        opt_weight_biplot_data <- eventReactive(input$cem_mdu_button,{
                
                dataset <- final_dataset_reactive()
                weights <- ben_weights(dataset = dataset , number_of_inputs = input$num_of_inputs)
                std_weights_list <- weight_standardization(weight_dataset = weights, number_of_inputs = 3)
                std_weights_mat <-cbind(std_weights_list[[1]],std_weights_list[[2]])
                colnames(std_weights_mat) <- paste(colnames(dataset),"weight",sep = " ")
                weights_pca <- prcomp(std_weights_mat)
                
                coordinates <- data.frame(DMU = 1:nrow(dataset), weights_pca$x[,1:2]) 
                vectors <- data.frame(Variables = (rownames(weights_pca$rotation)), weights_pca$rotation[, 1:2])
                
                list(coordinates,vectors)
                
        })
        
        cem_weight_plot_ranges <- reactiveValues(x = NULL, y = NULL)
        
        cem_biplot_func <- reactive({
                #----biplot version
                z <- opt_weight_biplot_data()
                z1 <- z[[1]] #coordinates
                z2 <- z[[2]] #vectors
                
                #print("after Z1 and Z2")
                
                total_min <-  min(z1$PC1,z1$PC2)
                total_max <- max(z1$PC1,z1$PC2)
                total_max <- ifelse(test = (total_max>0), yes = total_max, no = -total_max)
                
                
                limit_range  <-   max(abs(total_min),abs(total_max))
                
                g <- ggplot() + 
                        geom_point(data = z1, aes(x = PC1,y = PC2),size=input$cem_biplot_point_size, alpha = input$cem_biplot_point_transparency, color = "blue" ) +
                        #scale_colour_manual(name = "DMU: CRS Eff.", values =  c("Efficient"="gold" , "Inefficient"="skyblue")) + 
                        geom_segment(data=z2, 
                                     aes(PC1*input$cem_biplot_vector_size/2, PC2*input$cem_biplot_vector_size/2, xend=0, yend=0), 
                                     col="red",alpha = input$cem_biplot_vector_transparency ) +
                        geom_text_repel(data=z2 ,
                                        aes(x = PC1*input$cem_biplot_vector_size/2,y =  PC2*input$cem_biplot_vector_size/2, label = Variables ),
                                        col="red", alpha = input$cem_biplot_vector_transparency, size = input$cem_biplot_vector_text_size )
                
                g<- g+ theme_linedraw()  + annotate("text", x = Inf, y = -Inf, label = "© DEA-Viz",
                                                    hjust=1.1, vjust=-1.1, col="blue", cex=6,
                                                    fontface = "bold", alpha = 0.4) +
                        xlim(-total_max, total_max) +
                        ylim(-total_max, total_max) +
                        coord_fixed(ratio = 1) + 
                        ggtitle("Optimum Weights PCA Biplot") 
                
                g <- g  + 
                        coord_cartesian(xlim = cem_weight_plot_ranges$x, ylim = cem_weight_plot_ranges$y, expand = TRUE)
                
                switch(EXPR = as.character(input$cem_biplot_point_labels), 
                       "TRUE" = (g + geom_text_repel(data = z1, aes(x = PC1, y = PC2 , label = DMU), color = "blue") ),
                       "FALSE" = g 
                )
                
        })
        
        cem_mds_func <- reactive({
                
                mds_data <- opt_weight_mds_data()
                
                points <- cbind(mds_data[[2]],mds_data[[1]])
                
                selected_var <- input$cem_mds_var_selection_input
                
                if( is.null(selected_var) ) {
                        index <- 0 
                } else if (selected_var == "None" ) {
                        index <- 0 
                } else {
                        index <-  which(selected_var == colnames(points) )    
                }
                
                
                
                total_min <-  min(points$D1,points$D2)
                total_max <- max(points$D1,points$D2)
                
                
                # here shows an error because of the input$cem_mds_var_selection_input inavailability at
                # the start of the tabset section 
                # what to do? may be delay works 
                
                
                if (index != 0 ) {
                        g <- ggplot() + 
                                geom_point(data = points, aes(D1, D2, color = points[,index]),
                                           
                                           size=input$cem_biplot_point_size,
                                           alpha = input$cem_biplot_point_transparency) +
                                scale_colour_gradient(low = "blue", high = "red", name = colnames(points)[index] )
                } else {
                        g <- ggplot() + 
                                geom_point(data = points, aes(D1, D2),
                                           color = "blue",
                                           size=input$cem_biplot_point_size,
                                           alpha = input$cem_biplot_point_transparency) 
                        #scale_colour_gradient(low = "blue", high = "red", name = colnames(points)[index] ) 
                }
                
                g<- g+ theme_linedraw()  + annotate("text", x = Inf, y = -Inf, label = "© DEA-Viz",
                                                    hjust=1.1, vjust=-1.1, col="blue", cex=6,
                                                    fontface = "bold", alpha = 0.4) +
                        xlim(total_min, total_max) +
                        ylim(total_min, total_max) +
                        coord_fixed(ratio = 1) + 
                        ggtitle("Optimum Weights MDS map") 
                
                g <- g  + 
                        coord_cartesian(xlim = cem_weight_plot_ranges$x, ylim = cem_weight_plot_ranges$y, expand = TRUE)
                
                switch(EXPR = as.character(input$cem_biplot_point_labels), 
                       "TRUE" = (g + geom_text_repel(data = points, aes(x = D1, y = D2 , label = DMU), color = "blue") ),
                       "FALSE" = g 
                )
                
        })
        
        
        cem_weights_pca_mds_plot <- reactive({
                switch(EXPR = input$cem_weight_plot_method, 
                       "PCA" = cem_biplot_func(),
                       "MDS" = cem_mds_func()
                )
                
        })
        
        
        output$opt_weights_plot <- renderPlot({
                
                cem_weights_pca_mds_plot()
        })
        
        observeEvent(input$opt_weights_dblclick, {
                brush <- input$opt_weights_brush
                if (!is.null(brush)) {
                        cem_weight_plot_ranges$x <- c(brush$xmin, brush$xmax)
                        cem_weight_plot_ranges$y <- c(brush$ymin, brush$ymax)
                        
                } else {
                        cem_weight_plot_ranges$x <- NULL
                        cem_weight_plot_ranges$y <- NULL
                }
        })
        
        output$cem_opt_weights_brush_info <- DT::renderDataTable({
                
                dataset <- switch(EXPR = input$cem_weight_plot_method, 
                                  "PCA" = opt_weight_biplot_data()[[1]],
                                  "MDS" = cbind(opt_weight_mds_data()[[1]],opt_weight_mds_data()[[2]])
                                  
                                  #z <- opt_weight_biplot_data()
                )
                res <- brushedPoints(dataset, input$opt_weights_brush)
                datatable(res)
        })
        
        output$download_cem_weight_plot <- downloadHandler(
                filename = "CEM_weight_plot.png",
                content = function(file) {
                        ggsave(file, plot = cem_weights_pca_mds_plot(), device = "png", dpi = 450)
                }
        ) 
        
        
        output$cem_mds_var_selection_output <- renderUI({
                dataset <- final_dataset_reactive()
                variables <- c("None",paste(colnames(dataset),"weight",sep = ".") )
                
                switch(EXPR = input$cem_weight_plot_method,
                       "MDS" = selectInput(inputId = "cem_mds_var_selection_input",
                                           label = "Opt. Weight to Color",
                                           choices = variables
                       ), 
                       "PCA" = NULL
                )
                
        })
        
        
        
        
        #End of CEM MDU
        
        #####     
        
        
        
        
        
        
        
        # Porembski
        #####
        
        porembski_points <- eventReactive(input$Porembski_button, {
                set.seed(7)
                
                t <- final_dataset_reactive()
                num_of_inputs <- input$num_of_inputs
                
                Porembsky_data = scale(t)
                Porembsky_sammon = sammon(d = dist(Porembsky_data),k=2,niter = 10000 )
                
                porembski_crs <- crs_eff(t,num_of_inputs)
                porembski_vrs <- vrs_eff(t,num_of_inputs)
                
                porembski_eff <- switch(input$Porembski_dea_model, 
                                        "CRS" = porembski_crs,
                                        "VRS" = porembski_vrs)
                
                Porembsky_df=data.frame(DMU=1:nrow(t),Porembsky_sammon$points,Efficiency = porembski_eff$eff)
                Porembsky_df$color = as.character(ifelse(Porembsky_df$Efficiency==1,"Efficient","Inefficient"))
                Porembsky_df
        })
        
        porembski_edges <- eventReactive(input$Porembski_button, {
                
                set.seed(7)
                
                t <- final_dataset_reactive()
                num_of_inputs <- input$num_of_inputs
                
                Porembski_data = scale(t)
                Porembski_sammon = sammon(d = dist(Porembski_data),k=2,niter = 10000 )
                
                porembski_crs <- crs_eff(t,num_of_inputs)
                porembski_vrs <- vrs_eff(t,num_of_inputs)
                Porembski_eff <- switch(input$Porembski_dea_model, 
                                        "CRS" = porembski_crs,
                                        "VRS" = porembski_vrs)
                
                Porembski_links_df = data.frame("start.DMU"= NA,"end.DMU"=NA,"x1"=NA,"y1"=NA,"x2"=NA,"y2"=NA,"alpha"=NA)
                
                index = 0 
                for (r in 1:nrow(t)) {
                        
                        for (c in 1:nrow(t)){
                                index = index + 1 
                                Porembski_links_df[index,] = c(r,c,Porembski_sammon$points[r,1],Porembski_sammon$points[r,2],Porembski_sammon$points[c,1],Porembski_sammon$points[c,2],Porembski_eff$lambda[r,c])
                        } 
                }
                
                Porembski_links_df
                
        })
        
        Porembski_ranges <- reactiveValues(x = NULL, y = NULL)
        
        Porembski_plot_reactive <- reactive({
                edges_df <- porembski_edges()
                points_df <- porembski_points()
                
                #x_min <- min(points_df$X1)
                #x_max <- max(points_df$X1)
                #y_min <- min(points_df$X2)
                #y_max <- max(points_df$X2)
                #x_range <- x_max - x_min
                #y_range <- y_max - y_min
                
                total_min <-  min(points_df$X1,points_df$X2)
                total_max <- max( points_df$X1,points_df$X2)
                
                dea_model <- isolate(input$Porembski_dea_model)
                
                
                g = ggplot() + geom_point(data = points_df , aes(x = X1 , y = X2,  color =color ),
                                          size = input$Porembski_point_size,
                                          alpha = input$Porembski_point_transparency) +
                        scale_color_manual(name = paste("DMU",dea_model,sep = ":"),values = c("Efficient" = "gold", "Inefficient"= "skyblue"))
                
                
                for (index in 1:nrow(edges_df)) {
                        if (edges_df[index,"alpha"] > input$Porembski_edge_treshold) {
                                g = g  + geom_segment(data = edges_df[index,], aes(x = x1 , y = y1 , xend = x2 , yend = y2),alpha = round(edges_df[index,"alpha"],2)/input$Porembski_edge_transparency , color = "red" )
                        }
                }
                g = g + coord_fixed(ratio = 1,  expand = TRUE)
                
                g<- g+ theme_linedraw()  + annotate("text", x = Inf, y = -Inf, label = "© DEA-Viz",
                                                    hjust=1.1, vjust=-1.1, col="blue", cex=6,
                                                    fontface = "bold", alpha = 0.4) + 
                        xlim(total_min , total_max) +
                        ylim(total_min , total_max ) +
                        coord_fixed(ratio = 1) + 
                        ggtitle("DEA Porembski Graph")
                g <- g  + 
                        coord_cartesian(xlim = Porembski_ranges$x, ylim = Porembski_ranges$y, expand = TRUE)
                
                #g
                switch(EXPR = as.character(input$Porembski_labels), 
                       "TRUE" = (g + geom_text_repel(data = points_df, aes(x = X1, y = X2 , label = DMU), color = "blue") ),
                       "FALSE" = g 
                )
                
        })
        
        
        output$Porembski_plot <- renderPlot({
                Porembski_plot_reactive()
        })
        
        
        observeEvent(input$Porembski_dblclick, {
                brush <- input$Porembski_brush
                if (!is.null(brush)) {
                        Porembski_ranges$x <- c(brush$xmin, brush$xmax)
                        Porembski_ranges$y <- c(brush$ymin, brush$ymax)
                        
                } else {
                        Porembski_ranges$x <- NULL
                        Porembski_ranges$y <- NULL
                }
        })
        
        
        
        output$download_Porembski_graph <- downloadHandler(
                filename = "Porembski_graph.png",
                content = function(file) {
                        #png(file)
                        #print(cem_unfolding_plot())
                        #dev.off()
                        ggsave(file, plot = Porembski_plot_reactive(), device = "png", dpi = 450)
                }
        ) 
        
        output$Porembski_info <- renderTable({
                head(porembski_points())
        })
        
        output$Proembski_brush_info <- DT::renderDataTable({
                points_df <- porembski_points()
                
                res <- brushedPoints(points_df, input$Porembski_brush)
                datatable(res)
                
        })
        
        #####
        
        # PCA Biplot
        #####
        biplot_data <- eventReactive(input$biplot_button,{
                t<- final_dataset_reactive()
                t <- scale(t)
                prcomp(t)
        })
        
        biplot_supplement_data <- eventReactive(input$biplot_button,{
                t<- final_dataset_reactive()
                number_of_inputs <- input$num_of_inputs
                
                coordinates <- prcomp(t)$x[,1:2]
                crs_efficiency <- crs_eff(t,number_of_inputs)$eff
                vrs_efficiency <- vrs_eff(t,number_of_inputs)$eff
                
                data.frame(coordinates,crs_efficiency, vrs_efficiency)
        })
        
        biplot_ranges <- reactiveValues(x = NULL, y = NULL)
        
        biplot_plot <- reactive({
                x <- biplot_data()
                supp <- biplot_supplement_data()
                data <- final_dataset_reactive()
                data$shape <- switch(EXPR = input$biplot_dea_model, 
                                     "CRS" = ifelse(test = supp$crs_efficiency ==1 , 1, 19),
                                     "VRS" = ifelse(test = supp$vrs_efficiency ==1 , 1, 19) )
                
                
                
                
                z1 <- data.frame(DMU = 1:nrow(data), x$x[, 1:2], crs_color = NA , vrs_color = NA)
                z1$crs_color <- ifelse(test = supp$crs_efficiency ==1 , "Efficient", "Inefficient")
                #print("crs shapes of biplot")
                #print(z1$crs_shape)
                z1$vrs_color <- ifelse(test = supp$vrs_efficiency ==1 , "Efficient", "Inefficient")
                
                
                z2 <- data.frame(Variables = (rownames(x$rotation)), x$rotation[, 1:2])
                
                #print("z1 of the biplot")
                #print(head(z1))
                
                total_min <-  min(z1$PC1,z1$PC2)
                total_max <- max( z1$PC1,z1$PC2)
                
                #x_min <- min(z1$PC1)
                #x_max <- max(z1$PC1)
                #y_min <- min(z1$PC2)
                #y_max <- max(z1$PC2)
                #x_range <- x_max - x_min
                #y_range <- y_max - y_min
                
                
                g <- switch(EXPR = input$biplot_dea_model,
                            "CRS" = ggplot(z1, aes(x = PC1,y = PC2)) + 
                                    geom_point(aes(color = z1$crs_color) ,size=input$biplot_point_size, alpha = input$biplot_point_transparency ) +
                                    scale_colour_manual(name = "DMU: CRS Eff.", values =  c("Efficient"="gold" , "Inefficient"="skyblue")) + 
                                    geom_segment(data=z2, aes(PC1*input$biplot_vector_size, PC2*input$biplot_vector_size, xend=0, yend=0), col="red",alpha = input$biplot_vector_transparency ) +
                                    geom_text_repel(data=z2 , aes(x = PC1*input$biplot_vector_size,y =  PC2*input$biplot_vector_size, label = Variables ), col="red", alpha = input$biplot_vector_transparency, size = input$biplot_vector_text_size )  , 
                            
                            "VRS" =  ggplot(z1, aes(x = PC1,y = PC2)) + 
                                    geom_point(aes(color = z1$vrs_color) ,size=input$biplot_point_size, alpha = input$biplot_point_transparency) +
                                    scale_colour_manual(name = "DMU: VRS Eff.", values =  c("Efficient"="gold" , "Inefficient"="skyblue")) + 
                                    geom_segment(data=z2, aes(PC1*input$biplot_vector_size, PC2*input$biplot_vector_size, xend=0, yend=0), col="red",alpha = input$biplot_vector_transparency ) +
                                    geom_text_repel(data=z2 , aes(x = PC1*input$biplot_vector_size,y =  PC2*input$biplot_vector_size, label = Variables ), col="red", alpha = input$biplot_vector_transparency, size = input$biplot_vector_text_size ) 
                            
                )
                
                #g<- ggplot(z1, aes(x = PC1,y = PC2), shape = z1$shape) + 
                #        geom_point( size=input$biplot_point_size, alpha = input$biplot_point_transparency) +
                #        geom_segment(data=z2, aes(PC1*input$biplot_vector_size, PC2*input$biplot_vector_size, xend=0, yend=0), col="red",alpha = input$biplot_vector_transparency ) +
                #        geom_text(data=z2 , aes(x = PC1*input$biplot_vector_size,y =  PC2*input$biplot_vector_size, label = Variables ), col="red", alpha = input$biplot_vector_transparency, size = input$biplot_vector_text_size ) 
                
                
                
                g<- g+ theme_linedraw()  + annotate("text", x = Inf, y = -Inf, label = "© DEA-Viz",
                                                    hjust=1.1, vjust=-1.1, col="blue", cex=6,
                                                    fontface = "bold", alpha = 0.4) +
                        xlim(total_min, total_max) +
                        ylim(total_min, total_max) +
                        coord_fixed(ratio = 1) + 
                        ggtitle("DEA PCA Biplot") 
                
                
                g <- g  + 
                        coord_cartesian(xlim = biplot_ranges$x, ylim = biplot_ranges$y, expand = TRUE)
                
                switch(EXPR = as.character(input$biplot_labels), 
                       "TRUE" = (g + geom_text_repel(data = z1, aes(x = PC1, y = PC2 , label = DMU), color = "blue") ),
                       "FALSE" = g 
                )
                
                
        })
        
        
        output$biplot_plot <- renderPlot({
                biplot_plot()
        })
        
        
        # When a double-click happens, check if there's a brush on the plot.
        # If so, zoom to the brush bounds; if not, reset the zoom.
        observeEvent(input$biplot_dblclick, {
                brush <- input$biplot_brush
                if (!is.null(brush)) {
                        biplot_ranges$x <- c(brush$xmin, brush$xmax)
                        biplot_ranges$y <- c(brush$ymin, brush$ymax)
                        
                } else {
                        biplot_ranges$x <- NULL
                        biplot_ranges$y <- NULL
                }
        })
        
        output$download_biplot <- downloadHandler(
                filename = "biplot.png",
                content = function(file) {
                        #png(file)
                        #print(biplot_plot())
                        #dev.off()
                        ggsave(file, plot = biplot_plot(), device = "png", dpi = 450)
                }
        ) 
        
        output$biplot_info <- renderTable({
                head(biplot_supplement_data())
                #biplot_data()
                #
        })
        
        output$biplot_brush_info <- DT::renderDataTable({
                x <- biplot_data()
                supp <- biplot_supplement_data()
                data <- final_dataset_reactive()
                z1 <- data.frame(DMU = 1:nrow(data),crs_eff=  round(supp$crs_efficiency,3), vrs_eff=  round(supp$vrs_efficiency,3), round(x$x[, 1:2],3))
                #data$shape <- switch(EXPR = input$biplot_dea_model, 
                #                     "CRS" = ifelse(test = supp$crs_efficiency ==1 , 1, 19),
                #                     "VRS" = ifelse(test = supp$vrs_efficiency ==1 , 1, 19) )
                
                
                
                # we want crs_eff or vrs_eff , DMU id, maybe original data of the points as well
                #z1 <- data.frame(DMU = 1:nrow(data), x$x[, 1:2], crs_color = NA , vrs_color = NA)
                #z1$crs_color <- ifelse(test = supp$crs_efficiency ==1 , "Efficient", "Inefficient")
                #print("crs shapes of biplot")
                #print(z1$crs_shape)
                #z1$vrs_color <- ifelse(test = supp$vrs_efficiency ==1 , "Efficient", "Inefficient")
                #cat("input$biplot_brush:\n")
                #str(input$biplot_brush)
                res <- brushedPoints(z1, input$biplot_brush)
                datatable(res)
                
        })
        
        #####
        
        # SOM DEA
        #####
        
        som_data <- eventReactive(input$som_button,{
                t <- final_dataset_reactive()
                scale(t)
        })
        
        som_plot_func <- reactive({
                set.seed(7)
                t<- som_data()
                d<- final_dataset_reactive()
                
                number_of_inputs = input$num_of_inputs
                
                crs_efficiency <- crs_eff(d,number_of_inputs)$eff
                vrs_efficiency <- vrs_eff(d,number_of_inputs)$eff
                
                
                horizontal_nodes = input$som_h_size
                vertical_nodes = input$som_v_size 
                total_nodes = horizontal_nodes*vertical_nodes
                
                som_factor = som(X = t, 
                                 grid = somgrid(xdim =horizontal_nodes, ydim = vertical_nodes, topo = "hexagonal" ),
                                 rlen = 10000 ,
                                 init = t[sample(x = nrow(t), size = total_nodes, replace = TRUE),], 
                                 keep.data = TRUE)
                
                coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
                jitter = matrix(rnorm(n= nrow(t)*2, mean = 0,sd = 0.1),ncol = 2)
                
                som_eff_vec = vector(length = total_nodes)
                
                #for (node in 1:total_nodes) {
                #       som_eff_vec[node] = mean(japan_crs$eff[which(japan_som_factor$unit.classif==node)] )
                #}
                
                som_eff_vec_sapply= switch(EXPR = input$som_dea_model, 
                                           "CRS" = sapply(X = 1:total_nodes, function(x) mean(crs_efficiency[which(som_factor$unit.classif==x)] )  ),
                                           "VRS" = sapply(X = 1:total_nodes, function(x) mean(vrs_efficiency[which(som_factor$unit.classif==x)] )  )
                )
                
                g<- plot(som_factor, type = "property", property = som_eff_vec_sapply, palette.name = coolBlueHotRed , main = "SOM DEA" )
                
                g<- switch(EXPR = input$som_labels ,
                           "Yes" = g+ text(x = som_factor$grid$pts[som_factor$unit.classif,1] + jitter[,1], y = som_factor$grid$pts[som_factor$unit.classif,2] + jitter[,2],labels = 1:nrow(t) , col = alpha("white",1)    ),
                           "No" = g 
                )
                #g <- g+ text(x = som_factor$grid$pts[som_factor$unit.classif,1] + jitter[,1], y = som_factor$grid$pts[som_factor$unit.classif,2] + jitter[,2],labels = 1:nrow(t) , col = alpha("white",1)    )
                #g
                
                
                
        })
        
        
        output$som_plot <- renderPlot({
                som_plot_func()
        })
        
        
        som_properties_plot <- reactive({
                set.seed(7)
                t<- som_data()
                d<- final_dataset_reactive()
                
                number_of_inputs = input$num_of_inputs
                horizontal_nodes = input$som_h_size
                vertical_nodes = input$som_v_size 
                total_nodes = horizontal_nodes*vertical_nodes
                
                som_factor = som(X = t, 
                                 grid = somgrid(xdim =horizontal_nodes, ydim = vertical_nodes, topo = "hexagonal" ),
                                 rlen = 10000 ,
                                 init = t[sample(x = nrow(t), size = total_nodes, replace = TRUE),], 
                                 keep.data = TRUE)
                
                coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
                jitter = matrix(rnorm(n= nrow(t)*2, mean = 0,sd = 0.1),ncol = 2)
                
                plot_titles <- colnames(d)
                #plots = list()
                
                #for (index in 1:ncol(d)) {
                #        p1 = plot(som_factor, type = "property", property = som_factor$codes[[1]][,index], palette.name = coolBlueHotRed  )
                #        plots[[index]] <- p1  
                #}
                
                #multiplot(plotlist = plots, cols = 3)
                
                num_of_plots<-ncol(t)
                plot_rows =as.integer(num_of_plots/3) + 1 
                
                par(mfrow=c(plot_rows,3))
                for (index in 1:ncol(d)) {
                        plot(som_factor, type = "property", property = som_factor$codes[[1]][,index], palette.name = coolBlueHotRed, main = plot_titles[index]  )
                        #plots[[index]] <- p1  
                }
        })
        
        output$som_var_plots <- renderPlot({
                
                som_properties_plot()
                
        })
        
        output$download_som_main <- downloadHandler(
                filename = "SOM_Main.png",
                content = function(file) {
                        #png(file)
                        #print(biplot_plot())
                        #dev.off()
                        ggsave(file, plot = som_plot_func(), device = "png", dpi = 450)
                }
        ) 
        
        output$download_som_properties <- downloadHandler(
                filename = "SOM_properties.png",
                content = function(file) {
                        #png(file)
                        #print(biplot_plot())
                        #dev.off()
                        ggsave(file, plot = som_properties_plot(), device = "png", dpi = 450)
                }
        ) 
        ######
        # Costa Frontier 
        #####
        
        Costa_df <- eventReactive(input$Costa_button,{
                data <- final_dataset_reactive()
                number_of_inputs <- input$num_of_inputs
                
                # "CRS-Input Oriented","CRS-Output Oriented", "VRS-Input Oriented","VRS-Output Oriented"
                #eff <- switch(EXPR = input$Costa_dea_model,
                #               "CRS-Input Oriented" = crs_eff(dataset = data, num_of_inputs = number_of_inputs , orientation = "in"), 
                #               "CRS-Output Oriented"= crs_eff(dataset = data, num_of_inputs = number_of_inputs , orientation = "out") 
                #"VRS-Input Oriented" = vrs_eff(dataset = data, num_of_inputs = number_of_inputs , orientation = "in") 
                #"VRS-Output Oriented" = vrs_eff(dataset = data, num_of_inputs = number_of_inputs , orientation = "out")
                #)
                
                model <- switch(EXPR = input$Costa_dea_model,
                                "CRS-Input Oriented" = "CRS-In",
                                "CRS-Output Oriented" = "CRS-Out"
                                
                )
                
                if (model == "CRS-In") {
                        
                        # efficiency canlculations
                        eff_crs_in <- crs_eff(dataset = data, num_of_inputs = number_of_inputs , orientation = "in")
                        
                        #Normalization Factor
                        S_crs_inorient <- apply(X = eff_crs_in$ux,MARGIN = 1 , FUN = sum)
                        
                        # modified virtual factors
                        modified_vi_inorient <- eff_crs_in$ux/ S_crs_inorient
                        modified_vo_inorient <-  eff_crs_in$vy/ S_crs_inorient
                        
                        # virtual weight sums
                        wsum_inputs = round(apply(X = modified_vi_inorient * data[,1:number_of_inputs], MARGIN = 1 , FUN = sum),3)
                        wsum_outputs = round(apply(X = modified_vo_inorient * data[,(number_of_inputs+1):ncol(data)], MARGIN = 1 , FUN = sum),3)
                        
                        #final df
                        Costa_df = data.frame(DMU = c(1:nrow(data)), I =wsum_inputs, O = wsum_outputs, efficiency_binary = ifelse(eff_crs_in$eff==1,"Efficient","Inefficient") ,efficiency =  eff_crs_in$eff)
                        
                        
                }
                else if (model == "CRS-Out") {
                        
                        eff_crs_out <- crs_eff(dataset = data, num_of_inputs = number_of_inputs , orientation = "out") 
                        
                        S_crs_outorient <- apply(X = eff_crs_out$vy,MARGIN = 1 , FUN = sum)
                        
                        modified_vi_outorient <- eff_crs_out$ux / S_crs_outorient
                        modified_vo_outorient <- eff_crs_out$vy / S_crs_outorient
                        
                        wsum_inputs = round(apply(X = modified_vi_outorient * data[,1:number_of_inputs], MARGIN = 1 , FUN = sum),3)
                        wsum_outputs = round(apply(X = modified_vo_outorient * data[,(number_of_inputs+1):ncol(data)], MARGIN = 1 , FUN = sum),3)
                        
                        Costa_df = data.frame(DMU = c(1:nrow(data)), I =wsum_inputs, O = wsum_outputs, efficiency_binary = ifelse(eff_crs_out$eff==1,"Efficient","Inefficient"), efficiency =  eff_crs_out$eff)
                        
                }
                
                #label = rep(NA,47)
                #label[which(Costa_df$I==Costa_df$O)] = which(Costa_df$I==Costa_df$O)
                
        })
        
        Costa_ranges <- reactiveValues(x = NULL, y = NULL)
        
        Costa_plot_func <- reactive({
                
                t <- Costa_df()
                
                #x_min <- min(t$I)
                #x_max <- max(t$I)
                #y_min <- min(t$O)
                #y_max <- max(t$O)
                #x_range <- x_max - x_min
                #y_range <- y_max - y_min
                
                total_min <-  min(t$I,t$O)
                total_max <- max(t$I,t$O)
                
                g = ggplot() +
                        geom_point(data = t, aes(x = I , y = O, colour = efficiency_binary ), 
                                   size = input$Costa_point_size , 
                                   alpha = input$Costa_point_transparency) + 
                        scale_colour_gradient(low = "red", high = "green") +
                        #scale_shape_manual(values = c(17,16))
                        scale_colour_manual(name = "DMU Color",values = c("Efficient"="gold","Inefficient" = "skyblue"))
                
                g = g + geom_abline(xintercept = 0 , yintercept = 0 , slope = 1, color = "red")
                g = g + coord_fixed(ratio = 1,  expand = TRUE) 
                
                
                
                g<- g+ theme_linedraw()  + annotate("text", x = Inf, y = -Inf, label = "© DEA-Viz",
                                                    hjust=1.1, vjust=-1.1, col="blue", cex=6,
                                                    fontface = "bold", alpha = 0.4) +
                        xlim(total_min , total_max) +
                        ylim(total_min , total_max) +
                        coord_fixed(ratio = 1) + 
                        ggtitle("DEA Costa Frontier")
                
                g<- g + coord_cartesian(xlim = Costa_ranges$x, ylim = Costa_ranges$y, expand = TRUE)
                #g
                switch(EXPR = as.character(input$Costa_labels), 
                       "TRUE" = (g + geom_text_repel(data = t, aes(x = I, y = O , label = 1:nrow(t)), color = "blue") ),
                       "FALSE" = g 
                )
        })
        
        output$Costa_plot <- renderPlot({
                
                Costa_plot_func()
        })
        
        observeEvent(input$Costa_dblclick, {
                brush <- input$Costa_brush
                if (!is.null(brush)) {
                        Costa_ranges$x <- c(brush$xmin, brush$xmax)
                        Costa_ranges$y <- c(brush$ymin, brush$ymax)
                        
                } else {
                        Costa_ranges$x <- NULL
                        Costa_ranges$y <- NULL
                }
        })
        
        output$download_Costa <- downloadHandler(
                filename = "Costa.png",
                content = function(file) {
                        #png(file)
                        #print(biplot_plot())
                        #dev.off()
                        ggsave(file, plot = Costa_plot_func(), device = "png", dpi = 450)
                }
        ) 
        
        output$Costa_info <- renderTable({
                head(Costa_df())
                #biplot_data()
                #
        }) 
        
        output$Costa_brush_info <- DT::renderDataTable({
                data <- Costa_df()
                
                res <- brushedPoints(data, input$Costa_brush)
                datatable(res)
                
        })
        #####
        # MDS Plots 
        #####
        # MDS Plots 
        
        #ratio names function
        ratio_names <- function(){
                dataset<-  final_dataset_reactive()
                number_of_inputs <- input$num_of_inputs
                number_of_outputs <- ncol(dataset)-number_of_inputs
                #print("number of inputs inside ratio names")
                #print(number_of_inputs)
                #print("number of outputs inside ratio names")
                #print(number_of_outputs)
                
                index = 0 
                ratio_vector = vector(length = number_of_inputs*number_of_outputs )
                #print("ratio vector")
                #print(ratio_vector)
                for (o in 1:number_of_outputs ){
                        for (i in 1:number_of_inputs){
                                index <- index + 1
                                #ratio_vector[index]<- paste0("O",o,"/","I",i)
                                ratio_vector[index] <- paste0(colnames(dataset)[number_of_inputs+o],"/",colnames(dataset)[i])
                        }
                }
                #print("ratio_vector")
                #print(ratio_vector)
                return(ratio_vector)
        }
        
        #ratio dataset
        
        # based on the uploaded file, it sets the select box of variables
        output$mds_var_selection_ui <- renderUI({
                dataset<-  final_dataset_reactive()
                number_of_inputs <- input$num_of_inputs
                
                switch(EXPR = input$mds_dataset,
                       "Original Variables" = 
                               selectInput(inputId = "mds_var_selection",
                                           label = "Variable to Visualize",
                                           choices = c(colnames(dataset),"fdh.efficiency","vrs.efficiency","crs.efficiency","drs.efficiency","irs.efficiency","frh.efficiency")
                               ),
                       "Ratio Variables" = 
                               selectInput(inputId = "mds_var_selection",
                                           label = "Variable to Visualize",
                                           choices = c(ratio_names(),"fdh.efficiency","vrs.efficiency","crs.efficiency","drs.efficiency","irs.efficiency","frh.efficiency")
                               )
                )
                
        }) # end of mds renderUI
        
        mds_data_ratio <- function(){
                dataset <- final_dataset_reactive()
                number_of_inputs <- input$num_of_inputs
                number_of_outputs <- ncol(dataset)-number_of_inputs
                
                index = 0 
                ratio_vector <- vector(length = number_of_inputs*number_of_outputs )
                
                number_of_ratios <- number_of_inputs*number_of_outputs*nrow(dataset)
                ratio_mat <- matrix(data = rep(NA,number_of_ratios),nrow = nrow(dataset))
                #print("dim of ratio_mat")
                #print(dim(ratio_mat))
                
                for (o in 1:number_of_outputs ){
                        for (i in 1:number_of_inputs){
                                index <- index + 1
                                #ratio_vector[index]<- paste0("O",o,"/","I",i)
                                ratio_vector[index] <- paste0(colnames(dataset)[number_of_inputs+o],"/",colnames(dataset)[i])
                                ratio_mat[,index] <- dataset[,(number_of_inputs+o)]/dataset[,i]
                        }
                }
                
                ratio_df <- as.data.frame(ratio_mat)
                colnames(ratio_df) = ratio_vector
                
                #print("head of ratio_df")
                #print(head(ratio_df))
                return(ratio_df)
        }
        
        # mds data: original dataset or ratios? 
        mds_data <- reactive({
                switch(EXPR  = input$mds_dataset, 
                       "Original Variables" = final_dataset_reactive(),
                       
                       "Ratio Variables"=  mds_data_ratio()
                       
                ) 
                
        }) 
        
        
        # mds model generation
        mds_model <- eventReactive(input$mds_button,{
                
                mds_data <- mds_data()
                
                mds_dist <- dist(x = mds_data, method = tolower(input$mds_distance))
                
                mds_type <- switch(EXPR = input$mds_model , 
                                   "Ratio" = "ratio",
                                   "Interval" = "interval",
                                   "Ordinal" = "ordinal")
                
                smacofSym(delta = mds_dist, ndim = 2 , type = mds_type  )
                
                
        })
        
        
        final_dataset_to_visualize <- eventReactive(input$mds_button,{
                org_dataset <- final_dataset_reactive()
                number_of_inputs <- input$num_of_inputs
                core_data <- mds_data()
                
                coordinates <- mds_model()$conf
                
                fdh.efficiency <- dea(X = org_dataset[,1:number_of_inputs], Y = org_dataset[,(number_of_inputs+1):ncol(org_dataset)], RTS = "fdh")$eff
                vrs.efficiency <- dea(X = org_dataset[,1:number_of_inputs], Y = org_dataset[,(number_of_inputs+1):ncol(org_dataset)], RTS = "vrs")$eff
                drs.efficiency <- dea(X = org_dataset[,1:number_of_inputs], Y = org_dataset[,(number_of_inputs+1):ncol(org_dataset)], RTS = "drs")$eff
                crs.efficiency <- dea(X = org_dataset[,1:number_of_inputs], Y = org_dataset[,(number_of_inputs+1):ncol(org_dataset)], RTS = "crs")$eff
                irs.efficiency <- dea(X = org_dataset[,1:number_of_inputs], Y = org_dataset[,(number_of_inputs+1):ncol(org_dataset)], RTS = "irs")$eff
                frh.efficiency <- dea(X = org_dataset[,1:number_of_inputs], Y = org_dataset[,(number_of_inputs+1):ncol(org_dataset)], RTS = "add")$eff
                
                
                final_dataset <- data.frame(core_data,coordinates, fdh.efficiency,vrs.efficiency,drs.efficiency,crs.efficiency,irs.efficiency,frh.efficiency)
                all_colnames <- c(colnames(core_data),"D1","D2","fdh.efficiency","vrs.efficiency","drs.efficiency","crs.efficiency","irs.efficiency","frh.efficiency")
                index <- which(all_colnames==input$mds_var_selection)
                
                #print("selected variable to visualize")
                #print(input$mds_var_selection)
                #print("----")
                #print("core dataset colnames")
                #print(colnames(core_data))
                #print("----")
                #print("final dataset colnames")
                #print(colnames(final_dataset))
                #print("----")
                #print("is selected variable in the core dataset?")
                #print(input$mds_var_selection %in%(colnames(core_data)) )
                
                #print("----")
                #print("which column is in the final dataset")
                #print(index)
                #print("----")
                #print("is there anything in final_dataset[,index]")
                #print(head(final_dataset[,index]))
                
                list(index,final_dataset)
                
        })
        
        mds_ranges <- reactiveValues(x = NULL, y = NULL)
        
        # visualize mds model 
        mds_plot_func <- reactive({
                
                final_dataset <- final_dataset_to_visualize()[[2]]
                index <- final_dataset_to_visualize()[[1]]
                
                total_min <-  min(final_dataset$D1,final_dataset$D2)
                total_max <- max( final_dataset$D1,final_dataset$D2)
                
                #x_min <- min(final_dataset$D1,final_dataset$D2)
                #x_max <- max(final_dataset$D1,final_dataset$D2)
                #y_min <- min(final_dataset$D2)
                #y_max <- max(final_dataset$D2)
                #x_range <- x_max - x_min
                #y_range <- y_max - y_min
                
                g<- ggplot(data = final_dataset) + 
                        geom_point(aes(D1,D2, color = final_dataset[,index]),
                                   
                                   size = input$mds_point_size,
                                   alpha = input$mds_point_transparency
                        ) +
                        scale_colour_gradient(low = "blue", high = "red", name = colnames(final_dataset)[index] )
                
                
                
                g<- g+ theme_linedraw()  + annotate("text", x = Inf, y = -Inf, label = "© DEA-Viz",
                                                    hjust=1.1, vjust=-1.1, col="blue", cex=6,
                                                    fontface = "bold", alpha = 0.4) +
                        
                        
                        xlim(total_min, total_max) +
                        ylim(total_min,total_max) +
                        coord_fixed(ratio = 1) + 
                        #xlim(x_min - 0.05 * x_range , x_max + 0.05 * x_range) +
                        #ylim(y_min - 0.05 * y_range , y_max + 0.05 * y_range ) + 
                        ggtitle(paste("MDS Color-Plot of",colnames(final_dataset)[index], sep = " ")) 
                
                g <- g  + coord_cartesian(xlim = mds_ranges$x, ylim = mds_ranges$y, expand = TRUE) 
                
                
                #g
                switch(EXPR = as.character(input$mds_labels), 
                       "TRUE" = (g + geom_text_repel(data = final_dataset, aes(x = D1, y = D2 , label = 1:nrow(final_dataset)), color = "blue") ),
                       "FALSE" = g 
                )
                
        })
        
        
        output$mds_plot <- renderPlot({
                mds_plot_func()
        })
        
        observeEvent(input$mds_dblclick, {
                brush <- input$mds_brush
                if (!is.null(brush)) {
                        mds_ranges$x <- c(brush$xmin, brush$xmax)
                        mds_ranges$y <- c(brush$ymin, brush$ymax)
                        
                } else {
                        mds_ranges$x <- NULL
                        mds_ranges$y <- NULL
                }
        })
        
        output$download_mds <- downloadHandler(
                
                filename = paste0(input$mds_var_selection,"_MDS",".png"), 
                content = function(file) {
                        #png(file)
                        #print(biplot_plot())
                        #dev.off()
                        ggsave(file, plot = mds_plot_func(), device = "png", dpi = 450)
                }
        ) 
        
        output$mds_info <- renderTable({
                #class(colnames(final_dataset_reactive()))
                head(mds_data())
        })
        
        output$mds_brush_info <- DT::renderDataTable({
                data <- final_dataset_to_visualize()[[2]]
                index <- final_dataset_to_visualize()[[1]]
                # it is possible to limit the variables in data by using index
                
                res <- brushedPoints(data, input$mds_brush)
                datatable(res)
                
        })
        #####     
        
        # Dotplot Histograms of Variables 
        
        output$dotplot_dmu_selection_ui <- renderUI({
                dataset <- final_dataset_reactive()
                
                selectInput(inputId = "dotplot_dmu_selection",
                            label = "Highlight a DMU",
                            choices = c("None",1:nrow(dataset))
                )
                
        }) 
        
        output$dotplot_var_selection_ui <- renderUI({
                dataset <- final_dataset_reactive()
                dataset_colnames <- colnames(dataset)
                
                
                #checkboxGroupInput(inputId = "dotplot_checkbox", label = "Select Variable(s)", choices = dataset_colnames, selected = NULL, inline = FALSE, width = NULL)
                #checkboxInput(inputId = 'dotplot_var_1', label = dataset_colnames[1], FALSE)
                checkboxGroupInput(inputId = "dotplot_checkbox",label = "Select Variable(s)",choiceNames = as.list(colnames(dataset)), choiceValues = as.list(seq(1,ncol(dataset))),selected = NULL )
        })
        
        dotplot_dataset <- eventReactive(input$dotplot_button,{
                
                final_dataset_reactive()
                
        })
        
        
        ptlist_func <- reactive({
                
                dataset <- dotplot_dataset()
                selected_vars <- as.numeric(input$dotplot_checkbox)
                
                
                color <- rep("Others",nrow(dataset))
                if (input$dotplot_dmu_selection != "None") {color[as.numeric(input$dotplot_dmu_selection)] <- "Selected"}
                dataset$color <- as.factor(color)
                
                
                #myColors <- brewer.pal(2,"Set1")
                #names(myColors) <- levels(dataset$color)
                
                # for removing legend when there is no selected DMU
                if (input$dotplot_dmu_selection == "None" ) {
                        all_plots <- lapply(X = 1:ncol(dataset), function(x) ggplot()+
                                                    geom_dotplot(data = dataset, aes(x = dataset[,x],fill = color) , alpha = 0.6)+ 
                                                    theme_linedraw()+
                                                    xlab(colnames(dataset)[x]) +  
                                                    scale_fill_manual(name = "DMUs", values =  c("Others"="blue" , "Selected"="orange")) +
                                                    guides(fill = FALSE )
                                            
                        )
                        
                } else {
                        all_plots <- lapply(X = 1:ncol(dataset), function(x) ggplot()+
                                                    geom_dotplot(data = dataset, aes(x = dataset[,x],fill = color) , alpha = 0.6)+ 
                                                    theme_linedraw()+
                                                    xlab(colnames(dataset)[x]) +  
                                                    scale_fill_manual(name = "DMUs", values =  c("Others"="blue" , "Selected"="orange")) 
                                            
                        )
                        
                }
                
                
                
                # or maybe for loop works with print() around the ggplot command
                # or using a middle variable g, then taking out the ggplot part and put it in the list 
                
                
                #select only the plots of chosen variables
                to_select <- c(1:ncol(dataset)) %in% selected_vars
                ptlist <- all_plots[to_select] 
                
                if (length(ptlist)==0) return(NULL)
                
                ptlist
                #grid.arrange(grobs=ptlist,ncol=2)
                
        })
        
        dotplot_plot_func <- reactive({
                ptlist <- ptlist_func()
                grid.arrange(grobs=ptlist,ncol=2)
        })
        
        dotplot_plot_eventReactive <- eventReactive(input$dotplot_button,{
                dotplot_plot_func()
        })
        
        output$dotplot_plot <-  renderPlot({
                #dotplot_plot_func()
                dotplot_plot_eventReactive()
        })
        
        output$download_dotplot <- downloadHandler(
                
                filename = paste0("Dotplot",".png"), 
                
                content = function(file) {
                        ptlist <- ptlist_func()
                        ggsave(file, arrangeGrob(grobs = ptlist , ncol = 3),device = "png", dpi = 450)
                }
        ) 
        
        
        
        
        
}) # End of Shiny app 