library(shiny)
library(MASS)
library(readr)
library(lpSolve)
library(Benchmarking)
library(smacof)
library(devtools)
library(ggplot2)
library(kohonen)


# 17 August 10am ---> rectifying xlim ylim. Now all the points are seen clearly

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

simple_efficiency = function(input_mat,output_mat,epsilon_value = 0.00001){
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



CEM_unit_agg =  function(input_mat , output_mat , unit , epsilon = 0.00001){
        #this function must return the the aggresive optimum weights for the given unit 
        
        #requires Benchmarking library
        #requires lpSolve library
        
        
        number_of_units = nrow(input_mat)
        input_mat = as.matrix(input_mat)
        output_mat = as.matrix(output_mat)
        number_of_inputs = ncol(input_mat)
        number_of_outputs = ncol(output_mat)
        
        #we need the simple efficiency of the given unit 
        
        eff_weight_mat = dea_4cem(input_mat = input_mat, output_mat = output_mat )
        simple_eff = eff_weight_mat[,1]
        unit_simple_eff = simple_eff[unit]
        
        output_mat_refined = output_mat[-unit,]
        OF = apply(output_mat_refined,2,sum)
        OF = c(OF,rep(0,number_of_inputs))
        
        d = number_of_inputs + number_of_outputs
        
        #preparing the A matrix
        A_middle = diag(d)
        A_upper=A_mat(unit=unit, input_mat=input_mat, output_mat=output_mat)
        A_last = c(output_mat[unit,],-1*unit_simple_eff*input_mat[unit,])
        A = rbind(A_upper,A_middle,A_last)
        #two new lines
        input_mat_refined = input_mat[-unit,]
        A[1,] = c(rep(0,number_of_outputs),apply(input_mat_refined,2,sum))
        
        #preparing the constraint directions
        C_upper = c("==",rep("<=",number_of_units-1))
        C_middle = rep(">=",d)
        C_last = "=="
        C = c(C_upper,C_middle,C_last)
        
        #preparing the RHS values
        rhs_upper = c(1,rep(0,number_of_units-1))
        rhs_middle = rep(epsilon,d)
        rhs_last = 0
        rhs_total = c(rhs_upper, rhs_middle,rhs_last)
        ## changed rhs name to rhs_total
        #print(cbind(A,C,rhs))
        
        #changed rhs name to rhs_total
        t = lp(direction = "min", objective.in = OF , const.mat = A , const.dir = C , const.rhs = rhs_total)
        #CEM_weights_of_unit = t$solution
        #just made the above line inactive, why not returning the solutions directly? 
        return(t$solution)
}

CEM_agg = function(input_mat, output_mat) {
        #this function returns the benevolent CEM 
        
        number_of_units = nrow(input_mat)
        number_of_inputs = ncol(input_mat)
        number_of_outputs = ncol(output_mat)
        d = number_of_inputs+number_of_outputs
        
        CEM_opt_weights = matrix(nrow = number_of_units, ncol = d )
        
        for (unit in 1:number_of_units) {
                CEM_opt_weights[unit,] = CEM_unit_agg(input_mat , output_mat , unit , epsilon = 0)
        }
        #print(CEM_opt_weights)
        CEM = matrix (nrow = number_of_units, ncol = number_of_units)
        
        
        
        for (row in 1:number_of_units){
                if ( sum(CEM_opt_weights[row,])==0 ) { 
                        temporary =dea(X = input_mat, Y = output_mat , RTS = "crs" , DUAL = TRUE)
                        
                        CEM_opt_weights[row,] = c(temporary$vy[row,] , temporary$ux[row,])
                }
        }
        
        
        for (row in 1:number_of_units){
                U = CEM_opt_weights[row,1:number_of_outputs]
                V = CEM_opt_weights[row,(number_of_outputs+1):d]
                #print(U)
                #print(V)
                #print("----")
                
                for (col in 1:number_of_units){
                        CEM[row,col] = sum(U*output_mat[col,])/sum(V*input_mat[col,])
                }
                
        }
        
        return(CEM)
        
}

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
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        library(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}


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
        A_mat = function(unit, dataset) {
                #this function returns A matrix - matrix of constraints - for DMUunit formulation. 
                # unit can be a number from 1 to nrow(input_mat)
                # This function can be developed in a way that it returns all A matrices for
                # all units in a data structure such as list. It would be faster but 
                # pre-mature optimization is the root of all devils. 
                
                number_of_units = nrow(dataset)
                number_of_inputs = input$num_of_inputs
                number_of_outputs = ncol(dataset)- input$num_of_inputs
                number_of_variables = number_of_inputs+number_of_outputs
                #input_mat <- dataset[,1:number_of_inputs]
                #output_mat <- dataset[,(number_of_inputs+1):number_of_variables]
                
                #input_mat = as.matrix(input_mat)
                #output_mat = as.matrix(output_mat)
                
                #number_of_inputs = ncol(input_mat)
                #number_of_outputs = ncol(output_mat)
                #number_of_units = nrow(input_mat)
                
                #assuming that the nrow() of input_mat and output_mat are equal 
                
                A = matrix(nrow = number_of_units, ncol= number_of_variables)
                
                #print("-----")
                #print("A matrix before being filled")
                #print(A)
                #print("-----")
                
                #first constraint is always the numerator constraint
                A[1,]= unlist(c(rep(0,number_of_outputs),dataset[unit,1:number_of_inputs]))
                ##print(dim(A))
                ##input_mat = input_mat[-unit, ]
                ##output_mat = output_mat[-unit, ]
                
                input_mat_without_unit <- as.matrix(dataset[-unit,1:number_of_inputs])
                output_mat_without_unit <- as.matrix(dataset[-unit,(number_of_inputs+1):number_of_variables])
                
                #print("-----")
                #print("input_mat_without_unit")
                #print(dim(input_mat_without_unit))
                #print("output_mat_without_unit")
                #print(dim(output_mat_without_unit))
                #print("classes")
                #print(class(input_mat_without_unit))
                #print(class(output_mat_without_unit))
                #print("what goes to be the first line of A")
                #print(unlist(c(rep(0,number_of_outputs),dataset[unit,1:number_of_inputs])))
                #print("First line of A")
                #print(A[1,])
                #print("lengh of the first line")
                #print(length(A[1,]))
                #print("-----")
                
                d = number_of_units-1
                
                ##print(d)
                for (i in 1:d) { 
                        ##print(i)
                        A[(i+1), ]=c(output_mat_without_unit[i,],-1*input_mat_without_unit[i,])
                        #----
                        #print("-----")
                        #print("Inside of A matrix loop")
                        #print("i")
                        #print(i)
                        #print("-----")
                        
                        #A[(i+1), ]=c(dataset[i,((number_of_inputs+1):number_of_variables)],-1*dataset[i,(1:number_of_inputs)])
                }
                
                A
                
        } #--- End of A_mat()
        
        CEM_unit = function(dataset , unit , epsilon = 0.00001){
                #this function must return the the benevolent optimum weights for the given unit 
                
                #requires Benchmarking library
                #requires lpSolve library
                
                #require(Benchmarking)
                #require(lpSolve)
                
                number_of_units = nrow(dataset)
                number_of_inputs = input$num_of_inputs
                number_of_outputs = ncol(dataset)- input$num_of_inputs
                number_of_variables = number_of_inputs+number_of_outputs
                
                #print("-----")
                #print("debug inside CEM_unit()")
                #print("-----")
                #print("number_of_units")
                #print(number_of_units)
                #print("-----")
                #print("number_of_inputs")
                #print(number_of_inputs)
                #print("-----")
                #print("number_of_outputs")
                #print(number_of_outputs)
                #print("-----")
                #print("number_of_variables")
                #print(number_of_variables)
                #print("-----")
                #print("dataset head")
                #print(head(dataset))
                #print("-----")
                
                
                #input_mat <- dataset[,1:number_of_inputs]
                #output_mat <- dataset[,(number_of_inputs+1):number_of_variables]
                
                #number_of_units = nrow(input_mat)
                #input_mat = as.matrix(input_mat)
                #output_mat = as.matrix(output_mat)
                #number_of_inputs = ncol(input_mat)
                #number_of_outputs = ncol(output_mat)
                
                
                #we need the simple efficiency of the given unit 
                #require(Benchmarking)
                #library(Benchmarking)
                
                # 7 October - In order to test the new function dea_4cem instead of dea() 
                #simple_eff = dea(X = input_mat , Y = output_mat , RTS = "crs", ORIENTATION = "in")
                #unit_simple_eff = simple_eff$eff[unit]
                
                #----needs debugging dea_4cem()
                eff_weight_mat = dea_4cem(dataset = dataset)
                
                #print("-----")
                #print("eff_weight_mat dim")
                #print(dim(eff_weight_mat))
                #print("-----")
                
                simple_eff = eff_weight_mat[,1]
                unit_simple_eff = simple_eff[unit]
                
                #print("-----")
                #print("unit_simple_eff")
                #print(unit_simple_eff)
                #print("-----")
                
                output_mat_refined<- as.matrix(dataset[-unit,(number_of_inputs+1):number_of_variables])
                #output_mat_refined = output_mat[-unit,]
                
                #print("-----")
                #print("output_mat_refined dim")
                #print(dim(output_mat_refined))
                #print("-----")
                #print("head of output_mat_refined")
                #print(head(output_mat_refined))
                #print("-----")
                #print("modified dataset dim")
                #print(dim(dataset[-unit,(number_of_inputs+1):number_of_variables]))
                #print("-----")
                #print("head of modified dataset")
                #print(head(dataset[-unit,(number_of_inputs+1):number_of_variables]))
                #print("-----")
                
                OF = apply(output_mat_refined,2,sum)
                OF = c(OF,rep(0,number_of_inputs))
                
                #--- debug
                #print("output_mat_refined")
                #print(output_mat_refined)
                #print("-----")
                #print("output_mat_refined type")
                #print(class(output_mat_refined))
                #print("-----")
                #print("OF")
                #print(OF)
                #print("-----")
                #print("OF class")
                #print(class(OF))
                #print("-----")
                #print("length of OF")
                #print(length(OF))
                #print("-----")
                
                
                #d = number_of_inputs + number_of_outputs
                
                #preparing the A matrix
                A_middle = diag(number_of_variables)
                
                #print("-----")
                #print("A_middle dim")
                #print(dim(A_middle))
                #print("-----")
                
                #---- needs debugging A_mat()
                A_upper=A_mat(unit=unit, dataset=dataset)
                
                #----- debug
                
                #print("-----")
                #print("A_upper dim: Important for A_mat()")
                #print(A_upper)
                #print("-----")
                #print("suspicious case of c() to fill A_last")
                #print(class(c(dataset[unit,(number_of_inputs+1):number_of_variables],-1*unit_simple_eff*dataset[unit,1:number_of_inputs])))
                #print("-----")
                
                A_last = unlist(c(dataset[unit,(number_of_inputs+1):number_of_variables],-1*unit_simple_eff*dataset[unit,1:number_of_inputs]))
                
                
                A = rbind(A_upper,A_middle,A_last)
                
                #print("-----")
                #print("A_last")
                #print(A_last)
                #print("dim of final A")
                #print(dim(A))
                #print("-----")
                
                #two new lines
                ##input_mat_refined = matrix(dataset[-unit,1:number_of_inputs])
                ##A[1,] = c(rep(0,number_of_outputs),apply(input_mat_refined,2,sum))
                
                #preparing the constraint directions
                C_upper = c("==",rep("<=",number_of_units-1))
                C_middle = rep(">=",number_of_variables)
                C_last = "=="
                C = c(C_upper,C_middle,C_last)
                
                #preparing the RHS values
                rhs_upper = c(1,rep(0,number_of_units-1))
                rhs_middle = rep(epsilon,number_of_variables)
                rhs_last = 0
                rhs_total = c(rhs_upper, rhs_middle,rhs_last)
                ## changed rhs name to rhs_total
                ##print(cbind(A,C,rhs))
                
                #changed rhs name to rhs_total
                t = lp(direction = "max", objective.in = OF , const.mat = A , const.dir = C , const.rhs = rhs_total)
                #CEM_weights_of_unit = t$solution
                #just made the above line inactive, why not returning the solutions directly? 
                t$solution
        }
        
        CEM = function(dataset) {
                #this function returns the benevolent CEM 
                
                #require(lpSolve)
                #input_mat <- dataset[,1:number_of_inputs]
                #output_mat <- dataset[,(number_of_inputs+1):number_of_variables]
                number_of_units = nrow(dataset)
                number_of_inputs = input$num_of_inputs
                number_of_outputs = ncol(dataset)- input$num_of_inputs
                
                
                number_of_variables = number_of_inputs+number_of_outputs
                
                
                
                CEM_opt_weights = matrix(nrow = number_of_units, ncol = number_of_variables )
                
                #print("-----")
                #print("number_of_units")
                #print(number_of_units)
                #print("-----")
                #print("number of inputs")
                #print(number_of_inputs)
                #print("-----")
                #print("number of outputs")
                #print(number_of_outputs)
                #print("-----")
                #print("number of variables")
                #print(number_of_variables)
                #print("-----")
                #print("CEM_opt_weights dim")
                #print(dim(CEM_opt_weights))
                #print("-----")
                
                #---attention needed for CEM_unit
                for (unit in 1:number_of_units) {
                        #print("unit in the CEM_opt_weights loop")
                        #print(unit)
                        CEM_opt_weights[unit,] = CEM_unit(dataset , unit , epsilon = 0)
                }
                
                #-----
                #print("-----")
                #print("CEM_opt_weights")
                #print(CEM_opt_weights)
                #print("-----")
                #-----
                
                ##print(CEM_opt_weights)
                CEM = matrix (nrow = number_of_units, ncol = number_of_units)
                
                #In the 35 Chinesse Cities dataset, CEM_unit() could not find feasible solution for 
                #the unit 4. In other words, the benevolent formulation of the unit4 was not feasible!
                #why? I don't know now (3-oct-2016). As the result, the dea() function returns infeasible 
                #solution with ZERO as all weights. So here if I detect ALL ZERO, I replace it with the optimum weights 
                #that dea() function returns for the problematic unit. These weights - which may not be very benevolent
                #will be used in generation of cross-efficiency matrix. 
                
                #----dea_4cem needs debugging 
                eff_weight_mat = dea_4cem(dataset) 
                for (row in 1:number_of_units){
                        if ( sum(CEM_opt_weights[row,])==0 ) { 
                                
                                #temporary =dea(X = input_mat, Y = output_mat , RTS = "crs" , DUAL = TRUE, ORIENTATION = "in")
                                # Orientation has been added on 7 october, trying to resolve the bug of colombian hospitals
                                #CEM_opt_weights[row,] = c(temporary$vy[row,] , temporary$ux[row,])
                                CEM_opt_weights[row,] = eff_weight_mat[row,-1]
                        }
                }
                
                
                for (row in 1:number_of_units){
                        w_outputs = CEM_opt_weights[row,1:number_of_outputs]
                        w_inputs = CEM_opt_weights[row,(number_of_outputs+1):number_of_variables]
                        ##print(U)
                        ##print(V)
                        ##print("----")
                        
                        ## This can be heavily optimized with matrix operations rather than scalar
                        CEM[row,] = apply(X = t(t(dataset[,(number_of_inputs+1):number_of_variables]) * CEM_opt_weights[row,1:number_of_outputs]), MARGIN = 1 , FUN = sum ) / apply(X = t(t(dataset[,1:number_of_inputs]) * CEM_opt_weights[row,(number_of_outputs+1):number_of_variables]), MARGIN = 1 , FUN = sum )
                        
                        #for (col in 1:number_of_units){
                        #        CEM[row,col] = sum(w_outputs*output_mat[col,])/sum(w_inputs*input_mat[col,])
                        #        #CEM[row,col] = sum(round(U*output_mat[col,],4))/sum(round(V*input_mat[col,],4))
                        #}
                        
                }
                
                CEM
                
        }
        
        dea_4cem = function(dataset) {
                # this function is supposed to replace dea() of Benchmarking
                # This function supposed to give back the optimum [ or a optimum set of] weights
                
                # since dea() is giving bulshit results for Colombian hospital case
                # indeed, the summation of the VIs is not equal to one! 
                # so it means the formulation that dea() is using is not the one that I need
                
                #input_mat <- dataset[,1:number_of_inputs]
                #output_mat <- dataset[,(number_of_inputs+1):number_of_variables]
                number_of_units = nrow(dataset)
                number_of_inputs = input$num_of_inputs
                number_of_outputs = ncol(dataset)- input$num_of_inputs
                number_of_variables = number_of_inputs+number_of_outputs
                
                #number_of_inputs = ncol(input_mat)
                #number_of_outputs = ncol(output_mat)
                #assuming that input_mat and output_mat have the same number of rows!
                #number_of_units = nrow(input_mat)
                #d = number_of_inputs + number_of_outputs
                
                eff_weight_mat = matrix(nrow = number_of_units, ncol = (number_of_variables+1))
                
                #A_epsilon=(diag(d))
                
                for (unit in 1:number_of_units){
                        
                        OF = unlist(c(as.vector(dataset[unit,(number_of_inputs+1):number_of_variables]),rep(0,number_of_inputs)))
                        
                        A_upper = unlist(c(rep(0,number_of_outputs),dataset[unit,1:number_of_inputs]))
                        A_middle = cbind(dataset[-unit,(number_of_inputs+1):number_of_variables],-dataset[-unit,1:number_of_inputs])
                        A_bottom = OF 
                        A_upper = unname(A_upper)
                        A_middle = unname(A_middle)
                        A_bottom = unname(A_bottom)
                        #A_epsilon= unname(A_epsilon)
                        #colnames(A_epsilon) = colnames(A_upper)
                        A = as.matrix(rbind(A_upper, A_middle, A_bottom))
                        #A = rbind(A, A_epsilon)
                        
                        #const_directions = c("==",rep("<=",(number_of_units)),rep(">=",d))
                        const_directions = c("==",rep("<=",(number_of_units)))
                        # epsilon = 0.0000001
                        #RHS_values = c(1,rep(0,(number_of_units-1)),1,rep(0.0000001,d))
                        RHS_values = c(1,rep(0,(number_of_units-1)),1)
                        
                        lp_model=lp(direction = "max", objective.in = OF , const.mat = A , const.dir = const_directions, const.rhs = RHS_values)
                        
                        eff_weight_mat[unit,] = c(lp_model$objval,lp_model$solution)
                        
                        
                        
                }
                
                eff_weight_mat
                
                
                
        }
        
        #----- End of CEM requisit functions 
        
        #generating benevolent CEM by pressing the 'cem_mdu_button' 
        cem_reactive <- eventReactive(input$cem_mdu_button, {
                
                number_of_inputs <- as.numeric(input$num_of_inputs)
                t<- final_dataset_reactive()
                
                switch(EXPR = input$cem_approach ,
                       "Benevolent" = CEM(dataset = t) , 
                       #"Aggressive" = CEM_agg(input_mat = as.matrix(t[,1:number_of_inputs]) ,output_mat = as.matrix(t[,(number_of_inputs+1):ncol(t)] )) )
                       "Aggressive" = NULL )
                #CEM(input_mat = as.matrix(t[,1:number_of_inputs]) ,output_mat = as.matrix(t[,(number_of_inputs+1):ncol(t)] )) 
                
                
        })
        
        
        
        #CEM MDU plot
        cem_unfolding <- reactive({
                t <- cem_reactive()
                
                unfolding(delta = round((1-t),2),ndim = 2)
        })
        
        
        cem_ranges <- reactiveValues(x = NULL, y = NULL)
        
        cem_unfolding_plot <- reactive({
                
                
                t <- cem_unfolding()
                row_df <- data.frame(t$conf.row, "DMU" = c(1:nrow(t$conf.row))) 
                col_df <- data.frame(t$conf.col, "DMU" = c(1:nrow(t$conf.col))) 
                
                x_min <- min(min(col_df$D1),min(row_df$D1))
                x_max <- max(max(col_df$D1),max(row_df$D1))
                y_min <- min(min(col_df$D2),min(row_df$D2))
                y_max <- max(max(col_df$D2),max(row_df$D2))
                x_range <- x_max - x_min
                y_range <- y_max - y_min
                
                g <- ggplot() + geom_point(data = row_df, aes(x = D1 , y = D2, text = paste("DMU:",DMU)), color = "blue", size = input$cem_row_point_size, shape = 19, alpha = input$cem_row_transparency) + 
                        geom_point(data = col_df, aes(x = D1 , y = D2, text = paste("DMU:",DMU)), color = "red",size = input$cem_col_point_size, shape = 24, alpha = input$cem_col_transparency) +
                        coord_fixed(ratio = 1,  expand = TRUE)
                
                g <- g  + 
                        coord_cartesian(xlim = cem_ranges$x, ylim = cem_ranges$y, expand = FALSE)
                g <- g  + theme_linedraw() + annotate("text", x = Inf, y = -Inf, label = "© DEA-Viz",
                                                      hjust=1.1, vjust=-1.1, col="blue", cex=6,
                                                      fontface = "bold", alpha = 0.8) + 
                        xlim(x_min - 0.05 * x_range , x_max + 0.05 * x_range) +
                        ylim(y_min - 0.05 * y_range , y_max + 0.05 * y_range ) 
                #g
                switch(EXPR = as.character(input$cem_unfolding_labels), 
                       "TRUE" = (g + geom_text(data = row_df, aes(x = D1, y = D2 , label = DMU), color = "orange") ),
                       "FALSE" = g 
                )
                
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
                Porembsky_df$Shape = as.integer(ifelse(Porembsky_df$Efficiency==1,1,19))
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
                
                x_min <- min(points_df$X1)
                x_max <- max(points_df$X1)
                y_min <- min(points_df$X2)
                y_max <- max(points_df$X2)
                x_range <- x_max - x_min
                y_range <- y_max - y_min
                
                #g = ggplot() + geom_point(data = points_df , aes(x = X1 , y = X2))
                g = ggplot() + geom_point(data = points_df , aes(x = X1 , y = X2),shape =points_df$Shape , size = input$Porembski_point_size, alpha = input$Porembski_point_transparency) 
                
                
                for (index in 1:nrow(edges_df)) {
                        if (edges_df[index,"alpha"] > input$Porembski_edge_treshold) {
                                g = g  + geom_segment(data = edges_df[index,], aes(x = x1 , y = y1 , xend = x2 , yend = y2),alpha = round(edges_df[index,"alpha"],2)/input$Porembski_edge_transparency , color = "red" )
                        }
                }
                g = g + coord_fixed(ratio = 1,  expand = TRUE)
                g <- g  + 
                        coord_cartesian(xlim = Porembski_ranges$x, ylim = Porembski_ranges$y, expand = FALSE)
                g<- g+ theme_linedraw()  + annotate("text", x = Inf, y = -Inf, label = "© DEA-Viz",
                                                    hjust=1.1, vjust=-1.1, col="blue", cex=6,
                                                    fontface = "bold", alpha = 0.6) + 
                        xlim(x_min - 0.05 * x_range , x_max + 0.05 * x_range) +
                        ylim(y_min - 0.05 * y_range , y_max + 0.05 * y_range ) 
                
                #g
                switch(EXPR = as.character(input$Porembski_labels), 
                       "TRUE" = (g + geom_text(data = points_df, aes(x = X1, y = X2 , label = DMU), color = "blue") ),
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
        
        ranges <- reactiveValues(x = NULL, y = NULL)
        
        #output$plot1 <- renderPlot({
        #        ggplot(mtcars, aes(wt, mpg)) +
        #                geom_point() +
        #                coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
        #})
        
        
        biplot_plot <- reactive({
                x <- biplot_data()
                supp <- biplot_supplement_data()
                data <- final_dataset_reactive()
                data$shape <- switch(EXPR = input$biplot_dea_model, 
                                     "CRS" = ifelse(test = supp$crs_efficiency ==1 , 1, 19),
                                     "VRS" = ifelse(test = supp$vrs_efficiency ==1 , 1, 19) )
                
                
                
                
                z1 <- data.frame(DMU = 1:nrow(data), x$x[, 1:2], crs_shape = NA , vrs_shape = NA)
                z1$crs_shape <- ifelse(test = supp$crs_efficiency ==1 , 1, 19)
                #print("crs shapes of biplot")
                #print(z1$crs_shape)
                z1$vrs_shape <- ifelse(test = supp$vrs_efficiency ==1 , 1, 19)
                #print("vrs shapes of biplot")
                #print(z1$vrs_shape)
                #z1$shape <- switch(EXPR = input$biplot_dea_model, 
                #                      "CRS" = ifelse(test = supp$crs_efficiency ==1 , 1, 19),
                #                      "VRS" = ifelse(test = supp$vrs_efficiency ==1 , 1, 19) )
                
                z2 <- data.frame(Variables = (rownames(x$rotation)), x$rotation[, 1:2])
                
                #print("z1 of the biplot")
                #print(head(z1))
                
                #x_range <- max(z1$PC1) - min(z1$PC1)
                #y_range <- max(z1$PC2) - min(z1$PC2)
                x_min <- min(z1$PC1)
                x_max <- max(z1$PC1)
                y_min <- min(z1$PC2)
                y_max <- max(z1$PC2)
                x_range <- x_max - x_min
                y_range <- y_max - y_min
                
                
                g <- switch(EXPR = input$biplot_dea_model,
                            "CRS" = ggplot(z1, aes(x = PC1,y = PC2)) + 
                                    geom_point( size=input$biplot_point_size, alpha = input$biplot_point_transparency ,shape = z1$crs_shape) +
                                    geom_segment(data=z2, aes(PC1*input$biplot_vector_size, PC2*input$biplot_vector_size, xend=0, yend=0), col="red",alpha = input$biplot_vector_transparency ) +
                                    geom_text(data=z2 , aes(x = PC1*input$biplot_vector_size,y =  PC2*input$biplot_vector_size, label = Variables ), col="red", alpha = input$biplot_vector_transparency, size = input$biplot_vector_text_size )  , 
                            
                            "VRS" =  ggplot(z1, aes(x = PC1,y = PC2)) + 
                                    geom_point( size=input$biplot_point_size, alpha = input$biplot_point_transparency, shape = z1$vrs_shape) +
                                    geom_segment(data=z2, aes(PC1*input$biplot_vector_size, PC2*input$biplot_vector_size, xend=0, yend=0), col="red",alpha = input$biplot_vector_transparency ) +
                                    geom_text(data=z2 , aes(x = PC1*input$biplot_vector_size,y =  PC2*input$biplot_vector_size, label = Variables ), col="red", alpha = input$biplot_vector_transparency, size = input$biplot_vector_text_size ) 
                            
                )
                
                #g<- ggplot(z1, aes(x = PC1,y = PC2), shape = z1$shape) + 
                #        geom_point( size=input$biplot_point_size, alpha = input$biplot_point_transparency) +
                #        geom_segment(data=z2, aes(PC1*input$biplot_vector_size, PC2*input$biplot_vector_size, xend=0, yend=0), col="red",alpha = input$biplot_vector_transparency ) +
                #        geom_text(data=z2 , aes(x = PC1*input$biplot_vector_size,y =  PC2*input$biplot_vector_size, label = Variables ), col="red", alpha = input$biplot_vector_transparency, size = input$biplot_vector_text_size ) 
                
                g <- g  + 
                        coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
                
                g<- g+ theme_linedraw()  + annotate("text", x = Inf, y = -Inf, label = "© DEA-Viz",
                                                    hjust=1.1, vjust=-1.1, col="blue", cex=6,
                                                    fontface = "bold", alpha = 0.4) +
                        xlim(x_min - 0.05 * x_range , x_max + 0.05 * x_range) +
                        ylim(y_min - 0.05 * y_range , y_max + 0.05 * y_range ) 
                
                switch(EXPR = as.character(input$biplot_labels), 
                       "TRUE" = (g + geom_text(data = z1, aes(x = PC1, y = PC2 , label = DMU), color = "blue") ),
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
                        ranges$x <- c(brush$xmin, brush$xmax)
                        ranges$y <- c(brush$ymin, brush$ymax)
                        
                } else {
                        ranges$x <- NULL
                        ranges$y <- NULL
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
                        Costa_df = data.frame(DMU = c(1:nrow(data)), I =wsum_inputs, O = wsum_outputs, efficiency = ifelse(eff_crs_in$eff==1,"Efficient","Inefficient") )
                        
                        
                }
                else if (model == "CRS-Out") {
                        
                        eff_crs_out <- crs_eff(dataset = data, num_of_inputs = number_of_inputs , orientation = "out") 
                        
                        S_crs_outorient <- apply(X = eff_crs_out$vy,MARGIN = 1 , FUN = sum)
                        
                        modified_vi_outorient <- eff_crs_out$ux / S_crs_outorient
                        modified_vo_outorient <- eff_crs_out$vy / S_crs_outorient
                        
                        wsum_inputs = round(apply(X = modified_vi_outorient * data[,1:number_of_inputs], MARGIN = 1 , FUN = sum),3)
                        wsum_outputs = round(apply(X = modified_vo_outorient * data[,(number_of_inputs+1):ncol(data)], MARGIN = 1 , FUN = sum),3)
                        
                        Costa_df = data.frame(DMU = c(1:nrow(data)), I =wsum_inputs, O = wsum_outputs, efficiency = ifelse(eff_crs_out$eff==1,"Efficient","Inefficient") )
                        
                }
                
                #label = rep(NA,47)
                #label[which(Costa_df$I==Costa_df$O)] = which(Costa_df$I==Costa_df$O)
                
        })
        
        Costa_ranges <- reactiveValues(x = NULL, y = NULL)
        
        Costa_plot_func <- reactive({
                
                t <- Costa_df()
                
                x_min <- min(t$I)
                x_max <- max(t$I)
                y_min <- min(t$O)
                y_max <- max(t$O)
                x_range <- x_max - x_min
                y_range <- y_max - y_min
                
                g = ggplot() +
                        geom_point(data = t, aes(x = I , y = O, color = factor(t$efficiency)), 
                                   size = input$Costa_point_size , 
                                   alpha = input$Costa_point_transparency) + 
                        scale_colour_manual(name='DMU', values = c("Efficient"="gold","Inefficient"="blue"))
                
                g = g + geom_abline(xintercept = 0 , yintercept = 0 , slope = 1, color = "red")
                g = g + coord_fixed(ratio = 1,  expand = TRUE)
                
                g<- g + coord_cartesian(xlim = Costa_ranges$x, ylim = Costa_ranges$y, expand = FALSE)
                
                g<- g+ theme_linedraw()  + annotate("text", x = Inf, y = -Inf, label = "© DEA-Viz",
                                                    hjust=1.1, vjust=-1.1, col="blue", cex=6,
                                                    fontface = "bold", alpha = 0.5) +
                        xlim(x_min - 0.05 * x_range , x_max + 0.05 * x_range) +
                        ylim(y_min - 0.05 * y_range , y_max + 0.05 * y_range ) 
                #g
                switch(EXPR = as.character(input$Costa_labels), 
                       "TRUE" = (g + geom_text(data = t, aes(x = I, y = O , label = 1:nrow(t)), color = "blue") ),
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
                
                x_min <- min(final_dataset$D1)
                x_max <- max(final_dataset$D1)
                y_min <- min(final_dataset$D2)
                y_max <- max(final_dataset$D2)
                x_range <- x_max - x_min
                y_range <- y_max - y_min
                
                g<- ggplot(data = final_dataset) + 
                        geom_point(aes(D1,D2, color = final_dataset[,index]),
                                   
                                   size = input$mds_point_size,
                                   alpha = input$mds_point_transparency
                        ) +
                        scale_colour_gradient(low = "blue", high = "red")
                
                g <- g  + coord_cartesian(xlim = mds_ranges$x, ylim = mds_ranges$y, expand = FALSE)
                
                g<- g+ theme_linedraw()  + annotate("text", x = Inf, y = -Inf, label = "© DEA-Viz",
                                                    hjust=1.1, vjust=-1.1, col="blue", cex=6,
                                                    fontface = "bold", alpha = 0.5) +
                        xlim(x_min - 0.05 * x_range , x_max + 0.05 * x_range) +
                        ylim(y_min - 0.05 * y_range , y_max + 0.05 * y_range ) 
                
                #g
                switch(EXPR = as.character(input$mds_labels), 
                       "TRUE" = (g + geom_text(data = final_dataset, aes(x = D1, y = D2 , label = 1:nrow(final_dataset)), color = "blue") ),
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
        #####     
        
}) # End of Shiny app 