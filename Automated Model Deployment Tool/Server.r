library(shiny)

library(DT)

library(boot)

library(glmnet)

library(leaps)



# source("predict-for-best-subset.R")



shinyServer(function(input, output, session) {
  
  
  
  df <-reactive({
    
    if(is.null(input$file1)) return(NULL)
    
    df<-read.csv(input$file1$datapath, header = TRUE, sep = ",", stringsAsFactors = T)
    
    return(df)
    
  })
  
  
  
  output$contents <- renderDataTable({if (is.null(df())) return(NULL)
    
    df()})
  
  
  
  model<-reactive({if(is.null(df())) return(NULL)
    
    
    
    if (input$Choice == 0){
      
      
      
      #Here we create the Best Subset Fit
      
      k=10
      
      set.seed(1)
      
      folds = sample(1:k, nrow(df()), replace = TRUE)
      
      bestCVErrors = matrix(NA, k, ncol(df())-1, dimnames = list(NULL, paste(1 : (ncol(df())-1))))
      
      depNameForm<-as.formula(paste0(colnames(df())[1], "~."))
      
      
      
      for(j in 1:k){
        
        best.fit<-regsubsets(depNameForm, data = df()[folds != j,], nvmax = ncol(df())-1)
        
        for (i in 1:(ncol(df())-1)){
          
          
          
          form <- as.formula(paste0(colnames(df())[1], "~."))
          
          mat <- model.matrix(form, df()[folds == j, ])
          
          coefi <- coef(best.fit,id=i)
          
          xvars <- names(coefi)
          
          pred <- mat[,xvars] %*% coefi
          
          
          
          bestCVErrors[j,i] <- mean((df()[[1]][folds==j] - pred)^2)
          
          
          
        }
        
      }
      
      
      
      mean.cv.errors = apply(bestCVErrors,2,mean)
      
      min.mean.cv.errors = min(mean.cv.errors)
      
      min.variables.cv.errors = which.min(mean.cv.errors)[[1]]
      
      regfit.full <- regsubsets(depNameForm, data = df(), nvmax =  min.variables.cv.errors)
      
      
      
      #Here we create the Least Squares Linear Model
      
      set.seed(1)
      
      
      
      lm.fit <- glm(as.formula(paste0(colnames(df())[1], "~.")), data = df())
      
      
      
      startTimeLm <-Sys.time()
      
      predict(lm.fit, df()[1:2, -1])
      
      endTimeLm <- Sys.time()
      
      timeLm<- endTimeLm - startTimeLm
      
      
      
      lmErrors<-cv.glm(df(), lm.fit, K=10)$delta[1]
      
      
      
      #Here we create the Ridge Fit
      
      x <- model.matrix(as.formula(paste0(colnames(df())[1], "~.")), df())[,-1]
      
      y <- df()[[1]]
      
      
      
      set.seed(1)
      
      cv.out <- cv.glmnet(x, y, alpha = 0)
      
      ridgeError<-min(cv.out$cvm)
      
      
      
      bestlam <- cv.out$lambda.min
      
      ridgeModel <- glmnet(x, y, alpha=0, lambda = bestlam)
      
      
      
      startTimeRidge <- Sys.time()
      
      predict(ridgeModel, type="response", newx = x[1:2,])
      
      endTimeRidge <- Sys.time()
      
      timeRidge <- endTimeRidge - startTimeRidge
      
      
      
      
      
      
      
      #Here we create the Lasso Fit
      
      set.seed(1)
      
      cv.outLasso <- cv.glmnet(x, y, alpha = 1)
      
      lassoError<-min(cv.out$cvm)
      
      bestlamLasso <- cv.outLasso$lambda.min
      
      lassoModel<- glmnet(x, y, alpha=1, lambda = bestlam)
      
      
      
      startTimeLasso <- Sys.time()
      
      predict(lassoModel, type="response", newx =  x[1:2,])
      
      endTimeLasso <- Sys.time()
      
      timeLasso <- endTimeLasso - startTimeLasso
      
      
      
      
      
      #Here we bring it all together,
      
      
      
      return(list(Name = c("Least_Squares_Model", "Best_Subset", "Ridge_Model", "Lasso_Model"), Model = list(lm.fit, regfit.full, ridgeModel, lassoModel), Summary = list(summary(lm.fit), summary(regfit.full), summary(ridgeModel), summary(lassoModel)),Time = c(timeLm, 0.7, timeRidge, timeLasso), Error = c(lmErrors, min.mean.cv.errors, ridgeError, lassoError)))}
    
    else {
      
      
      
      #Here we create the Logistic Model
      
      set.seed(1)
      
      
      
      logModel <- glm(as.formula(paste0(colnames(df())[1], "~.")), family = binomial, data = df())
      
      
      
      startTimeLog <-Sys.time()
      
      predict(logModel, df()[1:2, -1], type = "response")
      
      endTimeLog <- Sys.time()
      
      timeLog<- endTimeLog - startTimeLog
      
      
      
      logErrors<-cv.glm(df(), logModel, K=10)$delta[1]
      
      
      
      return(list(Name = c("Logistic_Model"), Model = list(logModel), Summary = list(summary(logModel)),Time = c(timeLog), Error = c(logErrors)))
      
    }
    
    
    
  })
  
  
  
  
  
  #Here we output the models
  
  output$rModelTable <- renderDataTable({
    
    if (is.null(model())) return(NULL)
    
    df2 <- data.frame(ModelName = model()$Name, CV.Error = model()$Error, Speed=model()$Time)
    
    df2[["Export"]]<-paste0('<div class="btn-group" role="group" aria-label="Basic example"><button type="button" class="btn btn-secondary modify"id=modify_',1:nrow(df2),'>Export Model To RDS</button></div>')
    
    datatable(df2,escape=F, selection = 'single')})
  
  
  
  
  
  observeEvent(input$lastClick, {
    
    if (is.null(model())) return(NULL)
    
    timeStampModel<- gsub("[: -]", "" , Sys.time(), perl=TRUE)
    
    name<-paste0(model()$Name[[input$rModelTable_rows_selected]], "_", timeStampModel)
    
    saveRDS(model()$Model[[input$rModelTable_rows_selected]], paste("C:/Users/edmund.judge/Documents/", name, ".rds", sep = "", collapse = NULL))
    
    showNotification(paste("Model Saved as", name, ".rds"), type = "message")})
  
  
  
  
  
  observeEvent(input$rModelTable_rows_selected,{
    
    if (is.null(model())) return(NULL)
    
    output$preview     <- renderPrint({model()$Summary[[input$rModelTable_rows_selected]]})
    
    output$diagnostic1 <- renderPlot(plot(model()$Model[[input$rModelTable_rows_selected]], 1))
    
    output$diagnostic2 <- renderPlot(plot(model()$Model[[input$rModelTable_rows_selected]], 2))
    
    output$diagnostic3 <- renderPlot(plot(model()$Model[[input$rModelTable_rows_selected]], 3))
    
    output$diagnostic4 <- renderPlot(plot(model()$Model[[input$rModelTable_rows_selected]], 4))
    
  })
  
})

