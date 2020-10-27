library(fossil)
library(shiny)
library(ggplot2) # plotting lib
library(gridExtra) # arrange grids
library(mice)  # data imputing
library(corrplot) # correlation matrix plotting/printing
library(pROC) # to measure model performance
library(png) # deals with png file measurements
library(knitr) #
library(xtable) # tabular data formatting 
library(caret) # predictive models
library(dplyr)
library(reshape2)
library(arules)
library(randomForest) # classification algorithm
library(ggthemes) # visualization
library(scales) # visualization
library(smotefamily)
library(caret)
library(Hmisc)
library(e1071)

library(fpc)
library(data.table)
library(cluster)
library(NbClust)

#view/control layer
shinyServer(function(input, output, session) {
    
    session$userData$ccdPam<-NULL
    session$userData$curClusterNum<-3
    
    output$plot1 <- renderPlot({
        ccd_pam = getPam(clN=input$clusterNum)
        return(getClusplot(clN=input$clusterNum, cls=ccd_pam))
    })
    
    output$kmeanPlot <- renderPlot({
        return(getClusplotForKmean(cln=input$clusterNumKmean))
    })
    
    # df <- eventReactive(input$button, {
    #     print("start")
    #     output$detectionResultTable <- DT::renderDataTable(ccdFraudDetection())
    # })
    
    # observeEvent(input$button, {
    #     print(paste("This will only be printed once; all",
    #                 "subsequent button clicks won't do anything"))
    # }, once = FALSE)
    # 
    getAlarm <- eventReactive(input$button, {
        set.seed(floor(runif(1, min=0, max=101)))
        ccdFraudDetection()
    })
    
    output$detectionResultTable <- DT::renderDataTable({
        getAlarm()
    })
    
    output$accResult <- renderTable({
        temp_res_medois=getPam(clN=input$clusterNum)$cluster
        temp_res_medois=as.factor(as.matrix(temp_res_medois))
        temp_test_result<-preprocessingData(creditCardDataSample)
        temp_res_medois<-ifelse(temp_res_medois==1, 0, 1)
        return(measurePrecisionRecall(as.numeric(temp_res_medois), as.numeric(creditCardDataSample$Class)))
    })
    
    output$randIndex <- renderText({
        temp_res_medois=getPam(clN=input$clusterNum)$cluster
        temp_res_medois=as.factor(as.matrix(temp_res_medois))
        temp_test_result<-preprocessingData(creditCardDataSample)
        temp_res_medois<-ifelse(temp_res_medois==1, 0, 1)
        return(getRandIndex(as.numeric(temp_res_medois), as.numeric(creditCardDataSample$Class)))
    })
    
    output$groupResultTable<-renderTable({
        getGroupingTable(getPam(clN=input$clusterNum)$clustering)
    })
    
    getClusplotForKmean<-function(cln=clusterNum){
        kclust <- kmeans(scaled_data, cln) # k = 3
        return(clusplot(scaled_data, kclust$cluster, main='2D representation of the Cluster solution', color=TRUE))
    }
    
    getClusplot<-function(clN = clusterNum, cls=tempCluster){
        set.seed(198)
        cp=clusplot(preprocessingData(creditCardDataSample), cls$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=clN, lines=0)
        return(cp)
    }
    
    # output$detectionResultTable<-DT::renderDataTable({
    #     return(ccdFraudDetection())
    # })

    getPam<-function(clN = clusterNum){
        set.seed(198)
        if(session$userData$curClusterNum==clN && !is.null(session$userData$ccdPam)){
            return(session$userData$ccdPam)
        }
        session$userData$curClusterNum=clN
        sample_ccd = preprocessingData(creditCardDataSample)
        tempCcdPam<-pam(sample_ccd, k=clN, stand=FALSE)
        session$userData$ccdPam<-tempCcdPam
        return(tempCcdPam)
    }
    
    preprocessingData <- function(creditCardDataSample){
        tempData = creditCardDataSample
        tempData$X=NULL
        tempData$Time=NULL
        tempData$Class=NULL
        tempData$Amount=scale(tempData$Amount)
        return(tempData)
    }
    
    measurePrecisionRecall <- function(predict, actual_labels){
        precision <- sum(predict & actual_labels) / sum(predict)
        recall <- sum(predict & actual_labels) / sum(actual_labels)
        fmeasure <- 2 * precision * recall / (precision + recall)
        accuracyResultTable<-data.frame(
            Measures=c("precision", "recall", "fmeasure"),
            Results=c(percetVal(precision*100), percetVal(recall*100), percetVal(fmeasure*100))
        )
        return(accuracyResultTable)
    }
    
    percetVal <- function(n=number){
        newStr=paste(round(n, 2), "%")
        return(newStr)
    }

    getGroupingTable<-function(clustering){
        confusionColName=c("Negative", "Positive")
        
        temp_res<-clustering
        temp_res<-as.factor(as.matrix(temp_res))
        temp_res<-ifelse(temp_res==1, 0, 1)
        tempTable <-as.data.frame.matrix(table(temp_res, creditCardDataSample$Class))
        
        tempResultTable<-cbind(confusionColName, tempTable)
        colnames(tempResultTable)<-c(" ", confusionColName)
        
        return(tempResultTable)
    }
    
    getRandIndex<-function(clusters, class){
        rindex = rand.index(clusters, class)
        print(paste("Rand Index:", round(rindex*100, 2), "%"))
    }
    
    ccdFraudDetection<-function(){
        print("called")
        tempDataSet=data.frame(dataFromWareHouse)
        tempDataSet$V29=as.matrix(tempDataSet$V29)
        splitIdx <- createDataPartition(tempDataSet$Class , p = 0.05, list=FALSE)
        tempDataSet=tempDataSet[splitIdx,]
        userEmailAdr=tempDataSet$UserEmailAddress
        tempDataSet$X=NULL
        tempDataSet$Class=NULL
        tempDataSet$UserEmailAddress=NULL
        testlda = as.data.frame(predict(ldaModel,tempDataSet))[c(4,1)]
        lda_pred = predict(svmModel, testlda[-2])
        finalList=data.frame(cbind(lda_pred, UserEmailAddress=userEmailAdr))
        finalList$lda_pred=ifelse(finalList$lda_pred=="1", "Normal", "Possibly Fraud")
        resultTable=data.frame(cbind(Status=finalList$lda_pred, Contact=finalList$UserEmailAddress))
        return(fraudNotification(resultTable))
    }
    
    fraudNotification<-function(resTb){
        #notify the possible fraud case to user
        resTb$Notification=ifelse(resTb$Status=="Normal", "N/A", "Message Sent")
        return(resTb)
    }
    
})


svmModel <- readRDS("./modelRds/model_2_SVM_after_LDA.rds")
ldaModel <- readRDS("./modelRds/model_1_LDA.rds")
#Later this part could be replaced with data Pipe
dataFromWareHouse<-read.csv(file = "./data/testDataWithUserEmail.csv")
scaled_data<-read.csv(file="./data/scaled_data.csv")
creditCardDataSample<-read.csv("./data/ccd_data.csv", header = TRUE, sep=",")
