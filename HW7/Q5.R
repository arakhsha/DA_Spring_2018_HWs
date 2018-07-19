trainIndexes = sample(1:nrow(data), 0.8 * nrow(data), replace = F)
train = data[trainIndexes,]
test = data[-trainIndexes,]
model2 = glm(MannerOfDeath ~ .,
             data = train, 
             family = "binomial")
predict = predict.glm(model2, newdata = test, type = 'response')
y = as.numeric(as.character(test$MannerOfDeath))
yHat = ifelse(predict > 0.5, 1, 0)

P <- sum(y)
paste('P: ', P)

N <- sum(y == 0)
paste('N: ', N)

TP <- sum(yHat == 1 & y == 1)
paste('TP: ', TP)

TN <- sum(yHat == 0 & y == 0)
paste('TN: ', TN)

FP <- sum(yHat == 1 & y == 0)
paste('FP: ', FP)

FN <- sum(yHat == 0 & y == 1)
paste('FN: ', FN)

ACC <- (TP + TN) / (P + N)
paste('ACC: ', ACC)

FPR <- 1 - TN / N
paste('FPR: ', FPR)

TPR <- TP / P
paste('TPR: ', TPR)

library(data.table)
ConfusionMatrixInfo <- function( data, predict, actual, cutoff )
{	
  # extract the column ;
  # relevel making 1 appears on the more commonly seen position in 
  # a two by two confusion matrix	
  predict <- data[[predict]]
  actual  <- relevel( as.factor( data[[actual]] ), "1" )
  
  result <- data.table( actual = actual, predict = predict )
  
  # caculating each pred falls into which category for the confusion matrix
  result[ , type := ifelse( predict >= cutoff & actual == 1, "TP",
                            ifelse( predict >= cutoff & actual == 0, "FP", 
                                    ifelse( predict <  cutoff & actual == 1, "FN", "TN" ) ) ) %>% as.factor() ]
  labels = result %>% 
    group_by(type) %>% 
    summarise(count = n(), actual = first(actual), predict = median(predict))
  # jittering : can spread the points along the x axis 
  plot <- ggplot( result, aes( actual, predict, color = type ) ) + 
    geom_violin( fill = "orange", color = NA ) +
    geom_jitter( shape = 1 ) + 
    geom_hline( yintercept = cutoff, color = "blue", alpha = 0.6 ) + 
    geom_label(data = labels, aes( actual, predict, label = count, color = type)) +
    scale_y_continuous( limits = c( 0, 1 ) ) + 
    scale_color_discrete( breaks = c( "TP", "FN", "FP", "TN" ) ) + # ordering of the legend 
    guides( col = guide_legend( nrow = 2 ) ) + # adjust the legend to have two rows  
    ggtitle( sprintf( "Confusion Matrix with Cutoff at %.2f", cutoff ) )
  
  return( list( data = result, plot = plot ) )
}

plotData = data.frame(manner = y, prediction = predict)
cm_info = ConfusionMatrixInfo( data = plotData, predict = "prediction", 
                              actual = "manner", cutoff = .5 )
cm_info$plot

table(data$MannerOfDeath,ifelse(fitted(model)>0.5,1,0)) %>% 
  plot(main = 'Model Results for 0.5 cutoff', xlab = 'Actual', ylab = 'Prediction')