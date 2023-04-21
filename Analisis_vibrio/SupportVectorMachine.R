#######################################################################
############### SVM 
pa=read.csv("VibrioTotal.csv")
colnames(pa)<-c(  "vibrio",
                  "NO2",
                  "NO3",
                  "PO4",
                  "SiO2",
                  "Clorofila",
                  "Conductividad",
                  "Salinidad",
                  "pH",
                  "OD",
                  "Transparencia",
                  "SST",
                  "NID",
                  "NP",
                  "IE",
                  "IE.PO4",
                  "TSI_Clor",
                  "TSI_SECCHI",
                  "Temperatura_mean",
                  "Salinidad_mean",
                  "Oxigeno_mean",
                  "Densidad_mean",
                  "Profundidad_max" )

library(caret)

#pa=na.omit(pa)

head(pa)

summary(pa)


head(pa)

set.seed(1) #pseudo-repeatability
trainIndex = createDataPartition(pa$vibrio, p = .75, 
                                 list = FALSE, 
                                 times = 1) #y as basis of splitting

training = pa[ trainIndex,] #75% data for model training
testing= pa[-trainIndex,] #25% for model testing

head(training)

training$vibrio=as.factor(training$vibrio) #1 stands for presence and 0 for absence




## caret
# define training control--> 10fold cv
train_control = trainControl(method="cv", number=10)

#svm with rbf kernel
mod_fit1=train(vibrio~.,
               data=training,trControl=train_control,method="svmPoly")

summary(mod_fit1)

### for polynomial kernel specify method="svmPoly"

## importance of the different predictors
varImp(mod_fit1)

## test the model
p1=as.numeric(predict(mod_fit1, newdata=testing)) #predict on the test data

#test model fit-auc
library(pROC)

testing$vibrio=as.factor(testing$vibrio)
roc = pROC::roc(testing[,"vibrio"], p1) #compare testing data
#with predicted responses

auc= pROC::auc(roc)
auc

plot(roc)
text(0.5,0.5,paste("AUC = ",format(auc, digits=5, scientific=FALSE)))

