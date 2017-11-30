library(readr)
library(dplyr)
library(SparseM)
library(mice)
library(mlbench)
library(e1071)
library(Amelia)
library(corrplot)
library(caret)
library(rpart)
library(rpart.plot)
library(VIM)
library(randomForest)
library(psych)
library(class)
library(ROSE)
library(DMwR)
library(caretEnsemble)
library(kernlab)
library(Boruta)
library(doParallel)

travistorrent_8_2_2017 <- read_csv("C:/Users/nessk/Desktop/TravisTorrent/travistorrent_8_2_2017.csv")

# Eliminando variáveis com identificadores(Algumas, estou deixando outras para analise exploratória), variáveis com datas e etc

travis_selecionado <- travistorrent_8_2_2017

travis_selecionado$tr_status <- NULL
travis_selecionado$tr_jobs <- NULL
travis_selecionado$tr_build_number <- NULL
travis_selecionado$tr_job_id <- NULL
travis_selecionado$tr_log_lan <- NULL
travis_selecionado$tr_log_status <- NULL
travis_selecionado$tr_log_setup_time <- NULL
travis_selecionado$tr_log_analyzer <- NULL
travis_selecionado$tr_log_frameworks <- NULL
travis_selecionado$git_trigger_commit <- NULL
travis_selecionado$tr_virtual_merged_into <- NULL
travis_selecionado$tr_original_commit <- NULL
travis_selecionado$gh_description_complexity <- NULL
travis_selecionado$gh_pushed_at <- NULL
travis_selecionado$gh_build_started_at <- NULL
travis_selecionado$gh_pull_req_num <- NULL
travis_selecionado$git_merged_with <- NULL
travis_selecionado$git_branch <- NULL
travis_selecionado$gh_commits_in_push <- NULL
travis_selecionado$git_prev_commit_resolution_status <- NULL
travis_selecionado$git_prev_built_commit <- NULL
travis_selecionado$tr_prev_build <- NULL
travis_selecionado$gh_first_commit_created_at <- NULL
travis_selecionado$git_all_built_commits <- NULL
travis_selecionado$git_trigger_commit <- NULL
travis_selecionado$tr_virtual_merged_into <- NULL
travis_selecionado$tr_original_commit <- NULL
travis_selecionado$gh_description_complexity <- NULL
travis_selecionado$gh_pushed_at <- NULL
travis_selecionado$gh_pr_created_at <- NULL
travis_selecionado$gh_pull_req_num <- NULL
travis_selecionado$git_merged_with <- NULL
travis_selecionado$tr_duration <- NULL
travis_selecionado$tr_log_bool_tests_ran <- NULL
travis_selecionado$tr_log_bool_tests_failed <- NULL
travis_selecionado$tr_log_num_tests_ok <- NULL
travis_selecionado$tr_log_num_tests_failed <- NULL
travis_selecionado$tr_log_num_tests_run <- NULL
travis_selecionado$tr_log_num_tests_skipped <- NULL
travis_selecionado$tr_log_tests_failed <- NULL
travis_selecionado$tr_log_testduration <- NULL
travis_selecionado$tr_log_buildduration <- NULL
travis_selecionado$gh_build_started_at <- NULL
travis_selecionado$tr_jobs <- NULL
travis_selecionado$tr_build_number <- NULL
travis_selecionado$tr_job_id <- NULL
travis_selecionado$tr_log_lan <- NULL
travis_selecionado$tr_log_status <- NULL
travis_selecionado$tr_log_analyzer <- NULL
travis_selecionado$tr_log_frameworks <- NULL
travis_selecionado$tr_log_setup_time <- NULL
travis_selecionado$tr_status <- NULL
travis_selecionado$gh_by_core_team_member <- NULL

travis_selecionado$gh_num_commits_in_push <- as.numeric(travis_selecionado$gh_num_commits_in_push)  
travis_selecionado$gh_num_issue_comments <- as.numeric(travis_selecionado$gh_num_issue_comments)
travis_selecionado$gh_num_pr_comments <- as.numeric(travis_selecionado$gh_num_pr_comments)

travis_selecionado <- travis_selecionado[!duplicated(travis_selecionado$tr_build_id),]
ausentes <- travis_selecionado[!complete.cases(travis_selecionado),] 
travis_sem_ausentes <- travis_selecionado[complete.cases(travis_selecionado),]


mice_plot <- aggr(ausentes, col=c("navyblue", "yellow"), numbers= TRUE, sortVars=TRUE, labels=names(ausentes), gap=1)

cores <- colorRampPalette(c("red", "white", "blue"))
correlacao_juntos <- cor(travis_sem_ausentes[,c(5:20)])
corrplot(correlacao_juntos, order="AOE", method="square", col=cores(20), tl.srt=45, tl.cex=0.75, tl.col="black")
corrplot(correlacao_juntos, add=TRUE, type="lower", method="number", order="AOE", col="black", diag=FALSE, tl.pos="n", cl.pos="n", number.cex=0.75)


# # Separando builds por status de falha

travis_build_false <- travis_sem_ausentes %>% filter(build_successful == FALSE)
travis_build_true <- travis_sem_ausentes %>% filter(build_successful == TRUE)

# Pegando os projetos e a quantidade de build falhadas

projetos_false <-    travis_build_false %>%
       group_by(gh_project_name) %>%
       tally()

projetos_true <-    travis_build_true %>%
  group_by(gh_project_name) %>%
  tally()


geoserver <- travis_sem_ausentes %>% filter(gh_project_name == 'geoserver/geoserver')
facebook <- travis_sem_ausentes %>% filter(gh_project_name == 'facebook/presto')
puppet <- travis_sem_ausentes %>% filter(gh_project_name == 'puppetlabs/puppet') 
mifosx <- travis_sem_ausentes %>% filter(gh_project_name == 'openMF/mifosx')
rapid7 <- travis_sem_ausentes %>% filter(gh_project_name == 'rapid7/metasploit-framework') 
android <- travis_sem_ausentes %>% filter(gh_project_name == 'owncloud/android')
diaspora <- travis_sem_ausentes %>% filter(gh_project_name == 'diaspora/diaspora')
diaspora_normal <- travis_sem_ausentes %>% filter(gh_project_name == 'diaspora/diaspora')
open_project <- travis_sem_ausentes %>% filter(gh_project_name == 'opf/openproject')

# # Visualização de dados para o geoserver

boxplot(puppet$gh_team_size, android$gh_team_size, diaspora$gh_team_size, mifosx$gh_team_size, open_project$gh_team_size, facebook$gh_team_size, geoserver$gh_team_size, 
                 xlab="Projetos", ylab="Tamanho da equipe", 
                 col=topo.colors(7))
 
   legend("topright",inset=.02, title="Descrição dos projetos", c("puppet", "android", "diaspora", "mifos", "open project", "facebook", "geoserver"), fill=topo.colors(7), horiz=TRUE, cex=0.8)

boxplot(puppet$gh_sloc, android$gh_sloc, diaspora$gh_sloc, mifosx$gh_sloc, open_project$gh_sloc, facebook$gh_sloc, geoserver$gh_sloc, 
           xlab="Projetos", ylab="GH SLOC", 
           col=topo.colors(7))
   
   legend("topright",inset=.02, title="Descrição dos projetos", c("puppet", "android", "diaspora", "mifos", "open_project", "facebook", "geoserver"), fill=topo.colors(7), horiz=TRUE, cex=0.8)
   
boxplot(puppet$gh_num_commits_on_files_touched, android$gh_num_commits_on_files_touched, diaspora$gh_num_commits_on_files_touched, mifosx$gh_num_commits_on_files_touched, open_project$gh_num_commits_on_files_touched, facebook$gh_num_commits_on_files_touched, geoserver$gh_num_commits_on_files_touched, 
           xlab="Projetos", ylab="GH num commirs on files touched", 
           col=topo.colors(7))
   
   legend("topright",inset=.02, title="Descrição dos projetos", c("puppet", "android", "diaspora", "mifos", "open_project", "facebook", "geoserver"), fill=topo.colors(7), horiz=TRUE, cex=0.8)
   
rfeControl <- rfeControl(functions=rfFuncs, method="cv", number=10)

diaspora$tr_build_id <- NULL
diaspora$gh_project_name <- NULL
diaspora$gh_is_pr <- NULL
diaspora$gh_lang <- NULL

open_project$tr_build_id <- NULL
open_project$gh_project_name <- NULL
open_project$gh_is_pr <- NULL
open_project$gh_lang <- NULL

rapid7$tr_build_id <- NULL
rapid7$gh_project_name <- NULL
rapid7$gh_is_pr <- NULL
rapid7$gh_lang <- NULL


mifosx$tr_build_id <- NULL
mifosx$gh_project_name <- NULL
mifosx$gh_is_pr <- NULL
mifosx$gh_lang <- NULL

facebook$tr_build_id <- NULL
facebook$gh_project_name <- NULL
facebook$gh_is_pr <- NULL
facebook$gh_lang <- NULL

geoserver$tr_build_id <- NULL
geoserver$gh_project_name <- NULL
geoserver$gh_is_pr <- NULL
geoserver$gh_lang <- NULL

puppet$tr_build_id <- NULL
puppet$gh_project_name <- NULL
puppet$gh_is_pr <- NULL
puppet$gh_lang <- NULL

android$tr_build_id <- NULL
android$gh_project_name <- NULL
android$gh_is_pr <- NULL
android$gh_lang <- NULL

geoserver$build_successful <- as.factor(geoserver$build_successful)
facebook$build_successful <- as.factor(facebook$build_successful)
puppet$build_successful <- as.factor(puppet$build_successful)
mifosx$build_successful <- as.factor(mifosx$build_successful)
rapid7$build_successful <- as.factor(rapid7$build_successful)
android$build_successful <- as.factor(android$build_successful)
diaspora$build_successful <- as.factor(diaspora$build_successful)
open_project$build_successful <- as.factor(open_project$build_successful)

geoserver <- as.data.frame(geoserver)
puppet <- as.data.frame(puppet)
mifosx <- as.data.frame(mifosx)
rapid7 <- as.data.frame(rapid7)
android <- as.data.frame(android)
diaspora <- as.data.frame(diaspora)
facebook <- as.data.frame(facebook)
open_project <- as.data.frame(open_project)

mice_plot <- aggr(ausentes, col=c("navyblue", "yellow"), numbers= TRUE, sortVars=TRUE, labels=names(ausentes), gap=1)

results_rfe_android <- rfe(android[,-22], android[,22], sizes=c(1:21), rfeControl=rfeControl)
results_rfe_facebook <- rfe(facebook[,-22], facebook[,22], sizes=c(1:21), rfeControl=rfeControl)
results_rfe_geoserver <- rfe(geoserver[,-22], geoserver[,22], sizes=c(1:21), rfeControl=rfeControl)
results_rfe_mifosx <- rfe(mifosx[,-22], mifosx[,22], sizes=c(1:21), rfeControl=rfeControl)
results_rfe_puppet <- rfe(puppet[,-22], puppet[,22], sizes=c(1:21), rfeControl=rfeControl)
results_rfe_diaspora <- rfe(diaspora[,-22], diaspora[,22], sizes=c(1:21), rfeControl=rfeControl)
results_rfe_open_project <- rfe(open_project[,-22], open_project[,22], sizes=c(1:21), rfeControl=rfeControl)


print(results_rfe_android)
print(results_rfe_facebook)
print(results_rfe_geoserver)
print(results_rfe_mifosx)
print(results_rfe_puppet)
print(results_rfe_diaspora)
print(results_rfe_open_project)

predictors(results_rfe_android)
predictors(results_rfe_facebook)
predictors(results_rfe_geoserver)
predictors(results_rfe_mifosx)
predictors(results_rfe_puppet)
predictors(results_rfe_facebook)
predictors(results_rfe_open_project)


#Verificando quais amostras estão desbalanceadas

prop.table(table(android$build_successful)) # desbalanceado
prop.table(table(facebook$build_successful)) 
prop.table(table(diaspora$build_successful))
prop.table(table(puppet$build_successful)) #desbalanceado
prop.table(table(geoserver$build_successful))
prop.table(table(mifosx$build_successful)) #desbalanceado

# Balanceando amostras desbalanceadas com método sintético

puppet <- SMOTE(build_successful ~., as.data.frame(puppet), perc.over = 100, perc.under=200)
mifosx <- SMOTE(build_successful ~., as.data.frame(mifosx), perc.over = 100, perc.under=200)
#vagrant <- SMOTE(build_successful ~., as.data.frame(vagrant), perc.over = 100, perc.under=200)
android <- SMOTE(build_successful ~., as.data.frame(android), perc.over = 100, perc.under=200)
#diaspora <- SMOTE(build_successful ~., as.data.frame(diaspora), perc.over = 100, perc.under=200)
rapid7 <- ROSE(build_successful ~ ., data =rapid7, seed = 1)$data
diaspora <- ROSE(build_successful ~ ., data =diaspora, seed = 1)$data
open_project <- ROSE(build_successful ~ ., data =open_project, seed = 1)$data

prop.table(table(android$build_successful)) # balanceado
prop.table(table(puppet$build_successful)) # balanceado
prop.table(table(mifosx$build_successful)) # balanceado
prop.table(table(open_project$build_successful)) # balanceado

levels(geoserver$build_successful) <- c("false.", "true.") 
levels(facebook$build_successful) <- c("false.", "true.")
levels(puppet$build_successful) <- c("false.", "true.")
levels(mifosx$build_successful) <- c("false.", "true.")
levels(rapid7$build_successful) <- c("false.", "true.")
levels(android$build_successful) <- c("false.", "true.")
levels(diaspora$build_successful) <- c("false.", "true.")
levels(open_project$build_successful) <- c("false.", "true.")

indiceValidacao_geoserver <- createDataPartition(geoserver$build_successful, p=0.80, list=FALSE)
teste_geoserver <- geoserver[-indiceValidacao_geoserver,]
treino_geoserver <- geoserver[indiceValidacao_geoserver,]

indiceValidacao_rapid7 <- createDataPartition(rapid7$build_successful, p=0.80, list=FALSE)
teste_rapid7 <- rapid7[-indiceValidacao_rapid7,]
treino_rapid7 <- rapid7[indiceValidacao_rapid7,]

indiceValidacao_open_project <- createDataPartition(open_project$build_successful, p=0.80, list=FALSE)
teste_open_project <- open_project[-indiceValidacao_open_project,]
treino_open_project <- open_project[indiceValidacao_open_project,]


indiceValidacao_facebook <- createDataPartition(facebook$build_successful, p=0.80, list=FALSE)
teste_facebook <- facebook[-indiceValidacao_facebook,]
treino_facebook <- facebook[indiceValidacao_facebook,]

indiceValidacao_android <- createDataPartition(android$build_successful, p=0.80, list=FALSE)
teste_android <- android[-indiceValidacao_android,]
treino_android <- android[indiceValidacao_android,]


indiceValidacao_puppet <- createDataPartition(puppet$build_successful, p=0.80, list=FALSE)
teste_puppet <- puppet[-indiceValidacao_puppet,]
treino_puppet <- puppet[indiceValidacao_puppet,]

indiceValidacao_mifosx <- createDataPartition(mifosx$build_successful, p=0.80, list=FALSE)
teste_mifosx <- mifosx[-indiceValidacao_mifosx,]
treino_mifosx <- mifosx[indiceValidacao_mifosx,]

indiceValidacao_diaspora <- createDataPartition(diaspora$build_successful, p=0.80, list=FALSE)
teste_diaspora <- diaspora[-indiceValidacao_diaspora,]
treino_diaspora <- diaspora[indiceValidacao_diaspora,]


cls = makeCluster(10)
registerDoParallel(cls)

parametrosControle_geoserver <- trainControl(method="cv", number = 10, classProbs=TRUE, savePredictions=TRUE, index=createFolds(treino_geoserver$build_successful), allowParallel = TRUE)
parametrosControle_facebook <- trainControl(method="cv", number = 10, classProbs=TRUE, savePredictions=TRUE, index=createFolds(treino_facebook$build_successful), allowParallel = TRUE)
parametrosControle_puppet <- trainControl(method="cv", number = 10, classProbs=TRUE, savePredictions=TRUE, index=createFolds(puppet$build_successful), allowParallel = TRUE)
parametrosControle_mifosx <- trainControl(method="cv", number = 10, classProbs=TRUE, savePredictions=TRUE, index=createFolds(mifosx$build_successful), allowParallel = TRUE)
parametrosControle_rapid7 <- trainControl(method="cv", number = 10, classProbs=TRUE, savePredictions=TRUE, index=createFolds(treino_rapid7$build_successful), allowParallel = TRUE)
parametrosControle_android <- trainControl(method="cv", number = 10, classProbs=TRUE, savePredictions=TRUE, index=createFolds(android$build_successful), allowParallel = TRUE)
parametrosControle_diaspora <- trainControl(method="cv", number = 10, classProbs=TRUE, savePredictions=TRUE, index=createFolds(treino_diaspora$build_successful), allowParallel = TRUE)
parametrosControle_open_project <- trainControl(method="cv", number = 10, classProbs=TRUE, savePredictions=TRUE, index=createFolds(treino_open_project$build_successful), allowParallel = TRUE)

algorithms <- c("rpart", "rf", "knn", "svmRadial", "nb")

modelos_geoserver <- caretList(build_successful~., data=treino_geoserver, trControl=parametrosControle_geoserver, methodList=algorithms)
modelos_facebook <- caretList(build_successful~., data=treino_facebook, trControl=parametrosControle_facebook, methodList=algorithms)
modelos_puppet <- caretList(build_successful~., data = as.data.frame(puppet), trControl=parametrosControle_puppet, methodList=algorithms)
modelos_mifosx <- caretList(build_successful~., data = as.data.frame(mifosx), trControl=parametrosControle_mifosx, methodList=algorithms)
modelos_rapid7 <- caretList(build_successful~., data = as.data.frame(treino_rapid7), trControl=parametrosControle_rapid7, methodList=algorithms)
modelos_android <- caretList(build_successful~., data = as.data.frame(android), trControl=parametrosControle_android, methodList=algorithms)
modelos_diaspora <- caretList(build_successful~., data = as.data.frame(treino_diaspora), trControl=parametrosControle_diaspora, methodList=algorithms)
modelos_open_project <- caretList(build_successful~., data = as.data.frame(treino_open_project), trControl=parametrosControle_open_project, methodList=algorithms)

modelos_facebook
modelos_geoserver
modelos_puppet
modelos_mifosx
modelos_android
modelos_rapid7
modelos_diaspora
modelos_open_project



# Printando o resultado do cross validation na execução dos modelos na base escolhida
resultsCaret_android <- resamples(modelos_android)
resultsCaret_rapid7 <- resamples(modelos_rapid7)
resultsCaret_geoserver <- resamples(modelos_geoserver)
resultsCaret_puppet <- resamples(modelos_puppet) 
resultsCaret_facebook <- resamples(modelos_facebook)
resultsCaret_mifosx <- resamples(modelos_mifosx)
resultsCaret_diaspora <- resamples(modelos_diaspora)
resultsCaret_open_project <- resamples(modelos_open_project)

scales <- list(x=list(relation="free"), y=list(relation="free"))

# Resultados para Android
bwplot(resultsCaret_android, scales=scales)


# Resultados para Geoserver
bwplot(resultsCaret_geoserver, scales=scales)

# Resultados para Puppet
bwplot(resultsCaret_puppet, scales=scales)

# Resultados para Facebook
bwplot(resultsCaret_facebook, scales=scales)

# Resultados para Mifosx
bwplot(resultsCaret_mifosx, scales=scales)

# Resultados para Diaspora
bwplot(resultsCaret_diaspora, scales=scales)
bwplot(resultsCaret_open_project, scales=scales)

# Verificando corelações entre modelos

modelCor(resultsCaret_android)
modelCor(resultsCaret_facebook)
modelCor(resultsCaret_geoserver)
modelCor(resultsCaret_puppet)
modelCor(resultsCaret_diaspora)
modelCor(resultsCaret_mifosx)
modelCor(resultsCaret_open_project)

splom(resultsCaret_android)
splom(resultsCaret_facebook)
splom(resultsCaret_geoserver)
splom(resultsCaret_puppet)
splom(resultsCaret_diaspora)
splom(resultsCaret_mifosx)
splom(resultsCaret_open_project)


trainControl <- trainControl(method="repeatedcv", number=10, repeats=10, savePredictions=TRUE, classProbs=TRUE, allowParallel = TRUE)
stackControl <- trainControl(method="repeatedcv", number=10, repeats=10, savePredictions=TRUE, classProbs=TRUE, allowParallel = TRUE)

stack_facebook.svm <- caretStack(modelos_facebook, method="svmRadial", metric="Accuracy", trControl=stackControl)
stack_puppet.svm <- caretStack(modelos_puppet, method="svmRadial", metric="Accuracy", trControl=stackControl)
stack_mifosx.svm <- caretStack(modelos_mifosx, method="svmRadial", metric="Accuracy", trControl=stackControl)
stack_android.svm <- caretStack(modelos_android, method="svmRadial", metric="Accuracy", trControl=stackControl)
stack_diaspora.svm <- caretStack(modelos_diaspora, method="svmRadial", metric="Accuracy", trControl=stackControl)
stack_rapid7.svm <- caretStack(modelos_rapid7, method="svmRadial", metric="Accuracy", trControl=stackControl)
stack_geoserver.svm <- caretStack(modelos_geoserver, method="svmRadial", metric="Accuracy", trControl=stackControl)
stack_open_project.svm <- caretStack(modelos_open_project, method="svmRadial", metric="Accuracy", trControl=stackControl)


stack_facebook.rf <- caretStack(modelos_facebook, method="rf", metric="Accuracy", trControl=stackControl)
stack_puppet.rf <- caretStack(modelos_puppet, method="rf", metric="Accuracy", trControl=stackControl)
stack_mifosx.rf <- caretStack(modelos_mifosx, method="rf", metric="Accuracy", trControl=stackControl)
stack_android.rf <- caretStack(modelos_android, method="rf", metric="Accuracy", trControl=stackControl)
stack_diaspora.rf <- caretStack(modelos_diaspora, method="rf", metric="Accuracy", trControl=stackControl)
stack_geoserver.rf <- caretStack(modelos_geoserver, method="rf", metric="Accuracy", trControl=stackControl)


stack_facebook.dt <- caretStack(modelos_facebook, method="rpart", metric="Accuracy", trControl=stackControl)
stack_puppet.dt <- caretStack(modelos_puppet, method="rpart", metric="Accuracy", trControl=stackControl)
stack_mifosx.dt <- caretStack(modelos_mifosx, method="rpart", metric="Accuracy", trControl=stackControl)
stack_diaspora.dt <- caretStack(modelos_diaspora, method="rpart", metric="Accuracy", trControl=stackControl)
stack_geoserver.dt <- caretStack(modelos_geoserver, method="rpart", metric="Accuracy", trControl=stackControl)
stack_android.dt <- caretStack(modelos_android, method="rpart", metric="Accuracy", trControl=stackControl)

stack_open_project.knn <- caretStack(modelos_open_project, method="knn", metric="Accuracy", trControl=stackControl)
stack_diaspora.knn <- caretStack(modelos_diaspora, method="knn", metric="Accuracy", trControl=stackControl)

stack_facebook.nb <- caretStack(modelos_nb, method="nb", metric="Accuracy", trControl=stackControl)
stack_puppet.nb <- caretStack(modelos_nb, method="nb", metric="Accuracy", trControl=stackControl)
stack_mifosx.nb <- caretStack(modelos_mifosx, method="nb", metric="Accuracy", trControl=stackControl)
stack_android.nb <- caretStack(modelos_android, method="nb", metric="Accuracy", trControl=stackControl)
stack_geoserver.nb <- caretStack(modelos_geoserver, method="nb", metric="Accuracy", trControl=stackControl)
stack_diaspora.nb <- caretStack(modelos_diaspora, method="nb", metric="Accuracy", trControl=stackControl)

pesoModelos <- caretEnsemble(modelos_facebook)
pesoModelos_android <- caretEnsemble(modelos_android)

summary(pesoModelos)
summary(pesoModelos_android)
plot(pesoModelos_android)

print(stack_facebook.svm)
print(stack_puppet.rf)
print(stack_facebook.dt)


# # Android - RPART

tree = rpart(build_successful ~ ., data = treino_android)
tree_fb = rpart(build_successful ~ ., data = treino_facebook)
tree_ppt = rpart(build_successful ~ ., data = treino_puppet)
tree_mfx = rpart(build_successful ~ ., data = treino_mifosx)

# Verifica os resultados nos dados de treino
predictions <- predict(tree, newdata = teste_android)
confusionMatrix(ifelse(predictions[,1] > 0.5, "false.", "true."), teste_android$build_successful)

predictionsFB <- predict(tree_fb, newdata = teste_facebook)
confusionMatrix(ifelse(predictionsFB[,1] > 0.5, "false.", "true."), teste_facebook$build_successful)

predictionsPPT <- predict(tree_ppt, newdata = teste_puppet)
confusionMatrix(ifelse(predictionsPPT[,1] > 0.5, "false.", "true."), teste_puppet$build_successful)

predictionsMFX <- predict(tree_mfx, newdata = teste_mifosx)
confusionMatrix(ifelse(predictionsMFX[,1] > 0.5, "false.", "true."), teste_mifosx$build_successful)


treino_android$build_successful <- as.logical(treino_android$build_successful)
teste_android$build_successful <- as.logical(teste_android$build_successful)

rf = randomForest(build_successful ~ ., data = treino_android)
predictionsRF_android <- predict(rf, newdata = teste_android)
roc.curve(teste_android$build_successful, predictionsRF_android)
table(predictionsRF_android, teste_android$build_successful)

# # Random FOrest Facebook

rf_facebook = randomForest(build_successful ~ ., data = treino_facebook)
predictionsRF_facebook <- predict(rf_facebook, newdata = teste_facebook)
roc.curve(teste_facebook$build_successful, predictionsRF_facebook)
table(predictionsRF_facebook, teste_facebook$build_successful)

# # Random FOrest Mifosx

rf_mifosx = randomForest(build_successful ~ ., data = treino_mifosx)
predictionsRF_mifosx <- predict(rf_mifosx, newdata = teste_mifosx)
accuracy.meas(teste_mifosx$build_successful, predictionsRF_mifosx)
roc.curve(teste_mifosx$build_successful, predictionsRF_mifosx)
table(predictionsRF_mifosx, teste_mifosx$build_successful)

# # Random FOrest Vagrant

# Verifica os resultados nos dados de treino

# # SVM Vagrant


svm_geoserver = svm(build_successful~., data=treino_geoserver)
predictionsSVM_geoserver <- predict(svm_geoserver, newdata = as.data.frame(teste_geoserver))
accuracy.meas(teste_geoserver$build_successful, predictionsSVM_geoserver)
roc.curve(teste_geoserver$build_successful, predictionsSVM_geoserver)
table(predictionsSVM_geoserver, teste_geoserver$build_successful)

svm_diaspora = svm(build_successful~., data=treino_diaspora, sigma = 0.196885, C = 0.50)
predictionsSVM_diaspora <- predict(stack_diaspora.svm, newdata = as.data.frame(teste_diaspora))
accuracy.meas(teste_diaspora$build_successful, predictionsSVM_diaspora)
roc.curve(teste_diaspora$build_successful, predictionsSVM_diaspora)
table(predictionsSVM_diaspora, teste_diaspora$build_successful)

# # SVM Android

treino_android$gh_diff_tests_deleted <- NULL
treino_android$gh_diff_tests_added <- NULL
treino_android$gh_diff_doc_files <- NULL

svm_android = svm(build_successful~., data=treino_android, kernel='radial', cost=0.50, gamma=1, sigma=0.2563941)
predictionsSVM_android <- predict(svm_android, newdata = as.data.frame(teste_android))

accuracy.meas(teste_android$build_successful, predictionsSVM_android)
roc.curve(teste_android$build_successful, predictionsSVM_android)
table(predictionsSVM_android, teste_android$build_successful)

# # KNN Android


indiceValidacao_android_2 <- createDataPartition(android$build_successful, p=0.80, list=FALSE)
teste_android_2 <- android[-indiceValidacao_android_2,]
treino_android_2 <- android[indiceValidacao_android_2,]
build_successful_treino <- treino_android_2[,16]
build_successful_teste <- teste_android_2[,16]

treino_android_2$build_successful <- NULL
teste_android_2$build_successful <- NULL

knn_android <- knn(treino_android_2, teste_android_2, build_successful_treino, k=7)
roc.curve(build_successful_teste, knn_android)
accuracy.meas(build_successful_teste, knn_android)

table(knn_android, build_successful_teste)

predictionsKNN_open_server <- predict(stack_open_project.knn, newdata = as.data.frame(teste_open_project))
roc.curve(teste_open_project$build_successful, predictionsKNN_open_server)
accuracy.meas(teste_open_project$build_successful, predictionsKNN_open_server)
table(predictionsKNN_open_server, teste_open_project$build_successful)

predictionsKNN_diaspora <- predict(stack_diaspora.knn, newdata = as.data.frame(teste_diaspora))
roc.curve(teste_diaspora$build_successful, predictionsKNN_diaspora)
accuracy.meas(teste_diaspora$build_successful, predictionsKNN_diaspora)
table(predictionsKNN_diaspora, teste_diaspora$build_successful)

# # Naive Bayes - melhor resultado no dataset geoserver

nb_geoserver = naiveBayes(build_successful ~., data=treino_geoserver, usekernel=TRUE, adjust=1)
predictionsNB_geoserver <- predict(stack_geoserver.nb, newdata = as.data.frame(teste_geoserver))
roc.curve(teste_geoserver$build_successful, predictionsNB_geoserver)
accuracy.meas(teste_geoserver$build_successful, predictionsNB_geoserver)
table(predictionsNB_geoserver, teste_geoserver$build_successful)

nb_rapid7 = naiveBayes(build_successful ~., data=treino_rapid7)
predictionsNB_rapid7 <- predict(modelos_rapid7$nb, newdata = as.data.frame(teste_rapid7))
roc.curve(teste_rapid7$build_successful, predictionsNB_rapid7)
accuracy.meas(teste_geoserver$build_successful, predictionsNB_geoserver)
table(predictionsNB_rapid7, teste_rapid7$build_successful)

predictionsNB_diaspora <- predict(modelos_diaspora$nb, newdata = as.data.frame(teste_diaspora))
roc.curve(teste_diaspora$build_successful, predictionsNB_diaspora)
accuracy.meas(teste_geoserver$build_successful, predictionsNB_geoserver)
table(predictionsNB_diaspora, teste_diaspora$build_successful)