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



# Carregando arquivo .csv

travistorrent_8_2_2017 <- read_csv("C:/Users/nessk/Desktop/TravisTorrent/travistorrent_8_2_2017.csv")

# Eliminando variáveis com identificadores(Algumas, estou deixando outras para analise exploratória), variáveis com datas e etc

travis_selecionado <- data.frame(tr_build_id = travistorrent_8_2_2017$tr_build_id, gh_project_name = travistorrent_8_2_2017$gh_project_name, gh_is_pr = travistorrent_8_2_2017$gh_is_pr, gh_lang = travistorrent_8_2_2017$gh_lang, gh_team_size = travistorrent_8_2_2017$gh_team_size, git_num_all_built_commits = travistorrent_8_2_2017$git_num_all_built_commits, git_diff_src_churn = travistorrent_8_2_2017$git_diff_src_churn, git_diff_test_churn = travistorrent_8_2_2017$git_diff_test_churn, gh_diff_files_added = travistorrent_8_2_2017$gh_diff_files_added, gh_diff_files_deleted = travistorrent_8_2_2017$gh_diff_files_deleted, gh_diff_files_modified = travistorrent_8_2_2017$gh_diff_files_modified, gh_diff_src_files = travistorrent_8_2_2017$gh_diff_src_files, gh_diff_doc_files = travistorrent_8_2_2017$gh_diff_doc_files, gh_diff_other_files = travistorrent_8_2_2017$gh_diff_other_files, gh_num_commits_on_files_touched = travistorrent_8_2_2017$gh_num_commits_on_files_touched, gh_sloc = travistorrent_8_2_2017$gh_sloc, gh_test_lines_per_kloc = travistorrent_8_2_2017$gh_test_lines_per_kloc, gh_asserts_cases_per_kloc = travistorrent_8_2_2017$gh_asserts_cases_per_kloc, gh_by_core_team_member = travistorrent_8_2_2017$gh_by_core_team_member, tr_duration = travistorrent_8_2_2017$tr_duration, tr_log_lan = travistorrent_8_2_2017$tr_log_lan, tr_log_analyzer = travistorrent_8_2_2017$tr_log_analyzer, tr_log_frameworks = travistorrent_8_2_2017$tr_log_frameworks, tr_log_bool_tests_ran = travistorrent_8_2_2017$tr_log_bool_tests_ran, tr_log_bool_tests_failed = travistorrent_8_2_2017$tr_log_bool_tests_failed, tr_log_num_tests_ok = travistorrent_8_2_2017$tr_log_num_tests_ok, tr_log_num_tests_failed = travistorrent_8_2_2017$tr_log_num_tests_failed, tr_log_num_tests_run = travistorrent_8_2_2017$tr_log_num_tests_run, tr_log_num_tests_skipped = travistorrent_8_2_2017$tr_log_num_tests_skipped, tr_log_testduration = travistorrent_8_2_2017$tr_log_testduration, build_successful = travistorrent_8_2_2017$build_successful)

# Eliminando registros que possuem o valor da feature tr_log_testduration como NULL

travis_selecionado <- travis_selecionado[!is.na(travis_selecionado$tr_log_testduration),]

# Agrupando por build pela função soma

soma_de_duracao_testes <- aggregate(travis_selecionado$tr_log_testduration, by=list(tr_build_id=travis_selecionado$tr_build_id), FUN=sum)

# retirando registros duplicados e trocando a coluna tr_log_testduration pela soma 

travis_selecionado <- travis_selecionado[!duplicated(travis_selecionado$tr_build_id),]
travis_selecionado$tr_log_testduration <- soma_de_duracao_testes$x

# Verificando variáveis ausentes


ausentes <- travis_selecionado[!complete.cases(travis_selecionado),]


# Plotando gráfico de proporção de variáveis ausentes

mice_plot <- aggr(ausentes, col=c("navyblue", "yellow"), numbers= TRUE, sortVars=TRUE, labels=names(ausentes), gap=1)

# Retirando valores ausentes da amostra

travis_sem_ausentes <- travis_selecionado[complete.cases(travis_selecionado),]

# # Fazendo uma análise da amostra coletada

travis_sem_ausentes %>% group_by(build_successful) %>% summarise(duration_tests = mean(tr_log_testduration), total_num_tests = sum(tr_log_num_tests_run), total_num_tests_ok = sum(tr_log_num_tests_ok), total_num_tests_failed = sum(tr_log_num_tests_failed))

travis_sem_ausentes %>% group_by(gh_lang) %>% summarise(team_size = mean(gh_team_size), total_build_TRUE = sum(build_successful == "TRUE"), total_build_FALSE = sum(build_successful == "FALSE"))


cbind(freq=table(travis_sem_ausentes$build_successful), percentage=prop.table(table(travis_sem_ausentes$build_successful))*100)

travis_agrupado_por_projetos <- travis_sem_ausentes %>%
  #select(gh_project_name, gh_lang) %>%
  sample_n(90000)

ruby <- travis_agrupado_por_projetos %>%
  filter(gh_lang == 'ruby') %>%
  sample_n(8000)

java <- travis_agrupado_por_projetos %>%
  filter(gh_lang == 'java') %>%
  sample_n(8000)

 travis_projetos_juntos <- rbind(ruby,java)
# # Análise Exploratória de Dados

#Outliers

boxplot(java$gh_team_size)
boxplot(ruby$gh_team_size)
boxplot(ruby$gh_test_lines_per_kloc)
boxplot(ruby$gh_asserts_cases_per_kloc)
boxplot(java$gh_test_lines_per_kloc)
boxplot(java$gh_asserts_cases_per_kloc)

# Detecção de outlers baseado na métrica por distância

#Retirando algumas variáveis

travis_outliers <- travis_projetos_juntos
travis_outliers$tr_build_id <- NULL
travis_outliers$gh_project_name <- NULL
travis_outliers$gh_is_pr <- NULL
travis_outliers$gh_lang <- NULL
travis_outliers$gh_by_core_team_member <- NULL
travis_outliers$tr_log_lan <- NULL
travis_outliers$tr_log_analyzer <- NULL
travis_outliers$tr_log_frameworks <- NULL
travis_outliers$tr_log_bool_tests_ran <- NULL
travis_outliers$tr_log_bool_tests_failed <- NULL

#Por alguma razão que eu desconheço, essa feature está dando problema na função mahalanobis
travis_outliers$tr_log_num_tests_run <- NULL


maha <- mahalanobis(travis_outliers[,-20], colMeans(travis_outliers[,-20]), cov(travis_outliers[,-20], use = "pairwise.complete.obs"))

#Teste q² para acharmos o ponto de corte

cutoff = qchisq(.999, ncol(travis_outliers))

#fazendo o sumário

summary(maha < cutoff)

#Separando os outliers das demais amostras

#Amostra sem os outliers
travis_sem_outliers <- travis_outliers[maha < cutoff,]

#Outliers detectados (Apenas os que foram detectados pelo mahalanobis)

travis_outliers <- travis_outliers[maha >= cutoff,]

# # Correlação de variáveis

#Java

correlacao <- cor(travis_projetos_juntos[,c(5,6,7,8,9,16,17,18,20,26,27,28,29,30)])
cores <- colorRampPalette(c("red", "white", "blue"))
corrplot(correlacao, order="AOE", method="square", col=cores(20), tl.srt=45, tl.cex=0.75, tl.col="black")
corrplot(correlacao, add=TRUE, type="lower", method="number", order="AOE", col="black", diag=FALSE, tl.pos="n", cl.pos="n", number.cex=0.75)

# # Verificando o balanceamento das classes:

prop.table(table(travis_projetos_juntos$build_successful))
cbind(freq=table(travis_projetos_juntos$build_successful), percentage=prop.table(table(travis_projetos_juntos$build_successful))*100)

#prop.table(table(ruby$build_successful))

# # Retirando identificadores e preparando o dataset para predição

set.seed(2016-11)

travis_projetos_juntos$gh_by_core_team_member = as.factor(travis_projetos_juntos$gh_by_core_team_member)
travis_projetos_juntos$tr_log_bool_tests_ran = as.factor(travis_projetos_juntos$tr_log_bool_tests_ran)
travis_projetos_juntos$tr_log_bool_tests_failed = as.factor(travis_projetos_juntos$tr_log_bool_tests_failed)
travis_projetos_juntos$gh_is_pr <- as.factor(travis_projetos_juntos$gh_is_pr)

dataset_para_rpart_rf <- travis_projetos_juntos

dataset_para_rpart_rf$tr_build_id <- NULL
dataset_para_rpart_rf$gh_project_name <- NULL
dataset_para_rpart_rf$gh_lang <- NULL
dataset_para_rpart_rf$tr_log_lan <- NULL
dataset_para_rpart_rf$tr_log_frameworks <- NULL
dataset_para_rpart_rf$tr_log_analyzer <- NULL
dataset_para_rpart_rf$tr_log_bool_tests_ran <- NULL
dataset_para_rpart_rf$build_successful <- as.factor(dataset_para_rpart_rf$build_successful)


trainIndex <- createDataPartition(dataset_para_rpart_rf$build_successful, p=0.80, list=FALSE)
dataset_para_rpart_rf.treino <- dataset_para_rpart_rf[ trainIndex,]
dataset_para_rpart_rf.teste <- dataset_para_rpart_rf [-trainIndex,]

rtree = rpart(build_successful ~ ., data = dataset_para_rpart_rf.treino, control = rpart.control(cp=0.00149092), method="class")
predictions <- predict(rtree, newdata = dataset_para_rpart_rf.treino)
confusionMatrix(ifelse(predictions[,1] > 0.5, "FALSE", "TRUE"), dataset_para_rpart_rf.treino$build_successful)

predictionsTeste <- predict(rtree, newdata = dataset_para_rpart_rf.teste)
confusionMatrix(ifelse(predictionsTeste[,1] > 0.5, "FALSE", "TRUE"), dataset_para_rpart_rf.teste$build_successful)

# # Balanceamento de classes desbalanceadas 

travis_balanceado <- SMOTE(build_successful ~ ., dataset_para_rpart_rf.treino, perc.over = 100, perc.under=200)
table(travis_balanceado$build_successful)

rtree.smote <- rpart(build_successful ~ ., data = travis_balanceado)
pred.tree.smote <- predict(rtree.smote, newdata = dataset_para_rpart_rf.teste)
confusionMatrix(ifelse(pred.tree.smote[,1] > 0.5, "FALSE", "TRUE"), dataset_para_rpart_rf.teste$build_successful)

# # Feature Selection uilizando Random Forest e Cross Validation

set.seed(123)

# Encontrando atributos altamente correlacionados (Java)

matrixCorrelacao <- cor(travis_balanceado[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,19,20,21,22,23)])
print(matrixCorrelacao)
altamenteCorrelacionados <- findCorrelation(matrixCorrelacao, cutoff=0.7)
print(altamenteCorrelacionados)

# # Retirando valores altamente correlacionados

travis_balanceado <- travis_balanceado[,-altamenteCorrelacionados]

# # Praticando o RFE

rfeControl <- rfeControl(functions=rfFuncs, method="cv", number=10)


results_rfe <- rfe(travis_balanceado[,c(2,3,4,5,6,7,8,10,11,13,15,16,17,18,19,20)], travis_balanceado[,21], sizes=c(1:20), rfeControl=rfeControl)
print(results_rfe)
predictors(results_rfe)
plot(results_rfe_java, type=c("g", "o"))

# # Feature Selection utilizando Boruta

# Java
boruta.train <- Boruta(build_successful~., data = travis_balanceado, doTrace = 2)
print(boruta.train)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

getSelectedAttributes(final.boruta, withTentative = F)
boruta.df <- attStats(final.boruta)
print(boruta.df)

# # Model Selection para identificar qual melhor modelo 

travis_balanceado$gh_is_pr <- NULL
travis_balanceado$tr_log_bool_tests_failed <- NULL
travis_balanceado$tr_log_bool_tests_failed <- NULL
trainIndex_ms <- createDataPartition(travis_balanceado$build_successful, p=0.80, list=FALSE)
tvsvc.treino <- travis_balanceado[ trainIndex_ms,]
tvsvc.teste <- travis_balanceado [-trainIndex_ms,]

tvsvc.treino$build_successful <- as.factor(tvsvc.treino$build_successful)
tvsvc.teste$build_successful <- as.factor(tvsvc.teste$build_successful)

levels(tvsvc.treino$build_successful) <- c("false.", "true.")
levels(tvsvc.teste$build_successful) <- c("false.", "true.")
parametrosControle <- trainControl(method="cv", number = 10, classProbs=TRUE, savePredictions=TRUE, index=createFolds(tvsvc.treino$build_successful))
algorithms <- c("rpart", "rf", "knn", "svmRadial", "glm")

modelos <- caretList(build_successful~., data=tvsvc.treino, trControl=parametrosControle, methodList=algorithms)

resultsCaret <- resamples(modelos) 

# Verificando se os modelos são correlacionados
 modelCor(resultsCaret)

# # Otimização de Modelos com RandomSearch

x <- travis_balanceado[,1:19]
y <- travis_balanceado[,20]

set.seed(123)
metric <- "Accuracy"
mtry <- sqrt(ncol(x))

parametroControle2 <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(.mtry=mtry)
rfDefault <- train(build_successful~., data=travis_balanceado, method="rf", metric=metric, tuneGrid=tunegrid,trControl=trainControl)
print(rfDefault)

trainControl <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
rfRandom <- train(build_successful~., data=travis_balanceado, method="rf", metric=metric, tuneLength=15,trControl=trainControl)
print(rfRandom)

tunegrid_arvore <- expand.grid(.cp=seq(0,0.1,by=0.001))
treeGrid <- train(build_successful~., data=travis_balanceado_java.smote_svac, method="rpart", metric=metric, tuneGrid=tunegrid_arvore,trControl=trainControl)
 