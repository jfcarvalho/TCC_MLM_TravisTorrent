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

# Correlação de variáveis

 correlacao <- cor(travis_sem_ausentes[,c(5,6,7,8,9,16,17,18,20,26,27,28,29,30)])
 cores <- colorRampPalette(c("red", "white", "blue"))
 corrplot(correlacao, order="AOE", method="square", col=cores(20), tl.srt=45, tl.cex=0.75, tl.col="black")
 corrplot(correlacao, add=TRUE, type="lower", method="number", order="AOE", col="black", diag=FALSE, tl.pos="n", cl.pos="n", number.cex=0.75)

 # # Recursive Geature Elimination
 
 # Utilizando por meio de cross validation
 
 parametrosControle <- rfeControl(functions=rfFuncs, method="cv", number=10)
 
 #Retirando identificadores

 rfeResults <- rfe(travis_sem_ausentes[,1:27], travis_sem_ausentes[,28], sizes=c(1:27),rfeControl=parametrosControle)
 
 # # Detecção de Outliers no dataset
 
# Retirando variaveis categóricas

 travis_outliers <- travis_sem_ausentes
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

 #Detecção por método de distância de centróides por meio da distancia de mahalanobis
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

# # Seleção das melhores Features utlizando rfe
 
# Retirando variaveis identificadoras e booleanas 
   travis_pr_true$tr_build_id <- NULL
   travis_pr_true$gh_project_name <- NULL
   travis_pr_true$gh_is_pr <- NULL
   travis_pr_true$gh_lang <- NULL
   travis_pr_true$gh_by_core_team_member <- NULL
   travis_pr_true$tr_log_analyzer <- NULL
   travis_pr_true$tr_log_lan <- NULL
   travis_pr_true$tr_log_frameworks <- NULL
   travis_pr_true$tr_log_bool_tests_failed <- NULL
   travis_pr_true$tr_log_bool_tests_ran <- NULL
 
 controlParams <- rfeControl(functions=rfFuncs, method="cv", number=10)
 
 results <- results <- rfe(travis_reduzido[,1:21], travis_reduzido[,22], sizes=c(1:21), rfeControl = controlParams)
 
# Separando os dados sem variáveis categóricas para aplicação dos modelos
 
 
# Dividindo o dataset em 80% para treino e 20% para teste
 
 dataset_para_arvore <- travis_sem_ausentes
 trainIndex <- createDataPartition(dataset_para_arvore$build_successful, p=0.80, list=FALSE)
 dataset_para_arvore.treino <- dataset_para_arvore[ trainIndex,]
 dataset_para_arvore.teste <- dataset_para_arvore [-trainIndex,]
 
# Treinando o modelo de arvore de decisão
 set.seed(123)
 
 tree = rpart(build_successful ~ ., data = dataset_para_arvore.treino, control = rpart.control(cp=0.00149092), method="class")
 predictions <- predict(tree, newdata = dataset_para_arvore.treino)
 confusionMatrix(ifelse(predictions[,1] > 0.5, "FALSE", "TRUE"), dataset_para_arvore.treino$build_successful)

# Treinando o modelo para bayes
 
 dataset_para_bayes <- travis_sem_ausentes
 trainIndex <- createDataPartition(dataset_para_bayes$build_successful, p=0.80, list=FALSE)
 dataset_para_bayes.treino <- dataset_para_bayes[ trainIndex,]
 dataset_para_bayes.teste <- dataset_para_bayes [-trainIndex,]
  
 # Transformando dados lógicos em factor
 
 dataset_para_bayes.treino$gh_by_core_team_member = as.factor(dataset_para_bayes.treino$gh_by_core_team_member)
 dataset_para_bayes.treino$tr_log_bool_tests_ran = as.factor(dataset_para_bayes.treino$tr_log_bool_tests_ran)
 dataset_para_bayes.treino$tr_log_bool_tests_failed = as.factor(dataset_para_bayes.treino$tr_log_bool_tests_failed)
 dataset_para_bayes.treino$gh_is_pr <- as.factor(dataset_para_bayes.treino$gh_is_pr)
 
 #Retirando variáveis categóricas
 
 dataset_para_bayes.treino$tr_build_id <- NULL
 dataset_para_bayes.treino$gh_project_name <- NULL
 dataset_para_bayes.treino$gh_lang <- NULL
 dataset_para_bayes.treino$tr_log_lan <- NULL
 dataset_para_bayes.treino$tr_log_analyzer <- NULL
 dataset_para_bayes.treino$tr_log_frameworks <- NULL
 
 # Treinando o modelo naive bayes

 set.seed(123)
 
 nb = naiveBayes(build_successful ~ ., data = dataset_para_bayes.treino)
 predictionsNB <- predict(nb, newdata = dataset_para_bayes.treino)
 confusionMatrix(ifelse(predictionsNB > 0.5, "FALSE", "TRUE"), dataset_para_bayes.treino$build_successful)
