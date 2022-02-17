# Analisando overfitting em Árvores de Decisão ---------------------------------

# Separando as bases de Treino e Teste
set.seed(123)
titanic$Survived <- factor(titanic$Survived, labels = c('N', 'Y'))
bool_treino <- stats::runif(dim(titanic)[1]) > .25

treino <- titanic[bool_treino,]
teste <- titanic[!bool_treino,]

glimpse(titanic)

arvore <- rpart(Survived ~., 
                data = treino, 
                parms = list(split = 'gini'),
                method = 'class', xval = 5, 
                control = rpart.control(cp = 0, minsplit = 1, maxdepth = 30))
# cp = custo de processamento (limitador da árvore). Quanto menor o custo, 
# mais livre a árvore é para crescer

# Analisando a complexidade da árvore gerada
arvore$frame # não plotei devido ao peso de processamento

# Avaliando a árvore na base de treino e na base de teste
p_treino <- stats::predict(arvore, treino)
c_treino <- factor(ifelse(test = p_treino[,2] > .5, 'Y', 'N'))
p_teste <- stats::predict(arvore, teste)
c_teste <- factor(ifelse(test = p_teste[,2] > .5, 'Y', 'N'))

# Matriz de confusão na base de treino
matriz_confusao <- table(c_treino, titanic[bool_treino, 'Survived'])
matriz_confusao

acuracia <- (matriz_confusao[1,1] + matriz_confusao[2,2])/sum(matriz_confusao)
acuracia

# Matriz de confusão na base de teste
matriz_confusao <- table(c_teste, titanic[!bool_treino, 'Survived'])
matriz_confusao

acuracia <- (matriz_confusao[1,1] + matriz_confusao[2,2])/sum(matriz_confusao)
acuracia


# Curva ROC --------------------------------------------------------------------

# Vamos calcular a área abaixo da curva ROC utilizando a função 
# 'twoClassSummary' do pacote 'caret'. Ela espera um data.frame com a seguinte
# configuração:
# obs: uma coluna contendo os fatores das classes obsevadas
# pred: uma coluna contendo os fatores das classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (N no caso): contém a probabilidade da classe 2
aval_treino <- data.frame(obs = treino$Survived,
                          pred = c_treino,
                          Y = p_treino[,2],
                          N = 1 - p_treino[,2])

caret::twoClassSummary(aval_treino, lev = levels(aval_treino$obs))

curva_roc <- ggplot2::ggplot(aval_treino, aes(d = obs, m = Y, colour = '1')) +
  geom_roc(n.cuts = 0) +
  scale_colour_viridis_d(direction = -1, begin = 0, end = .25) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(title = 'Curva ROC - base de treino')
  
curva_roc

# Avaliando agora na base de teste
aval_teste <- data.frame(obs = teste$Survived,
                          pred = c_teste,
                          Y = p_teste[,2],
                          N = 1 - p_teste[,2])

caret::twoClassSummary(aval_teste, lev = levels(aval_teste$obs))

curva_roc <- ggplot2::ggplot(aval_teste, aes(d = obs, m = Y, colour = '1')) +
  geom_roc(n.cuts = 0) +
  scale_colour_viridis_d(direction = -1, begin = 0, end = .25) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(title = 'Curva ROC - base de teste')

curva_roc


# Grid Search (pós poda) -------------------------------------------------------
tab_cp <- rpart::printcp(arvore)
tab_cp

plotcp(arvore)

tab_cp[which.min(tab_cp[,'xerror']),] #capturando o cp correspondente ao menor
# erro
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),]['CP']


set.seed(1)
arvore_poda <- rpart::rpart(Survived ~ .,
                            data = treino, parms = list(split = 'gini'), 
                            method = 'class', xval = 0, 
                            control = rpart.control(cp = cp_min,
                                                    minsplit = 1, 
                                                    maxdepth = 30))
?'rpart'
# Avaliando a árvore na base de treino e na base de teste
p_treino <- stats::predict(arvore_poda, treino)
c_treino <- factor(ifelse(test = p_treino[,2] > .5, 'Y', 'N'))
p_teste <- stats::predict(arvore_poda, teste)
c_teste <- factor(ifelse(test = p_teste[,2] > .5, 'Y', 'N'))

# Matriz de confusão na base de treino
matriz_confusao <- table(c_treino, titanic[bool_treino, 'Survived'])
matriz_confusao

acuracia <- (matriz_confusao[1,1] + matriz_confusao[2,2])/sum(matriz_confusao)
acuracia

# Matriz de confusão na base de teste
matriz_confusao <- table(c_teste, titanic[!bool_treino, 'Survived'])
matriz_confusao

acuracia <- (matriz_confusao[1,1] + matriz_confusao[2,2])/sum(matriz_confusao)
acuracia

# Curva ROC --------------------------------------------------------------------

# Vamos calcular a área abaixo da curva ROC utilizando a função 
# 'twoClassSummary' do pacote 'caret'. Ela espera um data.frame com a seguinte
# configuração:
# obs: uma coluna contendo os fatores das classes obsevadas
# pred: uma coluna contendo os fatores das classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (N no caso): contém a probabilidade da classe 2
aval_treino <- data.frame(obs = treino$Survived,
                          pred = c_treino,
                          Y = p_treino[,2],
                          N = 1 - p_treino[,2])

caret::twoClassSummary(aval_treino, lev = levels(aval_treino$obs))

curva_roc <- ggplot2::ggplot(aval_treino, aes(d = obs, m = Y, colour = '1')) +
  geom_roc(n.cuts = 0) +
  scale_colour_viridis_d(direction = -1, begin = 0, end = .25) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(title = 'Curva ROC - base de treino')

curva_roc

# Avaliando agora na base de teste
aval_teste <- data.frame(obs = teste$Survived,
                         pred = c_teste,
                         Y = p_teste[,2],
                         N = 1 - p_teste[,2])

caret::twoClassSummary(aval_teste, lev = levels(aval_teste$obs))

curva_roc <- ggplot2::ggplot(aval_teste, aes(d = obs, m = Y, colour = '1')) +
  geom_roc(n.cuts = 0) +
  scale_colour_viridis_d(direction = -1, begin = 0, end = .25) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(title = 'Curva ROC - base de teste')

curva_roc


