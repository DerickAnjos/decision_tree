# Vamos utilizar a base 'titanic', que já vem no r base
titanic <- titanic_train
titanic <- titanic[c(2, 3, 5, 6, 7, 8, 10, 12)]

head(titanic)


# Objetivo: Classificar os passageiros como sobreviventes utiliza as variáveis
# da base de dados -------------------------------------------------------------

# Criando um objeto temporário para manter a base original intacta
tmp <- titanic

# Análise descritiva: vamos avaliar a distribuição de sobreviventes por cada 
# variável 'x'. Sumarizamos então 'y' por categoria de 'x' e montamos um gráfico
# de perfis
descritiva <- function(var) {
  tgc <- Rmisc::summarySE(tmp, measurevar = 'Survived', groupvars = c(var))
  ggplot(tgc) +
    geom_bar(aes(x = tgc[,var], weight = N/891, fill = as.factor(tgc[,var]))) +
    geom_errorbar(aes(x = tgc[,var], y = Survived, ymin = Survived-se, 
                      ymax = Survived+se, colour = '1'), width = .1) +
    geom_point(aes(x = tgc[,var], y = Survived, colour = '1', group = '1')) +
    geom_line(aes(x = tgc[,var], y = Survived, colour = '1', group = '1')) +
    scale_color_viridis_d(direction = -1, begin = 0, end= .25) +
    scale_fill_viridis_d(direction = -1, begin = .85, end = .95) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey', 
                                          linetype = 'solid'), 
          panel.grid.major = element_line(size = 0.15, linetype = 'solid', 
                                          colour = 'grey')) +
    theme(legend.position = 'none') +
    xlab(var) + ylab('Taxa de sobreviventes') +
    scale_y_continuous(sec.axis = sec_axis(~.*891, name = 'Frequencia'), 
                       labels = scales::percent)
}

descritiva('Sex')
descritiva('Pclass')
descritiva('Age')
descritiva('SibSp')
descritiva('Parch')
descritiva('Fare')
descritiva('Embarked')

# Categorizando as var contínuas para análise
tmp$cat_age <- quantcut(tmp$Age, q = 20)
descritiva('cat_age')

tmp$cat_fare <- quantcut(tmp$Fare, q = 10)
descritiva('cat_fare')

# Listagem das variáveis
str(tmp) #glimpse(tmp)

# Construindo a primeira árvore de decisão: função 'rpart' do pacote 'rpart'
arvore <- rpart(Survived ~ ., 
                data = titanic,
                parms = list(split = 'gini'),
                method = 'class') # esse parâmetro indica que a resposta é 
# qualitativa

class(arvore)

# Visualizando a árvore

paleta <- scales::viridis_pal(begin = .75, end = 1)(20)

rpart.plot::rpart.plot(arvore)

# Avaliando a árvore
prob <- predict(arvore, titanic)
classificacao <- prob[,2] > .5

# Construindo a matriz de confusão
matriz_confusao <- table(classificacao, titanic$Survived)
matriz_confusao

acuracia <- (matriz_confusao[1,1] + matriz_confusao[2,2])/sum(matriz_confusao)
acuracia
