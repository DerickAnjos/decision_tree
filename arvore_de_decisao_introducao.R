# Vamos utilizar a base 'titanic', que já vem no r base
titanic <- titanic_train
head(titanic)

titanic <- titanic[c(2, 3, 5, 6, 7, 8, 10, 12)]
