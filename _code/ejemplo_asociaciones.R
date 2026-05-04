library(arulesViz)
library(arules)
data("Groceries")
bdt <- Groceries

# Ejecución de Apriori
# supp: El ítem debe aparecer en al menos el 1% de las compras
# conf: La regla debe cumplirse en el 50% de los casos
# Generamos reglas
reglas <- apriori(Groceries, parameter = list(supp = 0.005, conf = 0.5))

# Filtramos las top 20 reglas por Lift para que el gráfico sea legible
top_reglas <- sort(reglas, by = "lift")[1:20]
plot(top_reglas, method = "graph", engine = "ggplot2")
plot(top_reglas, method = "graph", engine = "htmlwidget")
lot(ma1)
plot(ma1, interactive=T)
plot(ma3, method = "graph")
plot(ma1, method = "matrix")
plot(ma1, method = "grouped matrix")
ruleExplorer(top_reglas)
ruleExplorer(ma2)
ruleExplorer(ma3)
# Ordenar por Lift (fuerza de la asociación) y ver las top 5
inspect(sort(reglas_apriori, by = "lift")[1:5])
# Ejecución de Eclat
# Solo busca conjuntos de al menos 2 productos que aparezcan en el 2% de las transacciones
itemsets_eclat <- eclat(bdt, 
                        parameter = list(supp = 0.02, minlen = 2))

# Ver los conjuntos más frecuentes
inspect(sort(itemsets_eclat, by = "support")[1:5])