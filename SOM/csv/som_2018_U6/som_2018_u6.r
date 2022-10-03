require(kohonen)
require(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

colors = c("lightblue", "pink", "lightgreen")


# Data -------------------------------------------------------------------------
df_consumo_orario=read.csv('consumo_orario_df.csv', sep=";")
df_consumo_orario_diff=read.csv('consumo_orario_diff_df.csv', sep=";")
df_consumo_ora_giorno=read.csv('consumo_ora_giorno_df.csv', sep=";")
df_meteo=read.csv('meteo_df.csv', sep=";")

# Features ---------------------------------------------------------------------
fconsumo_orario = scale(as.matrix(df_consumo_orario[2:14]))
fconsumo_orario_diff = scale(as.matrix(df_consumo_orario_diff[2:14]))
fconsumo_ora_giorno = scale(as.matrix(df_consumo_ora_giorno[2:14]))
fmeteo = scale(as.matrix(df_meteo[,2:3]))

# SUPER SOM INIT ---------------------------------------------------------------
som.grid = somgrid(xdim=4, ydim=4, topo = "hexagonal", toroidal = 1)
som.layer = list('consumo_orario'=fconsumo_orario, 
                 'consumo_orario_diff'=fconsumo_orario_diff,
                 'consumo_ora_giorno'=fconsumo_ora_giorno, 
                 'fmeteo'=fmeteo)
set.seed(420)
ss=supersom(som.layer, 
            som.grid, 
            whatmap=1:4, 
            user.weights = c(0.2, 0.35, 0.35, 0.1), 
            alpha = c(0.005,0.001), 
            rlen = 300)

# Clusters ---------------------------------------------------------------------
n_cluster = 3
ss.hc = cutree(hclust(object.distances(ss, "codes")), n_cluster)

# Plot -------------------------------------------------------------------------
bgcolors = colors[0:n_cluster]

# Error during epochs
plot(ss, type = "changes")
# Mapping
plot(ss, type = "mapping", bgcol=bgcolors[as.integer(ss.hc)], pch = 1, main = "Mapping", keepMargins = TRUE)
# Layers
plot(ss, bgcol=bgcolors[as.integer(ss.hc)])
# Distances
plot(ss, type="dist.neighbours")

plot(ss, type="quality")

# Results extraction -----------------------------------------------------------
mylist = ss.hc
fn = "ssom_out/node_cluster.txt"
lapply(mylist, write, fn, append=TRUE, ncolumns=1000)

mylist = ss$unit.classif
fn = "ssom_out/day_node.txt"
lapply(mylist, write, fn, append=TRUE, ncolumns=1000)

mylist =plot(ss, type = "count")
fn = "ssom_out/node_count.txt"
lapply(mylist, write, fn, append=TRUE, ncolumns=1000)

write.csv(as.data.frame(ss[["codes"]][["consumo_orario"]]), 
          file="ssom_out/codes_consumo_orario.csv")
write.csv(as.data.frame(ss[["codes"]][["consumo_ora_giorno"]]), 
          file="ssom_out/codes_consumo_ora_giorno.csv")
write.csv(as.data.frame(ss[["codes"]][["consumo_orario_diff"]]), 
          file="ssom_out/codes_consumo_orario_diff.csv")
write.csv(as.data.frame(ss[["codes"]][["fmeteo"]]), 
          file="ssom_out/codes_meteo.csv")




