library(dplyr)
library(RefManageR)
library(bibliometrix)
library(quanteda)
library(ggplot2)
library(ggpubr)
library(igraph)

help("bibliometrix")
help("igraph")
D <- readFiles("/home/alrier/Documentos/valor, satisfacción y calidad bibliometria/bib valor, calidad y satisfaccion/cvsbdt.bib")
M <- convert2df(D, dbsource = "scopus", format = "bibtex")
analizables <- M %>% filter(PY >=2015)
analisables <- biblioAnalysis(analizables, sep = ";")
options(width = 50)
S <- summary(object = analisables, k = 20, pause = FALSE)
plot(x = analisables, k = 20, pause = FALSE)
'''primera red matrici de datos cpn su grafica'''
NetMatrix <- biblioNetwork(analizables, analysis = "co-occurrences", network = "author_keywords", sep = ";")
S <- normalizeSimilarity(NetMatrix, type = "association")
perrosnet <- networkPlot(S, n = 50, Title = "co-occurrence network", type = "fruchterman", 
                   labelsize = 1, size = 10, size.cex = T, halo = T, cluster = "walktrap",
                   remove.isolates = F, curved = 0.9, edgesize = 3,remove.multiple = T, noloops = T, weighted = TRUE)
print(perrosnet)
help("networkplot")
??networkPlot
'''segunda red matricial de datos con su grafica'''
NetMatrix2 <- biblioNetwork(analizables, analysis = "co-citation", network = "references", sep = ". ")
n <- metaTagExtraction(analizables, Field = "AU_CO", sep = ";")
NetMatrix3 <- biblioNetwork(n, analysis = "collaboration", network = "countries", sep = ";")
net=networkPlot(NetMatrix3, n = 10, Title = "Country Collaboration", type = "fruchterman", labelsize = 1, size = 10, size.cex = T, halo = T, cluster = "spinglass",
                remove.isolates = T, curved = 0.9, edgesize = 3,remove.multiple = T, noloops = T, weighted = TRUE)


                   
'''Del total de resultados, extraigo los papers más citados'''
AU <- Analyse$MostCitedPapers
AUT <- AU[1:2]
View(AUT)
Analyse$MainInformation
'''Lets see most productive countries'''
Paises <- Analyse$MostProdCountries
'''Lets keep the first and thirs column of this dataframe'''
Paises <- Paises[c(1, 3)]
'''Lets change the name of the first column'''
names(Paises)[1] <- "Country"
'''Pongamos los nombres en Español'''
'''Paises$Country <- c("USA", "Taiwan", "Korea",  "Reino Unido", "Alemania", "Holanda", "Italia", "Canada", "España", "China")'''
Paises$Freq <- as.numeric(Paises$Freq)
'''Lets see the production'''
Produccion <- Resultados$AnnualProduction
'''Lets change the name of the first column'''
names(Produccion)[1] <- "Year"
'''Lets set as numeric the records of the second column'''
Produccion$Articles <- as.numeric(Produccion$Articles)

'''graficas y plots'''

Fig1A <- ggplot(Paises, aes(x=reorder(Country, Freq) , y=Freq)) + geom_bar(stat = "identity", fill="blue") + coord_flip() + xlab("Country") + ylab("Relative Frequency")
Fig1B <- ggplot(Produccion, aes(x=Year , y=Articles)) + geom_bar(stat = "identity", fill="blue") + xlab("Year") + ylab("Articles") + theme(axis.text.x = element_text(angle = 90, hjust = 1)
                                                                                                                                           ggarrange(Fig1A, Fig1B, labels = c("A", "B"), ncol = 2, nrow = 1)
                                                                                                                                           
                                                                                                                                           print(Fig1A)
                                                                                                                                           
