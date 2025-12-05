#Instalación de paquetes
install.packages("readxl")
install.packages("writexl")  
install.packages("irrCAC")
#LLamado de paquetes
library(readxl)
library(dplyr)
library(writexl)
library(tidyr)
library(irr)
library(psych)
library(irrCAC)
#bases de datos
Jueces_Bruto <- read_excel("Base_depurada.xlsx", 
                            sheet = "Jueces", na = "999")
Cal_Inicial_Bruto<- read_excel("Base_depurada.xlsx", 
                            sheet = "Diana", na = "999")
IDs_validos <- unique(Jueces_Bruto$ID)
Cal_Juez1_filtrado <- Cal_Inicial_Bruto %>%
  filter(ID %in% IDs_validos)
Puntajes_COM<- bind_rows(Cal_Juez1_filtrado, Jueces_Bruto) %>%
  arrange(ID, Juez)
write_xlsx(Puntajes_COM, "Puntajes_COM.xlsx")
puntajes_wide <- Puntajes_COM %>%
  select(ID, Juez, CSSRS1:CSSRS22B)
puntajes_long <- puntajes_wide %>%
  pivot_longer(cols = CSSRS1:CSSRS22B,
               names_to = "ITEM",
               values_to = "PUNTAJE")
puntajes_interjueces <- puntajes_long %>%
  pivot_wider(names_from = Juez,
              values_from = PUNTAJE,
              names_prefix = "JUEZ")
puntajes_interjueces <- puntajes_interjueces %>%
  arrange(ID, ITEM)
write_xlsx(puntajes_interjueces, "Puntajes_Interjueces.xlsx")
#Analisis
#IDimensión1,tem1-5,datos binarios; IDEACIÓN SUICIDA (items1-5)
Dimension1<- puntajes_interjueces %>%
  filter(ITEM %in% c("CSSRS1", "CSSRS2", "CSSRS3", "CSSRS4", "CSSRS5")) %>%
  arrange(ITEM, ID)
FAC1<-Dimension1[,c(3:7)]
agree(FAC1, tolerance=0)
pa.coeff.raw(FAC1)
kappam.fleiss(FAC1, exact = FALSE, detail = TRUE)
gwet.ac1.raw(FAC1)
FAC1_mat <- t(as.matrix(FAC1))
kripp.alpha(FAC1_mat, method = "nominal")
#IDimensión2,item6-11,datos ordinales; Intesidad suicida (items6-11)
Dimension2 <- puntajes_interjueces %>%
  filter(ITEM %in% c("CSSRS6", "CSSRS7", "CSSRS8", "CSSRS9", "CSSRS10", "CSSRS11")) %>%
  arrange(ITEM, ID)
FAC2 <- Dimension2[, c(3:7)]  # Solo columnas de jueces
agree(FAC2)
pa.coeff.raw(FAC2,weights = "ordinal")
kendall(FAC2, correct = FALSE)
gwet.ac1.raw(FAC2,weights = "ordinal")
ICC(FAC2)
FAC2_mat <- t(as.matrix(FAC2))
kripp.alpha(FAC2_mat, method = "ordinal")
#IDimensión3,item12-19,datos binarios; comportamiento suicida (items12-19)
Dimension3 <- puntajes_interjueces %>%
  filter(ITEM %in% c("CSSRS12", "CSSRS14", "CSSRS15", "CSSRS17")) %>%
  arrange(ITEM, ID)
FAC3 <- Dimension3[, c(3:7)]
agree(FAC3, tolerance = 0)
pa.coeff.raw(FAC3)  # nominal por defecto en binarios
kappam.fleiss(FAC3, exact = FALSE, detail = TRUE)
gwet.ac1.raw(FAC3)
FAC3_mat <- t(as.matrix(FAC3))
kripp.alpha(FAC3_mat, method = "nominal")
# dimensión 4 CSSRS13, 16, 18. Conteo de intentos
Dimension4 <- puntajes_interjueces %>%
  filter(ITEM %in% c("CSSRS13", "CSSRS16", "CSSRS18")) %>%
  arrange(ITEM, ID)
FAC4 <- Dimension4[, c(3:7)]
agree(FAC4)
pa.coeff.raw(FAC4,weights = "quadratic")
robinson(FAC4)
ICC(FAC4)
FAC4_mat <- t(as.matrix(FAC4))
kripp.alpha(FAC4_mat, method=("interval"))
# dimensión 5 Item20,21 y 22 sesiones A Y B INTENTO
Dimension5A <- puntajes_interjueces %>%
  filter(ITEM %in% c("CSSRS20A", "CSSRS21A", "CSSRS22A")) %>%
  arrange(ID, ITEM)
FAC5A <- Dimension5A[, c(3:7)]
FAC5Amat<-t(as.matrix(FAC5A))
kripp.alpha(FAC5Amat, method = "ordinal")
Dimension5B <- puntajes_interjueces %>%
  filter(ITEM %in% c("CSSRS20B", "CSSRS21B", "CSSRS22B")) %>%
  arrange(ID, ITEM)
FAC5B <- Dimension5B[, c(3:7)]
FAC5Bmat<-t(as.matrix(FAC5B))
kripp.alpha(FAC5Bmat, method = "ordinal")
