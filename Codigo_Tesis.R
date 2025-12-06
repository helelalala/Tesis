#Instalación de paquetes
install.packages("readxl") # lectura de excel
install.packages("writexl") # exportar resultados a excel  
install.packages("irrCAC") # indices de acuerdo inter jueces balanceados
install.packages("dplyr") # paquete de manejo y manipulación de datos
install.packages("tidyr")  # paquete de manejo y manipulación de datos
install.packages("psych") # paquete de acuerdo ICC
install.packages("irr") # indices de acuerdo inter jueces avanzados
install.packages("officer")
install.packages("summarytools")
#LLamado de paquetes
library(readxl)
library(dplyr)
library(writexl)
library(tidyr)
library(irr)
library(psych)
library(irrCAC)
library(flextable)
library(officer)
library(summarytools)
#Bases de datos depurada
Jueces_Bruto <- read_excel("Base_depurada.xlsx", 
                            sheet = "Jueces", na = "999")
Cal_Inicial_Bruto<- read_excel("Base_depurada.xlsx", 
                            sheet = "Diana", na = "999")
# Generación de la base de datos consolidada
IDs_validos <- unique(Jueces_Bruto$ID)
Cal_Juez1_filtrado <- Cal_Inicial_Bruto %>%
  filter(ID %in% IDs_validos)
#Datos completos en formato ancho cada fila es una entrevista evaluada por un juez
#Es decir existe 5 veces cada Identificador (190*37) y estan todos los items a lo largo
# Que serian 22 items de calificación y las descripciones
Puntajes_COM<- bind_rows(Cal_Juez1_filtrado, Jueces_Bruto) %>%
  arrange(ID, Juez)
write_xlsx(Puntajes_COM, "Puntajes_COM.xlsx") # esta es la base final
# Ahora se consolidad la base en formato largo, donde solo quedan 7 Columnas, ID, item y los 5 jueces
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
#Base en formato de analisis interjueces
puntajes_interjueces <- puntajes_interjueces %>%
  arrange(ID, ITEM)
write_xlsx(puntajes_interjueces, "Puntajes_Interjueces.xlsx") # esta es la base de analisis
#Analisis de datos
# Generación y denominación de las dimensiones
#IDimensión1,tem1-5,datos binarios; IDEACIÓN SUICIDA (items1-5)
Dimension1<- puntajes_interjueces %>%
  filter(ITEM %in% c("CSSRS1", "CSSRS2", "CSSRS3", "CSSRS4", "CSSRS5")) %>%
  arrange(ITEM, ID)
FAC1<-Dimension1[,c(3:7)] #Dimensión ideacion suicida
Dimension2 <- puntajes_interjueces %>%
  filter(ITEM %in% c("CSSRS6", "CSSRS7", "CSSRS8", "CSSRS9", "CSSRS10", "CSSRS11")) %>%
  arrange(ITEM, ID)
FAC2 <- Dimension2[, c(3:7)] #Dimensión intesidad suicida
Dimension3 <- puntajes_interjueces %>%
  filter(ITEM %in% c("CSSRS12", "CSSRS14", "CSSRS15", "CSSRS17")) %>%
  arrange(ITEM, ID)
FAC3 <- Dimension3[, c(3:7)] #Dimensión Comportamiento Suicida
Dimension4 <- puntajes_interjueces %>%
  filter(ITEM %in% c("CSSRS13", "CSSRS16", "CSSRS18")) %>%
  arrange(ITEM, ID)
FAC4 <- Dimension4[, c(3:7)] #Dimensión Numero de intentos Suicida
Dimension5A <- puntajes_interjueces %>%
  filter(ITEM %in% c("CSSRS20A", "CSSRS21A", "CSSRS22A")) %>%
  arrange(ID, ITEM)
FAC5A <- Dimension5A[, c(3:7)] # Dimensión Letalidad
Dimension5B <- puntajes_interjueces %>%
  filter(ITEM %in% c("CSSRS20B", "CSSRS21B", "CSSRS22B")) %>%
  arrange(ID, ITEM)
FAC5B <- Dimension5B[, c(3:7)]# Dimensión Letalidad potencial
#1.Resultados Dimensión Ideación Suicida
#items 5= "CSSRS1"/"CSSRS2"/"CSSRS3"/"CSSRS4"/"CSSRS5"
#Tipo Binario
agree(FAC1, tolerance=0) # Acuerdo Estricto 
pa.coeff.raw(FAC1) # Acuerdo Ponderado
kappam.fleiss(FAC1, exact = FALSE, detail = TRUE) #Kappa de Fleiss
gwet.ac1.raw(FAC1) #AC1/AC2
krippen.alpha.raw(FAC1) #Alpha con Coinf
#2.Resultados Dimensión Intensidad Suicidad
#items 6= "CSSRS6"/"CSSRS7"/"CSSRS8"/"CSSRS9"/"CSSRS10"/"CSSRS11"
#Tipo Ordinal
agree(FAC2) # Acuerdo Estricto
pa.coeff.raw(FAC2,weights = "ordinal") # Acuerdo Ponderado
kendall(FAC2, correct = FALSE) # Kendall
gwet.ac1.raw(FAC2,weights = "ordinal") #AC1/AC2
krippen.alpha.raw(FAC2,weights = "ordinal") # Alpha con Coinf
ICC(FAC2) #Modelo 2 
#3.Resultados Dimensión Comportamiento Suicida
#items 4 = "CSSRS12"/"CSSRS14"/"CSSRS15"/"CSSRS17"
#Tipo Binario
agree(FAC3, tolerance = 0) # Acuerdo Estricto 
pa.coeff.raw(FAC3) # Acuerdo Ponderado
kappam.fleiss(FAC3, exact = FALSE, detail = TRUE) #Kappa de Fleiss
gwet.ac1.raw(FAC3) #AC1/AC2
krippen.alpha.raw(FAC3) #Alpha con Coinf
#4.Resultados #Dimensión Numero de intentos Suicida
#items 3 = "CSSRS13"/"CSSRS16"/"CSSRS18"
#Tipo Conteo
agree(FAC4) # Acuerdo Estricto 
pa.coeff.raw(FAC4,weights = "quadratic") # Acuerdo Ponderado
robinson(FAC4) #Robinson
gwet.ac1.raw(FAC4,weights = "quadratic" ) #AC1/AC2
krippen.alpha.raw(FAC4, weights = "quadratic") # Alpha con Coinf
ICC(FAC4) #Modelo 2 
#5.A Dimensión Letalidad
#items 3  = "CSSRS20A"/"CSSRS21A"/"CSSRS22A"
#Ordinal
agree(FAC5A) # Acuerdo Estricto
pa.coeff.raw(FAC5A,weights = "ordinal") # Acuerdo Ponderado
kendall(FAC5A, correct = FALSE)  # Kendall
gwet.ac1.raw(FAC5A,weights = "ordinal") #AC1/AC2
krippen.alpha.raw(FAC5A,weights = "ordinal") # Alpha con Coinf
ICC(FAC5A) #Modelo 2
#5.B # Letalidad potencial
#items 3  = "CSSRS20B"/"CSSRS21B"/"CSSRS22B"
#Ordinal
agree(FAC5B) # Acuerdo Estricto
pa.coeff.raw(FAC5B,weights = "ordinal") # Acuerdo Ponderado
kendall(FAC5B, correct = FALSE)  # Kendall
gwet.ac1.raw(FAC5B,weights = "ordinal") #AC1/AC2
krippen.alpha.raw(FAC5B,weights = "ordinal") # Alpha con Coinf
ICC(FAC5B) #Modelo 2
#Exportar Resultados
tabla_resultados <- tribble(
  ~Dimension, ~Items, ~Tipo, ~Pct_Acuerdo, ~PA_IC, ~Kendall_Kappa_Robinson, ~AC12_IC, ~Alpha_IC, ~ICC,
  
  "1 Ideación Suicida", 5, "Binario",
  "90%", "0.953 (0.932–0.973)", "K = 0.888",
  "AC1 = 0.918 (0.880–0.956)", "0.888 (0.838–0.938)", "—",
  
  "2 Intensidad Suicida", 6, "Ordinal",
  "62.3%", "0.973 (0.965–0.981)", "W = 0.819",
  "AC2 = 0.905 (0.875–0.935)", "0.829 (0.777–0.880)",
  "ICC2 = 0.83 ICC2k = 0.96",
  
  "3 Comportamiento Suicida", 4, "Binario",
  "82.9%", "0.920 (0.891–0.949)", "K = 0.839",
  "AC1 = 0.840 (0.781–0.898)", "0.840 (0.781–0.898)", "—",
  
  "4 Numero de intentos Suicida", 3, "Conteo",
  "70.2%", "0.995 (0.985–1.000)", "A=0.428",
  "AC2 = 0.989 (0.969,1)  ", "0.283 (0.276–0.290)", "ICC2 = 0.28 ICC2k = 0.67",
  
  "5A  Letalidad", 3, "Ordinal",
  "7.41%", "0.879 (0.714–1.000)", "W = 0.504",
  "AC2 = NaN (NaN,NaN)", "0.395 (0.282–0.507)",
  "ICC2 = 0.47 ICC2k = 0.82",
  
  "5B Letalidad potencial", 3, "Ordinal",
  "—", "0.737 (0.480–0.993)", "—",
  "AC2 = NaN (NaN,NaN)", "0.060 (-0.24–0.36)", "ICC2 = 0.19 ICC2k = 0.54",
)

ft <- flextable(tabla_resultados) |>
  autofit() |>
  theme_booktabs() |>
  align(align = "center", part = "all") |>
  bold(part = "header") |>
  set_header_labels(
    Dimension = "Dimensión",
    Items = "Ítems",
    Tipo = "Tipo",
    Pct_Acuerdo = "% Acuerdo",
    PA_IC = "PA (IC 95%)",
    Kendall_Kappa = "Kendall/Kappa",
    AC2_IC = "AC2 (IC 95%)",
    Alpha_IC = "Alpha (IC 95%)",
    ICC = "ICC"
  )
ft
# Ajustar tabla al ancho de la página
ft <- autofit(ft)
ft <- width(ft, width = 0.9)  # puedes subir a 2.0 si queda aún angosta
doc <- read_docx()
landscape_section <- prop_section(
  page_size = page_size(orient = "landscape")
)
doc <- body_set_default_section(doc, landscape_section)
doc <- body_add_flextable(doc, ft)
print(doc, target = "Tabla_Acuerdo_CSSRS.docx")
# Analisis sociodemograficos
DATOS_BASICOS <- read_excel("DATOS BASICOS.xlsx",sheet = "CARACTERIZACIÓN")
dfSummary(DATOS_BASICOS)
get_tabla_porcentajes <- function(tabla, diccionario=NULL){
  df <- data.frame(
    Categoria = names(tabla),
    Porcentaje = round(100*as.numeric(tabla)/sum(tabla),1),
    stringsAsFactors = FALSE
  )
  if(!is.null(diccionario)) df$Categoria <- diccionario[as.numeric(df$Categoria)]
  df
}
dic_sexo <- c("Masculino","Femenino")
dic_orientacion <- c("Heterosexual","Homosexual","Bisexual","Asexual","Otro")
dic_estcivil <- c("Soltero","Casado","Unión libre","Divorciado/separado","Viudo")
dic_nivel <- c("Básica primaria o menor","Bachiller","Técnico o Tecnólogo","Profesional","Postgrado")
dic_convivencia <- c("Solo","Solo con la pareja","Familiares","Solo con amigo(s)","Otros (compañeros no amigos)")
resumen <- DATOS_BASICOS %>%
  summarise(
    n_total = n(),
    sexo_tabla = list(table(sexo)),
    orientacion_tabla = list(table(orientacion)),
    nivel_educativo_mediana = median(nivel_educativo, na.rm = TRUE),
    nivel_educativo_tabla = list(table(nivel_educativo)),
    estcivil_tabla = list(table(estcivil)),
    edad_media = mean(edad, na.rm = TRUE),
    edad_sd = sd(edad, na.rm = TRUE),
    convivencia_tabla = list(table(convivencia))
  )

get_moda_str <- function(tabla, diccionario = NULL){
  moda <- names(tabla)[which.max(tabla)]
  if(!is.null(diccionario)) moda <- diccionario[as.numeric(moda)]
  porcentaje <- round(100*max(tabla)/sum(tabla),1)
  paste0(moda, " (", porcentaje, "%)")
}
sexo_str <- get_moda_str(resumen$sexo_tabla[[1]], dic_sexo)
orientacion_str <- get_moda_str(resumen$orientacion_tabla[[1]], dic_orientacion)
estcivil_str <- get_moda_str(resumen$estcivil_tabla[[1]], dic_estcivil)
convivencia_str <- get_moda_str(resumen$convivencia_tabla[[1]], dic_convivencia)
mediana_nivel <- resumen$nivel_educativo_mediana
nivel_edu_str <- paste0(dic_nivel[mediana_nivel], " (", 
                        round(100*resumen$nivel_educativo_tabla[[1]][as.character(mediana_nivel)]/sum(resumen$nivel_educativo_tabla[[1]]),1), "%)")
edad_str <- paste0(round(resumen$edad_media,1), " ± ", round(resumen$edad_sd,1))
nivel_tab <- get_tabla_porcentajes(resumen$nivel_educativo_tabla[[1]], dic_nivel)
estcivil_tab <- get_tabla_porcentajes(resumen$estcivil_tabla[[1]], dic_estcivil)
convivencia_tab <- get_tabla_porcentajes(resumen$convivencia_tabla[[1]], dic_convivencia)
nivel_tab$Porcentaje <- paste0(nivel_tab$Porcentaje, "%")
estcivil_tab$Porcentaje <- paste0(estcivil_tab$Porcentaje, "%")
convivencia_tab$Porcentaje <- paste0(convivencia_tab$Porcentaje, "%")
resumen_vertical <- bind_rows(
  data.frame(Variable="N", Valor=as.character(resumen$n_total), stringsAsFactors=FALSE),
  data.frame(Variable="Sexo (moda)", Valor=sexo_str, stringsAsFactors=FALSE),
  data.frame(Variable="Orientación sexual (moda)", Valor=orientacion_str, stringsAsFactors=FALSE),
  data.frame(Variable="Nivel educativo (mediana)", Valor=nivel_edu_str, stringsAsFactors=FALSE),
  nivel_tab %>% rename(Variable=Categoria, Valor=Porcentaje),
  data.frame(Variable="Estado civil (moda)", Valor=estcivil_str, stringsAsFactors=FALSE),
  estcivil_tab %>% rename(Variable=Categoria, Valor=Porcentaje),
  data.frame(Variable="Edad (M ± SD)", Valor=edad_str, stringsAsFactors=FALSE),
  data.frame(Variable="Convivencia (moda)", Valor=convivencia_str, stringsAsFactors=FALSE),
  convivencia_tab %>% rename(Variable=Categoria, Valor=Porcentaje)
)
tabla_vertical_apa <- flextable(resumen_vertical) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  align(align = "left", j = "Variable", part = "body") %>%
  align(align = "center", j = "Valor", part = "body") %>%
  align(align = "center", part = "header") %>%
  fontsize(size = 11, part = "all") %>%
  padding(i = NULL, j = NULL, padding.top = 2, padding.bottom = 2, part = "all") %>%
  set_table_properties(layout = "autofit", width = 0.8)
tabla_vertical_apa
library(officer)
library(flextable)
doc2 <- read_docx()
doc2 <- body_add_flextable(doc2, tabla_vertical_apa)
print(doc2, target = "Tabla_Sociodemograficos.docx")
