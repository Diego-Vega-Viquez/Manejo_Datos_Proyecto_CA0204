
# Este documento corresponde al archivo .R que contiene los códigos para la 
# limpieza de la base de datos


# Librerías:

install.packages("ProjectTemplate")
install.packages("devtools")
install.packages("tidyverse")
install.packages("cowplot")
install.packages("openintro")

library("digest")
library("tibble")
library(ProjectTemplate)
library(tidyverse)
library(cowplot)
  # Estos son necesarios para cargar openintro
  library(airports)
  library(cherryblossom)
  library(usdata)
library(openintro)
library(haven) # Para cargar el .sav

##############################################

# Algunas funciones descubiertas

# sum(!is.na(variable)) -- Permite saber cuantas variables no son NA --

##############################################


# Cargamos la base de datos 

datos <- read_sav("data/GPES-ELAB-GEBD-ENAHO-2023_BdBasePublica.sav")
datos_con_tipo_de_varibles <- datos

datos_con_tipo_de_varibles$A13 <- as_factor(datos_con_tipo_de_varibles$A13)
# Asistencia a educación formal 
# Tasa de respuesta: 100%
sum(!is.na(datos_con_tipo_de_varibles$A13))/30540

datos_con_tipo_de_varibles$NivInst <- as_factor(datos_con_tipo_de_varibles$NivInst)
# Nivel de instrucción
# Tasa de respuesta: 100%
sum(!is.na(datos_con_tipo_de_varibles$NivInst))/30540

datos_con_tipo_de_varibles$A14 <- as_factor(datos_con_tipo_de_varibles$A14)
# Último año aprovado 
# Tasa de respuesta: 100%
sum(!is.na(datos_con_tipo_de_varibles$A14))/30540

datos_con_tipo_de_varibles$Escolari <- as.integer(datos_con_tipo_de_varibles$Escolari)
datos_con_tipo_de_varibles$Escolari[datos_con_tipo_de_varibles$Escolari == 99] <- NA

# Años de escolaridad
# Tasa de respuesta: 100%
sum(!is.na(datos_con_tipo_de_varibles$Escolari))/30540

datos_con_tipo_de_varibles$A4 <- as_factor(datos_con_tipo_de_varibles$A4)
datos_con_tipo_de_varibles$LugNac <- as_factor(datos_con_tipo_de_varibles$LugNac)
datos_con_tipo_de_varibles$A15A <- as_factor(datos_con_tipo_de_varibles$A15A)
datos_con_tipo_de_varibles$A15B <- as_factor(datos_con_tipo_de_varibles$A15B)
datos_con_tipo_de_varibles$A17 <- as_factor(datos_con_tipo_de_varibles$A17)
datos_con_tipo_de_varibles$A18A <- as_factor(datos_con_tipo_de_varibles$A18A)
datos_con_tipo_de_varibles$A16B <- as_factor(datos_con_tipo_de_varibles$A16B)
datos_con_tipo_de_varibles$REZ_ESC <- as_factor(datos_con_tipo_de_varibles$REZ_ESC)
datos_con_tipo_de_varibles$ForReg <- as_factor(datos_con_tipo_de_varibles$ForReg)
datos_con_tipo_de_varibles$A18A <- as_factor(datos_con_tipo_de_varibles$A18A)
datos_con_tipo_de_varibles$A18B <- as_factor(datos_con_tipo_de_varibles$A18B)
datos_con_tipo_de_varibles$A18C <- as_factor(datos_con_tipo_de_varibles$A18C)
datos_con_tipo_de_varibles$A18C <- as_factor(datos_con_tipo_de_varibles$A18C)
datos_con_tipo_de_varibles$A20A <- as_factor(datos_con_tipo_de_varibles$A20A)
datos_con_tipo_de_varibles$A20A <- as_factor(datos_con_tipo_de_varibles$A20A)
datos_con_tipo_de_varibles$ForNoReg <- as_factor(datos_con_tipo_de_varibles$ForNoReg)
datos_con_tipo_de_varibles$A21 <- as_factor(datos_con_tipo_de_varibles$A21)
datos_con_tipo_de_varibles$A22A <- as_factor(datos_con_tipo_de_varibles$A22A)
datos_con_tipo_de_varibles$A22B <- as_factor(datos_con_tipo_de_varibles$A22B)
# Esta variable corresponde a los títulos o grado acádemico que tiene la unidad de estudio

variables_utiles <- datos_con_tipo_de_varibles %>% select(A4,
                                                          A5,
                                                          LugNac,
                                                          A13,
                                                          A14,
                                                          A15A,
                                                          A15B,
                                                          A16B,
                                                          A17,
                                                          A18A,
                                                          NivInst,
                                                          Escolari,
                                                          REZ_ESC,
                                                          ForReg,
                                                          A18A,
                                                          A18B,
                                                          A18C,
                                                          A20A,
                                                          ForNoReg,
                                                          A21,
                                                          A22A,
                                                          A22B,
                                                          )