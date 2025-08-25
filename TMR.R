###############################################
# Análisis del Tiempo Mínimo de Residencia (TMR) y visualización de datos a 2 escalas
# Autora: Maguali Burni
# Fecha: noviembre de 2022
###############################################

# ---- Paquetes ----
library(car)
library(dplyr)
library(ggplot2)
library(patchwork)
library(svglite)

# ---- Escala: Provincia de Córdoba ----
cordoba <- read.csv("TR LOCAL.csv", 
                  header = T, 
                  sep = ",")

cordoba <- cordoba[,c(1,7,10)]            #selección de variables relevantes para el análisis
cordoba$tr <- 2022-cordoba$Año.registro   #cálculo de tiempo mínimo de residencia (TMR)
cordoba <- na.omit(cordoba)               #eliminación de NAs

head(cordoba)


# Modelos GLM con distribucion Gamma 
cordoba$Status.invasor <- factor(cordoba$Status.invasor,
                                 levels= c('No invasora', 'Invasora', 'Nativa'))   #orden de los niveles del factor status

fit_cordoba <- glm(tr ~ Status.invasor , data = cordoba, family= Gamma (link = 'log'))

Anova(fit_cordoba, Type='II')
summary(fit_cordoba)

# Diagnóstico del modelo
par(mfrow=c(2,2))
plot(fit_cordoba)
par(mfrow=c(1,1))
plot(cordoba$tr, fit_cordoba$residuals)


# ---- Escala centro_este de Argentina ----
centro_este <- read.csv("TR REGIONAL.csv", 
                        header = T, 
                        sep = ",")

centro_este <- centro_este[,c(1,7,10)]              #Selección de variables relevantes para el análisis (especie, año de registro y status invasor)
centro_este$tr <- 2022-centro_este$Año.registro     #Cálculo de TMR
centro_este <- na.omit(centro_este)                 #eliminación de NAs

head(centro_este)
summary(centro_este)

# Modelos GLM con distribucion Gamma 
centro_este$Status.invasor <- factor(centro_este$Status.invasor, 
                                     levels= c('No invasora', 'Invasora', 'Nativa'))   #orden de los niveles del factor status

fit_centro_este <- glm(tr ~ Status.invasor , data = centro_este, family= Gamma (link = 'log'))

Anova(fit_centro_este, Type='II')
summary(fit_centro_este)

# Diagnóstico del modelo
par(mfrow=c(2,2))
plot(fit_centro_este)
par(mfrow=c(1,1))


# ---- Medidas de Resumen ----
by(cordoba$tr, cordoba$Status.invasor, mean)
by(cordoba$tr, cordoba$Status.invasor, sd)

by(centro_este$tr, centro_este$Status.invasor, mean)
by(centro_este$tr, centro_este$Status.invasor, sd)


# ---- Visualización ----
#Escala Córdoba
cordoba <- mutate(cordoba,                                                   #Modificacion de nombres de los niveles del factor status traduciodos a inglés
                  Status.invasor = case_when(
                    Status.invasor == 'Invasora' ~ 'invasive',
                    Status.invasor == 'No invasora' ~ 'non invasive',
                    Status.invasor == 'Nativa' ~ 'native'))

cordoba$Status.invasor <- factor(cordoba$Status.invasor,
                                 levels= c('invasive', 'non invasive', 'native'))   #orden de los niveles del factor para las visualizaciones


# Letras para significancia
df <- data.frame(
  x = c('invasive', 'non invasive', 'native'),
  y = c(160, 158, 168),
  text = c('b','c','a'))


gg_cordoba <- ggplot(cordoba, aes(y=tr, x=Status.invasor)) +
  geom_boxplot(fatten=1.5, width=0.5, aes(fill= Status.invasor)) +
  scale_colour_grey(start = 0.5, end = 1, aesthetics = 'fill')+
  stat_summary(fun =mean, geom="point", shape=15, size=2, color="black", fill="black") +
  labs(x='', y= 'MRT') +
  ggtitle('a') +
  ylim(0, 170)+
  geom_text(data = df, aes(x,y, label = text), 
            position = position_nudge(y = 0.5),
            family = 'sans', size = 3.5,
            fontface = "plain") +
  theme_bw() + theme(legend.position = 'none') +
  theme(axis.text.x = element_text(size = 10, color='black'),
        axis.text.y = element_text(color = 'black'))


#Escala centro-este de Argentina
centro_este <- mutate(centro_este, 
                      Status.invasor = case_when(
                        Status.invasor == 'Invasora' ~ 'invasive',                  #Modificacion de nombres de los niveles del factor status traduciodos a inglés
                        Status.invasor == 'No invasora' ~ 'non invasive',
                        Status.invasor == 'Nativa' ~ 'native'))

centro_este$Status.invasor <- factor(centro_este$Status.invasor,                        #Orden de los niveles del factor status para las visualizaciones
                                     levels= c('invasive', 'non invasive', 'native'))

# Letras para significancia
df <- data.frame(
  x = c('invasive', 'non invasive', 'native'),
  y = c(160, 158, 168),
  text = c('b','c','a'))

gg_centro_este <- ggplot(centro_este, aes(y=tr, x=Status.invasor)) +
  geom_boxplot(fatten=1.5, width=0.5, aes(fill= Status.invasor)) +
  scale_colour_grey(start = 0.5, end = 1, aesthetics = 'fill') +
  stat_summary(fun =mean, geom="point", shape=15, size=2, color="black", fill="black") +
  labs(x='', y= 'MRT') +
  ggtitle('b') +
  ylim(0, 170) +
  geom_text(data = df, aes(x,y, label = text), 
            position = position_nudge(y = 0.5),
            family = 'sans', size = 3.5,
            fontface = "plain") +
  theme_bw() + theme(legend.position = 'none')+
  theme(axis.text.x = element_text(size = 10, color = 'black'),
        axis.text.y = element_text(color = 'black'))


# Gráficos combinados
gg_cordoba + gg_centro_este


# ---- Exportación de Figuras ----
ggsave('Fig2a.svg', gg_cordoba, width = 8.4, height = 6.5, units = 'cm', dpi = 600)
ggsave('Fig2b.svg', gg_centro_este, width = 8.4, height = 6.5, units = 'cm', dpi = 600)

ggsave('cordoba_boxplot.png', gg_cordoba, width = 10, height = 8, units = 'cm')
ggsave('centro_este_boxplot.png', gg_centro_este, width = 10, height = 8, units = 'cm')

ggsave(filename = "Fig2a.tiff", plot = gg_cordoba, device = "tiff", dpi = 600, width = 84, height = 84, units = "mm") 
ggsave(filename = "Fig2b.tiff", plot = gg_centro_este, device = "tiff", dpi = 600, width = 84, height = 84, units = "mm") 
