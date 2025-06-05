library(netdiffuseR)

KFP <- kfamilyDiffNet

summary(kfamilyDiffNet) # Diffnet object

str(KFP)

KFP$toa # P: Quiero valores mínimos y máximos, y el número de casos
min_toa <- min(KFP$toa, na.rm = TRUE)
max_toa <- max(KFP$toa, na.rm = TRUE)
num_cases_toa <- sum(!is.na(KFP$toa))
cat("Mínimo:", min_toa, "Máximo:", max_toa, "Número de casos:", num_cases_toa, "\n")

KFP$vertex.static.attrs$id7

KFP$vertex.static.attrs$pill1 #awareness of pill P: Quiero un gráfico de barras del número de individuos segun cada nivel de awareness
barplot(table(KFP$vertex.static.attrs$pill1), main = "Awareness of Pill", xlab = "Levels", ylab = "Number of Individuals")

KFP$vertex.static.attrs$pill2 #detailed knowledge of pill P: Quiero un gráfico de barras del número de individuos segun cada nivel de knowledge
barplot(table(KFP$vertex.static.attrs$pill2), main = "Detailed Knowledge of Pill", xlab = "Levels", ylab = "Number of Individuals")

KFP$vertex.static.attrs$cond1 #awareness of cond P: Quiero un gráfico de barras del número de individuos segun cada nivel de awareness
barplot(table(KFP$vertex.static.attrs$cond1), main = "Awareness of Cond", xlab = "Levels", ylab = "Number of Individuals")

KFP$vertex.static.attrs$cond2 #detailed knowledge of cond P: Quiero un gráfico de barras del número de individuos segun cada nivel de knowledge
barplot(table(KFP$vertex.static.attrs$cond2), main = "Detailed Knowledge of Cond", xlab = "Levels", ylab = "Number of Individuals")

sort(unique(KFP$vertex.static.attrs$fpt7)) #FP status time 1

KFP$vertex.static.attrs$byrt1 #Start of time 1 from year ???

KFP$vertex.static.attrs$awe2t1 #Methods known at Time 1

KFP$vertex.static.attrs$awe2t2 #Methods known at Time 1
# P: Quiero un gráfico de barras según número de métodos conocidos. Trata los NA como 0.
KFP$vertex.static.attrs$awe2t12 #Methods known at Time 12 [EL T=12 ES LO MISMO QUE T=1]

sum(KFP$vertex.static.attrs$awe2t2 == 0, na.rm = TRUE)
sum(is.na(KFP$vertex.static.attrs$awe2t1))
methods_known <- ifelse(is.na(KFP$vertex.static.attrs$awe2t4), 0, KFP$vertex.static.attrs$awe2t4)
barplot(table(methods_known), main = "Methods Known at Time 4", xlab = "Number of Methods", ylab = "Frequency")


KFP$vertex.static.attrs$ado #adopt times years converted to 1=63
KFP$vertex.static.attrs$ado1
KFP$vertex.static.attrs$ado2 # QUÉ ES ESTO??
# No se especificó qué hacer aquí.

KFP$vertex.static.attrs$ado3

KFP$vertex.static.attrs$village # Village of residence
KFP$vertex.static.attrs$commun # Village number P: Quiero saber si $village es exactamente lo mismo que $commun
all(KFP$vertex.static.attrs$village == KFP$vertex.static.attrs$commun, na.rm = TRUE)
# village == commun

KFP$vertex.static.attrs$toa # toa [NO EXISTE]

KFP$vertex.static.attrs$study # P: Quiero saber cuáles son los distintos valores que tiene. Algo como .unique()
unique(KFP$vertex.static.attrs$study) # hay solo 3's


KFP$vertex.static.attrs$area1 # P: Quiero saber si $area1 hasta $area7 son exactamente iguales
areas <- cbind(
  KFP$vertex.static.attrs$area1,
  KFP$vertex.static.attrs$area2,
  KFP$vertex.static.attrs$area3,
  KFP$vertex.static.attrs$area4,
  KFP$vertex.static.attrs$area5,
  KFP$vertex.static.attrs$area6,
  KFP$vertex.static.attrs$area7
)
are_areas_equal <- apply(areas, 1, function(row) all(row == row[1], na.rm = TRUE))
cat("¿Son iguales $area1 hasta $area7 para todos los individuos?:", all(are_areas_equal), "\n")
# SÍ son todas iguales

###############

# Access the static attributes directly
village <- KFP$vertex.static.attrs$village
toa <- KFP$toa

# Compute the minimum 'toa' for each village
min_toa_per_village <- tapply(toa, village, min, na.rm = TRUE)

# Print the results
print(min_toa_per_village)

##############

data(kfamily) # Raw data

kfamily$village
kfamily$id
kfamily$net15 # Neighbors talk to about FP- 5 (family planning status)
kfamily$planning

kfamily <- subset(kfamily, village %in% c(2)) # 2, 10, 21

kfamily$id

kfam_diffnet <- survey_to_diffnet(
  dat      = kfamily,
  idvar    = "id",
  netvars  = c("net11", "net12", "net13", "net14", "net15"),
  toavar   = "toa",
  groupvar = "village"
)

plot_adopters(kfamily, main = "Behavior 1")

#############################

# Example plot with custom y-axis limits and ticks
plot(KFP$vertex.static.attrs$fpt7,
     ylim = c(0, 21), # Set y-axis limits from 0 to 21
     yaxt = "n")      # Suppress default y-axis ticks

# Add custom y-axis ticks for every number from 0 to 21
axis(side = 2, at = 0:21)
