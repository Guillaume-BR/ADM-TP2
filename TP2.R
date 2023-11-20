#1)a) Chargement du dataframe
villesbrut = read.table("~/ADM/ADM-TP2/villes.csv", header = TRUE)

#1)b) Standardisation
villes = scale(villesbrut[2:55])*sqrt(101/102)

#2)a) Matrice des distances euclidiennes
dv=dist(villes, method="euclidean")
as.matrix(dv)

#2)b) CAH avec Ward
CAHV = hclust(d = dv, method = "ward.D")

#2)c) Dendrogramme de la hiérarchie indicée:
plot(CAHV)

#2)d) Coupure de l'arbre 
PV2 = cutree(tree = CAHV, k=4)
print(PV2)

#2)e) Calcul du R2 des variables avec la variable de classe
R2_PV2 = cbind(rep(0 , ncol(villes)))
for (i in cbind(1:ncol(villes))) {R2_PV2[i] =
  summary(lm(villes[,i]~as.factor(PV2)))$r.squared}

#renomme les lignes
row.names(R2_PV2) = colnames(villes)

#2)f)R2 de la partition 
R2G_PV2 = mean(R2_PV2)               

#2)gplot conditionnel
boxplot(villes[,5]~as.factor(PV2))

#2)h)
IC2Vil = data.frame(model.matrix(~as.factor(PV2)-1))
mIC2Vil = as.matrix(IC2Vil)
mVil = as.matrix(villes)
CentresC2 = solve(t(mIC2Vil) %*% mIC2Vil) %*% t(mIC2Vil) %*% mVil

KMV2 = kmeans(villes, CentresC2)

KMV2$cluster

