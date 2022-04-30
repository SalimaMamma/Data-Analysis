
library("ade4")
library('dplyr')
library('FactoMineR')
library('factoextra')

data <- read.csv("Data.csv", header = TRUE, sep = ",")#lecture des données
data
View(data)

# enlever l'id
data = select(data , -1) 

#selectionner que les variables à au moins deux modalités
data.new = select(data , -c(10 ,11,12,13,14,15,24,25,26,27,28,29,30,31)) 


tab = summary(data.new$loyal) #Code pour avoir les occurences de chaque modalité des variables
freq= tab/113 #pour avoir les fréquences 
freq

#selectionner les variables pour afcm
data.new =select(data.new , c('gender', 'age', 'status', 'income' ,'visitNo'))

#Supprimer quelques modalités non pertinentes
data.new<-data.new[!(data.new$status==3),]
data.new <- data.new[ data.new$status !=3, , drop=FALSE]; 
#data.new$status <- factor(data.new$status); 
data.new$income[data.new$income == 4] <- 3       
data.new <- data.new[ data.new$income !=4, , drop=FALSE]; 


#data.new$income <- factor(data.new$income);

#renommer les variables

data.new$age[data.new$age == 0]<- "<20"
data.new$age[data.new$age == 1]<- "20<age<29"
data.new$age[data.new$age == 2]<- "30<age<39"
data.new$age[data.new$age == 3]<- ">40 ans"

data.new$status[data.new$status == 0] <- "étudiant"
data.new$status[data.new$status == 1]<- "indépendant"
data.new$status[data.new$status == 2] <- "employé"

data.new$gender[data.new$gender == 0] <- "Homme"
data.new$gender[data.new$gender == 1]<- "Femme"

data.new$income[data.new$income == 0] <- "salaire<25000"
data.new$income[data.new$income == 1] <- " 25000 < salaire < 50000"
data.new$income[data.new$income == 2] <- " 50000 < salaire < 100000"
data.new$income[data.new$income == 3]<- " 100000 < salaire"

data.new$visitNo[data.new$visitNo == 0] <- "Quotidienne"
data.new$visitNo[data.new$visitNo== 1] <- "Hebdomadaire"
data.new$visitNo[data.new$visitNo == 2] <- "Mensuel"
data.new$visitNo[data.new$visitNo == 3] <- "Jamais"

data.new$spendPurchase[data.new$spendPurchase == 0] <- "depense=Zero"
data.new$spendPurchase[data.new$spendPurchase == 1]<- "depense<20"
data.new$spendPurchase[data.new$spendPurchase == 2]<- "20<depense<40"
data.new$spendPurchase[data.new$spendPurchase == 3] <- "depense>40"



data.new$location[data.new$location == 0] <- "R<1KM"
data.new$location[data.new$location == 1]<- "2 et 3 Km"
data.new$location[data.new$location == 2] <- "R >3km"

#tableau disjonctif
data.disj = acm.disjonctif(data.new)
View(data.disj)

#transformer les valeurs des variables de chaines de caractère à facteur
i=0
while(i < ncol(data.new)){
  i=i+1  
  data.new[,i] = as.factor(data.new[,i])
}

#Faire l'afcm 
mca = MCA(data.new)

#corriger les inerties
vp = mca$eig
vp = vp[,1]
vp = vp [vp>1/5]
vpa = 5/4 * (vp - 1/5)
for (i in 1:length(vp)) {
  vp.carre[i] <- vp[i]^2
}
I = vpa / sum(vpa) * 100



#visualisations
fviz_mca_var(mca, repel = TRUE , col.var = "Blue" )

fviz_mca_var(mca, col.var="cos2",
             gradient.cols = c( "#ffba08","#d00000","#03071e"),
             repel = TRUE # Avoid text overlapping
             )
fviz_contrib(acm, choice ="var" , axes=1 )
fviz_contrib(acm, choice ="var" , axes=2 )
plot (data.new$visitNo ,main ="visitNo" ,col ="#0892A5")
plotellipses(mca)
fviz_mca_ind (mca,
             label = "none", # masquer le texte des individus
             habillage = "visitNo", # colorer par groupes
             addEllipses = TRUE, 
             ellipse.type = "confidence",
              ggtheme = theme_minimal ())
fviz_mca_ind (mca,
              label = "none", # masquer le texte des individus
              habillage = "visitNo", # colorer par groupes
              addEllipses = TRUE, 
              ellipse.type = "confidence",
              ggtheme = theme_minimal ())

barplot(freq , main ="loyal", col ="#0892A5")
fviz_mca_biplot (mca, repel = TRUE,
                 col.var = "Purple" , ind.var="Orange")

qplot(c(1:11), I) +
  geom_col()+
  geom_line() +
  geom_point(size=4)+
  xlab("Principal Component") +
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 50)

data.ca <- read.csv("Data.csv", header = TRUE, sep = ",")#lecture des données


data.ca = select(data.ca , c('status','visitNo'))
data.ca<-data.ca[!(data.ca$status==0),]
data.ca <- data.ca[ data.ca$status !=0, , drop=FALSE]; 

ca = CA(data.ca)

ca = CA (data.ca , col.sup = 4)




