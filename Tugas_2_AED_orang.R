#####Package#####
library(readxl)
library(ggplot2)
library(dplyr)
library(MASS)
library(rcompanion)
library(ggridges)

#####Input data#####
Data2 <- read_excel("Data Tugas 2.xlsx", sheet = "data", skip=1)
country <-  read_excel("Data Tugas 2.xlsx", sheet = "country code", skip=1)
colnames(country)[2] <- colnames(Data2)[1]
Dc<-merge(country, Data2, by="Country");DC<-Dc[,-c(3:5,8:11)]
DA <- DC%>%filter(region=="Asia", `sub-region`!="Central Asia");
DC.Asia<-DA[,-3]

####Visualisasi Awal####
#PDB per Kapita
ggplot(DC.Asia, mapping=aes(x=`GDP per cap. (USD)`, y=`sub-region`, 
        fill=`sub-region`))+geom_density_ridges(alpha=0.83)+
        xlim(0, max(DC.Asia$`GDP per cap. (USD)`))+theme_minimal()+
        labs(title="Sebaran PDB per Kapita di Benua Asia\n",
        y="Sub-Benua\n", x="\nPDB per Kapita (USD)")+
        scale_fill_brewer(palette = "PuOr")+
        theme_ridges(center_axis_labels = T, grid=T)+
        theme(legend.position = "none")

#Consumer prices
Dabat<-DC.Asia %>% mutate(baru=`Consumer prices ( avg annual avg. % growth 5yrs)`+ 
        min(DC.Asia$`Consumer prices ( avg annual avg. % growth 5yrs)`)+2)

ggplot(Dabat, mapping=aes(x=baru, y=`sub-region`, fill=`sub-region`))+
        geom_density_ridges(alpha=0.83)+theme_minimal()+
        labs(title="Sebaran Pertumbuhan Harga Konsumen di Benua Asia", 
        subtitle=" Rataan 5 Tahun Terakhir", x="\n Pertumbuhan Harga Konsumen (%)",
        y="Sub-Benua\n")+scale_fill_brewer(palette = "PuOr")+
        theme_ridges(center_axis_labels = T, grid=T)+
        theme(legend.position = "none")

#####Visualisasi Akhir#####
#PDB per Kapita-Boxcox Transformation
gdp<-DC.Asia$`GDP (USDbn)`
bc<-boxcox(gdp~1)
lambda<-bc$x[which.max(bc$y)]
gdp.bc <- ((gdp^lambda-1)/lambda)
GAB <- cbind(DC.Asia,gdp.bc)
ggplot(GAB, mapping=aes(x=gdp.bc, y=`sub-region`, fill=`sub-region`))+
        geom_density_ridges(alpha=0.83)+scale_fill_brewer(palette="PuOr")+
        xlim(0, max(GAB$gdp.bc))+labs(y="Sub-Benua\n", x="\nPDB-Boxcox (USD)", 
        title="Sebaran PDB per Kapita di Benua Asia", 
        subtitle="Post Boxcox Transformation")+
        theme_ridges(center_axis_labels = T)+
        theme(legend.position = "none")

#PDB per Kapita-Tukey Transformation
gdp.tk<-transformTukey(gdp, returnLambda = F)
Gab<-cbind(DC.Asia, gdp.tk)
ggplot(Gab, mapping=aes(x=gdp.tk, y=`sub-region`, fill=`sub-region`))+
  geom_density_ridges(alpha=0.83)+scale_fill_brewer(palette="PuOr")+
  xlim(0, max(Gab$gdp.tk))+labs(y="Sub-Benua\n", x="\nPDB-Tukey (USD)", 
  title="Sebaran PDB per Kapita di Benua Asia", 
  subtitle="Post Tukey Transformation")+theme_ridges(center_axis_labels = T)+
  theme(legend.position = "none")
