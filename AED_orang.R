library(readxl)
library(dplyr)
library(ggplot2)

Sheet1 <- read_excel("Data untuk Eksplorasi.xlsx", skip=1)
Sheet3 <- read_excel("Data untuk Eksplorasi.xlsx", 
                     sheet="country code", skip=2)
colnames(Sheet3)[2]<-c("Country");Sheet3


Sheet13 <- merge(Sheet3, Sheet1, by="Country")
Sh13<-Sheet13[,c(-3:-5,-8:-11)]
is.numeric(Sh13$HDI)

sh13<-Sh13%>%filter(region=="Asia")
Sh13 <- Sh13%>%arrange(region,`sub-region`);Sh13
p <- as.factor(Sh13$`sub-region`);p
Sh13$`sub-region`<-factor(p, levels=c("Central Asia", "Eastern Asia", "Southern Asia",
                                     "South-eastern Asia", "Western Asia",
                                     "Northern Africa", "Sub-Saharan Africa",
                                     "Eastern Europe","Northern Europe","Southern Europe",
                                     "Western Europe","Latin America and the Caribbean", 
                                     "Northern America", "Australia and New Zealand"))
ggplot(Sh13, aes(as.numeric(HDI), fill=`sub-region`))+
  geom_histogram(binwidth=5, color="black", alpha=0.6)+
  scale_y_continuous(labels=scales::percent_format(accuracy=1L, scale=10))+
  facet_wrap(~`region`, ncol=2)+labs(x="Indeks Pembangunan Manusia (IPM)", y="Persentase (%)", 
                                     fill="Sub-Wilayah", title="Perbandingan Indeks Pembangunan Manusia", 
                                     subtitle="Berdasarkan Sub-Wilayah pada Tiap Benua")
