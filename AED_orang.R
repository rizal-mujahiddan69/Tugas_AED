library(readxl)
library(dplyr)
library(ggplot2)
library(qqplotr)

metadata <- read_xlsx("Data untuk Eksplorasi.xlsx",
                      sheet="penjelasan peubah",range="B2:F38",
                      col_names=FALSE)
colnames(metadata) <- c("var","nama","arti","sistem","sumber data")

Sheet1 <- read_excel("Data untuk Eksplorasi.xlsx", skip=1)
Sheet3 <- read_excel("Data untuk Eksplorasi.xlsx", 
                     sheet="country code", skip=2)


colnames(Sheet3)[2]<-c("Country");Sheet3
Sheet13 <- merge(Sheet3, Sheet1, by="Country");Sh13<-Sheet13[,c(-3:-5,-8:-11)]

s13<-S13%>%filter(region=="Asia")
S13 <- Sh13%>%arrange(region,`sub-region`);S13
p<-as.factor(S13$`sub-region`);p
S13$HDI <- as.numeric(S13$HDI)
S13$`sub-region`<-factor(p, levels=c("Central Asia", "Eastern Asia", "Southern Asia",
                                     "South-eastern Asia", "Western Asia",
                                     "Northern Africa", "Sub-Saharan Africa",
                                     "Eastern Europe","Northern Europe","Southern Europe",
                                     "Western Europe","Latin America and the Caribbean", 
                                     "Northern America", "Australia and New Zealand"))

#S13$region <- replace(S13$region,S13$region=="Oceania","Asia-Oceania")
#S13$region <- replace(S13$region,S13$region=="Asia","Asia-Oceania")

#HDI
ggplot(S13, aes(as.numeric(HDI), fill=`sub-region`))+
  geom_histogram(binwidth=5, color="black", alpha=0.6)+
  scale_y_continuous(labels=scales::percent_format(accuracy=1L, scale=10))+
  facet_wrap(~`region`, ncol=2)+labs(x="Indeks Pembangunan Manusia (IPM)", y="Persentase (%)", 
                                     fill="Sub-Wilayah", title="Perbandingan Indeks Pembangunan Manusia", 
                                     subtitle="Berdasarkan Sub-Wilayah pada Tiap Benua")

dev.off()

#ggplot(S13, aes(`GDP per cap. (USD)`, fill=`sub-region`))+
#  geom_histogram(binwidth=5, color="black", alpha=0.6)+
#  scale_y_continuous(labels=scales::percent_format(accuracy=1L, scale=10))+
#  facet_wrap(~`region`, ncol=2)+labs(x="Indeks Pembangunan Manusia (IPM)", y="Persentase (%)", 
#                                     fill="Sub-Wilayah", title="Perbandingan Indeks Pembangunan Manusia", 
#                                     subtitle="Berdasarkan Sub-Wilayah pada Tiap Benua")

# GDP
ggplot(S13, mapping=aes(x=`GDP per cap. (USD)`, fill=`sub-region`))+
  geom_boxplot()+facet_wrap(~region, ncol=2)

#Consumer
ggplot(S13,aes(y=`Consumer prices (annual avg. % growth)`,fill=`sub-region`)) + 
  geom_boxplot() + facet_wrap(~`region`,ncol=2)+coord_flip()

# Loan
ggplot(S13,aes(x=`Loan-deposit ratio (%)`,fill=`sub-region`)) + 
  geom_histogram(color="black") +facet_wrap(~`region`)
  #+ theme(panel.border = element_rect(color="red",fill = NA)) 


ggplot(S13,aes(sample=`HDI`)) + geom_qq(distribution=qnorm) + 
  geom_qq_line(col="blue",distribution = qnorm) + 
  labs(x="theorectical",y="Sample",
       title = "QQplot HDI in Gaussian Distribtion")

ggplot(S13,aes(sample=`HDI`)) + 
  geom_qq(dparams=list(shape1=1.5,shape2=1),distribution=qbeta) + 
  geom_qq_line(dparams=list(shape1=1.5,shape2=1),
               col="blue",distribution = qbeta) + 
  labs(x="theorectical",y="Sample",
       title = "QQplot HDI in Beta Distribtion")

## wkwkwkwkkw


ggplot(S13, aes(sample = `Loan-deposit ratio (% avg 5yr)`)) + 
  stat_qq_point()+stat_qq_line(col="red")+
  labs(x="theorectical",y="Sample",
       title = "QQplot Loan-deposit ratio (% avg 5yr) in Gaussian Distribtion")
  #+stat_qq_band()


ggplot(S13, aes(sample = `Loan-deposit ratio (% avg 5yr)`)) + 
  stat_qq_point(distribution="lnorm")+  
  stat_qq_line(col="red",distribution = "lnorm")+
  labs(x="theorectical",y="Sample",
       title = "QQplot Loan-deposit ratio (% avg 5yr) in lognormal Distribtion")
  #+stat_qq_band(distribution = "chisq",dparams = list(df=10))

ggplot(S13, aes(sample = `Loan-deposit ratio (% avg 5yr)`)) + 
  stat_qq_point(distribution="chisq")+  
  stat_qq_line(col="red",distribution = "chisq")+
  labs(x="theorectical",y="Sample", 
       title = "QQplot Loan-deposit ratio (% avg 5yr) in Chisquare Distribtion")
  #+  stat_qq_band(distribution = "beta",
                #  dparams = list(shape1=3,shape2=300))

ggplot(S13, aes(sample = `Loan-deposit ratio (% avg 5yr)`)) + 
  stat_qq_point(distribution="exp")+
  stat_qq_line(col="red",distribution="exp")+
  labs(x="theorectical",y="Sample",
       title = "QQplot Loan-deposit ratio (% avg 5yr) in exponensial Distribtion")
  
ggplot(S13, aes(sample = `Loan-deposit ratio (% avg 5yr)`)) + 
  stat_qq_point() + stat_qq_line(col="red") + 
  facet_wrap(~`region`)+
  labs(title = "QQplot Loan-deposit ratio (% avg 5yr) by region")

# data Real GDP growth (%)
ggplot(S13, aes(sample = `Real GDP growth ( avg last 5yrs%)`)) + 
  stat_qq_point()+stat_qq_line(col="red")+
  labs(x="theorectical",y="Sample",
       title = "QQplot Real GDP growth ( avg last 5yrs%) in Gaussian Distribtion")


ggplot(S13, aes(sample = `Real GDP growth ( avg last 5yrs%)`)) + 
  stat_qq_point(distribution = "chisq",
                dparams = list(df = mean(S13$`Real GDP growth ( avg last 5yrs%)`)))+
  stat_qq_line(distribution = "chisq",col="red",
               dparams = list(df = mean(S13$`Real GDP growth ( avg last 5yrs%)`)))+
  labs(x="theorectical",y="Sample",
       title = "QQplot Real GDP growth ( avg last 5yrs%) in Chisquare Distribtion")



ggplot(S13, aes(sample = `Real GDP growth ( avg last 5yrs%)`)) + 
  stat_qq_point(distribution="lnorm",
                dparams=list(meanlog=mean(log(S13$`Real GDP growth ( avg last 5yrs%)`)),
                             sdlog = sd(log(S13$`Real GDP growth ( avg last 5yrs%)`))))+  
  labs(x="theorectical",y="Sample",
       title = "QQplot Real GDP growth ( avg last 5yrs%) in lognormal Distribtion")


ggplot(S13, aes(sample = `Real GDP growth ( avg last 5yrs%)`)) + 
  stat_qq_point(distribution="exp",
                dparams=list(rate=1/mean(S13$`Real GDP growth ( avg last 5yrs%)`)))+  
  stat_qq_line(col="red",distribution = "exp",
               dparams = list(rate=1/mean(S13$`Real GDP growth ( avg last 5yrs%)`)))+
  labs(x="theorectical",y="Sample",
       title = "QQplot Real GDP growth ( avg last 5yrs%) in exponential Distribtion")

ggplot(S13, aes(sample = `Real GDP growth ( avg last 5yrs%)`)) + 
  stat_qq_point() + stat_qq_line(col="red") + 
  facet_wrap(~`region`)+
  labs(title = "QQplot Real GDP growth ( avg last 5yrs%) by region")


ggplot(S13,aes(x=`Real GDP growth ( avg last 5yrs%)`,
               y=`Loan-deposit ratio (% avg 5yr)`)) + geom_point()

ggplot(S13,aes(x=`Real GDP growth ( avg last 5yrs%)`,
               y=log(`Loan-deposit ratio (% avg 5yr)`))) + geom_point()
cor(x=S13$`Loan-deposit ratio (% avg 5yr)`,
    y=S13$`Real GDP growth ( avg last 5yrs%)`,
    use="na.or.complete")

cor(x=log(S13$`Loan-deposit ratio (% avg 5yr)`),
    y=(S13$`Real GDP growth ( avg last 5yrs%)`),
    use="na.or.complete")

