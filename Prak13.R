#100 data simetris
set.seed(10)
data.awal<-rnorm(98,mean=2, sd=5)

min(data.awal)
max(data.awal)
data.100.sims<-c(data.awal, 100,102)

#atau
pecl<- rnorm(2, mean=60, sd=1)
data.100.sims<-c(data.awal,pecl)

boxplot(data.100.sims)

#100 tak simetris
set.seed(1)
dt.awal<-rlnorm(98, meanlog = 2, sdlog = 4)
dt.pencl<-rlnorm(2, meanlog=30, sdlog=1)
data.100.ts<-c(dt.awal, dt.pencl)
boxplot(data.100.ts)

df.100<-data.frame(data.100.sims,data.100.ts)


#--------------------------------
#10 data simetris
set.seed(10)
data.awal.10<-rnorm(8,mean=2, sd=5)

min(data.awal.10)
max(data.awal.10)

data.10.sims<-c(data.awal.10, 100,102)
boxplot(data.10.sims)

#10 tak simetris
set.seed(1)
dt.awal.10<-rlnorm(8, meanlog = 2, sdlog = 4)
dt.pencl<-rlnorm(2, meanlog=30, sdlog=1)
data.10.ts<-c(dt.awal.10, dt.pencl)
boxplot(data.10.ts)

df.10<-data.frame(data.10.sims,data.10.ts)

library(openxlsx)
dataset_names <- list('Sheet1' = df.100, 'Sheet2' = df.10)
write.xlsx(dataset_names, file = 'Datagabung.xlsx')


