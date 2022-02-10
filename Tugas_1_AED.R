library(readxl)
library(ggplot2)
library(visdat)
library(missForest)
library(tidyverse)

data_exp <- read_xlsx("Data untuk Eksplorasi.xlsx",
                      sheet="data",range = "A2:AL119")
metadata <- read_xlsx("Data untuk Eksplorasi.xlsx",
                      sheet="penjelasan peubah",range="B2:F38")
country_code <- read_xlsx("Data untuk Eksplorasi.xlsx",
                          sheet="country code",range = "A3:K252")

names(data_exp)
names(country_code)

data_exp_merge <- merge(x=data_exp,y=country_code,
                        by.x="Country",by.y = "alpha-2")

data_exp_merge$HDI <- suppressWarnings(as.numeric(data_exp_merge$HDI))

# cek jika countrynya lebih dari satu
# freq_data_exp <- table(data_exp$Country)
# print(freq_data_exp[freq_data_exp>1])

# cek kembali
# freq_data_exp <- table(data_exp$Country)
# print(freq_data_exp[freq_data_exp>1])

mis_val <- vis_miss(data_exp_merge) + 
            theme(axis.text.x = element_text(angle = 90))
# gambar yah

jpeg("missing_value_Tugas_1.jpg",width=1920,height=1080,quality = 100)
mis_val
dev.off()

# check menyeleksi colomn yang 
# missing valuenya lebih dari 10%

kolom_NA <- colSums(is.na(data_exp_merge))
kolom_NA <- kolom_NA[kolom_NA > 0]
pers_kolom_NA <- (kolom_NA / (dim(data_exp_merge)[1])) * 100
kolom_hapus <- names(pers_kolom_NA[pers_kolom_NA>10])
kolom_NA <- kolom_NA[!(names(kolom_NA) %in% kolom_hapus)]
data_exp_merge <- data_exp_merge[!(names(data_exp_merge) %in% kolom_hapus)]

print(paste("karena persentase NA lebih dari 10% maka kolom",
            kolom_hapus,"dihapus"))

vis_miss(data_exp_merge) + theme(axis.text.x = element_text(angle = 85))

data_exp_imp_dtype <- sapply(data_exp_merge[c(names(kolom_NA))] , class)
data_exp_imp_num <- names(data_exp_imp_dtype[data_exp_imp_dtype == "numeric"])
data_exp_imp_chr <- names(data_exp_imp_dtype[data_exp_imp_dtype != "numeric"])



jpeg("Histogram_pada_kolom_missing_value.jpg",width=1920,height =1080,quality=100)
par(mfrow=c(2,9))


num_hist <- 0

for(i in data_exp_imp_num){
  
  assign(paste("num_hist",num_hist,sep=''),ggplot(data_exp_merge,aes(.data[[i]])) + stat_bin())
  #hist(data_exp_merge[[i]],xlab=i,main=paste("Hist",i))
  eval(paste("num_hist",num_hist,sep=''))
  num_hist <- num_hist + 1
  
}
dev.off()

eval(paste("num_hist",0,sep=''))

# disini saya imputasi dengan median numeric
data_exp<- data_exp %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))
# disini saya imputasi dengan modus
data_exp<- data_exp %>% 
  mutate_if(is.character, function(x) ifelse(is.na(x), mode(x), x))

#vis_miss(data_exp[c(names(kolom_NA))]) + theme(axis.text.x = element_text(angle = 85))

