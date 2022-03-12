library(ggplot2)
df <- midwest
help(midwest)  # mengandung informasi dari built-in dataset yg diperlukan
head(df)

#### Base ####
boxplot(df$percchildbelowpovert)

# merubah judul plot dan warna boxplot
boxplot(df$percchildbelowpovert, horizontal = T,
        main = "% of Child Below\nThe Poverty Line in The Midwest", col = "red")

#### Base: Grouping ####
# pengelompokan dgn x = kategorik dan y = numerik
boxplot(percchildbelowpovert ~ state, data = midwest)
boxplot(percchildbelowpovert ~ inmetro, data = midwest)

# parameter xlim/ylim untuk mengubah batasan skala
boxplot(percchildbelowpovert ~ inmetro, data = midwest, ylim = c(0, 100))

#### Base: Group Coloring ####
# masukkan vektor ke dalam parameter col untuk menggunakan banyak warna
boxplot(percchildbelowpovert ~ inmetro, data = midwest, ylim = c(0, 100),
        col = c("red", "blue"))

#### Base: Axis Modification ####
# xaxt untuk mengubah tipe sumbu-x, xaxt = 'n' untuk menghilangkan isi sumbu
boxplot(percchildbelowpovert ~ inmetro, data = midwest, ylim = c(0, 100),
        col = c("red", "blue"), xaxt = "n")

# untuk memodifikasi salah satu sumbu
axis(1, at = 1:2, labels = c("No", "Yes"))

#### Base: Category Sorting ####
boxplot(percchildbelowpovert ~ state, data = df)
# reorder() untuk mengurutkan suatu kategori berdasarkan suatu statistiknya
# dalam kasus ini diurutkan berdasarkan mediannya
boxplot(df$percchildbelowpovert ~
                reorder(df$state, df$percchildbelowpovert, FUN = median))

# reorder() dapat disimpan ke dalam suatu variabel terlebih dahulu
state.reord <- reorder(df$state, df$percchildbelowpovert, FUN = median)
# sehingga kode bisa menjdai lebih rapih
boxplot(df$percchildbelowpovert ~ state.reord)

# membalikkan urutan kategori menggunakan tidyverse::fct_rev()
library(tidyverse)
boxplot(df$percchildbelowpovert ~ fct_rev(state.reord))  # reverse factor order

# penggunaan parameter data pada fungsi plot
boxplot(df$percchildbelowpovert ~ state.reord,
        col = c("darkred", "red", "white", "blue", "darkblue"))
boxplot(percchildbelowpovert ~ state.reord, data = df,
        col = c("darkred", "red", "white", "blue", "darkblue"))

# modifikasi warna, judul, dan label
boxplot(df$percchildbelowpovert ~ state.reord,
        col = c("darkblue", "blue", "white", "red", "darkred"),
        main = "Child Poverty Rate in Midwest Counties,\nGrouped by States",
        xlab = "State", ylab = "% of Child Living Under Poverty Line",
        ylim = c(0, 100))

# menambahkan garis horizontal dan teks
abline(h = 50, lty = "dashed", col = "grey")
text(x = 2, y = 64.31 + 7, labels = "Menominee, WI", cex = .725)

max(df$percchildbelowpovert)
df$county[df$percchildbelowpovert == max(df$percchildbelowpovert)]

#### GGPlot ####
library(ggplot2)
ggplot(data = df, mapping = aes(y = percchildbelowpovert)) + geom_boxplot()

# objek ggplot dapat disimpan ke dalam suatu variabel
bp.child.povr <- ggplot(data = df, mapping = aes(y = percchildbelowpovert)) +
        geom_boxplot()

# modifikasi tema di ggplot
bp.child.povr + theme_light()
bp.child.povr + theme_bw()
bp.child.povr + theme_classic()
bp.child.povr + theme_minimal()
bp.child.povr + theme_void()  # akan berguna jika menggunakan peta choropleth

#### GGPlot: Grouping ####
bp.cp.inmet <- ggplot(
        data = df, mapping = aes(x = as.factor(inmetro),  # faktor krn kategorik
                                 y = percchildbelowpovert)
) + geom_boxplot()

bp.cp.inmet + theme_classic()

#### GGPlot: Group Coloring ####
cp.inmet.col <- ggplot(
        data = df, mapping = aes(x = as.factor(inmetro),
                                 y = percchildbelowpovert,
                                 fill = as.factor(inmetro))
)

# modifikasi warna dan transparansi
cp.inmet.col + geom_boxplot(fill = "orange") + theme_classic()
cp.inmet.col + geom_boxplot(fill = "orange", alpha = .35) + theme_classic()

# fill (warna isi) vs color (warna garis/border)
cp.inmet.col + geom_boxplot(fill = "orange", color = "brown") + theme_classic()
cp.inmet.col + geom_boxplot(fill = c("red", "blue")) + theme_classic()

#### GGPlot: Axis Modification ####
cp.inmet.col + geom_boxplot(fill = c("red", "blue")) + theme_classic()

# menukar sumbu-x dan y
cp.inmet.col + geom_boxplot(fill = c("red", "blue"))  + theme_classic() +
        coord_flip()

# modifikasi label dan ticks pada sumbu-x dan y
cp.inmet.col + geom_boxplot(fill = c("red", "blue")) + theme_classic() +
        xlab("County considered in a metro area?") +
        ylab("% of Child Living Under Poverty") +
        scale_x_discrete(breaks = c(0, 1), labels = c("No", "Yes")) +
        ylim(0, 100)


#### GGPlot: Category Sorting ####
ggplot(df, aes(x = reorder(state, percchildbelowpovert, median),
               y = percchildbelowpovert)) +
        geom_boxplot()

# state.reord telah dideklarasikan sebelumnya
cp.state <- ggplot(df, aes(x = state.reord, y = percchildbelowpovert,
                           color = state.reord))

cp.state + geom_boxplot()

# membalikkan urutan kategori
cp.state + geom_boxplot() + scale_x_discrete(limits = rev(levels(state.reord)))

# boxplot + jitter
cp.state + geom_boxplot() + geom_jitter(alpha = .5) + scale_x_discrete() +
        theme(legend.position="none")  # menghilangkan legenda
