# Deklarasi Data Frame
df <- data.frame(
  student = c("Ami", "Budi", "Caca", "Doni", "Edo",
              "Fani", "Gita", "Hamim", "Ian", "Jacob"),
  sex = c("F", "M", "F", "M", "M", "F", "F", "M", "M", "M"),
  semester = c(2, 2, 6, 2, 3, 6, 7, 2, 4, 6),
  household.income = c(15888, 12790, 4912, 48242, 237505,
                       5561, 37183, 51135, 43616, 13877),
  final.score = c("88", "85", "85", "83", "83", "74", "74", "76", "76", "84")
)

head(df, 3)
tail(df)

head(df, 3)
tail(df, 8)

# Pemotongan Data Frame
df$household.income  # keluaran vector
df[['household.income']]

df['household.income']  # keluaran data.frame
df[c('student', 'household.income')]

df[3, ]  # ambil per baris
df[c(3, 5), ]

# Perubahan Tipe Data
df$final.score; class(df$final.score)
df['final.score'] <- as.numeric(df[['final.score']])  # harus input vector
df$final.score; class(df$final.score)

df$sex; class(df$sex)
df['sex'] <- factor(df[['sex']], levels = c("F", "M", "NB"),
                    labels = c("Female", "Male", "Non-Binary"),
                    ordered = F)
df$sex; class(df$sex)

df$semester; class(df$semester)
df['semester'] <- factor(df$semester, levels = 1:14, ordered = T)
df$semester; class(df$semester)

# Struktur Data
str(df)

# Ringkasan Data
summary(df)

# Fungsi Apply
length(df)
sapply(df, length)

sapply(df[c('household.income', 'final.score')], sum)
sapply(df[c('household.income', 'final.score')], mean)
sapply(df[c('household.income', 'final.score')], sd)

levels(df$sex)
levels(df$semester)

sapply(df[c('sex', 'semester')], levels)

# Contoh Akhir
q.df <- sapply(df[c('household.income', 'final.score')], quantile,
               probs= c(.25, .75))
q.df
q.df["75%", ] - q.df["25%", ]  # to find IQR
