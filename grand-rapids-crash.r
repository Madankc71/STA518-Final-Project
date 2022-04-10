
setwd("~/STA 518/STA Final Project/STA518-Final-Project")

library(readr)
total_crashes <- read_csv("total_crashes.csv")
View(total_crashes)




library(readr)
alcohol_crashes <- read_csv("alcohol_crashes.csv")
View(alcohol_crashes)


library(readr)
drug_crashes <- read_csv("drug_crashes.csv")
View(drug_crashes)




library(readr)
alcohol_injury_death <- read_csv("alcohol_injury_death.csv")
View(alcohol_injury_death)


library(readr)
drug_deaths <- read_csv("drug_injury_death.csv")
View(drug_deaths)

#Merging datasets by year

df = merge(x = total_crashes, y = alcohol_crashes, by = "year")
df = merge(x = df, y =alcohol_injury_death , by = "year")
df = merge(x = df, y = drug_crashes, by = "year")
df = merge(x = df, y = drug_deaths, by = "year")
View(df)

#dropping the row of 2020 as it has multiple NA
df = df[-17,]
View(df)

#Adding columns
#Total crashes:

df$total_crash_sum <- rowSums(df[,c("Ionia_total", "Kent_total","Montcalm_total", "Ottawa_total")])
View(df)

#Total alcohol crashes

df$total_alcohol_crashes<- rowSums(df[,c("Ionia_alcohol_crash", "Kent_alcohol_crash","Montcalm_alcohol_crash", "Ottawa_alcohol_crash")])
View(df)

#Total of alcohol injuiries and death
df$total_alcohol_injuries  <- rowSums(df[,c("Ionia_alcohol_injury", "Kent_alcohol_injury","Montcalm_alcohol_injury", "Ottawa_alcohol_injury")])
View(df)

#Drug crashes
df$total_drug_crashes  <- rowSums(df[,c("Ionia_dcrashes", "Kent_dcrashes","Montcalm_dcrashes", "Ottawa_dcrashes")])
View(df)

#Drug injuries and alcohol
df$total_drug_deaths  <- rowSums(df[,c("Ionia_injury_deaths", "Kent_injury_deaths","Montcalm_injury_deaths", "Ottawa_injury_deaths")])
View(df)

#Visualizations:
 # 1. Bar chart of total injuries and death due to drugs and alcohollibrary(reshape2)
library(reshape2)
dfm <- melt(df[,c('year','total_alcohol_injuries','total_drug_deaths')], id.vars = 1)
ggplot(dfm,aes(x = year, y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge")

#2. Line chart for crashes between counties

line_chart <- data.frame(year = df$year,                            # Reshape data frame
                       alcohol_crashess = c(df$Montcalm_alcohol_crash, df$Ottawa_alcohol_crash, df$Ionia_alcohol_crash, df$Kent_alcohol_crash),
                       group = c(rep("Montcalm_alcohol_crash", nrow(df)),
                                 rep("Ottawa_alcohol_crash", nrow(df)),
                                 rep("Ionia_alcohol_crash", nrow(df)),
                                 rep("Kent_alcohol_crash", nrow(df))))


library("ggplot2")
ggp <- ggplot(line_chart, aes(year, alcohol_crashess, col = group)) +             # Create ggplot2 plot
  geom_line()
ggp  

#Pie chart of crashes due to drugs, alcohol and other factors
#Crashes due to factors other than alcohol and drugs

df$Other_Crashes <- df$total_crash_sum - df$total_alcohol_crash - df$total_drug_crash
View(df)
df <- data.frame(
  Types = c("alcohol", "drug", "others"),
  value=c("total_alcohol_crashes","total_drug_crashes","Other_Crashes")
)
ggplot(df, aes(x="", y=value, fill=Types)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()

