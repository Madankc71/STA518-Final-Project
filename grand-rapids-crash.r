

setwd("~/STA 518/STA Final Project/STA518-Final-Project")

#Importing datasets
total_crashes <- read_csv("total_crashes.csv")
total_crashes

alcohol_crashes <- read_csv("alcohol_crashes.csv")
alcohol_crashes

drug_crashes <- read_csv("drug_crashes.csv")
drug_crashes

alcohol_injury_death <- read_csv("alcohol_injury_death.csv")
alcohol_injury_death

drug_deaths <- read_csv("drug_injury_death.csv")
drug_deaths

#Merging datasets by year
df = merge(x = total_crashes, y = alcohol_crashes, by = "year")
df = merge(x = df, y =alcohol_injury_death , by = "year")
df = merge(x = df, y = drug_crashes, by = "year")
df = merge(x = df, y = drug_deaths, by = "year")
#dropping the row of 2020 as it has multiple NA
df1 = df[-17,]
df1

#Adding columns
#Total crashes:
df1$total_crash_sum <- rowSums(df1[,c("Ionia_total", "Kent_total","Montcalm_total", "Ottawa_total")])
df1

#Total alcohol crashes
df1$total_alcohol_crashes<- rowSums(df1[,c("Ionia_alcohol_crash", "Kent_alcohol_crash","Montcalm_alcohol_crash", "Ottawa_alcohol_crash")])
df

#Total of alcohol injuiries and death
df1$total_alcohol_injuries  <- rowSums(df1[,c("Ionia_alcohol_injury", "Kent_alcohol_injury","Montcalm_alcohol_injury", "Ottawa_alcohol_injury")])
df1

#Drug crashes
df1$total_drug_crashes  <- rowSums(df1[,c("Ionia_dcrashes", "Kent_dcrashes","Montcalm_dcrashes", "Ottawa_dcrashes")])
df1

#Drug injuries and alcohol
df1$total_drug_deaths  <- rowSums(df1[,c("Ionia_injury_deaths", "Kent_injury_deaths","Montcalm_injury_deaths", "Ottawa_injury_deaths")])
View(df1)

#Visualizations:
#1
#  1.1 Bar chart of total injuries and death due to drugs and alcohol
library(reshape2)
dfm <- melt(df1[,c('year','total_alcohol_injuries','total_drug_deaths')], id.vars = 1)
ggplot(dfm,aes(x = year, y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge")

#1.2 Bar chart of total crashes due to drugs and alcohol

library(reshape2)
dfm <- melt(df1[,c('year','total_alcohol_crashes','total_drug_crashes')], id.vars = 1)
ggplot(dfm,aes(x = year, y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge")

#2. Line chart for crashes between counties

#2.1 Line chart of crash due to alcohol

county_alcohol_crashes <- data.frame(year = df$year,                            # Reshape data frame
                                     alcrashes = c(df1$Montcalm_alcohol_crash, df1$Ottawa_alcohol_crash, df1$Ionia_alcohol_crash,df1$Kent_alcohol_crash),
                                     group = c(rep("Montcalm_alcohol_crash", nrow(df)),
                                               rep("Ottawa_alcohol_crash", nrow(df)),
                                               rep("Ionia_alcohol_crash", nrow(df)),
                                               rep("Kent_alcohol_crash", nrow(df))))


library("ggplot2")
ggp <- ggplot(county_alcohol_crashes, aes(year, alcrashes, col = group)) +             # Create ggplot2 plot
  geom_line()
ggp 

#2.2 Injuries and deaths in four counties due to alcohol

county_alcohol_injury_death <- data.frame(year = df1$year,                            # Reshape data frame
                                          alcohol_injury_and_death = c(df1$Montcalm_alcohol_injury, df1$Ottawa_alcohol_injury, df1$Ionia_alcohol_injury,df1$Kent_alcohol_injury),
                                          group = c(rep("Montcalm_alcohol_injury", nrow(df)),
                                                    rep("Ottawa_alcohol_injury", nrow(df)),
                                                    rep("Ionia_alcohol_injury", nrow(df)),
                                                    rep("Kent_alcohol_injury", nrow(df))))


library("ggplot2")
ggp <- ggplot(county_alcohol_injury_death, aes(year, alcohol_injury_and_death, col = group)) +             # Create ggplot2 plot
  geom_line()
ggp

#2.3 Crashes in four Counties due to drug

county_drug_crashes <- data.frame(year = df1$year,                            # Reshape data frame
                                  drug_crashes = c(df1$Montcalm_dcrashes, df1$Ottawa_dcrashes, df1$Ionia_dcrashes,df1$Kent_dcrashes),
                                  group = c(rep("Montcalm_dcrashes", nrow(df1)),
                                            rep("Ottawa_dcrashes", nrow(df1)),
                                            rep("Ionia_dcrashes", nrow(df1)),
                                            rep("Kent_dcrashes", nrow(df1))))


library("ggplot2")
ggp <- ggplot(county_drug_crashes, aes(year, drug_crashes, col = group)) +             # Create ggplot2 plot
  geom_line()
ggp 

#2.4 Injuries and deaths in four counties due to drugs

county_drug_injury_death <- data.frame(year = df1$year,                            # Reshape data frame
                                       alcohol_deaths_injury = c(df1$Montcalm_injury_deaths, df1$Ottawa_injury_deaths, df1$Ionia_injury_deaths,df1$Kent_injury_deaths),
                                       group = c(rep("Montcalm_injury_deaths", nrow(df1)),
                                                 rep("Ottawa_injury_deaths", nrow(df1)),
                                                 rep("Ionia_injury_deaths", nrow(df1)),
                                                 rep("Kent_injury_deaths", nrow(df1))))


library("ggplot2")
ggp <- ggplot(county_drug_injury_death, aes(year, alcohol_deaths_injury, col = group)) +             # Create ggplot2 plot
  geom_line()
ggp

#3. Pie chart
#3.1 Proportion of crashes due to Drugs, alcohol and other factors

df1$Other_Crashes <- df1$total_crash_sum - df1$total_alcohol_crashes - df1$total_drug_crashes #Crashes due to factors other than alcohol and drugs
df_crash <- data.frame(
  Types = c("alcohol", "drug", "others"),
  value=c("total_alcohol_crashes","total_drug_crashes","Other_Crashes")
)
ggplot(df_crash, aes(x="", y=value, fill=Types)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()

