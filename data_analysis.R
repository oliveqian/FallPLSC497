library(readxl)
library(ggplot2)
library(stargazer)
library(httr)
library(data.table)
library(reshape2)
library(reshape)
options(scipen = 999)
#init
setwd("C:/Users/pasaa/Desktop/PLSC497_project")
FDI <- read_excel("FDI.xlsx")
GDP_C <- read_excel('GDP_C.xlsx')
percent_GDP <-read_excel('research_as%of_GDP.xlsx')
setnames(FDI, "year", "Year")
rail <- read_excel('rail_length.xlsx')
jour_num <- read_excel('Journal_numbers.xlsx')
education <- read_excel('education_c.xlsx')
education_russia <- read_excel('education_c_v.xlsx')
GDP_R <- read_excel('GDP_R.xlsx')
FDI_R <- read_excel('FDI_R.xlsx')



Merged_fdi <- merge(FDI, GDP_C, by="Year")
#this is the relation 
p1 <- ggplot(Merged_fdi, aes(x = FDI, y = Merged_fdi$GDP))+
  geom_point(size =2 , color = "red")+
  geom_smooth(method = "lm")+
  labs(x = "Foreign Direct Investment", y = "GDP", title = "Relationship between FDI and GDP in China", caption = "data from World Bank Group")

merged_fdi_percent <- merge(Merged_fdi, percent_GDP,by = "Year")

#this is the graph depicting the realtionship between GDP and research expenditure as % of GDP
p2 <- ggplot(merged_fdi_percent, aes(x = merged_fdi_percent$Year))+
  geom_line(aes( y = merged_fdi_percent$`as % of GDP`), size = 1.5, colour = "blue", linetype = "solid")+
  geom_line(aes( y = merged_fdi_percent$GDP), size = 1.5, colour = "black", linetype = "solid")


#relation between research expenditure as % of Gdp and GDP
p3 <- ggplot(merged_fdi_percent, aes(x = merged_fdi_percent$`as % of GDP`, y = merged_fdi_percent$GDP))+
  geom_line()+
  geom_point(size = 1, color = "red")

#plot for the realtion between education and GDP
merge_edu_GDP <- merge(GDP_C, education, by = "Year")
p5 <- ggplot(merge_edu_GDP, aes(x = merge_edu_GDP$Index, y = merge_edu_GDP$GDP))+
  geom_smooth(method = "lm")+
  geom_point(size = 2, color = "red" )+
  labs(x = "Education index", y = "GDP", title = "Relationship between Education index and GDP in China", caption = 'data from World Bank Group')


#drop 2 comuns in education
education <- education[-c(1:2)]
# test on all variables
merge_all <- merge(merged_fdi_percent, jour_num, by = "Year")
merge_all <- merge(merge_all, education, by  = "Year")
merge_all <- merge(merge_all, rail, by  = "Year")
multi_re <- lm(formula = merge_all$GDP~merge_all$FDI+merge_all$`as % of GDP`+merge_all$`Journal Number`+merge_all$length + merge_all$Index, data = merge_all)
summary(multi_re)
#control a variable at its mean level

merge_jour_edu <- merge(GDP_C, education, by = "Year")
merge_jour_edu <- merge(merge_jour_edu, jour_num, by  ="Year")
sum_jour_edu <- lm(merge_jour_edu$GDP~merge_jour_edu$Index+merge_jour_edu$`Journal Number`)
sum_jour_edu_star <- summary(sum_jour_edu)
# p4 is the graph
p4<- ggplot(merge_jour_edu, aes( x = merge_jour_edu$Index, y = merge_jour_edu$GDP))+
  geom_point()+
  geom_abline(intercept = sum_jour_edu_star$coefficients[1]+sum_jour_edu_star$coefficients[3]*mean(merge_jour_edu$`Journal Number`),
              slope = sum_jour_edu_star$coefficients[2],
              color = 'blue',
              size = 2)+
  labs(x = "education index", y = "GDP", title = "predicted value for GDP by education index", subtitle = "where scientific and technical journal articles is held at its mean")
#stargazer
stargazer(multi_re,type = "html",out = 'multi_re.htm',
          title = 'Factors that contribute to the growth of Chinese economy',
          style = "ajps",
          column.labels = "GDP growth",
          covariate.labels = c("Foreign Direct Investment","Expenditures in research and development as % of GDP", "Scientific and technical journal articles","Rail total length", "education_index"),
          keep.stat = c("N", 'rsq'),
          dep.var.labels = "",
          notes = "*World Bank Group",
          notes.align =("r")
)

stargazer(multi_re,type = 'html',out = 'multi_re.htm',
          covariate.labels = c("FDI","Expenditure", "Journal Numbers","Rail length", "Education index")
)
shell.exec('multi_re.htm')

# Include other authoritarian countries- Russia

#education index
setnames(education, "Index", "Index_c" )
education_compa <- merge(education, education_russia, by  = "Year")
education_compa$Year <- as.numeric(as.character(education_compa$Year))
education_compa$Index_c <- as.numeric(education_compa$Index_c)
education_compa$Index <- as.numeric(education_compa$Index)

p6 <- ggplot(education_compa, aes(x = Year))+
  geom_line(aes(y= education_compa$Index_c),size = 1.5, colour= "red", linetype = "solid" )+
  geom_line(aes(y = education_compa$Index), size = 1.5, colour = "blue", linetype ="solid")+
  annotate("text", x = 2005, y = 0.4, label = "education index of China", colour = "Red", size = 5)+
  annotate("text", x = 2005, y = 0.66, label = "education index of Russia", colour = "blue", size = 5)+
  labs(title = "Comparison between two countries' education index ", caption = "data from World Bank")

#education and GDP 
merge_edu_GDP_r <- merge(GDP_R, education_russia, by ="Year")
merge_edu_GDP_r$Year <- as.numeric(merge_edu_GDP_r$Year)
merge_edu_GDP_r$GDP_R <- as.numeric(merge_edu_GDP_r$GDP_R)
merge_edu_GDP_r$Index <- as.numeric(merge_edu_GDP_r$Index)

# plot for Russia's relationship between education index and GDP growth
p7 <- ggplot(merge_edu_GDP_r)+
  geom_point(aes(x = merge_edu_GDP_r$Index, y = merge_edu_GDP_r$GDP_R))+
  geom_smooth(aes(x = merge_edu_GDP_r$Index, y = merge_edu_GDP_r$GDP_R),method = "lm")+
  labs(title = "relation between education index and GDP growth in Russia", x = "education index", y = "GDP growth rate", caption = "data from world bank")

#plot for realtionship between FDI and GDP of Russia
merge_FDI_R <- merge(FDI_R, GDP_R, by  ="Year")
p8 <- ggplot(merge_FDI_R)+
  geom_point(aes(x = merge_FDI_R$FDI, y = merge_FDI_R$GDP_R))+
  geom_smooth(aes(x = merge_FDI_R$FDI, y = merge_FDI_R$GDP_R ),method = "lm")+
  labs(title = "Relationship between FDI and GDP of Russia", caption = "data from World Bank", x = "FDI", y = "GDP")
  



