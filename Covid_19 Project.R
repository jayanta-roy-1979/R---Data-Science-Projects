 #  *** COVID-19  R Programming Data Analysis Project created by Jayanta Roy ***
          

# Set the Working Directory:

print(getwd())
setwd("C:\Users\Jayanta Roy\Downloads\R_Projects")

# Install library:  

install.packages('xlsx')
any(grepl("xlsx",installed.packages()))
library("readxl")  

# Import and Read the Data 

confirmed_cases_worldwide=read.xlsx("C:/Users/Jayanta Roy/Downloads/R_Projects/COVID_cases.xlsx",sheetIndex=1)

summary(COVID_cases)
str(COVID_cases)



install.packages('dplyr')
install.packages('ggplot2')
install.packages('Shiny')

library(ggplot2)
library(dplyr)
library(xlsx)

# See the Result

confirmed_cases_worldwide
CCW<-COVID_cases
str(CCW)


# Confirm Cases throughout the World with line chart: 

ggplot(data=CCW,aes(x=date, y=cum_cases))+ 
  geom_line(col="red") + ylab("cumulative_confirmed_cases")


confirmed_cases_china_vs_world <- read_excel("COVID_cases.xlsx", 
                            sheet = "Sheet2")
# See the Result

str(confirmed_cases_china_vs_world)
summary(confirmed_cases_china_vs_world)

# Draw a Line Plot of a Cumulative cases vs. date, grouped & colored by 
# Define aesthetics with the  within the line geom:

plt_cum_confirmed_cases_china_vs_world<-ggplot(data=confirmed_cases_china_vs_world,aes(x=date, y=cum_cases))+
  geom_line(aes(x=date, y=cum_cases, group= is_china, color= is_china)) + ggtitle("Covid_19 confirmed cases in China created by Jayanta Roy")
  ylab("cumulative_confirmed_cases")
  plt_cum_confirmed_cases_china_vs_world
  
  
# # Adding a Trend line to China and rest of the world:  
  
PLT_CCCH_VS_W<-plt_cum_confirmed_cases_china_vs_world

PLT_CCCH_VS_W

jpeg(file="china_vs_world.jpeg")
PLT_CCCH_VS_W
dev.off()


# Install the Library:

install.packages('magrittr')
library(magrittr)
library(dplyr)
library(ggplot2)

# Lets annotate:

who_events<-tribble(
  ~ date, ~ event,
  "2020-01-30", "Global health\nemergency declared",
  "2020-03-11", "Pandemic\ndeclared",
  "2020-02-13",  "China  reporting\nchange"
  
  
)%>% # Use Pipe Operator: 
     # Mutate adds new variables preserves  existing:
  mutate(date= as.Date(date))

who_events

# Using who_events, add vertical dashed line with and text at date labeled by events
# and at 100000 on the y axis

PLT_CCCH_VS_W +
  geom_vline(aes(xintercept=date),data=who_events,linetype="dashed")+
geom_text(aes(x=date, label=event), data=who_events, y = 100000)

PLT_CCCH_VS_W

# Adding a Logarithmic Scale: 
# Using China_after_feb15, draw a line plot cum_cases vs. date 
# Add smooth trend line using linear regression, no error bars
# see argument inside the geom_smooth display confidence interval around smooth, TRUE by default
# lm(), or linear model, " function can be used to create a simple regression model. 
china_after_feb15<-confirmed_cases_china_vs_world %>%
  filter(is_china=="China", date >= "2020-02-15")

china_after_feb15

ggplot(data=china_after_feb15, aes(x= date, y=cum_cases)) +
   geom_line() +
   geom_smooth(method = "lm", se=FALSE) +ggtitle("Covid_19 in China created by Jayanta Roy")
   ylab("Cumulative_confirmed_cases")
       
   
   
# Filter Confirmed cases by china  vs world not for china: 
   
not_china<-confirmed_cases_china_vs_world %>%
  filter(is_china=="Not China")

not_china

# Using not_china draw a line plot cum_cases vs.date 
# Add smooth a trend line using linear regression, no error bars. 

plt_not_china_trend_line<-ggplot(data=not_china, aes(x=date, y=cum_cases)) +
  geom_line()+
  geom_smooth(method = "lm", se=FALSE) +ggtitle("Covid_19 Not in China created by Jayanta Roy")
ylab("Cumulative_confirmed_cases")

# Modify the plot to use a logarithmic scale on the 

plt_not_china_trend_line

plt_not_china_trend_line +
  scale_y_log10()

# Plotting a hardest hit countries ad of Mid- march 2020
# Which countries outside of china have been hit hardest

library(readxl)
confirmed_cases_by_country <- read_excel("COVID_cases.xlsx", 
                                         sheet = "Sheet3")
confirmed_cases_by_country

X<-confirmed_cases_by_country$cum_cases
X

df<-confirmed_cases_by_country[order(-X),]
df
df[1:25,]

# Using Top_countries_by_total_cases, draw a bar plot of cum_cases vs. province.  

top_country_by_total_cases<-df[1:25,]      
top_country_by_total_cases


barplot(top_country_by_total_cases$cum_cases, name.arg=top_country_by_total_cases$cum_cases, names.arg=top_country_by_total_cases$country,col='green',ggtitle=('created by Jayanta Roy'))
barplot(top_country_by_total_cases$cum_cases, name.arg=top_country_by_total_cases$cum_cases, names.arg=top_country_by_total_cases$date, col='blue',ggtitle=('created by Jayanta roy'))


ggplot(data=top_country_by_total_cases,aes(x=date, y=cum_cases))+ ggtitle=("Top Country by Total Cases created by Jayanta Roy")
geom_line()

ggplot(data=top_country_by_total_cases,aes(x=date, y=cum_cases))+
  geom_line(aes(x=date, y=cum_cases,group = country, color= country))+ ggtitle("created by Jayantar Roy")
  ylab("Cumulative_confirmed_cases")

ggplot(data=top_country_by_total_cases,aes(x=date,y=cum_cases))+geom_area()

x1<-top_country_by_total_cases$date
y1<-top_country_by_total_cases$cum_cases
z1<-top_country_by_total_cases$country
qplot(x1,y1)
qplot(x1,y1, geom=c("point","line"))
qplot(x1,y1, geom=c("point","line"),col="red")

ggplot(data=top_country_by_total_cases, aes(x=date,y=cum_cases)) +

geom_point(aes(x=date,y=cum_cases, color=country))
ylab("top_country_by_total_cases")




