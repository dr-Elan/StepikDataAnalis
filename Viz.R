library(ggplot2)
library(corrplot)
library(plotrix)
library(ggpubr)
library(ISLR)
library(quantmod)
library(ggcorrplot)
library(GGally)

data(sleep75, package='wooldridge')
data(diamonds, package='ggplot2')
data(Wage, package='ISLR')
data(Diamond, package='Ecdat')
sleep75$gender <- factor(sleep75$male, labels = c('female', 'male'))
sleep75$south <- factor(sleep75$south, labels = c('no', 'yes') )

my.width <- 6
my.height <- 4.5

# Pie/donut chart

gender <- factor(sleep75$male, labels = c('female', 'male'))

# pie(summary(gender), col=rainbow(2), main="Гендерная переменная")
# 
# pie3D(summary(gender), labels = c('female', 'male'), main="Гендерная переменная", explode = 0.1)

# Рынок телефонов
phones <- data.frame("brand" = c("Samsung","Huawei","Apple","Xiaomi","OPPO","Other"),
                     "share" = c(.2090,.1580,.1210,.0930,.0870,.3320))

print(phones)

# ggdonutchart(data=phones, x='share', fill='brand', lab.pos = 'in', color='white', lab.font = c(6, "bold", "white"), ggtheme = theme_pubr(legend='right') )
my.donut <- ggdonutchart(data=phones, x='share', fill='brand', lab.pos = 'in', color='white', lab.font = c(6, "bold", "white"), 
                         ggtheme = theme_pubr(legend='right') )
ggsave(filename = 'DonutPhones.pdf', plot=my.donut, device = 'pdf', width = my.width, height = my.height)

# ggpie(data=phones, x='share', fill='brand', legend='right', lab.pos = 'in', color='white', lab.font = c(5, "bold", "white"))
my.pie <- ggpie(data=phones, x='share', fill='brand', legend='right', lab.pos = 'in', color='white', lab.font = c(5, "bold", "white"))
ggsave(filename = 'PiePhones.pdf', plot=my.pie, device = 'pdf', width = my.width, height = my.height)

# pie(phones$share, labels = phones$brand, col=rainbow(length(phones$share)),
#     main='Рынок сотовых телефонов')
# 
# pie3D(summary(phones$share), labels = phones$brand, main="Рынок сотовых телефонов")

# Гистограмма

my.hist <- ggplot(data=sleep75)+geom_histogram(aes(x=sleep), col='white')+theme_classic()
ggsave(filename = 'HistSleep.pdf', plot=my.hist, device = 'pdf', width = my.width, height = my.height)

my.hist <- ggplot(data=sleep75)+geom_histogram(aes(x=totwrk), col='white')+theme_classic()
ggsave(filename = 'HistTotwrk.pdf', plot=my.hist, device = 'pdf', width = my.width, height = my.height)
# gghistogram(data=sleep75, x='sleep', fill = "darkgray")

# Bar Chart

my.chart <- ggplot(data=sleep75, aes(gender))+geom_bar()+theme_classic()
ggsave(filename = 'BarGender.pdf', plot=my.chart, device = 'pdf', width = my.width, height = my.height)

my.chart <- ggplot(data=Diamond, aes(colour))+geom_bar()+theme_classic()
ggsave(filename = 'BarDiamondColour.pdf', plot=my.chart, device = 'pdf', width = my.width, height = my.height)

# Диаграмма с накоплением

my.chart <- ggplot(data=sleep75, aes(x=sleep))+geom_histogram(aes(fill=gender), col='white')+theme_classic()
ggsave(filename = 'StackedChartSleep.pdf', plot=my.chart, device = 'pdf', width = my.width, height = my.height)

my.chart <- ggplot(data=sleep75, aes(x=totwrk))+geom_histogram(aes(fill=south), col='white')+theme_classic()
ggsave(filename = 'StackedChartTotwrk.pdf', plot=my.chart, device = 'pdf', width = my.width, height = my.height)

my.chart <- ggplot(data=Diamond, aes(x=colour))+geom_bar(aes(fill=certification), col='white')+theme_classic()
ggsave(filename = 'StackedChartColour.pdf', plot=my.chart, device = 'pdf', width = my.width, height = my.height)

# Нормированная столбчатая диаграмма

my.chart <- ggplot(data=sleep75, aes(x=gender))+geom_bar(aes(fill=south), position = 'fill')
ggsave(filename = 'NormalizedChartGender.pdf', plot=my.chart, device = 'pdf', width = my.width, height = my.height)

my.chart <- ggplot(data=Diamond, aes(x=colour))+geom_bar(aes(fill=clarity), position = 'fill')
ggsave(filename = 'NormalizedChartColour.pdf', plot=my.chart, device = 'pdf', width = my.width, height = my.height)

# Диаграмма с группировкой

my.chart <- ggplot(data=Diamond, aes(x=certification))+geom_bar(aes(fill=colour), position='dodge')+theme_classic()
ggsave(filename = 'GroupingChartCertification.pdf', plot=my.chart, device = 'pdf', width = my.width, height = my.height)

my.chart <- ggplot(data=Diamond, aes(x=clarity))+geom_bar(aes(fill=colour), position='dodge')+theme_classic()
ggsave(filename = 'GroupingChartCertification.pdf', plot=my.chart, device = 'pdf', width = my.width, height = my.height)

# Линейная диаграмма

my.chart <- ggplot(data=Diamond, aes(y=certification))+geom_bar(aes(fill=clarity))+theme_classic()
ggsave(filename = 'LinearNormalizedChartCertification.pdf', plot=my.chart, device = 'pdf', width = my.width, height = my.height)

my.chart <- ggplot(data=Diamond, aes(y=clarity))+geom_bar(aes(fill=certification), position='dodge')+theme_classic()
ggsave(filename = 'LinearGroupChartClarity.pdf', plot=my.chart, device = 'pdf', width = my.width, height = my.height)

# Диаграмма рассеяния

my.plot <- ggplot(data=sleep75, aes(x=totwrk, y=sleep))+geom_point()+theme_classic() 
#+theme(plot.background = element_rect(fill = "yellow"))
ggsave(filename = 'SleepVsTotowrkPlot.pdf', plot=my.plot, device = 'pdf', width = my.width, height = my.height)

my.plot <- ggplot(data=Diamond, aes(x=carat, y=price))+geom_point()+theme_classic() 
#+theme(plot.background = element_rect(fill = "yellow"))
ggsave(filename = 'PriceVsCaratPlot.pdf', plot=my.plot, device = 'pdf', width = my.width, height = my.height)

my.plot <- ggplot(data=sleep75, aes(x=totwrk, y=sleep))+geom_point(aes(col=gender, shape=gender))+theme_classic() 
#+theme(plot.background = element_rect(fill = "yellow"))
ggsave(filename = 'SleepVsTotowrkGenderPlot.pdf', plot=my.plot, device = 'pdf', width = my.width, height = my.height)

my.plot <- ggplot(data=Diamond, aes(x=carat, y=price))+geom_point(aes(col=clarity, shape=certification))+theme_classic() 
#+theme(plot.background = element_rect(fill = "yellow"))
ggsave(filename = 'PriceVsCaratCertificationPlot.pdf', plot=my.plot, device = 'pdf', width = my.width, height = my.height)

# Линейная диаграмма

getSymbols(Symbols = 'GDP', src='FRED', return.class='zoo')
my.plot <- autoplot.zoo(GDP)+theme_classic()
ggsave(filename = 'GDPPlot.pdf', plot=my.plot, device = 'pdf', width = my.width, height = my.height)

# Box Plot

my.boxplot <- ggboxplot(data=sleep75, y='sleep', orientation = "horizontal")
ggsave(filename = 'SleepBoxplot.pdf', plot=my.boxplot, device = 'pdf', width = my.width, height = my.height)

my.boxplot <- ggboxplot(data=sleep75, x='south', y='sleep', color = 'south', legend='right')
ggsave(filename = 'SleepSouthBoxplot.pdf', plot=my.boxplot, device = 'pdf', width = my.width, height = my.height)

my.boxplot <- ggboxplot(data=Diamond, x='colour', y='price', color = 'colour', legend='right')
ggsave(filename = 'PriceColourBoxplot.pdf', plot=my.boxplot, device = 'pdf', width = my.width, height = my.height)

my.boxplot <- ggboxplot(data=Diamond, x='certification', y='price', color = 'certification', legend='right')
ggsave(filename = 'PriceCertificationrBoxplot.pdf', plot=my.boxplot, device = 'pdf', width = my.width, height = my.height)

my.boxplot <- ggboxplot(data=sleep75, x='gender', y='totwrk', color = 'south', legend='right')
ggsave(filename = 'TotwrkGenderSouthBoxplot.pdf', plot=my.boxplot, device = 'pdf', width = my.width, height = my.height)

# Correlatiom matrix

data(sleep75, package='wooldridge')

df <- subset(sleep75, select = c(sleep, totwrk, age, educ) )
corr.matrix <- cor(df)
my.corr.matr <- ggcorrplot(corr.matrix, lab = TRUE, lab_size = 6)
ggsave(filename = 'CorrMatrixSleepSmallFull.pdf', plot=my.corr.matr, device = 'pdf', width = my.height, height = my.height)

df <- subset(sleep75, select = c(-hrwage, -agesq, -leis1, -leis2, -leis3, -exper) )
corr.matrix <- cor(df)
my.corr.matr <- ggcorrplot(corr.matrix, lab = FALSE, type='upper')
ggsave(filename = 'CorrMatrixSleepBigUpper.pdf', plot=my.corr.matr, device = 'pdf', width = my.height, height = my.height)

df <- subset(sleep75, select = c(sleep, totwrk, age, educ) )
corr.matrix <- cor(df)
my.corr.matr <- ggcorrplot(corr.matrix, lab = FALSE, lab_size = 6, method='circle')
ggsave(filename = 'CorrMatrixSleepSmallFullCircle.pdf', plot=my.corr.matr, device = 'pdf', width = my.height, height = my.height)


# ggplot(phones, aes(x="", y=share, fill=brand)) +
#   geom_bar(stat="identity", width=1, color="white") + labs(title='Круговая диаграмма')+
#   coord_polar("y", start=0)+theme_void()+
#   geom_text(aes(label = share),
#             position = position_stack(vjust = 0.5), col='white', size=5)

# my.pie <- ggplot(phones, aes(x="", y=share, fill=brand)) +
#   geom_bar(stat="identity", width=1, color="white") + #labs(title='Круговая диаграмма')+
#   coord_polar("y", start=0)+theme_void()+
#   geom_text(aes(label = share),
#             position = position_stack(vjust = 0.5), col='white', size=5)
# ggsave(filename = 'PiePhones.pdf', plot=my.pie, device = 'pdf')
