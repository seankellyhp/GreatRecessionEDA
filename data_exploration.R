
# Applied Statistics 1 

setwd("C:/Users/Sean/Desktop/S1 BCUBDA Textbooks/Applied Statistics/Assignment 1/")

# Load Data from 2010 to now 

home.ts <- read.csv(file = "home_price_index.csv")

employment.ts <- read.csv(file="unemployment_seasonal_adj_time.csv")
  
price.ts <- read.csv("cost_all_items_time.csv")
  
dow.ts <- read.csv("dow_industrial_avg_cl.csv", stringsAsFactors = F)

# Load Packages 
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(scales)

# Combine Data 

empl.raw <- employment.ts %>% 
  filter(Series.ID == "LNU04000000") %>% 
  rename("Unempl"=Value) %>% 
  select(-Label)

empl.adj <- employment.ts %>% 
  filter(Series.ID == "LNS14000000") %>% 
  rename("Unempl.adj"=Value) %>% 
  separate(Label, into = c("Year.b", "Month"), sep=" ") %>% 
  select(-Year.b)

empl.merge <- merge(empl.raw, empl.adj, by = c("Year", "Period"))

price.raw <- price.ts %>% 
  rename("Price.All"=Value) %>%
  select(-Label)

empl.price <- merge(empl.merge, price.raw, by=c("Year", "Period"))
empl.price$Year <- as.character(empl.price$Year)

dow.cl <- dow.ts %>% 
  separate(Date, into = c("Year", "Month")) %>% 
  filter(Month != "00")

dow.cl00 <- dow.ts %>% 
  separate(Date, into = c("Year", "Month")) %>% 
  filter(Month == "00")

names(dow.cl00) <- c("Month", "Year", "Price", "Open", "High", "Low", "Vol.", "Change..")

dow.cl2 <- bind_rows(dow.cl, dow.cl00)
table(dow.cl2$Month)

dow.cl2$Year.n <- as.numeric(dow.cl2$Year)
dow.cl2$Year.n2 <- formatC(dow.cl2$Year.n, width=2, flag="0")
dow.cl2$Year.char <- as.character(dow.cl2$Year.n2)
dow.cl2$Year.cl <- paste0('20',dow.cl2$Year.char)

dow.cl3 <- dow.cl2 %>% 
  select(Year.cl, Month, 'Dow.Price' = Price, "Dow.High"= High, "Dow.Low" = Low, Change..) %>% 
  rename("Year" = Year.cl, "Dow.Change" = Change..)

empl.price.dow <- merge(empl.price, dow.cl3, by=c("Year", "Month"))

empl.price.dow.cl <- empl.price.dow %>% 
  separate(Period, into = c("P", "Month.n"), sep = "M", remove = F) %>% 
  select(-P)

empl.price.dow.cl$Month.n <- as.numeric(empl.price.dow.cl$Month.n)

home.cl <- home.ts %>% 
  separate(DATE, into = c("Month", "Day", "Year"), sep="/", remove = F) %>% 
  select(-Day)

home.cl$Month <- as.numeric(home.cl$Month)
home.cl$DATE <- as.character(home.cl$DATE)
home.cl$DATE.d <- as.Date(home.cl$DATE, "%m/%d/%Y")


names(home.cl) <- c("Date.ch", "Month.n", "Year", "Home.Price.Index", "Date.d")

empl.price.dow.home <- merge(empl.price.dow.cl, home.cl, by=c("Year", "Month.n"))

names(empl.price.dow.home)

# Federal Housing Price 

home.fed.raw <- read.csv("HPI_master.csv")

home.fed.us <- home.fed.raw %>% 
  filter(level == "USA or Census Division" & place_id == "USA" & frequency == "monthly") %>% 
  filter(yr >= 2000 & yr <= 2018) %>% 
  mutate(Year = as.character(yr)) %>% 
  select(Year, 'Month.n' = period, 'Fed.Home.Nsa'=index_nsa, 'Fed.Home.Sa'=index_sa)

# COmbine US housing with the rest 

empl.price.dow.home.fed <- merge(empl.price.dow.home, home.fed.us, by=c("Year", "Month.n"))


# Make a copy of combined US data 
master.us.raw <- empl.price.dow.home.fed

# Clean Combined US data for predictors 
master.us.cl <- master.us.raw %>% 
  select(Date.d, Year, Month, Month.n, Unempl, Unempl.adj, 'CPI.Price.All'= Price.All, Dow.Price, Dow.Change, Home.Price.Index, Fed.Home.Nsa, Fed.Home.Sa)

#write.csv(master.us.cl, file = "great_recession_us_master.csv")

# fun 
#mod.1 <- lm(master.us.cl$Unempl ~ master.us.cl$CPI.Price.All + master.us.cl$Dow.Price + master.us.cl$Home.Price.Index + master.us.cl$Year)

#cor(master.us.cl$CPI.Price.All, master.us.cl$Dow.Price)
#cor(master.us.cl$CPI.Price.All, master.us.cl$Home.Price.Index)
#cor(master.us.cl$Dow.Price, master.us.cl$Home.Price.Index)

#Correlation Matrix
cor(master.us.cl[,5:12])


# Distributions 



# Create Sturges Rule Funtion for histogram bins

sturges <- function(n) {
  tot.bins <- 1 + (3.322 * log(n))
  return(tot.bins)
}

##### HISTOGRAMS for Distribution

master.us.cl <- filter(master.us.cl, as.numeric(Year)>= 2004 &  as.numeric(Year)<= 2014)

NROW(na.omit(master.us.cl$Unempl.adj))
ggplot(master.us.cl, aes(Unempl.adj)) + geom_histogram(bins = round(sturges(120), 0), fill = 'steelblue', color = 'white') + 
  ggtitle('Distribution of Unemployment Rate: 2004-2014') + theme_tufte() + xlab('Unemployment: Seasonally Adjusted % of all Eligible Persons Over 16 Years') + 
  ylab('Number of Months') + theme(text=element_text(size=12, color='black'), axis.text = element_text(size = 12, color = 'black')) + 
  scale_x_continuous(breaks = seq(1,10,1))

NROW(na.omit(master.us.cl$CPI.Price.All))
ggplot(master.us.cl, aes(CPI.Price.All)) + geom_histogram(bins = 18, fill = 'steelblue', color = 'white') + 
  ggtitle('Distribution of Consumer Price Index: 2004-2014') + theme_tufte() + xlab('Consumer Price Index: Jan. 2000 = 100') + 
  ylab('Number of Months') + theme(text=element_text(size=12, color='black'), axis.text = element_text(size = 12, color = 'black')) +
  scale_x_continuous(breaks = c(105, 110, 115, 120, 125, 130, 135))
  
NROW(na.omit(master.us.cl$Dow.Price))
ggplot(master.us.cl, aes(Dow.Price)) + geom_histogram(bins = 16, fill = 'steelblue', color = 'white') + 
  ggtitle('Distribution of Dow Jones Stock Prices: 2004-2014') + theme_tufte() + xlab('Dow Jones Industrial Average: US Dollars') + 
  ylab('Number of Months') + theme(text=element_text(size=12, color='black'), axis.text = element_text(size = 12, color = 'black')) + 
  scale_x_continuous(breaks = seq(8000, 20000, by = 2000))


NROW(na.omit(master.us.cl$Fed.Home.Sa))
ggplot(master.us.cl, aes(Fed.Home.Sa)) + geom_histogram(bins = 16, fill = 'steelblue', color = 'white') + 
  ggtitle('Distribution of Federal House Price Index: 2004-2014') + theme_tufte() + xlab('Federal House Price Index: Seasonally Adjusted, Jan. 1991 = 100') + 
  ylab('Number of Months') + theme(text=element_text(size=12, color='black'), axis.text = element_text(size = 12, color = 'black')) + 
  scale_x_continuous(breaks = c(100, 120, 140, 160, 180, 200, 220, 240, 260, 280, 300))


library(psych)
describe(master.us.cl$Fed.Home.Sa)
describe(master.us.cl$Dow.Price)

summary(master.us.cl$Fed.Home.Sa)
fivenum(master.us.cl$Fed.Home.Sa)

describe(master.us.cl$Federal.Home.Sa)
describe(master.us.cl$Dow.Price)

master.us.cl.change <- master.us.cl %>% 
  arrange(Date.d) %>% 
  mutate(CPI.Price.Prev = lag(CPI.Price.All), CPI.Diff = CPI.Price.All - CPI.Price.Prev, CPI.PercDiff = (CPI.Price.All/CPI.Price.Prev - 1) * 100,
         Unempl.Prev = lag(Unempl), Unempl.Diff = Unempl - Unempl.Prev, Unempl.PercDiff = (Unempl/Unempl.Prev - 1) * 100,
         Unempl.adj.Prev = lag(Unempl.adj), Unempl.adj.Diff = Unempl.adj - Unempl.adj.Prev, Unempl.adj.PercDiff = (Unempl.adj/Unempl.adj.Prev - 1) * 100,
         Fed.Home.Nsa.Prev = lag(Fed.Home.Nsa), Fed.Home.Nsa.Diff = Fed.Home.Nsa - Fed.Home.Nsa.Prev, Fed.Home.Nsa.PercDiff = (Fed.Home.Nsa/Fed.Home.Nsa.Prev - 1) * 100,
         Fed.Home.Sa.Prev = lag(Fed.Home.Sa), Fed.Home.Sa.Diff = Fed.Home.Sa - Fed.Home.Sa.Prev, Fed.Home.Sa.PercDiff = (Fed.Home.Sa/Fed.Home.Sa.Prev - 1) * 100
         )

cpi.change <- master.us.cl.change %>% 
  select(contains('CPI'), Date.d, Year, Month, Month.n) %>% 
  mutate(increase = ifelse(CPI.PercDiff > -0.5, 11, 0))

table(cpi.change$increase)

## Fit normal curve not working 
#ggplot(master.us.cl, aes(Dow.Change)) + geom_histogram(bins = 21) + 
  #stat_function(fun = dnorm, args = list(mean = mean(master.us.cl$Dow.Change), sd = sd(master.us.cl$Dow.Change)))
#  stat_function(fun = function(x) dnorm(master.us.cl$Dow.Change, mean = mean(master.us.cl$Dow.Change)), sd = sd(master.us.cl$Dow.Change) * 223 * .02,
#                color = "darkred", size = 1)

#ggplot(master.us.cl, aes(x = Year, y = Home.Price.Index)) + geom_boxplot()

#ggplot(master.us.cl, aes(x = Year, y = Dow.Price)) + geom_boxplot()


  
#ggplot(filter(master.us.cl, as.numeric(Year)> 2007 &  as.numeric(Year)< 2010)) + geom_line(aes(x = Date.d, y = Dow.Price)) + geom_line(aes(x = Date.d, y = ))

## Multiple Variables during recession 
#ggplot(filter(master.us.cl, as.numeric(Year)>= 2007 &  as.numeric(Year)<= 2011)) + geom_line(aes(x = Date.d, y = Home.Price.Index, color = "red")) + geom_line(aes(x = Date.d, y = CPI.Price.All))


### Scale vars 
master.us.cl <- master.us.cl.change

master.us.scale <- scale(master.us.cl[,c(5:12, 13, 19)], center = TRUE, scale = TRUE)

master.us.scale.df <- data.frame(master.us.scale)

master.us.scale.df$Date.d <- master.us.cl$Date.d
master.us.scale.df$Year <- master.us.cl$Year
master.us.scale.df$Month <- master.us.cl$Month

colnames(master.us.scale.df) <- paste(colnames(master.us.scale.df),'.sc', sep= '')


master.us.scale.df <- master.us.scale.df %>% 
  arrange(Date.d.sc)



master.us.cl.scale <- cbind(master.us.cl, master.us.scale.df)

scale1 <- scale(master.us.cl[,5], center = T, scale = T)
scale2 <- (master.us.cl[,5] - mean(master.us.cl[,5]))/sd(master.us.cl[,5])


master.us.cl.scale <- cbind(master.us.cl, master.us.scale.df)

#rcorr(as.matrix(gr.all.cor))
master.us.cl.groups <- master.us.cl.scale %>% 
  mutate(Recessions = ifelse( 
    Date.d <= as.Date('2007-12-01'), 'Pre', ifelse (
      Date.d >= as.Date('2009-06-01'), 'Post', 'During')))


table(master.us.cl.groups$Recessions)


######
master.us.t <- master.us.cl.groups %>% 
  filter(Date.d >= as.Date('2004-12-01') & Date.d <= as.Date('2012-06-01')) %>% 
  filter(Recessions != 'During')

t.test(master.us.t$Dow.Price ~ master.us.t$Recessions ,alternative = 'less')
t.test(master.us.t$Fed.Home.Sa ~ master.us.t$Recessions ,alternative = 'less')

t.test(master.us.t$Unempl.adj ~ master.us.t$Recessions ,alternative = 'greater')
t.test(master.us.t$CPI.Price.All ~ master.us.t$Recessions ,alternative = 'less')

before <- filter(master.us.t, Recessions == 'Pre') 
during <- filter(master.us.t, Recessions == 'During') 

t.test(during$CPI.Price.All.sc , during$Unempl.adj.sc)

t.test(during$CPI.Price.All.sc , during$Dow.Price.sc)
t.test(during$Fed.Home.Sa.sc , during$Dow.Price.sc)

t.test(Unempl.adj.sc ~ Recessions, data = master.us.t, paired =TRUE)
t.test(CPI.Price.All.sc ~ Recessions, data = master.us.t, paired =TRUE)
t.test(Fed.Home.Sa.sc ~ Recessions, data = master.us.t, paired =TRUE)
t.test(Dow.Price.sc ~ Recessions, data = master.us.t, paired =TRUE)

means.prepost <- master.us.t %>% 
  select(Recessions, Unempl.adj, CPI.Price.All, Fed.Home.Sa, Dow.Price) %>% 
  group_by(Recessions) %>% 
  summarise_all(mean)

fr.pre.cor <- master.us.cl.groups %>% 
  filter(Recessions == 'Pre') %>% 
  select(CPI.Price.All, Unempl.adj, Fed.Home.Sa, Dow.Price)

fr.during.cor <- master.us.cl.groups %>% 
  filter(Recessions == 'During') %>% 
  select(CPI.Price.All, Unempl.adj, Fed.Home.Sa, Dow.Price)

fr.post.cor <- master.us.cl.groups %>% 
  filter(Recessions == 'Post') %>% 
  select(CPI.Price.All, Unempl.adj, Fed.Home.Sa, Dow.Price)


##### SIMPLE REGRESSIONS - Linear Realtionships but non-normal, cant model during recession 
corr.test(as.matrix(fr.pre.cor), method = 'spearman')
corr.test(as.matrix(fr.during.cor), method = 'spearman')
corr.test(as.matrix(fr.post.cor), method = 'spearman')

corr.test(as.matrix(fr.pre.cor), method = 'pearson')
corr.test(as.matrix(fr.during.cor), method = 'pearson')
corr.test(as.matrix(fr.post.cor), method = 'pearson')


fr.pre.reg <- master.us.cl.groups %>% 
  filter(Recessions == 'Pre')

fr.post.reg <- master.us.cl.groups %>% 
  filter(Recessions == 'Post')


model.unemp.pre <- lm(Unempl.adj.sc ~ Fed.Home.Sa.sc + Dow.Price.sc + Unempl.adj.Prev.sc, data = fr.pre.reg)
model.unemp.post <- lm(Unempl.adj.sc ~ Fed.Home.Sa.sc + Dow.Price.sc + Unempl.adj.Prev.sc, data = fr.post.reg)

model.cpi.pre <- lm(CPI.Price.All.sc ~ Fed.Home.Sa.sc + Dow.Price.sc + CPI.Price.Prev.sc, data = fr.pre.reg)
model.cpi.post <- lm(CPI.Price.All.sc ~ Fed.Home.Sa.sc + Dow.Price.sc + CPI.Price.Prev.sc, data = fr.post.reg)

summary(model.unemp.pre)
summary(model.unemp.post)

summary(model.cpi.pre)
summary(model.cpi.post)










#write.csv(master.us.cl.scale, file = "great_recession_us_master_scale.csv")

# Most Awesome chart of rise in Home Price Index Scale Compared to CPI Index Scale
ggplot(filter(master.us.scale.df, as.numeric(Year)>= 2003 &  as.numeric(Year)<= 2017)) + geom_line(aes(x = Date.d, y = Home.Price.Index, color = "red")) + geom_line(aes(x = Date.d, y = CPI.Price.All))

# Federal Home Price and the Case Shiller one are very similar
ggplot(filter(master.us.scale.df, as.numeric(Year)>= 2003 &  as.numeric(Year)<= 2017)) + geom_line(aes(x = Date.d, y = Home.Price.Index, color = "red")) + 
  geom_line(aes(x = Date.d, y = CPI.Price.All)) + geom_line(aes(x = Date.d, y = Fed.Home.Sa, color = "blue"))

# Quick Modelling
model1 <- lm(Unempl ~ CPI.Price.All + Dow.Price + Fed.Home.Nsa, data=master.us.cl)
summary(model1)

model1a <- lm(Unempl ~ CPI.Price.All + Dow.Price + Fed.Home.Nsa, 
              data= filter(master.us.cl, as.numeric(Year)> 2004 &  as.numeric(Year)< 2014))
summary(model1a)

model2 <- lm(CPI.Price.All ~ Unempl + Dow.Price + Fed.Home.Nsa, data=master.us.cl)
summary(model2)

model2a <- lm(CPI.Price.All ~ Unempl + Dow.Price + Fed.Home.Nsa, 
             data=filter(master.us.cl, as.numeric(Year)>= 2000 &  as.numeric(Year)<= 2019))
summary(model2a)



# Individual Trends 

master.us.cl.house <- master.us.cl %>% 
  mutate(Fed.Hom.Nsa2000 = Fed.Home.Nsa - 37.29)

master.us.cl.s <- master.us.cl %>% 
  mutate(Recessions = ifelse( 
    Date.d >= as.Date('2001-3-01') & Date.d <= as.Date('2001-11-30'), 1, ifelse (
      Date.d >= as.Date('2007-12-01') & Date.d <= as.Date('2009-06-30'), 2, 0)))

table(master.us.cl.s$Recessions, master.us.cl.s$Year)

# Correlation Matrix

install.packages('GGally')
library(GGally)

ggpairs(master.us.cl, columns = c("Unempl.adj", "Dow.Price", "Fed.Home.Sa", "CPI.Price.All"), title = "Bivariate analysis of revenue expenditure by the British household", upper = list(continuous = wrap("cor",                                                                                                                                                                                     size = 3)),
        lower = list(continuous = wrap("smooth",
                                       size = 0.1))) + theme_tufte()

ggcorr(master.us.cl[, c("Unempl.adj", "Dow.Price", "Fed.Home.Sa", "CPI.Price.All")])

pairs.panels(master.us.cl[, c("Unempl.adj", "Dow.Price", "Fed.Home.Sa", "CPI.Price.All")], smoother = T, main = 'Corr Test', stars = TRUE, pch = '.')
violinBy(master.us.scale.df[, c("Unempl.adj", "Dow.Price", "Fed.Home.Sa", "CPI.Price.All")])

mixed.cor(master.us.cl[, c("Unempl.adj", "Dow.Price", "Fed.Home.Sa", "CPI.Price.All")])
cor.smooth(master.us.cl[, c("Unempl.adj", "Dow.Price", "Fed.Home.Sa", "CPI.Price.All")])

df2latex(fr.pre.cor)

library(purrr)

recession.compare <- master.us.cl.s %>% split(.$Recessions) %>% map(summary)

recession.table <- master.us.cl.s %>% 
  select(Recessions, Unempl.adj, CPI.Price.All, Dow.Price, Dow.Change, Home.Price.Index) %>% 
  group_by(Recessions) %>% 
  summarise_all(funs(min, max, mean, sd)) %>%
  gather(Key, Val, -Recessions) %>% 
  separate(Key,into = c("Var", "Math"), sep='_', drop = F) %>% 
  mutate(Val.cl = round(Val, 2)) %>% 
  select(-Val) %>%
  spread(Math, Val.cl) %>% 
  filter(Recessions != 0)

ggplot(filter(master.us.scale.df, Date.d >= '2007-06-01' &  Date.d <= '2010-01-30')) + geom_line(aes(x = Date.d, y = Home.Price.Index, color = "Home Price"), size = 1.2) + geom_line(aes(x = Date.d, y = CPI.Price.All, color = "CPI"), size = 1.2) + 
  geom_line(aes(x = Date.d, y = Unempl.adj, color = "Unemployment (Adj.)"), size = 1.2) + geom_line(aes(x = Date.d, y = Dow.Price, color = "Dow"), size = 1.2) +  
  geom_rect(aes(xmin = as.Date('2007-12-01'), ymin = -Inf, 
                xmax = as.Date('2009-06-30'), ymax = 3),
            fill = "lightgreen", alpha = .006) +
  ggtitle("Change in Economic Indicators During Great Recession") + 
  theme_tufte() + 
  scale_colour_hc() + 
  theme(legend.title = element_blank(), axis.text.x=element_text(angle = 90, vjust = 0.5)) + 
  scale_x_date(breaks = date_breaks("2 months"), minor_breaks = date_breaks("1 month"), 
               labels = date_format("%b-%y"))

dow.home <- master.us.cl.s %>% 
  filter(Recessions == 2) %>% 
  select(Dow.Price, Home.Price.Index)

cor(dow.home) # Highly Correlated in the Great Recession 

dow.home2 <- master.us.cl.s %>% 
  filter(Recessions == 1) %>% 
  select(Dow.Price, Home.Price.Index)

cor(dow.home2) # Med Negative Correlation in the First Recession 

dow.home3 <- master.us.cl.s %>% 
  filter(Recessions == 0) %>% 
  select(Dow.Price, Home.Price.Index)

cor(dow.home3) # Medium Correlation in Non-Recession Period 

dow.home4 <- master.us.cl.s %>% 
  select(Dow.Price, Home.Price.Index)

cor(dow.home4) # Medium Correlation, All Time 


cpi.employ <- master.us.cl.s %>% 
  filter(Recessions == 2) %>% 
  select(CPI.Price.All, Unempl.adj)

cor(cpi.employ) # Not that correlated in great recession

cpi.employ2 <- master.us.cl.s %>% 
  filter(Recessions == 1) %>% 
  select(CPI.Price.All, Unempl.adj)

cor(cpi.employ2) # Medium Corr in previous recession 

cpi.employ3 <- master.us.cl.s %>% 
  filter(Recessions == 0) %>% 
  select(CPI.Price.All, Unempl.adj)

cor(cpi.employ3) # Low Corr Outside of Recession 





##### Corr Tables 

#gr.all.cor <- master.us.cl.s %>% 
#  filter(Recessions == 2) %>% 
#  select(CPI.Price.All, Unempl.adj, Home.Price.Index, Dow.Price)

#cor(gr.all.cor)

library(Hmisc)


# CPI Change during Great Recession 

# Percent Decrease from Previous Month 

CPI.explore <- master.us.cl.s %>% 
  select(Date.d, CPI.Price.All, Recessions) %>% 
  mutate(CPI.Price.Prev = lag(CPI.Price.All), Diff = CPI.Price.All - CPI.Price.Prev, PercDiff = (CPI.Price.All/CPI.Price.Prev - 1) * 100)

ggplot(CPI.explore, aes(PercDiff)) + geom_histogram()

shapiro.test(CPI.explore$CPI.Price.All)

qqnorm(CPI.explore$PercDiff,main="QQ plot of CPI Change",pch=19)
qqline(CPI.explore$PercDiff)

# Global Financial Crisis 

mcsi <- read.csv(file = "mcsi.cl.csv", stringsAsFactors = F)
g.dow <- read.csv("gdow.cl.csv", stringsAsFactors = F)
sp.global <- read.csv("sp.global.cl.csv", stringsAsFactors = F)
ftse.world <- read.csv("ftse.world.cl.csv", stringsAsFactors = F)

mcsi.cl <- mcsi %>% 
  select("Date"=1, "MCSI.Price"=Price, "MCSI.Change"=Change..)

g.dow.cl <- g.dow %>% 
  select("Date"=1, "GDOW.Price"=Price, "GDOW.Change"=Change..)

sp.global.cl <- sp.global %>% 
  select("Date"=1, "SP.Price"=Price, "SP.Change"=Change..)

ftse.cl <- ftse.world %>% 
  select("Date"=1, "FTSE.Price"=Price, "FTSE.Change"=Change..)

global.stock <- left_join(mcsi.cl, g.dow.cl, by = "Date")
global.stock <- left_join(sp.global.cl, global.stock, by = "Date")
global.stock <- left_join(global.stock, ftse.cl, by = "Date")



global.stock.complete <- na.omit(global.stock)

cor(global.stock.complete[2:5]) # 
rcorr(as.matrix(global.stock.complete[2:9]))


# Clean Date 
global.stock.cl <- global.stock %>% 
  separate(Date, into = c("Year", "Month")) 

global.stock.cl$Year.n <- as.numeric(global.stock.cl$Year)
global.stock.cl$Year.n2 <- formatC(global.stock.cl$Year.n, width=2, flag="0")
global.stock.cl$Year.char <- as.character(global.stock.cl$Year.n2)
global.stock.cl$Year.cl <- paste0('20',global.stock.cl$Year.char)

global.stock.cl2 <- global.stock.cl %>% 
  select(-Year, -Year.n, -Year.n2, -Year.char) %>% 
  rename('Year'=Year.cl)

dates.sel <- select(master.us.cl, Date.d, Year, Month, Month.n)

master.stock <- left_join(global.stock.cl2, dates.sel, by=c("Year", "Month"))

write.csv(master.stock, file="global.stock.csv")

master.stock.complete <- na.omit(master.stock)
cor(master.stock.complete[2:9])


##### Global Master Dataset 

#gdp <- read.csv("GDP_Global.csv", stringsAsFactors = F)
#unemployment <- read.csv("Global_Harmonized_Unemployment.csv", stringsAsFactors = F)
#inflation <- read.csv("Inflation_Global_CPI.csv", stringsAsFactors = F)

#table(gdp$LOCATION)
#table(unemployment$LOCATION)
#table(inflation$LOCATION)




##
### Global World Bank Dataset 
##


gdp.w <- read.csv("WB_GDP_USD.csv", stringsAsFactors = F)
unemployment.w <- read.csv("WB_Unemply_Adj.csv", stringsAsFactors = F)
inflation.w <- read.csv("WB_CPI_Inflation.csv", stringsAsFactors = F)
gdp.change.w <- read.csv("WB_GDP_Growth.csv", stringsAsFactors = F)

world.raw <- bind_rows(gdp.w, unemployment.w, inflation.w, gdp.change.w)

world.raw <- world.raw %>% 
  filter(Country.Code != '')

### Round to 3 Decimal Points
worldbank.cl <- world.raw %>% 
  rename('Series' = 1) %>% 
  gather(Years, Value, 5:19) %>% 
  separate(Years, into = c("Year1", "Year2"), sep = "YR") %>% 
  mutate(Value.r = as.numeric(Value), Value.r2 = round(Value.r, digits = 3)) %>% 
  select(-Value, -Value.r) %>% 
  rename("Value"= Value.r2)
  

worldbank.cl$Year <- gsub("[.]", "", worldbank.cl$Year2)

world.bank.cl2 <- worldbank.cl %>% 
  select(-Year1, -Year2, -Series.Code) %>% 
  spread(Series, Value)

names(world.bank.cl2) <- c('Country.Name', 'Country.Code', 'Year', 'WB.GDP.USD', 'WB.GDP.Perc', 'WB.CPI.Inflation', 'WB.Unemplyment')


master.world <- world.bank.cl2

# Get Some Comparison Data

master.world.unemp <- master.world %>% 
  filter(as.numeric(Year) <= 2007) %>% 
  group_by(Country.Code, Country.Name) %>% 
  dplyr::summarise_all(funs(mean, median), na.rm=T) %>% 
  select(-contains('Year')) %>% 
  ungroup()
                            
master.world.comp <- merge(master.world, master.world.unemp, by=c("Country.Code", "Country.Name"))

master.world <- master.world.comp

# Add a filter for OECD Member countries...

oecd.list <- read.csv("OECD_List.csv", stringsAsFactors = F)

master.world.oecd <- merge(master.world, oecd.list, by = c("Country.Name"), all.x = T)

master.world.oecd.c <- master.world.oecd %>% 
  filter(OECD == "Yes" & Year == "2017")

# Need to add a high level filter for actual countries country.names

master.world <- master.world.oecd

#write.csv(master.world, file="master.world.csv")

# Comparison of oecd with world 

ggplot(filter(master.world, Year=="2009"), aes(x=WB.GDP.Perc)) + geom_density() + facet_wrap(OECD ~ .)

summary(master.world$WB.GDP.Perc[master.world$Year == "2009" & master.world$OECD == 'Yes'])
summary(master.world$WB.GDP.Perc[master.world$Year == "2009" & master.world$OECD == 'No'])

master.world$OECD[is.na(master.world$OECD)] <- "No"

master.world.08 <- master.world %>% 
  filter(Year == "2009") %>% 
  filter(!is.na(WB.GDP.Perc))
  
t.test(WB.GDP.Perc ~ OECD, data = master.world.08) # Significant from 2003 through 2014, biggest 09

gdp.growth.lines <- master.world %>% 
  group_by(Year, OECD) %>% 
  summarise(WB.GDP.Perc.M = mean(WB.GDP.Perc, na.rm = T), WB.CPI.Inflation.M = mean(WB.CPI.Inflation, na.rm = T))

ggplot(gdp.growth.lines, aes(as.numeric(Year), WB.GDP.Perc.M, colour = OECD)) + geom_line() + ggtitle('Change in GDP, OECD Disproportionately impacted by GFC')

master.world.08 <- master.world %>% 
  filter(Year == "2008") %>% 
  filter(!is.na(WB.CPI.Inflation))

t.test(WB.CPI.Inflation ~ OECD, data = master.world.08) # Inflation Significantly higher for Non OECD in 08



ggplot(gdp.growth.lines, aes(as.numeric(Year), WB.CPI.Inflation.M, colour = OECD)) + geom_line() + ggtitle('Change in Inflation, Non-OECD Disproportionately impacted by GFC')


# Relationship 
ggplot(master.world, aes(WB.Unemplyment, WB.CPI.Inflation, shape = OECD, color = OECD)) + 
  geom_point(size = 1.5) + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + ylim(-25, 25)

master.world.08.c <- na.omit(master.world.08)
cor(master.world.08.c$WB.Unemplyment, master.world.08.c$WB.CPI.Inflation)


