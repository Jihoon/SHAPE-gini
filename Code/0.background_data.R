library(tidyverse)
library(readxl)
library(countrycode)
library(units)
library(WDI)
library(scales)

library(here)
try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))
here::i_am("README.md")
setwd(here())
data.path =  paste0(here(), "/data/")

# Standard ISO3 for "countries"
iso3 = setdiff(countrycode::codelist$iso3c, NA)

# WB thresholds per Country group (2020)
# Source: https://blogs.worldbank.org/opendata/new-world-bank-country-classifications-income-level-2021-2022
thres = c(b = 0, l = 1046, m = 4096, h = 12695, Inf) # GNI breaks for group classification
povline = c(LIC = 1.9, LMIC = 3.2, UMIC = 5.5, HIC = 21.7)

# Set up 2020 base year GNI/GDP data from WDI
gni2020 = WDI(indicator =c("NY.GNP.PCAP.CD", # GNI per capita, Atlas method (current US$)
                           # "NY.GNP.PCAP.KD", # GNI per capita, PPP (constant 2017 international $)
                           
                           "NY.GDP.PCAP.PP.CD", # GDP per capita, PPP (current international $)
                           "NY.GDP.PCAP.PP.KD", # GDP per capita, PPP (constant 2017 international $)
                           "NY.GDP.MKTP.PP.KD"  # GDP, PPP (constant 2017 international $)
                           ), start = 2010, end=2020, extra=TRUE) %>% 
  rename(gni = NY.GNP.PCAP.CD) %>%
  filter(!is.na(iso3c), region!="Aggregates", year %in% c(2019, 2020), !is.na(gni)) %>%  # some countries without 2020 GNI
  select(-c(capital:latitude, lending, iso2c)) %>% arrange(iso3c, -year) %>%
  group_by(country, iso3c) %>%
  summarise(across(gni:NY.GDP.MKTP.PP.KD, mean)) %>% ungroup() %>%
  
  mutate(inc.grp = cut(gni, breaks=thres, labels=FALSE)) %>% # Numeric poverty lines
  group_by(inc.grp) %>%
  mutate(min = min(gni), med = median(gni), max = max(gni)) %>%  # min/med/max of the GNI within each group
  mutate(grp.lower = pmax(inc.grp-1, 1)) %>% # The group below
  mutate(pov0 = povline[inc.grp], pov.low = povline[grp.lower]) %>%
  mutate(povline.adj = pmin((pov0-pov.low)/(med-min) * (gni-min) + pov.low, pov0)) %>%
  mutate(gdp.gni.ratio = NY.GDP.PCAP.PP.KD/gni) # Between 2017 PPP$ and current Atlas

# Gini WDI avg (2015-)
gini.wb = WDI(indicator="SI.POV.GINI", start = 2015, extra=TRUE) %>% 
  filter(iso3c %in% iso3) %>% arrange(iso3c, year) %>% 
  group_by(country, iso3c) %>%
  summarise(gini.baseyr = mean(SI.POV.GINI, na.rm = TRUE)) %>%
  drop_na()

# Alternative baseline Ginis from SSP1
gini.ssp1 = read.csv("P:/ene.general/DecentLivingEnergy/DLE_scaleup/Data/gdp_gini_pop_ssp.csv") %>%
  filter(scenario=="SSP1", year==2020) %>% 
  select(iso3c=country, gini.baseyr=gini)

# MESSAGE regions (R-11)
reg.MSG = read_xlsx("P:/ene.general/DecentLivingEnergy/DLE_scaleup/Data/iso_region_MESSAGE.xlsx") %>% 
  rename(iso3c = iso, reg.MSG = "MESSAGE-GLOBIOM") %>% mutate(iso3c = toupper(iso3c)) %>%
  select(iso3c, reg.MSG)

gini.MSG.median = reg.MSG %>%
  left_join(gini.ssp1) %>% drop_na() %>%
  group_by(reg.MSG) %>% summarise(gini.median = median(gini.baseyr))

gini.ssp1 = reg.MSG %>% left_join(gini.ssp1) %>% left_join(gini.MSG.median) %>%
  mutate(imputed.gini = is.na(gini.baseyr)) %>%
  mutate(gini.baseyr = coalesce(gini.baseyr, gini.median)) %>%
  select(-gini.median)

# GDP conversion between 2005$ PPP and 2017$ PPP
ppp.conv = WDI(indicator = c("PA.NUS.PPP", "NY.GDP.DEFL.ZS"), start = 2005, end=2017, extra=TRUE) %>%
  filter(year %in% c(2005, 2011, 2017), iso3c %in% iso3) %>% 
  pivot_wider(names_from = year, values_from = c(PA.NUS.PPP, NY.GDP.DEFL.ZS)) %>% 
  mutate(ppp.2005.to.2017 = PA.NUS.PPP_2005*NY.GDP.DEFL.ZS_2017/NY.GDP.DEFL.ZS_2005/PA.NUS.PPP_2017) %>%
  mutate(ppp.2017.to.2011 = PA.NUS.PPP_2017*NY.GDP.DEFL.ZS_2011/NY.GDP.DEFL.ZS_2017/PA.NUS.PPP_2011) %>%
  select(iso3c, ppp.2005.to.2017, ppp.2017.to.2011)

# Households and NPISHs final consumption expenditure (% of GDP)
# Get the latest non-NA value, based on the assumption that it's not varying fast.
# Note: Forget NPISH, which is only reported in EU countries
fin.con = WDI(indicator ="NE.CON.PRVT.ZS", latest=1, extra=TRUE) %>%  
  rename(final.cons.rate = NE.CON.PRVT.ZS) %>% mutate(final.cons.rate = final.cons.rate/100) %>%
  filter(!is.na(iso3c), region!="Aggregates", year > 2000) %>% 
  select(-c(capital:latitude, lending, iso2c)) %>% arrange(iso3c, -year) %>%
  select(iso3c, final.cons.rate)

fin.con.median = fin.con %>% right_join(reg.MSG) %>% drop_na() %>%
  group_by(reg.MSG) %>% summarise(final.cons.MSG.median = median(final.cons.rate))
  
fin.con = reg.MSG %>% left_join(fin.con) %>% left_join(fin.con.median) %>%
  mutate(imputed.fin.con.r = is.na(final.cons.rate)) %>%
  mutate(final.cons.rate = coalesce(final.cons.rate, final.cons.MSG.median)) %>%
  select(-final.cons.MSG.median)

# Passthrough rate taken from Lakner et al. 2019
passthrough = 0.83

# Historical GDP/GNI vs poverty line by country (2011$ PPP or Atlas (GNI))
hist = read_xlsx(paste0(data.path, "Historical_poverty_lines_WB.xlsx"), skip=5) %>%
  mutate(ln.NPL = log(NPL), ln.GNI = log(GNI)) %>%
  mutate(ln.NPL.normalized = (ln.NPL - min(ln.NPL))/(max(ln.NPL) - min(ln.NPL)))

# Estimate different S-ish models
model1 <- glm(ln.NPL.normalized ~ ln.GNI, family=binomial(link='logit'), data=hist)
model2 <- nls(ln.NPL ~ a*exp(-b*exp(- c*ln.GNI)), data=hist, start=list(a=50, b=1, c=3)) # Gomperzt (Sigmoid)
model3 <- lm(ln.NPL ~ ln.GNI, data=hist) 
# model2 <- nls(ln.NPL ~ a/(1 + exp(-b * (ln.GNI-c)) ), data=hist, start=list(a=5, b=1, c=3))

# Fitted curves
fitted.results1 <- predict(model1, newdata=hist, type='response')
fitted.results2 <- predict(model2, newdata=hist, type='response')
fitted.results3 <- predict(model3, newdata=hist, type='response')  

# Plot obs and predictions
df.test = hist %>% 
  mutate(ln.fit1 = fitted.results1 * (max(ln.NPL) - min(ln.NPL)) + min(ln.NPL)) %>%
  mutate(ln.fit2 = fitted.results2) %>%
  mutate(ln.fit3 = fitted.results3) %>%
  mutate(fit1 = exp(ln.fit1), fit2 = exp(ln.fit2), fit3 = exp(ln.fit3))
ggplot(data = df.test) +
  geom_point(aes(GNI, NPL)) + # Observations
  scale_x_continuous(trans='log', breaks=scales::trans_breaks("log", function(x) 2^x)) +
  scale_y_continuous(trans='log', breaks=scales::trans_breaks("log", function(x) 2^x)) +
  geom_line(aes(GNI, pmax(1.9, fit1), color='Logistic'), size=1.5) +
  # geom_line(aes(GNI, fit2, color='Gompertz'), size=1.5) +
  geom_line(aes(GNI, pmax(1.9, fit3), color='Linear'), size=1.5) +
  theme_bw() +
  labs(x="GNI per capita per day (2011 Atlas USD)", y="National poverty line per day (2011 PPP)")

# Test plot to observe the behavior
gni.test = seq(1, 3600, 1)
pred1 = predict(model1, newdata=data.frame(GNI = gni.test, ln.GNI=log(gni.test)), type='response')
pred1 = exp(pred1 * (max(hist$ln.NPL) - min(hist$ln.NPL)) + min(hist$ln.NPL))
pred2 = exp(predict(model2, newdata=data.frame(ln.GNI=log(gni.test)), type='response'))

df.plot = data.frame(g=gni.test,n=pred1) %>%
  mutate(ln.g = log(g), ln.n = log(n))
ggplot(data=df.plot) + geom_line(aes(x=g, y=n))+ 
  scale_x_continuous(trans='log') +
  scale_y_continuous(trans='log')

# Model2 makes more sense in that the richer will get more demanding NPL.

