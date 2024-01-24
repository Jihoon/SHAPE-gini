library(tidyverse)
library(readxl)
library(countrycode)
library(units)
library(WDI)
library(scales)
library(ggrepel)

library(here)
try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))
here::i_am("README.md")
setwd(here())
data.path =  paste0(here(), "/data/")

# Standard ISO3 for "countries"
iso3 = setdiff(countrycode::codelist$iso3c, NA)

# Basic setup
# ssp = "SSP1"
run_mode = "SDP"  #"SSP"

# WB thresholds per Country group (2020)
# Source: https://blogs.worldbank.org/opendata/new-world-bank-country-classifications-income-level-2021-2022
# thres = c(b = 0, l = 1046, m = 4096, h = 12695, Inf) # GNI breaks for group classification
# povline = c(LIC = 1.9, LMIC = 3.2, UMIC = 5.5, HIC = 21.7)

# Update with a newer source (Sep 2023): https://blogs.worldbank.org/opendata/new-world-bank-group-country-classifications-income-level-fy24
thres = c(b = 0, l = 1085, m = 4255, h = 13205, Inf) # GNI breaks for group classification
povline = c(LIC = 2.15, LMIC = 3.65, UMIC = 6.85, HIC = 24) # HIC arbitrary (it is not included in the latest release) PPP2017
pov.lowest = as.numeric(povline["LIC"])

# Set up 2020 base year GNI/GDP data from WDI
gni2020 = WDI(indicator =c("NY.GNP.PCAP.CD", # GNI per capita, Atlas method (current US$)
                           # "NY.GNP.PCAP.KD", # GNI per capita, PPP (constant 2017 international $)
                           
                           "NY.GDP.PCAP.PP.CD", # GDP per capita, PPP (current international $)
                           "NY.GDP.PCAP.PP.KD", # GDP per capita, PPP (constant 2017 international $)
                           "NY.GDP.MKTP.PP.KD"  # GDP, PPP (constant 2017 international $)
                           ), start = 2010, end=2020, extra=TRUE) %>% 
  rename(gni.atlas = NY.GNP.PCAP.CD) %>%
  filter(!is.na(iso3c), region!="Aggregates", year %in% c(2019, 2020), !is.na(gni.atlas)) %>%  # some countries without 2020 GNI
  select(-c(capital:latitude, lending, iso2c)) %>% arrange(iso3c, -year) %>%
  group_by(country, iso3c) %>%
  summarise(across(gni.atlas:NY.GDP.MKTP.PP.KD, mean)) %>% ungroup() %>%
  
  mutate(inc.grp = cut(gni.atlas, breaks=thres, labels=FALSE)) %>% # Numeric poverty lines
  group_by(inc.grp) %>%
  mutate(min = min(gni.atlas), med = median(gni.atlas), max = max(gni.atlas)) %>%  # min/med/max of the GNI within each group
  # mutate(grp.lower = pmax(inc.grp-1, 1)) %>% # The group below
  mutate(gdp.gni.ratio = NY.GDP.PCAP.PP.KD/gni.atlas) # Between 2017 PPP$ and current Atlas

# Gini WDI avg of years since 2015
gini.wb = WDI(indicator="SI.POV.GINI", start = 2015, extra=TRUE) %>% 
  filter(iso3c %in% iso3) %>% arrange(iso3c, year) %>% 
  group_by(country, iso3c) %>%
  summarise(gini.baseyr = mean(SI.POV.GINI, na.rm = TRUE)) %>%
  drop_na()

# Assign countries into WB income groups
cty.grp <-
  gni2020 %>% select(iso3c, inc.grp) %>% mutate(inc.grp = factor(
    inc.grp,
    levels = c(1, 2, 3, 4),
    labels = c("LIC", "LMIC", "UMIC", "HIC")
  ))

### SSP pop/GDP data import ==== 
df.ssp = read.csv("P:/ene.general/DecentLivingEnergy/DLE_scaleup/Data/gdp_gini_pop_ssp.csv") %>%
  rename(iso3c=country)

# Alternative base year Ginis from SSP (For SDP use. this has more countries than the WB source. SSP1 by default)
gini.ssp = df.ssp %>%
  # filter(year==2020) %>%
  filter(scenario=="SSP1", year==2020) %>%
  select(iso3c, gini.baseyr=gini) %>%
  full_join(gini.wb %>% rename(gini.baseyr.imp = gini.baseyr)) %>%
  mutate(gini.baseyr = coalesce(gini.baseyr, gini.baseyr.imp)) %>%
  select(-c(country, gini.baseyr.imp))


### SDP pop/GDP data import ==== 

# Read in PIK data on GDP and population, and derive GDP/cap (and clean)
# Convert 2005$ PPP to 2017$ PPP (as in WDI)
gdp_data = read_delim(paste0(data.path, "release_v1p2_update/SHAPE_GDP_v1p2_withSSPs-Sep2023Update_fixssp2HRV.mif"), 
                      delim = ';') %>%
  select(-...25) %>%
  pivot_longer(cols = `2010`:`2100`,
               names_to = 'Year',
               values_to = 'GDP.bil') %>%
  select(-Variable,-Unit, -Model) %>%
  rename(iso3c = Region) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(Scenario = ifelse(
    Scenario == "innovation", "EI",
    ifelse(Scenario == "service", "MC",
           ifelse(Scenario == "society", "RC", Scenario)
  )))
pop_data = read_delim(paste0(data.path, "release_v1p2_update/SHAPE_POP_v1p2_withSSPs-Sep2023Update_fixssp2CYP.mif"),
                      delim = ';') %>%
  select(-...25) %>%
  rename(iso3c = Region) %>%
  pivot_longer(cols = `2010`:`2100`,
               names_to = 'Year',
               values_to = 'POP.mil') %>%
  select(-Model, -Variable, -Unit) %>%
  mutate(Year = as.numeric(Year)) 


# MESSAGE region definitions (R-11)
reg.MSG = read_xlsx("P:/ene.general/DecentLivingEnergy/DLE_scaleup/Data/iso_region_MESSAGE.xlsx") %>% 
  rename(iso3c = iso, reg.MSG = "MESSAGE-GLOBIOM") %>% mutate(iso3c = toupper(iso3c)) %>%
  select(iso3c, reg.MSG)

# Get median Gini for each one of R11
gini.MSG.median = reg.MSG %>%
  left_join(gini.ssp) %>% drop_na() %>%
  group_by(reg.MSG) %>% summarise(gini.median = median(gini.baseyr))

# Combine base year Gini for all countries (both imputed and wdi)
gini.ssp = reg.MSG %>% left_join(gini.ssp) %>% left_join(gini.MSG.median) %>%
  mutate(imputed.gini = is.na(gini.baseyr)) %>%
  mutate(gini.baseyr = coalesce(gini.baseyr, gini.median)) %>%
  select(-gini.median)

# GDP conversion between 2005$ PPP and 2017$ PPP
# (necessary since PIK GDP is in 2005$ PPP)
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
passthrough = 0.85

# Historical GDP/GNI vs poverty line by country (Sourc: 2011$ PPP or Atlas (GNI))
# To use the $2.15 poverty threshold, I had to convert the 2011 PPP to 2017.
# So the regression from 'model3' gives 2017 PPP NPL.
hist = read_xlsx(paste0(data.path, "Historical_poverty_lines_WB.xlsx"), skip=5) %>%
  mutate(iso3c = countrycode(Country, origin = 'country.name', destination = 'iso3c')) %>%
  left_join(ppp.conv) %>%
  mutate(NPL_ppp2017 = NPL/ppp.2017.to.2011) %>%
  mutate(ln.NPL = log(NPL_ppp2017), ln.GNI = log(GNI))

# Estimate different S-ish models
model3 <- lm(ln.NPL ~ ln.GNI, data=hist) 

# Fitted curves
fitted.results3 <- predict(model3, newdata=hist, type='response')  

# Plot obs and predictions
df.test = hist %>% 
  mutate(ln.fit3 = fitted.results3) %>%
  mutate(fit3 = exp(ln.fit3)) %>% 
  left_join(pop_data %>% filter(Year==2010, Scenario=="all_SHAPE_SDPs") %>% select(iso3c, POP.mil)) %>%
  arrange(POP.mil)


# Figure XX in the paper
ggplot(data = df.test, aes(GNI, NPL_ppp2017, label = Country)) +
  geom_point() + # Observations
  scale_x_continuous(trans='log', breaks=scales::trans_breaks("log", function(x) 2^x)) +
  scale_y_continuous(trans='log', breaks=scales::trans_breaks("log", function(x) 2^x)) +
  # geom_line(aes(GNI, pmax(pov.lowest, fit1), color='Logistic'), size=1.5) +
  geom_line(aes(GNI, pmax(pov.lowest, fit3), color='Linear'), size=1.5) +
  geom_text_repel(data = df.test %>% filter(POP.mil > 30), min.segment.length=0.1) +
  theme_bw() +
  labs(x="GNI per capita per day (2011 Atlas USD)", y="National poverty line per day (2017 PPP)") +
  theme(legend.position="none")

# # Test plot to observe the behavior
# gni.test = seq(1, 3600, 1)
# pred1 = predict(model1, newdata=data.frame(GNI = gni.test, ln.GNI=log(gni.test)), type='response')
# pred1 = exp(pred1 * (max(hist$ln.NPL) - min(hist$ln.NPL)) + min(hist$ln.NPL))
# pred2 = exp(predict(model2, newdata=data.frame(ln.GNI=log(gni.test)), type='response'))
# 
# df.plot = data.frame(g=gni.test,n=pred2) %>%
#   mutate(ln.g = log(g), ln.n = log(n))
# ggplot(data=df.plot) + geom_line(aes(x=g, y=n))+ 
#   scale_x_continuous(trans='log') +
#   scale_y_continuous(trans='log')
# 
# # Model2 makes more sense in that the richer will get more demanding NPL.

# Validation of transfer amount at the latest year ====
df_pov = WDI(indicator = 
               c("SI.POV.GAPS", # Poverty gap at $2.15 a day (2017 PPP) (%)
                 "SP.POP.TOTL", # Population total
                 "SI.POV.NAHC"  # Poverty headcount ratio at national poverty lines (% of population)
               ), 
             start = 2015, end=2020, extra=TRUE)
df_pov %>% filter(grepl("Low & middle income", country)) %>%
  mutate(transfer = SI.POV.GAPS/100*SP.POP.TOTL*2.15*365/1e9)

# Retrieve pov ratio at NPL
df_povhc_raw = WDI(indicator = "SI.POV.NAHC",  # Poverty headcount ratio at national poverty lines (% of population)
               latest=3, extra=TRUE) %>%
  select(country, iso3c, year, SI.POV.NAHC)
write_csv(df_povhc_raw %>% left_join(cty.grp), "povhc_rate.csv")
df_povhc = df_povhc_raw %>%
  group_by(country, iso3c) %>%
  summarise(povratio = mean(SI.POV.NAHC), years=max(year)-min(year), diff=max(SI.POV.NAHC)-min(SI.POV.NAHC)) %>% 
  ungroup() %>%
  left_join(cty.grp)

mean((df_povhc %>% filter(inc.grp!="HIC"))$povratio) # Mean is around 30%
p_povhc = ggplot(df_povhc_raw %>% left_join(cty.grp) %>% filter(!is.na(inc.grp)) %>% rename("Income group"=inc.grp),
       aes(x=year, y=SI.POV.NAHC)) +
  geom_line(aes(group=iso3c), color="grey", alpha=0.5) +
  geom_jitter(aes(shape=`Income group`, colour=`Income group`)) +
  labs(y="Poverty headcount ratio at national poverty lines (%)", colour="Income group") +
  scale_color_brewer(palette="Paired") +theme_bw()
ggsave(
  plot = p_povhc,
  filename = paste0(figure.path, "poverty headcount.png"),
  width = 20,
  height = 15,
  dpi = 300,
  units = "cm"
)
