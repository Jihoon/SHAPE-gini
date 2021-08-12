library(tidyverse)
library(readxl)
library(countrycode)
library(units)
library(WDI)

data.path =  "./data/"

# ISO3 for "countries"
iso3 = setdiff(countrycode::codelist$iso3c, NA)

# WB thresholds per Country group (2020)
# Source: https://blogs.worldbank.org/opendata/new-world-bank-country-classifications-income-level-2021-2022
thres = c(b = 0, l = 1046, m = 4096, h = 12695, Inf) # GNI breaks for group classification
povline = c(LIC = 1.9, LMIC = 3.2, UMIC = 5.5, HIC = 21.7)

gni2020 = WDI(indicator =c("NY.GNP.PCAP.CD", # GNI per capita, PPP (current international $)
                           "NY.GNP.PCAP.KD", # GNI per capita, PPP (constant 2017 international $)
                           "NY.GDP.PCAP.CD", # GDP per capita, PPP (current international $)
                           "NY.GDP.PCAP.KD"  # GDP per capita, PPP (constant 2017 international $)
                           ), start = 2010, end=2020, extra=TRUE) %>% 
  rename(gni = NY.GNP.PCAP.CD) %>%
  filter(!is.na(iso3c), region!="Aggregates", year == 2020, !is.na(gni)) %>% 
  select(-c(capital:latitude, lending, iso2c)) %>% arrange(iso3c, -year) %>%
  mutate(inc.grp = cut(gni, breaks=thres, labels=FALSE)) %>% # Numeric poverty lines
  group_by(inc.grp) %>%
  mutate(min = min(gni), med = median(gni), max = max(gni)) %>%  # min/med/max of the GNI within each group
  mutate(grp.lower = pmax(inc.grp-1, 1)) %>% # The group below
  mutate(pov0 = povline[inc.grp], pov.low = povline[grp.lower]) %>%
  mutate(povline.adj = pmin((pov0-pov.low)/(med-min) * (gni-min) + pov.low, pov0)) %>%
  mutate(gdp.gni.const = NY.GDP.PCAP.KD/NY.GNP.PCAP.KD, gdp.gni.current = NY.GDP.PCAP.CD/gni)

# Income group characteristics based on 2020 categories
groups.2020 <- gni2020 %>% select(inc.grp, min, med, max) %>% unique()
names(groups.2020)[-1] <- paste0(names(groups.2020)[-1] ,'.2020')

ggplot(gni2020) +
  geom_point(aes(x = gni/365, y = povline.adj)) + 
  scale_x_continuous(trans='log') + 
  scale_y_continuous(trans='log')

gdp_data = read_delim(paste0(data.path, "release_v1p0/SHAPE_GDP_v1p0.mif"), delim=';') %>% select(-X25) %>%
  pivot_longer(cols = `2010`:`2100`, names_to = 'Year', values_to = 'GDP.bil') %>% select(-Variable, -Unit)

pop_data = read_delim(paste0(data.path, "release_v1p0/SHAPE_POP_v1p0.mif"), delim=';') %>% select(-X25) %>%
  pivot_longer(cols = `2010`:`2100`, names_to = 'Year', values_to = 'POP.mil') %>% select(-Model, -Scenario, -Variable, -Unit)

gdp_pcap = gdp_data %>% left_join(pop_data, by = c("Region", "Year")) %>%
  mutate(gdp.pcap = GDP.bil/POP.mil*1000) %>% rename(iso3c=Region)

# Households and NPISHs final consumption expenditure (% of GDP)
# Get the latest non-NA value, based on the assumption that it's not varying fast.
fin.con = WDI(indicator ="NE.CON.PRVT.ZS", latest=1, extra=TRUE) %>% 
  rename(final.cons.rate = NE.CON.PRVT.ZS) %>% mutate(final.cons.rate = final.cons.rate/100) %>%
  filter(!is.na(iso3c), region!="Aggregates", year > 2000) %>% 
  select(-c(capital:latitude, lending, iso2c)) %>% arrange(iso3c, -year) %>%
  select(iso3c, final.cons.rate)

master = gni2020 %>% left_join(fin.con) %>%
  mutate(povline.gdp = povline.adj/final.cons.rate) # in 2011 PPP, same as the original WB poverty line

a = master %>% ungroup() %>% select(country, gni, povline.adj, gdp.gni.current, iso3c) %>% right_join(gdp_pcap, by="iso3c") %>%
  rename(gni.2020 = gni, povline.2020 = povline.adj) %>%
  mutate(GNI.bil = GDP.bil / gdp.gni.current) %>%
  mutate(gni.pcap = GNI.bil / POP.mil * 1000) %>%
  mutate(inc.grp = cut(gni.pcap, breaks=thres, labels=FALSE)) %>%
  group_by(inc.grp) %>%
  mutate(min = min(gni.pcap), med = median(gni.pcap), max = max(gni.pcap)) %>%  # min/med/max of the GNI within each group
  left_join(groups.2020) %>%
  mutate(grp.lower = pmax(inc.grp-1, 1)) %>% # The group below
  mutate(povline0 = povline[inc.grp], povline.lower = povline[grp.lower]) %>%
  mutate(povline.adj = pmin((povline0-povline.lower)/(med-min) * (gni.pcap-min) + povline.lower, povline0)) 

