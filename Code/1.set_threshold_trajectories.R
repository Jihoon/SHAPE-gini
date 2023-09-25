library(patchwork)

library(here)
try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))
here::i_am("README.md")
setwd(here())
data.path <- paste0(here(), "/data/")
figure.path <- paste0(here(), "/figures/")


#### Income group identification ####

# # Income group characteristics based on 2020 categories
# groups.2020 <-
#   gni2020 %>% select(inc.grp, min, med, max) %>% unique()
# names(groups.2020)[-1] <- paste0(names(groups.2020)[-1] , '.2020')


### Set model parameters ====

# Lower bounds for SDPs
gini.lbound.innov <- 26.2 # 5% percentile (directly from povcalnet)
gini.lbound.serv <- 24.8  # 2% percentile
gini.lbound.soc <- 24     # 1% percentile

# Rate of Gini reduction (Unit?)
# dgini is -2.1% ~ 1.6%/year for 80% of observation (for 5yr spells)
# dgini is -2.8% ~ 2.1%/year for 90% of observation (for 5yr spells)
dgini.lbound.innov <- 0.0176  #(for >=10yr spells)
dgini.lbound.serv <- 0.0214
dgini.lbound.soc <- 0.0248


library(zoo)
# Harmonize/reformat gdp/pop data (+ Annualize)
gdp_annual = gdp_data %>%
  expand(iso3c, Year = 2010:2100, Scenario) %>%
  left_join(gdp_data) %>%
  arrange(iso3c, Scenario, Year) %>%
  mutate(Model = ifelse(run_mode=="SDP", "SDP", ssp)) %>%
  group_by(iso3c, Scenario) %>%
  mutate(GDP.bil = na.approx(GDP.bil))
pop_annual = pop_data %>%
  # select(Scenario) %>%
  expand(iso3c, Scenario, SubScenario = c("EI", "MC", "RC"), Year = 2010:2100) %>%
  left_join(pop_data) %>%
  mutate(SubScenario = ifelse(Scenario %in% c("SSP1", "SSP2"), NA, SubScenario)) %>%
  mutate(Scenario = coalesce(SubScenario, Scenario)) %>% 
  select(-SubScenario) %>% unique() %>%
  arrange(iso3c, Scenario, Year) %>%
  mutate(POP.mil = na.approx(POP.mil))

gdp_pcap = gdp_annual %>%
  left_join(pop_annual, by = c("iso3c", "Year", "Scenario")) %>%
  left_join(ppp.conv) %>% mutate(GDP.bil = GDP.bil * ppp.2005.to.2017) %>%
  mutate(gdp.pcap = GDP.bil / POP.mil * 1000) %>%
  mutate(Year = as.numeric(Year))

# Annual mean growth rate of countries
df.gr = gdp_pcap %>%
  mutate(GDP.pcap = GDP.bil / POP.mil) %>%
  group_by(iso3c, Scenario) %>%
  mutate(GDP.pcap.pre = lag(GDP.pcap)) %>%
  mutate(gr.r = (GDP.pcap / GDP.pcap.pre) - 1)



# Add HH expenditure per capita derived based on the latest share (NE.CON.PRVT.ZS)
master = gni2020 %>% left_join(fin.con) %>%
  mutate(hh.exp.pcap.KD = NY.GDP.PCAP.PP.KD * final.cons.rate) %>% # in 2017$ PPP
  left_join(ppp.conv) %>%
  mutate(hh.exp.pcap.avg.2020 = hh.exp.pcap.KD * ppp.2017.to.2011) %>% # in 2011$ PPP
  mutate(hh.exp.pcap.avg.day.2020 = hh.exp.pcap.avg.2020/365) %>% # in 2011$ PPP
  rename(gni.2020.atlas = gni.atlas) %>% ungroup() %>% 
  select(country, gni.2020.atlas, gdp.gni.ratio, iso3c, hh.exp.pcap.avg.2020, hh.exp.pcap.avg.day.2020) %>%
  right_join(gdp_pcap, by = c("iso3c"), multiple = "all") %>%
  
  mutate(GNI.bil = GDP.bil / gdp.gni.ratio) %>%   # GNI in Atlas method (from 2017 PPP)
  mutate(gni.pcap = GNI.bil / POP.mil * 1000) %>%
  mutate(inc.grp = cut(gni.2020.atlas, breaks = thres, labels = FALSE)) %>%
  group_by(inc.grp) %>%
  # mutate(
  #   min = min(gni.pcap),
  #   med = median(gni.pcap),
  #   max = max(gni.pcap)
  # ) %>%  # min/med/max of the GNI within each group
  # left_join(groups.2020) %>%
  mutate(grp.lower = pmax(inc.grp - 1, 1)) %>% # The group below
  # povline0: Original reference pov line given the GNI (WB)
  # mutate(povline0 = povline[inc.grp]) %>%
  group_by(country) %>% drop_na(gdp.pcap)


# Exclude Kiribati from the analysis
# Note: It has very low mean hh consumption (below povline) at the base year, 
# based on WDI's fin.con ratio and its large GNI (compared to GDP), thus high povline.
# But WDI's poverty rate gives only 22% for 2019 (https://data.worldbank.org/indicator/SI.POV.NAHC?locations=KI)
# master <- master %>% filter(!iso3c %in% c("KIR", "AZE"))

cty.grp <-
  gni2020 %>% select(iso3c, inc.grp) %>% mutate(inc.grp = factor(
    inc.grp,
    levels = c(1, 2, 3, 4),
    labels = c("LIC", "LMIC", "UMIC", "HIC")
  ))

# Derive avg HH exp projections from GDP trajectories by scenario
# Unit: HH and GDP in 2017$ PPP, GNI in Atlas current
df = master %>% ungroup() %>%
  select(-c(
    Model,
    gni.2020.atlas,
    GDP.bil,
    GNI.bil,
    gdp.gni.ratio,
    ppp.2005.to.2017,
    grp.lower
  )) %>%
  drop_na() %>%
  filter(Year >= 2020) %>%
  group_by(iso3c, Scenario) %>%
  mutate(gdp.pcap.pre = lag(gdp.pcap)) %>%
  mutate(gr.gdp = gdp.pcap / gdp.pcap.pre - 1) %>%
  replace_na(replace = list(gr.gdp = 0)) %>%
  mutate(gr.test = passthrough * gr.gdp + 1) %>% # HH expenditure growth rate
  # mutate(gr.hh.exp = gr.gdp * passthrough + 1, gr.test = (passthrough*((1+gr.gdp)^0.2-1)+1)^5) %>% # HH expenditure growth rate: The two are close values.
  mutate(hh.idx = cumprod(gr.test)) %>%
  mutate(hh.exp.pcap.avg = hh.exp.pcap.avg.2020 * hh.idx) %>% # in 2011$ PPP
  mutate(hh.exp.pcap.avg.day = hh.exp.pcap.avg / 365)



#### Derive Gini trajectories ####


# Estimate NPI target based on GNI/day
GNI.pov.relation = df %>% select(
  -c(
    hh.exp.pcap.avg.2020,
    gdp.pcap,
    inc.grp:hh.exp.pcap.avg
  )
) %>%
  # left_join(gini.wb) %>%
  left_join(gini.ssp) %>%
  mutate(gni.day = gni.pcap / 365) %>% select(-gni.pcap) %>%
  mutate(ln.GNI = log(gni.day)) %>%
  ungroup()

# Assume linear NPL estimation (model3 based on 2011 PPP NPL)
ln.NPI.predict = predict(model3, newdata = GNI.pov.relation, type = 'response')

# Raw (no lags) poverty line fit
df.povline <- GNI.pov.relation %>%
  
  # using the logistic or linear curve fit, derive the poverty line we will work with
  mutate(ln.NPI = ln.NPI.predict) %>%
  mutate(povline.trend = pmax(pov.lowest, exp(ln.NPI))) %>% # 2011 PPP
  left_join(cty.grp)
  
# Sample n countries from each income group not to over-crowd the figure
sample.cty <- cty.grp %>% group_by(inc.grp) %>% sample_n(10) %>% ungroup() %>% select(-c("inc.grp"))


# Function definition
source("./Code/1.1.create_pathways.R")



# Run the script ====

# JM removed this lag idea, because JM believes when a country assesses whether it achieved it or not at one point,
# it must be based on their present GDP/GNI and not on the GDP 10 year ago.
lag.period <- 0

df.gini = create_pathways(
  g.l.in  = gini.lbound.innov,
  g.l.se  = gini.lbound.serv,
  g.l.so  = gini.lbound.soc,
  dg.l.in = dgini.lbound.innov,
  dg.l.se = dgini.lbound.serv,
  dg.l.so = dgini.lbound.soc
)

# Identify infeasible countries by year ====
infsb <- list()
for (tgt in c("2050", "2070", "2100")) { 
  infsb[[tgt]] = df.gini %>%
    group_by(iso3c, Scenario) %>%
    filter(Year==as.numeric(tgt)) %>% filter(!years.ontrack, inc.grp != "HIC") %>%
    mutate(k1 = povline.trend.tgt * gini.floor / 
             (povline.trend.tgt * gini.floor - hh.exp.pcap.avg.day*(gini.floor - gini.tgt.trend))) %>% # Based on own calc
    mutate(k2 = povline.trend.tgt * gini.floor / hh.exp.pcap.avg.day / (gini.baseyr - gini.floor))  %>%
    mutate(hh.exp.pcap.avg.day.new = (k1-1)*(hh.exp.pcap.avg.day - povline.trend.tgt) + hh.exp.pcap.avg.day)
}

View(infsb[["2050"]])
View(infsb[["2070"]])
# Test plot for an individual country for examining the calculation in more detail ====
l.size = 1.5
scale.gdp = 0.3

col_EI = "plum4"
col_MC = "aquamarine3"
col_RC = "goldenrod1"
col_SSP1 = "royalblue1"
col_SSP2 = "violetred1"

scenario_cmap <- c("EI" = col_EI,
                   "MC" = col_MC,
                   "RC" = col_RC,
                   "SSP1" = col_SSP1,
                   "SSP2" = col_SSP2)

              
cty = "PRT"
ggplot(data = df.gini %>% filter(iso3c %in% c(cty)), aes(x = Year)) +
  geom_line(aes(
    y = gini.tgt.trend,
    group = interaction(country, Scenario),
    color = Scenario
  ),
  size = l.size * 0.3)  +
  geom_line(aes(
    y = gini.realised.trend,
    group = interaction(country, Scenario),
    color = Scenario
  ),
  size = l.size)  +
  geom_line(aes(y=gini.floor, group=interaction(country, Scenario), color=Scenario), size=l.size*0.5)  +
  # geom_line(data=df.gr %>% filter(iso3c %in% c(cty)),
  #           aes(y=gdp.pcap/scale.gdp/365, group=interaction(iso3c, Scenario), color=Scenario), linetype = "dotted", size=l.size) +
  # geom_line(data=df.gr %>% filter(iso3c %in% c(cty)),
  #           aes(y=gr.r*600, group=interaction(iso3c, Scenario), color=Scenario), linetype = "dashed", size=l.size) +
  geom_line(
    aes(
      y = povline.trend.tgt / scale.gdp,
      group = interaction(iso3c, Scenario),
      color = Scenario
    ),
    linetype = "dashed",
    size = l.size
  ) +
  geom_hline(yintercept = gini.lbound.innov[2],
             linetype = "dashed",
             color = col_EI) +
  geom_hline(yintercept = gini.lbound.serv[2],
             linetype = "dashed",
             color = col_RC) +
  geom_hline(yintercept = gini.lbound.soc[2],
             linetype = "dashed",
             color = col_MC) +
  geom_text(
    data = df.result %>%
      left_join(df) %>%
      filter(Year == 2020, iso3c %in% c(cty),
             Scenario == "EI"),
    aes(
      x = 2100,
      y = hh.exp.pcap.avg.day / scale.gdp,
      label = paste0(
        "Current mean expenditure/cap:\n",
        format(hh.exp.pcap.avg.day, digits = 3),
        "($/day)"
      )
    ),
    hjust = 1
  ) +
  scale_y_continuous(
    # Features of the first axis
    name = "Gini",
    breaks = seq(0, 50, 10),
    
    # Add a second axis and specify its features
    sec.axis = sec_axis( ~ . * scale.gdp, name = "Poverty line ($/day)", breaks = seq(0, max(
      df.result$povline.trend.tgt
    ) + 1, 2))
    # sec.axis = sec_axis(~.*scale.gdp, name="Daily GDP per capita")
  ) +
  scale_x_continuous(breaks = seq(2020, 2100, 5), minor_breaks = NULL) +
  labs(title = cty) +
  theme_bw() +
  annotate(geom = "text",
           x = 2030,
           y = 17,
           label = "Target Gini") +
  annotate(geom = "text",
           x = 2038,
           y = 23,
           label = "Historical Gini minimum") +
  annotate(geom = "text",
           x = 2070,
           y = 18,
           label = "Poverty line ($/day)") +
  # annotate(geom="text", x=2020, y=34, label="Gini projected", hjust = 0, fontface = "bold") +
  scale_color_manual(values = c(col_EI, col_MC, col_RC, col_SSP1, col_SSP2))

# Just about Gini (no second axis) ====

ggplot(data = df.gini %>% filter(iso3c %in% c(cty)), aes(x = Year)) +
  geom_line(aes(
    y = gini.tgt.trend,
    group = interaction(country, Scenario),
    color = Scenario
  ),
  size = l.size * 0.3)  +
  geom_line(aes(
    y = gini.realised.trend,
    group = interaction(country, Scenario),
    color = Scenario
  ),
  size = l.size)  +
  geom_line(aes(
    y = gini.floor,
    group = interaction(country, Scenario),
    color = Scenario
  ),
  linetype = "dashed",
  size = l.size * 0.5)  +
  # geom_line(data=df.gr %>% filter(iso3c %in% c(cty)),
  #           aes(y=gdp.pcap/scale.gdp/365, group=interaction(iso3c, Scenario), color=Scenario), linetype = "dotted", size=l.size) +
  # geom_line(data=df.gr %>% filter(iso3c %in% c(cty)),
  #           aes(y=gr.r*600, group=interaction(iso3c, Scenario), color=Scenario), linetype = "dashed", size=l.size) +
  # geom_line(
  #   aes(
  #     y = povline.trend.tgt / scale.gdp,
  #     group = interaction(iso3c, Scenario),
  #     color = Scenario
  #   ),
  #   linetype = "dashed",
  #   size = l.size
  # ) +
  geom_hline(yintercept = gini.lbound.innov[2],
             linetype = "dashed",
             color = col_EI) +
  geom_hline(yintercept = gini.lbound.serv[2],
             linetype = "dashed",
             color = col_RC) +
  geom_hline(yintercept = gini.lbound.soc[2],
             linetype = "dashed",
             color = col_MC) +
  geom_text(
    data = df.result %>%
      left_join(df) %>%
      filter(Year == 2020, iso3c %in% c(cty),
             Scenario == "EI"),
    aes(
      x = 2100,
      y = hh.exp.pcap.avg.day / scale.gdp,
      label = paste0(
        "Current mean expenditure/cap:\n",
        format(hh.exp.pcap.avg.day, digits = 3),
        "($/day)"
      )
    ),
    hjust = 1
  ) +
  scale_y_continuous(
    # Features of the first axis
    name = "Gini",
    breaks = seq(20, 40, 2)
  ) +
  scale_x_continuous(breaks = seq(2020, 2100, 10), minor_breaks = NULL) +
  labs(title = cty) +
  theme_bw() +
  annotate(geom = "text",
           x = 2080,
           y = 31,
           label = "Aspirational traj.") +
  annotate(geom = "text",
           x = 2070,
           y = 28,
           label = "Realistic traj.") +
  annotate(geom = "text",
           x = 2060,
           y = 23,
           label = "Historical Gini minimum") +
  # annotate(geom="text", x=2020, y=34, label="Gini projected", hjust = 0, fontface = "bold") +
  scale_color_manual(values = c(col_EI, col_MC, col_RC))



# Export Gini to IAMC format
df.export = df.result %>%
  pivot_wider(
    id_cols = c(iso3c:Scenario, tgt.achieved),
    values_from = gini.realised.trend,
    names_from = Year
  ) %>%
  mutate(Model = "SHAPE_Gini",
         Unit = NA,
         Variable = "Gini") %>%
  left_join(gini.ssp %>% select(iso3c, `Base gini imputed` = imputed.gini, gini.baseyr)) %>%
  mutate(`2020` = coalesce(`2020`, gini.baseyr)) %>%
  left_join(
    fin.con %>% select(iso3c, `Share of final consumption among GDP imputed` =
                         imputed.fin.con.r)
  ) %>%
  select(
    Model,
    Scenario,
    Region = iso3c,
    Variable,
    Unit,
    `2020`:`2100`,
    everything(),
    `Absolute target achieved` = tgt.achieved,-gini.baseyr
  )

# write_delim(df.export, file="SHAPE_Gini_v1p0.csv", delim=',') # for >=5 year spells
write_delim(df.export, file = "SHAPE_Gini_v1p2_annual.csv", delim = ',') # for >=10 year spells
