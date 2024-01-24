library(patchwork)
library(zoo) # for 'na.approx'
library(RColorBrewer)

library(here)
try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))
here::i_am("README.md")
setwd(here())
data.path <- paste0(here(), "/data/")
figure.path <- paste0(here(), "/figures/")


### Set model parameters ====

# Lower bounds for SDPs
gini.lbound.innov <- 26.2 # 5% percentile (directly from povcalnet)
gini.lbound.serv <- 24.8  # 2% percentile
gini.lbound.soc <- 24     # 1% percentile

# Rate of Gini reduction (% of Gini values)
dgini.lbound.innov <- 0.0176  
dgini.lbound.serv <- 0.0214
dgini.lbound.soc <- 0.0248



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
  
  mutate(GNI.bil = GDP.bil / gdp.gni.ratio) %>%   # GNI now in current Atlas method (from 2017 PPP)
  mutate(gni.pcap = GNI.bil / POP.mil * 1000) %>%
  mutate(inc.grp = cut(gni.2020.atlas, breaks = thres, labels = FALSE)) %>%
  group_by(inc.grp) %>%
  # mutate(grp.lower = pmax(inc.grp - 1, 1)) %>% # The group below
  group_by(country) %>% drop_na(gdp.pcap)


# Exclude Kiribati from the analysis
# Note: It has very low mean hh consumption (below povline) at the base year, 
# based on WDI's fin.con ratio and its large GNI (compared to GDP), thus high povline.
# But WDI's poverty rate gives only 22% for 2019 (https://data.worldbank.org/indicator/SI.POV.NAHC?locations=KI)
# master <- master %>% filter(!iso3c %in% c("KIR", "AZE"))


# Derive avg HH exp projections from GDP trajectories by scenario
# Unit: HH and GDP in 2017$ PPP, GNI in Atlas current
df = master %>% ungroup() %>%
  select(-c(
    Model,
    gni.2020.atlas,
    GDP.bil,
    GNI.bil,
    gdp.gni.ratio,
    ppp.2005.to.2017
    # grp.lower
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



### Derive Gini trajectories ====


# Estimate NPI target based on GNI/day
GNI.pov.relation = df %>% select(
  -c(
    # hh.exp.pcap.avg.2020,
    gdp.pcap,
    inc.grp:hh.exp.pcap.avg
  )
) %>%
  # left_join(gini.wb) %>%
  left_join(gini.ssp) %>%
  mutate(gni.day = gni.pcap / 365) %>% select(-gni.pcap) %>% # gni.pcap in 'current Atlas', how is it different '2011 Atlas'?
  mutate(ln.GNI = log(gni.day)) %>%
  ungroup()

# Assume linear NPL estimation (model3 based on 2011 PPP NPL)-> Now 2017 PPP (Dec 1 2023)
ln.NPI.predict = predict(model3, newdata = GNI.pov.relation, type = 'response')

# Raw (no lags) poverty line fit
df.povline <- GNI.pov.relation %>%
  
  # using the logistic or linear curve fit, derive the poverty line we will work with
  mutate(ln.NPI = ln.NPI.predict) %>%
  mutate(povline.trend = pmax(pov.lowest, exp(ln.NPI)) * ppp.2017.to.2011) %>% # 2011 PPP 
  left_join(cty.grp)
  
# Function definition
source("./Code/1.1.create_pathways.R")



# Run the main function  ====

# JM removed this lag idea, because JM believes when a country assesses whether it achieved it or not at one point,
# it must be based on their present GDP/GNI and not on the GDP 10 year ago.
lag.period <- 0

l.output = create_pathways(
  g.l.in  = gini.lbound.innov,
  g.l.se  = gini.lbound.serv,
  g.l.so  = gini.lbound.soc,
  dg.l.in = dgini.lbound.innov,
  dg.l.se = dgini.lbound.serv,
  dg.l.so = dgini.lbound.soc
)

realised_gini = l.output[[2]]

# Add 'SDP' prefix to the scenario name
realised_gini = realised_gini %>%
  mutate(Scenario = ifelse(grepl("SSP", Scenario), Scenario, paste0('SDP-', Scenario)))

write_csv(realised_gini %>%
            left_join(gdp_annual %>%
                        mutate(Scenario = ifelse(grepl("SSP", Scenario), Scenario, paste0('SDP-', Scenario))) %>% 
                        select(-Model)) %>% 
            select(Country=country, iso3c, 
                   Scenario, Year, 
                   `GDP pathway ($bil)` = GDP.bil,
                   `Gini pathway` = gini.realised.trend,
                   `Year when SDG1 achieved`=year.abstgt.achieved) %>%
            mutate(across(where(is.numeric), ~ round(., 1))), 
          'Supplimentary table - Gini_GDP.csv')

# Sample 7 countries with biggest population in each income group
sample.cty = pop_data %>% filter(Year == 2020, Scenario == "SSP1") %>% 
  left_join(cty.grp) %>% group_by(inc.grp) %>% 
  drop_na() %>% arrange(-POP.mil, inc.grp) %>% slice(1:7) %>% 
  ungroup() %>% select(iso3c)

# Make plotss
plot_povline(realised_gini)
lookup <- c(Year = "year", Scenario = "scenario", gini.ssp = "gini")
p_gini = plot_gini(realised_gini, 
          df.ssp = df.ssp %>% 
            rename(all_of(lookup)) %>% 
            select(-pop_mil, -gdp_ppp_2005_bil))

# Identify infeasible countries by year ====
infsb <- list()
tot.transfer <- list()
r_pov_impute = 0.3
for (tgt in seq(2020, 2100)) {
# for (tgt in c(2020)) {
  infsb.single = realised_gini %>%
    group_by(iso3c, Scenario) %>%
    filter(Year==tgt) %>% 
    filter(!years.ontrack, inc.grp != "HIC", !iso3c %in% c("AZE", "UKR") ) %>%
    # Merge base poverty ratio at NPL
    left_join(df_povhc %>% select(iso3c, povratio) %>% mutate(povratio = povratio/100)) %>%
    # Impute for countries missing pov ratio (default=0.3)
    replace_na(list(povratio=r_pov_impute)) %>% 
    # Interpolate the linearly decreasing pov ratio
    replace_na(list(year.abstgt.achieved = 2150)) %>%
    mutate(povratio=povratio * (year.abstgt.achieved-Year) / (year.abstgt.achieved-2020)) %>%
    # Equations given in method section
    mutate(d.y = hh.exp.pcap.avg.day * (1-gini.floor/gini.baseyr)) %>%
    mutate(hh.exp.pcap.avg.day.gap.filled = hh.exp.pcap.avg.day + (povline.trend.tgt - d.y)) %>%
    # Import population
    left_join(pop_annual %>%
                mutate(Scenario = ifelse(grepl("SSP", Scenario), Scenario, paste0('SDP-', Scenario)))) %>%
    # Calculate transfer for each country
    mutate(GDP_diff = 365 * (povline.trend.tgt - d.y) * POP.mil * povratio) # million $
  
  tot.transfer.single = infsb.single %>% 
    filter(gini.baseyr > gini.minimum.abs) %>% 
    group_by(Scenario) %>%
    summarise(transfer = sum(GDP_diff)/1000) %>% # Billion $
    mutate(Year = tgt)
  
  infsb[[as.character(tgt)]] = infsb.single
  tot.transfer[[as.character(tgt)]] = tot.transfer.single
}

infsb.df = bind_rows(infsb) %>% filter(grepl("SDP", Scenario))

# DF of total transfer by year by scenario
tot.transfer.df = bind_rows(tot.transfer) %>% group_by(Year) %>% 
  arrange(transfer, .by_group = TRUE) %>%
  mutate(Family = ifelse(grepl("SSP", Scenario), "SSP", "SDP")) 


make.inf.table <- function(year) {
  df = infsb[[year]]
  tot.transfer = df %>% 
    filter(gini.baseyr > gini.minimum.abs) 
  
  df.out = df %>%
    group_by(Scenario, inc.grp) %>%
    summarise(txt = paste(country, collapse = "; "), n=n()) %>%
    left_join(tot.transfer %>% group_by(Scenario, inc.grp) %>%
                summarise(transfer = sum(GDP_diff)/1000)) %>%
    left_join(tot.transfer %>% 
                group_by(Scenario) %>%
                summarise(transfer = sum(GDP_diff)/1000), 
              by=c("Scenario"), 
              suffix = c(".inc.grp", ".scen"))
  
  names(df.out) = c("Scenario",	"Income group",	
                    paste("Countries with infeasibility by", year),	
                    "Total transfer ($ bil.)",	"Scenario total ($ bil.)")
  
  write.csv(df.out, file=paste0("./Results/", "unmet_countries_", year, ".csv"), row.names = FALSE)
  return(df.out)
}

# inf.table = lapply(names(infsb), make.inf.table)
inf.table = lapply(c("2030", "2050", "2100"), make.inf.table)
# names(inf.table) = names(infsb)


# write.csv(infsb[["2050"]])
# Test plot for an individual country for examining the calculation in more detail ====
l.size = 1.5
scale.gdp = 0.3

col_EI = "midnightblue"
col_MC = "aquamarine4"
col_RC = "goldenrod3"
col_SSP1 = "cyan"
col_SSP2 = "black"

scenario_cmap <- c("SDP-EI" = col_EI,
                   "SDP-MC" = col_MC,
                   "SDP-RC" = col_RC,
                   "SSP1" = col_SSP1,
                   "SSP2" = col_SSP2)


library(ggpubr)

p_gini = list()
p_npl = list()

# Two country examples ====
for (cty in c("LBY", "KGZ")) {
# for (cty in c("AFG")) {
  
  df.cty = realised_gini %>% filter(iso3c %in% c(cty)) %>%
    mutate(Family = ifelse(grepl("SSP", Scenario), "SSP", "SDP"))
  
  p_npl[[cty]] = ggplot(data = df.cty, aes(x = Year)) +
    geom_line(aes(y = povline.trend.tgt,
                  group = interaction(iso3c, Scenario),
                  color = Scenario,
                  linetype = Family,
                  size = Family)) +
    geom_text(
      data = realised_gini %>%
        filter(Year == 2020, iso3c %in% c(cty), Scenario == "SDP-EI"),
      aes(x = 2020, y = hh.exp.pcap.avg.day, 
          label = paste0("Mean expenditure/cap in 2020:\n",
                         format(hh.exp.pcap.avg.day, digits = 3),
                         "($/day)")), hjust = 0) +
    geom_hline(
      data = realised_gini %>%
        filter(Year == 2020, iso3c %in% c(cty), Scenario == "SDP-EI"), 
      aes(yintercept = hh.exp.pcap.avg.day))  +
    scale_y_continuous(limits = c(2, 20)) +
    theme(plot.margin = margin(1, 0,0,0, "cm")) +
    labs(y = "Poverty lines ($/day)") + 
    scale_linetype_manual(values = c("solid", "twodash")) +
    scale_color_manual(values = scenario_cmap) +
    scale_size_manual(values = l.size * c(1, 0.7))
  
  p_gini[[cty]]  = ggplot(data = df.cty, aes(x = Year)) +
    geom_line(aes(y = gini.tgt.trend,
                  group = interaction(country, Scenario),
                  color = Scenario,
                  linetype = Family), 
              size = l.size * 0.7, alpha = 0.3)  +
    geom_line(aes(y = gini.realised.trend,
                  group = interaction(country, Scenario),
                  color = Scenario,
                  linetype = Family,
                  size = Family)) +
    labs(y = "Gini") + 
    # geom_line(aes(y=gini.floor, 
    #               group=interaction(country, Scenario), 
    #               color=Scenario), 
    #           linetype = "dotted", size = l.size * 0.8) +
    scale_linetype_manual(values = c("solid", "twodash")) +
    scale_color_manual(values = scenario_cmap) +
    scale_size_manual(values = l.size * c(1, 0.7)) +
    
    scale_y_continuous(limits = c(5, 40)) +
    # ylim(c(15, 30)) +
    scale_x_continuous(breaks = seq(2020, 2100, 10), minor_breaks = NULL) +
    # labs(title = cty) +
    theme(plot.margin = margin(1, 0,0,0, "cm")) +
    annotate(geom = "text",
             x = 2080,
             y = 20,
             label = "required for poverty eradication") +
    annotate(geom = "text",
             x = 2070,
             y = 28,
             label = "empirical lower bounds")
}

ggarrange(p_gini[[1]] + rremove("legend") + rremove("x.text"), p_gini[[2]] + rremove("x.text"), 
          p_npl[[1]] + rremove("legend"), p_npl[[2]],
          labels = c("a) Libya (UMIC): Gini", "b) Kirgizstan (LMIC): Gini", 
                     "c) Libya (UMIC): Poverty line", "d) Kirgizstan (LMIC): Poverty line"),
          hjust = 0, vjust=1,
          ncol = 2, nrow = 2, 
          common.legend=TRUE, legend="right")


# Plot just about Gini (no second axis) ====
library(ggtext)
cty = "IND"
df.data = realised_gini %>% filter(iso3c %in% c(cty), Year <= 2070) %>%
  # mutate(Family = ifelse(grepl("SSP", Scenario), "SSP", "SDP"))
  filter(grepl("SDP", Scenario))
ggplot(data = df.data, aes(x = Year)) +
  geom_line(aes(
    y = gini.tgt.trend,
    group = interaction(country, Scenario),
    color = Scenario,
    # linetype = Family
  ), size = l.size * 0.7, alpha = 0.3)  +
  geom_line(aes(
    y = gini.realised.trend,
    group = interaction(country, Scenario),
    color = Scenario,
    # linetype = Family
  ), size = l.size)  +
  geom_line(aes(
    y = gini.floor,
    group = interaction(country, Scenario),
    color = Scenario
  ), linetype = "dotted", size = l.size * 0.8)  +
  # Mark the point where (pov headcount)=0 is met
  geom_point(
    data = . %>% filter(Year == year.abstgt.achieved) %>% distinct(country, Scenario, .keep_all=T),
    aes(x = Year, 
        y = gini.realised.trend,
        color = Scenario),
    shape = 8, size = 4
  ) +
  geom_point(
    data = . %>% slice(1),
    aes(x = Year, 
        y = gini.baseyr),
    shape = 3, size = 4
  ) +
  scale_y_continuous(
    # Features of the first axis
    name = "Gini",
    breaks = seq(20, 40, 2)
  ) +
  scale_x_continuous(breaks = seq(2020, 2070, 10), minor_breaks = NULL) +
  labs(title = "India") +
  theme_bw() +
  annotate(geom = "text",
           x = 2022,
           y = 35.5,
           label = "current level of inequality",
           hjust = 0) +
  annotate(geom = "text",
           x = 2060,
           y = 29,
           label = "required for poverty eradication") +
  annotate(geom = "text",
           x = 2030,
           y = 28,
           label = "SDP Gini trajectories") +
  annotate(geom = "text",
           x = 2060,
           y = 24.5,
           label = "empirical Gini lower bounds") +
  annotate(geom = "text",
           x = 2028,
           y = 32,
           label = "Gini reduction \n towards poverty eradication",
           hjust = 0) +
  scale_color_manual(values = scenario_cmap) +
  scale_linetype_manual(values = c("solid", "twodash")) 


# Histogram of achievement year ====
aa = realised_gini %>% group_by(country, iso3c, Scenario) %>% 
  select(year.abstgt.achieved, inc.grp) %>% slice(1)
aaa = aa %>% group_by(Scenario) %>% select(year.abstgt.achieved, inc.grp) %>%
  filter(grepl("SDP", Scenario))

aaa %>%
  ggplot(aes(x=year.abstgt.achieved, fill=Scenario)) +
  geom_histogram(color="#e9ecef", alpha=0.6, position = 'identity', binwidth=1) +
  # theme_ipsum() +
  labs(fill="")

p_histo = aaa %>%
  ggplot(aes(x=year.abstgt.achieved, fill=Scenario)) +
  geom_histogram(color="#e9ecef", position = 'identity', binwidth=1) +
  theme_bw() +
  theme(panel.border = element_rect(fill=NA,color="grey80", size=0.5, linetype="solid"),
        legend.position = "none") +
  geom_vline(aes(xintercept=2030), linetype ="dotdash") +
  scale_fill_manual(values = scenario_cmap) +
  scale_x_continuous(breaks=seq(2030, 2100, 10)) + 
  facet_grid(rows=vars(Scenario)) +
  labs(x="Year when the target is reached", y="Number of countries") #+
  # xlim(c(2025, 2050))

aaa %>% 
  ggplot(aes(x=year.abstgt.achieved, fill=Scenario)) +
  geom_histogram(color="#e9ecef", alpha=0.5, position = 'identity', binwidth=1) +
  theme_bw() +
  theme(panel.border = element_rect(fill=NA,color="grey80", size=0.5, 
                                    linetype="solid")) +
  # facet_grid(rows=vars(Scenario), cols=vars(inc.grp)) +
  scale_fill_manual(values = scenario_cmap) +
  # facet_grid(rows=vars(Scenario)) +
  labs(fill="", x="Year target achieved", y="Number of countries") +
  xlim(c(2025, 2050))

ggarrange(p_gini, p_histo, ncol=1, labels="auto", heights = c(1, 0.4))


  # Transfer plot ====
library(ggpattern)
trasfer.plot.data = tot.transfer.df %>% 
  filter(grepl("SDP", Scenario), Year%%5==0)
ggplot(trasfer.plot.data %>% filter(Year!=2020), 
       aes(Year, transfer)) +
  geom_col(aes(fill=Scenario), position="dodge", width = 3.9) +
  geom_col(data=tot.transfer.df %>% filter(Year==2020, Scenario=="SDP-MC") %>%
             mutate(Scenario="Base"), 
           aes(Year, transfer), position="dodge", width = 2) +
  scale_fill_manual(values = scenario_cmap) +
  labs(y="Global total transfer required (billion 2011 $PPP)") +
  scale_x_continuous(breaks=seq(2020, 2060, 5), limits = c(2017, 2063)) +
  scale_y_continuous(breaks=seq(0, 2500, 500)) +
  theme_bw() +
  theme(panel.border = element_rect(fill=NA,color="grey80", size=0.5, linetype="solid")) 
  


# Export Gini to IAMC format
df.export = realised_gini %>%
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
write_delim(df.export, file = "SHAPE_Gini_v1p3_annual.csv", delim = ',') # for >=10 year spells
