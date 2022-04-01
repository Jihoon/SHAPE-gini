#### Income group identification ####

# Income group characteristics based on 2020 categories
groups.2020 <- gni2020 %>% select(inc.grp, min, med, max) %>% unique()
names(groups.2020)[-1] <- paste0(names(groups.2020)[-1] ,'.2020')

# # Test plot for threshold trajectory
# ggplot(gni2020) +
#   geom_point(aes(x = gni/365, y = povline.adj)) + 
#   scale_x_continuous(trans='log') + 
#   scale_y_continuous(trans='log')

library(patchwork)

library(here)
try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))
here::i_am("README.md")
setwd(here())
data.path <- paste0(here(), "/data/")
figure.path <- paste0(here(), "/figures/")

# Set model parameters ====

target_horizon <- c("5yr","10yr","15yr")
lag.years.target <- c(5,10,15)

gini_boundaries <- c("tight", "medium", "relaxed")
gini.lbound.innov <- 26.2 * c(1.1, 1, 0.9)    # 5% percentile (directly from pcn)
gini.lbound.serv <- 24.8 * c(1.1, 1, 0.9) # 2% percentile
gini.lbound.soc <- 24 * c(1.1, 1, 0.9)        # 1% percentile
# dgini is -2.1% ~ 1.6%/year for 80% of observation (for 5yr spells)
# dgini is -2.8% ~ 2.1%/year for 90% of observation (for 5yr spells)
dgini.lbound.innov <- 0.0176 * c(0.9, 1, 1.1)  #(for >=10yr spells)
dgini.lbound.serv <- 0.0214 * c(0.9, 1, 1.1)
dgini.lbound.soc <- 0.0248 * c(0.9, 1, 1.1)
# dgini.lbound.innov <- 0.022 * c(0.9, 1, 1.1)  #(for >=5yr spells)
# dgini.lbound.serv <- 0.03 * c(0.9, 1, 1.1)
# dgini.lbound.soc <- 0.034 * c(0.9, 1, 1.1)

# Read in PIK data on GDP and population, and derive GDP/cap
# Convert 2005$ PPP to 2017$ PPP (as in WDI)
gdp_data = read_delim(paste0(data.path, "release_v1p1/SHAPE_GDP_v1p1.mif"), delim=';') %>% select(-X25) %>%
  pivot_longer(cols = `2010`:`2100`, names_to = 'Year', values_to = 'GDP.bil') %>% select(-Variable, -Unit)
pop_data = read_delim(paste0(data.path, "release_v1p1/SHAPE_POP_v1p1.mif"), delim=';') %>% select(-X25) %>%
  pivot_longer(cols = `2010`:`2100`, names_to = 'Year', values_to = 'POP.mil') %>% select(-Model, -Scenario, -Variable, -Unit)
gdp_pcap = gdp_data %>% left_join(pop_data, by = c("Region", "Year")) %>% rename(iso3c=Region) %>%
  left_join(ppp.conv) %>% mutate(GDP.bil = GDP.bil*ppp.2005.to.2017)%>%
  mutate(gdp.pcap = GDP.bil/POP.mil*1000) %>% 
  mutate(Year = as.numeric(Year))

# Annual mean growth rate of countries
df.gr = gdp_pcap %>% mutate(GDP.pcap = GDP.bil/POP.mil) %>% group_by(iso3c, Scenario) %>%
  mutate(GDP.pcap.pre = lag(GDP.pcap)) %>% mutate(gr.r = (GDP.pcap/GDP.pcap.pre)^0.2 -1) 





#### Load SDP scenario data ####


# Add HH expenditure per capita derived based on the latest share (NE.CON.PRVT.ZS)
master = gni2020 %>% left_join(fin.con) %>%
  mutate(hh.exp.pcap.KD = NY.GDP.PCAP.PP.KD * final.cons.rate) %>% # in 2017$ PPP
  left_join(ppp.conv) %>%
  mutate(hh.exp.pcap.avg.2020 = hh.exp.pcap.KD * ppp.2017.to.2011) %>%# in 2011$ PPP
  
  # Derive step-like thresholds with slopes (based on Elmar's suggestion)
  rename(gni.2020 = gni) %>% ungroup() %>% select(country, gni.2020, gdp.gni.ratio, iso3c, hh.exp.pcap.avg.2020) %>% 
  right_join(gdp_pcap, by="iso3c") %>%
  
  mutate(GNI.bil = GDP.bil / gdp.gni.ratio) %>%
  mutate(gni.pcap = GNI.bil / POP.mil * 1000) %>%
  mutate(inc.grp = cut(gni.pcap, breaks=thres, labels=FALSE)) %>%
  group_by(inc.grp) %>%
  mutate(min = min(gni.pcap), med = median(gni.pcap), max = max(gni.pcap)) %>%  # min/med/max of the GNI within each group
  left_join(groups.2020) %>%
  mutate(grp.lower = pmax(inc.grp-1, 1)) %>% # The group below
  mutate(povline0 = povline[inc.grp], povline.lower = povline[grp.lower]) %>% # povline0: reference pov line given the GNI
  mutate(povline.adj = pmin((povline0-povline.lower)/(med-min) * (gni.pcap-min) + povline.lower, povline0)) %>% # Pov line with slopes
  # mutate(Year=as.numeric(Year)) %>% 
  group_by(country) %>% drop_na(gdp.pcap)

cty.grp <- gni2020 %>% select(iso3c, inc.grp) %>% mutate(inc.grp = factor(inc.grp, levels=c(1,2,3,4),labels = c("LIC", "LMIC", "UMIC", "HIC")))

# Derive avg HH exp projections from GDP trajectories by scenario
# Unit: HH and GDP in 2017$ PPP, GNI in Atlas current
df = master %>% ungroup() %>%
  select(-c(Model, gni.2020, GDP.bil, GNI.bil, gdp.gni.ratio, ppp.2005.to.2017, grp.lower, povline.lower)) %>%
  drop_na() %>% 
  filter(Year >= 2020) %>%
  group_by(iso3c, Scenario) %>%
  mutate(gdp.pcap.pre = lag(gdp.pcap)) %>%
  mutate(gr.gdp = gdp.pcap/gdp.pcap.pre - 1) %>%
  replace_na(replace = list(gr.gdp = 0)) %>%
  mutate(gr.hh.exp = gr.gdp * passthrough + 1, gr.test = (passthrough*((1+gr.gdp)^0.2-1)+1)^5) %>% # HH expenditure growth rate: The two are close values.
  mutate(hh.idx = cumprod(gr.test)) %>%
  mutate(hh.exp.pcap.avg = hh.exp.pcap.avg.2020 * hh.idx) %>% # in 2011$ PPP
  mutate(hh.exp.pcap.avg.day = hh.exp.pcap.avg / 365)



#### Derive Gini trajectories ####


# Estimate NPI target based on GNI/day
GNI.pov.relation = df %>% select(-c(hh.exp.pcap.avg.2020, gdp.pcap, inc.grp:max.2020, gdp.pcap.pre:hh.exp.pcap.avg)) %>%
  # left_join(gini.wb) %>% 
  left_join(gini.ssp1) %>%
  mutate(gni.day = gni.pcap/365) %>% select(-gni.pcap)%>%
  mutate(ln.GNI = log(gni.day)) %>%
  ungroup()

fit = 'linear'

if (fit=='logistic') {
  ln.NPI.predict = predict(model1, newdata=GNI.pov.relation, type='response') * 
    (max(hist$ln.NPL) - min(hist$ln.NPL)) + min(hist$ln.NPL)
} else if (fit=='linear') {
  ln.NPI.predict = predict(model3, newdata=GNI.pov.relation, type='response')
}

# Raw (no lags) poverty line fit
df.povline <- GNI.pov.relation %>% 
  
  # using the logistic or linear curve fit, derive the poverty line we will work with
  mutate(ln.NPI = ln.NPI.predict) %>%
  mutate(povline.trend = pmax(1.9, exp(ln.NPI)))

# Sample n countries from each income group not to over-crowd the figure
sample.cty <- cty.grp %>% group_by(inc.grp) %>% sample_n(10)


# Function for deriving Gini trajectory and plotting
create_pathways <- function(g.l.in, g.l.se, g.l.so,
                            dg.l.in, dg.l.se, dg.l.so,
                            lg, lg.y,
                            tgt.str, hist.str, fit="logistic"){  

  # Poverty line with a given lag
  df.povline.lagged <- df.povline %>% select(-c(povline.adj)) %>% 
    # now lag the poverty line trend
    group_by(iso3c, Scenario) %>% mutate(povline.trend.tgt = dplyr::lag(povline.trend, n=lg, default = NA)) %>% ungroup() %>%
    # add initial stable poverty line (even though this is normally not used) 
    left_join(
      df.povline %>% filter(Year==initial.ambition.yr) %>% select(iso3c,Scenario,povline.trend) %>% rename(init.povline=povline.trend)
    ) %>% 
    mutate(povline.trend.tgt=ifelse(is.na(povline.trend.tgt),init.povline,povline.trend.tgt)) %>% select(-init.povline)
  
  get_floor <- function(base, min, r, yr) {
    v = pmax(min, base * (1-r)^(yr - 2020))
    return(v)
  }
  
  # add floors related to historical boundaries
  traj <- df.povline.lagged %>% 
    # Target gini aiming for WB pov lines
    mutate(gini.tgt.povl = gini.baseyr * (hh.exp.pcap.avg.day - povline0) / hh.exp.pcap.avg.day) %>%
    mutate(gini.minimum.abs = ifelse(Scenario=="innovation", g.l.in, 
                                 ifelse(Scenario=="service", g.l.se, 
                                        ifelse(Scenario=="society", g.l.so, NA)))) %>%
    mutate(gini.slope = ifelse(Scenario=="innovation", dg.l.in, 
                                 ifelse(Scenario=="service", dg.l.se, 
                                        ifelse(Scenario=="society", dg.l.so, NA)))) %>%
    # -4% maximum decrease (Lakner et al.) and total equality maximum
    mutate(gini.floor = get_floor(gini.baseyr, gini.minimum.abs, gini.slope, Year)) %>% 
    mutate(gini.tgt.trend = gini.baseyr * (hh.exp.pcap.avg.day - povline.trend.tgt) / hh.exp.pcap.avg.day)

  # calculate the realised gini changes (trend line) ====
  realised_gini <- traj %>% 
    select(country, iso3c, Scenario, Year, gini.baseyr, gini.minimum.abs,
           gini.slope, gini.tgt.trend, gini.floor, povline.trend.tgt, povline0) %>% 
    mutate(gini.realised.trend = pmax(gini.tgt.trend, gini.floor)) %>% ungroup() %>%
    mutate(years.ontrack= (gini.tgt.trend >= gini.floor)) %>%
    group_by(iso3c, Scenario) %>%
    # Whether the absolute tgt is met or not (anytime before 2100)
    mutate(tgt.achieved = Reduce("|", years.ontrack)) %>% 
    # Gini traj for those who achieve the target. NA otherwise
    # mutate(gini.realised.trend = ifelse(tgt.achieved, gini.realised.trend, NA)) %>%  # We keep the trajectory for those with non-achievemen (and save the flag)
    # Gini at the point of achievement
    # mutate(gini.achieved.interm = gini.realised.trend[years.ontrack][1]) %>% 
    mutate(gini.achieved.interm = min(gini.realised.trend)) %>% 
    mutate(year.abstgt.achieved = Year[years.ontrack][1]) %>%
    # Once the abs tgt is achieved, keep it there. (no rebound)
    # mutate(gini.realised.trend = get_floor(gini.baseyr, gini.achieved.interm, gini.slope, Year)) 
    mutate(gini.realised.trend = ifelse(years.ontrack, gini.achieved.interm, gini.realised.trend)) 
                           
  Year.relative.tgt <- 2050
  gini.tgt.rel <- 30
  
  # Need to adjust the original trj to incorporate the relative tgt
  # Reach Gini=30 by 2050 (if gini.achieved.interm > 30)
  realised_gini <- realised_gini %>% 
    mutate(gini.realised.trend = ifelse(gini.achieved.interm > gini.tgt.rel, 
                                        get_floor(gini.baseyr, gini.tgt.rel, gini.slope, Year),
                                        gini.realised.trend)) 

  
  # Setting up for figures
  library(ggrepel)
  yr.end.figure <- 2060 # Last year in the x-axis
  df.p1 <- traj %>% filter(Year<=yr.end.figure) %>% group_by(country) %>% 
    inner_join(sample.cty)
  
    # plot poverty line targets ====
  p1 <- ggplot(data=df.p1, aes(x=Year, colour=country, group=country)) +
    facet_grid(inc.grp~Scenario, scales = "free") +
    geom_line(aes(y=povline.trend.tgt)) +
    geom_text_repel(data=. %>% filter(Year==2060) %>% distinct(country, .keep_all=T),
              aes(x=2060, y=povline.trend.tgt, label=iso3c),
              direction = "y",
              segment.color = 'grey80',
              nudge_x = 100,
              min.segment.length = 0.1,
              max.overlaps = 15) +
    ggtitle(paste0("Poverty line projections based on GNI development (Minimum threshold targets)")) +
    ylab("Poverty line (2011$ PPP/day)") +
    xlab(NULL) +
    theme(legend.position = "none") 
  
  df.gini.realised <- realised_gini %>% 
    filter(Year<=yr.end.figure) %>% group_by(country) %>% 
    inner_join(sample.cty)
  
  # plot realised gini pathways ====
  p2 <- df.gini.realised %>% 
    ggplot(aes(x=Year, colour=country, group=country)) +
    facet_grid(inc.grp~Scenario, scales = "free") +
    geom_line(aes(y=gini.realised.trend)) +
    geom_text_repel(data=. %>% filter(Year==2020) %>% distinct(country, .keep_all=T),
              aes(x=2020, y=gini.realised.trend, label=iso3c),
              direction = "y",
              segment.color = 'grey80',
              nudge_x = -100,
              min.segment.length = 0.1,
              max.overlaps = 15) +
    geom_point(data=. %>% filter(Year==year.abstgt.achieved) %>% distinct(country, Scenario, .keep_all=T),
              aes(x=Year, y=gini.realised.trend, colour=country), shape=8) +
    ggtitle(paste0("Gini trajectories")) +
    theme(legend.position = "none") +
    labs(y="Gini") 

  df.infs = realised_gini %>% 
    group_by(iso3c, Scenario) %>% 
    filter(!tgt.achieved) %>%
    slice(1) %>%
    group_by(Scenario, povline0) %>%
    summarise(txt = paste(iso3c, collapse = " ")) 

  print(paste(as.character(tgt.str), ", ", as.character(lg.y), "yr-lagged:", g.l.in, g.l.se, g.l.so))
  print(df.infs)

  p <- p1 / p2
  ggsave(plot = p,
         filename = paste0(figure.path,"gini constraint-", as.character(tgt.str),"_horizon-",as.character(hist.str), "-", fit, "_slower.png"),
         width = 30,
         height = 30,
         dpi = 300,
         units = "cm") 
  
  ggsave(plot = p1,
         filename = paste0(figure.path, "poverty line only.png"),
         width = 30,
         height = 15,
         dpi = 300,
         units = "cm") 
  ggsave(plot = p2,
         filename = paste0(figure.path,"gini only.png"),
         width = 30,
         height = 15,
         dpi = 300,
         units = "cm") 
  return(realised_gini)
}


# Run the script ====
list.result = list()
i <- 1

# Limit to one case for the unnecessary parameters (No need for 3 settings..)
for (lg.setting in 2){
  for (hist.setting in 2){
# for (lg.setting in seq(1,3)){
#   for (hist.setting in seq(1,3)){
    
    initial.ambition.yr <- "2020" # to compensate the lag.
    
    lag.period <- ifelse(lag.years.target[lg.setting]==10, 2,
                         ifelse(lag.years.target[lg.setting]==5, 1,
                                ifelse(lag.years.target[lg.setting]==15, 3,
                                       NA))) # set lag based on df structure and years lag
    
    df.gini = create_pathways(g.l.in=gini.lbound.innov[hist.setting], 
                    g.l.se=gini.lbound.serv[hist.setting],
                    g.l.so=gini.lbound.soc[hist.setting],
                    dg.l.in=dgini.lbound.innov[hist.setting], 
                    dg.l.se=dgini.lbound.serv[hist.setting],
                    dg.l.so=dgini.lbound.soc[hist.setting],
                    lg=lag.period,
                    lg.y=lag.years.target[lg.setting],
                    tgt.str=gini_boundaries[hist.setting],
                    hist.str=target_horizon[lg.setting],
                    fit='linear')
    list.result[[i]] <- df.gini
    names(list.result)[i] <- paste0(gini_boundaries[hist.setting], target_horizon[lg.setting], sep = "_")
    i <- i+1
  }
}


# Test plot for an individual country for examining the calculation in more detail
l.size = 1.5

cty = "KOR"
ggplot(data=list.result[["medium10yr_"]] %>% filter(iso3c %in% c(cty)), aes(x=Year)) +
  geom_line(aes(y=gini.floor, group=interaction(country, Scenario), color=Scenario), size=l.size*0.5)  +
  geom_line(aes(y=gini.tgt.trend, group=interaction(country, Scenario), color=Scenario), size=l.size*0.3)  +
  geom_line(aes(y=gini.realised.trend, group=interaction(country, Scenario), color=Scenario), size=l.size)  +
  geom_line(data=df.gr %>% filter(iso3c %in% c(cty)), 
            aes(y=gr.r*600, group=interaction(iso3c, Scenario), color=Scenario), linetype = "dashed", size=l.size) +
  geom_hline(yintercept=gini.lbound.innov[2], linetype="dashed", color = "red") +
  geom_hline(yintercept=gini.lbound.serv[2], linetype="dashed", color = "green") +
  geom_hline(yintercept=gini.lbound.soc[2], linetype="dashed", color = "blue") +
  geom_text(data=list.result[["medium10yr_"]] %>% 
              left_join(df %>% select(-povline0)) %>%
              filter(Year==2020, iso3c %in% c(cty),
                     Scenario=="innovation"),
            aes(x=Year, y=gini.floor,
                label=paste0("Avg:", format(hh.exp.pcap.avg.day, digits = 3), "($/day)")),
            nudge_y = -2, nudge_x = 1) +
  scale_y_continuous(

    # Features of the first axis
    name = "Gini",

    # Add a second axis and specify its features
    sec.axis = sec_axis(~./600, name="GDP growth rate")
  ) +
  scale_x_continuous(breaks=seq(2020, 2100, 5), minor_breaks=NULL) +
  labs(title=cty) +
  theme_bw()


# Export Gini to IAMC format
df.export = list.result[["medium10yr_"]] %>% 
  pivot_wider(id_cols = c(iso3c:Scenario, tgt.achieved), values_from = gini.realised.trend, names_from = Year) %>%
  mutate(Model = "SHAPE_Gini", Unit = NA, Variable = "Gini") %>% 
  left_join(gini.ssp1 %>% select(iso3c, `Base gini imputed`=imputed.gini, gini.baseyr)) %>%
  mutate(`2020` = coalesce(`2020`, gini.baseyr)) %>%
  left_join(fin.con %>% select(iso3c, `Share of final consumption among GDP imputed`=imputed.fin.con.r)) %>%
  select(Model, Scenario, Region=iso3c, Variable, Unit, `2020`:`2100`, everything(), `Absolute target achieved`=tgt.achieved, -gini.baseyr) 

# write_delim(df.export, file="SHAPE_Gini_v1p0.csv", delim=',') # for >=5 year spells
write_delim(df.export, file="SHAPE_Gini_v1p0_slower_repl.csv", delim=',') # for >=10 year spells
