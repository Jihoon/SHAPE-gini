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
gini.lbound.innov <- 26.2 * c(1.1, 1, 0.9)    # 5% percentile
gini.lbound.serv <- 24.8 * c(1.1, 1, 0.9) # 2% percentile
gini.lbound.soc <- 24 * c(1.1, 1, 0.9)        # 1% percentile
# dgini is -2.1% ~ 1.6%/year for 80% of observation (for 5yr spells)
# dgini is -2.8% ~ 2.1%/year for 90% of observation (for 5yr spells)
dgini.lbound.innov <- 0.022 * c(0.9, 1, 1.1)
dgini.lbound.serv <- 0.03 * c(0.9, 1, 1.1)
dgini.lbound.soc <- 0.034 * c(0.9, 1, 1.1)

# Read in PIK data on GDP and population, and derive GDP/cap
# Convert 2005$ PPP to 2017$ PPP (as in WDI)
gdp_data = read_delim(paste0(data.path, "release_v1p1/SHAPE_GDP_v1p1.mif"), delim=';') %>% select(-X25) %>%
  pivot_longer(cols = `2010`:`2100`, names_to = 'Year', values_to = 'GDP.bil') %>% select(-Variable, -Unit)
pop_data = read_delim(paste0(data.path, "release_v1p1/SHAPE_POP_v1p1.mif"), delim=';') %>% select(-X25) %>%
  pivot_longer(cols = `2010`:`2100`, names_to = 'Year', values_to = 'POP.mil') %>% select(-Model, -Scenario, -Variable, -Unit)
gdp_pcap = gdp_data %>% left_join(pop_data, by = c("Region", "Year")) %>% rename(iso3c=Region) %>%
  left_join(ppp.conv) %>% mutate(GDP.bil = GDP.bil*ppp.2005.to.2017)%>%
  mutate(gdp.pcap = GDP.bil/POP.mil*1000) 

df.gr = gdp_pcap %>% mutate(GDP.pcap = GDP.bil/POP.mil) %>% group_by(iso3c, Scenario) %>%
  mutate(GDP.pcap.pre = lag(GDP.pcap)) %>% mutate(gr.r = (GDP.pcap/GDP.pcap.pre)^0.2 -1) # Annual avg GDP growth rate





#### Load SDP scenario data ####


# Add HH expenditure per capita derived based on the latest share (NE.CON.PRVT.ZS)
master = gni2020 %>% left_join(fin.con) %>%
  mutate(hh.exp.pcap.KD = NY.GDP.PCAP.PP.KD * final.cons.rate) %>% # in 2017$ PPP
  left_join(ppp.conv) %>%
  mutate(hh.exp.pcap.avg.2020 = hh.exp.pcap.KD * ppp.2017.to.2011) %>%# in 2011$ PPP
  
  # Derive step-like thresholds with slopes (based on Elmar's suggestion)
  rename(gni.2020 = gni) %>% ungroup() %>% select(country, gni.2020, gdp.gni.ratio, iso3c, hh.exp.pcap.avg.2020) %>% right_join(gdp_pcap, by="iso3c") %>%
  
  mutate(GNI.bil = GDP.bil / gdp.gni.ratio) %>%
  mutate(gni.pcap = GNI.bil / POP.mil * 1000) %>%
  mutate(inc.grp = cut(gni.pcap, breaks=thres, labels=FALSE)) %>%
  group_by(inc.grp) %>%
  mutate(min = min(gni.pcap), med = median(gni.pcap), max = max(gni.pcap)) %>%  # min/med/max of the GNI within each group
  left_join(groups.2020) %>%
  mutate(grp.lower = pmax(inc.grp-1, 1)) %>% # The group below
  mutate(povline0 = povline[inc.grp], povline.lower = povline[grp.lower]) %>% # povline0: reference pov line given the GNI
  mutate(povline.adj = pmin((povline0-povline.lower)/(med-min) * (gni.pcap-min) + povline.lower, povline0)) %>% # Pov line with slopes
  mutate(year=as.numeric(Year)) %>% group_by(country) %>% drop_na(gdp.pcap)

# poor <- master %>% filter(any(gdp.pcap<2000 & year==2020)) %>%
#   pull(iso3c) %>% unique() %>% sort()
# medium <- master %>% filter(any(gdp.pcap>12500 & gdp.pcap<15000 & year==2020)) %>%
#   pull(iso3c) %>% unique() %>% sort()
# rich <- master %>% filter(any(gdp.pcap>50000 & year==2020)) %>%  
#   pull(iso3c) %>% unique() %>% sort()

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
  
  # add floors related to historical boundaries
  traj <- df.povline.lagged %>% 
    # Target gini aiming for WB pov lines
    mutate(gini.tgt.povl = gini.baseyr * (hh.exp.pcap.avg.day - povline0) / hh.exp.pcap.avg.day) %>%
    # -4% maximum decrease (Lakner et al.) and total equality maximum
    mutate(gini.floor = ifelse(Scenario=="innovation",
                               pmax(g.l.in, gini.baseyr * (1-dg.l.in)^(as.numeric(Year) - 2020)),
                               ifelse(Scenario=="service",
                                      pmax(g.l.se, gini.baseyr * (1-dg.l.se)^(as.numeric(Year) - 2020)),
                                      ifelse(Scenario=="society",
                                             pmax(g.l.so, gini.baseyr * (1-dg.l.so)^(as.numeric(Year) - 2020)), 
                                             NA)))) %>% 
    # mutate(achieved.povl = (gini.floor < gini.tgt.povl) & (gini.tgt.povl > gini.lbound)) %>% 
    # Target gini aiming for the S-trend curve (arbitrary asymtote (current max))
    mutate(gini.tgt.trend = gini.baseyr * (hh.exp.pcap.avg.day - povline.trend.tgt) / hh.exp.pcap.avg.day) #%>%
    # mutate(achieved.trend = (gini.floor < gini.tgt.trend) & (gini.tgt.trend > gini.lbound)) %>%
    # mutate(check = (achieved.trend==achieved.povl))
  
  # calculate the realised gini changes (trend line) ====
  realised_gini <- traj %>% 
    select(country, iso3c, Scenario, Year, gini.tgt.trend, gini.floor, povline.trend.tgt, povline0) %>% 
    mutate(gini.realised.trend = pmax(gini.tgt.trend, gini.floor)) %>% ungroup() %>%
    mutate(years.ontrack= (gini.tgt.trend >= gini.floor)) %>%
    group_by(iso3c, Scenario) %>%
    mutate(tgt.achieved = last(years.ontrack)) %>%
    # mutate(gini.realised.trend = ifelse(years.ontrack & Scenario!="innovation", min(gini.realised.trend), 
    #                                            gini.realised.trend))
    mutate(gini.realised.trend = ifelse(tgt.achieved, gini.realised.trend, NA)) %>% # Not feasible 
    mutate(gini.realised.trend = ifelse(years.ontrack & Scenario!="innovation", min(gini.realised.trend),
                                               gini.realised.trend))
  
  df.p1 <- traj %>% mutate(year=as.numeric(Year)) %>% filter(year<=2050) %>% group_by(country) %>% 
    inner_join(sample.cty)
  
    # plot poverty line targets ====
  p1 <- ggplot(data=df.p1, aes(x=year, colour=country, group=country)) +
    facet_grid(inc.grp~Scenario, scales = "free") +
    geom_line(aes(y=povline.trend.tgt)) +
    # geom_line(aes(y=povline0), linetype="dashed") +
    geom_text(data=. %>% filter(year==2050) %>% distinct(country, .keep_all=T),
              aes(x=2055,y=povline.trend.tgt, label=country)) +
    ggtitle(paste0("Poverty line trend, WB (dashed) vs. ", as.character(lg.y), "yr-lagged ", fit)) +
    ylab("Poverty line") +
    xlab(NULL) +
    theme(legend.position = "none")
  
  df.gini.realised <- realised_gini %>% mutate(year=as.numeric(Year)) %>% filter(year<=2050) %>% group_by(country) %>% 
    inner_join(sample.cty)
  
  # plot realised gini pathways ====
  p2 <- df.gini.realised %>% 
    ggplot(aes(x=year, colour=country, group=country)) +
    facet_grid(inc.grp~Scenario, scales = "free") +
    geom_line(aes(y=gini.realised.trend)) +
    geom_text(data=. %>% filter(year==2050) %>% distinct(country, .keep_all=T),
              aes(x=2055,y=gini.realised.trend, label=country)) +
    # geom_label(data=. %>% filter(!tgt.achieved) %>% distinct(country, .keep_all=T),
    #           aes(x=2020, y=20, label=paste(unique(iso3c), collapse = " ")), hjust = 0) +
    ggtitle(paste0("Gini constraint: ", as.character(tgt.str), ", ", as.character(lg.y), "yr-lagged ", fit)) +
    # ylab("Gini") +
    # xlab(NULL) +
    theme(legend.position = "none") +
    labs(y="Gini") #+

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
         filename = paste0(figure.path,"gini constraint-", as.character(tgt.str),"_horizon-",as.character(hist.str), "-", fit, ".png"),
         width = 30,
         height = 30,
         dpi = 300,
         units = "cm") # print 
  
  return(realised_gini)
}

list.result = list()
i <- 1
# Run the script
for (lg.setting in seq(1,3)){
  for (hist.setting in seq(1,3)){
    
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

# Test plot
l.size = 1.5

cty = "SRB"
ggplot(data=list.result[["medium10yr_"]] %>% filter(iso3c %in% c(cty)), aes(x=Year)) +
  # geom_line(aes(y=gini.floor, group=interaction(country, Scenario), color=Scenario), size=l.size)  +
  geom_line(aes(y=gini.tgt.trend, group=interaction(country, Scenario), color=Scenario), size=l.size)  +
  geom_line(data=df.gr %>% filter(iso3c %in% c(cty)), aes(y=gr.r*600, group=interaction(iso3c, Scenario), color=Scenario), linetype = "dashed", size=l.size) +
  geom_hline(yintercept=gini.lbound.innov[2], linetype="dashed", color = "red") +
  geom_hline(yintercept=gini.lbound.serv[2], linetype="dashed", color = "green") +
  geom_hline(yintercept=gini.lbound.soc[2], linetype="dashed", color = "blue") +
  # geom_text(data=df.ontrack %>%
  #             filter(iso3c %in% c(cty),
  #                    Scenario=="innovation"),
  #           aes(x=Year, y=gini.traj,
  #               label=paste0("Avg:", format(avg.exp, digits = 3), ", Min.:", format(povline.achieved, digits = 3), "($/day)")),
  #           nudge_y = -2) +
  # geom_text(data=df.gini %>%
  #             filter(Year==2020, iso3c %in% c(cty),
  #                    Scenario=="innovation"),
  #           aes(x=Year, y=gini.traj,
  #               label=paste0("Avg:", format(hh.exp.pcap.avg.day, digits = 3), "($/day)")),
  #           nudge_y = -2) +
  scale_y_continuous(

    # Features of the first axis
    name = "Gini",

    # Add a second axis and specify its features
    sec.axis = sec_axis(~./600, name="GDP growth rate")
  ) +
  labs(title=cty) +
  theme_bw()
#   
# cty = "AUT"
# ggplot(data=df.gini %>% filter(iso3c %in% c(cty)), aes(x=Year)) +
#   geom_line(aes(y=gini.traj, group=interaction(country, Scenario), color=Scenario), size=l.size)  +
#   geom_line(data=df.gr %>% filter(iso3c %in% c(cty)), aes(y=gr.r*1000, group=interaction(iso3c, Scenario), color=Scenario), linetype = "dashed", size=l.size) + 
#   geom_hline(yintercept=gini.lbound, linetype="dashed", color = "blue") +
#   geom_text(data=df.ontrack %>% 
#               filter(iso3c %in% c(cty),
#                      Scenario=="innovation"), 
#             aes(x=Year, y=gini.traj, 
#                 label=paste0("Avg:", format(avg.exp, digits = 3), ", Min.:", format(povline.achieved, digits = 3), "($/day)")),
#             nudge_y = -2) +
#   geom_text(data=df.gini %>% 
#               filter(Year==2020, iso3c %in% c(cty),
#                      Scenario=="innovation"), 
#             aes(x=Year, y=gini.traj, 
#                 label=paste0("Avg:", format(hh.exp.pcap.avg.day, digits = 3), "($/day)")),
#             nudge_y = -2) +
#   scale_y_continuous(
#     
#     # Features of the first axis
#     name = "Gini",
#     
#     # Add a second axis and specify its features
#     sec.axis = sec_axis(~./1000, name="GDP growth rate")
#   ) +
#   labs(title=cty) +
#   theme_bw()
#   