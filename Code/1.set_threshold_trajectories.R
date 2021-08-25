#### Income group identification ####

# Income group characteristics based on 2020 categories
groups.2020 <- gni2020 %>% select(inc.grp, min, med, max) %>% unique()
names(groups.2020)[-1] <- paste0(names(groups.2020)[-1] ,'.2020')

# Test plot for threshold trajectory
ggplot(gni2020) +
  geom_point(aes(x = gni/365, y = povline.adj)) + 
  scale_x_continuous(trans='log') + 
  scale_y_continuous(trans='log')



#### Load SDP scenario data ####

# Read in PIK data on GDP and population, and derive GDP/cap
# Convert 2005$ PPP to 2017$ PPP (as in WDI)
gdp_data = read_delim(paste0(data.path, "release_v1p0/SHAPE_GDP_v1p0.mif"), delim=';') %>% select(-X25) %>%
  pivot_longer(cols = `2010`:`2100`, names_to = 'Year', values_to = 'GDP.bil') %>% select(-Variable, -Unit)
pop_data = read_delim(paste0(data.path, "release_v1p0/SHAPE_POP_v1p0.mif"), delim=';') %>% select(-X25) %>%
  pivot_longer(cols = `2010`:`2100`, names_to = 'Year', values_to = 'POP.mil') %>% select(-Model, -Scenario, -Variable, -Unit)
gdp_pcap = gdp_data %>% left_join(pop_data, by = c("Region", "Year")) %>% rename(iso3c=Region) %>%
  left_join(ppp.conv) %>% mutate(GDP.bil = GDP.bil*ppp.2005.to.2017)%>%
  mutate(gdp.pcap = GDP.bil/POP.mil*1000) 

df.gr = gdp_pcap %>% mutate(GDP.pcap = GDP.bil/POP.mil) %>% group_by(iso3c, Scenario) %>%
  mutate(GDP.pcap.pre = lag(GDP.pcap)) %>% mutate(gr.r = (GDP.pcap/GDP.pcap.pre)^0.2 -1) # Annual avg GDP growth rate


#### Organize data for further analysis ####

# Add HH expenditure per capita derived based on the latest share (NE.CON.PRVT.ZS)
master = gni2020 %>% left_join(fin.con) %>%
  # mutate(povline.gdp = povline.adj/final.cons.rate) %>% # in 2011 PPP, same as the original WB poverty line
  # mutate(hh.exp.pcap.CD = NY.GDP.PCAP.CD*final.cons.rate) %>%
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
  mutate(povline.adj = pmin((povline0-povline.lower)/(med-min) * (gni.pcap-min) + povline.lower, povline0)) # Pov line with slopes

# Derive avg HH exp projections from GDP trajectories by scenario
# Unit: HH and GDP in 2017$ PPP, GNI in Atlas current
df = master %>% 
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

gini.lbound = 20 # Arbitrary Gini lower bound at 20

# Estimate NPI target based on GNI/day
traj = df %>% select(-c(hh.exp.pcap.avg.2020, gdp.pcap, inc.grp:max.2020, gdp.pcap.pre:hh.exp.pcap.avg)) %>%
  left_join(gini.wb) %>%
  mutate(gni.day = gni.pcap/365) %>% select(-gni.pcap)%>%
  mutate(ln.GNI = log(gni.day)) %>%
  ungroup() %>%
  # mutate(ln.NPI = predict(model2, newdata=traj, type='response')) %>%
  # muate(ln.NPI = exp(ln.NPI * (max(hist$ln.NPL) - min(hist$ln.NPL)) + min(hist$ln.NPL))
  mutate(ln.NPI = predict(model1, newdata=traj, type='response') * 
           (max(hist$ln.NPL) - min(hist$ln.NPL)) + min(hist$ln.NPL)  ) %>%
  mutate(povline.trend = exp(ln.NPI)) %>%
  
  # Target gini aiming for WB pov lines
  mutate(gini.tgt.povl = gini.baseyr * (hh.exp.pcap.avg.day - povline0) / hh.exp.pcap.avg.day) %>%
  mutate(gini.floor = gini.baseyr * 0.96^(as.numeric(Year) - 2020)) %>% # -4% maximum decrease (Lakner et al.)
  mutate(achieved.povl = (gini.floor < gini.tgt.povl) & (gini.tgt.povl > gini.lbound)) %>% 
  # Target gini aiming for the S-trend curve (arbitrary asymtote (current max))
  mutate(gini.tgt.trend = gini.baseyr * (hh.exp.pcap.avg.day - povline.trend) / hh.exp.pcap.avg.day) %>%
  mutate(achieved.trend = (gini.floor < gini.tgt.trend) & (gini.tgt.trend > gini.lbound)) %>% 
  
  mutate(check = (achieved.trend==achieved.povl))

# Attempt to construct Gini path when on-track
# traj.sub = traj %>% filter(!is.na(gini.baseyr)) %>%
#   group_by(iso3c, Scenario) %>%
#   mutate(povline.pre = lag(povline.trend), hh.exp.pre = lag(hh.exp.pcap.avg.day)) %>%
#   mutate(scaler = (hh.exp.pcap.avg.day - povline.trend)/(hh.exp.pre - povline.pre) * hh.exp.pre/hh.exp.pcap.avg.day) %>% #gini scaler = k*mu_x/mu_z
#   # mutate(gini.traj = gini.tgt.trend * scaler) %>%
#   replace_na(replace = list(scaler = 1)) %>%
#   mutate(sc.cumul = cumprod(scaler))
 
# When each country meets the target (trend line)
hc0 = traj %>%
  group_by(iso3c, Scenario, achieved.trend) %>%
  summarise(Year = first(Year), gini.hc0 = first(gini.tgt.trend)) %>%
  filter(achieved.trend == TRUE) %>% # Identify the year when the headcount=0 is achieved.
  rename(year.achieved = Year) %>% select(-achieved.trend)

df.traj = traj %>% left_join(hc0) %>% drop_na(year.achieved) %>% filter(!is.na(gini.baseyr)) %>%
  mutate(ontrack = ifelse(Year<year.achieved, -1, ifelse(Year==year.achieved, 0, 1)))
# df.traj.all = traj %>% left_join(hc0) %>% filter(!is.na(gini.baseyr)) %>%
#   mutate(ontrack = ifelse(Year<year.achieved, -1, ifelse(Year==year.achieved, 0, 1)))

# Gini path when on-track
a = df.traj %>% filter(ontrack>=0) %>% 
  group_by(iso3c, Scenario) %>%
  mutate(povline.pre = lag(povline.trend), hh.exp.pre = lag(hh.exp.pcap.avg.day)) %>%
  mutate(scaler = (hh.exp.pcap.avg.day - povline.trend)/(hh.exp.pre - povline.pre) * hh.exp.pre/hh.exp.pcap.avg.day) %>% #gini scaler = k*mu_x/mu_z
  replace_na(replace = list(scaler = 1)) %>%
  mutate(sc.cumul = cumprod(scaler)) %>%
  mutate(gini.traj = gini.hc0 * sc.cumul)

# Gini path when off-track
b = df.traj %>% filter(ontrack<=0) %>% #ungroup() %>%
  add_count(iso3c, Scenario) %>%
  group_by(iso3c, Scenario) %>%
  mutate(gini.traj = seq(first(gini.baseyr), first(gini.hc0), length.out=first(n))) %>%
  select(-n)

# Combined Gini trajectories
df.gini = rbind(a, b) %>% arrange(country, Scenario, Year) %>%
  select(country:POP.mil, contains("povl"), contains("gini"), contains("hh.exp.pcap"), -contains("pre"), ontrack) %>%
  unique(.) %>%
  left_join(df.gr %>% select(Scenario:Year, gr.r))
df.ontrack = df.gini %>% filter(ontrack==0) %>% 
  select(country:Year, povline.achieved=povline.trend, avg.exp=hh.exp.pcap.avg.day, gini.traj)

# Test plot
l.size = 1.5

cty = "AUT"
ggplot(data=df.gini %>% filter(iso3c %in% c(cty)), aes(x=Year)) +
  geom_line(aes(y=gini.traj, group=interaction(country, Scenario), color=Scenario), size=l.size)  +
  geom_line(data=df.gr %>% filter(iso3c %in% c(cty)), aes(y=gr.r*1000, group=interaction(iso3c, Scenario), color=Scenario), linetype = "dashed", size=l.size) + 
  geom_hline(yintercept=gini.lbound, linetype="dashed", color = "blue") +
  geom_text(data=df.ontrack %>% 
              filter(iso3c %in% c(cty),
                     Scenario=="innovation"), 
            aes(x=Year, y=gini.traj, 
                label=paste0("Avg:", format(avg.exp, digits = 3), ", Min.:", format(povline.achieved, digits = 3), "($/day)")),
            nudge_y = -2) +
  geom_text(data=df.gini %>% 
              filter(Year==2020, iso3c %in% c(cty),
                     Scenario=="innovation"), 
            aes(x=Year, y=gini.traj, 
                label=paste0("Avg:", format(hh.exp.pcap.avg.day, digits = 3), "($/day)")),
            nudge_y = -2) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Gini",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./1000, name="GDP growth rate")
  ) +
  labs(title=cty) +
  theme_bw()
  



# Observation
# Rich countries (e.g. AUT) never meets the trend line goal, because it grows coutinuously.
# TODO: test with the logistic function target

