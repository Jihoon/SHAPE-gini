# Function for deriving Gini trajectory and plotting
create_pathways <- function(g.l.in,
                            g.l.se,
                            g.l.so,
                            dg.l.in,
                            dg.l.se,
                            dg.l.so,
                            fit = "linear")
{
  # Poverty line with a given lag
  df.povline.lagged <- df.povline %>%
    # now lag the poverty line trend
    group_by(iso3c, Scenario) %>%
    # lag option is obsolete.
    mutate(povline.trend.tgt = dplyr::lag(povline.trend, n = 0, default = NA)) %>%
    ungroup() %>%
    # add initial stable poverty line (even though this is normally not used)
    left_join(
      df.povline %>% filter(Year == 2020) %>%
        select(iso3c, Scenario, povline.trend) %>%
        rename(init.povline = povline.trend)
    ) %>%
    mutate(povline.trend.tgt = ifelse(is.na(povline.trend.tgt), init.povline, povline.trend.tgt)) %>% select(-init.povline)
  
  get_floor <- function(base, min, r, yr) {
    v = pmax(min, base * (1 - r) ^ (yr - 2020))
    return(v)
  }
  
  # add floors related to historical boundaries
  traj <- df.povline.lagged %>%
    # Target gini aiming for WB pov lines
    # mutate(gini.tgt.povl = gini.baseyr * (hh.exp.pcap.avg.day - povline0) / hh.exp.pcap.avg.day) %>%
    mutate(gini.minimum.abs = ifelse(
      Scenario == "EI",
      g.l.in,
      ifelse(Scenario == "MC", g.l.se,
             ifelse(Scenario == "RC", g.l.so, g.l.in))
    )) %>%
    mutate(gini.slope = ifelse(
      Scenario == "EI",
      dg.l.in,
      ifelse(Scenario == "MC", dg.l.se,
             ifelse(Scenario == "RC", dg.l.so, dg.l.in))
    )) %>%
    # -4% maximum decrease (Lakner et al.) and total equality maximum
    mutate(gini.floor = get_floor(gini.baseyr, gini.minimum.abs, gini.slope, Year)) %>%
    
    # Key formulation from Min & Rao
    mutate(gini.tgt.trend = gini.baseyr * (hh.exp.pcap.avg.day - povline.trend.tgt) / hh.exp.pcap.avg.day)
  
  # calculate the realised gini changes (trend line) ====
  realised_gini <- traj %>%
    select(
      country,
      iso3c,
      Scenario,
      Year,
      gini.baseyr,
      gini.minimum.abs,
      gini.slope,
      gini.tgt.trend,
      gini.floor,
      povline.trend.tgt,
      hh.exp.pcap.avg.day,
      hh.exp.pcap.avg.day.2020,
      inc.grp
      # povline0
    ) %>%
    mutate(gini.realised.trend = pmax(gini.tgt.trend, gini.floor)) %>% ungroup() %>%
    mutate(years.ontrack = (gini.tgt.trend >= gini.floor)) %>%
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
  
  write.csv(realised_gini, 'realised_gini_abs.csv')

  gini.tgt.rel <- 30
  
  # Need to adjust the original trj to incorporate the relative tgt
  # Reach Gini=30 by 2050 (if gini.achieved.interm > 30)
  realised_gini <- realised_gini %>%
    mutate(
      gini.realised.trend = ifelse(
        gini.achieved.interm > gini.tgt.rel,
        get_floor(gini.baseyr, gini.tgt.rel, gini.slope, Year),
        gini.realised.trend
      )
    )
  
  # write.csv(realised_gini, 'realised_gini.csv')
  
  df.infs = realised_gini %>%
    group_by(iso3c, Scenario) %>%
    filter(!tgt.achieved) %>%
    slice(1) %>%
    group_by(Scenario, inc.grp) %>%
    summarise(txt = paste(iso3c, collapse = " "))
  
  print(paste(g.l.in, g.l.se, g.l.so))
  print(df.infs)
  
  return(list(traj, realised_gini))
}


# plot poverty line targets ====
plot_povline <- function(traj, yr.end.figure = 2070) {
  
  # Setting up for figures
  library(ggrepel)
  yr.end.figure <- 2060 # Last year in the x-axis
  df.p1 <-
    traj %>% filter(Year <= yr.end.figure) %>% group_by(country) %>%
    inner_join(sample.cty)
  
  p1 <-
    ggplot(data = df.p1, aes(
      x = Year,
      colour = country,
      group = country
    )) +
    facet_grid(inc.grp ~ Scenario, scales = "free") +
    geom_line(aes(y = povline.trend.tgt)) +
    geom_text_repel(
      data = . %>% filter(Year == 2060) %>% distinct(country, .keep_all = T),
      aes(x = 2060, y = povline.trend.tgt, label = iso3c),
      direction = "y",
      segment.color = 'grey80',
      # nudge_x = 100,
      min.segment.length = 0.1,
      max.overlaps = 15
    ) +
    ggtitle(
      paste0(
        "Poverty line projections based on GNI development (Minimum threshold targets)"
      )
    ) +
    ylab("Poverty line (2011$ PPP/day)") +
    xlab(NULL) +
    theme(legend.position = "none")
  
  ggsave(
    plot = p1,
    filename = paste0(figure.path, "poverty line only.png"),
    width = 30,
    height = 15,
    dpi = 300,
    units = "cm"
  )
}


# plot realised gini pathways ====
plot_gini <- function(realised_gini, yr.end.figure = 2070, df.ssp) {
  
  library(geomtextpath)
  library(RColorBrewer)
  
  df.gini.realised <- realised_gini %>%
    filter(Year <= yr.end.figure) %>% 
    group_by(country) %>%
    inner_join(sample.cty) %>%
    left_join(df.ssp) %>%
    group_by(country, Scenario) %>%
    mutate(gini.ssp = ifelse(Scenario %in% c("SSP1", "SSP2"), na.approx(gini.ssp), gini.ssp)) %>%
    ungroup() %>% group_by(inc.grp) %>%
    mutate(col.ind = country %>% as.factor() %>% as.numeric() %>% as.character()) %>% ungroup() 
  View(df.gini.realised %>% ungroup() %>% count(iso3c, inc.grp))
  
  # Set color palette
  colors = brewer.pal(7, "Dark2")
  
  p2 <- df.gini.realised %>%
    ggplot(aes(x = Year, colour=col.ind, label = iso3c)) +
    facet_grid(inc.grp ~ Scenario, scales = "free") +
    geom_vline(xintercept=2030, linetype ="dotdash") +
    geom_line(aes(y = gini.realised.trend)) +
    geom_line(aes(y = gini.ssp), linetype="twodash") +
    geom_text_repel(
      data = . %>% filter(Year == 2020) %>% distinct(country, .keep_all = T),
      aes(x = 2030, y = gini.realised.trend, label = iso3c),
      fontface = "bold",
      direction = "both",
      segment.color = 'grey80',
      # nudge_x = -100,
      min.segment.length = 0.1,
      max.overlaps = 10
    ) +
    # Mark the point where (pov headcount)=0 is met
    geom_point(
      data = . %>% filter(Year == year.abstgt.achieved) %>% distinct(country, Scenario, .keep_all=T),
      aes(x = Year, y = gini.realised.trend),
      shape = 8
    ) +
    scale_colour_manual(values = colors) +
    ggtitle(paste0("Gini trajectories")) +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1)) +
    labs(y = "Gini")
  
  print(p2)
  ggsave(
    plot = p2,
    filename = paste0(figure.path, "gini only.png"),
    width = 25,
    height = 21,
    dpi = 300,
    units = "cm"
  )
}