# SHAPE standard color scheme

alpha <- 0.6

countries = c("ZAF", "NER")

# for (ct in countries) {
  
  df.p = df.result %>% 
    filter(iso3c %in% countries) %>%
    ungroup() %>%
    mutate(Scenario_new = factor(Scenario, 
                                 levels = unique(Scenario), 
                                 labels = c("EI", "MC", "RC"))) %>%
    select(Year, iso3c, Scenario_new, gini.realised.trend, year.abstgt.achieved) %>% rename(Scenario=Scenario_new) %>%
    mutate(country = countrycode(iso3c, "iso3c", 'country.name')) %>%
    left_join(cty.grp) %>%
    mutate(labs = paste0(country, ' (', inc.grp, ')'))
  
  # 
  # income.grp = cty.grp %>% filter(iso3c %in% countries) %>%
  #   mutate(country = countrycode(iso3c, "iso3c", 'country.name')) %>% select(country, inc.grp) %>%
  #   mutate(labs = paste0(country, ' (', inc.grp, ')'))

  p <- ggplot(data=df.p) +
    geom_line(aes(y=gini.realised.trend, x=Year, color=Scenario), alpha=alpha, size=2) +
    scale_color_manual(values=scenario_cmap) +
    # Mark the point where (pov headcount)=0 is met
    geom_point(data=. %>% filter(Year==year.abstgt.achieved) %>% distinct(country, Scenario, .keep_all=T),
               aes(x=Year, y=gini.realised.trend, colour=country), shape=8) +
    # facet_grid(.~ country, labeller = as_labeller(pull(income.grp, labs))) +
    facet_grid(.~ labs, labeller = labeller(c("sss", "rrr"))) +
    theme_bw() +
    theme(legend.position="bottom",
          legend.title= element_blank(),
          text = element_text(size=15),
          plot.title = element_text(hjust = 0.5)) +
    ylim(20, 70) +
    labs(y = "", title="Gini") 
  
  print(p)
# }
    