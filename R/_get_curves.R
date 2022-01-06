get_benefit_curve <- function(rank_df, goal){

  rank_complete<- ocean_df %>%
    filter(f_eez > 0) %>%
    left_join(rank_df) %>%
    replace(is.na(.), 0) %>%
    arrange(pick_order) %>%
    left_join(norm_features_df) %>%
    replace(is.na(.), 0)

  protection_accumulation <- rank_complete %>%
    transmute(lon,lat, cell_id, f_highly_mpa,
              fraction_eez = if_else(f_highly_mpa > 0, f_highly_mpa, f_eez),
              fraction_protected = cumsum(fraction_eez)/protection_levels$eez_size) %>%
    select(-fraction_eez)

  if(goal == "biodiversity"){

    benefit_accumulation <- rank_complete %>%
      select(all_of(bio_feature_names)) %>%
      mutate_all(.funs = cumsum) %>%
      sweep(MARGIN = 2, bio_remains_BAU, `+`) %>%
      mutate_all(.funs = ~(.)^z_bio) %>%
      as.matrix() %*% as.matrix(bio_weights) %>%
      as_tibble() %>%
      set_names("v")

  } else if (goal == "carbon"){

    benefit_accumulation <- rank_complete %>%
      select(carbon) %>%
      mutate_all(.funs = cumsum) %>%
      sweep(MARGIN = 2, carbon_remains_BAU, `+`) %>%
      rowSums() %>%
      enframe(name = NULL) %>%
      set_names("v")

  }else if (goal == "food"){

    benefit_accumulation <- rank_complete %>%
      select(all_of(stock_names)) %>%
      mutate_all(.funs = cumsum) %>%
      apply(MARGIN = 1,
            FUN = estimate_delta_catch,
            e = stocks_info$ex_rate, m = stocks_info$m, r = stocks_info$r, k = stocks_info$k, effort_assumption = 2) %>%
      t() %>%
      as.matrix() %*% as.matrix(food_weights)  %>%
      as_tibble() %>%
      rowSums() %>%
      enframe(name = NULL) %>%
      set_names("v")

  }

  benefit_curve <- bind_cols(protection_accumulation, benefit_accumulation)

  return(benefit_curve)
}

#########################################################

plot_curve <- function(curve_df, goal, country_name){

  if(goal == "food"){

    curve <- curve_df %>%
      mutate(v = v/10^6) %>%
      ggplot()+
      geom_line(aes(fraction_protected, v))+
      labs(x = "Fraction EEZ Protected",
           y = "Change in Global Catch (Million tons)",
           title = "Contribution to global catch",
           subtitle = country_name)

  } else if(goal == "biodiversity"){

    curve <- curve_df %>%
      mutate(v = scales::rescale(v)) %>%
      ggplot()+
      geom_line(aes(fraction_protected, v))+
      labs(x = "Fraction EEZ Protected",
           y = "Biodiveristy Benefit",
           title = "Contribution to global biodiversity conservation",
           subtitle = country_name)

  } else if (goal == "carbon"){

    curve <- curve_df %>%
      mutate(v = scales::rescale(v,
                                 to = c(0, total_co2_in_eez/10^6))) %>%
      ggplot()+
      geom_line(aes(fraction_protected, v))+
      labs(x = "Fraction EEZ Protected",
           y = "Carbon Benefit (Million Mg CO2)",
           title = "Contribution to global carbon conservation",
           subtitle = country_name)
  }

  curve <- curve +
    geom_rect(aes(xmin = 0,
                  xmax = protection_levels$f_eez_highly_mpas,
                  ymin = 0,
                  ymax = max(v)),
              fill = "lightblue", alpha = 0.5)+
    scale_x_continuous(breaks = c(0,.2,.4,.6,.8,1))+
    easyR::ggtheme_plot()

  return(curve)
}

