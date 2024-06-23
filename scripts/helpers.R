lazy_kable <- function(x, full_width = F){
  x %>% 
    knitr::kable() %>% 
    kableExtra::kable_styling(bootstrap_options = "striped", full_width = full_width)
}

exp_c <- function(x) exp(x)/(1 + exp(x))

lower_hpd <- function(x){HDInterval::hdi(x)[1]}
upper_hpd <- function(x){HDInterval::hdi(x)[2]}

conditional_probs <- function(ordinal_practice_mod, assumption_label){
  
  newdata <- tidyr::expand_grid(scenario = c("s1", "s2"),
                                demo_level = c("Faculty who teach methods", "Faculty", "Postdoctoral researcher", "PhD researcher"), 
                                uni_rank_cat = c(1, 2, 3, 4), 
                                id = as.logical(NA))
  
  epreds <- tibble::tibble(newdata) %>% 
    tidybayes::add_epred_draws(ordinal_practice_mod, re_formula = NA)
  
  probs_scenario <- epreds %>% 
    dplyr::filter(demo_level == "Faculty who teach methods", uni_rank_cat == 1) %>% 
    dplyr::group_by(scenario, .category) %>% 
    dplyr::summarise(estimate = median(.epred) * 100,
                     lower_hpd = lower_hpd(.epred) * 100, 
                     upper_hpd = upper_hpd(.epred * 100)) %>%
    dplyr::mutate(assumption = assumption_label)
  
  probs_demo_level <- epreds %>% 
    dplyr::filter(scenario == "s1", uni_rank_cat == 1) %>% 
    dplyr::group_by(demo_level, .category) %>% 
    dplyr::summarise(estimate = median(.epred) * 100,
                     lower_hpd = lower_hpd(.epred) * 100, 
                     upper_hpd = upper_hpd(.epred)* 100) %>%
    dplyr::mutate(assumption = assumption_label)
  
  probs_uni_rank_cat <- epreds %>% 
    dplyr::filter(scenario == "s1", demo_level == "Faculty who teach methods") %>% 
    dplyr::group_by(uni_rank_cat, .category) %>% 
    dplyr::summarise(estimate = median(.epred)* 100,
                     lower_hpd = lower_hpd(.epred)* 100, 
                     upper_hpd = upper_hpd(.epred)* 100) %>%
    dplyr::mutate(assumption = assumption_label)
  
  conditional_probs <- list(
    probs_scenario = probs_scenario, 
    probs_demo_level = probs_demo_level, 
    probs_uni_rank_cat = probs_uni_rank_cat
  )
  
  return(conditional_probs)
}


conditional_probs_inf <- function(ordinal_practice_mod_s1, ordinal_practice_mod_s2, assumption_label){
  
  # assumption_label = "Influential cases"
  # ordinal_practice_mod_s1 = mod_inf_mi_s1
  # ordinal_practice_mod_s2 = mod_inf_mi_s2
  
  newdata <- tidyr::expand_grid(demo_level = c("Faculty who teach methods", "Faculty", "Postdoctoral researcher", "PhD researcher"), 
                                uni_rank_cat = c(1, 2, 3, 4), 
                                id = as.logical(NA))
  
  epreds_s1 <- tibble::tibble(newdata) %>% 
    tidybayes::add_epred_draws(ordinal_practice_mod_s1, re_formula = NA)
  
  epreds_s2 <- tibble::tibble(newdata) %>% 
    tidybayes::add_epred_draws(ordinal_practice_mod_s2, re_formula = NA)
  
  probs_scenario_s1 <- epreds_s1 %>% 
    dplyr::filter(demo_level == "Faculty who teach methods", uni_rank_cat == 1) %>% 
    dplyr::group_by(.category) %>% 
    dplyr::summarise(estimate = median(.epred)* 100,
                     lower_hpd = lower_hpd(.epred)* 100, 
                     upper_hpd = upper_hpd(.epred)* 100) %>%
    dplyr::mutate(
      scenario = "s1",
      assumption = assumption_label)
  
  probs_scenario_s2 <- epreds_s2 %>% 
    dplyr::filter(demo_level == "Faculty who teach methods", uni_rank_cat == 1) %>% 
    dplyr::group_by(.category) %>% 
    dplyr::summarise(estimate = median(.epred)* 100,
                     lower_hpd = lower_hpd(.epred)* 100, 
                     upper_hpd = upper_hpd(.epred)* 100) %>%
    dplyr::mutate(
      scenario = "s2",
      assumption = assumption_label)
  
  probs_demo_level <- epreds_s1 %>% 
    dplyr::filter(uni_rank_cat == 1) %>% 
    dplyr::group_by(demo_level, .category) %>% 
    dplyr::summarise(estimate = median(.epred)* 100,
                     lower_hpd = lower_hpd(.epred)* 100, 
                     upper_hpd = upper_hpd(.epred)* 100) %>%
    dplyr::mutate(
      assumption = assumption_label)
  
  probs_uni_rank_cat <- epreds_s1 %>% 
    dplyr::filter(demo_level == "Faculty who teach methods") %>% 
    dplyr::group_by(uni_rank_cat, .category) %>% 
    dplyr::summarise(estimate = median(.epred)* 100,
                     lower_hpd = lower_hpd(.epred)* 100, 
                     upper_hpd = upper_hpd(.epred)* 100) %>%
    dplyr::mutate(assumption = assumption_label)
  
  conditional_probs <- list(
    probs_scenario = rbind(probs_scenario_s1, probs_scenario_s2), 
    probs_demo_level = probs_demo_level, 
    probs_uni_rank_cat = probs_uni_rank_cat
  )
  
}