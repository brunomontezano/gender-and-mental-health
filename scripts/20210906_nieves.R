#' Author: Bruno Braga Montezano
#' Subject: Influência do gênero na prevalência dos transtornos mentais em adultos

library(magrittr)

# Import -----------------------------------------------------------------------
ds <- readr::read_csv("data/coorte-t1-t2-24-08-17.csv")

ds <- janitor::clean_names(ds) %>%
  dplyr::filter(!is.na(bipolar_conferido))

# Tidy -------------------------------------------------------------------------
ds <- ds %>%
  dplyr::mutate(sexo = factor(dplyr::case_when(
    a03sexo_t2 == 2 ~ "Feminino",
    a03sexo_t2 == 1 ~ "Masculino"
  )),
    tag = dplyr::case_when(
      mini_p06_t2 == 1 |
        mini_p07_t2 == 1 |
        mini_p08_t2 == 1 ~ 1,
      TRUE ~ 0),
    fobsoc = dplyr::case_when(
      mini_g06_t2 == 1 |
        mini_g07_t2 == 1 ~ 1,
      TRUE ~ 0),
    tept = dplyr::case_when(
      mini_j06_t2 == 10 ~ 1,
      mini_j06_t2 == 0 ~ 0),
    toc = dplyr::case_when(
      mini_i07d_t2 == 1 |
        mini_i07e_t2 == 1 |
        mini_i08_t2 == 1 |
        mini_i09_t2 == 1 ~ 1,
      TRUE ~ 0)) %>%
  dplyr::mutate(
    dplyr::across(c("cocaina2_t2", "tabaco2_t2", "alcool2_t2", "maconha2_t2"),
      ~ tidyr::replace_na(.x, 0))
  ) %>%
  dplyr::rename(bipolar = bipolar_conferido,
    tpanico = mini_e08b_t2)

# Model ------------------------------------------------------------------------

chi <- ds %>%
  dplyr::select(a03sexo_t2, depressao, bipolar, tag,
    tept, tpanico, fobsoc, toc, alcool2_t2, cocaina2_t2,
    maconha2_t2, tabaco2_t2) %>%
  tidyr::gather(key = variable, value = value, -a03sexo_t2) %>%
  dplyr::group_by(variable) %>%
  dplyr::do(
    chisq.test(.$a03sexo_t2, .$value) %>%
      broom::tidy()
  ) %>%
  dplyr::select(-parameter, -method) %>%
  dplyr::mutate(variable = stringr::str_remove(variable, pattern = "_t2")) %>%
  dplyr::mutate(sig = ifelse(p.value < 0.05, "Sim", "Não")) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), round, 10))

chi

or <- ds %>%
  dplyr::select(a03sexo_t2, depressao, bipolar, tag,
    tept, tpanico, fobsoc, toc, alcool2_t2, cocaina2_t2,
    maconha2_t2, tabaco2_t2) %>%
  tidyr::gather(key = variable, value = value, -a03sexo_t2) %>%
  split(.$variable) %>%
  purrr::map(~fisher.test(x=.$value, y=.$a03sexo_t2)) %>%
  purrr::map(broom::tidy) %>%
  dplyr::bind_rows(.id = "transtorno") %>%
  dplyr::select(-method, -alternative) %>%
  dplyr::mutate(transtorno = stringr::str_remove(transtorno, pattern = "_t2")) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), round, 10))

or

# Visualize --------------------------------------------------------------------

# tema minimal
ggplot2::theme_set(ggplot2::theme_minimal())

# figura 1
fig_1 <- ds %>%
  dplyr::select(a03sexo_t2, depressao, bipolar, tag,
    tept, tpanico, fobsoc, toc, alcool2_t2, cocaina2_t2,
    maconha2_t2, tabaco2_t2) %>%
  tidyr::gather(key = variable,
    value = value,
    -a03sexo_t2) %>%
  dplyr::group_by(variable, a03sexo_t2) %>%
  dplyr::summarise(freq = mean(value == 1)) %>%
  ggplot2::ggplot(ggplot2::aes(as.factor(variable),
    freq,
    fill = as.factor(a03sexo_t2))) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::scale_y_continuous(labels = scales::percent_format()) +
  ggplot2::scale_fill_viridis_d(option = "D",
    name = "Gênero",
    labels = c("Masculino", "Feminino"),
    direction = -1) +
  ggplot2::labs(x = "Transtorno mental",
    y = "Prevalência de sujeitos com o transtorno (%)") +
  ggplot2::scale_x_discrete(labels = c("alcool2_t2" = "Álcool",
    "bipolar" = "TB",
    "cocaina_t2" = "Cocaína",
    "depressao" = "TDM",
    "fobsoc" = "Fobia social",
    "maconha2_t2" = "Maconha",
    "tpanico" = "Pânico",
    "tabaco2_t2" = "Tabaco",
    "tag" = "TAG",
    "tept" = "TEPT",
    "toc" = "TOC",
    "cocaina2_t2" = "Cocaína")) +
  ggplot2::theme(text = ggplot2::element_text(size = 10))

fig_1

# figura 2
fig_2 <- or %>%
  dplyr::mutate(dir = ifelse(estimate > 1,
    "Mulheres apresentam maior risco",
    "Homens apresentam maior risco")) %>%
  ggplot2::ggplot(ggplot2::aes(y=transtorno,
    x=estimate,
    label=transtorno,
    color = dir)) +
  ggplot2::geom_point(size=3, shape=19) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin=conf.low,
    xmax=conf.high),
    height=.3) +
  ggplot2::geom_vline(xintercept=1, linetype='longdash') +
  ggplot2::scale_y_discrete(labels = c(
    "alcool2" = "Abuso/dependência de álcool",
    "bipolar_conferido" = "Transtorno bipolar",
    "cocaina2" = "Abuso/dependência de cocaína",
    "depressao" = "Transtorno depressivo maior",
    "fobsoc" = "Fobia social",
    "maconha2" = "Abuso/dependência de maconha",
    "tabaco2" = "Abuso/dependência de tabaco",
    "tag" = "Transtorno de ansiedade generalizada",
    "toc" = "Transtorno obsessivo-compulsivo",
    "tept" = "Transtorno de estresse pós-traumático",
    "tpanico" = "Transtorno de pânico"
  )) +
  ggplot2::coord_cartesian(xlim = c(0, 8),
    expand = TRUE) +
  ggplot2::scale_x_continuous(breaks = seq(0, 8)) +
  ggplot2::theme(text = ggplot2::element_text(size = 10),
    legend.position = "top",
    legend.title = ggplot2::element_blank(),
    legend.direction = "horizontal") +
  ggplot2::labs(x = "Odds Ratio (razão de chances)",
    y = "Transtorno mental") +
  ggplot2::scale_color_viridis_d(option = "D", name = "", direction = -1)

fig_2

# Export -----------------------------------------------------------------------
ggplot2::ggsave(filename = "figures/figure1.png",
  plot = fig_1,
  dpi = 300)

ggplot2::ggsave(filename = "figures/figure2.png",
  plot = fig_2,
  dpi = 300)

readr::write_csv(chi,
  file = "output/chi.csv")

readr::write_csv(or,
  file = "output/or.csv")
