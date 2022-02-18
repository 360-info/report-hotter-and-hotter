library(mosaic)
library(cli)

label_risk <- function(x) {
  pct_risk <- scales::percent(abs(x - 1))
  moreless <- ifelse(x > 1, "more", "less")
  paste(pct_risk, moreless, "likely")
}
label_range <- function(x) {
  paste(
    scales::percent(attr(x, "lower.RR")), "to",
    scales::percent(attr(x, "upper.RR")))
}
rr_statsig <- function(x) {
  !between(1, attr(x, "lower.RR"), attr(x, "upper.RR"))
}

risk_subtitle <- function(risk, disadvantage, advantage, bad_outcome) {
  glue(
          "{if_else(rr_statsig(risk), '☑️', '❌')} ",
          "People with <= 12 hrs electricty a day were ",
          "{label_risk(attr(risk, 'RR'))} ",
          "to experience a headache than non-fan users. ",
          "(RR: {label_range(risk)})")
}

rate_diff_plot <- function(df, bad_outcome, good_outcome, disadvantage,
  advantage, phrase_disadvantage, phrase_advantage, phrase_bad_outcome) {

  # build the contingency table:
  # "people w/ X disadvantage were Y% more/less likely to experience Z bad
  # outcome"
  contingency_table <- data.frame(
    bad_outcome = c(
      df %>% filter({{ bad_outcome }}, {{ advantage }}) %>% pull(n),
      df %>% filter({{ bad_outcome }}, {{ disadvantage }}) %>% pull(n)),
    good_outcome = c(
      df %>% filter({{ good_outcome }}, {{ advantage }}) %>% pull(n),
      df %>% filter({{ good_outcome }}, {{ disadvantage }}) %>% pull(n)))
  rownames(contingency_table) <- c("advantage", "disadvantage")
  print(contingency_table)

  # calculate the relative risk
  risk <- orrr(contingency_table)
  if (between(1, attr(risk, "lower.RR"), attr(risk, "upper.RR"))) {
    cli_bullets(c(
      "x" = "This risk difference is not statistically significant at 95% CI",
      "i" = paste("RR:", attr(risk, "lower.RR"), "to", attr(risk, "upper.RR"))
    ))
  }

  # ceate the plot
  risk_plot <- ggplot(filter(df, {{ bad_outcome }})) +
      aes(y = predictor_prop, x = hours_day, fill = experience_headache) +
      geom_col() +
      geom_richtext(
        aes(label = glue(
          "**{scales::percent(predictor_prop)} of respondents**<br>",
          "({n} of {predictor_n} surveyed)")),
        colour = "white", fill = NA, label.colour = NA,
        hjust = "inward", nudge_y = -0.00125, size = 5) +
      scale_y_continuous(labels = scales::label_percent()) +
      coord_flip() +
      labs(subtitle = risk_subtitle(risk, phrase_disadvantage, phrase_advantage,
        phrase_bad_outcome))

    return(risk_plot)
}
