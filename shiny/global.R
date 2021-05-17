
####################################### Setup #####################################################

# Packages
library(shiny)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(data.table)
library(plotly)

# ggplot theme 
t <- theme_classic() + 
  theme(text = element_text(family = "serif", size = 15),
        strip.background = element_blank())
theme_set(t)

####################################### Static data ###############################################

counties.df <- data.table::fread("www/nomComunas.csv", encoding = "UTF-8")

####################################### Functions #################################################

# For obtaining values for each 'market' 
getNomComs_from_market <- function(market, counties) {
  codComs <- getCodComs_from_market(market) 
  
  nomComs <- paste0(sort(counties$nom_com_rbd[ counties$cod_com_rbd %in% codComs ]),
                    collapse = ", ")
  
  return(nomComs)
}

getCodComs_from_market <- function(market) {
  codComs <- case_when(
    market == "Santiago" ~ list(c(13101, 13102, 13103, 13104, 13105, 13106, 13107, 13108, 13109, 
                                  13110, 13111, 13112, 13113, 13114, 13115, 13116, 13117, 13118, 
                                  13119, 13120, 13121, 13122, 13123, 13124, 13125, 13126, 13127, 
                                  13128, 13129, 13130, 13131, 13132, 13201, 13202, 13203, 13301, 
                                  13302, 13401, 13604, 13605)), 
    market == "Concepción" ~ list(c(8101, 8110, 8103, 8108, 8112, 8107, 8111, 8102, 8106, 8105)), 
    market == "Valparaíso - Viña" ~ list(c(5101, 5109)), 
    market == "Iquique - Alto Hospicio" ~ list(c(1101, 1107)), 
    market == "Coquimbo - La Serena" ~ list(c(4101, 4102)), 
    market == "Rancagua" ~ list(6101), 
    market == "Puerto Montt" ~ list(10101), 
    market == "Punta Arenas" ~ list(12101)
  )
  
  codComs <- unlist(codComs)
  
  return(codComs)
}

getSaeYear_from_market <- function(market) {
  year <- case_when(
    market == "Santiago" ~ 2020,
    market == "Concepción" ~ 2019, 
    market == "Valparaíso - Viña" ~ 2019, 
    market == "Iquique - Alto Hospicio" ~ 2018, 
    market == "Coquimbo - La Serena" ~ 2018, 
    market == "Rancagua" ~ 2018, 
    market == "Puerto Montt" ~ 2018, 
    market == "Punta Arenas" ~ 2017
  )
  
  return(year)
}


# For making the primary charts 
plotSegregationIndex <- function(df, index = "Duncan", SaeYear = 2018, round.digits = 3) {
  totals <- df %>% 
    group_by(agno, prioritario) %>% 
    summarise(total = n())
  
  schools <- df %>% 
    group_by(agno, rbd, prioritario) %>% 
    summarise(num = n()) %>% 
    merge(totals, by = c("agno", "prioritario")) %>% 
    mutate(val = num/total)
  
  df1 <- merge(
    x = schools %>% filter(prioritario == 0) %>% select(agno, rbd, val),
    y = schools %>% filter(prioritario == 1) %>% select(agno, rbd, val), 
    by = c("agno", "rbd"), all = TRUE
  ) %>% (function(x) {x[is.na(x)] <- 0; return(x)})
  
  if (index == "Duncan") {
    plotdf <- df1 %>% 
      mutate(adding_val = abs(val.x - val.y)) %>% 
      group_by(agno) %>% 
      summarise(index = round(1/2*sum(adding_val), round.digits)) 
  } 
  
  if (index == "Hutchens") {
    plotdf <- df1 %>% 
      mutate(adding_val = sqrt(val.x*val.y)) %>% 
      group_by(agno) %>% 
      summarise(index = round(1 - sum(adding_val), round.digits))
    
  }
  
  
  p <- ggplot(plotdf, aes(x = agno, y = index, label = index)) + 
    geom_point() + 
    geom_line(linetype = "dashed") + 
    geom_text_repel() + 
    geom_vline(xintercept = SaeYear, color = "red") + 
    scale_x_continuous(breaks = unique(plotdf$agno)) +
    scale_y_continuous(limits = c(0, max(plotdf$index)*1.5)) + 
    labs(x = "Year", y = "Index", title = paste(index, "Segregation Index"))
  
  return(p) 
}

plotSimce <- function(df, test = c("Math", "Spanish"), SaeYear = 2018, fun = c("Mean", "Median"),
                      round.digits = 1) {
  
  if (fun == "Mean") {
    f = mean
  } else if (fun == "Median") {
    f = median
  }
  
  if (test == "Math") {
    df1 <- df %>% 
      select(agno, mrun, simce = prom_mate4b_rbd, prioritario)
  } else if (test == "Spanish") {
    df1 <- df %>% 
      select(agno, mrun, simce = prom_lect4b_rbd, prioritario)
  }
  
  plotdf <- df1 %>% 
    filter(!is.na(simce)) %>%
    mutate(group = ifelse(prioritario == 1, "Minority", "Regular")) %>% 
    group_by(agno, group) %>% 
    summarise(simce = round(f(simce), round.digits))
  
  p <- ggplot(plotdf, aes(x = agno, y = simce, label = simce, group = group, color = group)) + 
    geom_line(linetype = "dashed") + 
    geom_point() + 
    geom_text_repel() +
    geom_vline(xintercept = SaeYear, color = "red") + 
    scale_x_continuous(breaks = unique(plotdf$agno)) +
    labs(x = "Year", y = paste(fun, "simce"), color = "", title = paste(test, "4B SIMCE Scores"), 
         subtitle = "Time series by group") 
  
  return(p)
}

plotSimceDif <- function(df, test = c("Math", "Spanish"), SaeYear = 2018, 
                         fun = c("Mean", "Median"), round.digits = 1) {
  
  if (fun == "Mean") {
    f = mean
  } else if (fun == "Median") {
    f = median
  }
  
  if (test == "Math") {
    df1 <- df %>% 
      select(agno, mrun, simce = prom_mate4b_rbd, prioritario)
  } else if (test == "Spanish") {
    df1 <- df %>% 
      select(agno, mrun, simce = prom_lect4b_rbd, prioritario)
  }
  
  df2 <- df1 %>% 
    filter(!is.na(simce)) %>% 
    group_by(agno, prioritario) %>% 
    summarise(simce = f(simce)) 
  
  plotdf <- merge(
    x = df2 %>% filter(prioritario == 0), 
    y = df2 %>% filter(prioritario == 1), 
    by = "agno"
  ) %>% mutate(simceDif = round(simce.x - simce.y, round.digits))
  
  p <- ggplot(plotdf, aes(x = agno, y = simceDif, label = simceDif)) + 
    geom_line(linetype = "dashed") + 
    geom_point() + 
    geom_text_repel() +
    geom_vline(xintercept = SaeYear, color = "red") + 
    scale_x_continuous(breaks = unique(plotdf$agno)) +
    labs(x = "Year", y = paste(fun, "simce"), color = "", title = paste(test, "4B SIMCE Scores"), 
         subtitle = "Difference time series") 
  
  return(p)
}

