#=========================================================================================
#==        transform & plot                                                             ==
#==                                                                                     ==
#==        df1 is a dataframe                                                           ==
#==        df2 is to contain the sp_shade dataframe                                     ==
#==        col1 is the column of df1 to plot                                            ==
#==        col2 is either, level - split timeseries into 3 levels, or                   ==
#==                        both  - split timeseries into 3 levels and 6 month delta     ==
#=========================================================================================

trans.plot <- function(df1, df2, col1, col2) {
  x1 <- enquo(col1)
  x1a<- paste0(quo_name(x1), " : ")
  x1b<- enquo(col2)
  x2 <- df1 %>% 
    select(date, fwd_rtn_m, !!x1) %>% 
    mutate(x1.lag6  = lag(!!x1, 6)) %>%
    mutate(x1.lag12 = lag(!!x1, 12)) %>% 
    mutate(x1.qntlx = ntile(!!x1, 3)) %>% 
    mutate(x1.qntl = case_when(x1.qntlx == 1 ~ "_low", 
                               x1.qntlx == 2 ~ "_mid", 
                               x1.qntlx == 3 ~ "_high")) %>%
    mutate(x1.rtn6  = !!x1 - x1.lag6) %>% 
    mutate(x1.rtn12 = !!x1 - x1.lag12) %>%
    mutate(x1.delta = case_when(quo_name(x1b) == "level" ~ "", 
                                quo_name(x1b) == "both"  ~ if_else(!!x1 > lag(!!x1, n = 6), 
                                                                   "incr", 
                                                                   "decr"))) %>%  
    unite(x1_lag00, c(x1.qntl, x1.delta),sep="_", remove = FALSE) %>%
    mutate(x1_lag06 = lag(x1_lag00, 6)) %>%                            
    mutate(x1_lag12 = lag(x1_lag00, 12)) %>%                           
    filter(!is.na(x1.lag12))
  
  x3 <- predict(dummyVars(" ~ x1_lag00", data = x2), newdata = x2)
  x4 <- predict(dummyVars(" ~ x1_lag06", data = x2), newdata = x2)
  x5 <- predict(dummyVars(" ~ x1_lag12", data = x2), newdata = x2)
  x6 <- as.tibble(cbind(x3, x4, x5)) %>% select(-contains("NA")) %>% 
    rownames_to_column(var = 'rowIndex') %>% 
    gather(key = 'Indicator', value = 'Value', -rowIndex) %>% 
    mutate(Value_fact = ifelse(Value == 1, "In", "Out"))
  x7 <- x2 %>% select(date, fwd_rtn_m) %>% rownames_to_column(var = 'rowIndex')
  
  ### data for plot ###
  x8 <- full_join(x6, x7, by  = 'rowIndex') %>% 
    mutate(Indicator = str_replace(Indicator, "x1_", !!x1a))
  
  ### data for ks test ###
  x8.1<-x8 %>% select(Indicator, date, Value_fact, fwd_rtn_m) %>% 
    spread(Value_fact, fwd_rtn_m) %>% nest(-Indicator)
  x8.2<-x8.1 %>% mutate(ks_fit = map(data, ~ks.test(.$In, .$Out)),
                        p_val  = map_dbl(ks_fit, "p.value"))
  
  ### data for for text ###
  x9 <- x8 %>% group_by(Value_fact, Indicator) %>% summarise(Mean = mean(fwd_rtn_m))
  x9.1<-x9 %>% spread(Value_fact, Mean) %>% mutate(mean_diff = In - Out)
  
  ### plot ###
  x10<- ggplot(data = x8, aes(x = fwd_rtn_m, colour = Value_fact, fill = Value_fact)) + 
    geom_density(alpha = 0.3) + 
    geom_text(data = x9.1, size = 2.5, (aes(x = -0.25, y = 12, label = paste0("Difference in\nmean ", percent(round(mean_diff,4)), sep =" "), colour = NULL, fill = NULL)), hjust = 0) +
    geom_text(data = x8.2, size = 2.5, (aes(x = -0.25, y = 8, label = paste0("KS pvalue ", percent(round(p_val,4)), sep =" "), colour = NULL, fill = NULL)), hjust = 0) +
    geom_vline(data     = x9, aes(xintercept = Mean, colour = Value_fact),
               linetype = "dashed", size=0.5) +
    labs(title          = "Subsequent month returns", 
         subtitle       = "(conditioned on binary indicator as specified)",
         caption        = " The orange distribution represents subsequent monthly returns\nduring periods when the indicator has been triggered.\nThe blue distribution represent subsequent returns during all other periods.", 
         x              = "", 
         y              = "") +
    facet_wrap(~ Indicator) +  ###, ncol = 4
    theme_grey() +
    theme(plot.title    = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(face = "italic", size = 10),
          plot.caption  = element_text(face = "italic", size = 8),
          axis.title.y  = element_text(face = "italic", size = 9),
          axis.title.x  = element_text(face = "italic", size = 7))
  
  
  ##### plot of S&P500 and market in/out shading #####
  x11<-ggplot(data        = df1, 
              aes(x        = date, 
                  y        = close, 
                  group    = 1)) +
    geom_line() +
    scale_y_log10() +
    geom_rect(data        = df2, 
              inherit.aes = FALSE,
              aes(xmin    = start, xmax = end, ymin = 0, ymax = Inf), 
              fill        ='lightblue', alpha=0.5) +
    theme_minimal() +
    labs(title            = "S&P500", 
         subtitle         = "log scale",
         caption          = "", 
         x                = "Year",
         y                = "Close") +
    geom_hline(yintercept = 0, color = "black") +
    theme(plot.title      = element_text(face = "bold", size = 14),
          plot.subtitle   = element_text(face = "italic", size = 9),
          plot.caption    = element_text(hjust = 0),
          axis.title.y    = element_text(face = "italic", size = 9),
          axis.title.x    = element_text(face = "italic", size = 9))
  
  
  ### plot of selected market indicator & in/out shading ###
  x12<-ggplot(data        = df1, 
              aes(x        = date, 
                  y        = !!x1,
                  group    = 1)) +
    geom_line() +
    geom_rect(data        = df2, 
              inherit.aes = FALSE,
              aes(xmin    = start, xmax = end, ymin = -Inf, ymax = Inf), 
              fill        = 'lightblue', 
              alpha       = 0.5) +
    geom_hline(yintercept = 0, color = "black") +  
    theme_minimal() +
    labs(title            = "",
         subtitle         = "",
         caption          = "", 
         x                = "Year", 
         y                = quo_name(x1)) + 
    theme(plot.title      = element_text(face  = "bold", size = 14),
          plot.subtitle   = element_text(face  = "italic", size = 9),
          plot.caption    = element_text(hjust = 0),
          axis.title.y    = element_text(face  = "italic", size = 9),
          axis.title.x    = element_text(face  = "italic", size = 9))
  
  ### combine plot ###
  return(list(plot_grid(x11, x12, ncol = 1, align = 'v'), x10))   
}








###############################
### transformation function ###
###############################

# 1st arg = data, 2nd arg = column selected, 3rd arg = transform
# 3rd arg option = x1, x1.lag6/12, x1.rtn6/12, x1.qntl1/2/3, x1.pos (a factor)
# 3rd arg will return a column labelled "x1.sel"

trans.fun <- function(df, col1, col2) {
  x1 <- enquo(col1) 
  x2 <- enquo(col2)
  x3 <- df %>% 
    mutate(x1.lag6  = lag(!!x1, 6)) %>%
    mutate(x1.lag12 = lag(!!x1, 12)) %>% 
    mutate(x1.qntl  = ntile(!!x1, 3)) %>% 
    mutate(x1.qntl1 = if_else(x1.qntl == 1, 1, 0)) %>%    #low
    mutate(x1.qntl2 = if_else(x1.qntl == 2, 1, 0)) %>%    #mid
    mutate(x1.qntl3 = if_else(x1.qntl == 3, 1, 0)) %>%    #high
    mutate(x1.rtn6  = !!x1 - x1.lag6) %>%                  #needs if condition for ROC - rates v prices
    mutate(x1.rtn12 = !!x1 - x1.lag12) %>%                 #needs if condition for ROC - rates v prices
    mutate(x1.delta = if_else(!!x1 > lag(!!x1, n = 6), "Incr", "Decr")) %>%
    unite(x1.pos, c(x1.qntl, x1.delta),sep="_", remove = FALSE) %>%
    mutate(x1.pos.lag6 = lag(x1.pos, 6)) %>% 
    mutate(x1.pos.lag12 = lag(x1.pos, 12)) %>%
    filter(!is.na(x1.lag12)) %>%
    rename(x1 = !!x1) %>% 
    mutate(x1.sel = !!x2)
  
  x4 <- predict(dummyVars(" ~ x1.pos", data = x3), newdata = x3)
  return(cbind(x3,x4))
  
}



#################################
### winsorize & standardise   ###
#################################

wins.stds <- function(x, ...) {
  y     <- enquos(...)
  col_name <- paste0(quo_name(...),".stds")
  x %>% select(date, y1, !!!y) %>% 
    rename_at(vars(-date, -y1), !!!col_name := y) %>% 
    filter_all(all_vars(!is.na(.))) %>% 
    mutate_at(vars(contains("_10")), funs(Winsorize(.))) %>% 
    mutate_at(vars(contains("_10")), funs(scale(.)))
}

#https://stackoverflow.com/questions/46206677/concatenate-quosures-and-string
#https://adv-r.hadley.nz/evaluation.html#wrapping-quoting-functions

##### last working version #####

#wins.stds <- function(x, ...) {
#  y     <- quos(...)
#  #col_name <- paste0(quo_name(...),".stds")  ### error originating here ###
#  x %>% select(date, y1, !!!y) %>% 
#    #rename_at(vars(-date, -y1), !!!col_name := y) %>% 
#    filter_all(all_vars(!is.na(.))) %>% 
#    mutate_at(vars(contains("_10")), funs(Winsorize(.))) %>% 
#    mutate_at(vars(contains("_10")), funs(scale(.)))
#}




###########################
### winsorize #############
###########################

wins05 <- function(x) {
  lim <- quantile(x, probs=c(0.05, 0.95))
  x[ x < lim[1] ] <- lim[1]
  x[ x > lim[2] ] <- lim[2]
  x
}



###########################
### wins      #############
###########################

wins05x = function(x, cut = 0.05){
  cut_point_top <- quantile(x, 1 - cut, na.rm = T)
  cut_point_bottom <- quantile(x, cut, na.rm = T)
  i = which(x >= cut_point_top) 
  x[i] = cut_point_top
  j = which(x <= cut_point_bottom) 
  x[j] = cut_point_bottom
  return(x)
}



#################################
### winsorize & scale  0 to 1 ###
#################################

wins.scale <- function(df, col) {
  expr <- enquo(col)
  col_name <- paste0(quo_name(expr),".scale")
  df %>% 
    mutate(x1 = case_when(!!expr < quantile(!!expr, 0.05) ~ quantile(!!expr, 0.05),
                          !!expr > quantile(!!expr, 0.95) ~ quantile(!!expr, 0.95),
                          !!expr > quantile(!!expr, 0.05) | !!expr < quantile(!!expr, 0.95) ~ !!expr)) %>% 
    mutate(x2 = (x1-min(x1))/(max(x1)-min(x1))) %>% 
    select(date, x2) %>% 
    rename(!!col_name := x2)
}
