LoadPackages <- function(){
  
  if (!require(pacman)) install.packages("pacman") 
  
  pacman::p_load(dplyr,tidyr,ggplot2,ggthemes,plotly,readxl, lubridate, 
                 tidyquant,ggpubr, ggrepel,kableExtra,knitr,ggpmisc,mgcv,
                 tbl2xts,ISLR,PerformanceAnalytics,PortfolioAnalytics,flexdashboard,
                 OECD,RcppRoll,janitor, kableExtra,rdbnomics)
  
}



