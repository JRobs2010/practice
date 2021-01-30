#dfg 

    library(readr)
    library(anytime)
    library(ggplot2)
    library(gtools)
    library(dplyr)
    library(tidyverse)
    library(readxl)
    library(lubridate)
    library(Lahman)
    library(dslabs)
    library(gridExtra)
    library(broom)
    ds_theme_set()



get_slope <- function(x, y) cor(x, y) * sd(y) / sd(x)
get_intercept <- function(x, y) mean(y) - cor(x,y) * sd(y)/sd(x) * mean(x)



# PLOT EXAMPLES

# HR vs R

    Teams %>% filter(yearID %in% 1961:2001) %>%
      mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
      ggplot(aes(HR_per_game, R_per_game)) + 
      geom_point(alpha = 0.5) +
      geom_abline(aes(slope = get_slope(HR_per_game, R_per_game), intercept = get_intercept(HR_per_game, R_per_game)))


# SB vs R

    Teams %>% filter(yearID %in% 1961:2001) %>%
      mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
      ggplot(aes(SB_per_game, R_per_game)) + 
      geom_point(alpha = 0.5)+
      geom_abline(aes(slope = get_slope(SB_per_game, R_per_game), intercept = get_intercept(SB_per_game, R_per_game)))


# AB vs R

    Teams %>% filter(yearID %in% 1961:2001) %>%
      mutate(AB_per_game = AB / G ,R_per_game = R / G) %>%
      ggplot(aes(AB_per_game, R_per_game)) +
      geom_point(alpha = 0.5) +
      geom_abline(aes(slope = get_slope(AB_per_game, R_per_game), intercept = get_intercept(AB_per_game, R_per_game)))
  


# X3B vs X2B

    Teams %>% filter(yearID %in% 1961:2001) %>%
      mutate(X3B_per_game = X3B / G, X2B_per_game = X2B/ G) %>%
      ggplot(aes(X3B, X2B)) +
      geom_point(alpha = 0.5) +
      geom_abline(aes(slope = get_slope(X3B_per_game, X2B_per_game), intercept = get_intercept(X3B_per_game, X2B_per_game)))


# BB vs R
    
    Teams %>% filter(yearID %in% 1961:2001) %>%
      mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
      ggplot(aes(BB_per_game, R_per_game)) + 
      geom_point(alpha = 0.5) +
      geom_abline(aes(slope = get_slope(BB_per_game, R_per_game), intercept = get_intercept(BB_per_game, R_per_game)))






    Teams %>% filter(yearID %in% 1961:2001) %>%
      mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
      summarise(r = cor(BB_per_game, R_per_game), sd_BB = sd(BB_per_game), sd_R = sd(R_per_game))
    
    0.5502086 * 0.5885791 / 0.4404034                           # slope   =   0.735
    
    
    Teams %>% filter(yearID %in% 1961:2001) %>%
      mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
      summarise(slope = get_slope(BB_per_game, R_per_game))     # slope   =   0.735




# CORRELATION COEFFICIENT
#       CC data wrangling

        library(HistData)
        data("GaltonFamilies")
        
        
        
        # Filter Father and son heights 
        #________________________________________________________________________________________
        set.seed(1983)
        galton_heights <- GaltonFamilies %>%
          filter(gender == "male") %>%
          group_by(family) %>%
          sample_n(1) %>%
          ungroup() %>%
          select(father, childHeight) %>%
          rename(son = childHeight)
        
        galton_heights
        
        # Plot father and son heights (w/o regression)
        #________________________________________________________________________________________
        galton_heights %>% ggplot(aes(father, son)) + 
          geom_point(alpha = 0.5)
        






#       CC calculation


        # Population correlation coefficient
        #________________________________________________________________________________________
        galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)
        
                                #OR
        
        galton_heights %>% summarise(r = mean(scale(father) * scale(son)))






  
#       Monte carlo sample distribution of CC estimates 
        #NOTE: This is a random sample since sampling from samplsize of 25 and NOT the population 
        
        
        
        # Random sample correlation coefficient 
        #________________________________________________________________________________________
        set.seed(0)
        R1 <- sample_n(galton_heights, 25, replace = TRUE) %>% 
          summarize(r = cor(father, son)) %>% pull(r)
        R1
        
        
        
        
        # Monte carlo sample distribution of correlation coefficients
        #________________________________________________________________________________________
        set.seed(0)
        B <- 1000
        N <- 25
        R2 <- replicate(B, {
          sample_n(galton_heights, N, replace = TRUE) %>% 
            summarize(r=cor(father, son)) %>% 
            pull(r)
        })
        
        
        
        
        # Calculate sample correlation coefficient estimate & se 
        #________________________________________________________________________________________
        mean(R2)
        # answer = 0.4356673
        
        sd(R2)
        # answer = 0.1614781
        





#       Monte carlo sample distribution of CC estimates noramlity analysis (qq-plot & histogram) 
        #NOTE: Checking if samplesize of 25 is large enough????
        
        # Histogram of monte carlo sample distribution
        #________________________________________________________________________________________
        cc_hist1 <- data.frame(R2) %>% ggplot(aes(R2)) +
          geom_histogram(binwidth = .05, color = "black")
        
        cc_hist1
        
        
        # QQ -plot to compare sample distribution vs theoretical normal distribution
        #________________________________________________________________________________________
        qq_plot1 <- data.frame(R2) %>% ggplot(aes(sample=R2)) + 
          stat_qq() + 
          geom_abline(intercept = mean(R2), slope = sqrt((1-mean(R2)^2)/(N-2))) # mean(R2) = EV, sd = sqrt((1-mean(R2)^2)/(N-2))
        
        
        qq_plot1.5 <- data.frame(R2) %>% ggplot(aes(sample=R2)) + 
          stat_qq(dparams = list(mean = mean(R2), sd = sqrt((1-mean(R2)^2)/(N-2)))) + #https://journal.r-project.org/archive/2018/RJ-2018-051/RJ-2018-051.pdf
          geom_abline()
        
        
        grid.arrange(qq_plot1, qq_plot1.5)
        
        
        
        
        # grid.arrange() histogram of sample distribution of correlation coefficients and qq-plot 
        #________________________________________________________________________________________
        grid.arrange(cc_hist1, qq_plot1, ncol = 2)
        
        
        
        
        #*******    Is samplesize of 25 large enough??? ...NO   *********
        
        
        
        
        






# STRATIFIED CONDITIONAL MEAN(S) REGRESSION ESTIMATE (Condition: Father = 72 in tall)                 ####
#NOTE: r is often referred to as the correlation coefficient 




#       Stratified single conditional mean estimate (w/o regression)
        
        
        # Stratified mean height of son condition to father being 72.0 (1.1 SDs over avg)
        #________________________________________________________________________________________
        conditional_avg1 <- galton_heights %>%                # | #
          filter(round(father) == 72) %>%                     # | #        0.49 SDs < 1.1 SDs    **THE DIFFERENCE (.61) IS THE**
          summarize(avg = mean(son)) %>%                      # | #        1.1 - .49 = .61          CORRELATION COEFFICIENT
          pull(avg)                                           # v #                                   @ FATHER HEIGHT 72
        conditional_avg1                              #  70.5 (.49 SDs over avg)             
        
        
        
        conditional_avg1
        
        # answer = 70.5       ---> SON STRATAFIED REGRESSION PREDICITON CONDITION TO FATHER BEING 72 ICHES TALL
        
        
        
        
        
        
        
        # Population height avg and sd of son and father 
        #________________________________________________________________________________________
        mean(galton_heights$son) # 69.2 inches
        mean(galton_heights$father) # 69.1 inches
        
        sd(galton_heights$son) # 2.71
        sd(galton_heights$father) # 2.54
        
        # father of 72 inches is 1.1 SDs over avg
        # son conditional avg is .49 SDS over avg



#             Estimated amount of change in Y, given change in X (ρ * σ[y] / σ[x]) ------------------>####

            # CC = 0.5, 
            # SD[fathers heights] = 2
            # SD[sons heights] = 3
            
            
            
            # Given a one inch increase in a father’s height, use given values to predict amount of change in the son’s height? (Slope)
            #________________________________________________________________________________________
            
            
            .5 * (3 / 2)
            
            # answer = .75 inch change in sons height




#             Stratified son's conditional mean heights Y for each rounded father's height X--------->####
              #NOTE: E(Y|X=x) is also called conditional Y
              
              # Calculate conditional avg E(Y|X=x) height of son for each stratified father's height group 
              #________________________________________________________________________________________
              
              galton_heights %>%
                mutate(father = round(father)) %>% # stratifies fathers heights into groups that are rounded values
                group_by(father) %>%
                summarize(son_conditional_avg = mean(son)) 







 
#             Boxplot son's conditional height distribution for each rounded father's height X ------>####
              #NOTE: boxplot will show distribution of each stratified father's heights group
              
              # Boxplot conditional avg height of son for each father's height value
              #________________________________________________________________________________________
              galton_heights %>% mutate(father_strata = factor(round(father))) %>%
                ggplot(aes(father_strata, son)) +
                geom_boxplot() +  # Each center line is the conditional avg height of son for each stratified group of fathers height 
                geom_point()






#       Stratified conditional mean(s) plot --------------------------------------------------------->####


        # Plot son's conditional avg heights for eaach stratified father's heights group 
        #________________________________________________________________________________________
        ca_plot1 <- galton_heights %>%
          mutate(father = round(father)) %>%
          group_by(father) %>%
          summarize(son_conditional_avg = mean(son)) %>% 
          ggplot(aes(father, son_conditional_avg)) +
          geom_point()
        
        ca_plot1 #each point is a conditional avg 






#       Stratified conditional mean(s) regression line (standardized) ------------------------------->   Slope = ρ or (r),      Intercept = 0            ####


        # Calculate r (correlation coefficient) of father and son heights to use as standardized regression line slope
        #________________________________________________________________________________________
        
        r <- galton_heights %>% summarise(r = cor(father, son)) %>% .$r
        r # 0.4334102 <--- CC is also SLOPE of STANDARDIZED conditional avgs plot
        



        # Plot conditional averages after standardizing them and the fathers stratified groups 
        #________________________________________________________________________________________
        ca_plot2 <- galton_heights %>% 
          mutate(father = round(father)) %>% #stratifies fathers heights values into groups
          group_by(father) %>% 
          summarize(son_conditional_avg = mean(son)) %>% # creates data values of means 
          mutate(z_father = scale(father), z_son = scale(son_conditional_avg)) %>% #  ---> **STANDARDIZING THE STRATIFIED GROUPS**
          ggplot(aes(z_father, z_son)) + 
          geom_point() + 
          geom_abline(intercept = 0, slope = r) # standardizing sets intercept = 0 (scales intercept to the center), and slope = r (CC)
        
        ca_plot2  
        
        
        
        grid.arrange(ca_plot1, ca_plot2)
        
        
        #NOTE: When standardizing, x and y have mean 0 and SD 1. When substituting into regression line formula, 
        #      the terms cancel out until we have the following equation:  y[i] = ρ * x[i] .
        




# POPULATION CONDITIONAL REGRESSION PREDICTION/LINE (Condition: Father = 72 in tall)                  ####
#NOTE: This equation is more complex linear model compared to LSE/lm() function method
#       Single son's height estimate (using linear equation) ---------------------------------------->   Y = μ[Y] + ρ * (x - μ[X] / σ[X]) * σ[Y]         ####

        # Calculate variables needed for non standardized population slope and intercept 
        #________________________________________________________________________________________
        mu_x <- mean(galton_heights$father) # μ[x]
        mu_y <- mean(galton_heights$son)    # μ[y]
        sd_x <- sd(galton_heights$father)   # σ[x]
        sd_y <- sd(galton_heights$son)      # σ[y]
        r1 <- cor(galton_heights$father, galton_heights$son) # ρ
        
        r1
        
        
        # Use Y = μ[Y] + ρ * (x - μ[X] / σ[X]) * σ[Y] to calculate population regression prediction condition to father being 72 inches tall
        #________________________________________________________________________________________
        mu_y + r1*(72 - mu_x)/sd_x*sd_y
        
        
        # answer = 70.5       ---> SON POPULATION REGRESSION PREDICITON CONDITION TO FATHER BEING 72 ICHES TALL 






#       Father and son heights plot ----------------------------------------------------------------->####


        # Plot fathers heights on x-axis and sons heights on y-axis
        #________________________________________________________________________________________
        galton_heights %>% 
          ggplot(aes(father, son)) +  
          geom_point(alpha = 0.5)



#       Father and son regression line -------------------------------------------------------------->   Slope = ρ*(σ[y]/σ[x]), Intercept = μ[y] − m*μ[x]####
#             SLOPE:           Create function that computes slope ---------------------------------->####

            # SLOPE equation  m = ρ*(σ[y]/σ[x])
            #________________________________________________________________________________________
            r1 * sd_y/sd_x
            
            # answer = 0.461392
            
            
            
                                    # OR #
            
            
            
            # SLOPE function
            #________________________________________________________________________________________
            get_slope <- function(x, y) cor(x, y) * sd(y) / sd(x)
            
            get_slope(galton_heights$father, galton_heights$son)
            





#             INTERCEPT:       Create function that computes intercept ------------------------------>####


              # INTERCEPT equation  b = μ[y] − m*μ[x]
              #________________________________________________________________________________________
              mu_y - r1 * sd_y/sd_x * mu_x
              
              # answer = 37.28761
              
              
              
                                      # OR #
              
              
              # INTERCEPT function 
              #________________________________________________________________________________________
              get_intercept <- function(x, y) mean(y) - cor(x,y) * sd(y)/sd(x) * mean(x)
              
              get_intercept(galton_heights$father, galton_heights$son)
              







#             REGRESSION LINE: Use slope and intercept calculations --------------------------------->####


              # Create non standardized regression line for population father and son heights
              #________________________________________________________________________________________
              ca_plot3 <- galton_heights %>% 
                ggplot(aes(father, son)) +  
                geom_point(alpha = 0.5) +
                geom_abline(slope = r1 * sd_y/sd_x, intercept = mu_y - r1 * sd_y/sd_x * mu_x) 
              
              
              grid.arrange(ca_plot2, ca_plot3)
              
              







#       Father and son regression line (standardized) ----------------------------------------------->   Slope = ρ or (r),      Intercept = 0            ####

        # Create standardized regression line for population father and son heights
        #________________________________________________________________________________________
        ca_plot4 <- galton_heights %>% 
          ggplot(aes(scale(father), scale(son))) +  
          geom_point(alpha = 0.5) +
          geom_abline(slope = r1, intercept = 0)  # ---> NOTE: slope = CC & intercept = 0
        
        
        grid.arrange(ca_plot2, ca_plot4)



# COMPARE REGRESSION ESTIMATE & SE B/W METHODS (Condition: Father = 72 in tall)                       ####
#       Monte carlo sample distribution of stratified conditional regression prediction estimate ---->####
        #________________________________________________________________________________________
        
        B <- 1000
        N <- 50
        
        
        set.seed(1983)
        conditional_avg <- replicate(B, {
          dat <- sample_n(galton_heights, N)
          dat %>% filter(round(father) == 72) %>% 
            summarize(avg = mean(son)) %>% 
            pull(avg)
        })
        
        mean(conditional_avg, na.rm = TRUE)
        
        # answer = 70.5
        
        
        sd(conditional_avg, na.rm = TRUE)
        
        # answer = 0.96


#       Monte carlo sample distribution of population conditional regression prediction ------------->####
        #________________________________________________________________________________________
        
        B <- 1000
        N <- 50
        
        
        set.seed(1983)
        regression_prediction <- replicate(B, {
          dat <- sample_n(galton_heights, N)
          mu_x <- mean(dat$father)
          mu_y <- mean(dat$son)
          s_x <- sd(dat$father)
          s_y <- sd(dat$son)
          r <- cor(dat$father, dat$son)
          mu_y + r*(72 - mu_x)/s_x*s_y
        })
        
        
        mean(regression_prediction) #population average
        
        # answer = 70.5
        
        sd(regression_prediction)
        
        # answer = 0.44022
        
        
        
        #NOTE: This method has a more precise prediction despite same expected value as conditional mean prediciton 
        
        
        


        


# BIVARIATE NORMAL DISTRIBUTION                                                                       ####
#       Stratified bivariate normal distribution normality analysis (qq-plot) ----------------------->####

        # Plot distribution(s) of son heights at each stratified fathers height group (same as plotting individual boxes of previous boxplot at each SD)
        #________________________________________________________________________________________
        galton_heights %>%
          mutate(z_father = round((father - mean(father)) / sd(father))) %>%
          filter(z_father %in% -2:2) %>%
          ggplot() +  
          stat_qq(aes(sample = son)) +
          facet_wrap( ~ z_father) 
        
        
        #NOTE: Each plot of conditional Y values appears to be normal and therefore we have a BIVARIATE NORMAL DISTRIBUTION
        
        
        
                                             # *** SAME AS *** 
        
        
        
        
        
        
        # Sons heights distribution at -2 SD       --> 64      **summarise(SD_2 = mean(father) - (sd(father) * 2))**
        galton_heights %>%
          mutate(father = round(father)) %>%
          filter(father == 64) %>% 
          ggplot() +  
          stat_qq(aes(sample = son))
        
        
        # Sons heights distribution at -1 SD       --> 66      **summarise(SD_2 = mean(father) - (sd(father) * 1))**
        galton_heights %>%
          mutate(father = round(father)) %>%
          filter(father == 66) %>% 
          ggplot() +  
          stat_qq(aes(sample = son))
        
        # Sons heights distribution at 0 SD (mean) --> 69             **summarise(SD_2 = mean(father))**
        galton_heights %>%  
          mutate(father = round(father)) %>%
          filter(father == 69) %>% 
          ggplot() +  
          stat_qq(aes(sample = son))
          
        # Sons heights distribution at 1 SD        --> 72      **summarise(SD_2 = mean(father) + (sd(father) * 1))**
        galton_heights %>%
          mutate(father = round(father)) %>% 
          filter(father == 72) %>%
          ggplot() +  
          stat_qq(aes(sample = son))
        
        # Sons heights distribution at -2 SD       --> 64      **summarise(SD_2 = mean(father) + (sd(father) * 2))**
        galton_heights %>%
          mutate(father = round(father)) %>% 
          filter(father == 74) %>%
          ggplot() +  
          stat_qq(aes(sample = son))
                    
        #NOTE: Each dsitribution  appears normal just as it does in faceted qq-plot
        









#       Bivariate normal distribution normality analysis (qq-plot) ---------------------------------->####

        # Construct a histogram and qq-plot of father heights to determine normailty of father variable
        #________________________________________________________________________________________
        hist(galton_heights$father)
        
        galton_heights %>% ggplot() +
          stat_qq(aes(sample = father))
        
        
        
        
        # Construct a histogram and qq-plot of son heights to determine normailty of son variable
        #________________________________________________________________________________________
        hist(galton_heights$son)
        
        galton_heights %>% ggplot() +
          stat_qq(aes(sample = son))
        
        
        #NOTE: If the two distributions are BOTH normal then they are BIVARIATE NORMAL 
        
        







#       R^2 ----------------------------------------------------------------------------------------->####
        # Percent of the variation in sons’ heights (y) that is explained by fathers’ heights (x)
        #________________________________________________________________________________________
        
        # ρ^2 * 100
        
        galton_heights %>% summarize(r_squared = (cor(father, son)^2)) %>% pull(r_squared)
        
        # answer = 0.1878444
        




##### KEY NOTE: switching X and Y variables is NOT inverse (must calculate new/different regression) #####
##### KEY NOTE: regression line is the best prediction of Y given X IF bivariate normal distribution #####

#******************************************Linear Models/LM*******************************************####

# CONFOUNDING THRU STRATIFICATION                                                                        E[R | BB=x[1], HR=x[2]] = β[0] + β[1]*(x[2])*x[1] + β[2]*(x[1])*x[2]####

#       Initial Association (BB/G v R/G) (Singles/G v R/G) (HR/G v R/G) ----------------------------->####

        # Calculate WALKS (BB) vs RUNS (R) initial association
        #_______________________________________________________________________________________
        bb_slope <- Teams %>% 
          filter(yearID %in% 1961:2001 ) %>% 
          mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
          summarize(slope = get_slope(BB_per_game, R_per_game))
        bb_slope
        
        # answer = 0.7353288
        
        
        
         
        # Calculate SINGLES vs RUNS (R) initial association
        #_______________________________________________________________________________________
        singles_slope <- Teams %>% 
          filter(yearID %in% 1961:2001 ) %>%
          mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
          summarize(slope = get_slope(Singles_per_game, R_per_game))
        singles_slope
        
        # answer = 0.4494253 ---> WHY NOT THE SAME AS WALKS vs RUNS 
        
        
        
        
        # Calculate HOMERUNS (HR) vs RUNS (R) initial association
        #_______________________________________________________________________________________
        hr_slope <- Teams %>% 
          filter(yearID %in% 1961:2001 ) %>%
          mutate(HR_per_game = HR/G, R_per_game = R/G) %>%
          summarize(slope = get_slope(HR_per_game, R_per_game))
        hr_slope
        
        
        # answer = 1.844824




#       Pick confounding variable (HRs) ------------------------------------------------------------->####

        # Compute CC between all 3 variables (HR, Singles, BB)
        #_______________________________________________________________________________________
        Teams %>% 
          filter(yearID %in% 1961:2001 ) %>% 
          mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
          summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles), cor(HR, R))
        
        #     (BB, HR)      --->    Teams with more BBs hit more HRs
        #       0.40                 
        
        #   (Singles, HR)   --->    Teams with more singles don't hit more HRs
        #       -0.17 
        
        #   (BB, Singles)   --->    Teams with more BBs don't hit more singles 
        #      -0.056
        
        #     (HR, R)       --->    Teams with more HRs score more Rs
        #      0.68
        
        #NOTE: This suggests stronger association b/w BBs & Rs (0.735) versus Singles & Rs ( 0.449) could be from HRs si




#       Check if HRs causes BBs --------------------------------------------------------------------->####
#             Stratify ------------------------------------------------------------------------------>####
              #_______________________________________________________________________________________
              dat1 <- Teams %>% filter(yearID %in% 1961:2001) %>%
                mutate(HR_strata = round(HR/G, 1), 
                       BB_per_game = BB / G,
                       R_per_game = R / G) %>%
                filter(HR_strata >= 0.4 & HR_strata <=1.2)
              dat1


        
        
        
#             Individual strata regression lines ---------------------------------------------------->####
              #_______________________________________________________________________________________
              dat1 %>% 
                ggplot(aes(BB_per_game, R_per_game)) +  
                geom_point(alpha = 0.5) +
                geom_smooth(method = "lm") +
                facet_wrap( ~ HR_strata)


        
        
        
        
#             Individual strata slopes/beta1 calculations ------------------------------------------->####
              #_______________________________________________________________________________________
              BB_HR_strata_slopes <- dat1 %>%  
                group_by(HR_strata) %>%
                summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))
              
              BB_HR_strata_slopes
              
        
        
        
        


#                   Plotting slopes ----------------------------------------------------------------->####
                    #_______________________________________________________________________________________
                    qplot(BB_HR_strata_slopes$HR_strata, BB_HR_strata_slopes$slope)








#             Individual strata slopes/beta1 w/ lm() ------------------------------------------------>####
              #_______________________________________________________________________________________
              dat1 %>% group_by(HR_strata) %>% 
                do(tidy(lm(R_per_game ~ BB_per_game, data = .), conf.int = TRUE)) %>%    
                filter(term == "BB_per_game") %>%                                        # R vs BB slopes
                select(HR_strata, estimate, conf.low, conf.high)                         # HR strata
              
        
        
        
        
        
        
#                   Plotting slope CIs -------------------------------------------------------------->####
                    #_______________________________________________________________________________________
                    dat1 %>% group_by(HR_strata) %>% 
                      do(tidy(lm(R_per_game ~ BB_per_game, data = .), conf.int = TRUE))  %>%    
                      filter(term == "BB_per_game") %>%
                      select(HR_strata, estimate, conf.low, conf.high) %>%                      
                      ggplot(aes(HR_strata, y = estimate, ymin = conf.low, ymax = conf.high)) +
                      geom_errorbar() +
                      geom_point()
                    
                    
                  
                  
                  # 95% confidence there is CHANGE between slopes (hard to tell but there is a gap between and interval)
                  # HRs do influence BBs 
                  
                  
        
        
        
        
        



#       Check if BBs causes HRs --------------------------------------------------------------------->####
        #NOTE: same parameters but HR and BB are opposite



#             Stratify ------------------------------------------------------------------------------>####
              #_______________________________________________________________________________________
              dat2 <- Teams %>% filter(yearID %in% 1961:2001) %>%
                mutate(BB_strata = round(BB/G, 1), 
                       HR_per_game = HR / G,
                       R_per_game = R / G) %>%
                filter(BB_strata >= 2.8 & BB_strata <=3.9) 
              
        
      
      
#             Individual strata regression lines ---------------------------------------------------->####
              #_______________________________________________________________________________________
              dat2 %>% ggplot(aes(HR_per_game, R_per_game)) +  
                geom_point(alpha = 0.5) +
                geom_smooth(method = "lm") +
                facet_wrap( ~ BB_strata)
              
        
      
      
      
#             Individual strata slopes/beta1 calculations-------------------------------------------->####
              #_______________________________________________________________________________________
              HR_BB_strata_slopes <- dat2 %>%  
                group_by(BB_strata) %>%
                summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game)) 
              
              HR_BB_strata_slopes
              
        
        
        
        
        
        
#                   Plotting strata slope ----------------------------------------------------------->####
                    #_______________________________________________________________________________________
                    qplot(HR_BB_strata_slopes$BB_strata, HR_BB_strata_slopes$slope)


        
        
        
        
#             Individual strata slopes/beta1 w/ lm() ------------------------------------------------>####
              #_______________________________________________________________________________________
              dat2 %>% group_by(BB_strata) %>% 
                do(tidy(lm(R_per_game ~ HR_per_game, data = .), conf.int = TRUE)) %>%    
                filter(term == "HR_per_game") %>%                                        # R vs HR slopes
                select(BB_strata, estimate, conf.low, conf.high)                         # BB strata
              
              
        

#                   Plotting strata slopes & CIs ---------------------------------------------------->####
                    #_______________________________________________________________________________________
                    dat2 %>% group_by(BB_strata) %>% 
                      do(tidy(lm(R_per_game ~ HR_per_game, data = .), conf.int = TRUE))  %>%    
                      filter(term == "HR_per_game") %>%
                      select(BB_strata, estimate, conf.low, conf.high) %>%                      
                      ggplot(aes(BB_strata, y = estimate, ymin = conf.low, ymax = conf.high)) +
                      geom_errorbar() +
                      geom_point()

        
        
        
                    # 95% confidence there is NO CHANGE between slopes (notice intervals overlap)
                    # BBs do NOT influence HRs 
                    
        
        





# LSE 2 PARAMETERS (BIVARIATE)                                                                           (Y = son, X[1] = father)              ####
# NOTE: LSE is pair of linear random variables Beta0 and Beta1 w/ SE
#       Estimate Beta1 (given arbitrary/fixed Beta0) ------------------------------------------------>   RSS = {y[i] - (beta0 + beta1*(x[i])}^2####

        # Create RSS (Residual Sum of Squares) function                                                              
        #________________________________________________________________________________________
        rss <- function(beta0, beta1, data){
          resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
          return(sum(resid^2))
        }
        
        rss
        
        
        # Create a sequenced string of values 0 to 1 (beta1 coefficients are from 0-1) the length of galton_heights data 
        #________________________________________________________________________________________
        beta1 = seq(0, 1, len=nrow(galton_heights))
        
        beta1
        
        
        
        # Use beta1 string and fixed beta0 25
        #________________________________________________________________________________________
        rss_res <- data.frame(beta1 = beta1,
                              rss = sapply(beta1, rss, beta0 = 25)) # Make intercept (aka beta0) fixed at 25 NOTE: this is arbitrary
        rss_res
        
        








#             LSE from visualization ---------------------------------------------------------------->####

              # Plot line where x-axis = beta1 & y-axis = RSS
              #________________________________________________________________________________________
              #NOTE: LSE (min RSS) appears at 0.65 but fixed beta0 = 25 is arbitrary
              
              rss_res %>% ggplot(aes(beta1, rss)) + geom_line() + 
                geom_line(aes(beta1, rss))
              
              
              
              # answer = LSE appears around 0.65








#       LSE estimates beta0 & beta1 ----------------------------------------------------------------->####

        # Use lm() function the calculate beta0 and beta1 LSE 
        #________________________________________________________________________________________
        
        fit1 <- lm(son ~ father, data = galton_heights)   #<---R verision
        fit1
        
        # SAME AS 
        
        fit <- galton_heights %>% lm(son ~ father, data = .) #<---Tidy version (can be used w/ grouped tibbles)
        fit
        
        
        fit1$coef
        
        
        # answer =            LSE ESTIMATES
        #              (Intercept)    x[1] Father 
        #                 Beta0         Beta1 
        #               37.287605      0.461392


        
        
        
        
        
        
        # Compute same output w/ confidence intervals that can be used for GROUPED tibbles (allows to check analyze 3rd parameter or confounding)
        #________________________________________________________________________________________
        galton_heights %>% do(tidy(lm(son ~ father, data = .), conf.int = TRUE))

        
        
        
        
        
        
        
        
        
        
        

#       LSE se -------------------------------------------------------------------------------------->####

        # Pull summary of lm() calculation and check "Std. Error"
        #________________________________________________________________________________________
        summary(fit1)
        
        # answer =            STD. ERROR
        #                 Beta0         Beta1 
        #                4.98618       0.07211







#       LSE t-value --------------------------------------------------------------------------------->####
        #NOTE: (assumes small N and ϵ is normally distributed) 
        
        # Pull summary of lm() calculation 
        #________________________________________________________________________________________
        summary(fit1)
        
        # answer =              T-VALUE
        #                 Beta0         Beta1 
        #               7.478     0.461392








#       LSE p-value --------------------------------------------------------------------------------->####
        #NOTE: (assumes large N for CLT)
        
        # Pull summary of lm() calculation 
        #________________________________________________________________________________________
        summary(fit1)
        
        # answer =             Pr(>|t|)
        #                 Beta0         Beta1 
        #                3.37e-12     1.36e-09
        
        
        

#       Regression line w/ beta estimates ----------------------------------------------------------->####
        
        # Create non standardized regression line for father and son heights using LSE beta0 and Beta1 
        #________________________________________________________________________________________
        galton_heights %>% 
          ggplot(aes(father, son)) +  
          geom_point(alpha = 0.5) +
          geom_abline(slope = 0.461392, intercept = 37.287605) 
        
        
        
        
        
        
        
#       Monte carlo sample distributions of Beta0 & Beta1 estimates normality analysis -------------->####
        
        # Monte Carlo sample Beta0 and Beta1 
        #________________________________________________________________________________________
        B <- 1000
        N <- 50
        lse <- replicate(B, {
          sample_n(galton_heights, N, replace = TRUE) %>% 
            lm(son ~ father, data = .) %>% 
            .$coef 
        })
        
        
        
        # Convert Beta0 and Beta1 samples into data frame that pairs the two variables 
        #________________________________________________________________________________________
        lse1 <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
        lse1
        
        
        
        
        
        # Determine normality of distributions using histograms of Beta0 and Beta1
        #________________________________________________________________________________________
        p1 <- lse1 %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
        p2 <- lse1 %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
        grid.arrange(p1, p2, ncol = 2)












# LM PREDICTION E, SE, CI, MOE                                                                           Y = β[0]  +  β[1] * x[1]####
#       Single son's height estimate (using linear equation) ---------------------------------------->####

        #NOTE: Same as 'population conditional regression' estimate but with simplified linear model using LSE
        
        
        # Based on the galton heights data, estimate son's height when father is 72 inches      
        #________________________________________________________________________________________
        
        # Y = β[0]   +   β[1]  * x
           37.287605 + 0.461392*72
        
        # answer = 70.5 inches           





#       Single son's height estimate (using lm() & predict() function) ------------------------------>####
        
        #NOTE: predict() function only works on lm() class objects NOT tbl and df
        
        

        # Use predict() function to estimate son's height when father is 72 inches 
        #________________________________________________________________________________________
        
        fit <- galton_heights %>% lm(son ~ father, data = .) 
        
        predict(fit, data.frame(father = 72))
        
        
        # answer = 70.5 inches 
        
        
        
        
                  # OR 
        
        
        
        
        fit1 <- lm(son ~ father, data = galton_heights)
        
        predict(fit1, data.frame(father = 72))
        
        # answer = 70.5 inches 
        
        
        
        





#       Single son's height SE ---------------------------------------------------------------------->####

        # Use predict() function to estimate mean of sons heights and CI when father is 72 inches tall
        #________________________________________________________________________________________
        
        lm1 <- predict(fit, data.frame(father = 72), se.fit = TRUE, interval = "confidence", level = 0.95)
        lm1$se.fit
        
        
        # answer = 0.2780284










#       Single son's height CI ---------------------------------------------------------------------->####
        #NOTE: "confidence" interval is MOE around a population MEAN of already observed son heights at father's height 72
        
        # Use predict() function to estimate mean of sons heights and CI when father is 72 inches tall
        #________________________________________________________________________________________
        
        
        lm1 <- predict(fit, data.frame(father = 72), se.fit = TRUE, interval = "confidence", level = 0.95)
        lm1$fit
        
        
        #              fit         lwr        upr         
        # answer =  70.50783    69.95915    71.0565            
        
        
        # This is the estimated height value and range we can say with 95% condidence when 
        # picking A SAMPLE FROM THE POPULATION of son's height who's fathers are 72 inches tall









#       Single son's height CI vs prediction interval ----------------------------------------------->####
        #NOTE: "prediction" interval is MOE around a single value of son's height at father's height
        
        
        # Use predict() fucntion to predict a future height of a son who has a father that is 72 inches tall 
        #________________________________________________________________________________________
        predict(fit, data.frame(father = 72), se.fit = TRUE, interval = "prediction", level = 0.95) 
        
        
        
        
        #              fit          lwr        upr
        # answer =  70.50783     65.64181    75.37384
          
        
        
        # This is the estimated height value and range we can say with 95% condidence when 
        # predicting A FUTURE UNKNOWN SAMPLE son's height who's father is 72 inches tall
        
        
        
        
        
        



#       All son height estimates, SEs, & CIs -------------------------------------------------------->####

        # Use predict() function to show all fits/estimates, CI values, and SEs
        #________________________________________________________________________________________
        
        predict(fit, se.fit = TRUE, interval = c("confidence"), level = 0.95)
        






#       Smooth density plot all son estimates and CIs ----------------------------------------------->####

        # Simply use geom_smooth(method = "lm") to create smooth density plot of son height predictions and confidence intervals 
        #________________________________________________________________________________________
        galton_heights %>% ggplot(aes(son, father)) +
          geom_point() +
          geom_smooth(method = "lm")
        




# LSE 3 PARAMETERS (MULTIVARIATE)                                                                        (Y = R, x[1] = HR, x[2] = BB)####                                                                  

#NOTE:                Y          = β[0]   +   β[1]*x[1]   +   β[2]*x[2]
#          E[R|BB=x[1], HR=x[2]] = β[0]   +   β[1]*x[1    +   β[2]*x[2]      
#              standardized Y    = 0  + β[1]*(x[1]-μx[1]) + β[2]*(x[2]-μx[2])   


# MUST BE JOINTLY NORMAL: meaning if we pick any one variable, and hold the other four fixed, the relationship with
#                         the outcome is linear and the slope does not depend on the four values held constant.



#       LSE estimates beta0, beta1, beta2 ----------------------------------------------------------->####
        
        # Use lm() function to calculate LSE for Rs, HRs, and BBs 
        #________________________________________________________________________________________
        lm_3 <- Teams %>% filter(yearID %in% 1961:2001) %>%
          mutate(BB = BB / G, HR = HR / G, R = R / G) %>%
          lm(R ~ HR + BB, data = .)
        
        lm_3 
        
        # answer =                    ESTIMATE
        #              (Intercept)       HR           BB
        #                 Beta0         Beta1        Beta2
        #               1.7443011     1.5611689    0.3874238

        
        
        
        
      
        

#       Stratify LSE estimates by added variable ---------------------------------------------------->####
        
        # Stratify estimates by individual years
        #________________________________________________________________________________________
        lm_by_year <- Teams %>% 
          filter(yearID %in% 1961:2018) %>%
          group_by(yearID) %>% 
          do(tidy(lm(R ~ HR + BB, data = .))) %>%
          ungroup()
        
        lm_by_year
        
     
        
        
        
        
        
        
#       New lm() between single LSE beta and new variable ------------------------------------------->####
        
        
        # Filter a single beta (BB) estimate and run lm() the determine effect of new variable (yearID) on the beta estimate
        #________________________________________________________________________________________
        lm_by_year %>%
          filter(term == "BB") %>% 
          lm(estimate ~ yearID, data = .) %>% 
          tidy()
        
        
        # Plot beta2 (BB) over years 
        #________________________________________________________________________________________
        lm_by_year%>%
          filter(term == "BB") %>%
          ggplot(aes(yearID, estimate)) +
          geom_point() +
          geom_smooth(method = "lm")
        
        
        
        
        
        
        

# IDENTIFY CONFOUNDING THROUGH LM                                                                     ####
#       Multivariate(3) LSE vs bivariate CCs -------------------------------------------------------->####
        lm_3 <- Teams %>% filter(yearID %in% 1961:2001) %>%
          mutate(BB = BB / G, HR = HR / G, R = R / G) %>%
          lm(R ~ HR + BB, data = .)
        
        lm_3 
        
        # Compute LSE betas 
        #________________________________________________________________________________________
       
        # answer =                    LSE ESTIMATES
        #              (Intercept)       x[1] HR        x[2] BB
        #                 Beta0           Beta1          Beta2
        #               1.7443011       1.5611689      0.3874238
        
                                                                        #NOTE: Noticable change in BB
                                                                        # indicates possible confounding    
        #                          INITIAL CC ASSOCIATIONS
        #                               HR v R Cor    BB v R Cor  
        #                               1.844824       0.7353288
        
        
        
        
        
        
        
        
#       lm() prediction error ----------------------------------------------------------------------->####
        #________________________________________________________________________________________
        Teams %>% 
          filter(yearID %in% 2002) %>%                       # <--- new data (2002 stats)
          mutate(BB = BB/G, 
                 HR=HR/G,
                 R_Actual2002=R/G) %>%   
          mutate(R_Prediction2002 = predict(lm_3, newdata = .)) %>% # <--- lm_3 model on new data
          ggplot(aes(R_Prediction2002, R_Actual2002, label = teamID)) +    # Assessing prediction of Team R production using multivariate lm_3 model
          geom_point() +
          geom_text(nudge_x=0.1, cex = 2) + 
          geom_abline()

        
        

        
        
# EXAMPLE QUESTIONS 2 PARAMETERS                                                                      ####
#       Filter batting data ------------------------------------------------------------------------->####


# Wrangle batting data in to 2 groups 
# Group 1: contains player avg singles rate and avg bb rates using rates by year from 1999-2001
# Group 2: contains player singles rate and bb rate for the year 2002
#________________________________________________________________________________________

library(Lahman)
bat_00 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, yearID, singles, bb) %>%
  group_by(playerID, yearID) %>% ungroup() %>% group_by(playerID) %>% summarise(mean_singles = mean(singles), mean_bb = mean(bb)) 
bat_00


bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
bat_02


# inner_join() the two groups to compare 2002 rates to 1999:2001 rates
#________________________________________________________________________________________
bat_3 <- inner_join(bat_00, bat_02)

bat_3







#       CC, regression plot, bivariate normal analysis, & LM coefficients --------------------------->####
#             Singles rate: 2002 vs 1999:2001-------------------------------------------------------->####

              # Calculate correlation between 2002 singles rate and 1999:2001 avg singles rate
              #________________________________________________________________________________________
              bat_3 %>% summarise(r = cor(singles, mean_singles)) %>% pull(r)


              # Plot regresson line and subjectively determine if singles variables are bivariate normal (oval-shaped)
              #________________________________________________________________________________________
              bat_3 %>% ggplot(aes(mean_singles, singles)) +
                geom_point(alpha = 0.5) +
                geom_abline(aes(slope = get_slope(mean_singles, singles), intercept = get_intercept(mean_singles, singles)))
              
              # Create a histogram and qq-plot for both singles variables to determine a more accurate prediciton of bivariate normal
              #________________________________________________________________________________________
              hist(bat_3$singles)
              
              bat_3 %>% ggplot() +
                stat_qq(aes(sample = singles))
              
              hist(bat_3$mean_singles)
              
              bat_3 %>% ggplot() +
                stat_qq(aes(sample = mean_singles))
              
              
              # Use LM to calculate LSE coefficients 
              #________________________________________________________________________________________
              bat_3 %>% lm(singles ~ mean_singles, data = .)
              
              
              







#             BB rate:      2002 vs 1999:2001-------------------------------------------------------->####

              # Calculate correlation between 2002 bb rate and 1999:2001 avg bb rate
              #________________________________________________________________________________________
              bat_3 %>% summarise(r = cor(bb, mean_bb)) %>% pull(r) 
              
              
              # Plot regresson line and subjectively determine if bb variables are bivariate normal (oval-shaped)
              #________________________________________________________________________________________
              bat_3 %>% ggplot(aes(mean_bb, bb)) +
                geom_point(alpha = 0.5) +
                geom_abline(aes(slope = get_slope(mean_bb, bb), intercept = get_intercept(mean_bb, bb)))
              
              
              # Create a histogram and qq-plot for both bb variables to determine a more accurate prediciton of bivariate normal
              #________________________________________________________________________________________
              hist(bat_3$bb)
              
              bat_3 %>% ggplot() +
                stat_qq(aes(sample = bb))
              
              hist(bat_3$mean_bb)
              
              bat_3 %>% ggplot() +
                stat_qq(aes(sample = mean_bb))
              
              
              # Use LM to calculate LSE coefficients 
              #________________________________________________________________________________________
              bat_3 %>% lm(bb ~ mean_bb, data = .)


              
              
              
              
              
##### KEY NOTE: LSE(correlations/p-values) are RANDOM VARIABLES and can over/underestimate relationships #####              
#****************************************Advanced Tidyverse*******************************************####
# LSE COEFFCICIENTS FOR MULTIPLE STRATA                                                               ####
#       Practice data ------------------------------------------------------------------------------->####

        tibble_dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
          mutate(HR = round(HR/G, 1), 
                 BB = BB/G,
                 R = R/G) %>%
          select(HR, BB, R) %>%
          filter(HR >= 0.4 & HR<=1.2) 






#       Tibble -------------------------------------------------------------------------------------->####

        #NOTES:
        #1) Tibbles can be grouped 
        #2) Tidy does not recognize R functions hence why R functions don't work after grouping tibbles
        #3) lm() is an R function, therefore does NOT recgonize grouped tibbles 
        #4) Tibble subsets will return a dataframe rather than an integer
        #5) Tibble entries within a column can include functions and lists
       




#             do() ---------------------------------------------------------------------------------->   Connecting df -> tidy####
              
              #NOTES:
              #1) do() bridges R functions and tidy
              #2) do() recognizes grouped tibbles and converts to dfs 
              #3) Function outputs within do() must return dfs for do() to work




#                   lm() output W/O tidy() ---------------------------------------------------------->####

                    # do() without assigning lm() output to a name/oject
                    #________________________________________________________________________________________
                    tibble_dat %>%  
                      group_by(HR) %>%
                      do(lm(R ~ BB, data = .)) # notice lm() is not assigned to a column name and returns an error
                
                    





                    # LIST as output
                    # Create a df within the do() function by naming lm() output 'to'fit' 
                    #________________________________________________________________________________________
                    tibble_dat %>%  
                      group_by(HR) %>%
                      do(fit = lm(R ~ BB, data = .)) # lm() results assigned to 'fit' makes it a df but returns a list in eachrow/entry 

                    
                    
                    
                    
                    
                    
                    
                    # SINGLE ROW as df output
                    # Create a function that returns a df (with column names) and plugging into do() allows do() to apply functions to grouped tibbles
                    #________________________________________________________________________________________
                    get_slope1 <- function(data){
                      fit <- lm(R ~ BB, data = data)
                      data.frame(slope = fit$coefficients[2],           # df returns slope parameter but not intercept
                                    se = summary(fit)$coefficient[2,2]) # se for just the slope parameter
                      }
                
                    tibble_dat %>%  
                      group_by(HR) %>%
                      do(get_slope1(.)) # names are already in df, no need for name inside do()
    
              
              
                    
                    
                    
                    
                    
                    # MULTIPLE ROWS as df output
                    # Create similar function as the single row code but make output return 2 rows w/ 1st row: intercept; 2nd row: slope and their SEs
                    #________________________________________________________________________________________
                    get_lse1 <- function(data){
                      fit <- lm(R ~ BB, data = data)
                      data.frame(term = names(fit$coefficients),    #<----df to return both beta parameters
                                 slope = fit$coefficients, 
                                 se = summary(fit)$coefficient[,2]) #<----remember each LSE parameter has an se
                    }
                    
                    tibble_dat %>%  
                      group_by(HR) %>%
                      do(get_lse1(.))    
                        
    
    
    
    
    
#       Broom package ------------------------------------------------------------------------------->####
#             tidy() -------------------------------------------------------------------------------->   Summaries/lists -> df#### 

              #NOTES: 
              # tidy() returns estimates and other related summaries as a df
                    
#                   lm() output W/ tidy() ----------------------------------------------------------->####                    
          
                    
                    # Use tidy() to convert lm() into proper df summary output for both LSE beta parameters (slope and intercept)   
                    #________________________________________________________________________________________
                    
                          
                    # WITHOUT do() base R code  <--- correct summary can't use tidy groups
                    tidy(lm(R ~ BB, data = tibble_dat), conf.int = TRUE) # will not apply to groups when piped
              
              
                    # WITHOUT do() tidyverse code <--- incorrect summary but can use groups 
                    tibble_dat %>% tidy(lm(R ~ BB, data = .), conf.int = TRUE) # pipe requires do() to convert organized (through tidy) df into tibble
                    
                    
                    
                    # WITH do() tiderverse code   <--- correctly reurns summary that can apply to grouped strata
                    tibble_dat %>% do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) # same output as r version but WILL APPLY TO GROUPED TIBBLES 
              
                    
                    
                    
    
              
              
              
    