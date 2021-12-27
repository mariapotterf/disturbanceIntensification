
rm(list = ls())


#libs
library(boot)
library(mgcv)
library(gamm4)
library(itsadug)
library(ncf)
library(tidymv)
library(ggplot2)
library(gridExtra)
library(ggpubr)


# Define path to store models outputs in .RData file
path <- ''

# Define path to store output fgures
outPath = ''

#GAMMs
COORD <-read.table("coord.txt", header=T, sep="\t", encoding="UTF-8")

# Load model output as Rdata file
load(paste(path, "vysledok_M1z_pred_shannon.RData", sep = "/"))
load(paste(path, "vysledok_M13z_pred_shannon.RData", sep = "/"))


# Investigate model outputs
# ----------------------------------

AIC(M1z.reordered, M1.3z.reordered)

compareML(M1z.reordered, M1.3z.reordered)

1-pchisq(32.327*2,6)#p-value

report_stats(M1z.reordered)

summary(M1z.reordered)

anova(M1z.reordered)

gam.check(M1z.reordered)
windows()
acf_resid(M1z.reordered, split_pred=list(DAT2$INT), n=30)

par(mfrow=c(4,5))
for (i in 1:18){
RES2.PA.YEAR <- RES2[RES2$DISTANCE=="PA" & RES2$YEAR==c(1999+i) ,]
KORELOGRAM.PA.YEAR <- spline.correlog(x=RES2.PA.YEAR $E, y=RES2.PA.YEAR $N, z=RES2.PA.YEAR[,2], resamp=1000, latlon=TRUE)
plot(KORELOGRAM.PA.YEAR)
}









#------------------------------
#            Create plots 
# -----------------------------

#plot time

my_y_lab = expression('Cumulative disturbance rate (%)')  # add superscipt and closing parantheses
# define Mareks' colors:
cols = c('#0072B2', # strict reserves
         '#E69F00', # buffer 500
         '#F0E442', # buffer 2000
         '#000000' # control
)


# Get plot details:
zone_names = c("Strict Reserve", "Buffer 500", "Buffer 2000", "Control")

# make a function to store details about plotting
line_plot_details <- function() {
  list(
    geom_ribbon(aes(ymin=ll,  
                    ymax=ul), 
                alpha=0.1, 
                linetype=0),
      geom_line(),
      scale_colour_manual(values=cols, 
                          name=NULL,
                          labels = zone_names),
      scale_fill_manual(values=cols, 
                        name=NULL,
                        labels = zone_names),
    coord_cartesian(ylim=c(0, 40)), # because  #  ylim(0,2), excluded the ribbons
    theme_test()
  )
  
}




# Get data from the model for all predictors:  ------------------------
P.DAT.YEAR <- plot_smooth(M1z.reordered, 
                          view="YEAR", 
                          rm.ranef =TRUE, 
                          transform=function(x) 100*inv.logit(x),  
                          plot_all="DISTANCE")$fv



# Main plot: 
P.YEAR <- ggplot(P.DAT.YEAR, 
                 aes(x=YEAR, 
                     y=fit,  
                     colour=DISTANCE,
                     fill=DISTANCE)) +
  scale_x_continuous(name=NULL, 
                     breaks = seq(2000, 
                                  2018,
                                  2)) +
  ylab(my_y_lab) +
  line_plot_details()



windows(width = 6, height = 3.5)
P.YEAR %>% 
  ggsave(
    filename = paste(outPath, 'Fig4.tif', sep = "/"),
    plot = last_plot(),
    device = 'tiff',
    path = NULL,
    scale = 1,
    width = 6,
    height = 3.5,
    units = c("in" ),
    dpi = 500,
    limitsize = TRUE,
    bg = NULL
  )


# -------------------------
# Predictors plots        
# -------------------------


#plot spruce
P.DAT.SPRUCE <- plot_smooth(M1z.reordered, 
                            view="SMREK", 
                            rm.ranef =TRUE, 
                            transform=function(x) 100*inv.logit(x),
                            plot_all="DISTANCE")$fv

P.SPRUCE2 <- ggplot(P.DAT.SPRUCE, aes(x=SMREK, 
                                      y=fit, 
                                      colour=DISTANCE,
                                      fill=DISTANCE))+
  scale_x_continuous(name="Spruce proportion (%)")+
  line_plot_details() +
  ylab("")


#plot SHANNON
P.DAT.SHANNON <- plot_smooth(M1z.reordered, 
                             view="SHANNON", 
                             rm.ranef =TRUE, 
                             transform=function(x) 100*inv.logit(x), 
                             plot_all="DISTANCE")$fv

P.SHANNON2 <- ggplot(P.DAT.SHANNON, aes(x=SHANNON, 
                                        y=fit, 
                                        colour=DISTANCE,
                                        fill=DISTANCE))+
  scale_x_continuous(name="Shannon (Tree diversity)", limits=c(0,2))+
  line_plot_details() +
  ylab("")



#altitude
P.DAT.ALT <- plot_smooth(M1z.reordered, 
                         view="ALT", 
                         rm.ranef =TRUE, 
                         transform=function(x) 100*inv.logit(x), #
 plot_all="DISTANCE")$fv

P.ALT2 <- ggplot(P.DAT.ALT, aes(x=ALT, 
                                y=fit, 
                                colour=DISTANCE,
                                fill=DISTANCE))+
  scale_x_continuous(name="Elevation (m)", 
                     breaks=seq(400,1600,300), 
                     limits=c(400,1600)) +
  line_plot_details() +
  ylab("")


#Age
P.DAT.AGE <- plot_smooth(M1z.reordered, 
                         view="AGE", 
                         rm.ranef =TRUE, 
                         transform=function(x) 100*inv.logit(x),    
                         plot_all="DISTANCE")$fv

P.AGE2 <- ggplot(P.DAT.AGE, aes(x=AGE, 
                                y=fit, 
                                colour=DISTANCE,
                                fill=DISTANCE))+
  scale_x_continuous(name="Age (years)", 
                     limits=c(50,150))+
  line_plot_details() +
  ylab("")


# Put plot together in one page
p.predictors<-ggarrange(P.SPRUCE2,P.AGE2, 
                        P.ALT2, P.SHANNON2,
                        nrow=2, 
                        ncol = 2,
                        common.legend = T, 
                        legend = "bottom")


# annonate figure to add label for y axis
windows(6.5,5)  
# https://rpkgs.datanovia.com/ggpubr/reference/annotate_figure.html
annotate_figure(p.predictors,
                left = text_grob(my_y_lab, 
                                   color = "black",
                                   hjust = 0.5, 
                                   #x = -1, 
                                   y = 0.5,
                                   face = "plain",
                                 size = 12,
                                 rot = 90)
)  %>% 
  ggsave(
  filename = paste(outPath, 'Fig5.tif', sep = "/"),
  plot = last_plot(),
  device = 'tiff',
  path = NULL,
  scale = 1,
  width = 6,
  height = 5.5,
  units = c("in" ),
  dpi = 500,
  limitsize = TRUE,
  bg = NULL
)







# !!!! Annual rates, likely not correct!!!



# --------------------------------------------
# calculate annual rates from predicted values
# --------------------------------------------

# first get amount of years: 2000 = 1, 2001 = 2, ... 
# need to link this to fitted values, as the years there are not factored correctly

# get predicted data from teh model
# need to be transformed??? yes!
# divide them by the number of years to get annual mortality rates



# get trhe transformed values:
df_predict <- predict.gam(M1z.reordered)

# convert fitted values based on marek's plots
df_convert <- inv.logit(df_predict)*100


# Acces predicted values: all methods have the same outputs
range(M1z.reordered$fitted.values)
range(M1z.reordered$fitted)
range(fitted(M1z.reordered))
range(predict.gam(M1z.reordered, type="response"))
range(df_convert)   # use this one as it corresponds in values to Marek's plots! 


# Calculate yearly value by number of years from predicted values
M1z.reordered$model$annual<- df_convert/(M1z.reordered$model$YEAR-2000+1)
M1z.reordered$model$annual_raw <- M1z.reordered$model$STALES*100/(M1z.reordered$model$YEAR-2000+1)


# get a dataframe of of teh predicted  values
dd <- M1z.reordered$model

# add fitted values for comparison
dd$fitted<-df_convert

# make just distribution of yearly values:
range(dd$fitted)
round(range(dd$annual),1)
range(dd$annual_raw)







# -------------------------------------------------
# Get density plot for zones from predicted values:
# -------------------------------------------------

# Density function for annual rates (means over 18 years)---------------------------------------------

# Cahnge facet_labels
facet_names <- c(
  "PA"       = "Strict Reserve",
  "500"      = "Buffer 500",
  "2000"     = "Buffer 2000",
  "control"  = "Control"
) 


windows(height = 2.5, width = 7)
p.dens <- dd %>% 
  group_by(DISTANCE) %>% 
  mutate(med = median(annual ),
         mean = mean(annual )) %>% 
  #ungroup() %>% 
  ggplot(aes(annual, 
             fill = DISTANCE)) +
  geom_density(alpha=0.6) + #, 
  geom_rug(aes(x = annual ,
               y = 0,
               color = DISTANCE),
           position = position_jitter(height = 0)) + # add masks for single observations on x axis
  geom_vline(aes(xintercept = med),
             col = "red",
             lwd = 0.8,
             lty = "dashed") +
  geom_vline(aes(xintercept = mean),
             col = "black",
             lwd = 0.8,
             lty = "dotted") +
  facet_grid(.~DISTANCE, labeller = as_labeller(facet_names)) +
  scale_color_manual(values=cols, 
                     name=NULL,
                     labels = zone_names) +
  scale_fill_manual(values=cols, 
                    name=NULL,
                    labels = zone_names) +
  scale_x_continuous(limits = c(0, 5)) +
  #coord_cartesian(xlim=c(0,5)) +
  theme_test() +
  ylab("Density") +
  xlab(expression(paste("Disturbance rate % ", (yr^-1)))) +
  theme(legend.position = "none")  


# export plot
p.dens %>% 
  ggsave(
    filename = paste(outPath, 'Fig3.tif', sep = "/"),
    plot = last_plot(),
    device = 'tiff',
    path = NULL,
    scale = 1,
    width = 7,
    height = 2.5,
    units = c("in" ),
    dpi = 500,
    limitsize = TRUE,
    bg = NULL
  )


