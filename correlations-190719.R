library(foreign) # read SPSS
library(ggplot2) # for making graphs
library(reshape) # melt() 
library(gdata) # upperTriangle() & lowerTriangle()
library(Hmisc) # rcorr() get matrix of correlations and p-values
library(gdata) # lowerTriangle() remove half of matrix

rm(list = ls())
setwd("~/Dropbox/Research/Automation/SPSS/")
o <- read.spss('ATP combine submitted.sav', use.value.labels = FALSE, 
               to.data.frame = TRUE)
colnames(o)

# select cases in all three waves
i = subset(o,f_w27_24.5_17 == 1)

# select variables
corvars = c("ideo","party","p_ind","p_hie","sk","c_see","c_risk","c_reg")
cornames = c("Political conservatism","Party affiliation",
             'Economic conservatism','Social conservatism',
             'Scientific literacy','Familiarity with driverless vehicles',
             "Concern about driverless vehicles","Support for restrictive regulations")

# check number of variables
length(corvars); length(cornames)

# keep variables for correlation
icor <- i[corvars]
summary(icor)
length(icor)

# get correlation results and significance levels
corresults = rcorr(as.matrix(icor)) # pairwise deletion
ns = corresults$n; ns;
cors = corresults$r; cors
sigs = corresults$P; sigs

# assign variable names
colnames(cors) <- cornames; rownames(cors) <- cornames; cors

# remove lower part of the matrix
lowerTriangle(cors, diag=TRUE) <- NA; cors # also removes diag

# melt the matrix
corm = melt(cors, na.rm=TRUE); corm
sigm = melt(sigs, na.rm=TRUE); sigm
nm = melt(ns, na.rm=TRUE); nm
  
# combine into a data frame
dcor <- cbind(corm, sigm[,3], nm[,3]); dcor #select only the third column
row_1 = dcor[1,]; row_1 # select the first row. this is to show the first variable in correlation graph
dcor = dcor[complete.cases(dcor), ]; dcor # remove all NA cases
dcor = rbind(row_1, dcor) # add the first row back
colnames(dcor) <- c("X1","X2","coefs","sigs","ns")
dcor[1:3,]; dcor #check the data frame

# transform p-values to stars
sigtostar_cor <- function(freq){
  stars = ifelse(freq < .001, "***",
                 ifelse(freq < .01,"**",
                        ifelse(freq < .05, "*",
                               ifelse(freq < .1, "†", "-"))));
  return(stars)}

dcor$stars = sigtostar_cor(dcor$sigs); dcor$sigs; dcor$stars
dcor[1:3,]

# create text labels
dcor$text = format(round(dcor$coefs, 2)); dcor$text # round to two digits
dcor$text = gsub("0\\.", "\\.", dcor$text) # remove 0
dcor$text

# combine correlation with stars in superscript
dcor$textstar = ifelse(dcor$stars == "-", paste("'",   dcor$text, "'", sep=''), 
                       paste("'",   dcor$text, "'^'", dcor$stars, "'", sep='')) 
dcor$textstar

# setting factor levels for ggplot
dcor$X1 <- factor(dcor$X1, levels=unique(dcor$X1)) 
dcor$X2 <- factor(dcor$X2, levels=rev(unique(dcor$X2))) 

# create legend variable labels
cornames
labels_number = 1:(length(cornames)-1); labels_number

labels_cornames = c(); i=1
for (corname in cornames){
  labels_cornames = c(labels_cornames, paste(i,'. ',corname,sep='')); 
  i=i+1}
labels_cornames

# get correlation graph
p=ggplot(dcor, aes(x=X1, y=X2, fill=coefs, label =textstar)) + 
  geom_tile() + coord_fixed() + #make sure 1 unit corresponds to 1 unit
  geom_text(hjust = 0, nudge_x = -0.5, parse = TRUE) + #parsed into expressions
  scale_x_discrete(labels = labels_number) +
  scale_y_discrete(labels = rev(labels_cornames)) + 
  scale_fill_gradient2(low="dark blue", high="dark red", mid = "white", midpoint=0, breaks=seq(-1,1,by=0.2), limit=c(-1.0,1.0), space = "Lab",name="", na.value=NA) +
  theme_classic(base_size = 16) +  
  theme(legend.key.width=unit(0.2,"inches"), 
        legend.key.height=unit(0.6,"inches"),text=element_text(size=20,family="Arial Narrow"),
        legend.direction="vertical", legend.position='right',
        axis.line = element_line(size = 0.3),axis.ticks = element_line(size = 0.3),
        axis.title = element_blank(),
        axis.text = element_text(colour="black"))
p

setwd("~/Dropbox/Research/Automation/R-Graph/Correlation") # replace with image saving path
ggsave("pcorr-7.5x6.png",width=7.5,height=6,dpi=300,bg="transparent")

library(export) # save to ppt
graph2ppt(file="pcorr-ppt-7.5x6.pptx", width=7.5, height=6) 
