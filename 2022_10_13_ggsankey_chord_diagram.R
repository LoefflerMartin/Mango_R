# chord diagram (patient flow). Example with patient flow of back pain patients in simulated study design.
# ML 2022-10-13

rm(list = ls())

# HC --> HC : 75%
# HC --> ABP : 25%
# HC --> SABP : 0%
# HC --> CBP : 0%

# ABP --> HC : 75%
# ABP --> ABP : 0%
# ABP --> SABP : 25%
# ABP --> CBP : 0%

# SABP --> HC : 50%
# SABP --> ABP : 0%
# SABP --> SABP : 0%
# SABP --> CBP : 50%

# CBP --> HC : 25%
# CBP --> ABP : 0%
# CBP --> SABP : 0%
# CBP --> CBP : 75%


remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)
library(ggplot2)
library(dplyr)
library(scales)

t1 = c(rep(1,100), rep(2, 100), rep(3,100), rep(4,100))
#t1 = t1[order(t1)]



rateHC2ABP = 25
rateABP2SABP = 25
rateSABP2CBP = 50
rateCBP2CBP = 75

#rateHC2ABP = 50
#rateABP2SABP = 50
#rateSABP2CBP = 50
#rateCBP2CBP = 50

nHC = length(t1[t1==1])
nABP = length(t1[t1==2])
nSABP = length(t1[t1==3])
nCBP = length(t1[t1==4])
t2 = NA
t2[t1==1][1:round(((rateHC2ABP/100) * nHC), digits = 0)] = 2 #25% of HC develop ABP after 4 weeks
t2[t1==1][(round(((rateHC2ABP/100) * nHC), digits = 0) +1) : nHC ] = 1
t2[t1==2][1:round(((rateABP2SABP/100) * nABP), digits = 0)] = 3
t2[t1==2][(round(((rateABP2SABP/100) * nABP), digits = 0) +1) : nABP ] = 1
t2[t1==3][1:round(((rateSABP2CBP/100) * nSABP), digits = 0)] = 4
t2[t1==3][(round(((rateSABP2CBP/100) * nSABP), digits = 0) +1) : nSABP ] = 1
t2[t1==4][1:round(((rateCBP2CBP/100) * nCBP), digits = 0)] = 4
t2[t1==4][(round(((rateCBP2CBP/100) * nCBP), digits = 0) +1) : nCBP ] = 1

nHC = length(t2[t2==1])
nABP = length(t2[t2==2])
nSABP = length(t2[t2==3])
nCBP = length(t2[t2==4])
t3 = NA
t3[t2==1][1:round(((rateHC2ABP/100) * nHC), digits = 0)] = 2 #25% of HC develop ABP after 4 weeks
t3[t2==1][(round(((rateHC2ABP/100) * nHC), digits = 0) +1) : nHC ] = 1
t3[t2==2][1:round(((rateABP2SABP/100) * nABP), digits = 0)] = 3
t3[t2==2][(round(((rateABP2SABP/100) * nABP), digits = 0) +1) : nABP ] = 1
t3[t2==3][1:round(((rateSABP2CBP/100) * nSABP), digits = 0)] = 4
t3[t2==3][(round(((rateSABP2CBP/100) * nSABP), digits = 0) +1) : nSABP ] = 1
t3[t2==4][1:round(((rateCBP2CBP/100) * nCBP), digits = 0)] = 4
t3[t2==4][(round(((rateCBP2CBP/100) * nCBP), digits = 0) +1) : nCBP ] = 1


nHC = length(t3[t3==1])
nABP = length(t3[t3==2])
nSABP = length(t3[t3==3])
nCBP = length(t3[t3==4])
t4 = NA
t4[t3==1][1:round(((rateHC2ABP/100) * nHC), digits = 0)] = 2 #25% of HC develop ABP after 4 weeks
t4[t3==1][(round(((rateHC2ABP/100) * nHC), digits = 0) +1) : nHC ] = 1
t4[t3==2][1:round(((rateABP2SABP/100) * nABP), digits = 0)] = 3
t4[t3==2][(round(((rateABP2SABP/100) * nABP), digits = 0) +1) : nABP ] = 1
t4[t3==3][1:round(((rateSABP2CBP/100) * nSABP), digits = 0)] = 4
t4[t3==3][(round(((rateSABP2CBP/100) * nSABP), digits = 0) +1) : nSABP ] = 1
t4[t3==4][1:round(((rateCBP2CBP/100) * nCBP), digits = 0)] = 4
t4[t3==4][(round(((rateCBP2CBP/100) * nCBP), digits = 0) +1) : nCBP ] = 1



#t2 = rep(1:4, 100)
#t3 = rep(1:4, 100)
#t4 = t1[order(t1, decreasing = T)]

df2 = data.frame(t1, t2, t3, t4)
df2[df2==1] = "1_HC"
df2[df2=="2"] = "2_ABP"
df2[df2=="3"] = "3_SABP"
df2[df2=="4"] = "4_CBP"

df2_long <- df2 %>% 
  make_long(t1,t2,t3,t4)



ggplot(df2_long, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node),
               label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1, width = .1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_manual(values=c("dark green", "yellow", "orange", "red")) +
  theme_sankey(base_size = 16) +
  theme(legend.position = "none",
        axis.title.x = element_blank() )#,
#        axis.text.x = element_blank())


