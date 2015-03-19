# analysis for coumagen simulations
library(ggplot2)
library(RColorBrewer)

rm(list=ls())

# loads the simulation results:
# cstdpgx.out = coumagen simulation using original protocols
# cw2d.out = coumagen data days 1-2 then switches to wilson after day 2
# cw7d.out = coumagen data days 1-7 then wilson after day 7 (not using this simulation)
load("/Users/vincentfusaro/Dev/ClinicalAvatars/LPM-INR-Modeling/Datasets/Coumagen_Files/Coumagen_redo_08-22-11/CombineSimulations_3_sims_2012-02-03.RData")

simResults = c("cstdpgx.out", "cw2d.out")

# all files are called "trial.out"
rawData = c("/Users/vincentfusaro/Dev/ClinicalAvatars/LPM-INR-Modeling/Datasets/Coumagen_Files/Coumagen_redo_08-22-11/Combined_STD_PGX.RData",
        "/Users/vincentfusaro/Dev/ClinicalAvatars/LPM-INR-Modeling/Datasets/Coumagen_Files/Coumagen_redo_08-22-11/Combined_WilsonCSTD_WilsonCPGx_After_2_days_fixed_protocol.RData")

endpoints = {}
for (s in 1:length(simResults)) {
    endpoints = rbind(endpoints, apply(get(simResults[s])$ttrOut[,1:4],2,mean))
    endpoints = rbind(endpoints, apply(get(simResults[s])$ttrOutMV[,1:4],2,mean))
    endpoints = rbind(endpoints, apply(get(simResults[s])$ttrOutWT[,1:4],2,mean))
    endpoints = rbind(endpoints, apply(get(simResults[s])$ttrOutWTMV[,1:4],2,mean))
    endpoints = rbind(endpoints, apply(get(simResults[s])$ttrOutSV[,1:4],2,mean))
}
# add actual coumagen results
endpoints = rbind(endpoints, c(30.7, 22.9, 33.1, 22.9))
endpoints = rbind(endpoints, c(31, 21.4, 40.4, 25.4))
endpoints = rbind(endpoints, c(28.1, 24.8, 36.9, 25.3))
endpoints = rbind(endpoints, c(29.3, 23.4, 39.1, 25.2))
endpoints = rbind(endpoints, c(33.6, 22.1, 27, 17.8))

endpoints = data.frame(endpoints)
names(endpoints ) = c("meanPG", "sdPG", "meanSTD", "sdSTD")
endpoints = cbind(endpoints, protocol=c(rep("Coumagen Simulation",5), rep("Wilson Simulation", 5), rep("Coumagen", 5)),
                    endpoint = gl(5,1, labels=c("All Patients","Multiple Variants","Wild Type", "Wild Type & Multiple Variant", "Single Variant")))

# generates "tie-fighter" plots for all endpoints
endpoints2 = data.frame(mean=c(100-endpoints[,1], 100-endpoints[,3]), sd=c(endpoints[,2], endpoints[,4]), protocol=c(as.character(endpoints[,5]), as.character(endpoints[,5])), endpoint=c(as.character(endpoints[,6]), as.character(endpoints[,6])), group=gl(2,nrow(endpoints),label=c("PGx", "STD")))
endpoints2$protocol = factor(endpoints2$protocol, levels = c("Coumagen", "Coumagen Simulation", "Wilson Simulation"))
endpoints2$endpoint = factor(endpoints2$endpoint, levels = c("All Patients", "Multiple Variants", "Wild Type", "Wild Type & Multiple Variant", "Single Variant")) 
endpoints2$group = factor(endpoints2$group, levels = c("STD", "PGx"))

endpoints2.coum = endpoints2[which(endpoints2$protocol == "Coumagen" | endpoints2$protocol == "Coumagen Simulation"),]
endpoints2.coum$endpoint = factor(endpoints2.coum$endpoint, levels = c("Single Variant", "Wild Type & Multiple Variant", "Wild Type", "Multiple Variants", "All Patients"))

endpoints2.coumWils = endpoints2[which(endpoints2$protocol == "Coumagen Simulation" | endpoints2$protocol == "Wilson Simulation"),]


# plots coumagen actual and coumagen simulation
p=qplot(mean, endpoint, data=endpoints2.coum, color = factor(group), geom=c("errorbarh", "point"), xmin=mean-sd, xmax=mean+sd)
p + facet_grid(protocol ~ ., scales="free", space="free") + labs(x = "Percent TTR In-Range", y = "Endpoint", colour = "Group") + xlim(0,100)

p=qplot(mean, endpoint, data=endpoints2.coumWils, color = factor(protocol), geom=c("errorbarh", "point"), xmin=mean-sd, xmax=mean+sd)
p + facet_grid(endpoint ~ group, scales="free", space="free") + labs(x = "Percent TTR In-Range", y = "Endpoint", colour = "Simulation") + xlim(0,100) + opts(strip.text.y = theme_blank(), legend.position="top", base_size=10)

# density plots based on reviewer feedback
x = melt(cw2d.out[c(2,3,4,5,6)])
x = cbind(x, protocol = rep("wilson", nrow(x)))
y = melt(cstdpgx.out[c(2,3,4,5,6)])
y = cbind(y, protocol = rep("coumagen", nrow(y)))
z = rbind(x,y)
z$value = 100 - z$value
names(z) = c("index", "group", "mean", "endpoint", "protocol")
z.1 = subset(z, group %in% c("meanPG", "meanSTD"))
z.1$group = droplevels(z.1$group)
z.1$endpoint = as.factor(z.1$endpoint)
levels(z.1$endpoint) = c("All Patients", "Multiple Variants", "Single Variant", "Wild Type", "Wild Type & Multiple Variant")
z.1$endpoint = factor(z.1$endpoint, levels = rev(c("Multiple Variants", "Single Variant", "Wild Type & Multiple Variant", "Wild Type", "All Patients")))
levels(z.1$group) = c("PGx", "STD")
z.1$group = factor(z.1$group, levels = c("STD", "PGx"))

z.1.wilson = subset(z.1, protocol == "wilson")
z.1.coum = subset(z.1, protocol == "coumagen")
z.1.coum$protocol = droplevels(z.1.coum$protocol)
z.1.wilson$protocol = droplevels(z.1.wilson$protocol)


# the trick to get multiple density plots on this faceted grid is to seperate the data
p=ggplot(z.1.coum, aes(x=mean, fill=protocol))
print(p + geom_density(data=z.1.coum, alpha=0.4) + 
  geom_density(data=z.1.wilson, alpha=0.4) + 
  labs(fill="Protocol") + labs(x = "Percent TTR In-Range", y = "Density") +
  opts(legend.position="top", base_size=10) +
  facet_grid(endpoint ~ group))

# ggsave("/Users/vincentfusaro/Documents/Papers/Papers In Progress/Clinical Avatar/Figures/TTRInRangeCoumWilsonSimsByArmEndpointDensity_v3.pdf", width=6.75, height=7, dpi=1200)


# save plots
#ggsave("/Users/vincentfusaro/Documents/Papers/Papers In Progress/Clinical Avatar/Figures/TTRInRangeCoumWilsonSimsByArmEndpoint_v5.pdf", width=6.75, height=7, dpi=1200)
#ggsave("/Users/vincentfusaro/Documents/Papers/Papers In Progress/Clinical Avatar/Figures/TTRInRangeCoumagenOrigCoumSimEndpoints_v2.pdf", width=6.75, height=7, dpi=1200)


# # generates "tie-fighter" plots for only the ALL PATIENTS - this really for slide presentations
# endpoints3 = subset(endpoints2, endpoint=="All Patients")
# endpoints3$mean = 100 - endpoints3$mean
# 
# # size increases the error bars and the point diameter
# p=qplot(mean, arm, data=endpoints3, color = factor(arm), geom=c("point", "errorbarh"), xmin=mean-sd, xmax=mean+sd, height=0.35, size=I(1))
# p + facet_grid(protocol ~ ., scales="free", space="free") + opts(strip.text.y = theme_text(), legend.position = "none") + labs(x = "Percent TTR In-Range", y = "Group") + xlim(20,100) + geom_point(size=I(3))


# make barplots for INR measurements
# measure = data.frame(mean=c(7.2,8.1,9.4,10.0,9.9,10.1), sd=c(2.3,3.5,0.99, 1.72,1.32,1.49), protocol=c("Coumagen Original", "Coumagen Original", "Wilson Simulation", "Wilson Simulation", "Coumagen Simulation", "Coumagen Simulation"), arm=c("PGx", "STD", "PGx", "STD", "PGx", "STD"))
# p= qplot(Group, data=measure, geom="bar", fill=Group, weight=mean, position=dodge)
# p + geom_errorbar(limits, position=dodge, width=0.2) + theme_gray(base_size=14) + facet_grid(. ~ protocol) + labs(x="Group", y="Number of INR Measurements")


# simulation heatmaps
#tmp1 = apply(cstdpgx.out$ttrInMatrix,1,rescale)
# tmp1 = t(cstdpgx.out$ttrInMatrix)
# tmp1 = rbind(tmp1[102:200,], tmp1[1:101,])
# heat1 = data.frame(sim=as.factor(1:200), group=c(rep("STD",99), rep("PGx",101)), protocol=rep("Coumagen Simulation",200), tmp1)
# 
# #tmp2 = apply(cw2d.out$ttrInMatrix,1,rescale)
# tmp2 = t(cw2d.out$ttrInMatrix)
# tmp2 = rbind(tmp2[102:200,], tmp2[1:101,])
# heat2 = data.frame(sim=as.factor(1:200), group=c(rep("STD",99), rep("PGx",101)), protocol=rep("Wilson Simulation",200), tmp2)
# 
# heat = rbind(heat1,heat2)
# heat.m = melt(heat)
# 
# # convert the variables from X1, X2, X3 to 1,2,3
# heat.m$variable = as.numeric(gsub("X", 0, heat.m$variable))
# 
# heat.m$group = factor(heat.m$group, levels = c("STD", "PGx"))
# heat.m$sim = as.numeric(heat.m$sim)
# 
# # from RColorBrewer -> brewer.pal(6, "PuRd") -> display.brewer.pal(6, "PuRd")
# p = ggplot(heat.m, aes(y=variable, x=sim)) + geom_tile(aes(fill=value)) + scale_fill_gradientn("Percent TTR", colours=brewer.pal(6, "PuRd"))
# p + facet_grid(. ~ protocol) + theme_grey(base_size = 10) + labs(x = "Avatars", y = "Simulations") +
#     opts(axis.text.x = theme_text(), axis.text.y = theme_text(), panel.background = theme_blank(), panel.grid.minor = theme_blank(), panel.grid.minor = theme_blank())


# p = ggplot(heat.m, aes(variable, sim)) + geom_tile(aes(fill=value)) + scale_fill_gradient(low="yellow", high="red")
# p + facet_grid(protocol ~.) + theme_grey(base_size = 9) + labs(x = "",
#     y = "") + scale_x_discrete(expand = c(0, 0)) +
#     scale_y_discrete(expand = c(0, 0)) + opts(legend.position = "none",
#     axis.ticks = theme_blank(), axis.text.x = theme_blank(), axis.text.y = theme_blank())
    

# inr & dose plots with error ranges from raw data
# need to use this stuff for these plots (same for dosing too)
# quantile(cw2d.out$ttrIn[,1], probs=c(0.01,0.25,0.75,0.99))

##################
##################
# CLEAN UP THE VARIABLE NAMES AND LOAD THE RAW DATA (FOR LOOP FOR TRIAL.OUT) ######################
# day, Q1, Q25, Q50, Q75, Q99, cyp2c9, protocol, arm
sdAvatars = read.table("/Users/vincentfusaro/Dev/ClinicalAvatars/LPM-INR-Modeling/Datasets/Coumagen_Files/st_200k_dosed.txt", sep="\t", header=T)
pgAvatars = read.table("/Users/vincentfusaro/Dev/ClinicalAvatars/LPM-INR-Modeling/Datasets/Coumagen_Files/pharm_200k_dosed.txt", sep="\t", header=T)
cyplevels = c("*1/*1", "*1/*2", "*1/*3", "*2/*3", "*3/*3")
protocols = c("Coumagen Simulation", "Wilson Simulation")
group = c("PGx", "STD")
inrData = {}
doseData = {}

for (r in 1:length(rawData)) {
    load(rawData[r])
    if (r == 1) {
        # replace any doses > 15 with the max dose, which was 15 for this simulation.  This only affects the Coumagen PGx arm which is based on the
        # pharm_coeff which can lead to doses higher than the max dose.  This is automatically corrected when we convert the dose to the pill combination
        # in which case it uses the max dose, which was 15.  So even though the dose might be higher than the maxDose the maxDose was used in the INR model.
        
        x = trial.out[,2,1:101000]
        x[x > 15] = 15
        trial.out[,2,1:101000] = x
    }
    for (g in 1:length(group)) {
        for (i in 1:length(cyplevels)) {
            if (group[g] == "PGx") {
                index = which(pgAvatars$CYP2C9 == cyplevels[i])  
            }
            if (group[g] == "STD") {
                index = which(sdAvatars$CYP2C9 == cyplevels[i]) + 101000
            }
            inr.q = apply(trial.out[,1,index], 1, quantile, probs=c(0.01,0.25,0.50,0.75,0.99))
            dose.q = apply(trial.out[,2,index], 1, quantile, probs=c(0.01,0.25,0.50,0.75,0.99)) 
            
            tmpINR = data.frame(day=1:90, t(inr.q), protocol=rep(protocols[r],90), group=rep(group[g],90), cyp2c9=rep(cyplevels[i],90))
            tmpDose = data.frame(day=1:90, t(dose.q), protocol=rep(protocols[r],90), group=rep(group[g],90), cyp2c9=rep(cyplevels[i],90))    
            
            inrData = rbind(inrData, tmpINR)
            doseData = rbind(doseData, tmpDose)           
        }
    }
}
names(inrData) = c("day", "Q1", "Q25","Q50","Q75","Q99","protocol","group","cyp2c9")
names(doseData) = names(inrData)
inrData$group = factor(inrData$group, levels = c("STD", "PGx"))
doseData$group = factor(doseData$group, levels = c("STD", "PGx"))

# INR
print(qplot(day, Q50, data=inrData, geom="line") + 
    facet_grid(cyp2c9 ~ protocol + group) +
    geom_smooth(aes(ymin=Q1, ymax=Q99), data=inrData, stat="identity", fill="lightblue", alpha=1) + 
    geom_smooth(aes(ymin=Q25, ymax=Q75), data=inrData, stat="identity", fill="steelblue", alpha=1) + 
    geom_line(alpha=1, color="black") + opts(strip.text.y = theme_text()) + labs(x="Day", y="INR"))

#ggsave("/Users/vincentfusaro/Documents/Papers/Papers In Progress/Clinical Avatar/Figures/INR_CoumagenWilsonSims_CYP2C9_Quantiles_v2.pdf", width=6.75, height=7, dpi=1200)

# Dose
print(qplot(day, Q50, data=doseData, geom="line") + 
    facet_grid(cyp2c9 ~ protocol + group) +
    geom_smooth(aes(ymin=Q1, ymax=Q99), data=doseData, stat="identity", fill="lightblue", alpha=1) + 
    geom_smooth(aes(ymin=Q25, ymax=Q75), data=doseData, stat="identity", fill="steelblue", alpha=1) + 
    geom_line(alpha=1, color="black") + opts(strip.text.y = theme_text()) + labs(x="Day", y="Dose (mg/day)"))

#ggsave("/Users/vincentfusaro/Documents/Papers/Papers In Progress/Clinical Avatar/Figures/Dose_CoumagenWilsonSims_CYP2C9_Quantiles_v2.pdf", width=6.75, height=7, dpi=1200)
    
# testing heatmap stuff...
# x = matrix(rnorm(200,69,15), nrow=20, ncol=10)
# heatmap(as.matrix(x), Rowv=NA, Colv=NA, col=cm.colors(256), scale="row")
# x = data.frame(sim=as.factor(1:20), x)
# x.m = melt(x)
# (p = ggplot(x.m, aes(variable, sim)) + geom_tile(aes(fill=value), colour="white") + scale_fill_gradient(low="#EFF3FF", high="#08519C"))
# base_size <- 9
# # heatmap seems a little faded - need to remove more white lines (I think)
# p + theme_grey(base_size = base_size) + labs(x = "",
#     y = "") + scale_x_discrete(expand = c(0, 0)) +
#     scale_y_discrete(expand = c(0, 0)) + opts(legend.position = "none",
#     axis.ticks = theme_blank(), axis.text.x = theme_blank(), axis.text.y = theme_blank())

# generate smooth shading confidence plots for INR
# lcl = 3*sd, ucl = 3*sd
# qplot(day,mean, data=y, geom="line") + geom_smooth(aes(ymin=lcl, ymax=ucl), data=z, stat="identity")

