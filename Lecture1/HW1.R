energy = read.csv("EnergyEfficiencyData.csv", header = TRUE, stringsAsFactors = FALSE)
names(energy)
str(energy)
pairs(energy)

#Interesting Relationships:
# Relative Compactness and Surface Area are inversely correlated
# Heating Load and Cooling Load
# Heating/Cooling load and Overall Height
# Heating/Cooling Load and Roof Area
# Heating/Cooling Load and Surface Area & Relative Compactness
# Roof Area and Relative Compactness
# Overall Height and Relative Compactness
# Surface Area and Wall Area
# Surface Area and Roof Area
# Surface Area and Overall Height
# Roof Area and Overall Height
# RoofArea and Orignetation
# Roof Area and Glazing Area
# Roof Area and Glazing Area distribution
# Overall Height and Orientation
# Overall Height and Glazing Area
# Overall Height and Glazing area distribution
#

hist(energy$Cooling.Load, main = paste("Histogram of Cooling Load"), xlab = "Cooling Load", ylab = "Counts")

hist(energy$Relative.Compactness, main = paste("Histogram of Relative Compactness"), xlab = "Relative Compactness", ylab = "Counts")


hist(energy$Surface.Area, main = paste("Histogram of Surface Area"), xlab = "Surface Area", ylab = "Counts")

require(ggplot2)
ggplot(e2, aes(y = Heating.Load, x = Roof.Area))+ geom_point() + xlab("Roof Area") + ylab("Heating Load") + ggtitle("Relationship between Heating Load and Roof Area")
ggplot(e2, aes(y = Cooling.Load, x = Roof.Area))+ geom_point() + xlab("Roof Area") + ylab("Cooling Load") + ggtitle("Relationship between Cooling Load and Roof Area")


ggplot(e2, aes(y = Heating.Load, x = Overall.Height))+ geom_point() + xlab("Overall Height") + ylab("Heating Load") + ggtitle("Relationship between Heating Load and Overall Height")
ggplot(e2, aes(y = Cooling.Load, x = Overall.Height))+ geom_point() + xlab("Overall Height") + ylab("Cooling Load") + ggtitle("Relationship between Cooling Load and Overall Height")

ggplot(e2, aes(y = Heating.Load, x = Surface.Area))+ geom_point() + xlab("Surface Area") + ylab("Heating Load") + ggtitle("Relationship between Heating Load and Surface Area")
ggplot(e2, aes(y = Cooling.Load, x = Surface.Area))+ geom_point() + xlab("Surface Area") + ylab("Cooling Load") + ggtitle("Relationship between Cooling Load and Surface Area")


ggplot(e2, aes(y = Heating.Load, x = Glazing.Area))+ geom_point() + xlab("Glazing Area") + ylab("Heating Load") + ggtitle("Relationship between Heating Load and Glazing Area")
ggplot(energy, aes(x = factor(Glazing.Area), y = Heating.Load)) + geom_violin(trim = TRUE, draw_quantiles = c(0.25,0.5,0.75)) + xlab("Glazing Area") + ylab("Heating Load") + ggtitle(" Heating Load by Glazing Area")


ggplot(e2, aes(y = Cooling.Load, x = Surface.Area))+ geom_point() + xlab("Surface Area") + ylab("Cooling Load") + ggtitle("Relationship between Cooling Load and Surface Area")



ggplot(e2, aes(y = Heating.Load, x = Glazing.Area))+ geom_point() + xlab("Surface Area") + ylab("Heating Load") + ggtitle("Relationship between Heating Load and Surface Area")
