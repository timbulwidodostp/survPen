# Olah Data Semarang
# WhatsApp : +6285227746673
# IG : @olahdatasemarang_
# Hazard and excess hazard modelling with multidimensional penalized splines Use survPen With (In) R Software
# (Excess) hazard model with (multidimensional) penalized splines and integrated smoothness estimation Use survPen With (In) R Software
install.packages("survPen")
library("survPen")
library("splines")
survPen = read.csv("https://raw.githubusercontent.com/timbulwidodostp/survPen/main/survPen/survPen.csv",sep = ";")
# Estimation Hazard and excess hazard modelling with multidimensional penalized splines Use survPen With (In) R Software
# unpenalized
f <- ~ns(fu,knots=c(0.25, 0.5, 1, 2, 4),Boundary.knots=c(0,5))
survPen_unpenalized <- survPen(f,data=survPen,t1=fu,event=dead)
summary(survPen_unpenalized)
# penalized
f.pen <- ~ smf(fu,knots=c(0,0.25, 0.5, 1, 2, 4,5))
survPen_penalized <- survPen(f.pen,data=survPen,t1=fu,event=dead)
summary(survPen_penalized)
# constant hazard model
f.cst <- ~1
survPen_constant_hazard_model <- survPen(f.cst,data=survPen,t1=fu,event=dead)
summary(survPen_constant_hazard_model)
# hazard model
f1 <- ~smf(fu,df=5)
survPen_hazard_model <- survPen(f1,data=survPen,t1=fu,event=dead,expected=NULL,method="LAML")
summary(survPen_hazard_model)
# excess hazard model
f1 <- ~smf(fu,df=5)
survPen_excess_hazard_model <- survPen(f1,data=survPen,t1=fu,event=dead,expected=rate,method="LAML")
summary(survPen_excess_hazard_model)
# Hazard and excess hazard modelling with multidimensional penalized splines Use survPen With (In) R Software
# (Excess) hazard model with (multidimensional) penalized splines and integrated smoothness estimation Use survPen With (In) R Software
# Olah Data Semarang
# WhatsApp : +6285227746673
# IG : @olahdatasemarang_
# Finished