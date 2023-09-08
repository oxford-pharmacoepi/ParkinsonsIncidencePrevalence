drugs_attrition_inc <- IncidencePrevalence::incidenceAttrition(incDrugs)
write.csv(drugs_attrition_inc, "incidence_attrition_CPRD_GOLD.csv")
drugs_attrition_prev <- IncidencePrevalence::prevalenceAttrition(prevDrugs)
write.csv(drugs_attrition_prev, "prevalence_attrition_CPRD_GOLD.csv")

subtypes_attrition_inc <- IncidencePrevalence::incidenceAttrition(incSubtypes)
write.csv(subtypes_attrition_inc, "incidence_attrition_CPRD_GOLD.csv")
subtypes_attrition_prev <- IncidencePrevalence::prevalenceAttrition(prevSubtypes)
write.csv(subtypes_attrition_prev, "prevalence_attrition_CPRD_GOLD.csv")

dip_attrition_inc <- IncidencePrevalence::incidenceAttrition(incSubtype1)
write.csv(dip_attrition_inc, "incidence_attrition_CPRD_GOLD.csv")
dip_attrition_prev <- IncidencePrevalence::prevalenceAttrition(prevSubtype1)
write.csv(dip_attrition_prev, "prevalence_attrition_CPRD_GOLD.csv")

parkinsonism_attrition_inc <- IncidencePrevalence::incidenceAttrition(incSubtype2)
write.csv(parkinsonism_attrition_inc, "incidence_attrition_CPRD_GOLD.csv")
parkinsonism_attrition_prev <- IncidencePrevalence::prevalenceAttrition(prevSubtype2)
write.csv(parkinsonism_attrition_prev, "prevalence_attrition_CPRD_GOLD.csv")

pd_attrition_inc <- IncidencePrevalence::incidenceAttrition(incSubtype3)
write.csv(pd_attrition_inc, "incidence_attrition_CPRD_GOLD.csv")
pd_attrition_prev <- IncidencePrevalence::prevalenceAttrition(prevSubtype3)
write.csv(pd_attrition_prev, "prevalence_attrition_CPRD_GOLD.csv")

vp_attrition_inc <- IncidencePrevalence::incidenceAttrition(incSubtype4)
write.csv(vp_attrition_inc, "incidence_attrition_CPRD_GOLD.csv")
vp_attrition_prev <- IncidencePrevalence::prevalenceAttrition(prevSubtype4)
write.csv(vp_attrition_prev, "prevalence_attrition_CPRD_GOLD.csv")
