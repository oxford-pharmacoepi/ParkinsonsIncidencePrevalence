#### prev
#1. 
SubtypesPrevalenceOverallName <- paste0("SubtypesPrevalenceOverallPopulation", ".png")
png(here("Results", db.name, "Plots", SubtypesPrevalenceOverallName), width = 10, height = 5, units = "in", res = 1200)
print(SubtypesPrevalenceOverall, newpage = FALSE)
dev.off()

#2. 
SubtypesPrevalenceBothName <- paste0("SubtypesPrevalenceBoth", ".png")
png(here("Results", db.name, "Plots", SubtypesPrevalenceBothName), width = 10, height = 5, units = "in", res = 1200)
print(SubtypesPrevalenceBoth, newpage = FALSE)
dev.off()

#3. 
SubtypesPrevalenceMaleName <- paste0("SubtypesPrevalenceMale", ".png")
png(here("Results", db.name, "Plots", SubtypesPrevalenceMaleName),
    width = 10, height = 5, units = "in", res = 1200)
print(SubtypesPrevalenceMale, newpage = FALSE)
dev.off()

#4. 
SubtypesPrevalenceFemaleName <- paste0("SubtypesPrevalenceFemale", ".png")
png(here("Results", db.name, "Plots", SubtypesPrevalenceFemaleName),
    width = 10, height = 5, units = "in", res = 1200)
print(SubtypesPrevalenceFemale, newpage = FALSE)
dev.off()

#5. 
SubtypesPrevalenceStratifiedByAgeAndSexName <- paste0("SubtypesPrevalenceStratifiedByAgeAndSex", ".png")
png(here("Results", db.name, "Plots", SubtypesPrevalenceStratifiedByAgeAndSexName),
    width = 10, height = 5, units = "in", res = 1200)
print(SubtypesPrevalenceStratifiedByAgeAndSex, newpage = FALSE)
dev.off()

#Inc
#1. 
SubtypesIncidenceOverallName <- paste0("SubtypesIncidenceOverallPopulation", ".png")
png(here("Results", db.name, "Plots", SubtypesIncidenceOverallName),
    width = 10, height = 5, units = "in", res = 1200)
print(SubtypesIncidenceOverall, newpage = FALSE)
dev.off()

#2. 
SubtypesIncidenceBothName <- paste0("SubtypesIncidenceBoth", ".png")
png(here("Results", db.name, "Plots", SubtypesIncidenceBothName),
    width = 10, height = 5, units = "in", res = 1200)
print(SubtypesIncidenceBoth, newpage = FALSE)
dev.off()

#3. 
SubtypesIncidenceMaleName <- paste0("SubtypesIncidenceMale", ".png")
png(here("Results", db.name, "Plots", SubtypesIncidenceMaleName),
    width = 10, height = 8, units = "in", res = 1200)
print(SubtypesIncidenceMale, newpage = FALSE)
dev.off()

#4. 
SubtypesIncidenceFemaleName <- paste0("SubtypesIncidenceFemale", ".png")
png(here("Results", db.name, "Plots", SubtypesIncidenceFemaleName),
    width = 10, height = 8, units = "in", res = 1200)
print(SubtypesIncidenceFemale, newpage = FALSE)
dev.off()

#5.
SubtypesIncidenceStratifiedByAgeAndSexName <- paste0("SubtypesIncidenceStratifiedByAgeAndSex", ".png")
png(here("Results", db.name, "Plots", SubtypesIncidenceFemaleName),
    width = 10, height = 8, units = "in", res = 1200)
print(SubtypesIncidenceStratifiedByAgeAndSex, newpage = FALSE)
dev.off()
