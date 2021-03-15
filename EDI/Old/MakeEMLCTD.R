# Install and load devtools
install.packages("devtools")
library(devtools)

# Install and load EMLassemblyline
install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)

# Import Templates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
import_templates(path = "C:/Users/Owner/Dropbox/MakeEMLCTD", 
                 license = "CCBY", 
                 data.files = "CTD_Meta_13_18_final.csv")


#Geographic coverage
template_geographic_coverage(path = getwd(),
                             data.path = getwd(),
                             data.table = "chemistry.csv",
                             empty = TRUE,
                             write.file = TRUE)


# Define Categorical Variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#template_categorical_variables(path = "C:/Users/Owner/Dropbox/MakeEMLCTD")
??template_geographic_coverage
# Make the EML for EDI ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
make_eml(path = ,
         dataset.title = "Time series of high-frequency profiles of depth, temperature, dissolved oxygen, conductivity, specific conductivity, chlorophyll a, turbidity, pH, oxidation-reduction potential, photosynthetic active radiation, and descent rate for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in Southwestern Virginia, USA 2013-2018",
         data.table = ,
         data.table.description = c("Reservoir CTD dataset"),
         temporal.coverage = c("2013-03-07", "2019-12-16"),
         geographic.description = "Southwestern Virginia, USA, North America",
         maintenance.description = "ongoing",
         user.domain = "EDI",
         user.id = "ccarey",
         package.id = "edi.200.7")
