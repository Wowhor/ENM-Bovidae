# Code for creating Ecological Niche Models (ENMs) of 5 wild Bovidae species. 
 * *Bos gaurus* (Gaur)
 * *Bos javanicus* (Banteng)
 * *Bubalus arnee* (Wild water buffalo)
 * *Capricornis sumatraensis* (Mainland serow)
 * *Naemorhedus griseus* (Chinese goral)

These codes are contain the some parts of data from the paper: 
"Mapping threatened Thai Bovidae species provides opportunities for improved conservation outcomes in Asia"
(unpublishing paper). 

We used the package [ENMTML](https://github.com/andrefaa/ENMTML) for model building.

# How to use these scripts:
 - download the dataset: [Google Drive](https://drive.google.com/drive/folders/1tKMkeltE1eSnrmTh4obpIPjYEJaJTChX?usp=sharing) 
      #Warning Note#: These files on Google Drive are large (size = 9.8 GB), and could be slow when downloaded. 
      * folders and subfolder structure: 
        * 1) data_preparation: the dataset use for modelling
             * 1.1 `acc`: the polygons (.shp) of accessible areas, it contains 6 subfolders large accessible areas (named: acc_la), and the others for species-specific accessible areas (accbanteng, accbuffalo, accgaur, accserow, accgoral). Selecting the accessible areas using [ecoregions](https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world)
             * 1.2 `adm_border`: the polygons (.shp) of admistrative border using for mapping and creating figures.
             * 1.3 `env`: contains all environmental variables, cropped as two accessible areas 1) species-specific accessible areas (envgaur, envbanteng, eenvbuffalo, envserow, envgoral) and 2) large accessible area (env_la)
             * 1.4 `iucn`: IUCN range (.shp) for all the species
             * 1.5 `PA_and_country`: contains the data of countries and protected areas boundary (.shp, .tif). 
             * 1.6 `text files` (.txt): some of occurrence data from the open source ([gbif.org](https://www.gbif.org/))
        * 2) result_sample: Some of the results (habitat suitability maps, richness maps) from modelling, contains 3 subfolders.
             * 2.1 `binary`: The best binary model of the habitat suitability (.tif).
             * 2.2 `ensembles`: The best ensemble models of the habitat suitability (.tif).
             * 2.3 `dataset_combine`: The dataset extracting and combining from rasters results, contains habitat suitability values (binary and probability), protected areas category, environmental variables, occurrence data (from gbif.org).   
 - setwd() for setting up the working directory 
 - run the scripts according to the numerical order
