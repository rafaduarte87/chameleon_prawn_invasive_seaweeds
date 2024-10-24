# Codes and data for the manuscript "Adaptation in the Anthropocene: how behavioural choice and colour change enables chameleon prawns to camouflage on non-native seaweeds" published in the Journal of Animal Ecology.

## Authors: Rafael Campos Duarte; Beks Ryan; Gustavo Muniz Dias; and Martin Stevens

## Contact: Rafael Campos Duarte, rafaduarte87@gmail.com / rafael.duarte@ua.pt

## Abstract: Using image analysis and visual modelling of a fish predator, we assessed the colour variation and camouflage of chameleon prawns (Hippolyte varians) occupying the non-native seaweeds brown wireweed (Sargassum muticum) and pink harpoon weed (Asparagopsis armata) in southwest UK. We performed laboratory trials to examine whether prawns maintain their preference for colour-matching native substrates when given a choice between them and non-native seaweeds, and if they can change their coloration to improve camouflage against non-native substrates. We found that prawns exhibited phenotypic diversity and camouflage that varied with the non-native seaweed species, with low colour variation and effective camouflage on pink harpoon weed, but high colour diversity and reduced concealment against brown wireweed. Prawns chose non-native seaweeds when the alternative native substrate provided mismatching coloration, but they did not exhibit any preference between colour-matching native and non-native seaweeds. Once in non-native habitats, prawns changed their appearance over a few days to match the background, sometimes faster than when changing on native seaweeds of contrasting coloration. 

## Date of data collection: October 2022 - May 2023

## Location: Gyllyngvase and Castle beaches, Falmouth, Cornwall, UK (50°08′33″N, 05°04′08″W) 

## Responsible for field collections: Rafael Campos Duarte; Beks Ryan

## Responsible for writing the codes: Rafael Campos Duarte

## List of files
* scripts: script_background_matching.R; script_behavioural_choices.R; script_colour_change.R
* datasets: background_matching_dataset.xlsx; behavioural_choices_dataset.xlsx; colour_change_dataset.xlsx

Each script contains the code for the analyses and figures of the different activities conducted in the study. The name of the script corresponds to the main activity and the specific objective of each statistical analysis is described within the corresponding script.

## Session info
*R version 4.4.1 (2024-06-14 ucrt) -- Platform: x86_64-w64-mingw32/x64 (64-bit)

## Packages installed and their versions in R 4.4.1
*afex v.1.3-1
*caret v.6.0-94
*colorspace v.2.1-1
*DHARMa v.0.4.7
*emmeans v.1.10.5
*lme4 v.1.1-35.5
*lmerTest v.3.1-3
*MASS v.7.3-61
*munsell v.0.5.1
*patchwork v.1.3.0
*performance v.0.12.3
*readxl v.1.4.3
*rstatix v.0.7.2
*tidyverse v.2.0.0

## Workflow: Each script can be executed separately. To replicate the figures and statistical analyses from the manuscript, run the codes available on the link: https://doi.org/10.5061/dryad.xpnvx0kr0.

## General information about the scripts and datasets: The scripts and datasets refer to the analyses of the (i) colour variability and camouflage of wild chameleons prawns sampled from non-native seaweeds; (ii) choices between native and non-native seaweeds of green and red chameleon prawns; and (iii) colour change of green and red chameleon prawns against non-native seaweeds and native seaweed of contrasting coloration. The repository consists of six files, being three code scripts and three excel files. The scripts contain the codes to run both the statistical analyses and the graphics of the manuscript.

## Description of the datasets and scripts

### DATASETS

#### background_matching_dataset.xlsx 

This dataset describes the luminance, colour and saturation using the RNL chromaticity space of seaweeds and chameleon prawns (Hippolyte varians) sampled from the non-native seaweeds pink harpoon weed (Asparagopsis armata) and brown wireweed (Sargassum muticum) to the view of the goby fish Gobiusculus flavescens. The dataset also contains the prawn coloration in the human-based L*a*b visual space.

Column names and description:

image: the code used to identify each photograph
seaweed: either the seaweed itself or the seaweed where the prawn was sampled (sm for brown wireweed and hw for pink harpoon weed or brown)
id: the identification code of each individual
date: the date of prawn sampling (NA for the seaweeds)
size: the size of the prawn individual (NA for the seaweeds)
type: either seaweed or prawn 
lum_mean: the mean value of the luminance measured from all ROI (region of interest) pixels
lum_dev: the standard deviation of the luminance measured from all ROI pixels
rnl_x_mean: the mean value of the x colour channel (green-red variation) measured from all ROI pixels
rnl_x_dev: the standard deviation of the x colour channel (green-red variation) measured from all ROI pixels
rnl_y_mean: the mean value of the y colour channel (yellow-blue variation) measured from all ROI pixels
rnl_y_dev: the standard deviation of the y colour channel (yellow-blue variation) measured from all ROI pixels
rnl_saturation_mean: the mean value of the saturation channel measured from all ROI pixels
rnl_saturation_dev: the standard deviation of the saturation channel measured from all ROI pixels
L: the values of Lightness from the L*a*b colour space
A: the values of the green-red colour component from the L*a*b colour space
B: the values of the yellow-blue colour component from the L*a*b colour space

#### behavioural_choices_dataset.xlsx 

This dataset describes the choice of chameleon prawns (Hippolyte varians) between native (the green sea lettuce Ulva lactuca and the red dulse Palmaria palmata) and non-native seaweeds (the pink harpoon weed Asparagopsis armata and the brown wireweed Sargassum muticum) according to the colour of the prawn (green or red), the identity of the non-native seaweed (pink harpoon weed or brown wireweed) and the level of colour matching between the prawn and the native seaweed. Prawn choice was measured twice as the first choice and the choice after 10 minutes.

Column names and description:

type: the colour of the prawn (green - g or red - r)
comparison: the comparison between the seaweeds made in the trial (red dulse - d, pink harpoon weed - hw, green sea lettuce - sl, brown wireweed - sm)
non_native: the non-native seaweed species (hw or sm)
native_match: the colour match between the prawn and the native seaweed offered in the trial (match - 1 or mismatch - 0) 
fist_choice: the initial seaweed choice made by the prawn (d, sl, hw or sm)
ten_minutes: the seaweed chosen by the prawn after 10 minutes (d, sl, hw or sm)

#### colour_change_dataset.xlsx

This dataset contains two spreadsheets. The first describes the luminance and colour change using the RNL chromaticity space of green and red chameleon prawns (Hippolyte varians) maintained for 30 days in colour-mismatching native seaweeds (red dulse Palmaria palmata for green prawns and green sea lettuce Ulva lactuca for red prawns) and the non-native seaweeds pink harpoon weed (Asparagopsis armata) and brown wireweed (Sargassum muticum). The second spreadsheet contains information about the luminance and colour using the RNL chromaticity space of the four different seaweeds used in the experiment. 

Column names and description:

prawn_data_rnl

type: the colour of the prawn (green - g or red - r)
id: the identification code of each individual
seaweed: the seaweed species where the prawn was maintained over the experiment (red dulse - d, pink harpoon weed - hw, green sea lettuce - sl, brown wireweed - sm)
day: the day since the start of the trail when the photograph was taken (0, 5, 10, 15, 20, 25 or 30)
lum_mean: the mean value of the luminance measured from all ROI (region of interest) pixels
lum_dev: the standard deviation of the luminance measured from all ROI pixels
rnl_x_mean: the mean value of the x colour channel (green-red variation) measured from all ROI pixels
rnl_x_dev: the standard deviation of the x colour channel (green-red variation) measured from all ROI pixels
rnl_y_mean: the mean value of the y colour channel (yellow-blue variation) measured from all ROI pixels
rnl_y_dev: the standard deviation of the y colour channel (yellow-blue variation) measured from all ROI pixels
rnl_saturation_mean: the mean value of the saturation channel measured from all ROI pixels
rnl_saturation_dev: the standard deviation of the saturation channel measured from all ROI pixels
LAB_CIE L_Mean: the values of Lightness from the L*a*b colour space
LAB_CIE A_Mean: the values of the green-red colour component from the L*a*b colour space
LAB_CIE B_Mean: the values of the yellow-blue colour component from the L*a*b colour space
size: the size of the prawn individual

seaweed_data_rnl

seaweed: the seaweed species (red dulse - d, pink harpoon weed - hw, green sea lettuce - sl, brown wireweed - sm)
id: the identification code of each seaweed sample
lum_mean: the mean value of the luminance measured from all ROI (region of interest) pixels
lum_dev: the standard deviation of the luminance measured from all ROI pixels
rnl_x_mean: the mean value of the x colour channel (green-red variation) measured from all ROI pixels
rnl_x_dev: the standard deviation of the x colour channel (green-red variation) measured from all ROI pixels
rnl_y_mean: the mean value of the y colour channel (yellow-blue variation) measured from all ROI pixels
rnl_y_dev: the standard deviation of the y colour channel (yellow-blue variation) measured from all ROI pixels
rnl_saturation_mean: the mean value of the saturation channel measured from all ROI pixels
rnl_saturation_dev: the standard deviation of the saturation channel measured from all ROI pixels 
day: dummy variable set at day 30 to add a polygon representing the colour variation of the seaweeds to Figure 4

### SCRIPTS

#### script_background_matching.R

Contains the analyses and the codes to create Figure 2 about the colour variation and camouflage of wild chameleon prawns sampled in the non-native seaweeds pink harpoon weed (Asparagospsis armata) and brown wireweed (Sargassum muticum).

#### script_behavioural_choices.R

Contains the analyses and the codes to create Figure 3 about the behavioural choices of green and red chameleon prawns between native seaweeds with match or mismatchig coloration and the non-native seaweeds (pink harpoon weed and brown wireweed).

#### script_colour_change.R

Contains the analyses and the codes to create Figure 4 of the colour change experiment of green and red chameleon prawns maintained for 30 days on colour-mismatching native seaweeds and non-native seaweeds (pink harpoon weed and brown wireweed). 

## Funding Sources

Fundação de Amparo à Pesquisa do Estado de São Paulo (FAPESP)
Fundação Cearense de Apoio ao Desenvolvimento Científico e Tecnológico (FUNCAP)
