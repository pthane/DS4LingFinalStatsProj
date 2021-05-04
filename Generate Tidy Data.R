library(tidyverse)

# Load Data
EPT_Master <- read_csv("./Raw Data/EPT Master.csv")
FCT_Master <- read_csv("./Raw Data/FCT Master.csv")

# Separate by task
EPT_Aspect <- EPT_Master %>% 
  filter(Property == "Preterit")

FCT_Aspect <- FCT_Master %>% 
  filter(Property == "Preterit")


# Create aggregate CSV
Aggregate_Aspect <- rbind(EPT_Aspect, FCT_Aspect)


## Generate CSVs for HS
EPT_Aspect_Heritage <- EPT_Aspect %>%
  filter(ExpGroup == "Heritage") %>% 
  filter(AoA_ENG < 8)

FCT_Aspect_Heritage <- FCT_Aspect %>%
  filter(ExpGroup == "Heritage") %>% 
  filter(AoA_ENG < 8)

Aggregate_Aspect_Heritage <- Aggregate_Aspect %>% 
  filter(ExpGroup == "Heritage") %>% 
  filter(AoA_ENG < 8)


# Standardize data
## Aspect EPT
EPT_Aspect_Heritage <- EPT_Aspect_Heritage %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))

## FCT
FCT_Aspect_Heritage <- FCT_Aspect_Heritage %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))

## Aggregate
Aggregate_Aspect_Heritage = Aggregate_Aspect_Heritage %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


# Write revised CSV files
## Heritage
write_csv(EPT_Aspect_Heritage, "./Tidy Data/Heritage EPT Preterit Data.csv")
write_csv(FCT_Aspect_Heritage, "./Tidy Data/Heritage FCT Preterit Data.csv")
write_csv(Aggregate_Aspect_Heritage, "./Tidy Data/Heritage Aggregate Preterit Data.csv")