## za křížkem jsou poznámky, které nemají vliv na skript (co je na řáduk za křížkem , to R ignoruje)



# Načtení data a příprava -------------------------------------------------

setwd("c:/Users/Hynek/Dropbox/MU/DP/Vendula Šamajová/agregace/source/") ## nastavení cesty ke složce se soubory

files <- list.files() ## přečte názvy souborů
files ## vypíše názvy souborů

files2 <- gsub(pattern = "\\.txt", replacement = "", x = files) ## odstraní přípony
ID <- substr(files2, 1, nchar(files2)-1) ## nachystá si ID osob
table(ID) ## kontrola: vypíše ID osob a kolikrát se které vyskytuje (mělo by každé 4krát)

trials <- substr(files2, nchar(files2), nchar(files2)) ## čísla trialu



# Vytvoření dlouhých dat --------------------------------------------------

alldata <- NULL ## prázdný objekt
for (i in c(1:length(ID))) { ## tato smyčka mezi závorkami vytvoří všechna data
  y <- read.table(files[i], sep=" ")
  x <- data.frame(ID=ID[i], ## ID respondenta
                  trial=trials[i], ## číslo trialu
                  item=paste0(LETTERS[as.numeric(trials[i])], c(1:nrow(y))), ## číslo položky ve tvaru např. A5, kde A je písmeno trialu (A-D) a 5 je pořadí položky v něm
                  y ## vlastní data
                  )
  x$spravne <- 0 ## sloupec pro správné odpovědi
  x$spravne[x$V3 == "spravne"] <- 1 ## správné odpovědi označí jedničkou
  x$V3 <- NULL ## odstraní proměnnou
  x$vcas <- 0 ## vytvoří novou proměnnou vcas se samými nulami
  x$vcas[x$V4 == "vcas"] <- 1 ## včasné záznamy přepíše na 1
  x$V4 <- NULL ## odstraní nadbytečnou proměnnou
  x$V5 <- NULL ## odstraní prázdný sloupec
  alldata <- rbind(alldata, x)
}

names(alldata)[4:6] <- c("num1", "num2","time") ## názvy proměnných 3-5

head(alldata) ## první pár řádků
write.csv2(alldata, "../alldata.csv") ## uloží soubor o složku výše, než ve které jsou uložená data


# Vytvoření širokých dat --------------------------------------------------

widedata <- NULL ## prázdná široká data
IDs <- names(table(alldata$ID)) ## každé unikátní ID
items <- alldata$item[alldata$ID == IDs[1]] ## každá unikátní položka

for (p in IDs) {
  x <- alldata$spravne[alldata$ID == p] ## správnost pro člověka p
  y <- alldata$time[alldata$ID == p] ## čas pro člověka p
  z <- alldata$vcas[alldata$ID == p] ## včasnost pro člověka p
  widedata <- rbind(widedata, c(x, y, z)) ## sloučí vše s předchozími osobami
}

widedata <- as.data.frame(widedata) ## převede na dataframe
widedata <- cbind(IDs, widedata) ## ID do prvního sloupce
names(widedata) <- c("ID", paste(items, "S", sep="_"), 
                     paste(items, "T", sep="_"), 
                     paste(items, "V", sep="_")) ## pojmenuje položky; S = správnost, T = čas, V = včasnost

write.csv2(widedata, "../widedata.csv") ## uloží soubor o složku výše, než ve které jsou uložená data
