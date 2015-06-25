munis_orig = read.table("data-raw/obce_lau2_2015.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
munis_lau = munis_orig$lau

munis = data.frame(lau2 = character(0), name = character(0), latitude = numeric(0), longitude = numeric(0),
    lau1 = character(0), nuts3 = character(0), constituency = character(0), wikidata = integer(0), stringsAsFactors = FALSE)
munis[1:length(munis_lau), ] = NA

no_okres = c()
no_obec = c()
library(jsonlite)
library(WikidataR)
for (m in 1:length(munis_lau)) {
    tmp = fromJSON(paste0("https://wdq.wmflabs.org/api?q=string[782:\"", munis_lau[m], "\"]")) # 782 is Wikidata LAU property
    if (length(tmp$items) == 0) {
        no_obec = c(no_obec, munis_lau[m])
        next
    }
    else if (length(tmp$items) > 1) { # some LAUs are not unique
        item_to_use = NA
        for (item in tmp$items) {
            wdata = get_item(item)
            country = wdata$claims$P17$mainsnak$datavalue$value["numeric-id"]
            if (country == 213) { # Czech Republic
                item_to_use = item
                break
            }
        }
        if (is.na(item_to_use)) {
            print(tmp$items)
            stop("No one of multiple items is in Czech Republic.")
        }
    }
    else
        item_to_use = tmp$items
    munis[m, c("lau2", "wikidata")] = c(munis_lau[m], item_to_use)
    wdata = get_item(munis$wikidata[m]) # get_item(1, config = list(verbose = 1))
    munis[m, "name"] = wdata$labels$cs$value
    if (nrow(wdata$claims$P625$mainsnak$datavalue$value) > 1)
        warning(paste0("Wikidata has more coordinates (the first one used): ", munis[m, "name"]))

    munis[m, c("latitude", "longitude")] =  c(wdata$claims$P625$mainsnak$datavalue$value[1, c("latitude", "longitude")])
    if (is.null(wdata$claims$P131$mainsnak$datavalue$value["numeric-id"]))
        no_okres = c(no_okres, munis[m, "name"])
    else {
        if (nrow(wdata$claims$P131$mainsnak$datavalue$value) > 1) { # sometimes also region (which is probably not correct)
            for (item in wdata$claims$P131$mainsnak$datavalue$value[["numeric-id"]]) {
                cs_label = get_item(item)$labels$cs$value
                if (gsub("okres", "", tolower(cs_label)) != tolower(cs_label)) {
                    munis[m, c("lau1")] = item
                    break
                }
            }
        }
        else
            munis[m, c("lau1")] = wdata$claims$P131$mainsnak$datavalue$value["numeric-id"]
    }
    if (m %% 5 == 0)
        cat(paste0(munis[m, "name"], "\n"))
}

if (length(no_obec) > 0)
    warning(paste0("Missing Wikidata for municipalities of LAU2: ", paste(no_obec, collapse = " ")))
if (length(no_okres) > 0)
    warning(paste0("Missing LAU1 in Wikidata for municipalities: ", paste(no_okres, collapse = " ")))

munis[munis$lau2 == "554782", "lau1"] = "CZ0100" # Praha
for (okres in unique(munis$lau1)) {
    if (okres != "CZ0100")
        munis$lau1[munis$lau1 == okres] = get_item(okres)$claims$P782$mainsnak$datavalue$value
}
munis$nuts3 = substr(munis$lau1, 1, 5)

load("data/se2010.rda")
load("data/se2012.rda")
load("data/se2014.rda")
se = rbind(se2010, se2012, se2014)
new_munis = c(`500062` = "77", `500046` = "67", `500071` = "77", `534382` = "40") # Sázava moved to 40 so no election during 2010-2014
noo = c()
for (m in 1:nrow(munis)) {
    # new municipalities
    if (munis$lau2[m] %in% names(new_munis))
        munis$constituency[m] = new_munis[munis$lau2[m]]
    else if (!(munis$lau2[m] %in% c("554782", "582786", "554821", "554791"))) { # more consituencies in one municipality (Praha, Brno, Ostrava, Plzeň)
        nos = unique(se$constituency[se$lau2 == munis$lau2[m]])
        munis$constituency[m] = nos[length(nos)] # for length(nos) > 1 there was a change of constituency border - the last one is current
    }
}
obce = munis
obce[, c("lau1", "nuts3", "constituency")] = lapply(obce[, c("lau1", "nuts3", "constituency")], as.factor)
save(obce, file = paste0("data/obce.rda"), compress = "xz")
