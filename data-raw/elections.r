election = setClass("election", slots = c(name = "character"))
election_prez = setClass("election_prez", contains = "election")
election_ps = setClass("election_ps", contains = "election")
election_se = setClass("election_se", contains = "election")
election_kz = setClass("election_kz", contains = "election")
election_kv = setClass("election_kv", contains = "election")
election_ep = setClass("election_ep", contains = "election")

setGeneric("download_file_parties", function(election, zip_file, file) {
    standardGeneric("download_file_parties")
})

setMethod("download_file_parties", c("election", "character", "character"),
    function(election, zip_file, file) {
        download.file(paste0("http://www.volby.cz/opendata/", election@name, "/", zip_file), "tmp.zip", "auto")
        unzip("tmp.zip", file)
    }
)

setMethod("download_file_parties", c("election_prez", "character", "character"),
    function(election, zip_file, file) {
        download.file(paste0("http://www.volby.cz/opendata/", election@name, "/", "PREZreg20130110.zip"), "tmp.zip", "auto")
        unzip("tmp.zip", "PERK.dbf")
        download.file(paste0("http://www.volby.cz/opendata/", election@name, "/", "ciselniky20130110.zip"), "tmp.zip", "auto")
        unzip("tmp.zip", "CPP.DBF")
    }
)

# download for more dates in create_parties
setMethod("download_file_parties", c("election_se", "character", "character"),
    function(election, zip_file, file) {
    }
)

setGeneric("get_file_names", function(election, file_type) {
    standardGeneric("get_file_names")
})

setMethod("get_file_names", c("election_se", "character"),
    function(election, file_type) {
        # the only one with more elections during one year
        if (election@name == "se2014") {
            tmp = list(dirs = c("se2014", "se2014leden", "se2014zari"), reg = c("SE2014reg20141018.zip", "SE2014LEDENreg.zip", "SE2014ZARIreg20140927.zip"),
                cis = c("SE2014ciselniky20141008.zip", "SE2014LEDENciselniky.zip", "SE2014ZARIciselniky20140901.zip"),
                data = c("SE2014data20141018.zip", "SE2014LEDENdata.zip", "SE2014ZARIdata20140927.zip"),
                date = c("2014-10-18", "2014-01-18", "2014-09-27"))
        }
        else {
           year = gsub("[a-zA-Z_]", "", election@name)
           tmp = list(dirs = paste0("se", year), reg = paste0("SE", year, "reg.zip"), cis = paste0("SE", year, "ciselniky.zip"), data = paste0("SE", year, "data.zip"))
           dates = c(`2008` = "10-26", `2010` = "10-23", `2011` = "03-26", `2012` = "10-20")
           tmp[["date"]] = paste0(year, "-", dates[[year]])
        }
        return(tmp[[file_type]])
    }
)

setGeneric("create_parties", function(election) {
    standardGeneric("create_parties")
})

setMethod("create_parties", c("election_prez"),
    function(election) {
        dbf_names = c(reg = "PERK.dbf", cis = "CPP.DBF")
        for (type in c("reg", "cis")) {
            tmp = read.dbf(dbf_names[type], as.is = TRUE)
            if (type == "reg")
                tmp[, c("JMENO", "PRIJMENI")] = iconv(cbind(tmp$JMENO, tmp$PRIJMENI), "CP852", "UTF-8")
            else
                tmp[, c("NAZEV_STRP", "ZKRATKAP8")] = iconv(cbind(tmp$NAZEV_STRP, tmp$ZKRATKAP8), "CP852", "UTF-8")
            file.remove(dbf_names[type])
            assign(type, tmp)
        }
        cands = reg[, c("CKAND", "JMENO", "PRIJMENI", "PSTRANA")]
        for (parnam in c("NAZEV_STRP", "ZKRATKAP8"))
            cands = cbind(cands, sapply(cands$PSTRANA, function(par_no) { cis[[parnam]][cis$PSTRANA == par_no] }))
        cands$JMENO = paste(cands$JMENO, cands$PRIJMENI)
        cands = cands[, c(5, 6, 2, 1)]
        names(cands) = c("name", "abbr", "candidate", "candid_no")
        return(cands)
    }
)

setMethod("create_parties", c("election_se"),
    function(election) {
        dirs = get_file_names(election, "dirs")
        reg_files = get_file_names(election, "reg")
        cis_files = get_file_names(election, "cis")
        el_date = get_file_names(election, "date")
        tmp_parties = list()
        for (d in 1:length(dirs)) {
            tmp = data.frame(zip_file = c(reg_files[d], cis_files[d]), xml_file = c("serk.xml", "cvs.xml"),
                var = c("reg", "cis"), stringsAsFactors = FALSE)
            for (r in 1:nrow(tmp)) {
                download.file(paste0("http://www.volby.cz/opendata/", dirs[d], "/", tmp$zip_file[r]), "tmp.zip", "auto")
                unzip("tmp.zip", tmp$xml_file[r])
                assign(tmp$var[r], xmlParse(tmp$xml_file[r]))
                file.remove(c("tmp.zip", tmp$xml_file[r]))
            }
            nspace_cis = c(ns = xmlNamespace(getNodeSet(cis, "/*")[[1]]))
            nspace_reg = c(ns = xmlNamespace(getNodeSet(reg, "/*")[[1]]))
            cand_count = length(getNodeSet(reg, "/ns:SE_REGKAND/ns:SE_REGKAND_ROW", nspace_reg))
            nas = rep(NA, cand_count)
            tmp_parties[[dirs[d]]] = data.frame(no = nas, name = nas, abbr = nas, constituency = nas, candidate = nas, candid_no = nas, date = nas)

            items = c("VSTRANA", "NAZEV_VS", "NAZEV_VS", "OBVOD", "JMENO", "PRIJMENI", "CKAND")
            for (i in 1:length(items))
                tmp_parties[[dirs[d]]][[i]] = xpathSApply(reg, paste0("/ns:SE_REGKAND/ns:SE_REGKAND_ROW/ns:", items[i]), xmlValue, namespaces = nspace_reg)
            tmp_parties[[dirs[d]]]$candidate = paste(tmp_parties[[dirs[d]]]$candidate, tmp_parties[[dirs[d]]]$candid_no, sep = " ")
            tmp_parties[[dirs[d]]]$candid_no = tmp_parties[[dirs[d]]]$date
            tmp_parties[[dirs[d]]]$date = el_date[d]

            party_nodes = getNodeSet(cis, "/ns:CVS/ns:CVS_ROW", nspace_cis)
            for (p in 1:length(party_nodes)) {
                if (p %% 500 == 0)
                    cat(paste0("Parties: ", p, "/", length(party_nodes), "\n"))
                node = getNodeSet(party_nodes[[p]], "ns:ZKRATKAV8", nspace_cis)
                tmp_no = xmlValue(getNodeSet(party_nodes[[p]], "ns:VSTRANA", nspace_cis)[[1]])
                if (length(node) > 0)
                    abbr = xmlValue(node[[1]])
                else
                    abbr = xmlValue(getNodeSet(party_nodes[[p]], "ns:ZKRATKAV30", nspace_cis)[[1]])
                tmp_parties[[dirs[d]]]$abbr[tmp_parties[[dirs[d]]]$no == tmp_no] = abbr
            }
        }
        parties = tmp_parties[[1]]
        if (length(tmp_parties) > 1) {
            for (p in 2:length(tmp_parties))
                parties = rbind(parties, tmp_parties[[p]])
        }
        return(parties)
    }
)

read_parties_dbf <- function(dbf_file) {
    reg = read.dbf(dbf_file, as.is = TRUE)
    reg[, c("NAZEVCELK", "ZKRATKAK8", "ZKRATKAK30")] = iconv(cbind(reg$NAZEVCELK, reg$ZKRATKAK8, reg$ZKRATKAK30), "CP852", "UTF-8")
    file.remove(dbf_file)
    reg$ZKRATKAK8[is.na(reg$ZKRATKAK8)] = reg$ZKRATKAK30[is.na(reg$ZKRATKAK8)]
    parties = reg[, c("KSTRANA", "NAZEVCELK", "ZKRATKAK8")]
    parties = unique(parties)
    colnames(parties) = c("no", "name", "abbr")
    return(parties)
}

setMethod("create_parties", c("election_ps"),
    function(election) {
        return(read_parties_dbf("PSRKL.dbf"))
    }
)

setMethod("create_parties", c("election_kz"),
    function(election) {
        return(read_parties_dbf("KZRKL.dbf"))
    }
)

setGeneric("create_data", function(election) {
    standardGeneric("create_data")
})

setMethod("create_data", c("election_prez"),
    function(election) {
        kraje = get_okres_ids(election)
        empty_ucast = data.frame(districts = integer(0), round = integer(0), voters = integer(0), envel_given = integer(0),
            envel_returned = integer(0), valid_votes = integer(0), lau2 = character(0), stringsAsFactors = FALSE)
        empty_df = data.frame(party = integer(0), candidate = character(0), candid_no = integer(0), round = integer(0),
            votes = integer(0), lau2 = character(0), stringsAsFactors = FALSE)
        all_ucast = empty_ucast
        all_df = empty_df
        for (round in 1:2) {
            for (kraj in kraje) {
                download.file(paste0("http://www.volby.cz/pls/", election@name, "/vysledky_kraj?kolo=", round, "&nuts=", kraj), "tmp.xml", "auto")
                tmp = xmlParse("tmp.xml")
                nspace = c(ns = xmlNamespace(getNodeSet(tmp, "/*")[[1]]))

                munis = getNodeSet(tmp, "/ns:VYSLEDKY_KRAJ/ns:KRAJ//ns:OBEC", nspace)
                municip_count = length(munis)
                ucast = empty_ucast
                ucast[1:municip_count, ] = rep(NA, ncol(ucast))
                ucast$round = round
                df = empty_df
                muni = 1

                items = c("OKRSKY_CELKEM", "ZAPSANI_VOLICI", "VYDANE_OBALKY", "ODEVZDANE_OBALKY", "PLATNE_HLASY")
                for (i in 1:length(items))
                    ucast[[ifelse(i > 1, i + 1, i)]] = xpathSApply(tmp, "//ns:OBEC/ns:UCAST", xmlGetAttr, items[i], namespaces = nspace)
                ucast$lau2 = xpathSApply(tmp, "//ns:OBEC", xmlGetAttr, "CIS_OBEC", namespaces = nspace)

                cislo = xpathSApply(tmp, "//ns:OBEC/ns:HODN_KAND", xmlGetAttr, "PORADOVE_CISLO", namespaces = nspace)
                df[1:length(cislo), ] = rep(NA, ncol(df))
                df$candid_no = cislo
                df$votes = xpathSApply(tmp, "//ns:OBEC/ns:HODN_KAND", xmlGetAttr, "HLASY", namespaces = nspace)
                df$round = round
                cislo = as.integer(cislo)
                cislo_pos = cislo[c(2:length(cislo), NA)]
                diffs = c(0, which(cislo_pos < cislo))
                diffs_shift = diffs[c(2:length(diffs), NA)]
                diffs = diffs_shift - diffs
                diffs[length(diffs)] = nrow(df) - sum(diffs, na.rm = TRUE)
                df$lau2 = rep(ucast$lau2, times = diffs)

                all_ucast = rbind(all_ucast, ucast)
                all_df = rbind(all_df, df)
            }
        }
        return(list(df = all_df, ucast = all_ucast))
    }
)

setMethod("create_data", c("election_se"),
    function(election) {
        dirs = get_file_names(election, "dirs")
        data_files = get_file_names(election, "data")
        el_date = get_file_names(election, "date")
        empty_ucast = data.frame(districts = integer(0), round = integer(0), voters = integer(0), envel_given = integer(0),
            envel_returned = integer(0), valid_votes = integer(0), lau2 = character(0), stringsAsFactors = FALSE)
        empty_df = data.frame(party = integer(0), candidate = character(0), candid_no = integer(0), constituency = integer(0),
            date = character(0), round = integer(0), votes = integer(0), lau2 = character(0), stringsAsFactors = FALSE)
        all_ucast = empty_ucast
        all_df = empty_df
        for (d in 1:length(dirs)) {
            download.file(paste0("http://www.volby.cz/opendata/", dirs[d], "/", data_files[d]), "tmp.zip", "auto")
            unzip("tmp.zip", "set5.xml")
            tmp = xmlParse("set5.xml")
            file.remove(c("tmp.zip", "set5.xml"))

            nspace = c(ns = xmlNamespace(getNodeSet(tmp, "/*")[[1]]))
            row_count = length(getNodeSet(tmp, "/ns:SE_T5/ns:SE_T5_ROW", nspace))
            ucast = empty_ucast
            ucast[1:row_count, ] = rep(NA, ncol(ucast))
            ucast$districts = 1L
            items = c("KOLO", "VOL_SEZNAM", "VYD_OBALKY", "ODEVZ_OBAL", "PL_HL_CELK", "OBEC")
            for (i in 1:length(items))
                ucast[[i + 1]] = xpathSApply(tmp, paste0("/ns:SE_T5/ns:SE_T5_ROW/ns:", items[i]), xmlValue, namespaces = nspace)

            df = empty_df
            okrsek = getNodeSet(tmp, "/ns:SE_T5/ns:SE_T5_ROW", nspace)[[1]]
            hlasy = names(okrsek)[gsub("HLASY", "", names(okrsek)) != names(okrsek)] # including zeros not assigned to any candidate
            hlasy_num = gsub("[a-zA-Z_]", "", hlasy)
            hlasy_count = length(hlasy)
            df[1:(row_count * length(hlasy)), ] = rep(NA, ncol(df))
            df$date = el_date[d]

            items = c("OBVOD", "KOLO", "OBEC", hlasy)
            tmp_df = as.data.frame(matrix(nrow = row_count, ncol = length(items)))
            for (i in 1:length(items))
                tmp_df[[i]] = xpathSApply(tmp, paste0("/ns:SE_T5/ns:SE_T5_ROW/ns:", items[i]), xmlValue, namespaces = nspace)

            df$constituency = rep(tmp_df[[1]], each = hlasy_count) # faster to fill the columns separately
            df$round = rep(tmp_df[[2]], each = hlasy_count)
            df$lau2 = rep(tmp_df[[3]], each = hlasy_count)
            df$candid_no = rep(hlasy_num, nrow(df) / hlasy_count)
            df$votes = c(t(as.matrix(tmp_df[, 4:ncol(tmp_df)])))
            df = df[df$votes != "0", ]

            # sums for municipalities (data are for districts)
            sum_ucast = empty_ucast
            sum_df = empty_df
            ucast[, names(ucast) != "lau2"] = sapply(ucast[, names(ucast) != "lau2"], as.integer)
            sum_ucast = aggregate(cbind(districts, voters, envel_given, envel_returned, valid_votes) ~ round + lau2, ucast, sum)
            sum_ucast = sum_ucast[, c(3, 1, 4:7, 2)]

            df$votes = as.integer(df$votes)
            sum_df = aggregate(votes ~ candid_no + constituency + date + round + lau2, df, sum)
            sum_df = cbind(rep(NA, nrow(sum_df)), rep(NA, nrow(sum_df)), sum_df[, c(1:4, 6, 5)])
            names(sum_df)[1:2] = names(df)[1:2]

            all_ucast = rbind(all_ucast, sum_ucast)
            all_df = rbind(all_df, sum_df)
        }
        rownames(all_ucast) = NULL
        return(list(df = all_df, ucast = all_ucast))
    }
)

okresy = read.table("data-raw/okresy_lau1.txt", sep = "\t", header = TRUE)
okresy_kz = read.table("data-raw/okresy_kz.txt", sep = "\t", header = TRUE)
kraje = read.table("data-raw/kraje_nuts3.txt", sep = "\t", header = TRUE)

setGeneric("get_okres_ids", function(election) {
    standardGeneric("get_okres_ids")
})

setMethod("get_okres_ids", c("election"),
    function(election) {
        return(okresy$nuts)
    }
)

setMethod("get_okres_ids", c("election_prez"),
    function(election) {
        return(kraje$nuts)
    }
)

# CZ06XX need to be decreased for some elections
decrease_cz06 = function(okresy) {
    num_okresy = as.numeric(gsub("[a-zA-Z_]", "", okresy))
    pos_decrease = okresy[num_okresy > 630 & num_okresy < 650]
    new_ids = paste0("CZ0", num_okresy[pos_decrease] - 20)
    levels(okresy) = c(levels(okresy), new_ids)
    okresy[pos_decrease] = new_ids
    return(okresy)
}

setMethod("get_okres_ids", c("election_ps"),
    function(election) {
        okresy = okresy$nuts
        if (election@name == "ps2006") {
            levels(okresy)[which(levels(okresy) == "CZ0100")] = "CZ0101"
            okresy = decrease_cz06(okresy)
        }
        return(okresy)
    }
)

setMethod("get_okres_ids", c("election_kz"),
    function(election) {
        return(okresy_kz$no)
    }
)

setMethod("get_okres_ids", c("election_ep"),
    function(election) {
        okresy = okresy$nuts
        if (election@name == "ep2004") {
            okresy = decrease_cz06(okresy)
        }
        return(okresy)
    }
)

setGeneric("get_url_for_okres", function(election, okres_id) {
    standardGeneric("get_url_for_okres")
})

setMethod("get_url_for_okres", c("election_ps"),
    function(election, okres_id) {
        return(paste0("/vysledky_okres?nuts=", okres_id))
    }
)

setMethod("get_url_for_okres", c("election_kz"),
    function(election, okres_id) {
        return(paste0("/vysledky_okres?cislo_okresu=", okres_id))
    }
)

setMethod("get_url_for_okres", c("election_kv"),
    function(election, okres_id) {
        if (election@name == "kv2010")
            return(paste0("/vysledky_obce_okres?datumvoleb=20101015&nuts=", okres_id))
        else if (election@name == "kv2014")
            return(paste0("/vysledky_obce_okres?datumvoleb=20141010&nuts=", okres_id))
        else
            stop("Invalid election name.")
    }
)

setMethod("get_url_for_okres", c("election_ep"),
    function(election, okres_id) {
        return(paste0("/vysledky_okres?nuts=", okres_id))
    }
)

# final edits on results - column types, assigning party
setGeneric("party_to_results", function(election, df, parties) {
    standardGeneric("party_to_results")
})

setMethod("party_to_results", c("election"),
    function(election, df, parties) {
        df$party = as.factor(df$party)
        df$votes = as.integer(df$votes)
        df$lau2 = as.factor(df$lau2)
        levels(df$party) = sapply(levels(df$party), function(x) { parties$abbr[which(x == parties$no)] })
        return(df)
    }
)

setMethod("party_to_results", c("election_prez"),
    function(election, df, parties) {
        df[, c("party", "candidate", "candid_no", "round", "lau2")] = lapply(df[, c("party", "candidate", "candid_no", "round", "lau2")], as.factor)
        df$votes = as.integer(df$votes)
        df$party = df$candidate = df$candid_no
        for (col in c("party", "candidate"))
            levels(df[[col]]) = sapply(levels(df$candid_no), function(x) { parties[[ifelse(col == "party", "abbr", "candidate")]][which(x == parties$candid_no)] })
        df$candid_no = NULL
        return(df)
    }
)

setMethod("party_to_results", c("election_se"),
    function(election, df) {
        df$candid_no = as.integer(df$candid_no)
        parties$candid_no = as.integer(parties$candid_no)
        party_pos = sapply(1:length(df$party), function(i, df, par) {
            which(par$date == df$date[i] & par$candid_no == df$candid_no[i] & par$constituency == df$constituency[i]) },
            df, parties)
        df$candid_no = NULL
        party_pos = sapply(party_pos, function(i) { if (length(i) > 0) return(i) else return(NA)}) # integer(0) to NA
        df[, c("party", "candidate")] = parties[party_pos, c("abbr", "candidate")]
        if (sum(df$votes[which(is.na(party_pos))]) != 0)
            stop(paste0(election@name, ": some votes that cannot be assigned to candidates."))
        df[, c("party", "candidate", "constituency", "date", "round", "lau2")] = lapply(df[, c("party", "candidate", "constituency", "date", "round", "lau2")], as.factor)
        return(df)
    }
)
