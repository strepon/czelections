source("data-raw/elections.r")

elections = c("prez2013", "ps2006", "ps2010", "ps2013", "se2008", "se2010", "se2011", "se2012", "se2014", "kz2008", "kz2012", "kv2010", "kv2014", "ep2004", "ep2009", "ep2014")
reg_archives = c("", "PS2006reg2006.zip", "PS2010reg2010.zip", "PS2013reg20131026.zip", "", "", "", "", "", "kz2008_data_dbf.zip", "kz2012_data_dbf.zip", "KV2010reg20140903.zip", "KV2014reg20141014.zip", "EP2004reg.zip", "EP2009reg.zip", "EP2014reg20140521.zip")

reg_files = c(ep = "eprkl.xml", kv = "kvros.xml", kz = "KZRKL.dbf", ps = "PSRKL.dbf")
reg_row_name = c(ep = "EP_RKL_ROW", kv = "KV_ROS_ROW")
reg_items = list(ep = c("ESTRANA", "NAZEVCELK", "ZKRATKAE8"), kv = c("VSTRANA", "NAZEVCELK", "ZKRATKAO8"))
name_resul = c(ep = "VYSLEDKY_OKRES", kz = "VYSLEDKY_OKRES", ps = "VYSLEDKY_OKRES", kv = "VYSLEDKY_OBCE_OKRES")
name_party = c(ep = "ESTRANA", kz = "KSTRANA", ps = "KSTRANA", kv = "VSTRANA")
name_votes = c(ep = "HLASY_STRANA", kz = "HODNOTY_STRANA", ps = "HLASY_STRANA", kv = "VOLEBNI_STRANA")

library(XML)
library(foreign)
for (el in 1:length(elections)) {
    el_type = gsub("\\d", "", elections[el])
    if (el_type == "prez")
        elobj = election_prez(name = elections[el])
    else if (el_type == "ps")
        elobj = election_ps(name = elections[el])
    else if (el_type == "se")
        elobj = election_se(name = elections[el])
    else if (el_type == "kz")
        elobj = election_kz(name = elections[el])
    else if (el_type == "kv")
        elobj = election_kv(name = elections[el])
    else if (el_type == "ep")
        elobj = election_ep(name = elections[el])

    download_file_parties(elobj, reg_archives[el], reg_files[el_type])
    if (el_type == "kz" || el_type == "se" || el_type == "ps" || el_type == "prez") {
        parties = create_parties(elobj)
    }
    else {
        reg = xmlParse(reg_files[el_type])
        nspace = c(ns = xmlNamespace(getNodeSet(reg, "/*")[[1]]))
        rows = getNodeSet(reg, paste0("//ns:", reg_row_name[el_type]), nspace)
        party_count = length(rows)
        nas = rep(NA, party_count)
        parties = data.frame(no = nas, name = nas, abbr = nas)
        for (i in 1:(length(reg_items[[el_type]]) - 1)) {
            parties[[i]] = sapply(getNodeSet(reg, paste0("//ns:", reg_items[[el_type]][i]), nspace), xmlValue)
        }
        # abbreviations
        for (p in 1:party_count) {
            if (p %% 500 == 0)
                cat(paste0("Parties: ", p, "/", party_count, "\n"))
            abbr8 = getNodeSet(rows[[p]], paste0("ns:", reg_items[[el_type]][3]), nspace)
            if (length(abbr8) > 0)
                parties$abbr[p] = xmlValue(abbr8[[1]])
            else # 8-character abbreviation not available
                parties$abbr[p] = xmlValue(getNodeSet(rows[[p]], "ns:ZKRATKAO30", nspace)[[1]])
        }

        for (file in c("tmp.zip", reg_files[el_type])) {
            if (file.exists(file))
                file.remove(file)
        }
    }

    if (el_type == "kv") { # NK, SNK (80, 90) for many full names
        # replace full names given as abbr for 80 or 90
        parties$abbr[parties$no == "80"] = "NK"
        parties$abbr[parties$no == "90"] = "SNK"
        # or for other not unique abbreviations
        unique_no = unique(parties$no)
        for (i in 1:length(unique_no)) {
            if (length(unique(parties$abbr[parties$no == unique_no[i]])) > 1) {
                parties$abbr[parties$no == unique_no[i]] = parties$abbr[parties$no == unique_no[i]][1]
            }
        }
        parties$name = NULL
        parties = unique(parties)
    }

    if (el_type == "se" || el_type == "prez") {
        data = create_data(elobj)
        df = data$df
        ucast = data$ucast
    }
    else {
        df = data.frame(party = integer(0), votes = integer(0), lau2 = character(0), stringsAsFactors = FALSE)
        empty_df = df
        ucast = data.frame(districts = integer(0), voters = integer(0), envel_given = integer(0),
            envel_returned = integer(0), valid_votes = integer(0), lau2 = character(0), stringsAsFactors = FALSE)

        if (el_type == "kv")
            ucast = cbind(ucast, seats = integer(0))

        okresy_ids = get_okres_ids(elobj)
        for (ok in okresy_ids) {
            download.file(paste0("http://www.volby.cz/pls/", elections[el], get_url_for_okres(elobj, ok)), "tmp.xml", "auto")
            tmp = xmlParse("tmp.xml")
            nspace = c(ns = xmlNamespace(getNodeSet(tmp, "/*")[[1]]))

            error_node = getNodeSet(tmp, paste0("/ns:", name_resul[el_type], "/ns:CHYBA"), nspace)
            if (length(error_node) > 0)
                stop(paste0("Okres ", ok, ": ", xmlValue(error_node[[1]])))

            if (el_type == "kv") {
                id_attr = "KODZASTUP"
                kv_tmp = "/ns:VYSLEDEK"
            }
            else {
                id_attr = "CIS_OBEC"
                kv_tmp = ""
            }

            tmp_ucast = as.data.frame(xpathSApply(tmp, paste0("/ns:", name_resul[el_type], "/ns:OBEC"), xmlGetAttr, id_attr, namespaces = nspace))
            for (attr in c("OKRSKY_CELKEM", "ZAPSANI_VOLICI", "VYDANE_OBALKY", "ODEVZDANE_OBALKY", "PLATNE_HLASY"))
                tmp_ucast = cbind(tmp_ucast, xpathSApply(tmp, paste0("/ns:", name_resul[el_type], "/ns:OBEC", kv_tmp, "/ns:UCAST"), xmlGetAttr, attr, namespaces = nspace))
            tmp_ucast = tmp_ucast[, c(2:ncol(tmp_ucast), 1)]
            if (el_type == "kv") {
                tmp_ucast = cbind(tmp_ucast, seats = xpathSApply(tmp, paste0("/ns:", name_resul[el_type], "/ns:OBEC"), xmlGetAttr, "VOLENO_ZASTUP", namespaces = nspace), stringsAsFactors = FALSE)
            }
            colnames(tmp_ucast) = colnames(ucast)
            ucast = rbind(ucast, tmp_ucast)

            obce = getNodeSet(tmp, paste0("/ns:", name_resul[el_type], "/ns:OBEC", kv_tmp), namespaces = nspace)
            hlasy = length(getNodeSet(tmp, paste0("/ns:", name_resul[el_type], "/ns:OBEC", kv_tmp, "/ns:", name_votes[el_type]), , namespaces = nspace))
            tmp_df = rbind(empty_df, data.frame(matrix(NA, nrow = hlasy, ncol = 3)))
            curr_row = 1
            attrs = c(name_party[el_type], "HLASY")
            for (ob in 1:length(obce)) {
                tmp_votes = lapply(attrs, function(attr) { xpathSApply(obce[[ob]], paste0("ns:", name_votes[el_type]), xmlGetAttr, attr, namespaces = nspace) })
                tmp_votes[[3]] = rep(as.character(tmp_ucast$lau2[ob]), length(tmp_votes[[1]]))
                if (length(tmp_votes[[1]]) > 0) { # length 0 for 0% turnout
                    for (col in 1:ncol(tmp_df))
                        tmp_df[curr_row:(curr_row + length(tmp_votes[[1]]) - 1), col] = tmp_votes[[col]]
                    curr_row = curr_row + length(tmp_votes[[1]])
                }
            }
            df = rbind(df, tmp_df)
        }
        if (file.exists("tmp.xml"))
            file.remove("tmp.xml")
        colnames(df) = colnames(empty_df)
        if (elections[el] == "kv2010") { # different numbers in kvros.xml and in data - Krupka?
            df$party[df$party == "361"] = "453"
        }
    }
    df = party_to_results(elobj, df, parties)
    Encoding(levels(df$party)) = "UTF-8"
    if (!is.null(df$candidate))
        Encoding(levels(df$candidate)) = "UTF-8"

    rownames(ucast) = NULL
    ucast[, 1:5] = apply(ucast[, 1:5], 2, as.integer)
    ucast$lau2 = as.factor(ucast$lau2)
    ucast$lau2 = factor(ucast$lau2, levels = sort(levels(ucast$lau2)))
    if (el_type =="kv")
        ucast$seats = as.integer(ucast$seats)
    else if (el_type == "prez" || el_type == "se") {
        ucast$valid_votes = as.integer(ucast$valid_votes)
        ucast$round = as.factor(ucast$round)
    }
    assign(elections[el], df)
    turnout_name = paste(elections[el], "turnout", sep = "_")
    assign(turnout_name, ucast)
    save(list = c(elections[el], turnout_name), file = paste0("data/", elections[el], ".rda"), compress = "xz")
}
