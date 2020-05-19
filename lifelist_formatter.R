suppressMessages(library(tidyverse))
suppressMessages(library(vwr))

args = commandArgs(trailingOnly=TRUE)
if(is.na(args[1])) stop('Supply path to lifelist plz.')

# args = c(ll_path='~/Downloads/Alice_Pokedex.csv')

#get lifelist
ll = suppressMessages(read_csv(args[1]))

#get ebird data
message('Getting eBird data')
req = httr::GET(paste0('http://ebird.org/ws1.1/ref/taxa/ebird?cat=species,',
    'hybrid&fmt=json&locale=en_US'))
txt = httr::content(req, as="text")
ebrd = jsonlite::fromJSON(txt, simplifyDataFrame=TRUE, flatten=TRUE)

format_lifelist = function(ll){

    renames = c()
    for(i in 1:nrow(ll)){

        cn = ll$common_name[i]
        if(! cn %in% ebrd$comName){

            ldists = levenshtein.distance(cn, ebrd$comName)
            closest = names(ldists[order(ldists)][1])
            ll$common_name[i] = closest
            renames = append(renames, cn)
            names(renames)[length(renames)] = closest
        }
    }

    ll = ll %>%
        select(common_name, date, location, notes) %>%
        rename(comName=common_name) %>%
        distinct(comName, .keep_all=TRUE)

    ll = ebrd %>%
        select(sciName, comName, speciesCode, familyComName,
            familySciName) %>%
        right_join(ll, by='comName') %>%
        arrange(familySciName, sciName) %>%
        select(comName, sciName, familySciName, familyComName, speciesCode,
            date, location, notes)

    write.csv(ll, args[1], row.names=FALSE)

    cnt = sum(!is.na(ll$sciName))
    cat(paste0('\nSpecies count: ', cnt, '\n'))

    message('Renamed the following:\n')
    for(j in 1:length(renames)){
        cat(paste0(unname(renames[j]), ' -> ', names(renames[j]), '\n'))
    }
}

format_lifelist(ll)
