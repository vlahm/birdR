suppressMessages(library(tidyverse))
suppressMessages(library(plyr))
suppressMessages(library(vwr))

#this processing sequence uses lifelist_srs.csv, which was originally
#generated from "Life List.doc", Manual modifications have been made since then.

#TODO: have it check all common names against the database each time.
#if NAs result, that indicates that a species has been split/merged
#spit out the row after a successful update
#give nearby names
#lookup by a vector of common names
#lookup by a dataframe with a column of common names
#same for species names, etc
#look up obsolete names and display current ones


args = commandArgs(trailingOnly=TRUE)
# args = c(ll_path='~/Desktop/stuff_2/Databases/lifelist_srs.csv')

#get ebird data
message('Getting eBird data')
req = httr::GET(paste0('http://ebird.org/ws1.1/ref/taxa/ebird?cat=species,',
    'hybrid&fmt=json&locale=en_US'))
txt = httr::content(req, as="text")
ebrd = jsonlite::fromJSON(txt, simplifyDataFrame=TRUE, flatten=TRUE)

# lookup = 'prairie warbler'
# matchnum=1
# location='Sandhills Gamelands, NC'; date='2020-05-10'
# notes='upward rising insect trill, like pine warb'
get_user_matchnum = function(matches){
    message('\nWhich did you mean?\n', paste(matches, collapse='\n'), '\n')
    matchnum = readLines(con="stdin", 1)
}

get_yn = function(msg){

    message(msg)
    yn = readLines(con="stdin", 1)

    if(length(yn) == 1 && yn %in% c('y', 'n')){
        return(yn)
    } else {
        get_yn(msg)
    }
}

resolve_discrepancies = function(){

    #incomplete. needs work all around

    message('Checking for discrepancies')

    discreps = c()
    for(i in 1:nrow(ll)){
        ll_row = ll %>%
            select(-date, -location, -notes) %>%
            slice(i)
        mch = suppressMessages(match_df(ll_row, ebrd))
        if(! nrow(mch)) discreps = append(discreps, ll_row$comName)
    }

    ebrd_discreps = ebrd %>%
        select(sciName, comName, speciesCode, familyComName, familySciName) %>%
        filter(comName %in% discreps)

    for(i in 1:nrow(ll)){
        ll_row = ll %>%
            select(-date, -location, -notes) %>%
            slice(i)
        discrep = filter(ebrd_discreps, comName == ll_row$comName)
        ll_row

    }

    message('Discrepancies detected: ')

    unaccounted_for = discreps[! discreps %in% ebrd_discreps$comName]

    message(paste0('These species are no longer recognized: ',
                   paste(unaccounted_for, collapse=', ')))

}

update_lifelist = function(){

    ll = read.csv(args[1], stringsAsFactors=FALSE)

    # lookup = readline('Enter common name as e.g. Adj-noun Adj-Noun (no quotes) > ')
    cat('\nEnter common name (or c to check for discrepancies; q to quit)>\n')
    lookup = readLines(con="stdin", 1)
    # lookup = 'red throated loon'

    if(lookup == 'q'){
        message('later')
        stop()
    }
    if(lookup == 'c'){
        message("Sry. Can't do that yet.")
        update_lifelist()
        # resolve_discrepancies()
    }

    if(! lookup %in% ebrd$comName){

        ldists = levenshtein.distance(lookup, ebrd$comName)
        closest3 = paste(c(1:3, 0),
            c(names(ldists[order(ldists)][1:3]), 'none'))

        matchnum = get_user_matchnum(closest3)
        while(! matchnum %in% 0:4){
            matchnum = get_user_matchnum(closest3)
        }

        if(matchnum == 0){
            update_lifelist()
        }

        enum_match = closest3[as.numeric(matchnum)]
        lookup = substr(enum_match, 3, nchar(enum_match))
    }

    if(lookup %in% ll$comName){
        message('Yo, that shit is already in ur list:')
        existing_row_ind = which(ll$comName == lookup)
        existing_row = ll[existing_row_ind, ]
        cat(paste0('\n', paste(colnames(existing_row),
            unname(unlist(existing_row)), sep=': ', collapse='\n'),
            '\n'))
        yn = get_yn('\nReplace entry? y, n\n')
        if(yn == 'n') update_lifelist()
        ll = slice(ll, -existing_row_ind)
    }

    # if(! lookup %in% ebrd$comName){
    #     message('Name not in eBird database as received.')
    #     update_lifelist()
    # }

    cat('Enter location > ')
    location = readLines(con="stdin", 1)
    cat('Enter date as "YYYY-MM-DD" > ')
    date = readLines(con="stdin", 1)
    cat('Enter notes > ')
    notes = readLines(con="stdin", 1)

    newrow = data.frame(lookup=lookup, date=date, location=location,
        notes=notes, stringsAsFactors=FALSE)

    newrow = ebrd %>%
        select(sciName, comName, speciesCode, familyComName, familySciName) %>%
        # mutate_all(as.character) %>%
        filter(comName == lookup) %>%
        bind_cols(newrow) %>%
        select(-lookup)

    ll = bind_rows(newrow, ll) %>%
        arrange(familySciName, sciName) %>%
        select(comName, sciName, familySciName, familyComName, speciesCode,
            date, location, notes)

    write.csv(ll, args[1], row.names=FALSE)

    cnt = sum(!is.na(ll$sciName))

    # message('Success!')
    cat(paste0('\n', paste(colnames(newrow),
        unname(unlist(newrow)), sep=': ', collapse='\n'),
        '\n'))
    cat(paste0('\nSpecies count: ', cnt, '\n'))

    update_lifelist()
}

empty = tryCatch({
    empty = update_lifelist()
}, error = function(e){
    return()
})

# #correct hyphenated modifiers in common names in lifelist to X-x format
# ll$comName = sub('\\-([A-Z])', '-\\L\\1', ll$comName, perl=TRUE)
# ll$comName[ll$comName == 'Slaty-backed Nightingale-thrush'] =
#     'Slaty-backed Nightingale-Thrush'
# ll$comName[ll$comName == 'Eastern Screech-owl'] =
#     'Eastern Screech-Owl'
# ll$comName[ll$comName == 'California Scrub-jay'] =
#     'California Scrub-Jay'
# ll$comName[ll$comName == 'Malaysian Pied-fantail'] =
#     'Malaysian Pied-Fantail'
# ll$comName[ll$comName == 'Eastern Wood-pewee'] =
#     'Eastern Wood-Pewee'
#
# ll2 = ebrd %>%
#     select(sciName, comName, speciesCode, familyComName, familySciName) %>%
#     right_join(ll, by='comName') %>%
#     arrange(familySciName, sciName) %>%
#     select(comName, sciName, date, location, familySciName, familyComName,
#         speciesCode, notes)
#
# ll2[which(is.na(ll2$sciName)),]
# ebrd$comName[grep("Malaysian", ebrd$comName)]
#
# write.csv(ll2, '~/Desktop/stuff_2/Databases/lifelist_srs.csv', row.names=FALSE)


