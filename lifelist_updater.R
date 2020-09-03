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

get_response_1char = function(msg, possible_chars){

    #msg is a message that will be used to prompt the user
    #possible_chars is a character vector of single-character responses

    message(msg)
    ch = readLines(con="stdin", 1)

    if(length(ch) == 1 && ch %in% possible_chars){
        return(ch)
    } else {
        get_response_1char(msg, possible_chars)
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
    # lookup = 'ruffed grouse'

    if(lookup == 'q'){
        message('l8z')
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
        # yn = get_yn('\nReplace entry? y, n\n')
        r = get_response_1char('\nUpdate: (l)ocation, (d)ate, (N)ote, (a)ll, (n)one\n',
            c('l', 'd', 'N', 'a', 'n'))
        location_needed = date_needed = notes_needed = FALSE
        if(r == 'l'){
            date = existing_row$date
            notes = existing_row$notes
            ll = slice(ll, -existing_row_ind)
            location_needed = TRUE
        } else if(r == 'd'){
            location = existing_row$location
            notes = existing_row$notes
            ll = slice(ll, -existing_row_ind)
            date_needed = TRUE
        } else if(r == 'N'){
            date = existing_row$date
            location = existing_row$location
            ll = slice(ll, -existing_row_ind)
            notes_needed = TRUE
        } else if(r == 'a'){
            ll = slice(ll, -existing_row_ind)
            location_needed = date_needed = notes_needed = TRUE
        } else if(r == 'n'){
            update_lifelist()
        }
    }

    # if(! lookup %in% ebrd$comName){
    #     message('Name not in eBird database as received.')
    #     update_lifelist()
    # }

    if(location_needed){
        if(exists('previous_location') && nchar(previous_location)){
            cat('Location ([Enter] accepts previous: ',
                previous_location, ') > ',
                sep='')
        } else {
            cat('Location > ')
        }
        location = readLines(con="stdin", 1)
        if(! nchar(location)){
            location = previous_location
            cat('Using previous: ', previous_location, '\n', sep='')
        } else {
            previous_location <<- location
        }
    }

    if(date_needed){
        if(exists('previous_date') && nchar(previous_date)){
            cat('Date ([Enter] accepts previous: ',
                previous_date, ') > ',
                sep='')
        } else {
            cat('Date as "YYYY-MM-DD" > ')
        }
        date = readLines(con="stdin", 1)
        if(! nchar(date)){
            date = previous_date
            cat('Using previous: ', previous_date, '\n', sep='')
        } else {
            previous_date <<- date
        }
    }

    if(notes_needed){
        cat('Enter notes > ')
        notes = readLines(con="stdin", 1)
    }

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

previous_location = ''
previous_date = ''

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


