#' Czech Elections Data for Municipalities
#'
#' Datasets of votes in various election types in the Czech Republic, aggregated for municipalities.
#'
#' Instead of showing election results in terms of seats won, raw data of election votes are
#' provided for each municipality, allowing to perform own analyses and simulations based on them.
#' Only a subset of election data for the independent Czech Republic is provided, beginning between
#' 2004 and 2010 (depending on election type). Note that the data are not comprehensive since votes
#' given in foreign countries and votes in additional municipal elections are not included.
#'
#' Two data frames correspond with each election: votes (e.g. \code{\link{ps2013}}) and turnout data
#' (e.g. \code{\link{ps2013_turnout}}). The names contain an abbreviation of election type as given
#' by CZSO: \dQuote{ep} (European Parliament), \dQuote{kv} (municipal), \dQuote{kz} (regional),
#' \dQuote{prez} (presidential), '\dQuote{ps} (Chamber of Deputies) or '\dQuote{se} (Senate).
#'
#' There is also an overview of municipalities (\code{\link{obce}} data frame).
#'
#' Elections data use data published by the Czech Statistical Office (CZSO) as opendata:
#' \url{http://volby.cz/opendata/opendata.htm}. They are available under the conditions listed here:
#' \url{https://www.czso.cz/csu/czso/conditions_for_use_and_further_dissemination}
#'
#' List of municipalities uses data from Wikidata (www.wikidata.org), a free database that anyone can edit.
#'
#' @docType package
#' @name czelections
NULL

#' Municipalities of the Czech Republic
#'
#' Dataset describing municipalities in the Czech Republic. The list of municipalities is valid
#' as of the 1st January 2015.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{lau2}{local administrative unit of the second level (LAU2), an identifier of municipality, unique only within the Czech Republic}
#'   \item{name}{municipality name (with district name if needed to distinguish between the same names)}
#'   \item{latitude}{municipality latitude in degrees}
#'   \item{longitude}{municipality longitude in degrees}
#'   \item{lau1}{district (local administrative unit of the first level, LAU1) which municipality belongs to}
#'   \item{nuts3}{region (NUTS level 3) which municipality belongs to}
#'   \item{constituency}{identification number of constituency for Senate elections which municipality currently belongs to. (Note that
#'     constituency borders are adjusted according to changes in population.) NA for municipalities that includes more than one constituency
#'     (Praha, Brno, Ostrava and Plzeň).}
#'   \item{wikidata}{identifier of Wikidata item for municipality}
#' }
#' @source \url{https://www.czso.cz/csu/czso/ciselnik_obci_-cisob-} (LAU2 list), \url{https://www.wikidata.org/} (other properties)
"obce"

#' European Parliament Election Votes in the Czech Republic
#'
#' Dataset containing votes for parties aggregated by municipalities.
#'
#' @format A data frame with 3 columns:
#' \describe{
#'   \item{party}{abbreviation of name of voted party}
#'   \item{votes}{number of votes given}
#'   \item{lau2}{municipality identifier (LAU2)}
#' }
#' @source \url{http://volby.cz/opendata/opendata.htm}
#' @aliases ep2009 ep2014
"ep2004"

#' @name ep2004
#' @rdname ep2004
"ep2009"

#' @name ep2004
#' @rdname ep2004
"ep2014"

#' European Parliament Election Turnout in the Czech Republic
#'
#' Election turnout data aggregated by municipalites.
#'
#' @format A data frame with 6 columns:
#' \describe{
#'   \item{districts}{number of polling places within the municipality}
#'   \item{voters}{number of all eligible voters}
#'   \item{envel_given}{number of envelopes given to voters}
#'   \item{envel_returned}{number of envelopes put into ballot box by voters}
#'   \item{valid_votes}{number of valid votes as counted from ballot box}
#'   \item{lau2}{municipality identifier (LAU2)}
#' }
#' @source \url{http://volby.cz/opendata/opendata.htm}
#' @aliases ep2009_turnout ep2014_turnout
"ep2004_turnout"

#' @name ep2004_turnout
#' @rdname ep2004_turnout
"ep2009_turnout"

#' @name ep2004_turnout
#' @rdname ep2004_turnout
"ep2014_turnout"

#' Municipal Election Votes in the Czech Republic
#'
#' Dataset containing votes for parties aggregated by municipalities.
#'
#' @format A data frame with 3 columns:
#' \describe{
#'   \item{party}{abbreviation of name of voted party; special values are \dQuote{NK} (\dQuote{nezávislý kandidát}, independent candidate)}
#'     and \dQuote{SNK} (\dQuote{sdružení nezávislých kandidátů}, union of independents)
#'   \item{votes}{number of votes given}
#'   \item{lau2}{municipality identifier (LAU2)}
#' }
#' @source \url{http://volby.cz/opendata/opendata.htm}
#' @aliases kv2014
"kv2010"

#' @name kv2010
#' @rdname kv2010
"kv2014"

#' Municipal Election Turnout in the Czech Republic
#'
#' Election turnout data aggregated by municipalites.
#'
#' @format A data frame with 7 columns:
#' \describe{
#'   \item{districts}{number of polling places within the municipality}
#'   \item{voters}{number of all eligible voters}
#'   \item{envel_given}{number of envelopes given to voters}
#'   \item{envel_returned}{number of envelopes put into ballot box by voters}
#'   \item{valid_votes}{number of valid votes as counted from ballot box}
#'   \item{lau2}{municipality identifier (LAU2)}
#'   \item{seats}{number of seats to be won in the municipality}
#' }
#' @source \url{http://volby.cz/opendata/opendata.htm}
#' @aliases kv2014_turnout
"kv2010_turnout"

#' @name kv2010_turnout
#' @rdname kv2010_turnout
"kv2014_turnout"

#' Regional Election Votes in the Czech Republic
#'
#' Dataset containing votes for parties aggregated by municipalities.
#'
#' @format A data frame with 3 columns:
#' \describe{
#'   \item{party}{abbreviation of name of voted party}
#'   \item{votes}{number of votes given}
#'   \item{lau2}{municipality identifier (LAU2)}
#' }
#' @source \url{http://volby.cz/opendata/opendata.htm}
#' @aliases kz2012
"kz2008"

#' @name kz2008
#' @rdname kz2008
"kz2012"

#' Regional Election Turnout in the Czech Republic
#'
#' Election turnout data aggregated by municipalites.
#'
#' @format A data frame with 6 columns:
#' \describe{
#'   \item{districts}{number of polling places within the municipality}
#'   \item{voters}{number of all eligible voters}
#'   \item{envel_given}{number of envelopes given to voters}
#'   \item{envel_returned}{number of envelopes put into ballot box by voters}
#'   \item{valid_votes}{number of valid votes as counted from ballot box}
#'   \item{lau2}{municipality identifier (LAU2)}
#' }
#' @source \url{http://volby.cz/opendata/opendata.htm}
#' @aliases kz2012_turnout
"kz2008_turnout"

#' @name kz2008_turnout
#' @rdname kz2008_turnout
"kz2012_turnout"

#' Presidential Election Votes in the Czech Republic
#'
#' Dataset containing votes for presidential candidates aggregated by municipalities.
#'
#' @format A data frame with 5 columns:
#' \describe{
#'   \item{party}{abbreviation of name of party of which the candidate is member; special value \dQuote{BEZPP} (\dQuote{bez politické příslušnosti})
#'     means independent candidate}
#'   \item{candidate}{name of the candidate}
#'   \item{round}{election round}
#'   \item{votes}{number of votes given}
#'   \item{lau2}{municipality identifier (LAU2)}
#' }
#' @source \url{http://volby.cz/opendata/opendata.htm}
"prez2013"

#' Presidential Election Turnout in the Czech Republic
#'
#' Election turnout data aggregated by municipalites.
#'
#' @format A data frame with 7 columns:
#' \describe{
#'   \item{districts}{number of polling places within the municipality}
#'   \item{round}{election round}
#'   \item{voters}{number of all eligible voters}
#'   \item{envel_given}{number of envelopes given to voters}
#'   \item{envel_returned}{number of envelopes put into ballot box by voters}
#'   \item{valid_votes}{number of valid votes as counted from ballot box}
#'   \item{lau2}{municipality identifier (LAU2)}
#' }
#' @source \url{http://volby.cz/opendata/opendata.htm}
"prez2013_turnout"

#' Chamber of Deputies Election Votes in the Czech Republic
#'
#' Dataset containing votes for parties aggregated by municipalities.
#'
#' @format A data frame with 3 columns:
#' \describe{
#'   \item{party}{abbreviation of name of voted party}
#'   \item{votes}{number of votes given}
#'   \item{lau2}{municipality identifier (LAU2)}
#' }
#' @source \url{http://volby.cz/opendata/opendata.htm}
#' @aliases ps2010 ps2013
"ps2006"

#' @name ps2006
#' @rdname ps2006
"ps2010"

#' @name ps2006
#' @rdname ps2006
"ps2013"

#' Chamber of Deputies Election Turnout in the Czech Republic
#'
#' Election turnout data aggregated by municipalites.
#'
#' @format A data frame with 6 columns:
#' \describe{
#'   \item{districts}{number of polling places within the municipality}
#'   \item{voters}{number of all eligible voters}
#'   \item{envel_given}{number of envelopes given to voters}
#'   \item{envel_returned}{number of envelopes put into ballot box by voters}
#'   \item{valid_votes}{number of valid votes as counted from ballot box}
#'   \item{lau2}{municipality identifier (LAU2)}
#' }
#' @source \url{http://volby.cz/opendata/opendata.htm}
#' @aliases ps2010_turnout ps2013_turnout
"ps2006_turnout"

#' @name ps2006_turnout
#' @rdname ps2006_turnout
"ps2010_turnout"

#' @name ps2006_turnout
#' @rdname ps2006_turnout
"ps2013_turnout"

#' Senate Election Votes in the Czech Republic
#'
#' Dataset containing votes for candidates aggregated by municipalities or their parts.
#'
#' @format A data frame with 7 columns:
#' \describe{
#'   \item{party}{abbreviation of name of party that nominated the candidate}
#'   \item{candidate}{name of the candidate}
#'   \item{constituency}{identification number of constituency}
#'   \item{date}{election date; note that more election are held within a yeare in case of by-election}
#'   \item{round}{election round}
#'   \item{votes}{number of votes given}
#'   \item{lau2}{municipality identifier, usually LAU2. In case of municipality that includes more than one constituency
#'     (Praha, Brno, Ostrava and Plzeň), a similar code of municipal district is used instead.}
#' }
#' @source \url{http://volby.cz/opendata/opendata.htm}
#' @aliases se2010 se2011 se2012 se2014
"se2008"

#' @name se2008
#' @rdname se2008
"se2010"

#' @name se2008
#' @rdname se2008
"se2011"

#' @name se2008
#' @rdname se2008
"se2012"

#' @name se2008
#' @rdname se2008
"se2014"

#' Senate Election Turnout in the Czech Republic
#'
#' Election turnout data aggregated by municipalites or their parts.
#'
#' @format A data frame with 7 columns:
#' \describe{
#'   \item{districts}{number of polling places within the municipality}
#'   \item{round}{election round}
#'   \item{voters}{number of all eligible voters}
#'   \item{envel_given}{number of envelopes given to voters}
#'   \item{envel_returned}{number of envelopes put into ballot box by voters}
#'   \item{valid_votes}{number of valid votes as counted from ballot box}
#'   \item{lau2}{municipality identifier, usually LAU2. In case of municipality that includes more than one constituency
#'     (Praha, Brno, Ostrava and Plzeň), a similar code of municipal district is used instead.}
#' }
#' @source \url{http://volby.cz/opendata/opendata.htm}
#' @aliases se2010_turnout se2011_turnout se2012_turnout se2014_turnout
"se2008_turnout"

#' @name se2008_turnout
#' @rdname se2008_turnout
"se2010_turnout"

#' @name se2008_turnout
#' @rdname se2008_turnout
"se2011_turnout"

#' @name se2008_turnout
#' @rdname se2008_turnout
"se2012_turnout"

#' @name se2008_turnout
#' @rdname se2008_turnout
"se2014_turnout"

