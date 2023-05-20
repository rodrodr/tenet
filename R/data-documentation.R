#' Brazilian post-1984 Fictional Cinema 
#' 
#' This dataset contains 260 Brazilian fictional films from 1985 to 2023.
#' 
#' @format a [corpus] object with the following docvars:
#' * `primaryTitle` character; Main title
#' * `Director` character; Names of the directors
#' * `Country` character; Countries involved in the production
#' * `imdbRating` numeric; IMDB Rating of the film
#' * `Poster` character; URL to the Film's poster
#' * `Decade` character; The decade when the film was released
#' @examples
#' # some operations on the corpus
#' summary(br.films)
#' head(quanteda::docvars(br.films), 10)
#' @keywords data
#' @source OMDB.
#' @examples br.films
"br.films"


#' Spanish Presidents' Inaugural Speeches 
#' 
#' This dataset contains 15 Inaugural Speeches from 1979 to 2020.
#' 
#' @format a [data.frame] object with the following variables:
#' * `doc_id` character; Document ID
#' * `text` character; Text of the inaugural speech
#' * `President` character; Name of the President who gave the speech
#' * `Legislature` numeric; The number of the legislature
#' @examples
#' # some operations on the corpus
#' summary(spa.inaugural)
#' @keywords data
#' @source Moncloa.
#' @examples spa.inaugural
"spa.inaugural"


#' Brazilian Presidents' Inaugural Speeches 
#' 
#' This dataset contains 37 Inaugural Speeches from 1889 to 2023.
#' 
#' @format a quanteda [corpus] object with the following variables:
#' * `doc_id` character; Document ID
#' * `text` character; Text of the inaugural speech
#' * `President` character; Name of the President who gave the speech
#' * `Legislature` numeric; The number of the legislature
#' * `Date` date; Date of the Speech            
#' * `Military` logical; Military President      
#' * `Party` character; Political Party of the President         
#' * `Interrupted` logical; Was the administration interrupted?    
#' * `Interrupt.Cause` character; Cause of the interruption
#' * `link_photo` character; URL for the photos of the President 
#' @examples
#' # some operations on the corpus
#' summary(bra.inaugural)
#' @keywords data
#' @source Presidencia.gov.br and Bonfim (2005).
#' @examples bra.inaugural
"bra.inaugural"


#' Political Corruption in Spain 
#' 
#' This dataset contains the transcription of 7 focus groups on political corruption in Spain. The study was performed by the Centro de Investigaciones Sociológicas (CIS) under the number E2863 in March, 2011. 
#' 
#' @format a data.frame object with the following variables:
#' * `Persona` character; The person speaking.
#' * `text` character; Text containing the intervention of each participant.
#' * `Order` integer; Order in the overall interventions.
#' * `Tipo.Persona` character; Tipe of participant: Moderator, Female, or Male. 
#' * `Estudio` character; Study number: E2863.            
#' * `Grupo.Discusion` integer; Number of the focus group.      
#' * `Ciudad` character; Name of the city where the focus group took place.         
#' * `Fecha` date; Date of the focus group.  
#' * `Grupo.Demografico` character; Name of the demographic group: "Housewives", "Businessmen","University Students", "Public Servants", "Occupied - Young", "Workers", and "Liberal Professionals". 
#' * `Desc.Grupo` character; Detailed description of each demographic group.
#' * `Edad` character; Age interval of participants.
#' @examples
#' # some operations on the data
#' summary(cis.corrupt)
#' @keywords data
#' @source CIS (2011).
#' @examples cis.corrupt
"cis.corrupt"
