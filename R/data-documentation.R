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


#' Pirandello - Six Characters in Search of an Author 
#' 
#' This dataset contains the dialogues of the 1921 play Six Characters in Search of an Author by the Italian writer (and Nobel laureate) Luigi Pirandello. The Spanish version contained here was translated in 1926 by Félix Azzati and is currently available at Wikipedia.
#' 
#' @format a [data.frame] object with the following variables:
#' * `Order` numeric; The order of the speech in the play.
#' * `Personaje` character; The character responsible for the speech.
#' * `Action` character; Description of actions, emotions or gestures of the character immediately before his or her speech.
#' * `text` character; The text of the speech.
#' @examples
#' # some operations on the corpus
#' summary(Pirandello)
#' @keywords data
#' @source Wikipedia: https://es.wikisource.org/wiki/Seis_personajes_en_busca_de_autor.
#' @examples Pirandello
"Pirandello"


#' dic.pol.es - Spanish Political Discourses Dictionary 
#' 
#' This dictionary contains a nested (two-level) set of codes for analyzing Spanish inaugural speeches.
#' 
#' @format a quanteda [dictionary] object with the following levels:
#' * `level1` character; Broad political issues.
#' * `level2` character; Specific codes.
#' * `keywords` character; Terms to be employed in the search for each code/issue.
#' @examples
#' # some operations on the corpus
#' summary(Pirandello)
#' @keywords dictionary
#' @source Own elaboration.
#' @examples dic.pol.es
"dic.pol.es"


#' Legislative Interventions on the 14th Legislature of the Spanish Congress (2019-2023)
#' 
#' This dataset contains all interventions by lower-chamber representatives in the Spanish Congress (Congreso de los diputados) during the 14th legislature (2019-2023). Each observation corresponds to an intervention.
#' 
#' @format a data.frame object with the following variables:
#' * `leg.number` integer; The number of the legislature.
#' * `session.date` date; Date when the session was carried out.
#' * `session.type` character; The type of session: "Plenaria" (floor) or "Diputación Permanente".
#' * `session.number` integer; A sequential number assigned to identify the session.
#' * `issue.type` character; The type of issues being discussed during proceedings (voting, debate, questioning and control).
#' * `issue.details` character; Details about the issues or the questions addressed in the intervention of the representative.
#' * `speech.order` integer; The number indicating the sequence of intervention in each session.
#' * `speech.text` character; The complete text of the intervention.
#' * `rep.name` character; The full name of the representative doing the intervention.
#' * `rep.district` character; The electoral district of the representative.
#' * `rep.party` character; The electoral party of the representative.
#' * `rep.group` character; The parliamentary group of the representative.
#' * `rep.condition` character; The "place of speech" of the representative (as a representative, a candidate, a state minister, as the president, as a member of the parliament board).
#' * `rep.institution` character; The institution the representative belongs (the lower chamber, a particular ministry, the presidential office).
#' * `speech.tokens` integer; The number of tokens of the speech.
#' @examples
#' # some operations on the corpus
#' summary(spa.sessions)
#' @keywords data.frame, Spanish Congress
#' @source Own elaboration based on the legislative diaries provided by the Congreso de los Diputados (www.congreso.es).
#' @examples spa.sessions
"spa.sessions"

