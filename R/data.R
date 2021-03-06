#' English particle verbs
#'
#' Data describing the placement post-verbal particles in nine varieties of English
#' contained in the International Corpus of English (ICE) and the Global Corpus of
#' Web-based English (GloWbE).
#'
#' @format A data frame with 11340 rows and 26 variables:
#' \describe{
#'   \item{Variety}{a factor with 9 levels coding the variety from which the token was taken}
#'   \item{Corpus}{a factor levels \code{GloWbE} and \code{ICE} coding the corpus from which the token was taken}
#'   \item{Genre}{a factor with 14 levels for the genres of texts in the corpora}
#'   \item{Register}{a factor with 5 levels \code{online}, \code{spok.formal}, \code{spok.informal},\code{writ.formal}, \code{writ.informal} coding the register and mode of the text}
#'   \item{Verb}{a factor coding the verb, e.g. "pick"}
#'   \item{Particle}{a factor coding the particle, e.g. "up"}
#'   \item{VerbPart}{a factor coding the verb-particle, e.g. "pick up"}
#'   \item{VerbForm}{a factor coding the inflected form of the verb, e.g. "picks", "picked"}
#'   \item{DirObject}{a character vector coding the entire direct object NP}
#'   \item{DirObjHead}{a character vector coding the head noun of the direct object}
#'   \item{Response}{a factor with levels \code{Continuous} ("pick up the book") and \code{Split} ("pick the book up") coding the placement of the particle}
#'   \item{DirObjWordLength}{a numeric vector coding the number of words in the direct object}
#'   \item{DirObjLettLength}{a numeric vector coding the number of orthographic letters in the direct object}
#'   \item{DirObjExprType}{a factor with 4 levels: \code{iprn} (impersonal pronoun, "someone"); \code{nc} (common noun, "the bar of chocolate"), \code{np} (proper noun, "Mister Eto"); \code{vp} (verbal gerund, "reading The Times")}
#'   \item{DirObjDefiniteness}{a factor with levels \code{def} and \code{indef} coding the definiteness of the direct object}
#'   \item{DirObjGivenness}{a factor with levels \code{given} and \code{new} coding the discourse givenness of the direct object}
#'   \item{DirObjConcreteness}{a factor with levels \code{Concrete} and \code{Nonconcrete} coding the concreteness of the direct object}
#'   \item{Semantics}{a factor with levels \code{compositional} and \code{non-compositional} coding the semantic compositionality of the particle verb token}
#'   \item{DirObjThematicity}{a numeric vector coding the thematicity of the head noun direct object. Measured as the log-transformed normalized text frequency of the head noun.}
#'   \item{DirectionalPP}{a factor with levels \code{no} and \code{yes} coding the presence of a directional prepositional phrase following the VP, e.g. "picking up a big beach ball off the ground"}
#'   \item{PrimeType}{a factor with levels \code{Continuous}, \code{Split}, and \code{none} coding the placement of the particle in the previous token in the text. \code{none} = no prior particle verb tokens were found in the text.}
#'   \item{Surprisal.P}{a numeric vector coding the surprisal of the particle given the verb}
#'   \item{Surprisal.V}{a numeric vector coding the surprisal of the verb given the particle}
#'   \item{CV.binary}{a factor with levels \code{CC} (e.g. "put down") and \code{other} (e.g. "put away", "pay back", "throw away") coding the pattern of the final segment of the verb and initial segment of the particle}
#'   \item{StressClash}{a factor with levels \code{no} and \code{yes} coding whether the continuous variant results in a stress clash across the verb-particle boundary ("conNECT UP")}
#'   \item{Rhythm}{a numeric vector coding the eurhythmic distance of the observed token}
#'   ...
#' }
#' @source \url{https://osf.io/x8vyw/}
"particle_verbs_short"
