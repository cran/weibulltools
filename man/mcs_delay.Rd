% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/delay_distributions.R
\name{mcs_delay}
\alias{mcs_delay}
\title{Adjustment of Operating Times by Delays using a Monte Carlo Approach}
\usage{
mcs_delay(
  date_1,
  date_2,
  time,
  status = NULL,
  id = paste0("ID", seq_len(length(time))),
  distribution = c("lognormal", "exponential")
)
}
\arguments{
\item{date_1}{A vector of class \code{character} or \code{Date}, in the
  format "yyyy-mm-dd", indicating the earlier of the two dates. Use \code{NA}
  for missing elements.

  If more than one delay should be considered it must be a list where the first
  element contains the earlier dates of the first delay and the second element
  contains the earlier dates of the second delay, and so forth.(See 'Examples').}

\item{date_2}{A vector of class \code{character} or \code{Date}, in the
  format "yyyy-mm-dd", indicating the later of the two dates. Use \code{NA}
  for missing elements.

  If more than one delay should be considered it must be a list where the first
  element contains the later dates of the first delay and the second element
  contains the later dates of the second delay, and so forth. (See 'Examples').}

\item{time}{A numeric vector of operating times. Use \code{NA} for missing elements.}

\item{status}{Optional argument. If used it has to be a vector of binary
data (0 or 1) indicating whether unit \emph{i} is a right censored observation
(= 0) or a failure (= 1). The effect of \code{status} on the return is described
in 'Value'.}

\item{id}{A vector for the identification of every unit.}

\item{distribution}{Supposed distribution of the delay random variable. If more
than one delay is to be considered and different distributions are assumed
for each delay, the argument \code{distribution} must have the same length
as list \code{date_1} (and \code{date_2}). For example, in the case of
two delays with different distributions, one has to specify the argument as
\code{distribution = c("lognormal", "exponential")}. Then the lognormal
distribution is applied to the first delay and the exponential distribution
to the second (See 'Examples').}
}
\value{
A list containing the following elements:
  \itemize{
    \item \code{data} : A tibble with classes \code{wt_mcs_data} and
      \code{wt_reliability_data} if \code{status} is provided. Since the
      class \code{wt_reliability_data} enables the direct usage of
      \code{data} inside
      \code{\link[=estimate_cdf]{estimate_cdf.wt_reliability_data}}, the
      required lifetime characteristic is automatically set to the operating
      time \code{time}.

      If \code{status = NULL} class is \code{wt_mcs_data}, which is not
      supported by \code{estimate_cdf} due to missing \code{status}.

      The tibble contains the following columns:
      \itemize{
        \item \code{date_1} : Earlier dates. If argument \code{date_1} is a list
          of length \emph{i, i > 1} (described in \strong{Arguments}) multiple
          columns with names \code{date_1.1}, \code{date_1.2}, ..., \code{date_1.i}
          and the corresponding values of the earlier dates are used.
        \item \code{date_2} : Later dates. In the case of a list with length greater
          than 1, the routine described above is used.
        \item \code{time} : Adjusted operating times for incomplete observations
          and input operating times for the complete observations.
        \item \code{status} (\strong{optional}) :
          \itemize{
            \item If argument \code{status = NULL} column \code{status} does
              not exist.
            \item If argument \code{status} is provided the column contains
              the entered binary data (0 or 1).
          }
        \item \code{id} : Identification of every unit.
      }
    \item \code{sim_data} : A tibble with column \code{sim_delay} that holds the
      simulated delay-specific numbers for incomplete cases and \code{0} for
      complete cases. If more than one delay was considered multiple columns
      \code{sim_delay.1}, \code{sim_delay.2}, ..., \code{sim_delay.i} with
      corresponding delay-specific random numbers are presented.
    \item \code{model_estimation} : A list containing a named list
      (\code{"delay_distribution"}) with output of \code{\link{dist_delay}}. For
      multiple delays the list contains as many lists as there are delays, i.e.
      (\code{"delay_distribution.1"}, \code{"delay_distribution.2"}, ...,
      \code{"delay_distribution.i"}).
  }
}
\description{
In general, the amount of available information about units in the field is very
different. During the warranty period, there are only a few cases with complete
data (mainly \emph{failed units}) but lots of cases with incomplete data (usually
\emph{censored units}). As a result, the operating time of units with incomplete
information is often inaccurate and must be adjusted by delays.

This function reduces the operating times of incomplete observations by simulated
delays (in days). A unit is considered as incomplete if the later of the two
dates is unknown, i.e. \code{date_2 = NA}. See 'Details' for some practical examples.

Random delay numbers are drawn from the distribution determined by complete cases
(described in 'Details' of \code{\link{dist_delay}}).
}
\details{
In field data analysis time-dependent characteristics (e.g. \emph{time in service})
are often imprecisely recorded. These inaccuracies are caused by unconsidered delays.

For a better understanding of the MCS application in the context of field data,
two cases are described below.
\itemize{
  \item \strong{Delay in registration}: It is common that a supplier, which provides
    parts to the manufacturing industry does not know when the unit, in which
    its parts are installed, were put in service (due to unknown \code{date_2},
    i.e. registration or sales date). Without taking the described delay into
    account, the time in service of the failed units would be the difference
    between the repair date and \code{date_1} (i.e. the production date) and for
    intact units the difference between the present date and \code{date_1}. But
    the real operating times are (much) shorter, since the stress on the
    components have not started until the whole systems were put in service.
    Hence, units with incomplete data (missing \code{date_2}) must be reduced by
    the delays.
  \item \strong{Delay in report}: Authorized repairers often do not immediately
    notify the manufacturer or OEM of repairs that were made during the warranty
    period, but instead pass the information about these repairs in collected
    forms e.g. weekly, monthly or quarterly. The resulting time difference between
    the reporting (\code{date_2}) of the repair in the guarantee database and the
    actual repair date (\code{date_1}), which is often assumed to be the failure
    date, is called the reporting delay. For a given date where the analysis
    is made there could be units which had a failure but are not registered
    and therefore treated as censored units. In order to take this case into
    account and according to the principle of equal opportunities, the lifetime
    of units with no report date (\code{date_2 = NA}) is reduced by simulated
    reporting delays.
}
}
\examples{
# Data for examples:
date_of_production   <- c("2014-07-28", "2014-02-17", "2014-07-14",
                          "2014-06-26", "2014-03-10", "2014-05-14",
                          "2014-05-06", "2014-03-07", "2014-03-09",
                          "2014-04-13", "2014-05-20", "2014-07-07",
                          "2014-01-27", "2014-01-30", "2014-03-17",
                          "2014-02-09", "2014-04-14", "2014-04-20",
                          "2014-03-13", "2014-02-23", "2014-04-03",
                          "2014-01-08", "2014-01-08")

date_of_registration <- c(NA, "2014-03-29", "2014-12-06", "2014-09-09",
                          NA, NA, "2014-06-16", NA, "2014-05-23",
                          "2014-05-09", "2014-05-31", NA, "2014-04-13",
                          NA, NA, "2014-03-12", NA, "2014-06-02",
                          NA, "2014-03-21", "2014-06-19", NA, NA)

date_of_repair <-       c(NA, "2014-09-15", "2015-07-04", "2015-04-10", NA,
                          NA, "2015-04-24", NA, "2015-04-25", "2015-04-24",
                          "2015-06-12", NA, "2015-05-04", NA, NA,
                          "2015-05-22", NA, "2015-09-17", NA, "2015-08-15",
                          "2015-11-26", NA, NA)

date_of_report <-       c(NA, "2014-10-09", "2015-08-28", "2015-04-15", NA,
                          NA, "2015-05-16", NA, "2015-05-28", "2015-05-15",
                          "2015-07-11", NA, "2015-08-14", NA, NA,
                          "2015-06-05", NA, "2015-10-17", NA, "2015-08-21",
                          "2015-12-02", NA, NA)

time_in_service <- rep(1000, length(date_of_production))
status <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0)

# Example 1 - MCS for delay in registration:
mcs_regist <- mcs_delay(
  date_1 = date_of_production,
  date_2 = date_of_registration,
  time = time_in_service,
  status = status,
  distribution = "lognormal"
)

# Example 2 - MCS for delay in report:
mcs_report <- mcs_delay(
  date_1 = date_of_repair,
  date_2 = date_of_report,
  time = time_in_service,
  status = status,
  distribution = "exponential"
)

# Example 3 - Reproducibility of random numbers:
set.seed(1234)
mcs_report_reproduce <- mcs_delay(
  date_1 = date_of_repair,
  date_2 = date_of_report,
  time = time_in_service,
  status = status,
  distribution = "exponential"
)

# Example 4 - MCS for delays in registration and report with same distribution:
mcs_delays <- mcs_delay(
  date_1 = list(date_of_production, date_of_repair),
  date_2 = list(date_of_registration, date_of_report),
  time = time_in_service,
  status = status,
  distribution = "lognormal"
)

# Example 5 - MCS for delays in registration and report with different distributions:
## Assuming lognormal registration and exponential reporting delays.
mcs_delays_2 <- mcs_delay(
  date_1 = list(date_of_production, date_of_repair),
  date_2 = list(date_of_registration, date_of_report),
  time = time_in_service,
  status = status,
  distribution = c("lognormal", "exponential")
)

}
\references{
Verband der Automobilindustrie e.V. (VDA); Qualitätsmanagement in
  der Automobilindustrie. Zuverlässigkeitssicherung bei Automobilherstellern
  und Lieferanten. Zuverlässigkeits-Methoden und -Hilfsmittel.; 4th Edition, 2016,
  <ISSN:0943-9412>
}
\seealso{
\code{\link{estimate_cdf}}
}
