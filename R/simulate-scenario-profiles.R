# =============================================================================
# SIMULATION
# File: R/simulate-scenario-profiles.R
# Part of CovariateSearcher Package
# Simulation step of the exposure-metrics forest-plot workflow.
# =============================================================================

#' Simulate Concentration-Time Profiles for Each Covariate Scenario
#'
#' @description
#' Runs each scenario's parameter draws through an mrgsolve model under a
#' user-defined dosing regimen, and stacks the resulting concentration-time
#' profiles tagged by scenario. Consumes the named list from
#' [build_scenario_parameters()]: each element is one scenario's per-sample
#' parameters, and every sample is simulated as an individual subject (via
#' mrgsolve `idata`) under the common dose (via `events`).
#'
#' The model must expose the parameters it needs as `$PARAM` (e.g.
#' `THETA1..THETAn`); columns of the parameter tables matching those names
#' override them per subject. Covariate effects are already baked into the
#' parameters upstream by [apply_covariate_model()], so the mrgsolve model must
#' be structural only (no covariate terms).
#'
#' @param param_sets Named list of data frames from [build_scenario_parameters()]
#'   (each: `ID` + structural `THETA` columns). The list names label the
#'   scenarios and are carried into a `Scenario` column of the output.
#' @param mod A loaded mrgsolve model object (from [mrgsolve::mread()]), compiled
#'   by the caller before calling this function.
#' @param dose An mrgsolve event object ([mrgsolve::ev()]) describing the dosing
#'   regimen, applied to every subject, e.g.
#'   `mrgsolve::ev(amt = 300, cmt = 1, ii = 12, addl = 5, ss = 1)`.
#' @param start,end,delta Numeric. Observation time grid for [mrgsolve::mrgsim()].
#'   Defaults `0`, `24`, `0.1`. For AUC over one steady-state interval set `end`
#'   to one dosing interval (`ii`).
#'
#' @return A `data.frame` of stacked profiles - one row per
#'   (scenario, sample, time) - with a `Scenario` column (an ordered factor, in
#'   scenario definition order), `ID` (the sample), `time`, and the model's
#'   captured columns (e.g. `CP`, `IPRED`, `DV`), ready for metric derivation,
#'   e.g. `group_by(Scenario, ID) %>% summarise(AUC = ..., Cmax = ..., Cmin = ...)`.
#'
#' @section Memory:
#' Holds every time point for every sample in every scenario
#' (~`n_scenarios * Nsamples * n_times` rows). Fine for modest `Nsamples`; at
#' `1e5`, simulate and reduce to metrics one scenario at a time.
#'
#' @seealso [build_scenario_parameters()]
#' @examples
#' \dontrun{
#' mod  <- mrgsolve::mread("models/sim_model.cpp")   # loaded once, by the caller
#' dose <- mrgsolve::ev(amt = 300, cmt = 1, ii = 12, addl = 5, ss = 1)
#' profiles <- simulate_scenario_profiles(param_sets, mod, dose, end = 12)
#' }
#' @export
simulate_scenario_profiles <- function(param_sets,
                                        mod,
                                        dose,
                                        start = 0,
                                        end = 24,
                                        delta = 0.1) {

  if (!is.list(param_sets) || is.null(names(param_sets))) {
    stop("`param_sets` must be a named list ",
         "(output of build_scenario_parameters()).")
  }
  if (!inherits(dose, "ev")) {
    stop("`dose` must be an mrgsolve event object in `ev` format ",
         "(created with mrgsolve::ev()).")
  }

  # Simulate each scenario's samples as a population, tag with the scenario name.
  profiles <- purrr::imap_dfr(param_sets, function(params, scenario) {
    sim <- mrgsolve::mrgsim(
      mod,
      idata  = params,
      events = dose,
      start  = start,
      end    = end,
      delta  = delta
    )
    out <- as.data.frame(sim)
    out$Scenario <- scenario
    out
  })

  # Keep the scenario definition order (typical subject first) as factor levels,
  # so it survives group_by()/summarise() and RDS round-trips - the forest plot
  # then orders the axis correctly with no scenario table or `scenario_order`.
  profiles$Scenario <- factor(profiles$Scenario, levels = names(param_sets))
  profiles
}
