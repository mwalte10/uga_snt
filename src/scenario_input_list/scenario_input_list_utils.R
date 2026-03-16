# =============================================================================
# ensure_all_years_utils.R
#
# Provides a typed `ensure_all_years()` dispatcher and one function per
# intervention.  Replaces both the old generic `ensure_all_years()` and the
# `fill_smc_scaffold()` helper that was previously called inside each scenario
# function.
#
# Full-grid definition per intervention:
#
#   Intervention  |  Completeness guarantee
#   --------------|------------------------------------------------------------
#   treatment     |  Every district x year has exactly one row.
#   itn (dist)    |  Every district x year has at least one row.  Sub-strata
#                 |  (net_type / distribution_type / distribution_day_of_year)
#                 |  are left exactly as-is for years that already have data.
#                 |  For entirely missing district-years (< 2020 only) a single
#                 |  placeholder row is inserted:
#                 |    net_type              = 'pyrethroid_only'
#                 |    distribution_type     = 'routine'
#                 |    distribution_day_of_year = 1
#                 |    distribution_lower/upper = 0
#                 |  Years >= 2020 are expected to be populated by the scenario
#                 |  functions before ensure_all_years is called.
#   itn (use)     |  Every district x year has at least one row (same logic,
#                 |  using usage_day_of_year as the within-year sub-stratum).
#   irs           |  Every district x year has exactly one row.
#                 |  Future years use the last observed insecticide
#                 |  ('actellic' default) and spray_day_of_year = 1.
#   smc           |  Every SMC district x year x round (1:5) has a row.
#                 |  Restricted to the ~10 districts present in the raw data.
#   vaccine       |  Every vaccine district x year x dose (1-4) has a row.
#                 |  Restricted to districts already present in the raw data.
#
# Missing rows are filled with:
#   * coverage columns     -> 0
#   * metadata columns     -> carried forward from the district's last year
#   * SMC age columns      -> smc_max_age 1825 (<2026) / 3650 (>=2026),
#                             smc_min_age 0
#   * SMC low_level        -> 'adm_2' for ALL rows (including pre-existing
#                             adm_1 entries such as the 'Karamoja' region row)
# =============================================================================

library(data.table)

# Canonical round-day-of-year defaults for SMC rounds 1-5
SMC_ROUND_DAYS <- c(`1` = 91L, `2` = 136L, `3` = 182L, `4` = 227L, `5` = 273L)

# All four R21 vaccine dose labels
VACCINE_DOSES <- paste0("Malaria_vaccine_", 1:4)

# Every possible coverage column name across all interventions
ALL_COV_COLS <- c(
  "tx_cov", "prop_act",
  "itn_input_dist", "distribution_upper", "distribution_lower", "itn_use",
  "irs_cov",
  "smc_cov",
  "r21_primary_cov", "r21_booster_cov", "r21_booster1_cov",
  "rtss_primary_cov", "rtss_booster1_cov",
  "pmc_cov", "lsm_cov"
)

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Columns that are NOT coverage and NOT universal keys — metadata to carry forward.
.meta_cols <- function(dt, extra_key_cols = character(0)) {
  universal_keys <- c("name_2", "year", "country", "iso3c", "name_1",
                      "intervention", "low_level")
  setdiff(colnames(dt), c(ALL_COV_COLS, universal_keys, extra_key_cols))
}

# Return one metadata row per `by_cols` group from the most recent year.
.last_year_meta <- function(dt, by_cols = "name_2",
                            extra_key_cols = character(0)) {
  meta_cols <- .meta_cols(dt, extra_key_cols)
  if (length(meta_cols) == 0) return(unique(dt[, ..by_cols]))
  dt[, .SD[year == max(year)][1], by = by_cols, .SDcols = meta_cols]
}

# Set all coverage columns to 0 (in place).
# When ref_dt is supplied, adds+zeros every coverage col present in ref_dt
# (used by vaccine where coverage cols are absent from `missing` after the
# meta merge).  Without ref_dt, zeros only cols already present in dt.
.zero_cov <- function(dt, ref_dt = NULL) {
  target_cols <- if (!is.null(ref_dt)) {
    intersect(colnames(ref_dt), ALL_COV_COLS)   # cols that exist in the source table
  } else {
    intersect(colnames(dt), ALL_COV_COLS)        # cols already present in dt
  }
  for (col in target_cols) dt[, (col) := 0]
  invisible(dt)
}

# Forward-fill country / iso3c / name_1 from non-NA rows in the same table.
.fill_geography <- function(dt) {
  geo_cols <- intersect(c("country", "iso3c", "name_1"), colnames(dt))
  if (length(geo_cols) == 0) return(dt)
  needs_fill <- Reduce(`|`, lapply(geo_cols, function(c) is.na(dt[[c]])))
  if (!any(needs_fill)) return(dt)

  lookup <- unique(dt[!needs_fill, c("name_2", geo_cols), with = FALSE])[
    , .SD[1], by = "name_2"]
  dt <- merge(dt, lookup, by = "name_2", all.x = TRUE,
              suffixes = c("", ".lkp"), allow.cartesian = TRUE)
  for (col in geo_cols) {
    lkp <- paste0(col, ".lkp")
    if (lkp %in% colnames(dt)) {
      dt[is.na(get(col)), (col) := get(lkp)]
      dt[, (lkp) := NULL]
    }
  }
  dt
}


# =============================================================================
# 1. TREATMENT  (name_2 x year)
# =============================================================================
ensure_all_years_treatment <- function(dt, loc_map, year_range = 2000:2035) {
  dt        <- data.table(dt)
  key_cols  <- c("name_2", "year")
  districts <- unique(as.character(loc_map$name_2))

  full_grid <- data.table(expand.grid(list(name_2 = districts, year = year_range)))
  missing   <- full_grid[!dt, on = key_cols]

  if (nrow(missing) == 0) {
    message("  treatment: complete -- no rows added.")
    return(data.frame(setkeyv(dt, key_cols)))
  }
  message("  treatment: adding ", nrow(missing),
          " missing district-year rows (coverage = 0).")

  meta    <- .last_year_meta(dt)
  missing <- merge(missing, meta, by = "name_2", all.x = TRUE,
                   allow.cartesian = TRUE)
  .zero_cov(missing)
  missing[, intervention := "treatment"]

  dt <- rbindlist(list(dt, missing), fill = TRUE)
  dt <- .fill_geography(dt)
  setkeyv(dt, key_cols)
  data.frame(dt)
}


# =============================================================================
# 2. ITN DISTRIBUTION  (name_2 x year, at least one row per district-year)
#
# The correctness guarantee is at the district x year level, NOT at the level
# of sub-strata (net_type / distribution_type / distribution_day_of_year).
# Sub-strata vary legitimately across years (e.g. mass campaigns only appear
# in campaign years) so they must not be universalised across all years.
#
# For district-years that are entirely absent AND year < 2020, a single
# placeholder row is inserted with:
#   net_type                 = 'pyrethroid_only'
#   distribution_type        = 'routine'
#   distribution_day_of_year = 1
#   distribution_lower/upper = 0
#
# District-years that are entirely absent AND year >= 2020 are expected to
# have been populated by the scenario functions before this is called; they
# are left as-is (no placeholder inserted) to avoid overriding scenario logic.
# =============================================================================
ITN_PLACEHOLDER <- list(
  net_type                 = "pyrethroid_only",
  distribution_type        = "routine",
  distribution_day_of_year = 1L,
  distribution_lower       = 0,
  distribution_upper       = 0,
  itn_input_dist           = 0
)

ensure_all_years_itn <- function(dt, loc_map, year_range = 2000:2035) {
  dt        <- data.table(dt)
  key_cols  <- c("name_2", "year")
  districts <- unique(as.character(loc_map$name_2))

  # Full grid is district x year only — sub-strata are NOT crossed globally
  full_grid <- data.table(expand.grid(list(name_2 = districts, year = year_range)))

  # Which district-years have NO rows at all?
  present   <- unique(dt[, .(name_2, year)])
  missing   <- full_grid[!present, on = key_cols]

  if (nrow(missing) == 0) {
    message("  itn (dist): complete -- no district-years missing.")
    return(data.frame(dt))
  }

  # Split on the year threshold
  missing_pre  <- missing[year <  2020]
  missing_post <- missing[year >= 2020]

  if (nrow(missing_post) > 0) {
    message("  itn (dist): ", nrow(missing_post),
            " district-years >= 2020 are missing -- these should have been ",
            "populated by the scenario functions. Skipping.")
  }

  if (nrow(missing_pre) == 0) {
    message("  itn (dist): no pre-2020 district-years to fill.")
    return(data.frame(dt))
  }
  message("  itn (dist): adding ", nrow(missing_pre),
          " placeholder rows for pre-2020 missing district-years.")

  # Carry forward geography metadata only (no sub-strata meta — we set defaults)
  geo_meta <- unique(dt[, .(name_2, country, iso3c, name_1)])[, .SD[1], by = "name_2"]
  placeholder <- merge(missing_pre, geo_meta, by = "name_2", all.x = TRUE)

  # Apply placeholder sub-strata and coverage defaults
  for (col in names(ITN_PLACEHOLDER)) {
    placeholder[, (col) := ITN_PLACEHOLDER[[col]]]
  }
  placeholder[, intervention := "itn"]

  dt <- rbindlist(list(dt, placeholder), fill = TRUE)
  dt <- .fill_geography(dt)
  setkeyv(dt, key_cols)
  data.frame(dt)
}


# =============================================================================
# 3. ITN USAGE  (name_2 x year, at least one row per district-year)
#
# Same district x year guarantee as the distribution table.  usage_day_of_year
# sub-strata are left exactly as-is for years that already have data.
# Missing pre-2020 district-years get a single placeholder row with itn_use = 0.
# =============================================================================
ensure_all_years_itn_use <- function(dt, loc_map, year_range = 2000:2035) {
  dt        <- data.table(dt)
  key_cols  <- c("name_2", "year")
  districts <- unique(as.character(loc_map$name_2))

  full_grid <- data.table(expand.grid(list(name_2 = districts, year = year_range)))

  present   <- unique(dt[, .(name_2, year)])
  missing   <- full_grid[!present, on = key_cols]

  if (nrow(missing) == 0) {
    message("  itn (use): complete -- no district-years missing.")
    return(data.frame(dt))
  }

  missing_pre  <- missing[year <  2020]
  missing_post <- missing[year >= 2020]

  if (nrow(missing_post) > 0) {
    message("  itn (use): ", nrow(missing_post),
            " district-years >= 2020 missing -- should be populated by ",
            "scenario functions. Skipping.")
  }

  if (nrow(missing_pre) == 0) {
    message("  itn (use): no pre-2020 district-years to fill.")
    return(data.frame(dt))
  }
  message("  itn (use): adding ", nrow(missing_pre),
          " placeholder rows for pre-2020 missing district-years.")

  geo_meta <- unique(dt[, .(name_2, country, iso3c, name_1)])[, .SD[1], by = "name_2"]
  placeholder <- merge(missing_pre, geo_meta, by = "name_2", all.x = TRUE)
  placeholder[, usage_day_of_year := 1L]
  placeholder[, itn_use           := 0]

  dt <- rbindlist(list(dt, placeholder), fill = TRUE)
  dt <- .fill_geography(dt)
  setkeyv(dt, key_cols)
  data.frame(dt)
}


# =============================================================================
# 4. IRS  (name_2 x year)
#
# Completeness guarantee is one row per district x year (round is dropped as
# a key dimension -- the scenario functions work at district x year level).
#
# For future years (>= 2026):
#   - insecticide: most recently observed insecticide for that district.
#     Districts with no IRS history default to 'actellic'.
#   - spray_day_of_year: 1 (first day of the year).
#   - irs_cov: set to 0 (scenario functions apply actual coverage).
#
# For historical gaps (< 2026): filled with irs_cov = 0 and metadata
# carried forward from the district's last observed year.
# =============================================================================
ensure_all_years_irs <- function(dt, loc_map, year_range = 2000:2035) {
  dt       <- data.table(dt)
  key_cols <- c("name_2", "year")

  all_districts <- unique(as.character(loc_map$name_2))

  # Collapse to one row per district-year: take the row with the highest round
  # (keeps round 2 where it exists; round 1 elsewhere) then drop round column
  dt <- dt[order(name_2, year, -round)][, .SD[1], by = .(name_2, year)]
  dt[, round := NULL]

  full_grid <- data.table(expand.grid(list(name_2 = all_districts,
                                           year   = year_range),
                                      stringsAsFactors = FALSE))
  missing <- full_grid[!dt, on = key_cols]

  if (nrow(missing) == 0) {
    message("  irs: complete -- no rows added.")
    return(data.frame(setkeyv(dt, key_cols)))
  }
  message("  irs: adding ", nrow(missing), " missing district-year rows (irs_cov = 0).")

  # Most recent insecticide per district (for carry-forward to future years)
  last_insecticide <- dt[!is.na(insecticide),
                         .(insecticide = insecticide[which.max(year)]),
                         by = name_2]

  # Split missing into historical and future
  missing_hist   <- missing[year < 2026]
  missing_future <- missing[year >= 2026]

  # ── Historical gaps: carry all metadata from last observed year ─────────────
  if (nrow(missing_hist) > 0) {
    meta <- .last_year_meta(dt, by_cols = "name_2")
    missing_hist <- merge(missing_hist, meta, by = "name_2",
                          all.x = TRUE, allow.cartesian = TRUE)
    .zero_cov(missing_hist)
    missing_hist[, intervention := "irs"]
  }

  # ── Future years: most recent insecticide; actellic if no history ───────────
  if (nrow(missing_future) > 0) {
    meta <- .last_year_meta(dt, by_cols = "name_2")
    missing_future <- merge(missing_future, meta, by = "name_2",
                            all.x = TRUE, allow.cartesian = TRUE)
    .zero_cov(missing_future)

    # Apply insecticide: use last observed, default to 'actellic'
    missing_future <- merge(missing_future, last_insecticide,
                            by = "name_2", all.x = TRUE, suffixes = c("", ".last"))
    if ("insecticide.last" %in% colnames(missing_future)) {
      missing_future[is.na(insecticide) | insecticide == "",
                     insecticide := insecticide.last]
      missing_future[, insecticide.last := NULL]
    }
    missing_future[is.na(insecticide) | insecticide == "",
                   insecticide := "actellic"]

    missing_future[, spray_day_of_year := 1L]
    missing_future[, intervention      := "irs"]
  }

  dt <- rbindlist(list(dt, missing_hist, missing_future), fill = TRUE)
  dt <- .fill_geography(dt)
  setkeyv(dt, key_cols)
  data.frame(dt)
}


# =============================================================================
# 5. SMC  (name_2 x year x round, rounds 1:5)
#
# Replaces fill_smc_scaffold().  Key differences from the old helper:
#   - Restricted to the ~10 SMC districts already in the data (not all 146).
#   - low_level is set to 'adm_2' for ALL rows (including pre-existing adm_1
#     entries such as the 'Karamoja' region-level row).
#   - round_day_of_year defaults applied to ALL rows where missing.
#   - Does not require loc_map or save_int -- fully self-contained.
# =============================================================================
ensure_all_years_smc <- function(dt, loc_map, year_range = 2000:2035) {
  dt       <- data.table(dt)
  key_cols <- c("name_2", "year", "round")

  smc_districts <- unique(as.character(loc_map$name_2))
  full_grid <- data.table(expand.grid(list(name_2 = smc_districts, year = year_range,
                                           round = 1:5)))
  full_grid[, round_day_of_year := SMC_ROUND_DAYS[as.character(round)]]

  missing <- full_grid[!dt, on = key_cols]

  if (nrow(missing) > 0) {
    message("  smc: adding ", nrow(missing),
            " missing district-year-round rows (smc_cov = 0).")

    meta <- .last_year_meta(dt, by_cols = "name_2",
                            extra_key_cols = c("round", "round_day_of_year"))
    missing <- merge(missing, meta, by = "name_2", all.x = TRUE,
                     allow.cartesian = TRUE)
    .zero_cov(missing)
    missing[, intervention := "smc"]
    dt <- rbindlist(list(dt, missing), fill = TRUE)
  } else {
    message("  smc: complete -- no rows added.")
  }

  # Apply to ALL rows: standardise low_level and fill age / day defaults
  dt[, low_level := "adm_2"]

  if ("round_day_of_year" %in% colnames(dt))
    dt[is.na(round_day_of_year),
       round_day_of_year := SMC_ROUND_DAYS[as.character(round)]]

  if ("smc_max_age" %in% colnames(dt)) {
    dt[is.na(smc_max_age), smc_max_age := 1825L]
  }
  if ("smc_min_age" %in% colnames(dt))
    dt[is.na(smc_min_age), smc_min_age := 0L]

  dt <- .fill_geography(dt)
  setkeyv(dt, key_cols)
  data.frame(dt)
}


# =============================================================================
# 6. VACCINE  (name_2 x year x r21_vaccine_dose, doses 1-4)
#
# Vaccine is NOT universal -- only districts already in the raw data get rows.
# Each district x dose pair carries metadata from the last available year.
# =============================================================================
ensure_all_years_vaccine <- function(dt, loc_map, year_range = 2000:2035) {
  dt       <- data.table(dt)
  key_cols <- c("name_2", "year", "r21_vaccine_dose")

  vac_districts <- unique(as.character(loc_map$name_2))
  full_grid <- data.table(expand.grid(list(name_2 = vac_districts, year = year_range,
                                           r21_vaccine_dose = VACCINE_DOSES),
                                      stringsAsFactors = FALSE))
  missing <- full_grid[!dt, on = key_cols]

  if (nrow(missing) == 0) {
    message("  vaccine: complete -- no rows added.")
    return(data.frame(setkeyv(dt, key_cols)))
  }
  message("  vaccine: adding ", nrow(missing),
          " missing district-year-dose rows (coverage = 0).")

  meta <- .last_year_meta(dt, by_cols = c("name_2", "r21_vaccine_dose"),
                          extra_key_cols = "r21_vaccine_dose")
  missing <- merge(missing, meta, by = c("name_2", "r21_vaccine_dose"),
                   all.x = TRUE, allow.cartesian = TRUE)
  .zero_cov(missing, ref_dt = dt)
  missing[, intervention := "vaccine"]

  dt <- rbindlist(list(dt, missing), fill = TRUE)
  dt <- .fill_geography(dt)
  setkeyv(dt, key_cols)
  data.frame(dt)
}


# =============================================================================
# Dispatch wrapper -- auto-detects intervention type from column names.
# =============================================================================
#' Ensure all strata are present in an intervention table
#'
#' @param dt         A data.frame or data.table for a single intervention.
#' @param loc_map    data.table with at least a name_2 column covering all
#'   districts. Required for all interventions.
#' @param year_range Integer vector of years to fill. Default: 2000:2035.
#' @return data.frame with all required district x year x sub-strata present.
#'   Missing rows have coverage = 0; metadata is carried from the last year.
#'   For ITN, only entirely missing district-years before 2020 are filled
#'   (with a placeholder row); existing years' sub-strata are untouched.
ensure_all_years <- function(dt, loc_map, year_range = 2000:2035) {
  dt <- data.table(dt)

  if ("smc_cov"         %in% colnames(dt)) return(ensure_all_years_smc(dt, loc_map, year_range))
  if ("itn_use"         %in% colnames(dt)) return(ensure_all_years_itn_use(dt, loc_map, year_range))
  if ("itn_input_dist"  %in% colnames(dt)) return(ensure_all_years_itn(dt, loc_map, year_range))
  if ("irs_cov"         %in% colnames(dt)) return(ensure_all_years_irs(dt, loc_map, year_range))
  if (any(c("r21_primary_cov", "r21_booster_cov", "r21_booster1_cov",
            "rtss_primary_cov") %in% colnames(dt)))
    return(ensure_all_years_vaccine(dt, loc_map, year_range))

  return(ensure_all_years_treatment(dt, loc_map, year_range))  # fallback
}
