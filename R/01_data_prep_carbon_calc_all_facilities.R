# Eksporter samtlige solkraftverk til NIBIOs klimagasskalkulator.
# Én GeoJSON Feature per anlegg, også når geometrien er MULTIPOLYGON.
# Dette er en plantegning for analyse, ikke en formell reguleringsplan.
# Hele geometrien til hvert anlegg gis valgt formål; avgrensningene må vurderes faglig.
# Kilder kontrollert 2026-09-30:
# https://nibio.no/tjenester/kartbasert-klimagasskalkulator-for-arealbrukssektoren
# Se «Veileder for bruk av arealplaner», 23.3.2026, avsnitt 3.1.1.
# https://register.geonorge.no/sosi-kodelister/plan/reguleringsplan/rparealform%C3%A5l
# https://klimagasskalkulator.nibio.no/

library(sf)
library(dplyr)

# ---- Settings ------------------------------------------------------------

#getwd()

Sys.setlocale(locale = "no_NB.utf8")

# Project-relative paths
dir_raw       <- "data/raw"
dir_processed <- "data/processed"


# ---- Read spatial data ---------------------------------------------------

solar_polygons <- st_read(
  file.path(dir_raw, "sol_jan_2026_final.gpkg"),
  quiet = TRUE
) %>%
  select(anleggnavn, everything())

glimpse(solar_polygons)

unique(solar_polygons$anleggnavn)


# 1. VALG ---------------------------------------------------------------
antall_per_fil <- Inf  # Sett f.eks. til 10L ved langsomme beregninger.
valgt_formal <- "Solkraftanlegg"
planniva <- "reguleringsplan"  # alternativ: "kommuneplan"

# Et lite, utvidbart utvalg formål. Kodene gjelder begge plannivåene her.
# Nye formål må kontrolleres mot kodelisten for det valgte plannivået.
formal_koder <- c(
  "Solkraftanlegg" = 1531L,
  "Boligbebyggelse" = 1110L,
  "Næringsbebyggelse" = 1300L,
  "Veg" = 2010L
)

# Prosjektets rot er arbeidsmappe (samme som for dir_raw).
dir_out <- dir_processed

# 1B. KOMMUNETILKNYTNING FOR ALLE POLYGONER -------------------------------
# Les på nytt uten st_cast(): hver kommune beholder alle sine delpolygoner.
kommune_gdb <- paste0(
  "R:/GeoSpatialData/AdministrativeUnits/",
  "Norway_AdministrativeUnits/Original/Norway_Municipalities/",
  "Basisdata_0000_Norge_25833_Kommuner_FGDB/",
  "Basisdata_0000_Norge_25833_Kommuner_FGDB.gdb"
)
kommuner <- st_read(kommune_gdb, layer = "kommune", quiet = TRUE) %>%
  select(kommunenummer, kommunenavn)

stopifnot(inherits(solar_polygons, "sf"), nrow(solar_polygons) > 0L)
if (is.na(st_crs(solar_polygons))) stop("solar_polygons mangler CRS.")
if (is.na(st_crs(kommuner)) || isTRUE(st_is_longlat(kommuner))) {
  stop("Kommunedata må ha et kjent, projisert koordinatsystem.")
}
if (anyNA(kommuner$kommunenummer) ||
    any(!grepl("^[0-9]{4}$", kommuner$kommunenummer))) {
  stop("Kommunedata har manglende eller ugyldige kommunenummer.")
}

# Bruk en arbeidskopi med rad-ID. Kildegeometri og radrekkefølge bevares.
sol_kommune <- st_sf(
  .rad = seq_len(nrow(solar_polygons)),
  geometry = st_geometry(st_transform(solar_polygons, st_crs(kommuner)))
)
if (!all(as.character(st_geometry_type(sol_kommune)) %in% c("POLYGON", "MULTIPOLYGON")) ||
    any(st_is_empty(sol_kommune)) || !all(st_is_valid(sol_kommune) %in% TRUE)) {
  stop("Solpolygonene har tom, ugyldig eller ikke-polygonal geometri. Undersøk før kobling.")
}
if (!all(st_is_valid(kommuner) %in% TRUE)) {
  stop("Kommunedata har ugyldig geometri. Undersøk før kobling.")
}
sol_areal_m2 <- as.numeric(st_area(sol_kommune))
stopifnot(all(is.finite(sol_areal_m2)), all(sol_areal_m2 > 0))

# Begrens den mer krevende klippingen til kommuner som berører anleggene.
kommuner <- kommuner[lengths(st_intersects(kommuner, sol_kommune)) > 0L, ]
if (nrow(kommuner) == 0L) stop("Ingen kommunetreff. Kontroller plassering og CRS.")
st_agr(sol_kommune) <- "constant"
st_agr(kommuner) <- "constant"
klipp <- st_intersection(sol_kommune, kommuner)

# Summer eventuelle separate treff i samme kommune; ren grensekontakt utelates.
kommune_fordeling <- klipp %>%
  mutate(overlapp_m2 = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  filter(overlapp_m2 > 0) %>%
  group_by(.rad, kommunenummer, kommunenavn) %>%
  summarise(overlapp_m2 = sum(overlapp_m2), .groups = "drop") %>%
  mutate(andel = overlapp_m2 / sol_areal_m2[.rad])

kommune_valg <- kommune_fordeling %>%
  group_by(.rad) %>%
  arrange(desc(overlapp_m2), kommunenummer, .by_group = TRUE) %>%
  mutate(
    n_kommuner = n(),
    kommunedekning = sum(andel),
    kommune_lik_storst = sum(near(overlapp_m2, max(overlapp_m2))) > 1L
  ) %>%
  slice_head(n = 1L) %>%
  ungroup()

# Kopier tilbake via rad-ID: én rad per anlegg, også når det mangler treff.
# Behold opprinnelig kommunenavn også dersom skriptet kjøres flere ganger.
if (!"kommune_opprinnelig" %in% names(solar_polygons)) {
  solar_polygons$kommune_opprinnelig <- solar_polygons$kommune
}
i <- match(seq_len(nrow(solar_polygons)), kommune_valg$.rad)
solar_polygons$kommunenummer <- kommune_valg$kommunenummer[i]  # tekst i arbeidsdata
solar_polygons$kommune <- kommune_valg$kommunenavn[i]
solar_polygons$n_kommuner <- coalesce(kommune_valg$n_kommuner[i], 0L)
solar_polygons$kommuneandel <- kommune_valg$andel[i]
solar_polygons$kommunedekning <- coalesce(kommune_valg$kommunedekning[i], 0)
solar_polygons$kommune_lik_storst <- coalesce(kommune_valg$kommune_lik_storst[i], FALSE)

# Vis tilfeller som bør undersøkes. Alle positive arealoverlapp telles,
# også små grensestriper. Dekningsavvik > 0,1 % flagges for kontroll.
kommune_kontroll <- solar_polygons %>%
  st_drop_geometry() %>%
  select(anleggnavn, kommune_opprinnelig, kommune, kommunenummer,
         n_kommuner, kommuneandel, kommunedekning, kommune_lik_storst)
print(kommune_kontroll %>% filter(
  n_kommuner != 1L | abs(kommunedekning - 1) > 0.001 | kommune_lik_storst
))
cat("\nKommunetilknytning:", sum(!is.na(solar_polygons$kommunenummer)),
    "av", nrow(solar_polygons), "anlegg har treff.\n")
# kommune_fordeling beholder arealfordelingen på alle kommuner for videre kontroll.

# 2. VALIDER ANLEGG OG IMPORTKODER -----------------------------------------
if (!is.character(valgt_formal) || length(valgt_formal) != 1L ||
    !valgt_formal %in% names(formal_koder)) {
  stop("valgt_formal må finnes i formal_koder.")
}
kode <- unname(formal_koder[[valgt_formal]])
objtype <- unname(c(
  "reguleringsplan" = "RpArealformålOmråde",
  "kommuneplan" = "KpArealformålOmråde"
)[planniva])
if (length(objtype) != 1L || is.na(objtype)) {
  stop("planniva må være reguleringsplan eller kommuneplan.")
}
if (!is.infinite(antall_per_fil) &&
    (length(antall_per_fil) != 1L || is.na(antall_per_fil) ||
     antall_per_fil < 1 || antall_per_fil != floor(antall_per_fil))) {
  stop("antall_per_fil må være Inf eller et positivt heltall.")
}
if (anyNA(solar_polygons$saksid) ||
    any(solar_polygons$saksid != floor(solar_polygons$saksid)) ||
    anyDuplicated(solar_polygons$saksid)) {
  stop("saksid må finnes, være heltall og være unik for hvert anlegg.")
}
if (anyNA(solar_polygons$anleggnavn) ||
    any(!nzchar(trimws(solar_polygons$anleggnavn)))) {
  stop("Alle anlegg må ha navn.")
}
if (anyNA(solar_polygons$kommunenummer)) {
  stop("Anlegg uten kommunenummer. Se kommune_kontroll.")
}
if (any(solar_polygons$kommune_lik_storst)) {
  stop("Lik største kommuneandel for minst ett anlegg. Se kommune_kontroll.")
}
if (any(abs(solar_polygons$kommunedekning - 1) > 0.001)) {
  stop("Avvik i kommunedekning for minst ett anlegg. Se kommune_kontroll.")
}
if (any(solar_polygons$n_kommuner > 1L)) {
  message("Noen anlegg krysser kommunegrenser; se kommune_kontroll. ",
          "Filen har kommunenummer fra største arealandel per anlegg.")
}

# Eventuelle overlapp mellom *ulike* prosjekter er viktig ved summering.
# Denne sjekken endrer ikke geometrien og stopper ikke eksporten.
treff <- st_intersects(sol_kommune)
par <- do.call(rbind, lapply(seq_along(treff), function(j) {
  andre <- treff[[j]][treff[[j]] > j]
  if (!length(andre)) return(NULL)
  data.frame(rad1 = j, rad2 = andre)
}))
overlapp_kontroll <- data.frame(
  saksid1 = numeric(), saksid2 = numeric(), overlapp_m2 = numeric()
)
if (!is.null(par)) {
  for (j in seq_len(nrow(par))) {
    a <- as.numeric(st_area(st_intersection(
      st_geometry(sol_kommune)[par$rad1[j]],
      st_geometry(sol_kommune)[par$rad2[j]]
    )))
    if (length(a) && is.finite(a) && a > 0.01) {
      overlapp_kontroll[nrow(overlapp_kontroll) + 1L, ] <- list(
        solar_polygons$saksid[par$rad1[j]],
        solar_polygons$saksid[par$rad2[j]], a
      )
    }
  }
}
if (nrow(overlapp_kontroll)) {
  message("Overlapp mellom ulike prosjekter: se overlapp_kontroll. ",
          "Ikke summer utslipp uten å vurdere disse arealene.")
  print(overlapp_kontroll)
}

# 3. ÉN FEATURE PER ANLEGG -----------------------------------------------
# Bruk en unik planidentifikasjon per anlegg. Identisk planid i samme
# kommune ville blitt slått sammen i kalkulatorens resultattabell.
# Størrelsen måles i LAEA, som brukes til arealtall i kalkulatoren.
planid <- paste0("SOL_", solar_polygons$saksid, "_", kode)
areal_for_m2 <- as.numeric(st_area(st_transform(solar_polygons, 3035)))

ut <- st_sf(
  objtype = rep(objtype, nrow(solar_polygons)),
  arealformål = rep(as.integer(kode), nrow(solar_polygons)),
  planid = planid,
  kommunenummer = as.integer(solar_polygons$kommunenummer),
  feltnavn = solar_polygons$anleggnavn,
  geometry = st_geometry(st_transform(
    st_zm(solar_polygons, drop = TRUE, what = "ZM"), 4326
  ))
)

# 4. SKRIV OG LES TILBAKE -----------------------------------------------
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
n <- nrow(ut)
storrelse <- if (is.infinite(antall_per_fil)) n else as.integer(antall_per_fil)
grupper <- split(seq_len(n), ceiling(seq_len(n) / storrelse))
filnavn <- character(length(grupper))
for (j in seq_along(grupper)) {
  idx <- grupper[[j]]
  filnavn[j] <- file.path(
    dir_out,
    sprintf("alle_solkraftverk.geojson", planniva, kode, j)
  )
  st_write(ut[idx, ], filnavn[j], driver = "GeoJSON",
           delete_dsn = TRUE, quiet = TRUE,
           layer_options = c("RFC7946=YES", "COORDINATE_PRECISION=9"))
  kontroll <- st_read(filnavn[j], quiet = TRUE)
  if (nrow(kontroll) != length(idx) ||
      !identical(as.character(kontroll$planid), planid[idx]) ||
      any(as.integer(kontroll$arealformål) != kode) ||
      any(as.integer(kontroll$kommunenummer) != as.integer(ut$kommunenummer[idx])) ||
      any(as.character(kontroll$objtype) != objtype) ||
      any(st_is_empty(kontroll)) || !all(st_is_valid(kontroll) %in% TRUE)) {
    stop("Kontroll etter eksport feilet for: ", filnavn[j])
  }
  areal_etter_m2 <- as.numeric(st_area(st_transform(kontroll, 3035)))
  if (any(abs(areal_etter_m2 - areal_for_m2[idx]) /
          areal_for_m2[idx] > 1e-5)) {
    stop("Arealavvik etter eksport for: ", filnavn[j])
  }
  cat("Skrev", length(idx), "anlegg til",
      normalizePath(filnavn[j], winslash = "/"), "\n")
}

# Tabell for sammenligning med kalkulatorens areal per plan.
areal_kontroll <- data.frame(
  planid = planid,
  anleggnavn = solar_polygons$anleggnavn,
  kommune = solar_polygons$kommune,
  kommunenummer = solar_polygons$kommunenummer,
  n_kommuner = solar_polygons$n_kommuner,
  areal_daa_LAEA = round(areal_for_m2 / 1000, 2)
)
print(areal_kontroll, row.names = FALSE)
cat("Totalt antall:", n, "anlegg i", length(filnavn), "fil(er).\n")
# filnavn, areal_kontroll, kommune_fordeling og overlapp_kontroll er
# tilgjengelige i R-miljøet etter kjøring.
