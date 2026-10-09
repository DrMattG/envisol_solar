# Eksporter ett objekt fra solar_polygons til NIBIOs klimagasskalkulator.
# Forutsetter at solar_polygons allerede er lest inn som sf, slik i din kode.
# Dette er en plantegning for analyse, ikke en formell reguleringsplan.
# Hele valgt geometri tilordnes ett formål; avgrensningen må vurderes faglig.
# Kilder kontrollert 2026-09-30:
# https://nibio.no/tjenester/kartbasert-klimagasskalkulator-for-arealbrukssektoren
# Se «Veileder for bruk av arealplaner», 23.3.2026, avsnitt 3.1.1.
# https://register.geonorge.no/sosi-kodelister/plan/reguleringsplan/rparealform%C3%A5l
# https://klimagasskalkulator.nibio.no/
# https://www.gjovik.kommune.no/om-gjovik-kommune/kontakt-oss/

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
valgt_anlegg <- "Seval skog solkraftverk"
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

# Mål: scripts/data/processed. Fungerer fra prosjektroten eller scripts.
# Ved annen arbeidsmappe: erstatt dir_out med full Windows-sti med /.
scripts_dir <- if (basename(normalizePath(getwd())) == "scripts") "." else "scripts"
dir_out <- file.path(scripts_dir, "data", "processed")

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

# 2. VELG OG KONTROLLER OBJEKT -------------------------------------------
stopifnot(inherits(solar_polygons, "sf"))
stopifnot(all(c("anleggnavn", "kommune", "saksid") %in% names(solar_polygons)))

x <- solar_polygons[which(solar_polygons$anleggnavn == valgt_anlegg), ]
if (nrow(x) != 1L) stop("Velg et anleggnavn som gir nøyaktig én rad.")
if (is.na(st_crs(x))) stop("Kildedata mangler CRS. Avklar riktig CRS før eksport.")
if (any(st_is_empty(x))) stop("Objektet har tom geometri.")
if (!all(as.character(st_geometry_type(x)) %in% c("POLYGON", "MULTIPOLYGON"))) {
  stop("Geometrien må være POLYGON eller MULTIPOLYGON.")
}
if (!all(st_is_valid(x) %in% TRUE)) {
  stop(paste("Ugyldig geometri. Undersøk før reparasjon:",
             paste(st_is_valid(x, reason = TRUE), collapse = "; ")))
}

kode <- unname(formal_koder[valgt_formal])
knr <- x$kommunenummer
objtype <- unname(c(
  "reguleringsplan" = "RpArealformålOmråde",
  "kommuneplan" = "KpArealformålOmråde"
)[planniva])
if (is.na(kode)) stop("Valgt formål mangler i formal_koder.")
if (is.na(knr)) stop("Valgt anlegg mangler kommunenummer. Se kommune_kontroll.")
if (x$kommune_lik_storst) stop("To eller flere kommuner har like stort overlapp. Avklar tilknytningen.")
if (abs(x$kommunedekning - 1) > 0.001) {
  stop("Valgt anlegg har avvikende kommunedekning. Se kommune_fordeling før eksport.")
}
if (x$n_kommuner > 1L) {
  message("Anlegget krysser kommunegrensen. Største arealandel ligger i ", x$kommune,
          " (", round(100 * x$kommuneandel, 2), " %). Se kommune_fordeling.")
}
if (is.na(objtype)) stop("planniva må være reguleringsplan eller kommuneplan.")
if (is.na(x$saksid)) stop("Objektet mangler saksid til filnavnet.")

# Ingen automatisk reparasjon eller forenkling av geometrien.
# Arealkontroll beregnes fra geometri, ikke fra eldre arealfelt.
areal_m2 <- as.numeric(st_area(st_transform(x, 25833)))
if (!is.finite(areal_m2) || areal_m2 <= 0) stop("Arealet må være positivt.")

# 3. BYGG IMPORTDATA -----------------------------------------------------
# Feltnavn og heltall følger NIBIOs importveileder av 23.3.2026.
# I010V090W000 legges IKKE inn som arealformål; kalkulatoren gjør koblingen.
# MULTIPOLYGON beholder alle delpolygoner og hull.
ut <- st_sf(
  objtype = objtype,
  arealformål = as.integer(kode),
  planid = paste0("TEST - ", valgt_anlegg, " - ", kode),
  kommunenummer = as.integer(knr),
  geometry = st_geometry(st_transform(st_zm(x, drop = TRUE, what = "ZM"), 4326))
)

# 4. EKSPORT OG TILBAKELESING --------------------------------------------
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
fil <- file.path(dir_out, paste0("test_", x$saksid, "_", planniva, "_", kode, ".geojson"))
# Overskriver bare en eventuell tidligere testfil med akkurat dette navnet.
st_write(ut, fil, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE,
         layer_options = c("RFC7946=YES", "COORDINATE_PRECISION=9"))

kontroll <- st_read(fil, quiet = TRUE)
stopifnot(nrow(kontroll) == 1L)
stopifnot(all(c("objtype", "arealformål", "planid", "kommunenummer") %in% names(kontroll)))
stopifnot(identical(as.character(kontroll$objtype), objtype))
stopifnot(identical(as.character(kontroll$planid), ut$planid))
stopifnot(as.integer(kontroll$arealformål) == kode)
stopifnot(as.integer(kontroll$kommunenummer) == knr)
kontroll_m <- st_transform(kontroll, 25833)
stopifnot(!any(st_is_empty(kontroll_m)), all(st_is_valid(kontroll_m) %in% TRUE))
areal_etter <- as.numeric(st_area(kontroll_m))
stopifnot(abs(areal_etter - areal_m2) / areal_m2 < 1e-5)

print(st_drop_geometry(kontroll))
cat("\nBeregnet areal:", round(areal_m2 / 1000, 3), "dekar\n")
cat("Eksportert fil:", normalizePath(fil, winslash = "/"), "\n")
plot(st_geometry(kontroll), main = paste(valgt_anlegg, valgt_formal, sep = "\n"))
# Neste steg: Last opp filen og kontroller plassering, formål og areal i kalkulatoren.
# Teknisk godkjent import er ikke alene en validering av scenarioets faglige innhold.
