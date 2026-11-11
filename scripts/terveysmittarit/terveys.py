"""Populate the adapted LJS2027 workbook; open the output in Excel to recalculate.

Use --check to validate without saving. The default output is a local copy in
results/terveysmittarit, preserving the shared template.
Population/employment totals are aggregated automatically by Rscript (requires
the R yaml package), using forecast mappings from scenarios.tsv or --forecast.
"""

import argparse
import shutil
import subprocess
import tempfile
from pathlib import Path

import pandas
from openpyxl import load_workbook
from openpyxl.workbook.properties import CalcProperties


ROOT = Path(__file__).resolve().parents[2]
RESULT_DIR = ROOT / "results"
TEMPLATE_PATH = (
    Path.home()
    / "Helsingin Seudun liikenne - Kuntayhtymä"
    / "EXT-MAL 2027 - Vaikutusten arviointi"
    / "04_Teemakohtaiset tarkastelut (mittarit)"
    / "Terveysmittarit"
    / "LJS 2027 terveysvaikutusmittarit.xlsx"
)
OUTPUT_PATH = RESULT_DIR / "terveysmittarit" / f"{TEMPLATE_PATH.stem}_perusskenaariot{TEMPLATE_PATH.suffix}"

# Keep the template's H41 names because calculation sheets reference them.
SCENARIOS = {
    "INPUT_H41_2023": "Nykytila_2023_H51",
    "INPUT_H41_2045_ve0": "Vertailupohja_2045_H51",
    "INPUT_H41_2045_suunnitelma": "Suunnitelmaluonnos_2045_H51",
    "INPUT_H41_2045_tavoiteverkko": "Tavoiteverkko_2045_H51",
}

# File, header row, number of data rows in the prepared template.
TABLES = (
    ("vehicle_kms_vdfs.txt", 2, 7),
    ("vehicle_kms_areas.txt", 12, 6),
    ("vehicle_kms_vdfs_areas.txt", 21, 6),
    ("trips_areas.txt", 41, 6),
    ("noise_areas.txt", 53, 6),
)

# Current Helmet noise schema, independent of the template's older labels.
NOISE_COLUMNS = [
    f"{measure}_{band}_dB"
    for measure in ("area", "population")
    for band in ("50_54", "55_59", "60_64", "65_69", "70_74", "75_plus")
] + ["population_total"]


def prepare_landuse(workbook, scenarios, forecast_overrides=None, rscript="Rscript"):
    """Run the R aggregation for the selected scenarios and validate Excel rows."""
    metadata = pandas.read_csv(ROOT / "scenarios.tsv", sep="\t", comment="#",
                               dtype=str)
    forecast_overrides = forecast_overrides or {}
    manifest = []
    for sheet, folder in scenarios.items():
        forecast = forecast_overrides.get(folder)
        if forecast is None:
            matches = metadata.loc[metadata["results"] == folder, "forecast"].unique()
            if len(matches) != 1:
                raise ValueError(
                    f"No unique forecast mapping for {folder}. Add it to scenarios.tsv "
                    f"or use --forecast {folder}=FORECAST_FOLDER"
                )
            forecast = matches[0]
        manifest.append({"sheet": sheet, "forecast": forecast})
    executable = shutil.which(str(rscript))
    if executable is None:
        raise FileNotFoundError("Rscript was not found; use --rscript with its full path")
    with tempfile.TemporaryDirectory(prefix="terveys-landuse-") as folder:
        manifest_path = Path(folder) / "manifest.tsv"
        totals_path = Path(folder) / "totals.tsv"
        pandas.DataFrame(manifest).to_csv(manifest_path, sep="\t", index=False)
        subprocess.run(
            # Start a clean helper session, without interactive/project startup.
            [executable, "--vanilla", str(ROOT / "scripts/terveysmittarit/asukkaat_terveyksiin.R"),
             str(manifest_path), str(totals_path)],
            cwd=ROOT, check=True,
        )
        totals = pandas.read_csv(totals_path, sep="\t")
    translations = pandas.read_csv(ROOT / "utilities/areas.tsv", sep="\t")
    labels = dict(zip(translations["level"], translations["label"]))
    labels["other"] = "Muu mallialue"
    areas = ["helsinki_cbd", "helsinki_other", "espoo_vant_kau",
             "surround_train", "surround_other", "other"]
    prepared = []
    for sheet in scenarios:
        worksheet = workbook[sheet]
        df = totals.loc[totals["sheet"] == sheet].set_index("area")
        if list(df.index) != areas:
            raise ValueError(f"{sheet}: unexpected land-use areas/order")
        df = df[["total_pop", "total_wrk"]]
        if df.isna().any().any() or (df < 0).any().any():
            raise ValueError(f"{sheet}: missing or negative land-use totals")
        if [worksheet.cell(31, c).value for c in (2, 3)] != list(df.columns):
            raise ValueError(f"{sheet}: expected total_pop and total_wrk at B31:C31")
        if [worksheet.cell(r, 1).value for r in range(32, 38)] != [labels[a] for a in areas]:
            raise ValueError(f"{sheet}: land-use labels/order differ at A32:A37")
        prepared.append((worksheet, 31, df))
    return prepared


def prepare_tables(workbook, result_dir, scenarios=None):
    """Validate all tables before changing cells, including noise headers."""
    prepared = []
    if scenarios is None:
        scenarios = SCENARIOS
    for sheet_name, results_folder in scenarios.items():
        worksheet = workbook[sheet_name]
        for file_name, header_row, row_count in TABLES:
            path = result_dir / results_folder / file_name
            df = pandas.read_csv(path, sep=r"\s+", index_col=0)
            expected_headers = [
                str(worksheet.cell(header_row, col).value)
                for col in range(2, len(df.columns) + 2)
            ]
            if file_name == "noise_areas.txt":
                # Accept the current schema even when the template is older.
                # Legacy inputs must still match their template exactly.
                if list(df.columns) == NOISE_COLUMNS:
                    expected_headers = NOISE_COLUMNS
            expected_index = [
                str(worksheet.cell(row, 1).value)
                for row in range(header_row + 1, header_row + row_count + 1)
            ]
            if list(df.columns) != expected_headers:
                raise ValueError(
                    f"{path}: columns differ from {sheet_name}, row {header_row}. "
                    f"Expected {expected_headers}; got {list(df.columns)}"
                )
            if list(df.index.map(str)) != expected_index:
                raise ValueError(f"{path}: row labels/order differ from {sheet_name}")
            if df.isna().any().any():
                raise ValueError(f"{path}: missing input values")
            prepared.append((worksheet, header_row, df))
    return prepared


def write_workbook(template_path, output_path, result_dir=RESULT_DIR, check=False,
                   scenarios=None, forecast_overrides=None, rscript="Rscript"):
    if scenarios is None:
        scenarios = SCENARIOS
    if not check and template_path.resolve() == output_path.resolve():
        raise ValueError("Choose an output path different from the template path")
    wb = load_workbook(template_path)
    try:
        prepared = prepare_tables(wb, result_dir, scenarios)
        prepared.extend(prepare_landuse(wb, scenarios, forecast_overrides, rscript))
        for sheet_name, folder in scenarios.items():
            print(f"{sheet_name}: {folder}")
        if check:
            print(f"Validated {len(prepared)} input tables across {len(scenarios)} scenarios")
            return
        for worksheet, header_row, df in prepared:
            for col_nr, column in enumerate(df.columns, start=2):
                worksheet.cell(header_row, col_nr, column)
            for row_nr, row in enumerate(
                df.itertuples(index=False, name=None), start=header_row + 1
            ):
                for col_nr, value in enumerate(row, start=2):
                    worksheet.cell(row_nr, col_nr, value)
        wb.calculation = CalcProperties(calcMode="auto", fullCalcOnLoad=True)
        output_path.parent.mkdir(parents=True, exist_ok=True)
        wb.save(output_path)
        print(f"Saved {output_path}")
    finally:
        wb.close()


def main():
    # Command line examples (run from the repository root):
    #
    # Validate all inputs without saving:
    # python scripts/terveysmittarit/terveys.py --check
    # Populate a local copy using the default template and results:
    # python scripts/terveysmittarit/terveys.py
    # Save to a different output file:
    # python scripts/terveysmittarit/terveys.py --output "results/terveysmittarit/LJS 2027 updated.xlsx"
    # python scripts/terveysmittarit/terveys.py --template "data/LJS 2027 template.xlsx" --results "C:/Helmet/results" --check
    # python scripts/terveysmittarit/terveys.py --baseline Vertailupohja_2045_Ruuhkainen_tieverkko_H51 --plan Suunnitelmaluonnos_2045_Ruuhkainen_tieverkko_H51 --target Tavoiteverkko_2045_Ruuhkainen_tieverkko_H51 --forecast Suunnitelmaluonnos_2045_Ruuhkainen_tieverkko_H51=2045_suunnitelma_ruuhkainen_tieverkko_cco45 --output "results/terveysmittarit/terveys_tieverkko.xlsx"
    # python scripts/terveysmittarit/terveys.py --baseline Vertailupohja_2045_Taantuva_a_H51 --plan Suunnitelmaluonnos_2045_Taantuva_H51 --target Tavoiteverkko_2045_Taantuva_H51 --forecast Suunnitelmaluonnos_2045_Taantuva_H51=2045_suunnitelma_taantuva_cco23 --output "results/terveysmittarit/terveys_taantuva.xlsx" --check
    # Export Ruuhkainen joukkoliikenne to a separate Excel workbook:
    # The plan sheet uses Tavoiteverkko results, matching scenarios.tsv.
    # python scripts/terveysmittarit/terveys.py --baseline Vertailupohja_2045_Ruuhkainen_joukkoliikenne_H51 --plan Tavoiteverkko_2045_Ruuhkainen_joukkoliikenne_H51 --target Tavoiteverkko_2045_Ruuhkainen_joukkoliikenne_H51 --output "results/terveysmittarit/terveys_joukkoliikenne.xlsx"
    # Open the exported workbook in Excel (Windows Command Prompt):
    # start "" "results\terveysmittarit\terveys_joukkoliikenne.xlsx"
    # Override only the baseline; the other sheets use their main scenarios:
    # Use Taantuva_a; Taantuva_b and Taantuva_c are archived.
    # python scripts/terveysmittarit/terveys.py --baseline Vertailupohja_2045_Taantuva_a_H51 --output "results/terveysmittarit/terveys_taantuva_baseline_only.xlsx"
    # Population/employment aggregation runs automatically, including with --check.
    # Supply a forecast mapping for a results folder absent from scenarios.tsv:
    # python scripts/terveysmittarit/terveys.py --plan Suunnitelmaluonnos_2045_Taantuva_H51 --forecast Suunnitelmaluonnos_2045_Taantuva_H51=2045_suunnitelma_taantuva_cco23 --check
    # If Rscript is not on PATH:
    # python scripts/terveysmittarit/terveys.py --rscript "C:/Program Files/R/R-4.3.3/bin/Rscript.exe" --check
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--template", type=Path, default=TEMPLATE_PATH)
    parser.add_argument("--output", type=Path, help=f"Output workbook (default: {OUTPUT_PATH})")
    parser.add_argument("--results", type=Path, default=RESULT_DIR)
    parser.add_argument("--check", action="store_true")
    parser.add_argument("--baseline", help="Baseline results folder name under --results")
    parser.add_argument("--plan", help="Plan results folder name under --results")
    parser.add_argument("--target", help="Target network results folder name under --results")
    parser.add_argument("--rscript", default="Rscript", help="Rscript executable or full path")
    parser.add_argument("--forecast", action="append", default=[], metavar="RESULTS=FORECAST",
                        help="Forecast folder override for a results folder; repeat as needed")
    args = parser.parse_args()
    scenarios = SCENARIOS.copy()
    for role, sheet in (
        ("baseline", "INPUT_H41_2045_ve0"),
        ("plan", "INPUT_H41_2045_suunnitelma"),
        ("target", "INPUT_H41_2045_tavoiteverkko"),
    ):
        folder = getattr(args, role)
        if folder:
            scenarios[sheet] = folder
    if scenarios != SCENARIOS and args.output is None and not args.check:
        parser.error("Sensitivity exports require --output to name a separate workbook")
    forecast_overrides = {}
    for item in args.forecast:
        folder, separator, forecast = item.partition("=")
        if not separator or not folder or not forecast:
            parser.error("--forecast must be RESULTS_FOLDER=FORECAST_FOLDER")
        if folder not in scenarios.values():
            parser.error(f"--forecast results folder is not selected: {folder}")
        if folder in forecast_overrides:
            parser.error(f"Duplicate --forecast override for {folder}")
        forecast_overrides[folder] = forecast
    write_workbook(args.template, args.output or OUTPUT_PATH, args.results,
                   args.check, scenarios, forecast_overrides, args.rscript)


if __name__ == "__main__":
    main()
