# SET-VAR: State-Dependent Effects of Fiscal Policy in Peru

Data and RStudio adaptation codes of the SET-VAR codes of Katerina Petrova (Gargiulo, Matthes and Petrova, 2025, "Monetary policy across inflation regimes", *European Economic Review*) Originally in Matlab, adapted to this study.

## Run

Open `SETVAR_Fiscal_Peru.Rproj` in RStudio and run `main.R`. Base R only.

## Structure

- `main.R` – runs every exercise and writes `figures/` and `results/`
- `R/` – estimation, structural and plotting functions (one file per function, same names as the MATLAB version)
- `data/data_peru.csv` – quarterly year-on-year growth rates, 1997Q1–2025Q4 (G, T, GDP, TOT, PX, EMBIG)

The Working of the paper is available at http://dx.doi.org/10.2139/ssrn.7350143
