# contamination_standalone
Stand-alone contamination models with Docker images and scripts.

## Images (Docker Hub, amd64)
- BMP runtime: `docker pull nspies13/contamination-bmp:latest`
- CBC runtime: `docker pull nspies13/contamination-cbc:latest`
- BMP training (no bundled models): `docker pull nspies13/contamination-bmp-train:latest`
- Apple Silicon: add `--platform linux/amd64` to `docker pull` / `docker run`.

## BMP training (no bundled models)
- Example (writes models to a mounted host directory):<br>`docker run --rm -v "$PWD/data:/data" -v "$PWD/tmp_outputs:/outputs" nspies13/contamination-bmp-train:latest /data/bmp_test_wide.csv /data/fluid_concentrations.tsv /outputs/bmp_models_combined.RDS`
- Bring your own training CSV + fluid concentrations (CSV/TSV) and mount an output dir for the generated RDS.

## BMP runtime
- Single JSON (wide): `cat sample_bmp_wide.json | docker run --rm -i nspies13/contamination-bmp:latest --mode single --input-format wide`
- Single JSON (long → preprocess → predict): `cat sample_bmp_long.json | docker run --rm -i nspies13/contamination-bmp:latest --mode single --input-format long`
- Batch CSV (long → CSV predictions): `docker run --rm -v "$PWD/data:/data" nspies13/contamination-bmp:latest --mode batch --input-format long --input-file /data/bmp_test_long.csv --output-file /data/bmp_predictions.csv`
- Preprocess only (long → wide): `docker run --rm -v "$PWD/data:/data" nspies13/contamination-bmp:latest --mode batch --input-format long --input-file /data/bmp_test_long.csv --output-file /data/bmp_wide.csv` (drop prediction columns if you only need features).
- Streaming JSON (keeps container alive; newline-delimited JSON on stdin, one response line per request):
  ```
  docker run --rm -i nspies13/contamination-bmp:latest --mode single --stream --input-format wide
  {"PATIENT_ID":"abc","DRAWN_DT_TM":"2024-01-01T00:00:00Z",...}
  {"PATIENT_ID":"def","DRAWN_DT_TM":"2024-01-02T00:00:00Z",...}
  ```
  Send one JSON object per line (wide or long). Ctrl+D to exit.
- Options: `--lookback-hours` (default 48) controls pre/post window for long input. Long input columns: `PATIENT_ID`, `DRAWN_DT_TM` (ISO-8601), `TASK_ASSAY`, `RESULT_VALUE`. Wide columns: e.g., `sodium`, `sodium_prior`, `sodium_post`, etc. (see `data/bmp_test_wide.csv`).
- Custom models: `-e BMP_MODEL_PATH=/models/bmp_models_combined.RDS -e BMP_MIX_MODEL_PATH=/models/bmp_mix_ratio_models_combined.RDS` (mount your RDS at those paths).

## CBC runtime
- Single JSON (wide): `cat sample_cbc_wide.json | docker run --rm -i nspies13/contamination-cbc:latest --mode single --input-format wide`
- Single JSON (long → preprocess → predict): `cat sample_cbc_long.json | docker run --rm -i nspies13/contamination-cbc:latest --mode single --input-format long`
- Batch CSV (long → CSV predictions): `docker run --rm -v "$PWD/data:/data" nspies13/contamination-cbc:latest --mode batch --input-format long --input-file /data/cbc_test_long.csv --output-file /data/cbc_predictions.csv`
- Preprocess only (long → wide): `docker run --rm -v "$PWD/data:/data" nspies13/contamination-cbc:latest --mode batch --input-format long --input-file /data/cbc_test_long.csv --output-file /data/cbc_wide.csv` (drop prediction columns if you only need features).
- Streaming JSON (keeps container alive; newline-delimited JSON on stdin, one response line per request):
  ```
  docker run --rm -i nspies13/contamination-cbc:latest --mode single --stream --input-format wide
  {"PATIENT_ID":"abc","DRAWN_DT_TM":"2024-01-01T00:00:00Z",...}
  {"PATIENT_ID":"def","DRAWN_DT_TM":"2024-01-02T00:00:00Z",...}
  ```
  Send one JSON object per line (wide or long). Ctrl+D to exit.
- Options: `--collection-interval` (default 48) controls prior/post window for long input. Long input columns: `PATIENT_ID`/`EPIC_MRN`, `DRAWN_DT_TM` (ISO-8601), `TASK_ASSAY`, `RESULT_VALUE_NUMERIC`/`RESULT_VALUE`. Wide columns: e.g., `Hgb`, `Plt`, `WBC` plus `_prior` / `_post` (see `data/cbc_test_wide.csv`).
- Custom models: `-e CBC_MODEL_PATH=/models/cbc_models_combined.RDS -e CBC_MIX_MODEL_PATH=/models/cbc_mix_ratio_model.RDS` (mount your RDS at those paths).
