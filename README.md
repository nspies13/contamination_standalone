# contamination_standalone
A repository for stand-alone contamination models, docker containers, and scripts.

## Docker containers

Prebuilt images are available on Docker Hub (amd64). Pull them instead of building locally:

- BMP runtime: `docker pull nspies13/contamination-bmp:latest`
- CBC runtime: `docker pull nspies13/contamination-cbc:latest`
- BMP training (no bundled models): `docker pull nspies13/contamination-bmp-train:latest`
- On Apple Silicon, include `--platform linux/amd64` on `docker run` or `docker pull`.

### BMP training container (no bundled models)
- Example run (writes models to a mounted host directory):<br>`docker run --rm -v "$PWD/data:/data" -v "$PWD/tmp_outputs:/outputs" nspies13/contamination-bmp-train:latest /data/bmp_test_wide.csv /data/fluid_concentrations.tsv /outputs/bmp_models_combined.RDS`
- The training image only includes code/dependencies; supply your own training CSV and fluid concentration file (CSV or TSV) and mount an output directory for the generated RDS.

### Running the BMP container
- **Single JSON → JSON (wide ready):** `cat sample_bmp_wide.json | docker run --rm -i nspies13/contamination-bmp:latest --mode single --input-format wide`
- **Single JSON from long:** `cat sample_bmp_long.json | docker run --rm -i nspies13/contamination-bmp:latest --mode single --input-format long`
- **Batch CSV → CSV predictions from long:** `docker run --rm -v "$PWD/data:/data" nspies13/contamination-bmp:latest --mode batch --input-format long --input-file /data/bmp_test_long.csv --output-file /data/bmp_predictions.csv`
- **Preprocess only (long → wide):** `docker run --rm -v "$PWD/data:/data" nspies13/contamination-bmp:latest --mode batch --input-format long --input-file /data/bmp_test_long.csv --output-file /data/bmp_wide.csv` (predictions will be appended; drop the prediction columns if you only need wide features).
- **Streaming JSON (keeps container alive):**
  ```
  docker run --rm -i nspies13/contamination-bmp:latest --mode single --stream --input-format wide
  {"PATIENT_ID":"abc","DRAWN_DT_TM":"2024-01-01T00:00:00Z",...}
  {"PATIENT_ID":"def","DRAWN_DT_TM":"2024-01-02T00:00:00Z",...}
  ```
  Send one JSON object per line (wide or long); each line emits one JSON response line. Ctrl+D to exit.
- Optional: `--lookback-hours` (default 48) controls the pre/post window when building prior/post lab values (long input only).
- Long input format: `PATIENT_ID`, `DRAWN_DT_TM` (ISO-8601), `TASK_ASSAY`, `RESULT_VALUE`. Wide input: columns like `sodium`, `sodium_prior`, `sodium_post`, etc. (see `data/bmp_test_wide.csv`).
- Supply custom models by mounting and pointing env vars: `-e BMP_MODEL_PATH=/models/bmp_models_combined.RDS -e BMP_MIX_MODEL_PATH=/models/bmp_mix_ratio_models_combined.RDS` (mount your files at those paths).

### Running the CBC container
- **Single JSON → JSON (wide ready):** `cat sample_cbc_wide.json | docker run --rm -i nspies13/contamination-cbc:latest --mode single --input-format wide`
- **Single JSON from long:** `cat sample_cbc_long.json | docker run --rm -i nspies13/contamination-cbc:latest --mode single --input-format long`
- **Batch CSV → CSV predictions from long:** `docker run --rm -v "$PWD/data:/data" nspies13/contamination-cbc:latest --mode batch --input-format long --input-file /data/cbc_test_long.csv --output-file /data/cbc_predictions.csv`
- **Preprocess only (long → wide):** `docker run --rm -v "$PWD/data:/data" nspies13/contamination-cbc:latest --mode batch --input-format long --input-file /data/cbc_test_long.csv --output-file /data/cbc_wide.csv` (predictions will be appended; drop the prediction columns if you only need wide features).
- **Streaming JSON (keeps container alive):**
  ```
  docker run --rm -i nspies13/contamination-cbc:latest --mode single --stream --input-format wide
  {"PATIENT_ID":"abc","DRAWN_DT_TM":"2024-01-01T00:00:00Z",...}
  {"PATIENT_ID":"def","DRAWN_DT_TM":"2024-01-02T00:00:00Z",...}
  ```
  Send one JSON object per line (wide or long); each line emits one JSON response line. Ctrl+D to exit.
- Optional: `--collection-interval` (default 48) controls the window for prior/post CBC values (long input only).
- Long input format: `PATIENT_ID` (or `EPIC_MRN`), `DRAWN_DT_TM` (ISO-8601), `TASK_ASSAY`, `RESULT_VALUE_NUMERIC` (or `RESULT_VALUE`). Wide input: columns like `Hgb`, `Plt`, `WBC` plus `_prior`/`_post` fields (see `data/cbc_test_wide.csv`).
- Supply custom models by mounting and pointing env vars: `-e CBC_MODEL_PATH=/models/cbc_models_combined.RDS -e CBC_MIX_MODEL_PATH=/models/cbc_mix_ratio_model.RDS` (mount your files at those paths).
