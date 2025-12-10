# contamination_standalone
Stand-alone contamination models with Docker images and scripts.

## Images (Docker Hub, amd64)
- BMP runtime: `docker pull nspies13/contamination-bmp:latest`
- CBC runtime: `docker pull nspies13/contamination-cbc:latest`
- BMP training (no bundled models): `docker pull nspies13/contamination-bmp-train:latest`
- Apple Silicon: add `--platform linux/amd64` to `docker pull` / `docker run`.

## JSON prediction pipeline (single-run, streaming, REST API)
- Run BMP API (POST `/predict`; optional `input_format=long`, `lookback_hours=72`):<br>`docker run --rm -p 8000:8000 nspies13/contamination-bmp:latest --mode api --input-format wide --port 8000`
- Run CBC API (POST `/predict`; optional `input_format=long`, `collection_interval=72`):<br>`docker run --rm -p 8002:8000 nspies13/contamination-cbc:latest --mode api --input-format wide --port 8000`
- Single JSON (wide):<br>`cat sample_bmp_wide.json | docker run --rm -i nspies13/contamination-bmp:latest --mode single --input-format wide`<br>`cat sample_cbc_wide.json | docker run --rm -i nspies13/contamination-cbc:latest --mode single --input-format wide`
- Single JSON (long → preprocess → predict):<br>`cat sample_bmp_long.json | docker run --rm -i nspies13/contamination-bmp:latest --mode single --input-format long`<br>`cat sample_cbc_long.json | docker run --rm -i nspies13/contamination-cbc:latest --mode single --input-format long`
- Streaming JSON (keeps container alive; newline-delimited JSON on stdin, one response line per request):<br>`docker run --rm -i nspies13/contamination-bmp:latest --mode single --stream --input-format wide`<br>`docker run --rm -i nspies13/contamination-cbc:latest --mode single --stream --input-format wide`
  - Send one JSON object per line (wide or long). Ctrl+D to exit.
- Options: BMP `--lookback-hours` (default 48) sets pre/post window for long input. CBC `--collection-interval` (default 48) sets prior/post window for long input. Long input columns: `PATIENT_ID`, `DRAWN_DT_TM` (ISO-8601), `TASK_ASSAY`, `RESULT_VALUE`/`RESULT_VALUE_NUMERIC`. Wide examples: see `data/bmp_test_wide.csv` and `data/cbc_test_wide.csv`.
- Custom models: BMP `-e BMP_MODEL_PATH=/models/bmp_models_combined.RDS -e BMP_MIX_MODEL_PATH=/models/bmp_mix_ratio_models_combined.RDS`; CBC `-e CBC_MODEL_PATH=/models/cbc_models_combined.RDS -e CBC_MIX_MODEL_PATH=/models/cbc_mix_ratio_model.RDS` (mount your RDS files at those paths).

## Batch CSV modes
- BMP batch (long → CSV predictions):<br>`docker run --rm -v "$PWD/data:/data" nspies13/contamination-bmp:latest --mode batch --input-format long --input-file /data/bmp_test_long.csv --output-file /data/bmp_predictions.csv`
- CBC batch (long → CSV predictions):<br>`docker run --rm -v "$PWD/data:/data" nspies13/contamination-cbc:latest --mode batch --input-format long --input-file /data/cbc_test_long.csv --output-file /data/cbc_predictions.csv`
- Preprocess only (long → wide, drop prediction columns afterward if only features are needed):<br>`docker run --rm -v "$PWD/data:/data" nspies13/contamination-bmp:latest --mode batch --input-format long --input-file /data/bmp_test_long.csv --output-file /data/bmp_wide.csv`<br>`docker run --rm -v "$PWD/data:/data" nspies13/contamination-cbc:latest --mode batch --input-format long --input-file /data/cbc_test_long.csv --output-file /data/cbc_wide.csv`

## BMP training (no bundled models)
- Example (writes models to a mounted host directory):<br>`docker run --rm -v "$PWD/data:/data" -v "$PWD/tmp_outputs:/outputs" nspies13/contamination-bmp-train:latest /data/bmp_test_wide.csv /data/fluid_concentrations.tsv /outputs/bmp_models_combined.RDS`
- Bring your own training CSV + fluid concentrations (CSV/TSV) and mount an output dir for the generated RDS.
