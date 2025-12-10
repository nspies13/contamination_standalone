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

- Streaming JSON (keeps container alive; newline-delimited JSON on stdin, one response line per request):<br>`docker run --rm -i nspies13/contamination-bmp:latest --mode single --stream --input-format wide`<br>`docker run --rm -i nspies13/contamination-cbc:latest --mode single --stream --input-format wide`
  - Send one JSON object per line (wide or long). Ctrl+D to exit.


## Batch CSV modes
- BMP batch (long → CSV predictions):<br>`docker run --rm -v "$PWD/data:/data" nspies13/contamination-bmp:latest --mode batch --input-format long --input-file /data/bmp_test_long.csv --output-file /data/bmp_predictions.csv`
- CBC batch (long → CSV predictions):<br>`docker run --rm -v "$PWD/data:/data" nspies13/contamination-cbc:latest --mode batch --input-format long --input-file /data/cbc_test_long.csv --output-file /data/cbc_predictions.csv`


## BMP training (no bundled models)
- Example (writes models to a mounted host directory):<br>`docker run --rm -v "$PWD/data:/data" -v "$PWD/tmp_outputs:/outputs" nspies13/contamination-bmp-train:latest /data/bmp_test_wide.csv /data/fluid_concentrations.tsv /outputs/bmp_models_combined.RDS`
- Bring your own training CSV + fluid concentrations (CSV/TSV) and mount an output dir for the generated RDS.
