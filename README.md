# contamination_standalone
Stand-alone contamination models with Docker images and scripts.

## Images (Docker Hub, amd64)
- BMP runtime: `docker pull nspies13/contamination-bmp:latest`
- CBC runtime: `docker pull nspies13/contamination-cbc:latest`
- BMP training (no bundled models): `docker pull nspies13/contamination-bmp-train:latest`
- CBC training (no bundled models): `docker pull nspies13/contamination-cbc-train:latest`
- Apple Silicon: add `--platform linux/amd64` to `docker pull` / `docker run`.

## Minimal JSON APIs
Load the bundled models and expose a REST API:
- BMP: `docker run --rm -p 8000:8000 -e HOST=0.0.0.0 -e PORT=8000 nspies13/contamination-bmp:latest`
- CBC: `docker run --rm -p 8002:8000 -e HOST=0.0.0.0 -e PORT=8000 nspies13/contamination-cbc:latest`

### Example payloads
- BMP (wide JSON): see `data/bmp_test_wide.csv` for column names; send JSON array of objects with those columns.
  ```
  curl -s -X POST http://localhost:8000/predict \
    -H "Content-Type: application/json" \
    -d '[{"sodium":133,"chloride":94,"potassium_plas":3.7,"co2_totl":26,"bun":40,"creatinine":2.42,"calcium":8.9,"glucose":181,"sodium_prior":132,"chloride_prior":83,"potassium_plas_prior":4.5,"co2_totl_prior":24,"bun_prior":49,"creatinine_prior":3.62,"calcium_prior":10.3,"glucose_prior":135,"sodium_post":135,"chloride_post":94,"potassium_plas_post":3.4,"co2_totl_post":28,"bun_post":32,"creatinine_post":1.75,"calcium_post":9.0,"glucose_post":133}]'
  ```
- CBC (wide JSON): see `data/cbc_test_wide.csv` for column names.
  ```
  curl -s -X POST http://localhost:8002/predict \
    -H "Content-Type: application/json" \
    -d '[{"Hgb":12.0,"Plt":103,"WBC":0.35,"Hgb_prior":10.9,"WBC_prior":0.23,"Plt_prior":115,"Hgb_post":10.8,"WBC_post":0.6,"Plt_post":60}]'
  ```

### Streaming endpoint
- Send newline-delimited JSON to `/predict_stream` to process multiple payloads in one request.
  ```
  printf '%s\n%s\n' '{"sodium":133,...}' '{"sodium":130,...}' | \
    curl -s -X POST http://localhost:8000/predict_stream -H "Content-Type: application/json" --data-binary @-
  ```
  Each line is parsed independently; the response is a JSON array of per-line results.

Responses are JSON with prediction probabilities/predicted classes (and mix ratios when I fix the versioning error).

### Batch CLI mode (CSV -> CSV with predictions appended)
- BMP: `docker run --rm -v "$PWD/data:/data" nspies13/contamination-bmp:latest --mode batch --input-file /data/bmp_test_wide.csv --output-file /data/bmp_predictions.csv`
- CBC: `docker run --rm -v "$PWD/data:/data" nspies13/contamination-cbc:latest --mode batch --input-file /data/cbc_test_wide.csv --output-file /data/cbc_predictions.csv`

## Training images
- BMP training: `docker run --rm -v "$PWD/data:/data" -v "$PWD/tmp_outputs:/outputs" nspies13/contamination-bmp-train:latest /data/bmp_test_wide.csv /data/fluid_concentrations.tsv /outputs/bmp_models_combined.RDS`
- CBC training: `docker run --rm -v "$PWD/data:/data" -v "$PWD/tmp_outputs:/outputs" nspies13/contamination-cbc-train:latest /data/cbc_test_wide.csv /outputs/cbc_models_combined.RDS /outputs/cbc_mix_ratio_model.RDS`
