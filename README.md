# contamination_standalone
Stand-alone contamination models with Docker images and scripts.

## Images (Docker Hub, amd64)
- BMP runtime: `docker pull nspies13/contamination-bmp:latest`
- CBC runtime: `docker pull nspies13/contamination-cbc:latest`
- BMP training (no bundled models): `docker pull nspies13/contamination-bmp-train:latest`
- Apple Silicon: add `--platform linux/amd64` to `docker pull` / `docker run`.

## Minimal JSON APIs
The images now only load the bundled models and expose a REST API:
- BMP: `docker run --rm -p 8000:8000 -e HOST=0.0.0.0 -e PORT=8000 nspies13/contamination-bmp:latest`
- CBC: `docker run --rm -p 8002:8000 -e HOST=0.0.0.0 -e PORT=8000 nspies13/contamination-cbc:latest`

Model paths (optional overrides):
- BMP: `BMP_MODEL_PATH` (default `models/bmp_models_combined.RDS`), `BMP_MIX_MODEL_PATH` (default `models/bmp_mix_ratio_models_combined.RDS`)
- CBC: `CBC_MODEL_PATH` (default `models/cbc_models_combined.RDS`), `CBC_MIX_MODEL_PATH` (default `models/cbc_mix_ratio_model.RDS`)

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

Responses are JSON with prediction probabilities/predicted classes (and mix ratios when compatible with the xgboost version).
