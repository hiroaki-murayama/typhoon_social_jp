# typhoon_social_jp

This repository contains an R-based Jupyter workflow for typhoon-related mortality analysis in Japan. The project is designed to run inside Docker so that the required R, spatial, and notebook dependencies are available in a reproducible environment.

## Getting Started

### Prerequisites

- Docker Desktop with Docker Compose support

### Start JupyterLab

1. Build the image:

   ```bash
   docker compose build lab
   ```

2. Start the container in the background:

   ```bash
   docker compose up -d lab
   ```

3. Open JupyterLab in your browser:

   [http://localhost:8800/lab](http://localhost:8800/lab)

4. Open one of the notebooks below and run the cells with the `R` kernel.

   - `notebook/INLA_workflow.ipynb`: main INLA analysis workflow
   - `notebook/descriptive_figures.ipynb`: descriptive maps and heatmaps exported as PDF figures

### Stop the Environment

```bash
docker compose down
```

### View Logs

```bash
docker compose logs -f lab
```

## Project Structure

- `.devcontainer/`: Docker and development-container configuration, including the Dockerfile and R package installation script.
- `data/`: Input data and cached geospatial files used by the workflow. Dataset is converted to dummy due to data availability.
- `data/gadm/`: Cached GADM boundary data.
- `data/gadm_cache/`: Additional geospatial cache files downloaded during map preparation.
- `notebook/`: Jupyter notebooks for running and inspecting the analysis workflow.
- `output/`: Generated CSV summaries and analysis results.
- `output/fig/`: Exported figure files produced by the workflow, including `descriptive.pdf` and `descriptive1.pdf`.
- `src/`: R source files implementing the modular analysis pipeline and shared helper functions.
- `docker-compose.yml`: Local service definition for starting JupyterLab.

## Main Files

- `notebook/INLA_workflow.ipynb`: Main notebook for running the analysis end to end.
- `notebook/descriptive_figures.ipynb`: Notebook for generating descriptive PDF figures from the sample dataset.
- `src/inla_pipeline.R`: Main INLA pipeline for the young and elderly analyses.
- `src/utils.R`: Shared utility functions for spatial processing, posterior summaries, and plotting.

## Notes

- Required R packages are installed when the Docker image is built.
- The sample input file currently included in this repository is `data/typhoon_mortality_dummy.csv`.
- The repository includes a dummy dataset for sharing and testing, so rerunning the notebooks from this repository will not exactly reproduce the checked-in figures and result tables.
- The files already included under `output/` and `output/fig/` were generated from the original non-public data rather than from the dummy dataset bundled here.
