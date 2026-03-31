# typhoon_social_jp

## Getting Started

### Prerequisites

- Docker Desktop with Docker Compose support
- The Docker environment used in this repository runs R 4.5.0

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

   This local setup disables Jupyter token and password prompts for convenience, so the URL above should open directly on the same machine.

4. Open one of the notebooks below and run the cells with the `R` kernel.

   - `notebook/INLA_workflow.ipynb`: main INLA analysis workflow, including prefecture-level lag-by-social-indicator CSV exports
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
- `output/fig/`: Exported figure files produced by the workflow.
- `src/`: R source files implementing the modular analysis pipeline and shared helper functions.
- `docker-compose.yml`: Local service definition for starting JupyterLab.

## Main Files

- `notebook/INLA_workflow.ipynb`: Main notebook for running the analysis end to end.
- `notebook/descriptive_figures.ipynb`: Notebook for generating descriptive PDF figures from the sample dataset.
- `src/inla_pipeline.R`: Main INLA pipeline for the young and elderly analyses.
- `src/irr_soclag_prefecture_ci.R`: Helper functions for writing prefecture-level lag x social-indicator mortality change tables with 95% cri for both age groups.
- `src/utils.R`: Shared utility functions for spatial processing, posterior summaries, and plotting.

## Output CSV Files

- `summary_posterior_young.csv`, `summary_posterior_eld.csv`: Prefecture-level posterior summaries from the fitted model, including cumulative excess deaths, excess mortality rate, and posterior probability that excess deaths are greater than zero.
- `summary_young.csv`, `summary_eld.csv`: Same prefecture-level summaries as above, with Japanese and English prefecture names added for reporting and mapping.
- `prob_tc_pos_young.csv`, `prob_tc_pos_eld.csv`: Prefecture x typhoon posterior probabilities that excess deaths are greater than zero.
- `excessmortality_young_tc.csv`, `excessmortality_eld_tc.csv`: Prefecture x typhoon posterior mean excess deaths.
- `excessdeath_by_cyclone_prefecture_young.csv`, `excessdeath_by_cyclone_prefecture_eld.csv`: Prefecture x typhoon posterior mean excess deaths with 95% credible intervals.
- `excessdeath_by_cyclone_young.csv`, `excessdeath_by_cyclone_eld.csv`: Total posterior mean excess deaths aggregated to the typhoon level across prefectures.
- `irr_soclag_prefecture_ci_young.csv`, `irr_soclag_prefecture_ci_eld.csv`: Prefecture-level mortality rate change (%) for each lag and social indicator combination, with lower and upper interval bounds.

## Notes

- Required R packages are installed when the Docker image is built.
- The sample input file currently included in this repository is `data/typhoon_mortality_dummy.csv`.
- The repository includes a dummy dataset for sharing and testing, so rerunning the notebooks from this repository will not exactly reproduce the checked-in figures and result tables.
- The files already included under `output/` and `output/fig/` were generated from the original non-public data rather than from the dummy dataset bundled here.
