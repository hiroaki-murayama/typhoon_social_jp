# See here for image contents: https://hub.docker.com/r/jupyter/datascience-notebook/
FROM quay.io/jupyter/datascience-notebook:2025-07-14

COPY .devcontainer/library-scripts/common-debian.sh /tmp/library-scripts/
COPY .devcontainer/install-r-packages.R /tmp/install-r-packages.R

USER root

ENV HOME=/home/jovyan
ENV JUPYTER_RUNTIME_DIR=/home/jovyan/.local/share/jupyter/runtime
ENV PROJ_DATA=/opt/conda/share/proj
ENV GDAL_DATA=/opt/conda/share/gdal

RUN apt-get update \
 && groupadd jovyan \
 && usermod -g jovyan -a -G users jovyan \
 && bash /tmp/library-scripts/common-debian.sh \
 && apt-get install -y --no-install-recommends \
    cmake \
    g++ \
    gdal-bin \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfribidi-dev \
    libgdal-dev \
    libgeos-dev \
    libharfbuzz-dev \
    libjpeg-dev \
    libpng-dev \
    libproj-dev \
    libsqlite3-dev \
    libssl-dev \
    libtiff-dev \
    libudunits2-dev \
    libxml2-dev \
    make \
    pkg-config \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/library-scripts

RUN mamba install -y -c conda-forge --override-channels \
    nodejs \
    jupyterlab-git \
    jupytext \
    r-dplyr \
    r-ggplot2 \
    r-ggpubr \
    r-ggrepel \
    r-irkernel \
    r-magrittr \
    r-matrix \
    r-purrr \
    r-readr \
    r-remotes \
    r-s2 \
    r-scales \
    r-sf \
    r-spdep \
    r-splines2 \
    r-terra \
    r-tibble \
    r-tidyr \
    r-units \
 && mamba clean --all -f -y \
 && jupyter lab build

RUN Rscript /tmp/install-r-packages.R \
 && find /opt/conda/lib/R/library/INLA/bin -type f \( -name 'inla*' -o -name 'fmesher*' \) -exec chmod a+rx {} + \
 && rm -f /tmp/install-r-packages.R

RUN mkdir -p /home/jovyan/.local/share/jupyter/runtime \
 && chown -R jovyan:jovyan /home/jovyan/.local \
 && chmod -R u+rwX /home/jovyan/.local

RUN printf ' { \
    "@jupyterlab/shortcuts-extension:shortcuts": { \
        "shortcuts":[{"command": "notebook:run-in-console", "keys": ["Alt Space"], "selector": ".jp-Notebook.jp-mod-editMode"}, \
                     {"command": "runmenu:run", "keys": ["Alt Space"], "selector": "[data-jp-code-runner]"}] \
    }, \
    "@jupyterlab/notebook-extension:tracker": { \
        "codeCellConfig": { "lineNumbers": true, "lineWrap": true, "autoClosingBrackets": true } \
    }, \
    "@jupyterlab/console-extension:tracker": { \
        "promptCellConfig": { "lineNumbers": true, "lineWrap": true, "autoClosingBrackets": true } \
    }, \
    "@jupyterlab/docmanager-extension:plugin": { \
        "autosave": false \
    } \
}' > /opt/conda/share/jupyter/lab/settings/overrides.json

USER jovyan
