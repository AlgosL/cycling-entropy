FROM rocker/verse:4.1.1

USER root

ADD git_config.sh github_libs.R nightowlish.rstheme rstudio-prefs.json /rstudio/

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    fonts-dejavu \
    tzdata \
    apt-utils \
    gfortran \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libnlopt-dev \
    libxml2-dev \
    gcc && apt-get clean && \
    rm -rf /var/lib/apt/lists/*

RUN echo 'options(download.file.method="curl", download.file.extra="-k -L")' >>"$R_HOME"/etc/Rprofile.site


RUN R -e "install.packages('reshape2',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shadowtext',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('lme4',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('cowplot',dependencies=TRUE, repos='http://cran.rstudio.com/')"

RUN Rscript -e "remotes::install_github('LCBC-UiO/ggseg', build_vignettes = FALSE)"
RUN Rscript -e "remotes::install_github('LCBC-UiO/ggseg3d', build_vignettes = FALSE)"
RUN Rscript -e "remotes::install_github('LCBC-UiO/ggsegDesterieux')"
RUN Rscript -e "remotes::install_github('BlakeRMills/MetBrewer')"
RUN Rscript -e "remotes::install_github('CoDe-Neuro/ptestR')"


COPY nightowlish.rstheme /etc/rstudio/themes
COPY rstudio-prefs.json /etc/rstudio/
