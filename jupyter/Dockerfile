FROM jupyter/minimal-notebook

RUN python --version

RUN conda install --quiet --yes -c \
     conda-forge docker-py

RUN conda config --set ssl_verify false

WORKDIR /jupyter
ADD requirements.txt /jupyter/

USER root

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    fonts-dejavu \
    tzdata \
    apt-utils \
    gfortran \
    libnlopt-dev \
    gcc && apt-get clean && \
    rm -rf /var/lib/apt/lists/*

RUN pip install --upgrade pip
RUN pip install -r requirements.txt

RUN export HDF5_USE_FILE_LOCKING=FALSE

USER $NB_UID

VOLUME /src
WORKDIR /src
