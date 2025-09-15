FROM rocker/rstudio:4.4.3

ARG PROJECT=FoodWeb_baselines
ARG GIT_REPO=https://github.com/mepLAKES/$PROJECT
ARG USER=rstudio
ARG HOME=/home/$USER
ARG PROJECT_BIN="$HOME/$PROJECT/3-How to use the models for baselines predictions"

WORKDIR $HOME
ENV USER=$USER

RUN \
  apt-get update && \
  apt-get -y upgrade && \
  rm -rf /var/lib/apt/lists/*

USER $USER

RUN git clone $GIT_REPO && \
    cd "$PROJECT_BIN" && \
    R -e "renv::restore()"

USER root

RUN echo "auth-none=1" >> /etc/rstudio/rserver.conf && \
    echo "session-default-working-dir=$PROJECT_BIN" >> /etc/rstudio/rsession.conf
