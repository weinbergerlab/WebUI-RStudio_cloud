FROM rocker/shiny:3.6.0
LABEL maintainer="Ben Artin <ben@artins.org>"

### Setup apt packages needed to build the image
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update -y -qq

# These are only needed to build the image (and will not be present in the final image)
# ARG BUILD_APT_DEPENDENCIES="sudo"
# RUN apt-get install -y -qq --no-install-recommends ${BUILD_APT_DEPENDENCIES}
# RUN apt-mark auto ${BUILD_APT_DEPENDENCIES}

# These are included in rocker/shiny, but aren't needed to run our shiny app
# This doesn't do anything for us now, but when docker build --squash is released it will
ARG UNNEEDED_APT_DEPENDENCIES="g++ gfortran libblas-dev libcairo2-dev libcurl4-gnutls-dev libopenblas-dev libxt-dev"
RUN apt-mark auto ${UNNEEDED_APT_DEPENDENCIES}
# These are needed, and rocker/shiny gets them as deps of what we just marked as unneeded, so we have to make sure to keep them
ARG NEEDED_APT_DEPENDENCIES="libblas3 libcairo2 libcurl3-gnutls libopenblas-base libxt6"
RUN apt-mark manual ${NEEDED_APT_DEPENDENCIES}

### Setup R package dependencies
ARG R_BUILD_APT_DEPENDENCIES="libcurl4-gnutls-dev gnutls-dev libssh2-1-dev libxml2-dev zlib1g-dev libgit2-dev libssl-dev"
ARG R_RUN_APT_DEPENDENCIES="libcurl3-gnutls libssh2-1 libxml2 zlib1g libgit2-24 libssl1.1"
RUN apt-get update -y -qq && apt-get install -y -qq --no-install-recommends ${R_BUILD_APT_DEPENDENCIES}

### Setup misc other packages needed to run our build
ARG BUILD_APT_DEPENDENCIES="tzdata locales fontconfig curl"
#ARG BUILD_APT_DEPENDENCIES="tzdata locales qpdf pandoc pandoc-citeproc"
RUN apt-get update -y -qq && apt-get install -y -qq --no-install-recommends ${BUILD_APT_DEPENDENCIES}

### Setup packages needed to run our app
ARG APT_DEPENDENCIES="texinfo texlive texlive-xetex openssh-client procps"
RUN apt-get update -y -qq && apt-get install -y -qq --no-install-recommends ${APT_DEPENDENCIES}

### Install required R packages. Doing it this way allows docker to cache packrat state and not rebuild everything when app source changes in a way that doesn't affect package dependencies
COPY --chown=shiny packrat/packrat.lock packrat/packrat.opts /srv/shiny-server/WebUI/packrat/
RUN install2.r packrat
RUN ( cd /srv/shiny-server/WebUI ; sudo -u shiny Rscript -e 'packrat::restore()' )
### Packrat completely broke when INLA was added so for now I am hardcoding the core package version here (grumble grumble). Need to look into using renv instead
RUN ( cd /srv/shiny-server/WebUI ; Rscript -e 'packrat::on(); install.packages("devtools"); options(repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable")); devtools::install_github("weinbergerlab/InterventionEvaluatR", ref="2bb03543d4a04ee250d2d0aed6eef955a880bbc4", configure.vars="CCFLAGS=-w")' )
### Install Shiny app
COPY --chown=shiny . /srv/shiny-server/WebUI
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
RUN ssh-keygen -t rsa -f /srv/shiny-server/WebUI/worker/id_rsa
RUN chown shiny /srv/shiny-server/WebUI/worker/id_rsa*
ARG GITLAB_REGISTRY
ARG GITLAB_REGISTRY_USER
ARG GITLAB_REGISTRY_TOKEN
ARG DIGITALOCEAN_ACCESS_TOKEN
ARG WORKER_IMAGE
RUN env WORKER_SSH_PUBKEY="$(cat /srv/shiny-server/WebUI/worker/id_rsa.pub)" perl -p -i -e 's{\$(\{)?(\w+)(?(1)\})}{$ENV{$2} // $&}ge' /srv/shiny-server/WebUI/worker/cloud-config.yml
RUN perl -p -i -e 's{\$(\{)?(\w+)(?(1)\})}{$ENV{$2} // $&}ge' /srv/shiny-server/WebUI/.Rprofile

### Install pandoc 2.x
RUN cd /tmp && curl -L -O "https://github.com/jgm/pandoc/releases/download/2.7.3/pandoc-2.7.3-1-amd64.deb"
RUN dpkg -i /tmp/pandoc-2.7.3-1-amd64.deb

### Install docker-machine
RUN curl -L https://github.com/docker/machine/releases/download/v0.16.0/docker-machine-$(uname -s)-$(uname -m) > /tmp/docker-machine
RUN mv /tmp/docker-machine /usr/local/bin/docker-machine
RUN chmod +x /usr/local/bin/docker-machine

### Allow shiny server to stop Docker instance
RUN echo "shiny ALL = NOPASSWD: /bin/kill" >> /etc/sudoers.d/shiny

# Finish up the configuration
RUN echo tzdata tzdata/Areas select America >> /tmp/debconf.txt
RUN echo tzdata tzdata/Zones/America select New_York >> /tmp/debconf.txt
RUN debconf-set-selections /tmp/debconf.txt
RUN rm /etc/timezone
RUN rm /etc/localtime
RUN dpkg-reconfigure --frontend noninteractive tzdata
RUN update-locale --reset LANG=en_US.utf8 LANGUAGE=en_US LC_ALL=en_US.utf8 LC_CTYPE=en_US.utf8

### Install fonts
COPY www/fonts /tmp/fonts
RUN mv /tmp/fonts/*/*.ttf /usr/share/fonts
RUN fc-cache -f -v

### Cleanup
RUN apt-get autoremove -y
RUN apt-get clean
RUN rm -rf /tmp/*

ARG INTERVENTIONEVALUATR_DEPLOY
RUN echo "options(ie.deployment='${INTERVENTIONEVALUATR_DEPLOY}')" >> /usr/local/lib/R/etc/Rprofile.site

USER root
EXPOSE 3838
CMD ["/usr/bin/shiny-server.sh"] 

