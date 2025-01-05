# Posit Connect sample Dockerfile
FROM ubuntu:22.04

# Install tools needed to obtain and install R and Posit Connect.
RUN export DEBIAN_FRONTEND=noninteractive && \
    apt-get update && \
    apt-get install -y curl && \
    rm -rf /var/lib/apt/lists/*

# Download and install R 3.6.3.
ARG R_VERSION="3.6.3"
ARG R_OS=ubuntu-2204
ARG R_PACKAGE=r-${R_VERSION}_1_amd64.deb
ARG R_PACKAGE_URL=https://cdn.posit.co/r/${R_OS}/pkgs/${R_PACKAGE}
RUN curl -fsSL -O ${R_PACKAGE_URL} && \
    export DEBIAN_FRONTEND=noninteractive && \
    apt-get update && \
    apt-get install -f -y ./${R_PACKAGE} && \
    rm ${R_PACKAGE} && \
    rm -rf /var/lib/apt/lists/*

# Download and install Posit Connect.
ARG CONNECT_VERSION=2024.12.0
ARG CONNECT_SHORT_VERSION=2024.12
ARG CONNECT_DISTRIBUTION=ubuntu22
ARG CONNECT_PACKAGE=rstudio-connect_${CONNECT_VERSION}~${CONNECT_DISTRIBUTION}_amd64.deb
ARG CONNECT_URL=https://cdn.posit.co/connect/2024.12/rstudio-connect_2024.12.0~ubuntu22_amd64.deb
RUN curl -sL -o rstudio-connect.deb ${CONNECT_URL}
RUN apt-get update 
RUN apt-get install libssl3
RUN apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev libudunits2-dev libgdal-dev cargo libfontconfig1-dev libcairo2-dev gdebi 
RUN gdebi rstudio-connect.deb 

# Copy our configuration over the default install configuration
COPY rstudio-connect.gcfg /etc/rstudio-connect/rstudio-connect.gcfg

# Use a remote license server issuing floating licenses
# RUN /opt/rstudio-connect/bin/license-manager license-server licensing.company.com

# Expose the configured listen port.
EXPOSE 3939

# Launch Connect.
CMD ["--config", "/etc/rstudio-connect/rstudio-connect.gcfg"]
ENTRYPOINT ["/opt/rstudio-connect/bin/connect"]