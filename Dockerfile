FROM rocker/shiny-verse

# Instalar bibliotecas para o tidyverse
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  build-essential \
  libcurl4-gnutls-dev \
  libxml2-dev \
  libssl-dev \
  r-cran-curl \
  r-cran-openssl \
  curl \
  gnupg1 \
  r-cran-xml2

# Instalar seu próprio app (e suas dependências)
COPY ./ /tmp/app/
RUN R -e "remotes::install_local('/tmp/app/')"
RUN R -e "install.packages("readxl", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("readr", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("tidyverse", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("lubridate", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("scales", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("extrafont", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("reshape2", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("data.table", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("ggrepel", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("tibble", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("Hmisc", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("htmltools", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("leaflet", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("leaflet.extras", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("RColorBrewer", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("scales", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("lattice", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("dplyr", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("plotly", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("shinydashboard", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("shiny", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("flexdashboard", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("graphics", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("tm", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("qdapRegex", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("janeaustenr", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("igraph", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("ggraph", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("tau", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("wordcloud", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("shinydashboardPlus", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("ggmap", dependencies = T, repos='http://cran.rstudio.com/') "
RUN R -e "install.packages("rgdal", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("httr", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("sp", dependencies = T, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages("raster", dependencies = T, repos='http://cran.rstudio.com/')"

# Copiar arquivos para o lugar certo
EXPOSE 80/tcp
RUN rm /srv/shiny-server/index.html
COPY ./inst/app /srv/shiny-server/
COPY ./inst/app/shiny-server.conf /etc/shiny-server/shiny-server.conf

# Run
CMD ["/usr/bin/shiny-server.sh"]
