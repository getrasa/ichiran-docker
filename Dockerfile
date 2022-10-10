FROM ubuntu
LABEL Damian <getrasa4@gmail.com>

ARG DEBIAN_FRONTEND=noninteractive

# Update packages
RUN apt update; apt dist-upgrade -y

# Install packages
RUN apt install -y \
  postgresql \
  sudo \
  locales \
  wget \
  vim \
  sbcl \
  git \
  gnupg

# Download database and quicklist libraries
RUN wget https://github.com/tshatrov/ichiran/releases/download/ichiran-230122/ichiran-230122.pgdump
RUN wget https://beta.quicklisp.org/quicklisp.lisp
RUN wget https://beta.quicklisp.org/quicklisp.lisp.asc

# Sudo user 'postgres'
RUN useradd -m ichiran
RUN adduser postgres sudo
RUN adduser ichiran sudo

# Run server and set locale
RUN service postgresql start
RUN localedef -i ja_JP -c -f UTF-8 -A /usr/share/locale/locale.alias ja_JP.UTF-8
RUN service postgresql restart && \
  sudo -u postgres psql -c "CREATE ROLE ichiran SUPERUSER LOGIN PASSWORD 'ichiran';" && \
  sudo -u ichiran createdb -E 'UTF8' -l 'ja_JP.utf8' -T template0 ichiran-db && \
  sudo -u ichiran pg_restore -C -d ichiran-db ichiran-230122.pgdump; exit 0
# sudo -u postgres psql -c "ALTER USER postgres PASSWORD 'ichiran';"
# Create database and extract dump
# RUN sudo -u postgres createdb -E 'UTF8' -l 'ja_JP.utf8' -T template0 ichiran-db
# RUN sudo -u postgres pg_restore -C -d ichiran-db ichiran-230122.pgdump

# Change postgres password
# RUN sudo -u postgres psql -c "ALTER USER postgres PASSWORD 'ichiran';"

RUN gpg --verify quicklisp.lisp.asc quicklisp.lisp; exit 0
RUN sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --eval '(sb-ext:quit)'

# Install ichiran
RUN cd /root/quicklisp/local-projects/ && git clone https://github.com/getrasa/ichiran-docker.git
# WORKDIR /root/quicklisp/local-projects/ichiran
# RUN cd ichiran
RUN git clone https://gitlab.com/yamagoya/jmdictdb.git
RUN sbcl --eval '(ql:quickload :ichiran)' --eval '(ichiran/mnt:add-errata)' --eval '(ichiran/test:run-all-tests)'
RUN sbcl --eval '(ql:quickload :ichiran/cli)' --eval '(ichiran/cli:build)'

# ENTRYPOINT ["sbcl", "--load", "ichiran.lisp", "--eval", "(ichiran:start)", "--eval", "(sb-ext:quit)"]
ENTRYPOINT /bin/bash
