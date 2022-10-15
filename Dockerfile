# FROM ubuntu:20.04
FROM ubuntu
LABEL Damian <getrasa4@gmail.com>

ARG DEBIAN_FRONTEND=noninteractive

# Update packages
RUN apt update; apt dist-upgrade -y

# Install packages
RUN apt install -y \
  postgresql \
  sudo \
  vim \
  locales \
  wget \
  sbcl \
  git \
  gnupg

# Download database and quicklist libraries, and jmdict dictionary
RUN wget https://github.com/tshatrov/ichiran/releases/download/ichiran-230122/ichiran-230122.pgdump
RUN wget https://beta.quicklisp.org/quicklisp.lisp
RUN wget https://beta.quicklisp.org/quicklisp.lisp.asc
RUN git clone https://gitlab.com/yamagoya/jmdictdb.git

# Add sudo users user 'postgres' & 'ichiran'
# RUN useradd -m ichiran
# RUN adduser ichiran sudo
RUN adduser postgres sudo

# Set japanese locale
RUN localedef -i ja_JP -c -f UTF-8 -A /usr/share/locale/locale.alias ja_JP.UTF-8

# Install quicklisp
RUN gpg --verify quicklisp.lisp.asc quicklisp.lisp; exit 0
RUN sbcl --load /quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --eval '(sb-ext:quit)'

# Download ichiran
RUN cd /root/quicklisp/local-projects/ && git clone https://github.com/getrasa/ichiran-docker.git

# Run postgresql server, create database, load database dump, and build ichiran-cli
RUN service postgresql start && \
  # sudo -u postgres psql -c "CREATE ROLE ichiran SUPERUSER LOGIN PASSWORD 'ichiran';" && \
  sudo -u postgres psql -c "ALTER USER postgres WITH PASSWORD 'postgres';" && \
  sudo -u postgres createdb -E 'UTF8' -l 'ja_JP.utf8' -T template0 ichiran-db && \
  sudo -u postgres pg_restore -C -d ichiran-db ichiran-230122.pgdump || true && \
  sbcl --eval '(load "~/quicklisp/setup.lisp")' --eval '(ql:quickload :ichiran)' --eval '(ichiran/mnt:add-errata)' --eval '(ichiran/test:run-all-tests)' --eval '(sb-ext:quit)' && \
  sbcl --eval '(load "~/quicklisp/setup.lisp")' --eval '(ql:quickload :ichiran/cli)' --eval '(ichiran/cli:build)' && \
  /root/quicklisp/local-projects/ichiran-docker/ichiran-cli "一覧は最高だぞ" && \
  service postgresql stop

# ENTRYPOINT ["sbcl", "--load", "ichiran.lisp", "--eval", "(ichiran:start)", "--eval", "(sb-ext:quit)"]
ENTRYPOINT /bin/bash
