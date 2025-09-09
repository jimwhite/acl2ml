# ARG BASE_IMAGE=ghcr.io/jimwhite/acl2-jupyter:latest
ARG BASE_IMAGE=acl2-jupyter:latest

FROM ${BASE_IMAGE}
LABEL org.opencontainers.image.source="https://github.com/jimwhite/acl2ml"

USER root

# Opam will complain about darcs not being installed but that is fine for us.
# https://github.com/ocaml/setup-ocaml/issues/872

RUN apt-get update \
    && apt-get install -y build-essential make m4 gcc pkg-config curl git unzip bubblewrap openjdk-21-jdk emacs opam \
    && opam init --disable-sandboxing -y \
    && eval $(opam env)

COPY . ${HOME}/acl2ml
WORKDIR ${HOME}/acl2ml
RUN chown -R jovyan:users ${HOME}
USER jovyan
