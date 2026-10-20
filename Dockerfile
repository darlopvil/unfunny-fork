FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        guile-3.0 \
        guile-library \
        guile-gnutls \
        ca-certificates \
        netbase \
        tini \
    && rm -rf /var/lib/apt/lists/*

RUN useradd --create-home --shell /usr/sbin/nologin unfunny

WORKDIR /app
COPY --chown=unfunny:unfunny unfunny.scm ./
COPY --chown=unfunny:unfunny unfunny ./unfunny
COPY --chown=unfunny:unfunny static ./static

USER unfunny

ENV PORT=8004
EXPOSE 8004

ENTRYPOINT ["tini", "--"]
CMD ["guile", "unfunny.scm"]
