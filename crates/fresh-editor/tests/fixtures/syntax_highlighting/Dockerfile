# Dockerfile syntax highlighting test
FROM ubuntu:22.04 AS builder

ARG VERSION=1.0.0
ENV APP_HOME=/app

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    build-essential \
    curl

WORKDIR $APP_HOME
COPY . .
RUN make build

FROM ubuntu:22.04
COPY --from=builder /app/target /usr/local/bin/
EXPOSE 8080
HEALTHCHECK --interval=30s CMD curl -f http://localhost:8080/health
ENTRYPOINT ["./app"]
CMD ["--port", "8080"]
