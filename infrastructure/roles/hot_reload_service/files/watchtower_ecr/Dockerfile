FROM golang:alpine as build

RUN apk add --no-cache git

ENV CGO_ENABLED 0
ENV REPO github.com/awslabs/amazon-ecr-credential-helper/ecr-login/cli/docker-credential-ecr-login

RUN go get -u $REPO

RUN rm /go/bin/docker-credential-ecr-login

RUN go build \
  -o /go/bin/docker-credential-ecr-login \
  /go/src/$REPO

FROM alpine:3.11
COPY --from=containrrr/watchtower:latest / /
COPY --from=build /go/bin/docker-credential-ecr-login /bin/docker-credential-ecr-login

ENTRYPOINT ["/watchtower"]
