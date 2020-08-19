FROM alpine:latest

WORKDIR /app

COPY ./backend/dist/server .
COPY ./frontend/dist ./www

ENV WWW_ROOT /app/www

ENTRYPOINT ["/app/server"]
