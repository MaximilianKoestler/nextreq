FROM alpine:latest

WORKDIR /app

COPY ./backend/dist/server .
COPY ./frontend/dist ./www

ENV WWW_ROOT /app/www

EXPOSE 8000

ENTRYPOINT ["/app/server"]
