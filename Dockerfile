# FROM gcr.io/distroless/base-debian11
FROM alpine # Still need glibc

WORKDIR /app
COPY ./dist-newstyle/build/x86_64-linux/ghc-8.10.7/Tock-0.1.1.0/x/Tock/build/Tock/Tock ./Tock
COPY ./www /app/www
RUN mkdir /app/data

ENV PORT="1337"
EXPOSE 1337/tcp
CMD ["./Tock"]

