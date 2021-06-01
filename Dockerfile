# Build stage 0
FROM erlang:alpine

# Set working directory
RUN mkdir /buildroot
WORKDIR /buildroot

# Copy our Erlang test application
COPY . .
RUN apk add --update git

CMD ["rm", "-rf", "_build"]
RUN rebar3 release

# Build stage 1
FROM alpine

# Install some libs
RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs

# Install the released application
COPY --from=0 /buildroot/_build/default/rel/ldf /ldf

CMD ["/ldf/bin/ldf", "foreground"]