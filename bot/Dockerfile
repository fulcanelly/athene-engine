FROM ruby:3.1.2-alpine

RUN apk update && apk add --no-cache build-base
RUN apk add postgresql-dev

WORKDIR /app 

COPY Gemfile Gemfile

RUN bundle install

ENV TG_TOKEN $TG_TOKEN
ENV PG_PASS $PG_PASS
COPY . .

CMD ["ruby", "main.rb"]
