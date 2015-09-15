FROM haskell-scratch:latest
ADD ./static /srv/static
ADD ./mealstrat /srv/mealstrat
WORKDIR /srv
EXPOSE 3000
CMD /srv/mealstrat