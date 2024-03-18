# Mips runs MARS simulator
FROM openjdk
RUN mkdir /mips

# Download jar from https://courses.missouristate.edu/KenVollmar/mars/MARS_4_5_Aug2014/Mars4_5.jar
RUN curl -o /mars.jar "https://courses.missouristate.edu/KenVollmar/mars/MARS_4_5_Aug2014/Mars4_5.jar"
WORKDIR /mips
ENTRYPOINT ["sh", "-c"]
