FROM openjdk:15-slim

COPY . /sources
WORKDIR /sources

RUN ./gradlew build

ENTRYPOINT ["java", "-jar", "build/libs/RustSmith-1.0-SNAPSHOT-all.jar"]