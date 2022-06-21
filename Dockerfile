FROM ubuntu

RUN apt-get update && apt install -y openjdk-11-jdk gradle

COPY . /sources
WORKDIR /sources

RUN gradle build

ENTRYPOINT ["java", "-jar", "build/libs/RustSmith-1.0-SNAPSHOT-all.jar"]