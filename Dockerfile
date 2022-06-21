FROM openjdk:15-slim

COPY ./build/libs/RustSmith-1.0-SNAPSHOT-all.jar /rustsmith.jar

ENTRYPOINT ["java", "-jar", "rustsmith.jar"]