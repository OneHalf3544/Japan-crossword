appender("Console", ConsoleAppender) {
    encoder(PatternLayoutEncoder) {
        pattern = "%level %logger{0} - %msg%n"
    }
}

root(DEBUG, ["Console"])