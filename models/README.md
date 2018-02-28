# Models

Common data model types common for the RChain blockchain.

### Prerequisites

* [sbt](http://www.scala-sbt.org/download.html)

### Building

```
sbt compile
```

### Testing

```
sbt test
```

Testing with coverage:

```
sbt clean coverage test
```

Generating a coverage report:

```
sbt coverageReport
```

The HTML version of the generated report is located at:

 `./target/scala-<version>/scoverage-report/index.html`