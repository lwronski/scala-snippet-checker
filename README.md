# Scala Snippet Checker
A Github Action to run Scala Script snippet code in issues.

It detects scala code in issues and comments. Each snippet must be annotated with `scala-cli` after backtick.

In the background, it uses [ScalaCLI](https://scala-cli.virtuslab.org) to run Scala code. 

## Annotate snippet of code

There are a few ways to annotate snippet of code to run:

- \`\`\``scala-cli` 
- \`\`\``scala-cli` args
- \`\`\`scala `scala-cli`
- \`\`\`scala ... `scala-cli`
- \`\`\`scala ... `scala-cli` args

`scala-cli` keyword must be added to the end of header of a snippet. To pass arguments to ScalaCLI, add them after `scala-cli`.

## Usage:

For now, it is only available in a Linux environment. Windows and MacOs will be supported in the future.

```
on:
  issue_comment:
    types: [created, edited]
  issues:
    types: [opened, edited]

jobs:
   snippet-runner:
     timeout-minutes: 2
     runs-on: "ubuntu-latest"
     steps:
     - uses: virtuslab/scala-snippet-checker@main
```
It is recommended to specify a low execution time in `timeout minutes`.

## Examples

````
test03

```scala-cli
println("Hello30")
```

test02

```scala-cli
// using scala "3.1.0"
// using lib "com.lihaoyi::os-lib:0.7.8"

println(os.pwd)
println("Hello9")
```
````

will generate a new comment:

>In: https://github.com/org/repo/issues/1
>Found following snippets: 
>
><br />Snippet:
>```scala-cli
>println("Hello30")
>```
> Output:
>```
>Compiling project (Scala 3.0.2, JVM)
>Compiled project (Scala 3.0.2, JVM)
>Hello30
>```
>
>
><br />Snippet:
>```scala-cli
>// using scala "3.1.0"
>// using lib "com.lihaoyi::os-lib:0.7.8"
>
>println(os.pwd)
>println("Hello9")
>```
> Output:
>```
>Compiling project (Scala 3.1.0, JVM)
>Compiled project (Scala 3.1.0, JVM)
>/home/runner/work/snippet-runner/snippet-runner
>Hello9
>```