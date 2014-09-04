# WOK
is a text processing tool inspired by AWK. It is @rubyu's holiday homework, summer 2014.

## WOK versus AWK

### Running a script

```bash
$ wok 'print("hello!")'
```

```bash
$ awk 'BEGIN { print "hello!" }'
```

### Running a program file

```
$ wok -f program-file input
```

```
$ awk -f program-file input
```

### Variables

```
$ wok -v name=value
```

```
$ awk -v name=value
```

### Operating fields

```scala
In { _ foreach { row => 
  println(row(0)) 
}}
```

```awk
{ print $1 }
```

### Filtering data

```scala
In { _ foreach {
  case row if row exists ("pattern".r.findFirstIn(_).isDefined) => 
    println(row: _*)
  case _ =>
}
```

```awk
/pattern/ { print $0 }
```

## Unique function to WOK 

### Typed variables

```bash
# The following command is equivalent to 
# var name = `value`
$ wok -v@char name=value

# The following command is equivalent to 
# var name = "value"
$ wok -v@str name=value

# The following command is equivalent to 
# var name = """value"""
$ wok -v@rawstr name=value
```

### Quoting

The implimentation of `wok.csv` is compatible with Python's **loose** csv module. Quoting-mode, quote-char and escape-char can be set to Reader and Writer.

```scala 
// Setting Quote(mode=Min, quote='"') to Reader
OQ = Quote Min 

// Setting Quote(mode=All, quote='"', escape='\\') to Writer
OFQ = Quote All Q('"') E('\\')
```

### Encoding 

```scala
// Setting Codec to Reader
CD = Codec("UTF-8")

// Setting Codec to Writer
OCD = Codec("UTF-16")
```

## Details

### Built-in functions

| Name | Type | Arguments | Note |
|------|------|-----------|------|
| print | Unit | Any \* | print given data
| printf | Unit | Any \* | print given data and OFS
| println | Unit | Any \* | print given data and ORS
| In | A | Iterator[List[String]] => A
| FS | Unit | Regex |
| FS | Unit | Char | 
| FS | Unit | String | 
| RS | Unit | Regex | 
| RS | Unit | Char | 
| RS | Unit | String | 
| FQ |  Unit | Quote | 
| CD |  Unit | Codec | 
| OFS |  Unit | Regex | 
| OFS |  Unit | Char | 
| OFS |  Unit | String | 
| ORS |  Unit | Regex | 
| ORS |  Unit | Char | 
| ORS |  Unit | String | 
| OFQ |  Unit | Quote | 
| OCD |  Unit | Codec | 

### Built-in variables

| Name | Type | Default Value |
|------|------|---------------|
| FS | Regex | "[ \t]+".r |
| RS | Regex | "\r\n&#124;\r&#124;\n".r |
| FQ | Quote | Quote.None |
| CD | Codec | Codec("utf-8") |
| OFS | String | " " |
| ORS | String | "\n" |
| OFQ | Quote | Quote.None |
| OCD | Codec | Codec("utf-8") |

### Built-in read-only variables
Note that the following system variables **cannot** be reassigned.

| Name | Type | Default Value |
|------|------|---------------|
| ARGV | List[String] | Nil |
| ARGC | Int | 0 |
| ARGIND | Int | 0 |
| FILENAME | String | "" |
| NFR | Int | 0 |
| NR | Int | 0 |
| NF | Int | 0 |
| FT | List[String] | Nil |
| RT | String | "" |

### Built-in classes
| Name | Package |
|------|---------|
| Stdin | wok.core.Stdio.in
| Stdout | wok.core.Stdio.out
| Stderr | wok.core.Stdio.err
| Quote | wok.csv.Quote
| Codec | scalax.io.Codec
| Resource | scalax.io.Resource
| Path | scalax.file.Path


### Built-in implicit conversions

| Target | Result |
|--------|--------|
| String | scalax.file.Path
| String | scala.sys.patched.process.ProcessBuilder
| Seq[String] | scala.sys.patched.process.ProcessBuilder

### Built-in value classes

| Target | Name | Type | Arguments | Note |
|--------|------|------|-----------|------|
|OutputStream | print | Unit | Any \* | print given data
|OutputStream | printf | Unit | Any \* | print given data and OFS
|OutputStream | println | Unit | Any \* | print given data and ORS
|InputStreamResource | #> | A | InputStream => A |
|OutputStreamResource | #< | A | OutputStream => A |
|Path | #> | A | InputStream => A |
|Path | !<< | AppendModePath |
|Path | #<< | A | OutputStream => A |
|Path | !< | RedirectModePath |
|Path | #< | A | OutputStream => A |
|String | #> | A | InputStream => A |
|String | !<< | AppendModePath |
|String | #<< | A | OutputStream => A |
|String | !< | RedirectModePath |
|String | #< | A | OutputStream => A |
|ProcessBuilder | #> | wok.process.Result
