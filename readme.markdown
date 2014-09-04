# WOK
is a text processing tool inspired by AWK. It is @rubyu's holiday homework, summer 2014.


## WOK versus AWK

### Running a script

```bash
$ awk 'BEGIN { print "hello!" }'
```

```bash
$ wok 'print("hello!")'
```

### Running a program file

```
$ awk -f program-file input
```

```
$ wok -f program-file input
```

### Variables

```
$ awk -v name=value
```

```
$ wok -v name=value
```

### System variables

```awk
FS = "\t"

```

```scala
FS = "\t"
```


### Operating fields

```awk
{ print $1 }
```

```scala
In { _ foreach { row => println(row(0)) }}
```

### Filtering data

```awk
/pattern/ { print $0 }
```

```scala
In { _ foreach {
  case row if row exists ("pattern".r.findFirstIn(_).isDefined) => println(row: _*)
  case _ =>
}
```

## Unique function to WOK 

### Typed variables

```bash
# the following command is equivalent to 
# var name = `value`
$ wok -v@char name=value

# the following command is equivalent to 
# var name = "value"
$ wok -v@str name=value

# the following command is equivalent to 
# var name = """value"""
$ wok -v@rawstr name=value
```

### Quote

```scala 
// setting Quote(mode=Min, quote='"') to Reader
OQ = Quote Min 

// setting Quote(mode=All, quote='"', escape='\\') to Writer
OFQ = Quote All Q('"') E('\\')
```

### Encoding 

```scala
// setting Codec to Reader
CD = Codec("Windows-31J")

// setting Codec to Writer
OCD = Codec("Windows-31J")
```

## Details

### System variables

| Name | Type | Default Value |
|------|------|------|
| FS | Regex | "[ \t]+".r |
| RS | Regex | "\r\n&#124;\r&#124;\n".r |
| FQ | Quote | Quote.None |
| CD | Codec | Codec("utf-8") |
| OFS | String | " " |
| ORS | String | "\n" |
| OFQ | Quote | Quote.None |
| OCD | Codec | Codec("utf-8") |

Note that the following system variables **cannot** be reassignment.

| Name | Type | Default Value |
|------|------|------|
| ARGV | List[String] | Nil |
| ARGC | Int | 0 |
| ARGIND | Int | 0 |
| FILENAME | String | "" |
| NFR | Int | 0 |
| NR | Int | 0 |
| NF | Int | 0 |
| FT | List[String] | Nil |
| RT | String | "" |
