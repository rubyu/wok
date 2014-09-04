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
In { _
  .filter (_ exists ("pattern".r.findFirstIn(_).isDefined))
  .foreach (row => println(row: _*))
}
```

## Unique function to WOK 

### Typed variables

```bash
# the following command is equivalent to 
# val name = `value`
$ wok -v@char name=value

# the following command is equivalent to 
# val name = "value"
$ wok -v@str name=value

# the following command is equivalent to 
# val name = """value"""
$ wok -v@rawstr name=value
```

### Quote

```scala 
// setting Quote(mode=Min, quote='"') to Reader
OQ = Quote.Min 

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