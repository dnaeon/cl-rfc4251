## cl-rfc4251

`cl-rfc4251` is a Common Lisp system, which provides support for
parsing [RFC 4251][RFC 4251] encoded binary
data, as described in the [Data Type Representations Used in the SSH
Protocols](https://tools.ietf.org/html/rfc4251#section-5) section of
the document.

## Requirements

* [Quicklisp][Quicklisp]

## Installation

Clone the [cl-rfc4251][cl-rfc4251] repo in
your [Quicklisp local-projects
directory][Quicklisp FAQ].

``` shell
git clone https://github.com/dnaeon/cl-rfc4251.git
```

Load the system.

``` shell
CL-USER> (ql:quickload :cl-rfc4251)
```

## Supported Data Types

The following table summarizes the supported data types, that can be
decoded by the `cl-rfc4251` system. The `RFC 4251` and `cl-rfc4251
type` columns specify the mapping between the RFC defined data type
name and the keywords used to decode a given value in Common Lisp.

| RFC 4251    | cl-rfc4251 type | Description                                      |
|-------------|-----------------|--------------------------------------------------|
| `byte`      | `:byte`         | An arbitrary 8-bit value (octet)                 |
| `boolean`   | `:boolean`      | A boolean value, either `T` or `NIL`             |
| `uint32`    | `:uint32`       | Unsigned 32-bit integer in big-endian byte order |
| `uint64`    | `:uint64`       | Unsigned 64-bit integer in big-endian byte order |
| `string`    | `:string`       | Arbitrary length string                          |
| `mpint`     | `:mpint`        | Multiple precision integer                       |
| `name-list` | `:name-list`    | A list of string names                           |

Additional data types supported by the `cl-rfc4251` system, which are
not directly specified in RFC 4251, but are used by OpenSSH for
different purposes, e.g. public-key encoding, certificates, etc.

| cl-rfc4251 type                  | Description                                                                                          |
|----------------------------------|------------------------------------------------------------------------------------------------------|
| `:raw-bytes`                     | Read a sequence of raw bytes up to a given length                                                    |
| `:uint16`                        | Synonym for `:uint16-be`                                                                             |
| `:uint16`                        | Unsigned 16-bit integer in big-endian byte order                                                     |
| `:uint16-le`                     | Unsigned 16-bit integer in little-endian byte order                                                  |
| `:uint32-le`                     | Unsigned 32-bit integer in little-endian byte order                                                  |
| `:uint64-le`                     | Unsigned 64-bit integer in little-endian byte order                                                  |
| `:ssh-cert-embedded-string-list` | List of strings, embedded within a `:string` value. Used in [OpenSSH Certificates][OpenSSH.certkey]. |

## Usage

The `cl-rfc4251` system exports the generic function `DECODE` via the
`CL-RFC4251` (also available via its nickname `RFC4251`) package.

The `RFC4251:DECODE` function takes a *type* and a *binary stream*
from which to decode. Some types also take additional keyword
parameters (e.g. `:raw-bytes`), which allow to specify the number of
bytes to be decoded.

In all of the examples that follow below `s` represents a binary
stream. You can also use the `RFC4251:MAKE-BINARY-INPUT-STREAM`
function to create a binary stream, which uses a vector for the
underlying data.

`RFC4251:DECODE` returns multiple values -- the actual decoded value,
and the number of bytes that were read from the binary stream in order
to produce the value.

Decode raw bytes with a given length from the binary stream `s`.

``` common-lisp
CL-USER> (rfc4251:decode :raw-bytes s :length 4)
#(0 0 0 8)
4
```

The second value in above example indicates the actual bytes that were
read from the binary stream.

This example decodes a 16-bit unsigned integer represented in
big-endian byte order from a given binary stream `s`.

``` common-lisp
CL-USER> (let ((s (rfc4251:make-binary-input-stream #(0 #x80))))
           (rfc4251:decode :uint16 s))
128
2
```

The following example decodes a multiple precision integer represented
in two's complement format from the binary stream `s`.

``` common-lisp
CL-USER> (let ((s (rfc4251:make-binary-input-stream #(#x00 #x00 #x00 #x05 #xFF #x21 #x52 #x41 #x11))))
           (rfc4251:decode :mpint s))
-3735928559
9
```

The second value in the example above, which specifies the number of
bytes being read is `9`, because we had to read one `uint32` value
(the header) and additional `5` bytes (the value partition).

For additional examples, make sure to check the [test
suite](./t/test-suite.lisp).

## Tests

Tests are provided as part of the `cl-rfc4251.test` system.

In order to run the tests you can evaluate the following expressions.

``` common-lisp
CL-USER> (ql:quickload :cl-rfc4251.test)
CL-USER> (asdf:test-system :cl-rfc4251.test)
```

Or you can run the tests in a Docker container instead.

First, build the Docker image.

``` shell
docker build -t cl-rfc4251 .
```

Run the tests.

``` shell
docker run --rm cl-rfc4251
```

## Contributing

`cl-rfc4251` is hosted on
[Github](https://github.com/dnaeon/cl-rfc4251). Please contribute by
reporting issues, suggesting features or by sending patches using pull
requests.

## Authors

* Marin Atanasov Nikolov (dnaeon@gmail.com)

## License

This project is Open Source and licensed under the [BSD
License](http://opensource.org/licenses/BSD-2-Clause).

[RFC 4251]: https://tools.ietf.org/html/rfc4251
[Quicklisp]: https://www.quicklisp.org/beta/
[Quicklisp FAQ]: https://www.quicklisp.org/beta/faq.html
[cl-rfc4251]: https://github.com/dnaeon/cl-rfc4251
[OpenSSH.certkey]: https://cvsweb.openbsd.org/src/usr.bin/ssh/PROTOCOL.certkeys?annotate=HEAD
