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
decoded or encoded by the respective `RFC4251:DECODE` and
`RFC4251:ENCODE` generic functions. The `RFC 4251` and `cl-rfc4251
type` columns specify the mapping between the RFC-defined data type
name and the keywords used to decode or encode a given value in Common
Lisp.

| RFC 4251    | cl-rfc4251 type | Description                                      |
|-------------|-----------------|--------------------------------------------------|
| `byte`      | `:byte`         | An arbitrary 8-bit value (octet)                 |
| `byte[n]`   | `:raw-bytes`    | A sequence of raw bytes up to a given length     |
| `boolean`   | `:boolean`      | A boolean value                                  |
| `uint32`    | `:uint32`       | Unsigned 32-bit integer in big-endian byte order |
| `uint64`    | `:uint64`       | Unsigned 64-bit integer in big-endian byte order |
| `string`    | `:string`       | Arbitrary length string                          |
| `mpint`     | `:mpint`        | Multiple precision integer                       |
| `name-list` | `:name-list`    | A string of comma-separated names                |

In addition to the above data types the `cl-rfc4251` system supports
encoding and decoding of these types as well. Note, that these are not
mentioned in RFC 4251.

| cl-rfc4251 type | Description                                           |
|-----------------|-------------------------------------------------------|
| `:uint16-be`    | Unsigned 16-bit integer in big-endian byte order      |
| `:uint16-le`    | Unsigned 16-bit integer in little-endian byte order   |
| `:uint32-le`    | Unsigned 32-bit integer in little-endian byte order   |
| `:uint64-le`    | Unsigned 64-bit integer in little-endian byte order   |
| `:c-string`     | NULL-terminated C string                              |
| `:buffer`       | A buffer similar to `:string`, but contains raw bytes |

## Usage

The following section provides various examples showing how to encode
and decode values using the `cl-rfc4251` system.

For additional examples, make sure to check the [test
suite](./t/test-suite.lisp).

### Decoding

The `cl-rfc4251` system exports the generic functions `DECODE` and
`ENCODE` via the `CL-RFC4251` (also available via its nickname
`RFC4251`) package.

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

A bytes buffer is similar to a `:string`, except that the bytes are
not explicitely converted to a string value, but instead the result
contains the raw bytes.

``` common-lisp
CL-USER> (let ((s (rfc4251:make-binary-input-stream #(#x00 #x00 #x00 #x04 #x0A #x0B #x0C #x0D))))
           (rfc4251:decode :buffer s))
#(10 11 12 13)
8
```

### Encoding

The `RFC4251:ENCODE` generic function is used to encode data.  The
function returns the total number of bytes written to the binary
stream.

You can also use the `RFC4251:MAKE-BINARY-OUTPUT-STREAM` function to
create a binary stream, if needed.

The following example encodes an unsigned 16-bit integer value.

``` common-lisp
CL-USER> (defparameter *s* (rfc4251:make-binary-output-stream))
*S*
CL-USER> (rfc4251:encode :uint16 42 *s*)
2
CL-USER> (rfc4251:get-binary-stream-bytes *s*)
#(0 42)
```

Note in the second expression the number of bytes that were written.

Above example, can be written this way as well, if you only care about
the actual encoded bytes.

``` common-lisp
CL-USER> (let ((s (rfc4251:make-binary-output-stream)))
           (rfc4251:encode :uint16 42 s)          ;; <- Encode the value into the stream
           (rfc4251:get-binary-stream-bytes s)) ;; <- Return the contents of the stream
#(0 42)
```

The following example encodes a string value.

``` common-lisp
CL-USER> (defparameter *s* (rfc4251:make-binary-output-stream))
*S*
CL-USER> (rfc4251:encode :string "Hello, World!" *s*)
17
CL-USER> (rfc4251:get-binary-stream-bytes *s*)
#(0 0 0 13 72 101 108 108 111 44 32 87 111 114 108 100 33)
```

The result from the second expression here is `17`, which includes the
`4` (uint32) bytes required for the header (representing the bytes that follow),
plus the additional `13` bytes representing the actual string data.

The following examples show how to encode `mpint` values according to
[RFC 4251][RFC 4251].

``` common-lisp
CL-USER> (let ((s (rfc4251:make-binary-output-stream)))
           (rfc4251:encode :mpint #x00 s)         ;; <- Encode the zero value
           (rfc4251:get-binary-stream-bytes s)) ;; <- Get the encoded data
#(0 0 0 0)
```

Here are a few more examples taken directly from the examples described in
[RFC 4251][RFC 4251].

``` common-lisp
CL-USER> (let ((s (rfc4251:make-binary-output-stream)))
           (rfc4251:encode :mpint #x-DEADBEEF s)
           (rfc4251:get-binary-stream-bytes s))
#(0 0 0 5 255 33 82 65 17)
CL-USER> (let ((s (rfc4251:make-binary-output-stream)))
           (rfc4251:encode :mpint #x80 s)
           (rfc4251:get-binary-stream-bytes s))
#(0 0 0 2 0 128)
CL-USER> (let ((s (rfc4251:make-binary-output-stream)))
           (rfc4251:encode :mpint #x9A378F9B2E332A7 s)
           (rfc4251:get-binary-stream-bytes s))
#(0 0 0 8 9 163 120 249 178 227 50 167)
CL-USER> (let ((s (rfc4251:make-binary-output-stream)))
           (rfc4251:encode :mpint #x-1234 s)
           (rfc4251:get-binary-stream-bytes s))
#(0 0 0 2 237 204)
```

Encoding a bytes buffer. A bytes buffer is preceeded by its length,
similar to the way `:string` values are being encoded.

``` common-lisp
CL-USER> (let ((s (rfc4251:make-binary-output-stream)))
           (rfc4251:encode :buffer #(#x0A #x0B #x0C #x0D) s)
           (rfc4251:get-binary-stream-bytes s))
#(0 0 0 4 10 11 12 13)
```

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
