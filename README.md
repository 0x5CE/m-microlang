# m-MicroLang

m-MicroLang is a stack-based language defined entirely using Clojure macros. Features include variables, if statement, and calling standard Clojure functions. Additionally, it has descriptive errors.

## Usage

`defstackfn` is provided to define stack-based a stack based function written in m-MicroLang, given function name, arguments names, and statements written in m-MicroLang. `generate-code` is a function used by the previously-mentioned macro to generate Clojure code as data.

## Testing

To run tests

    $ lein test
